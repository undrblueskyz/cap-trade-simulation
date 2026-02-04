# app.R
library(shiny)
library(dplyr)
library(scales)
library(DBI)
library(RSQLite)

# -----------------------
# Scenario defaults
# -----------------------
default_plants <- tibble::tibble(
  Plant = c("A - Old Coal", "B - Mixed Fuel", "C - New Gas"),
  Baseline_Emissions = c(50, 30, 20),
  Allowances_Initial = c(30, 35, 35),
  MAC = c(600, 300, 100) # $/ton reduced
)

DEFAULT_CAP <- 100

# SQLite path:
# - On Render, mount a Persistent Disk at /var/data
# - Locally, it will just create ./data
DB_DIR <- Sys.getenv("DB_DIR", unset = "/var/data")
dir.create(DB_DIR, showWarnings = FALSE, recursive = TRUE)
DB_PATH <- file.path(DB_DIR, "cap_trade.sqlite")

db_connect <- function() {
  dbConnect(RSQLite::SQLite(), DB_PATH)
}

db_init <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)

  # Plants table (static scenario)
  if (!dbExistsTable(con, "plants")) {
    dbWriteTable(con, "plants", default_plants, overwrite = TRUE)
  }

  # Abatement table (shared, one row per plant)
  if (!dbExistsTable(con, "abatement")) {
    abate <- tibble::tibble(Plant = default_plants$Plant, Abatement = rep(0, nrow(default_plants)))
    dbWriteTable(con, "abatement", abate, overwrite = TRUE)
  }

  # Trades table (shared ledger)
  if (!dbExistsTable(con, "trades")) {
    trades <- tibble::tibble(
      id = integer(),
      timestamp = character(),
      Seller = character(),
      Buyer = character(),
      Allowances = numeric(),
      Price = numeric()
    )
    dbWriteTable(con, "trades", trades, overwrite = TRUE)
  }

  # Settings table (cap)
  if (!dbExistsTable(con, "settings")) {
    settings <- tibble::tibble(key = "cap", value = as.character(DEFAULT_CAP))
    dbWriteTable(con, "settings", settings, overwrite = TRUE)
  }
}

db_get_plants <- function() {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbReadTable(con, "plants") |> as_tibble()
}

db_get_abatement <- function() {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbReadTable(con, "abatement") |> as_tibble()
}

db_set_abatement <- function(plant, abatement_value) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "UPDATE abatement SET Abatement = ? WHERE Plant = ?", params = list(abatement_value, plant))
}

db_get_trades <- function() {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  tbl <- dbReadTable(con, "trades") |> as_tibble()
  if (nrow(tbl) == 0) return(tbl)
  tbl |> arrange(desc(id))
}

db_add_trade <- function(seller, buyer, allowances, price) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  # Determine next id
  max_id <- dbGetQuery(con, "SELECT COALESCE(MAX(id), 0) AS m FROM trades")$m[1]
  new_id <- max_id + 1
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dbExecute(
    con,
    "INSERT INTO trades (id, timestamp, Seller, Buyer, Allowances, Price) VALUES (?, ?, ?, ?, ?, ?)",
    params = list(new_id, ts, seller, buyer, allowances, price)
  )
}

db_clear_trades <- function() {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "DELETE FROM trades")
}

db_reset_round <- function() {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "DELETE FROM trades")
  dbExecute(con, "UPDATE abatement SET Abatement = 0")
  dbExecute(con, "UPDATE settings SET value = ? WHERE key = 'cap'", params = list(as.character(DEFAULT_CAP)))
}

db_get_cap <- function() {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  res <- dbGetQuery(con, "SELECT value FROM settings WHERE key='cap' LIMIT 1")
  if (nrow(res) == 0) return(DEFAULT_CAP)
  as.numeric(res$value[1])
}

db_set_cap <- function(cap_value) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "UPDATE settings SET value = ? WHERE key='cap'", params = list(as.character(cap_value)))
}

# Initialize DB on startup
db_init()

ui <- fluidPage(
  titlePanel("Shared Cap-and-Trade Trading Worksheet (SO₂) — Acid Rain Program"),
  tags$p("Everyone in class writes to the same ledger. Add trades, update abatements, and watch compliance update live."),

  # auto-refresh shared views every 2 seconds
  tags$script(HTML("
    setInterval(function(){
      Shiny.setInputValue('tick', new Date().getTime(), {priority: 'event'});
    }, 2000);
  ")),

  fluidRow(
    column(
      4,
      wellPanel(
        tags$h4("Scenario Controls (Shared)"),
        numericInput("cap", "Regional Cap (tons SO₂)", value = db_get_cap(), min = 0, step = 1),
        actionButton("save_cap", "Save Cap (shared)", class = "btn-primary"),
        tags$hr(),
        actionButton("reset_round", "Reset Round (clears trades + abatements)", class = "btn-danger"),
        tags$hr(),
        tags$h4("Quick Rules"),
        tags$ul(
          tags$li("Each allowance = 1 ton SO₂ allowed."),
          tags$li("Final Allowances = initial + bought − sold."),
          tags$li("Compliance: Final Allowances ≥ Final Emissions."),
          tags$li("Total class emissions must be ≤ the Cap.")
        )
      )
    ),

    column(
      8,
      tabsetPanel(
        tabPanel("1) Abatement (Shared)",
                 tags$h4("Update abatement (tons reduced)"),
                 tags$p("Enter abatement for your plant and click Save."),
                 fluidRow(
                   column(4, uiOutput("plant_select_ui")),
                   column(4, numericInput("abate_val", "Abatement (tons)", value = 0, min = 0, step = 1)),
                   column(4, br(), actionButton("save_abate", "Save Abatement", class = "btn-primary"))
                 ),
                 tags$hr(),
                 tableOutput("abatement_table")
        ),

        tabPanel("2) Trades (Shared Ledger)",
                 tags$h4("Add a trade (shared)"),
                 fluidRow(
                   column(3, uiOutput("seller_ui")),
                   column(3, uiOutput("buyer_ui")),
                   column(3, numericInput("allow", "Allowances (tons)", value = 1, min = 1, step = 1)),
                   column(3, numericInput("price", "Price/Allowance ($)", value = 0, min = 0, step = 10))
                 ),
                 fluidRow(
                   column(3, actionButton("add_trade", "Add Trade", class = "btn-primary")),
                   column(3, actionButton("clear_trades", "Clear Trades", class = "btn-warning")),
                   column(6, tags$small("Tip: Enter the negotiated price so we can discuss where the market cleared."))
                 ),
                 tags$hr(),
                 tableOutput("trades_table")
        ),

        tabPanel("3) Results (Live)",
                 tags$h4("Plant-by-plant results"),
                 tableOutput("results_table"),
                 tags$hr(),
                 tags$h4("Class totals"),
                 tableOutput("totals_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  plants <- reactive({
    input$tick
    db_get_plants()
  })

  output$plant_select_ui <- renderUI({
    selectInput("plant_choice", "Your Plant", choices = plants()$Plant)
  })

  output$seller_ui <- renderUI({
    selectInput("seller", "Seller", choices = plants()$Plant)
  })
  output$buyer_ui <- renderUI({
    selectInput("buyer", "Buyer", choices = plants()$Plant)
  })

  observeEvent(input$save_cap, {
    db_set_cap(input$cap)
    showNotification("Saved shared cap.", type = "message")
  })

  observeEvent(input$reset_round, {
    db_reset_round()
    updateNumericInput(session, "cap", value = db_get_cap())
    updateNumericInput(session, "abate_val", value = 0)
    showNotification("Round reset (trades cleared, abatements reset).", type = "warning")
  })

  observeEvent(input$save_abate, {
    req(input$plant_choice)
    db_set_abatement(input$plant_choice, as.numeric(input$abate_val))
    showNotification("Saved shared abatement.", type = "message")
  })

  observeEvent(input$add_trade, {
    req(input$seller, input$buyer, input$allow)
    validate(need(input$seller != input$buyer, "Seller and buyer must be different."))
    db_add_trade(input$seller, input$buyer, as.numeric(input$allow), as.numeric(input$price))
    showNotification("Trade added to shared ledger.", type = "message")
  })

  observeEvent(input$clear_trades, {
    db_clear_trades()
    showNotification("Trades cleared.", type = "warning")
  })

  abatement_tbl <- reactive({
    input$tick
    db_get_abatement()
  })

  trades_tbl <- reactive({
    input$tick
    db_get_trades()
  })

  output$abatement_table <- renderTable({
    plants() %>%
      left_join(abatement_tbl(), by = "Plant") %>%
      mutate(MAC = dollar(MAC)) %>%
      select(Plant, Baseline_Emissions, Allowances_Initial, Abatement, MAC) %>%
      rename(
        `Baseline Emissions` = Baseline_Emissions,
        `Initial Allowances` = Allowances_Initial,
        `MAC ($/ton)` = MAC
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$trades_table <- renderTable({
    tr <- trades_tbl()
    if (nrow(tr) == 0) return(data.frame(Note = "No trades yet."))
    tr %>%
      mutate(
        Price = ifelse(is.na(Price), 0, Price),
        Price = ifelse(Price == 0, "", dollar(Price))
      ) %>%
      rename(`Price/Allowance` = Price, `Time` = timestamp) %>%
      select(id, Time, Seller, Buyer, Allowances, `Price/Allowance`)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  results <- reactive({
    plants_df <- plants()
    abate_df <- abatement_tbl()
    tr <- trades_tbl()

    # Allowance deltas
    delta <- setNames(rep(0, nrow(plants_df)), plants_df$Plant)
    cash <- setNames(rep(0, nrow(plants_df)), plants_df$Plant)

    if (nrow(tr) > 0) {
      for (i in seq_len(nrow(tr))) {
        s <- tr$Seller[i]
        b <- tr$Buyer[i]
        qty <- as.numeric(tr$Allowances[i])
        price <- as.numeric(tr$Price[i])
        if (is.na(price)) price <- 0

        delta[[s]] <- delta[[s]] - qty
        delta[[b]] <- delta[[b]] + qty

        cash[[s]] <- cash[[s]] + price * qty
        cash[[b]] <- cash[[b]] - price * qty
      }
    }

    plants_df %>%
      left_join(abate_df, by = "Plant") %>%
      mutate(
        Abatement = ifelse(is.na(Abatement), 0, Abatement),
        Allowances_Delta = unname(delta[Plant]),
        Allowances_Final = Allowances_Initial + Allowances_Delta,
        Emissions_Final = pmax(0, Baseline_Emissions - Abatement),
        Abatement_Cost = Abatement * MAC,
        Trade_Cashflow = unname(cash[Plant]),
        Net_Cost = Abatement_Cost - Trade_Cashflow,
        Compliance = Allowances_Final >= Emissions_Final
      )
  })

  output$results_table <- renderTable({
    results() %>%
      mutate(
        Abatement_Cost = dollar(Abatement_Cost),
        Trade_Cashflow = dollar(Trade_Cashflow),
        Net_Cost = dollar(Net_Cost),
        Compliance = ifelse(Compliance, "✅ In Compliance", "❌ Out of Compliance")
      ) %>%
      select(
        Plant,
        Baseline_Emissions,
        Abatement,
        Emissions_Final,
        Allowances_Initial,
        Allowances_Final,
        Abatement_Cost,
        Trade_Cashflow,
        Net_Cost,
        Compliance
      ) %>%
      rename(
        `Baseline Emissions` = Baseline_Emissions,
        `Final Emissions` = Emissions_Final,
        `Initial Allowances` = Allowances_Initial,
        `Final Allowances` = Allowances_Final,
        `Abatement Cost` = Abatement_Cost,
        `Trade Cashflow (+rev / -cost)` = Trade_Cashflow,
        `Net Cost` = Net_Cost
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$totals_table <- renderTable({
    cap <- db_get_cap()
    res <- results()
    total_emissions <- sum(res$Emissions_Final)
    data.frame(
      `Total Final Emissions (tons)` = total_emissions,
      `Cap (tons)` = cap,
      `Meets Cap?` = ifelse(total_emissions <= cap, "✅ Yes", "❌ No"),
      check.names = FALSE
    )
  }, bordered = TRUE)

  # Keep UI cap synced with DB (if someone else changes it)
  observe({
    input$tick
    updateNumericInput(session, "cap", value = db_get_cap())
  })
}

shinyApp(ui, server)
