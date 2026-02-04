FROM rocker/shiny:4.3.3

RUN R -e "install.packages(c('shiny','dplyr','scales','DBI','RSQLite','tibble'), repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server
COPY app.R /srv/shiny-server/app.R

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
