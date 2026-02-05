FROM rocker/shiny:4.3.3

RUN R -e "install.packages(c('shiny','dplyr','scales','DBI','RSQLite','tibble'), repos='https://cloud.r-project.org')"

# Ensure the DB mount path exists and is writable by shiny-server's user
RUN mkdir -p /var/data && chown -R shiny:shiny /var/data

WORKDIR /srv/shiny-server
COPY app.R /srv/shiny-server/app.R

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
