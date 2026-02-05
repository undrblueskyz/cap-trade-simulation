FROM rocker/shiny:4.3.3

RUN R -e "install.packages(c('shiny','dplyr','scales','DBI','RSQLite','tibble'), repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server
COPY app.R /srv/shiny-server/app.R

EXPOSE 3838

# On Render, the disk mounts at /var/data. Ensure it's writable at runtime.
CMD bash -lc "mkdir -p /var/data && chown -R shiny:shiny /var/data && chmod -R 777 /var/data && /usr/bin/shiny-server"

