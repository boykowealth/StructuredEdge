FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    git

RUN R -e "install.packages(c('devtools', 'magrittr', 'dplyr', 'tidyverse', 'golem', 'shiny', 'bslib', 'testthat', 'DT', 'ggplot2', 'rcpp'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"

RUN git clone https://github.com/boykowealth/structurededge.git /srv/shiny-server/structurededge
RUN chown -R shiny:shiny /srv/shiny-server/structurededge

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/structurededge/', host = '0.0.0.0', port = 3838)"]
