FROM docker.io/rocker/geospatial

RUN install2.r -e -s \
    foreach \
    bigrquery \
    progressr \
    doParallel

RUN Rscript -e "remotes::install_github('ropensci/USAboundaries')"
RUN Rscript -e "remotes::install_github('ropensci/USAboundariesData')"
RUN Rscript -e "remotes::install_github('mikejohnson51/AOI')"
RUN Rscript -e "remotes::install_github('mikejohnson51/climateR')"

RUN mkdir -p /app/burndex/R/

COPY ./R/bigquery.R /app/burndex/R
COPY ./R/data-utils.R /app/burndex/R
COPY ./burndex-19597aebb216.json /app/burndex

WORKDIR /app/burndex/

CMD Rscript -e "source('R/bigquery.R')"