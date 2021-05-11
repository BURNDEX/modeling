FROM docker.io/rocker/geospatial

RUN install2.r -e -s \
    bigrquery \
    targets \
    future \
    aws.s3 \
    qs \
    progressr \
    tidymodels \
    timetk \
    kknn \
    foreach \
    doParallel

RUN installGithub.r \
    ropensci/tarchetypes \
    ropensci/USAboundaries \
    ropensci/USAboundariesData \
    mikejohnson51/AOI \
    mikejohnson51/climateR \
    tidymodels/spatialsample

RUN mkdir -p /app/burndex/R/

COPY ./R/data-utils.R /app/burndex/R
COPY ./R/model-utils.R /app/burndex/R
COPY ./R/targets-utils.R /app/burndex/R
COPY ./burndex-19597aebb216.json /app/burndex
COPY ./aws.json /app/burndex
COPY ./_targets.R /app/burndex

WORKDIR /app/burndex/

CMD Rscript -e "targets::tar_make()"