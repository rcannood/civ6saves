FROM rocker/tidyverse:4.0.2

RUN apt-get update && apt-get -y install ffmpeg nodejs npm libpoppler-cpp-dev

RUN mkdir /app && cd /app && npm install civ6-save-parser

RUN Rscript -e 'install.packages("remotes"); remotes::install_github("rcannood/civ6saves"); install.packages("ggnewscale"); install.packages("pdftools")'

COPY generate_movie.R /app/generate_movie.R

WORKDIR /app

ENTRYPOINT ["/app/generate_movie.R"]
