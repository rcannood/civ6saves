FROM rocker/tidyverse:4.0.2

RUN apt-get update && apt-get -y install ffmpeg nodejs npm libpoppler-cpp-dev

# install civ6-save-parser but remove quotes in strings
RUN mkdir /app && cd /app && npm install civ6-save-parser && sed -i "s#\\\\'##" /app/node_modules/civ6-save-parser/index.js

RUN cd /app && git clone --depth 1 https://github.com/rcannood/civ6save-editing.git

RUN Rscript -e 'install.packages("remotes"); install.packages("ggnewscale"); install.packages("pdftools")'

RUN Rscript -e 'remotes::install_github("rcannood/civ6saves")'

COPY generate_movie.R /app/generate_movie.R

WORKDIR /app

ENTRYPOINT ["/app/generate_movie.R"]
