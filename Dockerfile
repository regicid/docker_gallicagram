FROM openanalytics/r-base

MAINTAINER Tobias Verbeke "tobias.verbeke@openanalytics.eu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    libxml2-dev \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the gallicagram app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
	libnlopt-dev \
	&& rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
	cmake \
	&& rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get upgrade --yes

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('ggplot2','plotly','stringr','Hmisc','xml2','shinythemes','htmlwidgets','httr','ngramr','dplyr','htmltools'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('shinyWidgets'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('purrr'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('RSelenium'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('rvest'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('rclipboard'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('RSQLite'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('tidytext'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('DBI'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('shinybusy'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('lubridate'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('ggthemes'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('RColorBrewer'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('cowplot'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('raster','leaflet'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('sf'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('scales'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('cartogram'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('shinyjs'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('gtrendsR'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('timetk'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('jsonlite'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('gtrendsR'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('ggwordcloud'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('FactoMineR'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('chron'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('FactoMineR'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('tidyr'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('shinyalert'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('factoextra'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('bezier'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('jsonlite'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('doParallel'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('crul'), repos='https://cloud.r-project.org/')"


# copy the app to the image
RUN mkdir /root/gallicagram
COPY gallicagram /root/gallicagram

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/gallicagram')"]
