FROM openanalytics/r-base

MAINTAINER Benjamin Azoulay "benjamin.azoulay@ens-paris-saclay.fr"

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
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the gallicagram_app app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"


# copy the app to the image
RUN mkdir /root/gallicagram
COPY gallicagram /root/gallicagram

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/gallicagram')"]
