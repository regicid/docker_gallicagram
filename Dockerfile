FROM rocker/shiny:4.5.2

# System libraries for the heavier R packages (sf, cartogram, FactoMineR, Hmisc, nloptr).
# rocker/shiny is jammy-based, so these are modern versions (GDAL 3.4+, libcurl 7.81+).
RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libcairo2-dev \
    libxt-dev \
    libssh2-1-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libmpfr-dev \
    libnlopt-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# All R packages in ONE layer, from P3M jammy BINARIES (no compiling, fast, no libuv/libcurl/GDAL grief).
# Duplicates from the old Dockerfile (gtrendsR, FactoMineR, jsonlite) removed.
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')); \
    install.packages(c( \
      'shiny','rmarkdown','ggplot2','plotly','stringr','Hmisc','xml2','shinythemes', \
      'htmlwidgets','httr','ngramr','dplyr','htmltools','shinyWidgets','purrr', \
      'RSelenium','rvest','rclipboard','RSQLite','tidytext','DBI','shinybusy', \
      'lubridate','ggthemes','RColorBrewer','cowplot','sf','scales','cartogram', \
      'shinyjs','gtrendsR','timetk','jsonlite','ggwordcloud','FactoMineR','chron', \
      'tidyr','shinyalert','factoextra','bezier','doParallel','crul' \
    ))"

# Build guard: fail the build (with the exact list) if anything didn't install,
# instead of discovering it at the ShinyProxy timeout.
RUN R -e "pkgs <- c('shiny','rmarkdown','ggplot2','plotly','stringr','Hmisc','xml2','shinythemes', \
      'htmlwidgets','httr','ngramr','dplyr','htmltools','shinyWidgets','purrr', \
      'RSelenium','rvest','rclipboard','RSQLite','tidytext','DBI','shinybusy', \
      'lubridate','ggthemes','RColorBrewer','cowplot','sf','scales','cartogram', \
      'shinyjs','gtrendsR','timetk','jsonlite','ggwordcloud','FactoMineR','chron', \
      'tidyr','shinyalert','factoextra','bezier','doParallel','crul'); \
    miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]; \
    if (length(miss)) stop('MISSING PACKAGES: ', paste(miss, collapse=', '))"

# App code LAST so edits don't bust the package cache.
COPY gallicagram /root/gallicagram
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

# Override rocker/shiny's shiny-server entrypoint so only runApp owns the port,
# and bind 0.0.0.0 so ShinyProxy can reach the container.
ENTRYPOINT []
CMD ["R", "-e", "shiny::runApp('/root/gallicagram', host='0.0.0.0', port=3838)"]
