FROM rocker/r-base:4.2.1
RUN apt-get update -y && apt-get install -y  libicu-dev  make  zlib1g-dev  pandoc && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("renv","remotes"))'
COPY renv.lock renv.lock
RUN R -e 'renv::restore()
