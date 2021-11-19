FROM rocker/shiny:4.1.0

# Install and update system libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Copy app source and lock file
COPY src ./src
COPY renv.lock ./renv.lock

# Install dependencies using cache
ENV RENV_PATHS_CACHE=/renv/cache
RUN Rscript -e 'install.packages("renv")'

# Run app
EXPOSE 3838
CMD ["R", "-e", "renv::restore(); shiny::runApp('/dev/src', host = '0.0.0.0', port = 3838)"]
