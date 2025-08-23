FROM almalinux:9
# Optional: install uv and Python
# COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/

# Update system packages
RUN dnf -y install epel-release
RUN dnf -y update
RUN dnf -y install wget cmake https://github.com/r-lib/rig/releases/download/latest/r-rig-latest-1.$(arch).rpm

# Add targets/igraph dependencies
RUN dnf -y install glpk-devel libxml2-devel openssl-devel

# Create a WORKDIR named after the project
WORKDIR /usr

# Copy project subdirectories
COPY R/ /usr/R/
COPY stan/ /usr/stan/
COPY _targets.R /usr/_targets.R

# Create other subdirectories
RUN mkdir -p results/
RUN mkdir -p graphics/
RUN mkdir -p cmdstan/

# Copy renv.lock from local directory
COPY renv.lock renv.lock

# install R
RUN rig add release --without-pak

# Optional: install quarto
# RUN QUARTO_DL_URL=$(wget -qO- https://quarto.org/docs/download/_download.json | grep -oP "(?<=\"download_url\":\s\")https.*linux-amd64\.tar.gz") && \
# wget $QUARTO_DL_URL -O /tmp/quarto.tar.gz && \
# tar -C /etc -xvzf /tmp/quarto.tar.gz && \
# EXECUTABLE_PATH=$(find /etc -type d -maxdepth 1 -name "quarto-*") && \
# ln -s $EXECUTABLE_PATH/bin/quarto /usr/local/bin/quarto
# Optional: add TeX
# RUN quarto install tinytex --update-path

# set up PPM for Almalinux
RUN echo 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/rhel9/latest"))' >> .Rprofile

# Install renv and package dependencies
RUN Rscript R/setup.R
RUN Rscript R/install_cmdstan.R
RUN Rscript R/install_stantargets.R

# set up cmdstan path
RUN echo 'cmdstanr::set_cmdstan_path("/usr/cmdstan/cmdstan-2.36.0")' >> .Rprofile

# set up S3 access credentials
RUN --mount=type=secret,id=s3_key \
    --mount=type=secret,id=s3_secret \
    --mount=type=secret,id=s3_bucket \
    --mount=type=secret,id=s3_region \
    --mount=type=secret,id=s3_endpoint \
    echo "AWS_ACCESS_KEY_ID=$(cat /run/secrets/s3_key)" >> /usr/.Renviron && \
    echo "AWS_SECRET_ACCESS_KEY=$(cat /run/secrets/s3_secret)" >> /usr/.Renviron && \
    echo "S3_BUCKET=$(cat /run/secrets/s3_bucket)" >> /usr/.Renviron && \
    echo "S3_REGION=$(cat /run/secrets/s3_region)" >> /usr/.Renviron && \
    echo "S3_ENDPOINT=$(cat /run/secrets/s3_endpoint)" >> /usr/.Renviron

# Default to bash so I can choose which script to run 
ENTRYPOINT Rscript 'R/sync_targets_metadata.R' && \
           Rscript -e 'targets::tar_make()' && \
           Rscript -e 'targets::tar_meta_upload()'

# Set environmental variables
ENV RENV_WATCHDOG_ENABLED FALSE
