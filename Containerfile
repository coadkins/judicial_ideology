FROM ghcr.io/r-lib/rig/ubuntu-24.04-release
# install uv
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/

# Update system packages
RUN apt-get -y update && apt-get -y upgrade
RUN apt-get -y install wget
RUN apt-get -y install libglpk-dev

# Create a WORKDIR named after the project
WORKDIR /usr

# Copy project subdirectories
COPY R/ /usr/R/
COPY stan/ /usr/stan/
COPY _targets.R _targets.R

# Create other subdirectories
RUN mkdir -p results/
RUN mkdir -p graphics/
RUN mkdir -p cmdstan/

# Copy renv.lock from local directory
COPY renv.lock renv.lock

# Install quarto
RUN QUARTO_DL_URL=$(wget -qO- https://quarto.org/docs/download/_download.json | grep -oP "(?<=\"download_url\":\s\")https.*linux-amd64\.tar.gz") && \
wget $QUARTO_DL_URL -O /tmp/quarto.tar.gz && \
tar -C /etc -xvzf /tmp/quarto.tar.gz && \
EXECUTABLE_PATH=$(find /etc -type d -maxdepth 1 -name "quarto-*") && \
ln -s $EXECUTABLE_PATH/bin/quarto /usr/local/bin/quarto
# add TeX
RUN quarto install tinytex --update-path

# Install renv and package dependencies
RUN Rscript -e 'pak::pak("renv")'
RUN Rscript -e "renv::activate()"
RUN Rscript -e 'renv::restore(exclude = c("rstan", "rstanarm"))'

# set up cmdstan
RUN Rscript -e 'cmdstanr::install_cmdstan("/usr/cmdstan", version = "2.36.0")'
RUN Rscript -e 'cmdstanr::set_cmdstan_path("/usr/cmdstan/cmdstan-2.36.0")'

# Run targets pipeline
ENTRYPOINT Rscript /usr/R/stan_hirt.R
