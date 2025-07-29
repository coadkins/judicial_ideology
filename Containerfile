FROM almalinux:9
# install uv
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/

# Update system packages
RUN dnf -y update
RUN dnf -y install wget
RUN dnf -y install cmake
RUN dnf -y install https://github.com/r-lib/rig/releases/download/latest/r-rig-latest-1.$(arch).rpm

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
RUN mkdir -p .cache/
RUN mkdir -p renv/

# Copy renv.lock from local directory
COPY renv.lock renv.lock

# install R
RUN rig add release

# Install quarto
RUN QUARTO_DL_URL=$(wget -qO- https://quarto.org/docs/download/_download.json | grep -oP "(?<=\"download_url\":\s\")https.*linux-amd64\.tar.gz") && \
wget $QUARTO_DL_URL -O /tmp/quarto.tar.gz && \
tar -C /etc -xvzf /tmp/quarto.tar.gz && \
EXECUTABLE_PATH=$(find /etc -type d -maxdepth 1 -name "quarto-*") && \
ln -s $EXECUTABLE_PATH/bin/quarto /usr/local/bin/quarto
# add TeX
RUN quarto install tinytex --update-path

# Install renv and package dependencies
RUN Rscript -e 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/rhel9/latest")); pak::pak("renv"); renv::activate(); renv::restore(exclude = c("rstan", "rstanarm")); cmdstanr::install_cmdstan("/usr/cmdstan", version = "2.36.0")'
# set up cmdstan
RUN echo 'cmdstanr::set_cmdstan_path("/usr/cmdstan/cmdstan-2.36.0")' >> .Rprofile

# Run targets pipeline
ENTRYPOINT Rscript /usr/R/stan_hirt.R
