# Start with R version 4.3.3
FROM rocker/r2u:24.04

# Install utility packages
RUN Rscript -e "install.packages('bayesplot'); \
install.packages('dplyr'); \
install.packages('ggplot2'); \
install.packages('here'); \
install.packages('MASS'); \
install.packages('patchwork'); \
install.packages('posterior'); \
install.packages('purrr'); \
install.packages('tidyr'); \
install.packages('qs')"

# Install CmdStan
RUN Rscript -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"
RUN Rscript -e "cmdstanr::install_cmdstan(cores = 1)"

# Create directories
RUN mkdir -p /usr/R
RUN mkdir -p /usr/stan
RUN mkdir -p /usr/results
RUN mkdir -p /usr/graphics

# Set the working directory
WORKDIR /usr

# Copy entrypoint script
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Set the entrypoint
ENTRYPOINT ["/entrypoint.sh"]
