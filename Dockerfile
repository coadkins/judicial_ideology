# Start with R version 4.3.3
FROM rocker/r2u:24.04

# Install utility packages
RUN Rscript -e "install.packages('brms'); install.packages('dplyr'); install.packages('here'); install.packages('purrr'); install.packages('tidyr')"

# Install CmdStan
RUN Rscript -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"

RUN Rscript -e "cmdstanr::install_cmdstan(cores = 2)"

# Create a directory
RUN mkdir /home/judicial_ideology/
RUN mkdir /home/judicial_ideology/R
RUN mkdir /home/judicial_ideology/results

# Copy entrypoint script
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Set the entrypoint
ENTRYPOINT ["/entrypoint.sh"]

# Default command (can be overridden)
CMD ["help"]
