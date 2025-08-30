# README
This project contains an implementation of hierarchical Item Response model used in the report "Lopsided Judicial Polarization."

The code was run using R 4.51 and cmdstanR 2.36. On Linux, you should be to reproduce the results on a sufficiently powerful system with:

```
git clone --single-branch --branch main https://github.com/coadkins/judicial_ideology
cd judicial_ideology
Rscript R/setup.R
Rscript R/install_cmdstan.R
Rscript R/install_stantargets.R
Rscript -e 'targets::tar_make(store = "_targets")
```
