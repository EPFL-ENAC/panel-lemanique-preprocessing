[![Project Status](https://img.shields.io/badge/status-under%20development-yellow)](https://github.com/EPFL-ENAC/panel-lemanique-preprocessing)
![GitHub License](https://img.shields.io/github/license/EPFL-ENAC/panel-lemanique-preprocessing)

# panel-lemanique-preprocessing

## Install

To set up the pre-processing environment, you need [RStudio](https://posit.co/download/rstudio-desktop/) with the [renv](https://rstudio.github.io/renv/) package installed.

First, clone the repository. Then, open the `panel-lemanique-preprocessing.Rproj` file with RStudio and restore the dependencies from the lockfile using

```r
renv::restore()
```

## Usage

To run the preprocessing for all the waves, use

```bash
make preprocess_data
```

## Adding data preprocessing from a new wave or subproject

1. Add a pre-processing script in `R/`
2. Add a pre-processing rule to `Makefile` and include the rule as a dependency of the `preprocess_data` rule
