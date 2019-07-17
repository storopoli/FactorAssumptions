# FactorAssumptions 

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FactorAssumptions)](https://cran.r-project.org/package=FactorAssumptions)

Set of Assumptions for Factor and Principal Component Analysis.
Tests for Kaiser-Meyer-Olkin (KMO) and Communalities in a dataset. It provides a final sample by removing variables in a iterable manner while keeping account of the variables that were removed in each step.

## Installation

```r
# Install FactorAssumptions from CRAN
install.packages("FactorAssumptions")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("storopoli/FactorAssumptions")
```
## Vignette

I encourage you to check the [vignette](vignettes/vignette.Rmd) on how to use the package.
# Author
Jose Eduardo Storopoli
[e-mail](mailto:thestoropoli@gmail.com)

# Road Map
- Make an argument for `Communalities` function to work either with *Principal Components Analysis* and *Factor Analysis*
	- Also care for the factoring method argument `fm`
- Make a *Vignette*
