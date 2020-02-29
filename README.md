
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FactorAssumptions

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/FactorAssumptions)](https://cran.r-project.org/package=FactorAssumptions)

Set of Assumptions for Factor and Principal Component Analysis. Tests
for Kaiser-Meyer-Olkin (KMO) and Communalities in a dataset. It provides
a final sample by removing variables in a iterable manner while keeping
account of the variables that were removed in each step.

## Installation

  - You can install from CRAN
  - You can install the released version of FactorAssumptions from
    [Github](https://github.com/storopoli/FactorAssumptions)

<!-- end list -->

``` r
install.packages("FactorAssumptions")

## install devtools if not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install FactorAssumptions from Github
devtools::install_github("storopoli/FactorAssumptions")
```

## What is KMO and Communalities?

*Factor Analysis* and *Principal Components Analysis* (PCA) have some
precautions and assumptions to be observed (Hair et al. (2018)).

The first one is the KMO (Kaiser-Meyer-Olkin) measure, which measures
the proportion of variance among the variables that can be derived from
the common variance, also called systematic variance. KMO is computed
between 0 and 1. Low values (close to 0) indicate that there are large
partial correlations in comparison to the sum of the correlations, that
is, there is a predominance of correlations of the variables that are
problematic for the factorial/principal component analysis. Hair et al.
(2018) suggest that individual KMOs smaller than 0.5 be removed from the
factorial/principal component analysis. Consequently, this removal
causes the overall KMO of the remaining variables of the
factor/principal component analysis to be greater than 0.5.

The second assumption of a valid factor or PCA analysis is the
communality of the rotated variables. The commonalities indicate the
common variance shared by factors/components with certain variables.
Greater communality indicated that a greater amount of variance in the
variable was extracted by the factorial/principal component solution.
For a better measurement of factorial/principal component analysis,
communalities should be 0.5 or greater (Hair et al. (2018)).

## Vignette

I encourage you to check the vignette on how to use the package.

# Author

Jose Eduardo Storopoli [e-mail](mailto:thestoropoli@gmail.com)

# References

<div id="refs" class="references hanging-indent">

<div id="ref-hair2018">

Hair, Joseph F., William C. Black, Barry J. Babin, and Rolph E.
Anderson. 2018. *Multivariate Data Analysis*. 8th ed. Cengage Learning.

</div>

</div>
