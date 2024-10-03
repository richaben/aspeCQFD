
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aspeCQFD - aspe Contrôle Qualité Fiches Données

<!-- badges: start -->

[![R-CMD-check](https://github.com/richaben/aspeCQFD/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/richaben/aspeCQFD/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of aspeCQFD is to …

## Installation

You can install the development version of aspeCQFD from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("richaben/aspeCQFD")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(aspeCQFD)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

``` r
htmltools::tags$iframe("./man/figures/fiche_CQ_donnees_station_03174000_005.html",
                       height=600, width="100%", `data-external` = 1)
```

<iframe height="600" width="100%" data-external="1">./man/figures/fiche_CQ_donnees_station_03174000_005.html</iframe>
