# Créer un tableau peuplement à partir du dataframe pour les fiches stations

Créer un tableau peuplement à partir du dataframe pour les fiches
stations

## Usage

``` r
mef_creer_table_peuplement(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

## Value

un dataframe avec les informations pour le peuplement

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches %>%
  dplyr::filter(code_sta_pp == "03231000_013") %>% 
  mef_creer_table_peuplement()
} # }
```
