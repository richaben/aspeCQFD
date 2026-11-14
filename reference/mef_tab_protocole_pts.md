# Mise en forme d'un tableau protocole pêche points pour description opération

Mise en forme d'un tableau protocole pêche points pour description
opération

## Usage

``` r
mef_tab_protocole_pts(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

## Value

un dataframe avec les informations pour la description du protocole de
pêche

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches %>% 
  dplyr::filter(code_sta_pp == "03231000_013") %>% 
  mef_tab_protocole_pts()
} # }
```
