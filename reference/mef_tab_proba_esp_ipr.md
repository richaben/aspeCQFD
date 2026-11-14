# Créer un tableau avec les probas IPR pour les espèces dans les stations

Créer un tableau avec les probas IPR pour les espèces dans les stations

## Usage

``` r
mef_tab_proba_esp_ipr(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

## Value

un dataframe avec les informations pour les probabilités IPR

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches %>% 
  dplyr::filter(code_sta_pp == "03174000_005") %>% 
  mef_tab_proba_esp_ipr()
} # }
```
