# Plot le graphique des probas des espèces issues du calcul de l'IPR

Plot le graphique des probas des espèces issues du calcul de l'IPR

## Usage

``` r
plot_ipr_proba_esp_station(df)
```

## Arguments

- df:

  un dataframe issu de la fonction
  [`mef_tab_proba_esp_ipr()`](https://richaben.github.io/aspeCQFD/reference/mef_tab_proba_esp_ipr.md)

## Value

un graphique ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches %>% 
  dplyr::filter(code_sta_pp == "03174000_005") %>% 
  mef_tab_proba_esp_ipr() %>% 
  plot_ipr_proba_esp_station()
} # }
```
