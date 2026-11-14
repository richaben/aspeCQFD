# Plot le graphique des paramètres IPR (présences et variations)

Plot le graphique des paramètres IPR (présences et variations)

## Usage

``` r
plot_ipr_param_station(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)
  et
  [`mef_tab_proba_esp_ipr()`](https://richaben.github.io/aspeCQFD/reference/mef_tab_proba_esp_ipr.md)

## Value

un graphique ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches_ipr_env %>%
  dplyr::filter(code_sta_pp == "03174000_005") %>%
  plot_ipr_param_station()
  } # }
```
