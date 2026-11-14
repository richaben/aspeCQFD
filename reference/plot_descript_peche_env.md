# Plot descriptif des paramètres de pêche et environnementaux

Plot descriptif des paramètres de pêche et environnementaux

## Usage

``` r
plot_descript_peche_env(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

## Value

un graphique ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches %>%
  dplyr::filter(code_sta_pp == "03231000_013") %>%
  plot_descript_peche_env()
  } # }
```
