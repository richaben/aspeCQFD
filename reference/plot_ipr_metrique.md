# Plot un graphique avec les métriques IPR théoriques et observés pour une station

Plot un graphique avec les métriques IPR théoriques et observés pour une
station

## Usage

``` r
plot_ipr_metrique(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_peuplement()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_peuplement.md)

## Value

un graphique ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches_ipr %>% 
dplyr::filter(code_sta_pp == "03231000_013") %>% 
plot_ipr_metrique()
} # }
```
