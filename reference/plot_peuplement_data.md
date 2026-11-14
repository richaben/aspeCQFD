# Plot du graphique du peuplement par espèce et par année pour une station

Plot du graphique du peuplement par espèce et par année pour une station

## Usage

``` r
plot_peuplement_data(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_peuplement()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_peuplement.md)

## Value

un graphique ggplot2 représentant le nombre d'individus par espèce, par
année et type de points

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches_peuplement %>%
  dplyr::filter(code_sta_pp == "03231000_013") %>% 
  plot_peuplement_data()
} # }
```
