# Plot du graphique des données faciès pour une station

Plot du graphique des données faciès pour une station

## Usage

``` r
plot_facies_data(df)
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
  plot_facies_data()
} # }
```
