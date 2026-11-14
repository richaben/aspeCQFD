# Plot objectifs réseaux pour une station

Plot objectifs réseaux pour une station

## Usage

``` r
plot_objectif_reseau(df)
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
aspe_table_station1 <- 
  mef_creer_table_fiches() %>% 
  dplyr::filter(code_sta_pp == "03174000_005")

plot_objectif_reseau(aspe_table_station1)
} # }
```
