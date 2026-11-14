# Plot le graphique Protocole(s) + Passage(s) + Point(s) pour une station

Plot le graphique Protocole(s) + Passage(s) + Point(s) pour une station

## Usage

``` r
plot_proto_prospec_passage(df)
```

## Arguments

- df:

  un dataframe issu de la fonction
  [`mef_tab_protocole_pts()`](https://richaben.github.io/aspeCQFD/reference/mef_tab_protocole_pts.md)
  issu de la jointure avec une table générale
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

## Value

un graphique ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches %>%
  dplyr::filter(code_sta_pp == "03231000_013") %>%
  mef_tab_protocole_pts() %>% 
  plot_proto_prospec_passage()
} # }
```
