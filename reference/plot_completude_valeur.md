# Plot le graphique de completude des valeurs pour une station et liste de paramètres

Plot le graphique de completude des valeurs pour une station et liste de
paramètres

## Usage

``` r
plot_completude_valeur(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md),
  avec une liste de paramètres à inspecter

## Value

un graphique ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches %>%
  dplyr::filter(code_sta_pp == "03231000_013") %>%
  dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee, pre_id,
                odp_longueur,
                odp_largeur_lame_eau,
                profondeur,
                ope_surface_calculee) %>%
  plot_completude_valeur()
} # }
```
