# Plot jours de pêche pour une station ou plusieurs

Plot jours de pêche pour une station ou plusieurs

## Usage

``` r
plot_jour_peche(df, jour_julien = F)
```

## Arguments

- df:

  un dataframe issu de la fonction
  [`get_liste_ope_station()`](https://richaben.github.io/aspeCQFD/reference/get_liste_ope_station.md)

- jour_julien:

  un booléen pour afficher les jours de l'année et code couleur saison,
  par défaut `FALSE`

## Value

un graphique ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
stations_sel <-
  get_liste_ope_station(df = aspe_table_fiches,
                        code_sta_pp = c("03231000_013","03174000_005")
  )

plot_jour_peche(stations_sel, jour_julien = F)
} # }
```
