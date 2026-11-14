# Obtenir la liste des stations pour un département

Obtenir la liste des stations pour un département

## Usage

``` r
get_liste_stations_dpt(df, dpt)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

- dpt:

  un vecteur de numéro de département

## Value

un dataframe avec la liste des stations pour le département

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches <-
mef_creer_table_fiches()

get_liste_stations_dpt(df = aspe_table_fiches,
dpt = c("14", "27"))
} # }
```
