# Obtenir la liste des opérations sur une station

Obtenir la liste des opérations sur une station

## Usage

``` r
get_liste_ope_station(df, code_sta_pp)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

- code_sta_pp:

  le numéro 'code station et point de prélèvement'

## Value

un dataframe avec la liste des opérations pour la station

## Examples

``` r
if (FALSE) { # \dontrun{

aspe_table_fiches <-
mef_creer_table_fiches()

get_liste_ope_station(df = aspe_table_fiches, 
code_sta_pp = "03231000_013")
} # }
```
