# Ajoute les données de facies à un dataframe

Ajoute les données de facies à un dataframe

## Usage

``` r
mef_ajouter_facies(df)
```

## Arguments

- df:

  un dataframe issu de la fonction
  [`aspe::mef_creer_passerelle()`](https://pascalirz.github.io/aspe/reference/mef_creer_passerelle.html)

## Value

un dataframe avec les données de facies

## Examples

``` r
if (FALSE) { # \dontrun{
df <-
 aspe::mef_creer_passerelle() %>%
 mef_ajouter_facies()
 } # }
```
