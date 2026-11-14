# Ajoute les données habitats à un dataframe avec les données aspe

Ajoute les données habitats à un dataframe avec les données aspe

## Usage

``` r
mef_ajouter_habitat(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`aspe::mef_creer_passerelle()`](https://pascalirz.github.io/aspe/reference/mef_creer_passerelle.html)
  ou
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

## Value

un dataframe avec les données habitats

## Examples

``` r
if (FALSE) { # \dontrun{
df <- 
  aspe::mef_creer_passerelle() %>% 
  mef_ajouter_habitat()
} # }
```
