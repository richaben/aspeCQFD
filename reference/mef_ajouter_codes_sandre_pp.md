# Ajoute à un dataframe aspe les codes SANDRE et points de prélèvement

Ajoute à un dataframe aspe les codes SANDRE et points de prélèvement

## Usage

``` r
mef_ajouter_codes_sandre_pp(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`aspe::mef_creer_passerelle()`](https://pascalirz.github.io/aspe/reference/mef_creer_passerelle.html)

## Value

un dataframe avec les infos de coordonnées stations et le code combinant
code sandre et points de prélèvement `code_sta_pp`

## Examples

``` r
if (FALSE) { # \dontrun{
df <- 
  aspe::mef_creer_passerelle() %>% 
  mef_ajouter_codes_sandre_pp()
} # }
```
