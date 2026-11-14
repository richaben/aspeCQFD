# Ajoute les informations contextuelles pour les opérations issues de ASPE

Ajoute les informations contextuelles pour les opérations issues de ASPE

## Usage

``` r
mef_ajouter_infos_context(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

## Value

un dataframe avec les infos contextuelles sur les opérations de pêche

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_station1 <- 
  mef_creer_table_fiches() %>% 
  dplyr::filter(code_sta_pp == "03174000_005") %>% 
  dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee) %>% 
  mef_ajouter_infos_context() %>% 
  unique()
} # }
```
