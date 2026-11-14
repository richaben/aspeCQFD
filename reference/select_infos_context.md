# Selectionne les informations contextuelles pour fiche station

Selectionne les informations contextuelles pour fiche station

## Usage

``` r
select_infos_context(df)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_fiches()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_fiches.md)

## Value

un dataframe avec les informations contextuelles pour la fiche station

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches %>% 
dplyr::filter(code_sta_pp == "03231000_013") %>% 
select_infos_context()
} # }
```
