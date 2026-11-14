# Plot un graphique des notes IPR pour une station

Le graphique se compose de 2 parties

## Usage

``` r
plot_ipr_station(df, df_col_ipr)
```

## Arguments

- df:

  un dataframe aspe issu de la fonction
  [`mef_creer_table_peuplement()`](https://richaben.github.io/aspeCQFD/reference/mef_creer_table_peuplement.md)

- df_col_ipr:

  un dataframe contenant les classes IPR, à créer avec la fonction
  [`tab_col_class_ipr()`](https://richaben.github.io/aspeCQFD/reference/tab_col_class_ipr.md)

## Value

un graphique ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{

## tableau stations
aspe_table_fiches_ipr <-
  aspe_table_fiches_peuplement %>% 
  dplyr::select(code_sta_pp, 
                sta_libelle_sandre, 
                ope_id, 
                annee, 
                ope_date,
                ipr,
                cli_id,
                cli_libelle) %>% 
  dplyr::distinct() 

## tableau classes IPR
df_col_ipr <-
  tab_col_class_ipr()

## graphiques
aspe_table_fiches_ipr %>% 
  dplyr::filter(code_sta_pp == "03231000_013") %>% 
  plot_ipr_station(df = ., 
                   df_col_ipr = df_col_ipr)
                   
} # }
```
