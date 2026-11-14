# Création d'un tableau de base avec les infos ASPE pour les fiches

La fonction permet de constituer un tableau de base nécessaire pour la
création des fiches. Les données ASPE doivent être chargées dans
l'environnement au préalable. Seules les stations appartenant aux
réseaux RCS, RHP et RRP sont conservées.

## Usage

``` r
mef_creer_table_base_fiches()
```

## Value

un dataframe avec les informations utiles

## Examples

``` r
if (FALSE) { # \dontrun{
aspe_table_fiches <- 
  mef_creer_table_base_fiches()
} # }
```
