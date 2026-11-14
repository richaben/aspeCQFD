# Générer une fiche de Contrôle Qualité des données pour une station

Générer une fiche de Contrôle Qualité des données pour une station

## Usage

``` r
generate_fiche_cq_station(
  stations,
  df_data,
  dossier_sortie = getwd(),
  auteur,
  annee_debut,
  annee_fin,
  interactive = FALSE
)
```

## Arguments

- stations:

  un vecteur de code de station

- df_data:

  un dataframe avec les données nécessaires pour la fiche

- dossier_sortie:

  le dossier de sortie des fiches html

- auteur:

  le nom de l'auteur de la fiche

- annee_debut:

  l'année de début des données

- annee_fin:

  l'année de fin des données

- interactive:

  un booléen si les graphiques sont interactifs, FALSE par défaut

## Value

un ou des fichiers html dans le dossier de sortie spécifié

## Examples

``` r
if (FALSE) { # \dontrun{
generate_fiche_cq_station(stations = c("03231000_013", "03231000_014"),
dossier_sortie = "D:/fiches_CQ/",
auteur = "Jean Dupont")
} # }
```
