#' Fonction pour afficher des valeurs entières pour une variable
#'
#' @noRd
#' 
#' @param n nombre de breaks souhaité
#' @param ... 
#'
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

#' Colour scheme for ipr plot
#'
#' @noRd
#' 
#'
col_ipr <- setNames(c("#1f78b4","#b2df8a","#ffff33","#ff7f00","#e41a1c"),
                    c("Tr\u00e8s bon",'Bon', 'Moyen', 'M\u00e9diocre', 'Mauvais'))

#' Color table for ipr plot
#'
#' @noRd
#' 
#'
col_ipr_classe <- function(){
  classe_ipr %>% 
  dplyr::filter(cli_altitude_min != 500 | is.na(cli_altitude_min) ) %>% 
  dplyr::select(cli_classe,cli_libelle,cli_borne_inf,cli_borne_sup) %>% 
  unique() %>% 
  dplyr::mutate(col_ipr_classe = dplyr::recode(cli_libelle, !!!col_ipr),
                cli_libelle = factor(cli_libelle, 
                                     ordered = T, 
                                     levels= c("Tr\u00e8s bon",
                                               "Bon",
                                               "Moyen",
                                               "M\u00e9diocre",
                                               "Mauvais"))) %>% 
  dplyr::mutate(cli_borne_sup = dplyr::case_when(cli_borne_sup == 99 ~ 60,
                                                 TRUE ~ cli_borne_sup))
}