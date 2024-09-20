#' Ajoute un tableau couleur pour les classes IPR
#'
#' @return un dataframe avec les informations pour les couleurs des classes IPR
#' @export
#'
#' @importFrom dplyr filter select distinct mutate recode case_when
#'
#' @examples
#' \dontrun{
#' tab_col_class_ipr()
#' }
#' 

tab_col_class_ipr <- function(){
  
  col_ipr <- setNames(c("#1f78b4","#b2df8a","#ffff33","#ff7f00","#e41a1c"),
                      c("Tr\u00e8s bon",'Bon', 'Moyen', 'M\u00e9diocre', 'Mauvais'))
  
  col_ipr_classe <- 
    classe_ipr %>% 
    dplyr::filter(cli_altitude_min != 500 | is.na(cli_altitude_min) ) %>% 
    dplyr::select(cli_classe, cli_libelle, cli_borne_inf, cli_borne_sup) %>% 
    dplyr::distinct() %>% 
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