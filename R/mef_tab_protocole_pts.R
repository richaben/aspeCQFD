#' Mise en forme d'un tableau protocole pêche points pour description opération
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un dataframe avec les informations pour la description du protocole de pêche
#' @export
#' 
#' @importFrom aspe mef_ajouter_type_prelevement mef_ajouter_groupe_points
#' @importFrom dplyr left_join select mutate recode
#' @importFrom stringr str_wrap
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches %>% 
#'   dplyr::filter(code_sta_pp == "03231000_013") %>% 
#'   mef_tab_protocole_pts()
#' }
#' 

mef_tab_protocole_pts <- function(df){
  df %>% 
    aspe::mef_ajouter_type_prelevement() %>% 
    aspe::mef_ajouter_groupe_points() %>%
    dplyr::left_join(passage, by = c('pre_id' = 'pas_id')) %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, annee, 
                  pro_libelle, 
                  mop_libelle, 
                  tpe_libelle, 
                  grp_tgp_id, 
                  grp_nombre, 
                  pas_numero) %>%
    dplyr::mutate(grp_tgp_id = dplyr::recode(grp_tgp_id, 
                               !!!setNames(ref_type_groupe_points$tgp_libelle,
                                           ref_type_groupe_points$tgp_id)),
           grp_tgp_id = stringr::str_wrap(grp_tgp_id,12)) %>% 
    unique()
}

