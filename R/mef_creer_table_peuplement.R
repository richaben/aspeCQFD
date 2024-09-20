#' Créer un tableau peuplement à partir du dataframe pour les fiches stations
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un dataframe avec les informations pour le peuplement
#' @export
#' 
#' @importFrom aspe mef_ajouter_type_prelevement mef_ajouter_groupe_points
#' @importFrom dplyr select left_join mutate recode case_when distinct
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches %>%
#'   dplyr::filter(code_sta_pp == "03231000_013") %>% 
#'   mef_creer_table_peuplement()
#' }
#' 

mef_creer_table_peuplement <- function(df) {
  df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_date,
                  pre_id,
                  lop_id) %>% 
    dplyr::distinct() %>% 
    # jointure lot poissons
    dplyr::left_join(y = lot_poissons %>%
                       dplyr::select(lop_id, esp_id = lop_esp_id, lop_effectif)) %>%
    # jointure noms esp
    dplyr::left_join(y = ref_espece %>%
                       dplyr::select(esp_id, esp_code_alternatif,esp_nom_commun)) %>%
    
    # remove esp_id
    dplyr::select(-esp_id) %>% 
    aspe::mef_ajouter_type_prelevement() %>% 
    aspe::mef_ajouter_groupe_points() %>%
    dplyr::left_join(passage, by = c('pre_id' = 'pas_id')) %>% 
    dplyr::mutate(grp_tgp_id = dplyr::recode(grp_tgp_id, 
                                             !!!setNames(ref_type_groupe_points$tgp_libelle,
                                                         ref_type_groupe_points$tgp_id)), 
                  tpe_libelle = dplyr::case_when(tpe_libelle == 'Passage' ~ paste0(tpe_libelle,'#',pas_numero),
                                                 tpe_libelle == 'Groupe de points' ~ grp_tgp_id,
                                                 TRUE ~ tpe_libelle)
    ) %>%
    dplyr::mutate(tpe_libelle = factor(tpe_libelle, 
                                       ordered = T, 
                                       levels = c("Points standards","Points compl\u00e9mentaires",
                                                  "Passage#1",'Passage#2','Passage#3','Ambiance'
                                       )))
  
}