#' Création d'un tableau avec les données ASPE pour les paramètres de pêche et env sur les opérations
#'
#' @param df un dataframe avec les infos de peche et env sur les opérations
#'
#' @return un dataframe avec les infos de peche et env sur les opérations
#' @export
#' 
#' @importFrom aspe mef_ajouter_type_prelevement mef_ajouter_groupe_points
#' @importFrom dplyr select distinct left_join rename mutate recode
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches_ope_env <- 
#'   mef_creer_table_base_fiches() %>%
#'   mef_creer_table_ope_env_fiches()
#' }

mef_creer_table_ope_env <- function(df){
  df %>% 
    dplyr::select(code_sta_pp,
                  sta_id,
                  ope_id,
                  ope_date,
                  annee,
                  code_sandre_pp,
                  sta_libelle_sandre,
                  pro_libelle,
                  pre_id) %>% 
    dplyr::distinct()  %>%
    dplyr::left_join(y = operation_description_peche %>%
                       dplyr::rename(ope_id = odp_ope_id,
                                     mop_id = odp_mop_id)) %>%
    
    dplyr::mutate(odp_ted_id = dplyr::recode(odp_ted_id,
                                             !!!setNames(ref_tendance_debit$ted_libelle,
                                                         ref_tendance_debit$ted_id)),
                  odp_tur_id = dplyr::recode(odp_tur_id,
                                             !!!setNames(ref_turbidite$tur_libelle,
                                                         ref_turbidite$tur_id)),
                  odp_coh_id = dplyr::recode(odp_coh_id,
                                             !!!setNames(ref_condition_hydrologique$coh_libelle,
                                                         ref_condition_hydrologique$coh_id))) %>%
    dplyr::left_join(y = ref_moyen_prospection %>%
                       dplyr::select(mop_id, mop_libelle)) %>%
    dplyr::select(-mop_id) %>% 
    aspe::mef_ajouter_type_prelevement() %>% 
    aspe::mef_ajouter_groupe_points() %>%
    dplyr::left_join(passage, by = c('pre_id' = 'pas_id')) 
}

