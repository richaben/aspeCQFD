#' Ajoute les informations contextuelles issues de ASPE
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un dataframe avec les infos contextuelles sur les opérations de pêche
#' @export
#'
#' @importFrom dplyr left_join select mutate recode
#' @importFrom stringr str_wrap
#' 
#' @examples
#' \dontrun{
#' aspe_table_station1 <- 
#'   mef_creer_table_fiches() %>% 
#'   dplyr::filter(code_sta_pp == "03174000_005") %>% 
#'   dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee) %>% 
#'   mef_ajouter_infos_context() %>% 
#'   unique()
#' }

mef_ajouter_infos_context <- function(df){
  
  categ_recode <- 
    setNames(
      stringr::str_wrap(
        ref_categorie_piscicole$cap_libelle_sandre, 5), 
      ref_categorie_piscicole$cap_id)
  
  
  df %>% 
    dplyr::left_join(operation %>% 
                       dplyr::select(ope_id,
                                     #ope_date,
                                     ope_pente_ligne_eau,
                                     ope_section_mouillee,
                                     ope_durete_totale,
                                     ope_temp_max_moyenne_eau,
                                     ope_niveau_typologique_theorique,
                                     ope_cap_id,
                                     ope_temp_air_bassin_versant_cerema,
                                     ope_precipitation_bassin_versant_cerema,
                                     ope_amplitude_thermique_air_station_cerema,
                                     ope_temperature_air_station_cerema,
                                     ope_espece_ciblee)) %>% 
    dplyr::mutate(ope_cap_id = dplyr::recode(ope_cap_id, !!! categ_recode))
  
}
