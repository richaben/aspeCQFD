#' Selectionne les informations contextuelles pour fiche station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un dataframe avec les informations contextuelles pour la fiche station
#' @export
#' 
#' @importFrom dplyr select
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches %>% 
#' dplyr::filter(code_sta_pp == "03231000_013") %>% 
#' select_infos_context()
#' }
#' 
select_infos_context <- function(df) {
  df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_id,
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
                  ope_espece_ciblee) %>%
    unique()
}

