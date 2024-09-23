#' Créer un tableau avec les probas IPR pour les espèces dans les stations
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un dataframe avec les informations pour les probabilités IPR
#' @export
#'
#' @importFrom dplyr select distinct left_join mutate
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches %>% 
#'   dplyr::filter(code_sta_pp == "03174000_005") %>% 
#'   mef_tab_proba_esp_ipr()
#' }

mef_tab_proba_esp_ipr <- function(df){
  
  df %>% 
    dplyr::select(
      code_sta_pp, 
      sta_libelle_sandre, 
      ope_id, 
      annee, 
      ope_date) %>% 
    dplyr::distinct() %>%
    dplyr::left_join(probabilite_presence_ipr, by = c("ope_id" = "ppi_opi_ope_id")) %>%
    dplyr::left_join(y = ref_espece %>%
                       dplyr::select(esp_id,
                                     esp_code_alternatif,
                                     esp_nom_commun), by = c("ppi_esp_id" = "esp_id")) %>% 
    dplyr::mutate(presence = 
                    ifelse(test = 
                             (ppi_param_effectif > 0), 
                           yes = "Pr\u00e9sence", no = "Absence")
    )

}

