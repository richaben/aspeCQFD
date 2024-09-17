#' Ajoute les données habitats à un dataframe avec les données aspe
#'
#' @param df un dataframe aspe issu de la fonction [aspe::mef_creer_passerelle()]
#' ou [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un dataframe avec les données habitats
#' @export
#'
#' @importFrom dplyr left_join select mutate recode
#' 
#' @examples
#' \dontrun{
#' df <- 
#'   aspe::mef_creer_passerelle() %>% 
#'   mef_ajouter_habitat()
#' }
#' 
mef_ajouter_habitat <- function(df){
  
  sin_recode <- setNames(ref_sinuosite$sin_libelle, 
                         ref_sinuosite$sin_id)
  omr_recode <- setNames(ref_ombrage_riviere$omr_libelle, 
                         ref_ombrage_riviere$omr_id)
  tya_recode <- setNames(ref_type_abondance$tya_libelle, 
                         ref_type_abondance$tya_id)
  tys_recode <- setNames(ref_type_abondance_saumon$tys_libelle, 
                         ref_type_abondance_saumon$tys_id)
  
  df %>% 
    dplyr::left_join(operation_donnees_environnementales %>% 
                       dplyr::select(ode_profondeur_moyenne_station,
                                     ode_observation_vegetation,
                                     ode_observation_vegetation,
                                     ode_hab_id,
                                     ode_ope_id), 
                     by = c('ope_id' = 'ode_ope_id')) %>% 
    dplyr::left_join(habitat, by = c('ode_hab_id' = 'hab_id')) %>% 
    dplyr::select(-ode_hab_id) %>% 
    dplyr::mutate(hab_sin_id = dplyr::recode(hab_sin_id, !!!sin_recode),
                  hab_omr_id = dplyr::recode(hab_omr_id, !!!omr_recode),
                  hab_tya_id_trous_fosses = dplyr::recode(hab_tya_id_trous_fosses, !!!tya_recode), 
                  hab_tya_id_sous_berges = dplyr::recode(hab_tya_id_sous_berges, !!!tya_recode),
                  hab_tya_id_abris_rocheux = dplyr::recode(hab_tya_id_abris_rocheux, !!!tya_recode),
                  hab_tya_id_embacles_souches = dplyr::recode(hab_tya_id_embacles_souches, !!!tya_recode),
                  hab_tya_id_abri_vegetal_aquatique = dplyr::recode(hab_tya_id_abri_vegetal_aquatique, !!!tya_recode),
                  hab_tya_id_vegetation_bordure = dplyr::recode(hab_tya_id_vegetation_bordure, !!!tya_recode),
                  hab_tys_id_vegetation_aquatique = dplyr::recode(hab_tys_id_vegetation_aquatique, !!!tys_recode))
}