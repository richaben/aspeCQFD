#' Ajoute les données de facies à un dataframe
#'
#' @param df un dataframe issu de la fonction [aspe::mef_creer_passerelle()]
#'
#' @return un dataframe avec les données de facies
#' @export
#' 
#' @importFrom dplyr left_join rename mutate recode
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' df <-
#'  aspe::mef_creer_passerelle() %>%
#'  mef_ajouter_facies()
#'  }
mef_ajouter_facies <- function(df) {
  
  tyf_recode <- setNames(ref_type_facies$tyf_libelle, ref_type_facies$tyf_id)
  granulo_recode <- setNames(ref_granulometrie$gra_libelle, ref_granulometrie$gra_id)
  veg_recode <- setNames(ref_vegetation_dominante$ved_libelle, ref_vegetation_dominante$ved_id)
  colmatage_recode <- setNames(ref_type_colmatage$tyc_libelle, ref_type_colmatage$tyc_id)
  
  df %>% 
    dplyr::left_join(facies %>% dplyr::rename(ope_id = fac_ode_ope_id), relationship = "many-to-many") %>% 
    dplyr::mutate(fac_tyf_id  = dplyr::recode(fac_tyf_id , !!!tyf_recode)) %>% 
    dplyr::mutate(fac_gra_id_dominante = dplyr::recode(fac_gra_id_dominante, !!!granulo_recode)) %>% 
    dplyr::mutate(fac_gra_id_accessoire = dplyr::recode(fac_gra_id_accessoire, !!!granulo_recode)) %>% 
    dplyr::mutate(fac_ved_id = dplyr::recode(fac_ved_id, !!!veg_recode)) %>% 
    dplyr::mutate(fac_tyc_id = dplyr::recode(fac_tyc_id, !!!colmatage_recode))
  
}