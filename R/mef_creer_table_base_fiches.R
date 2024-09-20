#' Création d'un tableau de base avec les infos ASPE pour les fiches
#'
#' @description
#' La fonction permet de constituer un tableau de base nécessaire pour la création des fiches.
#' Les données ASPE doivent être chargées dans l'environnement au préalable.
#' Seules les stations appartenant aux réseaux RCS, RHP et RRP sont conservées. 
#' 
#' @return un dataframe avec les informations utiles
#' @export
#' 
#' @importFrom aspe mef_creer_passerelle mef_ajouter_objectif mef_ajouter_ope_date mef_ajouter_intervenants mef_ajouter_dept
#' @importFrom dplyr filter select starts_with as_tibble mutate
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches <- 
#'   mef_creer_table_base_fiches()
#' }

mef_creer_table_base_fiches <- function() {
  aspe::mef_creer_passerelle() %>% 
    # ajout objectif peche + filtre sur reseaux
    aspe::mef_ajouter_objectif() %>%
    dplyr::filter(obj_libelle %in% c(
      "RCS \u2013 R\u00e9seau de Contr\u00f4le de Surveillance",
      "RHP \u2013 R\u00e9seau Hydrobiologique Piscicole",
      "RRP \u2013 R\u00e9seau de R\u00e9f\u00e9rence P\u00e9renne")) %>% 
    # ajout date
    aspe::mef_ajouter_ope_date() %>%
    dplyr::mutate(ope_date = as.Date(ope_date)) %>% 
    # ajout intervenant
    aspe::mef_ajouter_intervenants() %>% 
    dplyr::select(- operateur_ofb, 
                  - commanditaire_ofb, 
                  - validation_ofb, 
                  - dplyr::starts_with('ope_int_id')) %>% 
    
    # ajout codes sandre et pp
    mef_ajouter_codes_sandre_pp() %>% 
    
    # ajout departement 
    aspe::mef_ajouter_dept() %>% 
    
    # ajout infos operation
    mef_ajouter_infos_context() %>% 
    
    # ajout protocole
    aspe::mef_ajouter_type_protocole() %>% 
    
    # format tibble
    dplyr::as_tibble()
  
}
