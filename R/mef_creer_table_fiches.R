#' Création d'un tableau de données ASPE pour les fiches de pêche
#'
#' @return un dataframe avec les informations utiles
#' @export
#' 
#' @importFrom aspe mef_creer_passerelle mef_ajouter_objectif mef_ajouter_ope_date mef_ajouter_intervenants mef_ajouter_dept mef_ajouter_type_protocole mef_ajouter_ope_env
#' @importFrom dplyr filter select starts_with left_join rename as_tibble recode
#' @importFrom stats setNames
#' 
#' @examples
#' \dontrun{
#' df <- mef_creer_table_fiches()
#' }
mef_creer_table_fiches <- function() {
  
  # creation passerelle
  aspe::mef_creer_passerelle() %>% 
    # ajout objectif peche + filtre sur reseaux
    aspe::mef_ajouter_objectif() %>%
    dplyr::filter(obj_libelle %in% c(
      "RCS \u2013 R\u00e9seau de Contr\u00f4le de Surveillance",
      "RHP \u2013 R\u00e9seau Hydrobiologique Piscicole",
      "RRP \u2013 R\u00e9seau de R\u00e9f\u00e9rence P\u00e9renne")) %>% 
    # ajout date
    aspe::mef_ajouter_ope_date() %>%
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
    
    # jointure operation peche + moyen prospection
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
    
    # jointure lot poissons
    dplyr::left_join(y = lot_poissons %>%
                       dplyr::select(lop_id, esp_id = lop_esp_id, lop_effectif)) %>%
    # jointure noms esp
    dplyr::left_join(y = ref_espece %>%
                       dplyr::select(esp_id, esp_code_alternatif,esp_nom_commun)) %>%
    
    # remove esp_id
    dplyr::select(-esp_id) %>%
    
    # ajout protocole
    aspe::mef_ajouter_type_protocole() %>% 
    
    # ajout donnees env. (ipr)
    aspe::mef_ajouter_ope_env() %>% 
    
    # ajout surface
    dplyr::left_join(operation %>% 
                       dplyr::select(ope_id, ope_surface_calculee, ope_eta_id, ope_niq_id)) %>%
    dplyr::mutate(ope_eta_id = dplyr::recode(ope_eta_id, 
                                             !!!setNames(ref_etat_avancement$eta_libelle, ref_etat_avancement$eta_id)),
                  ope_niq_id = dplyr::recode(ope_niq_id, 
                                             !!!setNames(ref_niveau_qualification$niq_libelle, ref_niveau_qualification$niq_id))) %>% 
    
    # ajout facies
    mef_ajouter_facies() %>% 
    
    dplyr::as_tibble()
}
