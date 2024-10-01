#' Fonction unique pour créer un ensemble de tables nécessaires à la création des fiches CQ stations
#'
#' @return une liste de dataframe au format tibble
#' @export
#' 
#' @importFrom aspe mef_ajouter_metriques mef_ajouter_ope_env
#' @importFrom cli cli_h1 cli_progress_step
#' @importFrom dplyr select distinct
#' @importFrom tibble lst
#' 
#' @examples
#' \dontrun{
#' data_fichesCQ <-
#' mef_creer_tables_fichesCQ()
#' }

mef_creer_ttes_tables_fichesCQ <- function(){
  
  cli::cli_h1("Cr\u00e9ation des donn\u00e9es pour les fiches CQ stations")
  
  cli::cli_progress_step("table de base")
  
  aspe_table_fiches <-
    mef_creer_table_base_fiches() %>% 
    suppressMessages()
  
  cli::cli_progress_step("table param. env.")
  
  aspe_table_fiches_ope_env <-
    aspe_table_fiches %>% 
    mef_creer_table_ope_env() %>% 
    mef_ajouter_facies() %>% 
    suppressMessages()
  
  cli::cli_progress_step("table peuplement")
  
  aspe_table_fiches_peuplement <-
    aspe_table_fiches %>%
    mef_creer_table_peuplement()%>% 
    suppressMessages()
  
  cli::cli_progress_step("table ipr stations")
  
  aspe_table_fiches_ipr <-
    aspe_table_fiches_peuplement %>%
    dplyr::select(code_sta_pp,
                  sta_libelle_sandre,
                  ope_id,
                  annee,
                  ope_date,
                  ipr,
                  cli_id,
                  cli_libelle) %>%
    dplyr::distinct() %>%
    aspe::mef_ajouter_metriques()%>% 
    suppressMessages()
  
  cli::cli_progress_step("table ipr env.")
  
  aspe_table_fiches_ipr_env <-
    aspe_table_fiches %>%
    dplyr::select(code_sta_pp,
                  sta_libelle_sandre,
                  ope_id,
                  annee,
                  ope_date,
                  pop_id) %>%
    dplyr::distinct() %>%
    aspe::mef_ajouter_ope_env()%>% 
    suppressMessages()
  
  cli::cli_progress_step("table ipr esp.")
  
  aspe_table_proba_esp_ipr <-
    aspe_table_fiches %>%
    mef_tab_proba_esp_ipr()%>% 
    suppressMessages()
  
  
  tibble::lst(aspe_table_fiches, 
              aspe_table_fiches_ope_env,
              aspe_table_fiches_peuplement,
              aspe_table_fiches_ipr,
              aspe_table_fiches_ipr_env,
              aspe_table_proba_esp_ipr
  )
  
}