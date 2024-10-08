#' Générer une fiche de Contrôle Qualité des données pour une station
#'
#' @param stations un vecteur de code de station
#' @param df_data un dataframe avec les données nécessaires pour la fiche
#' @param dossier_sortie le dossier de sortie des fiches html
#' @param auteur le nom de l'auteur de la fiche
#' @param annee_debut l'année de début des données
#' @param annee_fin l'année de fin des données
#' @param interactive un booléen si les graphiques sont interactifs, FALSE par défaut
#'
#' @return un ou des fichiers html dans le dossier de sortie spécifié
#' @export
#' 
#' @importFrom furrr future_walk
#' @importFrom progressr with_progress progressor
#' @importFrom rmarkdown render
#' @importFrom cli cli_alert_info
#' 
#' @examples
#' \dontrun{
#' generate_fiche_cq_station(stations = c("03231000_013", "03231000_014"),
#' dossier_sortie = "D:/fiches_CQ/",
#' auteur = "Jean Dupont")
#' }
#' 

generate_fiche_cq_station <- function(stations,
                                      df_data,
                                      dossier_sortie = getwd(),
                                      auteur,
                                      annee_debut,
                                      annee_fin,
                                      interactive = FALSE){
  options(future.rng.onMisuse = "ignore")
  cli::cli_alert_info("Cr\u00e9ation des fiches CQ stations")
  
  aspe_table_fiches <- df_data[[1]]
  aspe_table_fiches_ope_env <- df_data[[2]]
  aspe_table_fiches_peuplement <- df_data[[3]]
  aspe_table_fiches_ipr <- df_data[[4]]
  aspe_table_fiches_ipr_env <- df_data[[5]]
  aspe_table_proba_esp_ipr <- df_data[[6]]

  progressr::with_progress({
    
    p <- progressr::progressor(steps = length(stations))
    
    furrr::future_walk(stations,
                       
                       ~ {
                         p()
                         rmarkdown::render(input = system.file(
                           "rmarkdown/templates/fiche-cq-station/skeleton/skeleton.Rmd",
                           package = "aspeCQFD"
                         ), 
                         output_file = paste0("fiche_CQ_donnees_station_", .x, ".html"),
                         output_dir = dossier_sortie,
                         quiet = TRUE,
                         params=list(code_station = .x,
                                     auteur = auteur,
                                     annee_debut = annee_debut,
                                     annee_fin = annee_fin,
                                     interactive = interactive))
                         
                       }
    )
  }
  )
}

