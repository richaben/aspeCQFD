#' Générer une fiche de Contrôle Qualité des données pour une station
#'
#' @param stations un vecteur de code de station
#' @param dossier_sortie le dossier de sortie des fiches html
#' @param auteur le nom de l'auteur de la fiche
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
                                      dossier_sortie = getwd(),
                                      auteur){
  
  cli::cli_alert_info("Cr\u00e9ation des fiches CQ stations")
  
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
                                     auteur = auteur))
                         
                       }
    )
  }
  )
}

