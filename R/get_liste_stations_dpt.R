#' Obtenir la liste des stations pour un département
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#' @param dpt un vecteur de numéro de département
#'
#' @return un dataframe avec la liste des stations pour le département
#' @export
#' 
#' @importFrom dplyr filter select arrange
#'
#' @examples
#' \dontrun{
#' aspe_table_fiches <-
#' mef_creer_table_fiches()
#' 
#' get_liste_stations_dpt(df = aspe_table_fiches,
#' dpt = c("14", "27"))
#' }
#' 
get_liste_stations_dpt <- function(df, dpt)
{
  df %>%
    dplyr::filter(dept %in% {{dpt}}) %>%
    dplyr::select(code_sta_pp, sta_libelle_sandre, dept) %>% 
    unique() %>% 
    dplyr::arrange(dept, code_sta_pp)
}
