#' Obtenir la liste des opérations sur une station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#' @param code_sta_pp le numéro 'code station et point de prélèvement'
#'
#' @return un dataframe avec la liste des opérations pour la station
#' @export
#' 
#' @importFrom dplyr filter select arrange mutate
#' 
#' @examples
#' \dontrun{
#' 
#' aspe_table_fiches <-
#' mef_creer_table_fiches()
#' 
#' get_liste_ope_station(df = aspe_table_fiches, 
#' code_sta_pp = "03231000_013")
#' }
#' 

get_liste_ope_station <- function(df, code_sta_pp)
{
  df %>%
    dplyr::filter(code_sta_pp %in% {{code_sta_pp}}) %>%
    dplyr::select(code_sta_pp, 
                  ope_id, 
                  ope_date, 
                  operateur_peche, 
                  ope_eta_id, 
                  ope_commentaire,
                  dept) %>% 
    dplyr::mutate(ope_date = as.Date(ope_date, format = "%Y-%m-%d")) %>% 
    dplyr::arrange(dept, code_sta_pp, ope_date) %>% 
    unique()
}
