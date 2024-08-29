#' Ajoute à un dataframe aspe les codes SANDRE et points de prélèvement 
#'
#' @param df un dataframe aspe issu de la fonction [aspe::mef_creer_passerelle()]
#'
#' @return un dataframe avec les infos de coordonnées stations et le code combinant
#'  code sandre et points de prélèvement `code_sta_pp`
#' @export
#'
#' @importFrom dplyr left_join select rename mutate case_when
#' @importFrom stringr str_length
#'
#' @examples
#'\dontrun{
#' df <- 
#'   aspe::mef_creer_passerelle() %>% 
#'   mef_ajouter_codes_sandre_pp(df)
#' }
#'  

mef_ajouter_codes_sandre_pp <- function(df)
  
{
  df %>%
    dplyr::left_join(y = station %>%
                       dplyr::select(sta_id, 
                                     sta_code_sandre, 
                                     sta_libelle_sandre, 
                                     sta_coordonnees_x, 
                                     sta_coordonnees_y), 
                     by = "sta_id") %>% 
    dplyr::left_join(point_prelevement %>% 
                       dplyr::rename(code_sandre_pp = pop_code_sandre) %>%
                       dplyr::select(pop_id, code_sandre_pp), 
                     by = "pop_id") %>% 
    dplyr::mutate(code_sandre_pp = dplyr::case_when(
      stringr::str_length(code_sandre_pp) == 3 & !is.na(code_sandre_pp) ~ as.character(code_sandre_pp),
      stringr::str_length(code_sandre_pp) == 2 & !is.na(code_sandre_pp) ~ paste0("0", code_sandre_pp),
      stringr::str_length(code_sandre_pp) == 1 & !is.na(code_sandre_pp) ~ paste0("00", code_sandre_pp),
      TRUE ~ as.character(code_sandre_pp)),
      code_sta_pp = paste0(sta_code_sandre, "_", code_sandre_pp)
      )
}
