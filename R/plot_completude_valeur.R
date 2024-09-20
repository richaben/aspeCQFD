#' Plot le graphique de completude des valeurs pour une station et liste de paramètres
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()],
#' avec une liste de paramètres à inspecter
#'
#' @return un graphique ggplot2
#' @export
#'
#' @importFrom dplyr filter distinct
#' @importFrom ggplot2 ggplot geom_point scale_x_continuous ylab xlab theme_bw theme element_text labs
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom tidyr pivot_longer
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches %>%
#'   dplyr::filter(code_sta_pp == "03231000_013") %>%
#'   dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee, pre_id,
#'                 odp_longueur,
#'                 odp_largeur_lame_eau,
#'                 profondeur,
#'                 ope_surface_calculee) %>%
#'   plot_completude_valeur()
#' }
#' 

plot_completude_valeur <- function(df){
  df %>% 
    dplyr::distinct() %>% 
    tidyr::pivot_longer(cols = -c(code_sta_pp, sta_libelle_sandre, ope_id, annee, pre_id),
                 values_to = "value",
                 values_transform = list(value = as.character)) %>% 
    #dplyr::mutate(annee = as.factor(annee)) %>% 
    {ggplot2::ggplot(., aes(y = name, x = annee)) +
        ggplot2::geom_point(data = (. %>% dplyr::filter(is.na(value))), 
                            shape=4, 
                            size=2.8, 
                            col='red',
                            stroke = 2, 
                            show.legend =  F) +
        ggplot2::geom_point(data = (. %>% dplyr::filter(!is.na(value))), 
                            shape=21, 
                            fill= '#99B2B7', 
                            size=2.5, 
                            col='black', 
                            show.legend =  F) +
        ggplot2::scale_x_continuous(breaks = unique(.$annee)) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = 'bold',size=9),
              title = ggplot2::element_text(face = 'bold', size= 9),
              axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
              plot.subtitle = ggtext::element_textbox_simple()) +
        ggplot2::labs(title = 'Infos description p\u00eache + r\u00e9glages:',
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))
    }
}