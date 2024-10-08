#' Plot le graphique de completude des valeurs pour une station et liste de paramètres
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()],
#' avec une liste de paramètres à inspecter
#'
#' @return un graphique ggplot2
#' @export
#' 
#' @importFrom dplyr distinct filter
#' @importFrom ggiraph geom_point_interactive
#' @importFrom ggplot2 ggplot scale_x_date ylab xlab theme_bw theme element_text labs expansion
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
    tidyr::pivot_longer(cols = -c(code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date, pre_id),
                 values_to = "value",
                 values_transform = list(value = as.character)) %>% 
    #dplyr::mutate(annee = as.factor(annee)) %>% 
    {ggplot2::ggplot(., aes(y = name, x = ope_date)) +
        ggiraph::geom_point_interactive(data = (. %>% dplyr::filter(is.na(value))), 
                            shape=4, 
                            size=2.8, 
                            col='red',
                            stroke = 1.5,
                            alpha = 0.8,
                            show.legend = F,
                            aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                 "ope_date: ", ope_date),
                                data_id = ope_id)) +
        ggiraph::geom_point_interactive(data = (. %>% dplyr::filter(!is.na(value))), 
                            shape=21, 
                            fill= '#99B2B7', 
                            size=2.5, 
                            alpha = 0.8,
                            col='black', 
                            show.legend = F,
                            aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                 "ope_date: ", ope_date),
                                data_id = ope_id)) +
        #ggplot2::scale_x_continuous(breaks = unique(.$annee)) +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              expand = ggplot2::expansion(mult = .05)) + 
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