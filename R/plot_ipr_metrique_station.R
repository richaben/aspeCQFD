#' Plot un graphique avec les métriques IPR théoriques et observés pour une station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_peuplement()]
#'
#' @return un graphique ggplot2
#' @export
#'
#' @importFrom dplyr select contains
#' @importFrom ggiraph geom_bar_interactive
#' @importFrom ggplot2 ggplot position_dodge facet_wrap ylab xlab scale_x_date expansion theme_bw theme element_text labs
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom tidyr pivot_longer separate_wider_delim
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches_ipr %>% 
#' dplyr::filter(code_sta_pp == "03231000_013") %>% 
#' plot_ipr_metrique()
#' }
#' 

plot_ipr_metrique <- function(df){
  df %>% 
    dplyr::select(
      code_sta_pp, 
      sta_libelle_sandre, 
      ope_id, 
      annee, 
      ope_date,
      dplyr::contains("_observe"),
      dplyr::contains("_theorique")) %>% 
    tidyr::pivot_longer(-c(code_sta_pp, 
                           sta_libelle_sandre, 
                           ope_id, 
                           annee,
                           ope_date),
                        names_to = "metrique",
                        values_to = "valeur") %>% 
    tidyr::separate_wider_delim(metrique, "_", names = c("metrique", "type")) %>% 
    {ggplot2::ggplot(data=., aes(x = ope_date, 
                                 y = valeur, 
                                 fill=type, 
                                 group = type,
                                 )) +
        ggiraph::geom_bar_interactive(stat = 'identity',
                          position = ggplot2::position_dodge(), 
                          col='black',
                          linewidth = 0.1, 
                          alpha=0.8,
                          aes(tooltip = paste0("ope_id: ", ope_id, "<br>",
                                               "ope_date: ", ope_date, "<br>",
                                               "type: ", type),
                              data_id = paste(ope_id, type))) +
        ggplot2::facet_wrap(.~metrique, scales = 'free_y') +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::scale_x_date(date_breaks = "1 year",
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              expand = ggplot2::expansion(mult = .05)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(face = 'bold'),
          axis.text.y = ggplot2::element_text(face = 'bold',size=9),
          title = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
          legend.position = 'bottom',
          plot.subtitle = ggtext::element_textbox_simple()) +
        ggplot2::labs(title = "M\u00e9triques IPR:",
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))
    }
}





