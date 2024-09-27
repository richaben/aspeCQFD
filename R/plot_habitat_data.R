#' Plot du graphique des données habitat pour une station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un graphique ggplot2
#' @export
#' 
#' @importFrom dplyr select filter
#' @importFrom ggiraph geom_point_interactive geom_label_interactive
#' @importFrom ggplot2 ggplot ylab xlab scale_x_date theme_bw theme element_text labs
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' aspe_table_fiches %>% 
#' dplyr::filter(code_sta_pp == "03231000_013") %>% 
#' plot_habitat_data()
#' }

plot_habitat_data <- function(df){
  
  df %>% 
    dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date) %>% 
    unique() %>% 
    mef_ajouter_habitat() %>% 
    dplyr::select(
      -ode_profondeur_moyenne_station, 
      -hab_tys_id_vegetation_aquatique) %>% 
    tidyr::pivot_longer(-c(code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date),
                 values_to = "value",
                 values_transform = list(value = as.character)) %>% 
    {ggplot2::ggplot(., aes(x = ope_date, y = name)) +
        ggiraph::geom_point_interactive(data = (. %>% dplyr::filter(is.na(value))), 
                            shape=4, 
                            size=2.5, 
                            col='red',
                            alpha = 0.8,
                            stroke = 1.5, 
                            show.legend = F,
                            aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                 "ope_date: ", ope_date),
                                data_id = ope_id)) +
        ggiraph::geom_label_interactive(
          aes(label = stringr::str_wrap(value,10), 
                                fill= value,
              tooltip = paste0("ope_id: ", ope_id,"<br>",
                               "ope_date: ", ope_date),
              data_id = ope_id), 
                            alpha=0.5, 
                            show.legend = F, 
                            size=2.5) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        #ggplot2::scale_x_continuous(breaks = unique(.$annee)) +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              limits = c(min(df$ope_date)-120,
                                         max(df$ope_date)+120)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(face = 'bold',size=9),
              title = ggplot2::element_text(face = 'bold', size=9),
              axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 8),
              plot.subtitle = ggtext::element_textbox_simple())+
        ggplot2::labs(title = 'D\u00e9tails donn\u00e9es Habitat:',
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))
    }  
  
}
