#' Plot les infos contextuelles des opérations pour une station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un graphique ggplot2
#' @export
#' 
#' @importFrom dplyr mutate
#' @importFrom ggiraph geom_point_interactive geom_label_interactive
#' @importFrom ggplot2 ylab xlab theme_bw scale_x_date theme element_text labs
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' aspe_table_station1 <- 
#'   mef_creer_table_fiches() %>% 
#'   dplyr::filter(code_sta_pp == "03174000_005") %>% 
#'   dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee) %>% 
#'   mef_ajouter_infos_context() %>% 
#'   unique()
#'   
#'   plot_infos_context(aspe_table_station1)
#' }
#' 

plot_infos_context <- function(df){
  
  df %>% 
    unique() %>% 
    tidyr::pivot_longer(cols = -c(code_sta_pp, 
                                  sta_libelle_sandre, 
                                  ope_id, 
                                  ope_date,
                                  annee),
                        values_to = "value",
                        values_transform = list(value = as.character)) %>% 
    dplyr::mutate(
      name = stringr::str_wrap(gsub(replacement = " ", pattern = '_', name), 25)) %>% 
    {ggplot(., aes(y = name, x = ope_date)) +
        ggiraph::geom_point_interactive(data = (. %>% filter(is.na(value))), 
                                        shape=4, 
                                        size=3, 
                                        col='red',
                                        stroke = 2, 
                                        show.legend =  F,
                                        aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                             "ope_date: ", ope_date),
                                            data_id = ope_id)) +
        ggiraph::geom_label_interactive(aes(label = value, 
                                            fill=value,
                                            tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                             "ope_date: ", ope_date),
                                            data_id = ope_id), 
                                        alpha=0.5, 
                                        show.legend = F, 
                                        size=3.1) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab("Ann\u00e9es") +
        ggplot2::theme_bw() +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y"
        ) + 
        # ggplot2::scale_x_continuous(breaks = unique(.$annee), 
        #                             minor_breaks = 1) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(face = 'bold',size=10),
          title = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
          plot.subtitle = ggtext::element_textbox_simple()
        ) +
        ggplot2::labs(title = 'Informations contextuelles:',
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))
    }
  
}


