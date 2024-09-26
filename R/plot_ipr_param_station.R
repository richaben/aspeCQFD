#' Plot le graphique des paramètres IPR (présences et variations)
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()] 
#' et [aspeCQFD::mef_tab_proba_esp_ipr()]
#'
#' @return un graphique ggplot2
#' @export
#'
#' @importFrom dplyr select distinct filter
#' @importFrom ggiraph geom_point_interactive geom_line_interactive
#' @importFrom ggplot2 ggplot scale_x_date ylab xlab theme_bw theme element_text labs facet_wrap expansion element_blank
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom tidyr pivot_longer
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches_ipr_env %>%
#'   dplyr::filter(code_sta_pp == "03174000_005") %>%
#'   plot_ipr_param_station()
#'   }

plot_ipr_param_station <- function(df){
  
  df <-
    df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_date,
                  pop_id,
                  distance_mer:temp_janvier) %>% 
    dplyr::distinct() 
  
  
  part1 <-
    df %>% 
    tidyr::pivot_longer(c(distance_mer:temp_janvier)) %>%  
    {ggplot2::ggplot(., aes(y = name, x = ope_date)) +
        ggiraph::geom_point_interactive(data = (. %>% dplyr::filter(is.na(value))), 
                                        shape=4, 
                                        size=2.8, 
                                        col='red',
                                        stroke = 1.5,
                                        alpha = 0.8,
                                        show.legend =  F,
                                        aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                             ope_date),
                                            data_id = ope_id)) +
        ggiraph::geom_point_interactive(data = (. %>% dplyr::filter(!is.na(value))), 
                                        shape=21, 
                                        fill= '#99B2B7', 
                                        size=2.5, 
                                        alpha = 0.8,
                                        col='black', 
                                        show.legend =  F,
                                        aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                             ope_date),
                                            data_id = ope_id)
        ) +
        #ggplot2::scale_x_continuous(breaks = unique(.$annee)) +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y") + 
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = 'bold',size=9),
                       title = ggplot2::element_text(face = 'bold', size= 9),
                       axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                       plot.subtitle = ggtext::element_textbox_simple()) +
        ggplot2::labs(title = "Pr\u00e9sence des param\u00e8tres env. pour l\'IPR:",
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))
    }
  
  
  part2 <- 
    df %>% 
    tidyr::pivot_longer(c(distance_mer:temp_janvier)) %>%
    {
      ggplot2::ggplot(.,  aes(x = ope_date,
                              y = value)) +
        ggiraph::geom_line_interactive() +
        ggiraph::geom_point_interactive(pch = 21, size = 2, fill = '#ffc8dd',
                                        aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                             ope_date),
                                            data_id = ope_id)) +
        ggplot2::facet_wrap(~name,
                            scales = "free_y") +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::scale_x_date(date_breaks = "1 year",
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              expand = ggplot2::expansion(mult = .1)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title=ggplot2::element_blank(),
                       strip.text.x = ggplot2::element_text(face = 'bold'),
                       axis.text.y = ggplot2::element_text(face = 'bold',size=8),
                       title = ggplot2::element_text(face = 'bold', size=8),
                       axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size=7),
                       legend.position = 'bottom',
                       plot.subtitle = ggtext::element_textbox_simple()) +
        ggplot2::labs(title = "Variations des param\u00e8tres env. pour l\'IPR:",
                      subtitle = glue::glue("{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})"))
      
    }
  
  patchwork::wrap_plots(part1, part2, ncol = 1) +
    patchwork::plot_layout(heights = c(0.5, 0.8))
}



