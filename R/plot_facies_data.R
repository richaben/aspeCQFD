#' Plot du graphique des données faciès pour une station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un graphique ggplot2
#' @export
#' 
#' @importFrom dplyr select distinct mutate case_when filter
#' @importFrom forcats fct_rev
#' @importFrom ggiraph geom_point_interactive geom_label_interactive geom_bar_interactive
#' @importFrom ggplot2 ggplot scale_shape_manual scale_color_manual ylab xlab scale_x_date theme_bw theme element_text labs facet_wrap
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches %>% 
#'   dplyr::filter(code_sta_pp == "03231000_013") %>% 
#'   plot_facies_data()
#' }
#' 
plot_facies_data <- function(df){
  
  df <-
    df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_date,
                  fac_tyf_id,
                  fac_importance_relative,
                  fac_profondeur_moyenne,
                  fac_gra_id_dominante,
                  fac_gra_id_accessoire,
                  fac_tyc_id,
                  fac_ved_id,
                  fac_recouvrement_vegetation) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(fac_tyf_id = factor(fac_tyf_id, 
                                      levels = c('Courant','Plat','Profond'), 
                                      ordered = T),
                  pch_tyf = dplyr::case_when(is.na(fac_tyf_id)~ '4',
                                             TRUE ~ '21'))
  
  facies_part1 <-
    df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_date,
                  fac_tyf_id, 
                  pch_tyf) %>% 
    {ggplot2::ggplot(., aes(x = ope_date, 
                            y = forcats::fct_rev(fac_tyf_id), 
                            shape = pch_tyf, 
                            col=pch_tyf)) +
        ggiraph::geom_point_interactive(size=3.5, 
                            show.legend = F, 
                            fill= '#99B2B7', 
                            stroke= 1,
                            aes(
                              tooltip = paste0("ope_id: ", ope_id,"<br>",
                                               "ope_date: ", ope_date),
                              data_id = ope_id
                            )) +
        ggplot2::scale_shape_manual(values = c(21,4)) +
        ggplot2::scale_color_manual(values = c('black','red'))+
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
          plot.subtitle = ggtext::element_textbox_simple()
        )+
        ggplot2::labs(title = "Donn\u00e9es Faci\u00e8s:", 
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))}
  
  facies_part2 <-
    df %>% 
    dplyr::select(-fac_importance_relative) %>% 
    tidyr::pivot_longer(-c(code_sta_pp, 
                           sta_libelle_sandre, 
                           ope_id, 
                           annee,
                           ope_date,
                           fac_tyf_id, 
                           pch_tyf),
                        values_to = "value",
                        values_transform = list(value = as.character)) %>% 
    dplyr::filter(!is.na(fac_tyf_id)) %>% 
    {ggplot2::ggplot(., aes(x = ope_date, y = name)) +
        ggiraph::geom_point_interactive(data = (. %>% dplyr::filter(is.na(value))), 
                            shape=4, 
                            size=2.5, 
                            col='black',
                            alpha = 0.8,
                            stroke = 1.5, 
                            show.legend = F,
                            aes(
                              tooltip = paste0("ope_id: ", ope_id,"<br>",
                                               "ope_date: ", ope_date),
                              data_id = ope_id
                            )) +
        ggiraph::geom_label_interactive(aes(label = stringr::str_wrap(value, 20), fill= value,
                                
                                  tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                   "ope_date: ", ope_date),
                                  data_id = ope_id
                                ), 
                            alpha=0.5, 
                            show.legend = F, 
                            size=2.3) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        #ggplot2::scale_x_continuous(breaks = unique(.$annee)) +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              limits = c(min(df$ope_date)-120,
                                         max(df$ope_date)+120)) +
        ggplot2::facet_wrap(.~fac_tyf_id, nrow=3) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(face = 'bold'),
          axis.text.y = ggplot2::element_text(face = 'bold',size=9),
          title = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 8),
          plot.subtitle = ggtext::element_textbox_simple()) +
        ggplot2::labs(title = glue::glue('D\u00e9tails donn\u00e9es Faci\u00e8s'),
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))
    }
  
  facies_importance_plot <-
    df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_date,
                  fac_tyf_id, 
                  pch_tyf, 
                  fac_importance_relative) %>% 
    {ggplot2::ggplot(., aes(x = ope_date, 
                            y = fac_importance_relative, 
                            fill = fac_tyf_id) ) +
        ggiraph::geom_bar_interactive(
          stat="identity", 
          col='black', 
          linewidth = 0.2, 
          alpha = 0.8,
          aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                               "ope_date: ", ope_date),
              data_id = ope_id)) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        #ggplot2::scale_x_continuous(breaks = unique(.$annee)) +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              limits = c(min(df$ope_date)-120,
                                         max(df$ope_date)+120)) +
        ggplot2::labs(title = glue::glue('Importance relative Faci\u00e8s (%)'),
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'),
                      fill = 'Type') +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = 'bold',size=9),
                       title = ggplot2::element_text(face = 'bold', size=9),
                       axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 8),
                       legend.position = 'bottom',
                       plot.subtitle = ggtext::element_textbox_simple()
                       )
    }
  
  patchwork::wrap_plots(
    facies_part1, facies_part2, facies_importance_plot,
    ncol = 1
  ) + 
    patchwork::plot_layout(heights = c(0.7,3,1)) 
  
}
