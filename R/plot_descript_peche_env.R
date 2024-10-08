#' Plot descriptif des paramètres de pêche et environnementaux
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un graphique ggplot2
#' @export
#'
#' @importFrom dplyr select mutate
#' @importFrom ggiraph geom_bar_interactive geom_point_interactive geom_label_interactive
#' @importFrom ggplot2 ggplot facet_wrap ylab xlab theme_bw scale_x_date theme element_text expansion
#' @importFrom ggtext element_textbox_simple
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches %>%
#'   dplyr::filter(code_sta_pp == "03231000_013") %>%
#'   plot_descript_peche_env()
#'   }


plot_descript_peche_env <- function(df){
  # plot1_inspection_valeurs
  plot1a <-
   df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_date,
                  pre_id,
                  odp_longueur,
                  odp_largeur_lame_eau,
                  profondeur,
                  ope_surface_calculee) %>% 
    plot_completude_valeur()
  
  # plot2_inspection_valeurs
  plot2a <-
    df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_date,
                  pre_id,
                  odp_puissance,
                  odp_tension,
                  odp_intensite,
                  odp_nombre_anodes,
                  odp_nombre_epuisettes,
                  odp_maille_epuisette) %>% 
    plot_completude_valeur()
  
  part_val_a <- 
    patchwork::wrap_plots(plot1a, plot2a, ncol = 1)
  
  ## description parametres
  param_descript1 <-
    df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee, 
                  ope_date,
                  odp_longueur,
                  odp_largeur_lame_eau,
                  profondeur,
                  ope_surface_calculee) %>% 
    unique() %>% 
    tidyr::pivot_longer(cols = -c(code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date)) %>% 
    dplyr::mutate(name = gsub(name, pattern = 'odp_', replacement = ''),
           name = gsub(name, pattern = '_', replacement = ' ')) %>% 
    dplyr::mutate(#annee = as.factor(annee),
           name = stringr::str_wrap(gsub(replacement = " ", pattern = '_', name), 20)) %>% 
    {ggplot2::ggplot(., aes(y = value, x = ope_date)) + 
        ggiraph::geom_bar_interactive(col='black', 
                          linewidth= 0.2, 
                          fill = '#79BD9A', 
                          stat="identity",
                          aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                               "ope_date: ", ope_date),
                              data_id = ope_id)) +
        #scale_y_continuous(breaks = integer_breaks()) +
        ggplot2::facet_wrap(~name, scales = 'free_y', ncol = 2) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        # ggplot2::scale_x_continuous(breaks = unique(.$annee), 
        #                             minor_breaks = 1) +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              expand = ggplot2::expansion(mult = .05)) + 
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(face = 'bold', size = 9),
          axis.text.y = ggplot2::element_text(face = 'bold',size=9),
          title = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 7),
          plot.subtitle = ggtext::element_textbox_simple()
          )
      }
  
  param_descript2 <-
    df %>% 
    dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date,
                  odp_puissance,
                  odp_tension,
                  odp_intensite,
                  odp_nombre_anodes,
                  odp_nombre_epuisettes,
                  odp_maille_epuisette) %>% 
    unique() %>% 
    tidyr::pivot_longer(cols = -c(code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date)) %>% 
    dplyr::mutate(name = gsub(name, pattern = 'odp_', replacement = ''),
                  name = gsub(name, pattern = '_', replacement = ' ')) %>% 
    dplyr::mutate(
      name = stringr::str_wrap(gsub(replacement = " ", pattern = '_', name), 20)) %>% 
    {ggplot2::ggplot(., aes(y = value, x = ope_date)) + 
        ggiraph::geom_bar_interactive(col='black', 
                          linewidth= 0.2, 
                          fill = '#61a5c2', 
                          stat="identity",
                          aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                               "ope_date: ", ope_date),
                              data_id = ope_id)
                          ) +
        ggplot2::facet_wrap(~name, scales = 'free_y', ncol = 2) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        # ggplot2::scale_x_continuous(breaks = unique(.$annee), 
        #                             minor_breaks = 1) +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              expand = ggplot2::expansion(mult = .05)) + 
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(face = 'bold', size = 9),
          axis.text.y = ggplot2::element_text(face = 'bold',size=9),
          title = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 7),
          plot.subtitle = ggtext::element_textbox_simple()
        )
    }
  
  param_descript3 <-
    df %>% 
    dplyr::select(
                  code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date,
                  odp_ted_id,
                  odp_tur_id,
                  odp_coh_id) %>% 
    unique() %>% 
    tidyr::pivot_longer(cols = -c(code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date)) %>% 
    mutate(value = stringr::str_wrap(value, 10)) %>% 
    {
      ggplot2::ggplot(., aes(y = name, x = ope_date)) + 
        ggiraph::geom_point_interactive(data = (. %>% filter(is.na(value))), 
                                        shape=4, size=2.8, col='red',stroke = 2, show.legend =  F,
                                        aes(tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                             "ope_date: ", ope_date),
                                            data_id = ope_id)) +
        ggiraph::geom_label_interactive(aes(label = value, fill=value,
                                            tooltip = paste0("ope_id: ", ope_id,"<br>",
                                                             "ope_date: ", ope_date),
                                            data_id = ope_id), alpha=0.5, show.legend = F, size=2.5) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        # ggplot2::scale_x_continuous(breaks = unique(.$annee), 
        #                             minor_breaks = 1) +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              expand = ggplot2::expansion(mult = .05)) + 
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(face = 'bold',size=9),
          title = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 7),
          plot.subtitle = ggtext::element_textbox_simple()
        )
    }
  
  part_val_b <-
  patchwork::wrap_plots(param_descript1, param_descript2, ncol = 1)
  
  patchwork::wrap_plots(
    patchwork::wrap_plots(part_val_a, part_val_b, ncol = 2) ,
    param_descript3, ncol = 1) +
    patchwork::plot_layout(heights = c(2, 0.5))
  
}

