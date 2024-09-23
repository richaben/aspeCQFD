#' Plot du graphique du peuplement par espèce et par année pour une station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_peuplement()]
#'
#' @return un graphique ggplot2 représentant le nombre d'individus par espèce, par année et type de points
#' @export
#' 
#' @importFrom dplyr group_by summarise
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot expansion geom_bar facet_wrap scale_y_continuous ylab xlab scale_x_date theme_bw scale_fill_manual theme element_blank element_text labs
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' aspe_table_fiches_peuplement %>%
#'   dplyr::filter(code_sta_pp == "03231000_013") %>% 
#'   plot_peuplement_data()
#' }
plot_peuplement_data <- function(df){
  
  df <- 
    df %>% 
    dplyr::select(code_sta_pp, 
                  sta_libelle_sandre, 
                  ope_id, 
                  annee,
                  ope_date,
                  pre_id,
                  esp_code_alternatif, 
                  esp_nom_commun, 
                  tpe_libelle,
                  lop_effectif) %>% 
    dplyr::group_by(code_sta_pp, 
                    sta_libelle_sandre, 
                    ope_id, 
                    annee,
                    ope_date,
                    #pre_id,
                    esp_code_alternatif, 
                    esp_nom_commun, 
                    tpe_libelle) %>%
    dplyr::distinct() %>% 
    dplyr::summarise(lop_effectif = sum(lop_effectif))
  
  col_scale_peuplement <- 
    setNames(RColorBrewer::brewer.pal(n = length(df$tpe_libelle %>% unique()),
                                      'Set2'), 
             df$tpe_libelle %>% 
               unique() %>% 
               sort())
    
    df %>% 
    {ggplot2::ggplot(.,  aes(x = ope_date, 
                             y = lop_effectif, 
                             #color= forcats::fct_rev(tpe_libelle),
                             fill= forcats::fct_rev(tpe_libelle))) +
        ggplot2::geom_bar(stat="identity", 
                          col = 'black', 
                          linewidth = 0.1, 
                          width = diff(sort(unique(df$ope_date))) %>% min(),
                          alpha = 0.8) +
        ggplot2::facet_wrap(.~esp_code_alternatif, scales = 'free_y') + 
        #ggplot2::scale_y_continuous(breaks = integer_breaks()) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        #ggplot2::scale_x_continuous(breaks = unique(.$annee)) +
        # ggplot2::scale_x_date(date_breaks = "1 year",
        #                       date_minor_breaks = "1 year",
        #                       date_labels = "%Y",
        #                       limits = c(min(df$ope_date)-180,
        #                                  max(df$ope_date)+180)) +
        ggplot2::scale_x_date(date_breaks = "1 year",
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              expand = ggplot2::expansion(mult = .1)) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = col_scale_peuplement) +
        #ggplot2::scale_color_manual(values = col_scale_peuplement) +
        ggplot2::theme(legend.title=ggplot2::element_blank(),
              axis.text.y = ggplot2::element_text(face = 'bold',size=8),
              title = ggplot2::element_text(face = 'bold', size=8),
              axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size=7),
              legend.position = 'bottom',
              plot.subtitle = ggtext::element_textbox_simple()) +
        ggplot2::labs(title = "Nombre d\'individus par esp\u00e8ces et passages:",
                      subtitle = glue::glue("{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})"))
    }
}
