#' Plot jours de pêche pour une station ou plusieurs
#'
#' @param df un dataframe issu de la fonction [aspeCQFD::get_liste_ope_station()]
#' @param jour_julien un booléen pour afficher les jours de l'année et code couleur saison,
#' par défaut `FALSE`
#'
#' @return un graphique ggplot2
#' @export
#'
#' @importFrom dplyr mutate group_by filter
#' @importFrom ggplot2 ggplot geom_jitter aes geom_point labs theme_bw scale_x_date theme element_text geom_rect scale_fill_manual coord_cartesian
#' @importFrom ggrepel geom_label_repel
#' @importFrom glue glue
#' @importFrom lubridate yday
#'
#' @examples
#' \dontrun{
#' stations_sel <-
#'   get_liste_ope_station(df = aspe_table_fiches,
#'                         code_sta_pp = c("03231000_013","03174000_005")
#'   )
#' 
#' plot_jour_peche(stations_sel, jour_julien = F)
#' }

plot_jour_peche <- function(df, jour_julien = F){
  
  df <-
    df %>% 
    dplyr::mutate(jour_julien = lubridate::yday(ope_date),
                  ope_date2 = as.Date(paste0("2020-", format(ope_date, "%m-%d"))))
  
  if(jour_julien == FALSE){
    ggplot2::ggplot(data = df) + 
      ggplot2::geom_jitter(data = df,
                           ggplot2::aes(x = ope_date2, y = code_sta_pp),
                           size = 3, 
                           pch = 21, 
                           show.legend = F, 
                           fill = "grey50",
                           alpha = 0.6,
                           width = 0.001) +
      
      ggplot2::geom_point(data = df %>% dplyr::group_by(code_sta_pp) %>% dplyr::filter(ope_date == max(ope_date)), 
                          ggplot2::aes(x = ope_date2, y = code_sta_pp),
                          size = 4, 
                          pch = 23, 
                          fill = 'transparent') +
      ggrepel::geom_label_repel(
        data = df %>% dplyr::group_by(code_sta_pp) %>% dplyr::filter(ope_date == max(ope_date)),
        ggplot2::aes(x = ope_date2, y = code_sta_pp, label = glue::glue("{format(ope_date, \'%d-%m-%Y\')} (ope:{ope_id})")),
        size = 3,
        box.padding = 0.5,
        point.padding = 0.5,
        alpha = 0.8,
        segment.color = "grey50",
        segment.size = 0.5,
        nudge_y = 0.2
      ) +
      ggplot2::labs(y = NULL, 
                    x = "Jour de l\'ann\u00e9e", 
                    title = "Historicit\u00e9 jours p\u00eache station(s)",
                    subtitle = "Derni\u00e8re op\u00e9 (symbole losange)") +
      
      ggplot2::theme_bw() +
      ggplot2::scale_x_date(date_breaks = "7 days", 
                            date_labels = "%d-%b", 
                            date_minor_breaks = "1 day", 
                            expand = c(0.1, 0.1)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     plot.title = ggplot2::element_text(size = 12, face = "bold"),
                     axis.text.y = ggplot2::element_text(size = 9, face = "bold")
      )
    
  } else {
    col_saisons <- data.frame(
      ymin = c(-Inf, -Inf, -Inf, -Inf, -Inf),
      ymax = c(Inf, Inf, Inf, Inf, Inf),
      xmin = c(1, 79, 172, 266, 355),
      xmax = c(79, 172, 266, 355, 365),
      labels = factor(c("Hiver", "Printemps", "Et\u00e9", "Automne", "Hiver"), 
                      ordered = T, 
                      levels = c("Printemps", "Et\u00e9", "Automne","Hiver"))
    ) 
    
    legend_colors <- setNames(c("#A3D977", "#FFD700", "#D2691E","#4682B4"), 
                              c("Printemps", "Et\u00e9", "Automne", "Hiver"))
    
    ggplot2::ggplot(data = df) + 
      ggplot2::geom_rect(
        data = col_saisons,
        ggplot2::aes(xmin = xmin, xmax = xmax, fill = labels),
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.3
      ) +
      ggplot2::geom_jitter(data = df,
                           ggplot2::aes(x = jour_julien, y = code_sta_pp),
                           size = 3, 
                           pch = 21, 
                           show.legend = F, 
                           fill = "grey50",
                           alpha = 0.6,
                           width = 0.001) +
      
      ggplot2::geom_point(data = df %>% dplyr::group_by(code_sta_pp) %>% dplyr::filter(ope_date == max(ope_date)), 
                          ggplot2::aes(x = jour_julien, y = code_sta_pp),
                          size = 4, 
                          pch = 23, 
                          fill = "white") +
      ggplot2::labs(y = NULL, 
                    x = "Jour de l\'ann\u00e9e", 
                    title = "Historicit\u00e9 jours p\u00eache station(s)",
                    subtitle = "Derni\u00e8re op\u00e9 (symbole losange)") +
      
      
      ggplot2::scale_fill_manual(name = "Saison", values = legend_colors) +
      ggplot2::coord_cartesian(xlim = c(min(df$jour_julien), max(df$jour_julien))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     plot.title = ggplot2::element_text(size = 12, face = "bold"),
                     axis.text.y = ggplot2::element_text(size = 9, face = "bold")
      )
  }
  
  
}