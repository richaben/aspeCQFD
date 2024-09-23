#' Plot le graphique des probas des espèces issues du calcul de l'IPR 
#'
#' @param df un dataframe issu de la fonction [aspeCQFD::mef_tab_proba_esp_ipr()]
#'
#' @return un graphique ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' aspe_table_fiches %>% 
#'   dplyr::filter(code_sta_pp == "03174000_005") %>% 
#'   mef_tab_proba_esp_ipr() %>% 
#'   plot_ipr_proba_esp_station()
#' }
plot_ipr_proba_esp_station <- function(df){
  
  df <- 
    df %>% 
    dplyr::filter(!is.na(ppi_esp_id)) %>% 
    dplyr::mutate(
      esp_code_alternatif = forcats::fct_reorder(esp_code_alternatif, 
                                                 ppi_valeur_probabilite,
                                                 .na_rm = TRUE),
      couleur = ifelse(presence == "Pr\u00e9sence", "#009392FF", "#CF597EFF")) %>% 
    dplyr::arrange(esp_code_alternatif) %>% 
    dplyr::mutate(text = scales::percent(ppi_valeur_probabilite, accuracy = 1),
           text = dplyr::if_else(text == '0%', NA, text),
           pch_size = (log1p(ppi_param_effectif) / log1p(max(ppi_param_effectif))) + 3)
  
  ggplot2::ggplot(data = df,
         aes(y = esp_code_alternatif,
             x = ope_date)) +
    ggplot2::geom_point(aes(size = pch_size,
                   fill = presence,
                   shape = presence)) +
    ggplot2::geom_text(aes(label = text),
              hjust = -0.6,
              vjust = 0.5, 
              size=3,
              fontface = 3) +
    ggplot2::scale_x_date(date_breaks = "1 year",
                          date_minor_breaks = "1 year",
                          date_labels = "%Y",
                          expand = ggplot2::expansion(mult = .05)) +
    ggplot2::labs(x = NULL,
         y = NULL,
         title = "Pr\u00e9sence / absence des taxons (avec probabilit\u00e9 de pr\u00e9sence associ\u00e9e) et nombre d\'individus captur\u00e9s (taille point proportionnelle)",
         subtitle = glue::glue("{unique(df$sta_libelle_sandre)} ({unique(df$code_sta_pp)})"),
         caption = "Les valeurs \u00e0 0% ne sont pas affich\u00e9es",
         fill = NULL) +
    ggplot2::scale_shape_manual(values = c(21, 22), guide = 'none') +
    ggplot2::scale_fill_manual(values = unique(df$couleur), guide="none") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = 'bold', size=9),
          plot.title = ggplot2::element_text(face = 'bold', size=9),
          plot.subtitle = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
          legend.position = 'top') +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes=list(shape= c(21,22), size = 4))) +
    ggplot2::scale_size(guide = 'none')

}




