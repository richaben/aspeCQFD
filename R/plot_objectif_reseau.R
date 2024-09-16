#' Plot objectifs réseaux pour une station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un graphique ggplot2
#' @export
#' 
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot geom_point ylab xlab theme_bw scale_shape_manual scale_x_continuous theme element_text labs
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer
#' 
#' @examples
#' \dontrun{
#' aspe_table_station1 <- 
#'   mef_creer_table_fiches() %>% 
#'   dplyr::filter(code_sta_pp == "03174000_005")
#' 
#' plot_objectif_reseau(aspe_table_station1)
#' }

plot_objectif_reseau <- function(df){
  df %>% 
    dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee, obj_libelle) %>% 
    unique() %>%
    tidyr::pivot_longer(cols = -c(code_sta_pp, sta_libelle_sandre, ope_id, annee)) %>%
    {ggplot2::ggplot(., aes(y = stringr::str_wrap(value,15), 
                            x = annee, 
                            shape = value, 
                            fill = value)) +
        ggplot2::geom_point(col='black', 
                            size=4, 
                            show.legend = F) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab("Ann\u00e9es") +
        ggplot2::theme_bw() +
        ggplot2::scale_shape_manual(values = c(21, 23, 24, 25)) +
        ggplot2::scale_x_continuous(breaks = unique(.$annee), 
                                    minor_breaks = 1) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(face = 'bold',size=10),
          title = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
          plot.subtitle = ggtext::element_textbox_simple()
          ) +
        ggplot2::labs(title = 'Objectif(s) + Protocole(s):',
                      subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))
    }
}

