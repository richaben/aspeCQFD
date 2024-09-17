#' Plot du graphique des données habitat pour une station
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un graphique ggplot2
#' @export
#' 
#' @importFrom dplyr select filter
#' @importFrom ggplot2 ggplot geom_point geom_label ylab xlab scale_x_continuous theme_bw theme element_text labs
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
    dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee) %>% 
    unique() %>% 
    mef_ajouter_habitat() %>% 
    dplyr::select(
      -ode_profondeur_moyenne_station, 
      -hab_tys_id_vegetation_aquatique) %>% 
    tidyr::pivot_longer(-c(code_sta_pp, sta_libelle_sandre, ope_id, annee),
                 values_to = "value",
                 values_transform = list(value = as.character)) %>% 
    {ggplot2::ggplot(., aes(x = annee, y = name)) +
        ggplot2::geom_point(data = (. %>% dplyr::filter(is.na(value))), 
                            shape=4, 
                            size=2.5, 
                            col='red',
                            stroke = 1.5, 
                            show.legend =  F) +
        ggplot2::geom_label(aes(label = stringr::str_wrap(value,10), 
                                fill= value), 
                            alpha=0.5, 
                            show.legend = F, 
                            size=2.5) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::scale_x_continuous(breaks = unique(.$annee)) +
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
