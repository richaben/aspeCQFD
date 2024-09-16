#' Plot le graphique Protocole(s) + Passage(s) + Point(s) pour une station
#'
#' @param df un dataframe issu de la fonction [aspeCQFD::mef_tab_protocole_pts()] issu
#' de la jointure avec une table générale [aspeCQFD::mef_creer_table_fiches()]
#'
#' @return un graphique ggplot2
#' @export
#'
#' @importFrom dplyr select group_by mutate case_when filter
#' @importFrom ggplot2 ggplot geom_point ylab xlab scale_x_continuous theme_bw scale_shape_manual theme element_text labs geom_bar scale_fill_manual element_blank
#' @importFrom glue glue
#' @importFrom patchwork wrap_plots
#' @importFrom stringr str_detect str_wrap
#' @importFrom tidyr pivot_longer
#' 
#' @examples
#' \dontrun{
#' aspe_table_fiches %>%
#'   dplyr::filter(code_sta_pp == "03231000_013") %>%
#'   mef_tab_protocole_pts() %>% 
#'   plot_proto_prospec_passage()
#' }
#' 

plot_proto_prospec_passage <- function(df){
  
  if(stringr::str_detect(toString(unique(df$pro_libelle)), 'par points') == TRUE){
  
    df_part1 <-
    df %>% 
      dplyr::select(code_sta_pp, 
                    sta_libelle_sandre, 
                    ope_id, 
                    annee, 
                    pro_libelle,
                    mop_libelle, 
                    pas_numero) %>% 
      dplyr::group_by(code_sta_pp, 
                      sta_libelle_sandre, 
                      ope_id, 
                      annee, 
                      pro_libelle,
                      mop_libelle) %>% 
      tidyr::pivot_longer(cols = -c(code_sta_pp, sta_libelle_sandre, ope_id, annee),
                   values_to = "value",
                   values_transform = list(value = as.character)) %>% 
      dplyr::mutate(value = dplyr::case_when(name == 'pas_numero' & !is.na(value) ~ 
                                               paste0('#_passage: ', value),
                               TRUE ~ value)) %>% 
      dplyr::filter(!is.na(value)) 
    
      part1 <- 
        df_part1 %>% 
      ggplot2::ggplot(., aes(y = stringr::str_wrap(value,15), 
                              x = annee, 
                              shape = name, 
                              fill = name)) +
          ggplot2::geom_point(col='black',size=4, show.legend = F) +
          ggplot2::ylab(NULL) +
          ggplot2::xlab(NULL) +
          ggplot2::scale_x_continuous(breaks = unique(df_part1$annee)
                                      ) +
          ggplot2::theme_bw() +
          ggplot2::scale_shape_manual(values = c(21,23,24)) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(face = 'bold',size=10),
                title = ggplot2::element_text(face = 'bold', size=9),
                axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
          ggplot2::labs(title = "Protocole(s) + Prospection(s) + Passage(s):",
                        subtitle = glue::glue('{unique(df_part1$sta_libelle_sandre)} ({unique(df_part1$code_sta_pp)})'))
      
    
    part2 <-
      df %>% 
      ggplot2::ggplot(.,  aes(x = annee, y = grp_nombre, fill= grp_tgp_id)) +
          ggplot2::geom_bar(stat="identity", width = 0.75, col='black', linewidth = 0.2, alpha = 0.8) +
          ggplot2::ylab(NULL) +
          ggplot2::xlab("Ann\u00e9es") + 
          ggplot2::scale_x_continuous(breaks = unique(df$annee)
                                      ) +
          ggplot2::theme_bw() +
          ggplot2::scale_fill_manual(values=c('#4ECDC4','#999999')) +
          ggplot2::theme(legend.title=ggplot2::element_blank(),
                axis.text.y = ggplot2::element_text(face = 'bold',size=10),
                title = ggplot2::element_text(face = 'bold', size=9),
                axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                legend.position = 'right') +
          ggplot2::labs(subtitle = glue::glue('Pr\u00e9l\u00e8vement(s) - Points'))
      
    
    patchwork::wrap_plots(part1, part2, ncol = 1)
    
  } else {
    
    df %>% 
      dplyr::select(code_sta_pp, 
                    sta_libelle_sandre, 
                    ope_id, 
                    annee, 
                    pro_libelle, 
                    mop_libelle, 
                    pas_numero) %>% 
      dplyr::group_by(code_sta_pp, 
                      sta_libelle_sandre, 
                      ope_id, 
                      annee, 
                      pro_libelle, 
                      mop_libelle) %>% 
      tidyr::pivot_longer(cols = -c(code_sta_pp, sta_libelle_sandre, ope_id, annee),
                   values_to = "value",
                   values_transform = list(value = as.character)) %>% 
      dplyr::mutate(value = dplyr::case_when(name == 'pas_numero' & !is.na(value) ~ paste0('#_passage: ', value),
                               TRUE ~ value)) %>% 
      filter(!is.na(value)) %>% 
      {ggplot2::ggplot(., aes(y = stringr::str_wrap(value,15), 
                              x = annee, 
                              shape = name, 
                              fill = name)) +
          ggplot2::geom_point(col='black',size=4, show.legend = F) +
          ggplot2::ylab(NULL) +
          ggplot2::xlab(NULL) +
          #scale_x_continuous(breaks = seq(min(.$annee), max(.$annee), by = 1)) +
          ggplot2::theme_bw() +
          ggplot2::scale_shape_manual(values = c(21,23,24)) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(face = 'bold',size=10),
                title = ggplot2::element_text(face = 'bold', size=9),
                axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
          ggplot2::labs(title = 'Protocole(s) + Prospection(s) + Passage(s):',
                        subtitle = glue::glue('{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})'))
      }
  }
  
}
