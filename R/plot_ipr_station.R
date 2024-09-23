#' Plot un graphique des notes IPR pour une station
#' 
#' @description
#' Le graphique se compose de 2 parties
#' 
#'
#' @param df un dataframe aspe issu de la fonction [aspeCQFD::mef_creer_table_peuplement()]
#' @param df_col_ipr un dataframe contenant les classes IPR, à créer avec la fonction [aspeCQFD::tab_col_class_ipr()]
#'
#' @return un graphique ggplot2
#' @export
#'
#' @importFrom dplyr select filter
#' @importFrom ggplot2 ggplot geom_rect scale_fill_manual scale_y_continuous expansion scale_x_date geom_line geom_point ylab xlab theme_bw theme element_text element_line element_rect labs
#' @importFrom ggtext element_textbox_simple
#' @importFrom glue glue
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom tidyr pivot_longer
#' @examples
#' \dontrun{
#' 
#' ## tableau stations
#' aspe_table_fiches_ipr <-
#'   aspe_table_fiches_peuplement %>% 
#'   dplyr::select(code_sta_pp, 
#'                 sta_libelle_sandre, 
#'                 ope_id, 
#'                 annee, 
#'                 ope_date,
#'                 ipr,
#'                 cli_id,
#'                 cli_libelle) %>% 
#'   dplyr::distinct() 
#' 
#' ## tableau classes IPR
#' df_col_ipr <-
#'   tab_col_class_ipr()
#' 
#' ## graphiques
#' aspe_table_fiches_ipr %>% 
#'   dplyr::filter(code_sta_pp == "03231000_013") %>% 
#'   plot_ipr_station(df = ., 
#'                    df_col_ipr = df_col_ipr)
#'                    
#'}
#'

plot_ipr_station <- function(df, 
                             df_col_ipr){
  
  part2 <- df %>% 
    {ggplot2::ggplot(data = ., aes(group = 1)) + 
        ggplot2::geom_rect(data = df_col_ipr,
                           aes(ymin= cli_borne_inf, 
                               ymax= cli_borne_sup , 
                               fill= cli_libelle),
                           xmin = -Inf, 
                           xmax = Inf,
                           alpha = 0.3) +
        ggplot2::scale_fill_manual(values = col_ipr, 
                                   name = 'Classes IPR') +
        ggplot2::scale_y_continuous(trans = "reverse", 
                                    breaks = c(0,5,16,25,36,60),
                                    limits = c(60,0),
                                    expand = ggplot2::expansion(mult = c(0.05, 0.01))) +
        
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              limits = c(min(df$ope_date)-180,
                                         max(df$ope_date)+180)) +
        
        ggplot2::geom_line(show.legend = F,lty=2,aes(x=ope_date,y=ipr)) +
        
        ggplot2::geom_point(size=2.5,pch=21,fill="grey70",aes(x=ope_date,y=ipr)) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(title = ggplot2::element_text(size = 9), 
                       plot.title = ggplot2::element_text(face = "bold"),
                       strip.text = ggplot2::element_text(size = 9, face = "bold"),
                       axis.title = ggplot2::element_text(size = 9, face = "bold"),
                       axis.text.x = ggplot2::element_text(size = 9,angle = 45,hjust=1),
                       axis.text.y = ggplot2::element_text(size = 9),
                       legend.text = ggplot2::element_text(size = 9),
                       legend.title = ggplot2::element_text(size = 9, face= "bold"),
                       legend.position= "bottom",
                       legend.box= "vertical",
                       panel.grid.major = ggplot2::element_line(color=NA),
                       panel.grid.minor = ggplot2::element_line(color=NA),
                       panel.background = ggplot2::element_rect(fill=NA),
                       strip.text.x = ggplot2::element_text(size = 9,color="white"),
                       strip.background = ggplot2::element_rect(color="black",fill="grey30"),
                       plot.subtitle = ggtext::element_textbox_simple()) +
        ggplot2::labs(title = "Notes IPR:",
                      subtitle = glue::glue("{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})"))
    }
  
  part1 <- 
    df %>% 
    dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee, ope_date, ipr) %>% 
    tidyr::pivot_longer(c(ipr)) %>% 
    {ggplot2::ggplot(data = ., aes(x = ope_date, y = name)) +
        ggplot2::geom_point(data = (. %>% dplyr::filter(!is.na(value))), shape=21, size=3, fill='grey',show.legend =  F) +
        ggplot2::geom_point(data = (. %>% dplyr::filter(is.na(value))), shape=4, size=3, col='red',stroke = 2, show.legend =  F) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_date(date_breaks = "1 year", 
                              date_minor_breaks = "1 year",
                              date_labels = "%Y",
                              limits = c(min(df$ope_date)-180,
                                         max(df$ope_date)+180)) +
        #scale_shape_manual(values = c(21,23)) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(face = 'bold',size=9),
          title = ggplot2::element_text(face = 'bold', size=9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
          plot.subtitle = ggtext::element_textbox_simple()) +
        ggplot2::labs(title = "Pr\u00e9sence de notes IPR:",
                      subtitle = glue::glue("{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})"))
    }
  
  patchwork::wrap_plots(part1, part2, ncol = 1) +
    patchwork::plot_layout(heights = c(0.2, 1))
}


