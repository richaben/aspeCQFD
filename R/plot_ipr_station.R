plot_ipr_station <- 
  function(){
  # function(tab_ipr_station, 
  #          tab_param_ipr){
  # 
  #   tab_ipr_station %>% 
  #   {ggplot2::ggplot(data = ., aes(group = 1)) + 
  #       ggplot2::geom_rect(data = tab_param_ipr,
  #                 aes(ymin= cli_borne_inf, 
  #                     ymax= cli_borne_sup , 
  #                     fill= cli_libelle),
  #                 xmin = -Inf, 
  #                 xmax = Inf,
  #                 alpha = 0.3) +
  #       ggplot2::scale_fill_manual(values = col_ipr, name = 'Classes IPR') +
  #       ggplot2::scale_y_continuous(trans = "reverse", 
  #                                  breaks = c(0,5,16,25,36,60),
  #                                  limits = c(60,0),
  #                          expand = expansion(mult = c(0.05, 0.01))) +
  #       ggplot2::scale_x_continuous(breaks = .$annee %>% unique() %>% sort(),
  #                          limits = c(.$annee %>% unique() %>% min(),
  #                                     .$annee %>% unique() %>% max())) +
  #       
  #       ggplot2::geom_line(show.legend = F,lty=2,aes(x=annee,y=ipr)) +
  #       ggplot2::geom_point(size=2.5,pch=21,fill="grey70",aes(x=annee,y=ipr)) +
  #       
  #       ggplot2::theme_bw() +
  #       ggplot2::theme(
  #         title = ggplot2::element_text(size = 9), 
  #         plot.title = ggplot2::element_text(face = "bold"),
  #         strip.text = ggplot2::element_text(size = 9, face = "bold"),
  #         axis.title = ggplot2::element_text(size = 9, face = "bold"),
  #         axis.text.x = ggplot2::element_text(size = 9,angle = 45,hjust=1),
  #         axis.text.y = ggplot2::element_text(size = 9),
  #         legend.text = ggplot2::element_text(size = 9),
  #         legend.title = ggplot2::element_text(size = 9, face= "bold"),
  #         legend.position = "bottom",
  #         legend.box  = "vertical",
  #         panel.grid.major = ggplot2::element_line(color=NA),
  #         panel.grid.minor = ggplot2::element_line(color=NA),
  #         panel.background = ggplot2::element_rect(fill=NA),
  #         strip.text.x = ggplot2::element_text(size = 9,color="white"),
  #         strip.background = ggplot2::element_rect(color="black",fill="grey30")) +
  #       ggplot2::labs(title = "Notes IPR:", 
  #                    subtitle = glue::glue("{unique(.$sta_libelle_sandre)} ({unique(.$code_sta_pp)})"))
  #   }
}