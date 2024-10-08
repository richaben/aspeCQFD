% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_infos_context.R
\name{plot_infos_context}
\alias{plot_infos_context}
\title{Plot les infos contextuelles des opérations pour une station}
\usage{
plot_infos_context(df)
}
\arguments{
\item{df}{un dataframe aspe issu de la fonction \code{\link[=mef_creer_table_fiches]{mef_creer_table_fiches()}}}
}
\value{
un graphique ggplot2
}
\description{
Plot les infos contextuelles des opérations pour une station
}
\examples{
\dontrun{
aspe_table_station1 <- 
  mef_creer_table_fiches() \%>\% 
  dplyr::filter(code_sta_pp == "03174000_005") \%>\% 
  dplyr::select(code_sta_pp, sta_libelle_sandre, ope_id, annee) \%>\% 
  mef_ajouter_infos_context() \%>\% 
  unique()
  
  plot_infos_context(aspe_table_station1)
}

}
