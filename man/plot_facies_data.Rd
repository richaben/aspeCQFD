% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_facies_data.R
\name{plot_facies_data}
\alias{plot_facies_data}
\title{Plot du graphique des données faciès pour une station}
\usage{
plot_facies_data(df)
}
\arguments{
\item{df}{un dataframe aspe issu de la fonction \code{\link[=mef_creer_table_fiches]{mef_creer_table_fiches()}}}
}
\value{
un graphique ggplot2
}
\description{
Plot du graphique des données faciès pour une station
}
\examples{
\dontrun{
aspe_table_fiches \%>\% 
  dplyr::filter(code_sta_pp == "03231000_013") \%>\% 
  plot_facies_data()
}

}
