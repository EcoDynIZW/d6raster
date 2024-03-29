% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/berlin_wfs2.R
\name{berlin_wfs2}
\alias{berlin_wfs2}
\title{download wfs layer from the fisbroker database.}
\usage{
berlin_wfs2(
  table = data.frame(readr::read_delim("./data-raw/fis_broker_wfs_links.csv", delim =
    ";")) \%>\% dplyr::mutate(name_ger = stringi::stri_encode(name_ger, from =
    "ISO-8859-1", to = "UTF-8")),
  Geo_path,
  ...
)
}
\arguments{
\item{table}{The data frame where you find the names of the file, the data and the url.}

\item{Geo_path}{The path where the data has to be stored}

\item{...}{Optional. Columns in the data frame}
}
\value{
A folder with the data as a geopackage.
}
\description{
download wfs layer from the fisbroker database.
}
\examples{
\dontrun{
berlin_wfs2(table = table, Geo_path = ".")
}
}
