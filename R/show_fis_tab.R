#' show the names and links of the table for downloading the wfs layers.
#' @param x The path where the data lays
#' @return A dataframe with all the names and links.
#' @export
#' @examples
#' \dontrun{
#' show_fis_tab()
#' }


show_fis_tab <- function(x = "./data-raw/fis_broker_wfs_links.csv"){
  tab <- data.frame(readr::read_delim(x, delim = ";"))
  return(dplyr::mutate(tab, name_ger = stringi::stri_encode(tab$name_ger,
                                                       from = "ISO-8859-1",
                                                       to = "UTF-8")))
}

#show_fis_tab()

#devtools::install()

#d6raster::show_fis_tab()
