#' Compute descriptive statistics for the numeric columns of a data frame.
#' @param table The data frame where you find the names of the file, the data and the url.
#' @param Geo_path The path where the data has to be stored
#' @param ... Optional. Columns in the data frame
#' @return A folder with the data as a geopackage.
#' @export
#' @examples
#' \dontrun{
#' berlin_wfs(table = table, Geo_path = ".")
#' }

#### Function
berlin_wfs <- function(table, Geo_path, ...){ # a data frame is required, with at least a column for the year of data creation and WFS-link

  base_fun <- function(single_row){ # function, to be applied on each row of input table
    #dat <- single_row
    single_row  <- as.data.frame(t(as.matrix(single_row))) # preparation step
    name       <- single_row$name_eng # starting name for files; in case, please change column (and pay attention to surrogate assignment in a few lines below)
    title_engl  <- single_row$name_eng # title in English (translated from whole original title)
    date_cr     <- as.character(single_row$recent_publication) # date of creation; in case, please change column
    single_link <- single_row$link # WFS-link; in case, please change column

    wfs_client  <- ows4R::WFSClient$new(single_link, serviceVersion = "2.0.0")

    layer       <- wfs_client$ # layer name (incl. prefix, e. g.: "fis:")
      getCapabilities()$
      getFeatureTypes() %>%
      purrr::map_chr(function(x){x$getName()})

    if(length(layer) > 1) stop(paste0("This function is not suited for WFS-sets with multiple layers. First layer here: ", layer[1]))

    typename <- unlist(strsplit(layer, ":"))[2] # layer name without prefix

    name     <- title_engl # takes as surrogate for option at top of function (name <- single_row[...])

    title    <- wfs_client$ # layer title in German
      getCapabilities()$
      findFeatureTypeByName(layer)$
      getTitle()

    crs      <- wfs_client$ # CRS
      getCapabilities()$
      findFeatureTypeByName(layer)$
      getDefaultCRS()[1]$input

    link2       <- httr::parse_url(single_link)
    link2$query <- list(service   = "wfs",
                        version   = "2.0.0",
                        request   = "GetFeature",
                        typenames = typename,
                        srsName   = paste0("EPSG:", unlist(strsplit(crs, ":"))[2])) # applies CRS to shapefile for download
    request     <- httr::build_url(link2)
    request2    <- sf::st_read(request)

    # I need this for the rasterization in the next script
    col_choose <- names(request2)[utils::menu(c(paste(names(request2), lapply(request2, class), sep = " - ")))]
    rast_col <- if(rlang::is_empty(col_choose) == TRUE){
      #readline("Set name: ")
      c("area", "distance", "custom_categorical", "several columns")[utils::menu(c("area", "distance", "custom_categorical", "several columns"),
                                                                          title="Choose one?")]

    } else{
      col_choose
    }

    geo    <- as.character(sf::st_geometry_type(request2, by_geometry = FALSE)) # geometry type

    abstr  <- wfs_client$ # abstract
      getCapabilities()$
      findFeatureTypeByName(layer)$
      getAbstract()

    date_dl <- as.Date(paste(substr(date(), 5, 10), substr(date(), 21, 24), sep = " "), format = "%b %d %Y") # date of download (length of 'date()' is always 24 elements)

    mainDir <- Geo_path
    subDir  <- paste0(mainDir, "/", paste(name, tolower(geo), "berlin", substr(date_cr, 7, 10), unlist(strsplit(crs, ":"))[2], sep = "_"))

    if(!file.exists(subDir)){ # creates new folder per layer
      dir.create(subDir)
    }

    meta_new_row <- data.frame(name, title, title_engl, geo, crs, abstr, date_cr, date_dl, rast_col, source = "FIS-Broker", single_link) # metadata

    utils::write.table(meta_new_row, paste0(subDir, "/", paste("metadata", name, tolower(geo), "berlin", substr(date_cr, 7, 10), unlist(strsplit(crs, ":"))[2], sep = "_"), ".csv"), # (over)writes .csv
                row.names = FALSE, sep = ",", col.names = c("name", "title_ger", "title_eng", "geometry_type", "crs", "abstract",
                                                            "date_of_creation", "date_of_download", "rast_col", "source", "link"))

    sf::st_write(request2, paste0(subDir, "/", paste(name, tolower(geo), "berlin",substr(date_cr, 7, 10), unlist(strsplit(crs, ":"))[2], sep = "_"), ".gpkg"), delete_dsn = TRUE) # downloads (overwrites) file
  }

  safely_function <- purrr::safely(base_fun) # skip and save errors, when using this function
  execution       <- apply(table, 1, safely_function) # applies function on every row of input table
  print_output    <- purrr::transpose(execution) # reformates list of errors and results (only shows errors later)

  error_counter   <- 0 # for message about number of errors
  for(i in 1:length(print_output$error)){
    if(!is.null(print_output$error[[i]])){
      error_counter <- error_counter + 1
      print(paste0("Error for ", "row no. '", i, "' with link '", table[i, "link"], "': ", print_output$error[i])) # shows respective error
    }
  }

  print(paste0("Number of error(s): ", error_counter))
}

#d6raster::berlin_wfs(readr::read_delim("data-raw/test_data_fisbroker.csv", delim = ";"), ".")


#devtools::install()
