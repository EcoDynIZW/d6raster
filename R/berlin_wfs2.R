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

  base_fun <- function(data){ # function, to be applied on each row of input table
    #dat <- single_row
    # single_row  <- as.data.frame(t(as.matrix(data))) # preparation step
    # name       <- data$name_eng # starting name for files; in case, please change column (and pay attention to surrogate assignment in a few lines below)
    # title_engl  <- data$name_eng # title in English (translated from whole original title)
    # date_cr     <- as.character(data$recent_publication) # date of creation; in case, please change column
    # single_link <- data$link # WFS-link; in case, please change column

    wfs_client  <- ows4R::WFSClient$new(data$link, serviceVersion = "2.0.0")

    layer       <- wfs_client$ # layer name (incl. prefix, e. g.: "fis:")
      getCapabilities()$
      getFeatureTypes() %>%
      purrr::map_chr(function(x){x$getName()})

    if(length(layer) > 1) stop(paste0("This function is not suited for WFS-sets with multiple layers. First layer here: ", layer[1]))

    meta_df <- tibble() %>%
      mutate(name = title_engl,
             title = wfs_client$ # layer title in German
               getCapabilities()$
               findFeatureTypeByName(layer)$
               getTitle(),
             typename = unlist(strsplit(layer, ":"))[2],
             crs = wfs_client$ # CRS
               getCapabilities()$
               findFeatureTypeByName(layer)$
               getDefaultCRS()[1]$input,
             date_of_creation = data$recent_publication)
    #name, title, title_engl, geo, crs, abstr, date_cr, date_dl, rast_col, source = "FIS-Broker", single_link
    link2       <- httr::parse_url(data$link)
    link2$query <- list(service   = "wfs",
                        version   = "2.0.0",
                        request   = "GetFeature",
                        typenames = typename,
                        srsName   = paste0("EPSG:", unlist(strsplit(meta_df$crs, ":"))[2])) # applies CRS to shapefile for download
    request     <- sf::st_read(httr::build_url(link2))

    # I need this for the rasterization in the next script
    col_choose <- names(request)[utils::menu(c(paste(names(request), lapply(request, class), sep = " - ")))]
    rast_col <- if(rlang::is_empty(col_choose) == TRUE){
      #readline("Set name: ")
      c("area", "distance", "custom_categorical", "several columns")[utils::menu(c("area", "distance", "custom_categorical", "several columns"),
                                                                                 title="Choose one?")]

    } else{
      col_choose
    }

    meta_df <- meta_df %>%
      mutate(geometry_type = as.character(sf::st_geometry_type(request2,
                                                     by_geometry = FALSE)),
             abstract = wfs_client$ # abstract
               getCapabilities()$
               findFeatureTypeByName(layer)$
               getAbstract(),
             date_of_download = as.Date(paste(substr(date(), 5, 10), substr(date(), 21, 24), sep = " "), format = "%b %d %Y"),
             rast_col = rast_col,
             source = "FIS-Broker",
             link = data$link) # date of download (length of 'date()' is always 24 elements))


    mainDir <- Geo_path
    subDir  <- paste0(mainDir, "/", paste(name, tolower(geo), "berlin", substr(date_cr, 7, 10), unlist(strsplit(crs, ":"))[2], sep = "_"))

    if(!file.exists(subDir)){ # creates new folder per layer
      dir.create(subDir)
    }

    #meta_new_row <- data.frame(name, title, title_engl, geo, crs, abstr, date_cr, date_dl, rast_col, source = "FIS-Broker", single_link) # metadata

    utils::write.table(meta_df, paste0(subDir, "/", paste("metadata", meta_df$name, tolower(meta_df$geometry_type),
                                                          "berlin", substr(meta_df$date_of_creation, 7, 10), unlist(strsplit(meta_df$crs, ":"))[2], sep = "_"), ".csv"), # (over)writes .csv
                       row.names = FALSE, sep = ",")

    sf::st_write(request, paste0(subDir, "/", paste(meta_df$name, tolower(meta_df$geometry_type), "berlin",substr(meta_df$date_of_creation, 7, 10),
                                                    unlist(strsplit(meta_df$crs, ":"))[2], sep = "_"), ".gpkg"), delete_dsn = TRUE) # downloads (overwrites) file
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
