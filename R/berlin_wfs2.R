#' download wfs layer from the fisbroker database.
#' @param table The data frame where you find the names of the file, the data and the url.
#' @param Geo_path The path where the data has to be stored
#' @param ... Optional. Columns in the data frame
#' @return A folder with the data as a geopackage.
#' @export
#' @examples
#' \dontrun{
#' berlin_wfs2(table = table, Geo_path = ".")
#' }

#### Function
berlin_wfs2 <- function(table = data.frame(readr::read_delim("./data-raw/fis_broker_wfs_links.csv",
                                                                           delim = ";")) %>%
                        dplyr::mutate(name_ger = stringi::stri_encode(name_ger,
                                                                              from = "ISO-8859-1",
                                                                              to = "UTF-8")),
                        Geo_path, ...){ # a data frame is required, with at least a column for the year of data creation and WFS-link

  base_fun <- function(data){ # function, to be applied on each row of input table
    data <- as.data.frame(t(data))

    wfs_client  <- ows4R::WFSClient$new(data$link, serviceVersion = "2.0.0")

    layer       <- wfs_client$ # layer name (incl. prefix, e. g.: "fis:")
      getCapabilities()$
      getFeatureTypes() %>%
      purrr::map_chr(function(x){x$getName()})

    if(length(layer) > 1) stop(paste0("This function is not suited for WFS-sets with multiple layers. First layer here: ", layer[1]))

    meta_df <- dplyr::tibble(name = data$name_eng,
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

    link2       <- httr::parse_url(data$link)
    link2$query <- list(service   = "wfs",
                        version   = "2.0.0",
                        request   = "GetFeature",
                        typenames = meta_df$typename,
                        srsName   = paste0("EPSG:", unlist(strsplit(meta_df$crs, ":"))[2])) # applies CRS to shapefile for download
    request     <- sf::st_read(httr::build_url(link2))

    # I need this for the rasterization in the next script
    print("Choose one column or type 0 for another option.")
    col_choose <- c(names(request), "another")[utils::menu(c(paste(names(request), lapply(request, class), sep = " - "), "another"))]
    rast_col <- if(col_choose == "another"){
      #readline("Set name: ")
      c("area", "distance", "custom_categorical")[utils::menu(c("area", "distance", "custom_categorical"),
                                                                                 title="Choose one?")]

    } else{
      col_choose
    }

    meta_df <- meta_df %>%
      dplyr::mutate(geometry_type = as.character(sf::st_geometry_type(request,
                                                     by_geometry = FALSE)),
             abstract = wfs_client$ # abstract
               getCapabilities()$
               findFeatureTypeByName(layer)$
               getAbstract(),
             date_of_download = as.Date(paste(substr(date(), 5, 10), substr(date(), 21, 24), sep = " "), format = "%b %d %Y"),
             rast_col = rast_col,
             source = "FIS-Broker",
             link = data$link) # date of download (length of 'date()' is always 24 elements))

    print("Do you want to add another column?")
    if(nrow(meta_df) >= 1){
      if(c("yes", "no")[utils::menu(c("yes", "no"))] == "yes"){
        meta_df <- base::rbind(meta_df, meta_df)
        meta_df[nrow(meta_df), "rast_col"] <-  c(names(request), "another")[utils::menu(c(paste(names(request), lapply(request, class), sep = " - "), "another"))]
        if(meta_df[nrow(meta_df), "rast_col"] == "another"){
          meta_df[nrow(meta_df), "rast_col"] <- c("area", "distance", "custom_categorical")[utils::menu(c("area", "distance", "custom_categorical"))]
        }
      }
    }

    mainDir <- Geo_path
    subDir  <- paste0(mainDir, "/", paste(meta_df$name[1], tolower(meta_df$geometry_type[1]), "berlin", substr(meta_df$date_of_creation[1], 7, 10), unlist(strsplit(meta_df$crs[1], ":"))[2], sep = "_"))

    if(!file.exists(subDir)){ # creates new folder per layer
      dir.create(subDir)
    }

    #meta_new_row <- data.frame(name, title, title_engl, geo, crs, abstr, date_cr, date_dl, rast_col, source = "FIS-Broker", single_link) # metadata

    utils::write.table(meta_df, paste0(subDir, "/", paste("metadata", meta_df$name[1], tolower(meta_df$geometry_type[1]),
                                                          "berlin", substr(meta_df$date_of_creation[1], 7, 10), unlist(strsplit(meta_df$crs[1], ":"))[2], sep = "_"), ".csv"), # (over)writes .csv
                       row.names = FALSE, sep = ",")

    sf::st_write(request, paste0(subDir, "/", paste(meta_df$name[1], tolower(meta_df$geometry_type[1]), "berlin",substr(meta_df$date_of_creation[1], 7, 10),
                                                    unlist(strsplit(meta_df$crs[1], ":"))[2], sep = "_"), ".gpkg"), delete_dsn = TRUE) # downloads (overwrites) file
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


#data <- t(readr::read_delim("data-raw/test_data_fisbroker.csv", delim = ";"))


#d6raster::berlin_wfs2(readr::read_delim("data-raw/test_data_fisbroker.csv", delim = ";"), ".")

#d6raster::berlin_wfs2(Geo_path = ".")

#devtools::install()
