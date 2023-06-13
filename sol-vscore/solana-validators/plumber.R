library(plumber)
library(shroomDK)
library(dplyr)
library(plotly)
library(sf)

api_key <- readLines("api_key.txt")
world <- read_sf("world-shapefile/")
world_map = (world[ ,"name"])


source("001_ecosystem_appdata.R")

#* @apiTitle Solana Validator Score
#* @apiDescription API for updating local data, pulling images, and 

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Return 
#' @param target_epoch Solana Epoch. 
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake, for specific API keys, "data-science" is a valid segmented warehouse.
#' @param overwrite_save Default TRUE, enables plots to asynchronously use a save for speed.
#* @post /ecosystem_appdata
function(target_epoch, api_key = api_key, data_source = "data-science", overwrite_save = FALSE) {
  
  # see 001_ecosystem_appdata.R
 ecoappdata <- get_ecosystem_appdata(target_epoch = target_epoch, api_key = api_key, data_source = data_source)
 
 # use Latitude/Longitude for Country Labeling
 longlats <- ecoappdata[ , c("LONGITUDE", "LATITUDE")]
 longlats <- unique(longlats[complete.cases(longlats), ])
 longlats_shape <- st_as_sf(longlats, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
 longlats <- cbind(longlats, longlats_shape)
 longlats_country <- st_intersection(x = longlats_shape, y = world_map)
 longlats$geo <- as.character((longlats$geometry))
 longlats_country$geo <- as.character((longlats_country$geometry))
 
 longlats <- merge(longlats, longlats_country, by = "geo", all.x = TRUE)
 longlats <- longlats[ , c("LONGITUDE", "LATITUDE", "name")]
 colnames(longlats) <- c("LONGITUDE", "LATITUDE", "Country")
 
 ecoappdata <- merge(ecoappdata, longlats, by = c("LONGITUDE","LATITUDE"), all.x = TRUE, sort = FALSE)
 ecoappdata[is.na(ecoappdata$Country), "Country"] <- "Unknown"
 
 # consider overwrite 
 if(overwrite_save){
  saveRDS(ecoappdata, "001_latest_save.rds")
 }
 return(ecoappdata)
}


#* Return 
#* @get /saved_ecosystem_appdata
function(){
  tryCatch({
    ecoappdata <- readRDS("001_latest_save.rds")
  }, error = function(e){
    stop("No 001_latest_save.rds found in server, Post ecosystem_appdata with overwrite_save = TRUE")
  })
  
  return(ecoappdata)
}

#* Return
#' @param name description


#* Plot a Plotly of Ecosystem Software Version
#* @serializer htmlwidget
#* @param try_latest Default TRUE, if FALSE,  
#* @get /software_version_barchart
function(try_latest = TRUE) {
    rand <- rnorm(100)
    hist(rand)
}













# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
