library(plumber)
library(shroomDK)
library(dplyr)
library(plotly)
library(sf)

api_key <- readLines("api_key.txt")
world <- read_sf("world-shapefile/")
world_map = (world[ ,"name"])

source("001_ecosystem_appdata.R")
source("003_validator_staker_stats.R")
source("005_validator_vote_stats.R")

#* @apiTitle Solana Validator Score
#* @apiDescription API for updating local data, pulling images, and 

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

# Ecosystem App Data ---- 
#* Return 
#' @param target_epoch Solana Epoch to get histories up to specified epoch.
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake, for specific API keys, "data-science" is a valid segmented warehouse.
#' @param overwrite_save Default TRUE, enables plots to asynchronously use a save for speed.
#* @post /ecosystem_appdata
function(target_epoch, api_key, data_source = "data-science", overwrite_save = FALSE) {
  
  # see 001_ecosystem_appdata.R
 ecoappdata <- get_ecosystem_appdata(target_epoch = target_epoch, api_key = api_key, data_source = data_source)
 colnames(ecoappdata) <- tolower(colnames(ecoappdata))
 
 # software version upgrades occasionally cause duplicate voter_pubkey at lower software versions 
 
 # drop unique identifier
 ecoappdata$x__row_index <- NULL 
 # remove obvious duplicates
 ecoappdata <- unique(ecoappdata) 
 
 # some validators duplicated with slightly differing sol-stake within same epoch
 # pick MIN of sol_stake within epoch in this case
 # if somehow still duplicate pick the FIRST value (after min)
 ecoappdata <- ecoappdata %>% group_by(epoch, voter_pubkey) %>% 
   filter(sol_stake == min(sol_stake)) %>% 
   slice(1) %>% 
   as.data.frame()
 
 # use Latitude/Longitude for Country Labeling
 longlats <- ecoappdata[ , c("longitude", "latitude")]
 longlats <- unique(longlats[complete.cases(longlats), ])
 longlats_shape <- st_as_sf(longlats, coords = c("longitude","latitude"), crs = 4326)
 longlats <- cbind(longlats, longlats_shape)
 longlats_country <- st_intersection(x = longlats_shape, y = world_map)
 longlats$geo <- as.character((longlats$geometry))
 longlats_country$geo <- as.character((longlats_country$geometry))
 
 longlats <- merge(longlats, longlats_country, by = "geo", all.x = TRUE)
 longlats <- longlats[ , c("longitude", "latitude", "name")]
 colnames(longlats) <- c("longitude", "latitude", "country")
 
 ecoappdata <- merge(ecoappdata, longlats, by = c("longitude","latitude"), all.x = TRUE, sort = FALSE)
 ecoappdata[is.na(ecoappdata$country), "country"] <- "Unknown"
 
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

# Validator Stake Data ----

#* Return 
#' @param target_epoch Solana Epoch to get validator staker stats up to that epoch.
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake, for specific API keys, "data-science" is a valid segmented warehouse.
#' @param overwrite_save Default TRUE, enables plots to asynchronously use a save for speed.
#* @post /validator_staker_stats
function(target_epoch, api_key, data_source = "data-science", overwrite_save = FALSE) {
  
  # see 003_validator_staker_stats.R
  validator_stake <- get_validator_stake(target_epoch = target_epoch, api_key = api_key, data_source = data_source)
  
  # consider overwrite 
  if(overwrite_save){
    saveRDS(validator_stake, "003_latest_save.rds")
  }
  return(validator_stake)
}

#* Return 
#* @get /saved_validator_staker_stats
function(){
  tryCatch({
    validator_stake <- readRDS("003_latest_save.rds")
  }, error = function(e){
    stop("No 003_latest_save.rds found in server, Post validator_staker_stats with overwrite_save = TRUE")
  })
  
  return(validator_stake)
}

# Validator Vote Data ----

#* Return 
#' @param target_epoch Solana Epoch to get validator vote stats up to that epoch.
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake, for specific API keys, "data-science" is a valid segmented warehouse.
#' @param overwrite_save Default TRUE, enables plots to asynchronously use a save for speed.
#* @post /validator_vote_stats
function(target_epoch, api_key, data_source = "data-science", overwrite_save = FALSE) {
  
  # see 005_validator_vote_stats.R
  validator_vote <- get_validator_vote(target_epoch = target_epoch, api_key = api_key, data_source = data_source)
  
  # consider overwrite 
  if(overwrite_save){
    saveRDS(validator_vote, "005_latest_save.rds")
  }
  return(validator_vote)
}

#* Return 
#* @get /saved_validator_vote_stats
function(){
  tryCatch({
    validator_vote <- readRDS("005_latest_save.rds")
  }, error = function(e){
    stop("No 005_latest_save.rds found in server, Post validator_vote_stats with overwrite_save = TRUE")
  })
  
  return(validator_vote)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
