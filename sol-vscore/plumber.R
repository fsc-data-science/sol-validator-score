library(plumber)
library(httr)
library(shroomDK)
library(dplyr)
library(plotly)
library(sf)
library(data.table)

api_key <- readLines("api_key.txt")
world <- read_sf("world-shapefile/")
world_map = (world[ ,"name"])

source("001_ecosystem_appdata.R")
source("003_validator_staker_stats.R")
source("005_validator_vote_stats.R")
source("helper_functions.R")

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
#' @param overwrite_save Default FALSE, overwrites saved data (those available via GET)
#' @param from_previous Default TRUE, re-use available data to only infill new data
#* @post /ecosystem_appdata
function(target_epoch, api_key, data_source = "data-science", overwrite_save = FALSE, from_previous = TRUE) {
  
  # see 001_ecosystem_appdata.R
 
  if(from_previous){ 
    prev_ecoappdata <- readRDS('001_latest_save.rds')
    ecoappdata <- get_ecosystem_appdata(min_epoch = max(prev_ecoappdata$epoch)-1, 
                                        target_epoch = target_epoch, 
                                        api_key = api_key,
                                        data_source = data_source)
    colnames(ecoappdata) <- tolower(colnames(ecoappdata))
    ecoappdata$x__row_index <- NULL 

  } else {
 ecoappdata <- get_ecosystem_appdata(min_epoch = 0, 
                                     target_epoch = target_epoch,
                                     api_key = api_key,
                                     data_source = data_source)
 colnames(ecoappdata) <- tolower(colnames(ecoappdata))
 ecoappdata$x__row_index <- NULL 
  }  
 
 # software version upgrades occasionally cause duplicate voter_pubkey at lower software versions 
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
 
 if(from_previous){
   prev_ecoappdata <- prev_ecoappdata[ , colnames(ecoappdata)]
   ecoappdata <- rbind(prev_ecoappdata, ecoappdata)
 }
 
 # consider overwrite 
 if(overwrite_save){
  saveRDS(ecoappdata, "001_latest_save.rds")
   return("Data updated & available by GET")
 } else {
   # WARNING: large file 
 return(ecoappdata)
 }
}


#* Return 
#* @serializer rds
#* @get /saved_ecosystem_appdata
function(){
 if(!file.exists("001_latest_save.rds")){
    stop("No 001_latest_save.rds found in server, Post ecosystem_appdata with overwrite_save = TRUE")
 } else {
   rds_object <- readRDS("001_latest_save.rds")
   return(rds_object)
 }
}

# Validator Stake Data ----

#* Return 
#' @param target_epoch Solana Epoch to get validator staker stats up to that epoch.
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake, for specific API keys, "data-science" is a valid segmented warehouse.
#' @param overwrite_save Default FALSE, overwrites saved data (those available via GET)
#' @param from_previous Default TRUE, re-use available data to only infill new data
#* @post /validator_staker_stats
function(target_epoch, api_key, data_source = "data-science", overwrite_save = FALSE, from_previous = TRUE) {
  
  # see 003_validator_staker_stats.R
  
  if(from_previous){
  prev_validator_stake <- readRDS("003_latest_save.rds")
  validator_stake <- get_validator_stake(min_epoch = max(prev_validator_stake$epoch)-1,
    target_epoch = target_epoch, 
    api_key = api_key, 
    data_source = data_source)
  
  validator_stake$X__row_index <- NULL
  
  prev_validator_stake <- prev_validator_stake[ , colnames(validator_stake)]
  validator_stake <- rbind(prev_validator_stake, validator_stake)
  
  } else {
  validator_stake <- get_validator_stake(min_epoch = 0, 
                                         target_epoch = target_epoch, 
                                         api_key = api_key, 
                                         data_source = data_source)
  }
  
  validator_stake <- unique(validator_stake) 
  
  # consider overwrite 
  if(overwrite_save){
    saveRDS(validator_stake, "003_latest_save.rds")
    return("Data updated & available by GET")
  } else {
    # WARNING: large file 
  return(validator_stake)
  }
}

#* Return 
#* @serializer rds
#* @get /saved_validator_staker_stats
function(){
  if(!file.exists("003_latest_save.rds")){
    stop("No 003_latest_save.rds found in server, Post validator_staker_stats with overwrite_save = TRUE")
  } else {
    rds_object <- readRDS("003_latest_save.rds")
    return(rds_object)
  }
}

# Validator Vote Data ----

#* Return 
#' @param target_epoch Solana Epoch to get validator vote stats up to that epoch.
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake, for specific API keys, "data-science" is a valid segmented warehouse.
#' @param overwrite_save Default FALSE, overwrites saved data (those available via GET)
#' @param from_previous Default TRUE, re-use available data to only infill new data
#* @post /validator_vote_stats
function(target_epoch, api_key, data_source = "data-science", overwrite_save = FALSE, from_previous = TRUE) {
  
  # see 005_validator_vote_stats.R
  
  if(from_previous){
    prev_validator_vote <- readRDS("005_latest_save.rds")
    validator_vote <- get_validator_vote(min_epoch = max(prev_validator_vote$epoch)-1,
                                           target_epoch = target_epoch, 
                                           api_key = api_key, 
                                           data_source = data_source)
    validator_vote$X__row_index <- NULL
    
    prev_validator_vote <- prev_validator_vote[ , colnames(validator_vote)]
    validator_vote <- rbind(prev_validator_vote, validator_vote)
    
  } else {
    validator_vote <- get_validator_vote(min_epoch = 0, 
                                           target_epoch = target_epoch, 
                                           api_key = api_key, 
                                           data_source = data_source)
  }
  
  validator_vote <- unique(validator_vote)
  
  # consider overwrite 
  if(overwrite_save){
    saveRDS(validator_vote, "005_latest_save.rds")
    return("Data updated & available by GET")
  } else {
    # WARNING: large file 
  return(validator_vote)
  }
}

#* Return 
#* @serializer rds
#* @get /saved_validator_vote_stats
function(){
  if(!file.exists("005_latest_save.rds")){
    stop("No 005_latest_save.rds found in server, Post validator_vote_stats with overwrite_save = TRUE")
  } else {
    rds_object <- readRDS("005_latest_save.rds")
    return(rds_object)
  }
}

# Transformed Data ----

#* Return 
#' @param overwrite_save Default FALSE, overwrites saved data (those available via GET)
#* @post /transform_data
function(overwrite_save = FALSE){
  
  # Get latest RDS from API 
  
  all.data <-  httr::GET("https://science.flipsidecrypto.xyz/sol-vscore-api/saved_ecosystem_appdata") %>% 
    content() %>% unserialize() %>% data.table()
  
  voter_coordinate <- all.data[ , c("epoch","voter_pubkey", "longitude", "latitude")]
  voter_country <- all.data[ , c("epoch","voter_pubkey", "country")]
  
  
  validator.stake <-  httr::GET("https://science.flipsidecrypto.xyz/sol-vscore-api/saved_validator_staker_stats") %>% 
    content() %>% unserialize() %>% data.table()
  
  validator.vote <- httr::GET("https://science.flipsidecrypto.xyz/sol-vscore-api/saved_validator_vote_stats") %>% 
    content() %>% unserialize() %>% data.table()
  
  token.gini <- get_gini_currents(all.data) %>% data.table() %>% .[!is.na(gini)]
  country.gini <- get_current_gini_by_country(all.data) %>% data.table() %>% .[!is.na(country_gini)]
  
  # add in name:
  all.data[, display_name := ifelse(!is.na(validator_name), 
                                    validator_name,
                                    paste0(substr(voter_pubkey, 1, 6), "...", substr(voter_pubkey, 39, 44)))]
  all.data[, active := as.numeric(!delinquent)]
  
  # fix some long country names:
  all.data[country == "U.K. of Great Britain and Northern Ireland", country := "United Kingdom"]
  all.data[country == "United States of America", country := "USA"]
  
  all.data[, age_this_epoch := epoch - min(epoch), by = voter_pubkey]
  all.data[, first_epoch := min(epoch), by = voter_pubkey]
  
  current.epoch <- max(all.data[, .N, by = epoch][N > 100]$epoch)
  last.10.epochs <- sort(unique(all.data[epoch <= current.epoch & epoch > current.epoch - 10]$epoch))
  
  n.active.vals <- all.data[active == 1 & epoch == current.epoch, uniqueN(voter_pubkey)]
  n.inactive.vals <- all.data[active == 0 & epoch == current.epoch, uniqueN(voter_pubkey)]
  
  nval.by.epoch <- all.data[, list(n_validators = uniqueN(voter_pubkey)), 
                            by = list(block_producing = ifelse(active == 1, "Block Producing", "Inactive"), epoch)]
  
  nval.by.epoch.plot <- plot_ly(nval.by.epoch, x = ~epoch, y = ~n_validators, color = ~block_producing,
                                colors = c("#9945FF", "#BD8FFF"), type = "bar", 
                                marker = list(line = list(width = 1)),
                                text = ~paste0("Epoch: ", epoch, "<br>",
                                               "Block Producing Validators: ", n_validators[block_producing == "Block Producing"], "<br>",
                                               "Inactive Validators: ", n_validators[block_producing == "Inactive"]),
                                hoverinfo = "text") %>%
    layout(xaxis = list(title = "Epoch"),
           yaxis = list(title = "Number of Validators"),
           barmode = "stack",
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           legend = list(orientation = "h", y = 1.1),
           font = list(family = "Roboto Mono", size = 12))
  
  
  total.stake <- all.data[epoch == current.epoch, sum(sol_stake)]
  avg.stake <- all.data[epoch == current.epoch, mean(sol_stake)]
  
  stake.plot.data <- all.data[, list(total_sol_staked = sum(sol_stake), avg_sol_staked = mean(sol_stake)), 
                              by = epoch]
  
  avg.stake.plot.data <- rbind(stake.plot.data[, list(epoch, block_producing = "total", avg_sol_staked)],
                               all.data[, list(avg_sol_staked = mean(sol_stake)), 
                                        by = list(epoch, block_producing = ifelse(active == 1, "Block Producing", "Inactive"))]) %>%
    .[avg_sol_staked > 0]
  
  
  total.stake.plot <- plot_ly(stake.plot.data, x = ~epoch, y = ~total_sol_staked,
                              type = "bar", 
                              marker = list(color = "#9945FF"),
                              hovertext = ~paste0("Epoch: ", epoch, "<br>",
                                                  "Total SOL Staked: ", format(round(total_sol_staked), big.mark = ",")),
                              hoverinfo = "text") %>%
    layout(xaxis = list(title = "Epoch"),
           yaxis = list(title = "Number of Validators"),
           barmode = "stack",
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           legend = list(orientation = "h", y = 1.1),
           font = list(family = "Roboto Mono", size = 12))
  
  avg.stake.plot <- plot_ly(avg.stake.plot.data[order(epoch, block_producing)], 
                            x = ~epoch, y = ~avg_sol_staked, color = ~block_producing, colors = c("#14F195", "#9945FF", "black"),
                            text = ~paste("Epoch:", epoch, "<br>Block Producing:", block_producing, "<br>Avg SOL Staked:", avg_sol_staked),
                            type = "scatter", mode = "lines", hoverinfo = "text") %>% 
    layout(xaxis = list(title = "Epoch"),
           yaxis = list(title = "Average SOL Staked"),
           showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center"),
           hovermode = "closest",
           title = list(text = ""),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           font = list(family = "Roboto Mono", size = 12))
  #line = list(dash = c("solid", "dot", "dot"), width = c(2, 1, 1)))
  
  # validator age
  # delegator counts
  overview.vals <- merge(validator.stake[, list(first_epoch = min(epoch), age = current.epoch - min(epoch)), by = voter_pubkey],
                         validator.stake[epoch == current.epoch, list(voter_pubkey, sol_staked, nstakers, avg_stake_size)],
                         by = "voter_pubkey")
  
  
  overview.vals <- merge(overview.vals,
                         all.data[epoch == current.epoch, list(voter_pubkey, display_name)],
                         by = "voter_pubkey")
  overview.vals[, equal := 1]
  
  # Validator Age & Newness
  # age over time by total and active
  avg.age.data <- rbind(
    all.data[, list(block_producing = "All", avg_age = round(mean(age_this_epoch))), 
             by = list(epoch) ],
    all.data[, list(avg_age = round(mean(age_this_epoch))), 
             by = list(epoch, block_producing = ifelse(active == 1, "Block Producing", "Inactive")) ] )
  
  avg.age.plot <- plot_ly(avg.age.data[order(epoch, block_producing)], 
                          x = ~epoch, y = ~avg_age, color = ~block_producing, colors = c("#14F195", "#9945FF", "black"),
                          text = ~paste("Epoch:", epoch, "<br>Block Producing:", block_producing, "<br>Avg Age (Epochs):", avg_age),
                          type = "scatter", mode = "lines", hoverinfo = "text") %>% 
    layout(xaxis = list(title = "Epoch"),
           yaxis = list(title = "Average Age in Epochs"),
           showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center"),
           hovermode = "closest",
           title = list(text = ""),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           font = list(family = "Roboto Mono", size = 12))
  
  new.vals.data <- all.data[age_this_epoch == 0, 
                            list(n_new_validators = .N, 
                                 new_stake = round(sum(sol_stake))), by = epoch]
  
  new.vals.plot <- plot_ly(new.vals.data, x = ~epoch, y = ~n_new_validators,
                           type = "bar", 
                           marker = list(color = "#9945FF"),
                           hovertext = ~paste0("Epoch: ", epoch, "<br>",
                                               "# New Validators: ", n_new_validators, "<br>",
                                               "Total New Staked: ", format(new_stake, big.mark = ",")) ) %>%
    layout(xaxis = list(title = "Epoch"),
           yaxis = list(title = "Number of Validators"),
           barmode = "stack",
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           legend = list(orientation = "h", y = 1.1),
           font = list(family = "Roboto Mono", size = 12))
  
  # decentralization --- 
  
  # gini's over time
  all.gini <- merge(token.gini, country.gini, by = "epoch") %>% melt.data.table(id.vars = "epoch")
  all.gini[, variable := ifelse(variable == "gini", "by SOL Staked", "by Country")]
  
  gini.time.plot <- plot_ly(all.gini, 
                            x = ~epoch, y = ~value, color = ~variable, colors = c("#14F195", "#9945FF"),
                            hovertext = ~paste("Epoch:", epoch, "<br>GINI: ", value),
                            type = "scatter", mode = "lines") %>% 
    layout(xaxis = list(title = "Epoch"),
           yaxis = list(title = "Gini Coefficient"),
           showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center"),
           hovermode = "closest",
           title = list(text = ""),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           font = list(family = "Roboto Mono", size = 12))
  
  token.nakamoto <- get_nakamoto_currents(all.data) %>% data.table() %>% .[!is.na(nakamoto)]
  country.nakamoto <- get_current_nakamoto_by_country(all.data) %>% data.table() %>% .[!is.na(country_nakamoto)]
  
  all.nakamoto <- merge(token.nakamoto, country.nakamoto, by = "epoch") %>% 
    melt.data.table(id.vars = "epoch")
  
  all.nakamoto[, variable := ifelse(variable == "nakamoto", "by SOL Staked", "by Country")]
  
  nakamoto.time.plot <- plot_ly(all.nakamoto, 
                                x = ~epoch, y = ~value, color = ~variable, colors = c("#14F195", "#9945FF"),
                                hovertext = ~paste("Epoch:", epoch, "<br>GINI: ", value),
                                type = "scatter", mode = "lines") %>% 
    layout(xaxis = list(title = "Epoch"),
           yaxis = list(title = "Nakamoto Coefficient"),
           showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center"),
           hovermode = "closest",
           title = list(text = ""),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           font = list(family = "Roboto Mono", size = 12))
  
  
  
  
  # THE MAP
  
  # validator % of total vs. staker gini (current epoch)
  current.val.data <- validator.stake[epoch == current.epoch]
  current.val.data[, sol_percent := sum(sol_staked)/sum(current.val.data$sol_staked)*100, by = voter_pubkey]
  
  staker.gini.vs.prop.plot <- plot_ly(current.val.data, 
                                      x = ~sol_percent, 
                                      y = ~gini_coefficient, 
                                      type = "scatter", mode = "markers",
                                      marker = list(color = "#9945FF")) %>%
    layout(xaxis = list(title = "% of Total SOL Staked", type = "log"),
           yaxis = list(title = "Gini", type = "log"),
           showlegend = FALSE,
           #hovermode = "closest",
           #title = list(text = ""),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           font = list(family = "Roboto Mono", size = 12))
  
  # like a histogram with within validator gini??
  staker.val.gini.distr <- plot_ly(data = current.val.data, x = ~gini_coefficient, 
                                   type = "histogram",
                                   marker = list(color = "#9945FF")) %>%
    layout(xaxis = list(title = "Gini for # Stakers with Validators"),
           yaxis = list(title = "# of Validators"),
           showlegend = FALSE,
           #hovermode = "closest",
           #title = list(text = ""),
           plot_bgcolor = "transparent", 
           paper_bgcolor = "transparent", 
           font = list(family = "Roboto Mono", size = 12))
  
  
  
  # main page metrics:
  
  met.total.stake <- paste0(round(total.stake/1000000, 2), "MM")
  "Total SOL Staked"
  
  met.active.rate <- paste0(round(mean(all.data[epoch == current.epoch]$active)*100), "%")
  "Validator Active Rate"
  
  met.active10.rate <- paste0(round(mean(all.data[epoch %in% last.10.epochs, max(active), by = voter_pubkey]$V1)*100), "%")
  "10 Epoch Active Rate"
  
  tmp.net.sol <- sum(all.data[epoch == current.epoch]$sol_stake) - sum(all.data[epoch == (current.epoch-1)]$sol_stake)
  if(tmp.net.sol == 0) {
    met.net.sol <- "━"
  } else if(tmp.net.sol < 0) {
    met.net.sol <- paste0(format(tmp.net.sol, big.mark = ","))
  } else {
    met.net.sol <- paste0("+", format(tmp.net.sol, big.mark = ","))
  }
  "Net SOL Staked"
  
  tmp.net.validators <- uniqueN(all.data[epoch == current.epoch]$voter_pubkey) - uniqueN(all.data[epoch == (current.epoch-1)]$voter_pubkey)
  if(tmp.net.validators == 0) {
    met.net.validators <- "━"
  } else if(tmp.net.validators < 0) {
    met.net.validators <- tmp.net.validators
  } else {
    met.net.validators <- paste0("+", tmp.net.validators)
  }
  "Net Validators"
  
  
  met.nakamoto.token <- all.nakamoto[epoch == current.epoch & variable == "by SOL Staked"]$value
  "Nakamoto (Token)"
  met.nakamoto.country <- all.nakamoto[epoch == current.epoch & variable == "by Country"]$value
  "Nakamoto (Country)"
  met.gini.token <- round(all.gini[epoch == current.epoch & variable == "by SOL Staked"]$value, 2)
  "Gini (Token)"
  met.gini.country <- round(all.gini[epoch == current.epoch & variable == "by Country"]$value, 2)
  "Gini (Country)"
  met.staker.gini <- round(mean(validator.stake[epoch == current.epoch]$gini_coefficient, na.rm = TRUE), 2)
  "Avg Staker Gini"
  
  validator_stake_coords <- add_coordinate_to_validator(validator_lvl_data = validator.stake, 
                                                        voter_coordinate, 
                                                        select_epoch = current.epoch)
  validator_stake_coords$label <- paste0("<b>Validator ...", 
                                         substr(validator_stake_coords$voter_pubkey, start = 40, stop = 44), 
                                         "</b><br># Stakers: ",
                                         validator_stake_coords$nstakers, 
                                         "<br>Total Stake: ",
                                         format(floor(validator_stake_coords$sol_staked),
                                                big.mark = ","),
                                         
                                         '<br><a href="',
                                         'https://solanacompass.com/validators/',
                                         validator_stake_coords$voter_pubkey,
                                         '">', 
                                         'Compass Link',
                                         '</a>'
  )
  
  # new vals by epoch + total new stake from them
  all_outputs <-  list(current.epoch = current.epoch, 
                       n.active.vals = n.active.vals,
                       n.inactive.vals = n.inactive.vals,
                       nval.by.epoch.plot = nval.by.epoch.plot,
                       total.stake = total.stake,
                       avg.stake =avg.stake,
                       total.stake.plot = total.stake.plot,
                       avg.stake.plot = avg.stake.plot,
                       met.total.stake = met.total.stake, 
                       met.active.rate = met.active.rate,
                       met.active10.rate =met.active10.rate,
                       met.net.sol =met.net.sol,
                       met.net.validators = met.net.validators,
                       met.nakamoto.token = met.nakamoto.token,
                       met.nakamoto.country = met.nakamoto.country,
                       met.gini.token = met.gini.token,
                       met.gini.country = met.gini.country,
                       met.staker.gini = met.staker.gini,
                       new.vals.plot = new.vals.plot,
                       avg.age.plot = avg.age.plot,
                       overview.vals = overview.vals,
                       gini.time.plot = gini.time.plot,
                       nakamoto.time.plot = nakamoto.time.plot,
                       staker.gini.vs.prop.plot = staker.gini.vs.prop.plot,
                       staker.val.gini.distr = staker.val.gini.distr,
                       validator_stake_coords = validator_stake_coords
  )
  
  if(overwrite_save){
    saveRDS(all_outputs, "all_outputs.rds")
    return("Data Transformed & available by GET")
  } else {
    # warning variety of file-type objects 
    return(all_outputs)
  }
  
}

#* Return 
#* @serializer rds
#* @get /saved_transformed_data
function(){
  if(!file.exists("all_outputs.rds")){
    stop("No all_outputs.rds found in server, Post transform_data with overwrite_save = TRUE")
  } else {
    rds_object <- readRDS("all_outputs.rds")
    return(rds_object)
  }
}

