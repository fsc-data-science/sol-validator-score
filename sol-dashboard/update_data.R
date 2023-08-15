library(dplyr)
library(data.table)
library(httr)
library(sf)
library(plotly)

source("helper_functions.R")

# Get latest RDS from API 

# Get latest RDS from API 

all.data <-  httr::GET("https://science.flipsidecrypto.xyz/sol-vscore-api/saved_ecosystem_appdata") %>% 
  content() %>% unserialize() %>% data.table() %>% unique()

voter_coordinate <- all.data[ , c("epoch","voter_pubkey", "longitude", "latitude")]
voter_country <- all.data[ , c("epoch","voter_pubkey", "country")]


validator.stake <-  httr::GET("https://science.flipsidecrypto.xyz/sol-vscore-api/saved_validator_staker_stats") %>% 
  content() %>% unserialize() %>% data.table() %>% unique()

validator.vote <- httr::GET("https://science.flipsidecrypto.xyz/sol-vscore-api/saved_validator_vote_stats") %>% 
  content() %>% unserialize() %>% data.table() %>% unique()

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

# Saving item as list in RDS is more compressed & able to be passed via APIs 
  saveRDS(all_outputs, "all_outputs.rds")

