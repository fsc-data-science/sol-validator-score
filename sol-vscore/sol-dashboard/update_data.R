library(dplyr)
#setwd("../solana-validators/")
# for testing pass to metric functions 

setwd("..")
source("002_ecosystem_metrics.R")
source("004_validator_stake_metrics.R")
source("006_validator_vote_metrics.R")

all.data <- readRDS("001_latest_save.rds") %>% data.table()
validator.stake <- readRDS("003_latest_save.rds") %>% data.table()
validator.vote <- readRDS("005_latest_save.rds") %>% data.table()

setwd("sol-dashboard/")

token.gini <- get_gini_currents(ecoappdata) %>% data.table() %>% .[!is.na(gini)]
country.gini <- get_current_gini_by_country(ecoappdata) %>% data.table() %>% .[!is.na(country_gini)]


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
current.epoch <- 461
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
overview.vals <- merge(validator.data[, list(first_epoch = min(epoch), age = current.epoch - min(epoch)), by = voter_pubkey],
                       validator.data[epoch == current.epoch, list(voter_pubkey, sol_staked, nstakers, avg_stake_size)],
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


# retention
retention.data <- all.data[, list(prop_active = mean(active), sol_active = sum(sol_stake*active)), by = list(epoch, first_epoch)]
retention.data[, age_in_epochs := epoch - first_epoch]
setorder(retention.data, age_in_epochs, first_epoch)

#retention.activity.plot <- 
plot_ly(retention.data[prop_active != 0 & sol_active != 0 & first_epoch != 338], 
        x = ~age_in_epochs, y = ~prop_active, color = ~factor(first_epoch),
        hovertext = ~paste("TBD"),
        type = "scatter", mode = "lines") %>% 
  layout(xaxis = list(title = "Epoch"),
         yaxis = list(title = "Average Age in Epochs"),
         showlegend = FALSE,
         hovermode = "closest",
         title = list(text = ""),
         plot_bgcolor = "transparent", 
         paper_bgcolor = "transparent", 
         font = list(family = "Roboto Mono", size = 12))



# decentralization!

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

token.nakamoto <- get_nakamoto_currents(ecoappdata) %>% data.table() %>% .[!is.na(nakamoto)]
country.nakamoto <- get_current_nakamoto_by_country(ecoappdata) %>% data.table() %>% .[!is.na(country_nakamoto)]

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
current.val.data <- validator.data[epoch == current.epoch]
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
met.staker.gini <- round(mean(validator.data[epoch == current.epoch]$gini_coefficient, na.rm = TRUE), 2)
"Avg Staker Gini"


# new vals by epoch + total new stake from them
save(current.epoch, 
     n.active.vals, n.inactive.vals, nval.by.epoch.plot, 
     total.stake, avg.stake, total.stake.plot, avg.stake.plot,
     met.total.stake, met.active.rate, met.active10.rate, met.net.sol, met.net.validators,
     
     new.vals.plot, avg.age.plot,
     # decentralization
     gini.time.plot, nakamoto.time.plot,
     staker.gini.vs.prop.plot,
     staker.val.gini.distr,
     validator_stake_coords_463,
     file = "data.RData")


# Reference Objects ----
ecosystem_sol_stake_stats_by_epoch <- num_sol_staked(ecoappdata) %>% data.table()
ecosystem_sol_stake_stats_by_epoch_with_churn <- ecosystem_10epoch_churn(ecoappdata) %>% data.table()

ecosystem_sol_stake_stats_by_epoch_recently_active <- num_sol_staked_last10(ecoappdata) %>% data.table()
ecosystem_sol_stake_stats_by_epoch_country <- num_sol_staked_by_country(ecoappdata) %>% data.table()

ecosystem_gini_by_epoch <- get_gini_currents(ecoappdata) %>% data.table()
ecosystem_gini_by_epoch_recently_active <- get_gini_recent(ecoappdata) %>% data.table()
ecosystem_gini_by_epoch_country <- get_current_gini_by_country(ecoappdata) %>% data.table()

ecosystem_software_stats_by_epoch <- get_current_software_stats(ecoappdata) %>% data.table()

validator_10epoch_staker_churn <- get_10epoch_staker_churn(validator_stake) %>% data.table()
validator_10epoch_sol_stake_churn <- get_10epoch_sol_churn(validator_stake) %>% data.table()

vote_rolling_attendance <- get_vote_avg10_attendance(validator_vote) %>% data.table()
vote_profile <- get_vote_profile(validator_vote) %>% data.table()

validator.data <- validator_stake %>% data.table()

# //Reference Objects ----


# now we do some crunching and formatting
ecosystem_current_epoch <- ecosystem_sol_stake_stats_by_epoch[epoch == current.epoch]
ecosystem_current_epoch[, sol_staked := paste0(round(total_stake/1000000), "MM")]

ecosystem_current_epoch[, gini_token := round(ecosystem_gini_by_epoch[epoch == current.epoch]$gini, 2)]
ecosystem_current_epoch[, gini_geo := round(ecosystem_gini_by_epoch_country[epoch == current.epoch]$country_gini, 2)]

ecosystem_current_epoch[, active_rate_10 := round(ecosystem_sol_stake_stats_by_epoch_with_churn[epoch == current.epoch, list(active_rate = n_current_validators / n_validators)], 2)]
ecosystem_current_epoch[, churn_10 := round(ecosystem_sol_stake_stats_by_epoch_with_churn[epoch == current.epoch]$churn_rate, 2)]




last.10.any.active <- unique(all.data[epoch <= current.epoch & epoch > current.epoch - 10 & delinquent == FALSE]$voter_pubkey)


validator.pubkeys <- unique(all.data[epoch <= current.epoch]$voter_pubkey)
data.first.epoch <- min(all.data$epoch)

#i <- "646hXimsiNv2wsTm8hSXpuVCYHePXoaTz5gbqxMo9s6N"
all.data[, active := as.numeric(!delinquent == TRUE)]

# add the correct node software to the epoch:
all.data <- merge(all.data,
                  ecosystem_software_stats_by_epoch[ismax == TRUE, list(epoch, max_software = modified_software_version)],
                  by = "epoch", all.x = TRUE)
# make a list of all software versions in order
all.versions <- sort(unique(all.data$max_software))

current.epoch.total.staked <- sum(all.data[epoch == current.epoch]$sol_stake)

# we're going to loop through every validator (!) and make the data perfect
by.validator.output <- lapply(validator.pubkeys, function(i) {
  
  print(i)
  
  this.val <- all.data[voter_pubkey == i]
  setorder(this.val, epoch)
  
  val.last.epoch <- max(this.val$epoch)
  
  # what kind of validator is this?
  # first epoch
  val.info <- data.table(validator_status = ifelse(val.last.epoch != current.epoch, "Disabled", "Current"),
                         first_epoch = ifelse(min(this.val$epoch) == data.first.epoch, paste0("<", data.first.epoch), min(this.val$epoch)),
                         last_active_epoch = ifelse(max(this.val$epoch) == current.epoch, paste0("This Epoch"), max(this.val$epoch)),
                         prop_active_epochs = paste0(round(mean(!this.val$delinquent)*100,1),"%"),
                         attendence_last_10 = sum(!all.data[epoch %in% last.10.epochs & voter_pubkey == i]$delinquent),
                         rolling_10_attendance = vote_rolling_attendance[epoch == current.epoch & voter_pubkey == i]$avg_10epoch_attendance,
                         avg_per_slots_skipped = vote_rolling_attendance[epoch == current.epoch & voter_pubkey == i]$percent_slots_skipped_in_epoch
  )
  
  
  # now lets talk moneys
  val.info[, total_staked := ifelse(current.epoch == val.last.epoch, round(this.val[epoch == current.epoch]$sol_stake), 0)]
  val.info[, staked_prop  := ifelse(current.epoch == val.last.epoch, 
                                    round(this.val[epoch == current.epoch]$sol_stake/current.epoch.total.staked, 4), 0)]
  val.info[, max_staked   := paste0("Max Stake: ", format(round(this.val[sol_stake == max(sol_stake)]$sol_stake[1]), big.mark = ","), " SOL at Epoch ", min(this.val[sol_stake == max(sol_stake)]$epoch))]
  
  # are they on the current node?
  # how long do they usually take to upgrade their node?
  
  # Get the current software version and check if it's up to date
  current_version <- this.val[epoch == val.last.epoch, modified_software_version]
  is_up_to_date <- current_version == this.val[epoch == val.last.epoch, max_software]
  
  # Check if the current version is up to date
  if(is_up_to_date) {
    text_string <- paste("Node Version: Current (", current_version, ")")
  } else {
    # Calculate the number of versions behind
    versions_behind <- length(all.versions) - which(all.versions == current_version)
    
    # Get the last updated epoch
    last_updated_epoch <- max(this.val[modified_software_version != current_version & epoch <= current.epoch, epoch])
    
    # Generate the text string
    text_string <- paste0("Node Version: ", current_version, " (", versions_behind, " versions behind, last update in epoch ", last_updated_epoch, ")")
  }
  
  val.info[, node_version := current_version]
  val.info[, node_current := current_version]
  val.info[, node_uptodate := text_string]
  
  # where are they located? have they moved around?
  c.location = this.val[epoch == current.epoch]$country
  
  val.info[, current_location := paste0("Location: ", c.location, " (for ", 
                                        length(this.val[!is.na(country)]$country) - max(which(this.val[!is.na(country)]$country != c.location)),
                                        " epochs)")]
  p.locations <- this.val[, .N, by = country][country != c.location][order(-N)]
  
  if(nrow(p.locations) > 1) {
    val.info[, previous_locations := paste0("Previous Locations (#epochs): ", 
                                            paste(paste0(p.locations$country, " (", p.locations$N, ")"), collapse = ", "))]  
  } else {
    val.info[, previous_locations := "Previous Locations: none"]
  }
  
  # validator stake metrics (delegators)
  validator_10epoch_staker_churn
  
  val.info[]
  
  
  # --PLOTS!
  # -- money graph
  # Create a color mapping based on the "active" column
  this.val[, color := ifelse(active == 1, "#9945FF", "#BD8FFF")]
  
  # Plotting
  fig <- plot_ly(this.val, x = ~epoch, y = ~sol_stake, type = "bar",
                 marker = list(color = ~color),
                 hovertemplate = ~paste("Epoch:", epoch, "<br>SOL Staked:", sol_stake, "<br>Active:", ifelse(active == 1, "Yes", "No")))
  
  sol.staked.plot <- 
    fig %>% layout(
      font = list(family = "Roboto Mono", size = 12),
      xaxis = list(title = "Epoch"),
      yaxis = list(title = "SOL Staked"),
      showlegend = FALSE,
      hovermode = "closest",
      autosize = FALSE,
      plot_bgcolor  = "transparent",
      paper_bgcolor = "transparent",
      margin = list(l = 75, r = 50, b = 75, t = 50, pad = 4),
      modebar = list(remove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
                                "hoverClosestCartesian", "hoverCompareCartesian")),
      bargap = 0,
      legend = list(x = 0.5, y = 1.1, orientation = "h")
    )
  
  
  
  last.10 <- data.table(epoch = last.10.epochs,
                        active = this.val[epoch <= current.epoch & epoch > current.epoch - 10 & delinquent == FALSE,
                                          as.numeric(last.10.epochs %in% epoch)])
  # Define symbols based on activity
  symbols <- ifelse(last.10$active == 1, "circle", "circle-open")
  
  last.10.plot <- plot_ly() %>%
    add_trace(
      data = last.10, 
      x = ~epoch, y = ~rep(1, nrow(last.10)),  # set y to 1 for all rows
      type = 'scatter', mode = 'markers',
      marker = list(symbol = symbols, size = 15, color = "black"),
      hoverinfo = 'text',
      hovertext = ~paste('Epoch', epoch, ':', ifelse(active == 1, 'Active', 'Not Active'))
    ) %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),  # hide x axis labels
      yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, range = c(0.5, 1.5)),
      showlegend = FALSE,
      plot_bgcolor = 'rgba(0,0,0,0)', # transparent background
      hoverlabel = list(bgcolor = 'white', font = list(family = "Roboto Mono")), # hover label background color
      autosize = TRUE,
      margin = list(l = 1, r = 1, b = 1, t = 1),
      # add annotations for each epoch
      annotations = lapply(1:nrow(last.10), function(i) {
        list(
          x = last.10$epoch[i],
          y = 0.95,
          text = last.10$epoch[i],
          xref = 'x',
          yref = 'y',
          font = list(family = 'Roboto Mono', size = 12, color = 'black'),
          showarrow = FALSE
        )
      })
    ) %>%
    config(displayModeBar = FALSE) # disable zoom and other toolbar options
  
  to.return <- list(val.info = val.info,
                    sol.staked.plot = sol.staked.plot,
                    last.10.plot = last.10.plot)
  
  return(to.return)
  
})

names(last.10.plots) <- last.10.any.active

epoch.table.data <- ecosystem_sol_stake_stats_by_epoch_with_churn[epoch <= current.epoch]
epoch.table.data[, prop_current_stake := current_stake / total_stake]
epoch.table.data[, prop_current_validators := n_current_validators / n_validators]

epoch.table.data <- merge(epoch.table.data,
                          ecosystem_gini_by_epoch,
                          by = "epoch", all.x = TRUE)

epoch.table.data <- merge(epoch.table.data,
                          ecosystem_gini_by_epoch_country,
                          by = "epoch", all.x = TRUE)
setorder(epoch.table.data, -epoch)
epoch.table.data[, stake_epoch_delta := c(diff(total_stake), 0)]
epoch.table.data[, validator_epoch_delta := c(diff(n_validators), 0)]

epoch.table.data <- merge(epoch.table.data,
                          ecosystem_software_stats_by_epoch[ismax == TRUE, list(epoch, n_current_node = n)],
                          by = "epoch", all.x = TRUE)

epoch.table <- epoch.table.data[, list(epoch, 
                                       sol_staked = paste0(round(total_stake/1000000), "MM"),
                                       stake_epoch_delta = paste0(round(stake_epoch_delta/1000000, 2), "MM"),
                                       prop_current_stake,
                                       n_validators,
                                       validator_epoch_delta,
                                       prop_current_validators = round(prop_current_validators, 2),
                                       prop_on_max_node = round(n_current_node / n_validators, 3),
                                       churn_rate = round(churn_rate, 2),
                                       gini = round(gini, 2),
                                       country_gini = round(country_gini, 2))]

save(
  validator.table,
  last.10.plots,
  epoch.table,
  ecosystem_current_epoch,
  validator.pubkeys,
  file = "data.RData")








all.data[epoch == current.epoch, .N, by = voter_pubkey][N != 1]
all.data[voter_pubkey == "2ayMCC4aizr8RGg5ptXYqu8uoxW1whNek1hE1zaAd58z" & epoch == current.epoch]




