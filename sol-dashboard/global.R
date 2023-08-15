library(shiny)
library(data.table)
library(reactable)
library(shinyWidgets)
library(plotly)
library(leaflet)

source(file = "helper_functions.R")

# Cannot read RDS from Plumber API 
# because Plumber serialization 50x the file size for some reason

# returns a list (have to do it this way for serialization)
all.data <- readRDS("all_outputs.rds")

# Go through list and create the object with its own name
lapply(names(all.data), function(i){
  assign(x = i, value = all.data[[i]], envir = parent.env(environment()))
})


corr.plot.choices <- data.table(
  table_name = c("first_epoch", "age", "sol_staked", "nstakers", "avg_stake_size", "equal"),
  selection_name = c("First Epoch", "Age in Epochs", "SOL Staked", "# Stakers", "Avg Stake Size", "Equal"),
  log_axis = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))

plotly.height <- "300px"
plotly.width <- "90%"
