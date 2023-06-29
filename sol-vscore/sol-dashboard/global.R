library(shiny)
library(data.table)
library(reactable)
library(shinyWidgets)

#if(Sys.info()[["user"]] == "rstudio-connect") {
load("data.Rdata")

corr.plot.choices <- data.table(
  table_name = c("first_epoch", "age", "sol_staked", "nstakers", "avg_stake_size", "equal"),
  selection_name = c("First Epoch", "Age in Epochs", "SOL Staked", "# Stakers", "Avg Stake Size", "Equal"),
  log_axis = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))


plotly.height <- "300px"
plotly.width <- "90%"
