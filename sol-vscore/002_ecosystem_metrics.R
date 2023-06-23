library(dplyr)
library(plotly)

# for testing pass to metric functions 
ecoappdata <- readRDS("001_latest_save.rds")

# Functions ---- 

num_sol_staked <- function(ecoappdata){
   ecoappdata %>% 
    group_by(epoch) %>% 
    summarise(
    total_stake = sum(sol_stake), 
    n_validators = length(unique(voter_pubkey)),
    current_stake = sum( sol_stake * (delinquent == FALSE) ),
    n_current_validators = sum(delinquent == FALSE),
    delinquent_stake = sum( sol_stake * (delinquent == TRUE) ),
    n_delinquent_validators = sum(delinquent == TRUE)
    )
}

# If you loosen current to active at least 1 epoch in last 10 epochs counts as current.
num_sol_staked_last10 <- function(ecoappdata){
 
  ecoappdata %>% 
    group_by(epoch) %>% 
    summarise(
      total_stake = sum(sol_stake), 
      n_validators = length(unique(voter_pubkey)),
      recently_active_stake = sum( sol_stake * (count_active_last10 > 0) ),
      n_recent_active_validators = sum(count_active_last10 > 0),
      delinquent10_stake = sum( sol_stake * (count_active_last10 == 0) ),
      n_delinquent10_validators = sum(count_active_last10 == 0)
    )
  
}

num_sol_staked_by_country <- function(ecoappdata){
  ecoappdata %>% 
    group_by(epoch, country) %>% 
    summarise(
      total_stake = sum(sol_stake), 
      n_validators = length(unique(voter_pubkey)),
      current_stake = sum( sol_stake * (delinquent == FALSE) ),
      n_current_validators = sum(delinquent == FALSE),
      delinquent_stake = sum( sol_stake * (delinquent == TRUE) ),
      n_delinquent_validators = sum(delinquent == TRUE)
    )
}

ecosystem_10epoch_churn <- function(ecoappdata){
  eco_stats <- num_sol_staked(ecoappdata)
  
  churn_rate <- rep(0, length(eco_stats$current_stake))
  
  for(i in 10:length(eco_stats$current_stake)){
  temp <- sum(abs(diff(eco_stats$current_stake[(i-9):i])))/
            (eco_stats$current_stake[i])
  
  if(!is.na(temp) & !is.infinite(temp) & !is.nan(temp)){
    churn_rate[i] <- temp
  }
  }
  
  eco_stats$churn_rate <- churn_rate*100
  return(eco_stats)
  
  }

get_nakamoto <- function(values){
    vec <- sort(values,decreasing = TRUE)
  cumv <- cumsum(vec)
  return(
    which(cumv > (sum(vec) * 0.5))[1]
  )
}

get_nakamoto_currents <- function(ecoappdata){
  ecoappdata %>% 
    group_by(epoch) %>% 
    filter(delinquent == FALSE) %>% 
    summarise(
      nakamoto = get_nakamoto(sol_stake)
    )
}

get_gini <- function(values){
  mv <- mean(values)
  abs_diff <- abs(values-mv)
  mad <- mean(abs_diff)
  return(mad/mv/2)
}

get_gini_currents <- function(ecoappdata){
  
  ecoappdata %>% 
    group_by(epoch) %>% 
    filter(delinquent == FALSE) %>% 
    summarise(
      gini = get_gini(sol_stake)
    )
}

get_gini_currents <- function(ecoappdata){
 
  ecoappdata %>% 
    group_by(epoch) %>% 
    filter(delinquent == FALSE) %>% 
    summarise(
      gini = get_gini(sol_stake)
    )
}

get_gini_recent <- function(ecoappdata){
    ecoappdata %>% 
    group_by(epoch) %>% 
    filter(count_active_last10 > 0) %>% 
    summarise(
      gini = get_gini(sol_stake)
    )
}

get_current_gini_by_country <- function(ecoappdata){
  ecoappdata %>% 
    dplyr::filter(delinquent == FALSE) %>% 
    group_by(epoch, country) %>% 
    summarise(stake = sum(sol_stake)) %>% 
    group_by(epoch) %>% 
    summarise(
      country_gini = get_gini(stake)
    )
  
 
}

get_current_cdf_stats <- function(ecoappdata){
  current <- ecoappdata %>% dplyr::filter(delinquent == FALSE)
  
  # Gini defined as 1/2 the relative mean absolute difference 
  # this is better for discrete values; slightly different than integral approach
  # e.g., ineq::Gini but similar.
  
  # cdf 
  cdf_intervals <- seq(from = 50000, to = 10000000, by = 10000) 
  
  data.frame(
    reference_sol_stake = cdf_intervals, 
    cdf_value = ecdf(current$sol_stake)(cdf_intervals)
  )
}

plot_current_cdf <- function(ecoappdata, title = "~65% of Validators have between 70K-80K Sol Staked"){
  
  plotly::plot_ly(data = get_current_cdf_stats(ecoappdata),
          x = ~reference_sol_stake, 
          y = ~cdf_value, 
          type = 'scatter', mode = 'lines+markers') %>% 
    layout(xaxis = list(title = "# SOL Staked", 
                        titlefont = list(size = 18)),
           yaxis = list(title = "Cumulative % of Voter Pubkey",
                        titlefont = list(size = 18)),
           title = list(
             text = title, 
             font = list(size = 18),
             y = 0.95)
    )
}

get_current_software_stats <- function(ecoappdata){
  ecoappdata %>% filter(delinquent == FALSE) %>%  
    group_by(epoch) %>% 
    mutate(
      maxsoftware = max(modified_software_version)) %>% 
    group_by(epoch, modified_software_version) %>% 
    summarise(n = n(), 
              ismax = (modified_software_version == maxsoftware)) %>% 
    unique()
  
}

# Reference Objects ----

ecosystem_sol_stake_stats_by_epoch <- num_sol_staked(ecoappdata)
ecosystem_sol_stake_stats_by_epoch_with_churn <- ecosystem_10epoch_churn(ecoappdata)

ecosystem_sol_stake_stats_by_epoch_recently_active <- num_sol_staked_last10(ecoappdata)
ecosystem_sol_stake_stats_by_epoch_country <- num_sol_staked_by_country(ecoappdata)

ecosystem_gini_by_epoch <- get_gini_currents(ecoappdata)
ecosystem_nakamoto_by_epoch <- get_nakamoto_currents(ecoappdata)
ecosystem_gini_by_epoch_recently_active <- get_gini_recent(ecoappdata)
ecosystem_gini_by_epoch_country <- get_current_gini_by_country(ecoappdata)

ecosystem_software_stats_by_epoch <- get_current_software_stats(ecoappdata)

# Plots ----

plot_epoch_solstake <- function(ecosystem_sol_stake_stats_by_epoch, title = ""){
  plotly::plot_ly(data = ecosystem_sol_stake_stats_by_epoch,
                  x = ~epoch, 
                  y = ~total_stake, 
                  type = 'scatter', mode = 'lines+markers') %>% 
    layout(xaxis = list(title = "EPOCH", 
                        titlefont = list(size = 18)),
           yaxis = list(title = "# SOL Stake",
                        titlefont = list(size = 18)),
           title = list(
             text = title, 
             font = list(size = 18),
             y = 0.95)
    )
}

