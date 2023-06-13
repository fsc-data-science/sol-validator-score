library(dplyr)

# for testing pass to metric functions 
ecoappdata <- readRDS("001_latest_save.rds")

num_sol_staked_current <- function(ecoappdata){
  data.frame(
    total = sum(ecoappdata$SOL_STAKE),
    total_count = length(unique(ecoappdata$VOTER_PUBKEY)),
    current = sum(ecoappdata[ecoappdata$DELINQUENT == FALSE, "SOL_STAKE"]),
    current_count = length(unique(ecoappdata$VOTER_PUBKEY[ecoappdata$DELINQUENT == FALSE])),
    delinquent = sum(ecoappdata[ecoappdata$DELINQUENT == TRUE, "SOL_STAKE"]),
    delinquent_count = length(unique(ecoappdata$VOTER_PUBKEY[ecoappdata$DELINQUENT == TRUE])),
    deliquent_stake_percent = sum(ecoappdata[ecoappdata$DELINQUENT == TRUE, "SOL_STAKE"])/sum(ecoappdata$SOL_STAKE) * 100
  )
}

# If you loosen current to active at least 1 epoch in last 10 epochs counts as current.
num_sol_staked_last10 <- function(ecoappdata){
  data.frame(
    total = sum(ecoappdata$SOL_STAKE),
    total_count = length(unique(ecoappdata$VOTER_PUBKEY)),
    stake_among_active_last10 = sum(ecoappdata[ecoappdata$COUNT_ACTIVE_LAST10 > 0, "SOL_STAKE"]),
    active_last10_count = length(unique(ecoappdata$VOTER_PUBKEY[ecoappdata$COUNT_ACTIVE_LAST10 > 0])),
    stake_delinquent_last10 = sum(ecoappdata[ecoappdata$COUNT_ACTIVE_LAST10 == 0, "SOL_STAKE"]),
    delinquent_last10_count = length(unique(ecoappdata$VOTER_PUBKEY[ecoappdata$COUNT_ACTIVE_LAST10 == 0])),
    deliquent_stake_percent = sum(ecoappdata[ecoappdata$COUNT_ACTIVE_LAST10 == 0, "SOL_STAKE"])/sum(ecoappdata$SOL_STAKE) * 100
  )
  
}

get_gini <- function(values){
  mv <- mean(values)
  abs_diff <- abs(values-mv)
  mad <- mean(abs_diff)
  return(mad/mv/2)
}

get_gini_currents <- function(ecoappdata){
  current <- ecoappdata %>% dplyr::filter(DELINQUENT == FALSE)
  get_gini(current$SOL_STAKE)
}

get_current_gini_countries <- function(ecoappdata){
  current <- ecoappdata %>% 
    dplyr::filter(DELINQUENT == FALSE) 
  
  country_lvl <- current %>% 
    group_by(Country) %>% 
    summarise(stake = sum(SOL_STAKE))
  
  get_gini(country_lvl$stake)
 
}

get_current_cdf_stats <- function(ecoappdata){
  current <- ecoappdata %>% dplyr::filter(DELINQUENT == FALSE)
  
  # Gini defined as 1/2 the relative mean absolute difference 
  # this is better for discrete values; slightly different than integral approach
  # e.g., ineq::Gini but similar.
  
  # cdf 
  cdf_intervals <- seq(from = 50000, to = 10000000, by = 10000) 
  
  data.frame(
    reference_sol_stake = cdf_intervals, 
    cdf_value = ecdf(current$SOL_STAKE)(cdf_intervals)
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
  current <- ecoappdata %>% dplyr::filter(DELINQUENT == FALSE)
  current %>% 
    group_by(MODIFIED_SOFTWARE_VERSION) %>% 
    summarise(n = n())
}


