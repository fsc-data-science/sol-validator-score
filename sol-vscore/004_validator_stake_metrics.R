library(dplyr)

# for testing pass to metric functions 
validator_stake <- readRDS("003_latest_save.rds")

# Functions ---- 

rolling_absum <- function(v, k = 10){
  d = rep(0, length(v))
  
  if(length(v) < (k+1)){
  return(d)  
  }
  
  for(i in k:length(v)){
    d[i] <- sum( abs(v[ (i-k+1):i ]))
  }
  return(d)
}

get_10epoch_staker_churn <- function(validator_stake){
  validator_stake %>% group_by(voter_pubkey) %>% 
    dplyr::arrange(epoch) %>% 
    mutate(
      ten_epoch_staker_churn_percent = 100*rolling_absum(num_new_stakers, k = 10)/nstakers
    )
}

get_10epoch_sol_churn <- function(validator_stake){
  validator_stake %>% group_by(voter_pubkey) %>% 
    dplyr::arrange(epoch) %>% 
    mutate(
      ten_epoch_churn_percent = 100*rolling_absum(num_new_sol_staked, k = 10)/sol_staked
    )
}


# Reference Objects ----

validator_10epoch_staker_churn <- get_10epoch_staker_churn(validator_stake)
validator_10epoch_sol_stake_churn <- get_10epoch_sol_churn(validator_stake)




