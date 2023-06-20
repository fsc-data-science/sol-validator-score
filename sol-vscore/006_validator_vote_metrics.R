library(dplyr)

# for testing pass to metric functions 
validator_vote <- readRDS("005_latest_save.rds")

# Functions ---- 
rolling_avg <- function(v, k = 10, default = 100){
  d = rep(default, length(v))
  
  v[is.na(v)] <- default
  
  if(length(v) < (k+1)){
    return(d)  
  }
  
  for(i in k:length(v)){
    d[i] <- mean(v[ (i-k+1):i ])
  }
  return(d)
}

get_vote_avg10_attendance <- function(validator_vote){
  validator_vote %>% 
    group_by(voter_pubkey) %>%
    arrange(epoch) %>% 
    mutate(
      avg_10epoch_attendance = 100 - rolling_avg(percent_on_chosen_fork, k = 10)
    )
}

get_vote_profile <- function(validator_vote){
  validator_vote %>% 
    group_by(voter_pubkey) %>%
    arrange(epoch) %>% 
    summarise(
      earliest_epoch = min(last_active_epoch),
      latest_active_epoch = max(last_active_epoch),
      age = max(last_active_epoch) - min(last_active_epoch),
      experience = sum(active_category == "active")
    )
  
}

# Reference Objects ----

vote_rolling_attendance <- get_vote_avg10_attendance(validator_vote)
vote_profile <- get_vote_profile(validator_vote)
