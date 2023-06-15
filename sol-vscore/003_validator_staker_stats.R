#' Validator Stake Stats 
#'
#' @description For a given `target_epoch` return the available staker data up to that epoch. 
#' Epoch | Voter_Pubkey (NULL removed) | nstakers | sol_staked | avg_stake_size | gini_coefficient of stake | 
#' num_new_stakers (since previous epoch) | prev_sol_staked | stakers_churn_percent | sol_staked_churn_percent 
#' @param target_epoch Solana Epoch. 
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake.
#' @return Data frame of solana stake account data aggregated at voter pubkey level. 
#' @import shroomDK
#' @export

get_validator_stake <- function(target_epoch = 460, api_key, data_source = "snowflake_default"){
  
  validator_stake_query <- {
    "
 
with target_epoch AS (
-- change epoch here
SELECT '__TARGET_EPOCH__' AS epoch FROM dual
),

staker_history AS (
select epoch_ingested_at as epoch,
 activation_epoch, deactivation_epoch, 
 account_sol as active_stake,
 voter as voter_pubkey, 
 pubkey as stake_pubkey
from solana.silver.historical_stake_account 
WHERE epoch <= (select epoch from target_epoch)
ORDER BY epoch_ingested_at ASC, voter_pubkey
),
staker_snapshot AS (
select epoch_recorded as epoch,
 activation_epoch, deactivation_epoch, 
 account_sol as active_stake,
 vote_pubkey as voter_pubkey, 
 stake_pubkey
from solana.silver.snapshot_stake_accounts
WHERE epoch <= (select epoch from target_epoch)
ORDER BY epoch ASC, voter_pubkey
), 

combined_staker AS (
SELECT * FROM staker_history UNION ALL (SELECT * FROM staker_snapshot)
),

voter_avg_stake AS (
 select epoch, voter_pubkey, 
count(distinct(stake_pubkey)) as nstakers,
sum(active_stake) as sol_staked, 
COALESCE(sol_staked/NULLIF(nstakers,0),0) as avg_stake_size
from combined_staker 
WHERE deactivation_epoch > epoch
group by epoch, voter_pubkey
ORDER BY epoch ASC
),

combined_staker_with_dup_avg AS (
select * from combined_staker LEFT JOIN voter_avg_stake USING (epoch, voter_pubkey)
),

staker_sol_history AS (
 select epoch,
 voter_pubkey,
 nstakers,
 sol_staked, 
 avg_stake_size,
 -- technically for discrete values, gini coefficient is not exactly 0.5 * relative mean absolute deviation;  
 -- but using continuous values formula for simplicity and interpretability.
 AVG(ABS(active_stake - avg_stake_size))/avg_stake_size/2 as gini_coefficient
from combined_staker_with_dup_avg
WHERE deactivation_epoch > epoch
group by epoch, voter_pubkey, 
-- can safely group by these b/c left join purposefully duplicated across rows.
nstakers, sol_staked, avg_stake_size 
ORDER BY epoch ASC
),

staker_stats AS (
select *, 
  nstakers - LAG(nstakers, 1) OVER (PARTITION BY voter_pubkey ORDER BY epoch ASC) AS num_new_stakers,
  sol_staked - LAG(sol_staked, 1) OVER (PARTITION BY voter_pubkey ORDER BY epoch ASC) AS num_new_sol_staked,
  LAG(sol_staked, 1) OVER (PARTITION BY voter_pubkey ORDER BY epoch ASC) AS prev_sol_staked,
  round(COALESCE(num_new_stakers/NULLIF(nstakers,0),0)*100, 2) as stakers_churn_percent,
  round(COALESCE(num_new_sol_staked/NULLIF(prev_sol_staked,0),0) *100,2) as sol_staked_churn_percent
from staker_sol_history
)

SELECT *
  FROM staker_stats 
;
;

  "
  }
  
  validator_stake_query <- gsub("__TARGET_EPOCH__", target_epoch, validator_stake_query)
  
  # only certain API keys work on alternative sources.
  shroomDK::auto_paginate_query(query = validator_stake_query, 
                                api_key = api_key,
                                data_source = data_source)
}
