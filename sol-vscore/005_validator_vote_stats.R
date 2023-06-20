#' Validator Vote Stats 
#'
#' @description For a given `target_epoch` return the available vote data up to that epoch. 
#' Epoch | Voter_Pubkey (NULL removed) | last_active_epoch | active_category | minslot | maxslot | 
#' diffslot | num_slots_skipped | percent_slot_skipped_in_epoch (capped at 100%) 
#' @param target_epoch Solana Epoch. 
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake.
#' @return Data frame of solana vote account data aggregated at voter pubkey level. 
#' @import shroomDK
#' @export

get_validator_vote <- function(target_epoch = 460, api_key, data_source = "snowflake_default"){
  
  validator_vote_query <- {
    "
 
with target_epoch AS (
-- change epoch here
SELECT '__TARGET_EPOCH__' AS epoch FROM dual
),


vote_history AS (
select epoch_ingested_at as epoch, 
epoch as last_active_epoch,
vote_pubkey as voter_pubkey,
votes,
case when epoch_ingested_at = epoch then 'active'
when (epoch_ingested_at - epoch) = 1 then 'deactivated'
when (epoch_ingested_at - epoch) > 1 then 'currently_deactivated'
else 'weird'
end as active_category
from 
solana.silver.historical_vote_account
WHERE epoch_ingested_at <= (SELECT epoch from target_epoch) 
ORDER BY epoch_ingested_at ASC, voter_pubkey
),
vote_snapshot AS (
select epoch_recorded as epoch, 
epoch as last_active_epoch,
vote_pubkey as voter_pubkey,
votes,
case when epoch_recorded = epoch then 'active'
when (epoch_recorded - epoch) = 1 then 'deactivated'
when (epoch_recorded - epoch) > 1 then 'currently_deactivated'
else 'weird'
end as active_category
from 
solana.silver.snapshot_vote_accounts
WHERE epoch_recorded <= (SELECT epoch from target_epoch) 
ORDER BY epoch_recorded ASC, voter_pubkey
), 

block_production as (
select epoch, validator as voter_pubkey, num_leader_blocks, num_blocks_produced, start_slot, end_slot
from solana_dev.silver.rpc_block_production
),

combined_vote AS (
SELECT * FROM vote_history UNION ALL (SELECT * FROM vote_snapshot)
),

vote_with_block_production AS (
select * from combined_vote LEFT JOIN block_production USING (epoch, voter_pubkey)
)

select epoch, voter_pubkey,
last_active_epoch, active_category,
 votes[0]:slot as minslot,
 votes[30]:slot as maxslot,
 maxslot - minslot as diffslot,
 (diffslot - 30) as num_slots_skipped, 
 -- some old votes get kept in deactivated voters causing excessive skips, cap percent at 100%. 
 round(coalesce(least(num_slots_skipped,31), 31) / 31 * 100,2) as percent_on_chosen_fork,
  num_leader_blocks, num_blocks_produced, coalesce(num_blocks_produced/num_leader_blocks, 0) as attendance_rate
from vote_with_block_production
HAVING voter_pubkey IS NOT NULL;
  "
  }
  
  validator_vote_query <- gsub("__TARGET_EPOCH__", target_epoch, validator_vote_query)
  
  # only certain API keys work on alternative sources.
  shroomDK::auto_paginate_query(query = validator_vote_query, 
                                api_key = api_key,
                                data_source = data_source)
}
