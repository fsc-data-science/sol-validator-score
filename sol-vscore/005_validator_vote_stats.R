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

vote_snapshot AS (
select epoch, 
last_epoch_active,
vote_pubkey as voter_pubkey,
node_pubkey,
votes,
case when last_epoch_active = epoch then 'active'
when (epoch - last_epoch_active) = 1 then 'deactivated'
when (epoch - last_epoch_active) > 1 then 'currently_deactivated'
else 'weird'
end as active_category
from 
solana.core.fact_vote_accounts
WHERE epoch <= (SELECT epoch from target_epoch) 
ORDER BY epoch ASC, voter_pubkey
), 

block_production as (
select epoch, node_pubkey, num_leader_slots as num_leader_blocks, num_blocks_produced, start_slot, end_slot
from solana.core.fact_block_production   
WHERE epoch <= (SELECT epoch from target_epoch) 

),
vote_with_block_production AS (
select * from vote_snapshot LEFT JOIN block_production USING (epoch, node_pubkey)
)

select epoch, voter_pubkey,
last_epoch_active as last_active_epoch, active_category,
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
