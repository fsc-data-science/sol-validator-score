#' Ecosystem App Data 
#'
#' @description For a given `target_epoch` return the available app data up to that epoch. 
#' Epoch | Voter_Pubkey (NULL removed) | Validator Name (if available) | Validator Details (if available) | Software Version |
#' Latitude | Longitude | Commission | SOL_Stake | Data Center Key | 
#' Delinquent | # Epochs active in previous 10 epochs.
#' @param target_epoch Solana Epoch. 
#' @param api_key Flipside API Key.
#' @param data_source Default snowflake.
#' @return Data frame of solana validator appdata.
#' @import shroomDK
#' @export

get_ecosystem_appdata <- function(target_epoch = 460, api_key, data_source = "snowflake_default"){
  
  ecosystem_appdata_query <- {
  "
  with target_epoch AS (
-- change epoch here
SELECT '__TARGET_EPOCH__' AS epoch FROM dual
),

snapshot_appdata AS (
select 
 EPOCH_ACTIVE,
VOTE_PUBKEY as voter_pubkey,
VALIDATOR_NAME, details,
SOFTWARE_VERSION,
 SPLIT_PART(software_version, '.', 1) || '.' ||
    LPAD(SPLIT_PART(software_version, '.', 2), 2, '0') || '.' ||
    LPAD(SPLIT_PART(software_version, '.', 3), 2, '0') AS modified_software_version,
LATITUDE,
 LONGITUDE,
COMMISSION,
ACTIVE_STAKE, 
ACTIVE_STAKE/POW(10,9) as sol_stake, 
DATA_CENTER_HOST, 
DATA_CENTER_KEY, 
DELINQUENT
from solana.core.fact_validators   
)

select epoch_active as epoch,
 voter_pubkey,
 validator_name,details, 
software_version,
modified_software_version,
latitude, longitude,
commission, sol_stake,
data_center_key, delinquent,
  count_if(delinquent = FALSE) OVER (
    PARTITION BY voter_pubkey
    ORDER BY epoch_active 
    ROWS BETWEEN 9 PRECEDING AND CURRENT ROW
  ) AS count_active_last10
 FROM snapshot_appdata
 where voter_pubkey IS NOT NULL
QUALIFY epoch_active <= (SELECT epoch from target_epoch)
;
  "
  }
  
  ecosystem_appdata_query <- gsub("__TARGET_EPOCH__", target_epoch, ecosystem_appdata_query)
  
  # only certain API keys work on alternative sources.
  shroomDK::auto_paginate_query(query = ecosystem_appdata_query, 
                                api_key = api_key,
                                data_source = data_source)
}
