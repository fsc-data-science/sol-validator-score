
-- finding validators that have activated/deactivated multiple times
with base as (select *, (epoch_ingested_at - epoch) as test from solana_dev.silver.historical_vote_account
where test > 1),

base2 as (
select pubkey, epoch, count(*)
from base
group by pubkey,epoch)

select pubkey, count(distinct(epoch))
from base2
group by pubkey;

-- exploring single validator that has activated/deactivated multiple times
select epoch_ingested_at,epoch, pubkey,
case when epoch_ingested_at = epoch then 'active'
when (epoch_ingested_at - epoch) = 1 then 'deactivated'
when (epoch_ingested_at - epoch) > 1 then 'currently_deactivated'
else 'weird'
end as active_category
from 
solana_dev.silver.historical_vote_account
where pubkey = 'HK8x8tP8QkCQydvjCe1nQHPM9oFa51ogG8U1pYXqUskd';

--reminder that the 'active_stake' in the current snapshot vote_accounts needs to be x'ed by 10^9
select *, (active_stake * pow(10,9)) as test from solana_dev.silver.snapshot_vote_accounts
where vote_pubkey = '2Y2opv8Kq8zFATg6ipqb2AjgCf18tkv1CLMLXQGif2NH';

-- see sum of all deactivated stakes for a validator (via column summary)
select vote_pubkey, stake_pubkey, activation_epoch, deactivation_epoch, active_stake
from solana_dev.silver.snapshot_stake_accounts
where epoch_recorded = 446
and vote_pubkey = '2Y2opv8Kq8zFATg6ipqb2AjgCf18tkv1CLMLXQGif2NH'
and deactivation_epoch <= 446
order by deactivation_epoch asc;

-- noticed that all stakes activated in the current epoch have differing credits_observed
select * from solana_dev.silver.historical_stake_account
where vote_pubkey = '2Y2opv8Kq8zFATg6ipqb2AjgCf18tkv1CLMLXQGif2NH'
and epoch_ingested_at = 332
and activation_epoch = 332;

--all the stakes that were deactivated before the epoch = contain a different lower value 
select * from solana_dev.silver.historical_stake_account
where vote_pubkey = '2Y2opv8Kq8zFATg6ipqb2AjgCf18tkv1CLMLXQGif2NH'
and epoch_ingested_at = 332
and deactivation_epoch < 332
and credits_observed != 48977228;


