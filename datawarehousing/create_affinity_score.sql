DROP PROCEDURE IF EXISTS commits.generate_affinity;
CREATE PROCEDURE commits.`generate_affinity`()
BEGIN
	

insert into affinity
select 
g.pidm 
,  numlifecontacts
 + numthisyrcontacts
 + alum
 + trustee
 + cfa_donor
 + yrs_giving_plus_5
 + investor_soc
 + spouse_alum
 + spouse_trustee
 + trustee
 + hasaddr
 + hasemail
 + hasphone
 + recentgiving
 + fileact
 + leadership_org
 + more_than_1_act
 + athlete
 + greek
 + high_email_open_rate
as affinity

from (

select hallp.pidm
, if(lifecontacts > 1, 1, 0) as numlifecontacts
, if(fy0contacts + fy1contacts > 1, 1,0) as numthisyrcontacts
, if(dnrc like "ALUM", 1, 0) as alum
, if(dnrc like "TRU%", 1, 0) as trustee
, if(cfagiftsoc is null, 0 , 1) as cfa_donor
, if(yrsgiving > 5, 1, 0) as yrs_giving_plus_5
, if(`is` is null, 0, 1) as investor_soc
, if(spdnrc like "ALUM", 1, 0) as spouse_alum
, if(spdnrc like "TRU%", 1, 0) as spouse_trustee
, if(addr1 is null, 0, 1) as hasaddr
, if(prefemail is null, 0, 1) as hasemail
, CASE
    WHEN homephone is not null THEN 1
    WHEN cellphone is not null THEN 1
    ELSE 0 END as hasphone
, if (fy2gifts + fy1gifts + fy0gifts > 1, 1, 0) as recentgiving
, coalesce(fileact,0) as fileact
, coalesce(leadership_org,0) as leadership_org 
, coalesce(more_than_1_act,0) as more_than_1_act 
, coalesce(athlete,0) as athlete 
, coalesce(greek,0) as greek
, coalesce(high_email_open_rate, 0) as high_email_open_rate

from hallp

left join
(select distinct pidm
, 1 as fileact
from acts
where act = 'FILE') fileact  on hallp.pidm = fileact.pidm


left join
(select distinct pidm
, 1 as leadership_org
from acts left join acts_catg on acts.act = acts_catg.act
where 
   acts_catg.act_catg = 'Honorary'
or acts_catg.act_catg = 'Millikin Service Organizations'
or acts_catg.act_catg = 'MU Leadership'
or acts_catg.act_catg = 'Professional') leadership on hallp.pidm = leadership.pidm

left join
(select
pidm
,  if(count(pidm) > 1, 1, 0) as more_than_1_act
from
acts
group by pidm) numacts on hallp.pidm = numacts.pidm

left join
(select distinct pidm
, 1 as music_dance_groups
from acts left join acts_catg on acts.act = acts_catg.act
where 
   acts_catg.act_catg = 'Music/Dance Performance Groups') musicdance on hallp.pidm = musicdance.pidm

left join
(select distinct pidm
, 1 as athlete
from acts left join acts_catg on acts.act = acts_catg.act
where 
  acts_catg.act_catg = 'Sports (CCIW/NCAA)') athlete on hallp.pidm = athlete.pidm

left join
(select distinct pidm
, 1 as greek
from acts left join acts_catg on acts.act = acts_catg.act
where 
acts_catg.act_catg = 'Greek') greek on hallp.pidm = greek.pidm

left join
(select distinct pidm
, 1 as high_email_open_rate
from email_summary s 
where 
      pct_viewed >= .9 
  and n_sent > 10 
  and last_email_dt > (curdate() - interval 2 year)
) email on hallp.pidm = email.pidm

) g;
END;

