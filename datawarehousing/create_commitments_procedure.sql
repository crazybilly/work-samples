DROP PROCEDURE IF EXISTS commits.create_commit_summaries;
CREATE PROCEDURE commits.`create_commit_summaries`()
BEGIN

-- COLLAPSE GIFT AND PLEDGE TABLES INTO A TABLE OF COMMITMENTS
--   INCLUDES LOGIC FOR THIRD PARTY PAYMENTS
insert into commit 
(pidm, dnrc, desg, campaign, campaign_type, pledge_num, gift_num, fisc_code, commit_date, commit_amt, paid_amt, out_amt, num_payments, commit_type, commit_status, commit_code, pledge_amt, revocable, commit_active)

SELECT p.pidm,
       dnrc AS dnrc,
       p.desg AS desg,
       p.campaign AS campaign,
       p.campaign_type as campaign_type,
       p.pledge_num,
       max(pay.gift_num) AS gift_num,
       p.fisc_code,
       p.pledge_date AS commit_date,
       
       # calculate commit_amt based on pledge status       
       CASE
        WHEN pledge_status = 'A'
            THEN greatest(pledge_amt, coalesce(sum(gift_amt),0) )
        
        WHEN pledge_status = 'I'
            THEN greatest(pledge_amt, coalesce(sum(gift_amt),0) )
        
        WHEN pledge_status = 'L'
                  THEN amt_paid
        
        WHEN pledge_status = 'O'
            THEN greatest(pledge_amt, coalesce(sum(gift_amt),0) )
        
        WHEN pledge_status = 'P'
                THEN coalesce(sum(gift_amt),0)
          
        WHEN pledge_status = 'C' or pledge_status = 'U'
            THEN coalesce(sum(gift_amt),0) 
            
        END as commit_amt,
        
       coalesce(sum(gift_amt), 0) AS paid_amt,
       
       if(  p.pledge_amt - coalesce(sum(gift_amt), 0) < 0, 0
          , p.pledge_amt - coalesce(sum(gift_amt), 0))
          AS out_amt,
          
       count(gift_num) AS num_payments,
       pledge_type AS commit_type,
       coalesce(pledge_status,if(p.pledge_amt - coalesce(sum(gift_amt), 0) <= 0,"P","A")) as commit_status,
       coalesce(pledge_code,"RP") as commit_code,
       p.pledge_amt AS pledge_amt,
       p.revocable as revocable,
       CASE 
          when pledge_status = 'C' 
             then 'inactive'
          when pledge_status = 'I'
             then 'inactive'
          when pledge_status = 'L'
             then 'inactive'
          when pledge_status = 'U'
             then 'inactive'
          else 'active'
       end as commit_active
       
  FROM    pledges p
       LEFT JOIN
          (
           SELECT g.pledge_num,
                  g.gift_num,
                  g.gift_amt,
                  g.campaign,
                  g.desg
             FROM gifts g
            WHERE g.pledge_num > 0
           UNION
           
           SELECT m.`3pp_pledge_num` AS pledge_num,
                  g2.gift_num,
                  g2.gift_amt,
                  g2.campaign,
                  g2.desg
             FROM    gifts g2
                  LEFT JOIN
                     (SELECT DISTIncT
                             (memos.gift_num),
                             memos.`3pp_pledge_num`
                        FROM memos
                       WHERE memos.`3pp_pledge_num` IS NOT NULL) m
                  ON g2.gift_num = m.gift_num
            WHERE m.gift_num IS NOT NULL) pay
       ON p.pledge_num = pay.pledge_num and p.campaign = pay.campaign and p.desg = pay.desg
 WHERE pidm <> 0

GROUP BY pledge_num, campaign, desg
UNION

SELECT pidm,
       dnrc AS dnrc,
       desg,
       campaign,
       campaign_type,
       NULL AS pledge_num,
       gift_num,
       fisc_code,
       gift_date AS commit_date,
       gift_amt AS commit_amt,
       gift_amt AS commit_amt,
       0 AS out_amt,
       1 AS num_payments,
       "gift" AS commit_type,
       "P" AS commit_status,
       gift_code as commit_code,
       0 as pledge_amt,
       "irrevocable" as revocable,
       "active" as commit_active
  FROM gifts gnp
 WHERE gnp.pledge_num = 0 
 and gnp.gift_num not in (select * from third_party_payments);



-- CREATE A SUMMARY TABLE FOR HOW MUCH EACH PERSON COMMITED EACH FY
--   TO BOTH ALL AREAS AND TO ANNUAL FUND ONLY
create table commits_by_person as

select
    clife.pidm
  , clife.commit_life
  , clife.commit_avg
  , clife.commit_count
  , coalesce(cib.commit_cib,0) as commit_cib
  , coalesce(cnc.commit_nc,0) as commit_nc
  , coalesce(caf20.commit_af20,0) as commit_af20
  , coalesce(caf19.commit_af19,0) as commit_af19
  , coalesce(caf18.commit_af18,0) as commit_af18
  , coalesce(caf17.commit_af17,0) as commit_af17
  , coalesce(caf16.commit_af16,0) as commit_af16
  , coalesce(caf15.commit_af15,0) as commit_af15
  , coalesce(caf14.commit_af14,0) as commit_af14
  , coalesce(caf13.commit_af13,0) as commit_af13
  , coalesce(caf12.commit_af12,0) as commit_af12
  , coalesce(caf11.commit_af11,0) as commit_af11
  , coalesce(caf10.commit_af10,0) as commit_af10
  , coalesce(catv.commit_atv,0) as commit_atv
  , coalesce(cetv.commit_etv,0) as commit_etv
  , coalesce(cnh.commit_nh,0) as commit_nh
  , coalesce(fy20.commit_fy20,0) as commit_fy20
  , coalesce(fy19.commit_fy19,0) as commit_fy19
  , coalesce(fy18.commit_fy18,0) as commit_fy18
  , coalesce(fy17.commit_fy17,0) as commit_fy17
  , coalesce(fy16.commit_fy16,0) as commit_fy16
  , coalesce(fy15.commit_fy15,0) as commit_fy15
  , coalesce(fy14.commit_fy14,0) as commit_fy14
  , coalesce(fy13.commit_fy13,0) as commit_fy13
  , coalesce(fy12.commit_fy12,0) as commit_fy12
  , coalesce(fy11.commit_fy11,0) as commit_fy11
  , coalesce(fy10.commit_fy10,0) as commit_fy10
  , coalesce(fy09.commit_fy09,0) as commit_fy09
  

from 

        (
        select 
            pidm
          , sum(c.commit_amt) as commit_life
      , avg(c.commit_amt) as commit_avg
      , count(distinct(coalesce(gift_num,pledge_num))) as commit_count
        
        from commit c
        group by pidm ) clife
    
  left join
        
        (select
          pidm
          , sum(commit_amt) as commit_cib
         from commit c2
         where      
            c2.campaign_type = 'CI'
--                  c2.campaign = "BBF"
--               or c2.campaign = "SBF"
--               or c2.campaign = "INF16"
--               or c2.campaign = "SCH16"
--               or c2.campaign = "UNR16"
--               or c2.campaign = "NPL16"
--               or c2.campaign = "FAC16"
--               or c2.campaign = "SCH17"
--               or c2.campaign = "NPL17"
--               or c2.campaign = "SCH18"
--               or c2.campaign = "NPL18"
--               or c2.campaign = "SCH19"
--               or c2.campaign = "NPL19"
--               or c2.campaign = "TD"

         group by pidm) cib
        
        on clife.pidm = cib.pidm
          
    
    
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_nc
         from commit c2
         where c2.campaign = "nc"
         group by pidm) cnc
        
        on clife.pidm = cnc.pidm
  
      left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af20
         from commit 
         where campaign = "af20"
         group by pidm) caf20
        
        on clife.pidm = caf20.pidm   
  
  
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af19
         from commit 
         where campaign = "af19"
         group by pidm) caf19
        
        on clife.pidm = caf19.pidm    
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af18
         from commit 
         where campaign = "af18"
         group by pidm) caf18
        
        on clife.pidm = caf18.pidm
    
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af17
         from commit 
         where campaign = "af17"
         group by pidm) caf17
        
        on clife.pidm = caf17.pidm
    
    
  left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af16
         from commit 
         where campaign = "af16"
         group by pidm) caf16
        
        on clife.pidm = caf16.pidm
        
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af15
         from commit 
         where campaign = "af15"
         group by pidm) caf15
        
        on clife.pidm = caf15.pidm
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af14
         from commit 
         where campaign = "af14"
         group by pidm) caf14
        
        on clife.pidm = caf14.pidm
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af13
         from commit 
         where campaign = "af13"
         group by pidm) caf13
        
        on clife.pidm = caf13.pidm
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af12
         from commit 
         where campaign = "af12"
         group by pidm) caf12
        
        on clife.pidm = caf12.pidm
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af11
         from commit 
         where campaign = "af11"
         group by pidm) caf11
        
        on clife.pidm = caf11.pidm
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_af10
         from commit 
         where campaign = "af10"
         group by pidm) caf10
        
        on clife.pidm = caf10.pidm
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_atv
         from commit 
         where campaign = "atv"
         group by pidm) catv
        
        on clife.pidm = catv.pidm
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_etv
         from commit 
         where campaign = "etv"
         group by pidm) cetv
        
        on clife.pidm = cetv.pidm
        
    left join
        
        (select
          pidm
          , sum(commit_amt) as commit_nh
         from commit 
         where campaign = "nh"
         group by pidm) cnh
        
        on clife.pidm = cnh.pidm

    
    
    left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy20
      from commit
      where fisc_code = 2020
      group by pidm) fy20
      
    on clife.pidm = fy20.pidm
    
    
    left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy19
      from commit
      where fisc_code = 2019
      group by pidm) fy19
      
    on clife.pidm = fy19.pidm

    left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy18
      from commit
      where fisc_code = 2018
      group by pidm) fy18
      
    on clife.pidm = fy18.pidm

    left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy17
      from commit
      where fisc_code = 2017
      group by pidm) fy17
      
    on clife.pidm = fy17.pidm
  
  
  
    left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy16
      from commit
      where fisc_code = 2016
      group by pidm) fy16
      
    on clife.pidm = fy16.pidm
  
  
  
  
  left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy15
      from commit
      where fisc_code = 2015
      group by pidm) fy15
      
    on clife.pidm = fy15.pidm
    
    
    
    
    
left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy14
      from commit
      where fisc_code = 2014
      group by pidm) fy14
      
    on clife.pidm = fy14.pidm
left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy13
      from commit
      where fisc_code = 2013
      group by pidm) fy13
      
    on clife.pidm = fy13.pidm
left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy12
      from commit
      where fisc_code = 2012
      group by pidm) fy12
      
    on clife.pidm = fy12.pidm
left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy11
      from commit
      where fisc_code = 2011
      group by pidm) fy11
      
    on clife.pidm = fy11.pidm

     left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy10
      from commit
      where fisc_code = 2010
      group by pidm) fy10
      
    on clife.pidm = fy10.pidm
left join
  
    (select
      pidm
      , sum(commit_amt) as commit_fy09
      from commit
      where fisc_code = 2009
      group by pidm) fy09
      
    on clife.pidm = fy09.pidm



;

    
-- CREATE A SUMMARY TABLE FOR HOW MUCH HAS BEEN COMMITED TO EACH CAPITAL & ANNUAL FUND PROEJCT
create table commits_by_campaign as

  
    select 
        campaign
      , sum(commit_amt) as commit_camp_total
      , sum(paid_amt) as commit_camp_gifts
      , sum(out_amt) as commit_camp_out
    , sum(pledge_amt) as commit_camp_pledge
    from commit
    group by campaign


  UNION

  
  select "cib" as campaign
        , sum(commit_amt) as commit_camp_total
        , sum(paid_amt) as commit_camp_gifts
        , sum(out_amt) as commit_camp_out
      , sum(pledge_amt) as commit_camp_pledge
      from commit
      # consider replacing with campaign_type = "CI"
      where 
         campaign_type = 'CI'
--            campaign = "BBF"
--         or campaign = "SBF"
--         or campaign = "INF16"
--         or campaign = "SCH16"
--         or campaign = "UNR16"
--         or campaign = "NPL16"
--         or campaign = "FAC16"
--         or campaign = "SCH17"
--         or campaign = "NPL17"
--         or campaign = "SCH18"
--         or campaign = "NPL18"
--         or campaign = "SCH19"
--         or campaign = "NPL19"
--         or campaign = "TD"
    


;



-- SUMMARY OF COMMITMENTS TO AFFINITY AREAS
create table commits_by_club as

    
    select 
      c.club_club
      , commit_club_total
      , commit_club_pledges
      , commit_club_gifts
      , commit_club_out
    , commit_club_curfy_commits
    , commit_club_curfy_cash
    , commit_club_curfy_out
    
    from
    
    (
    select 
        cl.club_club
      , sum(c.commit_amt)  as commit_club_total
      , suM(c.paid_amt) as commit_club_gifts
      , sum(c.out_amt) as commit_club_out
    
    from desgs cl
    left join 
    commit c
    on cl.desg = c.desg
    
  where cl.club_club = 'BBC' or cl.club_club = 'CFA' or cl.club_club = 'ProjectConfirm'
  
    group by cl.club_club
  
  UNION
  
  select 
        cl.club_club
      , sum(c.commit_amt)  as commit_club_total
      , suM(c.paid_amt) as commit_club_gifts
      , sum(c.out_amt) as commit_club_out
    
    from desgs cl
    left join 
    commit c
    on cl.desg = c.desg
    
  where cl.club_club like 'nc%'
  and campaign = 'nc'
  
    group by cl.club_club
   
   ) c
    
left join
    
    (
    select 
      cl2.club_club
      , suM(c2.commit_amt) as commit_club_pledges
       
    
    from desgs cl2 left join commit c2 on cl2.desg = c2.desg
    where commit_type = "pledge"
  and c2.campaign = 'nc'
    group by cl2.club_club
  
  UNION
  
  select 
      cl2.club_club
      , suM(c2.commit_amt) as commit_club_pledges
       
    
    from desgs cl2 left join commit c2 on cl2.desg = c2.desg
    where commit_type = "pledge"
  and (cl2.club_club = 'BBC' or cl2.club_club = 'CFA' or cl2.club_club = 'ProjectConfirm')
    group by cl2.club_club
    ) p

on c.club_club = p.club_club

left join


(select 
  club_club
 
  , sum(c.commit_amt) as commit_club_curfy_commits
  , sum(c.paid_amt)   as commit_club_curfy_cash
  , sum(c.out_amt)    as commit_club_curfy_out

from desgs cl
left join commit c on cl.desg = c.desg
where c.fisc_code = 2020
group by club_club
) fy

on c.club_club = fy.club_club;


    
-- SUMMARY BY CONSTITUENT TYPES ("donor category" is Banner's term for constituent type, eg. degreed alum, current parent, etc)
create table commits_by_donor_catg as

select 
  a.dnrc_catg
  , commit_dcatg_total
  , coalesce(commit_dcatg_pledges,0) as commit_dcatg_pledges
  , commit_dcatg_gifts
  , commit_dcatg_out

from (
    select 
        dc.dnrc_catg
      , sum(c.commit_amt)  as commit_dcatg_total
      , suM(c.paid_amt) as commit_dcatg_gifts
      , sum(c.out_amt) as commit_dcatg_out
    
    from dnr_catg dc
    left join 
    commit c
    on dc.dnrc = c.dnrc
    
    group by dc.dnrc_catg
    
    ) a 

left join

    (
    select
      dc2.dnrc_catg
      , sum(coalesce(c2.commit_amt,0)) as commit_dcatg_pledges
     from dnr_catg dc2 left join commit c2 on c2.dnrc = dc2.dnrc
     where commit_type = "pledge"
     group by dc2.dnrc_catg
    ) p 

on a.dnrc_catg = p.dnrc_catg;


-- A LIST OF MATCHINGN GIFT COMPANIES
create table mg_company as

SELECT hallp.pidm, COALESCE(matching_gift_company, "non-matching") as matching_gift_company FROM hallp 
LEFT JOIN
(SELECT DISTINCT pidm, "matching gift co" as matching_gift_company 
FROM gifts
WHERE gift_code LIKE "M%") m
ON hallp.pidm = m.pidm;


insert into giftsandmemos
select
 giftmemokey
 , pidm
 , desg
 , campaign
 , campaign_type
 , fisc_code
 , amt
 , dt
 , type
 
 from (

select
    NULL as giftmemokey
  , pidm
  , desg
  , campaign
  , campaign_type 
  , fisc_code
  , gift_amt as amt
  , date(gift_date) as dt
   , 'gift' as type
FROM gifts
  
union all

select 
    NULL as giftmemokey
  , memo_pidm as pidm
  , memos.desg as desg
  , memos.campaign as campaign
  , memos.campaign_type as campaign_type
  , memos.fisc_code as fisc_code
  , memo_amt as amt
  , date(memos.gift_date) as dt
  , 'memo' as type
FROM memos

) gm;



alter table commits_by_campaign
  add index `commits_by_camp.pidm` (campaign);
  
alter table commits_by_club
  add index `commits_by_camp.club` (club_club);
  
alter table commits_by_donor_catg
  add index `commits_by_camp.dnrc` (dnrc_catg);
  
alter table commits_by_person
  add index `commits_by_camp.pidm` (pidm);




END;

