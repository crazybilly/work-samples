DROP PROCEDURE IF EXISTS commits.generate_lybunt_indicators;
CREATE PROCEDURE commits.`generate_lybunt_indicators`(currentfy INT)
BEGIN

create temporary table if not exists desg_types as
select distinct desg, "all" as desg_type from desgs
 union all
 select distinct desg, "budget" as desg_type from rfcclubs where rfcclubs.rfc_club = "Annual Fund/Budget" or rfc_club = 'Annual Scholarships'
 union all
 select distinct desg, "AF" as desg_type from `commit` where campaign_type = 'AF'
 ;
 
call make_lybunt_ind (1 , "all"   , currentfy, "one_all");
call make_lybunt_ind (1 , "af"    , currentfy, "one_af");
call make_lybunt_ind (1 , "budget", currentfy, "one_budget");
call make_lybunt_ind (3 , "all"   , currentfy, "three_all");
call make_lybunt_ind (3 , "af"    , currentfy, "three_af");
call make_lybunt_ind (3 , "budget", currentfy, "three_budget");
call make_lybunt_ind (5 , "all"   , currentfy, "five_all");
call make_lybunt_ind (5 , "af"    , currentfy, "five_af");
call make_lybunt_ind (5 , "budget", currentfy, "five_budget");
call make_lybunt_ind (10 , "all"   , currentfy, "ten_all");
call make_lybunt_ind (10 , "af"    , currentfy, "ten_af");
call make_lybunt_ind (10 , "budget", currentfy, "ten_budget");
call make_lybunt_ind (1000 , "all"   , currentfy, "alltime_all");
call make_lybunt_ind (1000 , "af"    , currentfy, "alltime_af");
call make_lybunt_ind (1000 , "budget", currentfy, "alltime_budget");

insert into lybunt_ind
select 
    h.pidm
 	, lyb_fy0_all
	, lyb_fy0_af
	, lyb_fy0_budget
	, lyb_fy3_all
	, lyb_fy3_af
	, lyb_fy3_budget
	, lyb_fy5_all
	, lyb_fy5_af
	, lyb_fy5_budget
  , lyb_fy10_all
	, lyb_fy10_af
	, lyb_fy10_budget
  , lyb_alltime_all
	, lyb_alltime_af
	, lyb_alltime_budget

  from hallp h
		left join (select pidm, lybunt as lyb_fy0_all from one_all) one_all                on one_all.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy0_af from one_af) one_af                   on one_af.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy0_budget from one_budget) one_budget       on one_budget.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy3_all from three_all) three_all            on three_all.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy3_af from three_af) three_af               on three_af.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy3_budget from three_budget) three_budget   on three_budget.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy5_all from five_all) five_all              on five_all.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy5_af from five_af) five_af                 on five_af.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy5_budget from five_budget) five_budget     on five_budget.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy10_all from ten_all) ten_all               on ten_all.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy10_af from ten_af) ten_af                  on ten_af.pidm = h.pidm
		left join (select pidm, lybunt as lyb_fy10_budget from ten_budget) ten_budget      on ten_budget.pidm = h.pidm
    left join (select pidm, lybunt as lyb_alltime_all from alltime_all) alltime_all    on alltime_all.pidm = h.pidm
		left join (select pidm, lybunt as lyb_alltime_af from alltime_af) alltime_af       on alltime_af.pidm = h.pidm
		left join (select pidm, lybunt as lyb_alltime_budget from alltime_budget) alltime_budget  on alltime_budget.pidm = h.pidm

    

    ;


END;

