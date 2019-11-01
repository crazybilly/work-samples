DROP PROCEDURE IF EXISTS commits.make_lybunt_ind;
CREATE PROCEDURE commits.`make_lybunt_ind`(yrs_out INT, desg_type VARCHAR(10), currentfy INT, tblname VARCHAR(20))
BEGIN


-- this procedure gets called repeatedly by generate_lybunt_indicators with different parameters
--   in order to generate a bunch of small temporary tables, for all the different ways we can think about lybunt. 
--   Pushing the lybunt logic into this procedure ensure that "lybuntness" is calculated the same way each time
--   (and makes it easy to change if we decide, for example, to ignore soft credits, ie. memos. 


SET @d = concat('drop temporary table if exists ', tblname);
prepare stmt2 from @d;
execute stmt2;
deallocate prepare stmt2;


SET @s = concat('
create temporary table ', tblname ,' as 
  select
      distinct pidm
    , TRUE as lybunt
  from giftsandmemos gm 
     left join 
      desg_types d on gm.desg = d.desg
  where
     fisc_code >= ', currentfy, ' - ',yrs_out,'    
     and desg_type = "', desg_type ,'"   
     and pidm not in (select pidm from giftsandmemos where fisc_code = ',currentfy,')
     and amt > 1
  ');

prepare stmt1 from @s;
execute stmt1;

DEALLOCATE PREPARE stmt1;
END;

