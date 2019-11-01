# libraries -------------------------------------------

library(lubridate); library(beepr)
library(tidyverse); library(muadc); library(magrittr)
library(readxl); library(rlang)
library(formattable); library(fundRaising)

# data ------------------------------------------------

if( !exists("lastinit") || lastinit < (now() - seconds(3) ) ) {
  initcommitsdb()
  lastinit  <- now()
}


hallp  <- hallptbl %>% 
  collect()

desgs  <- desgstbl %>% 
  collect()

dnrct  <- dnrctbl %>% 
  collect()

predictive  <- tbl(commitsdb, "predictive_scores") %>% 
  collect()

affinity  <- tbl(commitsdb, "affinity") %>% 
  collect()


# read major data ---------------------------------------------------------



heirarchy  <- read.tidy('data/class agent fy19 heirarchy.csv')

majors <- read.tidy('data/major-categories.csv')  %>% 
  left_join(heirarchy %>% select(letter, rank), by = 'letter')


mumajor  <- tbl(commitsdb, "mumajor") %>% 
  collect() %>% 
  rename(major = mumajor)
alumnimajor <- tbl(commitsdb, "alumnimajor") %>% 
  collect() %>% 
  rename(major = alumnimajor)
mumastersmajor <- tbl(commitsdb, "mumastersmajor") %>% 
  collect() %>% 
  rename(major = mumastersmajor)
  
allmajors  <- bind_rows(
    `1` = mumajor
  , `2` = alumnimajor
  , `3` = mumastersmajor
  , .id = 'majorrank'
  )
  

bestmajor  <- allmajors %>% 
  mutate(
      majorrank = as.numeric(majorrank)
    , majorrank = ifelse(major == '0000', majorrank + 50, majorrank)  # don't default to undecided
  ) %>% 
  group_by(pidm) %>% 
  filter(majorrank == min(majorrank, na.rm =T)) %>%  # need to figure this out 
  left_join(majors, by = 'major') %>% 
  summarize( rank = min(rank, na.rm = T)) %>% 
  left_join(heirarchy, by = 'rank') %>% 
  select(pidm, alumnisegment = letter)
    


# various exclusions ------------------------------------------------------

af19commits  <- commitsandmemos() %>% 
  filter(campaign == 'AF19') %>% 
  distinct(pidm) %>% 
  collect()

recurring  <- giftstbl %>% 
  filter(
      fisc_code == currentFY
    , gift_vehicle == 'RG'
  ) %>% 
  distinct(pidm) %>% 
  collect()


nomailsolc  <- tbl(commitsdb, "excl") %>% 
  filter(
      excl == "NOC" 
    | excl == "NMC"
    | excl == "NMS"
    | excl == "STUD"
  ) %>% 
  collect()


# mailable alumni ---------------------------------------------------------

cfadesgs  <- desgstbl %>% 
  filter(
      club_club == 'CFA' 
    | club_club == 'NCtd'
    | str_detect(desg_name, "Kirkland|KFAC")
  ) 

cfadonors  <- commitsandmemos() %>% 
  filter(fisc_code >= currentFY - 5) %>% 
  semi_join(cfadesgs, by = 'desg' )  %>% 
  collect()

mailablealumni  <- hallp %>% 
  filter(
      deceased == 'N'
    , !is.na(addr1)
  ) %>% 
  semi_join(dnrct %>% filter(dnrc_catg == "ALUM"), by = 'pidm')

mailablecfa <- hallp %>% 
  semi_join(cfadonors, by = 'pidm')  %>% 
  filter(
    !is.na(addr1)
  )



# pull alumni and CFA together ------------------------------------------------


mailableall  <- mailablealumni %>% 
  bind_rows(mailablecfa) %>% 
  distinct() %>% 
  anti_join(af19commits, by = 'pidm') %>% 
  anti_join(recurring  , by = 'pidm') %>% 
  anti_join(nomailsolc , by = 'pidm') %>% 
  filter(
      deceased == 'N'
    , !is.na(st)
  ) %>% 
  select(
      pidm
    , addr1:zip, keyline1, keyline2
    , primdnrind, sppidm, dnrc, classyr, spdnrc
    , prefname, cmname, frms, frmp, nckp, preffirstname
    , mumajor, alumnimajor, mumastersmajor
    , lifeg, lifememos
  ) %>% 
  left_join(predictive, by = 'pidm') %>% 
  left_join(affinity  , by = 'pidm') %>% 
  left_join(bestmajor , by = 'pidm') %>% 
  mutate(
      cfasegment = ifelse(pidm %in% (mailablecfa %>% select("pidm") %>% collect() %>% pluck("pidm")), "Other CFA", NA)
    , segment = coalesce(alumnisegment, cfasegment, "Other Alumni")
  ) %>% 
  filter(
      dnrc != 'STUD'
    , !str_detect (dnrc,   'TRUS|TRUE')
    , !str_detect2(spdnrc, 'TRUS|TRUE') 
  ) %>% 
  left_join(x = . , y = select(. , sppidm = pidm, spsegment = segment), by = 'sppidm') %>% 
  mutate(
      primdnrind = yesno.as.logical(primdnrind)
    , sponlist = sppidm %in% .$pidm
    , spsegmentsame = case_when(
          is.blank(sppidm) ~ T 
        , segment == coalesce(spsegment, "") ~ T
        , T ~ F
      )
    , usepluralsal = case_when(
          !sponlist ~ T
        , primdnrind & spsegmentsame & sponlist ~ T
        , T ~ F
      )
    , remove = !primdnrind & spsegmentsame & sponlist 
  ) %>% 
  filter(!remove) %>% 
  replace_na(list(
      affinity = 0
    , af19     = 0
    , lifememos = 0
    , segment  = 'Other Alumni' 
  )) %>% 
  mutate(
      target = affinity >= 8 | af19 >= .2 | ((lifeg + lifememos) > 100)
    , target2 = af19 > .08 | affinity > 6 # for groups that don't have a ton of people
  )
  
