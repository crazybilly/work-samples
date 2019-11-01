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


currentmanual  <- read.tidy('data/All_Const_w_All_Four_Manual_Attrs_12_04_2018_05.49.34_PM.csv')

allemailsegments <- hallp %>% 
  # anti_join(af19commits, by = 'pidm') %>% 
  # anti_join(recurring  , by = 'pidm') %>% 
  left_join(bestmajor  , by = 'pidm') %>% 
  # filter(
  #     !is.blank(prefemail)
  #   , !str_detect2(excl, "NOC|NES|NEC")
  #   , !is.blank(deceased)
  # ) %>% 
  mutate(
    type = case_when(
        pidm %in% bestmajor$pidm ~ "alumni"
      , pidm %in% cfadonors$pidm ~ "cfa"
      , str_detect(dnrc,'ADMH|ALMM|ALMN|ALUM') ~ "alumni"
      , str_detect(dnrc, "PRN") ~ "parent"
      , T                       ~ "others"
    )
    , segment = case_when(
         pidm %in% recurring$pidm ~ "recurring FY19"
       , pidm %in% af19commits$pidm | sppidm %in% af19commits$pidm ~ "already made AF19 commit"
       , str_detect(dnrc, "TRUS|TRUE") | str_detect2(spdnrc, "TRUE|TRUS")  ~ "trustee"
       , str_detect(dnrc, "FACT|FACI|FACC|FACP") ~ "current faculty"
       , str_detect(dnrc, "STUD") ~ "student"
       , !is.blank(alumnisegment) ~ alumnisegment
       , pidm %in% cfadonors$pidm ~ "Other CFA"
       , str_detect(dnrc, "ADMH") ~ "Nursing"
       , str_detect(dnrc, "ALMM") ~ "Other Business"
       , T                        ~ "no group" # includes current/former parents and others
       # , str_detect(dnrc, "AL")   ~ "Other Alumni"
       # , type == 'alumni'         ~ "Other Alumni"
       # , T                        ~ "???"
    )
  ) %>% 
  left_join(currentmanual, by = 'pidm') %>% 
  select(
      pidm
    , manual1 = segment
    , manual2 = manual4
    , manual3 = manual1.y
    , manual4 = manual2.y
  )


# write files -------------------------------------------------------------

# gotta do it in batches because H won't accept anything over 1.5MB 
writeH  <- function(df = allemailsegments, nsegs = 6, x ) {
  
  filename = paste0("output/to upload to H/FY19-class-agent-email-segments.2018-12-06.batch", x, ".csv")
  
  df %>% 
    filter(row_number() %% nsegs == x) %>% 
    write.tidy(filename)
  
}

writefiles  <- rstudioapi::showQuestion("Write files?", "Should files be written to 'output/to upload to H' ?", ok = "Write")

if(writefiles) {
  map(0:5, ~ writeH(x = .x))
}




beep()