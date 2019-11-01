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

if( !exists("mailableall")) {
  source("01 - build mailing list.R")
}


# figure out which segments need to be embiggened -------------------------


classagentsummary  <- mailableall %>% 
  mutate(
    type = case_when(
      target ~ "very likely"
      , target2 ~ "somewhat likely"
      , T ~ "unlikely"
    )
  ) %>% 
  count(segment, type) %>% 
  spread(type, n) %>% 
  mutate(
    sendto = case_when (
      `very likely` > 200 ~ "very likely only"
      , `very likely` + `somewhat likely` + `unlikely` > 400 ~ "very and somewhat"
      , T ~ "all"
    )
    , total = case_when(
      sendto == "very likely only"  ~ `very likely`
      , sendto == "very and somewhat" ~ `very likely` + `somewhat likely`
      , T ~ `very likely` + `somewhat likely` + `unlikely`
    )
    , sendtoall = sendto == 'all'
    , secondarytargets = sendto == 'very and somewhat'
  ) %>% 
  select(segment, `unlikely`, `somewhat likely`, `very likely`, sendto, total, sendtoall, secondarytargets) %>% 
  arrange(sendto, total)



sendtoallsegements  <- classagentsummary %>% 
  filter(
    sendtoall 
    , !secondarytargets
  )

secondarytargetsegments  <- classagentsummary %>% 
  filter(
    secondarytargets
  )


# initial pull ------------------------------------------------------------


targets <- mailableall %>% 
  filter(
    target 
    | segment == 'Other CFA' 
    | segment %in% sendtoallsegements$segment # segments with few alumni
    | (segment %in% secondarytargetsegments$segment & target2 ) # segments with a medium number of alumni
  ) %>% 
  arrange(segment, pidm) %>% 
  mutate(
    envelope_name = ifelse(usepluralsal, cmname, prefname)
    , letter_name   = ifelse(usepluralsal, nckp, preffirstname)
  )



# build the final mailing list --------------------------------------------


# Can't stack it because it's going to be letters and that'd be weird.
# stacked  <- targets %>% 
#   stackaddr(segment = segment) %>% 
#   arrange(finalsegment, pidm) 


finalmailinglist  <- targets %>% 
  select(
    pidm, segment
    , envelope_name 
    , letter_name 
    , addr1:zip
    , keyline1, keyline2
  )


finalsummary  <- finalmailinglist %>% 
  count(segment)

