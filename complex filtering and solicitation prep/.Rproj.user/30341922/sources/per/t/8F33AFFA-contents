
# This function rebuilds the mailing list so that you can test out different levels for
#   affinity and af predictive score to get the numbers to come in where you want them.
#   I used it in AF19 to figure out what "secondary targets" should be for segments that had 
#   few obvious targets, but a lot of non-target folks (so you can't just mail to everyone in the segment)


# libraries -------------------------------------------

library(lubridate); library(beepr)
library(tidyverse); library(muadc); library(magrittr)
library(readxl); library(rlang)
library(formattable); library(fundRaising)

# functions ---------------------------------------------------------------


buildCA  <- function(afscore, affinityscore, origdf) {
  
  mailableallf  <- origdf %>% 
    mutate(
      target = affinity >= 8 | af19 >= .2 | ((lifeg + lifememos) > 100)
      , target2 = af19 > afscore | affinity > affinityscore # for groups that don't have a ton of people
    )
  
  classagentsummary  <- mailableallf %>% 
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
  
  targets <- mailableallf %>% 
    filter(
      target 
      | segment == 'Other CFA' 
      | segment %in% sendtoallsegements$segment # segments with few alumni
      | (segment %in% secondarytargetsegments$segment & target2 ) # segments with a medium number of alumni
    ) # %>% 
    # mutate(
    #     sponlist = sppidm %in% .$pidm
    #   , primdnrind = yesno.as.logical(primdnrind)
    # ) 
    # 
  # 
  # tosend  <- targets %>% 
  #   filter(
  #       primdnrind 
  #       | (!primdnrind & !sponlist)
  #   )
  # 
  # stacked  <- targets %>% 
    # stackaddr(segment = segment)
  
  
  list(
      # stacked = nrow(stacked)
      targets = nrow(targets)
    # , tosend = nrow(tosend)
    , summarydf = classagentsummary
  )
  
  
}



# data --------------------------------------------------------------------

if( !exists("mailablealumni")) {
  source("01 - pull mailable alumni.R")
}


testingdf  <-  mailablealumni %>% 
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
    , primdnrind, sppidm, dnrc, classyr
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
  left_join(targets %>% select(sppidm = pidm, spsegment = segment), by = 'sppidm') %>% 
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
  ))


# test the numbers --------------------------------------------------------


afscores  <- seq(from = .02, to = .1, by=.005)
affinityscores <- seq(from = 2, to = 6, by = 1)

possibilities  <- expand.grid(af = afscores, affinity = affinityscores) 

# foo is mailableall minus the last mutate
testing  <- map2(possibilities$af, possibilities$affinity, ~buildCA(.x, .y, testingdf) )


testingresults  <- possibilities %>% 
  bind_cols(targets = testing %>% map_int("targets"))


