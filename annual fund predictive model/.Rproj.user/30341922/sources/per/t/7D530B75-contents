# libraries -------------------------------------------

library(tidyverse); library(muadc); library(magrittr)
library(stringr); library(lubridate); library(beepr)

library(mumodels)
library(moments)


# functions ---------------------------------------------------------------


# convert giving amounts from continous into factors
factorizegiving  <- function(x, cutoff = c(5,1000) ) {
  
  factor(
    case_when(
      x <= cutoff[1]      ~ "little to none"
      , x < cutoff[2]     ~ "small gifts"
      , T                 ~ "bigger giving"
    )
    , levels = c('little to none', 'small gifts', 'bigger giving')
  )
  # ) %>% 
  # lvls_reorder( c(2,3,1))
}

# report on a vector's variation, class, # of NA valuse and skewness
analyze_col  <- function(x, colname = "col", outcome = NULL) {
  
  var = var(x)
  class = class(x)
  n_NAs = sum(is.na(x))
  skewness = skewness(as.double(x))
  
  results  <- data_frame(
    col = colname, var = var, class = class, n_NAs, skewness
  ) 
  
  if(!is.null(outcome)) {
    results  <- results %>% 
      mutate(
        outcome_cor_pval =  cor.test(as.numeric(x), as.numeric(outcome))$p.value
      ) 
  }
  
  return(results)
}


# call with "predict from" to build the data to predict from
#   looks at currentFY and the month of today to figure out what dataset to use 
make_build_vars  <- function(type = 'training', currentmonth = month(today()), thisfy = currentFY, thisaf = currentAF) {
  
  monthlag  <- ifelse(currentmonth >= 7, 1, 0)
  typelag  <- ifelse(type == 'training', 1, 0)
  
  revisedfy  <- thisfy - typelag - monthlag 
  
  list( 
    currentFY  = revisedfy
    ,   db  = paste0("fy",revisedfy - 2000, "_end_snapshot")
    ,   fystartdate  = ymd(paste(revisedfy-1,"07-01", sep = "-"))
    ,   currentAF  = str_replace(revisedfy, "^20", "AF")
    ,   prevAF  = str_replace(revisedfy-1, "^20", "AF")
    ,   prevAF2  = str_replace(revisedfy-2, "^20", "AF")
    ,   fy1  = revisedfy -1
    ,   fy2  = revisedfy -2
    ,   outcomeAF  =str_replace(revisedfy+1, "^20", "AF")
  )
  
  
  
}



# data ------------------------------------------------

buildvars  <- make_build_vars()


mydb        <- buildvars$db
currentFY   <- buildvars$currentFY
fystartdate <- buildvars$fystartdate
currentAF   <- buildvars$currentAF
prevAF      <- buildvars$prevAF
prevAF2     <- buildvars$prevAF2
fy1         <- buildvars$fy1
fy2         <- buildvars$fy2
outcomeAF   <- buildvars$outcomeAF

possibledateformats  <- c("ymd","m/d/y H:M:S p")


initcommitsdb(db = mydb)

hallp  <- hallptbl %>% collect()

acts <- tbl(commitsdb, "acts") %>% 
  left_join(tbl(commitsdb, "acts_catg"), by = 'act') %>% 
  collect()



# contact reports ---------------------------------------------------------


if (!exists('cont')) {
  cont  <- read.tidy('data/xacontrpt-all-time.csv') %>% # report of all contact reports for all time
    mutate(
      contact_date = dmy(contact_date)
    ) %>% 
    filter(contact_date < fystartdate)
}


contactsummary  <- cont %>% 
  mutate(
      contact_fy = fy(contact_date)
    , fy1contact = contact_fy == currentFY - 1 
    , fy2contact = contact_fy == currentFY - 2
  ) %>% 
  group_by(pidm) %>% 
  summarize(
    lifecontacts = n() - sum(contact_fy >= currentFY - 1) # ignore contacts after FY17
    , fy1contacts  = sum(fy1contact)
    , fy2contacts  = sum(fy2contact)
    , contactsin5yrs= sum(contact_fy >= (currentFY - 6)  & contact_fy < (currentFY-1))
  ) %>% 
  mutate(
    
    lifecontacts = factor(case_when(
        lifecontacts == 0 ~ 'no contacts'
      , lifecontacts <= 3 ~ 'few contacts' # mean = 3.2
      , lifecontacts >  3 ~ 'over 3 contacts'
    ), levels = c('no contacts', 'few contacts', 'over 3 contacts'))
    
    , contactsin5yrs = factor(case_when(
      contactsin5yrs == 0 ~ 'no contacts'
      , contactsin5yrs <= 2 ~ 'few contacts' # mean = 1.8
      , contactsin5yrs >  2 ~ 'over 2 contacts'
    ), levels = c('no contacts', 'few contacts', 'over 2 contacts'))
    
  ) %>% 
  select(pidm, lifecontacts, contactsin5yrs)


# outcomes ----------------------------------------------------------------



# drop back to regular database to get best giving data
initcommitsdb()



outcomes  <- giftsandmemos()  %>% 
  filter(campaign == prevAF ) %>% 
  select(pidm , giftamt = amt )  %>% 
  collect %>%
  group_by(pidm)  %>% 
  summarize(numgifts = n(), totalg = sum(giftamt))  %>% 
  mutate( outcome = ifelse(totalg > 1,'donor','no_gift'))  %>% 
  select( -numgifts, -totalg)


fy1gifts  <- giftstbl %>% 
  # filter(fisc_code == currentFY - 3 | fisc_code == currentFY - 1)  %>% # changed this, so "fy1" becomes "fy1-3"
  filter(fisc_code >= currentFY - 4, fisc_code < currentFY -1 )  %>% # changed this, so "fy1" becomes "fy1-3"
  select(pidm , giftamt = gift_amt, desg , campaign = campaign, campaign_type)  %>% 
  collect %>% 
  group_by(pidm)  %>% 
  summarize( 
    fy1giftsnum = n()
    , fy1totalg = sum(giftamt)
    , fy1numdesgs = n_distinct(desg)
    # , fy1afgifts = sum(ifelse(campaign == prevAF2 | campaign == prevAF,1,0),na.rm=T)
    , fy1afgifts = sum(ifelse(campaign_type == 'AF',1,0),na.rm=T)
  )  


n_gifts_memos_alltime  <- giftsandmemos() %>% 
  filter(fisc_code < currentFY) %>% 
  group_by(pidm) %>% 
  summarize(
    n_gifts_all_time = n()
  ) %>% 
  collect()



correctedyrsgiving  <- giftstbl  %>% 
  group_by(pidm)  %>%
  summarize(totyrsgivingfixed = n_distinct(fisc_code)) %>% 
  collect() 


# greek indicator ---------------------------------------------------------


greek  <- acts %>% 
  filter(act_catg == "Greek") %>% 
  distinct(pidm) %>% 
  collect() %>% 
  mutate(greek_ind = T)


athletics  <- acts %>% 
  filter(act_catg == "Sports (CCIW/NCAA)") %>% 
  distinct(pidm) %>% 
  collect() %>% 
  mutate(athletics_ind = T)



# bin class years ---------------------------------------------------------


classbreaks  <-  c(1945,1955,1965,1985,1995,2010,currentFY)

improvedclassyr  <- hallp %>% 
  select(pidm, classyr) %>% 
  mutate(
    classyrbin = cut(
      classyr
      , breaks = c(classbreaks, Inf)
      , labels = paste(classbreaks-1, "-", lead(classbreaks))) 
    , classyrbin = forcats::fct_expand(classyrbin, "0")
  ) %>%
  select(-classyr)


# bin days since last event -----------------------------------------------


fakenulldate  <- hallp %>% 
  filter(pidm == 1) %>% 
  pull(lasteventdt)


actualeventdays  <- hallp %>% 
  select(pidm, lasteventdt) %>% 
  replace_na(list(lasteventdt = fakenulldate)) %>% 
  mutate(
      n_days = difftime(today(), lasteventdt) %>% as.numeric()
    , yrs_since_last_event = (n_days/365) %>% ifelse(. > 30, 30, .)
  ) %>% 
  select(pidm, yrs_since_last_event)





#  begin building predictors ------------------------------------------------------------------------

# start from hallp, the general fact table with all constituents
h2  <- hallp  %>% 
  
  #remove dead people, estates, organizations, etc
  filter(   deceased == 'N'
            , !grepl('EST|STUD|CORP|FOU',primdnrc)
  )  %>% 
  
  # get outcomes
  left_join(outcomes,by = 'pidm')   %>% 
  
  # corrected numbergifts, for orgs
  left_join(n_gifts_memos_alltime, by = 'pidm') %>% 
  
  # IMPORTANT NOTE! if you're going to focus on precision or recall when building
  #   models (and I really think you ought to focus on recall/sensitivity), then you
  #   need to ensure that the positive class is the FIRST level--at least if you're using
  #   caret to build your models. 
  #   
  #   Specifying which is the positive class is really important with PR -- if you don't
  #   know which is positive, you very well might optimize the majority class, which is
  #   exactly what you DON'T want.
  mutate( 
      outcome = fillna(outcome, "no_gift")
    , outcome = factor(outcome, levels = c('donor', 'no_gift'))
  )  %>% 
  
  # get fy1 giving info
  left_join(fy1gifts, by = 'pidm')  %>% 
  
  mutate( 
      fy1giftsnum = replace(fy1giftsnum,   is.na(fy1giftsnum),   0)
    , fy1totalg   = replace(fy1totalg,     is.na(fy1totalg),     0)
    , fy1numdesgs = replace(fy1numdesgs,   is.na(fy1numdesgs),   0)
    , fy1afgifts  = replace(fy1afgifts.y,  is.na(fy1afgifts.y),  0)
    , n_excls = ifelse( is.na(excl), 0, str_count(excl,"~") + 1)
    , n_acts = ifelse(acts == "" | is.na(acts), 0, str_count(acts,"~") + 1)
  )  %>% 
  
  # greek indicator
  left_join(greek, by = 'pidm') %>% 
  mutate( greek_ind = fillna(greek_ind, F)) %>% 
  
  # athletics indicator
  left_join(athletics, by = 'pidm') %>% 
  mutate( athletics_ind = fillna(athletics_ind, F)) %>% 
  
  
  # classyear bins
  left_join(improvedclassyr, by = 'pidm') %>% 
  mutate(classyrbin = fillna(classyrbin, "0")) %>% 
  

  # days since last event
  left_join(actualeventdays, by = 'pidm') %>% 
  mutate(yrs_since_last_event = fillna(yrs_since_last_event, "30")) %>% 
  
  
  # contact reports
  select(-lifecontacts, -fy0contacts, -fy1contacts) %>% 
  left_join(contactsummary, by = 'pidm') %>% 
  replace_na(list(
      lifecontacts   = 'no contacts'
    , contactsin5yrs = 'no contacts'
  )) %>% 
  
  # corrected yrs giving
  left_join(correctedyrsgiving, by= 'pidm')  %>% 
  
  
  # build predictors
  mutate(
    
    hasaddr = !is.blank(addr1)
    , hasphone = !is.blank(homephone) | !is.blank(cellphone) | !is.blank(buphone)
    , hasemail = !is.blank(prefemail)
    , hasbuaddr = !is.blank(buaddr1)
    # , hasbuphone = !is.blank(buphone) # removed for af19
    # , prefemailisMU = grepl('@millikin.edu',prefemail) # removed dues to low var
    
    , decaturregion    = grepl('(^|~)DECATUR($|~)',    region)
    , decatur1hrregion = grepl('(^|~)DECATURHR($|~)',  region)
    , chicago1hrregion = grepl('(^|~)CHICAGO1HR($|~)', region)
    , illinois         = fillna(st == 'IL', F)
    
    , alumnicollege    = factor(fillna(alumnicollege,"none"))
    
    , genderfactor     = factor(fillna(gender,"unknown"))
    , primdnrind       = yesno.as.logical(primdnrind)
    
    , isalum          = grepl('(^|~)ALUM|ALMP|ALMM($|~)',dnrc)   # is degreed alum 
    , isalmn          = grepl('(^|~)ALMN($|~)',dnrc)             # is non-degreed
    , isfact          = grepl('(^|~)FAC(T|I|C|P|R|E)($|~)',dnrc) # is faculty
    , isfactformer    = grepl('(^|~)FACF($|~)',dnrc)             # is former faculty
    , isparent        = grepl('(^|~)PRNT($|~)',dnrc)
    , isparentformer  = grepl('(^|~)PRNF($|~)',dnrc)
    , istrustee       = grepl('(^|~)TRU.($|~)',dnrc)
    , isotherdnrc     = grepl('OTHR|OTHM|OTHA|ALMS|ADMH|ALMH|ALMC',dnrc)
    
    , nocontact = grepl('NOC',excl)                              # opted out of all communication
    , noother   = grepl('NMC|NMS|NME|NES|NPS|NPC|NFC|NSC', excl) # opted out of soliciation
    , nomail    = grepl('NMC|NMS',excl)                          # oped out of mail contact
    , noofexcl  = factor(case_when(                              # how many communications have they opted out of
         n_excls == 0 ~ "no exclusions"
       , n_excls == 1 ~ "one exclusion"
       , n_excls > 1 ~  "two or more exclusions"
       )
       , levels = c('no exclusions', 'one exclusion', 'two or more exclusions'))
    
    , hasemployer = employer != "" & !is.na(employer)
    
    , hasspouse = !is.blank(splastname)
    , hasconstspouse = !is.blank(sppidm)
    , numdegreedincouple = grepl('ALUM|ALMM|ALMP',dnrc) + grepl('ALUM|ALMM|ALMP',spdnrc)
    
    , numactivities = factor(case_when(                           # number of clubs, sports, etc
        n_acts == 0 ~ 'no activities'
        , n_acts == 1 ~ 'one activity'
        , n_acts <= 6 ~ 'few activities'
        , n_acts  > 6 ~ 'lots of activities' 
      ), levels = c('no activities', 'one activity', 'few activities', 'lots of activities')
    )
    
    , dayssincelastgift  = mumodels:::bindates(lastgiftdt, possibledateformats)
    
    # , hasmg = !is.blank(matchinggiftco) # low var
    
    , lifeg = factor(case_when(lifeg < 5 ~ "little to none", lifeg < 1000 ~ "small gifts", T ~ "bigger giving"))
    , lifeg = lvls_reorder(lifeg, c(2,3,1))
    
    , yrsgiving = as.numeric(yrsgiving)
    , yrsgiving = coalesce(yrsgiving,totyrsgivingfixed,0)
    , yrsgiving = cut(
        yrsgiving
      , breaks = c(-1,0,1,4,Inf)
      , labels = c("never", "1 yr", "less than 4 yrs", "more than 4 yrs")
    )
    
    
    # , fy1giftsnum = factorizegiving(fy1giftsnum, cutoff = c(0, 3)) # removed in af20 due to high coorrelation
    , fy1morethan1desg = fy1numdesgs > 1
    , fy1totalg   = factorizegiving(fy1totalg,    cutoff = c(5, 1000))
    , fy2totalg   = factorizegiving(fy2gifts,     cutoff = c(5, 1000))
    , lifeevents   = factorizegiving(lifeevents,   cutoff = c(0, 1))
    , numbergifts  = factorizegiving(pmax(numbergifts, n_gifts_all_time, na.rm = T),  cutoff = c(0, 1))
    
    
    
  )  



# select variables --------------------------------------------------------

# need to review this - see if I've actually got what I think/want
predictors  <- h2  %>% 
  select(
      pidm
    , outcome
    , lifeg
    , yrsgiving
    , lifeevents
    , numbergifts
    , outcome
    , fy1totalg
    , greek_ind
    , athletics_ind
    , lifecontacts
    , hasaddr
    , hasphone
    , hasemail
    , hasbuaddr
    , decaturregion
    , classyrbin
    , genderfactor
    , isalum
    , isalmn
    , isfact
    , isfactformer
    , isparent
    , isparentformer
    , isotherdnrc
    , nocontact
    , noother
    , noofexcl
    , hasemployer
    , hasspouse
    , hasconstspouse
    , numdegreedincouple
    , numactivities
    , yrs_since_last_event
    , dayssincelastgift
    , fy1morethan1desg
    , isalum
    , istrustee
    , decatur1hrregion
    , chicago1hrregion
    , illinois
    
  )

