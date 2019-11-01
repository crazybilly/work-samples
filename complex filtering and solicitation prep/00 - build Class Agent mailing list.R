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


# pull all the class agent segments as well as non-alumni who are CFA prospects
#   final object: mailableall
source('01 - pull mailable alumni.R', echo = T)


# builds the actual mailing list
#   final object: finalmailinglist 
#                 (also finalsummary is available)
source('02 - finalize mailing list.R', echo = T)



# if finalmailinglist comes out too big (or too small), 
# use this to figure out what your secondary target criteria should be
# iterates through affinity and AF19 predictive score to see what the numbers will be like
#   final object: testingresults
# source('10-test-totals.R', echo = T)



# builds the manual attributes we need to import into Hobsons
# so we can run email filters against it
#   final object: allemailsegments
source('03-prep-segments-for-H.R', echo = T)


beep(3)