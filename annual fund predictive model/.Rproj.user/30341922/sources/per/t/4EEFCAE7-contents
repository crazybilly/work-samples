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


if( !exists("topredictfrom")) {
  source("04-build-data-to-predict-from.R")
}


if( !exists("finalmodel")) {
  source("03-assess-and-build-final-model.R")
}


# build predictions -------------------------------------------------------


# build predictions from final model
finalpredictions  <- predict(finalmodel, topredictfrom, type = 'prob' )

# prep the final scores to upload to the database
toupload  <- data_frame(
    pidm = topredictfrom$pidm
  , af20 = finalpredictions$donor
)



# write to disk -----------------------------------------------------------


# ask for confirmation to write to file
writeupdates <- rstudioapi::showQuestion("Write output?", "Do you want to write the scores to 'output/predictive-scores.csv'?")

if(writeupdates) {
  write.tidy(toupload, "output/predictive-scores.csv")
}







beep()