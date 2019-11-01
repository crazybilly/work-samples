# libraries -------------------------------------------

library(lubridate); library(beepr)
library(tidyverse); library(muadc); library(magrittr)
library(readxl); library(rlang)
library(formattable); library(fundRaising)

# data ------------------------------------------------

if( !exists("caret_combined_results")) {
  source("02-build-test-models.R")
}


# pull all the metrics from each model and take a look at them in one spreadsheet
#    see model-metrics.xlsx 
#    Look at allresults in Excel and make some decisions about trade-offs in accuracy
#    Don't forget that 'Balanced Accuracy' = AUC
allresults  <- ls(pattern = "_results$") %>% 
  map_dfr(~ get(.x) %>% pluck("metrics")) %>% 
  arrange(-sensitivity)

# NOTES FROM PREVIOUS YEARS (added to each year)
# 
# in FY16, one of the caret models won (the downsampled one, if I remember right).
# in FY17, the combined rf model wins:
#     The AUC is comparable to the sampled_rf and caret_sampled models, but it predicted more donors.
#     Whether or not this is more accurate, I don't care--when we're talking about a rare occurence, 
#        I'd prefer a model that incorrectly identifies people as donors, over one that that doesn't.
#     See output/compare-model-accuracy.xlsx for a color coded, abbreviated version of checkaccuracy
# for FY19, combined rf model won again:
#     I trained the caret models with AUPRC (ie. precision/recall) as the metric. The caret_sampled model 
#     was almost as good as the combined_rf model, but the latter identified 260 more donors AND had a 
#     slightly better AUPRC.
# for FY20, trained models with focus on sensitivity (ie. recall) - more than anything, we want to make sure we're not
#     ignoring folks who are donors. With that in mind. Precision is cool, but honestly, I'd rather have false
#     positives (isn't that the point of the model, anyway? to identify people who OUGHT to be donors but aren't)
#     With that in mind, the caretcombined model won--the recall was 99% (it only missed 14 donors). It's important
#     to note, though, that' it's a generous model: it identified over 10K people as possible donors (as opposed to other
#     models which only identified 3000, or 1400!). That makes this a much different model than in years past (and it'll be
#     more wrong that in years past. On the other hand, hopefully, it'll be more useful--rather than dropping the
#     cutoff down to 30% or so, we can actually use a 50% cutoff (or higher!).



# rebuild caret combined  -------------------------------------------------
#   with a focus on sensitivity, rather than build time


message(paste("Caret Combined training started at "), Sys.time())

finalmodel <- train(
    x = predictors %>% select(-pidm , -outcome)
  , y = predictors$outcome
  , classwt = c(.3, .7)
  , strata = predictors$outcome
  , sampsize = c(sampsize,sampsize)
  , trControl = trainControl(summaryFunction = twoClassSummary, classProbs = T )  # originally number = 3, I think it jumps to 25 now
  , metric = 'Sens'
)

final_model_results <- assess_model(finalmodel, trainingdata = predictors, testingdata = predictors)







