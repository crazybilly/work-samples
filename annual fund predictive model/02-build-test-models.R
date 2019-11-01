# libraries -------------------------------------------

library(tidyverse); library(muadc); library(magrittr)
library(stringr); library(lubridate); library(beepr)


library(randomForest)
library(caret)
library(broom)
library(PRROC)


# data ------------------------------------------------

if( !exists("predictors" )) {
  source('01-builddata.R')
}


message(paste("Training started at "), Sys.time())


# functions ---------------------------------------------------------------

# auprcSummary <- function(data, lev = NULL, model = NULL){
#   prob_good <- data$donor #take the probability of good class
#   the_curve <- pr.curve(scores.class0 = prob_good,
#                         weights.class0 = as.numeric(data$obs)-1, #provide the class labels as 0/1
#                         curve = FALSE)
#   out <- the_curve$auc.integral
#   names(out) <- "AUPRC"
#   out
# }

# calculate the Area Under a Precision/Recall Curve for a model/data
calc_auprc <- function(model, data){
  
  index_donors <- data$outcome == "donor"
  index_nogift <- data$outcome == "no_gift"
  
  predictions <- predict(model, data, type = "prob")
  
  pr.curve(predictions[,1][index_donors],
           predictions[,1][index_nogift],
           curve = TRUE)
  
}



# convert a confusion matrix to a data frame
confusion_to_df  <- function(conf, name) {
  
  conf$byClass %>% 
    as.data.frame_named.vector("val") %>% 
    mutate(model = name) %>% 
    spread(names, val) %>% 
    clean.df()
}


# assess model performance, returning a list of performance metrics
assess_model  <- function(model, modelname = deparse(substitute(model)), testingdata = testing, trainingdata = training) {
  
    predictions  <- predict(model, testingdata)
     
    
    n_donors  <- sum(predictions == 'donor')
    pct_donors  <- n_donors/nrow(testingdata)
    
    confmatrix  <- confusionMatrix(predictions, testingdata$outcome, positive = 'donor')
    prcurve  <- calc_auprc(model, testingdata)
    
    
    list(
        modelname    = modelname
      , model        = model
      , trainingdata = trainingdata
      , testingdata  = testingdata
      , predictions  = predictions
      , confusion    = confmatrix
      , prcurve      = prcurve
      , metrics      = confmatrix %>% 
          confusion_to_df(modelname) %>% 
          mutate(
              n_donors = n_donors
            , pct_donors = pct_donors
            , auprc      = prcurve$auc.integral
          )
    )
    
}


# reference model ---------------------------------------------------------

trainindex  <- createDataPartition(predictors$outcome, p = .5, list = F)

training <- predictors[trainindex ,]
testing  <- predictors[-trainindex,]
sampsize  <- training %>% 
  filter(outcome == 'donor') %>% 
  nrow()


plainrf <- randomForest(
  x = training %>% select(-pidm , -outcome)
  , y = training$outcome
)

plainresults  <- assess_model(plainrf, "plainrf")


# build sampled model -----------------------------------------------------


trainindex  <- createDataPartition(predictors$outcome, p = .5, list = F)

training <- predictors[trainindex ,]
testing  <- predictors[-trainindex,]
sampsize  <- training %>% 
  filter(outcome == 'donor') %>% 
  nrow()


sampledrf_one_to_one <- randomForest(
  x = training %>% select(-pidm , -outcome)
  , y = training$outcome
  , strata = training$outcome
  , sampsize = c(sampsize,sampsize)
)


sampled_results  <- assess_model(sampledrf_one_to_one, "sampledrf_one_to_one")

# build weighted model ----------------------------------------------------


trainindex  <- createDataPartition(predictors$outcome, p = .5, list = F)

training <- predictors[trainindex ,]
testing  <- predictors[-trainindex,]
sampsize  <- training %>% 
  filter(outcome == 'donor') %>% 
  nrow()


weightedrf  <- randomForest(
  x = training %>% select(-pidm , -outcome)
  , y = training$outcome
  , classwt = c(.1, .9)
)

weighted_results  <- assess_model(weightedrf, "weightedrf")

# build combined model ----------------------------------------------------


trainindex  <- createDataPartition(predictors$outcome, p = .5, list = F)

training <- predictors[trainindex ,]
testing  <- predictors[-trainindex,]
sampsize  <- training %>% 
  filter(outcome == 'donor') %>% 
  nrow()


combinedrf  <- randomForest(
  x = training %>% select(-pidm , -outcome)
  , y = training$outcome
  , classwt = c(.1, .9)
  , strata = training$outcome
  , sampsize = c(sampsize, sampsize)
)


combined_results  <- assess_model(combinedrf, "combinedrf")

# caret plain -----------------------------------------
#   this takes forever...you really should have reduced the number of bootstraps, with trControl

message(paste("Caret training started at "), Sys.time())



trainindex  <- createDataPartition(predictors$outcome, p = .5, list = F)

training <- predictors[trainindex ,]
testing  <- predictors[-trainindex,]
sampsize  <- training %>% 
  filter(outcome == 'donor') %>% 
  nrow()


caretmodel  <- train(
    x = training%>% select(-pidm , -outcome)
  , y = training$outcome
  # , weights = c(.3, .7)
  , strata = training$outcome
  # , sampsize = c(6775,1300)
  , trControl = trainControl(number = 3, summaryFunction = twoClassSummary, classProbs = T )  # originally 10, use 3 for testing
  , metric = 'Sens'
)


caret_plain_results  <- assess_model(caretmodel, modelname = "caretmodel")

# caret weighted ------------------------------------------------------

message(paste("Caret Weighted training started at "), Sys.time())

trainindex  <- createDataPartition(predictors$outcome, p = .5, list = F)

training <- predictors[trainindex ,]
testing  <- predictors[-trainindex,]
sampsize  <- training %>% 
  filter(outcome == 'donor') %>% 
  nrow()

caretweightmodel  <- train(
  x = training %>% select(-pidm , -outcome)
  , y = training$outcome
  , classwt = c(.1, .9)
  , strata = training$outcome
  # , sampsize = c(6775,1300)
  , trControl = trainControl(number = 3, summaryFunction = twoClassSummary, classProbs = T )  # originally 10
  , metric = 'Sens'
)


caret_weighted_results  <- assess_model(caretweightmodel, modelname = "caretweightmodel")


# caret oversampling ------------------------------------------------------


message(paste("Caret Oversampling training started at "), Sys.time())

trainindex  <- createDataPartition(predictors$outcome, p = .5, list = F)

training <- predictors[trainindex ,]
testing  <- predictors[-trainindex,]
sampsize  <- training %>% 
  filter(outcome == 'donor') %>% 
  nrow()

caretovermodel  <- train(
  x = training %>% select(-pidm , -outcome)
  , y = training$outcome
  # , weights = c(.3, .7)
  , strata = training$outcome
  , sampsize = c(sampsize,sampsize)
  , trControl = trainControl(number = 3, summaryFunction = twoClassSummary, classProbs = T )  # originally 10
  , metric = 'Sens'
)

caret_weighted_results  <- assess_model(caretweightmodel, modelname = "caretweightmodel")


# caret combined ----------------------------------------------------------


message(paste("Caret Combined training started at "), Sys.time())

trainindex  <- createDataPartition(predictors$outcome, p = .5, list = F)

training <- predictors[trainindex ,]
testing  <- predictors[-trainindex,]
sampsize  <- training %>% 
  filter(outcome == 'donor') %>% 
  nrow()

caretcombinedmodel  <- train(
  x = training %>% select(-pidm , -outcome)
  , y = training$outcome
  , classwt = c(.3, .7)
  , strata = training$outcome
  , sampsize = c(sampsize,sampsize)
  , trControl = trainControl(number = 3, summaryFunction = twoClassSummary, classProbs = T )  # originally 10
  , metric = 'Sens'
)

caret_combined_results  <- assess_model(caretcombinedmodel, "caretcombinedmodel")





# finish ------------------------------------------------------------------


message(paste("Training completed at "), Sys.time())
save.image('modelsbuilt.RData')


beep(3)



# allresults  <- ls(pattern = "_results$") %>% 
#   map_dfr(get) %>% 
#   arrange(-sensitivity)