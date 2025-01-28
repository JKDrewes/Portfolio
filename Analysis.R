source("/home/nixy/Desktop/Personal_Projects/Health_Coverage_IL/Census_Data.R")
library(tidyverse)
library(caret)
library(glmnet)
library(xgboost)
library(Matrix)

dV = dummyVars(HICOV ~ .,  adult_il_sample)
dV.adult_il_sample = predict(dV, adult_il_sample)

train_control = trainControl(method = "cv", 
                             number = 5,
                             classProbs = FALSE,
                             savePredictions = TRUE
                             )

#Logistic regression
logit.model = train(x = dV.adult_il_sample,
                    y = adult_il_sample$HICOV,
                    method = "glmnet", # for regularization (ridge or lasso)
                    tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, length = 10)), #Specifics of regularization
                    trControl = train_control,
                    family = 'binomial',
                    preProcess = c("center", "scale")
)
print("Fit the Logit model")

coef(logit.model$finalModel)

#XGBoost
xgb_grid = expand.grid(
  nrounds = c(2000, 4000), #Our n is possibly quite large so we include larger nrouound hyperparameter
  max_depth = c(1, 2), #We don't have a large feature set, 1 makes it a simple additive model and 2 prevents over-fitting
  eta = c(0.001, 0.01), #standard choices
  gamma = c(0, 2, 4), #We tune gamma here because we have shallow trees. This allows us to penalize complexity some.
  colsample_bytree = c(0.75), #Set here below 1 in order to de-correlate trees
  min_child_weight = c(0, 2, 4), #stopping parameter
  subsample = c(.1, .5, .75) #bagging rate is variable
)

xgb.model = train(HICOV ~., 
                  data = adult_il_sample,
                  method = "xgbTree",
                  trControl = train_control,
                  metric = "Accuracy",
                  tuneGrid = xgb_grid,
                  verbosity = 0 #supressing warning because caret is calling ntree_limit instead of iteration. This will not affect current code versions
                  )
print("Ran XGBoost on the data")

xgb.importance(model = xgb.model$finalModel)