###train & test split
set.seed(414)
split = createDataPartition(y = analysis$price,p = 0.8,list = F,groups=1470) # groups are ~5% of total number of rows
train = analysis[split,]
test = analysis[-split,]

###Gradient boost tree
library(gbm)
# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3), 
  interaction.depth = c(1, 3, 5), 
  n.minobsinnode = c(5, 10, 15), 
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               
  min_RMSE = 0                    
)

nrow(hyper_grid)

# randomize data
random_index <- sample(1:nrow(train), nrow(train))
random_train <- train[random_index, ]

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(414)
  
  # train model
  gbm.tune <- gbm(
    formula = random_train$price ~ .,
    distribution = "gaussian",
    data = random_train,
    n.trees = 5000, 
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75, 
    n.cores = NULL, 
    verbose = FALSE
  )
  
 
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)



####run gbm with best values
set.seed(414)
gbm.fit1 <- gbm(
  formula = price ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 4978,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  bag.fraction = .65, 
  train.fraction = 1, 
  cv.folds = 4, 
  n.cores = NULL,
  verbose = FALSE
)


sqrt(min(gbm.fit1$cv.error))
test.predict<- predict(gbm.fit1, test, ntree=1150)
sqrt(mean((test.predict-test$price)^2))

####submission gbm
gbmpred = predict(gbm.fit1, scoring)

submissionFilexgb = data.frame(id = scoring$id, price = gbmpred)
write.csv(submissionFilexgb, 'Submission_gbm.csv', row.names = F)

##random forest
library(randomForest)
rf = randomForest(price ~ .,data = train, ntree=100)
rf_predict = predict(rf, test)
rf_error = sqrt(mean((rf_predict-test$price)^2))
rf_error

rf2 = randomForest(price ~ .,data = train, ntree=1000)
rf_predict2 = predict(rf2, test)
rf_error2 = sqrt(mean((rf_predict2-test$price)^2))
rf_error2

rfpred = predict(rf2, scoring)

submissionFilexgb = data.frame(id = scoring$id, price = rfpred)
write.csv(submissionFilexgb, 'Submission_rf.csv', row.names = F)
