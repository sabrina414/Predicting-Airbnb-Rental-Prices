library(h2o)
h2o.init() #needs Java JDK 64 bit installed
#https://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
#h2o.shutdown(prompt = TRUE) #in case the local host server(?) becomes slow or crashes
analysis12 = subset(analysis,
                                 select = -c(len_house_rules, minimum_nights_avg_ntm, len_space, last_review))


library(caret)
set.seed(414)
split = createDataPartition(y = analysis$price,p = 0.8,list = F,groups=1850) # groups are ~5% of total number of rows
train = analysis[split,]
test = analysis[-split,]
#need to convert normal dfs into h2o compatible data frames
train.hex <- as.h2o(train, destination_frame = "train.hex")
test.hex <- as.h2o(test, destination_frame = "test.hex")
drops <- c("price","id")

#setting up the model with tuned paramters
hbm = h2o.gbm(x = colnames(train[ , !(names(train) %in% drops)]),
              y = "price",
              training_frame = train.hex,
              validation_frame = test.hex,
              ntrees = 4000, max_depth = 11,
              seed = 414, 
              learn_rate = 0.008, 
              learn_rate_annealing = 1,
              sample_rate = .65, sample_rate_per_class = NULL, col_sample_rate = .5,
              col_sample_rate_change_per_level = 1,
              col_sample_rate_per_tree = .5,
              min_split_improvement = 1e-06)


#view train and test rmse along with other information
summary(hbm)
#train RMSE is 17.60582 and test is 48.10652

#View variable importance
View(h2o.varimp(hbm))


#5. Building the model using all of the analysis data####
library(h2o)
h2o.init()

analysis.hex <- as.h2o(analysis12, destination_frame = "analysis.hex")
scoring.hex <- as.h2o(scoring, destination_frame = "scoring.hex")
drops <- c("price","id")

hbms = h2o.gbm(x = colnames(analysis12[ , !(names(analysis12) %in% drops)])
               ,y = "price",
               training_frame = analysis.hex,
               ntrees = 4000, max_depth = 11,
               seed = 100, 
               learn_rate = 0.008, 
               learn_rate_annealing = 1,
               sample_rate = .65, sample_rate_per_class = NULL, col_sample_rate = .5,
               col_sample_rate_change_per_level = 1,
               col_sample_rate_per_tree = .5,
               min_split_improvement = 1e-06)
summary(hbms)
#train rmse is 18.2618

#View feature importance once again
View(h2o.varimp(hbms))

#Submission file creation#####
hpreds = h2o.predict(hbms,scoring.hex)
submissionFile = data.frame(id = scoring$id, price = as.vector(hpreds))
write.csv(submissionFile, 'Submission7.csv',row.names = F)



hyper_grid <- list(
   max_depth = c(5, 7,11),
   min_rows = c(1, 5, 10),
   learn_rate = c(0.01, 0.05, 0.1),
   learn_rate_annealing = c(.99, 1),
   sample_rate = c(.5, .75, 1),
   col_sample_rate = c(.8, .9, 1)
)
# perform grid search 
   
 grid <- h2o.grid(
    algorithm = "gbm",
    grid_id = "gbm_grid1",
    x = colnames(train[ , !(names(train) %in% drops)]),
    y = "price",
    training_frame = train.hex,
    validation_frame = test.hex,
    hyper_params = hyper_grid,
    ntrees = 5000,
    stopping_rounds = 10,
    stopping_tolerance = 0,
    seed = 414
  )
 
 grid_perf <- h2o.getGrid(
    grid_id = "gbm_grid1", 
    sort_by = "mse", 
    decreasing = FALSE
 )
 grid_perf 
 
 
 
 search_criteria <- list(
   strategy = "RandomDiscrete",
   stopping_metric = "mse",
   stopping_tolerance = 0.005,
   stopping_rounds = 10,
   max_runtime_secs = 240*60
 )
 
 # perform grid search 
   grid <- h2o.grid(
     algorithm = "gbm",
     grid_id = "gbm_grid2",
     x = colnames(train[ , !(names(train) %in% drops)]),
     y = "price",
     training_frame = train.hex,
     validation_frame = test.hex,
     hyper_params = hyper_grid,
     search_criteria = search_criteria, # add search criteria
     ntrees = 5000,
     stopping_rounds = 10,
     stopping_tolerance = 0,
     seed = 123
   )

   
   grid_perf <- h2o.getGrid(
     grid_id = "gbm_grid2", 
     sort_by = "mse", 
     decreasing = FALSE
   )
   grid_perf

   
   # Grab the model_id for the top model, chosen by validation error
   best_model_id <- grid_perf@model_ids[[1]]
   best_model <- h2o.getModel(best_model_id)
   
   # Now let¡¯s get performance metrics on the best model
   h2o.performance(model = best_model, valid = TRUE)
  
   #setting up the model with tuned paramters
   hbm = h2o.gbm(x = colnames(train[ , !(names(train) %in% drops)]),
                 y = "price",
                 training_frame = train.hex,
                 validation_frame = test.hex,
                 ntrees = 4000, max_depth = 11,
                 seed = 414, 
                 learn_rate = 0.01, 
                 learn_rate_annealing = 1,
                 min_rows = 1,
                 sample_rate = .5, sample_rate_per_class = NULL, col_sample_rate = 0.8,
                 col_sample_rate_change_per_level = 1,
                 col_sample_rate_per_tree = .5,
                 min_split_improvement = 1e-06,
                 stopping_rounds = 10,
                 stopping_tolerance = 0,
                 nfolds = 5)
   
   
   
   
   hbms = h2o.gbm(x = colnames(train[ , !(names(train) %in% drops)]),
                 y = "price",
                 training_frame = analysis12.hex,
                 ntrees = 4000, max_depth = 7,
                 seed = 414, 
                 learn_rate = 0.01, 
                 learn_rate_annealing = 1,
                 min_rows = 5,
                 sample_rate = .75, sample_rate_per_class = NULL, col_sample_rate = 0.9,
                 col_sample_rate_change_per_level = 1,
                 col_sample_rate_per_tree = .5,
                 min_split_improvement = 1e-06,
                 stopping_rounds = 10,
                 stopping_tolerance = 0,
                 nfolds = 5
   )
   analysis12.hex <- as.h2o(analysis12, destination_frame = "analysis12.hex")
   scoring.hex <- as.h2o(scoring, destination_frame = "scoring.hex")
   drops <- c("price","id")
   