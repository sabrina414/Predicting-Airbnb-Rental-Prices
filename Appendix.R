###library all required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(glmnet)
library(lubridate)
library(randomForest)
library(gbm)
library(stringr)
library(caret)
RNGversion(vstr =3.6)

## Read the data, remove price=0 and add price column to scoring data to help the cleaning process
analysis <- read.csv('Kaggle/analysisData.csv',stringsAsFactors = F)
scoring <- read.csv('Kaggle/scoringData.csv',stringsAsFactors = F)
analysis <- analysis[!analysis$price==0,] 
scoring$price = NA 

## Combine two dataset for cleaning, and drop some useless variables based on exploration
all <- subset(rbind(analysis, scoring),
              select = -c(host_has_profile_pic,street,city,country,state,
                          smart_location,country_code,require_guest_profile_picture,
                          market,jurisdiction_names,has_availability,
                          require_guest_phone_verification,license, requires_license))


##Check correlations between numeric variabels and price
numeric_vars <- which(sapply(all, is.numeric)) 
corr_num <- cor(all[, numeric_vars],use="pairwise.complete.obs") 
corrplot(corr_num,method='square', type= 'lower', diag = F)
sorted_corr <- as.matrix(sort(corr_num[,'price'], decreasing = TRUE))
sorted_corr


#remove beds since it has high correlation with bedrooms and accommodates
all <- subset(all, select = -beds)


##After about 6 bedrooms, the impact on price not stable, so encoding all bedrooms>6 to 6. Similarly, encoding other variables.
ggplot(analysis, aes(x=bedrooms,y=price)) + geom_smooth()

combine_levels = function(column,threshold,target)
{
  value = levels(as.factor(column))
  t = table(column)
  for(i in value){
    if (t[i] < threshold)
    {column[column == i] <- target}
  }
  return(column)
}

table(all$bedrooms)
all$bedrooms = combine_levels(all$bedrooms,30,6)
table(all$bedrooms)

table(all$property_type)
all$property_type = as.factor(combine_levels(all$property_type,90,"Other"))
table(all$property_type)

table(all$host_listings_count)
ggplot(all, aes(x=host_listings_count,y=price)) + geom_smooth()
all$calculated_host_listings_count = combine_levels(all$calculated_host_listings_count,200,10)
#same for cancellation policy, bathroom and guests, code omitted here


###convert date
date <- mdy('11/14/2019')
all <- mutate(all, host_days = as.numeric(difftime(date,all$host_since)))
all <- mutate(all, first_review = as.numeric(difftime(date,all$first_review)))
all <- mutate(all, last_review = as.numeric(difftime(date,all$last_review)))

###count length of reviews or other text
all$len_name = nchar(all$name)
all$len_summary = nchar(all$summary)
all$len_space = nchar(all$space)
all$len_notes = nchar(all$notes)
all$len_description = nchar(all$description)
all$len_neighborhood_overview = nchar(all$neighborhood_overview)
all$len_transit = nchar(all$transit)
all$len_access = nchar(all$access)
all$len_house_rules = nchar(all$house_rules)
all$len_host_about = nchar(all$host_about)

##remove other useless thing 
allclean= subset(all,select = -c(name, summary,space,description,neighborhood_overview,notes,
                                 transit,access,interaction,house_rules, host_name, host_since,
                                 host_about,host_verifications, host_response_rate, 
                                 host_response_time, host_neighbourhood, host_acceptance_rate))

###deal with ameneties

allclean$amenities <- gsub("\\{","", allclean$amenities)
allclean$amenities <- gsub("\\}","", allclean$amenities)
allclean$amenities <- gsub("\"","", allclean$amenities)
library(stringr)
ame <- sort(unique(unlist(strsplit(as.character(allclean$amenities), ','))))
for(i in 1:length(ame)) {
  name = str_replace_all(noquote(ame[i])," ","_")
  allclean[, name] <- 0
  num = i+70
  allclean[str_detect(allclean$amenities,ame[i]), num] <- 1
}

#drop uncommon amenities
for (i in 1:length(ame)) {
  name = str_replace_all(noquote(ame[i])," ","_")
  if (sum(allclean[,name]) < 1000) {
    allclean[,name] <- NULL} }


##something wrong here, there are lots of duplicate columns with "_" ahead, so I mannually cleaned those columns.


### Impute missing value with median, but price was imputed as well, so I delete the price for scoring data manually
allclean = predict(preProcess(all, method = 'medianImpute'), newdata = allclean)
####cleaning all csv
write.csv(allclean, "all_clean.csv",row.names = F)

##load cleaned data
allclean <- read.csv('all_clean_mannual1.csv',stringsAsFactors = F)
##create dummy variables with one-hot encoding
dummy = dummyVars(" ~ .", data = model_data, drop2nd = T) 

#creating new columns for dummy variables 
model_ready = data.frame(predict(dummy, newdata = model_data)) 

analysis = model_ready[!is.na(model_ready$price),] 
scoring  = model_ready[is.na(model_ready$price),]
analysis = na.omit(analysis)


##variable selection (take too much time, use lasso only)
start_mode = lm(price~., data=allclean)
empty_mode = lm(price~1, data=allclean)
full_mode = lm(price~., data=allclean)
forwardstep<- step(start_mode,scope = list(upper= full_mode, lower= empty_mode), 
                   direction = 'forward')
hybridstep <- step(start_mode, scope = list(upper= full_mode, lower= empty_mode), 
                   direction = 'both')

analysis1= subset(analysis, select = -c(id, host_location, neighbourhood_cleansed))
x1 = model.matrix(price~.-1, data = analysis)
y1 = analysis$price
cv.lasso1 = cv.glmnet(x1,y1,alpha=1)
coef(cv.lasso1)


###select variables for modeling
model_var = c( "price", "id","host_is_superhost", "neighbourhood_group_cleansed","property_type",
               "room_type", "accommodates", "bedrooms", "bathrooms", 
               "security_deposit", "cleaning_fee", "guests_included", "extra_people",
               "minimum_nights", "minimum_nights_avg_ntm",
               "availability_30", "availability_90", "availability_365",
               "number_of_reviews", "last_review", "review_scores_rating",
               "review_scores_cleanliness", "review_scores_location",
               "review_scores_value", "calculated_host_listings_count","reviews_per_month",
               "host_days", "Elevator", "Dryer", "Doorman", "Dishwasher",
               "Cable_TV_.1", "Breakfast", "Suitable_for_events_",
               "Paid_parking_on_premises_", "Long_term_stays_allowed_",
               "Lock_on_bedroom_door_", "Indoor_fireplace_",
               "Garden_or_backyard_", "Fire_extinguisher_","len_space",
               "Cooking_basics_", "Building_staff_", "len_house_rules",
               "Hangers", "Gym","Free_parking_on_premises_", "Free_street_parking_",  
               "Wheelchair_accessible_", "TV", "translation_missing._en.hosting_amenity_49_",
               "Shampoo", "Paid_parking_off_premises_", "Microwave", "Keypad")


##split train&test
set.seed(414)
split = createDataPartition(y = analysis$price,p = 0.8,list = F,groups=1470) 
train = analysis[split,]
test = analysis[-split,]



##random forest (I've tried it with different combination of variables, here are only part of them)
rf = randomForest(price ~ .,data = train, ntree=100)
rf_predict = predict(rf, test)
rf_error = sqrt(mean((rf_predict-test$price)^2))
rf_error

rf2 = randomForest(price ~ .,data = train, ntree=1000)
rf_predict2 = predict(rf2, test)
rf_error2 = sqrt(mean((rf_predict2-test$price)^2))
rf_error2

rfpred = predict(rf2, scoring)


##gbm model (I've tried lots of combinations of hyperparameter, this might not be my best submission, but is the only one I can find now)
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

