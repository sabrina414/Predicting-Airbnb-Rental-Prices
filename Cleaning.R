all = subset(rbind(analysis, scoring),
             select = -c(beds, number_of_reviews_ltm,host_has_profile_pic,street,city,country,state,
                         smart_location,country_code,require_guest_profile_picture,
                         market,jurisdiction_names,has_availability,
                         require_guest_phone_verification,license, requires_license))


#Check correlations between numeric variabels and price
library(corrplot)
numericVars <- which(sapply(all, is.numeric)) 
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") 
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
print(cor_sorted)
#selecting only correlations higher than 0.1
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot(cor_numVar,method='square', type= 'lower', diag = F)

#remove beds since it has high correlation with bedrooms and accommodates
all <- subset(all, select = -beds)

##function to combine levels accoring to the threshold from the table
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
ggplot(all, aes(x=bedrooms,y=price)) + geom_smooth()
all$bedrooms = combine_levels(all$bedrooms,30,6)
table(all$bedrooms)

table(all$property_type)
all$property_type = as.factor(combine_levels(all$property_type,90,"Other"))
table(all$property_type)

table(all$bathrooms)
all$bathrooms = combine_levels(all$bathrooms,50,5)
table(all$bathrooms)

table(all$cancellation_policy)
all$cancellation_policy = combine_levels(all$cancellation_policy,50,"strict")

table(all$guests_included)
all$guests_included = combine_levels(all$guests_included,50,9)

table(all$host_listings_count)
ggplot(all, aes(x=host_listings_count,y=price)) + geom_smooth()
all$calculated_host_listings_count = combine_levels(all$calculated_host_listings_count,200,10)



###no transform on bed type

###convert date
date <- mdy('11/14/2019')
library(dplyr)
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

###



#number of amenities 别放交上去的
x = strsplit(all$amenities, ",")
y = c()
for (i in 1:length(x)){
  y[i] = length(x[[i]])
}
all$num_amenities = y

#whether a host has multiple listings 也别放
greaterThanOne <- function(n){
  if(n>1){
    return(1)
  }else{
    return(0)
  }
}
all$multiple_host_listings = unlist(lapply(all$calculated_host_listings_count,
                                           greaterThanOne),use.names=FALSE)


##remove other useless thing 
allclean= subset(all,select = -c(name, summary,space,description,neighborhood_overview,notes,
                             transit,access,interaction,house_rules, host_name, host_since,
                             host_about,host_verifications, host_response_rate, 
                             host_response_time, host_neighbourhood, host_acceptance_rate))



### Impute missing value with median(结果把price也impute分不开了，手动分)
allclean = predict(preProcess(all, method = 'medianImpute'), newdata = allclean)


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
noname = c()
for (i in 1:length(ame)) {
  name = str_replace_all(noquote(ame[i])," ","_")
  if (sum(allclean[,name]) < 1000) {
      allclean[,name] <- NULL
      noname = c(noname,name) } }

####cleaning all csv
write.csv(allclean, "all_clean.csv",row.names = F)

##load cleaned data
allclean <- read.csv('all_clean_mannual1.csv',stringsAsFactors = F)
analysis = allclean[!is.na(allclean$price),] 
scoring  = allclean[is.na(allclean$price),]
analysis = na.omit(analysis)



#test if encoding works for gbm prediction

model_data = subset(allclean, select = model_var)

##create dummy variables with one-hot encoding
library(caret)
dummy = dummyVars(" ~ .", data = model_data, drop2nd = T) 

#creating new columns for dummy variables 管用了!!!!
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

library(glmnet)
analysis1= subset(analysis, select = -c(id, host_location, neighbourhood_cleansed))
x1 = model.matrix(price~.-1, data = analysis)
y1 = analysis$price
cv.lasso1 = cv.glmnet(x1,y1,alpha=1)
coef(cv.lasso1)

####方便点
coef(cv.lasso1);preds <- coef(cv.lasso1)
non_zero_preds = which(preds[,1] != 0.0)
pred_names = names(non_zero_preds[2:length(non_zero_preds)])
preds_string = paste(pred_names, collapse=" + ")
lm_formula = paste0("price ~ ", preds_string)






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

model_data = subset(analysis, select = model_var)

##model_data$neighbourhood_group_cleansed<- as.factor(model_data$neighbourhood_group_cleansed)
##model_data$property_type <- as.factor(model_data$property_type)
##model_data$room_type <- as.factor(model_data$room_type)

##create dummy variables with one-hot encoding
library(caret)
dummy = dummyVars(" ~ .", data = model_data, drop2nd = T) 

#creating new columns for dummy variables 
model_ready = data.frame(predict(dummy, newdata = model_data)) 

####do the same for scoring
model_scoring = subset(scoring, select = model_var)

dummy = dummyVars(" ~ .", data = model_scoring, drop2nd = T) 

scoring_ready = data.frame(predict(dummy, newdata = model_scoring)) 

scoring_data=data.matrix(model_ready)

###covegboost 哪里出错了？？？？
xgb_data= data.matrix(model_ready)

####boost model
library(xgboost)
params <- list(
  eta = 0.01,
  max_depth = 6,
  subsample = 0.8,
  min_child_weight = 7,
  colsample_bytree = 1
)
xgbmodel <- xgboost(
  params = params,
  data = xgb_data,
  label = model_ready$price,
  nrounds = 4000,
  objective = "reg:linear"
  
)

####convert scoirng data to matrix

scoring_ready = data.frame(predict(dummy, newdata = scoring))

###covert to matrix for xgboost
xgb_scoring= data.matrix(scoring_ready)




##write submission

xgbpred = predict(xgbmodel, xgb_scoring)
submissionFilexgb = data.frame(id = scoring$id, price = xgbpred)
write.csv(submissionFilexgb, 'Submission3.csv', row.names = F)

## Model Results Analysis
importance_matrix <- xgb.importance(model = xgbmodel)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix, top_n = 15)










