### XGBoost
# https://www.kaggle.com/rtatman/machine-learning-with-xgboost-in-r/

# Install all needed packages in one operation, and then be smart.
###

packages = c("xgboost","tidyverse","DiagrammeR")

package.check <- lapply(packages, FUN = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

## reading the input file 

diseaseInfo <-read.csv("Outbreak_240817.csv")




# testing and training using sampling 
set.seed(1234)

diseaseInfo <- diseaseInfo[sample(1:nrow(diseaseInfo)),]

## removing info about target variable 

diseaseInfo_humansRemoved <- diseaseInfo %>%
  select(-starts_with("human"))

## getting labels 

diseaseLabels <- diseaseInfo %>%
  select(humansAffected) %>%  # to get column with # of humans affected
  is.na() %>% # check if na
  magrittr::not() # reverse the result of is.na, ie: false means: humans are affected but i need to change it to True.

# reduce the redundant information 
# removing id, long lat(as country is already there), removing any non numeric features 

diseaseInfo_numeric <- diseaseInfo_humansRemoved %>%
  select(-Id) %>%
  select(-c(longitude,latitude)) %>%
  select_if(is.numeric)


# converting categorial to numeric 

# one hot encoding for country 

region <- model.matrix(~country-1,diseaseInfo)

head(region)

# for species description 
summary(diseaseInfo$speciesDescription)

diseaseInfo_numeric$is_domestic <- as.numeric(str_detect(diseaseInfo$speciesDescription,"domestic"))


# now we are checking for species type which is stored in second str of species description


speciesList <- diseaseInfo$speciesDescription %>%
  str_replace("[[:punct:]]", "") %>% # remove punctuation (some rows have parentheses)
  str_extract("[a-z]*$") # extract the least word in each row

# convert our list into a dataframe...
speciesList <- tibble(species = speciesList)

# and convert to a matrix using 1 hot encoding
options(na.action='na.pass') # don't drop NA values!
species <- model.matrix(~species-1,speciesList)



#########

# combining diseaseinfo_numeric, region and species together 

diseaseInfo_numeric <- cbind(diseaseInfo_numeric,region,species)

class(region
      )

# converting into matrix as xgboost only works with matrix 
diseaseInfo_matrix <- data.matrix(diseaseInfo_numeric)

######### test and train ######

# next step is to convert into test(30%) and train data(70%)

train_len <- round(nrow(diseaseInfo_matrix)*0.7)

# training data 
train_data <- diseaseInfo_matrix[1:train_len,]
train_lables <- diseaseLabels[1:train_len,]

# test data
test_data <- diseaseInfo_matrix[-(1:train_len),]
test_lables <- diseaseLabels[-(1:train_len),]

########## converting data into dmatrix ######

dtrain <- xgb.DMatrix(data=train_data, label = train_lables)

dtest <- xgb.DMatrix(data=test_data, label=test_lables)


########## Model Training ##########

model <- xgboost(data=dtrain,
                 nround=2,
                 objective = 'binary:logistic')
# so both models have same error, that means our second model is not an improvement on our 1st model 

########## checking performance for test data #### 

pred <- predict(model,dtest)

# get & print the classification error
err <- mean((pred > 0.5) != test_lables)
print(paste("test-error=", err))


### "test-error= 0.0121520972167777" this means that our model is not overfit as error is less than the error in train model

######### Tuning our model 

#  overfitting 
#  we can reduce the depth of each tree by paramenet max.depth
#  the default depth is 6

model <- xgboost(data=dtrain,
                 max.depth=3,
                 nrounds=2,
                 objective="binary:logistic")

# testing 

pred <- predict(model, dtest)

err <- mean((pred >0.5) != test_lables)
print(paste("test-error",err))

# no improvement since there was no overfitting to start with

############# Further tuning
# 1. early stopping rounds : stop if there is no improvment after this many rounds
# 2. scale_pos_weight : to take care of imbalanced class. so in our case most cases are false, so we need our ensemble model to have weights for each model accordingly 
# 3. Gamma: for regularization 

negative_cases=sum(diseaseLabels == FALSE)
positive_cases=sum(diseaseLabels == TRUE)


model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 3, # the maximum depth of each decision tree
                       nround = 10, # number of boosting rounds
                       early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
                       objective = "binary:logistic", # the objective function
                       scale_pos_weight = negative_cases/positive_cases, # control for imbalanced classes
                       gamma = 1) # add a regularization term

pred <- predict(model_tuned,dtest)

err <- mean((pred>0.5) != test_lables)
print(paste("test error after tuning :", err))


###### Examining the model ######

# plot them features! what's contributing most to our model?
xgb.plot.multi.trees(feature_names = names(diseaseInfo_numeric), 
                     model = model_tuned)

importance_matrix <- xgb.importance(names(diseaseInfo_numeric), model = model)

names(diseaseInfo_numeric)

# and plot it!
xgb.plot.importance(importance_matrix)
