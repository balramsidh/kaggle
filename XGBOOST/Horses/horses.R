# Xgboost homework/practice 
# link : https://www.kaggle.com/rtatman/machine-learning-with-xgboost-in-r-workbook/notebook

# Install all needed packages in one operation, and then be smart.
###

packages = c("xgboost","tidyverse","DiagrammeR")

package.check <- lapply(packages, FUN = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

# reading the data 

df.horses <- read.csv( file = "horse.csv")


# reshuffling 

set.seed(1234)
df.horses <- df.horses[sample(1:nrow(df.horses)),]


# step: Remove information about the target variable from the training data

df.horses_input <- df.horses %>%
  select(-c(outcome,hospital_number,cp_data)
  
