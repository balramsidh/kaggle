# Balram Sidh 01/06/2018 
# Housing problem from Kaggel 

# Solution : https://www.kaggle.com/tannercarbonati/detailed-data-analysis-ensemble-modeling/notebook
require(ggplot2) # for data visualization
require(stringr) #extracting string patterns
require(Matrix) # matrix transformations
require(glmnet) # ridge, lasso & elastinet
require(xgboost) # gbm
require(randomForest)
require(Metrics) # rmse
require(dplyr) # load this in last so plyr doens't overlap it
require(caret) # one hot encoding
require(scales) # plotting $$
require(e1071) # skewness
require(corrplot) # correlation plot
# loading the train and test data files 

train <- read.csv(file="train.csv", stringsAsFactors = FALSE
                  )
test <- read.csv(file="test.csv", stringsAsFactors = FALSE)

df.combined <- rbind(within(train, rm('Id','SalePrice')), within(test, rm('Id')))

dim(df.combined
    )

# Trying to find all the missing values 

na.cols <- which(colSums(is.na(df.combined)) > 0)

sort(colSums(sapply(df.combined[na.cols], is.na)), decreasing = TRUE)

paste('There are', length(na.cols), 'columns with missing values')

# helper function for plotting categoric data for easier data visualization
plot.categoric <- function(cols, df){
  for (col in cols) {
    order.cols <- names(sort(table(df[,col]), decreasing = TRUE))
    
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    
    print(num.plot)
  }
}

 plot.categoric('PoolQC',df.combined)

# Checking if poolarea is >0 and poolqc is still NA
 
 df.combined[(df.combined$PoolArea > 0) & is.na(df.combined$PoolQC),c('PoolQC','PoolArea')] # found 3 such rows
 
# %>% is a sort of function which is one of the packages.
 
# Finding the mean area in each category and then populating 3 missing values of poolqc based on the pool area
 df.combined[,c('PoolQC','PoolArea')] %>%
   group_by(PoolQC) %>%
   summarise(mean = mean(PoolArea), counts = n())  

 df.combined[2421,'PoolQC'] = 'Ex'
 df.combined[2504,'PoolQC'] = 'Ex'
 df.combined[2600,'PoolQC'] = 'Fa'
 df.combined$PoolQC[is.na(df.combined$PoolQC)] = 'None'
 
 
 # Checking GarageYrBlt 
 
 # first checking if the garage was built at the same time when the house was build 
 length(which(df.combined$GarageYrBlt == df.combined$YearBuilt))
 # so 2216 houses have the garage built at the same time as the house
 
 # replacing NA values of GarageYrBuilt with YearBuilt 
 
idx <- which(is.na(df.combined$GarageYrBlt))

df.combined[idx, 'GarageYrBlt'] <- df.combined

# Now about other garage colums, we are checking if garagearea and garagecars are 0
garage.cols <- c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')
df.combined[is.na(df.combined$GarageQual),garage.cols]

# since only one value of garagearea is finite, we can replace is manually

# For the house with GarageArea = 360 and GarageCars = 1, but NA’s in the other columns, we can use the most frequent values for each columns from houses with a similar area and car count.

idx <- which(((df.combined$GarageArea < 370) & (df.combined$GarageArea > 350)) & (df.combined$GarageCars == 1))
idx 

names(sapply(df.combined[idx, garage.cols], function(x) sort(table(x), decreasing=TRUE)[1]))

df.combined[2127,'GarageQual'] = 'TA'
df.combined[2127, 'GarageFinish'] = 'Unf'
df.combined[2127, 'GarageCond'] = 'TA'

# we can fill in any missing numeric values with 0 and categoric with ‘None’ since these houses recorded having 0 area and 0 cars in their garage.

for (col in garage.cols) {
  if (sapply(df.combined[col], is.numeric) == TRUE ) {
    df.combined[sapply(df.combined[col], is.na),col] = 0
  }
  else {
    df.combined[sapply(df.combined[col], is.na),col] = 'None'
  }
}

# Moving on to Kitchen and electrical, both has only one missing value. Populating both by most freequent value

plot.categoric('Electrical', df.combined)

which(is.na(df.combined$Electrical))

df.combined[1380,"Electrical"] <- "SBrkr"

plot.categoric('KitchenQual', df.combined)

df.combined[sapply(df.combined$KitchenQual, is.na), "KitchenQual"] <- "TA"


# Now checking for basement factors 

bsmt.cols <- names(df.combined)[sapply(names(df.combined), function(x) str_detect(x, 'Bsmt'))]

df.combined[is.na(df.combined$BsmtExposure), bsmt.cols]

plot.categoric('BsmtExposure', df.combined)

#Almost all of the missing values for each categoric basement feature comes from houses with 0 on each features corresponding to area. We can fill in these values with ‘None’ since these houses certainly don’t have basements. Rows 949, 1488 and 2349 are the only missing values from BsmtExposure, we can fill this with No as that is the most frequent value and these houses most likely don’t have any exposure for their basements. The rest of the basement columns corresponding to area will be filled with 0 since they likely don’t have a basement and the categoric missing values will be filled with NoBsmt.

df.combined[c(949, 1488, 2349), 'BsmtExposure'] = 'No'

df.combined_bkp <- df.combined
for ( col in bsmt.cols) {
  if (sapply(df.combined[col], is.numeric) == TRUE) {
    df.combined[sapply(df.combined[col], is.na), col ] = 0
      }
    else{
      df.combined[sapply(df.combined[col],is.na), col] = 'None'
    }
}

which(colSums(sapply(df.combined_bkp, is.na)) >0)

# Exterior variables 

names(df.combined)[sapply(names(df.combined), function(x) str_detect(x, 'Exter'))]

idx <- which(is.na(df.combined$Exterior1st) | is.na(df.combined$Exterior2nd))

df.combined[idx,c("Exterior1st","Exterior2nd")]

#There is only 1 missing value for Exterior1st and Exterior2nd coming from the same hosue and there aren’t any other features that can help us predict what variable should be filled so we can fill this with ‘Other’ since the NA is likely due to having an exterior cover that is not listed.

df.combined[which(is.na(df.combined$Exterior1st)), 'Exterior1st'] <- 'Other'
df.combined$Exterior2nd[is.na(df.combined$Exterior2nd)] = 'Other'

# Saletype, functional, utilities all have less than 3 missing values

#checking relation b/w saletype and salecondition 

df.combined[is.na(df.combined$SaleType), 'SaleCondition']

table(df.combined$SaleCondition, df.combined$SaleType)

# since most common saletype for salecondition=normal is WD, updating missing value with WD

df.combined[is.na(df.combined$SaleType), 'SaleType'] <- 'WD'

# Functional 

plot.categoric('Functional',df.combined)

which(is.na(df.combined$Functional))

df.combined[c(2217,2474), 'Functional'] <- 'Typ'

# Utilities 

plot.categoric('Utilities',df.combined)

#Utilities only has 1 value for NoSeWa and the rest AllPub. We can drop this feature from our dataset as the house with ‘NoSeWa’ is from our training set and will have won’t help with any predictive modelling

which(df.combined$Utilities == 'NoSeWa') # in the training data set

col.drops <- c('Utilities')

df.combined <- df.combined[,!names(df.combined) %in% c('Utilities')]

# MSZoing 

df.combined[is.na(df.combined$MSZoning),c('MSZoning','MSSubClass')]

plot.categoric('MSZoning', df.combined)

table(df.combined$MSZoning, df.combined$MSSubClass)

df.combined$MSZoning[c(2217, 2905)] = 'RL'
df.combined$MSZoning[c(1916, 2251)] = 'RM'

# MasVnrtype, MasVnrArea

df.combined[is.na(df.combined$MasVnrArea) | is.na(df.combined$MasVnrType), c('MasVnrType','MasVnrArea')]

# since only one value has area populated, we need to calculate mean area grouped by type
na.omit(df.combined[,c('MasVnrType','MasVnrArea')]) %>%
  group_by(na.omit(MasVnrType)) %>%
  summarise(MedianArea = median(MasVnrArea,na.rm = TRUE), counts = n()) %>%
  arrange(MedianArea)

plot.categoric('MasVnrType', df.combined)

df.combined[2611, 'MasVnrType'] = 'BrkFace'

#The areas we can replace with 0 and types can be replaced with ‘None’

df.combined$MasVnrType[is.na(df.combined$MasVnrType)] = 'None'
df.combined$MasVnrArea[is.na(df.combined$MasVnrArea)] = 0

# LotFrontage 

names(df.combined)[sapply(names(df.combined),function(x) str_detect(x,'Lot'))]

idx <- which(is.na(df.combined$LotFrontage))

df.combined[idx,c('LotArea','LotFrontage')]

df.combined$LotFrontage[idx]

# lotfrontage is not dependent on lotarea. Its the length of street connected to the front of the lot.
# we are assuming that all frontages in a neighbourhood are similarish

df.combined['Nbrh.factor'] <- factor(df.combined$Neighborhood, levels = unique(df.combined$Neighborhood))

lot.by.nbrh <- df.combined[c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage, na.rm=TRUE))

lot.by.nbrh

#updating LotFrontage 


df.combined <- df.combined_bkp

idx <- which(is.na(df.combined$LotFrontage))
idx
for (i in idx) {
df.combined[i,'LotFrontage']  <- lot.by.nbrh[ lot.by.nbrh$Neighborhood == df.combined$Neighborhood[i] ,'median']
}

# Fence Quality 
which(is.na(df.combined$Fence))

plot.categoric('Fence',df.combined)

#We can replace any missing vlues for Fence and MiscFeature with ‘None’ as they probably don’t have this feature with their property.

df.combined$Fence[is.na(df.combined$Fence)] = 'None'


table(df.combined$MiscFeature)
df.combined$MiscFeature[is.na(df.combined$MiscFeature)] = 'None'

# Fireplace

(df.combined)[sapply(names(df.combined), function(x) str_detect(x,'Fire'))]

which((df.combined$Fireplaces > 0) & (is.na(df.combined$FireplaceQu)))

df.combined$FireplaceQu[is.na(df.combined$FireplaceQu)] = 'None'

# Alley 

plot.categoric('Alley',df.combined)

# mentioned in the data description file that NA means no alley access 

df.combined$Alley[is.na(df.combined$Alley)] = 'None'

paste('There are', sum(sapply(df.combined, is.na)), 'missing values left')
