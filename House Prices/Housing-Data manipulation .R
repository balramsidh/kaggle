# Balram Sidh 01/09/2018 
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


# We now need to convert categoric features into numeric to develop a prediction model 

# starting with seperating numeric features 

num_features <- names(which(sapply(df.combined, is.numeric)))
cat_features <- names(which(sapply(df.combined, is.character)))

cat_features

df.numeric <- df.combined[num_features]

# We are trying to see the median house price for each categorial values and go from there 

group.df <- df.combined[1:1460,] # for the training set 
group.df$SalePrice <- train$SalePrice # adding saleprice


# function that groups a column by its features and returns the mdedian saleprice for each unique feature. 
group.prices <- function(col) {
  group.table <- group.df[,c(col, 'SalePrice', 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.Quality = round(mean(OverallQual),2),
              mean.Price = mean(SalePrice), n = n()) %>%
    arrange(mean.Quality)
  
  print(qplot(x=reorder(group.table[[col]], -group.table[['mean.Price']]), y=group.table[['mean.Price']]) +
          geom_bar(stat='identity', fill='cornflowerblue') +
          theme_minimal() +
          scale_y_continuous(labels = dollar) +
          labs(x=col, y='Mean SalePrice') +
          theme(axis.text.x = element_text(angle = 45)))
  
  return(data.frame(group.table))
}

## functional to compute the mean overall quality for each quality
quality.mean <- function(col) {
  group.table <- df.combined[,c(col, 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.qual = mean(OverallQual)) %>%
    arrange(mean.qual)
  
  return(data.frame(group.table))
}


# function that maps a categoric value to its corresponding numeric value and returns that column to the data frame
map.fcn <- function(cols, map.list, df){
  for (col in cols){
    df[col] <- as.numeric(map.list[df.combined[,col]])
  }
  return(df)
}


###### features with quality and condition 

qual.cols <- c('ExterQual', 'ExterCond', 'GarageQual', 'GarageCond', 'FireplaceQu', 'KitchenQual', 'HeatingQC', 'BsmtQual')

group.prices('FireplaceQu')
group.prices('BsmtQual')

# all feature in qual.cols have following categories, mapping them from 0 to 5 
qual.list <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


df.numeric <- map.fcn(qual.cols, qual.list, df.numeric)


# moving on to next features 

group.prices('BsmtExposure')

bsmt.list <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

df.numeric = map.fcn(c('BsmtExposure'), bsmt.list, df.numeric)

group.prices('BsmtFinType1')

#Here returning the mean sale prices might not be as helpful as computing the median basement areas for both columns to determine which quality is better than the other.

# visualization for BsmtFinTyp2 instead of another table
df.combined[,c('BsmtFinType1', 'BsmtFinSF1')] %>%
  group_by(BsmtFinType1) %>%
  summarise(medianArea = median(BsmtFinSF1), counts = n()) %>%
  arrange(medianArea) %>%
  ggplot(aes(x=reorder(BsmtFinType1,-medianArea), y=medianArea)) +
  geom_bar(stat = 'identity', fill='cornflowerblue') +
  labs(x='BsmtFinType2', y='Median of BsmtFinSF2') +
  geom_text(aes(label = sort(medianArea)), vjust = -0.5) +
  scale_y_continuous(limits = c(0,850)) +
  theme_minimal()

# Through investigating the relationships between the basement quality and areas we an see the true order of qualities of each basement to be ‘None’ < ‘Unf’ < ‘LwQ’ < ‘BLQ’ < ‘Rec’ < ‘ALQ’ < ‘GLQ’.

bsmt.fin.list <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2,'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
df.numeric <- map.fcn(c('BsmtFinType1','BsmtFinType2'), bsmt.fin.list, df.numeric)

# functional, garagefinish and fence

group.prices('Functional')

functional.list <- c('None' = 0, 'Sal' = 1, 'Sev' = 2, 'Maj2' = 3, 'Maj1' = 4, 'Mod' = 5, 'Min2' = 6, 'Min1' = 7, 'Typ'= 8)

df.numeric['Functional'] <- as.numeric(functional.list[df.combined$Functional])

group.prices('GarageFinish')

garage.fin.list <- c('None' = 0,'Unf' = 1, 'RFn' = 1, 'Fin' = 2)

df.numeric['GarageFinish'] <- as.numeric(garage.fin.list[df.combined$GarageFinish])


group.prices('Fence')

fence.list <- c('None' = 0, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)

df.numeric['Fence'] <- as.numeric(fence.list[df.combined$Fence])

MSdwelling.list <- c('20' = 1, '30'= 0, '40' = 0, '45' = 0,'50' = 0, '60' = 1, '70' = 0, '75' = 0, '80' = 0, '85' = 0, '90' = 0, '120' = 1, '150' = 0, '160' = 0, '180' = 0, '190' = 0)

df.numeric['NewerDwelling'] <- as.numeric(MSdwelling.list[as.character(df.combined$MSSubClass)])


#########################

# need the SalePrice column
corr.df <- cbind(df.numeric[1:1460,], train['SalePrice'])

# only using the first 1460 rows - training data
correlations <- cor(corr.df)
# only want the columns that show strong correlations with SalePrice
corr.SalePrice <- as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))

corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))

corr.idx

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)


#####
#Now for some of the nominal variables we can take one of the categories that is distict from the others and create a binary feature that returns 1 if the house has that specific value and 0 if it does not.
plot.categoric('LotShape', df.combined)

df.numeric['RegularLotShape'] <- (df.combined$LotShape == 'Reg') * 1

plot.categoric('LandContour', df.combined)

df.numeric['LandLeveled'] <- (df.combined$LandContour == 'Lvl') * 1

plot.categoric('LandSlope', df.combined)

df.numeric['LandSlopeGentle'] <- (df.combined$LandSlope == 'Gtl') * 1

plot.categoric('Electrical', df.combined)

df.numeric['ElectricalSB'] <- (df.combined$Electrical == 'SBrkr') * 1

plot.categoric('GarageType', df.combined)

df.numeric['GarageDetchd'] <- (df.combined$GarageType == 'Detchd') * 1

plot.categoric('PavedDrive', df.combined)

df.numeric['HasPavedDrive'] <- (df.combined$PavedDrive == 'Y') * 1

df.numeric['HasWoodDeck'] <- (df.combined$WoodDeckSF > 0) * 1

df.numeric['Has2ndFlr'] <- (df.combined$X2ndFlrSF > 0) * 1

df.numeric['HasMasVnr'] <- (df.combined$MasVnrArea > 0) * 1

plot.categoric('MiscFeature', df.combined)

df.numeric['HasShed'] <- (df.combined$MiscFeature == 'Shed') * 1

df.numeric['Remodeled'] <- (df.combined$YearBuilt != df.combined$YearRemodAdd) * 1

df.numeric['RecentRemodel'] <- (df.combined$YearRemodAdd >= df.combined$YrSold) * 1

df.numeric['NewHouse'] <- (df.combined$YearBuilt == df.combined$YrSold) * 1

#cols.binary <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF')
cols.binary <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch')

for (col in cols.binary){
  df.numeric[str_c('Has',col)] <- (df.combined[,col] != 0) * 1
}

#We know how important the year a house was built and sold but what about what the specific month it was sold? How do houses sold during summer compare to the other seasons?

ggplot(df.combined, aes(x=MoSold)) +
  geom_bar(fill = 'cornflowerblue') +
  geom_text(aes(label=..count..), stat='count', vjust = -.5) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12)

df.numeric['HighSeason'] <- (df.combined$MoSold %in% c(5,6,7)) * 1

#What about which Neighborhoods are more expensive than others?

train[,c('Neighborhood','SalePrice')] %>%
  group_by(Neighborhood) %>%
  summarise(median.price = median(SalePrice, na.rm = TRUE)) %>%
  arrange(median.price) %>%
  mutate(nhbr.sorted = factor(Neighborhood, levels=Neighborhood)) %>%
  ggplot(aes(x=nhbr.sorted, y=median.price)) +
  geom_point() +
  geom_text(aes(label = median.price, angle = 45), vjust = 2) +
  theme_minimal() +
  labs(x='Neighborhood', y='Median price') +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45))

other.nbrh <- unique(df.combined$Neighborhood)[!unique(df.combined$Neighborhood) %in% c('StoneBr', 'NoRidge','NridgHt')]

ggplot(train, aes(x=SalePrice, y=GrLivArea, colour=Neighborhood)) +
  geom_point(shape=16, alpha=.8, size=4) +
  scale_color_manual(limits = c(other.nbrh, 'StoneBr', 'NoRidge', 'NridgHt'), values = c(rep('black', length(other.nbrh)), 'indianred', 'cornflowerblue', 'darkseagreen')) +
  theme_minimal() +
  scale_x_continuous(label=dollar)


#In the mean time lets one-hot encode the more expensive neighborhoods and add that to our dataframe

nbrh.rich <- c('Crawfor', 'Somerst, Timber', 'StoneBr', 'NoRidge', 'NridgeHt')
df.numeric['NbrhRich'] <- (df.combined$Neighborhood %in% nbrh.rich) *1

group.prices('Neighborhood')

nbrh.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 
              'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
              'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
              'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 4, 
              'NridgHt' = 4)

df.numeric['NeighborhoodBin'] <- as.numeric(nbrh.map[df.combined$Neighborhood])

group.prices('SaleCondition')

df.numeric['PartialPlan'] <- (df.combined$SaleCondition == 'Partial') * 1

group.prices('HeatingQC')

heating.list <- c('Po' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)

df.numeric['HeatingScale'] <- as.numeric(heating.list[df.combined$HeatingQC])
