# Balram Sidh 12/24/2017 
# tutorial 4 : Feature Engineering 

train$Name[1]

# combining test and train data

test$Survived <- NA
test$Survived

combi <- rbind(train,test)

class(combi$Name)

# since the name is factor, we are changing it to character 

combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], split ='[,.]' )

# To just extract title 
strsplit(combi$Name[1], split ='[,.]' )[[1]][2]

# to implement this on all the data

combi$Title <- sapply(combi$Name, FUN= function(x) {strsplit(x, split = '[,.]')[[1]][2]})

combi$Title <- sub(' ','', combi$Title)
                                      
table(combi$Title)

# Now we are going to combine a few titles 

combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Calculating total family members aboard 

combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Extracting surnames 
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

table(combi$Surname)

# creating a family with family size and surname 

combi$FamilyID <- paste( as.character(combi$FamilySize), combi$Surname, sep = "")

table(combi$FamilyID)

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "engineeredfeaturestree.csv", row.names = FALSE)

# Random forest trees Part 5 

# trying to fill the missing data

summary(combi$Age)# there are 263 NA's in age which we can replace by predicting age

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
summary(Agefit)
fancyRpartPlot(Agefit)

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Checking for other missing values
summary(combi)

summary(combi$Embarked)

which(combi$Embarked == '')

combi$Embarked[c(62,830)] = "S"

class(combi$Embarked)
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)

which(is.na(combi$Fare) == "TRUE")

combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)

# We dont have more missing data. Now since random forest can take on upto 32 factors for a variable, we need to decrease the variables for Family_id

table(combi$FamilyID)

combi$FamilyID2 <- as.character(combi$FamilyID)

table(combi$FamilyID2)


summary(as.factor(combi$FamilyID[combi$FamilySize == 3]))

combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

install.packages('randomForest')
library(randomForest)
set.seed(400)

summary(train)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

# it was giving below error 
#Error in randomForest.default(m, y, ...) : 
#NA/NaN/Inf in foreign function call (arg 1)
#In addition: Warning message:
#In data.matrix(x) : NAs introduced by coercion

# So one of the input variables to the randomforest was char instead of numeric or factor. 

# To find out class of every column in R
sapply(train, class)

# converting Title to factor 
combi$Title <- as.factor(combi$Title)
# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# running the algo again
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
summary(fit)

# to check which variables are most important 
varImpPlot(fit
           )

# the result from above is not really good, so trying conditional trees

install.packages('party')

library(party)

set.seed(400)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerID = test$PassengerId, Survived=Prediction)

write.csv(submit, file = "Cforest.csv", row.names = FALSE)

# Now i am trying to find out actual fare of passengers as one ticket is shared by multiple people 

tikfreq <- data.frame(table(combi$Ticket))
tikfreq
combi$pertik <- tikfreq[match(combi$Ticket,tikfreq$Var1),2
                        ]
table(combi$pertik)

combi$actualfare <- round((combi$Fare/combi$pertik),2)

summary(combi$pertik)

train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(420
        )
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + actualfare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerID = test$PassengerId, Survived=Prediction)

write.csv(submit, file = "Cforest_update1.csv", row.names = FALSE)
