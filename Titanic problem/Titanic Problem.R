.ta# date : 12/19/2017 - Balram Sidh
# Setting working directory and importing data files
# https://github.com/trevorstephens/titanic

setwd("~/Desktop/Kaggle/Titanic problem")
library(readr)
train <- read.csv("train.csv", stringsAsFactors = TRUE)
test <- read.csv("test.csv", stringsAsFactors = TRUE)

# To check the structure of columns
str(train)

# The table command is one of the most basic summary statistics functions in R, it runs through the vector you gave it and simply counts the occurrence of each value in it.
table(train$Survived)

# To get the proportion 
prop.table(table(train$Survived))

#Some more R syntax to keep moving. The assignment operator is <- and is used to store the right hand side value to the left hand side. For instance, x <- 3 will store the value of 3 to the variable x. In some limited cases, such as passing argument values to a function signature, the equals sign is used

#Okay, so let???s add our ???everyone dies??? prediction to the test set dataframe. To do this we???ll need to use a new command, rep that simply repeats something by the number of times we tell it to:

# Since there was no ???Survived??? column in the dataframe, it will create one for us and repeat our ???0??? prediction 418 times, the number of rows we have. If this column already existed, it would overwrite it with the new values, so be careful!
test$Survived <- rep(0,418)

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
View(submit)

# lesson 2 

# to check no of femals and males 
summary(train$Sex)
# To check ratio of males vs females that survived 

prop.table(table(train$Sex, train$Survived))

# the above didnt give us ratio by row, so doing below 


prop.table(table(train$Sex, train$Survived),1)

# now we are updating survived status of all females to 1 
#We just used two new pieces of R syntax, the equality operator, ==, and the square bracket operator. The square brackets create a subset of the total dataframe, and apply our assignment of ???1??? to only those rows that meet the criteria specified. The double equals sign no longer works as an assignment here, now it is a boolean test to see if they are already equal.

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
View(test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "result2.csv", row.names = FALSE)

# to check the age 
summary(train$Age)



# creating a column to indicate if person is below 18. We are assuming all with missing age field to be median age, in this case late twenties

train$Child <- 0
train$Child[train$Age < 18] <- 1

# trying to check if there is a survived is dependent on sex and child 
aggregate( Survived ~ Child + Sex, data=train, FUN = sum)

# to get the total count in each category
aggregate(Survived ~ Child + Sex, data = train, FUN = length)

# to get the proportion of each category
aggregate(Survived ~ Child + Sex, data=train, FUN = function(x) {sum(x)/length(x)})

# the result from above is very similar to our finding that most females survived 

# Now discretizing fare

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# checking

aggregate( Survived ~ Fare2 + Pclass + Sex, data=train, FUN = function(x){sum(x)/length(x)})

test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "result3.csv", row.names = FALSE)

