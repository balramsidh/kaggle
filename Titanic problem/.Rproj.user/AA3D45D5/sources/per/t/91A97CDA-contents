# Balram Sidh 12/21/2017 
# Titanic Problem Part 2 ( decision trees)
# http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/

# importing Recursive Partitioning and Regression Trees ( default R comes with it, just need to source it everytime)

library(rpart)

# cart decision tree

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

summary(fit)
plot(fit)
text(fit)

# installing packages to make it lok fancy

install.packages('rattle')
install.packages('rpart.plot')
library(rattle)
library(rpart.plot)
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#You can find the default limits by typing ?rpart.control. The first one we want to unleash is the cp parameter, this is the metric that stops splits that aren’t deemed important enough. The other one we want to open up is minsplit which governs how many passengers must sit in a bucket before even looking for a split. Let’s max both out and reduce cp to zero and minsplit to 2 (no split would obviously be possible for a single passenger in a bucket):
# to more fine grain the results 
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))

fancyRpartPlot(fit)
