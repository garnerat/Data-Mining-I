# Lab 4 Classification and Regression Trees


##### Load data #####

boston.data = read.csv("http://homepages.uc.edu/~maifg/7040/boston.csv")
boston.train = read.csv("http://homepages.uc.edu/~maifg/7040/boston.train.csv")
boston.test = read.csv("http://homepages.uc.edu/~maifg/7040/boston.test.csv")

credit.data = read.csv("http://homepages.uc.edu/~maifg/7040/credit0.csv")
credit.train = read.csv("http://homepages.uc.edu/~maifg/7040/credit.train.csv")
credit.test = read.csv("http://homepages.uc.edu/~maifg/7040/credit.test.csv")


#  load rpart

install.packages("rpart")
library(rpart)


##### Regression Tree Model #####

boston.rpart <- rpart(formula = medv ~ ., data = boston.train)

# raw output
boston.rpart

# plot tree
plot(boston.rpart)
text(boston.rpart, pretty = 0 ) 

# in sample prediction

train.rt.predict <- predict(boston.rpart)

# out of sample prediction

test.rt.predict <- predict(boston.rpart,boston.test)

#mean squared error out of sample prediction

mean((test.rt.predict - boston.test$medv)^2)


# compare to linear regression with all variables just for a rough comparison

boston.reg = lm(medv ~ ., data = boston.train)
boston.test.pred.reg = predict(boston.reg, boston.test)
mean((boston.test.pred.reg - boston.test$medv)^2)


##### Classification Tree (with known cost function) #####

credit.rpart <- rpart(formula = Y ~ . - id, data = credit.train, method = "class", parms = list(loss = matrix( c(0,10,1,0), nrow = 2)))

# fancier tree plot with rpart.plot package

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(credit.rpart, tweak =1.5, extra = 101)

# out of sample prediction

credit.test.predict<- predict(credit.rpart, credit.test, type = "class")

# confusion matrix
table(credit.test$Y, credit.test.predict, dnn = c("Truth", "Predicted"))

# expected loss cost

cost <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi == 0)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi == 1)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
cost(credit.test$Y, credit.test.predict)

# Probability of getting 1, credit.test.predict.prob has 2 columns first is probability 0 second is probability 1

credit.test.predict.prob <- predict(credit.rpart, credit.test)

# ROC

install.packages("ROCR")
library(ROCR)
pred = prediction(credit.test.predict.prob[, 2], credit.test$Y)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)

# AUC

slot(performance(pred, "auc"), "y.values")[[1]]


# cumulative gains chart

plot(performance(pred, "tpr", "rpp"))

##### Pruning Regression Tree #####


boston.largetree <- rpart(formula = medv ~ ., data = boston.train, cp = 0.001)

rpart.plot(boston.largetree, tweak = 1.5)

#The plotcp() function gives the relationship between 10-fold cross-validation error in the training set and size of tree.
plotcp(boston.largetree) #.0089 cp crosses line, size of 9

# root node error, average medv
sum((boston.train$medv - mean(boston.train$medv))^2)/455

# prune tree back to a cp level

prune(boston.largetree, cp = 0.05)

