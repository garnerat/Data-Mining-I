#Lab 2 Simple Linear Regression

library(MASS)
data(Boston)
names(Boston)

#### training/test subset ####
subset<-sample(nrow(Boston),nrow(Boston)*.9)
train<- Boston[subset,]
test<- Boston[-subset,]

#### Standardization (optional) ####

# (x - xbar) / sd

# Standardization not as important in Data Mining Application 
# as it will not affect estimation and inference
# In "modeling for insights" 


##### Build Linear Model ####

# model
model.linear <- lm( medv~.,data = train)

#summarize model
model.linear.summary <- summary(model.linear)


model.linear.summary$coefficients

model.linear.summary$residuals

model.linear.summary$sigma ^2 #MSE of model
model.linear.summary$adj.r.squared # adjuste R-squared
extractAIC(model.linear)# number of covariates + intercept and AIC
AIC(model.linear) # gives different AIC
BIC(model.linear) # AIC/BIC smaller the better for information criterion

#Out of sample prediction***
#predict() gives the predicted medv values for test set

pi = predict(object = model.linear, newdata = test) # if you forget to give new test data it will predict on training data

#model evaluation

#average of squared differences between the precicted and actual response
mean((pi-test$medv)^2)

mean(abs((pi-test$medv))) # average absolute value of differences


##### Variable Selection #####

#Best subset method - every possible combination, very expensive to run, won't use much in practice really
install.packages("leaps")
library(leaps)

subset_result = regsubsets(medv ~ ., data = train, nbest = 2, nvmax = 14)
#nbest specifies the number of best subsets of indepentent variable for each model 
#size (1 variable, to variable etc. up to nvmax) to keep in this case the top 2

summary(subset_result)

plot(subset_result, scale = "bic")
# filled in black rectangles are variables included in the model
#BIC: smaller is better, this plot starts with largest value (worst) at the bottom


