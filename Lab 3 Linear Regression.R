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



