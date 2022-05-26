#Boston Housing data
#Random sample a training data set that contains 90% of the original data points
pacman::p_load(pacman, rio, tidyverse)
library(MASS)
data(Boston)
colnames(Boston)
subset <- sample(nrow(Boston), nrow(Boston) * 0.9)
boston_train = Boston[subset, ]
boston_test = Boston[-subset, ]
df = boston_train

#(i) Start with exploratory data analysis. Are there outliers? (use boxplot) 
boxplot(boston_train$crim) #boxplot of one variable 
boxplot(boston_train$zn)
boxplot(boston_train$indus)
boxplot(boston_train$chas)
boxplot(boston_train$nox)
boxplot(boston_train$rm)
boxplot(boston_train$age)
boxplot(boston_train$dis)
boxplot(boston_train$rad)
boxplot(boston_train$tax)
boxplot(boston_train$ptratio)
boxplot(boston_train$black)
boxplot(boston_train$lstat)
boxplot(boston_train$medv)

boxplot(medv ~ indus,
        data=boston_train,
        main="Different boxplots",
        xlab="indus",
        ylab="medv",
        col="pink",
        border="green") #boxplot of two variables 

#(ii) Conduct linear regression on the training data. 
df %>%
  dplyr::select(indus, medv) %>%
  plot()

lm(df$medv ~ df$indus) %>% abline()

model1 = lm(medv ~ ., data = boston_train)
summary(model1)

#(iii) Conduct variable selection. Find the best linear model. Show residual diagnosis
#Forward/Backward/Stepwise Regression Using AIC
nullmodel = lm(medv ~ 1, data = boston_train)
fullmodel = lm(medv ~ ., data = boston_train)

#Backward Elimination
model.step <- step(fullmodel, direction = "backward")

#Forward Selection
model.step <- step(nullmodel, scope = list(lower = nullmodel,
                                           upper = fullmodel),
                   direction = "forward")

#compare models 
model_1 <- lm(medv ~ ., data = boston_train)
model_2 <- lm(medv ~ lstat + rm + ptratio + dis + nox + chas + black + rad + 
                crim + zn + tax, data = boston_train) 

summary(model_1)
summary(model_2)
AIC(model_1)
AIC(model_2)

#residual diagnosis
plot(model_1)
plot(model_2)

#(iv) Test the out-of-sample performance. Using final linear model built from (iii) on the 90% of original data, test with the remaining 10% testing data. Report out-of-sample model MSE etc. 
#we start off with in-sample evaluation first
#model 1 
model_summary <- summary(model_1)
(model_summary$sigma)^2 #MSE 
model_summary$r.squared #MSE, sum of squared errors/df of error: small is good
model_summary$adj.r.squared #big is good 
AIC(model_1) #small is good 
BIC(model_1)

#model 2
model_summary <- summary(model_2)
(model_summary$sigma)^2
model_summary$r.squared
model_summary$adj.r.squared
AIC(model_2)
BIC(model_2)

#Out-of-sample prediction (test error)
#model 1 
pi <- predict(model_1, boston_test)
mean((pi - boston_test$medv)^2) #MSE
mean(abs(pi - boston_test$medv)) #MAE 

#model 2 
pi <- predict(model_2, boston_test)
mean((pi - boston_test$medv)^2) #MSE
mean(abs(pi - boston_test$medv)) #MAE 

#(v) Cross validation on the original data. Use 10-fold cross validation. Does (v) yield similar answer as (iv)?
#10-fold Cross Validation
library(boot)
model_2 = glm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
                black + lstat, data = Boston) 
cv.glm(data = Boston, glmfit = model_2, K = 10)$delta[2] #variables selected from backward selection output

#LOOCV (Leave-one-out Cross Validation)
cv.glm(data = Boston, glmfit = model_2, K = nrow(Boston)) $delta[2]

#10-fold Cross Validation Using MAE
model_2 = glm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
                black + lstat, data = Boston)
MAE_cost = function(pi, r) {return(mean(abs(pi - r)))}
cv.glm(data = Boston, glmfit = model_2, cost = MAE_cost, K = 10)$delta[2]

#(vi)	Now repeat previous steps for another random sample (that is, to draw another training data set with 90% of original data, and the rest 10% as testing; or you can try 80% training vs. 20% left as testing). Do you get similar results? What’s your conclusion?
