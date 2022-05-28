#1. Boston Housing data 
#Random sample a training data set that contains 90% of the original data points. 
library(MASS) #this data is in MASS package 
boston_data <- data(Boston)
sample_index <- sample(nrow(Boston),nrow(Boston)*0.90) 
boston_train <- Boston[sample_index,]
boston_test <- Boston[-sample_index,]
#(i)Fit a regression tree (CART) on the training data. Report the model’s in-sample MSE performance.
install.packages('rpart')
install.packages('rpart.plot')
library(rpart) 
library(rpart.plot)
boston_rpart <- rpart(formula = medv ~ ., data = boston_train)
boston_rpart
summary(boston_rpart)
prp(boston_rpart,digits = 4, extra = 1)
boston_train_pred_tree = predict(boston_rpart)
mean((boston_train_pred_tree - boston_train$medv)^2) #in-sample MSE (without cp value specified)
#(ii)Test the out-of-sample performance. Using tree model built from (i) on the training data, test with the remaining 10% testing data. Report out-of-sample model MSE.
boston_test_pred_tree = predict(boston_rpart,boston_test)
mean((boston_test_pred_tree - boston_test$medv)^2) #out-of-sample MSE (without cp value specified)

#pruning the tree
boston_largetree <- rpart(formula = medv ~ ., data = boston_train, cp = 0.001)
summary(boston_largetree)
boston_largetree_test = predict(boston_largetree, boston_test)
prp(boston_largetree, digits = 4, extra = 1)
plotcp(boston_largetree)
printcp(boston_largetree)
sum((boston_train$medv - mean(boston_train$medv))^2)/nrow(boston_train) #MSE/Root node error 
mean((predict(boston_largetree) - boston_train$medv)^2) #in-sample MSE (with cp value specified)
mean((boston_largetree_test - boston_test$medv)^2) #out-of-sample MSE (with cp value specified)

boston_opttree <- rpart(formula = medv ~ ., data = boston_train, cp = 0.0085) #based on boston_largetree, select the cp value that falls under horizontal line to create optimal tree
summary(boston_opttree)
boston_opttree_test = predict(boston_opttree, boston_test)
prp(boston_opttree, digits = 4, extra = 1)
prune(boston_opttree, cp = 0.0085)
mean((predict(boston_opttree) - boston_train$medv)^2) #in-sample MSE after pruning tree
mean((boston_opttree_test - boston_test$medv)^2) #out-of-sample MSE after pruning tree 

#(iii)Conduct linear regression using all explanatory variables except “indus” and “age” on the training data. Report the model’s in-sample MSE. Test the out-of-sample performance with the remaining 10% testing data. Report out-of-sample model MSE etc?
linear_model = lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat, data = boston_train)
linear_model
summary(linear_model)
#in-sample MSE
model_summary <- summary(linear_model)
(model_summary$sigma)^2 #MSE 
model_summary$r.squared #MSE, sum of squared errors/df of error: small is good
model_summary$adj.r.squared #big is good 

#Out-of-sample MSE 
pi <- predict(linear_model, boston_test)
mean((pi - boston_test$medv)^2) #MSE

#(iv)What do you find comparing CART to the linear regression model fits from (iii)?

#2. German Credit Score data 
#Random sample a training data set that contains 80% of original data points.
library(dplyr)
german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", "present_resid", "property", "age", "other_install", "housing", "n_credits", "job", "n_people", "telephone", "foreign", "response")
colnames(german_credit)
german_credit$response = german_credit$response - 1
german_credit$response
#convert categorical variables to factor
german_credit$chk_acct<- as.factor(german_credit$chk_acct)
german_credit$credit_his<- as.factor(german_credit$credit_his)
german_credit$purpose<- as.factor(german_credit$purpose)
german_credit$saving_acct<- as.factor(german_credit$saving_acct)
german_credit$present_emp<- as.factor(german_credit$present_emp)
german_credit$sex<- as.factor(german_credit$sex)
german_credit$other_debtor<- as.factor(german_credit$other_debtor)
german_credit$property<- as.factor(german_credit$property)
german_credit$other_install<- as.factor(german_credit$other_install)
german_credit$housing<- as.factor(german_credit$housing)
german_credit$job<- as.factor(german_credit$job)
german_credit$telephone<- as.factor(german_credit$telephone)
german_credit$foreign<- as.factor(german_credit$foreign)
#split the data
index <- sample(nrow(german_credit),nrow(german_credit)*0.80)
german_train = german_credit[index,]
german_test = german_credit[-index,]
#(i)Fit a classification tree (CART) on the training data; Report the model’s in-sample performance, for example, misclassification rate etc. 
german_rpart <- rpart(formula = response ~ . , data = german_train, method =
                        "class", parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))
german_rpart
prp(german_rpart, extra = 1)

#without pruning the tree 
#in-sample prediction
pcut = 0.5 # default
cost1<-function(r, pi) mean( ((r==0)&(pi>pcut)) | ((r==1)&(pi<pcut)) )
cost1(german_train$response, predict(german_rpart, german_train, type="prob")) #define symmetric cost function

cost <- function(r, phat){
  weight1 <- 5
  weight0 <- 1
  pcut <- weight0/(weight1+weight0)
  c1 <- (r==1)&(phat<pcut) #logical vector - true if actual 1 but predict 0 
  c0 <-(r==0)&(phat>pcut) #logical vector - true if actual 0 but predict 1 
  return(mean(weight1*c1+weight0*c0))}
cost(german_train$response, predict(german_rpart, german_train, type="prob")) #define asymmetric cost function

german_train.pred.tree <- predict(german_rpart, german_train, type="class")
table(german_train$response, german_train.pred.tree, dnn=c("Truth","Predicted"))

#(ii)Test the out-of-sample performance. Using tree model built from (i) on the training data, test with the remaining 20% testing data. Report the out-of-sample AUC, misclassification cost and misclassification cost with symmetric cost and asymmetric cost function. 
cost1(german_test$response, predict(german_rpart, german_test, type="prob")) #symmetric cost
cost(german_test$response, predict(german_rpart, german_test, type="prob")) #asymmetric cost 
german_rpart <- rpart(formula = response ~ .,
                      data = german_train,
                      method = "class",
                      parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))
#Probability of getting 1
german_test_prob_rpart = predict(german_rpart, german_test, type="prob")
install.packages('ROCR')
library(ROCR)
#ROC curve
pred = prediction(german_test_prob_rpart[,2], german_test$response)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#AUC
slot(performance(pred, "auc"), "y.values")[[1]]

german_test_pred_rpart = as.numeric(german_test_prob_rpart[,2] > 1/(5+1))
table(german_test$response, german_test_pred_rpart, dnn=c("Truth","Predicted"))


