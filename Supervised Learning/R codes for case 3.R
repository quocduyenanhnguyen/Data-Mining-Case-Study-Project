library(dplyr)
german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", "present_resid", "property", "age", "other_install", "housing", "n_credits", "job", "n_people", "telephone", "foreign", "response")
colnames(german_credit)
german_credit$response = german_credit$response - 1
german_credit$response

#(i) Random sample a training data set that contains 80% of original data points. Start with exploratory data analysis on the training data. Fit a logistic regression model and evaluate the model fitting.
str(german_credit) #structure
summary(german_credit) #summary statistics
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
#model fitting
credit_glm0<- glm(response~., family=binomial, data=german_train)
summary(credit_glm0)

#(ii)	Find a best model using logistic regression with AIC and BIC. Draw ROC curve, report the AUC, and present the misclassification rate table of your final model.
#backward, AIC criteria
credit_glm_back <- step(credit_glm0) # backward selection (if you don't specify anything)
summary(credit_glm_back)
credit_glm_back$deviance
AIC(credit_glm_back)
BIC(credit_glm_back)
credit_glm1 = glm(response~ chk_acct + duration + credit_his + purpose + amount + 
                    saving_acct + present_emp + installment_rate + other_debtor + 
                    other_install + housing + telephone + foreign, family=binomial, data=german_train) #the best model
#in-sample prediction with the best model
pred_glm1_train <- predict(credit_glm1, type="response")
#ROC curve
install.packages('ROCR')
library(ROCR)
pred <- prediction(pred_glm1_train, german_train$response) 
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#AUC
unlist(slot(performance(pred, "auc"), "y.values"))
#specify objective function
pcut <- 1/(5+1)
cost2 <- function(r, pi, pcut){
  weight1 <- 5
  weight0 <- 1
  c1 <- (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0 
  c0 <-(r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1 
  return(mean(weight1*c1+weight0*c0))} #asymmetric cost 
  cost2

#cutoff value
pred_resp <- predict(credit_glm1,type="response")
table(german_train$response, (pred_glm1_train > pcut)*1, dnn=c("Truth","Predicted"))
#(iii) Test the out-of-sample performance. Using final logistic linear model built from (ii) on the 80% of original data, test with the remaining 20% testing data.  (Try predict() function in R.) Report out-of-sample AUC and misclassification rate. 
#out-of-sample prediction with the best model
pred_glm1_test<- predict(credit_glm1, newdata = german_test, type="response")
#ROC curve
pred <- prediction(pred_glm1_test, german_test$response)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#AUC
unlist(slot(performance(pred, "auc"), "y.values"))
#specify objective function
pcut <- 1/(5+1)
cost2 <- function(r, pi, pcut){
  weight1 <- 5
  weight0 <- 1
  c1 <- (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0 
  c0 <-(r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1 
  return(mean(weight1*c1+weight0*c0))} #asymmetric cost 

#cutoff value 
pred_resp <- predict(credit_glm1, data=german_test, type="response")
table(german_test$response, (pred_glm1_test > pcut)*1, dnn=c("Truth","Predicted"))
#(iv) Cross validation. Use 5-fold cross validation. (Try cv.glm() function in R on the ORIGINAL data.) Does (v) yield similar answer as (iii)? Make sure that you specify the right cost functions.
costfunc <- function(obs, pred.p){
  weight1 <- 5 # define the weight for "true=1 but pred=0" (FN)
  weight0 <- 1 # define the weight for "true=0 but pred=1" (FP)
  pcut <- 1/(1+weight1/weight0)
  c1 <- (obs==1)&(pred.p < pcut) # count for "true=1 but pred=0" (FN) 
  c0 <- (obs==0)&(pred.p >= pcut) # count for "true=0 but pred=1" (FP) 
  cost <- mean(weight1*c1 + weight0*c0) # misclassification with weight 
  return(cost) # you have to return to a value when you write R functions
} # end
library(boot)
credit_glm3<- glm(response~ chk_acct + duration + credit_his + purpose + amount + 
                    saving_acct + present_emp + installment_rate + other_debtor + 
                    other_install + housing + telephone + foreign, family=binomial, data=german_credit)
cv_result <- cv.glm(data=german_credit, glmfit=credit_glm3, cost=costfunc, K= 5)
cv_result$delta[2]
