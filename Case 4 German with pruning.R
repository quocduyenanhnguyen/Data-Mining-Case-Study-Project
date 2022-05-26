#2. German Credit Score data 
#Random sample a training data set that contains 80% of original data points.
library(dplyr)
library(rpart) 
library(rpart.plot)
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
#with pruning the tree 
german_rpart <- rpart(formula = response ~ . , data = german_train, method =
                        "class", parms = list(loss=matrix(c(0,5,1,0), nrow = 2)), cp = 0.001)
plotcp(german_rpart)
prp(german_rpart, digits = 4, extra = 1)

opttree = rpart(formula = response ~ . , data = german_train, method =
                  "class", parms = list(loss=matrix(c(0,5,1,0), nrow = 2)), cp = 0.0071) #optimal tree with new cp value

#in-sample prediction
pcut = 0.5 # default
cost1<-function(r, pi) mean( ((r==0)&(pi>pcut)) | ((r==1)&(pi<pcut)) )
cost1(german_train$response, predict(opttree, german_train, type="prob")) #define symmetric cost function

cost <- function(r, phat){
  weight1 <- 5
  weight0 <- 1
  pcut <- weight0/(weight1+weight0)
  c1 <- (r==1)&(phat<pcut) #logical vector - true if actual 1 but predict 0 
  c0 <-(r==0)&(phat>pcut) #logical vector - true if actual 0 but predict 1 
  return(mean(weight1*c1+weight0*c0))}
cost(german_train$response, predict(opttree, german_train, type="prob")) #define asymmetric cost function

german_train.pred.tree <- predict(opttree, german_train, type="class")
table(german_train$response, german_train.pred.tree, dnn=c("Truth","Predicted"))

#(ii)Test the out-of-sample performance. Using tree model built from (i) on the training data, test with the remaining 20% testing data. Report the out-of-sample AUC, misclassification cost and misclassification cost with symmetric cost and asymmetric cost function. 
cost1(german_test$response, predict(opttree, german_test, type="prob")) #symmetric cost
cost(german_test$response, predict(opttree, german_test, type="prob")) #asymmetric cost 

#Probability of getting 1
german_test_prob_rpart = predict(opttree, german_test, type="prob")
library(ROCR)
#ROC curve
pred = prediction(german_test_prob_rpart[,2], german_test$response)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#AUC
slot(performance(pred, "auc"), "y.values")[[1]]

german_test_pred_rpart = as.numeric(german_test_prob_rpart[,2] > 1/(5+1))
table(german_test$response, german_test_pred_rpart, dnn=c("Truth","Predicted"))

