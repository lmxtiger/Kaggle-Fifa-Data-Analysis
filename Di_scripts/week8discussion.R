### MATH 189 Week 8 Discussion
require(aplore3)
data(icu)
str(icu)

icu.fit=glm(sta~gender+age+race+loc, family=binomial(), data=icu)

# Recall from last time, the final model is:
fit = glm(sta ~ age + loc, family=binomial(), data=icu)

## prediction
summary(predict(icu.fit))
summary(predict(icu.fit, type = "response"))

summary(predict(fit))
summary(predict(fit, type = "response"))

## ROC curve and AUC
#install.packages("pROC")
library("pROC")
plot(roc(icu$sta, predict(icu.fit, type = "response")))
# or
plot.roc(icu$sta, predict(icu.fit, type = "response"))
auc(icu$sta, predict(icu.fit, type = "response"))

# for the model we selected
plot(roc(icu$sta, predict(fit, type = "response")))
## or
plot.roc(icu$sta, predict(fit, type = "response"))
auc(icu$sta, predict(fit, type = "response"))


## Prediction on new data
# prediction with the model we selected
n = dim(icu)[1]
set.seed(1234)
ind.train = sample(n, round(n*0.9),replace = FALSE)
icu.train = icu[ind.train, ]
icu.test = icu[-ind.train, ]
fit = glm(sta ~ age + loc, family=binomial(), data=icu.train)
summary(predict(fit, newdata = icu.test, type = "response"))
plot.roc(icu.test$sta, predict(fit,newdata = icu.test, type = "response"))
auc(icu.test$sta, predict(fit, newdata = icu.test, type = "response"))

# prediction with our full model
icu.fit = glm(sta ~ gender+age+race+loc, family=binomial(), data=icu.train)
summary(predict(icu.fit, newdata = icu.test, type = "response"))
plot.roc(icu.test$sta, predict(icu.fit,newdata = icu.test, type = "response"))
auc(icu.test$sta, predict(icu.fit, newdata = icu.test, type = "response"))

 
## 10-fold cross validation
#install.packages("cvTools")
require(cvTools)
k = 10
set.seed(123)
folds <- cvFolds(nrow(icu), K=k)
auc = numeric(length = k)
for( i in 1:k){
  icu.train = icu[folds$subsets[folds$which != i], ] #set the training set
  icu.test = icu[folds$subsets[folds$which == i], ] # set the test set
  fit = glm(sta ~ age + loc, family=binomial(), data=icu.train)
  #summary(predict(fit, newdata = icu.test, type = "response"))
  #plot.roc(icu.test$sta, predict(fit,newdata = icu.test, type = "response"))
  auc[i] = auc(icu.test$sta, predict(fit, newdata = icu.test, type = "response"))
}
auc
# the average of auc
sum(auc)/k


## Leave-one-out cross validation
n = nrow(icu)
count = 0
for(i in 1:n){
  icu.train = icu[-i, ] #set the training set
  icu.test = icu[i, ] # set the test set
  fit = glm(sta ~ age + loc, family=binomial(), data=icu.train)
  count = count + (as.numeric(predict(fit, newdata = icu.test, type = "response")>0.5)
                   != (icu.test$sta == "Died"))
}
err = count/n # err with 0-1 loss



