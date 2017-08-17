rm(list=ls())
setwd('C:/Users/vishwanath/Desktop/Skill set/EdWisor/Project 2')
#C:\Users\vishwanath\Desktop\Skill set\EdWisor\Project 2
getwd()

#load files
aisle = read.csv("aisles.csv", header = T)
dept = read.csv("departments.csv", header = T)
order_prod_prior = read.csv("order_products__prior.csv", header = T)
order_prod_train = read.csv("order_products__train.csv", header = T)  
orders= read.csv("orders.csv", header = T)
prod = read.csv("products.csv", header = T)

#merge aisle,dept and prod
AP = merge(aisle, prod, by = "aisle_id", all.x = TRUE)
APD = merge(AP, dept, by = "department_id", all.x = TRUE)
rm(aisle,dept,prod,AP)
#write.csv(APD, "APD.csv", col.names = TRUE)
sum(is.na(APD))

#split orders into train,test,prior
order_train = orders[orders$eval_set == "train",]
order_test = orders[which(orders$eval_set == "test"),]
order_prior = orders[which(orders$eval_set == "prior"),]
rm(orders)

#join order_prod_prior and order_prior
prior = merge(order_prior, order_prod_prior, by = "order_id", all.y = TRUE)
rm(order_prod_prior,order_prior)

#join order_prod_train and order_train
nex = merge(order_train, order_prod_train, by = "order_id", all.x = TRUE)
rm(order_prod_train,order_train)

#impute missing values with "None","0"
prior$days_since_prior_order[is.na(prior$days_since_prior_order)] = 0
prior$add_to_cart_order[is.na(prior$add_to_cart_order)] = 0
prior$reordered[is.na(prior$reordered)] = 0

#remove unnecessary columns
#prior$add_to_cart_order = NULL
prior$eval_set = NULL
train$add_to_cart_order = NULL
#train$eval_set = NULL

#join train and prior
#safe_prior = prior
#safe_train = train
TP = rbind(train,prior)
rm(prior,train)


#Naive Bayes
library(e1071)
library(mlbench)
library(caret)
TP$order_id = NULL
TP[] <- lapply( TP, factor)
TP$product_id = as.factor(TP$product_id)
TP$days_since_prior_order = as.numeric(TP$days_since_prior_order)
model = naiveBayes(product_id ~ . , data = TP)

#predictions
test <- fread("order_test.csv", stringsAsFactors = T)
test$V1 = NULL
test$eval_set = NULL
test$order_id = NULL
#system.time(
  pred1 = data.frame(predict(model, test))
  
write.csv(pred1,"pred1.csv")

#Random Forest
library(randomForest)
fit = randomForest(product_id ~ ., data = TP)


#
lm = lm(product_id ~ ., data = TP)

#
mo <- glm(product_id ~.,family=binomial(link='logit'),data=TP[])

#for huge data
library(data.table)
TP <- fread("prior.csv", stringsAsFactors = T)
TP$V1 = NULL
TP$order_id = NULL
#TP[]  = lapply( TP, factor)
TP$days_since_prior_order = as.numeric(TP$days_since_prior_order)
TP$product_id = as.factor(TP$product_id)
library('h2o')
localH2O <- h2o.init(nthreads = -1)
h2o.init()
TP.h2o <- as.h2o(TP)
colnames(TP.h2o)
#dependent variable (Product_id)
y.dep <- c(1)
#independent variables
x.indep <- c(2:5)
#deep learning
system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = TP.h2o,
                                      epoch = 1,
                                      hidden = c(100,100),
                                      activation = "Rectifier",
                                      seed = 1122)
)
#making predictions
test <- fread("order_test.csv", stringsAsFactors = T)
test$V1 = NULL
test$order_id = NULL
test$eval_set = NULL
#test$days_since_prior_order = as.factor(test$days_since_prior_order)
test.h2o = as.h2o(test)
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
write.csv(predict.dl2,"output.csv")

library("xgboost")  # the main algorithm
#library("archdata") # for the sample dataset
library("caret")    # for the confusionmatrix() function (also needs e1071 package)
library("e1071")
library("dplyr")    # for some data preperation


library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
TP = read.csv("TP.csv", header = TRUE)
TP$X = NULL
TP$order_id = NULL
TP$days_since_prior_order = as.numeric(TP$days_since_prior_order)
setDT(TP)
labels = TP[,"product_id"]
#data_matrix <- xgb.DMatrix(data = as.matrix(TP), label = labels)
#dtrain <- xgb.DMatrix(data = TP,label = labels)
xgb <- xgboost(data = data.matrix(TP), 
               label = labels,
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)
library(Matrix)
a = data.matrix(TP[2:6])
labels = TP[1]

bst <- xgboost(data = TP[2:6], label = labels, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "multi:softprob")



data_variables <- as.matrix(TP[,-1])
data_label <- TP$product_id
d = as.matrix(TP)
data_matrix <- xgb.DMatrix(data = d, label = data_label)
numberOfClasses <- length(unique(train$product_id))
xgb_params <- list("objective" = "multi:softprob",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = data_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)
bst_model <- xgb.train(params = xgb_params,
                       data = data_matrix,
                       nrounds = 20)
# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$label),
                factor(test_prediction$max_prob),
                mode = "everything")



#train
library(data.table)
TP <- fread("TP.csv", stringsAsFactors = T)
TP$V1 = NULL
APD <- fread("APD.csv", stringsAsFactors = T)
APD$V1 = NULL
TP <- merge(TP, APD, by = "product_id", all.x = TRUE)
rm(APD,TP)
safe=TP
#rezero = TP[TP$reordered == 0,]
TP = TP[TP$reordered == 1,]
TP$reordered = NULL
TP$order_id = NULL
TP$user_id = NULL

#test
test <- fread("order_test.csv", stringsAsFactors = T)
test$V1 = NULL
test$eval_set = NULL
test$order_id = NULL
test$user_id = NULL


library(dplyr)
GroupTP = group_by(TP,TP$product_id)


km=kmeans(TP,centers = 47301)


res1 = order_test$user_id
see.prior = order_prior[is.element(order_prior$user_id, res1),]
#see.train = order_train[is.element(order_train$user_id, res1),]
rm(see)
TP = TP[TP$reordered == 1,]
