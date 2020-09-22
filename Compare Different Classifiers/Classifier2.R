---
title: "Homework6"
output: html_document
---

```{r setup, include=FALSE}
rm(list=ls());
gc();
library(class)
library(gmodels)
library(caret) 
library(e1071)
library(naivebayes)
library(data.table)
library(tree)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(plm)
require(neuralnet)
set.seed(123456)
setwd('/Users/apple/Desktop/201909-201912/BUS-256A/hw6/')
```
# 1.Cancer Data
```{r}
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)
```
# 1.1Classifying Cancer Samples using KNN
```{r}
set.seed(123)
# dropping useless variables: id
wbcd = wbcd[-1]
table(wbcd$diagnosis)


# Renaming diagnosis as a factor with proper labels
wbcd$diagnosis = factor(x      = wbcd$diagnosis,
                        levels = c("B",      "M"),
                        labels = c("Benign", "Malignant"))

# Initial exploratory analysis
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)


# Prep the data:(i) normalizing and (ii)test vs non-test data sep - --------------

# Normalizing function and normalizing the wbcd data                                                        
normalize = function(x){return ((x - min(x)) / (max(x) - min(x)))}
wbcd_n    = as.data.frame(lapply(wbcd[,2:31], normalize))

# create training and test data
wbcd_test_n  = wbcd_n[470:569, ]
wbcd_train_n = wbcd_n[1:400  , ]
wbcd_valid_n = wbcd_n[401:569, ]
wbcd_test    = wbcd[  470:569, ]
wbcd_train   = wbcd[  1:400  , ]
wbcd_valid   = wbcd[401:569  , ]


# create labels for training and test data
wbcd_train_labels = wbcd_train[, 1]
wbcd_valid_labels = wbcd_valid[, 1]
wbcd_test_labels  = wbcd_test[ , 1]

# Training model on dta_training                                  - --------------
wbcd_valid_pred = class::knn(train = wbcd_train_n, 
                             cl    = wbcd_train_labels,
                             test  = wbcd_valid_n,
                             k     = 1)

# Evaluating performance on dta_test                              - --------------
k1_conf_mat  =   gmodels::CrossTable(x          = wbcd_valid_labels, 
                                     y          = wbcd_valid_pred,
                                     prop.chisq = TRUE)

# re-classify test cases
wbcd_valid_pred = class::knn(train = wbcd_train_n, 
                             cl    = wbcd_train_labels,
                             test  = wbcd_valid_n,
                             k     = 5)

k5_conf_mat     = gmodels::CrossTable(x          = wbcd_valid_labels, 
                                      y          = wbcd_valid_pred,
                                      prop.chisq = TRUE)
k5_conf_mat$t
k5_conf_mat$prop.row
k5_conf_mat$prop.col

#confusion matrices #0.9822
(106+60)/(106+60+3) 
```

# 1.2 Using Naive Bayes
```{r}
set.seed(123)
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)
# dropping useless variables: id
wbcd = wbcd[-1]

# Renaming diagnosis as a factor with proper labels
wbcd$diagnosis = factor(x      = wbcd$diagnosis,
                        levels = c("B",      "M"),
                        labels = c("Benign", "Malignant"))

# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(wbcd$diagnosis, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(wbcd))[! (1:nrow(wbcd) %in% inx_train)]

wbcd_train    = wbcd[ inx_train, ]
wbcd_valid    = wbcd[ inx_valid, ]
wbcd_test     = wbcd[-inx_train, ]

# Training a model on the data                                      # #######
NBclassfied  = e1071::naiveBayes(diagnosis~radius_mean+texture_mean+perimeter_mean+area_mean+smoothness_mean,compactness_mean+concavity_mean+points_mean+symmetry_mean+dimension_mean+texture_se+perimeter_se+area_se+smoothness_se+compactness_se+concavity_se+points_se+symmetry_se+dimension_se+radius_worst+texture_worst+perimeter_worst+area_worst+smoothness_worst,compactness_worst+concavity_worst+points_worst+symmetry_worst+dimension_worst, data=wbcd_train)
predict(NBclassfied,newdata = wbcd_test$diagnosis,type="class")


# Evaluating model performance using a confusion matrix # #######
test_data = wbcd_test$diagnosis
fitted_data          = data.table( cbind(test_data=as.character(test_data),
                                         pred_data = paste(predict(NBclassfied,newdata = wbcd_test)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
# 0.8882
(105+46)/(105+46+2+17)
```

# 1.3 Linear Regression
```{r}
set.seed(123)
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)
# dropping useless variables: id
wbcd = wbcd[-1]

# change diagnosis to dummy variables 0,1
wbcd$diagnosis = ifelse(wbcd$diagnosis =='B',1,0)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(wbcd$diagnosis, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(wbcd))[! (1:nrow(wbcd) %in% inx_train)]

wbcd_train    = wbcd[ inx_train, ]
wbcd_valid    = wbcd[ inx_valid, ]
wbcd_test     = wbcd[-inx_train, ]
# linear model
reg <- lm(diagnosis~radius_mean+texture_mean, data=wbcd_train)
summary(reg)
coeftest(reg, vcov = vcovHC(reg, type = "HC1"))
#test
predict = predict(reg,wbcd_test)

pred <- ifelse(predict>0.5, 1, 0)


# confusion matrix
fitted_data          = data.table( cbind(test_data=wbcd_test$diagnosis,
                                         pred_data = paste(ifelse(predict>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat

# 0.9118
(49+106)/(49+106+13+2)
```

#1.4 regression tree
```{r}
set.seed(123)
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)

wbcd$diagnosis = ifelse(wbcd$diagnosis =='B',1,0)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(wbcd$diagnosis, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(wbcd))[! (1:nrow(wbcd) %in% inx_train)]

wbcd_train    = wbcd[ inx_train, ]
wbcd_valid    = wbcd[ inx_valid, ]
wbcd_test     = wbcd[-inx_train, ]
#tree
treefit = tree(diagnosis ~., data=wbcd_train)
treefit
summary(treefit)

prediction <- predict(treefit, wbcd_test)
# confusion matrix
fitted_data          = data.table( cbind(test_data=wbcd_test$diagnosis,
                                         pred_data = paste(ifelse(prediction>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
#90.59%
(58+96)/(58+96+4+12)
plot(treefit)

```

#1.5 Neural Net
```{r}
# load library
require(neuralnet)
set.seed(123)
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)
# dropping useless variables: id
wbcd = wbcd[-1]

# change diagnosis to dummy variables 0,1
wbcd$diagnosis = ifelse(wbcd$diagnosis =='B',1,0)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(wbcd$diagnosis, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(wbcd))[! (1:nrow(wbcd) %in% inx_train)]

wbcd_train    = wbcd[ inx_train, ]
wbcd_valid    = wbcd[ inx_valid, ]
wbcd_test     = wbcd[-inx_train, ]

# fit neural network
nn=neuralnet(diagnosis~.,data=wbcd_train, hidden=3,act.fct = "logistic",
                linear.output = FALSE)
plot(nn)

# test
Predict=compute(nn,wbcd_test)
Predict$net.result

prob <- Predict$net.result


# confusion matrix
fitted_data          = data.table( cbind(test_data=wbcd_test$diagnosis,
                                         pred_data = paste(ifelse(prob>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat

# 0.9059
(101+53)/(101+53+9+7)
```
# Comparing the results of the five methods for Tumor data, the KNN predict model does the best, has 98.22% confusion matric.

#2.Program Application data
```{r}
mydata       = fread('data_program_application.csv');
head(mydata)
```
# 2.1Using knn
```{r}
set.seed(123)
# dropping useless variables
mydata = mydata[,c(5:10,12:13)]
table(mydata$prog)


# Renaming diagnosis as a factor with proper labels
mydata$prog = factor(x      = mydata$prog,
                      levels = c("academic","general", "vocation"),
                        labels = c("academic","general", "vocation"))

# Initial exploratory analysis
round(prop.table(table(mydata$prog)) * 100, digits = 1)


# Prep the data:(i) normalizing and (ii)test vs non-test data sep - --------------

# Normalizing function and normalizing the wbcd data                                                        
normalize = function(x){return ((x - min(x)) / (max(x) - min(x)))}
mydata_n    = as.data.frame(lapply(mydata[,2:8], normalize))


# create training and test data
dta_train_n = mydata_n[1:140  , ]
dta_valid_n = mydata_n[140:200, ]
dta_train   = mydata[1:140  , ]
dta_valid   = mydata[140:200  , ]

# create labels for training and test data
dta_train_labels = dta_train$prog
dta_valid_labels = dta_valid$prog

# Training model on dta_training                                  - --------------
dta_valid_pred = class::knn(train = dta_train_n, 
                             cl    = dta_train_labels,
                             test  = dta_valid_n,
                             k     = 1)

# Evaluating performance on dta_test                              - --------------
dta_valid_pred = class::knn(train = dta_train_n, 
                             cl    = dta_train_labels,
                             test  = dta_valid_n,
                             k     = 5)

k5_conf_mat     = gmodels::CrossTable(x          = dta_valid_labels, 
                                      y          = dta_valid_pred,
                                      prop.chisq = TRUE)
k5_conf_mat
k5_conf_mat$t
k5_conf_mat$prop.col

```

# 2.2 Using Naive Bayes
```{r}
mydata       = fread('data_program_application.csv');
head(mydata)
```

```{r}
set.seed(123)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
# standarize the data

inx_train    = caret::createDataPartition(mydata$prog, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]

# Training a model on the data                                      # #######                      # #######
NBclassifier=naivebayes::naive_bayes(formula      = prog~ses+science+socst,
                                        usekernel = T,
                                        data      = dta_train)
predict(NBclassifier,newdata = dta_train)


# Evaluating model performance using a confusion matrix             # #######
fitted_data          = data.table( cbind(test_data = dta_test[,prog],
                                         pred_data = paste(predict(NBclassifier,newdata = dta_test)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat

# 61.02%
(24+3+9)/(24+3+9+2+5+6+4+5+1)
```

# 2.3 Linear Regression
```{r}
set.seed(123)
mydata       = fread('data_program_application.csv');
head(mydata)

# academic
mydata$prog1=ifelse(mydata$prog =='academic',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog1, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#linear
reg1 = lm(prog1~read+write+math+science+socst,data=dta_train)
summary(reg1)
coeftest(reg1, vcov = vcovHC(reg, type = "HC1"))
#test
predict = predict(reg1,dta_test)

pred <- ifelse(predict>0.5, 1, 0)

# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog1,
                                         pred_data = paste(ifelse(predict>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
# 0.65
(19+20)/(19+20+10+11)

# vocation
mydata$prog2=ifelse(mydata$prog =='vocation',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog2, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#linear
reg2 = lm(prog2~read+write+math+science+socst,data=dta_train)
summary(reg2)
coeftest(reg2, vcov = vcovHC(reg, type = "HC1"))
#test
predict = predict(reg2,dta_test)

pred <- ifelse(predict>0.5, 1, 0)

# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog1,
                                         pred_data = paste(ifelse(predict>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
# 0.4667
(1+27)/(1+27+31+1)

# general
mydata$prog3=ifelse(mydata$prog =='general',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog3, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#linear
reg3 = lm(prog3~read+write+math+science+socst,data=dta_train)
summary(reg3)
coeftest(reg3, vcov = vcovHC(reg, type = "HC1"))
#test
predict = predict(reg3,dta_test)

pred <- ifelse(predict>0.5, 1, 0)

# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog1,
                                         pred_data = paste(ifelse(predict>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
# 0.4833
(29)/(29+29+2)

# the result for academic classification(academic=1,others=0) is the best, with 65% confussion matrix.
```

#2.4 regression tree
```{r}
set.seed(123)
mydata       = fread('data_program_application.csv');
head(mydata)

# academic
mydata$prog1=ifelse(mydata$prog =='academic',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog1, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#tree
treefit = tree(prog1 ~., data=dta_train)
treefit
summary(treefit)

prediction <- predict(treefit, dta_test)
# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog1,
                                         pred_data = paste(ifelse(prediction>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
# 0.5833
(21+14)/(21+14+8+17)

# vocation
mydata$prog2=ifelse(mydata$prog =='vocation',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog2, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#tree
treefit = tree(prog2 ~., data=dta_train)
treefit
summary(treefit)

prediction <- predict(treefit, dta_test)
# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog2,
                                         pred_data = paste(ifelse(prediction>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
# 0.7833
(8+39)/(8+39+6+7)

# general
mydata$prog3=ifelse(mydata$prog =='general',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog3, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#tree
treefit = tree(prog3 ~., data=dta_train)
treefit
summary(treefit)

prediction <- predict(treefit, dta_test)
# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog3,
                                         pred_data = paste(ifelse(prediction>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat 

# the result for general classification(general=1,others=0) is the best, with 100% confussion matrix.

```

#2.5 Neural Net
```{r}
# load library
require(neuralnet)
set.seed(123)
mydata       = fread('data_program_application.csv');
head(mydata)

# academic
mydata$prog1=ifelse(mydata$prog =='academic',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog1, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#train
nn=neuralnet(prog1~read+write+math+science+socst+awards+cid,data=dta_train, hidden=3,act.fct = "logistic",
                linear.output = FALSE)
plot(nn)

# test
Predict=compute(nn,dta_test)
Predict$net.result

prob <- Predict$net.result

# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog1,
                                         pred_data = paste(ifelse(prob>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
# 0.6441
(20+18)/(19+18+9+13)

# vocation
mydata$prog2=ifelse(mydata$prog =='vocation',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog2, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#train
nn=neuralnet(prog2~read+write+math+science+socst+awards+cid,data=dta_train, hidden=3,act.fct = "logistic",
                linear.output = FALSE)
plot(nn)

# test
Predict=compute(nn,dta_test)
Predict$net.result

prob <- Predict$net.result

# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog2,
                                         pred_data = paste(ifelse(prob>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
# 0.75
(45)/(45+15)

# general
mydata$prog3=ifelse(mydata$prog =='general',0,1)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog3, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
#train
nn=neuralnet(prog3~read+write+math+science+socst+awards+cid,data=dta_train, hidden=3,act.fct = "logistic",
                linear.output = FALSE)
plot(nn)

# test
Predict=compute(nn,dta_test)
Predict$net.result

prob <- Predict$net.result

# confusion matrix
fitted_data          = data.table( cbind(test_data=dta_test$prog3,
                                         pred_data = paste(ifelse(prob>0.5, 1, 0)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confuss_mat
#78.33
47/(47+12+1)
# the result for general classification(general=1,others=0) is the best, with 78.33% confussion matrix.

```


