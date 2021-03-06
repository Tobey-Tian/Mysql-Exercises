---
title: "-Yuan Tian"
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
# 1.Tumor Data
```{r}
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)
```
#### 1.1Classifying Cancer Samples using KNN
```{r}
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
summary(wbcd_n$area_mean)
head(wbcd_n)

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
# The confusion matrices of KNN is 98.22%

### 1.2 Using Naive Bayes
```{r}
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)
# dropping useless variables: id
wbcd = wbcd[-1]
table(wbcd$diagnosis)


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
names(NBclassfied)
NBclassfied$apriori
NBclassfied$tables
NBclassfied$call


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
sum(confuss_mat[,3:4])
dim(fitted_data)
# 0.9118
(105+50)/(105+50+2+13)
```
# The confusion matrices of Naive Bayes is 91.18%

### 1.3 Linear Regression
```{r}
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)
# dropping useless variables: id
wbcd = wbcd[-1]

# change diagnosis to dummy variables 0,1
wbcd$diagnosis = ifelse(wbcd$diagnosis =='B',1,0)
# in this case, 'y' which is diagnosis is dummy variable
inx_train    = caret::createDataPartition(wbcd$diagnosis, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(wbcd))[! (1:nrow(wbcd) %in% inx_train)]

wbcd_train    = wbcd[ inx_train, ]
wbcd_valid    = wbcd[ inx_valid, ]
wbcd_test     = wbcd[-inx_train, ]
# logit model
reg <- glm(diagnosis~radius_mean+texture_mean, data=wbcd_train,
            family = binomial(link = "logit"))
summary(reg)
coeftest(reg, vcov = vcovHC(reg, type = "HC1"))
logLik(reg)
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
sum(confuss_mat[,3:4])
dim(fitted_data)
# 0.8882
(57+94)/(57+94+9+10)
```
# The confusion matrices of linear regression is 88.82%


###1.4 regression tree
```{r}
wbcd= read.csv('data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)

wbcd$diagnosis = factor(x      = wbcd$diagnosis,
                        levels = c("B",      "M"),
                        labels = c("Benign", "Malignant"))

treefit = tree(diagnosis ~., data=wbcd)
treefit
treefit1 = snip.tree(treefit,nodes=c(50,24))
summary(treefit1)
price.deciles = quantile(calif$MedianHouseValue,0:10/10)
cut.prices    = cut(calif$MedianHouseValue,price.deciles,include.lowest=TRUE)
plot(x    = wbcd$radius_mean,
     y    = wbcd$texture_mean,
     pch  = 20,
     xlab ="radius_mean",
     ylab="texture_mean")

partition.tree(tree    = treefit,
               ordvars = c("radius_mean","texture_mean"),
               add     = TRUE)

plot(x    = calif$Longitude,
     y    = calif$Latitude,
     col  = grey(10:2/11)[cut.prices],
     pch  = 20,
     xlab = "Longitude",
     ylab = "Latitude")
text(wbcd$radius_mean,wbcd$texture_mean,c('B','M')[wbcd$diagnosis])


plot(treefit)
text(treefit,cex=0.5,digits=3)

cut.predictions = cut(predict(treefit3),log(price.deciles),include.lowest=TRUE)
plot(x    = calif$Longitude,
     y    = calif$Latitude,
     col  = grey(10:2/11)[cut.predictions],
     pch  = 20,
     xlab = "Longitude",
     ylab = "Latitude")


treefit   = tree(diagnosis ~ radius_mean+texture_mean,data=wbcd)
plot(treefit)
text(treefit,cex=0.75)
```
# The confusion matrices of regression tree is 98.22%

###1.5 Neural Net
```{r}
# load library
require(neuralnet)

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
nn=neuralnet(diagnosis~radius_mean+texture_mean,data=wbcd_train, hidden=3,act.fct = "logistic",
                linear.output = FALSE)
plot(nn)

# test
Predict=compute(nn,wbcd_test)
Predict$net.result

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

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
sum(confuss_mat[,3:4])
dim(fitted_data)
# 0.9176
(105+51)/(105+51+8+6)
```
# The confusion matrices of Neural Net is 91.76%


##2.Program Application data
```{r}
mydata       = fread('https://raw.githubusercontent.com/rlowrance/re/master/hsbdemo.csv');
head(mydata)
```
#### 2.1Using knn
```{r}
# dropping useless variables
mydata = mydata[,c(6:11,13:14)]
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
summary(mydata_n$area_mean)
head(mydata_n)

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
k1_conf_mat  =   gmodels::CrossTable(x          = dta_valid_labels, 
                                     y          = dta_valid_pred,
                                     prop.chisq = TRUE)
                                
k1_conf_mat$t
# re-classify test cases
dta_valid_pred = class::knn(train = dta_train_n, 
                             cl    = dta_train_labels,
                             test  = dta_valid_n,
                             k     = 5)

k5_conf_mat     = gmodels::CrossTable(x          = dta_valid_labels, 
                                      y          = dta_valid_pred,
                                      prop.chisq = TRUE)
k5_conf_mat$t
k5_conf_mat$prop.row
k5_conf_mat$prop.col
```
#### 2.2 Using Naive Bayes
```{r}
mydata       = fread('https://raw.githubusercontent.com/rlowrance/re/master/hsbdemo.csv');
head(mydata)
```

```{r}
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
# standarize the data

inx_train    = caret::createDataPartition(mydata$prog, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]

# Training a model on the data                                      # #######
NBclassfied  = e1071::naiveBayes(prog~science+socst, data=dta_train)
predict(NBclassfied,newdata = dta_test,type="class")
names(NBclassfied)
NBclassfied$apriori
NBclassfied$tables
NBclassfied$call
head(mydata)



# Training a model on the data AGAIN                                # #######
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
sum(confuss_mat[,3:4])
dim(fitted_data)
# 0.644
(27+2+9)/(27+2+9+1+3+9+2+4+2)
```
### 2.3 Linear Regression
```{r}
mydata       = fread('https://raw.githubusercontent.com/rlowrance/re/master/hsbdemo.csv');
head(mydata)


# change prog to numerica variables 

mydata1 = mydata %>% filter(mydata$prog =='academic')%>%mutate(prog1 =1)
mydata2 = mydata %>% filter(mydata$prog =='vocation')%>%mutate(prog1 =0)
mydata3 = mydata %>% filter(mydata$prog =='general')%>%mutate(prog1 =-1)
mydata = rbind(mydata1,mydata2,mydata3)
inx_train    = caret::createDataPartition(mydata$prog1, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]

library(nnet)

mult.cere<-multinom(prog1~science+socst,data=dta_train)
summary(mult.cere)

step.cere<-step(mult.cere) 

summary(step.cere)

exp(coef(step.cere))

cere.pred<-predict(step.cere) 

cere.pred

table(dta_train$prog1,cere.pred)
Category<-levels(dta_train$prog1)
Percantage<-c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]))

rbind(Category,Percantage)




```




library(tree)
library(data.table)

calif     = fread('https://raw.githubusercontent.com/jbryer/CompStats/master/Data/cadata.dat')
treefit   = tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif)


plot(treefit)
text(treefit,cex=0.75)


price.deciles = quantile(calif$MedianHouseValue,0:10/10)
cut.prices    = cut(calif$MedianHouseValue,price.deciles,include.lowest=TRUE)
plot(x    = calif$Longitude,
     y    = calif$Latitude,
     col  = grey(10:2/11)[cut.prices],
     pch  = 20,
     xlab ="Longitude",
     ylab="Latitude")

partition.tree(tree    = treefit,
               ordvars = c("Longitude","Latitude"),
               add     = TRUE)

treefit3 = tree(log(MedianHouseValue) ~., data=calif)


plot(x    = calif$Longitude,
     y    = calif$Latitude,
     col  = grey(10:2/11)[cut.prices],
     pch  = 20,
     xlab = "Longitude",
     ylab = "Latitude")



plot(treefit3)
text(treefit3,cex=0.5,digits=3)

cut.predictions = cut(predict(treefit3),log(price.deciles),include.lowest=TRUE)
plot(x    = calif$Longitude,
     y    = calif$Latitude,
     col  = grey(10:2/11)[cut.predictions],
     pch  = 20,
     xlab = "Longitude",
     ylab = "Latitude")

###1.5 Neural Net
```{r}
# load library
require(neuralnet)

mydata      = fread('https://raw.githubusercontent.com/rlowrance/re/master/hsbdemo.csv');
head(mydata)

# change diagnosis to dummy variables 0,1

mydata1 = mydata %>% filter(mydata$prog =='academic')%>%mutate(prog1 =1)
mydata2 = mydata %>% filter(mydata$prog =='vocation')%>%mutate(prog1 =0)
mydata3 = mydata %>% filter(mydata$prog =='general')%>%mutate(prog1 = -1)
mydata = rbind(mydata1,mydata2,mydata3)
view(mydata)

# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
inx_train    = caret::createDataPartition(mydata$prog1, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]
# fit neural network
nn=neuralnet(prog1~science+socst,data=dta_train, hidden=3,act.fct = "logistic",
                linear.output = FALSE)
plot(nn)

# test
Predict=compute(nn,dta_test)
Predict$net.result

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

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
sum(confuss_mat[,3:4])
dim(fitted_data)
# 0.9176
(105+51)/(105+51+8+6)
```

iris
ir.tr <- tree(Species ~., iris)
ir.tr
ir.tr1 <- snip.tree(ir.tr, nodes = c(12, 7))
summary(ir.tr1)
par(pty = "s")
plot(iris[, 3],iris[, 4], type="n",
     xlab="petal length", ylab="petal width")
text(iris[, 3], iris[, 4], c("s", "c", "v")[iris[, 5]])
partition.tree(ir.tr1, add = TRUE, cex = 1.5)


