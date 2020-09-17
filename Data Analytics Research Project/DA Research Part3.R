---
title: "No Error_Lung Cancer"
author: "Heyif"
date: "3/27/2020"
output:
  word_document: default
  html_document: default
---

preliminaries
```{R}
library(data.table)
library(tidyverse)
library(FNN)
library(ggplot2)
library(factoextra)
library(cluster)
library(caret)
library(gridExtra)
library(car)
library(modelr)
```

# Kmeans Clustering

prepare data
```{r,warning=FALSE, message=FALSE}
lungcancer <- read.csv(file = 'Air Quality_Lung Cancer Data.csv')
lung = lungcancer[,c(1,4,5,7,8,9,15,16,17,18,19,20,21,23)]
# nomalize data
row.names(lung) <- lung[,1]
lung <-lung[,-1]
lung.norm <- sapply(lung, scale)
lung.norm = as.data.frame(lung.norm)
head(lung.norm[,-1])
```

clustering
```{r,warning=FALSE, message=FALSE}

ratio_ss <- rep(0,7)

for (k in 1:7) {
  lung_km <- kmeans(lung.norm[,-1], k, nstart = 20)
  ratio_ss[k] <- lung_km$tot.withinss
  
}
plot(ratio_ss, type = 'b', xlab = 'k')

# dendrogram
hc <- hclust(dist(lung.norm[,-1]), "ward.D")
plot(hc) 
rect.hclust(hc, k = 3, border="red") 
grid()
memb <- cutree(hc, k = 3)

# from plot, we choose k=3
lungkm <- kmeans(lung.norm[,-1], 3)
lungkm$centers
lungkm$size
dist(lungkm$centers)

# line plots
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(lungkm$centers), max(lungkm$centers)), xlim = c(0, 13))
axis(1, at = c(1:12), labels = names(lung[-1]))
for (i in c(1:3))
  lines(lungkm$centers[i,], lty = i, lwd = 2, col = switch(i,  "red", 
                                                       "green", "purple"))
text(x = 0.5, y = lungkm$centers[, 1], labels = paste("Cluster", c(1:3)))
grid()

#cluster
fviz_cluster(lungkm, data = lung.norm)

#heatmap
heatmap(as.matrix(lung.norm)[order(lungkm$cluster),], Colv = NA, hclustfun = hclust)

# scatter plot
clusplot(lung.norm, lungkm$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

with(lung.norm, pairs(lung.norm[,c(2:4,6)], col=c(1:3)[lungkm$cluster])) 

lung.norm %>%
  as_tibble() %>%
  mutate(cluster = lungkm$cluster) %>%
  ggplot(aes(PM2.5,Sociod_EQI, color = factor(cluster))) +
  geom_point()
lung.norm %>%
  as_tibble() %>%
  mutate(cluster = lungkm$cluster) %>%
  ggplot(aes(PM2.5,Land_EQI, color = factor(cluster))) +
  geom_point()
```

Copy the cluster data back to your original data as a new predictor: KMeansCluster
```{r,warning=FALSE, message=FALSE}
KMeansCluster = as.factor(lungkm$cluster)
lungkmeans <- cbind(lungcancer,KMeansCluster)
head(lungkmeans)
#write.csv(lungkmeans, "lungcancerkmeans", sep="\t", row.names=FALSE, col.names=FALSE)
```

# Hierarchical Clustering
```{R}
lung_cancer<- read.csv("Air Quality_Lung Cancer Data.csv")
# Considering the high collinearity between some variables, we drop some variables to avoid that some variables get a higher weight than others
lung_cancer_data<- lung_cancer[,c(1,4,5,7,8,9,15,16,17,18,19,20,21,23)]

# Normalized distance:
lung_cancer_sc<- scale(lung_cancer_data[,2:14])

# Change the FIPS_code to row names 
row.names(lung_cancer_sc) <- lung_cancer_data[ ,1]

head(lung_cancer_sc)

# Try different methods for computing distance
lungcancer_dist1<- dist(lung_cancer_sc, method = "euclidean")
lungcancer_dist2<- dist(lung_cancer_sc, method = "maximum")
lungcancer_dist3<- dist(lung_cancer_sc, method = "manhattan")
lungcancer_dist4<- dist(lung_cancer_sc, method = "canberra")
lungcancer_dist5<- dist(lung_cancer_sc, method = "binary")
lungcancer_dist6<- dist(lung_cancer_sc, method = "minkowski")

lungcancer_hc1<- hclust(lungcancer_dist1, method = "single")
lungcancer_hc2<- hclust(lungcancer_dist1, method = "complete")
lungcancer_hc3<- hclust(lungcancer_dist1, method = "average")
lungcancer_hc4<- hclust(lungcancer_dist1, method = "median")
lungcancer_hc5<- hclust(lungcancer_dist1, method = "centroid")
lungcancer_hc6<- hclust(lungcancer_dist1, method = "ward.D")

plot(lungcancer_hc1, hang = -1, ann = FALSE) 
plot(lungcancer_hc2, hang = -1, ann = FALSE) 
plot(lungcancer_hc3, hang = -1, ann = FALSE) 
plot(lungcancer_hc4, hang = -1, ann = FALSE) 
plot(lungcancer_hc5, hang = -1, ann = FALSE) 
plot(lungcancer_hc6, hang = -1, ann = FALSE) 
# The plot using "ward.D" method looks the best.

```
# Determining Optimal Clusters
```{r cars}
# Perform Average Silhouette Method
fviz_nbclust(lung_cancer_sc, FUN = hcut, method = "silhouette")
# Perform Gap Statistic Method
mydist <- function(x) dist(x, method = "euclidean")
mycluster <- function(x, k) list(cluster=cutree(hclust(mydist(x), method = "ward.D"),k=k))
myclusGap <- clusGap(lung_cancer_sc,FUN = mycluster, K.max = 10, B = 50)
fviz_gap_stat(myclusGap)

# Considering the number of clusters and the distribution, we choose k equal to 3.

lungcancer_cut <- cutree(lungcancer_hc6, k = 3) #ward.D

```
#Visualization
```{r}
#dendrogram
plot(lungcancer_hc6)
rect.hclust(lungcancer_hc6, k=3, border = 2:4)

```

```{r pressure, echo=FALSE}
# heatmap
heatmap(as.matrix(lung_cancer_sc), Colv = NA, hclustfun = hclust, main = "heatmap")
# Visualize the result in a scatter plot
fviz_cluster(list(data = lung_cancer_sc, cluster = lungcancer_cut))
```

```{r}
# Analyze the trend between lung cancer Mortalityad and PM2.5 from the data cluster-wise 
seeds_df_cl <- mutate(lung_cancer_data, cluster = lungcancer_cut)
ggplot(seeds_df_cl, aes(x=PM2.5, y = Lung.Cancer, color = factor(cluster))) + geom_point()

#clusplot(lung_cancer_sc, lungcancer_cut, color = TRUE, shade = TRUE,          labels = 2, lines = 0)

```
# Add the new clustering result to the original dataset 
```{r}
Hie_Cluster = data.frame(lungcancer_cut)
lung_Hie <- cbind(lung_cancer,Hie_Cluster)
head(Hie_Cluster)

#Export the new dataset
#write.csv(lung_Hie, "lungcancer Hiecluster", sep="\t", row.names=FALSE, col.names=FALSE)

```

# Feature engineering
```{R}
set.seed(123456)
data <- fread("Lung_cancer_data.csv")

chosen_variable_original <- data[ , .(Lung.Cancer, PM2.5, Land_EQI, Sociod_EQI, Built_EQI, O3, CO, Water_EQI)]

chosen_variable <- data[ , .(Lung.Cancer, PM2.5, Land_EQI, Sociod_EQI, Built_EQI, O3, CO, Water_EQI, HierCluster, KMeansCluster)]

chosen_variable[ , `:=`(HierCluster = factor(HierCluster), KMeansCluster = factor(KMeansCluster))]


## training set 70%
id_train <- createDataPartition(data$Lung.Cancer, p=0.7)$Resample1
#training_original <- chosen_variable_original[id_train, ]
#training_new      <- chosen_variable[id_train, ]
training <- chosen_variable[id_train, ]
## validation set 30%
#validation_original <- chosen_variable_original[-id_train, ]
#validation_new      <- chosen_variable[-id_train]
validation <- chosen_variable[-id_train]
```

# original model
```{R}
model <- lm(Lung.Cancer ~ PM2.5 + Land_EQI + Sociod_EQI + Built_EQI + O3 + CO + Water_EQI + I(PM2.5^2) + I(O3^2) + I(CO^2) + I(Land_EQI^2) + I(Sociod_EQI^2) + I(Built_EQI^2) + I(Water_EQI^2) + PM2.5:Land_EQI + PM2.5:Sociod_EQI + PM2.5:Built_EQI + PM2.5:O3 + PM2.5:CO + PM2.5:Water_EQI + Land_EQI:O3 + Sociod_EQI:Built_EQI + Sociod_EQI:O3 + Sociod_EQI:CO + Sociod_EQI:Water_EQI + Built_EQI:O3 + Built_EQI:CO + Built_EQI:Water_EQI + CO:Water_EQI, data = training)

broom::glance(model)
```

```{R}
prediction_original <- predict(model, newdata = validation)

R_square <- rsquare(model, data = validation)

RMSE <- sqrt(mean((validation$Lung.Cancer - prediction_original)^2))

cat("R-Squared:", R_square, "\nRMSE:", RMSE)

residuals_original <- prediction_original - validation$Lung.Cancer
``` 

# add cluster variables 
```{R}
model2 <- lm(Lung.Cancer ~ PM2.5 + Land_EQI + Sociod_EQI + Built_EQI + O3 + CO + Water_EQI + I(PM2.5^2) + I(O3^2) + I(CO^2) + I(Land_EQI^2) + I(Sociod_EQI^2) + I(Built_EQI^2) + I(Water_EQI^2) + PM2.5:Land_EQI + PM2.5:Sociod_EQI + PM2.5:Built_EQI + PM2.5:O3 + PM2.5:CO + PM2.5:Water_EQI + Land_EQI:O3 + Sociod_EQI:Built_EQI + Sociod_EQI:O3 + Sociod_EQI:CO + Sociod_EQI:Water_EQI + Built_EQI:O3 + Built_EQI:CO + Built_EQI:Water_EQI + CO:Water_EQI + HierCluster + KMeansCluster, data = training)

summary(model2)
```

```{R}
model3 <- lm(Lung.Cancer ~ PM2.5 + Land_EQI + Sociod_EQI + 
    Built_EQI + O3 + CO + Water_EQI + I(PM2.5^2) + I(O3^2) + 
    I(CO^2)  + I(Sociod_EQI^2) + 
    I(Water_EQI^2)  + PM2.5:Sociod_EQI + PM2.5:CO + PM2.5:Water_EQI + Sociod_EQI:Built_EQI + Sociod_EQI:O3 + Sociod_EQI:CO + Sociod_EQI:Water_EQI   + CO:Water_EQI + HierCluster + KMeansCluster, data = training)

summary(model3)

vif(model3)
```

# assessing residuals
## residuals histogram

```{R}
ggplot(mapping = aes(x = model3$residuals)) + geom_histogram(fill = "pink", alpha = 0.5, color = "black") + ggtitle("Model3 Residuals") + labs(x = "residuals")
```

```{R}
plot(model3, which = 1, id.n = 10)
plot(model3, which = 2, id.n = 10)
plot(model3, which = 4, id.n = 10)
plot(model3, which = 5, id.n = 10)
```


```{R}
training_remove_outliers <- training[-c(660, 615, 298, 640, 663, 647, 1330, 1410, 1015, 134, 1138)]

model4 <- lm(Lung.Cancer ~ PM2.5 + Land_EQI + Sociod_EQI + 
    Built_EQI + O3 + CO + Water_EQI + I(PM2.5^2) + I(O3^2) + 
    I(CO^2)  + I(Sociod_EQI^2) + 
    I(Water_EQI^2)  + PM2.5:Sociod_EQI + PM2.5:CO + PM2.5:Water_EQI + Sociod_EQI:Built_EQI + Sociod_EQI:O3 + Sociod_EQI:CO + Sociod_EQI:Water_EQI   + CO:Water_EQI + HierCluster + KMeansCluster, data = training_remove_outliers)

summary(model4)
```

```{R}
prediction_new <- predict(model4, newdata = validation)

R_square_new <- rsquare(model4, data = validation)

RMSE_new <- sqrt(mean((validation$Lung.Cancer - prediction_new)^2))

cat("R-Squared:", R_square_new, "\nRMSE:", RMSE_new)
```

```{R}
residuals_new <- prediction_new - validation$Lung.Cancer

ggplot(mapping = aes(x = residuals_new)) + geom_histogram(fill = "pink", alpha = 0.5, color = "black") + ggtitle("Model4 Errors") + labs(x = "Errors")
```

# model comparison
## training set 
```{R}
broom::glance(model4)
```

```{R}
par(mfrow=c(1, 2))
plot(model, which = 3, id.n = 6)
plot(model4, which = 3, id.n = 6)

```

```{R}
par(mfrow=c(1, 2))
plot(model, which = 2, id.n = 6)
plot(model4, which = 2, id.n = 6)
```


## validation set
```{R}
par(mfrow=c(1, 2))
# Left: model 1
qqnorm(residuals_original); qqline(residuals_original, col = "red")

# Right: model 2
qqnorm(residuals_new); qqline(residuals_new, col = "red")
```

