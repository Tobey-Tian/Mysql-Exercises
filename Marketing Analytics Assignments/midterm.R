rm(list=ls()); gc()
library(data.table)
library("stringr")
library(dplyr)
library(tidyverse)
library(sandwich)
library(lmtest)


revenue <- fread('/Users/apple/Desktop/data_movies_basic.csv',header = TRUE)
view(revenue)
reviews <- fread('/Users/apple/Desktop/reviews_at_movie.csv',header = TRUE)
view(reviews)
head(revenue)
head(reviews)
dt_revenue = as.data.table(revenue)
dt_reviews = as.data.table(reviews)
head(dt_reviews)
dt_revenue[,total_earn:=sum(daily_gross),by=movie_name]
mm = tapply(dt_revenue$daily_gross,dt_revenue$movie_name,sum)
nn = tapply(dt_revenue$daily_gross,weekdays(as.Date(dt_revenue$date)),sum)
a = dt_revenue[seq(2,nrow(dt_revenue),by=2),]
b = dt_revenue[,seq(2,ncol(dt_revenue),by=2)]
m = seq(2,nrow(dt_revenue),by=2)
n = c(seq(2,ncol(dt_revenue),by=2))

dt_all = merge(dt_revenue,dt_reviews)
dt_all[,avg_earn:=mean(daily_gross),by=reviews]

dt_revenue$weekday = weekdays(as.Date(dt_revenue$date))
dt_revenue[,week_earn:=sum(daily_gross),by=weekday]
dt_week = dt_revenue[,week_reve:=.(list(unique(week_earn))),by=weekday]
dt_week[,.(weekday,week_reve)]
summult=function(X,N){
  sum = 0
  X = as.numeric(X)
  N = as.integer(N)
  if(N <= 0){print("N must be integer")}
  else{
  for (i in X){
    if (i%%N==0){
      sum = i + sum
    }
  }
  return(sum)
  }
}
summult(10+(1:10),3)


