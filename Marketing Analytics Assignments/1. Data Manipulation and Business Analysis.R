#homework 4
remove(list=ls())
getwd()
library(dplyr)
library(tidyverse)
library(ggplot2)
# Problem 1 Date Manipulaon
#1.1 Program the function Is_leap_Year which takes a 4 digit year between 1950 and 2050 as
#input and returns TRUE only if it is a leap year and FALSE otherwise
Is_leap_Year = function(Year){
  year = as.integer(Year)
  condition_year = (year >= 1950) & (year <= 2050)
  if(!condition_year){print('Year should between 1950 and 2050')}
  if(condition_year){
    if((year%%4) == 0){
      if((year%%100) == 0){
        if((year%%400) == 0){
          print(TRUE)
        }else{print(FALSE)}
      }else{print(TRUE)}
    }else{print(FALSE)}
  }
}

Is_leap_Year(1998)
Is_leap_Year(2000)
Is_leap_Year(1900)
Is_leap_Year(2020)

#1.2 Program the function second_Saturday that displays a list of the dates for the 2nd Saturday
# of every month for a given year. It does not need to return anything.
second_Saturday = function(Year){
   for (month in 1:12){
   m <- paste(Year,month,'01',sep = '-')
   n <- paste(Year,'12','31',sep = '-')
   year_1 = as.Date(m)
   year_2 = as.Date(n) 
   dates_of_this_year =seq.Date(
   from =year_1,
   to   =year_2,by="day")
   a = dates_of_this_year[weekdays(dates_of_this_year)=="Saturday"][2]
   print(as.list(a))
 }
}

second_Saturday(1998)
second_Saturday(2019)

#1.3 Write a program that takes a date as input and one integer between -20000 and 20000. The
#function should return the day of the week of adding the integer number of days to the initial
#date. That is, new date = the input date +/- the number of days (the integer between -20000
#and 2000) and then compute the day of the week of the new date.
day_Of_the_Week_in_N_days = function(date,N){
  condition = (N >= -20000) & (N <= 20000)
  if(!condition){print('N should between -20000 and 20000')}
  else{
  date_a = as.Date(date)
  weekdays(date_a + N)
  }
}
day_Of_the_Week_in_N_days(date = '2019-09-22',N=2)


#2.Load the data frame flights.csv from LATE
library('data.table')
#2.1.a R data frames: use read.csv()
x1 = Sys.time()
fli_1 <- read.csv('/Users/apple/Desktop/BUS-256A/hw4/flights.csv',header = TRUE)
Sys.time()-x1  

#2.1.b data.tables use fread()
y1 = Sys.time()
fli_2 <- fread('/Users/apple/Desktop/BUS-256A/hw4/flights.csv',header = TRUE)
Sys.time()-y1   

#2.2 Order the rows of the data frame by Airline, Flight Number, Year, Monthly and day
#2.2.a data frame
head(fli_2)
df_fli = data.frame(fli_1)
x2 = Sys.time()
df_fli[order(df_fli$AIRLINE,df_fli$FLIGHT_NUMBER,df_fli$YEAR,df_fli$MONTH,df_fli$DAY),]
Sys.time()-x2  

#2.2.b data table
dt_fli = data.table(fli_2)
y2 = Sys.time()
dt_fli[order(dt_fli$AIRLINE,dt_fli$FLIGHT_NUMBER,dt_fli$YEAR,dt_fli$MONTH,dt_fli$DAY),]
Sys.time()-y2  

#2.3Conditional on both Airline and hour of the day (the hour of the day of 09:26 is 09), compute:
#2.3.a Total number of flights
# first I get the 'hour of the day'from DEPARTURE_TIME column and add a new column "dept_hour" to the data to filter.
head(dt_fli)
#data.frame
a1 <- c(df_fli$SCHEDULED_DEPARTURE)
b1 <- as.character(a1+10^4)
c1 <- substr(b1,2,3)
df_fli$dep_hour = c1
x3 = Sys.time()
df_fli %>% group_by(AIRLINE,dep_hour) %>% count(AIRLINE)
Sys.time()-x3 

#data.table
a2 <- c(dt_fli$SCHEDULED_DEPARTURE)
b2 <- as.character(a2+10^4)
c2 <- substr(b2,2,3)
dt_fli$dep_hour = c2
y3 = Sys.time()
dt_fli %>% group_by(AIRLINE,dep_hour) %>% count(AIRLINE)
Sys.time()-y3 

#2.3.b  Number of flights with a delay of more than 15 minutes
#data.frame
x4 = Sys.time()
filter(df_fli,DEPARTURE_DELAY > 15) %>% group_by(AIRLINE,dep_hour) %>% count(AIRLINE)
Sys.time()-x4

#data.table
y4 = Sys.time()
filter(dt_fli,DEPARTURE_DELAY > 15) %>% group_by(AIRLINE,dep_hour) %>% count(AIRLINE)
Sys.time()-y4

#2.3.c Number of flights that depart with less 15 minutes delay
#data.frame
x5 = Sys.time()
filter(df_fli,DEPARTURE_DELAY > 0 & DEPARTURE_DELAY< 15) %>% group_by(AIRLINE,dep_hour) %>% count(AIRLINE)
Sys.time()-x5

#data.table
y5 = Sys.time()
filter(dt_fli,DEPARTURE_DELAY >0 & DEPARTURE_DELAY < 15) %>% group_by(AIRLINE,dep_hour) %>% count(AIRLINE)
Sys.time()-y5

#2.3.d Does the distribution of the % delayed flights conditional on time of the day and firm vary by firm? 
# I want to see the distribution of flights that DEPARTURE_DELAY > 0 on time of the day and firm
dis_dep_delay <- data.table(filter(dt_fli, DEPARTURE_DELAY >0) %>% group_by(AIRLINE,dep_hour) %>% count(AIRLINE))
dis_dep_delay[,mean_delay_by_hour:=mean(n), by=dep_hour]
dis_dep_delay[,sd_delay_by_hour:=sd(n), by=dep_hour]
dis_dep_delay
ggplot(dis_dep_delay,aes(x = AIRLINE,y=mean_delay_by_hour,fill = dep_hour))+geom_bar(stat="identity")
ggplot(dis_dep_delay,aes(x = AIRLINE,y=sd_delay_by_hour,fill = dep_hour))+geom_bar(stat="identity")
# CONCLUSION
# From the plot result, we can see the distribution of the % delayed flights conditional on time of the day and firm 
#share similar same means, standard deviation for different airline firms, for some airlines such as VX and WN,they 
#share small difference mean and sd during hour 01-04..

#2.4 Compute the market share of number of flights that each firm has over
#2.4.a The entire year
#data.frame
x6 = Sys.time()
df_fli_1 <- df_fli %>% group_by(AIRLINE) %>% count(AIRLINE)
a = sum(df_fli_1[,2])
df_fli_1$mkt_share = df_fli_1$n/a
df_fli_1
Sys.time()-x6

#data.table
y6 = Sys.time()
dt_fli_1 <- dt_fli %>% group_by(AIRLINE) %>% count(AIRLINE)
a = sum(dt_fli_1[,2])
dt_fli_1$mkt_share = dt_fli_1$n/a
dt_fli_1
Sys.time()-y6

#visualizations
ggplot(dt_fli_1,aes(x = AIRLINE,y = mkt_share,fill = AIRLINE ))+geom_bar(stat="identity")
# Result explanation
# From the plot result, for the entire year,the first three airlines that has the largest market share 
# are WN,DL and AA. And WN has over 20% market shares over the year. The three airlines that has the smallest
# market share are HA,VX and F9,and each of them has less then 2% market share over the year.

#2.4.b Month by month.
#data.frame
x7 = Sys.time()
df_fli_2 <- df_fli %>% group_by(AIRLINE) %>% group_by(MONTH) %>% count(AIRLINE)
b = sum(df_fli_2[,3])
df_fli_2$month_share = df_fli_2$n/b
df_fli_2
Sys.time()-x7

#data.table
y7 = Sys.time()
dt_fli_2 <- dt_fli %>% group_by(AIRLINE) %>% group_by(MONTH) %>% count(AIRLINE)
b = sum(dt_fli_2[,3])
dt_fli_2$month_share = dt_fli_2$n/b
dt_fli_2
Sys.time()-y7

#Export in excel
write.csv(dt_fli_2,file = '/Users/apple/Desktop/mktshare_by_month.csv')
#visualizations
ggplot(dt_fli_2,aes(x = MONTH,y = month_share,fill = AIRLINE ))+geom_bar(stat="identity")

# From the plot result, 
#For the entire year,the first three airlines that has the largest market share are WN,DL and AA. 
#And WN has over 20% market shares over the year. The three airlines that has the smallest
# market share are HA,VX and F9,and each of them has less then 2% market share over the year.

#For month by month,the market share distribution across airlines vary by month. 
#For different airlines, they may have different market strategies vary by month. 
#For instance, airline US has higher market shares from Jan to Jun, but has zero market share from Jul to Dec, 
#it may because there major airline flights concentrate on the first half of the year. 
#Most airlines have lower market share in Feb may because there is less vacations in Feb. 

## Problem 3
rm(list=ls()); gc()
library(data.table)
library("stringr")

#  . Creating a global environtment (kind of a global environment)                              # ----------
e0 = list()

#  . Loading dta and removing columns without variation                                         # ----------
rm(list = ls()[ls()!='e0'])
dta_at_imdb = fread(file = '/Users/apple/Desktop/BUS-256A/hw4/data_movies_at_imdb.csv')
col_variation = apply(dta_at_imdb,2,function(x)return(length(unique(x))))
names(col_variation[col_variation==1])
plot_at_imdb     = dta_at_imdb$Plot
poster_at_imdb  = dta_at_imdb$Poster
dta_at_imdb[, V1       := NULL]
dta_at_imdb[, Response := NULL]
dta_at_imdb[, Plot     := NULL]
dta_at_imdb[, Poster   := NULL]

e0$dta_at_imdb        = dta_at_imdb
e0$dta_plot_at_imdb   = plot_at_imdb
e0$dta_poster_at_imdb = poster_at_imdb

#  . Fixing the column Actors                                                                   # ----------
rm(list     = ls()[ls()!='e0'])
dta_at_imdb  = data.table::copy(e0$dta_at_imdb)
dta_at_imdb$Actors

b            = strsplit(dta_at_imdb$Actors,',')
temp         = cbind(table(unlist(b)))
NA_text      = b[[1]]
for (k  in 1:length(b)) {
  b[[k]]             = stringr::str_trim(b[[k]],side = "both")   
  if(b[[k]]==NA_text && length(b[[k]])==1 ) b[[k]]=NA
}

c = list()
for (k  in 1:length(b)) {
  c[[k]]              = as.data.table(cbind(dta_at_imdb$imdbID[k],
                                            cbind(b[[k]])))
}

dta_at_imdb$Actors      = sapply(b, length)
movies_actors           = rbindlist(c)
colnames(movies_actors) = c('imdbID','Actor') 
e0$dta_at_imdb_actors   = movies_actors
head(e0$dta_at_imdb_actors)

## Problem 4
#n = 2  prob = 1/2
#n = 3  prob = 1/3+1/3*1/2 = 0.5
#n = 4  prob = 1/4+1/4(1/3+1/3*1/2)+1/4*1/2 =0.5
#n = 5  prob = 1/5+1/5(1/4+1/4(1/3+1/3*1/2)+1/4*1/2)+1/5*1/2 =0.5

seat_free = function(n){
    if (n ==1 ){return (1)}
    if (n ==2 ){return (1/2)}
    else{
      for(i in 3:n){
  solution = 0
  solution = 1/i+1/i*1/2 + 1/i*solution
  return(solution)
      }
  }
}
a <- matrix(NA,nrow = 9)
rownames(a) = paste('passenger number',seq(100,500,by=50))
a[] <- sapply(seq(100,500,by=50),FUN =seat_free)
print(a)
barplot(a, beside = TRUE,xlab ='passenger number', main = "prob")

# From the result, we can see no matter how much passenger it is, the probability that the last passenger's 
#seat has not been taken is always 0.5.


