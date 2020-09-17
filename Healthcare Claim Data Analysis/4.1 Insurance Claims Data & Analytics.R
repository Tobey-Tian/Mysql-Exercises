# hs-256  hw4
rm(list=ls());
library(stargazer)
library(tidyverse)
library(knitr)
library(readr)
library(dplyr)
library(data.table)

# question 1
# step 1
# read the outpatient table and the two RA codes 
outp <- read.table('/Users/apple/Desktop/hs_256/VTOUTP16.TXT',sep=',',header=TRUE)

ra1 <- read.csv('/Users/apple/Desktop/hs_256/hw4/RA1.csv',sep=',',header=TRUE)
ra2 <- read.csv('/Users/apple/Desktop/hs_256/hw4/RA2.csv',sep=',',header=TRUE)

ra1_code = ra1$ICD.10.Codes
ra2_code = ra2$ICD.10.Codes

#filter the two sub-cohorts
outp_ra1 = outp%>% filter(DX1 %in% ra1_code|DX2 %in% ra1_code|DX3 %in% ra1_code|DX4 %in% ra1_code|DX5 %in% ra1_code|
                     DX6 %in% ra1_code|DX7 %in% ra1_code|DX8 %in% ra1_code|DX9 %in% ra1_code|DX10 %in% ra1_code|
                     DX11 %in% ra1_code|DX12 %in% ra1_code|DX13 %in% ra1_code|DX14 %in% ra1_code|DX15 %in% ra1_code|
                     DX16 %in% ra1_code|DX17 %in% ra1_code|DX18 %in% ra1_code|DX19 %in% ra1_code|DX20 %in% ra1_code)

outp_ra2 = outp%>% filter(DX1 %in% ra2_code|DX2 %in% ra2_code|DX3 %in% ra2_code|DX4 %in% ra2_code|DX5 %in% ra2_code|
                            DX6 %in% ra2_code|DX7 %in% ra2_code|DX8 %in% ra2_code|DX9 %in% ra2_code|DX10 %in% ra2_code|
                            DX11 %in% ra2_code|DX12 %in% ra2_code|DX13 %in% ra2_code|DX14 %in% ra2_code|DX15 %in% ra2_code|
                            DX16 %in% ra2_code|DX17 %in% ra2_code|DX18 %in% ra2_code|DX19 %in% ra2_code|DX20 %in% ra2_code)

#step 2
#frequency of chronic RA
outp_ra1_new <- outp_ra1[,10:29]
code_frequency1 <- table(unlist(outp_ra1_new))
code_frequency1 <- data.frame(code_frequency1)
code_frequency1_t = code_frequency1 %>% filter(Var1 %in% ra1_code)%>% arrange(-Freq)
head(code_frequency1_t, 3)


#frequency of other RA
outp_ra2_new <- outp_ra2[,10:29]
code_frequency2 <- table(unlist(outp_ra2_new))
code_frequency2 <- data.frame(code_frequency2)
code_frequency2_t = code_frequency2 %>% filter(Var1 %in% ra1_code)%>% arrange(-Freq)
head(code_frequency2_t, 3)

#step 3
# gender difference of chronic RA
ra1_male = count(outp_ra1 %>% filter(sex==1))
ra1_male
ra1_female = count(outp_ra1 %>% filter(sex==2))
ra1_female

# gender difference of other RA
ra2_male = count(outp_ra2 %>% filter(sex==1))
ra2_male
ra2_female = count(outp_ra2 %>% filter(sex==2))
ra2_female

#2*2 table
genderdiff = matrix(c(252, 724, 14, 16),
       nrow = 2,
       dimnames = list(Gender = c("Male", "Female"),
                       RA = c("chronic", "other")))
genderdiff
# fisher test
fisher.test(genderdiff, alternative = "less")

#step 4
# IQR of chronic RA charges
summary(outp_ra1$CHRGS)
IQR(outp_ra1$CHRGS)

# IQR of other RA charges
summary(outp_ra2$CHRGS)
IQR(outp_ra2$CHRGS)

#step 5
rev <- read.table('/Users/apple/Desktop/hs_256/VTREVCODE16.TXT',sep=',',header=TRUE)
ra1merge = merge(outp_ra1, rev, by = "Uniq")
ra2merge = merge(outp_ra2, rev, by = "Uniq")

#frequency of REVCODE for chronic RA
revcode_frequency1 <- table(ra1merge$REVCODE)
revcode_frequency1 <- data.frame(revcode_frequency1)
revcode_frequency1_t = revcode_frequency1 %>% arrange(-Freq)
head(revcode_frequency1_t, 5)

#frequency of REVCODE for other RA
revcode_frequency2 <- table(ra2merge$REVCODE)
revcode_frequency2 <- data.frame(revcode_frequency2)
revcode_frequency2_t = revcode_frequency2 %>% arrange(-Freq)
head(revcode_frequency2_t, 5)

#question 2
inp <- read.table('/Users/apple/Desktop/hs_256/VTINP16_upd.TXT',sep=',',header=TRUE)
view(inp)
mdc1 = inp %>% filter(MDC=="1")
mdc14 = inp %>% filter(MDC=="14")

###mdc1
#HHI of insurance company
mdc1 = data.table(mdc1)
table(mdc1$hnum2)
share = round(prop.table(table(mdc1$hnum2)), digits = 3)
share = data.frame(share)
HHI <- function(x){
  return(sum(x^2))
}
HHI(share$Freq)
#0.4065

#HHI by admissions
admissions = tapply(unique(mdc1$UNIQ),mdc1$hnum2,length)
totalad = sum(admissions)
share2 =lapply(admissions,function(x)x/totalad)
HHI(unlist(share2))
#0.4070

# HHI by charges
mdc1 = mdc1 %>% filter(is.na(CHRGS) == FALSE)
charge = tapply(mdc1$CHRGS,mdc1$hnum2,sum)
total = sum(charge)
share3 =lapply(charge,function(x)x/total)
HHI(unlist(share3))
#0.6368

###mdc14

#HHI of insurance company
mdc14 = data.table(mdc14)
table(mdc14$hnum2)
share4 = round(prop.table(table(mdc14$hnum2)), digits = 3)
share4 = data.frame(share4)
HHI(share4$Freq)
#0.2142

#HHI by admissions
admissions = tapply(unique(mdc14$UNIQ),mdc14$hnum2,length)
totalad = sum(admissions)
share5 =lapply(admissions,function(x)x/totalad)
HHI(unlist(share5))
#0.2141

# by charges
charge = tapply(mdc14$CHRGS,mdc14$hnum2,sum)
total = sum(charge)
share6 =lapply(charge,function(x)x/total)
HHI(unlist(share6))
#0.2453

#conclusion
#mdc1 is more concentrated
share %>% arrange(-Freq)
# lion share:5:University of Vermont Medical Center (as of 2014)
# for admission, the lion gets 62.24%
unlist(share2)
# for charges, the lion gets 79.27%
unlist(share3)

