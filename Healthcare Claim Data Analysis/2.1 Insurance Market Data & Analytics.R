library(sandwich)
library(plm)
library(lmtest)
library(stargazer)
library(tidyverse)
library(knitr)
library(car)
library(readr)
library(dplyr)

#HW2_Question1


CPSC_Enrollment_Info_2019_10 <- read.csv("~/Documents/Brandeis/Healthcare Data Analytics/Assignment2/CPSC_Enrollment_2019_10/CPSC_Enrollment_Info_2019_10.csv")
View(CPSC_Enrollment_Info_2019_10)

CPSC_Enrollment_Info_2019_10_new <- CPSC_Enrollment_Info_2019_10 %>% filter(Enrollment != "*")
CPSC_Enrollment_Info_2019_10_new <- CPSC_Enrollment_Info_2019_10_new %>% filter(State %in% c("NY","MI","TN","MN","OK","NV","ID","DE","WY") )
CPSC_Enrollment_Info_2019_10_new <- CPSC_Enrollment_Info_2019_10_new %>% filter(substr(CPSC_Enrollment_Info_2019_10_new$Contract.Number,1,1) != "S")
View(CPSC_Enrollment_Info_2019_10_new)


Monthly_Report_By_Plan_2019_10 <- read.csv("~/Documents/Brandeis/Healthcare Data Analytics/Assignment2/Monthly_Report_By_Plan_2019_10/Monthly_Report_By_Plan_2019_10.csv")
View(Monthly_Report_By_Plan_2019_10)

Monthly_Report_By_Plan_2019_10_new <- Monthly_Report_By_Plan_2019_10 %>% filter(Enrollment != "*", substr(Monthly_Report_By_Plan_2019_10$Contract.Number,1,1) != "S")
View(Monthly_Report_By_Plan_2019_10_new)


library(readxl)
MajorInsuranceOrgs <- read_excel("Documents/Brandeis/Healthcare Data Analytics/Assignment2/MajorInsuranceOrgs.xlsx")
View(MajorInsuranceOrgs)

#merge majorname
merge_majorname <- merge(Monthly_Report_By_Plan_2019_10_new, MajorInsuranceOrgs, by.x = "Organization.Marketing.Name", by.y = "Organization Marketing Name")
View(merge_majorname)

#merje contract.name
merge_majorname_id_name <- distinct(merge_majorname[,c("Contract.Number","MajorInsuranceOrgName")])
View(merge_majorname_id_name)
merge_contract_new <- merge(merge_majorname_id_name, CPSC_Enrollment_Info_2019_10_new, by = "Contract.Number")
View(merge_contract_new)


#转一下格式
merge_contract_new$Enrollment <- as.numeric(as.character(merge_contract_new$Enrollment))
write.csv(merge_contract_new, file = "~/Documents/Brandeis/Healthcare Data Analytics/Assignment2/merge_contract_new.csv")

#sum state
sumstate <- merge_contract_new %>% group_by(State) %>% summarise(sum_state = sum(Enrollment))
View(sumstate)

#sum state & company
sumstate_company <- merge_contract_new %>% group_by(State, MajorInsuranceOrgName) %>% summarise(sumstate_company = sum(Enrollment))
View(sumstate_company)

#merge state & company
merge_state_company <- merge(sumstate, sumstate_company, by = "State")
View(merge_state_company)

merge_state_company$market_share <- round(merge_state_company$sumstate_company / merge_state_company$sum_state,4)
merge_state_company <- merge_state_company %>% arrange(State, -market_share)
View(merge_state_company)
write.csv(merge_state_company, file = "~/Documents/Brandeis/Healthcare Data Analytics/Assignment2/merge_state_company.csv",  sep = "", row.names = FALSE, col.names = TRUE)

#HHI = (Xi/X)^2
#40%、25%、17%和18%，HHI= 0.4^2 + 0.25^2 + 0.17^2 + 0.18^2= 0.2838
HHI <- merge_state_company %>% group_by(State) %>% summarise(HHI = sum(market_share^2)) %>% arrange(-HHI)
View(HHI)
head(HHI, 4)

#1 WY    0.488
#2 MI    0.320
#3 DE    0.317
#4 NV    0.281



#HW2_Question2
#import excel
PBP_Benefits_2020_dictionary <- read_excel("Documents/Brandeis/Healthcare Data Analytics/Assignment2/PBP-Benefits-2020-Q1/PBP_Benefits_2020_dictionary.xlsx")
View(PBP_Benefits_2020_dictionary)  
#import txt
pbp_b16_dental <- read.delim("~/Documents/Brandeis/Healthcare Data Analytics/Assignment2/pbp_b16_dental.txt")
View(pbp_b16_dental)

#select top-5 company
merge_state_company_top5 <- read.csv("~/Documents/Brandeis/Healthcare Data Analytics/Assignment2/merge_state_company_top5.csv")
View(merge_state_company_top5)

#find useful column
pbp_b16_dental_new <- pbp_b16_dental[,c("pbp_a_hnumber","pbp_a_plan_identifier","segment_id","pbp_b16a_bendesc_yn","pbp_b16b_bendesc_yn")]
View(pbp_b16_dental_new)

pbp_b16_dental_new_row <- pbp_b16_dental_new %>%
  group_by(pbp_a_hnumber,pbp_a_plan_identifier) %>% 
  arrange(pbp_a_hnumber, pbp_a_plan_identifier, segment_id) %>%
  mutate(row = row_number()) %>%
  filter(row <= 1)
View(pbp_b16_dental_new_row)

pbp_b16_dental_new <- pbp_b16_dental_new_row

#merge contract and plan
pbp_b16_dental_new$pbp_a_plan_identifier <- as.factor(pbp_b16_dental_new$pbp_a_plan_identifier)
View(pbp_b16_dental_new)
merge_contract_new$Plan.ID <- as.factor(merge_contract_new$Plan.ID)
View(merge_contract_new)
merge_contract_plan <- merge(merge_contract_new, pbp_b16_dental_new, by.x = c("Contract.Number","Plan.ID"), by.y = c("pbp_a_hnumber","pbp_a_plan_identifier"))
View(merge_contract_plan)

#merge contract_plan with top-5
merge_contract_plan_top5 <- merge(merge_state_company_top5, merge_contract_plan, by = c("State", "MajorInsuranceOrgName"))
View(merge_contract_plan_top5)



#a What percentages of the enrollees enjoy the “Preventive Dental Items as a supplemental benefit under Part C”?
Pre_Dental <- merge_contract_plan_top5 %>% filter(pbp_b16a_bendesc_yn==1) %>% group_by(State,MajorInsuranceOrgName) %>% summarise(sum_pre_dental = sum(Enrollment))
View(Pre_Dental)
Total <- merge_contract_plan_top5 %>% group_by(State,MajorInsuranceOrgName) %>% summarise(sum_total = sum(Enrollment))
View(Total)
merge_pre_dental <- merge(Pre_Dental, Total, by = c("State","MajorInsuranceOrgName"))
merge_pre_dental$percentages_of_preventive_dental <- round(merge_pre_dental$sum_pre_dental/merge_pre_dental$sum_total,4)
merge_pre_dental <- merge_pre_dental %>% arrange(State, -percentages_of_preventive_dental)
View(merge_pre_dental)

#b What percentages of the enrollees enjoy the “Comprehensive Dental Items as a supplemental benefit under Part C”?
Com_Dental <- merge_contract_plan_top5 %>% filter(pbp_b16b_bendesc_yn==1) %>% group_by(State,MajorInsuranceOrgName) %>% summarise(sum_com_dental = sum(Enrollment))
View(Com_Dental)
merge_com_dental <- merge(Com_Dental, Total, by = c("State","MajorInsuranceOrgName"))
merge_com_dental$percentages_of_comprehensive_dental <- round(merge_com_dental$sum_com_dental/merge_com_dental$sum_total,4)
merge_com_dental <- merge_com_dental %>% arrange(State, -percentages_of_comprehensive_dental)
View(merge_com_dental)
