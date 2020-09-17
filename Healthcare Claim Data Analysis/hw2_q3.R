install.packages("readxl")
library('readxl')
library(data.table)
library(dplyr)
# read the excel which get from homework question1 ,it shows the insurance company,each contract and enrollment
#number of our group assigned state
setwd("C://Grad//Healthcare")
contract <- read_excel('contract.xlsx')
#read the excel extract from "MA HEDIS Public Use Files 2018",we only need sheet EOC 170, and we use the column
#'EOC170-0010',we then delete all the NAs in the file and read it
uod <- read_excel('uod.xlsx')
contract = data.table(contract)
uod = data.table(uod)
colnames(uod)[2] = 'uod_rate'
#mearge the two table together to get the uod of each contract, then we change the data type
uodstate=merge(contract,uod,by.x='Contract.Number',by.y='Contract Number')
uodstate$Enrollment = as.numeric(uodstate$Enrollment)
uodstate$uod_rate = as.numeric(uodstate$uod_rate)

glimpse(uodstate)
#in the following steps we filter the table by our states one by one and get the weighted calculated uod rate for each contract,
# then we group them by the top 10 company, order from highest udo rate to lowest
# but in fact, some state doesn't have 10 companies, or some top 10 companies do not have uod rate, so we just include all the data
# we can get, so some state may only have company numbers less than 10.

#NY
uodny = uodstate[State=='NY']
uodny[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodny1=uodny[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, 
             by=MajorInsuranceOrgName]
#combine with company market share to get the top 10 companies in NY before ordering by UOD rate.
company_share = fread("merge_state_company.csv")
order_company = merge(x = uodny1, y = company_share, by = "MajorInsuranceOrgName", all.x = TRUE)
order_ny = order_company[State =='NY'][order(market_share, decreasing = TRUE)][1:10]
uodny2 = order_ny[order(uod_company,decreasing=TRUE)]
fwrite(uodny2, "UOD of NY states.csv")

#MI
uodmi = uodstate[State=='MI']
uodmi[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodmi1=uodmi[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, 
             by=MajorInsuranceOrgName]
uodmi2 = uodmi1[order(uod_company,decreasing=TRUE)][1:10]
fwrite(uodmi2, "UOD of MI states(1).csv")


#TN
uodtn = uodstate[State=='TN']
uodtn[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodtn1=uodtn[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, by=MajorInsuranceOrgName]
uodtn2 = uodtn1[order(uod_company,decreasing=TRUE)][1:10]
fwrite(uodtn2, "UOD of TN states.csv")


#MN
uodmn = uodstate[State=='MN']
uodmn[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodmn1=uodmn[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, by=MajorInsuranceOrgName]
uodmn2 = uodmn1[order(uod_company,decreasing=TRUE)][1:10]
fwrite(uodmn2, "UOD of MN states.csv")


#OK
uodok = uodstate[State=='OK']
uodok[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodok1=uodok[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, by=MajorInsuranceOrgName]
uodok2 = uodok1[order(uod_company,decreasing=TRUE)]
fwrite(uodok2, "UOD of OK states.csv")


#NV
uodnv = uodstate[State=='NV']
uodnv[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodnv1=uodnv[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, by=MajorInsuranceOrgName]
uodnv2 = uodnv1[order(uod_company,decreasing=TRUE)]
fwrite(uodnv2, "UOD of NV states.csv")


#ID
uodid = uodstate[State=='ID']
uodid[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodid1=uodid[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, by=MajorInsuranceOrgName]
uodid2 = uodid1[order(uod_company,decreasing=TRUE)]
fwrite(uodid2, "UOD of ID states.csv")


#DE
uodde = uodstate[State=='DE']
uodde[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodde1=uodde[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, by=MajorInsuranceOrgName]
uodde2 = uodde1[order(uod_company,decreasing=TRUE)]
fwrite(uodde2, "UOD of DE states.csv")



#WY
uodwy = uodstate[State=='WY']
uodwy[,totalenrollment:=sum(Enrollment),by=Contract.Number ]
uodwy1=uodwy[,{tmp1=Enrollment*uod_rate; tmp2=tmp1/totalenrollment; tmp3=sum(tmp2); list(uod_company=tmp3)}, by=MajorInsuranceOrgName]
uodwy2 = uodwy1[order(uod_company,decreasing=TRUE)]
fwrite(uodwy2, "UOD of WY states.csv")
