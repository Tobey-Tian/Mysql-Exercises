#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Oct 26 10:40:51 2019

@author: yuantian
"""

import pandas as pd
from pandas import to_datetime
from pandas import Series,DataFrame
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

## Import dataset 
purchase = pd.read_csv("Desktop/purchase.csv")
product = pd.read_csv("Desktop/product.csv")
trip = pd.read_csv("Desktop/trip.csv")
household = pd.read_csv("Desktop/household.csv")

##c.3.i

table_private=product.loc[product["brand_at_prod_id"]=="CTL BR",]
p=pd.DataFrame(table_private[["brand_at_prod_id","group_at_prod_id"]].groupby("group_at_prod_id").size())
p=p.rename(columns={0:'number'})
p.reset_index(inplace=True)
p.head()
#plot product categories that have proven to be more “Private labelled” 
plt.figure(figsize = (20, 10))
sns.barplot(x = "group_at_prod_id", y = 'number',  data = p)
plt.xticks(rotation=90)
plt.xlabel('category')
plt.title("Private Labelled categories")
plt.show()
#Top 20 of categories
p1 = p.sort_values(by="number", ascending = False)
p2 = p1.head(20)
p2.rename(columns={'group_at_prod_id': 'category'}, inplace=True)
print(p2)
plt.figure(figsize = (20, 10))
sns.barplot(x = "category", y = 'number',  data = p2)
plt.xticks(rotation=90)
plt.xlabel('category')
plt.title("Top 20 Private Labelled categories")
plt.show()

##c.3.ii  Is the expenditure share in Private Labeled products constant across months? 

#change tc_date to month
trip['TC_date']=to_datetime(trip.TC_date,format="%Y/%m/%d")
trip['TC_date']=trip.TC_date.dt.month
trip['TC_date']
#merge table product,purchase,trip
productpurchase = pd.merge(product,purchase,on='prod_id')
df = pd.merge(productpurchase,trip,on='TC_id')
a = df['brand_at_prod_id'].unique()
df.head()
#calculate the expenditure of all product across month
total_expenditure = df.groupby(by=['TC_date'])['total_price_paid_at_TC_prod_id'].sum()
total_expenditure  = total_expenditure .to_frame()
print(total_expenditure)
#calculate the expenditure of private product across month
df_private = df[df.brand_at_prod_id == 'CTL BR']
private_expenditure = df_private.groupby(by=['TC_date'])['total_price_paid_at_TC_prod_id'].sum()
private_expenditure  = private_expenditure .to_frame()
print(private_expenditure)
#calculate the expenditure share
share = private_expenditure['total_price_paid_at_TC_prod_id']/total_expenditure['total_price_paid_at_TC_prod_id']
share  = share.to_frame()
share.reset_index(inplace=True)
share.rename(columns={'TC_date': 'month','total_price_paid_at_TC_prod_id':'private_share'}, inplace=True)
print(share)
#plot the share
fig,ax = plt.subplots()
sns.scatterplot(share["month"],share["private_share"])  
sns.lineplot(share["month"],share["private_share"])  
plt.title("Is the expenditure share in Private Labeled products constant across months? ")
plt.show()
sns.barplot(share["month"],share["private_share"]) 

##c.3.iii  Cluster households in three income groups, Low, Medium and High. 
#Report the average monthly expenditure on grocery. 

##Study the % of private label share in their monthly expenditures. 

#divide household into three groups
def group(income):
    if income >= 3 and income <= 16:
        return 'low'
    elif income >= 17 and income <= 26:
        return'middle'
    else:
        return'high'
household['income_group']=household.hh_income.apply(group)

#merge the tables
df1 = pd.merge(household,df,on='hh_id')
print(df1)
##Report the average monthly expenditure on grocery
df2 = df1.groupby(by=['income_group','TC_date'])['total_price_paid_at_TC_prod_id'].mean()
df2 = df2.to_frame()
print(df2)
df3 = df2.groupby(by=['income_group'])['total_price_paid_at_TC_prod_id'].mean()
df3 = df3.to_frame()
df3.rename(columns={'income_group': 'income_group','total_price_paid_at_TC_prod_id':'average_expenditure'}, inplace=True)
df3.sort_values(by="average_expenditure", ascending = True)
df3.reset_index(inplace=True)
print(df3)
#make visualization
plt.figure(figsize = (6, 4))
sns.barplot(x = "income_group", y = 'average_expenditure',  data = df3,color='skyblue')
plt.title("The average monthly expenditure of income groups")

##Study the % of private label share in their monthly expenditures. 

#income_group expenditure for private products
df1_private = df1[df1.brand_at_prod_id == 'CTL BR']
groupprivate_expenditure = df1_private.groupby(by=['TC_date','income_group'])['total_price_paid_at_TC_prod_id'].sum()
groupprivate_expenditure  = groupprivate_expenditure .to_frame()
print(groupprivate_expenditure)

#income_group expenditure for all products
grouptotal_expenditure = df1.groupby(by=['TC_date','income_group'])['total_price_paid_at_TC_prod_id'].sum()
grouptotal_expenditure  = grouptotal_expenditure .to_frame()
print(grouptotal_expenditure)

#private share for income groups
share_group = groupprivate_expenditure['total_price_paid_at_TC_prod_id']/grouptotal_expenditure['total_price_paid_at_TC_prod_id']
share_group = share_group.to_frame()
share_group.reset_index(inplace=True)
share_group.rename(columns={'TC_date': 'month','private share':'private_share'}, inplace=True)
print(share_group)
share_group.sort_values(by="month", ascending = True)
share_group.reset_index(inplace=True)
a = share_group[share_group['income_group']=='high']
b = share_group[share_group['income_group']=='middle']
c = share_group[share_group['income_group']=='low']

##plot the % of private label share in their monthly expenditures. 

plt.plot(a["month"], a['private_share'],label = 'high')
plt.plot(b["month"], b['private_share'],label = 'middle')
plt.plot(c["month"], c['private_share'],label = 'low')
plt.legend(loc='upper right')
plt.ylabel('% of private label share')
plt.xlabel("month")
plt.title("the % of private label share in monthly expenditures among different income groups")
plt.ylabel('% of private label share')
plt.xlabel("month")
plt.show()


#highgroup 
plt.figure(figsize = (6, 4))
sns.lineplot(a["month"], a['private_share'])
sns.scatterplot(a["month"], a['private_share'])
plt.title("the % of private label share in high group monthly expenditures")
#middle group
sns.lineplot(b["month"], b['private_share'])
sns.scatterplot(b["month"], b['private_share'])
plt.title("the % of private label share in middle group monthly expenditures")
#low group
sns.lineplot(c["month"], c['private_share'])
sns.scatterplot(c["month"], c['private_share'])
plt.title("the % of private label share in low group monthly expenditures")


