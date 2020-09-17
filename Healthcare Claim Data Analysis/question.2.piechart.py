#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 19 20:44:24 2019

@author: xupech
"""

import pandas as pd
import plotly
import plotly.graph_objects as go

inpatient = pd.read_csv(r'/Users/xupech/Desktop/brandeis graduate school/Academics/2019 FALL/HS 256 Healthcare analytics/Third Assignment/inpatient.csv', low_memory = False)
print(inpatient.columns.values)

commercial_sum=inpatient['COMMERCIAL PAYERS'].sum()
medicaid_sum=inpatient['MEDICAID'].sum()
medicare_sum=inpatient['MEDICARE'].sum()

inpatient['com_percent']= round(inpatient['COMMERCIAL PAYERS']/commercial_sum,3)

inpatient['caid_percent']=round(inpatient['MEDICAID']/medicaid_sum, 3)
inpatient['care_percent']=round(inpatient['MEDICARE']/medicare_sum, 3)


com_pie=pd.DataFrame(inpatient.loc[inpatient['com_percent']!= 0])
fig = go.Figure(data=[go.Pie(labels=com_pie['MDC_CAT_NAME'], values=com_pie['com_percent'])])
fig.update_layout(
    title_text="Commercial player MDC Spending Percentage")
fig.show()
plotly.offline.plot(fig, filename='Commercial player MDC Spending Percentage.html')

caid_pie=pd.DataFrame(inpatient.loc[inpatient['caid_percent']!= 0])
fig = go.Figure(data=[go.Pie(labels=com_pie['MDC_CAT_NAME'], values=com_pie['caid_percent'])])
fig.update_layout(
    title_text="Medicaid MDC Spending Percentage")
fig.show()
plotly.offline.plot(fig, filename='Medicaid MDC Spending Percentage.html')

care_pie=pd.DataFrame(inpatient.loc[inpatient['care_percent']!= 0])
fig = go.Figure(data=[go.Pie(labels=com_pie['MDC_CAT_NAME'], values=com_pie['care_percent'])])
fig.update_layout(
    title_text="Medicare MDC Spending Percentage")
fig.show()
plotly.offline.plot(fig, filename='Medicare MDC Spending Percentage.html')