import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime
import datetime as dt
import plotly.express as px

#read file purchase
path='D:/Brandeis Homework/analyzing big data/dta_at_TC_upc.csv'
purchase=pd.read_csv(path)
purchase.head()

#read file trips
path2='D:/Brandeis Homework/analyzing big data/dta_at_TC.csv'
trips=pd.read_csv(path2)

#combine trips and purchase together
trips_purchase=pd.merge(trips,purchase,on='TC_id',how='inner')

#get the month through table trips_purchase
l=pd.DataFrame(trips_purchase['TC_date'].apply(lambda x:x[5:7]))
l=l.rename(columns={'TC_date':'month'})  #rename columns
l.reset_index(inplace=True)     #create index on table l
l.head()
new_data=pd.concat([l,trips_purchase],axis=1)    #combine table l and trips_purchase

# q2.3.1
t=new_data[["month","quantity_at_TC_prod_id"]].groupby(["month"]).mean()
plt.scatter([0], [1.4689], s=50, color="#800080")
plt.scatter([1], [1.4648], s=50, color="#800080")
plt.scatter([2], [1.4621], s=50, color="#800080")
plt.scatter([3], [1.4648], s=50, color="#800080")
plt.scatter([4], [1.4613], s=50, color="#800080")
plt.scatter([5], [1.4539], s=50, color="#800080")
plt.scatter([6], [1.4667], s=50, color="#800080")
plt.scatter([7], [1.4744], s=50, color="#800080")
plt.scatter([8], [1.4730], s=50, color="#800080")
plt.scatter([9], [1.4715], s=50, color="#800080")
plt.scatter([10], [1.4849], s=50, color="#800080")
plt.scatter([11], [1.4681], s=50, color="#800080")
plt.plot(t)
plt.xlabel("month")
plt.ylabel("average number of items purchased")

plt.show()

#q2.3.2
t2=new_data[["month","TC_id"]].drop_duplicates().groupby(["month"]).size()
t2
plt.scatter([0], [480853], s=50, color="#800080")
plt.scatter([1], [460149], s=50, color="#800080")
plt.scatter([2], [487253], s=50, color="#800080")
plt.scatter([3], [475127], s=50, color="#800080")
plt.scatter([4], [483262], s=50, color="#800080")
plt.scatter([5], [455997], s=50, color="#800080")
plt.scatter([6], [472741], s=50, color="#800080")
plt.scatter([7], [476533], s=50, color="#800080")
plt.scatter([8], [444509], s=50, color="#800080")
plt.scatter([9], [473761], s=50, color="#800080")
plt.scatter([10], [463758], s=50, color="#800080")
plt.scatter([11], [477312], s=50, color="#800080")
plt.plot(t2)
plt.xlabel("month")
plt.ylabel("average number of shopping trips")
plt.show()


#q2.3.3

trips['month']=pd.DataFrame(trips['TC_date'].apply(lambda x:x[5:7]))  #create column month 
two_consecutive_shop=trips[['TC_date','TC_id','month']]    
t_even=two_consecutive_shop[::2]   #get only even rows from two_consecutive_shop
t_odd=two_consecutive_shop[1::2]   #get only odd rows from two_consecutive_shop
t_odd=t_odd.rename(columns={'TC_date':'TC_date2','TC_id':'TC_ID2'})  



t_odd=t_odd.drop('month',1) #drop month from t_odd
t_even.reset_index(inplace=True)  #create index
t_odd.reset_index(inplace=True)  


t_even['index']=t_even['index']+1   #let t_even index +1

t_new=pd.merge(t_even,t_odd,on='index',how='inner')   #combine 2 tables together


#convert to date frame
t_new['TC_date2']= pd.to_datetime(t_new['TC_date2'])
t_new['TC_date']= pd.to_datetime(t_new['TC_date'])

#find difference among dates
t_new['difference']=t_new['TC_date2'].sub(t_new['TC_date'],axis=0)
t_new['difference'] = t_new['difference'].astype(dt.timedelta).map(lambda x: np.nan if pd.isnull(x) else x.days)
t_new['difference']=abs(t_new['difference'])

#t_new=t_new.rename(columns={'month':'month'})
t3=t_new[['difference','month']].groupby(["month"]).mean()

plt.plot(t3)
plt.xlabel('month')
plt.ylabel('average number of days between 2 consecutive trips')
plt.show()

#plot the distribution by state
path3='D:/Brandeis Homework/analyzing big data/subplot/distribution_of_state.csv'
data=pd.read_csv(path3)


state=np.arange(len(data['hh_state']))
plt.bar(state,data['number'])
plt.xticks(state,data['hh_state'])
plt.show()