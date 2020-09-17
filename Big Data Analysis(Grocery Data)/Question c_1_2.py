#!/usr/bin/env python
# coding: utf-8

# In[2]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pandas as pd
np.set_printoptions(suppress=True)


# In[3]:


#import table purchase
path=r"D:\final_project\purchases.csv"
purchases=pd.read_csv(path)


# In[4]:


#clean envirenment
import gc
gc.collect()


# In[5]:


purchases.head()


# In[6]:


purchases.shape[0]


# In[7]:


purchases.isnull().sum()


# In[8]:


#import trips table
path=r"D:\final_project\trips.csv"
trips=pd.read_csv(path)


# In[9]:


trips.head()


# In[10]:


trips.shape[0]


# In[11]:


trips.isnull().sum()


# In[12]:


#clean envirenment
import gc
gc.collect()


# In[13]:


trips=trips.drop(["Unnamed: 0"],axis=1)


# In[14]:


trips.isnull().sum()


# In[15]:


#merge trips and purchases
trips_purchases=trips.merge(purchases,on="TC_id")


# In[16]:


#clean envirenment
import gc
gc.collect()


# In[17]:


trips_purchases.head()


# In[18]:


#select month
l=pd.DataFrame(trips_purchases['TC_date'].apply(lambda x:x[5:7]))
l=l.rename(columns={'TC_date':'month'})
l.reset_index(inplace=True)
l.head()


# In[19]:


#add one column month
new_data=pd.concat([l,trips_purchases],axis=1)
new_data.head()


# In[20]:


#calculate monthly the average number of items purchased
t=new_data[["month","quantity_at_TC_prod_id"]].groupby(["month"]).mean()
t


# In[21]:


#clean envirenment
import gc
gc.collect()


# In[22]:


#plot monthly the average number of items purchased
plt.plot(t)
plt.plot(t,"bo")
plt.xlabel("month")
plt.ylabel("average number of items purchased")


# In[23]:


#deal with table trips,add one column month
s=pd.DataFrame(trips['TC_date'].apply(lambda x:x[5:7]))
s=s.rename(columns={'TC_date':'month'})
s.reset_index(inplace=True)
trips=pd.concat([s,trips_purchases],axis=1)
trips.head()


# In[42]:


#clean envirenment
import gc
gc.collect()


# In[25]:


#calculate the number of shopping trips per month
t2=trips[["month","TC_id"]].groupby(["month"]).size()
t2


# In[26]:


#change t2 as dataframe
t2=pd.DataFrame(t2)


# In[28]:


#merge t and t2 together
t_merge=pd.concat([t,t2],axis=1)
t_merge.reset_index(inplace=True)
t_merge=t_merge.rename(columns={0:'number_of_shopping_trips'})


# In[29]:


t_merge


# In[30]:


#plot the number of shopping trips per month
plt.plot(t2)
plt.plot(t2,"bo")
plt.xlabel("month")
plt.ylabel("the number of shopping trips per month")


# In[31]:


#plot the correlation plot
import seaborn as sns
ax = sns.scatterplot(x="quantity_at_TC_prod_id", y="number_of_shopping_trips",legend="full",data=t_merge)
plt.title("Is the number of shopping trips per month correlated with the average number of items purchased?")


# In[40]:


#combine two plots together
fig, ax1 = plt.subplots()
ln1=plt.plot(t,'b-', lw=1.5,label='items')
plt.grid()
plt.ylabel('average number of items purchased')
plt.xlabel("month")
ax2 = ax1.twinx()
ln2=plt.plot(t2,'r--',lw=1.5,label='trips')
plt.ylabel('the number of shopping trips per month')
plt.title("Is the number of shopping trips per month correlated with the average number of items purchased?")
lns=ln1+ln2
plt.legend(lns,['items','trips'])


# From this plot, we can find that the number of shopping trips per month is partly correlated with the average number of items. In the month during Junaray and February and May to November, the trend of two lines is the same.

# In[41]:


#calculate the average price paid per item
average_price=purchases[["prod_id","total_price_paid_at_TC_prod_id"]].groupby(["prod_id"]).mean()
average_price.reset_index(inplace=True)
average_price.head()


# In[43]:


#calculate the number of items purchased
items_purchased=pd.DataFrame(purchases[["prod_id","quantity_at_TC_prod_id"]].groupby(["prod_id"]).size())
items_purchased.reset_index(inplace=True)
items_purchased.head()


# In[44]:


#merge two results together
items_purchased=average_price.merge(items_purchased)
items_purchased=items_purchased.rename(columns={0:"number_of_item_perchase"})
items_purchased.head()


# In[45]:


#plot total_price_paid_at_TC_prod_id
items_purchased[["total_price_paid_at_TC_prod_id"]].plot()
plt.ylabel('total_price_paid_at_TC_prod_id')
plt.xlabel("prod_id")


# In[46]:


#plot number_of_item_perchase
items_purchased[["number_of_item_perchase"]].plot()
plt.ylabel('number_of_item_perchase')
plt.xlabel("prod_id")


# In[47]:


#combine two plots together
fig, ax1 = plt.subplots()
plt.plot(items_purchased[["total_price_paid_at_TC_prod_id"]],'b-', lw=1.5,label='Winners')
plt.grid()
plt.ylabel('total_price_paid_at_TC_prod_id')
ax2 = ax1.twinx()
plt.plot(items_purchased[["number_of_item_perchase"]],'r--',lw=1.5,label='Losers')
plt.ylabel('number_of_item_perchase')
plt.title("Is the average price paid per item correlated with the number of items purchased?")


# In[52]:


plt.subplot(211)
plt.plot(items_purchased[["total_price_paid_at_TC_prod_id"]],'b-', lw=1.5,label='Winners')
plt.grid()
plt.ylabel('total_price_paid')
plt.title('Is the average price paid per item correlated with the number of items purchased?')
# bottom panel
plt.subplot(212)
plt.plot(items_purchased[["number_of_item_perchase"]],'r--',lw=1.5,label='Losers')
plt.grid()
plt.xlabel('prod_id')
plt.ylabel('number_of_item_perchase')


# In[48]:


#plot the correlation plot
import seaborn as sns
ax = sns.scatterplot(x="total_price_paid_at_TC_prod_id", y="number_of_item_perchase",legend="full",data=items_purchased)
plt.title("Is the average price paid per item correlated with the number of items purchased?")


# In[49]:


#plot the heatmap plot
import seaborn as sns
sns.heatmap(items_purchased[["total_price_paid_at_TC_prod_id","number_of_item_perchase"]].corr())


# From all these plots above, we can konw that the average price paid per item is not correlated with the number of items purchased.
