#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 22:44:39 2019

@author: r132505
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 19:00:18 2019

@author: qingyuesu
"""
#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sun Nov  3 20:39:54 2019

@author: hsiangyao
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov  2 17:04:35 2019

@author: yuan
"""

import pandas as pd
import scipy.stats as stats

 
npidata = pd.read_csv("~/Desktop/npidata_pfile_20050523_20191013.csv")
npidata.head()

npi_data = pd.DataFrame(npidata)
print(npi_data)
npi_data.head()

##question 2
#filter the gender and sole proprietor columns
npi_data['Provider Gender Code'].unique()
npi_data['Is Sole Proprietor'].unique()
npisole= npi_data[['Provider Gender Code','Is Sole Proprietor']]
#delete all the nan and other types
npisole = npisole.dropna(axis=0,how='any') 
npisole['Provider Gender Code'].unique()
npisole = npisole[npisole['Is Sole Proprietor']!= 'X']
print(npisole)
npisole.groupby['Provider Gender Code']
a = npisole.groupby(by=['Provider Gender Code','Is Sole Proprietor'])['Is Sole Proprietor'].count()
print(a)
obs = pd.DataFrame([[154,24], [419,87]])
new_col = ['N','Y']
obs.columns=new_col
obs.index=['F','M']

print(obs)
fisher_result = stats.fisher_exact(obs)
print(fisher_result)

##question 3
#hypothesis-part
#filter the gender and provider code columns
npirisk = npi_data[['Provider Gender Code','Healthcare Provider Taxonomy Code_1']]
print(npirisk)
#delete all the nan
npirisk = npirisk.dropna(axis=0,how='any') 
# find the four provider codes
#low risk: Obstetrics & Gynecology -207V00000X, “Pediatrics”-208000000X
#high risk: Surgery - 208600000X   Orthopaedic Surgery - 207X00000X
npiallrisk=npirisk.loc[npirisk['Healthcare Provider Taxonomy Code_1'].isin (['207V00000X', '208000000X', '208600000X ', '207X00000X'])]
npiallrisk = npiallrisk.dropna(axis=0,how='any') 

npif = npiallrisk[npiallrisk['Provider Gender Code'] == 'F']
npif = npif.dropna(axis=0,how='any') 

npim = npiallrisk[npiallrisk['Provider Gender Code'] == 'M']
npim = npim.dropna(axis=0,how='any') 


npiallgender=npirisk.loc[npirisk['Provider Gender Code'].isin (['F', 'M'])]
npiallgender = npiallgender.dropna(axis=0,how='any') 

npigenderlow = npiallgender[npiallgender['Healthcare Provider Taxonomy Code_1'].isin(['207V00000X','208000000X'])]
npigenderlow = npigenderlow.dropna(axis=0,how='any') 

npigenderhigh = npiallgender[npiallgender['Healthcare Provider Taxonomy Code_1'].isin(['208600000X ', '207X00000X'])]
npigenderhigh = npigenderhigh.dropna(axis=0,how='any') 
npirisk = npi_data[['Provider Gender Code','Healthcare Provider Taxonomy Code_1']]
print(npirisk)
npirisk = npirisk.dropna(axis=0,how='any') 
#low:Obstetrics & Gynecology is 207V00000X,“Pediatrics”-208000000X
#high risk: Surgery - 208600000X   Orthopaedic Surgery - 207X00000X'

#testing-part
npilowrisk=npirisk.loc[npirisk['Healthcare Provider Taxonomy Code_1'].isin (['207V00000X', '208000000X'])]
print(npilowrisk)
b = npilowrisk.groupby(by=['Provider Gender Code'])['Healthcare Provider Taxonomy Code_1'].size()
print(b)

npihighrisk=npirisk.loc[npirisk['Healthcare Provider Taxonomy Code_1'].isin (['208600000X ', '207X00000X'])]
print(npihighrisk)
c = npihighrisk.groupby(by=['Provider Gender Code'])['Healthcare Provider Taxonomy Code_1'].size()
print(c)

obs2 = pd.DataFrame([[66868,1688], [42206,22020]])
new_col = ['low','high']
obs2.columns=new_col
obs2.index=['F','M']
print(obs2)

fisher_result = stats.fisher_exact(obs2)
print(fisher_result)





