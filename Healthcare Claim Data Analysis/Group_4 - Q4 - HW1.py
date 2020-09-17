#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 11:25:42 2019

@author: xupech
"""
import pandas as pd
import plotly
import plotly.express as px
import plotly.graph_objects as go

npidata = pd.read_csv(r'/Users/xupech/Desktop/brandeis graduate school/Academics/2019 FALL/HS 256 Healthcare analytics/First Assignment/npidata.csv', low_memory = False)
entity2=pd.DataFrame(npidata.loc[npidata['Entity Type Code']==2])
mri=pd.DataFrame(entity2.loc[entity2['Healthcare Provider Taxonomy Code_1']=='261QM1200X'])
mri1=pd.DataFrame(mri.loc[mri['Provider Business Practice Location Address State Name']!='GU'])
mri1=pd.DataFrame(mri1.loc[mri1['Provider Business Practice Location Address State Name']!='PR'])

mri1.rename(columns={'Provider Business Practice Location Address State Name':'state'}, inplace=True)
mridensity=mri1.groupby('state')['Healthcare Provider Taxonomy Code_1']\
.count().reset_index(name='count')\
.sort_values(['count'], ascending=False)

population=[21299325,28701845,39557045,12741080,8908520,9995915,19542209,11689442,6902149,5611179,4659978,12807060,10519475,6042718,6691878,3943079,7171646,4887871,5695564,6126452,3013825,10383620,6770010,8517685,5813568,7535591,4468402,3156145,3572665,1356458,4190713,5084127,2986530,2095428,2911505,1929268,1754208,1338404,3034392,577737,967171,1062305,760077,3161105,737438,1057315,702455,1420491,882235,626299,1805832]
mridensity['population']=population
mridensity['mridensity']=mridensity['count']/mridensity['population']*1000000
mridensity['mridensity']=round(mridensity['mridensity'],2)

fig = go.Figure(data=go.Choropleth(
    locations=mridensity['state'], # Spatial coordinates
    z = mridensity['mridensity'].astype(float), # Data to be color-coded
    locationmode = 'USA-states', # set of locations match entries in `locations`
    colorscale = 'Reds',
    colorbar_title = "Mri density per 1M people per state",
))

fig.update_layout(
    title_text = 'Mri density per 1M people per state',
    geo_scope='usa', # limite map scope to USA
)

fig.show()
plotly.offline.plot(fig, filename='Mri density per 1M people per state.html')