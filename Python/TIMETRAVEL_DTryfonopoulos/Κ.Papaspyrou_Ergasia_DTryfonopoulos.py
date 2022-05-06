# -*- coding: utf-8 -*-
"""
Created on Sun Feb 28 14:49:35 2021

@author: DTryfonopoulos
"""
#%% Load Data 
import pandas as pd
import datetime as dt
import time
import copy
import numpy as np
import os


# Load Data 
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import os

StockNames=[]
AllStocks=[]
for dirname, _, filenames in os.walk('C:/Users/DTryfonopoulos/Downloads/Data/Stocks'):
    for filename in filenames:
        
        path = os.path.join(dirname, filename)
        
        StockNames.append(filename[:-4])
        
        try:
            #Create DataFrame for each Stock
            print(filename)
            df = pd.read_csv(path, sep=",", header=None)
            new_header = df.iloc[0] #grab the first row for the header
            df = df[1:] #take the data less the header row
            df.columns = new_header
            df[["Open", "High", "Low", "Close", "Volume", "OpenInt"]] = df[["Open", "High", "Low", "Close", "Volume", "OpenInt"]].apply(pd.to_numeric)
            #df = df.apply(pd.to_numeric)
            df["Name"] = filename[:-7]
            df["Name"] = df.Name.str.upper()
            AllStocks.append(df) 
            
        
        except :
            #To avoid empty Stocks 
            pass
        
if len(AllStocks) == len(StockNames):
    print('All Files Transfered')
    
#%% Data Preprocessing 
len(AllStocks)

Stocks = pd.concat(AllStocks, axis=0, ignore_index=True)

#Convert Time to pd TimeFormat
Stocks['Date'] = pd.to_datetime(Stocks['Date'])

SortedStocks = Stocks.sort_values(by='Date')

SortedStocks.info()

#Sort DF by Date & Reset Index
SortedStocks = SortedStocks.sort_values(by='Date')
SortedStocks = SortedStocks.reset_index()

#Remove Stocks with empty Entries for Open & Low Price
SortedStocks = SortedStocks[SortedStocks.Open != 0]
SortedStocks = SortedStocks[SortedStocks.Low != 0]
SortedStocks

#Calculate Normalized Differences for OpenVSHigh & CloseVSLow
SortedStocks['Profit_1'] = (SortedStocks.High.values - SortedStocks.Open.values) / SortedStocks.Open.values #Normalized Profit
SortedStocks['Profit_2'] = (SortedStocks.Close.values - SortedStocks.Low.values) / SortedStocks.Low.values

SortedStocks['Best_Profit'] = np.maximum(SortedStocks.Profit_1.values, SortedStocks.Profit_2.values)

#Remove Stocks with 0 Values for the Best_Profit
SortedStocks = SortedStocks[SortedStocks.Best_Profit != 0]
SortedStocks = SortedStocks.reset_index(drop=True)

#%% Calculate the Best Profit for all Stocks for Each Day 
FinalDataset = SortedStocks.copy()

#For each day find max(Best_Profit) and create a NewDataset
def BestDailyProfit(data):
    max=[]
    for days in data['Date'].unique():
        new=data[data['Date']==days]        #New DF for EachDay
        maxDProf = new.Best_Profit.idxmax() #Index of the max
        new2= FinalDataset.iloc[[maxDProf]] #Locate the max In Orig DataFrame
        max.append(new2)
    
    NewDataset = pd.concat(max, axis=0, ignore_index=True)
    return NewDataset

#Create the Dataset with one Stock(Best_Profit) per day 
FinalDataset =BestDailyProfit(FinalDataset)

len(FinalDataset)
#%%
# findBestProfit(simulate_data, dates_simulate_data)
def BestProfitOrder(dataset, dates):    
    
    trans = 0
    result = ""
    budget = 1
    sellShares = 0
    buyShares = 0
    
    orders = []
    balance = []

    for i in range(0,len(dataset)):
        
        case1 = (dataset.iloc[[i]].Profit_1 >= dataset.iloc[[i]].Profit_2).bool() and (
            dataset.iloc[[i]].Open *1.01 <= budget).bool() #Buy Comission
        case2 = (dataset.iloc[[i]].Profit_1 < dataset.iloc[[i]].Profit_2).bool() and (
            dataset.iloc[[i]].Low *1.01 <= budget).bool() #Buy Comission
        
        if case1:
            
            buyShares = int(float(budget) // float(dataset.iloc[[i]].Open *1.01)) #Buy Comission
            
            #Amount of (Buy-Sell)Stocks Should be Smaller than 10% of the Total Stocks' Volume
            if (buyShares > dataset.iloc[[i]].Volume *0.1).bool(): 
                buyShares = dataset.iloc[[i]].Volume.values[0]
            budget = float(budget) - float(buyShares) * float(dataset.iloc[[i]].Open * 1.01) #Buy Comission
            
            sellShares = buyShares
            budget = float(budget) + float(sellShares) * float(dataset.iloc[[i]].High *0.99) #Sell Comission
            
            result = dates[i] + " buy-open " + str(dataset.iloc[[i]].Name.values[0]) + " " + str(
                buyShares) + '\n' + dates[i] + " sell-high " + str(
                dataset.iloc[[i]].Name.values[0]) + " " + str(sellShares) + '\n'
            
            print('--Case1--\n',result)
            orders.append(result)
            
            trans += 2
            
        elif case2:
            
            buyShares = int(float(budget) // float(dataset.iloc[[i]].Low *1.01)) #Buy Comission
            
            #Amount of (Buy-Sell)Stocks Should be Smaller than 10% of the Total Stocks' Volume
            if (buyShares > dataset.iloc[[i]].Volume).bool():
                buyShares = dataset.iloc[[i]].Volume.values[0]
                
            budget = float(budget) - float(buyShares) * float(dataset.iloc[[i]].Low * 1.01) #Buy Comission
            
            sellShares = buyShares
            budget = float(budget) + float(sellShares) * float(dataset.iloc[[i]].Close*0.99) #Sell Comission
            
            result = dates[i] + " buy-low " + str(dataset.iloc[[i]].Name.values[0]) + " " + str(
                buyShares) + '\n' + dates[i] + " sell-close " + str(
                dataset.iloc[[i]].Name.values[0]) + " " + str(sellShares) + '\n'
            
            print('--Case2--\n',result)
            orders.append(result)
            
            trans += 2
            
        balance.append(budget)

    print("budget: ", str(budget))
    return trans, orders, balance
#%% Small Case 
# 9 Best Profit Stocks of All Time (9 used for i(orders)<1000)

SmallDataset = FinalDataset.copy()

# Add a Year Column
SmallDataset['Year'] = SmallDataset['Date'].dt.year
len(list(SmallDataset['Year'].unique())) #Years

SmallDatasetBest = SmallDataset.sort_values(by='Best_Profit', ascending=False).groupby('Year').head(9)

#Reset Index
SmallDatasetBest = (SmallDatasetBest.sort_values(by='Date')).reset_index()

# Delete Unused Columns
SmallDatasetBest = SmallDatasetBest.drop(['level_0', 'index', 'OpenInt'],axis=1)

# All Unique Dates
SmallDatasetBestDates = SmallDatasetBest['Date'].dt.strftime('%Y-%m-%d').tolist()

transactions_s, orders_s, balance_s= BestProfitOrder(SmallDatasetBest, SmallDatasetBestDates)

print ("Number of trabsactions:",transactions_s)
print (''.join(orders_s))
len(orders_s)
doc1=(''.join(orders_s))
#%% Write In txt File 

#os.chdir('../input/txtfiles')
os.chdir('C:/Users/DTryfonopoulos/Downloads')

with open('doc_01.txt', 'w') as f:
    f.write("Number of trabsactions: %s\n" % transactions_s)
    for item in orders_s:
        f.write("%s" % item)

#%% Plots
import matplotlib.pyplot as plt
from pylab import rcParams
rcParams['figure.figsize'] = 10, 5

plt.subplots_adjust(bottom=0.001, right=0.8, top=1.1)
plt.subplot(2,1,1)
plt.plot(SmallDatasetBest.Year, balance_s) # apotimhsh (= kerdos)
plt.title('Small Case: Profit Evaluation')
plt.xlabel('Years')
plt.ylabel('Balance')

plt.subplot(2,1,2)
plt.bar(SmallDatasetBest.Year, balance_s) # kerdos
plt.xlabel('Years')
plt.ylabel('Balance')

#%% Large Case 
# All Unique Dates
FinalDataset_data = FinalDataset['Date'].dt.strftime('%Y-%m-%d').tolist() # All Dates in List

FinalDataset['Year'] = FinalDataset['Date'].dt.year

transactions, orders, balance = BestProfitOrder(FinalDataset, FinalDataset_data)

print ('Number Of Transactions:', transactions)
print (''.join(orders))

len(orders)
doc2=(''.join(orders))


#%% Write In txt file 

os.chdir('../input/txtfiles')

with open('doc_02.txt', 'w') as f:
    f.write("Number of trabsactions: %s\n" % transactions)
    for item in orders:
        f.write("%s" % item)

#%% Plots 

import matplotlib.pyplot as plt
from pylab import rcParams
rcParams['figure.figsize'] = 10, 5

plt.subplots_adjust(bottom=0.001, right=0.8, top=1.1)
plt.subplot(2,1,1)
plt.plot(FinalDataset.Year, balance) # apotimhsh (= kerdos)
plt.title('Big Case: Profit Evaluation')
plt.xlabel('Years')
plt.ylabel('Balance')

plt.subplot(2,1,2)
plt.bar(FinalDataset.Year, balance) # kerdos
plt.xlabel('Years')
plt.ylabel('Balance')

