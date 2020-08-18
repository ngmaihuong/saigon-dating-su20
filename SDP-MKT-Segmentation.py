#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 18 15:38:46 2020

@author: QuynhMai
"""

#Change working directory
import os
os.chdir('/Users/Boo Boo/Downloads/SGD/Data Analytics/MKT Analytics/Final Data')

#Import libraries
import pandas as pd
import numpy as np

#Import data
df = pd.read_csv('Persona-Ced.csv')
df = df.dropna()

#Create empty data frame
newnames = ['1.1', '1.2', '1.3', '1.4',
            '2.1', '2.2', '2.3', '2.4',
            '3.1', '3.2', '3.3', '3.4', '3.5',
            '4.1', '4.2', '4.3', '4.4',
            '5.1', '5.2', '5.3', '5.4', '5.5']

df1 = pd.DataFrame(index=list(range(len(df))), columns=newnames)
df1 = df1.fillna(0)

#dataframe[start:stop:step]
#dataframe.iloc[row_index:column_index]

#Binary coding for Question 1
for row in range(len(df)):
    for val in range(1, 5):
        if df.iloc[row, 0] == val:
            df1.iloc[row,val-1] = 1

#Binary coding for Question 2
for row in range(len(df)):
    for val in range(1, 5):
        if df.iloc[row,1] == val:
            df1.iloc[row, val+3] = 1
            
#Binary coding for Question 3
for row in range(len(df)):
    for val in range(1, 6):
        if df.iloc[row,2] == val:
            df1.iloc[row, val+7] = 1
            
#Binary coding for Question 4
for row in range(len(df)):
    for val in range(1, 5):
        if df.iloc[row,3] == val:
            df1.iloc[row, val+12] = 1
            
#Binary coding for Question 5
for row in range(len(df)):
    for val in range(1, 6):
        if df.iloc[row,4] == val:
            df1.iloc[row, val+16] = 1
            
#Export data to Excel file
df1.to_excel('SDP-binarycoded.xlsx', index = False)
