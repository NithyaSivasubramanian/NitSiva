# -*- coding: utf-8 -*-
"""
Created on Wed Oct 21 23:05:00 2020

@author: JayNit
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import skew, kurtosis, norm
from scipy import stats
import joblib
import pickle

sales_data = pd.read_excel('C://Users//nithy//Desktop//Nithya 25_10_2020//Data Science//ExcelR//Project 1\\Final Dataset.xlsx')
sales_data = sales_data.head(17976) 
test_data = sales_data.tail(4494)
sales_data.head(10)
test_data.head(10)
sales_data.shape
test_data.shape

sales_data.dtypes
test_data.dtypes

sales_data.isnull()
sales_data.isnull().sum()
test_data.isnull()
test_data.isnull().sum()

sales_data['PROD_CD'] = sales_data['PROD_CD'].str.replace(r'\D', '').astype(int)
sales_data['SLSMAN_CD'] = sales_data['SLSMAN_CD'].str.replace(r'\D', '').astype(int)
test_data['PROD_CD'] = test_data['PROD_CD'].str.replace(r'\D', '').astype(int)
test_data['SLSMAN_CD'] = test_data['SLSMAN_CD'].str.replace(r'\D', '').astype(int)

types_train = sales_data.dtypes

types_train
types_test = sales_data.dtypes
types_test

sales_data.groupby(['PROD_CD','PLAN_MONTH'])['PROD_CD'].count()
sales_data.groupby(['SLSMAN_CD','PLAN_MONTH'])['SLSMAN_CD'].count()
sales_data.groupby(['SLSMAN_CD','PLAN_MONTH','PLAN_YEAR'])['SLSMAN_CD'].count()

from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import chi2
X = sales_data.iloc[:,:6]  #independent columns
y = sales_data.iloc[:,-1]    #target column i.e price range
X
y
#apply SelectKBest class to extract top 10 best features
bestfeatures = SelectKBest(score_func=chi2, k=5)
fit = bestfeatures.fit(X,y)
dfscores = pd.DataFrame(fit.scores_)
dfcolumns = pd.DataFrame(X.columns)

featureScores = pd.concat([dfcolumns,dfscores],axis=1)
featureScores.columns = ['imp','importance']  #naming the dataframe columns

featureScores

data19 = pd.DataFrame(sales_data.loc[(sales_data.PLAN_YEAR==2019)|(sales_data.PLAN_MONTH==10)])

datadrop2=pd.DataFrame(data19.loc[(data19.ACH_IN_EA ==0)&(data19.TARGET_IN_EA ==0)])

data20 = pd.DataFrame(sales_data.loc[(sales_data.PLAN_YEAR==2020)|(sales_data.PLAN_MONTH==1)])

from sklearn.preprocessing import LabelEncoder
enc = LabelEncoder()

data19.drop(["PLAN_YEAR"], inplace = True, axis = 1)

id=data19[(data19['ACH_IN_EA']==0) & (data19['TARGET_IN_EA']==0)].index

data19.drop(id,inplace=True)


from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split,cross_val_score,cross_val_predict

colnames=list(data19.columns)
print(colnames)

predictor=colnames[0:4]
predictor

target=colnames[-1]
target

train2020,test2020 =train_test_split(data19,test_size=0.25,random_state=42)

#train data

train2020_x=train2020[predictor]
train2020_x
train2020_y=train2020[target]
test2020_x=test2020[predictor]
test2020_y=test2020[target]

from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import RandomForestClassifier

rf=RandomForestRegressor()

model2020=rf.fit(train2020_x, train2020_y)

y_pred2020 = model2020.predict(test2020_x)

y_pred12020 = model2020.predict(train2020_x)


y_train2020=pd.DataFrame(train2020_y)
y_test2020=pd.DataFrame(test2020_y)


from sklearn.metrics import r2_score
rf_test_R2_n2020= r2_score(test2020_y, y_pred2020)
rf_train_R2_n2020= r2_score(train2020_y, y_pred12020)

rf_test_R2_n2020
rf_train_R2_n2020 
rf_test_R2_n2020
# evaluating the model for test


# plot from random forest regressor - having y as only target 
#from matplotlib import pyplot

#pyplot.plot(test2020_y, label='Expected')
#pyplot.plot(y_pred2020, label='Predicted')
#pyplot.legend()
#pyplot.show()
# plot from random forest regressor - having y as only target 

#pyplot.plot(train2020_y, label='Expected')
#pyplot.plot(y_pred12020, label='Predicted')
#pyplot.legend()
#pyplot.show()

pickle.dump(rf,open('model.pkl','wb'))
