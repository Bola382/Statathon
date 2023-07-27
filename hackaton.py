# -*- coding: utf-8 -*-
"""Hackaton.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1qa_q-xhLsljuDckpRhcK4g93o-w4FMWi

---
#Código do pre processamento + modelagem

##Pacotes a serem utilizados
"""

!pip install pycaret

# instalando biblioteca de visualização
!pip install sweetviz

import sweetviz as sv
import missingno as missing
import numpy as np                                    # Numpy
import pandas as pd
import tensorflow as tf
import matplotlib.pyplot as plt
import time
import warnings
import pickle
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder
from numpy import mean
from numpy import std
from sklearn.datasets import make_classification
from lightgbm import LGBMClassifier
from sklearn.model_selection import cross_val_score, train_test_split,RepeatedStratifiedKFold,KFold,StratifiedKFold,cross_validate,RandomizedSearchCV,GridSearchCV
from matplotlib import pyplot
from sklearn.metrics import log_loss,f1_score, recall_score, precision_score, accuracy_score, auc
from sklearn import metrics
warnings.filterwarnings('ignore')

"""---
##Importação da base de dados


"""

data = pd.read_excel("/content/Tomate.xlsx")

data

df_acumulado = data.groupby(['Tratamento', 'Bloco', 'colheita','Estação cultivo'])['Peso frutos parcela'].sum().reset_index()

df_acumulado

data['Peso Acumulado'] = data.groupby(['Tratamento', 'Bloco', 'colheita','Estação cultivo'])['Peso frutos parcela'].cumsum()

data

data = data.drop_duplicates(subset=['Tratamento', 'Bloco', 'Estação cultivo'], keep='last')

"""---
##Visualização dos dados
"""

eda = sv.analyze(source = data,target_feat = 'nº médio frutos',feat_cfg=sv.FeatureConfig(force_num=['nº médio frutos']))
eda.show_notebook()

eda = sv.analyze(source = data,target_feat = 'Tratamento')
eda.show_notebook()

"""#Gráficos"""

data['Comprimento médio frutos (cm)'].plot(kind = 'box');

data['Largura média frutos(cm)'].plot(kind = 'box');

data['Peso frutos parcela'].plot(kind = 'box');

data['nº médio frutos'].plot(kind = 'box')

"""#Dummyficação de variáveis"""

data_novo = data.drop(["Comprimento médio frutos (cm)", "nº médio frutos","Largura média frutos(cm)", "Peso frutos parcela","colheita"],axis=1)

data_novo

variaveis_cat = ['Estação cultivo','Tratamento',"Bloco"]

data_new = pd.get_dummies(data_novo, columns = variaveis_cat, drop_first = True)


data_new.head()

"""##Normalização/padronização de variáveis continuas"""

data_new['Peso Acumulado']=(data_new['Peso Acumulado']-mean(data_new['Peso Acumulado']))/std(data_new['Peso Acumulado'])

data_new.head()

"""#Modelo"""

train, test = train_test_split(data_new, test_size=0.25,
                                     random_state=42)

train

from pycaret.regression import *

exp_clf101 = setup(data = train,           # Banco de dados
              target = 'Peso Acumulado',   # Desfecho que estamos tentando prever
              train_size = 0.9, session_id=123)     # Proporção do banco de treino

mods1=compare_models(fold=10,round=4)

"""#Predições"""

a=predict_model(mods1, data=test)

from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

mae = mean_absolute_error(a['Peso Acumulado'], a['prediction_label'])
mse = mean_squared_error(a['Peso Acumulado'], a['prediction_label'])
rmse = np.sqrt(mse)
r2 = r2_score(a['Peso Acumulado'], a['prediction_label'])

print("Mean Absolute Error (MAE):", mae)
print("Mean Squared Error (MSE):", mse)
print("Root Mean Squared Error (RMSE):", rmse)
print("R-squared (R2):", r2)

"""#seleção de variáveis

"""

from sklearn.ensemble import RandomForestRegressor

data_new

X = data_new.drop('Peso Acumulado', axis=1)  # Variáveis preditoras (removendo a coluna "target")
y = data_new['Peso Acumulado']

model = RandomForestRegressor(n_estimators=100, random_state=42)

model.fit(X, y)

importances = model.feature_importances_

feature_importances = pd.Series(importances, index=X.columns)
feature_importances = feature_importances.sort_values(ascending=False)
print(feature_importances)

data