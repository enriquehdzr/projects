# THIS IS A TREE DECISION EXAMPLE FOR MACHINE LEARNING. THE IDEA IS TO REDICT IF WE MUST OR NOT BOY AN ESPECIFIC CAR

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


data = pd.read_csv('car.csv',header=None)

data.columns=['Price','Maintenance','Doors', 'Capacity','Lug_size','Safety','Decision']

#first lets take a look at the info by doing a little test

decision=data['Decision'].value_counts()
data['Decision'].value_counts().sort_index(ascending=True)

decision.plot(kind='bar')
data['Decision'].unique()
#data['Decision'].replace(('unacc','acc','vgood','good'),(4,3,2,1),inplace=True)
data['Decision'].unique()

colors=['#ddee01','#cc0101','#FE10D1','#BCC111']
decision.plot(kind='bar',color=colors)
plt.xlabel('Decison')
plt.ylabel('Autos')
plt.title('Decision final')


#Here we are ploting the safety qualification  


data['Safety'].unique()
data['Safety'].value_counts()

labels=['low','medium','high']
size=[576,576,576]
colors2=['cyan','red','yellow']
explode=[0,0,0.05]

plt.pie(size,labels=labels,colors=colors2,explode=explode,shadow=False,autopct='%.2f%%')
plt.title('Seguridad')
plt.axis('off')
plt.legend(loc='best')
plt.show()

#we can see that te most safety cars are exactly a third part of the total



############################################
#### Now lets start with a decision tree ###
############################################

# First lets convert the values to numeric
data.Price.replace(('vhigh','high','med','low'),(4,3,2,1),inplace=True)
data.Maintenance.replace(('vhigh','high','med','low'),(4,3,2,1),inplace=True)
data.Doors.replace(('2','3','4','5more'),(1,2,3,4),inplace=True)
data.Capacity.replace(('2','4','more'),(1,2,3),inplace=True)
data.Lug_size.replace(('small','med','big'),(1,2,3),inplace=True)
data.Safety.replace(('low','med','high'),(1,2,3),inplace=True)

data['Decision'].replace(('unacc','acc','good','vgood'),(1,2,3,4),inplace=True)

data.head(5)

dataset=data.values
X=dataset[:,0:6] #select the data wich is going to learn with
Y=np.asarray(dataset[:,6],dtype='S6')

# WE import the tree from sklearn
from sklearn import tree
from sklearn.model.selection import train_test_split,cross_val_score 
from sklearn import metrics


X_Train, X_Test, Y_Train, Y_Test= train_test_split(X,Y,test_size=0.2,random_sate=0)

tr= tree.DesicionTreeClassifier(max_depth=10) # We ask to get a 10 level depth tree

#start training the tree
tr.fit(X_Train,Y_Train)

#let's check if it is learning asking for a prediction 

y_pred=tr.predict(X_Test)
y_pred

score=tr.score(X_Test,Y_Test)
print('Precision: %0.4f'%(score))   ### with this line we are checking the precision of our program.
# we can see that our precision is 96%
