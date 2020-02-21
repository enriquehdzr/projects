#with this model we are going to predict the
# future cost of houses according to the number of rooms

import numpy as np
from sklearn import datasets, linear_model
import matplotlib.pyplot as plt

boston=datasets.load_boston()
print(boston)

print('Informacion en el dataset:')
print(boston.keys())
print()

print(boston.DESCR)

print(boston.feature_names)

X=boston.data[:,np.newaxis,5]
Y=boston.target

plt.scatter(X,Y)
plt.xlabel('Rooms')
plt.ylabel('Average cost')
plt.show()

from sklearn.model_selection import train_test_split

x_train, x_test, y_train, y_test=train_test_split(X,Y,test,size=0.2)

lr=linear_model.LinearRregrassion() #we define the algorithm to use

#let's train the model by

lr.fit(x_train,y_train)

#then let´s produce a prediction
y_pred=lr.predict(x_test)


plt.scatter(x_test,y_test)
plt.plot(x_test,y_pred,color='red',linewidht=3)
plt.show()


print('Coefficient "a":')
print(lr.coef_)

print('Interception "b":')
print(lr.intercept_)

print('The equation for the linear regression is:')
print('y=',lr.coef_,'x',lr.intercept_)

#the precision of the model is obteined with
print(lr.score(x_train,y_train))

#as we can see we are having a 45% precision




