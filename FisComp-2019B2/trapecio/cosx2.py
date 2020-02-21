#!/usr/bin/env python
# -*- coding: utf-8 -*-
# TRAPEZOIDAL INTEGRATION METHOD                 
from platform import python_version
print ("Python " + python_version())
import sys
import numpy as np

# FUNCTION TO BE INTEGRATED
def f(x):
    fxi = np.cos(x)*np.cos(x)
    return(fxi)

# LIMITES 
print("    ")
print("Dame los limites de integracion")
a =  float(input('a: '))
b =  float(input('b: '))
n =  int(input('particiones: '))

# PROCEDURE
h = (b-a)/float(n)
suma = 0.0
x = 0.0
for i in range(0,n-1,1):
    x = x+h
    suma = suma + f(x)
    integral = 0.5*h*(f(a) + 2.0*suma + f(b))

# PLOT RESULT
print('         ')
print('Integral= ' + str(integral))
print('         ')
