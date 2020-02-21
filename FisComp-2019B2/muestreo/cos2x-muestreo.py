#!/usr/bin/env python
# -*- coding: utf-8 -*-
## INTEGRACON NUMERICA USANDO EL MUESTREO      
from platform import python_version
print ("Python " + python_version())
import sys

import math
import random

# SE DEFINE LA FUNCION      
def f(x):
    return math.cos(x)

# LIMITES DE INTEGRACION                                 
print("    ")
print("Dame los limites de integracion")
xmin =  float(input('a: '))
xmax =  float(input('b: '))
numPoints =  int(input('Numeros a generar: '))

sumy = 0.0
for j in range(numPoints):
    x = xmin + (xmax - xmin) * random.random()
    sumy += f(x)*f(x)

numInt = (xmax - xmin) * sumy / numPoints     

# RESULTADO DE LA INTEGRAL
print('         ')
print('Integral= ' + str(numInt))
print('         ')
