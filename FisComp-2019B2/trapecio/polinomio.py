#!/usr/bin/env python
# -*- coding: utf-8 -*-
# METODO DEL TRAPECIO                    
from platform import python_version
print ("Python " + python_version())
import sys
import math

# FUNCION A INTEGRAR
def f(x):
    fxi = 1.0/(1.0 + x)
    return(fxi)

# LIMITES 
print("    ")
print("Dame los limites de integracion")
a =  float(input('a: '))
b =  float(input('b: '))
n =  int(input('particiones: '))

# PROCEDIMIENTO
h = (b-a)/float(n)
suma = 0.0
x = 0.0
for i in range(0,n-1,1):
    x = x+h
    suma = suma + f(x)
    integral = 0.5*h*(f(a) + 2.0*suma + f(b))

# RESULTADO DE LA INTEGRAL
print('         ')
print('Integral= ' + str(integral))
print('         ')
