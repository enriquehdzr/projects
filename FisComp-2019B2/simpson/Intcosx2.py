#!/usr/bin/env python
# -*- coding: utf-8 -*-
# METODO DE SIMPSON                      
from platform import python_version
print ("Python " + python_version())
import sys
import numpy as np

# FUNCION A INTEGRAR
def f(x):
    fxi = np.cos(x)*np.cos(x)
    return(fxi)

# LIMITES 
print("    ")
print("Dame los limites de integracion")
a =  float(input('a: '))
b =  float(input('b: '))
n =  int(input('particiones: '))

# PROCEDIMIENTO
h = (b-a)/float(n)
pares = 0.0
impares = 0.0

for i in range(2,n,2):
    pares = pares + 4.0*f(a+(i-1)*h)

for j in range(3,n,2):
    impares = impares + 2.0*f(a+(j-1)*h)

    integral = h*(f(a) + pares + impares + f(b))/3.0

# RESULTADO DE LA INTEGRAL
print('            ')
print('El valor de la integral es ' + str(integral))
print('            ')
print('Ya termine! ')
print('            ')
