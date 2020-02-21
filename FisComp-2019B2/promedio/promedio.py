# Get the average of data contained in datos.dat
#!/usr/bin/env python
# -*- coding: utf-8 -*-
from platform import python_version
print ("Python " + python_version())

import sys
import math 

suma = 0
sumad = 0
promedio = 0
archivo = open("datos.dat","r")
lista = archivo.readlines()
print"   "
print"Leyo datos del archivo datos.dat" 
c = 0
numlin = 0
for line in lista:
    amount = float(line)
    numlin += 1
    suma = suma + amount 
    sumad = sumad + amount*amount 
    c = c + 1

    prom = suma/c
    promc = prom*prom
    promd = sumad/c
    valabs = abs(promc-promd)
    sig = math.sqrt(valabs) 
print"   "
print"Numero de datos", c

#print the average
print"El promedio es", prom

#print the deviation
print"La desviacion es", sig
print"   "

archivo.close()
