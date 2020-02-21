from plataform import python_version
print ("python " + python_version())
import sys
import math


x = 0.050



archivo = open("ry.dat" , "r")
ry = archivo.readlines ()

numlin = 0
suma = 0.0
h = 0.05



for line in ry:
    amount = float(line)
    amoun= amount*x*x
    suma = suma + amoun
    print (amoun)
    x = x + h

integral = h*(0.00 + 2*4*3.14159*suma + 0.7666)
print (integral)




