
#Este programa sirve para generar la integral numerica de una curva para la cual nosotros asignamos los valores

from plataform import python_version
print ("python " + python_version())
import sys 
import math


x = 0.900

archivo = open("y.dat", "r")
y = archivo.readlines()


numlin = 0
suma = 0
h = 0.05


for line in y:  
    amoun = amount*4*x*x*3.14159
    suma = suma + amoun
    inte = h*(0.0257+2*suma + 0.9623)
    


print inte
