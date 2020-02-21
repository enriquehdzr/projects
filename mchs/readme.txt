Estos programas se utilizan para obtener la funcion de distribución radial.
Estadisticamente la importancia de esta funcion se refiere a determinar la posibilidad de que una
particula se encuentra cercana a una particula de prueba. Con esto podemos obtener patrones de densidad 
y tendencias de fluides para determinar el estado del sistema.
Nuevamente se recurre al modelo Monte Carlo para generar valoreas aleatorios que permitan predecir futuros 
comportamientos a aprtir de comportamientos anteriores



1.- Generar la configuracion inicial
correr el ejecutable conf.exe o conf (el codigo fuente es conf.f)
Para compilar el programa conf.f
./gfortran -o conf.exe conf.f

Ejecutar
./conf.exe

Seleccionar la opcion 1 (FCC)
Seleccionar el numero 3 (3-replicas) Numero de atomos = 108
Introducir la densidad numerica adimensional, dens=0.5
Seleccionar la opcion 2 (foto configuracion inicial)
Seleccionar la opcion 0 (salir)
Sugerencia, el archivo de salida puede llamarse ''fort.1x''

2.- Simulacion Monte Carlo

copiar el archivo fort.1x a fort.1
cp fort.1x  fort.1

Para compilar el programa de Monte Carlo
gfortran -o mc.exe mc.f

3.- corrida
./mchs.exe < mchs.dat

El archivo ''mc.dat'' contiene datos de entrada para llevar a cabo la corrida:
HS      !TITLE
5000    !NUMBER OF CYCLES
500     !NUMBER OF CYCLES BETWEEN OUTPUT
5       !INTERVAL FOR UPDATE OF MAX. DISPL.
100     !THE FRECUENCY TO CALCULATE G(R)
fort.1  
0.5     !DENSITY

Solo se pueden simular 108 moleculas que interactuan mediante el potencial de esfera dura.
Calcula la energia potencial y la presion

4.- Visualizar la funcion de distribucion radial
xmgrace gr.dat
