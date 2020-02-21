
This files are part of a system used to get the known Radial distribution Function, wich is used to determine la probability 
for one particle to be near to a sample one. With this we can get patterns of density and trens of fluidity in order to 
determine the state of the system. 
For this is evaluated the position for much particules as we want. Systems with over 10 thousand particles are commonly used so 
the amount of data analyzed used to be very large.
Monte Carlo predictions allow us to anticipate future status or behaivor according to previous data.

1.- Generate the initial configuration
running conf.exe or conf (source code is conf.f)
to do this conf.f
./gfortran -o conf.exe conf.f

Run
./conf.exe

Select option 1 (FCC)
Select number 3 (3-replicas) Number of atoms = 108
Set adimentional numeric density, dens=0.5
Select option 2 (photo of initial config)
Select option 0 (exit)
output file may be called ''fort.1x''

2.- Monte Carlo simulation

copy fort.1x to fort.1
cp fort.1x  fort.1

To compile the Monte Carlo program we use
gfortran -o mc.exe mc.f

3.- Run
./mchs.exe < mchs.dat

The file ''mc.dat'' contains input data to get the simulation done
HS      !TITLE
5000    !NUMBER OF CYCLES
500     !NUMBER OF CYCLES BETWEEN OUTPUT
5       !INTERVAL FOR UPDATE OF MAX. DISPL.
100     !THE FRECUENCY TO CALCULATE G(R)
fort.1  
0.5     !DENSITY


Estimate the potential energy and pressure  

4.- Plot the function using xmgrace
xmgrace gr.dat
