c This is a program used to read the information from a file table.xvg previously generated. In that file we found 105 000 diferent values for this particular case.
c THose values correspond to a information of the behaivor from every each one of the 15 000 molecules of a thermodinamical system and
c some derivates that are also calculated by the program.
c What we do with this program, made with fortran, is to read the contribution of every single particle over all the others left.
c we need to do this in order to be able to predict the future location, speed, force cotribution, etc, in whatever timelipse we would like to.
C DUe to the hardware that i had access to (Olinka Cluster), i was able to do this just for 10 million time steps, wich means that i had to get the
c statistics for almost  1.05x10^12 diferent values. For doing so, I help myself with a C++ program, shamely I'm still no having the permission to share it
c beacuse it is now my University's property until they release my thesis. In this field of study is hardly necesry to know how to predict the behaivor 
c for every single one of thousands, hopefully million of particles in order to recreate values as closest to their real behaivor as we can, this by using 
c commonly MOnte Carlo models wich is widely known in statistics. 

      PROGRAM potrep
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

      PI = 3.14159265358979D0
      
      WRITE(6,*)'    '
      WRITE(6,*)'DAR SIGMA Y EPSILON'
      READ(5,*) SIG, EPS   
      WRITE(6,*)'DAR SA, SR' 
      READ(5,*) SA, SR    
      WRITE(6,*)'DAR Rcut' 
      READ(5,*) Rcut      
      WRITE(6,*)'SIG,EPS',SIG,EPS
      WRITE(6,*)'SA,SR',SA,SR
      WRITE(6,*)'Rcut',Rcut      
      WRITE(6,*)'    '

      write(6,*)'Estoy trabajando!'

      OPEN(1,FILE='table.xvg')
c     OPEN(2,FILE='LJ.dat')
c     OPEN(3,FILE='ANC.dat')
c     OPEN(4,FILE='gij.dat')
c     OPEN(7,FILE='hij.dat')
c     OPEN(8,FILE='Dgij.dat')
c     OPEN(9,FILE='Dhij.dat')
c     OPEN(10,FILE='table-LJ.dat')

      SIG6  = SIG**6.0d0
      SIG12 = SIG6*SIG6
      C6    = 4.0*EPS*SIG6
      C12   = 4.0*EPS*SIG12
      a     = 0                      
      ds    = (2.0d0**(1.0/6.0))
      delta = SIG*ds
      delr  = 0.002
      nbins = int( (rcut + 1)/delr ) + 1
      R     = 0.000 
      write(6,*)'nbins',nbins
      write(6,*)'C6,C12',C6,C12
 
      DO  K = 0,nbins 
          R = delr*K

c        ******* COULOMB ************
   
          R2 = R*R
          IF(R.EQ.0)THEN
            UCoul    =  0.0   
            DUCoul   =  0.0    
          ELSE
            UCoul    =  1.0/R 
            DUCoul   = -1.0/R2
          ENDIF
           
c        ******* ANC ****************

         IF(R.LE.delta) s=sr
         IF(R.GT.delta) s=sa

         z     = R/delta
         z2    = z*z
         z3    = z2*z
         xi3   = 1.0d0 - 1.0d0/s + z3/s
         xi    = (xi3)**(1.0d0/3.0d0)
         xi2   = xi*xi
         fac0  = (1.0d0 -a)/(xi-a)
         term6 = fac0**6.0d0
         gij   = -term6/2.0d0/sig6
         Dgij  = -9.0d0*term6*z2/2.0/sig6/s/delta/xi2/(xi-a) 
         IF(R.EQ.0)THEN
            gij  = 0.0
            Dgij = 0.0 
         ELSE
            gij   = -term6/2.0d0/sig6
            Dgij  = -9.0d0*term6*z2/2.0/sig6/s/delta/xi2/(xi-a)
         ENDIF

         term12= term6*term6
         uanc  = eps*(term12-2.0d0*term6)           
         IF(R.EQ.0)THEN         
           hij  = 0.0
           Dhij = 0.0
         ELSE
           hij   = term12/4.0d0/sig12
           Dhij  = 9.0d0*term12*z2/2.0/sig12/s/delta/xi2/(xi-a)
         ENDIF

c        ******* Lennard-Jones ******

         R6      = R**6.0d0
         R12     = R6*R6
         ULJ     = 4.0d0*EPS*(SIG12/R12-SIG6/R6)
         IF(R.EQ.0)THEN
           gijLJ   = 0.0     
           DgijLJ  = 0.0        
           hijLJ   = 0.0      
           DhijLJ  = 0.0             
         ELSE
           gijLJ   = -1.0d0/R6
           DgijLJ  = -6.0d0/R6/R 
           hijLJ   = 1.0d0/R12
           DhijLJ  = 12.0d0/R12/R 
         ENDIF

c        *****************************************

         WRITE(1,*) R,UCoul,DUCoul,gij,Dgij,hij,Dhij
c        WRITE(2,*) R,ULJ         ! Lennard-Jones
c        WRITE(3,*) R,UANC        ! ANC
c        WRITE(4,*) R,gij,gijLJ
c        WRITE(7,*) R,hij,hijLJ
c        WRITE(8,*) R,Dgij,DgijLJ
c        WRITE(9,*) R,Dhij,DhijLJ
c        WRITE(10,*) R,UCoul,DUCoul,gijLJ,DgijLJ,hijLJ,DhiLJ
         IF(R.GT.RCut) go to 300
      ENDDO
300   CONTINUE      
      close(1)
c     close(2)
c     close(3)
c     close(4)
c     close(7)
c     close(8)
c     close(9)
c     close(10)

      write(6,*)'   '
      write(6,*)'Ya termine!'
      write(6,*)'   '

      STOP
      END
