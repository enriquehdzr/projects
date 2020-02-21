      program configuracion
      implicit double precision(a-h,o-z)
      parameter (maxnat=5000)
      common /pos/ rx(maxnat),ry(maxnat),rz(maxnat)
      common /posfcc/ rxfcc(maxnat),ryfcc(maxnat),rzfcc(maxnat)
      common /box/ boxx,boxy,boxz

      n = 0
10    continue
      write(6,*) 'crear FCC              [1]'
      write(6,*) 'crear foto             [2]'
      write(6,*) 'STOP                   [0]'

      read(5,*) noption

      if(noption.eq.0) go to 20
      if(noption.eq.1) then
         call fcc(nfcc,dens) 
         n = nfcc
         do i=1,n
            rx(i) = rxfcc(i)
            ry(i) = ryfcc(i)
            rz(i) = rzfcc(i)
         enddo
      endif

      if(noption.eq.2) call pdb(n)
      go to 10

20    continue
      call writecn(n)

      stop
      end

      subroutine writecn(n)
      implicit double precision(a-h,o-z)
      parameter (maxnat=5000)
      common /pos/ rx(maxnat),ry(maxnat),rz(maxnat)
      common /posfcc/ rxfcc(maxnat),ryfcc(maxnat),rzfcc(maxnat)
      common /box/ boxx,boxy,boxz
      dimension rxx(maxnat),ryy(maxnat),rzz(maxnat)
      character*30 name

      write(6,*) 'nombre del archivo de salida'
      read(5,'(A)') name
      open(12,file=name,form='unformatted')
      rewind (12)

      boxix = 1.0/boxx
      boxiy = 1.0/boxy
      boxiz = 1.0/boxz
      write(12) n
      write(6,*)'n', n
      write(12) boxx,boxy,boxz,boxix,boxiy,boxiz
      DO I=1,N
         WRITE (12) Rx(i),Ry(i),Rz(i)
      ENDDO

      return
      end                                               

      subroutine fcc(nfcc,dens)
      implicit double precision(a-h,o-z)
      parameter (maxnat=5000)
      common /pos/ rx(maxnat),ry(maxnat),rz(maxnat)
      common /posfcc/ rxfcc(maxnat),ryfcc(maxnat),rzfcc(maxnat)
      common /box/ boxx,boxy,boxz

      write(6,*) 'nc  N=4 nc^3'
      read(5,*) nc
      nfcc=4*nc*nc*nc
      
      write(6,*) 'numero de moleculas ',nfcc
      write(6,*) 'density'
      read(5,*) dens
      boxx = (dfloat(nfcc)/dens)**(1.0D0/3.0D0)
      boxy = boxx
      boxz = boxx

      write(6,*) 'boxx',boxx

c     calculate the side of the unit cell
      cellx=boxx/float(nc)
      celly=boxy/float(nc)
      cellz=boxz/float(nc)
      cell2x=0.5*cellx
      cell2y=0.5*celly
      cell2z=0.5*cellz
c     build the unit cell
c     sublattice A
      rxfcc(1)=0.0
      ryfcc(1)=0.0
      rzfcc(1)=0.0
c     sublattice B
      rxfcc(2)=cell2x
      ryfcc(2)=cell2y
      rzfcc(2)=0.0
c     sublattice C
      rxfcc(3)=0.0
      ryfcc(3)=cell2y
      rzfcc(3)=cell2z
c     sublattice D
      rxfcc(4)=cell2x
      ryfcc(4)=0.0
      rzfcc(4)=cell2z
c     construct the lattice from de unit cell
      m=0
      do 99 iz=1,nc
      do 98 iy=1,nc
      do 97 ix=1,nc
      do 96 iref=1,4
      rxfcc(iref+m)=rxfcc(iref)+cellx*float(ix-1)
      ryfcc(iref+m)=ryfcc(iref)+celly*float(iy-1)
      rzfcc(iref+m)=rzfcc(iref)+cellz*float(iz-1)
96    continue
      m=m+4
97    continue
98    continue
99    continue

      return
      end

      subroutine pdb(n)
      implicit double precision(a-h,o-z)
      parameter (maxnat=5000)
      common /pos/ rx(maxnat),ry(maxnat),rz(maxnat)
      common /posfcc/ rxfcc(maxnat),ryfcc(maxnat),rzfcc(maxnat)
      common /box/ boxx,boxy,boxz
      common /atom/ ngr,nrdf
      dimension symbol(5000)
      DIMENSION RXF(maxnat), RYF(maxnat), RZF(maxnat)
      CHARACTER*4 W1,W2,W4,SYMBOL
      CHARACTER*6 W3

      OPEN(7,file='FotoInicial.pdb')
      rewind(7)
      W1='ATOM'
      W3='     '
      W4='    '
      N1= 1

      NAT = n
      DO I=1,NAT
        SYMBOL(I) = '  H'
      ENDDO

      sigmar = 3.405

       do i=1,n
          rxf(i) = rx(i)*sigmar
          ryf(i) = ry(i)*sigmar
          rzf(i) = rz(i)*sigmar
       enddo

        DO I=1,N
           W2 = symbol(I)
           WRITE(7,10) w1,i,w2,w3,n1,i,w4,
     &                       rxf(i),ryf(i),rzf(i)
        ENDDO
10      FORMAT(A4,I7,A4,A5,I2,I4,A4,3F8.3)

        RETURN
        END
