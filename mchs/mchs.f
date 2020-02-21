        PROGRAM mcsh        
        implicit double precision(a-h,o-z)
        include 'parametros'

        COMMON / BLOCK1 / RX(MAXNAT), RY(MAXNAT), RZ(MAXNAT)
        COMMON / BOXXXX / BOXX,BOXY,BOXZ        
        COMMON / BOXIXI / BOXIX,BOXIY,BOXIZ        
        COMMON /NUMATOM/ N
        LOGICAL     OVERLAP
        CHARACTER   TITLE*80, CNFILE*80

C    *******************************************************************

C    ** READ INPUT DATA **

        WRITE(*,'(/   '' MONTE CARLO                             '')')
        WRITE(*,'(    '' FOR HARD SPHERES                        ''/)')
        WRITE(*,'('' ENTER THE RUN TITLE                          '')')
        READ (*,'(A)') TITLE
        WRITE(*,'('' ENTER NUMBER OF CYCLES                       '')')
        READ (*,*) NSTEP
        WRITE(*,'('' ENTER NUMBER OF CYCLES BETWEEN OUTPUT        '')')
        READ (*,*) IPRINT
        WRITE(*,'('' ENTER INTERVAL FOR UPDATE OF MAX. DISPL.     '')')
        READ (*,*) IRATIO
        WRITE(*,'('' ENTER THE FRECUENCY TO CALCULATE G(R)        '')')
        READ (*,*) NGR   
        WRITE(*,'('' ENTER THE CONFIGURATION FILE NAME            '')')
        READ (*,'(A)') CNFILE
        WRITE(*,'(/'' ENTER THE FOLLOWING IN LENNARD-JONES UNITS '',/)')
        WRITE(*,'('' ENTER THE DENSITY                            '')')
        READ (*,*) DENS

C    ** WRITE INPUT DATA **

        WRITE(*,'(       //1X                    ,A     )') TITLE
        WRITE(*,'('' NUMBER OF CYCLES          '',I10   )') NSTEP
        WRITE(*,'('' OUTPUT FREQUENCY          '',I10   )') IPRINT
        WRITE(*,'('' RATIO UPDATE FREQUENCY    '',I10   )') IRATIO
        WRITE(*,'('' FRECUENCY TO CALCULATE GR '',I10   )') NGR
        WRITE(*,'('' CONFIGURATION FILE  NAME  '',A     )') CNFILE
        WRITE(*,'('' DENSITY                   '',F10.5 )') DENS

C    ** SET DEPENDENT VARIABLES **

        SIGMA  = 1.0
        DRMAX  = 0.15

C    ** READ IN INITIAL CONFIGURATION **

        CALL READCN ( CNFILE )

        VOL  = BOXX*BOXY*BOXZ

        WRITE(*,'('' NUMBER OF ATOMS          =  '', I10   )' )  N
        WRITE( *, '( ''BOXX,..,'',3F12.4)')BOXX,BOXY,BOXZ               

        IF ( RCUT .GT. 0.5*BOXX ) STOP ' CUT-OFF TOO LARGE '

C    ** CHECK FOR OVERLAPS **

        CALL SUMUP ( RCUT, RMIN, SIGMA, OVERLAP)

        IF ( OVERLAP ) STOP ' OVERLAP IN INITIAL CONFIGURATION '

C    ** ZERO ACCUMULATORS **

        ACM    = 0.0
        ACMMVA = 0.0

        WRITE( 6, '(//'' START OF MARKOV CHAIN                ''//)')
        WRITE( 6, '( ''     STEP      ACM      RATIO          '')')
        WRITE( 6,* )'       '

C    *******************************************************************
C    ** LOOP OVER CYCLES BEGINS                                       **
C    *******************************************************************

        DO 100 ISTEP = 1, NSTEP

C       ** LOOP OVER MOLECULES **

           DO 99 I = 1, N

              RXIOLD = RX(I)
              RYIOLD = RY(I)
              RZIOLD = RZ(I)

              CALL ENERGY ( RXIOLD, RYIOLD, RZIOLD, I, RCUT, SIGMA,
     :                      OVERLAP)   

C          ** MOVE I AND PICKUP THE CENTRAL IMAGE **

              RXINEW = RXIOLD + ( 2.0 * RANF ( DUMMY ) - 1.0 ) * DRMAX
              RYINEW = RYIOLD + ( 2.0 * RANF ( DUMMY ) - 1.0 ) * DRMAX
              RZINEW = RZIOLD + ( 2.0 * RANF ( DUMMY ) - 1.0 ) * DRMAX

              RXINEW = RXINEW - DNINT ( RXINEW*BOXIX )*BOXX
              RYINEW = RYINEW - DNINT ( RYINEW*BOXIY )*BOXY
              RZINEW = RZINEW - DNINT ( RZINEW*BOXIZ )*BOXZ

              CALL ENERGY ( RXINEW, RYINEW, RZINEW, I, RCUT, SIGMA,
     :                      OVERLAP)

              IF(OVERLAP)GOTO 99

C          ** CHECK FOR ACCEPTANCE **

              IF ( .NOT. OVERLAP ) THEN

C             ** ACCEPT MOVE **

                 RX(I)  = RXINEW
                 RY(I)  = RYINEW
                 RZ(I)  = RZINEW
                 ACMMVA = ACMMVA + 1.0

              ENDIF

              ACM = ACM + 1.0

99         CONTINUE

C       ****************************************************************
C       ** LOOP OVER MOLECULES COMPLETE                               **
C       ****************************************************************

C       ** PERFORM PERIODIC OPERATIONS  **
C       ** CHANGE MAXIMUM DISPLACEMENT **

           IF ( MOD ( ISTEP, IRATIO ) .EQ. 0 ) THEN

              RATIO = ACMMVA / REAL ( N * IRATIO )

              IF ( RATIO .GT. 0.5 ) THEN

                 DRMAX = DRMAX * 1.05

              ELSE

                 DRMAX = DRMAX * 0.95

              ENDIF

              ACMMVA = 0.0

           ENDIF

C       ** WRITE OUT RUNTIME INFORMATION **


         IF ( MOD ( ISTEP, IPRINT ) .EQ. 0 ) THEN
           WRITE(6,12)ISTEP, INT(ACM),RATIO
         ENDIF
12       FORMAT(I8,I10,1F12.4)
         IF(MOD(ISTEP,NGR).EQ.0) THEN
             CALL NRDF
         ENDIF

100     CONTINUE

C    *******************************************************************
C    ** ENDS THE LOOP OVER CYCLES                                     **
C    *******************************************************************

C    ** WRITE OUT THE FINAL CONFIGURATION FROM THE RUN **

        CALL WRITCN ( CNFILE )

        NSTEPG = NSTEP/NGR
        IF(NSTEPG.GT.0) THEN
           CALL RDF(NSTEPG)
        ENDIF

C    ** FOTO DE LA CONFIGURACION FINAL **
        CALL PDB

        WRITE(6,*)'    '
        WRITE(6,*)'Ya termine'
        WRITE(6,*)'    '

        STOP
        END

C   **********************************************************
C                START SUBROUTINES
C   **********************************************************

        SUBROUTINE READCN ( CNFILE )
        implicit double precision(a-h,o-z)
        include 'parametros'

        COMMON / BLOCK1 / RX(MAXNAT), RY(MAXNAT), RZ(MAXNAT)
        COMMON / BOXXXX / BOXX,BOXY,BOXZ        
        COMMON / BOXIXI / BOXIX,BOXIY,BOXIZ
        COMMON /NUMATOM/ N

C    *******************************************************************
C    ** SUBROUTINE TO READ IN THE CONFIGURATION FROM UNIT 10          **
C    *******************************************************************

        CHARACTER   CNFILE*(*)
        INTEGER     CNUNIT
        PARAMETER ( CNUNIT = 10 )

        OPEN ( UNIT = CNUNIT, FILE = CNFILE, STATUS = 'OLD',
     :         FORM = 'UNFORMATTED'                        )

        READ ( CNUNIT ) N
        READ ( CNUNIT ) BOXX,BOXY,BOXZ,BOXIX,BOXIY,BOXIZ
        DO I = 1,N
           READ ( CNUNIT ) RX(I), RY(I), RZ(I)
        ENDDO

        CLOSE ( UNIT = CNUNIT )

        RETURN
        END

C    *******************************************************************

        SUBROUTINE WRITCN ( CNFILE )
        implicit double precision(a-h,o-z)
        include 'parametros'

        COMMON / BLOCK1 / RX(MAXNAT), RY(MAXNAT), RZ(MAXNAT)
        COMMON / BOXXXX / BOXX,BOXY,BOXZ        
        COMMON / BOXIXI / BOXIX,BOXIY,BOXIZ
        COMMON /NUMATOM/ N

C    *******************************************************************
C    ** SUBROUTINE TO WRITE OUT THE CONFIGURATION TO UNIT 10          **
C    *******************************************************************

        CHARACTER    CNFILE*(*)
        INTEGER      CNUNIT
        PARAMETER (  CNUNIT = 10 )

        OPEN ( UNIT = CNUNIT, FILE = CNFILE, STATUS = 'UNKNOWN',
     :         FORM = 'UNFORMATTED'                        )

        WRITE ( CNUNIT ) N
        WRITE ( CNUNIT ) BOXX,BOXY,BOXZ,BOXIX,BOXIY,BOXIZ
        DO I = 1,N
           WRITE ( CNUNIT ) RX(I), RY(I), RZ(I)
        ENDDO

        CLOSE ( UNIT = CNUNIT )

        RETURN
        END

C    *******************************************************************

        REAL*8 FUNCTION RANF ( DUMMY )

C    *******************************************************************
C    ** RETURNS A UNIFORM RANDOM VARIATE IN THE RANGE 0 TO 1.         **
C    **                                                               **
C    **                 ***************                               **
C    **                 **  WARNING  **                               **
C    **                 ***************                               **
C    **                                                               **
C    ** GOOD RANDOM NUMBER GENERATORS ARE MACHINE SPECIFIC.           **
C    ** PLEASE USE THE ONE RECOMMENDED FOR YOUR MACHINE.              **
C    *******************************************************************

        INTEGER     N 
        INTEGER     L, C, M
        PARAMETER ( L = 1029, C = 221591, M = 1048576 )

        INTEGER     SEED
        REAL*8      DUMMY
        SAVE        SEED
        DATA        SEED / 0 /

        SEED = MOD ( SEED * L + C, M )
        RANF = REAL ( SEED ) / M

        RETURN
        END

C    *******************************************************************

        SUBROUTINE NRDF
        implicit double precision(a-h,o-z)
        include 'parametros'

        COMMON / BLOCK1 / RX(MAXNAT), RY(MAXNAT), RZ(MAXNAT)
        COMMON / BOXXXX / BOXX,BOXY,BOXZ
        COMMON / BOXIXI / BOXIX,BOXIY,BOXIZ
        COMMON /NUMATOM/ N
        COMMON / GR / NGA(NMAX),GR(NMAX)

        BOX2     = 0.5*BOXX
        SIGMA    = 1.0  

        DO  I = 1,N-1
            RXI =  RX(I)
            RYI =  RY(I)
            RZI =  RZ(I)

          DO J = I+1, N
            DX = RXI - RX(J)
            DY = RYI - RY(J)
            DZ = RZI - RZ(J)

            DX = DX - DNINT(DX*BOXIX)*BOXX
            DY = DY - DNINT(DY*BOXIY)*BOXY
            DZ = DZ - DNINT(DZ*BOXIZ)*BOXZ

            RIJ = SQRT(DX*DX+DY*DY+DZ*DZ)

            IF (RIJ.LE.BOX2) THEN
                 IRIJ = RIJ/DELTAR
                 NGA(IRIJ) = NGA(IRIJ) + 2
            ENDIF

          ENDDO
        ENDDO

      RETURN
      END

C     ***********************************************************

        SUBROUTINE RDF (NSTEPG) 
        implicit double precision(a-h,o-z)
        include 'parametros'

        COMMON / BLOCK1 / RX(MAXNAT), RY(MAXNAT), RZ(MAXNAT)
        COMMON / BOXXXX / BOXX,BOXY,BOXZ
        COMMON / BOXIXI / BOXIX,BOXIY,BOXIZ
        COMMON /NUMATOM/ N
        COMMON / GR / NGA(NMAX),GR(NMAX)

        OPEN(31,FILE='gr.dat')
 
        NIG  = 0.5*BOXX/DELTAR
        VOL  = BOXX*BOXY*BOXZ
        RHOJ = N/VOL
        DENX = DFLOAT(NSTEPG*N)
        FACT = 4.0D0*PI*RHOJ/3.0D0

        DO 50 JJ=1,NIG
           R0     = (JJ) *DELTAR
           R      = R0 + DELTAR
           DEN    = FACT * ( R**3 - R0**3 )
           GAR    = NGA(JJ)/DEN
           GR(JJ) = GAR / DENX
           WRITE(31,'(2F10.4)') R0,GR(JJ)
50      CONTINUE

        RETURN
        END

C     **************************************************

        SUBROUTINE ENERGY (RXI,RYI,RZI,I,RCUT,SIGMA,OVERLAP)
        implicit double precision(a-h,o-z)
        include 'parametros'

        COMMON / BLOCK1 / RX(MAXNAT), RY(MAXNAT), RZ(MAXNAT)
        COMMON / BOXXXX / BOXX,BOXY,BOXZ
        COMMON / BOXIXI / BOXIX,BOXIY,BOXIZ
        COMMON /NUMATOM/ N
        LOGICAL     OVERLAP
        CHARACTER   TITLE*80, CNFILE*80

        OVERLAP = .FALSE.
        RCUTSQ  = RCUT * RCUT
        SIGSQ   = SIGMA * SIGMA

C    ** LOOP OVER ALL MOLECULES EXCEPT I  **

        DO 100 J = 1, N

           IF ( I .NE. J ) THEN

              RXIJ  = RXI - RX(J)
              RYIJ  = RYI - RY(J)
              RZIJ  = RZI - RZ(J)

              RXIJ  = RXIJ - DNINT ( RXIJ*BOXIX )*BOXX
              RYIJ  = RYIJ - DNINT ( RYIJ*BOXIY )*BOXY
              RZIJ  = RZIJ - DNINT ( RZIJ*BOXIZ )*BOXZ

              RIJSQ = RXIJ * RXIJ + RYIJ * RYIJ + RZIJ * RZIJ

              RIJ = SQRT(RIJSQ)

              IF(RIJ.LT.SIGMA)THEN
                  OVERLAP = .TRUE.
                  RETURN

              ENDIF
           ENDIF

100     CONTINUE

        RETURN
        END

C    *******************************************************************

        SUBROUTINE SUMUP ( RCUT, RMIN, SIGMA, OVERLAP)
        implicit double precision(a-h,o-z)
        include 'parametros'

        COMMON / BLOCK1 / RX(MAXNAT), RY(MAXNAT), RZ(MAXNAT)
        COMMON / BOXXXX / BOXX,BOXY,BOXZ
        COMMON / BOXIXI / BOXIX,BOXIY,BOXIZ
        COMMON /NUMATOM/ N
        LOGICAL     OVERLAP
        CHARACTER   TITLE*80, CNFILE*80

        OVERLAP = .FALSE.
        RCUTSQ  = RCUT * RCUT
        RMINSQ  = RMIN * RMIN
        SIGSQ   = SIGMA * SIGMA

C    ** LOOP OVER ALL THE PAIRS IN THE LIQUID **

        DO 100 I = 1, N - 1

           RXI = RX(I)
           RYI = RY(I)
           RZI = RZ(I)

           DO 99 J = I + 1, N

              RXIJ  = RXI - RX(J)
              RYIJ  = RYI - RY(J)
              RZIJ  = RZI - RZ(J)

C          ** MINIMUM IMAGE THE PAIR SEPARATIONS **
              RXIJ  = RXIJ - DNINT ( RXIJ*BOXIX )*BOXX
              RYIJ  = RYIJ - DNINT ( RYIJ*BOXIY )*BOXY
              RZIJ  = RZIJ - DNINT ( RZIJ*BOXIZ )*BOXZ

              RIJSQ = RXIJ*RXIJ + RYIJ*RYIJ + RZIJ*RZIJ

              RIJ = SQRT(RIJSQ)

                IF(RIJ.LT.SIGMA)THEN

                  OVERLAP = .TRUE.
                  RETURN

                ENDIF

99         CONTINUE
100     CONTINUE

        RETURN
        END

C      ***************************************************

        SUBROUTINE PDB
        implicit double precision(a-h,o-z)
        include 'parametros'

        COMMON / BLOCK1 / RX(MAXNAT), RY(MAXNAT), RZ(MAXNAT)
        COMMON / BOXXXX / BOXX,BOXY,BOXZ
        COMMON / BOXIXI / BOXIX,BOXIY,BOXIZ
        COMMON /NUMATOM/ N
        dimension symbol(5000)
        DIMENSION RXF(maxnat), RYF(maxnat), RZF(maxnat)
        CHARACTER*4 W1,W2,W4,SYMBOL
        CHARACTER*6 W3

        OPEN(7,file='FotoFinal.pdb')
        rewind(7)

        W1='ATOM'
        W3='    '
        W4='    '
        N1= 1

        DO I=1,N
          SYMBOL(I) = '  Si'
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
