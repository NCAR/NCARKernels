C     PROGRAM TO TEST POWER FUNCTION (**)
C
C     DATA REQUIRED
C
C        NONE
C
C     SUBPROGRAMS REQUIRED FROM THIS PACKAGE
C
C        MACHAR - AN ENVIRONMENTAL INQUIRY PROGRAM PROVIDING
C                 INFORMATION ON THE FLOATING-POINT ARITHMETIC
C                 SYSTEM.  NOTE THAT THE CALL TO MACHAR CAN
C                 BE DELETED PROVIDED THE FOLLOWING SIX
C                 PARAMETERS ARE ASSIGNED THE VALUES INDICATED
C
C                 IBETA  - THE RADIX OF THE FLOATING-POINT SYSTEM
C                 IT     - THE NUMBER OF BASE-IBETA DIGITS IN THE
C                          SIGNIFICAND OF A FLOATING-POINT NUMBER
C                 MINEXP - THE LARGEST IN MAGNITUDE NEGATIVE
C                          INTEGER SUCH THAT  DFLOAT(IBETA)**MINEXP
C                          IS A POSITIVE FLOATING-POINT NUMBER
C                 MAXEXP - THE LARGEST POSITIVE INTEGER EXPONENT
C                          FOR A FINITE FLOATING-POINT NUMBER
C                 XMIN   - THE SMALLEST NON-VANISHING FLOATING-POINT
C                          POWER OF THE RADIX
C                 XMAX   - THE LARGEST FINITE FLOATING-POINT
C                          NUMBER
C
C        REN(K) - A FUNCTION SUBPROGRAM RETURNING RANDOM REAL
C                 NUMBERS UNIFORMLY DISTRIBUTED OVER (0,1)
C
C
C     STANDARD FORTRAN SUBPROGRAMS REQUIRED
C
C         DABS, DLOG, DMAX1, DEXP, DFLOAT, DSQRT
C
C
C     LATEST REVISION - DECEMBER 6, 1979
C
C     AUTHOR - W. J. CODY
C              ARGONNE NATIONAL LABORATORY
C
C
      INTEGER I,IBETA,IDEXP,IOUT,IRND,IT,I1,J,K1,K2,K3,MACHEP,
     1        MAXEXP,MINEXP,N,NEGEP,NGRD
      DOUBLE PRECISION
     1     A,AIT,ALBETA,ALXMAX,B,BETA,C,DEL,DELY,EPS,EPSNEG,ONE,
     2     ONEP5,REN,R6,R7,SCALE,TWO,W,X,XL,XMAX,XMIN,XN,
     3     XSQ,X1,Y,Y1,Y2,Z,ZERO,ZZ
C
      IOUT = 6
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IDEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = DFLOAT(IBETA)
      ALBETA = DLOG(BETA)
      AIT = DFLOAT(IT)
      ALXMAX = DLOG(XMAX)
      ZERO = 0.0D0
      ONE = DFLOAT(1)
      TWO = ONE + ONE
      ONEP5 = (TWO + ONE) / TWO
      SCALE = ONE
      J = (IT+1) / 2
C
      DO 20 I = 1, J
         SCALE = SCALE * BETA
   20 CONTINUE
C
      A = ONE / BETA
      B = ONE
      C = -DMAX1(ALXMAX,-DLOG(XMIN))/DLOG(100D0)
      DELY = -C - C
      N = 2000
      XN = DFLOAT(N)
      I1 = 0
      Y1 = ZERO
C-----------------------------------------------------------------
C     RANDOM ARGUMENT ACCURACY TESTS
C-----------------------------------------------------------------
      DO 300 J = 1, 4
         K1 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
C
         DO 200 I = 1, N
            X = DEL * REN(I1) + XL
            IF (J .NE. 1) GO TO 50
            ZZ = X ** ONE
            Z = X
            GO TO 110
   50       W = SCALE * X
            X = (X + W) - W
            XSQ = X * X
            IF (J .EQ. 4) GO TO 70
            ZZ = XSQ ** ONEP5
            Z = X * XSQ
            GO TO 110
   70       Y = DELY * REN(I1) + C
            Y2 = (Y/TWO + Y) - Y
            Y = Y2 + Y2
            Z = X ** Y
            ZZ = XSQ ** Y2
  110       W = ONE
            IF (Z .NE. ZERO) W = (Z - ZZ) / Z
            IF (W .GT. ZERO) K1 = K1 + 1
            IF (W .LT. ZERO) K3 = K3 + 1
            W = DABS(W)
            IF (W .LE. R6) GO TO 120
            R6 = W
            X1 = X
            IF (J .EQ. 4) Y1 = Y
  120       R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C
         K2 = N - K3 - K1
         R7 = DSQRT(R7/XN)
         IF (J .GT. 1) GO TO 210
         WRITE (IOUT,1000)
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         GO TO 220
  210    IF (J .EQ. 4) GO TO 215
         WRITE (IOUT,1001)
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1012) K1,K2,K3
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         GO TO 220
  215    WRITE (IOUT,1002)
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         W = C + DELY
         WRITE (IOUT,1014) N,A,B,C,W
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1013) K1,K2,K3
         WRITE (IOUT,*)
         WRITE (IOUT,*)

  220    WRITE (IOUT,1020) IT,IBETA
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         W = -999.0D0
         IF (R6 .NE. ZERO) W = DLOG(DABS(R6))/ALBETA
         IF (J .NE. 4) WRITE (IOUT,1021) R6,IBETA,W,X1
         IF (J .EQ. 4) WRITE (IOUT,1024) R6,IBETA,W,X1,Y1
         W = DMAX1(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         W = -999.0D0
         IF (R7 .NE. ZERO) W = DLOG(DABS(R7))/ALBETA
         WRITE (IOUT,1023) R7,IBETA,W
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         W = DMAX1(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         IF (J .EQ. 1) GO TO 300
         B = 10.0D0
         A = 0.01D0
         IF (J .EQ. 3) GO TO 300
         A = ONE
         B = DEXP(ALXMAX/3.0D0)
  300 CONTINUE
C-----------------------------------------------------------------
C     SPECIAL TESTS
C-----------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      WRITE (IOUT,1030)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      B = 10.0D0
C
      DO 320 I = 1, 5
         X = REN(I1) * B + ONE
         Y = REN(I1) * B + ONE
         Z = X ** Y
         ZZ = (ONE/X) ** (-Y)
         W = (Z - ZZ) / Z
         WRITE (IOUT,1060) X, Y, W
  320 CONTINUE
C-----------------------------------------------------------------
C     TEST OF ERROR RETURNS
C-----------------------------------------------------------------
      WRITE (IOUT,1050)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = BETA
      Y = DFLOAT(MINEXP)
      WRITE (IOUT,1051) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      Z = X ** Y
      WRITE (IOUT,1055) Z
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      Y = DFLOAT(MAXEXP-1)
      WRITE (IOUT,1051) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      Z = X ** Y
      WRITE (IOUT,1055) Z
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = ZERO
      Y = TWO
      WRITE (IOUT,1051) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      Z = X ** Y
      WRITE (IOUT,1055) Z
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = -Y
      Y = ZERO
      WRITE (IOUT,1052) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#if !defined(CRAY_PVP)
      Z = X ** Y
      WRITE (IOUT,1055) Z
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#else
      WRITE(IOUT,*)'DISABLED FOR CRAYS'
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#endif
      Y = TWO
      WRITE (IOUT,1052) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#if !defined(CRAY_PVP)
      Z = X ** Y
      WRITE (IOUT,1055) Z
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#else
      WRITE(IOUT,*)'DISABLED FOR CRAYS'
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#endif
      X = ZERO
      Y = ZERO
      WRITE (IOUT,1052) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#if !defined(CRAY_PVP)
      Z = X ** Y
      WRITE (IOUT,1055) Z
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#else
      WRITE(IOUT,*)'DISABLED FOR CRAYS'
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#endif
      WRITE (IOUT,1100)
      STOP
 1000 FORMAT("TEST OF X**1.0 VS X  ")
 1001 FORMAT("TEST OF XSQ**1.5 VS XSQ*X  ")
 1002 FORMAT("TEST OF X**Y VS XSQ**(Y/2)  ")
 1010 FORMAT(I7," RANDOM ARGUMENTS WERE TESTED FROM THE INTERVAL "
     1 6X,"(",E15.4,",",E15.4,")")
 1011 FORMAT(" X**1.0 WAS LARGER",I6," TIMES,",
     1     11X," AGREED",I6," TIMES, AND ",
     2   7X,"WAS SMALLER",I6," TIMES.")
 1012 FORMAT(" X**1.5 WAS LARGER",I6," TIMES,",
     1     11X," AGREED,"I6," TIMES, AND ",
     2   7X,"WAS SMALLER",I6," TIMES.")
 1013 FORMAT("  X**Y  WAS LARGER",I6," TIMES,",
     1     11X," AGREED",I6," TIMES, AND ",
     2   7X,"WAS SMALLER",I6," TIMES.")
 1014 FORMAT(I7," RANDOM ARGUMENTS WERE TESTED FROM THE REGION ",
     1 6X,"X IN (",E15.4,",",E15.4,"), Y IN (",E15.4,",",E15.4,")")
 1020 FORMAT(" THERE ARE",I4," BASE",I4,
     1    " SIGNIFICANT DIGITS IN A FLOATING-POINT NUMBER  ")
 1021 FORMAT(" THE MAXIMUM RELATIVE ERROR OF",E15.4," = ",I4," **",
     1  F7.2,4X,"OCCURRED FOR X =",E17.6)
 1022 FORMAT(" THE ESTIMATED LOSS OF BASE",I4,
     1  " SIGNIFICANT DIGITS IS",F7.2)
 1023 FORMAT(" THE ROOT MEAN SQUARE RELATIVE ERROR WAS",E15.4,
     1    " = ",I4," **",F7.2)
 1024 FORMAT(" THE MAXIMUM RELATIVE ERROR OF",E15.4," = ",I4," **",
     1  F7.2,4X,"OCCURRED FOR X =",E17.6," Y =",E17.6)
 1025 FORMAT("SPECIAL TESTS")
 1030 FORMAT(" THE IDENTITY  X ** Y = (1/X) ** (-Y)  WILL BE TESTED.",
     1    8X,"X",14X,"Y",9X,"(X**Y-(1/X)**(-Y) / X**Y ")
 1050 FORMAT("TEST OF ERROR RETURNS")
 1051 FORMAT(" (",E14.7," ) ** (",E14.7," ) WILL BE COMPUTED.",
     1       " THIS SHOULD NOT TRIGGER AN ERROR MESSAGE")
 1052 FORMAT(" (",E14.7," ) ** (",E14.7," ) WILL BE COMPUTED.",
     1       " THIS SHOULD TRIGGER AN ERROR MESSAGE")
 1055 FORMAT(" THE VALUE RETURNED IS",E15.4)
 1060 FORMAT(2E15.7,6X,E15.7)
 1100 FORMAT(" THIS CONCLUDES THE TESTS ")
C     ---------- LAST CARD OF POWER TEST PROGRAM ----------
      END
      DOUBLE PRECISION FUNCTION REN(K)
C
C     RANDOM NUMBER GENERATOR - BASED ON ALGORITHM 266 BY PIKE AND
C      HILL (MODIFIED BY HANSSON), COMMUNICATIONS OF THE ACM,
C      VOL. 8, NO. 10, OCTOBER 1965.
C
C     THIS SUBPROGRAM IS INTENDED FOR USE ON COMPUTERS WITH
C      FIXED POINT WORDLENGTH OF AT LEAST 29 BITS.  IT IS
C      BEST IF THE FLOATING POINT SIGNIFICAND HAS AT MOST
C      29 BITS.
C
      INTEGER IY,J,K
      DATA IY/100001/
C
      J = K
      IY = IY * 125
      IY = IY - (IY/2796203) * 2796203
      REN = DFLOAT(IY) / 2796203.0D0 * (1.0D0 + 1.0D-6 + 1.0D-12)
      RETURN
C     ---------- LAST CARD OF REN ----------
      END
