C     PROGRAM TO TEST DLOG
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
C                 BE DELETED PROVIDED THE FOLLOWING FOUR
C                 PARAMETERS ARE ASSIGNED THE VALUES INDICATED
C
C                 IBETA - THE RADIX OF THE FLOATING-POINT SYSTEM
C                 IT    - THE NUMBER OF BASE-IBETA DIGITS IN THE
C                         SIGNIFICAND OF A FLOATING-POINT NUMBER
C                 XMIN  - THE SMALLEST NON-VANISHING FLOATING-POINT
C                         POWER OF THE RADIX
C                 XMAX  - THE LARGEST FINITE FLOATING-POINT NO.
C
C        REN(K) - A FUNCTION SUBPROGRAM RETURNING RANDOM REAL
C                 NUMBERS UNIFORMLY DISTRIBUTED OVER (0,1)
C
C
C     STANDARD FORTRAN SUBPROGRAMS REQUIRED
C
C         DABS, DLOG, DLOG10, DMAX1, DFLOAT, DSIGN, DSQRT
C
C
C     LATEST REVISION - DECEMBER 6, 1979
C
C     AUTHOR - W. J. CODY
C              ARGONNE NATIONAL LABORATORY
C
C
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,I1,J,K1,K2,K3,MACHEP,
     1        MAXEXP,MINEXP,N,NEGEP,NGRD
      DOUBLE PRECISION 
     1     A,AIT,ALBETA,B,BETA,C,DEL,EIGHT,EPS,EPSNEG,HALF,ONE,
     2     REN,R6,R7,TENTH,W,X,XL,XMAX,XMIN,XN,X1,Y,Z,ZERO,ZZ
C
      IOUT = 6
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = DFLOAT(IBETA)
      ALBETA = DLOG(BETA)
      AIT = DFLOAT(IT)
      J = IT / 3
      ZERO = 0.0D0
      HALF = 0.5D0
      EIGHT = 8.0D0
      TENTH = 0.1D0
      ONE = 1.0D0
      C = ONE
C
      DO 50 I = 1, J
         C = C / BETA
   50 CONTINUE
C
      B = ONE + C
      A = ONE - C
      N = 2000
      XN = DFLOAT(N)
      I1 = 0
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
            IF (J .NE. 1) GO TO 100
            Y = (X - HALF) - HALF
            ZZ = DLOG(X)
            Z = ONE / 3.0D0
            Z = Y * (Z - Y / 4.0D0)
            Z = (Z - HALF) * Y * Y + Y
            GO TO 150
  100       IF (J .NE. 2) GO TO 110
            X = (X + EIGHT) - EIGHT
            Y = X + X / 16.0D0
            Z = DLOG(X)
            ZZ = DLOG(Y) - 7.7746816434842581D-5
            ZZ = ZZ - 31.0D0/512.0D0
            GO TO 150
  110       IF (J .NE. 3) GO TO 120
            X = (X + EIGHT) - EIGHT
            Y = X + X * TENTH
            Z = DLOG10(X)
            ZZ = DLOG10(Y) - 3.7706015822504075D-4
            ZZ = ZZ - 21.0D0/512.0D0
            GO TO 150
  120       Z = DLOG(X*X)
            ZZ = DLOG(X)
            ZZ = ZZ + ZZ
  150       W = ONE
            IF (Z .NE. ZERO) W = (Z - ZZ) / Z
            Z = DSIGN(W,Z)
            IF (Z .GT. ZERO) K1 = K1 + 1
            IF (Z .LT. ZERO) K3 = K3 + 1
            W = DABS(W)
            IF (W .LE. R6) GO TO 160
            R6 = W
            X1 = X
  160       R7 = R7 + W*W
            XL = XL + DEL
  200    CONTINUE
C
         K2 = N - K3 - K1
         R7 = DSQRT(R7/XN)
         IF (J .EQ. 1) THEN
            WRITE (IOUT,1000)
            WRITE(IOUT,*)
            WRITE(IOUT,*)
            WRITE (IOUT,1009) N,C
            WRITE(IOUT,*)
            WRITE(IOUT,*)
         ELSE IF (J .EQ. 2) THEN
            WRITE (IOUT,1001)
            WRITE(IOUT,*)
            WRITE(IOUT,*)
         ELSE IF (J .EQ. 3) THEN
            WRITE (IOUT,1005)
            WRITE(IOUT,*)
            WRITE(IOUT,*)
         ELSE IF (J .EQ. 4) THEN
            WRITE (IOUT,1002)
            WRITE(IOUT,*)
            WRITE(IOUT,*)
         END IF

         IF (J .NE. 1) THEN
            WRITE (IOUT,1010) N,A,B
            WRITE(IOUT,*)
            WRITE(IOUT,*)
         END IF
         IF (J .NE. 3) THEN
            WRITE (IOUT,1011) K1,K2,K3
            WRITE(IOUT,*)
            WRITE(IOUT,*)
         ELSE
            WRITE (IOUT,1012) K1,K2,K3
            WRITE(IOUT,*)
            WRITE(IOUT,*)
         END IF
         WRITE (IOUT,1020) IT,IBETA
         WRITE(IOUT,*)
         WRITE(IOUT,*)

         W = -999.0D0
         IF (R6 .NE. ZERO) W = DLOG(DABS(R6))/ALBETA
         WRITE (IOUT,1021) R6,IBETA,W,X1
         WRITE(IOUT,*)
         WRITE(IOUT,*)

         W = DMAX1(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         WRITE(IOUT,*)
         WRITE(IOUT,*)

         W = -999.0D0
         IF (R7 .NE. ZERO) W = DLOG(DABS(R7))/ALBETA
         WRITE (IOUT,1023) R7,IBETA,W
         WRITE(IOUT,*)
         WRITE(IOUT,*)

         W = DMAX1(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         WRITE(IOUT,*)
         WRITE(IOUT,*)

         IF (J .GT. 1) GO TO 230
         A = DSQRT(HALF)
         B = 15.0D0 / 16.0D0
         GO TO 300
  230    IF (J .GT. 2) GO TO 240
         A = DSQRT(TENTH)
         B = 0.9D0
         GO TO 300
  240    A = 16.0D0
         B = 240.0D0
  300 CONTINUE
C-----------------------------------------------------------------
C     SPECIAL TESTS
C-----------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      WRITE (IOUT,1030)
      WRITE(IOUT,*)
      WRITE(IOUT,*)
C
      DO 320 I = 1, 5
         X = REN(I1)
         X = X + X + 15.0D0
         Y = ONE / X
         Z = DLOG(X) + DLOG(Y)
         WRITE (IOUT,1060) X, Z
         WRITE(IOUT,*)
         WRITE(IOUT,*)

  320 CONTINUE
C
      WRITE (IOUT,1040)
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      X = ONE
      Y = DLOG(X)
      WRITE (IOUT,1041) Y
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      X = XMIN
      Y = DLOG(X)
      WRITE (IOUT,1042) X, Y
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      X = XMAX
      Y = DLOG(X)
      WRITE (IOUT,1043) X, Y
      WRITE(IOUT,*)
      WRITE(IOUT,*)

C-----------------------------------------------------------------
C     TEST OF ERROR RETURNS
C-----------------------------------------------------------------
      WRITE (IOUT,1050)
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      X = -2.0D0
      WRITE (IOUT,1052) X
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      Y = DLOG(X)
      WRITE (IOUT,1055) Y
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      X = ZERO
      WRITE (IOUT,1052) X
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      Y = DLOG(X)
      WRITE (IOUT,1055) Y
      WRITE(IOUT,*)
      WRITE(IOUT,*)

      WRITE (IOUT,1100)
      STOP
 1000 FORMAT("TEST OF DLOG(X) VS T.S. EXPANSION OF DLOG(1+Y)  ")
 1001 FORMAT("TEST OF DLOG(X) VS DLOG(17X/16)-DLOG(17/16)   ")
 1002 FORMAT("TEST OF DLOG(X*X) VS 2 * LOG(X)  ")
 1005 FORMAT("TEST OF DLOG10(X) VS DLOG10(11X/10)-DLOG10(11/10) ")
 1009 FORMAT(I7," RANDOM ARGUMENTS WERE TESTED FROM THE INTERVAL ",
     1 6X,"(1-EPS,1+EPS), WHERE EPS =", E15.4)
 1010 FORMAT(I7,"RANDOM ARGUMENTS WERE TESTED FROM THE INTERVAL ",
     1 6X,"(",E15.4,",",E15.4,")")
 1011 FORMAT("DLOG(X) WAS LARGER ",I6," TIMES",
     1     12X," AGREED ",I6," TIMES, AND"
     2     8X," WAS SMALLER",I6," TIMES.")
 1012 FORMAT(" DLOG10(X) WAS LARGER ",I6," TIMES,"
     1     14X," AGREED",I6," TIMES, AND"
     2     10X,"WAS SMALLER ",I6," TIMES.")
 1020 FORMAT(" THERE ARE",I4," BASE",I4,
     1    " SIGNIFICANT DIGITS IN A FLOATING-POINT NUMBER  ")
 1021 FORMAT(" THE MAXIMUM RELATIVE ERROR OF",E15.4," = ",I4," **",
     1  F7.2,4X,"OCCURRED FOR X =",E17.6)
 1022 FORMAT(" THE ESTIMATED LOSS OF BASE",I4,
     1  " SIGNIFICANT DIGITS IS",F7.2)
 1023 FORMAT(" THE ROOT MEAN SQUARE RELATIVE ERROR WAS",E15.4,
     1    " = ",I4," **",F7.2)
 1025 FORMAT("SPECIAL TESTS")
 1030 FORMAT(" THE IDENTITY  DLOG(X) = -DLOG(1/X)  WILL BE TESTED.",
     1      8X,"X",9X,"F(X) + F(1/X)")
 1040 FORMAT(" TEST OF SPECIAL ARGUMENTS ")
 1041 FORMAT(" DLOG(1.0) = ",E15.7)
 1042 FORMAT(" DLOG(XMIN) = DLOG(",E15.7,") = ",E15.7)
 1043 FORMAT(" DLOG(XMAX) = DLOG(",E15.7,") = ",E15.7)
 1050 FORMAT("TEST OF ERROR RETURNS")
 1052 FORMAT(" DLOG WILL BE CALLED WITH THE ARGUMENT",E15.4,
     1       " THIS SHOULD TRIGGER AN ERROR MESSAGE")
 1055 FORMAT(" DLOG RETURNED THE VALUE",E15.4)
 1060 FORMAT(2E15.7)
 1100 FORMAT(" THIS CONCLUDES THE TESTS " )
C     ---------- LAST CARD OF DLOG/DLOG10 TEST PROGRAM ----------
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
