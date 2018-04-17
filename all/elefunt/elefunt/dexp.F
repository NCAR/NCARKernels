C     PROGRAM TO TEST DEXP
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
C         DABS, DINT, DLOG, DMAX1, DEXP, DFLOAT, DSQRT
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
     1     A,AIT,ALBETA,B,BETA,D,DEL,EPS,EPSNEG,ONE,REN,R6,R7,
     2     TWO,TEN,V,W,X,XL,XMAX,XMIN,XN,X1,Y,Z,ZERO,ZZ
C
      IOUT = 6
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IDEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = DFLOAT(IBETA)
      ALBETA = DLOG(BETA)
      AIT = DFLOAT(IT)
      ONE = 1.0D0
      TWO = 2.0D0
      TEN = 10.0D0
      ZERO = 0.0D0
      V = 0.0625D0
      A = TWO
      B = DLOG(A) * 0.5D0
      A = -B + V
      D = DLOG(0.9D0*XMAX)
      N = 2000
      XN = DFLOAT(N)
      I1 = 0
C---------------------------------------------------------------------
C     RANDOM ARGUMENT ACCURACY TESTS
C---------------------------------------------------------------------
      DO 300 J = 1, 3
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
C---------------------------------------------------------------------
C     PURIFY ARGUMENTS
C---------------------------------------------------------------------
            Y = X - V
            IF (Y .LT. ZERO) X = Y + V
            Z = DEXP(X)
            ZZ = DEXP(Y)
            IF (J .EQ. 1) GO TO 100
            IF (IBETA .NE. 10) Z = Z * .0625D0 -
     1                             Z * 2.4453321046920570389D-3
            IF (IBETA .EQ. 10) Z = Z * 6.0D-2 +
     1                             Z * 5.466789530794296106D-5
            GO TO 110
  100       Z = Z - Z * 6.058693718652421388D-2
  110       W = ONE
            IF (ZZ .NE. ZERO) W = (Z - ZZ) / ZZ
            IF (W .LT. ZERO) K1 = K1 + 1
            IF (W .GT. ZERO) K3 = K3 + 1
            W = DABS(W)
            IF (W .LE. R6) GO TO 120
            R6 = W
            X1 = X
  120       R7 = R7 + W*W
            XL = XL + DEL
  200    CONTINUE
C
         K2 = N - K3 - K1
         R7 = DSQRT(R7/XN)
         WRITE (IOUT,1000) V, V
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1020) IT,IBETA
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         W = -999.0D0
         IF (R6 .NE. ZERO) W = DLOG(DABS(R6))/ALBETA
         WRITE (IOUT,1021) R6,IBETA,W,X1
         WRITE (IOUT,*)
         WRITE (IOUT,*)

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

         IF (J .EQ. 2) GO TO 270
         V = 45.0D0 / 16.0D0
         A = -TEN * B
         B = 4.0D0 * XMIN * BETA ** IT
         B = DLOG(B)
         GO TO 300
  270    A = -TWO * A
         B = TEN * A
         IF (B .LT. D) B = D
  300 CONTINUE
C---------------------------------------------------------------------
C     SPECIAL TESTS
C---------------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      WRITE (IOUT,1030)
      WRITE (IOUT,*)
      WRITE (IOUT,*)
C
      DO 320 I = 1, 5
         X = REN(I1) * BETA
         Y = -X
         Z = DEXP(X) * DEXP(Y) - ONE
         WRITE (IOUT,1060) X, Z
  320 CONTINUE
C
      WRITE (IOUT,1040)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = ZERO
      Y = DEXP(X) - ONE
      WRITE (IOUT,1041) Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = DINT(DLOG(XMIN))
      Y = DEXP(X)
      WRITE (IOUT,1042) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = DINT(DLOG(XMAX))
      Y = DEXP(X)
      WRITE (IOUT,1042) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = X / TWO
      V = X / TWO
      Y = DEXP(X)
      Z = DEXP (V)
      Z = Z * Z
      WRITE (IOUT,1043) X, Y, V, Z
      WRITE (IOUT,*)
      WRITE (IOUT,*)

C---------------------------------------------------------------------
C     TEST OF ERROR RETURNS
C---------------------------------------------------------------------
      WRITE (IOUT,1050)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = -ONE / DSQRT(XMIN)
      WRITE (IOUT,1052) X
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      Y = DEXP(X)
      WRITE (IOUT,1061) Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = -X
      WRITE (IOUT,1052) X
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      Y = DEXP(X)
      WRITE (IOUT,1061) Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      WRITE (IOUT,1100)
      STOP
 1000 FORMAT("TEST OF DEXP(X-",F7.4,") VS DEXP(X)/DEXP(",F7.4,") ")
 1010 FORMAT(I7," RANDOM ARGUMENTS WERE TESTED FROM THE INTERVAL ",
     1 6X,"(",E15.4,",",E15.4,")")
 1011 FORMAT(" DEXP(X-V) WAS LARGER",I6," TIMES,",
     1     13X," AGREED",I6," TIMES, AND ",
     2      9X," WAS SMALLER",I6," TIMES.")
 1020 FORMAT(" THERE ARE",I4," BASE",I4,
     1    " SIGNIFICANT DIGITS IN A FLOATING-POINT NUMBER  ")
 1021 FORMAT(" THE MAXIMUM RELATIVE ERROR OF",E15.4," = ",I4," **",
     1  F7.2,4X,"OCCURRED FOR X =",E17.6)
 1022 FORMAT(" THE ESTIMATED LOSS OF BASE",I4,
     1  " SIGNIFICANT DIGITS IS",F7.2)
 1023 FORMAT(" THE ROOT MEAN SQUARE RELATIVE ERROR WAS",E15.4,
     1    " = ",I4," **",F7.2)
 1025 FORMAT("SPECIAL TESTS")
 1030 FORMAT("THE IDENTITY  DEXP(X)*DEXP(-X) = 1.0  WILL BE TESTED.",
     1      8X,"X",9X,"F(X)*F(-X) - 1 ")
 1040 FORMAT(" TEST OF SPECIAL ARGUMENTS ")
 1041 FORMAT(" DEXP(0.0) - 1.0E0 = ",E15.7)
 1042 FORMAT(" DEXP(",E13.6,") =",E13.6)
 1043 FORMAT("IF DEXP(",E13.6,") = ",E13.6," IS NOT ABOUT ",
     1 " DEXP(",E13.6,")**2 = ",E13.6," THERE IS AN ARG RED ERROR")
 1050 FORMAT("TEST OF ERROR RETURNS  ")
 1052 FORMAT("DEXP WILL BE CALLED WITH THE ARGUMENT",E15.4,
     1       " THIS SHOULD TRIGGER AN ERROR MESSAGE")
 1060 FORMAT(2E15.7)
 1061 FORMAT(" DEXP RETURNED THE VALUE",E15.4)
 1100 FORMAT(" THIS CONCLUDES THE TESTS ")
C     ---------- LAST CARD OF DEXP TEST PROGRAM ----------
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
