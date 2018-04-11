C     PROGRAM TO TEST SIN/COS
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
C                 BE DELETED PROVIDED THE FOLLOWING FIVE
C                 PARAMETERS ARE ASSIGNED THE VALUES INDICATED
C
C                 IBETA  - THE RADIX OF THE FLOATING-POINT SYSTEM
C                 IT     - THE NUMBER OF BASE-IBETA DIGITS IN THE
C                          SIGNIFICAND OF A FLOATING-POINT NUMBER
C                 MINEXP - THE LARGEST IN MAGNITUDE NEGATIVE
C                          INTEGER SUCH THAT  FLOAT(IBETA)**MINEXP
C                          IS A POSITIVE FLOATING-POINT NUMBER
C                 EPS    - THE SMALLEST POSITIVE FLOATING-POINT
C                          NUMBER SUCH THAT 1.0+EPS .NE. 1.0
C                 EPSNEG - THE SMALLEST POSITIVE FLOATING-POINT
C                          NUMBER SUCH THAT 1.0-EPSNEG .NE. 1.0
C
C        REN(K) - A FUNCTION SUBPROGRAM RETURNING RANDOM REAL
C                 NUMBERS UNIFORMLY DISTRIBUTED OVER (0,1)
C
C
C     STANDARD FORTRAN SUBPROGRAMS REQUIRED
C
C         ABS, ALOG, AMAX1, COS, FLOAT, SIN, SQRT
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
      REAL A,AIT,ALBETA,B,BETA,BETAP,C,DEL,EPS,EPSNEG,EXPON,ONE,REN,
     1     R6,R7,THREE,W,X,XL,XMAX,XMIN,XN,X1,Y,Z,ZERO,ZZ
C
      IOUT = 6
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = FLOAT(IBETA)
      ALBETA = ALOG(BETA)
      AIT = FLOAT(IT)
      ONE = 1.0E0
      ZERO = 0.0E0
      THREE = 3.0E0
      A = ZERO
      B = 1.570796327E0
      C = B
      N = 2000
      XN = FLOAT(N)
      I1 = 0
C-----------------------------------------------------------------
C     RANDOM ARGUMENT ACCURACY TESTS
C-----------------------------------------------------------------
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
            Y = X / THREE
            Y = (X + Y) - X
            X = THREE * Y
            IF (J .EQ. 3) GO TO 100
            Z = SIN(X)
            ZZ = SIN(Y)
            W = ONE
            IF (Z .NE. ZERO) W = (Z - ZZ*(THREE-4.0E0*ZZ*ZZ)) / Z
            GO TO 110
  100       Z = COS(X)
            ZZ = COS(Y)
            W = ONE
            IF (Z .NE. ZERO) W = (Z + ZZ*(THREE-4.0E0*ZZ*ZZ)) / Z
  110       IF (W .GT. ZERO) K1 = K1 + 1
            IF (W .LT. ZERO) K3 = K3 + 1
            W = ABS(W)
            IF (W .LE. R6) GO TO 120
            R6 = W
            X1 = X
  120       R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         IF (J .EQ. 3) GO TO 210
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
  210    WRITE (IOUT,1005)
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,*)
         WRITE (IOUT,*)
         WRITE (IOUT,1012) K1,K2,K3
         WRITE (IOUT,*)
         WRITE (IOUT,*)
  220    WRITE (IOUT,1020) IT,IBETA
         WRITE (IOUT,*)
         WRITE (IOUT,*)
         W = -999.0E0
         IF (R6 .NE. ZERO) W = ALOG(ABS(R6))/ALBETA
         WRITE (IOUT,1021) R6,IBETA,W,X1
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         W = AMAX1(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         W = -999.0E0
         IF (R7 .NE. ZERO) W = ALOG(ABS(R7))/ALBETA
         WRITE (IOUT,1023) R7,IBETA,W
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         W = AMAX1(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         A = 18.84955592E0
         IF (J .EQ. 2) A = B + C
         B = A + C
  300 CONTINUE
C-----------------------------------------------------------------
C     SPECIAL TESTS
C-----------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      C = ONE / BETA ** (IT/2)
      Z = (SIN(A+C) - SIN(A-C)) / (C + C)
      WRITE (IOUT,1026) Z
      WRITE (IOUT,*)
      WRITE (IOUT,*)

C
      WRITE (IOUT,1030)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

C
      DO 320 I = 1, 5
         X = REN(I1) * A
         Z = SIN(X) + SIN(-X)
         WRITE (IOUT,1060) X, Z
         WRITE (IOUT,*)
         WRITE (IOUT,*)

  320 CONTINUE
C
      WRITE (IOUT,1031)
      WRITE (IOUT,*)
      WRITE (IOUT,*)


      BETAP = BETA ** IT
      X = REN(I1) / BETAP
C
      DO 330 I = 1, 5
         Z = X - SIN(X)
         WRITE (IOUT,1060) X, Z
         WRITE (IOUT,*)
         WRITE (IOUT,*)

         X = X / BETA
  330 CONTINUE
C
      WRITE (IOUT,1032)
      WRITE (IOUT,*)
      WRITE (IOUT,*)


C
      DO 340 I = 1, 5
         X = REN(I1) * A
         Z = COS(X) - COS(-X)
         WRITE (IOUT,1060) X, Z
         WRITE (IOUT,*)
         WRITE (IOUT,*)

  340 CONTINUE
C
      WRITE (IOUT,1035)
      WRITE (IOUT,*)
      WRITE (IOUT,*)


      EXPON = FLOAT(MINEXP) * 0.75E0
      X = BETA ** EXPON
      Y = SIN(X)
      WRITE (IOUT,1061) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      WRITE (IOUT,1040)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      Z = SQRT(BETAP)
      X = Z * (ONE - EPSNEG)
      Y = SIN(X)
      WRITE (IOUT,1061) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      Y = SIN(Z)
      WRITE (IOUT,1061) Z, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = Z * (ONE + EPS)
      Y = SIN(X)
      WRITE (IOUT,1061) X, Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

C-----------------------------------------------------------------
C     TEST OF ERROR RETURNS
C-----------------------------------------------------------------
      WRITE (IOUT,1050)
      WRITE (IOUT,*)
      WRITE (IOUT,*)

      X = BETAP
      WRITE (IOUT,1052) X
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#if !defined(CRAY_PVP)
      Y = SIN(X)
      WRITE (IOUT,1055) Y
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#else
      WRITE(IOUT,*)'DISABLED FOR CRAYS'
      WRITE (IOUT,*)
      WRITE (IOUT,*)

#endif
      WRITE (IOUT,1100)
      STOP
 1000 FORMAT("TEST OF SIN(X) VS 3*SIN(X/3)-4*SIN(X/3)**3  ")
 1005 FORMAT("TEST OF COS(X) VS 4*COS(X/3)**3-3*COS(X/3)  ")
 1010 FORMAT(I7," RANDOM ARGUMENTS WERE TESTED FROM THE INTERVAL ",
     1 6X,"(",E15.4,",",E15.4,")")
 1011 FORMAT(" SIN(X) WAS LARGER",I6," TIMES,",
     1     11X," AGREED",I6," TIMES, AND ",
     1   7X,"WAS SMALLER",I6," TIMES.")
 1012 FORMAT(" COS(X) WAS LARGER",I6," TIMES", 
     1     11X," AGREED",I6," TIMES, AND ",
     1   7X,"WAS SMALLER",I6," TIMES.")
 1020 FORMAT(" THERE ARE",I4," BASE",I4,
     1    " SIGNIFICANT DIGITS IN A FLOATING-POINT NUMBER  ")
 1021 FORMAT(" THE MAXIMUM RELATIVE ERROR OF",E15.4," = ",I4," **",
     1  F7.2,4X,"OCCURRED FOR X =",E17.6)
 1022 FORMAT(" THE ESTIMATED LOSS OF BASE",I4,
     1  " SIGNIFICANT DIGITS IS",F7.2)
 1023 FORMAT(" THE ROOT MEAN SQUARE RELATIVE ERROR WAS",E15.4,
     1    " = ",I4," **",F7.2)
 1025 FORMAT("SPECIAL TESTS")
 1026 FORMAT(" IF ",E13.6," IS NOT ALMOST 1.0E0,",
     1   4X,"SIN HAS THE WRONG PERIOD.  ")
 1030 FORMAT(" THE IDENTITY   SIN(-X) = -SIN(X)   WILL BE TESTED.",
     1      8X,"X",9X,"F(X) + F(-X)")
 1031 FORMAT(" THE IDENTITY SIN(X) = X , X SMALL, WILL BE TESTED.",
     1      8X,"X",9X,"X - F(X)")
 1032 FORMAT(" THE IDENTITY   COS(-X) = COS(X)   WILL BE TESTED.",
     1      8X,"X",9X,"F(X) - F(-X)")
 1035 FORMAT(" TEST OF UNDERFLOW FOR VERY SMALL ARGUMENT.")
 1040 FORMAT(" THE FOLLOWING THREE LINES ILLUSTRATE THE LOSS IN",
     1  " SIGNIFICANCE FOR LARGE ARGUMENTS.  THE ARGUMENTS",
     2  " ARE CONSECUTIVE.")
 1050 FORMAT("TEST OF ERROR RETURNS")
 1052 FORMAT(" SIN WILL BE CALLED WITH THE ARGUMENT",E15.4
     1       " THIS SHOULD TRIGGER AN ERROR MESSAGE")
 1055 FORMAT(" SIN RETURNED THE VALUE",E15.4)
 1060 FORMAT(2E15.7)
 1061 FORMAT(6X," SIN(",E13.6,") =",E13.6)
 1100 FORMAT(" THIS CONCLUDES THE TESTS ")
C     ---------- LAST CARD OF SIN/COS TEST PROGRAM ----------
      END
      REAL FUNCTION REN(K)
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
      REN = FLOAT(IY) / 2796203.0E0
      RETURN
C     ---------- LAST CARD OF RAN ----------
      END
