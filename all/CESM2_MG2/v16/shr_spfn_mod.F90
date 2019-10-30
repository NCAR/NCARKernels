!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  
! Define flags for compilers supporting Fortran 2008 intrinsics
! HAVE_GAMMA_INTRINSICS: gamma and log_gamma
! HAVE_ERF_INTRINSICS: erf, erfc, and erfc_scaled
! erfc_scaled(x) = (exp(x**2)*erfc(x))
! Use this flag for compilers that don't have real intrinsics, but link in
! a library for you.
! HAVE_ERF_EXTERNALS: erf and erfc
! These compilers have the intrinsics.
! Intel also has them (and Cray), but as of mid-2015, our implementation is
! actually faster, in part because they do not properly vectorize, so we
! pretend that the compiler version doesn't exist.
! PGI has external erf/derf and erfc/derfc, and will link them for you, but
! it does not consider them "intrinsics" right now.
! As of 5.3.1, NAG does not have any of these.


module shr_spfn_mod
  ! Module for common mathematical functions
  ! This #ifdef is to allow the module to be compiled with no dependencies,
  ! even on shr_kind_mod.

    USE shr_kind_mod, ONLY: rkind_comp
    USE shr_const_mod, ONLY: pi => shr_const_pi 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 
  ! Error functions


  ! Gamma functions
  ! Note that we lack an implementation of log_gamma, but we do have an
  ! implementation of the upper incomplete gamma function, which is not in
  ! Fortran 2008.
  ! Note also that this gamma function is only for double precision. We
  ! haven't needed an r4 version yet.


    PUBLIC shr_spfn_gamma 

  interface shr_spfn_gamma
     module procedure shr_spfn_gamma_rkind_comp
  end interface shr_spfn_gamma
  ! Mathematical constants
  ! sqrt(pi)

  ! Define machine-specific constants needed in this module.
  ! These were used by the original gamma and calerf functions to guarantee
  ! safety against overflow, and precision, on many different machines.
  ! By defining the constants in this way, we assume that 1/xmin is
  ! representable (i.e. does not overflow the real type). This assumption was
  ! not in the original code, but is valid for IEEE single and double
  ! precision.
  ! Double precision
  !---------------------------------------------------------------------
  ! Machine epsilon


  real(rkind_comp), parameter :: epsrkind_comp = epsilon(1._rkind_comp)
  ! "Huge" value is returned when actual value would be infinite.
  real(rkind_comp), parameter :: xinfrkind_comp = huge(1._rkind_comp)
  ! Smallest normal value.
  real(rkind_comp), parameter :: xminrkind_comp = tiny(1._rkind_comp)
  ! Largest number that, when added to 1., yields 1.
  ! Largest argument for which erfcx > 0.
  ! Single precision
  !---------------------------------------------------------------------
  ! Machine epsilon

  ! "Huge" value is returned when actual value would be infinite.
  ! Smallest normal value.
  ! Largest number that, when added to 1., yields 1.
  ! Largest argument for which erfcx > 0.
  ! For gamma/igamma
  ! Approximate value of largest acceptable argument to gamma,
  ! for IEEE double-precision.


  real(rkind_comp), parameter :: xbig_gamma = 171.624_rkind_comp

contains
  ! Wrapper functions for erf


  ! Wrapper functions for erfc


  ! Wrapper functions for erfc_scaled


  elemental function shr_spfn_gamma_rkind_comp(x) result(res)
    !$acc routine vector
    real(rkind_comp), intent(in) :: x
    real(rkind_comp) :: res
    ! No intrinsic

    res = shr_spfn_gamma_nonintrinsic_rkind_comp(x)

  end function shr_spfn_gamma_rkind_comp
  !------------------------------------------------------------------
  ! 6 December 2006 -- B. Eaton
  ! The following comments are from the original version of CALERF.
  ! The only changes in implementing this module are that the function
  ! names previously used for the single precision versions have been
  ! adopted for the new generic interfaces.  To support these interfaces
  ! there is now both a single precision version (calerf_r4) and a
  ! double precision version (calerf_rkind_comp) of CALERF below.  These versions
  ! are hardcoded to use IEEE arithmetic.
  !------------------------------------------------------------------
  ! This packet evaluates  erf(x),  erfc(x),  and  exp(x*x)*erfc(x)
  !   for a real argument  x.  It contains three FUNCTION type
  !   subprograms: ERF, ERFC, and ERFCX (or ERF_R8, ERFC_R8, and ERFCX_R8),
  !   and one SUBROUTINE type subprogram, CALERF.  The calling
  !   statements for the primary entries are:
  !                   Y=ERF(X)     (or   Y=ERF_R8(X)),
  !                   Y=ERFC(X)    (or   Y=ERFC_R8(X)),
  !   and
  !                   Y=ERFCX(X)   (or   Y=ERFCX_R8(X)).
  !   The routine  CALERF  is intended for internal packet use only,
  !   all computations within the packet being concentrated in this
  !   routine.  The function subprograms invoke  CALERF  with the
  !   statement
  !          CALL CALERF(ARG,RESULT,JINT)
  !   where the parameter usage is as follows
  !      Function                     Parameters for CALERF
  !       call              ARG                  Result          JINT
  !     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0
  !     ERFC(ARG)     ABS(ARG) .LT. XBIG        ERFC(ARG)         1
  !     ERFCX(ARG)    XNEG .LT. ARG .LT. XMAX   ERFCX(ARG)        2
  !   The main computation evaluates near-minimax approximations
  !   from "Rational Chebyshev approximations for the error function"
  !   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This
  !   transportable program uses rational functions that theoretically
  !   approximate  erf(x)  and  erfc(x)  to at least 18 significant
  !   decimal digits.  The accuracy achieved depends on the arithmetic
  !   system, the compiler, the intrinsic functions, and proper
  !   selection of the machine-dependent constants.
  !*******************************************************************
  !*******************************************************************
  ! Explanation of machine-dependent constants
  !   XMIN   = the smallest positive floating-point number.
  !   XINF   = the largest positive finite floating-point number.
  !   XNEG   = the largest negative argument acceptable to ERFCX;
  !            the negative of the solution to the equation
  !            2*exp(x*x) = XINF.
  !   XSMALL = argument below which erf(x) may be represented by
  !            2*x/sqrt(pi)  and above which  x*x  will not underflow.
  !            A conservative value is the largest machine number X
  !            such that   1.0 + X = 1.0   to machine precision.
  !   XBIG   = largest argument acceptable to ERFC;  solution to
  !            the equation:  W(x) * (1-0.5/x**2) = XMIN,  where
  !            W(x) = exp(-x*x)/[x*sqrt(pi)].
  !   XHUGE  = argument above which  1.0 - 1/(2*x*x) = 1.0  to
  !            machine precision.  A conservative value is
  !            1/[2*sqrt(XSMALL)]
  !   XMAX   = largest acceptable argument to ERFCX; the minimum
  !            of XINF and 1/[sqrt(pi)*XMIN].
  !   Approximate values for some important machines are:
  !                          XMIN       XINF        XNEG     XSMALL
  !  CDC 7600      (S.P.)  3.13E-294   1.26E+322   -27.220  7.11E-15
  !  CRAY-1        (S.P.)  4.58E-2467  5.45E+2465  -75.345  7.11E-15
  !  IEEE (IBM/XT,
  !    SUN, etc.)  (S.P.)  1.18E-38    3.40E+38     -9.382  5.96E-8
  !  IEEE (IBM/XT,
  !    SUN, etc.)  (D.P.)  2.23D-308   1.79D+308   -26.628  1.11D-16
  !  IBM 195       (D.P.)  5.40D-79    7.23E+75    -13.190  1.39D-17
  !  UNIVAC 1108   (D.P.)  2.78D-309   8.98D+307   -26.615  1.73D-18
  !  VAX D-Format  (D.P.)  2.94D-39    1.70D+38     -9.345  1.39D-17
  !  VAX G-Format  (D.P.)  5.56D-309   8.98D+307   -26.615  1.11D-16
  !                          XBIG       XHUGE       XMAX
  !  CDC 7600      (S.P.)  25.922      8.39E+6     1.80X+293
  !  CRAY-1        (S.P.)  75.326      8.39E+6     5.45E+2465
  !  IEEE (IBM/XT,
  !    SUN, etc.)  (S.P.)   9.194      2.90E+3     4.79E+37
  !  IEEE (IBM/XT,
  !    SUN, etc.)  (D.P.)  26.543      6.71D+7     2.53D+307
  !  IBM 195       (D.P.)  13.306      1.90D+8     7.23E+75
  !  UNIVAC 1108   (D.P.)  26.582      5.37D+8     8.98D+307
  !  VAX D-Format  (D.P.)   9.269      1.90D+8     1.70D+38
  !  VAX G-Format  (D.P.)  26.569      6.71D+7     8.98D+307
  !*******************************************************************
  !*******************************************************************
  ! Error returns
  !  The program returns  ERFC = 0      for  ARG .GE. XBIG;
  !                       ERFCX = XINF  for  ARG .LT. XNEG;
  !      and
  !                       ERFCX = 0     for  ARG .GE. XMAX.
  ! Intrinsic functions required are:
  !     ABS, AINT, EXP
  !  Author: W. J. Cody
  !          Mathematics and Computer Science Division
  !          Argonne National Laboratory
  !          Argonne, IL 60439
  !  Latest modification: March 19, 1990
  !------------------------------------------------------------------

  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !


  !------------------------------------------------------------------------------------------


  !------------------------------------------------------------------------------------------
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


  pure function shr_spfn_gamma_nonintrinsic_rkind_comp(X) result(gamma)
    !$acc routine vector

    !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    ! 7 Feb 2013 -- S. Santos
    ! The following comments are from the original version. Changes have
    ! been made to update syntax and allow inclusion into this module.
    !----------------------------------------------------------------------
    ! THIS ROUTINE CALCULATES THE GAMMA FUNCTION FOR A REAL ARGUMENT X.
    !   COMPUTATION IS BASED ON AN ALGORITHM OUTLINED IN REFERENCE 1.
    !   THE PROGRAM USES RATIONAL FUNCTIONS THAT APPROXIMATE THE GAMMA
    !   FUNCTION TO AT LEAST 20 SIGNIFICANT DECIMAL DIGITS.  COEFFICIENTS
    !   FOR THE APPROXIMATION OVER THE INTERVAL (1,2) ARE UNPUBLISHED.
    !   THOSE FOR THE APPROXIMATION FOR X .GE. 12 ARE FROM REFERENCE 2.
    !   THE ACCURACY ACHIEVED DEPENDS ON THE ARITHMETIC SYSTEM, THE
    !   COMPILER, THE INTRINSIC FUNCTIONS, AND PROPER SELECTION OF THE
    !   MACHINE-DEPENDENT CONSTANTS.
    !*******************************************************************
    !*******************************************************************
    ! EXPLANATION OF MACHINE-DEPENDENT CONSTANTS
    ! BETA   - RADIX FOR THE FLOATING-POINT REPRESENTATION
    ! MAXEXP - THE SMALLEST POSITIVE POWER OF BETA THAT OVERFLOWS
    ! XBIG   - THE LARGEST ARGUMENT FOR WHICH GAMMA(X) IS REPRESENTABLE
    !          IN THE MACHINE, I.E., THE SOLUTION TO THE EQUATION
    !                  GAMMA(XBIG) = BETA**MAXEXP
    ! XINF   - THE LARGEST MACHINE REPRESENTABLE FLOATING-POINT NUMBER;
    !          APPROXIMATELY BETA**MAXEXP
    ! EPS    - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
    !          1.0+EPS .GT. 1.0
    ! XMININ - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
    !          1/XMININ IS MACHINE REPRESENTABLE
    !     APPROXIMATE VALUES FOR SOME IMPORTANT MACHINES ARE:
    !                            BETA       MAXEXP        XBIG
    ! CRAY-1         (S.P.)        2         8191        966.961
    ! CYBER 180/855
    !   UNDER NOS    (S.P.)        2         1070        177.803
    ! IEEE (IBM/XT,
    !   SUN, ETC.)   (S.P.)        2          128        35.040
    ! IEEE (IBM/XT,
    !   SUN, ETC.)   (D.P.)        2         1024        171.624
    ! IBM 3033       (D.P.)       16           63        57.574
    ! VAX D-FORMAT   (D.P.)        2          127        34.844
    ! VAX G-FORMAT   (D.P.)        2         1023        171.489
    !                            XINF         EPS        XMININ
    ! CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466
    ! CYBER 180/855
    !   UNDER NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294
    ! IEEE (IBM/XT,
    !   SUN, ETC.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38
    ! IEEE (IBM/XT,
    !   SUN, ETC.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308
    ! IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76
    ! VAX D-FORMAT   (D.P.)   1.70D+38     1.39D-17    5.88D-39
    ! VAX G-FORMAT   (D.P.)   8.98D+307    1.11D-16    1.12D-308
    !*******************************************************************
    !*******************************************************************
    ! ERROR RETURNS
    !  THE PROGRAM RETURNS THE VALUE XINF FOR SINGULARITIES OR
    !     WHEN OVERFLOW WOULD OCCUR.  THE COMPUTATION IS BELIEVED
    !     TO BE FREE OF UNDERFLOW AND OVERFLOW.
    !  INTRINSIC FUNCTIONS REQUIRED ARE:
    !     INT, DBLE, EXP, LOG, REAL, SIN
    ! REFERENCES:  AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL
    !              FUNCTIONS   W. J. CODY, LECTURE NOTES IN MATHEMATICS,
    !              506, NUMERICAL ANALYSIS DUNDEE, 1975, G. A. WATSON
    !              (ED.), SPRINGER VERLAG, BERLIN, 1976.
    !              COMPUTER APPROXIMATIONS, HART, ET. AL., WILEY AND
    !              SONS, NEW YORK, 1968.
    !  LATEST MODIFICATION: OCTOBER 12, 1989
    !  AUTHORS: W. J. CODY AND L. STOLTZ
    !           APPLIED MATHEMATICS DIVISION
    !           ARGONNE NATIONAL LABORATORY
    !           ARGONNE, IL 60439
    !----------------------------------------------------------------------

    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !

    real(rkind_comp), intent(in) :: x
    real(rkind_comp) :: gamma
    real(rkind_comp) :: fact, res, sum, xden, xnum, y, y1, ysq, z

    integer :: i, n
    logical :: negative_odd
    ! log(2*pi)/2

    real(rkind_comp), parameter :: logsqrt2pi = 0.9189385332046727417803297E0_rkind_comp
    !----------------------------------------------------------------------
    !  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
    !     APPROXIMATION OVER (1,2).
    !----------------------------------------------------------------------

    real(rkind_comp), parameter :: P(8) = &
         (/-1.71618513886549492533811E+0_rkind_comp, 2.47656508055759199108314E+1_rkind_comp, &
         -3.79804256470945635097577E+2_rkind_comp, 6.29331155312818442661052E+2_rkind_comp, &
         8.66966202790413211295064E+2_rkind_comp,-3.14512729688483675254357E+4_rkind_comp, &
         -3.61444134186911729807069E+4_rkind_comp, 6.64561438202405440627855E+4_rkind_comp /)
    real(rkind_comp), parameter :: Q(8) = &
         (/-3.08402300119738975254353E+1_rkind_comp, 3.15350626979604161529144E+2_rkind_comp, &
         -1.01515636749021914166146E+3_rkind_comp,-3.10777167157231109440444E+3_rkind_comp, &
         2.25381184209801510330112E+4_rkind_comp, 4.75584627752788110767815E+3_rkind_comp, &
         -1.34659959864969306392456E+5_rkind_comp,-1.15132259675553483497211E+5_rkind_comp /)
    !----------------------------------------------------------------------
    !  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).
    !----------------------------------------------------------------------
    real(rkind_comp), parameter :: C(7) = &
         (/-1.910444077728E-03_rkind_comp,          8.4171387781295E-04_rkind_comp, &
         -5.952379913043012E-04_rkind_comp,       7.93650793500350248E-04_rkind_comp, &
         -2.777777777777681622553E-03_rkind_comp, 8.333333333333333331554247E-02_rkind_comp, &
         5.7083835261E-03_rkind_comp /)

    negative_odd = .false.
    fact = 1._rkind_comp
    n = 0
    y = x
    if (y <= 0._rkind_comp) then
       !----------------------------------------------------------------------
       !  ARGUMENT IS NEGATIVE
       !----------------------------------------------------------------------
       y = -x
       y1 = aint(y)
       res = y - y1
       if (res /= 0._rkind_comp) then
          negative_odd = (y1 /= aint(y1*0.5_rkind_comp)*2._rkind_comp)
          fact = -pi/sin(pi*res)
          y = y + 1._rkind_comp
       else
          gamma = xinfrkind_comp
          return
       end if
    end if
    !----------------------------------------------------------------------
    !  ARGUMENT IS POSITIVE
    !----------------------------------------------------------------------
    if (y < epsrkind_comp) then
       !----------------------------------------------------------------------
       !  ARGUMENT .LT. EPS
       !----------------------------------------------------------------------
       if (y >= xminrkind_comp) then
          res = 1._rkind_comp/y
       else
          gamma = xinfrkind_comp
          return
       end if
    elseif (y < 12._rkind_comp) then
       y1 = y
       if (y < 1._rkind_comp) then
          !----------------------------------------------------------------------
          !  0.0 .LT. ARGUMENT .LT. 1.0
          !----------------------------------------------------------------------
          z = y
          y = y + 1._rkind_comp
       else
          !----------------------------------------------------------------------
          !  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY
          !----------------------------------------------------------------------
          n = int(y) - 1
          y = y - real(n, rkind_comp)
          z = y - 1._rkind_comp
       end if
       !----------------------------------------------------------------------
       !  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
       !----------------------------------------------------------------------
       xnum = 0._rkind_comp
       xden = 1._rkind_comp
       do i=1,8
          xnum = (xnum+P(i))*z
          xden = xden*z + Q(i)
       end do
       res = xnum/xden + 1._rkind_comp
       if (y1 < y) then
          !----------------------------------------------------------------------
          !  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0
          !----------------------------------------------------------------------
          res = res/y1
       elseif (y1 > y) then
          !----------------------------------------------------------------------
          !  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0
          !----------------------------------------------------------------------
          do i = 1,n
             res = res*y
             y = y + 1._rkind_comp
          end do
       end if
    else
       !----------------------------------------------------------------------
       !  EVALUATE FOR ARGUMENT .GE. 12.0,
       !----------------------------------------------------------------------
       if (y <= xbig_gamma) then
          ysq = y*y
          sum = C(7)
          do i=1,6
             sum = sum/ysq + C(i)
          end do
          sum = sum/y - y + logsqrt2pi
          sum = sum + (y-0.5_rkind_comp)*log(y)
          res = exp(sum)
       else
          gamma = xinfrkind_comp
          return
       end if
    end if
    !----------------------------------------------------------------------
    !  FINAL ADJUSTMENTS AND RETURN
    !----------------------------------------------------------------------
    if (negative_odd)  res = -res
    if (fact /= 1._rkind_comp) res = fact/res
    gamma = res
    ! ---------- LAST LINE OF GAMMA ----------
  end function shr_spfn_gamma_nonintrinsic_rkind_comp
  !! Incomplete Gamma function
  !!
  !! @author  Tianyi Fan
  !! @version August-2010


end module shr_spfn_mod
