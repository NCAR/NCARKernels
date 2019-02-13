!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:29 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: random_seq_mod.f90 11864 2017-08-02 19:40:28Z nancy@ucar.edu $
!> Random number and random sequence routines.  Can generate random draws 
!> from a uniform distribution or random draws from differently shaped
!> distributions (e.g. gaussian, gamma, exponential)


!


module random_seq_mod
! This module now contains both the original contents and the routines
! which used to be in a separate random_nr module.


    USE types_mod, ONLY: i8, r8 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC random_seq_type 
! version controlled file description for error handling, do not edit

! Gives ability to generate unique repeatable sequences of random numbers
! using random congruential package. Needed to allow different assim algorithms
! that require random numbers to see identical observational sequences.
! Used to give different sequences a different but repeatable start
! There may be problems with incestuous series here; be cautious of this
! in the future.


! the following routines were transcribed from C to F90, originally
! from the GNU scientific library:  init_ran, ran_unif, ran_gauss,
! ran_gamma


integer, parameter :: N = 624   ! period parameters
! hexadecimal constants


type random_seq_type
   private
   integer     :: mti
   integer(i8) :: mt(0:N-1)
   real(r8)    :: lastg
   logical     :: gset
end type random_seq_type

PUBLIC kr_random_seq_mod_random_seq_type 
PUBLIC kv_random_seq_mod_random_seq_type 


!========================================================================
! Public entry points
!========================================================================
!------------------------------------------------------------------------
!> An integer seed can be used to get a particular repeatable sequence.
  
CONTAINS 
  


!------------------------------------------------------------------------
!> return a random draw from a uniform distribution
!> between 0.0 and 1.0


!------------------------------------------------------------------------
!> return a random draw from a gaussian distribution
!> with the specified mean and standard deviation.


!------------------------------------------------------------------------
!> return multiple random draws from a gaussian distribution
!> with the specified mean and standard deviation.


!------------------------------------------------------------------------
!> return a random draw from a 2D multivariate gaussian distribution
!> with the specified 2 means and covariances.  


!------------------------------------------------------------------------
!> return a random draw from a gamma distribution 
!> with the specified shape and scale parameter, often
!> denoted with the symbols kappa and theta.
!>
!> note that there are multiple common formulations of
!> the second parameter to this function.  if you have
!> 'rate' instead of 'scale' specify:  1.0 / rate  
!> for the scale parameter.  shape and rate are often 
!> denoted with the symbols alpha and beta.
!>
!> The distribution mean should be: shape * rate, 
!> and the variance: shape * (rate^2).


!------------------------------------------------------------------------
!> return a random draw from an inverse gamma distribution 
!> with the specified shape and scale parameter.
!> NOTE THIS IS DIFFERENT FROM THE RANDOM GAMMA FORUMULATION.
!> gamma uses a rate parameter, inverse gamma uses scale.
!> if you have 'rate' instead of 'scale', specify 1.0 / rate


!------------------------------------------------------------------------
!> return a random draw from an exponential distribution with
!> the specified rate parameter lambda.  if you have a scale parameter,
!> also called the mean, standard deviation, or survival parameter,
!> specify 1.0 / scale for rate.


!-------------------------------------------------------------------
!========================================================================
! Private, internal routines below here
!========================================================================
!-------------------------------------------------------------------


!-------------------------------------------------------------------
!> A random congruential random number generator of the Mersenne Twister type
!> See GNU Scientific Library code.


!-----------------------------------------------------------------
!> A random congruential random number generator from
!> the GNU Scientific Library (The Mersenne Twister MT19937 varient.)


!------------------------------------------------------------------------
!> Polar (Box-Mueller) method; See Knuth v2, 3rd ed, p122 
!> Returns a N(-1, 1) random number draw from a gaussian distribution


!-------------------------------------------------------------------
!> Gamma(a,b) generator from the GNU Scientific library.
!>
!> There are three different parameterizations in common use:
!>  1. With a shape parameter k and a scale parameter θ.
!>  2. With a shape parameter α = k and an inverse scale parameter β = 1/θ, called a rate parameter.
!>  3. With a shape parameter k and a mean parameter μ = k/β.
!>
!> warning!
!> this is the private, internal interface and it uses the first
!> parameterization, kappa and theta, like the original GSL code.
!> our public interfaces use the words 'shape', 'scale' and 'rate'
!> to try to be more clear about which parameterization we're using.
!>
!> this internal routine assumes the caller has already verified that
!> the shape and scale are positive.


!------------------------------------------------------------------------


!read state subroutine for kr_random_seq_mod_random_seq_type 
RECURSIVE SUBROUTINE kr_random_seq_mod_random_seq_type(var, kgen_unit, printname, printvar) 
    TYPE(random_seq_type), INTENT(INOUT) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN) :: printname 
    LOGICAL, INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) var%mti 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%mti = ", var%mti 
    END IF   
      
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) var%mt 
        CALL kgen_array_sumcheck(printname // "%mt", kgen_array_sum, DBLE(SUM(var%mt, mask=(var%mt .eq. var%mt))), .TRUE.) 
        IF (PRESENT( printvar ) .AND. printvar) THEN 
            WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%mt)) = ", DBLE(SUM(var%mt, mask=(var%mt .eq. var%mt))) 
        END IF   
    END IF   
      
    READ (UNIT = kgen_unit) var%lastg 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%lastg = ", var%lastg 
    END IF   
      
    READ (UNIT = kgen_unit) var%gset 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%gset = ", var%gset 
    END IF   
      
END SUBROUTINE kr_random_seq_mod_random_seq_type 
  
!verify state subroutine for kv_random_seq_mod_random_seq_type 
RECURSIVE SUBROUTINE kv_random_seq_mod_random_seq_type(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    TYPE(random_seq_type), INTENT(IN) :: var, kgenref_var 
    TYPE(check_t) :: dtype_check_status, comp_check_status 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    integer :: diff_mti 
    INTEGER :: n_mt 
    integer(KIND=i8) :: nrmsdiff_mt, rmsdiff_mt 
    integer(KIND=i8), ALLOCATABLE :: buf1_mt(:), buf2_mt(:) 
    real(KIND=r8) :: diff_lastg 
    logical :: diff_gset 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%mti == kgenref_var%mti) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%mti is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_mti = ABS(var%mti - kgenref_var%mti) 
        IF (diff_mti <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%mti is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%mti is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_mti 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_mti 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (ALL(var%mt == kgenref_var%mt)) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%mt is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        ALLOCATE (buf1_mt(SIZE(var%mt,dim=1))) 
        ALLOCATE (buf2_mt(SIZE(var%mt,dim=1))) 
        n_mt = COUNT(var%mt /= kgenref_var%mt) 
        WHERE ( ABS(kgenref_var%mt) > kgen_minvalue ) 
            buf1_mt = ((var%mt-kgenref_var%mt)/kgenref_var%mt)**2 
            buf2_mt = (var%mt-kgenref_var%mt)**2 
        ELSEWHERE 
            buf1_mt = (var%mt-kgenref_var%mt)**2 
            buf2_mt = buf1_mt 
        END WHERE   
        nrmsdiff_mt = SQRT(SUM(buf1_mt)/REAL(n_mt)) 
        rmsdiff_mt = SQRT(SUM(buf2_mt)/REAL(n_mt)) 
        IF (rmsdiff_mt > kgen_tolerance) THEN 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%mt is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        ELSE 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%mt is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) count( var%mt /= kgenref_var%mt), " of ", size( var%mt ), " elements are different." 
                WRITE (*, *) "Average - kernel ", sum(var%mt)/real(size(var%mt)) 
                WRITE (*, *) "Average - reference ", sum(kgenref_var%mt)/real(size(kgenref_var%mt)) 
                WRITE (*, *) "RMS of difference is ", rmsdiff_mt 
                WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mt 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) count( var%mt /= kgenref_var%mt), " of ", size( var%mt ), " elements are different." 
                WRITE (*, *) "Average - kernel ", sum(var%mt)/real(size(var%mt)) 
                WRITE (*, *) "Average - reference ", sum(kgenref_var%mt)/real(size(kgenref_var%mt)) 
                WRITE (*, *) "RMS of difference is ", rmsdiff_mt 
                WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mt 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%lastg == kgenref_var%lastg) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%lastg is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_lastg = ABS(var%lastg - kgenref_var%lastg) 
        IF (diff_lastg <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%lastg is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%lastg is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_lastg 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_lastg 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%gset .EQV. kgenref_var%gset) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%gset is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%gset is NOT IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_OUT_TOL 
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "NOT IMPLEMENTED YET" 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "NOT IMPLEMENTED YET" 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
        check_status%numIdentical = check_status%numIdentical + 1 
    ELSE IF (dtype_check_status%numOutTol > 0) THEN 
        check_status%numOutTol = check_status%numOutTol + 1 
    ELSE IF (dtype_check_status%numInTol > 0) THEN 
        check_status%numInTol = check_status%numInTol + 1 
    END IF   
END SUBROUTINE kv_random_seq_mod_random_seq_type 
  
end module random_seq_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/random_seq_mod.f90 $
! $Id: random_seq_mod.f90 11864 2017-08-02 19:40:28Z nancy@ucar.edu $
! $Revision: 11864 $
! $Date: 2017-08-02 13:40:28 -0600 (Wed, 02 Aug 2017) $
