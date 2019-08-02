!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:55 
!KGEN version : 0.8.1 
  
! A C-program for MT19937, with initialization improved 2002/1/26.
! Coded by Takuji Nishimura and Makoto Matsumoto.                 
! Code converted to Fortran 95 by José Rui Faustino de Sousa
! Date: 2002-02-01
! Enhanced version by José Rui Faustino de Sousa
! Date: 2003-04-30
! Interface:
! Kinds:
!   genrand_intg
!     Integer kind used must be at least 32 bits.
!   genrand_real
!     Real kind used 
! Types:
!   genrand_state
!     Internal representation of the RNG state.
!   genrand_srepr
!     Public representation of the RNG state. Should be used to save the RNG state.
! Procedures:
!   assignment(=)
!     Converts from type genrand_state to genrand_srepr and vice versa.
!   genrand_init
!     Internal RNG state initialization subroutine accepts either an genrand_intg integer
!     or a vector as seed or a new state using "put=" returns the present state using
!     "get=". If it is called with "get=" before being seeded with "put=" returns a state
!     initialized with a default seed.
!   genrand_int32
!     Subroutine returns an array or scalar whose elements are random integer on the
!     [0,0xffffffff] interval.
!   genrand_int31
!     Subroutine returns an array or scalar whose elements are random integer on the
!     [0,0x7fffffff] interval.
!   genrand_real1
!     Subroutine returns an array or scalar whose elements are random real on the
!     [0,1] interval.
!   genrand_real2
!     Subroutine returns an array or scalar whose elements are random real on the
!     [0,1[ interval.
!   genrand_real3
!     Subroutine returns an array or scalar whose elements are random real on the
!     ]0,1[ interval.
!   genrand_res53
!     Subroutine returns an array or scalar whose elements are random real on the
!     [0,1[ interval with 53-bit resolution.
! Before using, initialize the state by using genrand_init( put=seed )  
! This library is free software.                                  
! This library is distributed in the hope that it will be useful, 
! but WITHOUT ANY WARRANTY; without even the implied warranty of  
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            
! Copyright (C) 1997, 2002 Makoto Matsumoto and Takuji Nishimura. 
! Any feedback is very welcome.                                   
! http://www.math.keio.ac.jp/matumoto/emt.html                    
! email: matumoto@math.keio.ac.jp                                 


!
!
!


module mt95
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 
  
    IMPLICIT NONE 

    PUBLIC genrand_int32 
    PUBLIC genrand_real3 
    PRIVATE uiadd, uimlt 
    PRIVATE init_by_scalar, next_state 
    PRIVATE genrand_int32_0d, genrand_int32_1d, genrand_int32_2d, genrand_int32_3d 
    PRIVATE genrand_int32_4d, genrand_int32_5d, genrand_int32_6d, genrand_int32_7d 
    PRIVATE genrand_real3_0d, genrand_real3_1d, genrand_real3_2d, genrand_real3_3d 
    PRIVATE genrand_real3_4d, genrand_real3_5d, genrand_real3_6d, genrand_real3_7d 


  integer, public, parameter  :: genrand_intg = selected_int_kind( 9 )
  integer, public, parameter  :: genrand_real = selected_real_kind( 15 )

  integer, private, parameter :: wi = genrand_intg
  integer, private, parameter :: wr = genrand_real
  ! Period parameters   

  integer(kind=wi), private, parameter :: n = 624_wi
  integer(kind=wi), private, parameter :: m = 397_wi

  integer(kind=wi), private, parameter :: default_seed = 5489_wi

  integer(kind=wi), private, parameter :: fbs = 32_wi
  integer(kind=wi), private, parameter :: hbs = fbs / 2_wi
  integer(kind=wi), private, parameter :: qbs = hbs / 2_wi
  integer(kind=wi), private, parameter :: tbs = 3_wi * qbs

  real(kind=wr), private, parameter :: p231       = 2147483648.0_wr
  real(kind=wr), private, parameter :: p232       = 4294967296.0_wr
  real(kind=wr), private, parameter :: pi232      = 1.0_wr / p232
  real(kind=wr), private, parameter :: p231_5d232 = ( p231 + 0.5_wr ) / p232


  type, public :: genrand_state
    private
    logical(kind=wi)                :: ini = .false._wi
    integer(kind=wi)                :: cnt = n+1_wi
    integer(kind=wi), dimension(n)  :: val = 0_wi
  end type genrand_state


  type(genrand_state), private, save  :: state
  ! 23 Feb 2015: Threadprivate statement added by NCAR for CAM
!$omp threadprivate( state )


  interface genrand_int32
    module procedure genrand_int32_0d
    module procedure genrand_int32_1d
    module procedure genrand_int32_2d
    module procedure genrand_int32_3d
    module procedure genrand_int32_4d
    module procedure genrand_int32_5d
    module procedure genrand_int32_6d
    module procedure genrand_int32_7d
  end interface genrand_int32


  interface genrand_real3
    module procedure genrand_real3_0d
    module procedure genrand_real3_1d
    module procedure genrand_real3_2d
    module procedure genrand_real3_3d
    module procedure genrand_real3_4d
    module procedure genrand_real3_5d
    module procedure genrand_real3_6d
    module procedure genrand_real3_7d
  end interface genrand_real3


  PUBLIC kr_externs_in_mt95 
  PUBLIC kr_externs_out_mt95 
  PUBLIC kv_externs_mt95 
  PUBLIC kr_mt95_genrand_state 
  TYPE(genrand_state) :: kgenref_state 
  PUBLIC kv_mt95_genrand_state 

  contains

  elemental function uiadd( a, b ) result( c )

    intrinsic :: ibits, ior, ishft

    integer( kind = wi ), intent( in )  :: a, b

    integer( kind = wi )  :: c

    integer( kind = wi )  :: a1, a2, b1, b2, s1, s2

    a1 = ibits( a, 0, hbs )
    a2 = ibits( a, hbs, hbs )
    b1 = ibits( b, 0, hbs )
    b2 = ibits( b, hbs, hbs )
    s1 = a1 + b1
    s2 = a2 + b2 + ibits( s1, hbs, hbs )
    c  = ior( ishft( s2, hbs ), ibits( s1, 0, hbs ) )
    return

  end function uiadd
  

  
  elemental function uimlt( a, b ) result( c )

    intrinsic :: ibits, ior, ishft

    integer(kind=wi), intent(in)  :: a, b

    integer(kind=wi)  :: c

    integer(kind=wi)  :: a0, a1, a2, a3
    integer(kind=wi)  :: b0, b1, b2, b3
    integer(kind=wi)  :: p0, p1, p2, p3

    a0 = ibits( a, 0, qbs )
    a1 = ibits( a, qbs, qbs )
    a2 = ibits( a, hbs, qbs )
    a3 = ibits( a, tbs, qbs )
    b0 = ibits( b, 0, qbs )
    b1 = ibits( b, qbs, qbs )
    b2 = ibits( b, hbs, qbs )
    b3 = ibits( b, tbs, qbs )
    p0 = a0 * b0
    p1 = a1 * b0 + a0 * b1 + ibits( p0, qbs, tbs )
    p2 = a2 * b0 + a1 * b1 + a0 * b2 + ibits( p1, qbs, tbs )
    p3 = a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3 + ibits( p2, qbs, tbs )
    c  = ior( ishft( p1, qbs ), ibits( p0, 0, qbs ) )
    c  = ior( ishft( p2, hbs ), ibits( c, 0, hbs ) )
    c  = ior( ishft( p3, tbs ), ibits( c, 0, tbs ) )
    return

  end function uimlt


  ! initializes mt[N] with a seed

  subroutine init_by_scalar( put )

    intrinsic :: ishft, ieor, ibits

    integer(kind=wi), parameter :: mult_a = 1812433253_wi !z'6C078965'

    integer(kind=wi), intent(in)  :: put

    integer(kind=wi)  :: i

    state%ini = .true._wi
    state%val(1) = ibits( put, 0, fbs )
    do i = 2, n, 1
      state%val(i) = ieor( state%val(i-1), ishft( state%val(i-1), -30 ) )
      state%val(i) = uimlt( state%val(i), mult_a )
      state%val(i) = uiadd( state%val(i), i-1_wi )
      ! See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. 
      ! In the previous versions, MSBs of the seed affect   
      ! only MSBs of the array mt[].                        
      ! 2002/01/09 modified by Makoto Matsumoto             
      state%val(i) = ibits( state%val(i), 0, fbs )
      ! for >32 bit machines 
    end do
    state%cnt = n + 1_wi
    return

  end subroutine init_by_scalar
  ! initialize by an array with array-length 
  ! init_key is the array for initializing keys 
  ! key_length is its length


  subroutine next_state( )

    intrinsic :: ishft, ieor, btest, ibits, mvbits

    integer(kind=wi), parameter :: matrix_a = -1727483681_wi !z'9908b0df'

    integer(kind=wi)  :: i, mld

    if ( .not. state%ini ) call init_by_scalar( default_seed )
    do i = 1, n-m, 1
      mld = ibits( state%val(i+1), 0, 31 )
      call mvbits( state%val(i), 31, 1, mld, 31 )
      state%val(i) = ieor( state%val(i+m), ishft( mld, -1 ) )
      if ( btest( state%val(i+1), 0 ) ) state%val(i) = ieor( state%val(i), matrix_a )
    end do
    do i = n-m+1, n-1, 1
      mld = ibits( state%val(i+1), 0, 31 )
      call mvbits( state%val(i), 31, 1, mld, 31 )
      state%val(i) = ieor( state%val(i+m-n), ishft( mld, -1 ) )
      if ( btest( state%val(i+1), 0 ) ) state%val(i) = ieor( state%val(i), matrix_a )
    end do
    mld = ibits( state%val(1), 0, 31 )
    call mvbits( state%val(n), 31, 1, mld, 31 )
    state%val(n) = ieor( state%val(m), ishft( mld, -1 ) )
    if ( btest( state%val(1), 0 ) ) state%val(n) = ieor( state%val(n), matrix_a )
    state%cnt = 1_wi
    return

  end subroutine next_state


  ! generates a random number on [0,0xffffffff]-interval

  subroutine genrand_int32_0d( y )

    intrinsic :: ieor, iand, ishft

    integer(kind=wi), parameter :: temper_a = -1658038656_wi !z'9D2C5680'
    integer(kind=wi), parameter :: temper_b =  -272236544_wi !z'EFC60000'

    integer(kind=wi), intent(out) :: y
    
    if ( state%cnt > n ) call next_state( )
    y = state%val(state%cnt)
    state%cnt = state%cnt + 1_wi
    ! Tempering 
    y = ieor( y, ishft( y, -11 ) )
    y = ieor( y, iand( ishft( y,  7 ), temper_a ) )
    y = ieor( y, iand( ishft( y, 15 ), temper_b ) )
    y = ieor( y, ishft( y, -18 ) )
    return

  end subroutine genrand_int32_0d

  subroutine genrand_int32_1d( y )

    intrinsic :: size

    integer(kind=wi), dimension(:), intent(out) :: y

    integer(kind=wi)  :: i

    do i = 1, size( y, 1 ), 1
      call genrand_int32_0d( y(i) )
    end do
    return

  end subroutine genrand_int32_1d

  subroutine genrand_int32_2d( y )

    intrinsic :: size

    integer(kind=wi), dimension(:,:), intent(out) :: y
    
    integer(kind=wi)  :: i

    do i = 1, size( y, 2 ), 1
      call genrand_int32_1d( y(:,i) )
    end do
    return

  end subroutine genrand_int32_2d

  subroutine genrand_int32_3d( y )

    intrinsic :: size

    integer(kind=wi), dimension(:,:,:), intent(out) :: y
    
    integer(kind=wi)  :: i

    do i = 1, size( y, 3 ), 1
      call genrand_int32_2d( y(:,:,i) )
    end do
    return

  end subroutine genrand_int32_3d

  subroutine genrand_int32_4d( y )

    intrinsic :: size

    integer(kind=wi), dimension(:,:,:,:), intent(out) :: y
    
    integer(kind=wi)  :: i

    do i = 1, size( y, 4 ), 1
      call genrand_int32_3d( y(:,:,:,i) )
    end do
    return

  end subroutine genrand_int32_4d

  subroutine genrand_int32_5d( y )

    intrinsic :: size

    integer(kind=wi), dimension(:,:,:,:,:), intent(out) :: y
    
    integer(kind=wi)  :: i

    do i = 1, size( y, 5 ), 1
      call genrand_int32_4d( y(:,:,:,:,i) )
    end do
    return

  end subroutine genrand_int32_5d

  subroutine genrand_int32_6d( y )

    intrinsic :: size

    integer(kind=wi), dimension(:,:,:,:,:,:), intent(out) :: y
    
    integer(kind=wi)  :: i

    do i = 1, size( y, 6 ), 1
      call genrand_int32_5d( y(:,:,:,:,:,i) )
    end do
    return

  end subroutine genrand_int32_6d

  subroutine genrand_int32_7d( y )

    intrinsic :: size

    integer(kind=wi), dimension(:,:,:,:,:,:,:), intent(out) :: y
    
    integer(kind=wi)  :: i

    do i = 1, size( y, 7 ), 1
      call genrand_int32_6d( y(:,:,:,:,:,:,i) )
    end do
    return

  end subroutine genrand_int32_7d
  ! generates a random number on [0,0x7fffffff]-interval


  ! generates a random number on [0,1]-real-interval


  ! generates a random number on [0,1)-real-interval


  ! generates a random number on (0,1)-real-interval

  subroutine genrand_real3_0d( r )

    intrinsic :: real

    real(kind=wr), intent(out)  :: r

    integer(kind=wi)  :: a

    call genrand_int32_0d( a )
    r = real( a, kind=wr ) * pi232 + p231_5d232
    ! divided by 2^32 
    return

  end subroutine genrand_real3_0d

  subroutine genrand_real3_1d( r )

    intrinsic :: size

    real(kind=wr), dimension(:), intent(out)  :: r

    integer(kind=wi)  :: i

    do i = 1, size( r, 1 ), 1
      call genrand_real3_0d( r(i) )
    end do
    return

  end subroutine genrand_real3_1d

  subroutine genrand_real3_2d( r )

    intrinsic :: size

    real(kind=wr), dimension(:,:), intent(out)  :: r

    integer(kind=wi)  :: i

    do i = 1, size( r, 2 ), 1
      call genrand_real3_1d( r(:,i) )
    end do
    return

  end subroutine genrand_real3_2d

  subroutine genrand_real3_3d( r )

    intrinsic :: size

    real(kind=wr), dimension(:,:,:), intent(out)  :: r

    integer(kind=wi)  :: i

    do i = 1, size( r, 3 ), 1
      call genrand_real3_2d( r(:,:,i) )
    end do
    return

  end subroutine genrand_real3_3d

  subroutine genrand_real3_4d( r )

    intrinsic :: size

    real(kind=wr), dimension(:,:,:,:), intent(out)  :: r

    integer(kind=wi)  :: i

    do i = 1, size( r, 4 ), 1
      call genrand_real3_3d( r(:,:,:,i) )
    end do
    return

  end subroutine genrand_real3_4d

  subroutine genrand_real3_5d( r )

    intrinsic :: size

    real(kind=wr), dimension(:,:,:,:,:), intent(out)  :: r

    integer(kind=wi)  :: i

    do i = 1, size( r, 5 ), 1
      call genrand_real3_4d( r(:,:,:,:,i) )
    end do
    return

  end subroutine genrand_real3_5d

  subroutine genrand_real3_6d( r )

    intrinsic :: size

    real(kind=wr), dimension(:,:,:,:,:,:), intent(out)  :: r

    integer(kind=wi)  :: i

    do i = 1, size( r, 6 ), 1
      call genrand_real3_5d( r(:,:,:,:,:,i) )
    end do
    return

  end subroutine genrand_real3_6d

  subroutine genrand_real3_7d( r )

    intrinsic :: size

    real(kind=wr), dimension(:,:,:,:,:,:,:), intent(out)  :: r

    integer(kind=wi)  :: i

    do i = 1, size( r, 7 ), 1
      call genrand_real3_6d( r(:,:,:,:,:,:,i) )
    end do
    return

  end subroutine genrand_real3_7d
  ! generates a random number on [0,1) with 53-bit resolution


  ! These real versions are due to Isaku Wada, 2002/01/09 added 
  ! Altered by José Sousa genrand_real[1-3] will not return exactely
  ! the same values but should have the same properties and are faster

  !read state subroutine for kr_externs_in_mt95 
  SUBROUTINE kr_externs_in_mt95(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kr_mt95_genrand_state(state, kgen_unit, "state", .FALSE.) 
  END SUBROUTINE kr_externs_in_mt95 
    
  !read state subroutine for kr_externs_out_mt95 
  SUBROUTINE kr_externs_out_mt95(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      CALL kr_mt95_genrand_state(kgenref_state, kgen_unit, "kgenref_state", .FALSE.) 
  END SUBROUTINE kr_externs_out_mt95 
    
  !verify state subroutine for kv_externs_mt95 
  SUBROUTINE kv_externs_mt95(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_mt95_genrand_state("state", check_status, state, kgenref_state) 
  END SUBROUTINE kv_externs_mt95 
    
  !read state subroutine for kr_mt95_genrand_state 
  RECURSIVE SUBROUTINE kr_mt95_genrand_state(var, kgen_unit, printname, printvar) 
      TYPE(genrand_state), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%ini 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ini = ", var%ini 
      END IF   
        
      READ (UNIT = kgen_unit) var%cnt 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cnt = ", var%cnt 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%val 
          CALL kgen_array_sumcheck(printname // "%val", kgen_array_sum, DBLE(SUM(var%val, mask=(var%val .eq. var%val))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%val)) = ", DBLE(SUM(var%val, mask=(var%val .eq. var%val))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_mt95_genrand_state 
    
  !verify state subroutine for kv_mt95_genrand_state 
  RECURSIVE SUBROUTINE kv_mt95_genrand_state(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(genrand_state), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      logical(KIND=wi) :: diff_ini 
      integer(KIND=wi) :: diff_cnt 
      INTEGER :: n_val 
      integer(KIND=wi) :: nrmsdiff_val, rmsdiff_val 
      integer(KIND=wi), ALLOCATABLE :: buf1_val(:), buf2_val(:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ini .EQV. kgenref_var%ini) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ini is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ini is NOT IDENTICAL." 
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
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cnt == kgenref_var%cnt) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cnt is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cnt = ABS(var%cnt - kgenref_var%cnt) 
          IF (diff_cnt <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cnt is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cnt is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_cnt 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cnt 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%val == kgenref_var%val)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%val is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_val(SIZE(var%val,dim=1))) 
          ALLOCATE (buf2_val(SIZE(var%val,dim=1))) 
          n_val = COUNT(var%val /= kgenref_var%val) 
          WHERE ( ABS(kgenref_var%val) > kgen_minvalue ) 
              buf1_val = ((var%val-kgenref_var%val)/kgenref_var%val)**2 
              buf2_val = (var%val-kgenref_var%val)**2 
          ELSEWHERE 
              buf1_val = (var%val-kgenref_var%val)**2 
              buf2_val = buf1_val 
          END WHERE   
          nrmsdiff_val = SQRT(SUM(buf1_val)/REAL(n_val)) 
          rmsdiff_val = SQRT(SUM(buf2_val)/REAL(n_val)) 
          IF (rmsdiff_val > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%val is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%val is NOT IDENTICAL(within tolerance)." 
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
                  WRITE (*, *) count( var%val /= kgenref_var%val), " of ", size( var%val ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%val)/real(size(var%val)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%val)/real(size(kgenref_var%val)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_val 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_val 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%val /= kgenref_var%val), " of ", size( var%val ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%val)/real(size(var%val)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%val)/real(size(kgenref_var%val)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_val 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_val 
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
  END SUBROUTINE kv_mt95_genrand_state 
    
end module mt95
