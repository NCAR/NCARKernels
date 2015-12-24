!KGEN-generated Fortran source file

!Generated at : 2015-12-24 11:55:33
!KGEN version : 0.6.0

!MODULE module_ra_rrtmg_lw

      module parkind
!     implicit none
          SAVE

!------------------------------------------------------------------
! rrtmg kinds
! Define integer and real kinds for various types.
!
! Initial version: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!
!     integer kinds
!     -------------
!
!     integer, parameter :: kind_ib = selected_int_kind(13)  ! 8 byte integer
!     integer, parameter :: kind_im = selected_int_kind(6)   ! 4 byte integer
      integer, parameter :: kind_im = kind(1)            

!
!     real kinds
!     ----------
!
!      integer, parameter :: kind_rb = selected_real_kind(12) ! 8 byte real
!      integer, parameter :: kind_rm = selected_real_kind(6)  ! 4 byte real
!      integer, parameter :: kind_rn = kind(1.0)              ! native real









       integer, parameter :: kind_rb = kind(1.0)              ! native real


      end module parkind

      module parrrtm

          USE parkind, ONLY: im => kind_im

!     implicit none
          SAVE

!------------------------------------------------------------------
! rrtmg_lw main parameters
!
! Initial version:  JJMorcrette, ECMWF, Jul 1998
! Revised: MJIacono, AER, Jun 2006
! Revised: MJIacono, AER, Aug 2007
! Revised: MJIacono, AER, Aug 2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! mxlay  :  integer: maximum number of layers
! mg     :  integer: number of original g-intervals per spectral band
! nbndlw :  integer: number of spectral bands
! maxxsec:  integer: maximum number of cross-section molecules
!                    (e.g. cfcs)
! maxinpx:  integer: 
! ngptlw :  integer: total number of reduced g-intervals for rrtmg_lw
! ngNN   :  integer: number of reduced g-intervals per spectral band
! ngsNN  :  integer: cumulative number of g-intervals per band
!------------------------------------------------------------------

      integer(kind=im), parameter :: nbndlw = 16
      integer(kind=im), parameter :: maxxsec= 4
      integer(kind=im), parameter :: mxmol  = 38
      integer(kind=im), parameter :: maxinpx= 38
      integer(kind=im), parameter :: nmol   = 7
! Use for 140 g-point model 
      integer(kind=im), parameter :: ngptlw = 140
! Use for 256 g-point model 
!      integer(kind=im), parameter :: ngptlw = 256

! Use for 140 g-point model
      integer(kind=im), parameter :: ng1  = 10
      integer(kind=im), parameter :: ng2  = 12
      integer(kind=im), parameter :: ng3  = 16
      integer(kind=im), parameter :: ng4  = 14
      integer(kind=im), parameter :: ng5  = 16
      integer(kind=im), parameter :: ng6  = 8
      integer(kind=im), parameter :: ng7  = 12
      integer(kind=im), parameter :: ng8  = 8
      integer(kind=im), parameter :: ng9  = 12
      integer(kind=im), parameter :: ng10 = 6
      integer(kind=im), parameter :: ng11 = 8
      integer(kind=im), parameter :: ng12 = 8
      integer(kind=im), parameter :: ng13 = 4
      integer(kind=im), parameter :: ng14 = 2
      integer(kind=im), parameter :: ng15 = 2
      integer(kind=im), parameter :: ng16 = 2

      integer(kind=im), parameter :: ngs1  = 10
      integer(kind=im), parameter :: ngs2  = 22
      integer(kind=im), parameter :: ngs3  = 38
      integer(kind=im), parameter :: ngs4  = 52
      integer(kind=im), parameter :: ngs5  = 68
      integer(kind=im), parameter :: ngs6  = 76
      integer(kind=im), parameter :: ngs7  = 88
      integer(kind=im), parameter :: ngs8  = 96
      integer(kind=im), parameter :: ngs9  = 108
      integer(kind=im), parameter :: ngs10 = 114
      integer(kind=im), parameter :: ngs11 = 122
      integer(kind=im), parameter :: ngs12 = 130
      integer(kind=im), parameter :: ngs13 = 134
      integer(kind=im), parameter :: ngs14 = 136
      integer(kind=im), parameter :: ngs15 = 138

! Use for 256 g-point model
!      integer(kind=im), parameter :: ng1  = 16
!      integer(kind=im), parameter :: ng2  = 16
!      integer(kind=im), parameter :: ng3  = 16
!      integer(kind=im), parameter :: ng4  = 16
!      integer(kind=im), parameter :: ng5  = 16
!      integer(kind=im), parameter :: ng6  = 16
!      integer(kind=im), parameter :: ng7  = 16
!      integer(kind=im), parameter :: ng8  = 16
!      integer(kind=im), parameter :: ng9  = 16
!      integer(kind=im), parameter :: ng10 = 16
!      integer(kind=im), parameter :: ng11 = 16
!      integer(kind=im), parameter :: ng12 = 16
!      integer(kind=im), parameter :: ng13 = 16
!      integer(kind=im), parameter :: ng14 = 16
!      integer(kind=im), parameter :: ng15 = 16
!      integer(kind=im), parameter :: ng16 = 16

!      integer(kind=im), parameter :: ngs1  = 16
!      integer(kind=im), parameter :: ngs2  = 32
!      integer(kind=im), parameter :: ngs3  = 48
!      integer(kind=im), parameter :: ngs4  = 64
!      integer(kind=im), parameter :: ngs5  = 80
!      integer(kind=im), parameter :: ngs6  = 96
!      integer(kind=im), parameter :: ngs7  = 112
!      integer(kind=im), parameter :: ngs8  = 128
!      integer(kind=im), parameter :: ngs9  = 144
!      integer(kind=im), parameter :: ngs10 = 160
!      integer(kind=im), parameter :: ngs11 = 176
!      integer(kind=im), parameter :: ngs12 = 192
!      integer(kind=im), parameter :: ngs13 = 208
!      integer(kind=im), parameter :: ngs14 = 224
!      integer(kind=im), parameter :: ngs15 = 240
!      integer(kind=im), parameter :: ngs16 = 256

      end module parrrtm

      module rrlw_cld

          USE parkind, ONLY: rb => kind_rb

!     implicit none
          SAVE

!------------------------------------------------------------------
! rrtmg_lw cloud property coefficients

! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! abscld1:  real   : 
! absice0:  real   : 
! absice1:  real   : 
! absice2:  real   : 
! absice3:  real   : 
! absliq0:  real   : 
! absliq1:  real   : 
!------------------------------------------------------------------

      real(kind=rb) , dimension(2) :: absice0
      real(kind=rb) , dimension(2,5) :: absice1
      real(kind=rb) , dimension(43,16) :: absice2
      real(kind=rb) , dimension(46,16) :: absice3
      real(kind=rb) :: absliq0
      real(kind=rb) , dimension(58,16) :: absliq1

      PUBLIC kr_externs_in_rrlw_cld
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_cld
      SUBROUTINE kr_externs_in_rrlw_cld(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) absice0
          READ (UNIT = kgen_unit) absice1
          READ (UNIT = kgen_unit) absice2
          READ (UNIT = kgen_unit) absice3
          READ (UNIT = kgen_unit) absliq0
          READ (UNIT = kgen_unit) absliq1
      END SUBROUTINE kr_externs_in_rrlw_cld
      
      end module rrlw_cld

      module rrlw_con

          USE parkind, ONLY: rb => kind_rb

!     implicit none
          USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
          SAVE

!------------------------------------------------------------------
! rrtmg_lw constants

! Initial version: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! fluxfac:  real   : radiance to flux conversion factor 
! heatfac:  real   : flux to heating rate conversion factor
!oneminus:  real   : 1.-1.e-6
! pi     :  real   : pi
! grav   :  real   : acceleration of gravity
! planck :  real   : planck constant
! boltz  :  real   : boltzmann constant
! clight :  real   : speed of light
! avogad :  real   : avogadro constant 
! alosmt :  real   : loschmidt constant
! gascon :  real   : molar gas constant
! radcn1 :  real   : first radiation constant
! radcn2 :  real   : second radiation constant
! sbcnst :  real   : stefan-boltzmann constant
!  secdy :  real   : seconds per day  
!------------------------------------------------------------------

      real(kind=rb) :: fluxfac, heatfac
      real(kind=rb) :: oneminus, pi, grav
      real(kind=rb) :: avogad, alosmt, gascon

      PUBLIC kr_externs_in_rrlw_con
      PUBLIC kr_externs_out_rrlw_con
      REAL(KIND=rb) :: kgenref_fluxfac
      REAL(KIND=rb) :: kgenref_oneminus, kgenref_pi
      PUBLIC kv_externs_rrlw_con
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_con
      SUBROUTINE kr_externs_in_rrlw_con(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fluxfac
          READ (UNIT = kgen_unit) heatfac
          READ (UNIT = kgen_unit) pi
          READ (UNIT = kgen_unit) oneminus
          READ (UNIT = kgen_unit) grav
          READ (UNIT = kgen_unit) avogad
      END SUBROUTINE kr_externs_in_rrlw_con
      
      !read state subroutine for kr_externs_out_rrlw_con
      SUBROUTINE kr_externs_out_rrlw_con(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          
          LOGICAL :: kgen_istrue
          READ (UNIT = kgen_unit) kgenref_fluxfac
          READ (UNIT = kgen_unit) kgenref_pi
          READ (UNIT = kgen_unit) kgenref_oneminus
      END SUBROUTINE kr_externs_out_rrlw_con
      
      !verify state subroutine for kv_externs_rrlw_con
      SUBROUTINE kv_externs_rrlw_con(check_status)
          TYPE(check_t), INTENT(INOUT) :: check_status
          
          CALL kv_rrlw_con_real__rb("fluxfac", check_status, fluxfac, kgenref_fluxfac)
          CALL kv_rrlw_con_real__rb("pi", check_status, pi, kgenref_pi)
          CALL kv_rrlw_con_real__rb("oneminus", check_status, oneminus, kgenref_oneminus)
      END SUBROUTINE kv_externs_rrlw_con
      
      !verify state subroutine for kv_rrlw_con_real__rb
      RECURSIVE SUBROUTINE kv_rrlw_con_real__rb(varname, check_status, var, kgenref_var)
          CHARACTER(LEN=*), INTENT(IN) :: varname
          TYPE(check_t), INTENT(INOUT) :: check_status
          REAL(KIND=rb), INTENT(IN) :: var, kgenref_var
          INTEGER :: check_result
          LOGICAL :: is_print = .FALSE.
          
          real(KIND=rb) :: diff
          
          check_status%numTotal = check_status%numTotal + 1
          
          IF (var == kgenref_var) THEN
              check_status%numIdentical = check_status%numIdentical + 1
              WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
              check_result = CHECK_IDENTICAL
          ELSE
              diff = ABS(var - kgenref_var)
              IF (diff <= check_status%tolerance) THEN
                  check_status%numInTol = check_status%numInTol + 1
                  WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL within tolerance."
                  check_result = CHECK_IN_TOL
              ELSE
                  check_status%numOutTol = check_status%numOutTol + 1
                  WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL out of tolerance."
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", 0
              END IF 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 0) THEN
                  WRITE (*, *) "Difference is ", diff
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) "Difference is ", diff
              END IF 
          END IF 
          
      END SUBROUTINE kv_rrlw_con_real__rb
      
      end module rrlw_con

      module rrlw_kg01

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 1
! band 1:  10-250 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! kao_mn2 : real     
! kbo_mn2 : real     
! selfrefo: real     
! forrefo : real
!-----------------------------------------------------------------



!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 1
! band 1:  10-250 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! ka_mn2  : real     
! kb_mn2  : real     
! selfref : real     
! forref  : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng1  = 10

      real(kind=rb) :: fracrefa(ng1)  , fracrefb(ng1)
      real(kind=rb) :: ka(5,13,ng1)   , absa(65,ng1)
      real(kind=rb) :: kb(5,13:59,ng1), absb(235,ng1)
      real(kind=rb) :: ka_mn2(19,ng1) , kb_mn2(19,ng1)
      real(kind=rb) :: selfref(10,ng1), forref(4,ng1)

      

      PUBLIC kr_externs_in_rrlw_kg01
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg01
      SUBROUTINE kr_externs_in_rrlw_kg01(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) kb_mn2
          READ (UNIT = kgen_unit) ka_mn2
          READ (UNIT = kgen_unit) forref
          READ (UNIT = kgen_unit) selfref
      END SUBROUTINE kr_externs_in_rrlw_kg01
      
      end module rrlw_kg01

      module rrlw_kg02

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 2
! band 2:  250-500 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!-----------------------------------------------------------------



!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 2
! band 2:  250-500 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
!
! refparam: real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng2  = 12

      real(kind=rb) :: fracrefa(ng2)  , fracrefb(ng2)
      real(kind=rb) :: ka(5,13,ng2)   , absa(65,ng2)
      real(kind=rb) :: kb(5,13:59,ng2), absb(235,ng2)
      real(kind=rb) :: selfref(10,ng2), forref(4,ng2)


      

      PUBLIC kr_externs_in_rrlw_kg02
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg02
      SUBROUTINE kr_externs_in_rrlw_kg02(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) forref
          READ (UNIT = kgen_unit) selfref
      END SUBROUTINE kr_externs_in_rrlw_kg02
      
      end module rrlw_kg02

      module rrlw_kg03

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 3
! band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! kao_mn2o: real     
! kbo_mn2o: real     
! selfrefo: real     
! forrefo : real
!-----------------------------------------------------------------



!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 3
! band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! ka_mn2o : real     
! kb_mn2o : real     
! selfref : real     
! forref  : real
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng3  = 16

      real(kind=rb) :: fracrefa(ng3,9) ,fracrefb(ng3,5)
      real(kind=rb) :: ka(9,5,13,ng3)  ,absa(585,ng3)
      real(kind=rb) :: kb(5,5,13:59,ng3),absb(1175,ng3)
      real(kind=rb) :: ka_mn2o(9,19,ng3), kb_mn2o(5,19,ng3)
      real(kind=rb) :: selfref(10,ng3)
      real(kind=rb) :: forref(4,ng3)

      

      PUBLIC kr_externs_in_rrlw_kg03
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg03
      SUBROUTINE kr_externs_in_rrlw_kg03(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) ka_mn2o
          READ (UNIT = kgen_unit) kb_mn2o
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg03
      
      end module rrlw_kg03

      module rrlw_kg04

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 4
! band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------



!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 4
! band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! absa    : real
! absb    : real
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! selfref : real     
! forref  : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng4  = 14

      real(kind=rb) :: fracrefa(ng4,9)  ,fracrefb(ng4,5)
      real(kind=rb) :: ka(9,5,13,ng4)   ,absa(585,ng4)
      real(kind=rb) :: kb(5,5,13:59,ng4),absb(1175,ng4)
      real(kind=rb) :: selfref(10,ng4)  ,forref(4,ng4)

      

      PUBLIC kr_externs_in_rrlw_kg04
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg04
      SUBROUTINE kr_externs_in_rrlw_kg04(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) forref
          READ (UNIT = kgen_unit) selfref
      END SUBROUTINE kr_externs_in_rrlw_kg04
      
      end module rrlw_kg04

      module rrlw_kg05

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 5
! band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! kao_mo3 : real     
! selfrefo: real     
! forrefo : real     
! ccl4o   : real
!-----------------------------------------------------------------



!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 5
! band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! ka_mo3  : real     
! selfref : real     
! forref  : real     
! ccl4    : real
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng5  = 16

      real(kind=rb) :: fracrefa(ng5,9) ,fracrefb(ng5,5)
      real(kind=rb) :: ka(9,5,13,ng5)   ,absa(585,ng5)
      real(kind=rb) :: kb(5,5,13:59,ng5),absb(1175,ng5)
      real(kind=rb) :: ka_mo3(9,19,ng5)
      real(kind=rb) :: selfref(10,ng5)
      real(kind=rb) :: forref(4,ng5)
      real(kind=rb) :: ccl4(ng5)
      
      

      PUBLIC kr_externs_in_rrlw_kg05
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg05
      SUBROUTINE kr_externs_in_rrlw_kg05(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) ka_mo3
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
          READ (UNIT = kgen_unit) ccl4
      END SUBROUTINE kr_externs_in_rrlw_kg05
      
      end module rrlw_kg05

      module rrlw_kg06

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 6
! band 6:  820-980 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! kao_mco2: real     
! selfrefo: real     
! forrefo : real     
!cfc11adjo: real
! cfc12o  : real
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 6
! band 6:  820-980 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! ka_mco2 : real     
! selfref : real     
! forref  : real     
!cfc11adj : real
! cfc12   : real
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng6  = 8

      real(kind=rb) , dimension(ng6) :: fracrefa
      real(kind=rb) :: ka(5,13,ng6),absa(65,ng6)
      real(kind=rb) :: ka_mco2(19,ng6)
      real(kind=rb) :: selfref(10,ng6)
      real(kind=rb) :: forref(4,ng6)

      real(kind=rb) , dimension(ng6) :: cfc11adj
      real(kind=rb) , dimension(ng6) :: cfc12

      

      PUBLIC kr_externs_in_rrlw_kg06
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg06
      SUBROUTINE kr_externs_in_rrlw_kg06(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) ka_mco2
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
          READ (UNIT = kgen_unit) cfc11adj
          READ (UNIT = kgen_unit) cfc12
      END SUBROUTINE kr_externs_in_rrlw_kg06
      
      end module rrlw_kg06

      module rrlw_kg07

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 7
! band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! kao_mco2: real     
! kbo_mco2: real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------



!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 7
! band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! ka_mco2 : real     
! kb_mco2 : real     
! selfref : real     
! forref  : real     
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng7  = 12

      real(kind=rb) , dimension(ng7) :: fracrefb
      real(kind=rb) :: fracrefa(ng7,9)
      real(kind=rb) :: ka(9,5,13,ng7) ,absa(585,ng7)
      real(kind=rb) :: kb(5,13:59,ng7),absb(235,ng7)
      real(kind=rb) :: ka_mco2(9,19,ng7)
      real(kind=rb) :: kb_mco2(19,ng7)
      real(kind=rb) :: selfref(10,ng7)
      real(kind=rb) :: forref(4,ng7)

      

      PUBLIC kr_externs_in_rrlw_kg07
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg07
      SUBROUTINE kr_externs_in_rrlw_kg07(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) ka_mco2
          READ (UNIT = kgen_unit) kb_mco2
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg07
      
      end module rrlw_kg07

      module rrlw_kg08

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 8
! band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! kao_mco2: real     
! kbo_mco2: real     
! kao_mn2o: real     
! kbo_mn2o: real     
! kao_mo3 : real     
! selfrefo: real     
! forrefo : real     
! cfc12o  : real     
!cfc22adjo: real     
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 8
! band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! ka_mco2 : real     
! kb_mco2 : real     
! ka_mn2o : real     
! kb_mn2o : real     
! ka_mo3  : real     
! selfref : real     
! forref  : real     
! cfc12   : real     
! cfc22adj: real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng8  = 8

      real(kind=rb) , dimension(ng8) :: fracrefa
      real(kind=rb) , dimension(ng8) :: fracrefb
      real(kind=rb) , dimension(ng8) :: cfc12
      real(kind=rb) , dimension(ng8) :: cfc22adj

      real(kind=rb) :: ka(5,13,ng8)    ,absa(65,ng8)
      real(kind=rb) :: kb(5,13:59,ng8) ,absb(235,ng8)
      real(kind=rb) :: ka_mco2(19,ng8)
      real(kind=rb) :: ka_mn2o(19,ng8)
      real(kind=rb) :: ka_mo3(19,ng8)
      real(kind=rb) :: kb_mco2(19,ng8)
      real(kind=rb) :: kb_mn2o(19,ng8)
      real(kind=rb) :: selfref(10,ng8)
      real(kind=rb) :: forref(4,ng8)

      

      PUBLIC kr_externs_in_rrlw_kg08
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg08
      SUBROUTINE kr_externs_in_rrlw_kg08(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) cfc12
          READ (UNIT = kgen_unit) cfc22adj
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) ka_mco2
          READ (UNIT = kgen_unit) ka_mn2o
          READ (UNIT = kgen_unit) ka_mo3
          READ (UNIT = kgen_unit) kb_mco2
          READ (UNIT = kgen_unit) kb_mn2o
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg08
      
      end module rrlw_kg08

      module rrlw_kg09

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 9
! band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! kao_mn2o: real     
! kbo_mn2o: real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 9
! band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! ka_mn2o : real     
! kb_mn2o : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng9  = 12

      real(kind=rb) , dimension(ng9) :: fracrefb
      real(kind=rb) :: fracrefa(ng9,9)
      real(kind=rb) :: ka(9,5,13,ng9) ,absa(585,ng9)
      real(kind=rb) :: kb(5,13:59,ng9) ,absb(235,ng9)
      real(kind=rb) :: ka_mn2o(9,19,ng9)
      real(kind=rb) :: kb_mn2o(19,ng9)
      real(kind=rb) :: selfref(10,ng9)
      real(kind=rb) :: forref(4,ng9)

      

      PUBLIC kr_externs_in_rrlw_kg09
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg09
      SUBROUTINE kr_externs_in_rrlw_kg09(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) ka_mn2o
          READ (UNIT = kgen_unit) kb_mn2o
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg09
      
      end module rrlw_kg09

      module rrlw_kg10

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 10
! band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 10
! band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng10 = 6

      real(kind=rb) , dimension(ng10) :: fracrefa
      real(kind=rb) , dimension(ng10) :: fracrefb

      real(kind=rb) :: ka(5,13,ng10)   , absa(65,ng10)
      real(kind=rb) :: kb(5,13:59,ng10), absb(235,ng10)
      real(kind=rb) :: selfref(10,ng10)
      real(kind=rb) :: forref(4,ng10)

      

      PUBLIC kr_externs_in_rrlw_kg10
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg10
      SUBROUTINE kr_externs_in_rrlw_kg10(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg10
      
      end module rrlw_kg10

      module rrlw_kg11

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 11
! band 11:  1480-1800 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! kao_mo2 : real     
! kbo_mo2 : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 11
! band 11:  1480-1800 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! ka_mo2  : real     
! kb_mo2  : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng11 = 8

      real(kind=rb) , dimension(ng11) :: fracrefa
      real(kind=rb) , dimension(ng11) :: fracrefb

      real(kind=rb) :: ka(5,13,ng11)   , absa(65,ng11)
      real(kind=rb) :: kb(5,13:59,ng11), absb(235,ng11)
      real(kind=rb) :: ka_mo2(19,ng11)
      real(kind=rb) :: kb_mo2(19,ng11)
      real(kind=rb) :: selfref(10,ng11)
      real(kind=rb) :: forref(4,ng11)

      

      PUBLIC kr_externs_in_rrlw_kg11
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg11
      SUBROUTINE kr_externs_in_rrlw_kg11(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) ka_mo2
          READ (UNIT = kgen_unit) kb_mo2
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg11
      
      end module rrlw_kg11

      module rrlw_kg12

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 12
! band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------



!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 12
! band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! selfref : real     
! forref  : real     
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng12 = 8

      real(kind=rb) :: fracrefa(ng12,9)
      real(kind=rb) :: ka(9,5,13,ng12) ,absa(585,ng12)
      real(kind=rb) :: selfref(10,ng12)
      real(kind=rb) :: forref(4,ng12)

      

      PUBLIC kr_externs_in_rrlw_kg12
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg12
      SUBROUTINE kr_externs_in_rrlw_kg12(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg12
      
      end module rrlw_kg12

      module rrlw_kg13

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 13
! band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! kao_mco2: real     
! kao_mco : real     
! kbo_mo3 : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 13
! band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! ka_mco2 : real     
! ka_mco  : real     
! kb_mo3  : real     
! selfref : real     
! forref  : real     
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng13 = 4

      real(kind=rb) , dimension(ng13) :: fracrefb

      real(kind=rb) :: fracrefa(ng13,9)
      real(kind=rb) :: ka(9,5,13,ng13) ,absa(585,ng13)
      real(kind=rb) :: ka_mco2(9,19,ng13)
      real(kind=rb) :: ka_mco(9,19,ng13)
      real(kind=rb) :: kb_mo3(19,ng13)
      real(kind=rb) :: selfref(10,ng13)
      real(kind=rb) :: forref(4,ng13)

      

      PUBLIC kr_externs_in_rrlw_kg13
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg13
      SUBROUTINE kr_externs_in_rrlw_kg13(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) ka_mco2
          READ (UNIT = kgen_unit) ka_mco
          READ (UNIT = kgen_unit) kb_mo3
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg13
      
      end module rrlw_kg13

      module rrlw_kg14

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 14
! band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 14
! band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng14 = 2

      real(kind=rb) , dimension(ng14) :: fracrefa
      real(kind=rb) , dimension(ng14) :: fracrefb

      real(kind=rb) :: ka(5,13,ng14)   ,absa(65,ng14)
      real(kind=rb) :: kb(5,13:59,ng14),absb(235,ng14)
      real(kind=rb) :: selfref(10,ng14)
      real(kind=rb) :: forref(4,ng14)

      

      PUBLIC kr_externs_in_rrlw_kg14
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg14
      SUBROUTINE kr_externs_in_rrlw_kg14(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg14
      
      end module rrlw_kg14

      module rrlw_kg15

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 15
! band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! kao_mn2 : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 15
! band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! ka_mn2  : real     
! selfref : real     
! forref  : real     
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng15 = 2

      real(kind=rb) :: fracrefa(ng15,9)
      real(kind=rb) :: ka(9,5,13,ng15) ,absa(585,ng15)
      real(kind=rb) :: ka_mn2(9,19,ng15)
      real(kind=rb) :: selfref(10,ng15)
      real(kind=rb) :: forref(4,ng15)

      

      PUBLIC kr_externs_in_rrlw_kg15
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg15
      SUBROUTINE kr_externs_in_rrlw_kg15(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) ka_mn2
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg15
      
      end module rrlw_kg15

      module rrlw_kg16

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 16
! band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------




!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 16
! band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! kb      : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng16 = 2

      real(kind=rb) , dimension(ng16) :: fracrefb

      real(kind=rb) :: fracrefa(ng16,9)
      real(kind=rb) :: ka(9,5,13,ng16) ,absa(585,ng16)
      real(kind=rb) :: kb(5,13:59,ng16), absb(235,ng16)
      real(kind=rb) :: selfref(10,ng16)
      real(kind=rb) :: forref(4,ng16)

      

      PUBLIC kr_externs_in_rrlw_kg16
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_kg16
      SUBROUTINE kr_externs_in_rrlw_kg16(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) fracrefb
          READ (UNIT = kgen_unit) fracrefa
          READ (UNIT = kgen_unit) absa
          READ (UNIT = kgen_unit) absb
          READ (UNIT = kgen_unit) selfref
          READ (UNIT = kgen_unit) forref
      END SUBROUTINE kr_externs_in_rrlw_kg16
      
      end module rrlw_kg16


      module rrlw_ref

          USE parkind, ONLY: rb => kind_rb

!     implicit none
          SAVE

!------------------------------------------------------------------
! rrtmg_lw reference atmosphere 
! Based on standard mid-latitude summer profile
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! pref   :  real   : Reference pressure levels
! preflog:  real   : Reference pressure levels, ln(pref)
! tref   :  real   : Reference temperature levels for MLS profile
! chi_mls:  real   : 
!------------------------------------------------------------------

      real(kind=rb) , dimension(59) :: preflog
      real(kind=rb) , dimension(59) :: tref
      real(kind=rb) :: chi_mls(7,59)

      PUBLIC kr_externs_in_rrlw_ref
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_ref
      SUBROUTINE kr_externs_in_rrlw_ref(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) preflog
          READ (UNIT = kgen_unit) tref
          READ (UNIT = kgen_unit) chi_mls
      END SUBROUTINE kr_externs_in_rrlw_ref
      
      end module rrlw_ref

      module rrlw_tbl

          USE parkind, ONLY: im => kind_im, rb => kind_rb

!     implicit none
          SAVE

!------------------------------------------------------------------
! rrtmg_lw exponential lookup table arrays

! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, Jun 2006
! Revised: MJIacono, AER, Aug 2007
! Revised: MJIacono, AER, Aug 2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! ntbl   :  integer: Lookup table dimension
! tblint :  real   : Lookup table conversion factor
! tau_tbl:  real   : Clear-sky optical depth (used in cloudy radiative
!                    transfer)
! exp_tbl:  real   : Transmittance lookup table
! tfn_tbl:  real   : Tau transition function; i.e. the transition of
!                    the Planck function from that for the mean layer
!                    temperature to that for the layer boundary
!                    temperature as a function of optical depth.
!                    The "linear in tau" method is used to make 
!                    the table.
! pade   :  real   : Pade constant   
! bpade  :  real   : Inverse of Pade constant   
!------------------------------------------------------------------

      integer(kind=im), parameter :: ntbl = 10000

      real(kind=rb), parameter :: tblint = 10000.0_rb

      real(kind=rb) , dimension(0:ntbl) :: tau_tbl
      real(kind=rb) , dimension(0:ntbl) :: exp_tbl
      real(kind=rb) , dimension(0:ntbl) :: tfn_tbl

      real(kind=rb) :: bpade

      PUBLIC kr_externs_in_rrlw_tbl
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_tbl
      SUBROUTINE kr_externs_in_rrlw_tbl(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) tau_tbl
          READ (UNIT = kgen_unit) exp_tbl
          READ (UNIT = kgen_unit) tfn_tbl
          READ (UNIT = kgen_unit) bpade
      END SUBROUTINE kr_externs_in_rrlw_tbl
      
      end module rrlw_tbl

      module rrlw_vsn

!     implicit none
          USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
          SAVE

!------------------------------------------------------------------
! rrtmg_lw version information

! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
!hnamrtm :character: 
!hnamini :character: 
!hnamcld :character: 
!hnamclc :character: 
!hnamrtr :character: 
!hnamrtx :character: 
!hnamrtc :character: 
!hnamset :character: 
!hnamtau :character: 
!hnamatm :character: 
!hnamutl :character: 
!hnamext :character: 
!hnamkg  :character: 
!
! hvrrtm :character: 
! hvrini :character: 
! hvrcld :character: 
! hvrclc :character: 
! hvrrtr :character: 
! hvrrtx :character: 
! hvrrtc :character: 
! hvrset :character: 
! hvrtau :character: 
! hvratm :character: 
! hvrutl :character: 
! hvrext :character: 
! hvrkg  :character: 
!------------------------------------------------------------------

      character*18 hvrrtm,hvrini,hvrcld,hvrclc,hvrrtr,hvrrtx, &
                   hvrrtc,hvrset,hvrtau,hvratm,hvrutl,hvrext


      PUBLIC kr_externs_in_rrlw_vsn
      PUBLIC kr_externs_out_rrlw_vsn
      CHARACTER(LEN=18) :: kgenref_hvrclc, kgenref_hvrrtc, kgenref_hvrset, kgenref_hvrtau
      PUBLIC kv_externs_rrlw_vsn
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_vsn
      SUBROUTINE kr_externs_in_rrlw_vsn(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) hvrclc
          READ (UNIT = kgen_unit) hvrrtc
          READ (UNIT = kgen_unit) hvrtau
          READ (UNIT = kgen_unit) hvrset
      END SUBROUTINE kr_externs_in_rrlw_vsn
      
      !read state subroutine for kr_externs_out_rrlw_vsn
      SUBROUTINE kr_externs_out_rrlw_vsn(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          
          LOGICAL :: kgen_istrue
          READ (UNIT = kgen_unit) kgenref_hvrclc
          READ (UNIT = kgen_unit) kgenref_hvrrtc
          READ (UNIT = kgen_unit) kgenref_hvrtau
          READ (UNIT = kgen_unit) kgenref_hvrset
      END SUBROUTINE kr_externs_out_rrlw_vsn
      
      !verify state subroutine for kv_externs_rrlw_vsn
      SUBROUTINE kv_externs_rrlw_vsn(check_status)
          TYPE(check_t), INTENT(INOUT) :: check_status
          
          CALL kv_rrlw_vsn_character_18_("hvrclc", check_status, hvrclc, kgenref_hvrclc)
          CALL kv_rrlw_vsn_character_18_("hvrrtc", check_status, hvrrtc, kgenref_hvrrtc)
          CALL kv_rrlw_vsn_character_18_("hvrtau", check_status, hvrtau, kgenref_hvrtau)
          CALL kv_rrlw_vsn_character_18_("hvrset", check_status, hvrset, kgenref_hvrset)
      END SUBROUTINE kv_externs_rrlw_vsn
      
      !verify state subroutine for kv_rrlw_vsn_character_18_
      RECURSIVE SUBROUTINE kv_rrlw_vsn_character_18_(varname, check_status, var, kgenref_var)
          CHARACTER(LEN=*), INTENT(IN) :: varname
          TYPE(check_t), INTENT(INOUT) :: check_status
          CHARACTER(LEN=18), INTENT(IN) :: var, kgenref_var
          INTEGER :: check_result
          LOGICAL :: is_print = .FALSE.
          
          character(LEN=18) :: diff
          
          check_status%numTotal = check_status%numTotal + 1
          
          IF (var == kgenref_var) THEN
              check_status%numIdentical = check_status%numIdentical + 1
              WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
              check_result = CHECK_IDENTICAL
          ELSE
              check_status%numOutTol = check_status%numOutTol + 1
              WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL."
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED"
              END IF 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 0) THEN
                  WRITE (*, *) "NOT IMPLEMENTED"
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) "NOT IMPLEMENTED"
              END IF 
          END IF 
          
      END SUBROUTINE kv_rrlw_vsn_character_18_
      
      end module rrlw_vsn

      module rrlw_wvn

          USE parkind, ONLY: im => kind_im, rb => kind_rb
          USE parrrtm, ONLY: nbndlw, ngptlw, maxinpx

!     implicit none
          SAVE

!------------------------------------------------------------------
! rrtmg_lw spectral information

! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! ng     :  integer: Number of original g-intervals in each spectral band
! nspa   :  integer: For the lower atmosphere, the number of reference
!                    atmospheres that are stored for each spectral band
!                    per pressure level and temperature.  Each of these
!                    atmospheres has different relative amounts of the 
!                    key species for the band (i.e. different binary
!                    species parameters).
! nspb   :  integer: Same as nspa for the upper atmosphere
!wavenum1:  real   : Spectral band lower boundary in wavenumbers
!wavenum2:  real   : Spectral band upper boundary in wavenumbers
! delwave:  real   : Spectral band width in wavenumbers
! totplnk:  real   : Integrated Planck value for each band; (band 16
!                    includes total from 2600 cm-1 to infinity)
!                    Used for calculation across total spectrum
!totplk16:  real   : Integrated Planck value for band 16 (2600-3250 cm-1)
!                    Used for calculation in band 16 only if 
!                    individual band output requested
!
! ngc    :  integer: The number of new g-intervals in each band
! ngs    :  integer: The cumulative sum of new g-intervals for each band
! ngm    :  integer: The index of each new g-interval relative to the
!                    original 16 g-intervals in each band
! ngn    :  integer: The number of original g-intervals that are 
!                    combined to make each new g-intervals in each band
! ngb    :  integer: The band index for each new g-interval
! wt     :  real   : RRTM weights for the original 16 g-intervals
! rwgt   :  real   : Weights for combining original 16 g-intervals 
!                    (256 total) into reduced set of g-intervals 
!                    (140 total)
! nxmol  :  integer: Number of cross-section molecules
! ixindx :  integer: Flag for active cross-sections in calculation
!------------------------------------------------------------------

      integer(kind=im) :: nspa(nbndlw)
      integer(kind=im) :: nspb(nbndlw)

      real(kind=rb) :: delwave(nbndlw)

      real(kind=rb) :: totplnk(181,nbndlw)
      real(kind=rb) :: totplk16(181)

      integer(kind=im) :: ngs(nbndlw)
      integer(kind=im) :: ngb(ngptlw)


      integer(kind=im) :: ixindx(maxinpx)

      PUBLIC kr_externs_in_rrlw_wvn
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_rrlw_wvn
      SUBROUTINE kr_externs_in_rrlw_wvn(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) nspa
          READ (UNIT = kgen_unit) nspb
          READ (UNIT = kgen_unit) delwave
          READ (UNIT = kgen_unit) totplnk
          READ (UNIT = kgen_unit) totplk16
          READ (UNIT = kgen_unit) ngs
          READ (UNIT = kgen_unit) ngb
          READ (UNIT = kgen_unit) ixindx
      END SUBROUTINE kr_externs_in_rrlw_wvn
      
      end module rrlw_wvn

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!

! Fortran-95 implementation of the Mersenne Twister 19937, following 
!   the C implementation described below (code mt19937ar-cok.c, dated 2002/2/10), 
!   adapted cosmetically by making the names more general.  
! Users must declare one or more variables of type randomNumberSequence in the calling 
!   procedure which are then initialized using a required seed. If the 
!   variable is not initialized the random numbers will all be 0. 
! For example: 
! program testRandoms 
!   use RandomNumbers
!   type(randomNumberSequence) :: randomNumbers
!   integer                    :: i
!   
!   randomNumbers = new_RandomNumberSequence(seed = 100)
!   do i = 1, 10
!     print ('(f12.10, 2x)'), getRandomReal(randomNumbers)
!   end do
! end program testRandoms
! 
! Fortran-95 implementation by 
!   Robert Pincus
!   NOAA-CIRES Climate Diagnostics Center
!   Boulder, CO 80305 
!   email: Robert.Pincus@colorado.edu
!
! This documentation in the original C program reads:
! -------------------------------------------------------------
!    A C-program for MT19937, with initialization improved 2002/2/10.
!    Coded by Takuji Nishimura and Makoto Matsumoto.
!    This is a faster version by taking Shawn Cokus's optimization,
!    Matthe Bellew's simplification, Isaku Wada's real version.
! 
!    Before using, initialize the state by using init_genrand(seed) 
!    or init_by_array(init_key, key_length).
! 
!    Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
!    All rights reserved.                          
! 
!    Redistribution and use in source and binary forms, with or without
!    modification, are permitted provided that the following conditions
!    are met:
! 
!      1. Redistributions of source code must retain the above copyright
!         notice, this list of conditions and the following disclaimer.
! 
!      2. Redistributions in binary form must reproduce the above copyright
!         notice, this list of conditions and the following disclaimer in the
!         documentation and/or other materials provided with the distribution.
! 
!      3. The names of its contributors may not be used to endorse or promote 
!         products derived from this software without specific prior written 
!         permission.
! 
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!    A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
!    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
!    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
!    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
!    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! 
! 
!    Any feedback is very welcome.
!    http://www.math.keio.ac.jp/matumoto/emt.html
!    email: matumoto@math.keio.ac.jp
! -------------------------------------------------------------

  module MersenneTwister
! -------------------------------------------------------------

      USE parkind, ONLY: im => kind_im, rb => kind_rb

      USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
      IMPLICIT NONE
      PRIVATE
  
  ! Algorithm parameters
  ! -------
  ! Period parameters
  integer(kind=im), parameter :: blockSize = 624,         &
                        M         = 397,         &
                        MATRIX_A  = -1727483681, & ! constant vector a         (0x9908b0dfUL)
                        UMASK     = -2147483647-1, & ! most significant w-r bits (0x80000000UL)
                        LMASK     =  2147483647    ! least significant r bits  (0x7fffffffUL)
  ! Tempering parameters
  integer(kind=im), parameter :: TMASKB= -1658038656, & ! (0x9d2c5680UL)
                        TMASKC= -272236544     ! (0xefc60000UL)
  ! -------

  ! The type containing the state variable  
  type randomNumberSequence
    integer(kind=im)                            :: currentElement ! = blockSize
    integer(kind=im), dimension(0:blockSize -1) :: state ! = 0
  end type randomNumberSequence

  interface new_RandomNumberSequence
    module procedure initialize_scalar, initialize_vector
  end interface new_RandomNumberSequence 

  PUBLIC randomnumbersequence
  PUBLIC new_randomnumbersequence, getrandomint, getrandomreal
! -------------------------------------------------------------
  PUBLIC kr_mersennetwister_randomnumbersequence
  PUBLIC kv_mersennetwister_randomnumbersequence
contains
  ! -------------------------------------------------------------
  ! Private functions
  ! ---------------------------
  function mixbits(u, v)
    integer(kind=im), intent( in) :: u, v
    integer(kind=im)              :: mixbits
    
    mixbits = ior(iand(u, UMASK), iand(v, LMASK))
  end function mixbits
  ! ---------------------------
  function twist(u, v)
    integer(kind=im), intent( in) :: u, v
    integer(kind=im)              :: twist

    ! Local variable
    integer(kind=im), parameter, dimension(0:1) :: t_matrix = (/ 0_im, MATRIX_A /)
    
    twist = ieor(ishft(mixbits(u, v), -1_im), t_matrix(iand(v, 1_im)))
    twist = ieor(ishft(mixbits(u, v), -1_im), t_matrix(iand(v, 1_im)))
  end function twist
  ! ---------------------------
  subroutine nextState(twister)
    type(randomNumberSequence), intent(inout) :: twister
    
    ! Local variables
    integer(kind=im) :: k
    
    do k = 0, blockSize - M - 1
      twister%state(k) = ieor(twister%state(k + M), &
                              twist(twister%state(k), twister%state(k + 1_im)))
    end do 
    do k = blockSize - M, blockSize - 2
      twister%state(k) = ieor(twister%state(k + M - blockSize), &
                              twist(twister%state(k), twister%state(k + 1_im)))
    end do 
    twister%state(blockSize - 1_im) = ieor(twister%state(M - 1_im), &
                                        twist(twister%state(blockSize - 1_im), twister%state(0_im)))
    twister%currentElement = 0_im

  end subroutine nextState
  ! ---------------------------
  elemental function temper(y)
    integer(kind=im), intent(in) :: y
    integer(kind=im)             :: temper
    
    integer(kind=im) :: x
    
    ! Tempering
    x      = ieor(y, ishft(y, -11))
    x      = ieor(x, iand(ishft(x,  7), TMASKB))
    x      = ieor(x, iand(ishft(x, 15), TMASKC))
    temper = ieor(x, ishft(x, -18))
  end function temper
  ! -------------------------------------------------------------
  ! Public (but hidden) functions
  ! --------------------
  function initialize_scalar(seed) result(twister)
    integer(kind=im),       intent(in   ) :: seed
    type(randomNumberSequence)                :: twister 
    
    integer(kind=im) :: i
    ! See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. In the previous versions, 
    !   MSBs of the seed affect only MSBs of the array state[].                       
    !   2002/01/09 modified by Makoto Matsumoto            
    
    twister%state(0) = iand(seed, -1_im)
    do i = 1,  blockSize - 1 ! ubound(twister%state)
       twister%state(i) = 1812433253_im * ieor(twister%state(i-1), &
                                            ishft(twister%state(i-1), -30_im)) + i
       twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
    end do
    twister%currentElement = blockSize
  end function initialize_scalar
  ! -------------------------------------------------------------
  function initialize_vector(seed) result(twister)
    integer(kind=im), dimension(0:), intent(in) :: seed
    type(randomNumberSequence)                      :: twister 
    
    integer(kind=im) :: i, j, k, nFirstLoop, nWraps
    
    nWraps  = 0
    twister = initialize_scalar(19650218_im)
    
    nFirstLoop = max(blockSize, size(seed))
    do k = 1, nFirstLoop
       i = mod(k + nWraps, blockSize)
       j = mod(k - 1,      size(seed))
       if(i == 0) then
         twister%state(i) = twister%state(blockSize - 1)
         twister%state(1) = ieor(twister%state(1),                                 &
                                 ieor(twister%state(1-1),                          & 
                                      ishft(twister%state(1-1), -30_im)) * 1664525_im) + & 
                            seed(j) + j ! Non-linear
         twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
         nWraps = nWraps + 1
       else
         twister%state(i) = ieor(twister%state(i),                                 &
                                 ieor(twister%state(i-1),                          & 
                                      ishft(twister%state(i-1), -30_im)) * 1664525_im) + & 
                            seed(j) + j ! Non-linear
         twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
      end if
    end do
    
    !
    ! Walk through the state array, beginning where we left off in the block above
    ! 
    do i = mod(nFirstLoop, blockSize) + nWraps + 1, blockSize - 1
      twister%state(i) = ieor(twister%state(i),                                 &
                              ieor(twister%state(i-1),                          & 
                                   ishft(twister%state(i-1), -30_im)) * 1566083941_im) - i ! Non-linear
      twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
    end do
    
    twister%state(0) = twister%state(blockSize - 1) 
    
    do i = 1, mod(nFirstLoop, blockSize) + nWraps
      twister%state(i) = ieor(twister%state(i),                                 &
                              ieor(twister%state(i-1),                          & 
                                   ishft(twister%state(i-1), -30_im)) * 1566083941_im) - i ! Non-linear
      twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
    end do
    
    twister%state(0) = UMASK 
    twister%currentElement = blockSize
    
  end function initialize_vector
  ! -------------------------------------------------------------
  ! Public functions
  ! --------------------
  function getRandomInt(twister)
    type(randomNumberSequence), intent(inout) :: twister
    integer(kind=im)                        :: getRandomInt
    ! Generate a random integer on the interval [0,0xffffffff]
    !   Equivalent to genrand_int32 in the C code. 
    !   Fortran doesn't have a type that's unsigned like C does, 
    !   so this is integers in the range -2**31 - 2**31
    ! All functions for getting random numbers call this one, 
    !   then manipulate the result
    
    if(twister%currentElement >= blockSize) call nextState(twister)
      
    getRandomInt = temper(twister%state(twister%currentElement))
    twister%currentElement = twister%currentElement + 1
  
  end function getRandomInt
  ! --------------------
    ! Generate a random integer on the interval [0,0x7fffffff]
    !   or [0,2**31]
    !   Equivalent to genrand_int31 in the C code. 
    
    ! Local integers

  
  ! --------------------
!! mji - modified Jan 2007, double converted to rrtmg real kind type
  function getRandomReal(twister)
    type(randomNumberSequence), intent(inout) :: twister
!    double precision             :: getRandomReal
    real(kind=rb)             :: getRandomReal
    ! Generate a random number on [0,1]
    !   Equivalent to genrand_real1 in the C code
    !   The result is stored as double precision but has 32 bit resolution
    
    integer(kind=im) :: localInt
    
    localInt = getRandomInt(twister)
    if(localInt < 0) then
!      getRandomReal = dble(localInt + 2.0d0**32)/(2.0d0**32 - 1.0d0)
      getRandomReal = (localInt + 2.0**32_rb)/(2.0**32_rb - 1.0_rb)
    else
!      getRandomReal = dble(localInt            )/(2.0d0**32 - 1.0d0)
      getRandomReal = (localInt            )/(2.0**32_rb - 1.0_rb)
    end if

  end function getRandomReal
  ! --------------------
    

  ! --------------------  
  
  !read state subroutine for kr_mersennetwister_randomnumbersequence
  RECURSIVE SUBROUTINE kr_mersennetwister_randomnumbersequence(var, kgen_unit, printvar)
      TYPE(randomnumbersequence), INTENT(INOUT) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      
      READ (UNIT = kgen_unit) var%currentelement
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%currentelement **" // NEW_LINE("A"), var%currentelement
      END IF 
      
      READ (UNIT = kgen_unit) var%state
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%state **", SUM(var%state)
      END IF 
      
  END SUBROUTINE kr_mersennetwister_randomnumbersequence
  
  !verify state subroutine for kv_mersennetwister_randomnumbersequence
  RECURSIVE SUBROUTINE kv_mersennetwister_randomnumbersequence(varname, check_status, var, kgenref_var)
      CHARACTER(LEN=*), INTENT(IN) :: varname
      TYPE(check_t), INTENT(INOUT) :: check_status
      TYPE(randomnumbersequence), INTENT(IN) :: var, kgenref_var
      TYPE(check_t) :: dtype_check_status, comp_check_status
      INTEGER :: check_result
      LOGICAL :: is_print = .FALSE.
      
      integer(KIND=im) :: diff_currentelement
      INTEGER :: n_state
      integer(KIND=im) :: nrmsdiff_state, rmsdiff_state
      integer(KIND=im), ALLOCATABLE :: buf1_state(:), buf2_state(:)
      
      check_status%numTotal = check_status%numTotal + 1
      
      CALL kgen_init_check(dtype_check_status)
      IF (var%currentelement == kgenref_var%currentelement) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          WRITE (*, *) trim(adjustl(varname)), "%currentelement is IDENTICAL."
          check_result = CHECK_IDENTICAL
      ELSE
          diff_currentelement = ABS(var%currentelement - kgenref_var%currentelement)
          IF (diff_currentelement <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              WRITE (*, *) trim(adjustl(varname)), "%currentelement is NOT IDENTICAL within tolerance."
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              WRITE (*, *) trim(adjustl(varname)), "%currentelement is NOT IDENTICAL out of tolerance."
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_currentelement
          END IF 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 0) THEN
              WRITE (*, *) "Difference is ", diff_currentelement
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 1) THEN
              WRITE (*, *) "Difference is ", diff_currentelement
          END IF 
      END IF 
      
      IF (ALL(var%state == kgenref_var%state)) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          WRITE (*, *) trim(adjustl(varname)), "%state is IDENTICAL."
          check_result = CHECK_IDENTICAL
      ELSE
          ALLOCATE (buf1_state(SIZE(var%state,dim=1)))
          ALLOCATE (buf2_state(SIZE(var%state,dim=1)))
          n_state = COUNT(var%state /= kgenref_var%state)
          WHERE ( ABS(kgenref_var%state) > dtype_check_status%minvalue )
              buf1_state = ((var%state-kgenref_var%state)/kgenref_var%state)**2
              buf2_state = (var%state-kgenref_var%state)**2
          ELSEWHERE
              buf1_state = (var%state-kgenref_var%state)**2
              buf2_state = buf1_state
          END WHERE 
          nrmsdiff_state = SQRT(SUM(buf1_state)/REAL(n_state))
          rmsdiff_state = SQRT(SUM(buf2_state)/REAL(n_state))
          IF (nrmsdiff_state > dtype_check_status%tolerance) THEN
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              WRITE (*, *) trim(adjustl(varname)), "%state is NOT IDENTICAL out of tolerance."
              check_result = CHECK_OUT_TOL
          ELSE
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              WRITE (*, *) trim(adjustl(varname)), "%state is NOT IDENTICAL within tolerance."
              check_result = CHECK_IN_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) count( var%state /= kgenref_var%state), " of ", size( var%state ), " elements are different."
              WRITE (*, *) "Average - kernel ", sum(var%state)/real(size(var%state))
              WRITE (*, *) "Average - reference ", sum(kgenref_var%state)/real(size(kgenref_var%state))
              WRITE (*, *) "RMS of difference is ", rmsdiff_state
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_state
              
          END IF 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 0) THEN
              WRITE (*, *) count( var%state /= kgenref_var%state), " of ", size( var%state ), " elements are different."
              WRITE (*, *) "Average - kernel ", sum(var%state)/real(size(var%state))
              WRITE (*, *) "Average - reference ", sum(kgenref_var%state)/real(size(kgenref_var%state))
              WRITE (*, *) "RMS of difference is ", rmsdiff_state
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_state
              
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 1) THEN
              WRITE (*, *) count( var%state /= kgenref_var%state), " of ", size( var%state ), " elements are different."
              WRITE (*, *) "Average - kernel ", sum(var%state)/real(size(var%state))
              WRITE (*, *) "Average - reference ", sum(kgenref_var%state)/real(size(kgenref_var%state))
              WRITE (*, *) "RMS of difference is ", rmsdiff_state
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_state
              
          END IF 
      END IF 
      
  END SUBROUTINE kv_mersennetwister_randomnumbersequence
  
  end module MersenneTwister



  ! Generic module to wrap random number generators. 
  !   The module defines a type that identifies the particular stream of random 
  !   numbers, and has procedures for initializing it and getting real numbers 
  !   in the range 0 to 1. 
  ! This version uses the Mersenne Twister to generate random numbers on [0, 1]. 
  !
!! mji
!!  use time_manager_mod, only: time_type, get_date


  

  

  


!! mji
!!            initializeRandomNumberStream, getRandomNumbers, &
!!            constructSeed
  ! ---------------------------------------------------------
  ! Initialization
  ! ---------------------------------------------------------
    
    
  ! ---------------------------------------------------------
    
    
  ! ---------------------------------------------------------
  ! Procedures for drawing random numbers
  ! ---------------------------------------------------------
    
  ! ---------------------------------------------------------
    
    ! Local variables
    

  ! ---------------------------------------------------------
    
    ! Local variables
    

! mji
!  ! ---------------------------------------------------------
!  ! Constructing a unique seed from grid cell index and model date/time
!  !   Once we have the GFDL stuff we'll add the year, month, day, hour, minute
!  ! ---------------------------------------------------------
!  function constructSeed(i, j, time) result(seed)
!    integer(kind=im),         intent( in)  :: i, j
!    type(time_type), intent( in) :: time
!    integer(kind=im), dimension(8) :: seed
!    
!    ! Local variables
!    integer(kind=im) :: year, month, day, hour, minute, second
!    
!    
!    call get_date(time, year, month, day, hour, minute, second)
!    seed = (/ i, j, year, month, day, hour, minute, second /)
!  end function constructSeed


!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!
      module mcica_subcol_gen_lw

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2006-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! Purpose: Create McICA stochastic arrays for cloud physical or optical properties.
! Two options are possible:
! 1) Input cloud physical properties: cloud fraction, ice and liquid water
!    paths, ice fraction, and particle sizes.  Output will be stochastic
!    arrays of these variables.  (inflag = 1)
! 2) Input cloud optical properties directly: cloud optical depth, single
!    scattering albedo and asymmetry parameter.  Output will be stochastic
!    arrays of these variables.  (inflag = 0; longwave scattering is not
!    yet available, ssac and asmc are for future expansion)

! --------- Modules ----------

          USE parkind, ONLY: im => kind_im, rb => kind_rb
          USE parrrtm, ONLY: ngptlw
          USE rrlw_wvn, ONLY: ngb

          IMPLICIT NONE

! public interfaces/functions/subroutines
          PUBLIC mcica_subcol_lw, generate_stochastic_clouds

      contains

!------------------------------------------------------------------
! Public subroutines
!------------------------------------------------------------------

      subroutine mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play, &
                       cldfrac, ciwp, clwp, rei, rel, tauc, cldfmcl, &
                       ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)

! ----- Input -----
! Control
      integer(kind=im), intent(in) :: iplon           ! column/longitude index
      integer(kind=im), intent(in) :: ncol            ! number of columns
      integer(kind=im), intent(in) :: nlay            ! number of model layers
      integer(kind=im), intent(in) :: icld            ! clear/cloud, cloud overlap flag
      integer(kind=im), intent(in) :: permuteseed     ! if the cloud generator is called multiple times, 
                                                      ! permute the seed between each call.
                                                      ! between calls for LW and SW, recommended
                                                      ! permuteseed differes by 'ngpt'
      integer(kind=im), intent(inout) :: irng         ! flag for random number generator
                                                      !  0 = kissvec
                                                      !  1 = Mersenne Twister

! Atmosphere
      real(kind=rb), intent(in) :: play(:,:)          ! layer pressures (mb) 
                                                      !    Dimensions: (ncol,nlay)

! Atmosphere/clouds - cldprop
      real(kind=rb), intent(in) :: cldfrac(:,:)       ! layer cloud fraction
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tauc(:,:,:)        ! in-cloud optical depth
                                                      !    Dimensions: (nbndlw,ncol,nlay)
!      real(kind=rb), intent(in) :: ssac(:,:,:)       ! in-cloud single scattering albedo
                                                      !    Dimensions: (nbndlw,ncol,nlay)
!      real(kind=rb), intent(in) :: asmc(:,:,:)       ! in-cloud asymmetry parameter
                                                      !    Dimensions: (nbndlw,ncol,nlay)
      real(kind=rb), intent(in) :: ciwp(:,:)          ! in-cloud ice water path
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: clwp(:,:)          ! in-cloud liquid water path
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: rei(:,:)           ! cloud ice particle size
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: rel(:,:)           ! cloud liquid particle size
                                                      !    Dimensions: (ncol,nlay)

! ----- Output -----
! Atmosphere/clouds - cldprmc [mcica]
      real(kind=rb), intent(out) :: cldfmcl(:,:,:)    ! cloud fraction [mcica]
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(out) :: ciwpmcl(:,:,:)    ! in-cloud ice water path [mcica]
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(out) :: clwpmcl(:,:,:)    ! in-cloud liquid water path [mcica]
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(out) :: relqmcl(:,:)      ! liquid particle size (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(out) :: reicmcl(:,:)      ! ice partcle size (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(out) :: taucmcl(:,:,:)    ! in-cloud optical depth [mcica]
                                                      !    Dimensions: (ngptlw,ncol,nlay)
!      real(kind=rb), intent(out) :: ssacmcl(:,:,:)   ! in-cloud single scattering albedo [mcica]
                                                      !    Dimensions: (ngptlw,ncol,nlay)
!      real(kind=rb), intent(out) :: asmcmcl(:,:,:)   ! in-cloud asymmetry parameter [mcica]
                                                      !    Dimensions: (ngptlw,ncol,nlay)

! ----- Local -----

! Stochastic cloud generator variables [mcica]
      integer(kind=im), parameter :: nsubclw = ngptlw ! number of sub-columns (g-point intervals)
      integer(kind=im) :: ilev                        ! loop index

      real(kind=rb) :: pmid(ncol, nlay)               ! layer pressures (Pa) 
!      real(kind=rb) :: pdel(ncol, nlay)              ! layer pressure thickness (Pa) 
!      real(kind=rb) :: qi(ncol, nlay)                ! ice water (specific humidity)
!      real(kind=rb) :: ql(ncol, nlay)                ! liq water (specific humidity)


! Return if clear sky; or stop if icld out of range
      if (icld.eq.0) return
      if (icld.lt.0.or.icld.gt.3) then 
         stop 'MCICA_SUBCOL: INVALID ICLD'
      endif 

! NOTE: For GCM mode, permuteseed must be offset between LW and SW by at least the number of subcolumns


! Pass particle sizes to new arrays, no subcolumns for these properties yet
! Convert pressures from mb to Pa

      reicmcl(:ncol,:nlay) = rei(:ncol,:nlay)
      relqmcl(:ncol,:nlay) = rel(:ncol,:nlay)
      pmid(:ncol,:nlay) = play(:ncol,:nlay)*1.e2_rb

! Convert input ice and liquid cloud water paths to specific humidity ice and liquid components 

!      cwp =  (q * pdel * 1000.) / gravit)
!           = (kg/kg * kg m-1 s-2 *1000.) / m s-2
!           = (g m-2)
!
!      q  = (cwp * gravit) / (pdel *1000.)
!         = (g m-2 * m s-2) / (kg m-1 s-2 * 1000.)
!         =  kg/kg

!      do ilev = 1, nlay
!         qi(ilev) = (ciwp(ilev) * grav) / (pdel(ilev) * 1000._rb)
!         ql(ilev) = (clwp(ilev) * grav) / (pdel(ilev) * 1000._rb)
!      enddo

!  Generate the stochastic subcolumns of cloud optical properties for the longwave;
      call generate_stochastic_clouds (ncol, nlay, nsubclw, icld, irng, pmid, cldfrac, clwp, ciwp, tauc, &
                               cldfmcl, clwpmcl, ciwpmcl, taucmcl, permuteseed)

      end subroutine mcica_subcol_lw


!-------------------------------------------------------------------------------------------------
      subroutine generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, ciwp, tauc, &
                                   cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch, changeSeed) 
!-------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------
  ! ---------------------
  ! Contact: Cecile Hannay (hannay@ucar.edu)
  ! 
  ! Original code: Based on Raisanen et al., QJRMS, 2004.
  ! 
  ! Modifications: Generalized for use with RRTMG and added Mersenne Twister as the default
  !   random number generator, which can be changed to the optional kissvec random number generator
  !   with flag 'irng'. Some extra functionality has been commented or removed.  
  !   Michael J. Iacono, AER, Inc., February 2007
  !
  ! Given a profile of cloud fraction, cloud water and cloud ice, we produce a set of subcolumns.
  ! Each layer within each subcolumn is homogeneous, with cloud fraction equal to zero or one 
  ! and uniform cloud liquid and cloud ice concentration.
  ! The ensemble as a whole reproduces the probability function of cloud liquid and ice within each layer 
  ! and obeys an overlap assumption in the vertical.   
  ! 
  ! Overlap assumption:
  !  The cloud are consistent with 4 overlap assumptions: random, maximum, maximum-random and exponential. 
  !  The default option is maximum-random (option 3)
  !  The options are: 1=random overlap, 2=max/random, 3=maximum overlap, 4=exponential overlap
  !  This is set with the variable "overlap" 
  !mji - Exponential overlap option (overlap=4) has been deactivated in this version
  !  The exponential overlap uses also a length scale, Zo. (real,    parameter  :: Zo = 2500. ) 
  ! 
  ! Seed:
  !  If the stochastic cloud generator is called several times during the same timestep, 
  !  one should change the seed between the call to insure that the subcolumns are different.
  !  This is done by changing the argument 'changeSeed'
  !  For example, if one wants to create a set of columns for the shortwave and another set for the longwave ,
  !  use 'changeSeed = 1' for the first call and'changeSeed = 2' for the second call 
  !
  ! PDF assumption:
  !  We can use arbitrary complicated PDFS. 
  !  In the present version, we produce homogeneuous clouds (the simplest case).  
  !  Future developments include using the PDF scheme of Ben Johnson. 
  !
  ! History file:
  !  Option to add diagnostics variables in the history file. (using FINCL in the namelist)
  !  nsubcol = number of subcolumns
  !  overlap = overlap type (1-3)
  !  Zo = length scale 
  !  CLOUD_S = mean of the subcolumn cloud fraction ('_S" means Stochastic)
  !  CLDLIQ_S = mean of the subcolumn cloud water
  !  CLDICE_S = mean of the subcolumn cloud ice 
  !
  ! Note:
  !   Here: we force that the cloud condensate to be consistent with the cloud fraction 
  !   i.e we only have cloud condensate when the cell is cloudy. 
  !   In CAM: The cloud condensate and the cloud fraction are obtained from 2 different equations 
  !   and the 2 quantities can be inconsistent (i.e. CAM can produce cloud fraction 
  !   without cloud condensate or the opposite).
  !---------------------------------------------------------------------------------------------------------------

! The Mersenne Twister random number engine
          USE mersennetwister, ONLY: randomnumbersequence, new_randomnumbersequence, getrandomreal

          USE mersennetwister, ONLY: kr_mersennetwister_randomnumbersequence
          USE mersennetwister, ONLY: kv_mersennetwister_randomnumbersequence
      type(randomNumberSequence) :: randomNumbers

! -- Arguments

      integer(kind=im), intent(in) :: ncol            ! number of columns
      integer(kind=im), intent(in) :: nlay            ! number of layers
      integer(kind=im), intent(in) :: icld            ! clear/cloud, cloud overlap flag
      integer(kind=im), intent(inout) :: irng         ! flag for random number generator
                                                      !  0 = kissvec
                                                      !  1 = Mersenne Twister
      integer(kind=im), intent(in) :: nsubcol         ! number of sub-columns (g-point intervals)
      integer(kind=im), optional, intent(in) :: changeSeed     ! allows permuting seed

! Column state (cloud fraction, cloud water, cloud ice) + variables needed to read physics state 
      real(kind=rb), intent(in) :: pmid(:,:)          ! layer pressure (Pa)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cld(:,:)           ! cloud fraction 
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: clwp(:,:)          ! in-cloud liquid water path
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: ciwp(:,:)          ! in-cloud ice water path
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tauc(:,:,:)        ! in-cloud optical depth
                                                      !    Dimensions: (nbndlw,ncol,nlay)
!      real(kind=rb), intent(in) :: ssac(:,:,:)       ! in-cloud single scattering albedo
                                                      !    Dimensions: (nbndlw,ncol,nlay)
                                                      !   inactive - for future expansion
!      real(kind=rb), intent(in) :: asmc(:,:,:)       ! in-cloud asymmetry parameter
                                                      !    Dimensions: (nbndlw,ncol,nlay)
                                                      !   inactive - for future expansion

      real(kind=rb), intent(out) :: cld_stoch(:,:,:)  ! subcolumn cloud fraction 
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(out) :: clwp_stoch(:,:,:) ! subcolumn in-cloud liquid water path
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(out) :: ciwp_stoch(:,:,:) ! subcolumn in-cloud ice water path
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(out) :: tauc_stoch(:,:,:) ! subcolumn in-cloud optical depth
                                                      !    Dimensions: (ngptlw,ncol,nlay)
!      real(kind=rb), intent(out) :: ssac_stoch(:,:,:)! subcolumn in-cloud single scattering albedo
                                                      !    Dimensions: (ngptlw,ncol,nlay)
                                                      !   inactive - for future expansion
!      real(kind=rb), intent(out) :: asmc_stoch(:,:,:)! subcolumn in-cloud asymmetry parameter
                                                      !    Dimensions: (ngptlw,ncol,nlay)
                                                      !   inactive - for future expansion

! -- Local variables
      real(kind=rb) :: cldf(ncol,nlay)                ! cloud fraction 
    
! Mean over the subcolumns (cloud fraction, cloud water , cloud ice) - inactive
!      real(kind=rb) :: mean_cld_stoch(ncol, nlay)    ! cloud fraction 
!      real(kind=rb) :: mean_clwp_stoch(ncol, nlay)   ! cloud water
!      real(kind=rb) :: mean_ciwp_stoch(ncol, nlay)   ! cloud ice
!      real(kind=rb) :: mean_tauc_stoch(ncol, nlay)   ! cloud optical depth
!      real(kind=rb) :: mean_ssac_stoch(ncol, nlay)   ! cloud single scattering albedo
!      real(kind=rb) :: mean_asmc_stoch(ncol, nlay)   ! cloud asymmetry parameter

! Set overlap
      integer(kind=im) :: overlap                     ! 1 = random overlap, 2 = maximum/random,
                                                      ! 3 = maximum overlap, 
!      real(kind=rb), parameter  :: Zo = 2500._rb        ! length scale (m) 
!      real(kind=rb) :: zm(ncol,nlay)                 ! Height of midpoints (above surface)
!      real(kind=rb), dimension(nlay) :: alpha=0.0_rb    ! overlap parameter  

! Constants (min value for cloud fraction and cloud water and ice)
      real(kind=rb), parameter :: cldmin = 1.0e-20_rb ! min cloud fraction
!      real(kind=rb), parameter :: qmin   = 1.0e-10_rb   ! min cloud water and cloud ice (not used)

! Variables related to random number and seed 
      real(kind=rb), dimension(nsubcol, ncol, nlay) :: CDF, CDF2      ! random numbers
      integer(kind=im), dimension(ncol) :: seed1, seed2, seed3, seed4 ! seed to create random number (kissvec)
      real(kind=rb), dimension(ncol) :: rand_num      ! random number (kissvec)
      integer(kind=im) :: iseed                       ! seed to create random number (Mersenne Teister)
      real(kind=rb) :: rand_num_mt                    ! random number (Mersenne Twister)

! Flag to identify cloud fraction in subcolumns
      logical,  dimension(nsubcol, ncol, nlay) :: iscloudy   ! flag that says whether a gridbox is cloudy

! Indices
      integer(kind=im) :: ilev, isubcol, i, n         ! indices

!------------------------------------------------------------------------------------------ 

! Check that irng is in bounds; if not, set to default
      if (irng .ne. 0) irng = 1

! Pass input cloud overlap setting to local variable
      overlap = icld

! Ensure that cloud fractions are in bounds 
      do ilev = 1, nlay
         do i = 1, ncol
            cldf(i,ilev) = cld(i,ilev)
            if (cldf(i,ilev) < cldmin) then
               cldf(i,ilev) = 0._rb
            endif
         enddo
      enddo

! ----- Create seed  --------
   
! Advance randum number generator by changeseed values
      if (irng.eq.0) then   
! For kissvec, create a seed that depends on the state of the columns. Maybe not the best way, but it works.  
! Must use pmid from bottom four layers. 
         do i=1,ncol
            if (pmid(i,1).lt.pmid(i,2)) then 
               stop 'MCICA_SUBCOL: KISSVEC SEED GENERATOR REQUIRES PMID FROM BOTTOM FOUR LAYERS.'
            endif 
            seed1(i) = (pmid(i,1) - int(pmid(i,1)))  * 1000000000_im
            seed2(i) = (pmid(i,2) - int(pmid(i,2)))  * 1000000000_im
            seed3(i) = (pmid(i,3) - int(pmid(i,3)))  * 1000000000_im
            seed4(i) = (pmid(i,4) - int(pmid(i,4)))  * 1000000000_im
          enddo
         do i=1,changeSeed
            call kissvec(seed1, seed2, seed3, seed4, rand_num)
         enddo
      elseif (irng.eq.1) then
         randomNumbers = new_RandomNumberSequence(seed = changeSeed)
      endif 


! ------ Apply overlap assumption --------

! generate the random numbers  

      select case (overlap)

      case(1) 
! Random overlap
! i) pick a random value at every level
  
         if (irng.eq.0) then 
            do isubcol = 1,nsubcol
               do ilev = 1,nlay
                  call kissvec(seed1, seed2, seed3, seed4, rand_num)  ! we get different random number for each level
                  CDF(isubcol,:,ilev) = rand_num
               enddo
            enddo
         elseif (irng.eq.1) then
            do isubcol = 1, nsubcol
               do i = 1, ncol
                  do ilev = 1, nlay
                     rand_num_mt = getRandomReal(randomNumbers)
                     CDF(isubcol,i,ilev) = rand_num_mt
                  enddo
               enddo
             enddo
         endif

      case(2) 
! Maximum-Random overlap
! i) pick a random number for top layer.
! ii) walk down the column: 
!    - if the layer above is cloudy, we use the same random number than in the layer above
!    - if the layer above is clear, we use a new random number 

         if (irng.eq.0) then 
            do isubcol = 1,nsubcol
               do ilev = 1,nlay
                  call kissvec(seed1, seed2, seed3, seed4, rand_num) 
                  CDF(isubcol,:,ilev) = rand_num
               enddo
            enddo
         elseif (irng.eq.1) then
            do isubcol = 1, nsubcol
               do i = 1, ncol
                  do ilev = 1, nlay
                     rand_num_mt = getRandomReal(randomNumbers)
                     CDF(isubcol,i,ilev) = rand_num_mt
                  enddo
               enddo
             enddo
         endif

         do ilev = 2,nlay
            do i = 1, ncol
               do isubcol = 1, nsubcol
                  if (CDF(isubcol, i, ilev-1) > 1._rb - cldf(i,ilev-1) ) then
                     CDF(isubcol,i,ilev) = CDF(isubcol,i,ilev-1) 
                  else
                     CDF(isubcol,i,ilev) = CDF(isubcol,i,ilev) * (1._rb - cldf(i,ilev-1)) 
                  endif
               enddo
            enddo
         enddo
       
      case(3) 
! Maximum overlap
! i) pick the same random numebr at every level  

         if (irng.eq.0) then 
            do isubcol = 1,nsubcol
               call kissvec(seed1, seed2, seed3, seed4, rand_num)
               do ilev = 1,nlay
                  CDF(isubcol,:,ilev) = rand_num
               enddo
            enddo
         elseif (irng.eq.1) then
            do isubcol = 1, nsubcol
               do i = 1, ncol
                  rand_num_mt = getRandomReal(randomNumbers)
                  do ilev = 1, nlay
                     CDF(isubcol,i,ilev) = rand_num_mt
                  enddo
               enddo
             enddo
         endif

!    case(4) - inactive
!       ! Exponential overlap: weighting between maximum and random overlap increases with the distance. 
!       ! The random numbers for exponential overlap verify:
!       ! j=1   RAN(j)=RND1
!       ! j>1   if RND1 < alpha(j,j-1) => RAN(j) = RAN(j-1)
!       !                                 RAN(j) = RND2
!       ! alpha is obtained from the equation
!       ! alpha = exp(- (Zi-Zj-1)/Zo) where Zo is a characteristic length scale    


!       ! compute alpha
!       zm    = state%zm     
!       alpha(:, 1) = 0.
!       do ilev = 2,nlay
!          alpha(:, ilev) = exp( -( zm (:, ilev-1) -  zm (:, ilev)) / Zo)
!       end do
       
!       ! generate 2 streams of random numbers
!       do isubcol = 1,nsubcol
!          do ilev = 1,nlay
!             call kissvec(seed1, seed2, seed3, seed4, rand_num)
!             CDF(isubcol, :, ilev) = rand_num
!             call kissvec(seed1, seed2, seed3, seed4, rand_num)
!             CDF2(isubcol, :, ilev) = rand_num
!          end do
!       end do

!       ! generate random numbers
!       do ilev = 2,nlay
!          where (CDF2(:, :, ilev) < spread(alpha (:,ilev), dim=1, nCopies=nsubcol) )
!             CDF(:,:,ilev) = CDF(:,:,ilev-1) 
!          end where
!       end do

      end select

 
! -- generate subcolumns for homogeneous clouds -----
      do ilev = 1,nlay
         iscloudy(:,:,ilev) = (CDF(:,:,ilev) >= 1._rb - spread(cldf(:,ilev), dim=1, nCopies=nsubcol) )
      enddo

! where the subcolumn is cloudy, the subcolumn cloud fraction is 1;
! where the subcolumn is not cloudy, the subcolumn cloud fraction is 0;
! where there is a cloud, define the subcolumn cloud properties, 
! otherwise set these to zero

      do ilev = 1,nlay
         do i = 1, ncol
            do isubcol = 1, nsubcol
               if (iscloudy(isubcol,i,ilev) ) then
                  cld_stoch(isubcol,i,ilev) = 1._rb
                  clwp_stoch(isubcol,i,ilev) = clwp(i,ilev)
                  ciwp_stoch(isubcol,i,ilev) = ciwp(i,ilev)
                  n = ngb(isubcol)
                  tauc_stoch(isubcol,i,ilev) = tauc(n,i,ilev)
!                  ssac_stoch(isubcol,i,ilev) = ssac(n,i,ilev)
!                  asmc_stoch(isubcol,i,ilev) = asmc(n,i,ilev)
               else
                  cld_stoch(isubcol,i,ilev) = 0._rb
                  clwp_stoch(isubcol,i,ilev) = 0._rb
                  ciwp_stoch(isubcol,i,ilev) = 0._rb
                  tauc_stoch(isubcol,i,ilev) = 0._rb
!                  ssac_stoch(isubcol,i,ilev) = 1._rb
!                  asmc_stoch(isubcol,i,ilev) = 1._rb
               endif
            enddo
         enddo
      enddo

! -- compute the means of the subcolumns ---
!      mean_cld_stoch(:,:) = 0._rb
!      mean_clwp_stoch(:,:) = 0._rb
!      mean_ciwp_stoch(:,:) = 0._rb
!      mean_tauc_stoch(:,:) = 0._rb
!      mean_ssac_stoch(:,:) = 0._rb
!      mean_asmc_stoch(:,:) = 0._rb
!      do i = 1, nsubcol
!         mean_cld_stoch(:,:) =  cld_stoch(i,:,:) + mean_cld_stoch(:,:) 
!         mean_clwp_stoch(:,:) =  clwp_stoch( i,:,:) + mean_clwp_stoch(:,:) 
!         mean_ciwp_stoch(:,:) =  ciwp_stoch( i,:,:) + mean_ciwp_stoch(:,:) 
!         mean_tauc_stoch(:,:) =  tauc_stoch( i,:,:) + mean_tauc_stoch(:,:) 
!         mean_ssac_stoch(:,:) =  ssac_stoch( i,:,:) + mean_ssac_stoch(:,:) 
!         mean_asmc_stoch(:,:) =  asmc_stoch( i,:,:) + mean_asmc_stoch(:,:) 
!      end do
!      mean_cld_stoch(:,:) = mean_cld_stoch(:,:) / nsubcol
!      mean_clwp_stoch(:,:) = mean_clwp_stoch(:,:) / nsubcol
!      mean_ciwp_stoch(:,:) = mean_ciwp_stoch(:,:) / nsubcol
!      mean_tauc_stoch(:,:) = mean_tauc_stoch(:,:) / nsubcol
!      mean_ssac_stoch(:,:) = mean_ssac_stoch(:,:) / nsubcol
!      mean_asmc_stoch(:,:) = mean_asmc_stoch(:,:) / nsubcol

      end subroutine generate_stochastic_clouds


!------------------------------------------------------------------
! Private subroutines
!------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------- 
      subroutine kissvec(seed1,seed2,seed3,seed4,ran_arr)
!-------------------------------------------------------------------------------------------------- 

! public domain code
! made available from http://www.fortran.com/
! downloaded by pjr on 03/16/04 for NCAR CAM
! converted to vector form, functions inlined by pjr,mvr on 05/10/2004

! The  KISS (Keep It Simple Stupid) random number generator. Combines:
! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
! (2) A 3-shift shift-register generator, period 2^32-1,
! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
!  Overall period>2^123; 
!
      real(kind=rb), dimension(:), intent(inout)  :: ran_arr
      integer(kind=im), dimension(:), intent(inout) :: seed1,seed2,seed3,seed4
      integer(kind=im) :: i,sz,kiss
      integer(kind=im) :: m, k, n

! inline function 
      m(k, n) = ieor (k, ishft (k, n) )

      sz = size(ran_arr)
      do i = 1, sz
         seed1(i) = 69069_im * seed1(i) + 1327217885_im
         seed2(i) = m (m (m (seed2(i), 13_im), - 17_im), 5_im)
         seed3(i) = 18000_im * iand (seed3(i), 65535_im) + ishft (seed3(i), - 16_im)
         seed4(i) = 30903_im * iand (seed4(i), 65535_im) + ishft (seed4(i), - 16_im)
         kiss = seed1(i) + seed2(i) + ishft (seed3(i), 16_im) + seed4(i)
         ran_arr(i) = kiss*2.328306e-10_rb + 0.5_rb
      end do
    
      end subroutine kissvec

      end module mcica_subcol_gen_lw

!     path:      $Source: /storm/rc1/cvsroot/rc/rrtmg_lw/src/rrtmg_lw_cldprmc.f90,v $
!     author:    $Author: mike $
!     revision:  $Revision: 1.8 $
!     created:   $Date: 2009/05/22 21:04:30 $
!
      module rrtmg_lw_cldprmc

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! --------- Modules ----------

          USE parkind, ONLY: im => kind_im, rb => kind_rb
          USE parrrtm, ONLY: ngptlw, nbndlw
          USE rrlw_cld, ONLY: absliq0, absliq1, absice0, absice1, absice2, absice3
          USE rrlw_wvn, ONLY: ngb
          USE rrlw_vsn, ONLY: hvrclc

          IMPLICIT NONE

      contains

! ------------------------------------------------------------------------------
      subroutine cldprmc(nlayers, inflag, iceflag, liqflag, cldfmc, &
                         ciwpmc, clwpmc, reicmc, relqmc, ncbands, taucmc)
! ------------------------------------------------------------------------------

! Purpose:  Compute the cloud optical depth(s) for each cloudy layer.

! ------- Input -------

      integer(kind=im), intent(in) :: nlayers         ! total number of layers
      integer(kind=im), intent(in) :: inflag          ! see definitions
      integer(kind=im), intent(in) :: iceflag         ! see definitions
      integer(kind=im), intent(in) :: liqflag         ! see definitions

      real(kind=rb), intent(in) :: cldfmc(:,:)        ! cloud fraction [mcica]
                                                      !    Dimensions: (ngptlw,nlayers)
      real(kind=rb), intent(in) :: ciwpmc(:,:)        ! cloud ice water path [mcica]
                                                      !    Dimensions: (ngptlw,nlayers)
      real(kind=rb), intent(in) :: clwpmc(:,:)        ! cloud liquid water path [mcica]
                                                      !    Dimensions: (ngptlw,nlayers)
      real(kind=rb), intent(in) :: relqmc(:)          ! liquid particle effective radius (microns)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: reicmc(:)          ! ice particle effective radius (microns)
                                                      !    Dimensions: (nlayers)
                                                      ! specific definition of reicmc depends on setting of iceflag:
                                                      ! iceflag = 0: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                                      !              r_ec must be >= 10.0 microns
                                                      ! iceflag = 1: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                                      !              r_ec range is limited to 13.0 to 130.0 microns
                                                      ! iceflag = 2: ice effective radius, r_k, (Key, Streamer Ref. Manual, 1996)
                                                      !              r_k range is limited to 5.0 to 131.0 microns
                                                      ! iceflag = 3: generalized effective size, dge, (Fu, 1996),
                                                      !              dge range is limited to 5.0 to 140.0 microns
                                                      !              [dge = 1.0315 * r_ec]

! ------- Output -------

      integer(kind=im), intent(out) :: ncbands        ! number of cloud spectral bands
      real(kind=rb), intent(inout) :: taucmc(:,:)     ! cloud optical depth [mcica]
                                                      !    Dimensions: (ngptlw,nlayers)

! ------- Local -------

      integer(kind=im) :: lay                         ! Layer index
      integer(kind=im) :: ib                          ! spectral band index
      integer(kind=im) :: ig                          ! g-point interval index
      integer(kind=im) :: index 
      integer(kind=im) :: icb(nbndlw)

      real(kind=rb) :: abscoice(ngptlw)               ! ice absorption coefficients
      real(kind=rb) :: abscoliq(ngptlw)               ! liquid absorption coefficients
      real(kind=rb) :: cwp                            ! cloud water path
      real(kind=rb) :: radice                         ! cloud ice effective size (microns)
      real(kind=rb) :: factor                         ! 
      real(kind=rb) :: fint                           ! 
      real(kind=rb) :: radliq                         ! cloud liquid droplet radius (microns)
      real(kind=rb), parameter :: eps = 1.e-6_rb      ! epsilon
      real(kind=rb), parameter :: cldmin = 1.e-20_rb  ! minimum value for cloud quantities

! ------- Definitions -------

!     Explanation of the method for each value of INFLAG.  Values of
!     0 or 1 for INFLAG do not distingish being liquid and ice clouds.
!     INFLAG = 2 does distinguish between liquid and ice clouds, and
!     requires further user input to specify the method to be used to 
!     compute the aborption due to each.
!     INFLAG = 0:  For each cloudy layer, the cloud fraction and (gray)
!                  optical depth are input.  
!     INFLAG = 1:  For each cloudy layer, the cloud fraction and cloud
!                  water path (g/m2) are input.  The (gray) cloud optical 
!                  depth is computed as in CCM2.
!     INFLAG = 2:  For each cloudy layer, the cloud fraction, cloud 
!                  water path (g/m2), and cloud ice fraction are input.
!       ICEFLAG = 0:  The ice effective radius (microns) is input and the
!                     optical depths due to ice clouds are computed as in CCM3.
!       ICEFLAG = 1:  The ice effective radius (microns) is input and the
!                     optical depths due to ice clouds are computed as in 
!                     Ebert and Curry, JGR, 97, 3831-3836 (1992).  The 
!                     spectral regions in this work have been matched with
!                     the spectral bands in RRTM to as great an extent 
!                     as possible:  
!                     E&C 1      IB = 5      RRTM bands 9-16
!                     E&C 2      IB = 4      RRTM bands 6-8
!                     E&C 3      IB = 3      RRTM bands 3-5
!                     E&C 4      IB = 2      RRTM band 2
!                     E&C 5      IB = 1      RRTM band 1
!       ICEFLAG = 2:  The ice effective radius (microns) is input and the
!                     optical properties due to ice clouds are computed from
!                     the optical properties stored in the RT code,
!                     STREAMER v3.0 (Reference: Key. J., Streamer 
!                     User's Guide, Cooperative Institute for
!                     Meteorological Satellite Studies, 2001, 96 pp.).
!                     Valid range of values for re are between 5.0 and
!                     131.0 micron.
!       ICEFLAG = 3: The ice generalized effective size (dge) is input
!                    and the optical properties, are calculated as in
!                    Q. Fu, J. Climate, (1998). Q. Fu provided high resolution
!                    tables which were appropriately averaged for the
!                    bands in RRTM_LW.  Linear interpolation is used to
!                    get the coefficients from the stored tables.
!                    Valid range of values for dge are between 5.0 and
!                    140.0 micron.
!       LIQFLAG = 0:  The optical depths due to water clouds are computed as
!                     in CCM3.
!       LIQFLAG = 1:  The water droplet effective radius (microns) is input 
!                     and the optical depths due to water clouds are computed 
!                     as in Hu and Stamnes, J., Clim., 6, 728-742, (1993).
!                     The values for absorption coefficients appropriate for
!                     the spectral bands in RRTM have been obtained for a 
!                     range of effective radii by an averaging procedure 
!                     based on the work of J. Pinto (private communication).
!                     Linear interpolation is used to get the absorption 
!                     coefficients for the input effective radius.

      data icb /1,2,3,3,3,4,4,4,5, 5, 5, 5, 5, 5, 5, 5/

      hvrclc = '$Revision: 1.8 $'

      ncbands = 1

! This initialization is done in rrtmg_lw_subcol.F90.
!      do lay = 1, nlayers
!         do ig = 1, ngptlw
!            taucmc(ig,lay) = 0.0_rb
!         enddo
!      enddo

! Main layer loop
      do lay = 1, nlayers

        do ig = 1, ngptlw
          cwp = ciwpmc(ig,lay) + clwpmc(ig,lay)
          if (cldfmc(ig,lay) .ge. cldmin .and. &
             (cwp .ge. cldmin .or. taucmc(ig,lay) .ge. cldmin)) then

! Ice clouds and water clouds combined.
            if (inflag .eq. 0) then
! Cloud optical depth already defined in taucmc, return to main program
               return

            elseif(inflag .eq. 1) then 
                stop 'INFLAG = 1 OPTION NOT AVAILABLE WITH MCICA'
!               cwp = ciwpmc(ig,lay) + clwpmc(ig,lay)
!               taucmc(ig,lay) = abscld1 * cwp

! Separate treatement of ice clouds and water clouds.
            elseif(inflag .eq. 2) then
               radice = reicmc(lay)

! Calculation of absorption coefficients due to ice clouds.
               if (ciwpmc(ig,lay) .eq. 0.0_rb) then
                  abscoice(ig) = 0.0_rb

               elseif (iceflag .eq. 0) then
                  if (radice .lt. 10.0_rb) stop 'ICE RADIUS TOO SMALL'
                  abscoice(ig) = absice0(1) + absice0(2)/radice

               elseif (iceflag .eq. 1) then
                  if (radice .lt. 13.0_rb .or. radice .gt. 130._rb) stop &
                      'ICE RADIUS OUT OF BOUNDS'
                  ncbands = 5
                  ib = icb(ngb(ig))
                  abscoice(ig) = absice1(1,ib) + absice1(2,ib)/radice

! For iceflag=2 option, ice particle effective radius is limited to 5.0 to 131.0 microns

               elseif (iceflag .eq. 2) then
                  if (radice .lt. 5.0_rb .or. radice .gt. 131.0_rb) stop 'ICE RADIUS OUT OF BOUNDS'
                     ncbands = 16
                     factor = (radice - 2._rb)/3._rb
                     index = int(factor)
                     if (index .eq. 43) index = 42
                     fint = factor - float(index)
                     ib = ngb(ig)
                     abscoice(ig) = &
                         absice2(index,ib) + fint * &
                         (absice2(index+1,ib) - (absice2(index,ib))) 
               
! For iceflag=3 option, ice particle generalized effective size is limited to 5.0 to 140.0 microns

               elseif (iceflag .eq. 3) then
                  if (radice .lt. 5.0_rb .or. radice .gt. 140.0_rb) stop 'ICE GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'
                     ncbands = 16
                     factor = (radice - 2._rb)/3._rb
                     index = int(factor)
                     if (index .eq. 46) index = 45
                     fint = factor - float(index)
                     ib = ngb(ig)
                     abscoice(ig) = &
                         absice3(index,ib) + fint * &
                         (absice3(index+1,ib) - (absice3(index,ib)))
   
               endif
                  
! Calculation of absorption coefficients due to water clouds.
               if (clwpmc(ig,lay) .eq. 0.0_rb) then
                  abscoliq(ig) = 0.0_rb

               elseif (liqflag .eq. 0) then
                   abscoliq(ig) = absliq0

               elseif (liqflag .eq. 1) then
                  radliq = relqmc(lay)
                  if (radliq .lt. 2.5_rb .or. radliq .gt. 60._rb) stop &
                       'LIQUID EFFECTIVE RADIUS OUT OF BOUNDS'
                  index = int(radliq - 1.5_rb)
                  if (index .eq. 0) index = 1
                  if (index .eq. 58) index = 57
                  fint = radliq - 1.5_rb - float(index)
                  ib = ngb(ig)
                  abscoliq(ig) = &
                        absliq1(index,ib) + fint * &
                        (absliq1(index+1,ib) - (absliq1(index,ib)))
               endif

               taucmc(ig,lay) = ciwpmc(ig,lay) * abscoice(ig) + &
                                clwpmc(ig,lay) * abscoliq(ig)

            endif
         endif
         enddo
      enddo

      end subroutine cldprmc

      end module rrtmg_lw_cldprmc

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!
      module rrtmg_lw_rtrnmc

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! --------- Modules ----------

          USE parkind, ONLY: im => kind_im, rb => kind_rb
          USE parrrtm, ONLY: nbndlw, ngptlw
          USE rrlw_con, ONLY: fluxfac, heatfac
          USE rrlw_wvn, ONLY: delwave, ngb, ngs
          USE rrlw_tbl, ONLY: tblint, bpade, tau_tbl, exp_tbl, tfn_tbl
          USE rrlw_vsn, ONLY: hvrrtc

          IMPLICIT NONE

      real(kind=rb) :: wtdiff, rec_6
      real(kind=rb) :: a0(nbndlw),a1(nbndlw),a2(nbndlw)! diffusivity angle adjustment coefficients

! This secant and weight corresponds to the standard diffusivity 
! angle.  This initial value is redefined below for some bands.

! Reset diffusivity angle for Bands 2-3 and 5-9 to vary (between 1.50
! and 1.80) as a function of total column water vapor.  The function
! has been defined to minimize flux and cooling rate errors in these bands
! over a wide range of precipitable water values.

      PUBLIC kr_externs_in_rrtmg_lw_rtrnmc
      PUBLIC kr_externs_out_rrtmg_lw_rtrnmc
      contains

!-----------------------------------------------------------------------------
      subroutine rtrnmc(nlayers, istart, iend, iout, pz, semiss, ncbands, &
                        cldfmc, taucmc, planklay, planklev, plankbnd, &
                        pwvcm, fracs, taut, &
                        totuflux, totdflux, fnet, htr, &
                        totuclfl, totdclfl, fnetc, htrc ) 
!-----------------------------------------------------------------------------
!
!  Original version:   E. J. Mlawer, et al. RRTM_V3.0
!  Revision for GCMs:  Michael J. Iacono; October, 2002
!  Revision for F90:  Michael J. Iacono; June, 2006
!
!  This program calculates the upward fluxes, downward fluxes, and
!  heating rates for an arbitrary clear or cloudy atmosphere.  The input
!  to this program is the atmospheric profile, all Planck function
!  information, and the cloud fraction by layer.  A variable diffusivity 
!  angle (SECDIFF) is used for the angle integration.  Bands 2-3 and 5-9 
!  use a value for SECDIFF that varies from 1.50 to 1.80 as a function of 
!  the column water vapor, and other bands use a value of 1.66.  The Gaussian 
!  weight appropriate to this angle (WTDIFF=0.5) is applied here.  Note that 
!  use of the emissivity angle for the flux integration can cause errors of 
!  1 to 4 W/m2 within cloudy layers.  
!  Clouds are treated with the McICA stochastic approach and maximum-random
!  cloud overlap. 
!***************************************************************************

! ------- Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: nlayers         ! total number of layers
      integer(kind=im), intent(in) :: istart          ! beginning band of calculation
      integer(kind=im), intent(in) :: iend            ! ending band of calculation
      integer(kind=im), intent(in) :: iout            ! output option flag

! Atmosphere
      real(kind=rb), intent(in) :: pz(0:)             ! level (interface) pressures (hPa, mb)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(in) :: pwvcm              ! precipitable water vapor (cm)
      real(kind=rb), intent(in) :: semiss(:)          ! lw surface emissivity
                                                      !    Dimensions: (nbndlw)
      real(kind=rb), intent(in) :: planklay(:,:)      ! 
                                                      !    Dimensions: (nlayers,nbndlw)
      real(kind=rb), intent(in) :: planklev(0:,:)     ! 
                                                      !    Dimensions: (0:nlayers,nbndlw)
      real(kind=rb), intent(in) :: plankbnd(:)        ! 
                                                      !    Dimensions: (nbndlw)
      real(kind=rb), intent(in) :: fracs(:,:)         ! 
                                                      !    Dimensions: (nlayers,ngptw)
      real(kind=rb), intent(in) :: taut(:,:)          ! gaseous + aerosol optical depths
                                                      !    Dimensions: (nlayers,ngptlw)

! Clouds
      integer(kind=im), intent(in) :: ncbands         ! number of cloud spectral bands
      real(kind=rb), intent(in) :: cldfmc(:,:)        ! layer cloud fraction [mcica]
                                                      !    Dimensions: (ngptlw,nlayers)
      real(kind=rb), intent(in) :: taucmc(:,:)        ! layer cloud optical depth [mcica]
                                                      !    Dimensions: (ngptlw,nlayers)

! ----- Output -----
      real(kind=rb), intent(out) :: totuflux(0:)      ! upward longwave flux (w/m2)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(out) :: totdflux(0:)      ! downward longwave flux (w/m2)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(out) :: fnet(0:)          ! net longwave flux (w/m2)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(out) :: htr(0:)           ! longwave heating rate (k/day)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(out) :: totuclfl(0:)      ! clear sky upward longwave flux (w/m2)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(out) :: totdclfl(0:)      ! clear sky downward longwave flux (w/m2)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(out) :: fnetc(0:)         ! clear sky net longwave flux (w/m2)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(out) :: htrc(0:)          ! clear sky longwave heating rate (k/day)
                                                      !    Dimensions: (0:nlayers)

! ----- Local -----
! Declarations for radiative transfer
      real(kind=rb) :: abscld(nlayers,ngptlw)
      real(kind=rb) :: atot(nlayers)
      real(kind=rb) :: atrans(nlayers)
      real(kind=rb) :: bbugas(nlayers)
      real(kind=rb) :: bbutot(nlayers)
      real(kind=rb) :: clrurad(0:nlayers)
      real(kind=rb) :: clrdrad(0:nlayers)
      real(kind=rb) :: efclfrac(nlayers,ngptlw)
      real(kind=rb) :: uflux(0:nlayers)
      real(kind=rb) :: dflux(0:nlayers)
      real(kind=rb) :: urad(0:nlayers)
      real(kind=rb) :: drad(0:nlayers)
      real(kind=rb) :: uclfl(0:nlayers)
      real(kind=rb) :: dclfl(0:nlayers)
      real(kind=rb) :: odcld(nlayers,ngptlw)


      real(kind=rb) :: secdiff(nbndlw)                 ! secant of diffusivity angle
      real(kind=rb) :: transcld, radld, radclrd, plfrac, blay, dplankup, dplankdn
      real(kind=rb) :: odepth, odtot, odepth_rec, odtot_rec, gassrc
      real(kind=rb) :: tblind, tfactot, bbd, bbdtot, tfacgas, transc, tausfac
      real(kind=rb) :: rad0, reflect, radlu, radclru

      integer(kind=im) :: icldlyr(nlayers)                  ! flag for cloud in layer
      integer(kind=im) :: ibnd, ib, iband, lay, lev, l, ig  ! loop indices
      integer(kind=im) :: igc                               ! g-point interval counter
      integer(kind=im) :: iclddn                            ! flag for cloud in down path
      integer(kind=im) :: ittot, itgas, itr                 ! lookup table indices

! ------- Definitions -------
! input
!    nlayers                      ! number of model layers
!    ngptlw                       ! total number of g-point subintervals
!    nbndlw                       ! number of longwave spectral bands
!    ncbands                      ! number of spectral bands for clouds
!    secdiff                      ! diffusivity angle
!    wtdiff                       ! weight for radiance to flux conversion
!    pavel                        ! layer pressures (mb)
!    pz                           ! level (interface) pressures (mb)
!    tavel                        ! layer temperatures (k)
!    tz                           ! level (interface) temperatures(mb)
!    tbound                       ! surface temperature (k)
!    cldfrac                      ! layer cloud fraction
!    taucloud                     ! layer cloud optical depth
!    itr                          ! integer look-up table index
!    icldlyr                      ! flag for cloudy layers
!    iclddn                       ! flag for cloud in column at any layer
!    semiss                       ! surface emissivities for each band
!    reflect                      ! surface reflectance
!    bpade                        ! 1/(pade constant)
!    tau_tbl                      ! clear sky optical depth look-up table
!    exp_tbl                      ! exponential look-up table for transmittance
!    tfn_tbl                      ! tau transition function look-up table

! local
!    atrans                       ! gaseous absorptivity
!    abscld                       ! cloud absorptivity
!    atot                         ! combined gaseous and cloud absorptivity
!    odclr                        ! clear sky (gaseous) optical depth
!    odcld                        ! cloud optical depth
!    odtot                        ! optical depth of gas and cloud
!    tfacgas                      ! gas-only pade factor, used for planck fn
!    tfactot                      ! gas and cloud pade factor, used for planck fn
!    bbdgas                       ! gas-only planck function for downward rt
!    bbugas                       ! gas-only planck function for upward rt
!    bbdtot                       ! gas and cloud planck function for downward rt
!    bbutot                       ! gas and cloud planck function for upward calc.
!    gassrc                       ! source radiance due to gas only
!    efclfrac                     ! effective cloud fraction
!    radlu                        ! spectrally summed upward radiance 
!    radclru                      ! spectrally summed clear sky upward radiance 
!    urad                         ! upward radiance by layer
!    clrurad                      ! clear sky upward radiance by layer
!    radld                        ! spectrally summed downward radiance 
!    radclrd                      ! spectrally summed clear sky downward radiance 
!    drad                         ! downward radiance by layer
!    clrdrad                      ! clear sky downward radiance by layer

! output
!    totuflux                     ! upward longwave flux (w/m2)
!    totdflux                     ! downward longwave flux (w/m2)
!    fnet                         ! net longwave flux (w/m2)
!    htr                          ! longwave heating rate (k/day)
!    totuclfl                     ! clear sky upward longwave flux (w/m2)
!    totdclfl                     ! clear sky downward longwave flux (w/m2)
!    fnetc                        ! clear sky net longwave flux (w/m2)
!    htrc                         ! clear sky longwave heating rate (k/day)


      hvrrtc = '$Revision: 1.3 $'

      do ibnd = 1,nbndlw
         if (ibnd.eq.1 .or. ibnd.eq.4 .or. ibnd.ge.10) then
           secdiff(ibnd) = 1.66_rb
         else
           secdiff(ibnd) = a0(ibnd) + a1(ibnd)*exp(a2(ibnd)*pwvcm)
           if (secdiff(ibnd) .gt. 1.80_rb) secdiff(ibnd) = 1.80_rb
           if (secdiff(ibnd) .lt. 1.50_rb) secdiff(ibnd) = 1.50_rb
         endif
      enddo

      urad(0) = 0.0_rb
      drad(0) = 0.0_rb
      totuflux(0) = 0.0_rb
      totdflux(0) = 0.0_rb
      clrurad(0) = 0.0_rb
      clrdrad(0) = 0.0_rb
      totuclfl(0) = 0.0_rb
      totdclfl(0) = 0.0_rb

      do lay = 1, nlayers
         urad(lay) = 0.0_rb
         drad(lay) = 0.0_rb
         totuflux(lay) = 0.0_rb
         totdflux(lay) = 0.0_rb
         clrurad(lay) = 0.0_rb
         clrdrad(lay) = 0.0_rb
         totuclfl(lay) = 0.0_rb
         totdclfl(lay) = 0.0_rb
         icldlyr(lay) = 0

! Change to band loop?
         do ig = 1, ngptlw
            if (cldfmc(ig,lay) .eq. 1._rb) then
               ib = ngb(ig)
               odcld(lay,ig) = secdiff(ib) * taucmc(ig,lay)
               transcld = exp(-odcld(lay,ig))
               abscld(lay,ig) = 1._rb - transcld
               efclfrac(lay,ig) = abscld(lay,ig) * cldfmc(ig,lay)
               icldlyr(lay) = 1
            else
               odcld(lay,ig) = 0.0_rb
               abscld(lay,ig) = 0.0_rb
               efclfrac(lay,ig) = 0.0_rb
            endif
         enddo

      enddo

      igc = 1
! Loop over frequency bands.
      do iband = istart, iend

! Reinitialize g-point counter for each band if output for each band is requested.
         if (iout.gt.0.and.iband.ge.2) igc = ngs(iband-1)+1

! Loop over g-channels.
 1000    continue

! Radiative transfer starts here.
         radld = 0._rb
         radclrd = 0._rb
         iclddn = 0

! Downward radiative transfer loop.  

         do lev = nlayers, 1, -1
               plfrac = fracs(lev,igc)
               blay = planklay(lev,iband)
               dplankup = planklev(lev,iband) - blay
               dplankdn = planklev(lev-1,iband) - blay
               odepth = secdiff(iband) * taut(lev,igc)
               if (odepth .lt. 0.0_rb) odepth = 0.0_rb
!  Cloudy layer
               if (icldlyr(lev).eq.1) then
                  iclddn = 1
                  odtot = odepth + odcld(lev,igc)
                  if (odtot .lt. 0.06_rb) then
                     atrans(lev) = odepth - 0.5_rb*odepth*odepth
                     odepth_rec = rec_6*odepth
                     gassrc = plfrac*(blay+dplankdn*odepth_rec)*atrans(lev)

                     atot(lev) =  odtot - 0.5_rb*odtot*odtot
                     odtot_rec = rec_6*odtot
                     bbdtot =  plfrac * (blay+dplankdn*odtot_rec)
                     bbd = plfrac*(blay+dplankdn*odepth_rec)
                     radld = radld - radld * (atrans(lev) + &
                         efclfrac(lev,igc) * (1. - atrans(lev))) + &
                         gassrc + cldfmc(igc,lev) * &
                         (bbdtot * atot(lev) - gassrc)
                     drad(lev-1) = drad(lev-1) + radld
                  
                     bbugas(lev) =  plfrac * (blay+dplankup*odepth_rec)
                     bbutot(lev) =  plfrac * (blay+dplankup*odtot_rec)

                  elseif (odepth .le. 0.06_rb) then
                     atrans(lev) = odepth - 0.5_rb*odepth*odepth
                     odepth_rec = rec_6*odepth
                     gassrc = plfrac*(blay+dplankdn*odepth_rec)*atrans(lev)

                     odtot = odepth + odcld(lev,igc)
                     tblind = odtot/(bpade+odtot)
                     ittot = tblint*tblind + 0.5_rb
                     tfactot = tfn_tbl(ittot)
                     bbdtot = plfrac * (blay + tfactot*dplankdn)
                     bbd = plfrac*(blay+dplankdn*odepth_rec)
                     atot(lev) = 1. - exp_tbl(ittot)

                     radld = radld - radld * (atrans(lev) + &
                         efclfrac(lev,igc) * (1._rb - atrans(lev))) + &
                         gassrc + cldfmc(igc,lev) * &
                         (bbdtot * atot(lev) - gassrc)
                     drad(lev-1) = drad(lev-1) + radld

                     bbugas(lev) = plfrac * (blay + dplankup*odepth_rec)
                     bbutot(lev) = plfrac * (blay + tfactot * dplankup)

                  else

                     tblind = odepth/(bpade+odepth)
                     itgas = tblint*tblind+0.5_rb
                     odepth = tau_tbl(itgas)
                     atrans(lev) = 1._rb - exp_tbl(itgas)
                     tfacgas = tfn_tbl(itgas)
                     gassrc = atrans(lev) * plfrac * (blay + tfacgas*dplankdn)

                     odtot = odepth + odcld(lev,igc)
                     tblind = odtot/(bpade+odtot)
                     ittot = tblint*tblind + 0.5_rb
                     tfactot = tfn_tbl(ittot)
                     bbdtot = plfrac * (blay + tfactot*dplankdn)
                     bbd = plfrac*(blay+tfacgas*dplankdn)
                     atot(lev) = 1._rb - exp_tbl(ittot)

                  radld = radld - radld * (atrans(lev) + &
                    efclfrac(lev,igc) * (1._rb - atrans(lev))) + &
                    gassrc + cldfmc(igc,lev) * &
                    (bbdtot * atot(lev) - gassrc)
                  drad(lev-1) = drad(lev-1) + radld
                  bbugas(lev) = plfrac * (blay + tfacgas * dplankup)
                  bbutot(lev) = plfrac * (blay + tfactot * dplankup)
                  endif
!  Clear layer
               else
                  if (odepth .le. 0.06_rb) then
                     atrans(lev) = odepth-0.5_rb*odepth*odepth
                     odepth = rec_6*odepth
                     bbd = plfrac*(blay+dplankdn*odepth)
                     bbugas(lev) = plfrac*(blay+dplankup*odepth)
                  else
                     tblind = odepth/(bpade+odepth)
                     itr = tblint*tblind+0.5_rb
                     transc = exp_tbl(itr)
                     atrans(lev) = 1._rb-transc
                     tausfac = tfn_tbl(itr)
                     bbd = plfrac*(blay+tausfac*dplankdn)
                     bbugas(lev) = plfrac * (blay + tausfac * dplankup)
                  endif   
                  radld = radld + (bbd-radld)*atrans(lev)
                  drad(lev-1) = drad(lev-1) + radld
               endif
!  Set clear sky stream to total sky stream as long as layers
!  remain clear.  Streams diverge when a cloud is reached (iclddn=1),
!  and clear sky stream must be computed separately from that point.
                  if (iclddn.eq.1) then
                     radclrd = radclrd + (bbd-radclrd) * atrans(lev) 
                     clrdrad(lev-1) = clrdrad(lev-1) + radclrd
                  else
                     radclrd = radld
                     clrdrad(lev-1) = drad(lev-1)
                  endif
            enddo

! Spectral emissivity & reflectance
!  Include the contribution of spectrally varying longwave emissivity
!  and reflection from the surface to the upward radiative transfer.
!  Note: Spectral and Lambertian reflection are identical for the
!  diffusivity angle flux integration used here.

         rad0 = fracs(1,igc) * plankbnd(iband)
!  Add in specular reflection of surface downward radiance.
         reflect = 1._rb - semiss(iband)
         radlu = rad0 + reflect * radld
         radclru = rad0 + reflect * radclrd


! Upward radiative transfer loop.
         urad(0) = urad(0) + radlu
         clrurad(0) = clrurad(0) + radclru

         do lev = 1, nlayers
!  Cloudy layer
            if (icldlyr(lev) .eq. 1) then
               gassrc = bbugas(lev) * atrans(lev)
               radlu = radlu - radlu * (atrans(lev) + &
                   efclfrac(lev,igc) * (1._rb - atrans(lev))) + &
                   gassrc + cldfmc(igc,lev) * &
                   (bbutot(lev) * atot(lev) - gassrc)
               urad(lev) = urad(lev) + radlu
!  Clear layer
            else
               radlu = radlu + (bbugas(lev)-radlu)*atrans(lev)
               urad(lev) = urad(lev) + radlu
            endif
!  Set clear sky stream to total sky stream as long as all layers
!  are clear (iclddn=0).  Streams must be calculated separately at 
!  all layers when a cloud is present (ICLDDN=1), because surface 
!  reflectance is different for each stream.
               if (iclddn.eq.1) then
                  radclru = radclru + (bbugas(lev)-radclru)*atrans(lev) 
                  clrurad(lev) = clrurad(lev) + radclru
               else
                  radclru = radlu
                  clrurad(lev) = urad(lev)
               endif
         enddo

! Increment g-point counter
         igc = igc + 1
! Return to continue radiative transfer for all g-channels in present band
         if (igc .le. ngs(iband)) go to 1000

! Process longwave output from band for total and clear streams.
! Calculate upward, downward, and net flux.
         do lev = nlayers, 0, -1
            uflux(lev) = urad(lev)*wtdiff
            dflux(lev) = drad(lev)*wtdiff
            urad(lev) = 0.0_rb
            drad(lev) = 0.0_rb
            totuflux(lev) = totuflux(lev) + uflux(lev) * delwave(iband)
            totdflux(lev) = totdflux(lev) + dflux(lev) * delwave(iband)
            uclfl(lev) = clrurad(lev)*wtdiff
            dclfl(lev) = clrdrad(lev)*wtdiff
            clrurad(lev) = 0.0_rb
            clrdrad(lev) = 0.0_rb
            totuclfl(lev) = totuclfl(lev) + uclfl(lev) * delwave(iband)
            totdclfl(lev) = totdclfl(lev) + dclfl(lev) * delwave(iband)
         enddo

! End spectral band loop
      enddo

! Calculate fluxes at surface
      totuflux(0) = totuflux(0) * fluxfac
      totdflux(0) = totdflux(0) * fluxfac
      fnet(0) = totuflux(0) - totdflux(0)
      totuclfl(0) = totuclfl(0) * fluxfac
      totdclfl(0) = totdclfl(0) * fluxfac
      fnetc(0) = totuclfl(0) - totdclfl(0)

! Calculate fluxes at model levels
      do lev = 1, nlayers
         totuflux(lev) = totuflux(lev) * fluxfac
         totdflux(lev) = totdflux(lev) * fluxfac
         fnet(lev) = totuflux(lev) - totdflux(lev)
         totuclfl(lev) = totuclfl(lev) * fluxfac
         totdclfl(lev) = totdclfl(lev) * fluxfac
         fnetc(lev) = totuclfl(lev) - totdclfl(lev)
         l = lev - 1

! Calculate heating rates at model layers
         htr(l)=heatfac*(fnet(l)-fnet(lev))/(pz(l)-pz(lev)) 
         htrc(l)=heatfac*(fnetc(l)-fnetc(lev))/(pz(l)-pz(lev)) 
      enddo

! Set heating rate to zero in top layer
      htr(nlayers) = 0.0_rb
      htrc(nlayers) = 0.0_rb

      end subroutine rtrnmc

      !read state subroutine for kr_externs_in_rrtmg_lw_rtrnmc
      SUBROUTINE kr_externs_in_rrtmg_lw_rtrnmc(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          
          READ (UNIT = kgen_unit) rec_6
          READ (UNIT = kgen_unit) wtdiff
          READ (UNIT = kgen_unit) a1
          READ (UNIT = kgen_unit) a0
          READ (UNIT = kgen_unit) a2
      END SUBROUTINE kr_externs_in_rrtmg_lw_rtrnmc
      
      !read state subroutine for kr_externs_out_rrtmg_lw_rtrnmc
      SUBROUTINE kr_externs_out_rrtmg_lw_rtrnmc(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          
          LOGICAL :: kgen_istrue
      END SUBROUTINE kr_externs_out_rrtmg_lw_rtrnmc
      
      end module rrtmg_lw_rtrnmc

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!
      module rrtmg_lw_setcoef

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! ------- Modules -------

          USE parkind, ONLY: im => kind_im, rb => kind_rb
          USE rrlw_wvn, ONLY: totplnk, totplk16
          USE rrlw_ref
          USE rrlw_vsn, ONLY: hvrset

          IMPLICIT NONE

      contains

!----------------------------------------------------------------------------
      subroutine setcoef(nlayers, istart, pavel, tavel, tz, tbound, semiss, &
                         coldry, wkl, wbroad, &
                         laytrop, jp, jt, jt1, planklay, planklev, plankbnd, &
                         colh2o, colco2, colo3, coln2o, colco, colch4, colo2, &
                         colbrd, fac00, fac01, fac10, fac11, &
                         rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1, &
                         rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1, &
                         rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1, &
                         selffac, selffrac, indself, forfac, forfrac, indfor, &
                         minorfrac, scaleminor, scaleminorn2, indminor)
!----------------------------------------------------------------------------
!
!  Purpose:  For a given atmosphere, calculate the indices and
!  fractions related to the pressure and temperature interpolations.
!  Also calculate the values of the integrated Planck functions 
!  for each band at the level and layer temperatures.

! ------- Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: nlayers         ! total number of layers
      integer(kind=im), intent(in) :: istart          ! beginning band of calculation

      real(kind=rb), intent(in) :: pavel(:)           ! layer pressures (mb) 
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: tavel(:)           ! layer temperatures (K)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: tz(0:)             ! level (interface) temperatures (K)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(in) :: tbound             ! surface temperature (K)
      real(kind=rb), intent(in) :: coldry(:)          ! dry air column density (mol/cm2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: wbroad(:)          ! broadening gas column density (mol/cm2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: wkl(:,:)           ! molecular amounts (mol/cm-2)
                                                      !    Dimensions: (mxmol,nlayers)
      real(kind=rb), intent(in) :: semiss(:)          ! lw surface emissivity
                                                      !    Dimensions: (nbndlw)

! ----- Output -----
      integer(kind=im), intent(out) :: laytrop        ! tropopause layer index
      integer(kind=im), intent(out) :: jp(:)          ! 
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: jt(:)          !
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: jt1(:)         !
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: planklay(:,:)     ! 
                                                      !    Dimensions: (nlayers,nbndlw)
      real(kind=rb), intent(out) :: planklev(0:,:)    ! 
                                                      !    Dimensions: (0:nlayers,nbndlw)
      real(kind=rb), intent(out) :: plankbnd(:)       ! 
                                                      !    Dimensions: (nbndlw)

      real(kind=rb), intent(out) :: colh2o(:)         ! column amount (h2o)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colco2(:)         ! column amount (co2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colo3(:)          ! column amount (o3)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: coln2o(:)         ! column amount (n2o)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colco(:)          ! column amount (co)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colch4(:)         ! column amount (ch4)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colo2(:)          ! column amount (o2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colbrd(:)         ! column amount (broadening gases)
                                                      !    Dimensions: (nlayers)

      integer(kind=im), intent(out) :: indself(:)
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: indfor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: selffac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: selffrac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: forfac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: forfrac(:)
                                                      !    Dimensions: (nlayers)

      integer(kind=im), intent(out) :: indminor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: minorfrac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: scaleminor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: scaleminorn2(:)
                                                      !    Dimensions: (nlayers)

      real(kind=rb), intent(out) :: &                 !
                       fac00(:), fac01(:), &          !    Dimensions: (nlayers)
                       fac10(:), fac11(:) 
                                                        
      real(kind=rb), intent(out) :: &                 !
                       rat_h2oco2(:),rat_h2oco2_1(:), &
                       rat_h2oo3(:),rat_h2oo3_1(:), & !    Dimensions: (nlayers)
                       rat_h2on2o(:),rat_h2on2o_1(:), &
                       rat_h2och4(:),rat_h2och4_1(:), &
                       rat_n2oco2(:),rat_n2oco2_1(:), &
                       rat_o3co2(:),rat_o3co2_1(:)
                                                        

! ----- Local -----
      integer(kind=im) :: indbound, indlev0
      integer(kind=im) :: lay, indlay, indlev, iband
      integer(kind=im) :: jp1
      real(kind=rb) :: stpfac, tbndfrac, t0frac, tlayfrac, tlevfrac
      real(kind=rb) :: dbdtlev, dbdtlay
      real(kind=rb) :: plog, fp, ft, ft1, water, scalefac, factor, compfp


      hvrset = '$Revision: 1.3 $'

      stpfac = 296._rb/1013._rb

      indbound = tbound - 159._rb
      if (indbound .lt. 1) then
         indbound = 1
      elseif (indbound .gt. 180) then
         indbound = 180
      endif
      tbndfrac = tbound - 159._rb - float(indbound)
      indlev0 = tz(0) - 159._rb
      if (indlev0 .lt. 1) then
         indlev0 = 1
      elseif (indlev0 .gt. 180) then
         indlev0 = 180
      endif
      t0frac = tz(0) - 159._rb - float(indlev0)
      laytrop = 0

! Begin layer loop 
!  Calculate the integrated Planck functions for each band at the
!  surface, level, and layer temperatures.
      do lay = 1, nlayers
         indlay = tavel(lay) - 159._rb
         if (indlay .lt. 1) then
            indlay = 1
         elseif (indlay .gt. 180) then
            indlay = 180
         endif
         tlayfrac = tavel(lay) - 159._rb - float(indlay)
         indlev = tz(lay) - 159._rb
         if (indlev .lt. 1) then
            indlev = 1
         elseif (indlev .gt. 180) then
            indlev = 180
         endif
         tlevfrac = tz(lay) - 159._rb - float(indlev)

! Begin spectral band loop 
         do iband = 1, 15
            if (lay.eq.1) then
               dbdtlev = totplnk(indbound+1,iband) - totplnk(indbound,iband)
               plankbnd(iband) = semiss(iband) * &
                   (totplnk(indbound,iband) + tbndfrac * dbdtlev)
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplnk(indlev0,iband) + t0frac * dbdtlev
            endif
            dbdtlev = totplnk(indlev+1,iband) - totplnk(indlev,iband)
            dbdtlay = totplnk(indlay+1,iband) - totplnk(indlay,iband)
            planklay(lay,iband) = totplnk(indlay,iband) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplnk(indlev,iband) + tlevfrac * dbdtlev
         enddo

!  For band 16, if radiative transfer will be performed on just
!  this band, use integrated Planck values up to 3250 cm-1.  
!  If radiative transfer will be performed across all 16 bands,
!  then include in the integrated Planck values for this band
!  contributions from 2600 cm-1 to infinity.
         iband = 16
         if (istart .eq. 16) then
            if (lay.eq.1) then
               dbdtlev = totplk16(indbound+1) - totplk16(indbound)
               plankbnd(iband) = semiss(iband) * &
                    (totplk16(indbound) + tbndfrac * dbdtlev)
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplk16(indlev0) + &
                    t0frac * dbdtlev
            endif
            dbdtlev = totplk16(indlev+1) - totplk16(indlev)
            dbdtlay = totplk16(indlay+1) - totplk16(indlay)
            planklay(lay,iband) = totplk16(indlay) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplk16(indlev) + tlevfrac * dbdtlev
         else
            if (lay.eq.1) then
               dbdtlev = totplnk(indbound+1,iband) - totplnk(indbound,iband)
               plankbnd(iband) = semiss(iband) * &
                    (totplnk(indbound,iband) + tbndfrac * dbdtlev)
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplnk(indlev0,iband) + t0frac * dbdtlev
            endif
            dbdtlev = totplnk(indlev+1,iband) - totplnk(indlev,iband)
            dbdtlay = totplnk(indlay+1,iband) - totplnk(indlay,iband)
            planklay(lay,iband) = totplnk(indlay,iband) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplnk(indlev,iband) + tlevfrac * dbdtlev
         endif

!  Find the two reference pressures on either side of the
!  layer pressure.  Store them in JP and JP1.  Store in FP the
!  fraction of the difference (in ln(pressure)) between these
!  two values that the layer pressure lies.
         plog = log(pavel(lay))
!         plog = dlog(pavel(lay))
         jp(lay) = int(36._rb - 5*(plog+0.04_rb))
         if (jp(lay) .lt. 1) then
            jp(lay) = 1
         elseif (jp(lay) .gt. 58) then
            jp(lay) = 58
         endif
         jp1 = jp(lay) + 1
         fp = 5._rb *(preflog(jp(lay)) - plog)

!  Determine, for each reference pressure (JP and JP1), which
!  reference temperature (these are different for each  
!  reference pressure) is nearest the layer temperature but does
!  not exceed it.  Store these indices in JT and JT1, resp.
!  Store in FT (resp. FT1) the fraction of the way between JT
!  (JT1) and the next highest reference temperature that the 
!  layer temperature falls.
         jt(lay) = int(3._rb + (tavel(lay)-tref(jp(lay)))/15._rb)
         if (jt(lay) .lt. 1) then
            jt(lay) = 1
         elseif (jt(lay) .gt. 4) then
            jt(lay) = 4
         endif
         ft = ((tavel(lay)-tref(jp(lay)))/15._rb) - float(jt(lay)-3)
         jt1(lay) = int(3._rb + (tavel(lay)-tref(jp1))/15._rb)
         if (jt1(lay) .lt. 1) then
            jt1(lay) = 1
         elseif (jt1(lay) .gt. 4) then
            jt1(lay) = 4
         endif
         ft1 = ((tavel(lay)-tref(jp1))/15._rb) - float(jt1(lay)-3)
         water = wkl(1,lay)/coldry(lay)
         scalefac = pavel(lay) * stpfac / tavel(lay)

!  If the pressure is less than ~100mb, perform a different
!  set of species interpolations.
         if (plog .le. 4.56_rb) go to 5300
         laytrop =  laytrop + 1

         forfac(lay) = scalefac / (1.+water)
         factor = (332.0_rb-tavel(lay))/36.0_rb
         indfor(lay) = min(2, max(1, int(factor)))
         forfrac(lay) = factor - float(indfor(lay))

!  Set up factors needed to separately include the water vapor
!  self-continuum in the calculation of absorption coefficient.
         selffac(lay) = water * forfac(lay)
         factor = (tavel(lay)-188.0_rb)/7.2_rb
         indself(lay) = min(9, max(1, int(factor)-7))
         selffrac(lay) = factor - float(indself(lay) + 7)

!  Set up factors needed to separately include the minor gases
!  in the calculation of absorption coefficient
         scaleminor(lay) = pavel(lay)/tavel(lay)
         scaleminorn2(lay) = (pavel(lay)/tavel(lay)) &
             *(wbroad(lay)/(coldry(lay)+wkl(1,lay)))
         factor = (tavel(lay)-180.8_rb)/7.2_rb
         indminor(lay) = min(18, max(1, int(factor)))
         minorfrac(lay) = factor - float(indminor(lay))

!  Setup reference ratio to be used in calculation of binary
!  species parameter in lower atmosphere.
         rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
         rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)

         rat_h2oo3(lay)=chi_mls(1,jp(lay))/chi_mls(3,jp(lay))
         rat_h2oo3_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(3,jp(lay)+1)

         rat_h2on2o(lay)=chi_mls(1,jp(lay))/chi_mls(4,jp(lay))
         rat_h2on2o_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(4,jp(lay)+1)

         rat_h2och4(lay)=chi_mls(1,jp(lay))/chi_mls(6,jp(lay))
         rat_h2och4_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(6,jp(lay)+1)

         rat_n2oco2(lay)=chi_mls(4,jp(lay))/chi_mls(2,jp(lay))
         rat_n2oco2_1(lay)=chi_mls(4,jp(lay)+1)/chi_mls(2,jp(lay)+1)

!  Calculate needed column amounts.
         colh2o(lay) = 1.e-20_rb * wkl(1,lay)
         colco2(lay) = 1.e-20_rb * wkl(2,lay)
         colo3(lay) = 1.e-20_rb * wkl(3,lay)
         coln2o(lay) = 1.e-20_rb * wkl(4,lay)
         colco(lay) = 1.e-20_rb * wkl(5,lay)
         colch4(lay) = 1.e-20_rb * wkl(6,lay)
         colo2(lay) = 1.e-20_rb * wkl(7,lay)
         if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
         if (colo3(lay) .eq. 0._rb) colo3(lay) = 1.e-32_rb * coldry(lay)
         if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
         if (colco(lay) .eq. 0._rb) colco(lay) = 1.e-32_rb * coldry(lay)
         if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
         colbrd(lay) = 1.e-20_rb * wbroad(lay)
         go to 5400

!  Above laytrop.
 5300    continue

         forfac(lay) = scalefac / (1.+water)
         factor = (tavel(lay)-188.0_rb)/36.0_rb
         indfor(lay) = 3
         forfrac(lay) = factor - 1.0_rb

!  Set up factors needed to separately include the water vapor
!  self-continuum in the calculation of absorption coefficient.
         selffac(lay) = water * forfac(lay)

!  Set up factors needed to separately include the minor gases
!  in the calculation of absorption coefficient
         scaleminor(lay) = pavel(lay)/tavel(lay)         
         scaleminorn2(lay) = (pavel(lay)/tavel(lay)) &
             * (wbroad(lay)/(coldry(lay)+wkl(1,lay)))
         factor = (tavel(lay)-180.8_rb)/7.2_rb
         indminor(lay) = min(18, max(1, int(factor)))
         minorfrac(lay) = factor - float(indminor(lay))

!  Setup reference ratio to be used in calculation of binary
!  species parameter in upper atmosphere.
         rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
         rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)         

         rat_o3co2(lay)=chi_mls(3,jp(lay))/chi_mls(2,jp(lay))
         rat_o3co2_1(lay)=chi_mls(3,jp(lay)+1)/chi_mls(2,jp(lay)+1)         

!  Calculate needed column amounts.
         colh2o(lay) = 1.e-20_rb * wkl(1,lay)
         colco2(lay) = 1.e-20_rb * wkl(2,lay)
         colo3(lay) = 1.e-20_rb * wkl(3,lay)
         coln2o(lay) = 1.e-20_rb * wkl(4,lay)
         colco(lay) = 1.e-20_rb * wkl(5,lay)
         colch4(lay) = 1.e-20_rb * wkl(6,lay)
         colo2(lay) = 1.e-20_rb * wkl(7,lay)
         if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
         if (colo3(lay) .eq. 0._rb) colo3(lay) = 1.e-32_rb * coldry(lay)
         if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
         if (colco(lay)  .eq. 0._rb) colco(lay) = 1.e-32_rb * coldry(lay)
         if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
         colbrd(lay) = 1.e-20_rb * wbroad(lay)
 5400    continue

!  We have now isolated the layer ln pressure and temperature,
!  between two reference pressures and two reference temperatures 
!  (for each reference pressure).  We multiply the pressure 
!  fraction FP with the appropriate temperature fractions to get 
!  the factors that will be needed for the interpolation that yields
!  the optical depths (performed in routines TAUGBn for band n).`

         compfp = 1. - fp
         fac10(lay) = compfp * ft
         fac00(lay) = compfp * (1._rb - ft)
         fac11(lay) = fp * ft1
         fac01(lay) = fp * (1._rb - ft1)

!  Rescale selffac and forfac for use in taumol
         selffac(lay) = colh2o(lay)*selffac(lay)
         forfac(lay) = colh2o(lay)*forfac(lay)

! End layer loop
      enddo

      end subroutine setcoef

!***************************************************************************
!***************************************************************************

 
! These pressures are chosen such that the ln of the first pressure
! has only a few non-zero digits (i.e. ln(PREF(1)) = 6.96000) and
! each subsequent ln(pressure) differs from the previous one by 0.2.



! These are the temperatures associated with the respective 
! pressures for the mls standard atmosphere. 




!***************************************************************************
!***************************************************************************

 


      end module rrtmg_lw_setcoef

!     path:      $Source: /storm/rc1/cvsroot/rc/rrtmg_lw/src/rrtmg_lw_taumol.f90,v $
!     author:    $Author: mike $
!     revision:  $Revision: 1.7 $
!     created:   $Date: 2009/10/20 15:08:37 $
!
      module rrtmg_lw_taumol

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! ------- Modules -------

          USE parkind, ONLY: im => kind_im, rb => kind_rb
          USE rrlw_con, ONLY: oneminus
          USE rrlw_wvn, ONLY: nspa, nspb
          USE rrlw_vsn, ONLY: hvrtau

          IMPLICIT NONE

      contains

!----------------------------------------------------------------------------
      subroutine taumol(nlayers, pavel, wx, coldry, &
                        laytrop, jp, jt, jt1, planklay, planklev, plankbnd, &
                        colh2o, colco2, colo3, coln2o, colco, colch4, colo2, &
                        colbrd, fac00, fac01, fac10, fac11, &
                        rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1, &
                        rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1, &
                        rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1, &
                        selffac, selffrac, indself, forfac, forfrac, indfor, &
                        minorfrac, scaleminor, scaleminorn2, indminor, &
                        fracs, taug)
!----------------------------------------------------------------------------

! *******************************************************************************
! *                                                                             *
! *                  Optical depths developed for the                           *
! *                                                                             *
! *                RAPID RADIATIVE TRANSFER MODEL (RRTM)                        *
! *                                                                             *
! *                                                                             *
! *            ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                     *
! *                        131 HARTWELL AVENUE                                  *
! *                        LEXINGTON, MA 02421                                  *
! *                                                                             *
! *                                                                             *
! *                           ELI J. MLAWER                                     * 
! *                         JENNIFER DELAMERE                                   * 
! *                         STEVEN J. TAUBMAN                                   *
! *                         SHEPARD A. CLOUGH                                   *
! *                                                                             *
! *                                                                             *
! *                                                                             *
! *                                                                             *
! *                       email:  mlawer@aer.com                                *
! *                       email:  jdelamer@aer.com                              *
! *                                                                             *
! *        The authors wish to acknowledge the contributions of the             *
! *        following people:  Karen Cady-Pereira, Patrick D. Brown,             *  
! *        Michael J. Iacono, Ronald E. Farren, Luke Chen, Robert Bergstrom.    *
! *                                                                             *
! *******************************************************************************
! *                                                                             *
! *  Revision for g-point reduction: Michael J. Iacono, AER, Inc.               *
! *                                                                             *
! *******************************************************************************
! *     TAUMOL                                                                  *
! *                                                                             *
! *     This file contains the subroutines TAUGBn (where n goes from            *
! *     1 to 16).  TAUGBn calculates the optical depths and Planck fractions    *
! *     per g-value and layer for band n.                                       *
! *                                                                             *
! *  Output:  optical depths (unitless)                                         *
! *           fractions needed to compute Planck functions at every layer       *
! *               and g-value                                                   *
! *                                                                             *
! *     COMMON /TAUGCOM/  TAUG(MXLAY,MG)                                        *
! *     COMMON /PLANKG/   FRACS(MXLAY,MG)                                       *
! *                                                                             *
! *  Input                                                                      *
! *                                                                             *
! *     COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                  *
! *     COMMON /PRECISE/  ONEMINUS                                              *
! *     COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                    *
! *     &                 PZ(0:MXLAY),TZ(0:MXLAY)                               *
! *     COMMON /PROFDATA/ LAYTROP,                                              *
! *    &                  COLH2O(MXLAY),COLCO2(MXLAY),COLO3(MXLAY),             *
! *    &                  COLN2O(MXLAY),COLCO(MXLAY),COLCH4(MXLAY),             *
! *    &                  COLO2(MXLAY)
! *     COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                            *
! *    &                  FAC10(MXLAY),FAC11(MXLAY)                             *
! *     COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                        *
! *     COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)       *
! *                                                                             *
! *     Description:                                                            *
! *     NG(IBAND) - number of g-values in band IBAND                            *
! *     NSPA(IBAND) - for the lower atmosphere, the number of reference         *
! *                   atmospheres that are stored for band IBAND per            *
! *                   pressure level and temperature.  Each of these            *
! *                   atmospheres has different relative amounts of the         *
! *                   key species for the band (i.e. different binary           *
! *                   species parameters).                                      *
! *     NSPB(IBAND) - same for upper atmosphere                                 *
! *     ONEMINUS - since problems are caused in some cases by interpolation     *
! *                parameters equal to or greater than 1, for these cases       *
! *                these parameters are set to this value, slightly < 1.        *
! *     PAVEL - layer pressures (mb)                                            *
! *     TAVEL - layer temperatures (degrees K)                                  *
! *     PZ - level pressures (mb)                                               *
! *     TZ - level temperatures (degrees K)                                     *
! *     LAYTROP - layer at which switch is made from one combination of         *
! *               key species to another                                        *
! *     COLH2O, COLCO2, COLO3, COLN2O, COLCH4 - column amounts of water         *
! *               vapor,carbon dioxide, ozone, nitrous ozide, methane,          *
! *               respectively (molecules/cm**2)                                *
! *     FACij(LAY) - for layer LAY, these are factors that are needed to        *
! *                  compute the interpolation factors that multiply the        *
! *                  appropriate reference k-values.  A value of 0 (1) for      *
! *                  i,j indicates that the corresponding factor multiplies     *
! *                  reference k-value for the lower (higher) of the two        *
! *                  appropriate temperatures, and altitudes, respectively.     *
! *     JP - the index of the lower (in altitude) of the two appropriate        *
! *          reference pressure levels needed for interpolation                 *
! *     JT, JT1 - the indices of the lower of the two appropriate reference     *
! *               temperatures needed for interpolation (for pressure           *
! *               levels JP and JP+1, respectively)                             *
! *     SELFFAC - scale factor needed for water vapor self-continuum, equals    *
! *               (water vapor density)/(atmospheric density at 296K and        *
! *               1013 mb)                                                      *
! *     SELFFRAC - factor needed for temperature interpolation of reference     *
! *                water vapor self-continuum data                              *
! *     INDSELF - index of the lower of the two appropriate reference           *
! *               temperatures needed for the self-continuum interpolation      *
! *     FORFAC  - scale factor needed for water vapor foreign-continuum.        *
! *     FORFRAC - factor needed for temperature interpolation of reference      *
! *                water vapor foreign-continuum data                           *
! *     INDFOR  - index of the lower of the two appropriate reference           *
! *               temperatures needed for the foreign-continuum interpolation   *
! *                                                                             *
! *  Data input                                                                 *
! *     COMMON /Kn/ KA(NSPA(n),5,13,MG), KB(NSPB(n),5,13:59,MG), SELFREF(10,MG),*
! *                 FORREF(4,MG), KA_M'MGAS', KB_M'MGAS'                        *
! *        (note:  n is the band number,'MGAS' is the species name of the minor *
! *         gas)                                                                *
! *                                                                             *
! *     Description:                                                            *
! *     KA - k-values for low reference atmospheres (key-species only)          *
! *          (units: cm**2/molecule)                                            *
! *     KB - k-values for high reference atmospheres (key-species only)         *
! *          (units: cm**2/molecule)                                            *
! *     KA_M'MGAS' - k-values for low reference atmosphere minor species        *
! *          (units: cm**2/molecule)                                            *
! *     KB_M'MGAS' - k-values for high reference atmosphere minor species       *
! *          (units: cm**2/molecule)                                            *
! *     SELFREF - k-values for water vapor self-continuum for reference         *
! *               atmospheres (used below LAYTROP)                              *
! *               (units: cm**2/molecule)                                       *
! *     FORREF  - k-values for water vapor foreign-continuum for reference      *
! *               atmospheres (used below/above LAYTROP)                        *
! *               (units: cm**2/molecule)                                       *
! *                                                                             *
! *     DIMENSION ABSA(65*NSPA(n),MG), ABSB(235*NSPB(n),MG)                     *
! *     EQUIVALENCE (KA,ABSA),(KB,ABSB)                                         *
! *                                                                             *
!*******************************************************************************

! ------- Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: nlayers         ! total number of layers
      real(kind=rb), intent(in) :: pavel(:)           ! layer pressures (mb) 
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: wx(:,:)            ! cross-section amounts (mol/cm2)
                                                      !    Dimensions: (maxxsec,nlayers)
      real(kind=rb), intent(in) :: coldry(:)          ! column amount (dry air)
                                                      !    Dimensions: (nlayers)

      integer(kind=im), intent(in) :: laytrop         ! tropopause layer index
      integer(kind=im), intent(in) :: jp(:)           ! 
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(in) :: jt(:)           !
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(in) :: jt1(:)          !
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: planklay(:,:)      ! 
                                                      !    Dimensions: (nlayers,nbndlw)
      real(kind=rb), intent(in) :: planklev(0:,:)     ! 
                                                      !    Dimensions: (nlayers,nbndlw)
      real(kind=rb), intent(in) :: plankbnd(:)        ! 
                                                      !    Dimensions: (nbndlw)

      real(kind=rb), intent(in) :: colh2o(:)          ! column amount (h2o)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colco2(:)          ! column amount (co2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colo3(:)           ! column amount (o3)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: coln2o(:)          ! column amount (n2o)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colco(:)           ! column amount (co)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colch4(:)          ! column amount (ch4)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colo2(:)           ! column amount (o2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colbrd(:)          ! column amount (broadening gases)
                                                      !    Dimensions: (nlayers)

      integer(kind=im), intent(in) :: indself(:)
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(in) :: indfor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: selffac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: selffrac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: forfac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: forfrac(:)
                                                      !    Dimensions: (nlayers)

      integer(kind=im), intent(in) :: indminor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: minorfrac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: scaleminor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: scaleminorn2(:)
                                                      !    Dimensions: (nlayers)

      real(kind=rb), intent(in) :: &                  !
                       fac00(:), fac01(:), &          !    Dimensions: (nlayers)
                       fac10(:), fac11(:) 
      real(kind=rb), intent(in) :: &                  !
                       rat_h2oco2(:),rat_h2oco2_1(:), &
                       rat_h2oo3(:),rat_h2oo3_1(:), & !    Dimensions: (nlayers)
                       rat_h2on2o(:),rat_h2on2o_1(:), &
                       rat_h2och4(:),rat_h2och4_1(:), &
                       rat_n2oco2(:),rat_n2oco2_1(:), &
                       rat_o3co2(:),rat_o3co2_1(:)

! ----- Output -----
      real(kind=rb), intent(out) :: fracs(:,:)        ! planck fractions
                                                      !    Dimensions: (nlayers,ngptlw)
      real(kind=rb), intent(out) :: taug(:,:)         ! gaseous optical depth 
                                                      !    Dimensions: (nlayers,ngptlw)

      hvrtau = '$Revision: 1.7 $'

! Calculate gaseous optical depth and planck fractions for each spectral band.

      call taugb1
      call taugb2
      call taugb3
      call taugb4
      call taugb5
      call taugb6
      call taugb7
      call taugb8
      call taugb9
      call taugb10
      call taugb11
      call taugb12
      call taugb13
      call taugb14
      call taugb15
      call taugb16

      contains

!----------------------------------------------------------------------------
      subroutine taugb1
!----------------------------------------------------------------------------

! ------- Modifications -------
!  Written by Eli J. Mlawer, Atmospheric & Environmental Research.
!  Revised by Michael J. Iacono, Atmospheric & Environmental Research.
!
!     band 1:  10-350 cm-1 (low key - h2o; low minor - n2)
!                          (high key - h2o; high minor - n2)
!
!     note: previous versions of rrtm band 1: 
!           10-250 cm-1 (low - h2o; high - h2o)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng1
          USE rrlw_kg01, ONLY: fracrefa, fracrefb, absa, absb, ka_mn2, kb_mn2, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      real(kind=rb) :: pp, corradj, scalen2, tauself, taufor, taun2


! Minor gas mapping levels:
!     lower - n2, p = 142.5490 mbar, t = 215.70 k
!     upper - n2, p = 142.5490 mbar, t = 215.70 k

! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum and
! foreign continuum is interpolated (in temperature) separately.

! Lower atmosphere loop
      do lay = 1, laytrop

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(1) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(1) + 1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)
         pp = pavel(lay)
         corradj =  1.
         if (pp .lt. 250._rb) then
            corradj = 1._rb - 0.15_rb * (250._rb-pp) / 154.4_rb
         endif

         scalen2 = colbrd(lay) * scaleminorn2(lay)
         do ig = 1, ng1
            tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) -  forref(indf,ig))) 
            taun2 = scalen2*(ka_mn2(indm,ig) + & 
                 minorfrac(lay) * (ka_mn2(indm+1,ig) - ka_mn2(indm,ig)))
            taug(lay,ig) = corradj * (colh2o(lay) * &
                (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig)) & 
                 + tauself + taufor + taun2)
             fracs(lay,ig) = fracrefa(ig)
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(1) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(1) + 1
         indf = indfor(lay)
         indm = indminor(lay)
         pp = pavel(lay)
         corradj =  1._rb - 0.15_rb * (pp / 95.6_rb)

         scalen2 = colbrd(lay) * scaleminorn2(lay)
         do ig = 1, ng1
            taufor = forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * (forref(indf+1,ig) - forref(indf,ig))) 
            taun2 = scalen2*(kb_mn2(indm,ig) + & 
                 minorfrac(lay) * (kb_mn2(indm+1,ig) - kb_mn2(indm,ig)))
            taug(lay,ig) = corradj * (colh2o(lay) * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) &  
                 + taufor + taun2)
            fracs(lay,ig) = fracrefb(ig)
         enddo
      enddo

      end subroutine taugb1

!----------------------------------------------------------------------------
      subroutine taugb2
!----------------------------------------------------------------------------
!
!     band 2:  350-500 cm-1 (low key - h2o; high key - h2o)
!
!     note: previous version of rrtm band 2: 
!           250 - 500 cm-1 (low - h2o; high - h2o)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng2, ngs1
          USE rrlw_kg02, ONLY: fracrefa, fracrefb, absa, absb, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
      real(kind=rb) :: pp, corradj, tauself, taufor


! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum and
! foreign continuum is interpolated (in temperature) separately.

! Lower atmosphere loop
      do lay = 1, laytrop

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(2) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(2) + 1
         inds = indself(lay)
         indf = indfor(lay)
         pp = pavel(lay)
         corradj = 1._rb - .05_rb * (pp - 100._rb) / 900._rb
         do ig = 1, ng2
            tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            taug(lay,ngs1+ig) = corradj * (colh2o(lay) * &
                (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig)) &
                 + tauself + taufor)
            fracs(lay,ngs1+ig) = fracrefa(ig)
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(2) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(2) + 1
         indf = indfor(lay)
         do ig = 1, ng2
            taufor =  forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * (forref(indf+1,ig) - forref(indf,ig))) 
            taug(lay,ngs1+ig) = colh2o(lay) * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) &
                 + taufor
            fracs(lay,ngs1+ig) = fracrefb(ig)
         enddo
      enddo

      end subroutine taugb2

!----------------------------------------------------------------------------
      subroutine taugb3
!----------------------------------------------------------------------------
!
!     band 3:  500-630 cm-1 (low key - h2o,co2; low minor - n2o)
!                           (high key - h2o,co2; high minor - n2o)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng3, ngs2
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg03, ONLY: fracrefa, fracrefb, absa, absb, ka_mn2o, kb_mn2o, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      integer(kind=im) :: js, js1, jmn2o, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_mn2o, specparm_mn2o, specmult_mn2o, &
                       fmn2o, fmn2omf, chi_n2o, ratn2o, adjfac, adjcoln2o
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: tauself, taufor, n2om1, n2om2, absn2o
      real(kind=rb) :: refrat_planck_a, refrat_planck_b, refrat_m_a, refrat_m_b
      real(kind=rb) :: tau_major, tau_major1


! Minor gas mapping levels:
!     lower - n2o, p = 706.272 mbar, t = 278.94 k
!     upper - n2o, p = 95.58 mbar, t = 215.7 k

!  P = 212.725 mb
      refrat_planck_a = chi_mls(1,9)/chi_mls(2,9)

!  P = 95.58 mb
      refrat_planck_b = chi_mls(1,13)/chi_mls(2,13)

!  P = 706.270mb
      refrat_m_a = chi_mls(1,3)/chi_mls(2,3)

!  P = 95.58 mb 
      refrat_m_b = chi_mls(1,13)/chi_mls(2,13)

! Compute the optical depth by interpolating in ln(pressure) and 
! temperature, and appropriate species.  Below laytrop, the water vapor 
! self-continuum and foreign continuum is interpolated (in temperature) 
! separately.

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)        

         speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_mn2o = colh2o(lay) + refrat_m_a*colco2(lay)
         specparm_mn2o = colh2o(lay)/speccomb_mn2o
         if (specparm_mn2o .ge. oneminus) specparm_mn2o = oneminus
         specmult_mn2o = 8._rb*specparm_mn2o
         jmn2o = 1 + int(specmult_mn2o)
         fmn2o = mod(specmult_mn2o,1.0_rb)
         fmn2omf = minorfrac(lay)*fmn2o
!  In atmospheres where the amount of N2O is too great to be considered
!  a minor species, adjust the column amount of N2O by an empirical factor 
!  to obtain the proper contribution.
         chi_n2o = coln2o(lay)/coldry(lay)
         ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
         if (ratn2o .gt. 1.5_rb) then
            adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
            adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
         else
            adjcoln2o = coln2o(lay)
         endif

         speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(3) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(3) + js1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif
         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng3
            tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            n2om1 = ka_mn2o(jmn2o,indm,ig) + fmn2o * &
                 (ka_mn2o(jmn2o+1,indm,ig) - ka_mn2o(jmn2o,indm,ig))
            n2om2 = ka_mn2o(jmn2o,indm+1,ig) + fmn2o * &
                 (ka_mn2o(jmn2o+1,indm+1,ig) - ka_mn2o(jmn2o,indm+1,ig))
            absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) +  &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs2+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + adjcoln2o*absn2o
            fracs(lay,ngs2+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

         speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 4._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 4._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         fac000 = (1._rb - fs) * fac00(lay)
         fac010 = (1._rb - fs) * fac10(lay)
         fac100 = fs * fac00(lay)
         fac110 = fs * fac10(lay)
         fac001 = (1._rb - fs1) * fac01(lay)
         fac011 = (1._rb - fs1) * fac11(lay)
         fac101 = fs1 * fac01(lay)
         fac111 = fs1 * fac11(lay)

         speccomb_mn2o = colh2o(lay) + refrat_m_b*colco2(lay)
         specparm_mn2o = colh2o(lay)/speccomb_mn2o
         if (specparm_mn2o .ge. oneminus) specparm_mn2o = oneminus
         specmult_mn2o = 4._rb*specparm_mn2o
         jmn2o = 1 + int(specmult_mn2o)
         fmn2o = mod(specmult_mn2o,1.0_rb)
         fmn2omf = minorfrac(lay)*fmn2o
!  In atmospheres where the amount of N2O is too great to be considered
!  a minor species, adjust the column amount of N2O by an empirical factor 
!  to obtain the proper contribution.
         chi_n2o = coln2o(lay)/coldry(lay)
         ratn2o = 1.e20*chi_n2o/chi_mls(4,jp(lay)+1)
         if (ratn2o .gt. 1.5_rb) then
            adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
            adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
         else
            adjcoln2o = coln2o(lay)
         endif

         speccomb_planck = colh2o(lay)+refrat_planck_b*colco2(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 4._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(3) + js
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(3) + js1
         indf = indfor(lay)
         indm = indminor(lay)

         do ig = 1, ng3
            taufor = forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * (forref(indf+1,ig) - forref(indf,ig))) 
            n2om1 = kb_mn2o(jmn2o,indm,ig) + fmn2o * &
                 (kb_mn2o(jmn2o+1,indm,ig)-kb_mn2o(jmn2o,indm,ig))
            n2om2 = kb_mn2o(jmn2o,indm+1,ig) + fmn2o * &
                 (kb_mn2o(jmn2o+1,indm+1,ig)-kb_mn2o(jmn2o,indm+1,ig))
            absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)
            taug(lay,ngs2+ig) = speccomb * &
                (fac000 * absb(ind0,ig) + &
                fac100 * absb(ind0+1,ig) + &
                fac010 * absb(ind0+5,ig) + &
                fac110 * absb(ind0+6,ig)) &
                + speccomb1 * &
                (fac001 * absb(ind1,ig) +  &
                fac101 * absb(ind1+1,ig) + &
                fac011 * absb(ind1+5,ig) + &
                fac111 * absb(ind1+6,ig))  &
                + taufor &
                + adjcoln2o*absn2o
            fracs(lay,ngs2+ig) = fracrefb(ig,jpl) + fpl * &
                (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
         enddo
      enddo

      end subroutine taugb3

!----------------------------------------------------------------------------
      subroutine taugb4
!----------------------------------------------------------------------------
!
!     band 4:  630-700 cm-1 (low key - h2o,co2; high key - o3,co2)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng4, ngs3
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg04, ONLY: fracrefa, fracrefb, absa, absb, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
      integer(kind=im) :: js, js1, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: tauself, taufor
      real(kind=rb) :: refrat_planck_a, refrat_planck_b
      real(kind=rb) :: tau_major, tau_major1


! P =   142.5940 mb
      refrat_planck_a = chi_mls(1,11)/chi_mls(2,11)

! P = 95.58350 mb
      refrat_planck_b = chi_mls(3,13)/chi_mls(2,13)

! Compute the optical depth by interpolating in ln(pressure) and 
! temperature, and appropriate species.  Below laytrop, the water 
! vapor self-continuum and foreign continuum is interpolated (in temperature) 
! separately.

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(4) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(4) + js1
         inds = indself(lay)
         indf = indfor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif

         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng4
            tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) +  &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs3+ig) = tau_major + tau_major1 &
                 + tauself + taufor
            fracs(lay,ngs3+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

         speccomb = colo3(lay) + rat_o3co2(lay)*colco2(lay)
         specparm = colo3(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 4._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colo3(lay) + rat_o3co2_1(lay)*colco2(lay)
         specparm1 = colo3(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 4._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         fac000 = (1._rb - fs) * fac00(lay)
         fac010 = (1._rb - fs) * fac10(lay)
         fac100 = fs * fac00(lay)
         fac110 = fs * fac10(lay)
         fac001 = (1._rb - fs1) * fac01(lay)
         fac011 = (1._rb - fs1) * fac11(lay)
         fac101 = fs1 * fac01(lay)
         fac111 = fs1 * fac11(lay)

         speccomb_planck = colo3(lay)+refrat_planck_b*colco2(lay)
         specparm_planck = colo3(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 4._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(4) + js
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(4) + js1

         do ig = 1, ng4
            taug(lay,ngs3+ig) =  speccomb * &
                (fac000 * absb(ind0,ig) + &
                fac100 * absb(ind0+1,ig) + &
                fac010 * absb(ind0+5,ig) + &
                fac110 * absb(ind0+6,ig)) &
                + speccomb1 * &
                (fac001 * absb(ind1,ig) +  &
                fac101 * absb(ind1+1,ig) + &
                fac011 * absb(ind1+5,ig) + &
                fac111 * absb(ind1+6,ig))
            fracs(lay,ngs3+ig) = fracrefb(ig,jpl) + fpl * &
                (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
         enddo

! Empirical modification to code to improve stratospheric cooling rates
! for co2.  Revised to apply weighting for g-point reduction in this band.

         taug(lay,ngs3+8)=taug(lay,ngs3+8)*0.92
         taug(lay,ngs3+9)=taug(lay,ngs3+9)*0.88
         taug(lay,ngs3+10)=taug(lay,ngs3+10)*1.07
         taug(lay,ngs3+11)=taug(lay,ngs3+11)*1.1
         taug(lay,ngs3+12)=taug(lay,ngs3+12)*0.99
         taug(lay,ngs3+13)=taug(lay,ngs3+13)*0.88
         taug(lay,ngs3+14)=taug(lay,ngs3+14)*0.943

      enddo

      end subroutine taugb4

!----------------------------------------------------------------------------
      subroutine taugb5
!----------------------------------------------------------------------------
!
!     band 5:  700-820 cm-1 (low key - h2o,co2; low minor - o3, ccl4)
!                           (high key - o3,co2)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng5, ngs4
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg05, ONLY: fracrefa, fracrefb, absa, absb, ka_mo3, selfref, forref, ccl4

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      integer(kind=im) :: js, js1, jmo3, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_mo3, specparm_mo3, specmult_mo3, fmo3
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: tauself, taufor, o3m1, o3m2, abso3
      real(kind=rb) :: refrat_planck_a, refrat_planck_b, refrat_m_a
      real(kind=rb) :: tau_major, tau_major1


! Minor gas mapping level :
!     lower - o3, p = 317.34 mbar, t = 240.77 k
!     lower - ccl4

! Calculate reference ratio to be used in calculation of Planck
! fraction in lower/upper atmosphere.

! P = 473.420 mb
      refrat_planck_a = chi_mls(1,5)/chi_mls(2,5)

! P = 0.2369 mb
      refrat_planck_b = chi_mls(3,43)/chi_mls(2,43)

! P = 317.3480
      refrat_m_a = chi_mls(1,7)/chi_mls(2,7)

! Compute the optical depth by interpolating in ln(pressure) and 
! temperature, and appropriate species.  Below laytrop, the 
! water vapor self-continuum and foreign continuum is 
! interpolated (in temperature) separately.

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_mo3 = colh2o(lay) + refrat_m_a*colco2(lay)
         specparm_mo3 = colh2o(lay)/speccomb_mo3
         if (specparm_mo3 .ge. oneminus) specparm_mo3 = oneminus
         specmult_mo3 = 8._rb*specparm_mo3
         jmo3 = 1 + int(specmult_mo3)
         fmo3 = mod(specmult_mo3,1.0_rb)

         speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(5) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(5) + js1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif

         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng5
            tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            o3m1 = ka_mo3(jmo3,indm,ig) + fmo3 * &
                 (ka_mo3(jmo3+1,indm,ig)-ka_mo3(jmo3,indm,ig))
            o3m2 = ka_mo3(jmo3,indm+1,ig) + fmo3 * &
                 (ka_mo3(jmo3+1,indm+1,ig)-ka_mo3(jmo3,indm+1,ig))
            abso3 = o3m1 + minorfrac(lay)*(o3m2-o3m1)

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * & 
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs4+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + abso3*colo3(lay) &
                 + wx(1,lay) * ccl4(ig)
            fracs(lay,ngs4+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

         speccomb = colo3(lay) + rat_o3co2(lay)*colco2(lay)
         specparm = colo3(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 4._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colo3(lay) + rat_o3co2_1(lay)*colco2(lay)
         specparm1 = colo3(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 4._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         fac000 = (1._rb - fs) * fac00(lay)
         fac010 = (1._rb - fs) * fac10(lay)
         fac100 = fs * fac00(lay)
         fac110 = fs * fac10(lay)
         fac001 = (1._rb - fs1) * fac01(lay)
         fac011 = (1._rb - fs1) * fac11(lay)
         fac101 = fs1 * fac01(lay)
         fac111 = fs1 * fac11(lay)

         speccomb_planck = colo3(lay)+refrat_planck_b*colco2(lay)
         specparm_planck = colo3(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 4._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(5) + js
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(5) + js1
         
         do ig = 1, ng5
            taug(lay,ngs4+ig) = speccomb * &
                (fac000 * absb(ind0,ig) + &
                fac100 * absb(ind0+1,ig) + &
                fac010 * absb(ind0+5,ig) + &
                fac110 * absb(ind0+6,ig)) &
                + speccomb1 * &
                (fac001 * absb(ind1,ig) + &
                fac101 * absb(ind1+1,ig) + &
                fac011 * absb(ind1+5,ig) + &
                fac111 * absb(ind1+6,ig))  &
                + wx(1,lay) * ccl4(ig)
            fracs(lay,ngs4+ig) = fracrefb(ig,jpl) + fpl * &
                (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
         enddo
      enddo

      end subroutine taugb5

!----------------------------------------------------------------------------
      subroutine taugb6
!----------------------------------------------------------------------------
!
!     band 6:  820-980 cm-1 (low key - h2o; low minor - co2)
!                           (high key - nothing; high minor - cfc11, cfc12)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng6, ngs5
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg06, ONLY: fracrefa, absa, ka_mco2, selfref, forref, cfc11adj, cfc12

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      real(kind=rb) :: chi_co2, ratco2, adjfac, adjcolco2
      real(kind=rb) :: tauself, taufor, absco2


! Minor gas mapping level:
!     lower - co2, p = 706.2720 mb, t = 294.2 k
!     upper - cfc11, cfc12

! Compute the optical depth by interpolating in ln(pressure) and
! temperature. The water vapor self-continuum and foreign continuum
! is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop

! In atmospheres where the amount of CO2 is too great to be considered
! a minor species, adjust the column amount of CO2 by an empirical factor 
! to obtain the proper contribution.
         chi_co2 = colco2(lay)/(coldry(lay))
         ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
         if (ratco2 .gt. 3.0_rb) then
            adjfac = 2.0_rb+(ratco2-2.0_rb)**0.77_rb
            adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
         else
            adjcolco2 = colco2(lay)
         endif

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(6) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(6) + 1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)

         do ig = 1, ng6
            tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            absco2 =  (ka_mco2(indm,ig) + minorfrac(lay) * &
                 (ka_mco2(indm+1,ig) - ka_mco2(indm,ig)))
            taug(lay,ngs5+ig) = colh2o(lay) * &
                (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) +  &
                 fac11(lay) * absa(ind1+1,ig))  &
                 + tauself + taufor &
                 + adjcolco2 * absco2 &
                 + wx(2,lay) * cfc11adj(ig) &
                 + wx(3,lay) * cfc12(ig)
            fracs(lay,ngs5+ig) = fracrefa(ig)
         enddo
      enddo

! Upper atmosphere loop
! Nothing important goes on above laytrop in this band.
      do lay = laytrop+1, nlayers

         do ig = 1, ng6
            taug(lay,ngs5+ig) = 0.0_rb &
                 + wx(2,lay) * cfc11adj(ig) &
                 + wx(3,lay) * cfc12(ig)
            fracs(lay,ngs5+ig) = fracrefa(ig)
         enddo
      enddo

      end subroutine taugb6

!----------------------------------------------------------------------------
      subroutine taugb7
!----------------------------------------------------------------------------
!
!     band 7:  980-1080 cm-1 (low key - h2o,o3; low minor - co2)
!                            (high key - o3; high minor - co2)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng7, ngs6
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg07, ONLY: fracrefa, fracrefb, absa, absb, ka_mco2, kb_mco2, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      integer(kind=im) :: js, js1, jmco2, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_mco2, specparm_mco2, specmult_mco2, fmco2
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: tauself, taufor, co2m1, co2m2, absco2
      real(kind=rb) :: chi_co2, ratco2, adjfac, adjcolco2
      real(kind=rb) :: refrat_planck_a, refrat_m_a
      real(kind=rb) :: tau_major, tau_major1


! Minor gas mapping level :
!     lower - co2, p = 706.2620 mbar, t= 278.94 k
!     upper - co2, p = 12.9350 mbar, t = 234.01 k

! Calculate reference ratio to be used in calculation of Planck
! fraction in lower atmosphere.

! P = 706.2620 mb
      refrat_planck_a = chi_mls(1,3)/chi_mls(3,3)

! P = 706.2720 mb
      refrat_m_a = chi_mls(1,3)/chi_mls(3,3)

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately. 

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = colh2o(lay) + rat_h2oo3(lay)*colo3(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colh2o(lay) + rat_h2oo3_1(lay)*colo3(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_mco2 = colh2o(lay) + refrat_m_a*colo3(lay)
         specparm_mco2 = colh2o(lay)/speccomb_mco2
         if (specparm_mco2 .ge. oneminus) specparm_mco2 = oneminus
         specmult_mco2 = 8._rb*specparm_mco2

         jmco2 = 1 + int(specmult_mco2)
         fmco2 = mod(specmult_mco2,1.0_rb)

!  In atmospheres where the amount of CO2 is too great to be considered
!  a minor species, adjust the column amount of CO2 by an empirical factor 
!  to obtain the proper contribution.
         chi_co2 = colco2(lay)/(coldry(lay))
         ratco2 = 1.e20*chi_co2/chi_mls(2,jp(lay)+1)
         if (ratco2 .gt. 3.0_rb) then
            adjfac = 3.0_rb+(ratco2-3.0_rb)**0.79_rb
            adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
         else
            adjcolco2 = colco2(lay)
         endif

         speccomb_planck = colh2o(lay)+refrat_planck_a*colo3(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(7) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(7) + js1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif
         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng7
            tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            co2m1 = ka_mco2(jmco2,indm,ig) + fmco2 * &
                 (ka_mco2(jmco2+1,indm,ig) - ka_mco2(jmco2,indm,ig))
            co2m2 = ka_mco2(jmco2,indm+1,ig) + fmco2 * &
                 (ka_mco2(jmco2+1,indm+1,ig) - ka_mco2(jmco2,indm+1,ig))
            absco2 = co2m1 + minorfrac(lay) * (co2m2 - co2m1)

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) +  &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs6+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + adjcolco2*absco2
            fracs(lay,ngs6+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

!  In atmospheres where the amount of CO2 is too great to be considered
!  a minor species, adjust the column amount of CO2 by an empirical factor 
!  to obtain the proper contribution.
         chi_co2 = colco2(lay)/(coldry(lay))
         ratco2 = 1.e20*chi_co2/chi_mls(2,jp(lay)+1)
         if (ratco2 .gt. 3.0_rb) then
            adjfac = 2.0_rb+(ratco2-2.0_rb)**0.79_rb
            adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
         else
            adjcolco2 = colco2(lay)
         endif

         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(7) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(7) + 1
         indm = indminor(lay)

         do ig = 1, ng7
            absco2 = kb_mco2(indm,ig) + minorfrac(lay) * &
                 (kb_mco2(indm+1,ig) - kb_mco2(indm,ig))
            taug(lay,ngs6+ig) = colo3(lay) * &
                 (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) &
                 + adjcolco2 * absco2
            fracs(lay,ngs6+ig) = fracrefb(ig)
         enddo

! Empirical modification to code to improve stratospheric cooling rates
! for o3.  Revised to apply weighting for g-point reduction in this band.

         taug(lay,ngs6+6)=taug(lay,ngs6+6)*0.92_rb
         taug(lay,ngs6+7)=taug(lay,ngs6+7)*0.88_rb
         taug(lay,ngs6+8)=taug(lay,ngs6+8)*1.07_rb
         taug(lay,ngs6+9)=taug(lay,ngs6+9)*1.1_rb
         taug(lay,ngs6+10)=taug(lay,ngs6+10)*0.99_rb
         taug(lay,ngs6+11)=taug(lay,ngs6+11)*0.855_rb

      enddo

      end subroutine taugb7

!----------------------------------------------------------------------------
      subroutine taugb8
!----------------------------------------------------------------------------
!
!     band 8:  1080-1180 cm-1 (low key - h2o; low minor - co2,o3,n2o)
!                             (high key - o3; high minor - co2, n2o)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng8, ngs7
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg08, ONLY: fracrefa, fracrefb, absa, absb, ka_mco2, ka_mn2o, ka_mo3, kb_mco2, kb_mn2o, selfref, forref, cfc12, cfc22adj

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      real(kind=rb) :: tauself, taufor, absco2, abso3, absn2o
      real(kind=rb) :: chi_co2, ratco2, adjfac, adjcolco2


! Minor gas mapping level:
!     lower - co2, p = 1053.63 mb, t = 294.2 k
!     lower - o3,  p = 317.348 mb, t = 240.77 k
!     lower - n2o, p = 706.2720 mb, t= 278.94 k
!     lower - cfc12,cfc11
!     upper - co2, p = 35.1632 mb, t = 223.28 k
!     upper - n2o, p = 8.716e-2 mb, t = 226.03 k

! Compute the optical depth by interpolating in ln(pressure) and 
! temperature, and appropriate species.  Below laytrop, the water vapor 
! self-continuum and foreign continuum is interpolated (in temperature) 
! separately.

! Lower atmosphere loop
      do lay = 1, laytrop

!  In atmospheres where the amount of CO2 is too great to be considered
!  a minor species, adjust the column amount of CO2 by an empirical factor 
!  to obtain the proper contribution.
         chi_co2 = colco2(lay)/(coldry(lay))
         ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
         if (ratco2 .gt. 3.0_rb) then
            adjfac = 2.0_rb+(ratco2-2.0_rb)**0.65_rb
            adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
         else
            adjcolco2 = colco2(lay)
         endif

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(8) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(8) + 1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)

         do ig = 1, ng8
            tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            absco2 =  (ka_mco2(indm,ig) + minorfrac(lay) * &
                 (ka_mco2(indm+1,ig) - ka_mco2(indm,ig)))
            abso3 =  (ka_mo3(indm,ig) + minorfrac(lay) * &
                 (ka_mo3(indm+1,ig) - ka_mo3(indm,ig)))
            absn2o =  (ka_mn2o(indm,ig) + minorfrac(lay) * &
                 (ka_mn2o(indm+1,ig) - ka_mn2o(indm,ig)))
            taug(lay,ngs7+ig) = colh2o(lay) * &
                 (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) +  &
                 fac11(lay) * absa(ind1+1,ig)) &
                 + tauself + taufor &
                 + adjcolco2*absco2 &
                 + colo3(lay) * abso3 &
                 + coln2o(lay) * absn2o &
                 + wx(3,lay) * cfc12(ig) &
                 + wx(4,lay) * cfc22adj(ig)
            fracs(lay,ngs7+ig) = fracrefa(ig)
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

!  In atmospheres where the amount of CO2 is too great to be considered
!  a minor species, adjust the column amount of CO2 by an empirical factor 
!  to obtain the proper contribution.
         chi_co2 = colco2(lay)/coldry(lay)
         ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
         if (ratco2 .gt. 3.0_rb) then
            adjfac = 2.0_rb+(ratco2-2.0_rb)**0.65_rb
            adjcolco2 = adjfac*chi_mls(2,jp(lay)+1) * coldry(lay)*1.e-20_rb
         else
            adjcolco2 = colco2(lay)
         endif

         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(8) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(8) + 1
         indm = indminor(lay)

         do ig = 1, ng8
            absco2 =  (kb_mco2(indm,ig) + minorfrac(lay) * &
                 (kb_mco2(indm+1,ig) - kb_mco2(indm,ig)))
            absn2o =  (kb_mn2o(indm,ig) + minorfrac(lay) * &
                 (kb_mn2o(indm+1,ig) - kb_mn2o(indm,ig)))
            taug(lay,ngs7+ig) = colo3(lay) * &
                 (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) &
                 + adjcolco2*absco2 &
                 + coln2o(lay)*absn2o & 
                 + wx(3,lay) * cfc12(ig) &
                 + wx(4,lay) * cfc22adj(ig)
            fracs(lay,ngs7+ig) = fracrefb(ig)
         enddo
      enddo

      end subroutine taugb8

!----------------------------------------------------------------------------
      subroutine taugb9
!----------------------------------------------------------------------------
!
!     band 9:  1180-1390 cm-1 (low key - h2o,ch4; low minor - n2o)
!                             (high key - ch4; high minor - n2o)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng9, ngs8
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg09, ONLY: fracrefa, fracrefb, absa, absb, ka_mn2o, kb_mn2o, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      integer(kind=im) :: js, js1, jmn2o, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_mn2o, specparm_mn2o, specmult_mn2o, fmn2o
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: tauself, taufor, n2om1, n2om2, absn2o
      real(kind=rb) :: chi_n2o, ratn2o, adjfac, adjcoln2o
      real(kind=rb) :: refrat_planck_a, refrat_m_a
      real(kind=rb) :: tau_major, tau_major1


! Minor gas mapping level :
!     lower - n2o, p = 706.272 mbar, t = 278.94 k
!     upper - n2o, p = 95.58 mbar, t = 215.7 k

! Calculate reference ratio to be used in calculation of Planck
! fraction in lower/upper atmosphere.

! P = 212 mb
      refrat_planck_a = chi_mls(1,9)/chi_mls(6,9)

! P = 706.272 mb 
      refrat_m_a = chi_mls(1,3)/chi_mls(6,3)

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = colh2o(lay) + rat_h2och4(lay)*colch4(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colh2o(lay) + rat_h2och4_1(lay)*colch4(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_mn2o = colh2o(lay) + refrat_m_a*colch4(lay)
         specparm_mn2o = colh2o(lay)/speccomb_mn2o
         if (specparm_mn2o .ge. oneminus) specparm_mn2o = oneminus
         specmult_mn2o = 8._rb*specparm_mn2o
         jmn2o = 1 + int(specmult_mn2o)
         fmn2o = mod(specmult_mn2o,1.0_rb)

!  In atmospheres where the amount of N2O is too great to be considered
!  a minor species, adjust the column amount of N2O by an empirical factor 
!  to obtain the proper contribution.
         chi_n2o = coln2o(lay)/(coldry(lay))
         ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
         if (ratn2o .gt. 1.5_rb) then
            adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
            adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
         else
            adjcoln2o = coln2o(lay)
         endif

         speccomb_planck = colh2o(lay)+refrat_planck_a*colch4(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(9) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(9) + js1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif

         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng9
            tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            n2om1 = ka_mn2o(jmn2o,indm,ig) + fmn2o * &
                 (ka_mn2o(jmn2o+1,indm,ig) - ka_mn2o(jmn2o,indm,ig))
            n2om2 = ka_mn2o(jmn2o,indm+1,ig) + fmn2o * &
                 (ka_mn2o(jmn2o+1,indm+1,ig) - ka_mn2o(jmn2o,indm+1,ig))
            absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + & 
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs8+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + adjcoln2o*absn2o
            fracs(lay,ngs8+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

!  In atmospheres where the amount of N2O is too great to be considered
!  a minor species, adjust the column amount of N2O by an empirical factor 
!  to obtain the proper contribution.
         chi_n2o = coln2o(lay)/(coldry(lay))
         ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
         if (ratn2o .gt. 1.5_rb) then
            adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
            adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
         else
            adjcoln2o = coln2o(lay)
         endif

         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(9) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(9) + 1
         indm = indminor(lay)

         do ig = 1, ng9
            absn2o = kb_mn2o(indm,ig) + minorfrac(lay) * &
                (kb_mn2o(indm+1,ig) - kb_mn2o(indm,ig))
            taug(lay,ngs8+ig) = colch4(lay) * &
                 (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) +  &
                 fac11(lay) * absb(ind1+1,ig)) &
                 + adjcoln2o*absn2o
            fracs(lay,ngs8+ig) = fracrefb(ig)
         enddo
      enddo

      end subroutine taugb9

!----------------------------------------------------------------------------
      subroutine taugb10
!----------------------------------------------------------------------------
!
!     band 10:  1390-1480 cm-1 (low key - h2o; high key - h2o)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng10, ngs9
          USE rrlw_kg10, ONLY: fracrefa, fracrefb, absa, absb, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
      real(kind=rb) :: tauself, taufor


! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum and
! foreign continuum is interpolated (in temperature) separately.

! Lower atmosphere loop
      do lay = 1, laytrop
         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(10) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(10) + 1
         inds = indself(lay)
         indf = indfor(lay)

         do ig = 1, ng10
            tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            taug(lay,ngs9+ig) = colh2o(lay) * &
                 (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig))  &
                 + tauself + taufor
            fracs(lay,ngs9+ig) = fracrefa(ig)
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(10) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(10) + 1
         indf = indfor(lay)

         do ig = 1, ng10
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            taug(lay,ngs9+ig) = colh2o(lay) * &
                 (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) +  &
                 fac11(lay) * absb(ind1+1,ig)) &
                 + taufor
            fracs(lay,ngs9+ig) = fracrefb(ig)
         enddo
      enddo

      end subroutine taugb10

!----------------------------------------------------------------------------
      subroutine taugb11
!----------------------------------------------------------------------------
!
!     band 11:  1480-1800 cm-1 (low - h2o; low minor - o2)
!                              (high key - h2o; high minor - o2)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng11, ngs10
          USE rrlw_kg11, ONLY: fracrefa, fracrefb, absa, absb, ka_mo2, kb_mo2, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      real(kind=rb) :: scaleo2, tauself, taufor, tauo2


! Minor gas mapping level :
!     lower - o2, p = 706.2720 mbar, t = 278.94 k
!     upper - o2, p = 4.758820 mbarm t = 250.85 k

! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum and
! foreign continuum is interpolated (in temperature) separately.

! Lower atmosphere loop
      do lay = 1, laytrop
         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(11) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(11) + 1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)
         scaleo2 = colo2(lay)*scaleminor(lay)
         do ig = 1, ng11
            tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
            tauo2 =  scaleo2 * (ka_mo2(indm,ig) + minorfrac(lay) * &
                 (ka_mo2(indm+1,ig) - ka_mo2(indm,ig)))
            taug(lay,ngs10+ig) = colh2o(lay) * &
                 (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig)) &
                 + tauself + taufor &
                 + tauo2
            fracs(lay,ngs10+ig) = fracrefa(ig)
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(11) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(11) + 1
         indf = indfor(lay)
         indm = indminor(lay)
         scaleo2 = colo2(lay)*scaleminor(lay)
         do ig = 1, ng11
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            tauo2 =  scaleo2 * (kb_mo2(indm,ig) + minorfrac(lay) * &
                 (kb_mo2(indm+1,ig) - kb_mo2(indm,ig)))
            taug(lay,ngs10+ig) = colh2o(lay) * &
                 (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig))  &
                 + taufor &
                 + tauo2
            fracs(lay,ngs10+ig) = fracrefb(ig)
         enddo
      enddo

      end subroutine taugb11

!----------------------------------------------------------------------------
      subroutine taugb12
!----------------------------------------------------------------------------
!
!     band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng12, ngs11
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg12, ONLY: fracrefa, absa, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
      integer(kind=im) :: js, js1, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: tauself, taufor
      real(kind=rb) :: refrat_planck_a
      real(kind=rb) :: tau_major, tau_major1


! Calculate reference ratio to be used in calculation of Planck
! fraction in lower/upper atmosphere.

! P =   174.164 mb 
      refrat_planck_a = chi_mls(1,10)/chi_mls(2,10)

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum adn foreign continuum is interpolated 
! (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(12) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(12) + js1
         inds = indself(lay)
         indf = indfor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif

         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng12
            tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs11+ig) = tau_major + tau_major1 &
                 + tauself + taufor
            fracs(lay,ngs11+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers

         do ig = 1, ng12
            taug(lay,ngs11+ig) = 0.0_rb
            fracs(lay,ngs11+ig) = 0.0_rb
         enddo
      enddo

      end subroutine taugb12

!----------------------------------------------------------------------------
      subroutine taugb13
!----------------------------------------------------------------------------
!
!     band 13:  2080-2250 cm-1 (low key - h2o,n2o; high minor - o3 minor)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng13, ngs12
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg13, ONLY: fracrefa, fracrefb, absa, ka_mco2, ka_mco, kb_mo3, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      integer(kind=im) :: js, js1, jmco2, jmco, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_mco2, specparm_mco2, specmult_mco2, fmco2
      real(kind=rb) :: speccomb_mco, specparm_mco, specmult_mco, fmco
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: tauself, taufor, co2m1, co2m2, absco2 
      real(kind=rb) :: com1, com2, absco, abso3
      real(kind=rb) :: chi_co2, ratco2, adjfac, adjcolco2
      real(kind=rb) :: refrat_planck_a, refrat_m_a, refrat_m_a3
      real(kind=rb) :: tau_major, tau_major1

! Minor gas mapping levels :
!     lower - co2, p = 1053.63 mb, t = 294.2 k
!     lower - co, p = 706 mb, t = 278.94 k
!     upper - o3, p = 95.5835 mb, t = 215.7 k

! Calculate reference ratio to be used in calculation of Planck
! fraction in lower/upper atmosphere.

! P = 473.420 mb (Level 5)
      refrat_planck_a = chi_mls(1,5)/chi_mls(4,5)

! P = 1053. (Level 1)
      refrat_m_a = chi_mls(1,1)/chi_mls(4,1)

! P = 706. (Level 3)
      refrat_m_a3 = chi_mls(1,3)/chi_mls(4,3)

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = colh2o(lay) + rat_h2on2o(lay)*coln2o(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colh2o(lay) + rat_h2on2o_1(lay)*coln2o(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_mco2 = colh2o(lay) + refrat_m_a*coln2o(lay)
         specparm_mco2 = colh2o(lay)/speccomb_mco2
         if (specparm_mco2 .ge. oneminus) specparm_mco2 = oneminus
         specmult_mco2 = 8._rb*specparm_mco2
         jmco2 = 1 + int(specmult_mco2)
         fmco2 = mod(specmult_mco2,1.0_rb)

!  In atmospheres where the amount of CO2 is too great to be considered
!  a minor species, adjust the column amount of CO2 by an empirical factor 
!  to obtain the proper contribution.
         chi_co2 = colco2(lay)/(coldry(lay))
         ratco2 = 1.e20_rb*chi_co2/3.55e-4_rb
         if (ratco2 .gt. 3.0_rb) then
            adjfac = 2.0_rb+(ratco2-2.0_rb)**0.68_rb
            adjcolco2 = adjfac*3.55e-4*coldry(lay)*1.e-20_rb
         else
            adjcolco2 = colco2(lay)
         endif

         speccomb_mco = colh2o(lay) + refrat_m_a3*coln2o(lay)
         specparm_mco = colh2o(lay)/speccomb_mco
         if (specparm_mco .ge. oneminus) specparm_mco = oneminus
         specmult_mco = 8._rb*specparm_mco
         jmco = 1 + int(specmult_mco)
         fmco = mod(specmult_mco,1.0_rb)

         speccomb_planck = colh2o(lay)+refrat_planck_a*coln2o(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(13) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(13) + js1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif

         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng13
            tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            co2m1 = ka_mco2(jmco2,indm,ig) + fmco2 * &
                 (ka_mco2(jmco2+1,indm,ig) - ka_mco2(jmco2,indm,ig))
            co2m2 = ka_mco2(jmco2,indm+1,ig) + fmco2 * &
                 (ka_mco2(jmco2+1,indm+1,ig) - ka_mco2(jmco2,indm+1,ig))
            absco2 = co2m1 + minorfrac(lay) * (co2m2 - co2m1)
            com1 = ka_mco(jmco,indm,ig) + fmco * &
                 (ka_mco(jmco+1,indm,ig) - ka_mco(jmco,indm,ig))
            com2 = ka_mco(jmco,indm+1,ig) + fmco * &
                 (ka_mco(jmco+1,indm+1,ig) - ka_mco(jmco,indm+1,ig))
            absco = com1 + minorfrac(lay) * (com2 - com1)

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs12+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + adjcolco2*absco2 &
                 + colco(lay)*absco
            fracs(lay,ngs12+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         indm = indminor(lay)
         do ig = 1, ng13
            abso3 = kb_mo3(indm,ig) + minorfrac(lay) * &
                 (kb_mo3(indm+1,ig) - kb_mo3(indm,ig))
            taug(lay,ngs12+ig) = colo3(lay)*abso3
            fracs(lay,ngs12+ig) =  fracrefb(ig)
         enddo
      enddo

      end subroutine taugb13

!----------------------------------------------------------------------------
      subroutine taugb14
!----------------------------------------------------------------------------
!
!     band 14:  2250-2380 cm-1 (low - co2; high - co2)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng14, ngs13
          USE rrlw_kg14, ONLY: fracrefa, fracrefb, absa, absb, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
      real(kind=rb) :: tauself, taufor


! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum 
! and foreign continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(14) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(14) + 1
         inds = indself(lay)
         indf = indfor(lay)
         do ig = 1, ng14
            tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            taug(lay,ngs13+ig) = colco2(lay) * &
                 (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig)) &
                 + tauself + taufor
            fracs(lay,ngs13+ig) = fracrefa(ig)
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(14) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(14) + 1
         do ig = 1, ng14
            taug(lay,ngs13+ig) = colco2(lay) * &
                 (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig))
            fracs(lay,ngs13+ig) = fracrefb(ig)
         enddo
      enddo

      end subroutine taugb14

!----------------------------------------------------------------------------
      subroutine taugb15
!----------------------------------------------------------------------------
!
!     band 15:  2380-2600 cm-1 (low - n2o,co2; low minor - n2)
!                              (high - nothing)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng15, ngs14
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg15, ONLY: fracrefa, absa, ka_mn2, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
      integer(kind=im) :: js, js1, jmn2, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_mn2, specparm_mn2, specmult_mn2, fmn2
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: scalen2, tauself, taufor, n2m1, n2m2, taun2 
      real(kind=rb) :: refrat_planck_a, refrat_m_a
      real(kind=rb) :: tau_major, tau_major1


! Minor gas mapping level : 
!     Lower - Nitrogen Continuum, P = 1053., T = 294.

! Calculate reference ratio to be used in calculation of Planck
! fraction in lower atmosphere.
! P = 1053. mb (Level 1)
      refrat_planck_a = chi_mls(4,1)/chi_mls(2,1)

! P = 1053.
      refrat_m_a = chi_mls(4,1)/chi_mls(2,1)

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = coln2o(lay) + rat_n2oco2(lay)*colco2(lay)
         specparm = coln2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = coln2o(lay) + rat_n2oco2_1(lay)*colco2(lay)
         specparm1 = coln2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_mn2 = coln2o(lay) + refrat_m_a*colco2(lay)
         specparm_mn2 = coln2o(lay)/speccomb_mn2
         if (specparm_mn2 .ge. oneminus) specparm_mn2 = oneminus
         specmult_mn2 = 8._rb*specparm_mn2
         jmn2 = 1 + int(specmult_mn2)
         fmn2 = mod(specmult_mn2,1.0_rb)

         speccomb_planck = coln2o(lay)+refrat_planck_a*colco2(lay)
         specparm_planck = coln2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(15) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(15) + js1
         inds = indself(lay)
         indf = indfor(lay)
         indm = indminor(lay)
         
         scalen2 = colbrd(lay)*scaleminor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif
         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng15
            tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
            n2m1 = ka_mn2(jmn2,indm,ig) + fmn2 * &
                 (ka_mn2(jmn2+1,indm,ig) - ka_mn2(jmn2,indm,ig))
            n2m2 = ka_mn2(jmn2,indm+1,ig) + fmn2 * &
                 (ka_mn2(jmn2+1,indm+1,ig) - ka_mn2(jmn2,indm+1,ig))
            taun2 = scalen2 * (n2m1 + minorfrac(lay) * (n2m2 - n2m1))

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif 

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs14+ig) = tau_major + tau_major1 &
                 + tauself + taufor &
                 + taun2
            fracs(lay,ngs14+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         do ig = 1, ng15
            taug(lay,ngs14+ig) = 0.0_rb
            fracs(lay,ngs14+ig) = 0.0_rb
         enddo
      enddo

      end subroutine taugb15

!----------------------------------------------------------------------------
      subroutine taugb16
!----------------------------------------------------------------------------
!
!     band 16:  2600-3250 cm-1 (low key- h2o,ch4; high key - ch4)
!----------------------------------------------------------------------------

! ------- Modules -------

          USE parrrtm, ONLY: ng16, ngs15
          USE rrlw_ref, ONLY: chi_mls
          USE rrlw_kg16, ONLY: fracrefa, fracrefb, absa, absb, selfref, forref

! ------- Declarations -------

! Local 
      integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
      integer(kind=im) :: js, js1, jpl
      real(kind=rb) :: speccomb, specparm, specmult, fs
      real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
      real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      real(kind=rb) :: p, p4, fk0, fk1, fk2
      real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
      real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
      real(kind=rb) :: tauself, taufor
      real(kind=rb) :: refrat_planck_a
      real(kind=rb) :: tau_major, tau_major1


! Calculate reference ratio to be used in calculation of Planck
! fraction in lower atmosphere.

! P = 387. mb (Level 6)
      refrat_planck_a = chi_mls(1,6)/chi_mls(6,6)

! Compute the optical depth by interpolating in ln(pressure), 
! temperature,and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop

         speccomb = colh2o(lay) + rat_h2och4(lay)*colch4(lay)
         specparm = colh2o(lay)/speccomb
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
         js = 1 + int(specmult)
         fs = mod(specmult,1.0_rb)

         speccomb1 = colh2o(lay) + rat_h2och4_1(lay)*colch4(lay)
         specparm1 = colh2o(lay)/speccomb1
         if (specparm1 .ge. oneminus) specparm1 = oneminus
         specmult1 = 8._rb*(specparm1)
         js1 = 1 + int(specmult1)
         fs1 = mod(specmult1,1.0_rb)

         speccomb_planck = colh2o(lay)+refrat_planck_a*colch4(lay)
         specparm_planck = colh2o(lay)/speccomb_planck
         if (specparm_planck .ge. oneminus) specparm_planck=oneminus
         specmult_planck = 8._rb*specparm_planck
         jpl= 1 + int(specmult_planck)
         fpl = mod(specmult_planck,1.0_rb)

         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(16) + js
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(16) + js1
         inds = indself(lay)
         indf = indfor(lay)

         if (specparm .lt. 0.125_rb) then
            p = fs - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else if (specparm .gt. 0.875_rb) then
            p = -fs 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac000 = fk0*fac00(lay)
            fac100 = fk1*fac00(lay)
            fac200 = fk2*fac00(lay)
            fac010 = fk0*fac10(lay)
            fac110 = fk1*fac10(lay)
            fac210 = fk2*fac10(lay)
         else
            fac000 = (1._rb - fs) * fac00(lay)
            fac010 = (1._rb - fs) * fac10(lay)
            fac100 = fs * fac00(lay)
            fac110 = fs * fac10(lay)
         endif

         if (specparm1 .lt. 0.125_rb) then
            p = fs1 - 1
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else if (specparm1 .gt. 0.875_rb) then
            p = -fs1 
            p4 = p**4
            fk0 = p4
            fk1 = 1 - p - 2.0_rb*p4
            fk2 = p + p4
            fac001 = fk0*fac01(lay)
            fac101 = fk1*fac01(lay)
            fac201 = fk2*fac01(lay)
            fac011 = fk0*fac11(lay)
            fac111 = fk1*fac11(lay)
            fac211 = fk2*fac11(lay)
         else
            fac001 = (1._rb - fs1) * fac01(lay)
            fac011 = (1._rb - fs1) * fac11(lay)
            fac101 = fs1 * fac01(lay)
            fac111 = fs1 * fac11(lay)
         endif

         do ig = 1, ng16
            tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig)))
            taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 

            if (specparm .lt. 0.125_rb) then
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac200 * absa(ind0+2,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig) + &
                    fac210 * absa(ind0+11,ig))
            else if (specparm .gt. 0.875_rb) then
               tau_major = speccomb * &
                    (fac200 * absa(ind0-1,ig) + &
                    fac100 * absa(ind0,ig) + &
                    fac000 * absa(ind0+1,ig) + &
                    fac210 * absa(ind0+8,ig) + &
                    fac110 * absa(ind0+9,ig) + &
                    fac010 * absa(ind0+10,ig))
            else
               tau_major = speccomb * &
                    (fac000 * absa(ind0,ig) + &
                    fac100 * absa(ind0+1,ig) + &
                    fac010 * absa(ind0+9,ig) + &
                    fac110 * absa(ind0+10,ig))
            endif

            if (specparm1 .lt. 0.125_rb) then
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac201 * absa(ind1+2,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig) + &
                    fac211 * absa(ind1+11,ig))
            else if (specparm1 .gt. 0.875_rb) then
               tau_major1 = speccomb1 * &
                    (fac201 * absa(ind1-1,ig) + &
                    fac101 * absa(ind1,ig) + &
                    fac001 * absa(ind1+1,ig) + &
                    fac211 * absa(ind1+8,ig) + &
                    fac111 * absa(ind1+9,ig) + &
                    fac011 * absa(ind1+10,ig))
            else
               tau_major1 = speccomb1 * &
                    (fac001 * absa(ind1,ig) + &
                    fac101 * absa(ind1+1,ig) + &
                    fac011 * absa(ind1+9,ig) + &
                    fac111 * absa(ind1+10,ig))
            endif

            taug(lay,ngs15+ig) = tau_major + tau_major1 &
                 + tauself + taufor
            fracs(lay,ngs15+ig) = fracrefa(ig,jpl) + fpl * &
                 (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(16) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(16) + 1
         do ig = 1, ng16
            taug(lay,ngs15+ig) = colch4(lay) * &
                 (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig))
            fracs(lay,ngs15+ig) = fracrefb(ig)
         enddo
      enddo

      end subroutine taugb16

      end subroutine taumol

      end module rrtmg_lw_taumol

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! ------- Modules -------

! Steven Cavallo: added for buffer layer adjustment



! **************************************************************************
! **************************************************************************
!
!  Original version:       Michael J. Iacono; July, 1998
!  First revision for GCMs:   September, 1998
!  Second revision for RRTM_V3.0:  September, 2002
!
!  This subroutine performs calculations necessary for the initialization
!  of the longwave model.  Lookup tables are computed for use in the LW
!  radiative transfer, and input absorption coefficient data for each
!  spectral band are reduced from 256 g-point intervals to 140.
! **************************************************************************


                                              ! at constant pressure at 273 K
                                              ! (J kg-1 K-1)

! ------- Local -------




! ------- Definitions -------
!     Arrays for 10000-point look-up tables:
!     TAU_TBL Clear-sky optical depth (used in cloudy radiative transfer)
!     EXP_TBL Exponential lookup table for ransmittance
!     TFN_TBL Tau transition function; i.e. the transition of the Planck
!             function from that for the mean layer temperature to that for
!             the layer boundary temperature as a function of optical depth.
!             The "linear in tau" method is used to make the table.
!     PADE    Pade approximation constant (= 0.278)
!     BPADE   Inverse of the Pade approximation constant
!


! Initialize model data
! Moved to module_ra_rrtmg_lw for WRF
!      call lw_kgb01               ! molecular absorption coefficients
!      call lw_kgb02
!      call lw_kgb03
!      call lw_kgb04
!      call lw_kgb05
!      call lw_kgb06
!      call lw_kgb07
!      call lw_kgb08
!      call lw_kgb09
!      call lw_kgb10
!      call lw_kgb11
!      call lw_kgb12
!      call lw_kgb13
!      call lw_kgb14
!      call lw_kgb15
!      call lw_kgb16

! Compute lookup tables for transmittance, tau transition function,
! and clear sky tau (for the cloudy sky radiative transfer).  Tau is 
! computed as a function of the tau transition function, transmittance 
! is calculated as a function of tau, and the tau transition function 
! is calculated using the linear in tau formulation at values of tau 
! above 0.01.  TF is approximated as tau/6 for tau < 0.01.  All tables 
! are computed at intervals of 0.001.  The inverse of the constant used
! in the Pade approximation to the tau transition function is set to b.




! Perform g-point reduction from 16 per band (256 total points) to
! a band dependant number (140 total points) for all absorption
! coefficient input data and Planck fraction input data.
! Compute relative weighting for new g-point combinations.





! Reduce g-points for absorption coefficient data in each LW spectral band.



!***************************************************************************
!***************************************************************************

! --------- Modules ----------


 
                                               ! at constant pressure at 273 K
                                               ! (J kg-1 K-1)

! Longwave spectral band limits (wavenumbers)

! Spectral band information

!     nxmol     - number of cross-sections input by user
!     ixindx(i) - index of cross-section molecule corresponding to Ith
!                 cross-section specified by user
!                 = 0 -- not allowed in rrtm
!                 = 1 -- ccl4
!                 = 2 -- cfc11
!                 = 3 -- cfc12
!                 = 4 -- cfc22

! Fundamental physical constants from NIST 2002

                                              ! (m s-2)
                                              ! (ergs s; g cm2 s-1)
                                              ! (ergs K-1; g cm2 s-2 K-1)
                                              ! (cm s-1)
                                              ! (mol-1)
                                              ! (cm-3)
                                              ! (ergs mol-1 K-1)
                                              ! (W cm2 sr-1)
                                              ! (cm K)
                                              ! (W cm-2 K-4)
                                              ! (s d-1)
!
!     units are generally cgs
!
!     The first and second radiation constants are taken from NIST.
!     They were previously obtained from the relations:
!          radcn1 = 2.*planck*clight*clight*1.e-07
!          radcn2 = planck*clight/boltz

!     Heatfac is the factor by which delta-flux / delta-pressure is
!     multiplied, with flux in W/m-2 and pressure in mbar, to get 
!     the heating rate in units of degrees/day.  It is equal to:
!     Original value:
!           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!           Here, cpdair (1.004) is in units of J g-1 K-1, and the 
!           constant (1.e-5) converts mb to Pa and g-1 to kg-1.
!        =  (9.8066)(86400)(1e-5)/(1.004)
!      heatfac = 8.4391_rb
!
!     Modified value for consistency with CAM3:
!           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!           Here, cpdair (1.00464) is in units of J g-1 K-1, and the
!           constant (1.e-5) converts mb to Pa and g-1 to kg-1.
!        =  (9.80616)(86400)(1e-5)/(1.00464)
!      heatfac = 8.43339130434_rb
!
!     Calculated value:
!        (grav) x (#sec/day) / (specific heat of dry air at const. p x 1.e2)
!           Here, cpdair is in units of J kg-1 K-1, and the constant (1.e2) 
!           converts mb to Pa when heatfac is multiplied by W m-2 mb-1. 


!***************************************************************************
!***************************************************************************

 
! ------- Definitions -------
!     Arrays for the g-point reduction from 256 to 140 for the 16 LW bands:
!     This mapping from 256 to 140 points has been carefully selected to 
!     minimize the effect on the resulting fluxes and cooling rates, and
!     caution should be used if the mapping is modified.  The full 256
!     g-point set can be restored with ngptlw=256, ngc=16*16, ngn=256*1., etc.
!     ngptlw  The total number of new g-points
!     ngc     The number of new g-points in each band
!     ngs     The cumulative sum of new g-points for each band
!     ngm     The index of each new g-point relative to the original
!             16 g-points for each band.  
!     ngn     The number of original g-points that are combined to make
!             each new g-point in each band.
!     ngb     The band index for each new g-point.
!     wt      RRTM weights for 16 g-points.

! ------- Data statements -------


!***************************************************************************
!***************************************************************************
!
!  Original version:    MJIacono; July 1998
!  Revision for GCMs:   MJIacono; September 1998
!  Revision for RRTMG:  MJIacono, September 2002
!  Revision for F90 reformatting:  MJIacono, June 2006
!
!  The subroutines CMBGB1->CMBGB16 input the absorption coefficient
!  data for each band, which are defined for 16 g-points and 16 spectral
!  bands. The data are combined with appropriate weighting following the
!  g-point mapping arrays specified in RRTMINIT.  Plank fraction data
!  in arrays FRACREFA and FRACREFB are combined without weighting.  All
!  g-point reduced data are put into new arrays for use in RRTM.
!
!  band 1:  10-350 cm-1 (low key - h2o; low minor - n2)
!                       (high key - h2o; high minor - n2)
!  note: previous versions of rrtm band 1: 
!        10-250 cm-1 (low - h2o; high - h2o)
!***************************************************************************


! ------- Local -------














!***************************************************************************
!***************************************************************************
!
!     band 2:  350-500 cm-1 (low key - h2o; high key - h2o)
!
!     note: previous version of rrtm band 2: 
!           250 - 500 cm-1 (low - h2o; high - h2o)
!***************************************************************************


! ------- Local -------












!***************************************************************************
!***************************************************************************
!
!     band 3:  500-630 cm-1 (low key - h2o,co2; low minor - n2o)
!                           (high key - h2o,co2; high minor - n2o)
!
! old band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!***************************************************************************


! ------- Local -------


















!***************************************************************************
!***************************************************************************
!
!     band 4:  630-700 cm-1 (low key - h2o,co2; high key - o3,co2)
!
! old band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!***************************************************************************


! ------- Local -------














!***************************************************************************
!***************************************************************************
!
!     band 5:  700-820 cm-1 (low key - h2o,co2; low minor - o3, ccl4)
!                           (high key - o3,co2)
!
! old band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!***************************************************************************


! ------- Local -------


















!***************************************************************************
!***************************************************************************
!
!     band 6:  820-980 cm-1 (low key - h2o; low minor - co2)
!                           (high key - nothing; high minor - cfc11, cfc12)
!
! old band 6:  820-980 cm-1 (low - h2o; high - nothing)
!***************************************************************************


! ------- Local -------













!***************************************************************************
!***************************************************************************
!
!     band 7:  980-1080 cm-1 (low key - h2o,o3; low minor - co2)
!                            (high key - o3; high minor - co2)
!
! old band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!***************************************************************************


! ------- Local -------


















!***************************************************************************
!***************************************************************************
!
!     band 8:  1080-1180 cm-1 (low key - h2o; low minor - co2,o3,n2o)
!                             (high key - o3; high minor - co2, n2o)
!
! old band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!***************************************************************************


! ------- Local -------














!***************************************************************************
!***************************************************************************
!
!     band 9:  1180-1390 cm-1 (low key - h2o,ch4; low minor - n2o)
!                             (high key - ch4; high minor - n2o)!

! old band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!***************************************************************************


! ------- Local -------



















!***************************************************************************
!***************************************************************************
!
!     band 10:  1390-1480 cm-1 (low key - h2o; high key - h2o)
!
! old band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!***************************************************************************


! ------- Local -------













!***************************************************************************
!***************************************************************************
!
!     band 11:  1480-1800 cm-1 (low - h2o; low minor - o2)
!                              (high key - h2o; high minor - o2)
!
! old band 11:  1480-1800 cm-1 (low - h2o; low minor - o2)
!                              (high key - h2o; high minor - o2)
!***************************************************************************


! ------- Local -------














!***************************************************************************
!***************************************************************************
!
!     band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
! old band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!***************************************************************************


! ------- Local -------











!***************************************************************************
!***************************************************************************
!
!     band 13:  2080-2250 cm-1 (low key - h2o,n2o; high minor - o3 minor)
!
! old band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!***************************************************************************


! ------- Local -------

















!***************************************************************************
!***************************************************************************
!
!     band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
! old band 14:  2250-2380 cm-1 (low - co2; high - co2)
!***************************************************************************


! ------- Local -------













!***************************************************************************
!***************************************************************************
!
!     band 15:  2380-2600 cm-1 (low - n2o,co2; low minor - n2)
!                              (high - nothing)
!
! old band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!***************************************************************************


! ------- Local -------













!***************************************************************************
!***************************************************************************
!
!     band 16:  2600-3250 cm-1 (low key- h2o,ch4; high key - ch4)
!
! old band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!***************************************************************************


! ------- Local -------















!***************************************************************************
!***************************************************************************

! --------- Modules ----------



! ABSCLDn is the liquid water absorption coefficient (m2/g). 
! For INFLAG = 1.
!  
! Everything below is for INFLAG = 2.

! ABSICEn(J,IB) are the parameters needed to compute the liquid water 
! absorption coefficient in spectral region IB for ICEFLAG=n.  The units
! of ABSICEn(1,IB) are m2/g and ABSICEn(2,IB) has units (microns (m2/g)).
! For ICEFLAG = 0.


! For ICEFLAG = 1.

! For ICEFLAG = 2.  In each band, the absorption
! coefficients are listed for a range of effective radii from 5.0
! to 131.0 microns in increments of 3.0 microns.
! Spherical Ice Particle Parameterization
! absorption units (abs coef/iwc): [(m^-1)/(g m^-3)]
! band 1
! band 2
! band 3
! band 4
! band 5
! band 6
! band 7
! band 8
! band 9
! band 10
! band 11
! band 12
! band 13
! band 14
! band 15
! band 16

! ICEFLAG = 3; Fu parameterization. Particle size 5 - 140 micron in 
! increments of 3 microns.
! units = m2/g
! Hexagonal Ice Particle Parameterization
! absorption units (abs coef/iwc): [(m^-1)/(g m^-3)]
! band 1
! band 2
! band 3
! band 4
! band 5
! band 6
! band 7
! band 8
! band 9
! band 10
! band 11
! band 12
! band 13
! band 14
! band 15
! band 16

! For LIQFLAG = 0.

! For LIQFLAG = 1.  In each band, the absorption
! coefficients are listed for a range of effective radii from 2.5
! to 59.5 microns in increments of 1.0 micron.
! band  1
! band  2
! band  3
! band  4
! band  5
! band  6
! band  7
! band  8
! band  9
! band 10
! band 11
! band 12
! band 13
! band 14
! band 15
! band 16



!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!
       module rrtmg_lw_rad

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
!
! ****************************************************************************
! *                                                                          *
! *                              RRTMG_LW                                    *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                   a rapid radiative transfer model                       *
! *                       for the longwave region                            * 
! *             for application to general circulation models                *
! *                                                                          *
! *                                                                          *
! *            Atmospheric and Environmental Research, Inc.                  *
! *                        131 Hartwell Avenue                               *
! *                        Lexington, MA 02421                               *
! *                                                                          *
! *                                                                          *
! *                           Eli J. Mlawer                                  *
! *                        Jennifer S. Delamere                              *
! *                         Michael J. Iacono                                *
! *                         Shepard A. Clough                                *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                       email:  miacono@aer.com                            *
! *                       email:  emlawer@aer.com                            *
! *                       email:  jdelamer@aer.com                           *
! *                                                                          *
! *        The authors wish to acknowledge the contributions of the          *
! *        following people:  Steven J. Taubman, Karen Cady-Pereira,         *
! *        Patrick D. Brown, Ronald E. Farren, Luke Chen, Robert Bergstrom.  *
! *                                                                          *
! ****************************************************************************

! -------- Modules --------
           USE parkind, ONLY: im => kind_im, rb => kind_rb
           USE rrtmg_lw_cldprmc, ONLY: cldprmc
! *** Move the required call to rrtmg_lw_ini below and the following 
! use association to the GCM initialization area ***
!      use rrtmg_lw_init, only: rrtmg_lw_ini
           USE rrtmg_lw_rtrnmc, ONLY: rtrnmc
           USE rrtmg_lw_setcoef, ONLY: setcoef
           USE rrtmg_lw_taumol, ONLY: taumol

           IMPLICIT NONE

! public interfaces/functions/subroutines
           PUBLIC rrtmg_lw, inatm

!------------------------------------------------------------------
      contains
!------------------------------------------------------------------

!------------------------------------------------------------------
! Public subroutines
!------------------------------------------------------------------

      subroutine rrtmg_lw &
            (ncol    ,nlay    ,icld    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc    , & 
             h2ovmr  ,o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr  ,o2vmr , &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis    , &
             inflglw ,iceflglw,liqflglw,cldfmcl , &
             taucmcl ,ciwpmcl ,clwpmcl ,reicmcl ,relqmcl , &
             tauaer  , &
             uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc)

! -------- Description --------

! This program is the driver subroutine for RRTMG_LW, the AER LW radiation 
! model for application to GCMs, that has been adapted from RRTM_LW for
! improved efficiency.
!
! NOTE: The call to RRTMG_LW_INI should be moved to the GCM initialization
!  area, since this has to be called only once. 
!
! This routine:
!    a) calls INATM to read in the atmospheric profile from GCM;
!       all layering in RRTMG is ordered from surface to toa. 
!    b) calls CLDPRMC to set cloud optical depth for McICA based 
!       on input cloud properties 
!    c) calls SETCOEF to calculate various quantities needed for 
!       the radiative transfer algorithm
!    d) calls TAUMOL to calculate gaseous optical depths for each 
!       of the 16 spectral bands
!    e) calls RTRNMC (for both clear and cloudy profiles) to perform the
!       radiative transfer calculation using McICA, the Monte-Carlo 
!       Independent Column Approximation, to represent sub-grid scale 
!       cloud variability
!    f) passes the necessary fluxes and cooling rates back to GCM
!
! Two modes of operation are possible:
!     The mode is chosen by using either rrtmg_lw.nomcica.f90 (to not use
!     McICA) or rrtmg_lw.f90 (to use McICA) to interface with a GCM. 
!
!    1) Standard, single forward model calculation (imca = 0)
!    2) Monte Carlo Independent Column Approximation (McICA, Pincus et al., 
!       JC, 2003) method is applied to the forward model calculation (imca = 1)
!
! This call to RRTMG_LW must be preceeded by a call to the module
!     mcica_subcol_gen_lw.f90 to run the McICA sub-column cloud generator,
!     which will provide the cloud physical or cloud optical properties
!     on the RRTMG quadrature point (ngpt) dimension.
!     Two random number generators are available for use when imca = 1.
!     This is chosen by setting flag irnd on input to mcica_subcol_gen_lw.
!     1) KISSVEC (irnd = 0)
!     2) Mersenne-Twister (irnd = 1)
!
! Two methods of cloud property input are possible:
!     Cloud properties can be input in one of two ways (controlled by input 
!     flags inflglw, iceflglw, and liqflglw; see text file rrtmg_lw_instructions
!     and subroutine rrtmg_lw_cldprop.f90 for further details):
!
!    1) Input cloud fraction and cloud optical depth directly (inflglw = 0)
!    2) Input cloud fraction and cloud physical properties (inflglw = 1 or 2);  
!       cloud optical properties are calculated by cldprop or cldprmc based
!       on input settings of iceflglw and liqflglw.  Ice particle size provided
!       must be appropriately defined for the ice parameterization selected. 
!
! One method of aerosol property input is possible:
!     Aerosol properties can be input in only one way (controlled by input 
!     flag iaer; see text file rrtmg_lw_instructions for further details):
!
!    1) Input aerosol optical depth directly by layer and spectral band (iaer=10);
!       band average optical depth at the mid-point of each spectral band.
!       RRTMG_LW currently treats only aerosol absorption;
!       scattering capability is not presently available.
!
!
! ------- Modifications -------
!
! This version of RRTMG_LW has been modified from RRTM_LW to use a reduced 
! set of g-points for application to GCMs.  
!
!-- Original version (derived from RRTM_LW), reduction of g-points, other
!   revisions for use with GCMs.  
!     1999: M. J. Iacono, AER, Inc.
!-- Adapted for use with NCAR/CAM.
!     May 2004: M. J. Iacono, AER, Inc.
!-- Revised to add McICA capability. 
!     Nov 2005: M. J. Iacono, AER, Inc.
!-- Conversion to F90 formatting for consistency with rrtmg_sw.
!     Feb 2007: M. J. Iacono, AER, Inc.
!-- Modifications to formatting to use assumed-shape arrays.
!     Aug 2007: M. J. Iacono, AER, Inc.
!-- Modified to add longwave aerosol absorption.
!     Apr 2008: M. J. Iacono, AER, Inc.

! --------- Modules ----------

          USE parrrtm, ONLY: nbndlw, ngptlw, maxxsec, mxmol
          USE rrlw_con, ONLY: fluxfac, oneminus, pi
          USE rrlw_wvn, ONLY: ngb

! ------- Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: ncol            ! Number of horizontal columns
      integer(kind=im), intent(in) :: nlay            ! Number of model layers
      integer(kind=im), intent(inout) :: icld         ! Cloud overlap method
                                                      !    0: Clear only
                                                      !    1: Random
                                                      !    2: Maximum/random
                                                      !    3: Maximum
      real(kind=rb), intent(in) :: play(:,:)          ! Layer pressures (hPa, mb)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: plev(:,:)          ! Interface pressures (hPa, mb)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(in) :: tlay(:,:)          ! Layer temperatures (K)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tlev(:,:)          ! Interface temperatures (K)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(in) :: tsfc(:)            ! Surface temperature (K)
                                                      !    Dimensions: (ncol)
      real(kind=rb), intent(in) :: h2ovmr(:,:)        ! H2O volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: o3vmr(:,:)         ! O3 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: co2vmr(:,:)        ! CO2 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: ch4vmr(:,:)        ! Methane volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: n2ovmr(:,:)        ! Nitrous oxide volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: o2vmr(:,:)         ! Oxygen volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cfc11vmr(:,:)      ! CFC11 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cfc12vmr(:,:)      ! CFC12 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cfc22vmr(:,:)      ! CFC22 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: ccl4vmr(:,:)       ! CCL4 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: emis(:,:)          ! Surface emissivity
                                                      !    Dimensions: (ncol,nbndlw)

      integer(kind=im), intent(in) :: inflglw         ! Flag for cloud optical properties
      integer(kind=im), intent(in) :: iceflglw        ! Flag for ice particle specification
      integer(kind=im), intent(in) :: liqflglw        ! Flag for liquid droplet specification

      real(kind=rb), intent(in) :: cldfmcl(:,:,:)     ! Cloud fraction
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(in) :: ciwpmcl(:,:,:)     ! In-cloud ice water path (g/m2)
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(in) :: clwpmcl(:,:,:)     ! In-cloud liquid water path (g/m2)
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(in) :: reicmcl(:,:)       ! Cloud ice particle effective size (microns)
                                                      !    Dimensions: (ncol,nlay)
                                                      ! specific definition of reicmcl depends on setting of iceflglw:
                                                      ! iceflglw = 0: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                                      !               r_ec must be >= 10.0 microns
                                                      ! iceflglw = 1: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                                      !               r_ec range is limited to 13.0 to 130.0 microns
                                                      ! iceflglw = 2: ice effective radius, r_k, (Key, Streamer Ref. Manual, 1996)
                                                      !               r_k range is limited to 5.0 to 131.0 microns
                                                      ! iceflglw = 3: generalized effective size, dge, (Fu, 1996),
                                                      !               dge range is limited to 5.0 to 140.0 microns
                                                      !               [dge = 1.0315 * r_ec]
      real(kind=rb), intent(in) :: relqmcl(:,:)       ! Cloud water drop effective radius (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: taucmcl(:,:,:)     ! In-cloud optical depth
                                                      !    Dimensions: (ngptlw,ncol,nlay)
!      real(kind=rb), intent(in) :: ssacmcl(:,:,:)    ! In-cloud single scattering albedo
                                                      !    Dimensions: (ngptlw,ncol,nlay)
                                                      !   for future expansion
                                                      !   lw scattering not yet available
!      real(kind=rb), intent(in) :: asmcmcl(:,:,:)    ! In-cloud asymmetry parameter
                                                      !    Dimensions: (ngptlw,ncol,nlay)
                                                      !   for future expansion
                                                      !   lw scattering not yet available
      real(kind=rb), intent(in) :: tauaer(:,:,:)      ! aerosol optical depth
                                                      !   at mid-point of LW spectral bands
                                                      !    Dimensions: (ncol,nlay,nbndlw)
!      real(kind=rb), intent(in) :: ssaaer(:,:,:)     ! aerosol single scattering albedo
                                                      !    Dimensions: (ncol,nlay,nbndlw)
                                                      !   for future expansion 
                                                      !   (lw aerosols/scattering not yet available)
!      real(kind=rb), intent(in) :: asmaer(:,:,:)     ! aerosol asymmetry parameter
                                                      !    Dimensions: (ncol,nlay,nbndlw)
                                                      !   for future expansion 
                                                      !   (lw aerosols/scattering not yet available)

! ----- Output -----

      real(kind=rb), intent(out) :: uflx(:,:)         ! Total sky longwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: dflx(:,:)         ! Total sky longwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: hr(:,:)           ! Total sky longwave radiative heating rate (K/d)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(out) :: uflxc(:,:)        ! Clear sky longwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: dflxc(:,:)        ! Clear sky longwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: hrc(:,:)          ! Clear sky longwave radiative heating rate (K/d)
                                                      !    Dimensions: (ncol,nlay)

! ----- Local -----

! Control
      integer(kind=im) :: nlayers             ! total number of layers
      integer(kind=im) :: istart              ! beginning band of calculation
      integer(kind=im) :: iend                ! ending band of calculation
      integer(kind=im) :: iout                ! output option flag (inactive)
      integer(kind=im) :: iaer                ! aerosol option flag
      integer(kind=im) :: iplon               ! column loop index
      integer(kind=im) :: imca                ! flag for mcica [0=off, 1=on]
      integer(kind=im) :: ims                 ! value for changing mcica permute seed
      integer(kind=im) :: k                   ! layer loop index
      integer(kind=im) :: ig                  ! g-point loop index

! Atmosphere
      real(kind=rb) :: pavel(nlay+1)          ! layer pressures (mb) 
      real(kind=rb) :: tavel(nlay+1)          ! layer temperatures (K)
      real(kind=rb) :: pz(0:nlay+1)           ! level (interface) pressures (hPa, mb)
      real(kind=rb) :: tz(0:nlay+1)           ! level (interface) temperatures (K)
      real(kind=rb) :: tbound                 ! surface temperature (K)
      real(kind=rb) :: coldry(nlay+1)         ! dry air column density (mol/cm2)
      real(kind=rb) :: wbrodl(nlay+1)         ! broadening gas column density (mol/cm2)
      real(kind=rb) :: wkl(mxmol,nlay+1)      ! molecular amounts (mol/cm-2)
      real(kind=rb) :: wx(maxxsec,nlay+1)     ! cross-section amounts (mol/cm-2)
      real(kind=rb) :: pwvcm                  ! precipitable water vapor (cm)
      real(kind=rb) :: semiss(nbndlw)         ! lw surface emissivity
      real(kind=rb) :: fracs(nlay+1,ngptlw)   ! 
      real(kind=rb) :: taug(nlay+1,ngptlw)    ! gaseous optical depths
      real(kind=rb) :: taut(nlay+1,ngptlw)    ! gaseous + aerosol optical depths

      real(kind=rb) :: taua(nlay+1,nbndlw)    ! aerosol optical depth
!      real(kind=rb) :: ssaa(nlay+1,nbndlw)   ! aerosol single scattering albedo
                                              !   for future expansion 
                                              !   (lw aerosols/scattering not yet available)
!      real(kind=rb) :: asma(nlay+1,nbndlw)   ! aerosol asymmetry parameter
                                              !   for future expansion 
                                              !   (lw aerosols/scattering not yet available)

! Atmosphere - setcoef
      integer(kind=im) :: laytrop             ! tropopause layer index
      integer(kind=im) :: jp(nlay+1)          ! lookup table index 
      integer(kind=im) :: jt(nlay+1)          ! lookup table index 
      integer(kind=im) :: jt1(nlay+1)         ! lookup table index 
      real(kind=rb) :: planklay(nlay+1,nbndlw)! 
      real(kind=rb) :: planklev(0:nlay+1,nbndlw)! 
      real(kind=rb) :: plankbnd(nbndlw)       ! 

      real(kind=rb) :: colh2o(nlay+1)         ! column amount (h2o)
      real(kind=rb) :: colco2(nlay+1)         ! column amount (co2)
      real(kind=rb) :: colo3(nlay+1)          ! column amount (o3)
      real(kind=rb) :: coln2o(nlay+1)         ! column amount (n2o)
      real(kind=rb) :: colco(nlay+1)          ! column amount (co)
      real(kind=rb) :: colch4(nlay+1)         ! column amount (ch4)
      real(kind=rb) :: colo2(nlay+1)          ! column amount (o2)
      real(kind=rb) :: colbrd(nlay+1)         ! column amount (broadening gases)

      integer(kind=im) :: indself(nlay+1)
      integer(kind=im) :: indfor(nlay+1)
      real(kind=rb) :: selffac(nlay+1)
      real(kind=rb) :: selffrac(nlay+1)
      real(kind=rb) :: forfac(nlay+1)
      real(kind=rb) :: forfrac(nlay+1)

      integer(kind=im) :: indminor(nlay+1)
      real(kind=rb) :: minorfrac(nlay+1)
      real(kind=rb) :: scaleminor(nlay+1)
      real(kind=rb) :: scaleminorn2(nlay+1)

      real(kind=rb) :: &                      !
                         fac00(nlay+1), fac01(nlay+1), &
                         fac10(nlay+1), fac11(nlay+1) 
      real(kind=rb) :: &                      !
                         rat_h2oco2(nlay+1),rat_h2oco2_1(nlay+1), &
                         rat_h2oo3(nlay+1),rat_h2oo3_1(nlay+1), &
                         rat_h2on2o(nlay+1),rat_h2on2o_1(nlay+1), &
                         rat_h2och4(nlay+1),rat_h2och4_1(nlay+1), &
                         rat_n2oco2(nlay+1),rat_n2oco2_1(nlay+1), &
                         rat_o3co2(nlay+1),rat_o3co2_1(nlay+1)

! Atmosphere/clouds - cldprop
      integer(kind=im) :: ncbands             ! number of cloud spectral bands
      integer(kind=im) :: inflag              ! flag for cloud property method
      integer(kind=im) :: iceflag             ! flag for ice cloud properties
      integer(kind=im) :: liqflag             ! flag for liquid cloud properties

! Atmosphere/clouds - cldprmc [mcica]
      real(kind=rb) :: cldfmc(ngptlw,nlay+1)  ! cloud fraction [mcica]
      real(kind=rb) :: ciwpmc(ngptlw,nlay+1)  ! in-cloud ice water path [mcica]
      real(kind=rb) :: clwpmc(ngptlw,nlay+1)  ! in-cloud liquid water path [mcica]
      real(kind=rb) :: relqmc(nlay+1)         ! liquid particle effective radius (microns)
      real(kind=rb) :: reicmc(nlay+1)         ! ice particle effective size (microns)
      real(kind=rb) :: taucmc(ngptlw,nlay+1)  ! in-cloud optical depth [mcica]
!      real(kind=rb) :: ssacmc(ngptlw,nlay+1) ! in-cloud single scattering albedo [mcica]
                                              !   for future expansion 
                                              !   (lw scattering not yet available)
!      real(kind=rb) :: asmcmc(ngptlw,nlay+1) ! in-cloud asymmetry parameter [mcica]
                                              !   for future expansion 
                                              !   (lw scattering not yet available)

! Output
      real(kind=rb) :: totuflux(0:nlay+1)     ! upward longwave flux (w/m2)
      real(kind=rb) :: totdflux(0:nlay+1)     ! downward longwave flux (w/m2)
      real(kind=rb) :: fnet(0:nlay+1)         ! net longwave flux (w/m2)
      real(kind=rb) :: htr(0:nlay+1)          ! longwave heating rate (k/day)
      real(kind=rb) :: totuclfl(0:nlay+1)     ! clear sky upward longwave flux (w/m2)
      real(kind=rb) :: totdclfl(0:nlay+1)     ! clear sky downward longwave flux (w/m2)
      real(kind=rb) :: fnetc(0:nlay+1)        ! clear sky net longwave flux (w/m2)
      real(kind=rb) :: htrc(0:nlay+1)         ! clear sky longwave heating rate (k/day)

!
! Initializations

      oneminus = 1._rb - 1.e-6_rb
      pi = 2._rb * asin(1._rb)
      fluxfac = pi * 2.e4_rb                  ! orig:   fluxfac = pi * 2.d4  
      istart = 1
      iend = 16
      iout = 0
      ims = 1

! Set imca to select calculation type:
!  imca = 0, use standard forward model calculation
!  imca = 1, use McICA for Monte Carlo treatment of sub-grid cloud variability

! *** This version uses McICA (imca = 1) ***

! Set icld to select of clear or cloud calculation and cloud overlap method  
! icld = 0, clear only
! icld = 1, with clouds using random cloud overlap
! icld = 2, with clouds using maximum/random cloud overlap
! icld = 3, with clouds using maximum cloud overlap (McICA only)
      if (icld.lt.0.or.icld.gt.3) icld = 2

! Set iaer to select aerosol option
! iaer = 0, no aerosols
! icld = 10, input total aerosol optical depth (tauaer) directly
      iaer = 10

! Call model and data initialization, compute lookup tables, perform
! reduction of g-points from 256 to 140 for input absorption coefficient 
! data and other arrays.
!
! In a GCM this call should be placed in the model initialization
! area, since this has to be called only once.  
!      call rrtmg_lw_ini(cpdair)

!  This is the main longitude/column loop within RRTMG.
      do iplon = 1, ncol

!  Prepare atmospheric profile from GCM for use in RRTMG, and define
!  other input parameters.  

         call inatm (iplon, nlay, icld, iaer, &
              play, plev, tlay, tlev, tsfc, h2ovmr, &
              o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, &
              cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, &
              cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, &
              nlayers, pavel, pz, tavel, tz, tbound, semiss, coldry, &
              wkl, wbrodl, wx, pwvcm, inflag, iceflag, liqflag, &
              cldfmc, taucmc, ciwpmc, clwpmc, reicmc, relqmc, taua)

!  For cloudy atmosphere, use cldprop to set cloud optical properties based on
!  input cloud physical properties.  Select method based on choices described
!  in cldprop.  Cloud fraction, water path, liquid droplet and ice particle
!  effective radius must be passed into cldprop.  Cloud fraction and cloud
!  optical depth are transferred to rrtmg_lw arrays in cldprop.  

         call cldprmc(nlayers, inflag, iceflag, liqflag, cldfmc, ciwpmc, &
                      clwpmc, reicmc, relqmc, ncbands, taucmc)

! Calculate information needed by the radiative transfer routine
! that is specific to this atmosphere, especially some of the 
! coefficients and indices needed to compute the optical depths
! by interpolating data from stored reference atmospheres. 

         call setcoef(nlayers, istart, pavel, tavel, tz, tbound, semiss, &
                      coldry, wkl, wbrodl, &
                      laytrop, jp, jt, jt1, planklay, planklev, plankbnd, &
                      colh2o, colco2, colo3, coln2o, colco, colch4, colo2, &
                      colbrd, fac00, fac01, fac10, fac11, &
                      rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1, &
                      rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1, &
                      rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1, &
                      selffac, selffrac, indself, forfac, forfrac, indfor, &
                      minorfrac, scaleminor, scaleminorn2, indminor)

!  Calculate the gaseous optical depths and Planck fractions for 
!  each longwave spectral band.

         call taumol(nlayers, pavel, wx, coldry, &
                     laytrop, jp, jt, jt1, planklay, planklev, plankbnd, &
                     colh2o, colco2, colo3, coln2o, colco, colch4, colo2, &
                     colbrd, fac00, fac01, fac10, fac11, &
                     rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1, &
                     rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1, &
                     rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1, &
                     selffac, selffrac, indself, forfac, forfrac, indfor, &
                     minorfrac, scaleminor, scaleminorn2, indminor, &
                     fracs, taug)


! Combine gaseous and aerosol optical depths, if aerosol active
         if (iaer .eq. 0) then
            do k = 1, nlayers
               do ig = 1, ngptlw
                  taut(k,ig) = taug(k,ig)
               enddo
            enddo
         elseif (iaer .eq. 10) then
            do k = 1, nlayers
               do ig = 1, ngptlw
                  taut(k,ig) = taug(k,ig) + taua(k,ngb(ig))
               enddo
            enddo
         endif

! Call the radiative transfer routine.
! Either routine can be called to do clear sky calculation.  If clouds
! are present, then select routine based on cloud overlap assumption
! to be used.  Clear sky calculation is done simultaneously.
! For McICA, RTRNMC is called for clear and cloudy calculations.

         call rtrnmc(nlayers, istart, iend, iout, pz, semiss, ncbands, &
                     cldfmc, taucmc, planklay, planklev, plankbnd, &
                     pwvcm, fracs, taut, &
                     totuflux, totdflux, fnet, htr, &
                     totuclfl, totdclfl, fnetc, htrc )

!  Transfer up and down fluxes and heating rate to output arrays.
!  Vertical indexing goes from bottom to top; reverse here for GCM if necessary.

         do k = 0, nlayers
            uflx(iplon,k+1) = totuflux(k)
            dflx(iplon,k+1) = totdflux(k)
            uflxc(iplon,k+1) = totuclfl(k)
            dflxc(iplon,k+1) = totdclfl(k)
         enddo
         do k = 0, nlayers-1
            hr(iplon,k+1) = htr(k)
            hrc(iplon,k+1) = htrc(k)
         enddo

      enddo

      end subroutine rrtmg_lw

!***************************************************************************
      subroutine inatm (iplon, nlay, icld, iaer, &
              play, plev, tlay, tlev, tsfc, h2ovmr, &
              o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, &
              cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, &
              cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, &
              nlayers, pavel, pz, tavel, tz, tbound, semiss, coldry, &
              wkl, wbrodl, wx, pwvcm, inflag, iceflag, liqflag, &
              cldfmc, taucmc, ciwpmc, clwpmc, reicmc, relqmc, taua)
!***************************************************************************
!
!  Input atmospheric profile from GCM, and prepare it for use in RRTMG_LW.
!  Set other RRTMG_LW input parameters.  
!
!***************************************************************************

! --------- Modules ----------

          USE parrrtm, ONLY: nbndlw, ngptlw, nmol, maxxsec
          USE rrlw_con, ONLY: grav, avogad
          USE rrlw_wvn, ONLY: ixindx

! ------- Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: iplon           ! column loop index
      integer(kind=im), intent(in) :: nlay            ! Number of model layers
      integer(kind=im), intent(in) :: icld            ! clear/cloud and cloud overlap flag
      integer(kind=im), intent(in) :: iaer            ! aerosol option flag

      real(kind=rb), intent(in) :: play(:,:)          ! Layer pressures (hPa, mb)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: plev(:,:)          ! Interface pressures (hPa, mb)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(in) :: tlay(:,:)          ! Layer temperatures (K)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tlev(:,:)          ! Interface temperatures (K)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(in) :: tsfc(:)            ! Surface temperature (K)
                                                      !    Dimensions: (ncol)
      real(kind=rb), intent(in) :: h2ovmr(:,:)        ! H2O volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: o3vmr(:,:)         ! O3 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: co2vmr(:,:)        ! CO2 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: ch4vmr(:,:)        ! Methane volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: n2ovmr(:,:)        ! Nitrous oxide volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: o2vmr(:,:)         ! Oxygen volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cfc11vmr(:,:)      ! CFC11 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cfc12vmr(:,:)      ! CFC12 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cfc22vmr(:,:)      ! CFC22 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: ccl4vmr(:,:)       ! CCL4 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: emis(:,:)          ! Surface emissivity
                                                      !    Dimensions: (ncol,nbndlw)

      integer(kind=im), intent(in) :: inflglw         ! Flag for cloud optical properties
      integer(kind=im), intent(in) :: iceflglw        ! Flag for ice particle specification
      integer(kind=im), intent(in) :: liqflglw        ! Flag for liquid droplet specification

      real(kind=rb), intent(in) :: cldfmcl(:,:,:)     ! Cloud fraction
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(in) :: ciwpmcl(:,:,:)     ! In-cloud ice water path (g/m2)
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(in) :: clwpmcl(:,:,:)     ! In-cloud liquid water path (g/m2)
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(in) :: relqmcl(:,:)       ! Cloud water drop effective radius (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: reicmcl(:,:)       ! Cloud ice effective size (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: taucmcl(:,:,:)     ! In-cloud optical depth
                                                      !    Dimensions: (ngptlw,ncol,nlay)
      real(kind=rb), intent(in) :: tauaer(:,:,:)      ! Aerosol optical depth
                                                      !    Dimensions: (ncol,nlay,nbndlw)

! ----- Output -----
! Atmosphere
      integer(kind=im), intent(out) :: nlayers        ! number of layers

      real(kind=rb), intent(out) :: pavel(:)          ! layer pressures (mb) 
                                                      !    Dimensions: (nlay)
      real(kind=rb), intent(out) :: tavel(:)          ! layer temperatures (K)
                                                      !    Dimensions: (nlay)
      real(kind=rb), intent(out) :: pz(0:)            ! level (interface) pressures (hPa, mb)
                                                      !    Dimensions: (0:nlay)
      real(kind=rb), intent(out) :: tz(0:)            ! level (interface) temperatures (K)
                                                      !    Dimensions: (0:nlay)
      real(kind=rb), intent(out) :: tbound            ! surface temperature (K)
      real(kind=rb), intent(out) :: coldry(:)         ! dry air column density (mol/cm2)
                                                      !    Dimensions: (nlay)
      real(kind=rb), intent(out) :: wbrodl(:)         ! broadening gas column density (mol/cm2)
                                                      !    Dimensions: (nlay)
      real(kind=rb), intent(out) :: wkl(:,:)          ! molecular amounts (mol/cm-2)
                                                      !    Dimensions: (mxmol,nlay)
      real(kind=rb), intent(out) :: wx(:,:)           ! cross-section amounts (mol/cm-2)
                                                      !    Dimensions: (maxxsec,nlay)
      real(kind=rb), intent(out) :: pwvcm             ! precipitable water vapor (cm)
      real(kind=rb), intent(out) :: semiss(:)         ! lw surface emissivity
                                                      !    Dimensions: (nbndlw)

! Atmosphere/clouds - cldprop
      integer(kind=im), intent(out) :: inflag         ! flag for cloud property method
      integer(kind=im), intent(out) :: iceflag        ! flag for ice cloud properties
      integer(kind=im), intent(out) :: liqflag        ! flag for liquid cloud properties

      real(kind=rb), intent(out) :: cldfmc(:,:)       ! cloud fraction [mcica]
                                                      !    Dimensions: (ngptlw,nlay)
      real(kind=rb), intent(out) :: ciwpmc(:,:)       ! in-cloud ice water path [mcica]
                                                      !    Dimensions: (ngptlw,nlay)
      real(kind=rb), intent(out) :: clwpmc(:,:)       ! in-cloud liquid water path [mcica]
                                                      !    Dimensions: (ngptlw,nlay)
      real(kind=rb), intent(out) :: relqmc(:)         ! liquid particle effective radius (microns)
                                                      !    Dimensions: (nlay)
      real(kind=rb), intent(out) :: reicmc(:)         ! ice particle effective size (microns)
                                                      !    Dimensions: (nlay)
      real(kind=rb), intent(out) :: taucmc(:,:)       ! in-cloud optical depth [mcica]
                                                      !    Dimensions: (ngptlw,nlay)
      real(kind=rb), intent(out) :: taua(:,:)         ! aerosol optical depth
                                                      !    Dimensions: (nlay,nbndlw)


! ----- Local -----
      real(kind=rb), parameter :: amd = 28.9660_rb    ! Effective molecular weight of dry air (g/mol)
      real(kind=rb), parameter :: amw = 18.0160_rb    ! Molecular weight of water vapor (g/mol)
!      real(kind=rb), parameter :: amc = 44.0098_rb    ! Molecular weight of carbon dioxide (g/mol)
!      real(kind=rb), parameter :: amo = 47.9998_rb    ! Molecular weight of ozone (g/mol)
!      real(kind=rb), parameter :: amo2 = 31.9999_rb   ! Molecular weight of oxygen (g/mol)
!      real(kind=rb), parameter :: amch4 = 16.0430_rb  ! Molecular weight of methane (g/mol)
!      real(kind=rb), parameter :: amn2o = 44.0128_rb  ! Molecular weight of nitrous oxide (g/mol)
!      real(kind=rb), parameter :: amc11 = 137.3684_rb ! Molecular weight of CFC11 (g/mol) - CCL3F
!      real(kind=rb), parameter :: amc12 = 120.9138_rb ! Molecular weight of CFC12 (g/mol) - CCL2F2
!      real(kind=rb), parameter :: amc22 = 86.4688_rb  ! Molecular weight of CFC22 (g/mol) - CHCLF2
!      real(kind=rb), parameter :: amcl4 = 153.823_rb  ! Molecular weight of CCL4 (g/mol) - CCL4

! Set molecular weight ratios (for converting mmr to vmr)
!  e.g. h2ovmr = h2ommr * amdw)
      real(kind=rb), parameter :: amdw = 1.607793_rb  ! Molecular weight of dry air / water vapor
      real(kind=rb), parameter :: amdc = 0.658114_rb  ! Molecular weight of dry air / carbon dioxide
      real(kind=rb), parameter :: amdo = 0.603428_rb  ! Molecular weight of dry air / ozone
      real(kind=rb), parameter :: amdm = 1.805423_rb  ! Molecular weight of dry air / methane
      real(kind=rb), parameter :: amdn = 0.658090_rb  ! Molecular weight of dry air / nitrous oxide
      real(kind=rb), parameter :: amdo2 = 0.905140_rb ! Molecular weight of dry air / oxygen
      real(kind=rb), parameter :: amdc1 = 0.210852_rb ! Molecular weight of dry air / CFC11
      real(kind=rb), parameter :: amdc2 = 0.239546_rb ! Molecular weight of dry air / CFC12

      integer(kind=im) :: isp, l, ix, n, imol, ib, ig   ! Loop indices
      real(kind=rb) :: amm, amttl, wvttl, wvsh, summol  

! Add one to nlayers here to include extra model layer at top of atmosphere
      nlayers = nlay

!  Initialize all molecular amounts and cloud properties to zero here, then pass input amounts
!  into RRTM arrays below.

      wkl(:,:) = 0.0_rb
      wx(:,:) = 0.0_rb
      cldfmc(:,:) = 0.0_rb
      taucmc(:,:) = 0.0_rb
      ciwpmc(:,:) = 0.0_rb
      clwpmc(:,:) = 0.0_rb
      reicmc(:) = 0.0_rb
      relqmc(:) = 0.0_rb
      taua(:,:) = 0.0_rb
      amttl = 0.0_rb
      wvttl = 0.0_rb
 
!  Set surface temperature.
      tbound = tsfc(iplon)

!  Install input GCM arrays into RRTMG_LW arrays for pressure, temperature,
!  and molecular amounts.  
!  Pressures are input in mb, or are converted to mb here.
!  Molecular amounts are input in volume mixing ratio, or are converted from 
!  mass mixing ratio (or specific humidity for h2o) to volume mixing ratio
!  here. These are then converted to molecular amount (molec/cm2) below.  
!  The dry air column COLDRY (in molec/cm2) is calculated from the level 
!  pressures, pz (in mb), based on the hydrostatic equation and includes a 
!  correction to account for h2o in the layer.  The molecular weight of moist 
!  air (amm) is calculated for each layer.  
!  Note: In RRTMG, layer indexing goes from bottom to top, and coding below
!  assumes GCM input fields are also bottom to top. Input layer indexing
!  from GCM fields should be reversed here if necessary.

      pz(0) = plev(iplon,1)
      tz(0) = tlev(iplon,1)
      do l = 1, nlayers
         pavel(l) = play(iplon,l)
         tavel(l) = tlay(iplon,l)
         pz(l) = plev(iplon,l+1)
         tz(l) = tlev(iplon,l+1)
! For h2o input in vmr:
         wkl(1,l) = h2ovmr(iplon,l)
! For h2o input in mmr:
!         wkl(1,l) = h2o(iplon,l)*amdw
! For h2o input in specific humidity;
!         wkl(1,l) = (h2o(iplon,l)/(1._rb - h2o(iplon,l)))*amdw
         wkl(2,l) = co2vmr(iplon,l)
         wkl(3,l) = o3vmr(iplon,l)
         wkl(4,l) = n2ovmr(iplon,l)
         wkl(6,l) = ch4vmr(iplon,l)
         wkl(7,l) = o2vmr(iplon,l)
         amm = (1._rb - wkl(1,l)) * amd + wkl(1,l) * amw            
         coldry(l) = (pz(l-1)-pz(l)) * 1.e3_rb * avogad / &
                     (1.e2_rb * grav * amm * (1._rb + wkl(1,l)))
      enddo

! Set cross section molecule amounts from input; convert to vmr if necessary
      do l=1, nlayers
         wx(1,l) = ccl4vmr(iplon,l)
         wx(2,l) = cfc11vmr(iplon,l)
         wx(3,l) = cfc12vmr(iplon,l)
         wx(4,l) = cfc22vmr(iplon,l)
      enddo      

! The following section can be used to set values for an additional layer (from
! the GCM top level to 1.e-4 mb) for improved calculation of TOA fluxes. 
! Temperature and molecular amounts in the extra model layer are set to 
! their values in the top GCM model layer, though these can be modified
! here if necessary. 
! If this feature is utilized, increase nlayers by one above, limit the two
! loops above to (nlayers-1), and set the top most (extra) layer values here. 

!      pavel(nlayers) = 0.5_rb * pz(nlayers-1)
!      tavel(nlayers) = tavel(nlayers-1)
!      pz(nlayers) = 1.e-4_rb
!      tz(nlayers-1) = 0.5_rb * (tavel(nlayers)+tavel(nlayers-1))
!      tz(nlayers) = tz(nlayers-1)
!      wkl(1,nlayers) = wkl(1,nlayers-1)
!      wkl(2,nlayers) = wkl(2,nlayers-1)
!      wkl(3,nlayers) = wkl(3,nlayers-1)
!      wkl(4,nlayers) = wkl(4,nlayers-1)
!      wkl(6,nlayers) = wkl(6,nlayers-1)
!      wkl(7,nlayers) = wkl(7,nlayers-1)
!      amm = (1._rb - wkl(1,nlayers-1)) * amd + wkl(1,nlayers-1) * amw
!      coldry(nlayers) = (pz(nlayers-1)) * 1.e3_rb * avogad / &
!                        (1.e2_rb * grav * amm * (1._rb + wkl(1,nlayers-1)))
!      wx(1,nlayers) = wx(1,nlayers-1)
!      wx(2,nlayers) = wx(2,nlayers-1)
!      wx(3,nlayers) = wx(3,nlayers-1)
!      wx(4,nlayers) = wx(4,nlayers-1)

! At this point all molecular amounts in wkl and wx are in volume mixing ratio; 
! convert to molec/cm2 based on coldry for use in rrtm.  also, compute precipitable
! water vapor for diffusivity angle adjustments in rtrn and rtrnmr.

      do l = 1, nlayers
         summol = 0.0_rb
         do imol = 2, nmol
            summol = summol + wkl(imol,l)
         enddo
         wbrodl(l) = coldry(l) * (1._rb - summol)
         do imol = 1, nmol
            wkl(imol,l) = coldry(l) * wkl(imol,l)
         enddo
         amttl = amttl + coldry(l)+wkl(1,l)
         wvttl = wvttl + wkl(1,l)
         do ix = 1,maxxsec
            if (ixindx(ix) .ne. 0) then
               wx(ixindx(ix),l) = coldry(l) * wx(ix,l) * 1.e-20_rb
            endif
         enddo
      enddo

      wvsh = (amw * wvttl) / (amd * amttl)
      pwvcm = wvsh * (1.e3_rb * pz(0)) / (1.e2_rb * grav)

! Set spectral surface emissivity for each longwave band.  

      do n=1,nbndlw
         semiss(n) = emis(iplon,n)
!          semiss(n) = 1.0_rb
      enddo

! Transfer aerosol optical properties to RRTM variable;
! modify to reverse layer indexing here if necessary.

     if (iaer .ge. 1) then
        do l = 1, nlayers
           do ib = 1, nbndlw
              taua(l,ib) = tauaer(iplon,l,ib)
           enddo
        enddo
      endif

! Transfer cloud fraction and cloud optical properties to RRTM variables,
! modify to reverse layer indexing here if necessary.

      if (icld .ge. 1) then 
         inflag = inflglw
         iceflag = iceflglw
         liqflag = liqflglw

! Move incoming GCM cloud arrays to RRTMG cloud arrays.
! For GCM input, incoming reicmcl is defined based on selected ice parameterization (inflglw)

         do l = 1, nlayers
            do ig = 1, ngptlw
               cldfmc(ig,l) = cldfmcl(ig,iplon,l)
               taucmc(ig,l) = taucmcl(ig,iplon,l)
               ciwpmc(ig,l) = ciwpmcl(ig,iplon,l)
               clwpmc(ig,l) = clwpmcl(ig,iplon,l)
            enddo
            reicmc(l) = reicmcl(iplon,l)
            relqmc(l) = relqmcl(iplon,l)
         enddo

! If an extra layer is being used in RRTMG, set all cloud properties to zero in the extra layer.

!         cldfmc(:,nlayers) = 0.0_rb
!         taucmc(:,nlayers) = 0.0_rb
!         ciwpmc(:,nlayers) = 0.0_rb
!         clwpmc(:,nlayers) = 0.0_rb
!         reicmc(nlayers) = 0.0_rb
!         relqmc(nlayers) = 0.0_rb
!         taua(nlayers,:) = 0.0_rb

      endif
      
      end subroutine inatm

      end module rrtmg_lw_rad

!------------------------------------------------------------------
MODULE module_ra_rrtmg_lw


!MPAS specific (Laura D. Fowler):
    USE mpas_atmphys_o3climatology, ONLY: vinterp_ozn

!> add-ons and modifications to sourcecode:
!> ----------------------------------------
!>    * updated the sourcecode to WRF revision 3.5, except for the implementation
!>      of time-varying trace gases: added multiple layers above the model-top
!>      following Cavalo et al. (2010). added option to use the ozone climatology
!>      from the CAM radiation codes.
!>      Laura D. Fowler (birch.mmm.ucar.edu) / 2013-07-17.
!>    * in subroutine taugb7, corrected line number 6145 "if (specparm .lt. 0.125_rb) then"
!>      with if (specparm1 .lt. 0.125_rb) then, in accordance with the bug fix made in WRF
!>      revision 3.7.
!>      Laura D. Fowler (birch.mmm.ucar.edu) / 2015-05-04.

!MPAS specific end.







    USE parrrtm, ONLY: nbndlw, ngptlw
    USE rrtmg_lw_rad, ONLY: rrtmg_lw
    USE mcica_subcol_gen_lw, ONLY: mcica_subcol_lw

    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
    real retab(95)
    !
    SAVE retab
    ! For buffer layer adjustment.  Steven Cavallo, Dec 2010.
    integer , save    :: nlayers    
    real, PARAMETER :: deltap = 4.  ! Pressure interval for buffer layer in mb
    
    PUBLIC kr_externs_in_module_ra_rrtmg_lw
    PUBLIC kr_externs_out_module_ra_rrtmg_lw
    INTEGER :: kgenref_nlayers
    PUBLIC kv_externs_module_ra_rrtmg_lw
CONTAINS

!------------------------------------------------------------------
   SUBROUTINE RRTMG_LWRAD(                                        &
                       rthratenlw,                                &
                       lwupt, lwuptc, lwdnt, lwdntc,              &
                       lwupb, lwupbc, lwdnb, lwdnbc,              &
!                      lwupflx, lwupflxc, lwdnflx, lwdnflxc,      &
                       glw, olr, lwcf, emiss,                     &
                       p8w, p3d, pi3d,                            &
                       dz8w, tsk, t3d, t8w, rho3d, r, g,          &
                       icloud, warm_rain, cldfra3d,               &
                       f_ice_phy, f_rain_phy,                     &
                       xland, xice, snow,                         &
                       qv3d, qc3d, qr3d,                          &
                       qi3d, qs3d, qg3d,                          &
                       o3input, o33d,                             &
                       f_qv, f_qc, f_qr, f_qi, f_qs, f_qg,        &
                       tauaerlw1,tauaerlw2,tauaerlw3,tauaerlw4,   & ! czhao 
                       tauaerlw5,tauaerlw6,tauaerlw7,tauaerlw8,   & ! czhao 
                       tauaerlw9,tauaerlw10,tauaerlw11,tauaerlw12,   & ! czhao 
                       tauaerlw13,tauaerlw14,tauaerlw15,tauaerlw16,   & ! czhao 
                       aer_ra_feedback,                           & !czhao
!jdfcz                 progn,prescribe,                           & !czhao
                       progn,                                     & !czhao
                       qndrop3d,f_qndrop,                         & !czhao
                       ids,ide, jds,jde, kds,kde,                 & 
                       ims,ime, jms,jme, kms,kme,                 &
                       its,ite, jts,jte, kts,kte,                 &
                       lwupflx, lwupflxc, lwdnflx, lwdnflxc       &

                      ,noznlevels,pin,o3clim                      &

                                                                  )
!                      lwupflx, lwupflxc, lwdnflx, lwdnflxc,      &
!jdfcz                 progn,prescribe,                           & !czhao
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   LOGICAL, INTENT(IN )      ::        warm_rain
!
   INTEGER, INTENT(IN )      ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN )      ::        ICLOUD
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(IN   ) ::                                   dz8w, &
                                                             t3d, &
                                                             t8w, &
                                                             p8w, &
                                                             p3d, &
                                                            pi3d, &
                                                           rho3d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(INOUT)  ::                            RTHRATENLW

   REAL, DIMENSION( ims:ime, jms:jme )                          , &
         INTENT(INOUT)  ::                                   GLW, &
                                                             OLR, &
                                                            LWCF

   REAL, DIMENSION( ims:ime, jms:jme )                          , &
         INTENT(IN   )  ::                                 EMISS, &
                                                             TSK

   REAL, INTENT(IN  )   ::                                   R,G

   REAL, DIMENSION( ims:ime, jms:jme )                          , &
         INTENT(IN   )  ::                                 XLAND, &
                                                            XICE, &
                                                            SNOW
!
! Optional
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) ::                                         &
                                                        CLDFRA3D, &
                                                            QV3D, &
                                                            QC3D, &
                                                            QR3D, &
                                                            QI3D, &
                                                            QS3D, &
                                                            QG3D, &
                                                        QNDROP3D
   real pi,third,relconst,lwpmin,rhoh2o

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) ::                                         &
                                                       F_ICE_PHY, &
                                                      F_RAIN_PHY

   LOGICAL, OPTIONAL, INTENT(IN)   ::                             &
                                   F_QV,F_QC,F_QR,F_QI,F_QS,F_QG,F_QNDROP
! Optional
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN    ) :: tauaerlw1,tauaerlw2,tauaerlw3,tauaerlw4, & ! czhao 
                           tauaerlw5,tauaerlw6,tauaerlw7,tauaerlw8, & ! czhao 
                           tauaerlw9,tauaerlw10,tauaerlw11,tauaerlw12, & ! czhao 
                           tauaerlw13,tauaerlw14,tauaerlw15,tauaerlw16

   INTEGER,    INTENT(IN  ), OPTIONAL   ::       aer_ra_feedback
!jdfcz   INTEGER,    INTENT(IN  ), OPTIONAL   ::       progn,prescribe
   INTEGER,    INTENT(IN  ), OPTIONAL   ::       progn
!  Ozone
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) :: O33D
   INTEGER, OPTIONAL, INTENT(IN ) :: o3input

      real, parameter :: thresh=1.e-9
      real slope
      character(len=200) :: msg


! Top of atmosphere and surface longwave fluxes (W m-2)
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL, INTENT(INOUT) ::                               &
                                       LWUPT,LWUPTC,LWDNT,LWDNTC, &
                                       LWUPB,LWUPBC,LWDNB,LWDNBC

! Layer longwave fluxes (including extra layer above model top)
! Vertical ordering is from bottom to top (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(OUT) ::                                 &
                               LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC

!  LOCAL VARS
 
   REAL, DIMENSION( kts:kte+1 ) ::                          Pw1D, &
                                                            Tw1D

   REAL, DIMENSION( kts:kte ) ::                          TTEN1D, &
                                                        CLDFRA1D, &
                                                            DZ1D, &
                                                             P1D, &
                                                             T1D, &
                                                            QV1D, &
                                                            QC1D, &
                                                            QR1D, &
                                                            QI1D, &
                                                            QS1D, &
                                                            QG1D, &
                                                            O31D, &
                                                          qndrop1d 

! Added local arrays for RRTMG
    integer ::                                              ncol, &
                                                            nlay, &
                                                            icld, &
                                                         inflglw, &
                                                        iceflglw, &
                                                        liqflglw


!MPAS specific (Laura D. Fowler - 2013-03-15): Letting the arrays below to be
!allocatable allows the number of layers added above the model-top to vary as
!function of the grid-point. This is needed because the MPAS vertical coordi-
!nate system is a height coordinate system, and model-tops vary between grid-
!points:
 integer,dimension(ims:ime,jms:jme):: mpas_nlay
 real,dimension(:),allocatable:: o3mmr,varint
 real,dimension(:,:),allocatable:: &
    plev,tlev,play,tlay,h2ovmr,o3vmr,co2vmr,o2vmr,ch4vmr,n2ovmr,cfc11vmr,  &
    cfc12vmr,cfc22vmr,ccl4vmr,clwpth,ciwpth,rel,rei,cldfrac,relqmcl,reicmcl
 real,dimension(:,:),allocatable:: &
    uflx,dflx,uflxc,dflxc,hr,hrc
 real,dimension(:,:,:),allocatable:: &
    taucld,cldfmcl,clwpmcl,ciwpmcl,taucmcl,tauaer
!For old cloud property specification for rrtm_lw
 real,dimension(kts:kte):: clwp,ciwp,plwp,piwp
!Surface emissivity (for 16 LW spectral bands)
 real,dimension(1,nbndlw):: emis

!MPAS specific (Laura D. Fowler - 2013-04-16): Added the CAM ozone climatology.
!input arguments:
 integer,intent(in):: noznlevels
 real,intent(in),dimension(1:noznlevels),optional:: pin
 real,intent(in),dimension(ims:ime,1:noznlevels,jms:jme),optional:: o3clim
!local variables:
 real,dimension(1:noznlevels):: o3clim1d
!end MPAS specific.


    real, dimension ( 1 ) ::                                tsfc, &
                                                              ps
    real ::                                                   ro, &
                                                              dz


! Set trace gas volume mixing ratios, 2005 values, IPCC (2007)
! carbon dioxide (379 ppmv)
    real :: co2
    data co2 / 379.e-6 / 
! methane (1774 ppbv)
    real :: ch4
    data ch4 / 1774.e-9 / 
! nitrous oxide (319 ppbv)
    real :: n2o
    data n2o / 319.e-9 / 
! cfc-11 (251 ppt)
    real :: cfc11
    data cfc11 / 0.251e-9 / 
! cfc-12 (538 ppt)
    real :: cfc12
    data cfc12 / 0.538e-9 / 
! cfc-22 (169 ppt)
    real :: cfc22
    data cfc22 / 0.169e-9 / 
! ccl4 (93 ppt)
    real :: ccl4
    data ccl4 / 0.093e-9 / 
! Set oxygen volume mixing ratio (for o2mmr=0.23143)
    real :: o2
    data o2 / 0.209488 /

    integer :: iplon, irng, permuteseed
    integer :: nb

! For old cloud property specification for rrtm_lw
! Cloud and precipitation absorption coefficients
    real :: abcw,abice,abrn,absn
    data abcw /0.144/
    data abice /0.0735/
    data abrn /0.330e-3/
    data absn /2.34e-3/

! Molecular weights and ratios for converting mmr to vmr units
!    real :: amd       ! Effective molecular weight of dry air (g/mol)  
!    real :: amw       ! Molecular weight of water vapor (g/mol)        
!    real :: amo       ! Molecular weight of ozone (g/mol)              
!    real :: amo2      ! Molecular weight of oxygen (g/mol)              
! Atomic weights for conversion from mass to volume mixing ratios                
!    data amd   /  28.9660   /                                                  
!    data amw   /  18.0160   /                                                  
!    data amo   /  47.9998   /                                                  
!    data amo2  /  31.9999   /
                                                                                 
    real :: amdw     ! Molecular weight of dry air / water vapor  
    real :: amdo     ! Molecular weight of dry air / ozone
    real :: amdo2    ! Molecular weight of dry air / oxygen
    data amdw /  1.607793 /                                                    
    data amdo /  0.603461 /
    data amdo2 / 0.905190 /
    
!!
    real, dimension( 1, 1:kte-kts+1 )  :: pdel         ! Layer pressure thickness (mb)

    real, dimension(1, 1:kte-kts+1) ::   cicewp, &     ! in-cloud cloud ice water path
                                         cliqwp, &     ! in-cloud cloud liquid water path
                                          reliq, &     ! effective drop radius (microns)
                                          reice        ! ice effective drop size (microns)
    real :: gliqwp, gicewp, gravmks

!
!    REAL   ::  TSFC,GLW0,OLR0,EMISS0,FP

    real, dimension (1) :: landfrac, landm, snowh, icefrac

    integer :: pcols, pver

!
    INTEGER :: i,j,K
    LOGICAL :: predicate

! Added for top of model adjustment.  Steven Cavallo NCAR/MMM December 2010
    INTEGER, PARAMETER :: nproflevs = 60 ! Constant, from the table
    INTEGER :: L, LL, klev               ! Loop indices      



    REAL :: wght,vark,vark1       
    REAL :: PPROF(nproflevs), TPROF(nproflevs)            
    ! Weighted mean pressure and temperature profiles from midlatitude 
    ! summer (MLS),midlatitude winter (MLW), sub-Arctic 
    ! winter (SAW),sub-Arctic summer (SAS), and tropical (TROP) 
    ! standard atmospheres.
    DATA PPROF   /1000.00,855.47,731.82,626.05,535.57,458.16,     &
                  391.94,335.29,286.83,245.38,209.91,179.57,      &
                  153.62,131.41,112.42,96.17,82.27,70.38,         &
                  60.21,51.51,44.06,37.69,32.25,27.59,            &
                  23.60,20.19,17.27,14.77,12.64,10.81,            &
                  9.25,7.91,6.77,5.79,4.95,4.24,                  &
                  3.63,3.10,2.65,2.27,1.94,1.66,                  &
                  1.42,1.22,1.04,0.89,0.76,0.65,                  &
                  0.56,0.48,0.41,0.35,0.30,0.26,                  &
                  0.22,0.19,0.16,0.14,0.12,0.10/
    DATA TPROF   /286.96,281.07,275.16,268.11,260.56,253.02,      &
         	  245.62,238.41,231.57,225.91,221.72,217.79,      &
		  215.06,212.74,210.25,210.16,210.69,212.14,      &
		  213.74,215.37,216.82,217.94,219.03,220.18,      &
		  221.37,222.64,224.16,225.88,227.63,229.51,      &
		  231.50,233.73,236.18,238.78,241.60,244.44,      &
		  247.35,250.33,253.32,256.30,259.22,262.12,      &
		  264.80,266.50,267.59,268.44,268.69,267.76,      &
		  266.13,263.96,261.54,258.93,256.15,253.23,      &
		  249.89,246.67,243.48,240.25,236.66,233.86/	
!------------------------------------------------------------------

!-----CALCULATE LONG WAVE RADIATION
!                                                              
! All fields are ordered vertically from bottom to top
! Pressures are in mb

! latitude loop
  j_loop: do j = jts,jte

! longitude loop
     i_loop: do i = its,ite

         do k=kts,kte+1
            Pw1D(K) = p8w(I,K,J)/100.
            Tw1D(K) = t8w(I,K,J)
         enddo

         DO K=kts,kte
            QV1D(K)=0.
            QC1D(K)=0.
            QR1D(K)=0.
            QI1D(K)=0.
            QS1D(K)=0.
            CLDFRA1D(k)=0.
         ENDDO

         DO K=kts,kte
            QV1D(K)=QV3D(I,K,J)
            QV1D(K)=max(0.,QV1D(K))
            IF ( PRESENT( O33D ) ) THEN
            O31D(K)=O33D(I,K,J)
            ENDIF
         ENDDO

         DO K=kts,kte
            TTEN1D(K)=0.
            T1D(K)=T3D(I,K,J)
            P1D(K)=P3D(I,K,J)/100.
            DZ1D(K)=dz8w(I,K,J)
         ENDDO

! moist variables

         IF (ICLOUD .ne. 0) THEN
            IF ( PRESENT( CLDFRA3D ) ) THEN
              DO K=kts,kte
                 CLDFRA1D(k)=CLDFRA3D(I,K,J)
              ENDDO
            ENDIF

            IF (PRESENT(F_QC) .AND. PRESENT(QC3D)) THEN
              IF ( F_QC) THEN
                 DO K=kts,kte
                    QC1D(K)=QC3D(I,K,J)
                    QC1D(K)=max(0.,QC1D(K))
                 ENDDO
              ENDIF
            ENDIF

            IF (PRESENT(F_QR) .AND. PRESENT(QR3D)) THEN
              IF ( F_QR) THEN
                 DO K=kts,kte
                    QR1D(K)=QR3D(I,K,J)
                    QR1D(K)=max(0.,QR1D(K))
                 ENDDO
              ENDIF
            ENDIF

            IF ( PRESENT(F_QNDROP).AND.PRESENT(QNDROP3D)) THEN
             IF (F_QNDROP) THEN
              DO K=kts,kte
               qndrop1d(K)=qndrop3d(I,K,J)
              ENDDO
             ENDIF
            ENDIF

! This logic is tortured because cannot test F_QI unless
! it is present, and order of evaluation of expressions
! is not specified in Fortran

            IF ( PRESENT ( F_QI ) ) THEN
              predicate = F_QI
            ELSE
              predicate = .FALSE.
            ENDIF

! For MP option 3
            IF (.NOT. predicate .and. .not. warm_rain) THEN
               DO K=kts,kte
                  IF (T1D(K) .lt. 273.15) THEN
                  QI1D(K)=QC1D(K)
                  QS1D(K)=QR1D(K)
                  QC1D(K)=0.
                  QR1D(K)=0.
                  ENDIF
               ENDDO
            ENDIF

            IF (PRESENT(F_QI) .AND. PRESENT(QI3D)) THEN
               IF (F_QI) THEN
                  DO K=kts,kte
                     QI1D(K)=QI3D(I,K,J)
                     QI1D(K)=max(0.,QI1D(K))
                  ENDDO
               ENDIF
            ENDIF

            IF (PRESENT(F_QS) .AND. PRESENT(QS3D)) THEN
               IF (F_QS) THEN
                  DO K=kts,kte
                     QS1D(K)=QS3D(I,K,J)
                     QS1D(K)=max(0.,QS1D(K))
                  ENDDO
               ENDIF
            ENDIF

            IF (PRESENT(F_QG) .AND. PRESENT(QG3D)) THEN
               IF (F_QG) THEN
                  DO K=kts,kte
                     QG1D(K)=QG3D(I,K,J)
                     QG1D(K)=max(0.,QG1D(K))
                  ENDDO
               ENDIF
            ENDIF

! mji - For MP option 5
            IF ( PRESENT(F_QI) .and. PRESENT(F_QC) .and. PRESENT(F_QS) .and. PRESENT(F_ICE_PHY) ) THEN
               IF ( F_QC .and. .not. F_QI .and. F_QS ) THEN
                  DO K=kts,kte
                     qi1d(k) = qs3d(i,k,j)
                     qc1d(k) = qc3d(i,k,j)
                     qi1d(k) = max(0.,qi1d(k))
                     qc1d(k) = max(0.,qc1d(k))
                  ENDDO
               ENDIF
            ENDIF

        ENDIF

!         EMISS0=EMISS(I,J)
!         GLW0=0. 
!         OLR0=0. 
!         TSFC=TSK(I,J)
         DO K=kts,kte
            QV1D(K)=AMAX1(QV1D(K),1.E-12) 
         ENDDO

! Set up input for longwave
         ncol = 1
! Add extra layer from top of model to top of atmosphere
!         nlay = (kte - kts + 1) + 1
! Edited for top of model adjustment (nlayers = kte + 1).  


!MPAS specific (Laura D. Fowler - 2013-04-11):
!In contrast to WRF, MPAS columns have different model top presssures because MPAS uses a height
!coordinate system. Therefore, we define nlayers for each individual column:
          nlayers = kte + nint(pw1d(kte+1)/deltap)
          mpas_nlay(i,j) = nlayers-kte
!         write(0,101) j,i,kme,kte,nlayers,mpas_nlay(i,j),pw1d(kte+1)
!         101 format(6i9,1x,f9.4)
          if(.not.allocated(o3mmr)   ) allocate(o3mmr(kts:nlayers)     )
          if(.not.allocated(varint)  ) allocate(varint(kts:nlayers+1)  )
          if(.not.allocated(plev)    ) allocate(plev(1,kts:nlayers+1)  )
          if(.not.allocated(tlev)    ) allocate(tlev(1,kts:nlayers+1)  )
          if(.not.allocated(play)    ) allocate(play(1,kts:nlayers)    )
          if(.not.allocated(tlay)    ) allocate(tlay(1,kts:nlayers)    )
          if(.not.allocated(h2ovmr)  ) allocate(h2ovmr(1,kts:nlayers)  )
          if(.not.allocated(o3vmr)   ) allocate(o3vmr(1,kts:nlayers)   )
          if(.not.allocated(co2vmr)  ) allocate(co2vmr(1,kts:nlayers)  )
          if(.not.allocated(o2vmr)   ) allocate(o2vmr(1,kts:nlayers)   )
          if(.not.allocated(ch4vmr)  ) allocate(ch4vmr(1,kts:nlayers)  )
          if(.not.allocated(n2ovmr)  ) allocate(n2ovmr(1,kts:nlayers)  )
          if(.not.allocated(cfc11vmr)) allocate(cfc11vmr(1,kts:nlayers))
          if(.not.allocated(cfc12vmr)) allocate(cfc12vmr(1,kts:nlayers))
          if(.not.allocated(cfc22vmr)) allocate(cfc22vmr(1,kts:nlayers))
          if(.not.allocated(ccl4vmr) ) allocate(ccl4vmr(1,kts:nlayers) )
          if(.not.allocated(clwpth)  ) allocate(clwpth(1,kts:nlayers)  )
          if(.not.allocated(ciwpth)  ) allocate(ciwpth(1,kts:nlayers)  )
          if(.not.allocated(rel)     ) allocate(rel(1,kts:nlayers)     )
          if(.not.allocated(rei)     ) allocate(rei(1,kts:nlayers)     )
          if(.not.allocated(cldfrac) ) allocate(cldfrac(1,kts:nlayers) )
          if(.not.allocated(relqmcl) ) allocate(relqmcl(1,kts:nlayers) )
          if(.not.allocated(reicmcl) ) allocate(reicmcl(1,kts:nlayers) )
          if(.not.allocated(uflx)    ) allocate(uflx(1,kts:nlayers+1)  )
          if(.not.allocated(dflx)    ) allocate(dflx(1,kts:nlayers+1)  )
          if(.not.allocated(uflxc)   ) allocate(uflxc(1,kts:nlayers+1) )
          if(.not.allocated(dflxc)   ) allocate(dflxc(1,kts:nlayers+1) )
          if(.not.allocated(hr)      ) allocate(hr(1,kts:nlayers)      )
          if(.not.allocated(hrc)     ) allocate(hrc(1,kts:nlayers)     )
          if(.not.allocated(taucld)  ) allocate(taucld(nbndlw,1,kts:nlayers) )
          if(.not.allocated(cldfmcl) ) allocate(cldfmcl(ngptlw,1,kts:nlayers))         
          if(.not.allocated(clwpmcl) ) allocate(clwpmcl(ngptlw,1,kts:nlayers))
          if(.not.allocated(ciwpmcl) ) allocate(ciwpmcl(ngptlw,1,kts:nlayers))
          if(.not.allocated(taucmcl) ) allocate(taucmcl(ngptlw,1,kts:nlayers))
          if(.not.allocated(tauaer)  ) allocate(tauaer(1,kts:nlayers,nbndlw) )


! Steven Cavallo, December 2010
          nlay = nlayers ! Keep these indices the same


! Select cloud liquid and ice optics parameterization options
! For passing in cloud optical properties directly:
!         icld = 2
!         inflglw = 0
!         iceflglw = 0
!         liqflglw = 0
! For passing in cloud physical properties; cloud optics parameterized in RRTMG:
         icld = 2
         inflglw = 2
         iceflglw = 3
         liqflglw = 1

! Layer indexing goes bottom to top here for all fields.
! Water vapor and ozone are converted from mmr to vmr. 
! Pressures are in units of mb here. 
         plev(ncol,1) = pw1d(1)
         tlev(ncol,1) = tw1d(1)
         tsfc(ncol) = tsk(i,j)
         do k = kts, kte
            play(ncol,k) = p1d(k)
            plev(ncol,k+1) = pw1d(k+1)
            pdel(ncol,k) = plev(ncol,k) - plev(ncol,k+1)
            tlay(ncol,k) = t1d(k)
            tlev(ncol,k+1) = tw1d(k+1)
            h2ovmr(ncol,k) = qv1d(k) * amdw
            co2vmr(ncol,k) = co2
            o2vmr(ncol,k) = o2
            ch4vmr(ncol,k) = ch4
            n2ovmr(ncol,k) = n2o
            cfc11vmr(ncol,k) = cfc11
            cfc12vmr(ncol,k) = cfc12
            cfc22vmr(ncol,k) = cfc22
            ccl4vmr(ncol,k) = ccl4
         enddo

! This section is replaced with a new method to deal with model top
         if ( 1 == 0 ) then

!  Define profile values for extra layer from model top to top of atmosphere. 
!  The top layer temperature for all gridpoints is set to the top layer-1 
!  temperature plus a constant (0 K) that represents an isothermal layer    
!  above ptop.  Top layer interface temperatures are linearly interpolated 
!  from the layer temperatures.  

         play(ncol,kte+1) = 0.5 * plev(ncol,kte+1)
         tlay(ncol,kte+1) = tlev(ncol,kte+1) + 0.0
         plev(ncol,kte+2) = 1.0e-5
         tlev(ncol,kte+2) = tlev(ncol,kte+1) + 0.0
         h2ovmr(ncol,kte+1) = h2ovmr(ncol,kte) 
         co2vmr(ncol,kte+1) = co2vmr(ncol,kte) 
         o2vmr(ncol,kte+1) = o2vmr(ncol,kte) 
         ch4vmr(ncol,kte+1) = ch4vmr(ncol,kte) 
         n2ovmr(ncol,kte+1) = n2ovmr(ncol,kte) 
         cfc11vmr(ncol,kte+1) = cfc11vmr(ncol,kte) 
         cfc12vmr(ncol,kte+1) = cfc12vmr(ncol,kte) 
         cfc22vmr(ncol,kte+1) = cfc22vmr(ncol,kte) 
         ccl4vmr(ncol,kte+1) = ccl4vmr(ncol,kte) 

         endif

!  Set up values for extra layers to the top of the atmosphere.                       
!  Temperature is calculated based on an average temperature profile given
!  here in a table.  The input table data is linearly interpolated to the
!  column pressure.  Mixing ratios are held constant except for ozone.  
!  Caution should be used if model top pressure is less than 5 hPa.
!  Steven Cavallo, NCAR/MMM, December 2010
       ! Calculate the column pressure buffer levels above the 
       ! model top       
       do L=kte+1,nlayers,1
          plev(ncol,L+1) = plev(ncol,L) - deltap
          play(ncol,L) = 0.5*(plev(ncol,L) + plev(ncol,L+1))
       enddo          

       ! Add zero as top level.  This gets the temperature max at the
       ! stratopause, reducing the downward flux errors in the top 
       ! levels.  If zero happened to be the top level already,
       ! this will add another level with zero, but will not affect
       ! the radiative transfer calculation.
       plev(ncol,nlayers+1) = 0.00
       play(ncol,nlayers) =  0.5*(plev(ncol,nlayers) + plev(ncol,nlayers+1))

       ! Interpolate the table temperatures to column pressure levels    
       do L=1,nlayers+1,1
          if ( PPROF(nproflevs) .lt. plev(ncol,L) ) then
             do LL=2,nproflevs,1       
                if ( PPROF(LL) .lt. plev(ncol,L) ) then           
                   klev = LL - 1
                   exit
                endif
             enddo
          
          else
             klev = nproflevs
          endif  
  
          if (klev .ne. nproflevs ) then
             vark  = TPROF(klev) 
             vark1 = TPROF(klev+1)
             wght=(plev(ncol,L)-PPROF(klev) )/( PPROF(klev+1)-PPROF(klev))
          else
             vark  = TPROF(klev) 
             vark1 = TPROF(klev)
             wght = 0.0
          endif
          varint(L) = wght*(vark1-vark)+vark

       enddo                   
       
       ! Match the interpolated table temperature profile to WRF column
       do L=kte+1,nlayers+1,1
          tlev(ncol,L) = varint(L) + (tlev(ncol,kte) - varint(kte))
          !if ( L .le. nlay ) then
          tlay(ncol,L-1) = 0.5*(tlev(ncol,L) + tlev(ncol,L-1))
          !endif
       enddo

       ! Now the chemical species (except for ozone)
       do L=kte+1,nlayers,1
          h2ovmr(ncol,L) = h2ovmr(ncol,kte) 
          co2vmr(ncol,L) = co2vmr(ncol,kte) 
          o2vmr(ncol,L) = o2vmr(ncol,kte) 
          ch4vmr(ncol,L) = ch4vmr(ncol,kte) 
          n2ovmr(ncol,L) = n2ovmr(ncol,kte) 
          cfc11vmr(ncol,L) = cfc11vmr(ncol,kte) 
          cfc12vmr(ncol,L) = cfc12vmr(ncol,kte) 
          cfc22vmr(ncol,L) = cfc22vmr(ncol,kte) 
          ccl4vmr(ncol,L) = ccl4vmr(ncol,kte) 
       enddo	 
! End top of model buffer 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
! Get ozone profile including amount in extra layer above model top.
! Steven Cavallo: Must pass nlay-1 into subroutine to get nlayers 
! dimension for o3mmr
         call inirad (o3mmr,plev,kts,nlay-1)


! Laura D. Fowler (2013-04-16): Added the option to use the ozone climatology from the
! CAM radiation codes, instead of the annual mean ozone in subroutine o3dat. As output
! to the subroutine vinterp_ozn, the unit of o3mmr is actually "volume mixing ratio".
       if(o3input .eq. 2) then
          do k = 1, noznlevels
             o3clim1d(k) = o3clim(i,k,j)
          enddo
          call vinterp_ozn(1,ncol,ncol,nlayers,play,pin,noznlevels,o3clim1d,o3mmr)
          do k = kts,nlayers
             o3vmr(ncol,k) = o3mmr(k)
          enddo
       else
          do k = kts,nlayers
             o3vmr(ncol,k) = o3mmr(k)*amdo
          enddo
       endif

! Set surface emissivity in each RRTMG longwave band
         do nb = 1, nbndlw
            emis(ncol, nb) = emiss(i,j)
         enddo

! Define cloud optical properties for radiation (inflglw = 0)
! This is approach used with older RRTM_LW;
! Cloud and precipitation paths in g/m2 
! qi=0 if no ice phase
! qs=0 if no ice phase
         if (inflglw .eq. 0) then
            do k = kts,kte
               ro = p1d(k) / (r * t1d(k))*100. 
               dz = dz1d(k)
               clwp(k) = ro*qc1d(k)*dz*1000.         
               ciwp(k) = ro*qi1d(k)*dz*1000.         
               plwp(k) = (ro*qr1d(k))**0.75*dz*1000. 
               piwp(k) = (ro*qs1d(k))**0.75*dz*1000. 
            enddo

! Cloud fraction and cloud optical depth; old approach used with RRTM_LW
            do k = kts, kte
               cldfrac(ncol,k) = cldfra1d(k)
               do nb = 1, nbndlw
                  taucld(nb,ncol,k) = abcw*clwp(k) + abice*ciwp(k) & 
                            +abrn*plwp(k) + absn*piwp(k) 
                  if (taucld(nb,ncol,k) .gt. 0.01) cldfrac(ncol,k) = 1. 
               enddo
            enddo

! Zero out cloud physical property arrays; not used when passing optical properties
! into radiation
            do k = kts, kte
               clwpth(ncol,k) = 0.0
               ciwpth(ncol,k) = 0.0
               rel(ncol,k) = 10.0
               rei(ncol,k) = 10.0
            enddo
         endif

! Define cloud physical properties for radiation (inflglw = 1 or 2)
! Cloud fraction
! Set cloud arrays if passing cloud physical properties into radiation
         if (inflglw .gt. 0) then 
            do k = kts, kte
               cldfrac(ncol,k) = cldfra1d(k)
            enddo

! Compute cloud water/ice paths and particle sizes for input to radiation (CAM method)
            pcols = ncol
            pver = kte - kts + 1
            gravmks = g
            landfrac(ncol) = 2.-XLAND(I,J)
            landm(ncol) = landfrac(ncol)
            snowh(ncol) = 0.001*SNOW(I,J)
            icefrac(ncol) = XICE(I,J)

! From module_ra_cam: Convert liquid and ice mixing ratios to water paths;
! pdel is in mb here; convert back to Pa (*100.)
! Water paths are in units of g/m2
! snow added as ice cloud (JD 091022)
            do k = kts, kte
               gicewp = (qi1d(k)+qs1d(k)) * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box ice water path.
               gliqwp = qc1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box liquid water path.
               cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))               ! In-cloud ice water path.
               cliqwp(ncol,k) = gliqwp / max(0.01,cldfrac(ncol,k))               ! In-cloud liquid water path.
            end do

!link the aerosol feedback to cloud  -czhao
  if( PRESENT( progn ) ) then
    if (progn == 1) then
!jdfcz     if(prescribe==0) then

      pi = 4.*atan(1.0)
      third=1./3.
      rhoh2o=1.e3
      relconst=3/(4.*pi*rhoh2o)
!     minimun liquid water path to calculate rel
!     corresponds to optical depth of 1.e-3 for radius 4 microns.
      lwpmin=3.e-5
      do k = kts, kte
         reliq(ncol,k) = 10.
         if( PRESENT( F_QNDROP ) ) then
            if( F_QNDROP ) then
              if ( qc1d(k)*pdel(ncol,k).gt.lwpmin.and. &
                   qndrop1d(k).gt.1000. ) then
               reliq(ncol,k)=(relconst*qc1d(k)/qndrop1d(k))**third ! effective radius in m
!           apply scaling from Martin et al., JAS 51, 1830.
               reliq(ncol,k)=1.1*reliq(ncol,k)
               reliq(ncol,k)=reliq(ncol,k)*1.e6 ! convert from m to microns
               reliq(ncol,k)=max(reliq(ncol,k),4.)
               reliq(ncol,k)=min(reliq(ncol,k),20.)
              end if
            end if
         end if
      end do
!jdfcz     else ! prescribe 
! following Kiehl
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
!     write(0,*) 'lw prescribe aerosol',maxval(qndrop3d)
!jdfcz     endif
    else  ! progn   
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
    endif
  else   !present(progn) 
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
  endif

! following Kristjansson and Mitchell
            call reicalc(ncol, pcols, pver, tlay, reice)

! Limit upper bound of reice for Fu ice parameterization and convert
! from effective radius to generalized effective size (*1.0315; Fu, 1996)
            if (iceflglw .eq. 3) then
               do k = kts, kte
                  reice(ncol,k) = reice(ncol,k) * 1.0315
                  reice(ncol,k) = min(140.0,reice(ncol,k))
               end do
            endif

! Set cloud physical property arrays
            do k = kts, kte
               clwpth(ncol,k) = cliqwp(ncol,k)
               ciwpth(ncol,k) = cicewp(ncol,k)
               rel(ncol,k) = reliq(ncol,k)
               rei(ncol,k) = reice(ncol,k)
            enddo

! Zero out cloud optical properties here; not used when passing physical properties
! to radiation and taucld is calculated in radiation 
            do k = kts, kte
               do nb = 1, nbndlw
                  taucld(nb,ncol,k) = 0.0
               enddo
            enddo
         endif

! No clouds are allowed in the extra layer from model top to TOA
         ! Steven Cavallo: Edited out for buffer adjustment below
         if ( 1 == 0 ) then


         clwpth(ncol,kte+1) = 0.
         ciwpth(ncol,kte+1) = 0.
         rel(ncol,kte+1) = 10.
         rei(ncol,kte+1) = 10.
         cldfrac(ncol,kte+1) = 0.
         do nb = 1, nbndlw
            taucld(nb,ncol,kte+1) = 0.
         enddo

         endif

         ! Buffer adjustment. Steven Cavallo December 2010
	 do k=kte+1,nlayers
	    clwpth(ncol,k) = 0.
	    ciwpth(ncol,k) = 0.
	    rel(ncol,k) = 10.
            rei(ncol,k) = 10.
            cldfrac(ncol,k) = 0.
	    do nb = 1,nbndlw
               taucld(nb,ncol,k) = 0.
            enddo
	 enddo	 

         iplon = 1
         irng = 0
         permuteseed = 150

! Sub-column generator for McICA
         call mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play, &
                       cldfrac, ciwpth, clwpth, rei, rel, taucld, cldfmcl, &
                       ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)

!--------------------------------------------------------------------------
! Aerosol optical depth, single scattering albedo and asymmetry parameter -czhao 03/2010
!--------------------------------------------------------------------------
! Aerosol optical depth by layer for each RRTMG longwave band
! No aerosols in layer above model top (kte+1)
! Steven Cavallo: Upper bound of loop changed to nlayers from kte+1
!        do nb = 1, nbndlw
!           do k = kts, kte+1
!              tauaer(ncol,k,nb) = 0.
!           enddo
!        enddo

! ... Aerosol effects. Added aerosol feedbacks from Chem , 03/2010 -czhao
!
      do nb = 1, nbndlw
      do k = kts,nlayers
         tauaer(ncol,k,nb) = 0.
      end do
      end do


! Call RRTMG longwave radiation model
         call rrtmg_lw &
            (ncol    ,nlay    ,icld    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc    , & 
             h2ovmr  ,o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr  ,o2vmr , &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis    , &
             inflglw ,iceflglw,liqflglw,cldfmcl , &
             taucmcl ,ciwpmcl ,clwpmcl ,reicmcl ,relqmcl , &
             tauaer  , &
             uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc)

! Output downard surface flux, and outgoing longwave flux and cloud forcing 
! at the top of atmosphere (W/m2)
         glw(i,j) = dflx(1,1)
!         olr(i,j) = uflx(1,kte+2)
!         lwcf(i,j) = uflxc(1,kte+2) - uflx(1,kte+2)
! Steven Cavallo: Changed OLR to be valid at the top of atmosphere instead 
! of top of model.  Dec 2010.
         olr(i,j) = uflx(1,nlayers+1)
         lwcf(i,j) = uflxc(1,nlayers+1) - uflx(1,nlayers+1)

         if (present(lwupt)) then 
! Output up and down toa fluxes for total and clear sky
            lwupt(i,j)     = uflx(1,kte+2)
            lwuptc(i,j)    = uflxc(1,kte+2)
            lwdnt(i,j)     = dflx(1,kte+2)
            lwdntc(i,j)    = dflxc(1,kte+2)
! Output up and down surface fluxes for total and clear sky
            lwupb(i,j)     = uflx(1,1)
            lwupbc(i,j)    = uflxc(1,1)
            lwdnb(i,j)     = dflx(1,1)
            lwdnbc(i,j)    = dflxc(1,1)
         endif

! Output up and down layer fluxes for total and clear sky.
! Vertical ordering is from bottom to top in units of W m-2. 
         if ( present (lwupflx) ) then
         do k=kts,kte+2
            lwupflx(i,k,j)  = uflx(1,k)
            lwupflxc(i,k,j) = uflxc(1,k)
            lwdnflx(i,k,j)  = dflx(1,k)
            lwdnflxc(i,k,j) = dflxc(1,k)
         enddo
         endif

! Output heating rate tendency; convert heating rate from K/d to K/s
! Heating rate arrays are ordered vertically from bottom to top here. 
         do k=kts,kte
            tten1d(k) = hr(ncol,k)/86400.
            rthratenlw(i,k,j) = tten1d(k)/pi3d(i,k,j)
         enddo

!

!MPAS specific (Laura D. Fowler - 2013-04-11): deallocate all local arrays.
!coordinate system. Therefore, we define nlayers for each individual column:
          if(allocated(o3mmr)   ) deallocate(o3mmr   )
          if(allocated(varint)  ) deallocate(varint  )
          if(allocated(plev)    ) deallocate(plev    )
          if(allocated(tlev)    ) deallocate(tlev    )
          if(allocated(play)    ) deallocate(play    )
          if(allocated(tlay)    ) deallocate(tlay    )
          if(allocated(h2ovmr)  ) deallocate(h2ovmr  )
          if(allocated(o3vmr)   ) deallocate(o3vmr   )
          if(allocated(co2vmr)  ) deallocate(co2vmr  )
          if(allocated(o2vmr)   ) deallocate(o2vmr   )
          if(allocated(ch4vmr)  ) deallocate(ch4vmr  )
          if(allocated(n2ovmr)  ) deallocate(n2ovmr  )
          if(allocated(cfc11vmr)) deallocate(cfc11vmr)
          if(allocated(cfc12vmr)) deallocate(cfc12vmr)
          if(allocated(cfc22vmr)) deallocate(cfc22vmr)
          if(allocated(ccl4vmr) ) deallocate(ccl4vmr )
          if(allocated(clwpth)  ) deallocate(clwpth  )
          if(allocated(ciwpth)  ) deallocate(ciwpth  )
          if(allocated(rel)     ) deallocate(rel     )
          if(allocated(rei)     ) deallocate(rei     )
          if(allocated(cldfrac) ) deallocate(cldfrac )
          if(allocated(relqmcl) ) deallocate(relqmcl )
          if(allocated(reicmcl) ) deallocate(reicmcl )
          if(allocated(uflx)    ) deallocate(uflx    )
          if(allocated(dflx)    ) deallocate(dflx    )
          if(allocated(uflxc)   ) deallocate(uflxc   )
          if(allocated(dflxc)   ) deallocate(dflxc   )
          if(allocated(hr)      ) deallocate(hr      )
          if(allocated(hrc)     ) deallocate(hrc     )
          if(allocated(taucld)  ) deallocate(taucld  )
          if(allocated(cldfmcl) ) deallocate(cldfmcl )         
          if(allocated(clwpmcl) ) deallocate(clwpmcl )
          if(allocated(ciwpmcl) ) deallocate(ciwpmcl )
          if(allocated(taucmcl) ) deallocate(taucmcl )
          if(allocated(tauaer)  ) deallocate(tauaer  )


      end do i_loop
   end do j_loop                                           

!-------------------------------------------------------------------

   END SUBROUTINE RRTMG_LWRAD

 
!-------------------------------------------------------------------------
   SUBROUTINE INIRAD (O3PROF,Plev, kts, kte)
!-------------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------------
   INTEGER, INTENT(IN   )                        ::    kts,kte

   REAL, DIMENSION( kts:kte+1 ),INTENT(INOUT)    ::    O3PROF

   REAL, DIMENSION( kts:kte+2 ),INTENT(IN   )    ::      Plev

! LOCAL VAR
  
   INTEGER :: k

!                                                                                
!  COMPUTE OZONE MIXING RATIO DISTRIBUTION                                       
!                                                                                
   DO K=kts,kte+1
      O3PROF(K)=0.                                                       
   ENDDO
                                                                                 
   CALL O3DATA(O3PROF, Plev, kts, kte)

   END SUBROUTINE INIRAD
                                                                                 
!-------------------------------------------------------------------------
   SUBROUTINE O3DATA (O3PROF, Plev, kts, kte)
!-------------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------------
!
   INTEGER, INTENT(IN   )   ::       kts, kte
!
   REAL, DIMENSION( kts:kte+1 ),INTENT(INOUT)    ::    O3PROF

   REAL, DIMENSION( kts:kte+2 ),INTENT(IN   )    ::      Plev

! LOCAL VAR
   INTEGER :: K, JJ

   REAL    ::  PRLEVH(kts:kte+2),PPWRKH(32),                     &
               O3WRK(31),PPWRK(31),O3SUM(31),PPSUM(31),          &
               O3WIN(31),PPWIN(31),O3ANN(31),PPANN(31)                                                       

   REAL    ::  PB1, PB2, PT1, PT2

   DATA O3SUM  /5.297E-8,5.852E-8,6.579E-8,7.505E-8,             &                    
        8.577E-8,9.895E-8,1.175E-7,1.399E-7,1.677E-7,2.003E-7,   &                 
        2.571E-7,3.325E-7,4.438E-7,6.255E-7,8.168E-7,1.036E-6,   &                 
        1.366E-6,1.855E-6,2.514E-6,3.240E-6,4.033E-6,4.854E-6,   &                 
        5.517E-6,6.089E-6,6.689E-6,1.106E-5,1.462E-5,1.321E-5,   &                 
        9.856E-6,5.960E-6,5.960E-6/                                              

   DATA PPSUM  /955.890,850.532,754.599,667.742,589.841,         &  
        519.421,455.480,398.085,347.171,301.735,261.310,225.360, &               
        193.419,165.490,141.032,120.125,102.689, 87.829, 75.123, &            
         64.306, 55.086, 47.209, 40.535, 34.795, 29.865, 19.122, &               
          9.277,  4.660,  2.421,  1.294,  0.647/                                 
!                                                                                
   DATA O3WIN  /4.629E-8,4.686E-8,5.017E-8,5.613E-8,             &
        6.871E-8,8.751E-8,1.138E-7,1.516E-7,2.161E-7,3.264E-7,   &               
        4.968E-7,7.338E-7,1.017E-6,1.308E-6,1.625E-6,2.011E-6,   &               
        2.516E-6,3.130E-6,3.840E-6,4.703E-6,5.486E-6,6.289E-6,   &               
        6.993E-6,7.494E-6,8.197E-6,9.632E-6,1.113E-5,1.146E-5,   &               
        9.389E-6,6.135E-6,6.135E-6/                                              

   DATA PPWIN  /955.747,841.783,740.199,649.538,568.404,         &
        495.815,431.069,373.464,322.354,277.190,237.635,203.433, &               
        174.070,148.949,127.408,108.915, 93.114, 79.551, 67.940, &               
         58.072, 49.593, 42.318, 36.138, 30.907, 26.362, 16.423, &               
          7.583,  3.620,  1.807,  0.938,  0.469/                                 
!                                                                                

   DO K=1,31                                                              
     PPANN(K)=PPSUM(K)                                                        
   ENDDO
!
   O3ANN(1)=0.5*(O3SUM(1)+O3WIN(1))                                           
!                                                                                
   DO K=2,31                                                              
      O3ANN(K)=O3WIN(K-1)+(O3WIN(K)-O3WIN(K-1))/(PPWIN(K)-PPWIN(K-1))* & 
               (PPSUM(K)-PPWIN(K-1))                                           
   ENDDO
!
   DO K=2,31                                                              
      O3ANN(K)=0.5*(O3ANN(K)+O3SUM(K))                                         
   ENDDO
!
   DO K=1,31                                                                
      O3WRK(K)=O3ANN(K)                                                        
      PPWRK(K)=PPANN(K)                                                        
   ENDDO
!                                                                                
!  CALCULATE HALF PRESSURE LEVELS FOR MODEL AND DATA LEVELS                     
!                                                                                

! Plev is total P at model levels, from bottom to top
! Plev is in mb

   DO K=kts,kte+2
      PRLEVH(K)=Plev(K)
   ENDDO
!                                                                                
   PPWRKH(1)=1100.                                                        
   DO K=2,31                                                           
      PPWRKH(K)=(PPWRK(K)+PPWRK(K-1))/2.                                   
   ENDDO
   PPWRKH(32)=0.                                                          
   DO K=kts,kte+1
      DO 25 JJ=1,31                                                        
         IF((-(PRLEVH(K)-PPWRKH(JJ))).GE.0.)THEN                            
           PB1=0.                                                           
         ELSE                                                               
           PB1=PRLEVH(K)-PPWRKH(JJ)                                         
         ENDIF                                                              
         IF((-(PRLEVH(K)-PPWRKH(JJ+1))).GE.0.)THEN                          
           PB2=0.                                                           
         ELSE                                                               
           PB2=PRLEVH(K)-PPWRKH(JJ+1)                                       
         ENDIF                                                              
         IF((-(PRLEVH(K+1)-PPWRKH(JJ))).GE.0.)THEN                          
           PT1=0.                                                           
         ELSE                                                               
           PT1=PRLEVH(K+1)-PPWRKH(JJ)                                       
         ENDIF                                                              
         IF((-(PRLEVH(K+1)-PPWRKH(JJ+1))).GE.0.)THEN                        
           PT2=0.                                                           
         ELSE                                                               
           PT2=PRLEVH(K+1)-PPWRKH(JJ+1)                                     
         ENDIF                                                              
         O3PROF(K)=O3PROF(K)+(PB2-PB1-PT2+PT1)*O3WRK(JJ)                
  25  CONTINUE                                                             
      O3PROF(K)=O3PROF(K)/(PRLEVH(K)-PRLEVH(K+1))                      

   ENDDO
!                                                                                
   END SUBROUTINE O3DATA

!------------------------------------------------------------------

!LDF (2013-03-15): This section of the module is moved to module_physics_rrtmg_lwinit.F in
!./../core_physics to accomodate differences in the mpi calls between WRF and MPAS.I thought
!that it would be cleaner to do this instead of adding a lot of #ifdef statements throughout
!the initialization of the longwave radiation code. Initialization is handled the same way
!for the shortwave radiation code.

!ldf end (2013-03-15).

!===============================================================================
  subroutine relcalc(ncol, pcols, pver, t, landfrac, landm, icefrac, rel, snowh)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Compute cloud water size
! 
! Method: 
! analytic formula following the formulation originally developed by J. T. Kiehl
! 
! Author: Phil Rasch
! 
!-----------------------------------------------------------------------
    implicit none
!------------------------------Arguments--------------------------------
!
! Input arguments
!
    integer, intent(in) :: ncol
    integer, intent(in) :: pcols, pver
    real, intent(in) :: landfrac(pcols)      ! Land fraction
    real, intent(in) :: icefrac(pcols)       ! Ice fraction
    real, intent(in) :: snowh(pcols)         ! Snow depth over land, water equivalent (m)
    real, intent(in) :: landm(pcols)         ! Land fraction ramping to zero over ocean
    real, intent(in) :: t(pcols,pver)        ! Temperature

!
! Output arguments
!
    real, intent(out) :: rel(pcols,pver)      ! Liquid effective drop size (microns)
!
!---------------------------Local workspace-----------------------------
!
    integer i,k           ! Lon, lev indices
    real tmelt            ! freezing temperature of fresh water (K)
    real rliqland         ! liquid drop size if over land
    real rliqocean        ! liquid drop size if over ocean
    real rliqice          ! liquid drop size if over sea ice
!
!-----------------------------------------------------------------------
!
    tmelt = 273.16
    rliqocean = 14.0
    rliqice   = 14.0
    rliqland  = 8.0
    do k=1,pver
       do i=1,ncol
! jrm Reworked effective radius algorithm
          ! Start with temperature-dependent value appropriate for continental air
          ! Note: findmcnew has a pressure dependence here
          rel(i,k) = rliqland + (rliqocean-rliqland) * min(1.0,max(0.0,(tmelt-t(i,k))*0.05))
          ! Modify for snow depth over land
          rel(i,k) = rel(i,k) + (rliqocean-rel(i,k)) * min(1.0,max(0.0,snowh(i)*10.))
          ! Ramp between polluted value over land to clean value over ocean.
          rel(i,k) = rel(i,k) + (rliqocean-rel(i,k)) * min(1.0,max(0.0,1.0-landm(i)))
          ! Ramp between the resultant value and a sea ice value in the presence of ice.
          rel(i,k) = rel(i,k) + (rliqice-rel(i,k)) * min(1.0,max(0.0,icefrac(i)))
! end jrm
       end do
    end do
  end subroutine relcalc
!===============================================================================
  subroutine reicalc(ncol, pcols, pver, t, re)
    !

    integer, intent(in) :: ncol, pcols, pver
    real, intent(out) :: re(pcols,pver)
    real, intent(in) :: t(pcols,pver)
    real corr
    integer i
    integer k
    integer index
    !
    !       Tabulated values of re(T) in the temperature interval
    !       180 K -- 274 K; hexagonal columns assumed:
    !
    !
    do k=1,pver
       do i=1,ncol
          index = int(t(i,k)-179.)
          index = min(max(index,1),94)
          corr = t(i,k) - int(t(i,k))
          re(i,k) = retab(index)*(1.-corr)		&
               +retab(index+1)*corr
          !           re(i,k) = amax1(amin1(re(i,k),30.),10.)
       end do
    end do
    !
    return
  end subroutine reicalc
!------------------------------------------------------------------

  !read state subroutine for kr_externs_in_module_ra_rrtmg_lw
  SUBROUTINE kr_externs_in_module_ra_rrtmg_lw(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      LOGICAL :: kgen_istrue
      
      READ (UNIT = kgen_unit) retab
      READ (UNIT = kgen_unit) nlayers
  END SUBROUTINE kr_externs_in_module_ra_rrtmg_lw
  
  !read state subroutine for kr_externs_out_module_ra_rrtmg_lw
  SUBROUTINE kr_externs_out_module_ra_rrtmg_lw(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      
      LOGICAL :: kgen_istrue
      READ (UNIT = kgen_unit) kgenref_nlayers
  END SUBROUTINE kr_externs_out_module_ra_rrtmg_lw
  
  !verify state subroutine for kv_externs_module_ra_rrtmg_lw
  SUBROUTINE kv_externs_module_ra_rrtmg_lw(check_status)
      TYPE(check_t), INTENT(INOUT) :: check_status
      
      CALL kv_module_ra_rrtmg_lw_integer__("nlayers", check_status, nlayers, kgenref_nlayers)
  END SUBROUTINE kv_externs_module_ra_rrtmg_lw
  
  !verify state subroutine for kv_module_ra_rrtmg_lw_integer__
  RECURSIVE SUBROUTINE kv_module_ra_rrtmg_lw_integer__(varname, check_status, var, kgenref_var)
      CHARACTER(LEN=*), INTENT(IN) :: varname
      TYPE(check_t), INTENT(INOUT) :: check_status
      INTEGER, INTENT(IN) :: var, kgenref_var
      INTEGER :: check_result
      LOGICAL :: is_print = .FALSE.
      
      integer :: diff
      
      check_status%numTotal = check_status%numTotal + 1
      
      IF (var == kgenref_var) THEN
          check_status%numIdentical = check_status%numIdentical + 1
          WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
          check_result = CHECK_IDENTICAL
      ELSE
          diff = ABS(var - kgenref_var)
          IF (diff <= check_status%tolerance) THEN
              check_status%numInTol = check_status%numInTol + 1
              WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL within tolerance."
              check_result = CHECK_IN_TOL
          ELSE
              check_status%numOutTol = check_status%numOutTol + 1
              WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL out of tolerance."
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", 0
          END IF 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 0) THEN
              WRITE (*, *) "Difference is ", diff
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 1) THEN
              WRITE (*, *) "Difference is ", diff
          END IF 
      END IF 
      
  END SUBROUTINE kv_module_ra_rrtmg_lw_integer__
  
END MODULE module_ra_rrtmg_lw