!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:44 
!KGEN version : 0.7.3 
  
! This module contains constants and namelist variables used through out the model
! to avoid circular dependancies please do not 'use' any further modules here.


!
module control_mod
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

!  character(len=MAX_STRING_LEN)    , public :: integration    ! time integration (only one currently supported is "explicit")
  ! Tracer transport type

!shallow water advection tests:
!kmass points to a level with density.  other levels contain test tracers


                                                              ! 1 = RK (foward-in-time)

                                          ! every rsplit tracer timesteps

! vert_remap_q_alg:    0  default value, Zerroukat monotonic splines
!                      1  PPM vertical remap with mirroring at the boundaries
!                         (solid wall bc's, high-order throughout)
!                      2  PPM vertical remap without mirroring at the boundaries
!                         (no bc's enforced, first-order at two cells bordering top and bottom boundaries)


                                           !  0 = equi-angle Gnomonic (default)
                                           !  1 = equi-spaced Gnomonic (not yet coded)
                                           !  2 = element-local projection  (for var-res)
                                           !  3 = parametric (not yet coded)
!tolerance to define smth small, was introduced for lim 8 in 2d and 3d


                                                               ! (only used for variable viscosity, recommend 1.9 in namelist)
                                          ! 1 = use (approx.) laplace on p surfaces

!three types of hyper viscosity are supported right now:
! (1) const hv:    nu * del^2 del^2
! (2) scalar hv:   nu(lat,lon) * del^2 del^2
! (3) tensor hv,   nu * ( \div * tensor * \grad ) * del^2
! (1) default:  hypervis_power=0, hypervis_scaling=0
! (2) Original version for var-res grids. (M. Levy)
!            scalar coefficient within each element
!            hypervisc_scaling=0
!            set hypervis_power>0 and set fine_ne, max_hypervis_courant
! (3) tensor HV var-res grids
!            tensor within each element:
!            set hypervis_scaling > 0 (typical values would be 3.2 or 4.0)
!            hypervis_power=0
!            (\div * tensor * \grad) operator uses cartesian laplace

!
!
!


  integer, public, parameter :: west  = 1
  integer, public, parameter :: east  = 2
  integer, public, parameter :: south = 3
  integer, public, parameter :: north = 4

  integer, public, parameter :: swest = 5
  integer, public, parameter :: seast = 6
  integer, public, parameter :: nwest = 7
  integer, public, parameter :: neast = 8


end module control_mod