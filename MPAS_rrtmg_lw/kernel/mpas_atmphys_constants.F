!KGEN-generated Fortran source file

!Generated at : 2015-12-24 11:55:35
!KGEN version : 0.6.0

! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://1-dev.github.com/license.html
!
!==================================================================================================
 module mpas_atmphys_constants
     USE mpas_constants, ONLY: gravity, r_d => rgas

     IMPLICIT NONE
     PUBLIC
     SAVE

 
!>\brief defines the constants needed for the physics parameterizations.
!>\author Laura D. Fowler (send comments to laura@ucar.edu).
!>\date 2013-05-01.
!>
!>\details
!>
!> add-ons and modifications to sourcecode:
!> ----------------------------------------
!>    * Replaced the variable g (that originally pointed to gravity) with gravity, for simplicity.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
!>    * Removed the constraint of only using RKIND from mpas_kind_types.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-09-18.
!>    * added empty subroutine atmphys_constants_init that does not do anything, but needed for 
!>      compiling MPAS with some compilers.
!>      Laura D. Fowler (laura@ucar.edu) / 2015-01-12.

 
!==================================================================================================






 


!constants specific to long- and short-wave radiation codes:
!real(kind=RKIND),parameter:: solcon_0 = 1365.          !solar constant                      [W/m2]


!==================================================================================================
!==================================================================================================
!dummy subroutine that does not do anything.


!==================================================================================================
 end module mpas_atmphys_constants
!==================================================================================================