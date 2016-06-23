!KGEN-generated Fortran source file

!Generated at : 2015-12-29 15:06:50
!KGEN version : 0.6.0

! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!***********************************************************************
!
!  mpas_constants
!
!> \brief   MPAS Constant Module
!> \author  Michael Duda
!> \date    03/27/13
!> \details 
!> This module provides various constants that can be used in different parts of MPAS.
!> They may or may not be a physical quantity.
!
!-----------------------------------------------------------------------

module mpas_constants

    USE mpas_kind_types

   real (kind=RKIND), parameter :: gravity = 9.80616             !< Constant: Acceleration due to gravity [m s-2]
   real (kind=RKIND), parameter :: rgas    = 287.0               !< Constant: Gas constant for dry air [J kg-1 K-1]



!***********************************************************************
!
!  routine dummy
!
!> \brief   MPAS Dummy Routine
!> \author  Michael Duda
!> \date    03/27/13
!> \details 
!> This is a dummy routine that doesn't do anything.
!
!-----------------------------------------------------------------------


end module mpas_constants