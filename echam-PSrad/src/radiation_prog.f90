!
! Module with netcdf utilities for reading and writing specific files
!  Robert Pincus, University of Colorado, Oct 2011 (Robert.Pincus@zmaw.de) 
!
MODULE nc_utils
  USE mo_kind          , ONLY: wp
  USE netcdf
  IMPLICIT NONE
  
  INTERFACE read_var
    MODULE procedure read_var_2, read_var_3, read_var_4
  END INTERFACE read_var
  
  INTERFACE write_var
    MODULE procedure write_nc3, write_nc4
  END INTERFACE write_var

  PRIVATE
  PUBLIC :: nc_check, read_dims, read_var, write_var
CONTAINS
!-----------------------------------------------------------------------
  function get_time result(string)
    character (len = 15) :: string
    integer :: vtime(8)
    
    call date_and_time(values = vtime)
    write(string,'(I4.4,I2.2,I2.2," ",I2.2,I2.2,I2.2)') vtime(1),vtime(2),vtime(3),vtime(5),vtime(6),vtime(7)
  end function get_time
!-----------------------------------------------------------------------
  !Netcdf error check
  subroutine nc_check(status)
    integer, intent(in) :: status
  
    if(status /= nf90_noerr) then
        write(*,'(A)') nf90_strerror(status)
        stop
     endif
  end subroutine nc_check
!-----------------------------------------------------------------------
  SUBROUTINE read_dims(infile, lon, lat, lev, time)
    CHARACTER(LEN=*),       INTENT(IN) :: infile
    REAL(wp), DIMENSION(:), INTENT(OUT) :: lon, lat, time
    INTEGER,  DIMENSION(:), INTENT(OUT) :: lev 
    
    INTEGER :: ncid, ncvarid
    
    call nc_check(nf90_open(infile, nf90_nowrite, ncid))
    
    CALL nc_check(nf90_inq_varid(ncid, 'lon',  ncvarid)) 
    CALL nc_check(nf90_get_var  (ncid, ncvarid, lon)   ) 
    CALL nc_check(nf90_inq_varid(ncid, 'lat',  ncvarid)) 
    CALL nc_check(nf90_get_var  (ncid, ncvarid, lat)   ) 
    CALL nc_check(nf90_inq_varid(ncid, 'lev',  ncvarid)) 
    CALL nc_check(nf90_get_var  (ncid, ncvarid, lev)   ) 
    CALL nc_check(nf90_inq_varid(ncid, 'time', ncvarid)) 
    CALL nc_check(nf90_get_var  (ncid, ncvarid, time)  ) 

    call nc_check(nf90_close(ncid))
  END SUBROUTINE read_dims
!-----------------------------------------------------------------------
  SUBROUTINE read_var_2(infile, varname, var)
    CHARACTER(LEN=*),           INTENT(IN) :: infile, varname
    REAL(wp), DIMENSION(:,:),   INTENT(OUT) :: var
    
    INTEGER :: ncid, ncvarid
    
    call nc_check(nf90_open(infile, nf90_nowrite, ncid))
    
    CALL nc_check(nf90_inq_varid(ncid, trim(varname), ncvarid) ) 
    CALL nc_check(nf90_get_var  (ncid, ncvarid, var)           ) 

    call nc_check(nf90_close(ncid))
  END SUBROUTINE read_var_2
!-----------------------------------------------------------------------
  SUBROUTINE read_var_3(infile, varname, var)
    CHARACTER(LEN=*),           INTENT(IN) :: infile, varname
    REAL(wp), DIMENSION(:,:,:), INTENT(OUT) :: var
    
    INTEGER :: ncid, ncvarid
    
    call nc_check(nf90_open(infile, nf90_nowrite, ncid))
    
    CALL nc_check(nf90_inq_varid(ncid, trim(varname), ncvarid) ) 
    CALL nc_check(nf90_get_var  (ncid, ncvarid, var)           ) 

    call nc_check(nf90_close(ncid))
  END SUBROUTINE read_var_3
!-----------------------------------------------------------------------
  SUBROUTINE read_var_4(infile, varname, var)
    CHARACTER(LEN=*),             INTENT(IN) :: infile, varname
    REAL(wp), DIMENSION(:,:,:,:), INTENT(OUT) :: var
    
    INTEGER :: ncid, ncvarid
    
    call nc_check(nf90_open(infile, nf90_nowrite, ncid))
    
    CALL nc_check(nf90_inq_varid(ncid, trim(varname), ncvarid) ) 
    CALL nc_check(nf90_get_var  (ncid, ncvarid, var)           ) 

    call nc_check(nf90_close(ncid))
  END SUBROUTINE read_var_4
  
!-----------------------------------------------------------------------
  subroutine write_nc4 (outDir, nday, xlon, xlat, ilev, xtime, varname, var)
    CHARACTER(len=*),             INTENT(IN) :: outDir, varname
    REAL(wp),                     INTENT(IN) :: nday
    REAL(wp), DIMENSION(:),       INTENT(IN) :: xlon, xlat, xtime
    INTEGER,  DIMENSION(:),       INTENT(IN) :: ilev
    REAL(wp), DIMENSION(:,:,:,:), INTENT(IN) :: var

    INTEGER :: ncid, ncvarid, ncomode 
    INTEGER :: dimids(4) 

    call nc_check(nf90_create(trim(make_filename(outDir, varname, nday)), nf90_clobber, ncid))
    CALL prep_file_template(ncid, xlon, xlat, ilev, xtime, dimids)
    !
    ! Create variable
    !
    call nc_check(nf90_def_var(ncid, TRIM(varname), nf90_double, dimids, ncvarid))
    call nc_check(nf90_put_att(ncid, ncvarid, "long_name", varname))
    call nc_check(nf90_put_att(ncid, ncvarid, "unit",      "hPa"))
    call nc_check(nf90_put_att(ncid, ncvarid, "grid_type", "gaussian"))
    call nc_check(nf90_enddef(ncid))

    call nc_check(nf90_set_fill(ncid, nf90_nofill, ncomode))

    ! Write the variable
    call nc_check(nf90_put_var(ncid, ncvarid, var))
    call nc_check(nf90_close(ncid))

end subroutine write_nc4
!-----------------------------------------------------------------------
  subroutine write_nc3 (outDir, nday, xlon, xlat, xtime, varname, var)
    CHARACTER(len=*),           INTENT(IN) :: outDir, varname
    REAL(wp),                   INTENT(IN) :: nday
    REAL(wp), DIMENSION(:),     INTENT(IN) :: xlon, xlat, xtime
    REAL(wp), DIMENSION(:,:,:), INTENT(IN) :: var

    INTEGER :: ncid, ncvarid, ncomode 
    INTEGER :: dimids(3) 

    call nc_check(nf90_create(trim(make_filename(outDir, varname, nday)), nf90_clobber, ncid))
    CALL prep_file_template(ncid, xlon, xlat, xtime = xtime, dimids = dimids)
    
    !
    ! Create variable
    !
    call nc_check(nf90_def_var(ncid, TRIM(varname), nf90_double, dimids, ncvarid))
    call nc_check(nf90_put_att(ncid, ncvarid, "long_name", varname))
    call nc_check(nf90_put_att(ncid, ncvarid, "unit",      "hPa"))
    call nc_check(nf90_put_att(ncid, ncvarid, "grid_type", "gaussian"))
    call nc_check(nf90_enddef(ncid))

    call nc_check(nf90_set_fill(ncid, nf90_nofill, ncomode))

    ! Write the variable
    call nc_check(nf90_put_var(ncid, ncvarid, var))
    call nc_check(nf90_close(ncid))

end subroutine write_nc3

!-----------------------------------------------------------------------
  SUBROUTINE prep_file_template(ncid, xlon, xlat, ilev, xtime, dimids)
    INTEGER, INTENT(IN) :: ncid 
    REAL(wp), DIMENSION(:), INTENT(IN) :: xlon, xlat, xtime
    INTEGER,  DIMENSION(:), OPTIONAL, & 
                            INTENT(IN) :: ilev
    INTEGER, DIMENSION(:), INTENT(OUT) :: dimids

   character(len=256) 			:: nctitle = "", ncechamver = ""
   integer :: ncvarid, timedimid
   
   !Define global attributes
   call nc_check(nf90_put_att(ncid, nf90_global, "title",         nctitle))
   call nc_check(nf90_put_att(ncid, nf90_global, "echam_version", ncechamver))
   call nc_check(nf90_put_att(ncid, nf90_global, "date_time", get_time()))
   
   !Define grid
   call nc_check(nf90_def_dim(ncid, 'lon', SIZE(xlon),      dimids(1)))
   call nc_check(nf90_def_dim(ncid, 'lat', SIZE(xlat),      dimids(2)))
   call nc_check(nf90_def_dim(ncid, 'time', nf90_unlimited, dimids(3)))
   timedimid = dimids(3)
   IF(PRESENT(ilev)) THEN 
     dimids(4) = dimids(3)
     call nc_check(nf90_def_dim(ncid, 'lev', SIZE(ilev), dimids(3)))
   END IF 
   
   !Define variables
   call nc_check(nf90_def_var(ncid, 'lon', nf90_double, dimids(1), ncvarid))
   call nc_check(nf90_put_att(ncid, ncvarid, "long_name", "longitude"))
   call nc_check(nf90_put_att(ncid, ncvarid, "units",     "degrees_east"))

   call nc_check(nf90_def_var(ncid, 'lat', nf90_double, dimids(2), ncvarid))
   call nc_check(nf90_put_att(ncid, ncvarid, "long_name", "latitude"))
   call nc_check(nf90_put_att(ncid, ncvarid, "units",     "degrees_north"))

   IF(PRESENT(ilev)) THEN 
     call nc_check(nf90_def_var(ncid, 'lev', nf90_int, dimids(3), ncvarid))
     call nc_check(nf90_put_att(ncid, ncvarid, "long_name", "hybrid level at layer midpoints"))
     call nc_check(nf90_put_att(ncid, ncvarid, "units",     "level"))
   END IF 

   call nc_check(nf90_def_var(ncid, 'time', nf90_double, timedimid, ncvarid))
   call nc_check(nf90_put_att(ncid, ncvarid, "units", "1997-02-01 00:00"))
   call nc_check(nf90_put_att(ncid, ncvarid, "calendar", "gregorian"))
   
   !
   ! Write dimension variables but leave in define mode
   !
   call nc_check(nf90_enddef(ncid)) 
   
   call nc_check(nf90_inq_varid(ncid, "lon", ncvarid)) 
   call nc_check(nf90_put_var(ncid, ncvarid, xlon)) 
   call nc_check(nf90_inq_varid(ncid, "lat", ncvarid)) 
   call nc_check(nf90_put_var(ncid, ncvarid, xlat)) 
   IF(PRESENT(ilev)) THEN 
     call nc_check(nf90_inq_varid(ncid, "lev", ncvarid)) 
     call nc_check(nf90_put_var(ncid, ncvarid, ilev)) 
   END IF 
   call nc_check(nf90_inq_varid(ncid, "time", ncvarid)) 
   call nc_check(nf90_put_var(ncid, ncvarid, xtime)) 
   
   call nc_check(nf90_redef(ncid)) 
  END SUBROUTINE prep_file_template

!-----------------------------------------------------------------------

  FUNCTION make_filename(dirname, varname, nday) result(fname) 
    CHARACTER(LEN=*), INTENT(IN) :: dirname, varname
    REAL(wp),         INTENT(IN) :: nday
    CHARACTER(LEN=1024)          :: fname
 
    CHARACTER(len=3) :: dayAsChar
    
    write(dayAsChar, '(I3)') int(nday)
    dayAsChar = AdjustL(dayAsChar)

    fname = trim(dirname) // "/" // trim(varname) // "_" // trim(dayAsChar) // ".nc"
  END FUNCTION make_filename
END MODULE nc_utils

!-----------------------------------------------------------------------

PROGRAM radiation
  USE mo_kind          , ONLY: wp
  USE mo_math_constants, ONLY: pi
  USE mo_physical_constants &
                       , ONLY: amd,amco2,amch4,amo3,amn2o,rgrav,cpd,vtmpc2
  USE mo_exception,      ONLY: finish, message, message_text
  USE mo_control       , ONLY: control_nn=>nn, control_nlev=>nlev, control_nlevp1=>nlevp1, &
                               lcouple, lmidatm
  USE mo_io_units,       ONLY: nin
  USE mo_decomposition,  ONLY: grid_init
  USE mo_memory_g3b,     ONLY: construct_g3b, geosp
  USE mo_interpo,        ONLY: wgt1_m, wgt2_m, nmw1_m, nmw2_m
  USE mo_radiation_parameters, ONLY: ldiur, lradforcing,                          &
                                     l_interp_rad_in_time, zepzen,                &
                                     lyr_perp, yr_perp, nmonth, isolrad, nb_sw,   &
                                     lw_spec_samp, sw_spec_samp,                  & 
                                     lw_gpts_ts,   sw_gpts_ts,   rad_perm,        &
                                     l_lw_unique,  l_sw_unique,                   &
                                     l_do_sep_clear_sky, i_overlap, l_partition_h2o, &
                                     ih2o, ico2, ich4, io3, io2, in2o, icfc,      &
                                     ighg, fco2, nmonth, iaero,                   &
                                     co2vmr, ch4vmr, o2vmr, n2ovmr, cfcvmr,       &
                                     co2mmr, ch4mmr, o2mmr, n2ommr,               &
                                     ch4_v, n2o_v, cemiss, solc,                  &
                                     psct, psctm, ssi_factor,                     &
                                     flx_ratio_cur, flx_ratio_rad,                & 
                                     decl_sun_cur,solar_parameters, do_gpoint
  USE mo_psrad_interface,ONLY: setup_psrad, psrad_interface, lw_strat, sw_strat
  USE mo_srtm_setup,    ONLY : ssi_default, ssi_preind, ssi_amip
  USE mo_read_netcdf77, ONLY : read_diml_nf77
  USE nc_utils,          ONLY: nc_check, read_dims, read_var, write_var

use netcdf  
  IMPLICIT NONE
  INTEGER, PARAMETER    :: nnml = 22
  INTEGER, PARAMETER 	:: lat=96
  INTEGER, PARAMETER 	:: lon=192
  INTEGER, PARAMETER 	:: nlev=47
  INTEGER, PARAMETER 	:: naer=5
  INTEGER, PARAMETER 	:: nnewaer=0
  INTEGER, PARAMETER 	:: ncfc=2
  INTEGER, PARAMETER 	:: ntime=1
  INTEGER, PARAMETER    :: ntrac=1                  !LT
  INTEGER :: ktype(lon)=0                           !LT  < type of convection, not used
  REAL(wp) :: pgeom1(lon,nlev)=0._wp                !LT  < geopotential above ground, not used
  REAL(wp) :: pxtm1(lon,nlev,ntrac)=0._wp           !LT  < tracer mass mixing ratios, not used

  character(len=160):: infilelsm=("T63GR15_lsm.nc")   !land-sea mask
  character(len=160):: infileglc=("T63GR15_glc.nc")   !glacier mask

  REAL(wp), PARAMETER :: nan=-999.0

  LOGICAL  :: loland(lon,lat)
  LOGICAL  :: loglac(lon,lat)
  REAL(wp) :: lolandh(lon,lat)
  REAL(wp) :: loglach(lon,lat)
!dir$ attributes align: 64 :: loland,loglac,lolandh,loglach
  REAL(wp) :: vct_a(nlev+1), vct_b(nlev+1)  ! Interpolation coefficients 

  REAL(wp) :: mu0(lon,lat,ntime)    		! solar zenith angle
  REAL(wp) :: albedo(lon,lat,ntime)     	! SW albedo of surface
!dir$ attributes align: 64 :: mu0, albedo


!  REAL(wp) :: h0=7.            	  		! km, scale height
  REAL(wp) :: p0(lon,lat,ntime)
  REAL(wp) :: pf(lon,lat,nlev,ntime)    	! Pa, pressure
  REAL(wp) :: ph(lon,lat,nlev+1,ntime)  	! Pa, pressure at half levels
  REAL(wp) :: dpr(lon,lat,nlev,ntime)   	! Pa, pressure thickness
  REAL(wp) :: psrf(lon,lat,ntime)       	! Pa, pressure at the surface
!dir$ attributes align: 64 :: p0,pf,ph,dpr,psrf

  REAL(wp) :: tf(lon,lat,nlev,ntime)   		! K, temperature 
  REAL(wp) :: th(lon,lat,nlev+1,ntime)  	! K, temperature at half levels
  REAL(wp) :: tsrf(lon,lat,ntime)        	! surface temperature
!dir$ attributes align: 64 :: tf,th,tsrf

  REAL(wp) :: q(lon,lat,nlev,ntime)     	! g/g, water vapor mixing ratio
  REAL(wp) :: qs(lon,lat,nlev,ntime)    	! g/g, water vapor saturation mixing ratio
  REAL(wp) :: xl(lon,lat,nlev,ntime)    	! g/g, liquid water mixing ratio
  REAL(wp) :: xi(lon,lat,nlev,ntime)    	! g/g, ice mixing ratio
  REAL(wp) :: rhumidity(lon,lat,nlev,ntime)	! relativ humidity
!dir$ attributes align: 64 :: q, qs, xl, xi, rhumidity

  REAL(wp) :: cdnc(lon,lat,nlev,ntime)  	! cloud cond. nuclei
  REAL(wp) :: aclcac(lon,lat,nlev,ntime)	! m2/m2, cloud cover fraction
  REAL(wp) :: aclc(lon,lat,nlev,ntime)		! m2/m2, cloud cover fraction
  REAL(wp) :: aclcov(lon,lat,ntime)     	! m2/m2, total cloud cover from file

  REAL(wp) :: aer(lon,lat,nlev,naer,ntime)
!dir$ attributes align: 64 :: cdnc,aclcac,aclc,aclcov,aer

  REAL(wp) :: ao3(lon,lat,nlev,ntime)   	! g/g, O3 mass mixing ratio
  REAL(wp) :: co2(lon,lat,nlev,ntime)   	! g/g, CO2 mass mixing ratio
  REAL(wp) :: ch4(lon,lat,nlev,ntime)   	! g/g, CH4 mass mixing ratio
  REAL(wp) :: o2(lon,lat,nlev,ntime)   	! g/g, O2 mass mixing ratio
  REAL(wp) :: n2o(lon,lat,nlev,ntime)   	! g/g, N2O mass mixing ratio
  REAL(wp) :: cfc(lon,lat,nlev,ncfc,ntime)	! m3/m3, CFC11 and CFC12 vol. mixing ratio
!dir$ attributes align: 64 :: ao3,co2,ch4,o2,n2o,cfc

  ! output variables
  ! ----------------
  REAL(wp) :: flt(lon,lat,nlev+1,ntime) 	! W/m2, LW net flux
  REAL(wp) :: fls(lon,lat,nlev+1,ntime) 	! W/m2, SW net flux
  REAL(wp) :: flwdn (lon,lat,nlev+1,ntime) 	! W/m2, LW flux down
  REAL(wp) :: flwup (lon,lat,nlev+1,ntime) 	! W/m2, LW flux up
  REAL(wp) :: fswdn (lon,lat,nlev+1,ntime)	! W/m2, SW flux down
  REAL(wp) :: fswup (lon,lat,nlev+1,ntime)	! W/m2, SW flux up 
  REAL(wp) :: flwdnc(lon,lat,nlev+1,ntime) 	! W/m2, LW flux down, clear sky
  REAL(wp) :: flwupc(lon,lat,nlev+1,ntime) 	! W/m2, LW flux up, clear sky
  REAL(wp) :: fswdnc(lon,lat,nlev+1,ntime)	! W/m2, SW flux down, clear sky
  REAL(wp) :: fswupc(lon,lat,nlev+1,ntime)	! W/m2, SW flux up , clear sky
!dir$ attributes align: 64 :: flt,fls,flwdn,flwup,fswdn,fswup,flwdnc,flwupc,fswdnc,fswupc 

  REAL(wp) :: vis_frc_sfc(lon,lat,ntime)  ! Visible fraction of total surface downwelling
  REAL(wp) :: par_dn_sfc(lon,lat,ntime)   ! Downwelling PAR at surface
  REAL(wp) :: nir_dff_frc(lon,lat,ntime)  ! Diffuse fraction of surface near-ir
  REAL(wp) :: vis_dff_frc(lon,lat,ntime)  ! Diffuse fraction of surface vis
  REAL(wp) :: par_dff_frc(lon,lat,ntime)  ! Diffuse fraction of surface PAR 
!dir$ attributes align: 64 :: vis_frc_sfc, par_dn_sfc,nir_dff_frc,vis_dff_frc,par_dff_frc
  
  REAL(wp) :: dm(lon,lat,nlev,ntime)    	! kg/m2, density in layer
  REAL(wp) :: cp(lon,lat,nlev,ntime)    	! W/K/kg, cp(q)
  REAL(wp) :: dflt(lon,lat,nlev,ntime)  	! W/m2, divergence of LW net flux
  REAL(wp) :: dfls(lon,lat,nlev,ntime)  	! W/m2, divergence of SW net flux
  REAL(wp) :: qradt(lon,lat,nlev,ntime) 	! K/s, LW heating rate
  REAL(wp) :: qrads(lon,lat,nlev,ntime) 	! K/s, SW heating rate
!dir$ attributes align: 64 :: dm,cp, dflt,dfls, qradt, qrads

  REAL(wp) :: xlon(lon), xlat(lat)		! Longitude/latitude arrays
  REAL(wp) :: xtime(ntime)			! time axis
  INTEGER  :: i, j, k, jk, ilev(nlev), jlev(nlev+1) = (/ (i, i = 1,nlev+1) /) 

  REAL(wp) :: EqT, Tsol		   		        ! Equation of time, solar time
  REAL(wp) :: hang, dec, lati, loni  		! hour angle, declination, actual latitude, longitude
  REAL(wp) :: zang(lon,lat,ntime)	     	! solar zenith angle
!dir$ attributes align: 64 :: zang
  REAL(wp) :: tloc, tloc_c	             	! local time, local time correction for longitude

  INTEGER                    :: krow, klon, nLon, minL, maxL
  ! ======================================================================================================
  ! Radiation namelists
  include "radctl.inc" 
  
  CHARACTER(LEN=256) :: nmlFile="namelist", outDir="."
  INTEGER :: kproma = lon
  character(len=160) :: infile 	
  INTEGER :: day      =   1		
  REAL(WP):: nday    =   1._wp	
  INTEGER :: year     =   1		
  INTEGER :: nyear    =   1990	
  ! Zenith angle 				
  INTEGER :: iza       =   1	
  REAL(WP):: za0       =  45._wp
  NAMELIST /sa_rad_ctl/ nmlFile, outDir, kproma, &  
    iza, za0,  infile, day, nday, year, nyear 

  INTEGER :: ncid, dimids(4), start(4), ncvarids(16) 
  integer*8 :: c1,c2,cr,cm
  real*8 :: dt
  character(len=80), parameter :: kname='[psrad_interface]'
  ! ======================================================================================================
  !
  ! Read the namelists - can invoke the program with an argument to specify the name of the namelist file
  !
  ! Lorenzo's implementation used 1850 default values for gas concentrations. 
  !   These might get over-written by values in the namelist
  isolrad   =  0 
  co2vmr    =  284.7e-06_wp !< 1850 concentration
  ch4vmr    = 791.0e-09_wp !< 1850 concentration
  o2vmr     =    0.20946_wp !< O2
  n2ovmr    =  275.4e-09_wp !< 1850 concentration

  if(iargc() > 0) call getarg(1, nmlFile) 
  write(*,*) "Reading namelists from ", trim(nmlFile)
  OPEN (UNIT=nnml, FILE=trim(nmlFile)) 
  READ (nnml, radctl)
  READ (nnml, sa_rad_ctl) 
  CLOSE(nnml) 
  if(ntime /= read_diml_nf77(trim(infile), 'time')) &
    write(*,*) "Number of times don't agree. Program: ", lon, ", file: ", read_diml_nf77(trim(infile), 'time')

  ! read  geopotential heights
  CALL read_var(infile, 'geosp',geosp)
  ! ------------------------------------
  ! read dimensions
  CALL read_dims(inFile, xlon, xlat, ilev, xtime) 
  
  nlon = lon/kproma + MERGE(0, 1, MOD(lon, kproma) == 0)  ! How many loops over longitude? 
  IF(kproma /= lon) write(*,*) "kproma, nlon", kproma, nlon 
  
  ! Set nominal resolution in mo_control - this is relevant only for tuning of cloud optics
  control_nn = 63
  control_nlev = nlev
  control_nlevp1 = nlev+1

  ! Weights for time interpolation of aerosol quantities (mo_interpo)
  wgt1_m = 0.5_wp
  wgt2_m = 0.5_wp
  nmw1_m = 1
  nmw2_m = 1
  
  ! initialize grid
  CALL grid_init(lon,lat,nlev,ntime)

  ! allocate memory
  CALL construct_g3b

  ! initialize vertical grid
  CALL init_vct

  ! initialize radiation and cloud optics
  CALL setup_radiation
  CALL pre_radiation

  dy: SELECT CASE (day)
  CASE (0)
     day=1
  CASE (1)
     nday=nday
  CASE default
     STOP 'day not supported'
  END SELECT dy

  ! ------------------------------------
  ! Data input 
  ! ------------------------------------
  
  !read land-sea and glacier mask, convert to logicals
  call read_var(infilelsm, 'SLM', lolandh)
  call read_var(infileglc, 'GLAC',loglach)
  loland(:,:) = lolandh(:,:) > 0.5
  loglac(:,:) = loglach(:,:) > 0.5 

  ! ------------------------------------
  ! read cloud water/ice from file
  CALL read_var (inFile,'xl',xl)
  CALL read_var (inFile,'xi',xi)
  xl(:,:,:,:)=max(xl(:,:,:,:),0._wp) ! set negativ values to 0
  xi(:,:,:,:)=max(xi(:,:,:,:),0._wp) ! set negativ values to 0

  ! read cloud cover fraction
  CALL read_var (inFile, 'aclc', aclc)
  aclcac(:,:,:,:)=max(aclc(:,:,:,:),EPSILON(0._wp))

  !read surface pressure
  CALL read_var (inFile, 'aps',p0)

  ! read cloud droplet number concentration
  CALL read_var(inFile, 'acdnc',cdnc)
  cdnc(:,:,:,:)=max(cdnc(:,:,:,:),EPSILON(0._wp)) ! set negativ values to EPSILON(1.)
   
  ! read albedo
  CALL read_var(inFile, 'albedo',albedo(:,:,1))
  DO i=1,ntime
   albedo(:,:,i)=albedo(:,:,1)
  ENDDO
  
  ! read surface temperature
  CALL read_var(inFile,'tsurf',tsrf)

  !
  ! vertical coordinate system of input files
  ! -----------------------------------------
!
!  ! in ECHAM the pressure at the full levels is defined in the middle
!  ! between the upper and lower layer interfaces: 
!  ! pf(jk)=(ph(jk)+ph(jk+1))/2 with ph(1)=0. and ph(nlev+1)=psrf
! Pressure Levels are now constructed from vertical coordinat table A/B
! and surface pressure, like in ECHAM
  DO  jk=1,nlev+1
    ph (:,:,jk,:) = vct_a(jk)+ vct_b(jk)* p0(:,:,:)
  END DO
  DO jk=1,nlev
     pf(:,:,jk,:)=0.5_wp*(ph(:,:,jk,:)+ph(:,:,jk+1,:))
  END DO

  !
  ! solar zenith angle
  ! -----------------------------------------
  zenith: SELECT CASE (iza)
  CASE (1)
    ! calculate the solar zenith angle. Following:
    ! http://solardat.uoregon.edu/SolarRadiationBasics.html
    ! daniel.klocke@zmaw.de
    ! calcluate equation of time for solar time difference
    ! calculate difference of solar time to local time (gmt) other wise longitut correction is needed
    Tloc=-6._wp				! Tloc is set to -6, to start on 0 in the first loop
        write(*,*)'calculating zenith angles for Julian day:',nday
    DO j=1,ntime
      Tloc=Tloc+6._wp
      IF (Tloc > 23. ) THEN
        Tloc=0._wp
        nday=nday+1.
        write(*,*)'calculating zenith angles for Julian day:',nday
      ENDIF
        write(*,*)Tloc,'oclock'
      IF (nday > 0. .and. nday .le. 106.) then
        EqT = -14.2 * sin( pi * (nday + 7.)/111.)
      ELSEIF (nday .gt. 106. .and. nday .le. 166.) then
        EqT = 4. * sin( pi * (nday - 106.)/59.)
      ELSEIF (nday .gt. 166. .and. nday .le. 246.) then
        EqT = -6.5 * sin( pi * (nday - 166.)/80.)
      ELSEIF (nday .gt. 246. .and. nday .le. 366.) then
        EqT = 16.4 * sin( pi * (nday - 247.)/113.)
      ENDIF
              
      DO k=1,lon     				! zenith angle calculation for longitude
        loni = (+180. - 360./lon * k )		! longitude
        Tloc_c = Tloc - (loni - 180)/15.		! time correction for longitude
        Tsol = Tloc_c + EqT/60.			! solar time
        ! hour angle
        hang = pi * (12.-Tsol)/12.
        ! declination
        dec = (23.45*pi/180.) * (cos(2.*pi/365. * (nday-172.)))
        ! solar zenith angle 
        DO i=1,lat
          lati = (90. - 180./lat * i )* (pi/180.)
          zang(k,i,j) =  acos(sin(lati)*sin(dec)+(cos(lati)*cos(dec)*cos(hang)))*180./pi
        ENDDO
      ENDDO
    ENDDO
    mu0(:,:,:)=min(zang(:,:,:),90._wp)
  CASE (2)
     mu0(:,:,:)=za0
  CASE default
     STOP 'zenith: this "iza" is not supported'
  END SELECT zenith
  mu0(:,:,:)=MAX(COS(mu0(:,:,:)/180.*pi), zepzen)

  ! profiles of temperature at full levels 
  ! ------------------------------------------
  CALL read_var(infile, 't',tf)
  !define surface pressure
  psrf(:,:,:) = p0(:,:,:)

  ! thickness of layers
  ! -------------------
  dpr(:,:,1,:)=ph(:,:,2,:)
  DO jk=2,nlev
     dpr(:,:,jk,:)=ph(:,:,jk+1,:)-ph(:,:,jk,:)
  END DO

  ! temperature at half levels
  ! --------------------------
  ! interpolate between full levels
  DO jk=2,nlev
     th(:,:,jk,:)= ( tf(:,:,jk-1,:)*pf(:,:,jk-1,:)*(pf(:,:,jk,:)-ph(:,:,jk,:)  ) &
          &     +tf(:,:,jk,:)  *pf(:,:,jk,:)  *(ph(:,:,jk,:)-pf(:,:,jk-1,:)) ) &
          &   /(            ph(:,:,jk,:)  *(pf(:,:,jk,:)-pf(:,:,jk-1,:)) )
  END DO

  ! and extrapolate to TOA and surface
  th(:,:,1,:)     =tf(:,:,1,:)-pf(:,:,1,:)*(tf(:,:,1,:)-th(:,:,2,:))/(pf(:,:,1,:)-ph(:,:,2,:))
  th(:,:,nlev+1,:)=tsrf(:,:,:)


write(*,*)'-------------------------------------------'
write(*,*)'------------setting done!------------------'
write(*,*)'-------------------------------------------'

    !
    ! Initializations
    ! --------------------------------
   call nc_check(nf90_create("two-stream.nc", NF90_64BIT_OFFSET, ncid))
   call nc_check(nf90_def_dim(ncid, 'lon', lon,   dimids(1)))
   call nc_check(nf90_def_dim(ncid, 'lev', nlev,  dimids(2)))
   call nc_check(nf90_def_dim(ncid, 'gpt', 112,   dimids(3)))
   call nc_check(nf90_def_dim(ncid, 'lat', lat,   dimids(4)))
   
   !Define variables
   call nc_check(nf90_def_var(ncid, 'tau' , nf90_double, dimids, ncvarids(1)))
   call nc_check(nf90_def_var(ncid, 'w0'  , nf90_double, dimids, ncvarids(2)))
   call nc_check(nf90_def_var(ncid, 'g'   , nf90_double, dimids, ncvarids(3)))
   call nc_check(nf90_def_var(ncid, 'Rdir', nf90_double, dimids, ncvarids(4)))
   call nc_check(nf90_def_var(ncid, 'Tdir', nf90_double, dimids, ncvarids(5)))
   call nc_check(nf90_def_var(ncid, 'Rdif', nf90_double, dimids, ncvarids(6)))
   call nc_check(nf90_def_var(ncid, 'Tdif', nf90_double, dimids, ncvarids(7)))
   call nc_check(nf90_def_var(ncid, 'mu0' , nf90_double, dimids((/1,2,4/)), ncvarids(8)))


  DO i=1, ntime
    write(*,*)'calculating radiation for timestep: ',i,' from: ',ntime 
    call system_clock(c1,cr,cm)
    DO krow=1,lat
      DO klon = 1,nlon
        minL = (klon-1) * kproma + 1
        maxL = MIN(klon * kproma, lon) 
		CALL psrad_interface( ncid, ncvarids, &
			 & iaero           ,maxL-minL+1     ,maxL-minL+1     ,nlev            ,&
			 & krow            ,ntrac           ,ktype(minL:maxL),nb_sw           ,&
             & loland(minL:maxL,krow)           ,loglac(minL:maxL,krow)           ,cemiss,&
             & mu0(minL:maxL,krow,i)            ,pgeom1(minL:maxL, :)            , &		
             & albedo(minL:maxL,krow,i)         ,albedo(minL:maxL,krow,i)        , & 
             & albedo(minL:maxL,krow,i)         ,albedo(minL:maxL,krow,i)        , &
             & pf(minL:maxL,krow,:,i)            ,ph(minL:maxL,krow,:,i)            ,psrf(minL:maxL,krow,i)        ,tf(minL:maxL,krow,:,i)         ,&
             & th(minL:maxL,krow,:,i)            ,tsrf(minL:maxL,krow,i)            ,q(minL:maxL,krow,:,i), &
             & xl(minL:maxL,krow,:,i)            ,xi(minL:maxL,krow,:,i)            ,cdnc(minL:maxL,krow,:,i)      ,aclcac(minL:maxL,krow,:,i)     ,&
            & aclcov(minL:maxL,krow,i)          ,ao3(minL:maxL,krow,:,i)           ,co2(minL:maxL,krow,:,i)       ,ch4(minL:maxL,krow,:,i)        ,&
            & n2o(minL:maxL,krow,:,i)           ,cfc(minL:maxL,krow,:,:,i)         ,o2(minL:maxL,krow,:,i)        ,pxtm1                  ,&
			 & flwup(minL:maxL,krow,:,i),flwupc(minL:maxL,krow,:,i),flwdn(minL:maxL,krow,:,i),flwdnc(minL:maxL,krow,:,i),&
			 & fswup(minL:maxL,krow,:,i),fswupc(minL:maxL,krow,:,i),fswdn(minL:maxL,krow,:,i),fswdnc(minL:maxL,krow,:,i), &
			 & vis_frc_sfc(minL:maxL,:,i),par_dn_sfc(minL:maxL,:,i), &
			 & nir_dff_frc(minL:maxL,:,i),vis_dff_frc(minL:maxL,:,i),par_dff_frc(minL:maxL,:,i))
      END DO 
    END DO
    call system_clock(c2,cr,cm)
  END DO		! loop time
  dt = dble(c2-c1)/dble(cr)
  print *,'after call to psrad_interface'
  print *,TRIM(kname), ' total time(sec): ',dt 
  print *,TRIM(kname), ' time per call (usec): ',1.e6*dt/dble(lat*nlon*ntime)

  call nc_check(nf90_close(ncid))

  ! compute radiative heating rates
  ! -------------------------------
  DO jk=1,nlev
     dflt(:,:,jk,:) =-(flt(:,:,jk+1,:)-flt(:,:,jk,:))
     dfls(:,:,jk,:) =-(fls(:,:,jk+1,:)-fls(:,:,jk,:))
  END DO
  dm(:,:,:,:)   = dpr(:,:,:,:)*rgrav
  cp(:,:,:,:)   = cpd/(1.+vtmpc2*q(:,:,:,:))
  qradt(:,:,:,:)=dflt(:,:,:,:)/dm(:,:,:,:)/cp(:,:,:,:)
  qrads(:,:,:,:)=dfls(:,:,:,:)/dm(:,:,:,:)/cp(:,:,:,:)

  write(*,*) "Done with computation" 

  write(*,*) "Shortwave flux ranges: " 
  write(*,*) 'fswdn', minval(fswdn), maxval(fswdn)
  write(*,*) 'fswup', minval(fswup), maxval(fswup) 
  write(*,*) 'fswdnc', minval(fswdnc), maxval(fswdnc) 
  write(*,*) 'fswupc', minval(fswupc), maxval(fswupc) 

  write(*,*) "Boundary means : " 
  write(*,*) 'fswdn', sum(fswdn(:,:,1,1))/(real(lon*lat)), sum(fswdn(:,:,nlev,1))/(real(lon*lat))
  write(*,*) 'fswup', sum(fswup(:,:,1,1))/(real(lon*lat)), sum(fswup(:,:,nlev,1))/(real(lon*lat))
  write(*,*) 'fswdnc', sum(fswdnc(:,:,1,1))/(real(lon*lat)), sum(fswdnc(:,:,nlev,1))/(real(lon*lat))
  write(*,*) 'fswupc', sum(fswupc(:,:,1,1))/(real(lon*lat)), sum(fswupc(:,:,nlev,1))/(real(lon*lat))

  write(*,*) "Boundary means, net : " 
  write(*,*) 'fswdn', sum(fswdn(:,:,1,1) - fswup(:,:,1,1))/(real(lon*lat)), sum(fswdn(:,:,nlev,1) - fswup(:,:,1,1))/(real(lon*lat))
  write(*,*) 'fswdnc', sum(fswdnc(:,:,1,1) - fswupc(:,:,1,1))/(real(lon*lat)), sum(fswdnc(:,:,nlev,1) - fswupc(:,:,1,1))/(real(lon*lat))

  ! write profiles
  CALL write_var(outDir, nday, xlon, xlat, jlev, xtime,'flwdn',flwdn(:,:,1:nlev+1,:))
  CALL write_var(outDir, nday, xlon, xlat, jlev, xtime,'flwup',flwup(:,:,1:nlev+1,:))
  CALL write_var(outDir, nday, xlon, xlat, jlev, xtime,'fswdn',fswdn(:,:,1:nlev+1,:))
  CALL write_var(outDir, nday, xlon, xlat, jlev, xtime,'fswup',fswup(:,:,1:nlev+1,:))
  CALL write_var(outDir, nday, xlon, xlat, jlev, xtime,'flwdnc',flwdnc(:,:,1:nlev+1,:))
  CALL write_var(outDir, nday, xlon, xlat, jlev, xtime,'flwupc',flwupc(:,:,1:nlev+1,:))
  CALL write_var(outDir, nday, xlon, xlat, jlev, xtime,'fswdnc',fswdnc(:,:,1:nlev+1,:))
  CALL write_var(outDir, nday, xlon, xlat, jlev, xtime,'fswupc',fswupc(:,:,1:nlev+1,:))


!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime,'flth',flt(:,:,1:nlev,:))
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime,'flthc',fltc(:,:,1:nlev,:))
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime,'flsh',fls(:,:,1:nlev,:))
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime, 'flshc',flsc(:,:,1:nlev,:))
  !
  ! write surface fields
!  CALL write_var(outDir, nday, xlon, xlat, xtime, 'supt',supt)
!  CALL write_var(outDir, nday, xlon, xlat, xtime, 'sups',sups)
!  CALL write_var(outDir, nday, xlon, xlat, xtime, 'semit',semit)
!  CALL write_var(outDir, nday, xlon, xlat, xtime, 'tdws',tdws)
!  CALL write_var(outDir, nday, xlon, xlat, xtime, 'suptc',suptc)
!  CALL write_var(outDir, nday, xlon, xlat, xtime, 'supsc',supsc)


  !
  ! write profiles
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime, 'dfltf',dflt(:,:,1:nlev,:))
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime, 'dflsf',dfls(:,:,1:nlev,:))
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime, 'dmf',dm)
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime, 'cpf',cp)
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime, 'qlf',qradt*86400)
!  CALL write_var(outDir, nday, xlon, xlat, ilev, xtime, 'qsf',qrads*86400)
contains 
  SUBROUTINE setup_radiation
    ! Specific additions in radiation program
    USE mo_spec_sampling, ONLY : spec_sampling_strategy, &
                               & set_spec_sampling_lw, set_spec_sampling_sw, get_num_gpoints
    USE mo_rrtm_params,   ONLY : nbndsw
    USE mo_mpi,           ONLY: p_parallel, p_parallel_io, p_io, p_bcast
    USE mo_cloud_optics,  ONLY: setup_cloud_optics
    
    USE mo_aero_kinne,       ONLY: su_aero_kinne
!    USE mo_aero_volc,        ONLY: su_aero_volc
!    USE mo_aero_volc_tab,    ONLY: su_aero_prop_ham, su_aero_prop_crow, &
!                                   read_aero_volc_tables
!    USE mo_solar_irradiance, ONLY: init_solar_irradiance
    implicit none
    INTEGER :: ierr, inml, iunit
    !
    ! 1.0 Read radctl namelist to modify mo_radiation
    ! --------------------------------
    IF (p_parallel_io) THEN
      !
      ! --- In case of coupled runs initially set basic values to 1850 values
      ! 
      IF(lcouple) THEN
        co2vmr    = 284.725e-06_wp    !< 1850 concentration
        ch4vmr    = 0.79097924e-06_wp !< 1850 concentration
        n2ovmr    = 0.2754250e-06_wp  !< 1850 concentration
        cfcvmr(1) = 0.0_wp
        cfcvmr(2) = 0.0_wp
      ENDIF
      !
      ! --- Change default behavior for non Middle Atmosphere runs
      ! 
      IF(.NOT. lmidatm) THEN
        ich4 = 2
        in2o = 2
      ENDIF
      !
      ! --- Read NAMELIST
      ! 
!      inml = open_nml('namelist.echam')
!      iunit = position_nml ('RADCTL', inml, status=ierr)
!      SELECT CASE (ierr)
!      CASE (POSITIONED)
!        READ (iunit, radctl)
!      END SELECT
      IF(ich4 == 4 .OR. in2o == 4 .OR. ico2 == 4 .OR. icfc == 4 ) ighg = 1
    ENDIF
    !
    ! 2.0 Broadcast NAMELIST variables
    ! --------------------------------
    IF (p_parallel) THEN
      CALL p_bcast (nmonth, p_io)
      CALL p_bcast (isolrad, p_io)
      CALL p_bcast (ldiur, p_io)
      CALL p_bcast (lradforcing, p_io)
!      CALL p_bcast_event (trigrad, p_io)
      CALL p_bcast (ih2o, p_io)
      CALL p_bcast (ico2, p_io)
      CALL p_bcast (ich4, p_io)
      CALL p_bcast (io3, p_io)
      CALL p_bcast (io2, p_io)
      CALL p_bcast (in2o, p_io)
      CALL p_bcast (icfc, p_io)
      CALL p_bcast (ighg, p_io)
      CALL p_bcast (iaero, p_io)
      CALL p_bcast (fco2, p_io)
      CALL p_bcast (co2vmr, p_io)
      CALL p_bcast (ch4vmr, p_io)
      CALL p_bcast (n2ovmr, p_io)
      CALL p_bcast (cfcvmr, p_io)
      CALL p_bcast (o2vmr, p_io)
!      CALL p_bcast (cecc, p_io)
!      CALL p_bcast (cobld, p_io)
!      CALL p_bcast (clonp, p_io)
      CALL p_bcast (yr_perp, p_io)
      CALL p_bcast (lw_spec_samp, p_io)
      CALL p_bcast (sw_spec_samp, p_io)
      CALL p_bcast (lw_gpts_ts, p_io)
      CALL p_bcast (sw_gpts_ts, p_io)
      CALL p_bcast (rad_perm, p_io)
      CALL p_bcast (l_lw_unique, p_io)
      CALL p_bcast (l_sw_unique, p_io)
    ENDIF
    !
    !
    ! 3.0 If radiation is active check NAMELIST variable conformance
    ! --------------------------------
    call setup_psrad
    nb_sw = nbndsw
    
    ! Following drawn from original radiation program - 
    !  flags (ih2o, etc.) may have different meanings than in standard ECHAM
    h2o: SELECT CASE (ih2o)
    CASE (0)
       q(:,:,:,:)=EPSILON(1._wp)
       xl(:,:,:,:)=0._wp
       xi(:,:,:,:)=0._wp
    CASE (1)
       ! read specific humidity
       CALL read_var(infile, 'q',q)
    CASE default
       STOP 'h2o: this "ih2o" is not supported'
    END SELECT h2o
    q(:,:,:,:)=max(q(:,:,:,:),0._wp) ! set negative values to 0

    ! read rel. humidity to calculate saturation specific humidity
    CALL read_var(inFile, 'rhumidity',rhumidity)
    rhumidity(:,:,:,:)=max(rhumidity(:,:,:,:),EPSILON(0._wp)) ! set negativ values to EPSILON(1.)

    ! calculate saturation specific humidity
    qs(:,:,:,:) = q(:,:,:,:)/rhumidity(:,:,:,:)

    cd: SELECT CASE (ico2)
    CASE (0)
       co2(:,:,:,:)=EPSILON(1._wp)
    CASE (2)
       co2(:,:,:,:)=co2mmr
    CASE default
       STOP 'co2: this "ico2" is not supported'
    END SELECT cd


    methane: SELECT CASE (ich4)
    CASE (0)
       ch4(:,:,:,:)=EPSILON(1._wp)
    CASE (2)
       ch4(:,:,:,:)=ch4mmr
    CASE default
       STOP 'ch4: this "ich4" is not supported'
    END SELECT methane

    oxygen: SELECT CASE (io2)
    CASE (0)
       o2(:,:,:,:)=EPSILON(1._wp)
    CASE (2)
       o2(:,:,:,:)=o2mmr
    CASE default
       STOP 'o2: this "io2" is not supported'
    END SELECT oxygen

    ozone: SELECT CASE (io3)
    CASE (0)
       ao3(:,:,:,:)=EPSILON(1._wp)
    CASE (1)
       ! read o3 volume mixing ratio in ppmv
       CALL read_var(infile, 'ao3',ao3)
       ! convert to mass mixing ratio
       ao3(:,:,:,:)=ao3(:,:,:,:)*amo3/amd
    CASE default
       STOP 'o3: this "io3" is not supported'
    END SELECT ozone


    cn2o: SELECT CASE (in2o)
    CASE (0)
       n2o(:,:,:,:)=EPSILON(1._wp)
    CASE (1)
       CALL read_var(infile,  'N2O',n2o)
    CASE (2)
       n2o(:,:,:,:)=n2ommr
    CASE default
       STOP 'n2o: this "in2o" is not supported'
    END SELECT cn2o


    cfcs: SELECT CASE (icfc)
    CASE (0)
       cfc(:,:,:,:,:)=EPSILON(1._wp)
    CASE (2)
       cfc(:,:,:,1,:)=cfcvmr(1)
       cfc(:,:,:,2,:)=cfcvmr(2)
    CASE default
       STOP 'cfc: this "icfc" is not supported'
    END SELECT cfcs


    aerosol: SELECT CASE (iaero)
    CASE (0)
       aer(:,:,:,:,:)=0._wp
    CASE (3)
       aer(:,:,:,:,:)=0._wp
    CASE default
       STOP 'aerosols: this "iaero" is not supported'
    END SELECT aerosol
    ! aerosol values are at least epsilon
    aer(:,:,:,:,:)=MAX(aer(:,:,:,:,:),EPSILON(1._wp))

    SELECT CASE (isolrad)
    CASE (0) 
      CALL message('','isolrad = 0 --> standard rrtm solar constant')
!    CASE (1) 
!      CALL message('','isolrad = 1 --> time dependent spectrally resolved solar constant read from file')
!      CALL init_solar_irradiance(nb_sw)
    CASE (2) 
      CALL message('','isolrad = 2 --> preindustrial solar constant')
    CASE (3) 
      CALL message('','isolrad = 3 --> solar constant for amip runs')
    CASE default 
      WRITE (message_text, '(a,i3,a)') &
           'Run terminated isolrad = ', isolrad, ' not supported'
      CALL message('',message_text)
      CALL finish('setup_radiation', message_text)
    END SELECT
    !
    ! --- Check diurnal cycle
    ! 
    IF (ldiur) THEN
      CALL message('','ldiur =.TRUE.  --> diurnal cycle on')
    ELSE
      CALL message('','ldiur =.FALSE. --> diurnal cycle off')
    ENDIF
    !
    ! --- Check for diagnosis of instantaneous aerosol radiative forcing
    ! 
    CALL message('','instantaneous forcing diagnostic:')
    WRITE (message_text,'(a16,L3,a18,L3)')       &
         ' solar radiation: ',   lradforcing(1), &
         ' thermal radiation: ', lradforcing(2)
    CALL message('',message_text)

    SELECT CASE (nmonth)
    CASE(0)
      CALL message('','nmonth=0 --> annual cycle on')
    CASE(1:12)
      WRITE (message_text, '(a,i2.2,a)') &
           'nmonth = ', nmonth, ' --> perpetual month'
      CALL message('',message_text)
    CASE default
      WRITE (message_text, '(a,i2,a)') &
           'nmonth=', nmonth, ' in radctl namelist is not supported'
      CALL message('',message_text)
      CALL finish('setup_radiation','Run terminated nmonth')
    END SELECT

    !
    ! --- Spectral sampling strategy
    !
    lw_strat = set_spec_sampling_lw(lw_spec_samp, num_gpts_ts=lw_gpts_ts, &
                                    l_unique=l_lw_unique) 
    sw_strat = set_spec_sampling_sw(sw_spec_samp, num_gpts_ts=sw_gpts_ts, &
                                    l_unique=l_sw_unique) 
    WRITE (message_text, '("LW sampling strategy: ", i2, " using ", i3, " g-points per time step")') &
               lw_spec_samp, get_num_gpoints(lw_strat)
    CALL message('',message_text)
    WRITE (message_text, '("SW sampling strategy: ", i2, " using ", i3, " g-points per time step")') &
               sw_spec_samp, get_num_gpoints(sw_strat)
    CALL message('',message_text)

    CALL setup_cloud_optics
    CALL su_aero_kinne(nb_sw) ! Funny that we need to set up aerosols if we don't use them. 

!    IF (lrad) THEN
!
!      call setup_psrad
!      nb_sw = nbndsw
!      !
!      ! --- Spectral sampling strategy
!      !
!      lw_strat = set_spec_sampling_lw(lw_spec_samp, num_gpts_ts=lw_gpts_ts, &
!                                      l_unique=l_lw_unique) 
!      sw_strat = set_spec_sampling_sw(sw_spec_samp, num_gpts_ts=sw_gpts_ts, &
!                                      l_unique=l_sw_unique) 
!      WRITE (message_text, '("LW sampling strategy: ", i2, " using ", i3, " g-points per time step")') &
!                 lw_spec_samp, get_num_gpoints(lw_strat)
!      CALL message('',message_text)
!      WRITE (message_text, '("SW sampling strategy: ", i2, " using ", i3, " g-points per time step")') &
!                 sw_spec_samp, get_num_gpoints(sw_strat)
!      CALL message('',message_text)
!
!      !
!      CALL message('','lrad = .TRUE.  --> Doing radiation radiation')
!      IF(.false.) THEN 
!      !
!      ! --- Check  H2O
!      !
!      SELECT CASE (ih2o)
!      CASE(0)
!        CALL message('','ih2o = 0 --> no H2O(gas,liquid,ice) in radiation')
!      CASE(1)
!        CALL message('','ih2o = 1 --> prognostic H2O(gas,liquid,ice)')
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'ih2o =', ih2o, ' in radctl namelist is not supported'
!        CALL message('', message_text)
!        CALL finish('setup_radiation','Run terminated ih2o')
!      END SELECT
!      !
!      ! --- Check  CO2
!      ! 
!      SELECT CASE (ico2)
!      CASE(0)
!        CALL message('','ico2 = 0 --> no CO2 in radiation')
!        co2mmr=co2vmr*amco2/amd   ! Necessary for use with lco2=.TRUE.
!      CASE(1)  
!        IF (lco2) THEN
!          WRITE (message_text, '(a,e16.8)') &
!               'ico2 = 1 --> Initial CO2 volume mixing ratio=', co2vmr
!          CALL message('',message_text)
!          co2mmr = co2vmr*amco2/amd
!        ELSE
!          CALL finish('setup_radiation','ico2=1 (interactive CO2) not '// &
!               &      'a valid choice for lco2=.false.')
!        END IF
!      CASE(2)
!        WRITE (message_text, '(a,e16.8)') &
!             'ico2 = 2 --> CO2 volume mixing ratio=', co2vmr
!        CALL message('',message_text)
!        co2mmr = co2vmr*amco2/amd
!      CASE(4)
!        CALL message('','ico2 = 4 --> CO2 volume mixing ratio from scenario')
!        IF (ABS(fco2-1._wp) > EPSILON(1._wp)) THEN
!           WRITE (message_text, '(a,e16.8,a)') &
!                'fco2 = ', fco2, ' --> Factor for CO2 scenario'
!           CALL message('',message_text)
!        END IF
!        co2mmr = co2vmr*fco2*amco2/amd    ! This is only a dummy value for the first
!                                          ! initialization of co2m1 in the co2-module. 
!                                          ! co2m1 will be overwritten with the correct
!                                          ! values as soon as the ghg-data are
!                                          ! interpolated to the right date.
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'ico2 = ', ico2, ' in radctl namelist is not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation','Run terminated ico2')
!      END SELECT
!
!      IF (ico2 /= 4 .AND. ABS(fco2-1._wp) > EPSILON(1._wp)) THEN
!         WRITE (message_text, '(a,i2,a)') &
!              'ico2 = ', ico2, ' and fco2 != 1. --> Ignoring fco2'
!         CALL message('',message_text)
!      END IF
!      !
!      ! --- Check CH4
!      ! 
!      SELECT CASE (ich4)
!      CASE(0)
!        CALL message('','ich4 = 0 --> no CH4 in radiation')
!      CASE(1)
!        CALL message('','ich4 = 1 --> transported CH4 is not yet implemented')
!        CALL finish('setup_radiation','Run terminated ich4')
!      CASE(2)
!        WRITE (message_text, '(a,e16.8)') &
!             'ich4 = 2 --> CH4 volume mixing ratio=', ch4vmr
!        CALL message('',message_text)
!        ch4mmr = ch4vmr*amch4/amd
!      CASE(3)
!        WRITE (message_text, '(a,e16.8)') &
!             'ich4 = 3 --> CH4 (trop) volume mixing ratio =', ch4vmr
!        CALL message('',message_text)
!        ch4mmr = ch4vmr*amch4/amd
!      CASE(4)
!        CALL message('','ich4 = 4 --> CH4 volume mixing ratio from scenario')
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'ich4 =', ich4, ' in radctl namelist is not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation','Run terminated ich4')
!      END SELECT
!      !
!      ! --- Check O3
!      ! 
!      SELECT CASE (io3)
!      CASE(0)
!        CALL message('','io3  = 0 --> no O3 in radiation')
!      CASE(1)
!        CALL message('','io3  = 1 --> transported O3 is not yet implemented')
!        CALL finish('setup_radiation','Run terminated io3')
!      CASE(2)
!        CALL message('','io3  = 2 --> spectral O3 climatology (ECHAM4)')
!      CASE(3)
!        CALL message('','io3  = 3 --> gridpoint O3 climatology from NetCDF file')
!      CASE(4)
!        CALL message('','io3  = 4 --> gridpoint O3 climatology from IPCC-NetCDF file')
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'io3  =', io3, ' in radctl namelist is not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation','Run terminated io3')
!      END SELECT
!      !
!      ! --- Check N2O
!      ! 
!      SELECT CASE (in2o)
!      CASE(0)
!        CALL message('','in2o = 0 --> no N2O in radiation')
!      CASE(1)
!        CALL message('','in2o = 1 --> transported N2O is not yet implemented')
!        CALL finish('setup_radiation','Run terminated in2o')
!      CASE(2)
!        WRITE (message_text, '(a,e16.8)') &
!             'in2o = 2 --> N2O volume mixing ratio=', n2ovmr
!        CALL message('',message_text)
!        n2ommr = n2ovmr*amn2o/amd
!      CASE(3)
!        WRITE (message_text, '(a,e16.8)') &
!             'in2o = 3 --> N2O (trop) volume mixing ratio=', n2ovmr
!        CALL message('',message_text)
!        n2ommr = n2ovmr*amn2o/amd
!      CASE(4)
!        CALL message('','in2o = 4 --> N2O volume mixing ratio from scenario')
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'in2o =',in2o,' in radctl namelist is not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation','Run terminated in2o')
!      END SELECT
!      !
!      ! --- Check CFCs
!      ! 
!      SELECT CASE (icfc)
!      CASE(0)
!        CALL message('','icfc = 0 --> no CFCs in radiation')
!      CASE(1)
!        CALL message('','icfc = 1 --> transported CFCs not yet implemented')
!        CALL finish('setup_radiation','Run terminated icfc')
!      CASE(2)
!        WRITE (message_text, '(a,e16.8)') &
!             'icfc = 2 --> CFC11    volume mixing ratio=', cfcvmr(1)
!        CALL message('',message_text)
!        WRITE (message_text, '(a,e16.8)') &
!             '             CFC12    volume mixing ratio=', cfcvmr(2)
!        CALL message('',message_text)
!      CASE(4)
!        CALL message('','icfc = 4 --> CFC volume mixing ratio from scenario')
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'icfc=', icfc, ' in radctl namelist is not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation','Run terminated icfc')
!      END SELECT
!      !
!      ! --- Check Scenario
!      ! 
!      SELECT CASE (ighg)
!      CASE(0)
!        CALL message('','ighg = 0 --> no scenario, fixed greenhouse gases and/or cfc')
!      CASE(1)
!        CALL message('','ighg = 1 --> greenhouse gases from scenario, check setting of switches')
!      END SELECT
!      !
!      ! --- Check O2
!      ! 
!      SELECT CASE (io2)
!      CASE(0)
!        CALL message('','io2  = 0 --> no O2  in radiation')
!      CASE(2)
!        WRITE (message_text, '(a,e16.8)') &
!             'io2  = 2 --> O2    volume mixing ratio=', o2vmr
!        CALL message('',message_text)
!        o2mmr = o2vmr*amo2/amd
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'io2 =', io2, ' in radctl namelist is not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation','Run terminated io2')
!      END SELECT
!      !
!      ! --- Check aerosol
!      ! 
!      SELECT CASE (iaero)
!      CASE(0)
!        CALL message('','iaero= 0 --> no aerosol in radiation')
!      CASE(1)
!        CALL message('','iaero= 1 --> prognostic aerosol (sub model)')
!      CASE(3)
!        CALL message('','iaero= 3 --> Kinne climatology')
!        CALL su_aero_kinne(nb_sw)
!      CASE(5)
!        CALL message('','iaero= 5 --> Kinne climatology + Stenchikov volcanic aerosol')
!        CALL su_aero_kinne(nb_sw)
!        CALL su_aero_volc(nb_sw)
!      CASE(6)
!        CALL message('','iaero= 6 --> Kinne climatology + Stenchikov volcanic aerosols + HAM volcanic aerosol')
!        CALL su_aero_kinne(nb_sw)
!        CALL su_aero_volc(nb_sw)
!        CALL su_aero_prop_ham
!        CALL read_aero_volc_tables
!      CASE(7)
!        CALL message('','iaero= 7 --> Kinne climatology + Crowley volcanic aerosol')
!        CALL su_aero_kinne(nb_sw)
!        CALL su_aero_prop_crow
!        CALL read_aero_volc_tables
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'iaero=', iaero, ' in radctl namelist is not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation','Run terminated iaero')
!      END SELECT
!      !
!      ! --- Check annual cycle
!      ! 
!      SELECT CASE (nmonth)
!      CASE(0)
!        CALL message('','nmonth=0 --> annual cycle on')
!      CASE(1:12)
!        WRITE (message_text, '(a,i2.2,a)') &
!             'nmonth = ', nmonth, ' --> perpetual month'
!        CALL message('',message_text)
!      CASE default
!        WRITE (message_text, '(a,i2,a)') &
!             'nmonth=', nmonth, ' in radctl namelist is not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation','Run terminated nmonth')
!      END SELECT
!      !
!      ! --- Check Shortwave Model
!      ! 
!      CALL message('','  --> USE AER RRTM Shortwave Model')
!      !
!      ! --- Check Longwave Model
!      ! 
!      CALL message('','  --> USE New (V4) LRTM Model')
!      !
!      ! --- Check solar constant
!      !
!      SELECT CASE (isolrad)
!      CASE (0) 
!        CALL message('','isolrad = 0 --> standard rrtm solar constant')
!      CASE (1) 
!        CALL message('','isolrad = 1 --> time dependent spectrally resolved solar constant read from file')
!        CALL init_solar_irradiance(nb_sw)
!      CASE (2) 
!        CALL message('','isolrad = 2 --> preindustrial solar constant')
!      CASE (3) 
!        CALL message('','isolrad = 3 --> solar constant for amip runs')
!      CASE default 
!        WRITE (message_text, '(a,i3,a)') &
!             'Run terminated isolrad = ', isolrad, ' not supported'
!        CALL message('',message_text)
!        CALL finish('setup_radiation', message_text)
!      END SELECT
!      !
!      ! --- Check diurnal cycle
!      ! 
!      IF (ldiur) THEN
!        CALL message('','ldiur =.TRUE.  --> diurnal cycle on')
!      ELSE
!        CALL message('','ldiur =.FALSE. --> diurnal cycle off')
!      ENDIF
!      !
!      ! --- Check for diagnosis of instantaneous aerosol radiative forcing
!      ! 
!      CALL message('','instantaneous forcing diagnostic:')
!      WRITE (message_text,'(a16,L3,a18,L3)')       &
!           ' solar radiation: ',   lradforcing(1), &
!           ' thermal radiation: ', lradforcing(2)
!      CALL message('',message_text)
!      !
!      ! --- Check perpetual orbit
!      ! 
!      IF (yr_perp.NE.-99999)  lyr_perp = .TRUE.
!      CALL p_bcast (lyr_perp, p_io)
!
!      IF (lyr_perp) THEN
!        IF (l_orbvsop87) THEN
!          WRITE (message_text, '(a,i0,a)') &
!               'yr_perp=', yr_perp, ' --> perpetual year for orbit'
!          CALL message('',message_text)
!        ELSE
!          WRITE (message_text, '(a,i0,a,l1,a)') &
!               'yr_perp = ', yr_perp, ' l_orbvsop87 = ',l_orbvsop87,' not allowed!'
!          CALL message('',message_text)
!          CALL finish('setup_radiation', &
!               ' yr_perp.ne.-99999 cannot run  PCMDI-orbit (l_orbvsop87=.F.).')
!        END IF
!      END IF
!      !
!      ! 4.0 Initialization for radiation
!      ! -------------------------------
!      !
!      ! --- resolution/run dependent cloud optical parameters (tuning)
!      !
!      CALL setup_cloud_optics
!      !
!      ! --- Ozone climatology
!      ! 
!      IF (io3==3) CALL read_o3clim_3
!      !
!    ELSE
!      CALL message('','lrad = .FALSE. --> no radiation')
!      co2mmr = co2vmr*amco2/amd
!    ENDIF


  END SUBROUTINE setup_radiation


  SUBROUTINE pre_radiation

    LOGICAL  :: l_rad_call, l_write_solar
    INTEGER  :: icurrentyear, icurrentmonth, iprevmonth, i
    REAL(wp) :: rasc_sun, decl_sun, dist_sun, time_of_day, zrae
    REAL(wp) :: solcm
    !
    ! 1.0 Compute orbital parameters for current time step
    ! --------------------------------
!    l_rad_call = .FALSE.
!    CALL get_orbit_times(l_rad_call, lyr_perp, nmonth, yr_perp, time_of_day, &
!         &               orbit_date)
!
!    IF (l_orbvsop87) THEN 
!      CALL orbit_vsop87 (orbit_date, rasc_sun, decl_sun, dist_sun)
!    ELSE
!      CALL orbit_kepler (orbit_date, rasc_sun, decl_sun, dist_sun)
!    END IF
!    decl_sun_cur = decl_sun       ! save for aerosol and chemistry submodels
!    CALL solar_parameters(decl_sun, dist_sun, time_of_day, &
!         &                sinlon_2d, sinlat_2d, coslon_2d, coslat_2d, &
!         &                flx_ratio_cur, amu0_x, rdayl_x)
    SELECT CASE (isolrad)
    CASE (0)
      solc = SUM(ssi_default)
!    CASE (1)
!      CALL get_solar_irradiance(current_date, next_date)
!      CALL set_solar_irradiance(solc)
    CASE (2)
      solc = SUM(ssi_preind)
    CASE (3)
      solc = SUM(ssi_amip)
    CASE default
      WRITE (message_text, '(a,i2,a)') &
           'isolrad = ', isolrad, ' in radctl namelist is not supported'
      CALL message('pre_radiation', message_text)
    END SELECT
    psct = flx_ratio_cur*solc
    !
    ! 2.0 Prepare time dependent quantities for rad (on radiation timestep)
    ! --------------------------------
!    IF (lrad .AND. l_trigrad) THEN
    IF (.true.) THEN
!      l_rad_call = .TRUE.
!      CALL get_orbit_times(l_rad_call, lyr_perp, nmonth, yr_perp, time_of_day , &
!           &               orbit_date)
!
!      IF ( l_orbvsop87 ) THEN 
!        CALL orbit_vsop87 (orbit_date, rasc_sun, decl_sun, dist_sun)
!      ELSE
!        CALL orbit_kepler (orbit_date, rasc_sun, decl_sun, dist_sun)
!      END IF
!      CALL solar_parameters(decl_sun, dist_sun, time_of_day, &
!           &                sinlon_2d, sinlat_2d, coslon_2d, coslat_2d, &
!           &                flx_ratio_rad ,amu0m_x, rdaylm_x)
!      ! consider curvature of the atmosphere for high zenith angles
!      zrae = rae*(rae+2.0_wp)
!      amu0m_x(:,:)  = rae/(SQRT(amu0m_x(:,:)**2+zrae)-amu0m_x(:,:))
!      !jsr&hs: For the calculation of radiative transfer, a maximum zenith angle
!      !        of about 84 degrees is applied in order to avoid to much overshooting
!      !        when the extrapolation of the radiative fluxes from night time
!      !        regions to daytime regions is done for time steps at which no
!      !        radiation calculation is performed. This translates into cosines
!      !        of the zenith angle > 0.1.
!      !        This approach limits the calculation of the curvature effect
!      !        above, and may have to be reconsidered when radiation is cleaned
!      !        up.
!      amu0m_x(:,:) = MAX(amu0m_x(:,:),0.1_wp)
!      !
!      ! --- Prepare Ozone climatology
!      !
!      SELECT CASE (io3)
!      CASE (3) 
!        CALL pre_o3clim_3(nmonth)
!      CASE (4) 
!        CALL pre_o3clim_4
!      END SELECT
      !

    !++jsr&hs
    ! 3.0 Prepare possibly time dependent total solar and spectral irradiance
    ! --------------------------------
    ! ATTENTION: 
    ! This part requires some further work. Currently, a solar constant of
    ! 1361.371 is used as default. This is the TSI averaged over the
    ! years 1979 to 1988, and should be used for AMIP type runs. If lcouple is
    ! true, a solar constant of 1360.875 is used, the average for the years 1844
    ! to 1856. This should be used for a preindutrial control run.
    ! The spectral distribution of this TSI is currently also prescribed for
    ! these two cases depending on the lcouple switch.
    ! For transient CMIP5 simulations, the available time
    ! varying TSI and SSI has to be read in and used here.

    SELECT CASE (isolrad)
    CASE (0)
      solcm = SUM(ssi_default)
      ssi_factor = ssi_default
!    CASE (1)
!      CALL get_solar_irradiance_m(prev_radiation_date, radiation_date, nb_sw)
!      CALL set_solar_irradiance_m(solcm, ssi_factor, nb_sw)
    CASE (2)
      solcm = SUM(ssi_preind)
      ssi_factor = ssi_preind
    CASE (3)
      solcm = SUM(ssi_amip)
      ssi_factor = ssi_amip
    CASE default
      WRITE (message_text, '(a,i2,a)') &
           'isolrad = ', isolrad, ' in radctl namelist is not supported'
      CALL message('pre_radiation', message_text)
    END SELECT
    flx_ratio_rad = 1._wp
    psctm = flx_ratio_rad*solcm
    ssi_factor(:) = ssi_factor(:)/solcm

    ! output of solar constant every month

!    CALL get_date_components(current_date, month=icurrentmonth, &
!         year=icurrentyear)
!    CALL get_date_components(previous_date, month=iprevmonth)
!    l_write_solar = icurrentmonth/=iprevmonth
!    IF (l_write_solar .OR. lresume .OR. lstart) THEN
!      CALL message('','')
!      WRITE (message_text,'(a,i0,a,i2.2,a,f6.1)') &
!           'Total solar constant [W/m^2] for ',      &
!           icurrentyear, '-',                        & 
!           icurrentmonth, ' = ', solc
!      CALL message('',message_text)
!      CALL message('','')
!      DO i = 1, nb_sw
!        WRITE (message_text,'(a,i2,a,f7.5)') &
!             '   solar constant fraction: band ', i, &
!             ' = ', ssi_factor(i)
!        CALL message('',message_text)
!      END DO
!    END IF

    !--jsr&hs
    END IF

  END SUBROUTINE pre_radiation

  SUBROUTINE init_vct
    REAL(WP) :: VCT(96) 
    vct(1) = 0._wp
    vct(2) = 1.98918533325195_wp
    vct(3) = 6.57208919525146_wp
    vct(4) = 15.6739025115967_wp
    vct(5) = 30.6242828369141_wp
    vct(6) = 54.5457153320312_wp
    vct(7) = 92.558837890625_wp
    vct(8) = 150.504699707031_wp
    vct(9) = 235.327453613281_wp
    vct(10) = 356.100341796875_wp
    vct(11) = 523.91943359375_wp
    vct(12) = 751.04296875_wp
    vct(13) = 1051.13720703125_wp
    vct(14) = 1438.98852539062_wp
    vct(15) = 1930.17724609375_wp
    vct(16) = 2540.69702148438_wp
    vct(17) = 3286.55297851562_wp
    vct(18) = 4199.57421875_wp
    vct(19) = 5303.95703125_wp
    vct(20) = 6624.703125_wp
    vct(21) = 8187.18359375_wp
    vct(22) = 9976.13671875_wp
    vct(23) = 11820.5390625_wp
    vct(24) = 13431.390625_wp
    vct(25) = 14736.359375_wp
    vct(26) = 15689.2109375_wp
    vct(27) = 16266.609375_wp
    vct(28) = 16465._wp
    vct(29) = 16297.62109375_wp
    vct(30) = 15791.6015625_wp
    vct(31) = 14985.26953125_wp
    vct(32) = 13925.51953125_wp
    vct(33) = 12665.2890625_wp
    vct(34) = 11261.23046875_wp
    vct(35) = 9771.40625_wp
    vct(36) = 8253.2109375_wp
    vct(37) = 6761.33984375_wp
    vct(38) = 5345.9140625_wp
    vct(39) = 4050.71801757812_wp
    vct(40) = 2911.56909179688_wp
    vct(41) = 1954.80493164062_wp
    vct(42) = 1195.88989257812_wp
    vct(43) = 638.14892578125_wp
    vct(44) = 271.62646484375_wp
    vct(45) = 72.0635986328125_wp
    vct(46) = 0._wp
    vct(47) = 0._wp
    vct(48) = 0._wp
    vct(49) = 0._wp
    vct(50) = 0._wp
    vct(51) = 0._wp
    vct(52) = 0._wp
    vct(53) = 0._wp
    vct(54) = 0._wp
    vct(55) = 0._wp
    vct(56) = 0._wp
    vct(57) = 0._wp
    vct(58) = 0._wp
    vct(59) = 0._wp
    vct(60) = 0._wp
    vct(61) = 0._wp
    vct(62) = 0._wp
    vct(63) = 0._wp
    vct(64) = 0._wp
    vct(65) = 0._wp
    vct(66) = 0._wp
    vct(67) = 0._wp
    vct(68) = 0._wp
    vct(69) = 0._wp
    vct(70) = 0.000400000018998981_wp
    vct(71) = 0.00289999996311963_wp
    vct(72) = 0.00919999927282333_wp
    vct(73) = 0.0203000009059906_wp
    vct(74) = 0.0370000004768372_wp
    vct(75) = 0.0595000013709068_wp
    vct(76) = 0.0878999829292297_wp
    vct(77) = 0.121999979019165_wp
    vct(78) = 0.161400020122528_wp
    vct(79) = 0.205699980258942_wp
    vct(80) = 0.254199981689453_wp
    vct(81) = 0.30620002746582_wp
    vct(82) = 0.361100018024445_wp
    vct(83) = 0.418200016021729_wp
    vct(84) = 0.476700007915497_wp
    vct(85) = 0.535899996757507_wp
    vct(86) = 0.595099985599518_wp
    vct(87) = 0.653599977493286_wp
    vct(88) = 0.710600018501282_wp
    vct(89) = 0.765399992465973_wp
    vct(90) = 0.817200005054474_wp
    vct(91) = 0.865000009536743_wp
    vct(92) = 0.907700002193451_wp
    vct(93) = 0.944199979305267_wp
    vct(94) = 0.97299998998642_wp
    vct(95) = 0.992299973964691_wp
    vct(96) = 1._wp

    vct_a = vct( 1:48)
    vct_b = vct(49:96)
  end subroutine init_vct
END PROGRAM radiation


