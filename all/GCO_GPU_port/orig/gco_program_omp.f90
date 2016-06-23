
!*************************************************************************
!*************************************Program**************************************
program Prefix_Sum
use omp_lib
use parameter_m
use cpu_get_close_obs_m
!use gpu_get_close_obs_m

implicit none

!real:: gputime,gpusd
real::cputime,cpusd

integer::i,num,cpunum,istat

integer,allocatable::cpucindx(:)
double precision,allocatable::cdist(:)
integer,allocatable::cindx(:)
double precision,allocatable::cpucdist(:)
double precision::cmaxdist
type(location_type)::base_obs_loc
type(location_type),allocatable::obs(:)
double precision,allocatable::lon(:),lat(:)
call open_files

cmaxdist=2.0_fp_kind

allocate(obs(length))
allocate(lon(length))
allocate(lat(length))
allocate(cpucdist(length))
allocate(cpucindx(length))

allocate(cindx(length))
allocate(cdist(length))

!call omp_set_num_threads(ompthreads)

!!$OMP PARALLEL 

!obs%lon=0.0
!obs%lat=0.0
!cpucindx=0.0
!cpucdist=0.0
!!$OMP END PARALLEL



call read_file(obs,lon,lat)



base_obs_loc%lat=40.63913947_fp_kind/180.0_fp_kind*PI
base_obs_loc%lon=250.5818377_fp_kind/180.0_fp_kind*PI

call cpu_gco(base_obs_loc,obs,cpucindx,cpucdist,cpunum,cmaxdist,cputime,cpusd)
!call gpu_gco(base_obs_loc,lon,lat,cindx,cdist,num,cmaxdist,gputime,gpusd)


!write(*,*) 'speedup (cputime/gputime): ',(cputime*1000.0)/gputime

!write(*,*) "max indxError: ",MAXVAL(cpucindx-cindx)
!write(*,*) "min indxError: ",MINVAL(cpucindx-cindx)
!write(*,*) "max distError: ",MAXVAL(cpucdist-cdist)
!write(*,*) "min distError: ",MINVAL(cpucdist-cdist)

!if(ofile) call output2file(cpunum,cpucindx,cpucdist,num,cindx,cdist,cputime,cpusd,gputime,gpusd)


deallocate(obs)
deallocate(lon,lat)
deallocate(cpucindx)
deallocate(cpucdist)
deallocate(cindx)
deallocate(cdist)

!istat=cudaDeviceReset()
end program Prefix_Sum
