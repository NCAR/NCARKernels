module parameter_m


implicit none


integer::sn! kernel"gpu_scan" ,constant
parameter (sn=1)
integer::si!constant memory, kernel"extract",constant
parameter (si=1)
integer::length!data length, number of observation locations,constant
integer::TW!tile width(shared memory size factor),constant
parameter (length=2*1024*1024)
parameter (TW=1024/si)

!integer,parameter::ompthreads=16
integer,parameter::cpurun=100

!integer,parameter::gpurun=100

integer,parameter::ofile=0
character (len=*)::iname,oname,rname
parameter (iname="../data/Rand_Input_2M.txt") !input file name, change for different size of input
parameter (oname="omp_2M_Output.txt")
parameter (rname="omp_2M_ResultError.txt")

integer,parameter::singlePrecision=kind(0.0)
integer,parameter::doublePrecision=kind(0.0d0)
	integer,parameter::fp_kind=doublePrecision
	!integer,parameter::fp_kind=singlePrecision

!integer,parameter::TPB=256
!double precision,constant::PI
!parameter(PI=3.14159265358979323846_fp_kind)
!double precision,constant::maxdist!maximum distance to define close obs
    
integer,parameter::TPB=256
double precision::PI
parameter(PI=3.14159265358979323846_fp_kind)
double precision::maxdist!maximum distance to define close obs


!
!location type
type location_type
double precision::lon,lat,vloc
end type location_type

contains
subroutine open_files
open(unit=10,file=iname)
open(unit=20,file=oname)
open(unit=30,file=rname)
end subroutine open_files


subroutine read_file(obs,lon,lat)
type(location_type)::obs(:)
double precision::lon(:),lat(:)
integer::i,ierr,a
read(10,*)
do i=1,length
read(10,*,iostat=ierr) a,obs(i)%lon,obs(i)%lat
lon(i)=obs(i)%lon
lat(i)=obs(i)%lat
enddo
end subroutine read_file


subroutine output2file(cpunum,cpuindx,cpudist,gpunum,gpuindx,gpudist,cputime,cpusd,gputime,gpusd)
double precision::cpudist(:),gpudist(:)
real::cputime,cpusd
real::gputime,gpusd
integer::cpunum,gpunum,cpuindx(:),gpuindx(:),i
!write(20,*)"Output File"
write(30,*)"Result Error File"

if((cpunum-gpunum)==0)then
write(30,*) "CPU number of close location points",cpunum
write(30,*) "GPU number of close location points",gpunum
write(30,*) "******************************************"
write(30,*) "CPU overall time (ms)",cputime*1000.0
write(30,*) "CPU overall sd (ms)",cpusd*1000.0
write(30,*) "******************************************"
write(30,*) "GPU overall time (ms)",gputime
write(30,*) "GPU overall sd (ms)",gpusd
write(30,*) "******************************************"
write(20,*) "CPUindx,GPUindx,CPUdist,GPUdist"
do i=1,cpunum
write(20,*) cpuindx(i),gpuindx(i)
write(20,*) cpudist(i),gpudist(i)
enddo
write(30,*) "max indxError: ",MAXVAL(cpuindx-gpuindx)
write(30,*) "min indxError: ",MINVAL(cpuindx-gpuindx)
write(30,*) "max distError: ",MAXVAL(cpudist-gpudist)
write(30,*) "min distError: ",MINVAL(cpudist-gpudist)
write(30,*) "******************************************"
write(30,*) "Speedup",(cputime*1000.0)/gputime


else
write(*,*) "CPU & GPU results don't match!"
endif

end subroutine output2file

end module parameter_m
