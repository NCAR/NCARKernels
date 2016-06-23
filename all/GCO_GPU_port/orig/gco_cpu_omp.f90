module cpu_get_close_obs_m
use parameter_m
use omp_lib
contains

subroutine cpu_gco(base_obs_loc,obs,cpucindx,cpucdist,cpunum,cmaxdist,cputime,cpusd)

integer,intent(out)::cpucindx(:),cpunum
real(fp_kind),intent(out)::cpucdist(:)
real(fp_kind),intent(in)::cmaxdist
integer::i
type(location_type),intent(in)::base_obs_loc,obs(:)
real::cputime,cpusd,time(cpurun)
!real::t(2)
integer*8::t1,t2,r
do i=1,cpurun

cpucindx=0
cpucdist=0.0d0
cpunum=0
!call cpu_time(t(1))
call SYSTEM_CLOCK(t1,r)
call sequential_extract(base_obs_loc,obs,cpucindx,cpucdist,cpunum,cmaxdist)
call SYSTEM_CLOCK(t2,r)
!call cpu_time(t(2))
!time(i)=t(2)-t(1)
time(i)=(t2-t1)/real(r)
enddo


cputime=sum(time)/cpurun
cpusd=0.0
do i=1,cpurun
cpusd=(time(i)-cputime)**2+cpusd
enddo 
cpusd=sqrt(cpusd/cpurun)
write(*,*) 'sequential time(ms): ',cputime*1000.0
write(*,*) 'sequential sd(ms): ',cpusd*1000.0
write(*,*) 'cpu num',cpunum

end subroutine cpu_gco

!*******************************CPU Version******************************************
!------------------CPU Sequential Extract-----------------

subroutine sequential_extract(base_obs_loc,obs,cindxc,cdistc,cnumc,cmaxdist)
integer,intent(out)::cindxc(:),cnumc
real(fp_kind),intent(out)::cdistc(:)
real(fp_kind),intent(in)::cmaxdist
real(fp_kind),allocatable::distc(:)
integer::i
type(location_type),intent(in)::base_obs_loc,obs(:)
cnumc=0
cdistc=0.0_fp_kind
cindxc=0
allocate(distc(length))
!call omp_set_num_threads(ompthreads)
!!dec$ attribute inline::sequential_dist
!!$OMP PARALLEL SHARED(distc,base_obs_loc,obs)
!write(*,*) OMP_GET_NUM_PROCS(),OMP_GET_NUM_THREADS(),"thread:",OMP_GET_THREAD_NUM()
!!$OMP DO 
do i=1,length
distc(i)=sequential_dist(base_obs_loc,obs(i))
enddo
!!$OMP END DO NOWAIT

!!$OMP END PARALLEL




do i=1,length
if(distc(i)<=cmaxdist)then
cnumc=cnumc+1
cindxc(cnumc)=i
cdistc(cnumc)=distc(i)
endif
enddo
deallocate(distc)

end subroutine sequential_extract


!-------------------CPU Distance-------------------------

function sequential_dist(loc1,loc2)

type(location_type),intent(in)::loc1,loc2
real(fp_kind)::sequential_dist
real(fp_kind)::lon_dif,rtemp
lon_dif=loc1%lon-loc2%lon
   if(abs(loc1%lat) >= PI/2.0_fp_kind .or. abs(loc2%lat) >= PI/2.0_fp_kind .or. &
      lon_dif == 0.0_fp_kind) then
      sequential_dist = abs(loc2%lat - loc1%lat)
   else
      ! This test is for apparent roundoff error which may be a result of
      ! running r8 == r4. 
      rtemp = sin(loc2%lat) * sin(loc1%lat) + &
         cos(loc2%lat) * cos(loc1%lat) * cos(lon_dif)
      if (rtemp < -1.0_fp_kind) then
         sequential_dist = PI
      else if (rtemp > 1.0_fp_kind) then
         sequential_dist = 0.0_fp_kind
      else
         sequential_dist = acos(rtemp)
      endif
   endif
end function sequential_dist

end module cpu_get_close_obs_m
