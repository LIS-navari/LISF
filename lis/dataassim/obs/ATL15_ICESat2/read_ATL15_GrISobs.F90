!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2022 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"
!BOP
! !ROUTINE: read_ATL15_GrISobs
! \label{read_ATL15_GrISobs}
!
! !REVISION HISTORY: 
!  13 Dec 2023    Mahdi Navari;   Initial Specification
!
! !INTERFACE: 
subroutine read_ATL15_GrISobs(n,k, OBS_State, OBS_Pert_State)
! !USES: 
  use ESMF
  use LIS_mpiMod
  use LIS_coreMod
  use LIS_logMod
  use LIS_timeMgrMod
  use ATL15_GrISobs_module
  use LIS_fileIOMod
  use LIS_DAobservationsMod
  use LIS_constantsMod, only : LIS_CONST_PATH_LEN
  use ATL15_GrISobs_module

#if(defined USE_NETCDF3 || defined USE_NETCDF4)
  use netcdf
#endif

  implicit none
! !ARGUMENTS: 
  integer, intent(in) :: n
  integer, intent(in) :: k
  type(ESMF_State)    :: OBS_State
  type(ESMF_State)    :: OBS_Pert_State
!
! !DESCRIPTION:
!  
!  reads the ATL15_GrIS height-change observations 
!  The processed data is packaged into an ESMF State
!  for later use within the DA algorithm
!  
!  The arguments are: 
!  \begin{description}
!  \item[n]                index of the nest
!  \item[k]                index of the data assimilation instance
!  \item[OBS\_State]       observations state
!  \item[OBS\_Pert\_State] observations perturbation state
!  \end{description}
!
!EOP
  type(ESMF_Field)    :: dhdtfield
  type(ESMF_Time)     :: startdate, currentTime
!  integer             :: iret

  real,    pointer    :: obsl(:)
  integer             :: gid(LIS_rc%obs_ngrid(k))
  integer             :: assimflag(LIS_rc%obs_ngrid(k))
  character(len=LIS_CONST_PATH_LEN) :: ATL15_GrISobsdir ! BMc, change 200 to LIS_CONST_PATH_LEN
  character(len=LIS_CONST_PATH_LEN) :: name ! BMc, change 200 to LIS_CONST_PATH_LEN
  logical             :: file_exists
!  integer             :: col,row
  logical             :: data_upd
  logical             :: data_upd_flag_local
  logical             :: data_upd_flag(LIS_npes)
  integer             :: fnd
  logical             :: readflag
  logical             :: alarmCheck
  integer             :: status !,ftn
!  real, allocatable   :: ssdev(:)
  integer             :: t,p,r,c
!  integer             :: nc, nr, num_obs
  integer             :: dimid, ncid, varid, grp_ncid
  integer             :: leng
  real, allocatable   ::obs_time(:)
  real, allocatable   ::dhdt(:,:,:) ! "time y x" but Fortran reads in reverse order (x,y,time)
  real, allocatable   ::dhdt_local_grid(:,:,:)
  integer             :: yy,mm,dd,h,m,s
  integer             :: doy,ts
  real                :: gmt
  real*8              :: timenow
  real*8              :: start_date, start_date_tmp
  integer             :: offset
  real*8              :: start_date_sec, simulation_start_time_sec, timenow_sec, start_date_new_sec
  real*8              :: LIS_twStartTime_sec, LIS_twStopTime_sec
  integer             :: yr, mo, da, hr, mn, ss, ms

  call ESMF_AttributeGet(OBS_State,"Data Directory",&
       ATL15_GrISobsdir, rc=status)
  call LIS_verify(status)

  call ESMF_AttributeSet(OBS_State,"Data Update Status",&
       .false., rc=status)
  call LIS_verify(status)

  call ATL15_GrIS_filename(name,ATL15_GrISobsdir)

  inquire(file=name,exist=file_exists)
  if(file_exists) then
     call ESMF_AttributeSet(OBS_State,"File Status",&
          .true., rc=status)
     call LIS_verify(status)
  else
     call ESMF_AttributeSet(OBS_State,"File Status",&
          .false., rc=status)
     call LIS_verify(status)
     write(LIS_logunit,*)  '[ERR] ATL15 hight change data',trim(name)
     write(LIS_logunit,*)  'is missing. Please check the path and ATL15 file name'
     call LIS_endrun()
  endif

! dh  observations
     !2018-10-01 22:30:00.00   2019-01-01 06:00:00.00   2019-04-02 13:30:00.00   2019-07-02 21:00:00.00
     !2019-10-02 04:30:00.00   2020-01-01 12:00:00.00   2020-04-01 19:30:00.00   2020-07-02 03:00:00.00
     !2020-10-01 10:30:00.00   2020-12-31 18:00:00.00   2021-04-02 01:30:00.00   2021-07-02 09:00:00.00


! TODO add a condition to check the CROCUS81_struc(n)%ts == 15mn otherwise the model bypass the alarmCheck
!      we do not need to add condition. Changed the start_date to 2018,11,16,14,0,0

! ATL15 time "days since 2018-01-01".
! First and second observations for fo dh are @ 2018-10-01 22:30 and 2019-01-01 06:00
! First observation for dhdt is @ 2018-11-16 14:15 ( in the middle of the 1st and 2st dh obervation times)
! Time interval is 91.3125 days or 7889400 seconds

  yy = LIS_rc%yr
  mm = LIS_rc%mo
  dd = LIS_rc%da
  h  = LIS_rc%hr
  m  = LIS_rc%mn
  s  = 0 ! LIS_rc%sss

  call LIS_date2time(timenow,doy,gmt,yy,mm,dd,h,m,s)
  call LIS_compute_time_since_millennium(LIS_rc%yr, LIS_rc%mo, LIS_rc%da, LIS_rc%hr, LIS_rc%mn, 0, timenow_sec)
! Note: When observations become available, this reader reads the observation and
!       sets the assimilation flag to TRUE. Therefore, we set the start date to
!       the second dh observation (e.g., 2019-01-01 06:00:00.00). Crocus81_dhdt_DAlogMod
!       computes the dh at the observation times and then the BPS assimilates the observation.

! call LIS_date2time(start_date,doy,gmt,2019,01,01,6,0,0)
! set this to second dh obs (after 2nd dh obs read first dhdt)   
! for testing code chnage this date for 2019,01,01,6,0,0 to 2018,10,01,23,0,0
! TODO change to 2019,01,01,6,0,0 after test
  call LIS_date2time(start_date,doy,gmt,2018,10,01,23,00,0)
  call LIS_compute_time_since_millennium(2018,10,01,23,00,0, start_date_sec)
print*,'read_ATL15 timenow_sec , start_date_sec ,diff'
print '(1x,f20.4, 2x,f20.4, 2x,f10.2)', timenow_sec , start_date_sec , timenow_sec-start_date_sec

   ! reset timewindow when simulation reaches the ATL15_StartTime
   ! then reset the tw in the Crocus81_setparticleweight.F90 af DA
   if (timenow_sec .eq. (start_date_sec - LIS_rc%obsInterval)) then 
      !reset timewindow
      call LIS_resetClockForPBSTimeWindow(LIS_rc)
   end if

   ! reset timewindow when simulation start time is larger then ATL15_StartTime and simulation reaches first ATL15 observation
   call LIS_compute_time_since_millennium(LIS_rc%syr,LIS_rc%smo,LIS_rc%sda,LIS_rc%shr,LIS_rc%smn,0,simulation_start_time_sec)
   if ((simulation_start_time_sec .gt. (start_date_sec - LIS_rc%obsInterval)) .and. &
       mod((timenow_sec - (start_date_sec - LIS_rc%obsInterval)) , LIS_rc%obsInterval) == 0 .and. &
          ((timenow_sec - simulation_start_time_sec) .lt. LIS_rc%obsInterval)) then
       !reset timewindow
      call LIS_resetClockForPBSTimeWindow(LIS_rc)
   end if
! For print1 
   call ESMF_TimeGet(LIS_twStartTime, yy = yr, &
             mm = mo, &
             dd = da, &
             h  = hr, &
             m  = mn,&
             s  = ss, &
             calendar = LIS_calendar, &
             rc = status)
   call LIS_compute_time_since_millennium(yr,mo,da,hr,mn,0,LIS_twStartTime_sec)

   call ESMF_TimeGet(LIS_twStopTime, yy = yr, &
             mm = mo, &
             dd = da, &
             h  = hr, &
             m  = mn,&
             s  = ss, &
             calendar = LIS_calendar, &
             rc = status)
      call LIS_compute_time_since_millennium(yr,mo,da,hr,mn,0,LIS_twStopTime_sec)
print*, ' Read ATL15 twStartTime , twStopTime'
print '(1x,f20.4, 2x,f20.4)', LIS_twStartTime_sec , LIS_twStopTime_sec  
! end for print1   
 
  !alarmcheck = (mod(currentTime-startdate, 7889400.0).eq.0)
  alarmcheck = (mod(timenow_sec-start_date_sec, LIS_rc%obsInterval).eq.0)

 !if(alarmCheck .and. currentTime.ge.start_date_tmp ) then
  if(alarmCheck .and. (timenow .ge. start_date)) then ! _tmp ) then

      data_upd = .false.

      !if(LIS_rc%DAincrMode(n).eq.1) then  ! TODO what is this? hard codded to 1 in read_config
      ! This falg is used to run the EnBS for GRACE when LIS_rc%DAincrMode(n).eq.1 and then apply
      ! the increment in the sencond iteration when the LIS_rc%DAincrMode(n).eq.0 or otherway around.

      call ESMF_AttributeSet(OBS_State,"File Status",&
            .true., rc=status)
      call LIS_verify(status)

      write(LIS_logunit,*)  '[INFO] Reading ATL15_GrIS data ',trim(name)

#if ( defined USE_NETCDF3 || defined USE_NETCDF4 )
      call LIS_verify(nf90_open(path=trim(name), &
           mode=NF90_NOWRITE, &
           ncid=ncid), &
           '[ERR] Error in nf90_open for '//trim(name))
      
      call LIS_verify(nf90_inq_grp_ncid(ncid, "dhdt_lag1", grp_ncid),&
           'nf90_inq_grp_ncid failed in dhdt_lag1, read_ATL15_GrIS')

      call LIS_verify(nf90_inq_dimid(grp_ncid, "x",dimid),&
           'nf90_inq_dimid failed in x, read_ATL15_GrIS')
      call LIS_verify(nf90_inquire_dimension(grp_ncid, dimid, len=leng),&
           'nf90_inquire_dimension failed in x, read_ATL15_GrIS')
      ATL15_GrIS_struc(n)%nc = leng

      call LIS_verify(nf90_inq_dimid(grp_ncid, "y",dimid),&
           'nf90_inq_dimid failed in y, read_ATL15_GrIS')
      call LIS_verify(nf90_inquire_dimension(grp_ncid, dimid, len=leng),&
           'nf90_inquire_dimension failed in y, read_ATL15_GrIS')
      ATL15_GrIS_struc(n)%nr = leng

      call LIS_verify(nf90_inq_dimid(grp_ncid, "time",dimid),&
           'nf90_inq_dimid failed in time, read_ATL15_GrIS')
      call LIS_verify(nf90_inquire_dimension(grp_ncid, dimid, len=leng),&
           'nf90_inquire_dimension failed in time, read_ATL15_GrIS')
      ATL15_GrIS_struc(n)%num_obs = leng

      allocate(obs_time(ATL15_GrIS_struc(n)%num_obs))
      allocate(dhdt(ATL15_GrIS_struc(n)%nc,ATL15_GrIS_struc(n)%nr,ATL15_GrIS_struc(n)%num_obs))
      allocate(dhdt_local_grid(LIS_rc%obs_lnc(k),LIS_rc%obs_lnr(k),ATL15_GrIS_struc(n)%num_obs))

      call LIS_verify(nf90_inq_varid(grp_ncid,'time',varid), &
           'nf90_inq_varid failed in time, read_ATL15_GrIS')
      call LIS_verify(nf90_get_var(grp_ncid,varid,obs_time), &
           'nf90_get_var failed in time, read_ATL15_GrIS')

      call LIS_verify(nf90_inq_varid(grp_ncid,'dhdt',varid), &
          'nf90_inq_varid failed in dhdt, read_ATL15_GrIS')
      call LIS_verify(nf90_get_var(grp_ncid,varid,dhdt), &
          'nf90_get_var failed in dhdt, read_ATL15_GrIS')
      call LIS_verify(nf90_close(ncid),&
           'nf90_close failed in read_ATL15_GrIS')
#endif

! TODO for PBS: How to store all obs in assimilation window? We do not need to data that. We assimiliate one obs at a time and them in the setPW.F90 take care of # of obs in assim window.

      dhdt_local_grid = dhdt(&
             LIS_ews_obs_halo_ind(n,LIS_localPet+1):&
             LIS_ewe_obs_halo_ind(n,LIS_localPet+1), &
             LIS_nss_obs_halo_ind(n,LIS_localPet+1): &
             LIS_nse_obs_halo_ind(n,LIS_localPet+1),:)

     !dhdt_local_grid = dhdt_local_grid / 4  ! "meters years^-1." to meter/3monthly 
     dhdt_local_grid = dhdt_local_grid / (4*91.25*12) !  2-hourly   
!-------------------------------------------------------------------------
!  Extract data for the current time
!-------------------------------------------------------------------------     
      call ESMF_StateGet(OBS_State,"Observation01",dhdtfield,&
           rc=status)
      call LIS_verify(status, 'ESMF_StateGet failed in read_ATL15_GrISobs')

      call ESMF_FieldGet(dhdtfield,localDE=0,farrayPtr=obsl,rc=status)
      call LIS_verify(status,'ESMF_FieldGet failed in read_ATL15_GrISobs')

      obsl(:) = -9999.0

      !offset = int((currentTime - startTime)/ATL15_GrIS_struc(n)%ts) + 1 ! nint
      !offset = floor((timenow - start_date)/7889400.0) + 1 ! nint
      offset = floor((timenow_sec - start_date_sec)/LIS_rc%obsInterval) + 1 ! nint
      ! we might be able to use obs_time insted of offset.   

      ! fnd: flag to indicate if there are valid observations in the local 
      !      processor's domain

      if(offset.gt.0) then

          do r=1,LIS_rc%obs_lnr(k)
             do c=1,LIS_rc%obs_lnc(k)
                if(dhdt_local_grid(c,r,offset).gt.100) then ! dhdt:_FillValue = 3.402823e+38f ;
                   dhdt_local_grid(c,r,offset) = LIS_rc%udef
                endif
             enddo
          enddo

          fnd = 0
          !data_upd_flag_local = .false. 
          do r=1,LIS_rc%obs_lnr(k)
             do c=1,LIS_rc%obs_lnc(k)
                if(dhdt_local_grid(c,r,offset).ne.LIS_rc%udef) then
                   fnd = 1
                endif
             enddo
          enddo

          if(fnd.eq.0) then
             obsl = LIS_rc%udef
          else
             do r=1,LIS_rc%obs_lnr(k)
                do c=1,LIS_rc%obs_lnc(k)
                   if(LIS_obs_domain(n,k)%gindex(c,r).ne.-1) then
                      if(dhdt_local_grid(c,r,offset).gt.0.0) then
                         obsl(LIS_obs_domain(n,k)%gindex(c,r)) = dhdt_local_grid(c,r,offset)
                      end if
                   endif
                end do
             end do
          endif

          deallocate(obs_time)
          deallocate(dhdt)
          deallocate(dhdt_local_grid)

! TODO: do we need LSM based quality control and screening of observations
     !call lsmdaqcobsstate(trim(LIS_rc%lsm)//"+"&
     !     //trim(LIS_ATL15_GrISobsId)//char(0),n, k, OBS_state)
     !call LIS_checkForValidObs(n,k,obsl,fnd,dhdt_current)

          if(fnd.eq.0) then
             data_upd_flag_local = .false.
          else
             data_upd_flag_local = .true.
          endif
#if(defined SPMD)
          call MPI_ALLGATHER(data_upd_flag_local,1,&
             MPI_LOGICAL, data_upd_flag(:),&
             1, MPI_LOGICAL, LIS_mpi_comm, status)
#endif
          data_upd = .false.
          do p=1,LIS_npes
             data_upd = data_upd.or.data_upd_flag(p)
          enddo

          if(data_upd) then
             do t=1,LIS_rc%obs_ngrid(k)
                gid(t) = t
                if(obsl(t).ne.-9999.0) then
                   assimflag(t) = 1
                else
                   assimflag(t) = 0
                endif
             enddo

             call ESMF_AttributeSet(OBS_State,"Data Update Status",&
                  .true., rc=status)
             call LIS_verify(status)


             if(LIS_rc%obs_ngrid(k).gt.0) then
                call ESMF_AttributeSet(dhdtfield,"Grid Number",&
                     gid,itemCount=LIS_rc%obs_ngrid(k),rc=status)
                call LIS_verify(status)


                call ESMF_AttributeSet(dhdtfield,"Assimilation Flag",&
                     assimflag,itemCount=LIS_rc%obs_ngrid(k),rc=status)
                call LIS_verify(status)
             endif

          else
             call ESMF_AttributeSet(OBS_State,"Data Update Status",&
                 .false., rc=status)
             call LIS_verify(status)
          endif ! data_upd
      else
         call ESMF_AttributeSet(OBS_State,"Data Update Status",&
              .false., rc=status)
         call LIS_verify(status)
         return
      endif ! offset
  else
     call ESMF_AttributeSet(OBS_State,"Data Update Status",&
          .false., rc=status)
     call LIS_verify(status)
     return
  endif !  alarmCheck
end subroutine read_ATL15_GrISobs

subroutine ATL15_GrIS_filename(name, ndir)

  implicit none
  character(len=*)  :: name
  character (len=*) :: ndir
  name = trim(ndir)//'/ATL15_GL_0318_20km_003_01.nc'
end subroutine ATL15_GrIS_filename






