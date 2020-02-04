!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"
!BOP
!
! !ROUTINE: read_Col_de_Porte
! \label{read_Col_de_Porte}
! 
! !REVISION HISTORY:
! 06 Oct 2010: David Mocko, Updated for Bondville test case
! 20 Feb 2018: Shugong Wang, Updated for Loobos test case 
! 16 Sep 2019: Mahdi Navari, Updated for Col de Porte test case 
! 
! !INTERFACE:
subroutine read_Col_de_Porte(n,findex,order,itime) ! ,ftn
   ! !USES:
   use LIS_logMod, only            : LIS_logunit,LIS_endrun, LIS_verify,LIS_getNextUnitNumber, &
                                   LIS_releaseUnitNumber
   use LIS_coreMod, only           : LIS_rc,LIS_domain
   use LIS_metforcingMod,     only : LIS_forc
   use LIS_timeMgrMod, only        : LIS_date2time,LIS_tick, LIS_time2date
   use Col_de_Porte_forcingMod, only : Col_de_Porte_struc
#if (defined USE_NETCDF3 || defined USE_NETCDF4)
   use netcdf
#endif

   implicit none
   ! !ARGUMENTS:
   integer, intent(in) :: n
   integer, intent(in) :: findex
   integer, intent(in) :: order
   character*80       :: Col_de_Porte_filename
   !  integer, intent(out)     :: ferror
   integer, intent(in) :: itime

   !
   ! !DESCRIPTION:
   !  For the given time, reads parameters from the correct Col de Porte
   !  station data (ASCII), transforms into LIS forcing parameters, and
   !  interpolates to the LIS domain.
   !
   !  The arguments are:
   !  \begin{description}
   !  \item[n]
   !    index of the nest
   !  \item[findex]
   !    index of the forcing
   !  \item[order]
   !    flag indicating which data to be read (order=1, read the previous 
         !    hourly instance, order=2, read the next hourly instance)
   !  \item[name]
   !    name of the file to be read
   !  \item[ferror]
!    return error flag (0-fail, 1-success)
   !  \end{description}
   !
   !  The routines invoked are:
   !  \begin{description}
   !  \item[normalize\_stnwts](\ref{normalize_stnwts}) \newline
   !    renormalizes the station weights accounting for
   !    missing data
   !  \item[interp\_stndata](\ref{interp_stndata}) \newline
   !    spatially interpolates the station data onto the LIS grid.
   !  \end{description}
   !EOP
   integer             :: i,f,count1, k , ts, rc
   integer             :: ftn
   logical              :: file_exists  
   integer             :: c,r
   integer             :: t2mId, q2mId, w10mId, w_dirId, swdnId, dirswdnId, scaswdnId
   integer             :: lwdnId, psurfId, rainId, snowId, nebId ,co2Id, timeId
integer, parameter :: dp = selected_real_kind(15, 307)
   !  integer :: i,c,r,f,count1, k 
   real    :: time0(Col_de_Porte_struc(n)%nstns) , & 
time(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real    :: rainf0(Col_de_Porte_struc(n)%nstns) , & 
rainf(Col_de_Porte_struc(n)%nstns, & Col_de_Porte_struc(n)%Col_de_PorteNumTs) 
   real    :: snowf0(Col_de_Porte_struc(n)%nstns) , &
snowf(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real    :: psurf0(Col_de_Porte_struc(n)%nstns) , & 
psurf(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real    :: tair0(Col_de_Porte_struc(n)%nstns) , & 
tair(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real    :: qair0(Col_de_Porte_struc(n)%nstns) , &
qair(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real    :: swdown0(Col_de_Porte_struc(n)%nstns) , & 
swdown(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs) 
   real    :: SWdirect0(Col_de_Porte_struc(n)%nstns) , & 
SWdirect(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real    :: SWdiffuse0(Col_de_Porte_struc(n)%nstns) , & 
SWdiffuse(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real    :: lwdown0(Col_de_Porte_struc(n)%nstns) , & 
lwdown(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   !  real    :: wind0(Col_de_Porte_struc(n)%nstns) , & 
!                wind(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_Portetime2)
   !  real    :: wind_dir0(Col_de_Porte_struc(n)%nstns) , & 
!                wind_dir(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_Portetime2)
   !  real    :: cloudiness0(Col_de_Porte_struc(n)%nstns) , & 
!               cloudiness(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_Portetime2)
   !  real    :: CO2air0(Col_de_Porte_struc(n)%nstns) , & 
   !                CO2air(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_Portetime2)
!  real    :: varfield(LIS_rc%lnc(n)*LIS_rc%lnr(n))
   real   :: u0(Col_de_Porte_struc(n)%nstns) , & 
u(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real   :: v0(Col_de_Porte_struc(n)%nstns) , & 
v(Col_de_Porte_struc(n)%nstns, Col_de_Porte_struc(n)%Col_de_PorteNumTs)
   real :: tmp (Col_de_Porte_struc(n)%Col_de_PorteNumTs) 
   real (dp)  :: tmp2d (1, Col_de_Porte_struc(n)%Col_de_PorteNumTs)  ! MN double precision
   real    :: varfield(11,LIS_rc%lnc(n)*LIS_rc%lnr(n))
   real    :: varfield1(LIS_rc%lnc(n),LIS_rc%lnr(n))
   real*8  :: listime,Col_de_Porte_time
   real    :: lisgmt,Col_de_Porte_gmt
   integer :: lisdoy,Col_de_Porte_doy
   integer :: Col_de_Porte_yr,Col_de_Porte_mon,Col_de_Porte_day, &
   Col_de_Porte_hr,Col_de_Porte_min,Col_de_Porte_sec
   real    :: Col_de_Porte_tick , forcing_ts_tick
   !  character(len=500) :: line


   varfield = 0 
   !ferror = 1

   !      write(LIS_logunit,*) 'starting read_Col_de_Porte'
   do i = 1,Col_de_Porte_struc(n)%nstns
   ! Generate the Col de Porte filename and see if it exists
Col_de_Porte_filename = trim(Col_de_Porte_struc(n)%Col_de_Portefile)
   write(LIS_logunit,*) 'Reading Col_de_Porte file: ',             &
trim(Col_de_Porte_filename)

#if (defined USE_NETCDF3 || defined USE_NETCDF4)
inquire(file=Col_de_Porte_filename,exist=file_exists)
   if (file_exists) then
! ftn = LIS_getNextUnitNumber()
   call LIS_verify(nf90_open(path=trim(Col_de_Porte_filename),mode=NF90_NOWRITE,ncid=ftn),&
         'nf90_open failed in read_Col_de_Porte')

   call LIS_verify(nf90_inq_varid(ftn,'time',timeId),&
         'nf90_inq_varid failed for Time in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'Tair',t2mId),&
         'nf90_inq_varid failed for Tair in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'Qair',q2mId),&
         'nf90_inq_varid failed for Qair in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'Wind',w10mId),&
         'nf90_inq_varid failed for Wind in read_Col_de_Porte')
   !        call LIS_verify(nf90_inq_varid(ftn,'Wind_DIR',w_dirId),&
         !             'nf90_inq_varid failed for Wind_DIR in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'theorSW',swdnId),&
         'nf90_inq_varid failed for total SW DN in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'DIR_SWdown',dirswdnId),&
         'nf90_inq_varid failed for DIR_SWdown in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'SCA_SWdown',scaswdnId),&
         'nf90_inq_varid failed for SCA_SWdown in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'LWdown',lwdnId),&
         'nf90_inq_varid failed for LWdown in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'PSurf',psurfId),&
         'nf90_inq_varid failed for PSurf in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'Rainf',rainId),&
         'nf90_inq_varid failed for Rainf in read_Col_de_Porte')
   call LIS_verify(nf90_inq_varid(ftn,'Snowf',snowId),&
         'nf90_inq_varid failed for Snowf in read_Col_de_Porte')
   !     call LIS_verify(nf90_inq_varid(ftn,'NEB',nebId),&
         !          'nf90_inq_varid failed for cloudiness in read_Col_de_Porte')
   !     call LIS_verify(nf90_inq_varid(ftn,'CO2air',co2Id),&
         !          'nf90_inq_varid failed for CO2air in read_Col_de_Porte')


   call LIS_verify(nf90_get_var(ftn,timeId, tmp),&  ! can we do time(i,:) ? or use tmp and time(i,:) = tmp ? 
         'nf90_get_var failed for Time in read_Col_de_Porte')
   time(i,:) = tmp


# if 0
  rc = nf90_get_var(ftn,t2mId,tmp2d)
if (rc /=nf90_noerr) then
  write(LIS_LogUnit,*) trim(nf90_strerror(rc))
   call LIS_verify(rc,'nf90_get_var failed for Tair in read_Col_de_Porte')
endif
stop
#endif


   call LIS_verify(nf90_get_var(ftn,t2mId,tmp2d),&
         'nf90_get_var failed for Tair in read_Col_de_Porte')
   tair(i,:) = tmp2d(1,:)
   call LIS_verify(nf90_get_var(ftn,q2mId,tmp2d),&
         'nf90_get_var failed for Qair in read_Col_de_Porte')
   qair(i,:) = tmp2d(1,:)
   call LIS_verify(nf90_get_var(ftn,w10mId,tmp2d),&    ! MN:  based on ALMA;  set one of the wind components to zero and write the data into the other
         'nf90_get_var failed for Wind in read_Col_de_Porte')
   u(i,:) = tmp2d(1,:)
   v(i,:) = 0 
   !        call LIS_verify(nf90_get_var(ftn,w_dirId,wind_dir(i,:)),&
         !             'nf90_get_var failed for Wind_DIR in read_Col_de_Porte')
   call LIS_verify(nf90_get_var(ftn,swdnId,tmp2d),&
         'nf90_get_var failed for total SW DN in read_Col_de_Porte')
   swdown(i,:) = tmp2d(1,:)
   call LIS_verify(nf90_get_var(ftn,dirswdnId,tmp2d),&
         'nf90_get_var failed for DIR_SWdown in read_Col_de_Porte')
   SWdirect(i,:) = tmp2d(1,:)
   call LIS_verify(nf90_get_var(ftn,scaswdnId,tmp2d),&
         'nf90_get_var failed for SCA_SWdown in read_Col_de_Porte')
   SWdiffuse(i,:) = tmp2d(1,:)
   call LIS_verify(nf90_get_var(ftn,lwdnId,tmp2d),&
         'nf90_get_var failed for LWdown in read_Col_de_Porte')
   lwdown(i,:) = tmp2d(1,:)
   call LIS_verify(nf90_get_var(ftn,psurfId,tmp2d),&
         'nf90_get_var failed for PSurf in read_Col_de_Porte')
   psurf(i,:) = tmp2d(1,:)
   call LIS_verify(nf90_get_var(ftn,rainId,tmp2d),&
         'nf90_get_var failed for Rainf in read_Col_de_Porte')
   rainf(i,:) = tmp2d(1,:)
   call LIS_verify(nf90_get_var(ftn,snowId,tmp2d),&
         'nf90_get_var failed for Snowf in read_Col_de_Porte')
   snowf(i,:) = tmp2d(1,:)
   !     call LIS_verify(nf90_get_var(ftn,nebId,cloudiness(i,:)),&
         !          'nf90_get_var failed for cloudiness in read_Col_de_Porte')
   !     call LIS_verify(nf90_get_var(ftn,co2Id,co2air(i,:)),&
         !          'nf90_get_var failed for CO2air in read_Col_de_Porte')

      call LIS_verify(nf90_close(ftn))
!      call LIS_releaseUnitNumber(ftn)
   else
   write(LIS_logunit,*) &
   'Could not find file: ',trim(Col_de_Porte_filename)
   !ferror = 0
   endif

   ! Reading data for each time step 
   do ts = 1,210387 ! haed coded     Col_de_Porte_struc(n)%Col_de_PorteNumTs
!   print*, 'ts= ', ts 
!   if (ts .EQ. 9000) then
!    write(LIS_logunit,*) &
!   'Stop here ',trim(Col_de_Porte_filename)
!   endif 

   Col_de_Porte_yr =1993
   Col_de_Porte_mon = 8 
   Col_de_Porte_day = 1
   Col_de_Porte_hr = 6
   Col_de_Porte_min = 0
   Col_de_Porte_sec = 0
   time0(i) = time(i,ts)    ! time in second form 1993080106  
   tair0(i) = tair(i,ts)
   qair0(i) = qair(i,ts)
   !wind0(i) = wind(i,ts)
   !wind_dir0(i) = wind_dir(i,ts)
   swdown0(i) = swdown(i,ts) 
   SWdirect0(i) =SWdirect(i,ts) 
   SWdiffuse0(i) = SWdiffuse(i,ts)
   lwdown0(i) = lwdown(i,ts)
   psurf0(i) = psurf(i,ts) 
   rainf0(i) = rainf(i,ts) 
   snowf0(i) = snowf(i,ts)
   !         cloudiness0(i) = cloudiness(i,ts)
   !         co2air0(i) = co2air(i,ts) 
   u0(i) =  u(i,ts)
   v0(i) = 0
forcing_ts_tick = time0(i)   ! 0, 3600, 7200, ... , 757382400 (s) 
   call LIS_tick(Col_de_Porte_time, Col_de_Porte_doy, Col_de_Porte_gmt, &
         Col_de_Porte_yr, Col_de_Porte_mon, Col_de_Porte_day,      &
         Col_de_Porte_hr,Col_de_Porte_min, Col_de_Porte_sec, forcing_ts_tick)
   ! get the date of the current time step from the date number          
   call LIS_time2date(Col_de_Porte_time, Col_de_Porte_doy, Col_de_Porte_gmt, &
         Col_de_Porte_yr, Col_de_Porte_mon, Col_de_Porte_day,      &
         Col_de_Porte_hr,Col_de_Porte_min)

!           swdown(i),lwdown(i), rain(i), snow(i), tair(i),u(i),psurf(i),qair(i)            
   !           read(line,40) Col_de_Porte_yr,Col_de_Porte_mon,Col_de_Porte_day, &
   !                Col_de_Porte_hr,Col_de_Porte_min,         &
!                swdown(i),lwdown(i), rain(i), snow(i), tair(i),u(i),psurf(i),qair(i) 
   !           v(i)   = 0
!           !pcp(i) = rain(i) + snow(i) 
   Col_de_Porte_sec = 0
   !Col_de_Porte_tick = 21600
   Col_de_Porte_tick = 0 
   call LIS_date2time (Col_de_Porte_time,Col_de_Porte_doy,Col_de_Porte_gmt, &    
         Col_de_Porte_yr,Col_de_Porte_mon,Col_de_Porte_day,  &
         Col_de_Porte_hr,Col_de_Porte_min,Col_de_Porte_sec)
   call LIS_date2time(listime , lisdoy,    lisgmt,LIS_rc%yr, LIS_rc%mo, LIS_rc%da, & 
         LIS_rc%hr, LIS_rc%mn, LIS_rc%ss)
   ! Convert local solar time of Col_de_Porte forcing data into GMT time
   ! as in LIS; add 1 hours to Col_de_Porte to get GMT.  When Col_de_Porte
   ! +1 hours is greater than or equal to the LIS time (depending if
         ! we are interpolating between half-hourly data), then stop reading
   ! the Col_de_Porte data and use that line as forcing at the LIS time.
   call LIS_tick(Col_de_Porte_time,Col_de_Porte_doy,Col_de_Porte_gmt, &
         Col_de_Porte_yr,Col_de_Porte_mon,       &
         Col_de_Porte_day,Col_de_Porte_hr,Col_de_Porte_min, &
         Col_de_Porte_sec,Col_de_Porte_tick)
   if ((Col_de_Porte_time.ge.listime).and.(itime.eq.1)) exit
   if ((Col_de_Porte_time.gt.listime).and.(itime.eq.2)) then
      rainf0(i) = rainf(i,ts+1) 
      snowf0(i) = snowf(i,ts+1) 
      exit
   endif
   enddo
!close(ftn)
   !else
   !  write(LIS_logunit,*) 'Filename not found; stopping program'
   !  call LIS_endrun
   !endif
#endif     
   enddo ! loop over nstns

   !  forcing data order in the nc file 
   !{'Tair'}    {'Qair'}    {'Wind_DIR'}    {'Wind'}   
   !{'Rainf'}    {'Snowf'}    {'LWdown'}    {'DIR_SWdown'}    {'SCA_SWdown'}
   !{'CO2air'}    {'PSurf'}    {'NEB'}    {'HUMREL'}    {'theorSW'}
   call normalize_stnwts(tair0,Col_de_Porte_struc(n)%nstns,            &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,tair0,varfield(1,:),           &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   call normalize_stnwts(qair0,Col_de_Porte_struc(n)%nstns,            &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,qair0,varfield(2,:),           &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   !  call normalize_stnwts(wind_dir0,Col_de_Porte_struc(n)%nstns,               &
         !       LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         !       Col_de_Porte_struc(n)%stnwt)
   !  call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         !       Col_de_Porte_struc(n)%undef,wind_dir0,varfield(3,:),              &
         !       LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   call normalize_stnwts(u0,Col_de_Porte_struc(n)%nstns,               &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,u0,varfield(3,:),              &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   call normalize_stnwts(v0,Col_de_Porte_struc(n)%nstns,               &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,v0,varfield(4,:),              &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   call normalize_stnwts(rainf0,Col_de_Porte_struc(n)%nstns,             &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,rainf0,varfield(5,:),            &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   call normalize_stnwts(snowf0,Col_de_Porte_struc(n)%nstns,             &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,snowf0,varfield(6,:),            &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   call normalize_stnwts(lwdown0,Col_de_Porte_struc(n)%nstns,          &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,lwdown0,varfield(7,:),         &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   call normalize_stnwts(SWdirect0,Col_de_Porte_struc(n)%nstns,          &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,SWdirect0,varfield(8,:),         &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

   call normalize_stnwts(SWdiffuse0,Col_de_Porte_struc(n)%nstns,          &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,SWdiffuse0,varfield(9,:),         &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

#if 0
   call normalize_stnwts(co2air0,Col_de_Porte_struc(n)%nstns,           &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,co2air0,varfield(10,:),          &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)
#endif

   call normalize_stnwts(psurf0,Col_de_Porte_struc(n)%nstns,           &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,psurf0,varfield(10,:),          &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

#if 0 
   call normalize_stnwts(cloudiness0,Col_de_Porte_struc(n)%nstns,           &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,cloudiness0,varfield(12,:),          &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)
#endif

   call normalize_stnwts(swdown0,Col_de_Porte_struc(n)%nstns,          &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%undef,  &
         Col_de_Porte_struc(n)%stnwt)
   call interp_stndata(Col_de_Porte_struc(n)%stnwt,                   &
         Col_de_Porte_struc(n)%undef,swdown0,varfield(11,:),         &
         LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%nstns)

!WRITE(*,*) '**read_Col_de_Porte.F90 SW',Col_de_Porte_yr,Col_de_Porte_mon,  &
!                      Col_de_Porte_day,Col_de_Porte_hr , varfield(11,1)

varfield(11,:) = varfield(8,:) + varfield(9,:)
print *, '*****************************************'
Print *, 'ALERT : For the test case the SW was replaced with sum of SW_dir and SW_diff'
Print *, '               in the SURFEX-Crocus SW =  SW_dir and SW_diff'
!WRITE(*,*) '**read_Col_de_Porte.F90 SW',Col_de_Porte_yr,Col_de_Porte_mon,  &
!                      Col_de_Porte_day,Col_de_Porte_hr , varfield(11,1), varfield(8,1), varfield(9,1)


   do f = 1,11
   count1 = 0
   do r = 1,LIS_rc%lnr(n)
   do c = 1,LIS_rc%lnc(n)
varfield1(c,r) = varfield(f,c+count1)
   enddo
count1 = count1 + LIS_rc%lnc(n)
   enddo

   do r = 1,LIS_rc%lnr(n)
do c = 1,LIS_rc%lnc(n)
   if (LIS_domain(n)%gindex(c,r).ne.-1) then
   if(order.eq.1) then 
Col_de_Porte_struc(n)%metdata1(f,LIS_domain(n)%gindex(c,r)) = varfield1(c,r)
   elseif(order.eq.2) then 
Col_de_Porte_struc(n)%metdata2(f,LIS_domain(n)%gindex(c,r)) = varfield1(c,r)
   endif
   endif
   enddo
   enddo
   enddo


   end subroutine read_Col_de_Porte

