!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2022 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!
! 09 Jan 2024: Mahdi Navari; Initial Specification
!

module Crocus81_dhdt_DAlogMod

  use LIS_constantsMod,  only : LIS_CONST_RHOFW
  use ESMF
! !PUBLIC MEMBER FUNCTIONS:
!------------------------------------------
  public :: Crocus81_dhdt_DAlog
!-----------------------------------------
! !PUBLIC TYPES:
!-----------------------------------------
  public :: Crocus81pred_struc
!EOP

  type, public ::Crocus81pred_dec

     real,allocatable ::model_h(:,:)
     real,allocatable ::model_dh(:)
     real,allocatable ::Pw_combined(:)
     real,allocatable ::ens_id_SIR(:)
  end type Crocus81pred_dec

  type (Crocus81pred_dec),allocatable :: Crocus81pred_struc(:)

contains

  subroutine Crocus81_dhdt_DAlog(n)

     ! USES:
     use LIS_coreMod, only : LIS_rc,LIS_surface
     use LIS_timeMgrMod
     use Crocus81_lsmMod
     use LIS_logMod, only : LIS_logunit, LIS_verify
     !      use smootherDA_runMod, only : smootherDA_increments_mode
! MN for random number del later
  use LIS_numerRecipesMod, only : LIS_rand_func

     ! ARGUMENTS:  
     integer, intent(in)    :: n

     ! DESCRIPTION:
     ! Calculates total column water storage three times per month, to
     ! approximate the GRACE return frequency

     integer                  :: t,d
     integer                  :: yr,mo,da,hr,mn,ss
     integer                  :: yr1, mo1, da1, hr1, mn1, ss1
     !integer                  :: yr2, mo2, da2
     !integer                  :: yr3, mo3, da3
     !integer                  :: tw_tmp1, tw_tmp2
     !type(ESMF_Time)          :: tTime1,tTime2,tTime3
     !type(ESMF_TimeInterval)  :: tw1, tw2
     !integer                  :: status
     integer             :: yy,mm,dd,h,m,s
     integer             :: doy,doy1, ts
     real                :: gmt, gmt1
     real*8              :: timenow
     real*8              :: start_date, simulation_start_time, start_date_new, tmp !, start_date_tmp
     real*8                :: start_date_sec, simulation_start_time_sec, timenow_sec, start_date_new_sec
    real                                 :: ran_face
    integer                              :: n_t
     ! TODO: 
     !  1- two variables, first variable keeps the SD_1D at t-1 and t and second variable keeps the delta(SD_1D)
     !     if (floor(timenow-start_date)/7889400.0).ge.2) then
     !     Crocus81pred_struc(n)%delta_SD_1D = Crocus81pred_struc(n)%%model_dh(2,:) - Crocus81pred_struc(n)%%model_dh(1,:)
     !     Crocus81pred_struc(n)%%model_dh(1,:) = Crocus81pred_struc(n)%%model_dh(2,:) 
     !  2- if this does not work. We need to uses LIS_twStartTime, LIS_twStopTime, and LIS_twMidTime and 
     !     LIS_resetClockForTimeWindow within LIS_timeMgrMod.F9. Then we need to develop a new runmod similar 
     !     to runmodes/smootherDA/smootherDA_runMod.F90
     !     LIS time window interval:  --> lis.config --> LIS_parseTimeString(time,LIS_rc%twInterval) -->  LIS_rc%twInterval
     !     LIS_timeMgrMod

     !2018-10-01 22:30:00.00   2019-01-01 06:00:00.00   2019-04-02 13:30:00.00   2019-07-02 21:00:00.00
     !2019-10-02 04:30:00.00   2020-01-01 12:00:00.00   2020-04-01 19:30:00.00   2020-07-02 03:00:00.00
     !2020-10-01 10:30:00.00   2020-12-31 18:00:00.00   2021-04-02 01:30:00.00   2021-07-02 09:00:00.00
     !CROCUS81_struc(n)%NumOfObsPerAssimWindow
     ! call ESMF_ClockGet(LIS_clock, currTime = currTime, rc=status)
     ! call ESMF_TimeIntervalSet(tw,s=nint(LIS_rc%twInterval),rc=status)
     ! call ESMF_TimeIntervalSet(obs_interval,s=7889400.0,rc=status)


     yy = LIS_rc%yr
     mm = LIS_rc%mo
     dd = LIS_rc%da
     h  = LIS_rc%hr
     m  = LIS_rc%mn
     s  = 0 ! LIS_rc%sss
     !ts=0
     !call LIS_tick(timenow,doy,gmt,yy,mm,dd,h,m,s,real(ts))
     call LIS_date2time(timenow,doy,gmt,yy,mm,dd,h,m,s)
     call LIS_date2time(simulation_start_time,doy,gmt,LIS_rc%syr,LIS_rc%smo,LIS_rc%sda,LIS_rc%shr,LIS_rc%smn,s)   
    
! NOTE: first dh obs is @ 2018-10-01 22:30:00.00 that means the dh represents changes 
!       from 2018-07-02 03:00:00.00 to 2018-10-01 22:30:00.00. However, the first dhdh 
!       is @ 2018-11-16 14:15
     call LIS_date2time(start_date,doy,gmt,2018,10,01,22,0,0)

!print*,'DAlog timenow', timenow
     call LIS_compute_time_since_millennium(LIS_rc%yr, LIS_rc%mo, LIS_rc%da, LIS_rc%hr, LIS_rc%mn, 0, timenow_sec)
     call LIS_compute_time_since_millennium(2018,10,01,22,0,0, start_date_sec)
     call LIS_compute_time_since_millennium(LIS_rc%syr,LIS_rc%smo,LIS_rc%sda,LIS_rc%shr,LIS_rc%smn,0, simulation_start_time_sec)

print*,'DAlog timenow_sec start_date_sec diff'
print '(1x,f20.4, 2x,f20.4, 2x,f10.2)', timenow_sec , start_date_sec , timenow_sec-start_date_sec   
     if (floor(mod(timenow_sec-start_date_sec, LIS_rc%obsInterval)).eq.0 .and. timenow.ge.start_date) then 
        if(.not.allocated(Crocus81pred_struc)) then
           allocate(Crocus81pred_struc(LIS_rc%nnest))
           allocate(Crocus81pred_struc(n)%model_h(2,&
                    LIS_rc%npatch(n,LIS_rc%lsm_index)))
           allocate(Crocus81pred_struc(n)%model_dh(&
                    LIS_rc%npatch(n,LIS_rc%lsm_index)))
        endif

        d = -1 
        if (simulation_start_time .le. start_date) then        
           if ((LIS_rc%yr.eq.2018).and.(LIS_rc%mo.eq.10).and.(LIS_rc%da.eq.1) &
                .and.(LIS_rc%hr.eq.22).and.(LIS_rc%mn.eq.0)) then
              d = 1
           else 
              d = 2
           endif
        endif
print*,'DAlog d', d
        if (simulation_start_time .gt. start_date) then
           tmp = floor((simulation_start_time_sec - start_date_sec)/LIS_rc%obsInterval) + 1
           start_date_new_sec = start_date_sec + tmp * LIS_rc%obsInterval
           !start_date_new = start_date + tmp * LIS_rc%obsInterval 
           call LIS_convert_seconds_to_date(start_date_new_sec,yr1,mo1,da1,hr1,mn1,ss1)
           !call LIS_time2date(start_date_new,doy1,gmt1,yr1,mo1,da1,hr1,mn1) 
           if ((LIS_rc%yr.eq.yr1).and.(LIS_rc%mo.eq.mo1).and.(LIS_rc%da.eq.hr1) &
                .and.(LIS_rc%hr.eq.hr1).and.(LIS_rc%mn.eq.mn1)) then
              d = 1
           else
              d = 2
           endif
        endif
 
        write(LIS_logunit,fmt=24)' [INFO] logging obspred data for PBS-DA @: ',LIS_rc%mo,'/',LIS_rc%da,'/', &
         LIS_rc%yr,LIS_rc%hr,':',LIS_rc%mn,':',LIS_rc%ss 
        24  format(a45,i2.2,a1,i2.2,a1,i4,1x,i2.2,a1,i2.2,a1,i2.2) 
        Crocus81pred_struc(n)%model_dh(:) = 0.0
        
        !Crocus81pred_struc(n)%model_h(2,:)
        do t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
           Crocus81pred_struc(n)%model_h(d,t) = &
           CROCUS81_struc(n)%crocus81(t)%SD_1D   
        enddo
        
        Crocus81pred_struc(n)%model_dh(:) = 0.0
        if (d .eq. 2) then 
           !Crocus81pred_struc(n)%model_dh = &
           !Crocus81pred_struc(n)%model_h(2,:) - Crocus81pred_struc(n)%model_h(1,:)
! for test
           ! for testing the algorithm set %model_dh to a large values
           do n_t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
           !generate random number between 0-1 (random dice roll)
              call LIS_rand_func(1,ran_face)
              Crocus81pred_struc(n)%model_dh(n_t) = 0.05*ran_face   
           enddo
! end for test 
           write(LIS_logunit,fmt=24)' [INFO] obspred (dh) time @ 2nd obs: ',LIS_rc%mo,'/',LIS_rc%da,'/', &
                 LIS_rc%yr,LIS_rc%hr,':',LIS_rc%mn,':',LIS_rc%ss
           Crocus81pred_struc(n)%model_h(1,:) = 0.0
           Crocus81pred_struc(n)%model_h(1,:) = Crocus81pred_struc(n)%model_h(2,:)
        endif 
 
     endif ! mod

  end subroutine Crocus81_dhdt_DAlog

end module Crocus81_dhdt_DAlogMod
