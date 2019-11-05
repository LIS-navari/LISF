!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
!
! !ROUTINE: timeinterp_Col_de_Porte
! \label{timeinterp_Col_de_Porte}
! 
!
! !REVISION HISTORY:
! 05 Oct 2010: David Mocko, Updated for Loobos test case
! 16 Sep 2019: Mahdi Navari, Updated for Col de Porte test case 
!
! !INTERFACE:
subroutine timeinterp_Col_de_Porte(n,findex)
  ! !USES:
  use ESMF
  use LIS_logMod, only           : LIS_logunit,LIS_verify,LIS_endrun
  use LIS_coreMod, only          : LIS_rc,LIS_domain
  use LIS_metforcingMod, only   : LIS_FORC_Base_State, LIS_forc
  use Col_de_Porte_forcingMod, only : Col_de_Porte_struc
  use LIS_FORC_AttributesMod

  implicit none
  ! !ARGUMENTS:
  integer, intent(in) :: n
  integer, intent(in) :: findex

  ! !DESCRIPTION:
  ! Temporally interpolates the Col de Porte forcing data to
  ! the model timestep.  All variables except precipitation
  ! is linearly interpolated. 
  ! 
  !EOP

  real :: wt1,wt2
  integer :: t
  integer :: index1,tid
  integer :: status
  type(ESMF_Field)   :: tairField, qairField, swdownField, SWdirectField , SWdiffuseField ! windField, wind_dirField, 
  type(ESMF_Field)   :: lwdownField,psurfField,rainField,snowField, cloudinessFielf, CO2Field, uField,vField
  real,pointer       :: tair(:),qair(:),uwind(:),vwind(:) ! ,wind(:),wind_dir(:), 
  real,pointer       :: swdown(:),SWdirect(:), SWdiffuse(:),lwdown(:),psurf(:),rainf(:),snowf(:), cloudiness(:), co2air(:)
  real, parameter    :: eps = 0.622
  real               :: svp,qs,E

  !      write(LIS_logunit,*) 'starting timeinterp_Col_de_Porte'

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_Tair%varname(1),   &
       tairField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Tair in the forcing variables list')

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_Qair%varname(1),   &
       qairField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Qair in the forcing variables list')

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_SWdown%varname(1), &
       swdownField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable SWdown in the forcing variables list')

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_SWdirect%varname(1), &
       SWdirectField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable direct SWdown in the forcing variables list')  

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_SWdiffuse%varname(1), &
       SWdiffuseField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable diffuse SWdown in the forcing variables list') 


  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_LWdown%varname(1), &
       lwdownField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable LWdown in the forcing variables list')

#if 0 
  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_WIND%varname(1), &
       windField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Wind in the forcing variables list')    ! wind 

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_Wind_dir%varname(1), &
       wind_dirField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Wind direction in the forcing variables list') ! wind direction
#endif

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_Wind_E%varname(1), &
       uField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Wind_E in the forcing variables list')

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_Wind_N%varname(1), &
       vField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Wind_N in the forcing variables list')

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_Psurf%varname(1),  &
       psurfField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Psurf in the forcing variables list')

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_Rainf%varname(1),  &
       rainField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Rainf in the forcing variables list')

  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_Snowf%varname(1), &
       snowField,rc=status)
  call LIS_verify(status,                                          &
       'Error: Enable Snowf in the forcing variables list')

!  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_cloudiness%varname(1), &
!       cloudinessField,rc=status)
!  call LIS_verify(status,                                          &
!       'Error: Enable Snowf in the forcing variables list')  ! ?   cloudiness

!  call ESMF_StateGet(LIS_FORC_Base_State(n,findex),LIS_FORC_CO2%varname(1), &
!       CO2Field,rc=status)
!  call LIS_verify(status,                                          &
!       'Error: Enable Snowf in the forcing variables list') ! co2



  call ESMF_FieldGet(tairField,localDE=0,farrayPtr=tair,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(qairField,localDE=0,farrayPtr=qair,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(swdownField,localDE=0,farrayPtr=swdown,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(swdownField,localDE=0,farrayPtr=SWdirect,rc=status) ! added
  call LIS_verify(status)

  call ESMF_FieldGet(swdownField,localDE=0,farrayPtr=SWdiffuse,rc=status) ! added
  call LIS_verify(status)

  call ESMF_FieldGet(lwdownField,localDE=0,farrayPtr=lwdown,rc=status)
  call LIS_verify(status)

!  call ESMF_FieldGet(windField,localDE=0,farrayPtr=wind,rc=status) ! added
!  call LIS_verify(status)

!  call ESMF_FieldGet(wind_dirField,localDE=0,farrayPtr=wind_dir,rc=status) ! added
!  call LIS_verify(status)

  call ESMF_FieldGet(uField,localDE=0,farrayPtr=uwind,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(vField,localDE=0,farrayPtr=vwind,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(psurfField,localDE=0,farrayPtr=psurf,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(rainField,localDE=0,farrayPtr=rainf,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(snowField,localDE=0,farrayPtr=snowf,rc=status)
  call LIS_verify(status)

!  call ESMF_FieldGet(snowField,localDE=0,farrayPtr=cloudiness,rc=status)
!  call LIS_verify(status)

!  call ESMF_FieldGet(snowField,localDE=0,farrayPtr=co2air,rc=status)
!  call LIS_verify(status)

  !      write(LIS_logunit,*) 'Btime1: ',Col_de_Porte_struc(n)%Col_de_Portetime1
  !      write(LIS_logunit,*) 'Btime2: ',Col_de_Porte_struc(n)%Col_de_Portetime2
  !      write(LIS_logunit,*) 'realtime: ',LIS_rc%time
  wt1 = (Col_de_Porte_struc(n)%Col_de_Portetime2-LIS_rc%time) /        &
       (Col_de_Porte_struc(n)%Col_de_Portetime2-                      &
       Col_de_Porte_struc(n)%Col_de_Portetime1)
  wt2 = 1.0 - wt1
  !      write(LIS_logunit,*) wt1,wt2


!  forcing data order in the nc file 
!{'Tair'}    {'Qair'}    {'Wind_DIR'}    {'Wind'}   
!{'Rainf'}    {'Snowf'}    {'LWdown'}    {'DIR_SWdown'}    {'SCA_SWdown'}
!{'CO2air'}    {'PSurf'}    {'NEB'}    {'HUMREL'}    {'theorSW'}
  do t = 1,LIS_rc%ntiles(n)   ! MN ?  chech the metdata order 
     index1 = LIS_domain(n)%tile(t)%index
        tair(t) = wt1 * Col_de_Porte_struc(n)%metdata1(1,index1) +                      &
             wt2 * Col_de_Porte_struc(n)%metdata2(1,index1)
        qair(t) = wt1 * Col_de_Porte_struc(n)%metdata1(2,index1) +                      &
             wt2 * Col_de_Porte_struc(n)%metdata2(2,index1)
!       wind_dir(t) =  wt1 * Col_de_Porte_struc(n)%metdata1(3,index1) +                     &
!             wt2 * Col_de_Porte_struc(n)%metdata2(3,index1)
!        wind(t) = wt1 * Col_de_Porte_struc(n)%metdata1(4,index1) +                     &
!             wt2 * Col_de_Porte_struc(n)%metdata2(4,index1)
        uwind(t) = wt1 * Col_de_Porte_struc(n)%metdata1(3,index1) +                     &
             wt2 * Col_de_Porte_struc(n)%metdata2(3,index1)
        vwind(t) = 0.0
        rainf(t) = Col_de_Porte_struc(n)%metdata1(5,index1)
        snowf(t) = Col_de_Porte_struc(n)%metdata1(6,index1)
        lwdown(t) = wt1 * Col_de_Porte_struc(n)%metdata1(7,index1) +                    &
             wt2 * Col_de_Porte_struc(n)%metdata2(7,index1)
        SWdirect(t) = wt1 * Col_de_Porte_struc(n)%metdata1(8,index1) +                    &
             wt2 * Col_de_Porte_struc(n)%metdata2(8,index1)
        SWdiffuse(t) = wt1 * Col_de_Porte_struc(n)%metdata1(9,index1) +                    &
             wt2 * Col_de_Porte_struc(n)%metdata2(9,index1)
!        co2air(t) = wt1 * Col_de_Porte_struc(n)%metdata1(10,index1) +                     &
!             wt2 * Col_de_Porte_struc(n)%metdata2(10,index1)
        psurf(t) = wt1 * Col_de_Porte_struc(n)%metdata1(10,index1) +                     &
             wt2 * Col_de_Porte_struc(n)%metdata2(10,index1)
!        cloudiness(t) = wt1 * Col_de_Porte_struc(n)%metdata1(12,index1) +                     &
!             wt2 * Col_de_Porte_struc(n)%metdata2(12,index1)
        swdown(t) = wt1 * Col_de_Porte_struc(n)%metdata1(11,index1) +                    &
             wt2 * Col_de_Porte_struc(n)%metdata2(11,index1)

  enddo
end subroutine timeinterp_Col_de_Porte


