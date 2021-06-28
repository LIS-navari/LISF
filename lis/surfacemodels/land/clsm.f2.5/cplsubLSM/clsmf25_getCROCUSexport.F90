!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.3
!
! Copyright (c) 2020 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
! !ROUTINE: clsmf25_getCROCUSexport
! \label{clsmf25_getCROCUSexport}
!
! !REVISION HISTORY:
!  1 June 2021: Mahdi Navari; modified noahmp401_getCROCUSexport for Clsm.f2.5

!  
! !INTERFACE:
subroutine clsmf25_getCROCUSexport(n, LSM2SUBLSM_State)
! !USES:
  use ESMF
  use LIS_coreMod
  use LIS_logMod
  use clsmf25_lsmMod

  implicit none
! !ARGUMENTS: 
  integer, intent(in)    :: n
  type(ESMF_State)       :: LSM2SUBLSM_State
! 
! !DESCRIPTION:
! 
! 
!EOP



  type(ESMF_Field)   :: gtField
  type(ESMF_Field)   :: XWGIField
  type(ESMF_Field)   :: XWGField
  real, pointer      :: gt(:)
  real, pointer      :: XWGI(:)
  real, pointer      :: XWG(:)
  integer            :: t
  integer            :: status

  call ESMF_StateGet(LSM2SUBLSM_State,"Ground temperature",gtField,rc=status)
  call LIS_verify(status)
  call ESMF_StateGet(LSM2SUBLSM_State,"soil volumetric liquid water content",XWGField,rc=status)
  call LIS_verify(status)
  call ESMF_StateGet(LSM2SUBLSM_State,"soil volumetric frozen water content",XWGIField,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(gtField,localDE=0,farrayPtr=gt,rc=status)
  call LIS_verify(status)
  call ESMF_FieldGet(XWGField,localDE=0,farrayPtr=XWG,rc=status)
  call LIS_verify(status)
  call ESMF_FieldGet(XWGIField,localDE=0,farrayPtr=XWGI,rc=status)
  call LIS_verify(status)



  do t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     gt(t) = clsmf25_struc(n)%cat_diagn(t)%tsurf       !%tslb(1)
     !gt(t) = clsmf25_struc(n)%cat_progn(t)%tgb
     XWGI(t) = clsmf25_struc(n)%cat_diagn(t)%sfmc * clsmf25_struc(n)%cat_param(t)%fice        ! volumetric frozen soil moisture [m3/m3]
     XWG(t)  = clsmf25_struc(n)%cat_diagn(t)%sfmc * (1 - clsmf25_struc(n)%cat_param(t)%fice)  ! volumetric liquid soil moisture [m3/m3]
  enddo


end subroutine clsmf25_getCROCUSexport


