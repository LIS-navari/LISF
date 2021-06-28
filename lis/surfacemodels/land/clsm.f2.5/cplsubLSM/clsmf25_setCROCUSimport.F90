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
! !ROUTINE: noahmp401_setCROCUSimport
! \label{noahmp401_setCROCUSimport}
!
! !REVISION HISTORY:
! 19 Sep 2020: Sujay Kumar; Initial Specification
! 21 Jun 2021: Mahdi Navari; Modified for clsm25
!
! !INTERFACE:
subroutine clsmf25_setCROCUSimport(n, SubLSM2LSM_State)
! !USES:
  use ESMF
  use LIS_coreMod
  use LIS_logMod
  use clsmf25_lsmMod

  implicit none
! !ARGUMENTS: 
  integer, intent(in)    :: n
  type(ESMF_State)       :: SubLSM2LSM_State
! 
! !DESCRIPTION:
! 
! 
!EOP
  type(ESMF_Field)   :: snwdField, sweField
  real, pointer      :: swe(:), snwd(:)
  real               :: dsneqv,dsnowh,swe_total,sd_total
  integer            :: t
  integer            :: status

  call ESMF_StateGet(SubLSM2LSM_State,"Total SWE",sweField,rc=status)
  call LIS_verify(status)
  call ESMF_StateGet(SubLSM2LSM_State,"Total snowdepth",snwdField,rc=status)
  call LIS_verify(status)

  call ESMF_FieldGet(sweField,localDE=0,farrayPtr=swe,rc=status)
  call LIS_verify(status)
  call ESMF_FieldGet(snwdField,localDE=0,farrayPtr=snwd,rc=status)
  call LIS_verify(status)

  do t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     !swe_total = clsmf25_struc(n)%cat_progn(t)%wesn(1)+&
     !            clsmf25_struc(n)%cat_progn(t)%wesn(2)+&
     !            clsmf25_struc(n)%cat_progn(t)%wesn(3)
     !sd_total  = clsmf25_struc(n)%cat_progn(t)%sndz(1)+&
     !            clsmf25_struc(n)%cat_progn(t)%sndz(2)+&
     !            clsmf25_struc(n)%cat_progn(t)%sndz(3)
  
     !dsneqv = swe(t)  - swe_total ! in mm or (kg/m2)
     !dsnowh = snwd(t) -  sd_total ! in m

     ! update
     !if (dsnowh > 0.0) then
     !   if (dsneqv/dsnowh > 0.0 .and. dsneqv/dsnowh < 1000) then ! feasible range of snow density
        call clsmf25_snow_update(n, t,swe(t),snwd(t)  )! dsneqv, dsnowh)
     !   endif
     !endif
  enddo

end subroutine clsmf25_setCROCUSimport


