!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
!
! !ROUTINE: finalize_Col_de_Porte
! \label{finalize_Col_de_Porte}
! 
! !REVISION HISTORY: 
! 08Dec2004: Sujay Kumar; Initial Specification
! 16 Sep 2019: Mahdi Navari, Updated for Col de Porte test case
! 
! !INTERFACE:
subroutine finalize_Col_de_Porte(findex)

  ! !USES:
  use Col_de_Porte_forcingMod, only : Col_de_Porte_struc
  use LIS_logMod, only : LIS_logunit, LIS_endrun
  ! !DESCRIPTION:
  !  Routine to cleanup Col_de_Porte forcing related memory allocations.
  !
  !EOP
  implicit none

  integer, intent(IN) :: findex

  !      write(LIS_logunit,*) 'starting finalize_Col_de_Porte'
  deallocate(Col_de_Porte_struc)

end subroutine finalize_Col_de_Porte

