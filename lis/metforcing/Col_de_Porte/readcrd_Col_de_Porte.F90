!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
!
! !ROUTINE: readcrd_Col_de_Porte
! \label{readcrd_Col_de_Porte} 
!
! !REVISION HISTORY:
! 13 Apr 2007: Bailing Li, Initial Code
! 05 Oct 2010: David Mocko, Updated for Bondville test case
! 20 Feb 2018: Shugong Wang, Updated for Loobos test case 
! 16 Sep 2019: Mahdi Navari, Updated for Col de Porte test case
! 
! !INTERFACE:    
subroutine readcrd_Col_de_Porte()
! !USES:
  use ESMF
  use LIS_logMod, only : LIS_logunit
  use LIS_coreMod, only : LIS_rc, LIS_config
  use Col_de_Porte_forcingMod, only : Col_de_Porte_struc
!
! !DESCRIPTION:
!  This routine reads the options specific to the Col de Porte
!  test case station data forcing from the LIS configuration file.
!
!EOP

  implicit none
  integer :: n, rc

!      write(LIS_logunit,*) 'starting readcrd_Col_de_Porte'
  call ESMF_ConfigFindLabel(LIS_config,"Col de Porte forcing file:", &
       rc=rc)
  do n = 1,LIS_rc%nnest
     call ESMF_ConfigGetAttribute(LIS_config,                      &
          Col_de_Porte_struc(n)%Col_de_Portefile,rc=rc)
  enddo
  do n = 1,LIS_rc%nnest
     write(LIS_logunit,*) 'Using Col_de_Porte forcing'
     write(LIS_logunit,*) 'Col_de_Porte forcing file: ',             &
          trim(Col_de_Porte_struc(n)%Col_de_Portefile)
     Col_de_Porte_struc(n)%Col_de_PorteNumTs = 210387
     Col_de_Porte_struc(n)%Col_de_Portetime2 = 210387 !17520  ! MN? 
     Col_de_Porte_struc(n)%Col_de_Portetime1 = 0.0
     !Col_de_Porte_struc(n)%Col_de_PorteNumofdata = 210387 ! (210387-3) * 3600(s) = 757382400 (s)  (1993080106- 20170806)
     Col_de_Porte_struc(n)%startRead = .false.
  enddo
  
end subroutine readcrd_Col_de_Porte

