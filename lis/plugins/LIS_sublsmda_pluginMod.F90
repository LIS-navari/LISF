!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.4
!
! Copyright (c) 2022 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_plugins.h"
module LIS_sublsmda_pluginMod
!BOP
!
! !MODULE: LIS_sublsmda_pluginMod
!
! !DESCRIPTION:
!   This module contains the definition of the functions that
!   need to be defined to enable the use of a land surface
!   model in a data assimilation setup.
!
! !REVISION HISTORY:
!  27 Feb 2024    Mahdi Navari  Initial Specification
!
!EOP
  implicit none
  PRIVATE
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!------------------------------------------------------------------------------
  PUBLIC :: LIS_sublsmda_plugin
contains
!BOP
! !ROUTINE: LIS_sublsmda_plugin
!  \label{LIS_sublsmda_plugin}
!
! !DESCRIPTION:
! This is a custom-defined plugin point for introducing a new LSM
! in a data assimilation mode. The interface mandates that
! a number of routines be implemented and registered for
! each of the LSM that is used in a data assimilation setup.
! Currently one algorithms are supported, Particle Batch Smoother (PBS).

!
! !INTERFACE:
subroutine LIS_sublsmda_plugin
!EOP

#if ( ( defined DA_PBS ) )

   use LIS_pluginIndices

#if ( defined SM_Crocus_8_1 )
!#if ( defined DA_OBS_ATL15GrIS )
   use Crocus81_da_dhdt_Mod
   use Crocus81_dhdt_DAlogMod, only : Crocus81_dhdt_DAlog
!#endif
#endif  

#if ( defined SM_Crocus_8_1 )
!#if ( defined DA_OBS_ATL15GrIS ) 
   external Crocus81_getdhdtvars
   external Crocus81_setdhdt
   external Crocus81_qcdhdt
   external Crocus81_getdhdtpred
   external Crocus81_scale_dhdt
   external Crocus81_descale_dhdt
   external Crocus81_updatedhdtvars
   !external Crocus81_dhdt_DAlogMod 
   external Crocus81_qc_dhdtobs
   external Crocus81_setparticleweight
!#endif
#endif  

#if ( defined SM_Crocus_8_1 )
!#if ( defined DA_OBS_ATL15GrIS ) 
   call registersublsmdainit(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_da_dhdt_init)
   call registersublsmdagetstatevar(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_getdhdtvars)
   call registersublsmdasetstatevar(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_setdhdt)
   call registersublsmdagetobspred(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_getdhdtpred)
   call registersublsmdaqcstate(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_qcdhdt)
   call registersublsmdascalestatevar(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_scale_dhdt)
   call registersublsmdadescalestatevar(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_descale_dhdt)
   call registersublsmdaqcobsstate(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_qc_dhdtobs)
   call registersublsmdaupdatestate(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_updatedhdtvars)
   call registersublsmdadiagnosevars(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_dhdt_DAlog) ! Crocus81_dhdt_DAlogMod)
   call registersublsmdasetparticleweight(trim(LIS_Crocus81Id)//"+"//&
        trim(LIS_ATL15GrISdhdtobsId)//char(0),Crocus81_setparticleweight)
!#endif 
#endif  
   
#endif  !endif for PBS
   
end subroutine LIS_sublsmda_plugin
end module LIS_sublsmda_pluginMod
