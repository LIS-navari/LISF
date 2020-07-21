!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"
! Macros for tracing - Requires ESMF 7_1_0+
#ifdef ESMF_TRACE
#define TRACE_ENTER(region) call ESMF_TraceRegionEnter(region)
#define TRACE_EXIT(region) call ESMF_TraceRegionExit(region)
#else
#define TRACE_ENTER(region)
#define TRACE_EXIT(region)
#endif
module LIS_glacierMod
!BOP
!
! !MODULE: LIS_glacierMod
!
! !DESCRIPTION:
!  The code in this file implements routines to read various sources of
!  glacier parameter data. 
! 
!  \subsubsection{Overview}
!  This module provides routines for reading and manipulating various 
!  parameters related to glacier properties. The following list of 
!  glacier parameters are currently supported. 
!  \begin{description}
!   \item[glacier fraction data]
!  \end{description}
!
! !REVISION HISTORY:
!
!  21 Oct 2005: Sujay Kumar; Initial implementation
!  3  Apr 2012: Sujay Kumar; Switched to the use of LPT based parameter file
!  1  Jul 2020: Mahdi Navari; Modified for glacier parameters
!
  use LIS_fileIOMod

  implicit none

  PRIVATE
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!------------------------------------------------------------------------------
  public :: LIS_glacier_init  ! initializes data structures and read glacier data
  public :: LIS_diagnoseglacier ! routine that maps glacier data to the history writer
  public :: LIS_glacier_finalize !cleanup allocated structures
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!------------------------------------------------------------------------------
  public :: LIS_glacier      !data structure containing glacier data
!EOP

  type, public :: glacier_type_dec 
     real, allocatable :: glacierfrac(:,:)
  end type glacier_type_dec
  
  type(glacier_type_dec), allocatable :: LIS_glacier(:)

contains

!BOP
! 
! !ROUTINE: LIS_glacier_init
! \label{LIS_glacier_init}
! 
! !INTERFACE:
  subroutine LIS_glacier_init()
! !USES:
    use ESMF
    use LIS_coreMod,  only : LIS_rc
    use LIS_logMod,   only : LIS_logunit, LIS_endrun
! 
! !DESCRIPTION:
!
! Allocates memory for data structures for reading 
! glacier datasets
!
!  Reads the glacier data based on the choice of options specified 
!  in the lis configuration. 
! 
!  The routines invoked are: 
!  \begin{description}
!   \item[read\_glacierfraction](\ref{read_glacierfraction}) \newline
!    invokes the generic method in the registry to read
!    the glacier fraction data
!    LIS input parameters file (e.g.; lis\_input.d01.nc)
!  \end{description}
!
!EOP
    implicit none
    integer :: n, i
    integer :: rc

    TRACE_ENTER("glacier_init")
    allocate(LIS_glacier(LIS_rc%nnest))

    do n=1,LIS_rc%nnest
       if(LIS_rc%useglacierfracmap(n).ne."none") then 
          call read_glacierfraction(n)
       endif
    enddo
    TRACE_EXIT("glacier_init")
  end subroutine LIS_glacier_init

!BOP
! 
! !ROUTINE: LIS_diagnoseglacier
! \label{LIS_diagnoseglacier}
! 
! !INTERFACE: 
  subroutine LIS_diagnoseglacier(n)
! !USES: 
    use LIS_coreMod,     only : LIS_rc, LIS_domain
    use LIS_histDataMod, only : LIS_diagnoseSurfaceOutputVar, &
                                LIS_MOC_glacierfraction
#ifdef ESMF_TRACE
    use ESMF
#endif
! !ARGUMENTS:
    implicit none
    integer, intent(in)   :: n 

! !DESCRIPTION: 
!  This routine writes the LIS glacier data to the LIS 
!  history writer
! 
!  The arguments are: 
!  \begin{description}
!  \item[n] index of the nest \newline
!  \item[ftn] file unit number to be used \newline
!  \end{description}
! 
!  The routines called are: 
!  \begin{description}
!  \item[LIS\_diagnoseOutputVar] (\ref{LIS_diagnoseSurfaceOutputVar})  \newline
!   This routine maps a variable to the history writing routines
!  \end{description}
!EOP
    integer :: t,k
    real, pointer :: temp(:)

    TRACE_ENTER("glacier_diag")
    allocate(temp(LIS_rc%ntiles(n)))




# if 0
          temp = LIS_rc%udef
          do t=1,LIS_rc%ntiles(n)
             if(LIS_domain(n)%tile(t)%index.ne.-1) then 
                temp(t) = LIS_domain(n)%tile(t)%silt
             endif
             call LIS_diagnoseSurfaceOutputVar(n,t,LIS_MOC_SILTFRAC,vlevel=1,&
                                           value=temp(t),unit="-",direction="-")
          enddo
# endif 


    if(LIS_rc%useglacierfracmap(n).ne."none") then 
          temp = LIS_rc%udef
          do t=1,LIS_rc%ntiles(n)
             if(LIS_domain(n)%tile(t)%index.ne.-1) then 
                temp(t) = LIS_glacier(n)%glacierfrac(LIS_domain(n)%tile(t)%col,&
                          LIS_domain(n)%tile(t)%row)
             endif
          call LIS_diagnoseSurfaceOutputVar(n,t,LIS_MOC_glacierfraction,vlevel=1,&
                                           value=temp(t),unit="-",direction="-")
          enddo
    endif
    deallocate(temp)
    TRACE_EXIT("glacier_diag")

  end subroutine LIS_diagnoseglacier
  
!BOP
! 
! !ROUTINE: LIS_glacier_finalize
! \label{LIS_glacier_finalize}
! 
! !INTERFACE:
  subroutine LIS_glacier_finalize()
! !USES:
    use LIS_coreMod, only : LIS_rc
! !DESCRIPTION:
!
! deallocates memory for all datastructures used for reading
! glacier datasets. This method is typically called after the 
! information is translated to the LSM model tiles. 
! 
!
!EOP    
    implicit none
    integer :: n
    do n=1,LIS_rc%nnest
       if(LIS_rc%useglacierfracmap(n).ne."none") then 
          deallocate(LIS_glacier(n)%glacierfrac)
       endif
    enddo
  end subroutine LIS_glacier_finalize


!BOP
!
! !ROUTINE: read_glacierfraction
!  \label{read_glacierfraction}
!
! !REVISION HISTORY:
!  03 Sept 2004: Sujay Kumar; Initial Specification
!  01 Jul  2020: Mahdi Navari: modified for glacier fraction 
!
! !INTERFACE:
subroutine read_glacierfraction(n)
! !USES:
#if(defined USE_NETCDF3 || defined USE_NETCDF4)
  use netcdf
#endif
  use LIS_coreMod,        only : LIS_rc, LIS_localPet,&
       LIS_ews_ind, LIS_ewe_ind,&
       LIS_nss_ind, LIS_nse_ind, LIS_ews_halo_ind,LIS_ewe_halo_ind, &
       LIS_nss_halo_ind, LIS_nse_halo_ind
  use LIS_logMod,         only : LIS_logunit, LIS_getNextUnitNumber, &
       LIS_releaseUnitNumber, LIS_endrun, LIS_verify

  implicit none
! !ARGUMENTS: 
  integer, intent(in) :: n

! !DESCRIPTION:
!  This subroutine reads the glacier fraction data 
!  
!  The arguments are:
!  \begin{description}
!   \item[n]
!    index of n
!   \item[localmask]
!    porosity for the region of interest
!   \end{description}
!
!EOP      

  integer :: ios1
  integer :: ios,nid,glacierfracid,ncId, nrId
  integer :: nc,nr,t
  real, allocatable :: glacierfraction(:,:)
  logical :: file_exists

#if (defined USE_NETCDF3 || defined USE_NETCDF4)
  inquire(file=LIS_rc%paramfile(n), exist=file_exists)
  if(file_exists) then 

     write(LIS_logunit,*)'[ERR] Reading glacier fraction map from '&
          //trim(LIS_rc%paramfile(n))

     ios = nf90_open(path=LIS_rc%paramfile(n),&
          mode=NF90_NOWRITE,ncid=nid)
     call LIS_verify(ios,'Error in nf90_open in read_glacierfraction')
     
     ios = nf90_inq_dimid(nid,"east_west",ncId)
     call LIS_verify(ios,'Error in nf90_inq_dimid in read_glacierfraction')

     ios = nf90_inq_dimid(nid,"north_south",nrId)
     call LIS_verify(ios,'Error in nf90_inq_dimid in read_glacierfraction')

     ios = nf90_inquire_dimension(nid,ncId, len=nc)
     call LIS_verify(ios,'Error in nf90_inquire_dimension in read_glacierfraction')

     ios = nf90_inquire_dimension(nid,nrId, len=nr)
     call LIS_verify(ios,'Error in nf90_inquire_dimension in read_glacierfraction')

     ios = nf90_inq_varid(nid,'GLACIERFRAC',glacierfracid)
     call LIS_verify(ios,'GLACIERFRAC field not found in the LIS param file')

     allocate(glacierfraction(LIS_rc%gnc(n),LIS_rc%gnr(n)))
     allocate(LIS_glacier(n)%glacierfrac(LIS_rc%lnc(n),LIS_rc%lnr(n)))

     ios = nf90_get_var(nid,glacierfracid,glacierfraction)
     call LIS_verify(ios,'Error in nf90_get_var in read_glacierfraction')
     
     ios = nf90_close(nid)
     call LIS_verify(ios,'Error in nf90_close in read_glacierfraction')

     LIS_glacier(n)%glacierfrac(:,:) = &
          glacierfraction(LIS_ews_halo_ind(n,LIS_localPet+1):&         
          LIS_ewe_halo_ind(n,LIS_localPet+1), &
          LIS_nss_halo_ind(n,LIS_localPet+1): &
          LIS_nse_halo_ind(n,LIS_localPet+1))
     
     deallocate(glacierfraction)
  else
     write(LIS_logunit,*) '[ERR] glacier fraction  map: ',LIS_rc%paramfile(n), ' does not exist'
     write(LIS_logunit,*) '[ERR] program stopping ...'
     call LIS_endrun
  endif
#endif
end subroutine read_glacierfraction

end module LIS_glacierMod
