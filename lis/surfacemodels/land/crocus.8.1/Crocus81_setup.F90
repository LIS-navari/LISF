!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.0     
!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"
!BOP
!
! !ROUTINE: Crocus81_setup
! \label{Crocus81_setup}
!
! !REVISION HISTORY:
!  This subroutine is generated with the Model Implementation Toolkit developed
!  by Shugong Wang for the NASA Land Information System Version 7. The initial 
!  specification of the subroutine is defined by Sujay Kumar. 
!   11/6/19: Mahdi Navari, Shugong Wang; initial implementation for LIS 7 and Crocus81
!
! !INTERFACE:
subroutine Crocus81_setup()
! !USES:
    use LIS_logMod,    only: LIS_logunit, LIS_verify, LIS_endrun
    use LIS_fileIOMod, only: LIS_read_param !, LIS_convertParamDataToLocalDomain
    use LIS_coreMod,   only: LIS_rc, LIS_surface
    use Crocus81_lsmMod

!
! !DESCRIPTION:
!
!  This routine is the entry point to set up the parameters
!  required for Crocus81.  These include: 
!    SAND         - Soil sand fraction (-) [-]
!    SILT         - Soil silt fraction (-) [-]
!    CLAY         - Soil clay fraction (-) [-]
!    POROSITY     - Soil porosity (m3 m-3) [m3/m3]
! 
!  The routines invoked are:
!  \begin{description}
!  \item[LIS\_read\_param](\ref{LIS_read_param}) \\ 
!    retrieves LIS parameter data from NetCDF file
!  \end{description}
!EOP
    implicit none
    integer           :: mtype
    integer           :: t, n
    integer           :: col, row
    real, allocatable :: placeholder(:,:)
    
    mtype = LIS_rc%lsm_index
    
    do n=1, LIS_rc%nnest
        ! allocate memory for place holder for #n nest
        allocate(placeholder(LIS_rc%lnc(n), LIS_rc%lnr(n)))
        
        !------------------------------------!
        ! reading spatial spatial parameters !
        !------------------------------------!

        ! read: SAND
        write(LIS_logunit,*) "Crocus81: reading parameter SAND from ", trim(LIS_rc%paramfile(n))
        call LIS_read_param(n, trim(CROCUS81_struc(n)%LDT_ncvar_SAND), placeholder)
        do t = 1, LIS_rc%npatch(n, mtype)
            col = LIS_surface(n, mtype)%tile(t)%col
            row = LIS_surface(n, mtype)%tile(t)%row
            CROCUS81_struc(n)%crocus81(t)%sand = placeholder(col, row)
        enddo 

        ! read: SILT
        write(LIS_logunit,*) "Crocus81: reading parameter SILT from ", trim(LIS_rc%paramfile(n))
        call LIS_read_param(n, trim(CROCUS81_struc(n)%LDT_ncvar_SILT), placeholder)
        do t = 1, LIS_rc%npatch(n, mtype)
            col = LIS_surface(n, mtype)%tile(t)%col
            row = LIS_surface(n, mtype)%tile(t)%row
            CROCUS81_struc(n)%crocus81(t)%silt = placeholder(col, row)
        enddo 

        ! read: CLAY
        write(LIS_logunit,*) "Crocus81: reading parameter CLAY from ", trim(LIS_rc%paramfile(n))
        call LIS_read_param(n, trim(CROCUS81_struc(n)%LDT_ncvar_CLAY), placeholder)
        do t = 1, LIS_rc%npatch(n, mtype)
            col = LIS_surface(n, mtype)%tile(t)%col
            row = LIS_surface(n, mtype)%tile(t)%row
            CROCUS81_struc(n)%crocus81(t)%clay = placeholder(col, row)
        enddo 

        ! read: POROSITY
        write(LIS_logunit,*) "Crocus81: reading parameter POROSITY from ", trim(LIS_rc%paramfile(n))
        call LIS_read_param(n, trim(CROCUS81_struc(n)%LDT_ncvar_POROSITY), placeholder)
        do t = 1, LIS_rc%npatch(n, mtype)
            col = LIS_surface(n, mtype)%tile(t)%col
            row = LIS_surface(n, mtype)%tile(t)%row
            CROCUS81_struc(n)%crocus81(t)%porosity = placeholder(col, row)
        enddo 

        !----------------------------------------------!
        ! MULTILEVEL reading spatial spatial parameters !
        !----------------------------------------------!
        deallocate(placeholder)
    enddo


end subroutine Crocus81_setup


