!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
module Col_de_Porte_forcingMod
!BOP
! !MODULE: Col_de_Porte_forcingMod
! 
! !DESCRIPTION: 
!  Contains routines and data structures that are used for the 
!  implementation of the station data from various Col de Porte stations. 
!  The stations report estimates of meteorological forcing terms, 
!  which is spatially interpolated using the inverse distance 
!  weighting scheme (IDW). 
! 
!  The implementation in LIS has the derived data type {\tt Col_de_Porte\_struc}
!  that includes the variables to specify the runtime options, and the
!  calculation of weights for spatial interpolation.
!
! !REVISION HISTORY: 
! 05 Oct 2010: David Mocko, Updated for Loobos test case
! 16 Sep 2019: Mahdi Navari, Updated for Col de Porte test case
! 
  implicit none
  PRIVATE
!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------
  public :: init_Col_de_Porte !defines the native resolution of 
                                       !the input data
!-----------------------------------------------------------------------------
! !PUBLIC TYPES:
!-----------------------------------------------------------------------------
  public :: Col_de_Porte_struc
!EOP

  type, public         :: Col_de_Porte_type_dec
     real                 :: ts
     character*80         :: Col_de_Portefile
     real                 :: undef
     real*8               :: starttime,Col_de_Portetime1,Col_de_Portetime2 ,Col_de_PorteNumTs
     integer              :: findtime1,findtime2,nstns
     logical              :: startRead
     character*9, allocatable :: stnid(:)
     real, allocatable        :: stnwt(:,:)
     real, allocatable        :: stnlat(:),stnlon(:)

     real, allocatable :: metdata1(:,:) 
     real, allocatable :: metdata2(:,:) 

  end type Col_de_Porte_type_dec
  
  type(Col_de_Porte_type_dec), allocatable :: Col_de_Porte_struc(:)

contains
  
!BOP
!
! !ROUTINE: init_Col_de_Porte
! \label{init_Col_de_Porte}
! 
! !INTERFACE:
  subroutine init_Col_de_Porte(findex)
! !USES:
    use LIS_coreMod,only     : LIS_rc
    use LIS_logMod, only     : LIS_logunit,LIS_endrun, &
         LIS_getNextUnitNumber,  &
         LIS_releaseUnitNumber
    use LIS_timeMgrMod, only : LIS_date2time, LIS_update_timestep
    
    implicit none
    integer, intent(in) :: findex
    
! !DESCRIPTION:
!  This routines reads the runtime configurations for using the
!  Col_de_Porte station data. Using the metadata provided for the
!  stations, this routine invokes the call to compute the
!  interpolation weights to be later used.
!
!  The routines invoked are:
!  \begin{description}
!   \item[readcrd\_Col_de_Porte](\ref{readcrd_Col_de_Porte}) \newline
!     reads the runtime options specified for Col_de_Porte station data
!   \item[LIS\_date2time](\ref{LIS_date2time}) \newline
!     converts date to the real time format
!   \item[compute\_stnwts](\ref{compute_stnwts}) \newline
!    computes the weights for spatial interpolation
!  \end{description}
!EOP

    real    :: gmt
    integer :: i,n,styr,stmo,std,sth,stm,sts,tinc,doy
    character*25 :: skipline
    logical :: file_exists
    integer :: ftn

    ! Forecast mode -- NOT Available at this time for this forcing reader:
    if( LIS_rc%forecastMode.eq.1 ) then
       write(LIS_logunit,*) '[ERR] Currently the Col_de_Porte forcing reader'
       write(LIS_logunit,*) '[ERR]  is not set up to run in forecast mode.'
       write(LIS_logunit,*) '[ERR]  May be added in future releases.'
       write(LIS_logunit,*) '[ERR]  LIS forecast run-time ending.'
       call LIS_endrun()
    endif
    
    allocate(Col_de_Porte_struc(LIS_rc%nnest))
    call readcrd_Col_de_Porte()

    do n=1, LIS_rc%nnest
       Col_de_Porte_struc(n)%ts = 3600 ! MN is this in second?   1800
       call LIS_update_timestep(LIS_rc, n, Col_de_Porte_struc(n)%ts)
    enddo


    LIS_rc%met_nf(findex) = 11 !number of met variables in Col_de_Porte ?

    ftn = LIS_getNextUnitNumber()
    do n = 1,LIS_rc%nnest

       allocate(Col_de_Porte_struc(n)%metdata1(LIS_rc%met_nf(findex),&
            LIS_rc%ngrid(n)))
       allocate(Col_de_Porte_struc(n)%metdata2(LIS_rc%met_nf(findex),&
            LIS_rc%ngrid(n)))

       Col_de_Porte_struc(n)%metdata1 = 0
       Col_de_Porte_struc(n)%metdata2 = 0
       Col_de_Porte_struc(n)%nstns = 1
       Col_de_Porte_struc(n)%undef = -999.0
       allocate(Col_de_Porte_struc(n)%stnid(Col_de_Porte_struc(n)%nstns))
       allocate(Col_de_Porte_struc(n)%stnlat(Col_de_Porte_struc(n)%nstns))
       allocate(Col_de_Porte_struc(n)%stnlon(Col_de_Porte_struc(n)%nstns))
       
       styr = 1993
       stmo = 8
       std  = 1
       sth  = 6
       stm  = 0 
       Col_de_Porte_struc(n)%stnlat(1) = 45.29
       Col_de_Porte_struc(n)%stnlon(1) = 5.76
       sts = 0
       call LIS_date2time(Col_de_Porte_struc(n)%starttime,doy,gmt,     &
            styr,stmo,std,sth,stm,sts)

# if 0
       write(LIS_logunit,*)                                          &
            '--------------------------------------------------------'
       write(LIS_logunit,*) 'Opening Col_de_Porte forcing file: ',     &
            trim(Col_de_Porte_struc(n)%Col_de_Portefile)
       inquire(file=trim(Col_de_Porte_struc(n)%Col_de_Portefile),&
            exist=file_exists)
       if (.not.file_exists) then
          write(LIS_logunit,*) 'Filename not found; stopping program'
          call LIS_endrun
       endif
       open(ftn,file=trim(Col_de_Porte_struc(n)%Col_de_Portefile),status='old')
       Col_de_Porte_struc(n)%nstns = 1
       Col_de_Porte_struc(n)%undef = -999.0
       allocate(Col_de_Porte_struc(n)%stnid(Col_de_Porte_struc(n)%nstns))
       allocate(Col_de_Porte_struc(n)%stnlat(Col_de_Porte_struc(n)%nstns))
       allocate(Col_de_Porte_struc(n)%stnlon(Col_de_Porte_struc(n)%nstns))
       
       styr = 1993
       stmo = 8
       std  = 1
       sth  = 6
       stm  = 0 
       Col_de_Porte_struc(n)%stnlat(1) = 45.29
       Col_de_Porte_struc(n)%stnlon(1) = 5.76
       tinc = 60 ! 30  ! MN ? 

       read(ftn,100) skipline
       read(ftn,100) skipline
       read(ftn,100) skipline
       read(ftn,100) skipline
       read(ftn,100) skipline

       Col_de_Porte_struc(n)%stnid(1) = 'Col_de_Porte'
       write(LIS_logunit,*) 'Number of stations: ',                  &
            Col_de_Porte_struc(n)%nstns
       write(LIS_logunit,*) 'Undef value: ',                         &
            Col_de_Porte_struc(n)%undef
       write(LIS_logunit,103) 'Starting time:',styr,'/',stmo,'/',    &
            std,' ',sth,':',stm
       write(LIS_logunit,*) 'Observation time interval (min) ',tinc
       sts = 0
       call LIS_date2time(Col_de_Porte_struc(n)%starttime,doy,gmt,     &
            styr,stmo,std,sth,stm,sts)
       
       do i = 1,Col_de_Porte_struc(n)%nstns
          write(LIS_logunit,*) Col_de_Porte_struc(n)%stnid(i),         &
               Col_de_Porte_struc(n)%stnlat(i),        &
               Col_de_Porte_struc(n)%stnlon(i)
          if ((Col_de_Porte_struc(n)%stnlat(i).gt.LIS_rc%gridDesc(n,7)) &
               .or.(Col_de_Porte_struc(n)%stnlon(i).gt.LIS_rc%gridDesc(n,8)) &
               .or.(Col_de_Porte_struc(n)%stnlat(i).lt.LIS_rc%gridDesc(n,4)) &
               .or.(Col_de_Porte_struc(n)%stnlon(i).lt.LIS_rc%gridDesc(n,5))) then
             write(LIS_logunit,*) 'Station ',                        &
                  Col_de_Porte_struc(n)%stnid(i),'(',  &
                  Col_de_Porte_struc(n)%stnlat(i),',', &
                  Col_de_Porte_struc(n)%stnlon(i),')', &
                  'is not within bounds..'
             write(LIS_logunit,*) 'stopping program...'
             call LIS_endrun
          endif
       enddo
       write(LIS_logunit,*)                                          &
            '--------------------------------------------------------'
       close(ftn)
       
100    format(A25,F6.2)
101    format(A25,1X,I4,4I2)
102    format(A25,I4)
103    format(1X,A14,I8,A1,I2,A1,I2,A1,I2,A1,I2)
 
#endif
      
       allocate(Col_de_Porte_struc(n)%stnwt(LIS_rc%lnc(n)*LIS_rc%lnr(n),&
            Col_de_Porte_struc(n)%nstns))
       call compute_stnwts(Col_de_Porte_struc(n)%nstns,LIS_rc%gridDesc,&
            Col_de_Porte_struc(n)%stnlat,Col_de_Porte_struc(n)%stnlon,&
            LIS_rc%lnc(n)*LIS_rc%lnr(n),Col_de_Porte_struc(n)%stnwt)  ! MN do we need this?
    enddo
    call LIS_releaseUnitNumber(ftn)
    
  end subroutine init_Col_de_Porte

end module Col_de_Porte_forcingMod

