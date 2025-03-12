!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2024 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"
#include "LIS_NetCDF_inc.h"
!module Crocus81_write_particle_weights
!!BOP
!! !MODULE:

!! !DESCRIPTION:
!!  The code in this file wirte the

!! !REVISION HISTORY:
!!  25 Sep 2024: Mahdi Navari; Initial version
!!
!  !use ESMF
!  use LIS_coreMod
!  use LIS_histDataMod
!  use LIS_timeMgrMod
!  use LIS_logMod
!  use LIS_constantsMod, only : LIS_CONST_PATH_LEN
!#if (defined USE_NETCDF3 || defined USE_NETCDF4) 
!  use netcdf
!#endif
!  use LIS_mpiMod

!  implicit none
!  PRIVATE
!!-----------------------------------------------------------------------------
!! !PUBLIC MEMBER FUNCTIONS:
!!-----------------------------------------------------------------------------
!  public :: write_particle_weights
!!-----------------------------------------------------------------------------
!! !PUBLIC TYPES:
!!-----------------------------------------------------------------------------
!!EOP

!contains
!BOP
! 
! !ROUTINE: write_particle_weights
! \label{write_particle_weights}
!
 subroutine write_particle_weights(n)

! USES:
   use LIS_logMod 
   use LIS_coreMod   
   use LIS_fileIOMod
   use LIS_histDataMod
   use LIS_timeMgrMod
   use LIS_historyMod
   use LIS_fileIOMod
   use LIS_constantsMod, only : LIS_CONST_PATH_LEN
   use Crocus81_dhdt_DAlogMod
   use LIS_mpiMod  
#if (defined USE_NETCDF3 || defined USE_NETCDF4) 
  use netcdf
#endif

   implicit none

   ! Arguments
   integer                  :: n
   integer                  :: k
   integer                  :: count1 ,c,r,m,gid,ntiles,ierr,i,t
   integer                  :: l
   integer                  :: gdeltas
   character(len=LIS_CONST_PATH_LEN)       :: fname
   integer                  :: status
   integer                  :: ftn
   integer ::  dimID(3), tdimID, varID
   integer                 :: xtimeID
   character*8             :: xtime_date
   character*6             :: xtime_time
   character*50            :: xtime_units
   character*50            :: xtime_twInterval
   integer                 :: iret
   character(len=8)        :: date
   character(len=10)       :: time
   real                    :: var(LIS_rc%ntiles(n))
   real, allocatable       :: var1(:)
   real, allocatable       :: var1_ens(:,:)
   real, allocatable       :: gtmp(:,:)
   real, allocatable       :: gtmp_ens(:,:,:)
   real, allocatable       :: gtmp1(:)
   real, allocatable       :: gtmp1_ens(:,:)
   character(len=5)      :: zone
   integer, dimension(8) :: values

  !prepare particel weights  
   var = 0.0
   var = Crocus81pred_struc(n)%Pw_combined
   allocate(var1_ens(LIS_rc%ngrid(n), LIS_rc%nensem(n)))
   allocate(var1(LIS_rc%ngrid(n))) 
   if(LIS_masterproc) then
      allocate(gtmp_ens(LIS_rc%gnc(n),LIS_rc%gnr(n), LIS_rc%nensem(n)))
      allocate(gtmp1_ens(LIS_rc%glbngrid(n),LIS_rc%nensem(n)))
      gtmp_ens = 0.0
      gtmp1_ens = 0.0
   else
      allocate(gtmp1_ens(1,LIS_rc%nensem(n)))
      gtmp1_ens = 0.0
   endif

   var1_ens = 0
   do i=1,LIS_rc%ntiles(n),LIS_rc%nensem(n)
      c = LIS_domain(n)%tile(i)%index
      do m=1,LIS_rc%nensem(n)
         t = i+m-1
         if ( var(t) == -9999.0 ) then
            var1_ens(c,m) = -9999.0
         else
            var1_ens(c,m) = &
            var(t)*LIS_domain(n)%tile(t)%fgrd
         endif
      enddo
   enddo

#if (defined SPMD)      
   gdeltas = LIS_gdeltas(n,LIS_localPet)
   do m=1,LIS_rc%nensem(n)
             ! EMK: It is possible that the first dimension of var1_ens is 0 
             ! (no grid points with tiles in the PET).  Unfortunately, slicing
             ! such an array [e.g., var1_ens(:,m)] will cause an array bounds 
             ! error.  So, we add some defensive code here to (a) copy a 
             ! slice to a 1-d array only if the dimension is > 0; and (b) 
             ! always pass the 1d array to MPI_GATHERV.  Note that no memory 
             ! access error will occur in MPI_GATHERV for the zero-grid count 
             ! case as long as gdeltas is also zero.
        if (LIS_rc%ngrid(n) > 0) then
            var1(:) = var1_ens(:,m)
        end if
        call MPI_GATHERV(var1,gdeltas,&
             MPI_REAL,gtmp1_ens(:,m),LIS_gdeltas(n,:),LIS_goffsets(n,:),&
             MPI_REAL,0,LIS_mpi_comm,ierr)
   enddo
#else 
   do m=1,LIS_rc%nensem(n)
      gtmp1_ens(:,m) = var1_ens(:,m)
   enddo
#endif
   deallocate(var1)     ! EMK...Avoid memory leak
   deallocate(var1_ens) ! EMK...Avoid memory leak

   !shuffle = 1
   !deflate = 1
   !deflate_level =9

   !if(.not.pbs_struc(n,k)%fileopen.and.LIS_masterproc) then
   if(LIS_masterproc) then
      call LIS_create_output_directory('PBS')

      if (LIS_rc%ndas > 1) then
         write(LIS_logunit,*)'[ERR] This module writes the particle weights for PBS, assuming '
         write(LIS_logunit,*)'[ERR] there is only one DA instance. If there are multiple DA instances,'
         write(LIS_logunit,*)'[ERR] the code needs to be restructured by moving the SRI from '
         write(LIS_logunit,*)'[ERR] Crocus81_setparticleweight.F90 to pbs_general.F90.'
         call LIS_endrun 
      endif 
      k = 1  !only one DA instance
      !file name  
      call LIS_create_particleweights_filename(n, k, fname, 'PBS')

#if (defined USE_NETCDF4)
      status = nf90_create(path=fname,cmode=nf90_hdf5,&
               ncid = ftn)
      call LIS_verify(status,&
           'creating netcdf file '//trim(fname)//&
           ' failed in write_particle_weights')
#endif
#if (defined USE_NETCDF3)
      status = nf90_create(path=fname,cmode=nf90_clobber,&
               ncid = ftn)
      call LIS_verify(status,&
           'creating netcdf file '//trim(fname)//&
           ' failed in write_particle_weights')
#endif
#if (defined USE_NETCDF3 || defined USE_NETCDF4)
      call LIS_verify(nf90_def_dim(ftn,'east_west',LIS_rc%gnc(n),&
           dimID(1)),&
           'nf90_def_dim for east_west failed in write_particle_weights')
      call LIS_verify(nf90_def_dim(ftn,'north_south',LIS_rc%gnr(n),&
           dimID(2)),&
           'nf90_def_dim for north_south failed in write_particle_weights')
      call LIS_verify(nf90_def_dim(ftn,'ensemble',LIS_rc%nensem(n),&
           dimID(3)),&
           'nf90_def_dim for ensemble failed in write_particle_weights')
      !call LIS_verify(nf90_def_dim(ftn,'time',NF90_UNLIMITED,tdimID),&
      !     'nf90_def_dim for time failed in write_particle_weights')

      ! Define the variable for particle weights
      call LIS_verify(nf90_def_var(ftn, 'particle_weights', NF90_REAL8, dimID, varID), &
          'nf90_def_var for particle_weights failed in write_particle_weights')


       ! defining time field
       !call LIS_verify(nf90_def_var(ftn,'time',nf90_float,dimids = tdimID, varID=xtimeID),&
       !     'nf90_def_var for time failed in write_particle_weights')

       write(xtime_units,200) LIS_rc%yr, LIS_rc%mo, LIS_rc%da, &
            LIS_rc%hr, LIS_rc%mn, LIS_rc%ss
200    format ('Time now ',I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':', &
            I2.2,':',I2.2)
       write(xtime_date, fmt='(I4.4,I2.2,I2.2)') &
            LIS_rc%yr, LIS_rc%mo, LIS_rc%da
       write(xtime_time, fmt='(I2.2,I2.2,I2.2)') &
            LIS_rc%hr, LIS_rc%mn, LIS_rc%ss
       write(xtime_twInterval, fmt='(I20)') nint(LIS_rc%twInterval)

       ! time field attributes
       !call LIS_verify(nf90_put_att(ftn,xtimeID,&
       !     "Time_Now",trim(xtime_units)),&
       !     'nf90_put_att for time now failed in write_particle_weights')
       !call LIS_verify(nf90_put_att(ftn,xtimeID,&
       !     "long_name","time"),&
       !     'nf90_put_att for long_name failed in write_particle_weights')
       call LIS_verify(nf90_put_att(ftn,xtimeID,&
            "time_window_length (sec)",trim(adjustl(xtime_twInterval))),&
            'nf90_put_att for time window length failed in write_particle_weights')
       call LIS_verify(nf90_put_att(ftn,xtimeID,&
            "twStop_date:time",trim(xtime_units)),&
            'nf90_put_att for begin_date failed in write_particle_weights')
       !call LIS_verify(nf90_put_att(ftn,xtimeID,&
       !     "twStop_time",xtime_time),&
       !     'nf90_put_att for begin_time failed in write_particle_weights')

       ! Define global attributes
       call date_and_time(date,time,zone,values)
       call LIS_verify(nf90_put_att(ftn,&
            NF90_GLOBAL,"missing_value", LIS_rc%udef),&
           'nf90_put_att failed for missing_value in write_particle_weights')
       call LIS_verify(nf90_put_att(ftn,NF90_GLOBAL,"title","LIS PBS DA, ensemble weights"), &
           'nf90_put_att failed for title')
       call LIS_verify(nf90_put_att(ftn,NF90_GLOBAL,"institution",trim(LIS_rc%institution)), &
           'nf90_put_att failed for institution')
       call LIS_verify(nf90_put_att(ftn,NF90_GLOBAL,"history", &
           "created on date: "//date(1:4)//"-"//date(5:6)//"-"//&
           date(7:8)//"T"//time(1:2)//":"//time(3:4)//":"//time(5:10)), &
           'nf90_put_att failed for history')
       call LIS_verify(nf90_put_att(ftn,NF90_GLOBAL,"references",       &
           ' '),&
           'nf90_put_att failed for references')
       call LIS_verify(nf90_put_att(ftn,NF90_GLOBAL,"comment", &
           'website: http://lis.gsfc.nasa.gov/'),&
           'nf90_put_att failed for comment')

       ! End define mode.
       call LIS_verify(nf90_enddef(ftn),'Error in ncf90_enddef in write_particle_weights')

       ! Write data to the NetCDF file
       !status = nf90_put_var(ftn,xtimeID,0.0)
       !call LIS_verify(status,'Error in nf90_put_var for time in write_particle_weights')

       gtmp_ens = LIS_rc%udef
       do m=1,LIS_rc%nensem(n)
          count1=1
          do l=1,LIS_npes
             do r=LIS_nss_halo_ind(n,l),LIS_nse_halo_ind(n,l)
                do c=LIS_ews_halo_ind(n,l),LIS_ewe_halo_ind(n,l)
                   gid = c+(r-1)*LIS_rc%gnc(n)
                   ntiles = LIS_domain(n)%ntiles_pergrid(gid)
                    if(ntiles.ne.0) then
                       if(r.ge.LIS_nss_ind(n,l).and.&
                          r.le.LIS_nse_ind(n,l).and.&
                          c.ge.LIS_ews_ind(n,l).and.&
                          c.le.LIS_ewe_ind(n,l))then !points not in halo
                          gtmp_ens(c,r,m) = gtmp1_ens(count1,m)
                       endif
                          count1 = count1 + 1
                    endif
                enddo
             enddo
          enddo
                !if(PRESENT(dim1)) then
                !   status = nf90_put_var(ftn,varid,gtmp_ens(:,:,m),(/1,1,m,dim1/),&
                !        (/LIS_rc%gnc(n),LIS_rc%gnr(n),1,1/))
                !else
          status = nf90_put_var(ftn,varid,gtmp_ens(:,:,m),(/1,1,m/),&
                        (/LIS_rc%gnc(n),LIS_rc%gnr(n),1/))
                !endif
       enddo
       deallocate(gtmp_ens)
       deallocate(gtmp1_ens)
       ! Close the file.
       call LIS_verify(nf90_close(ftn), "Error in nf90_close in RAPID_routing_output")
       write(LIS_logunit,*)'[INFO] Done writing particle weights'
   endif
#endif
       998 FORMAT(1X,A18,4E14.3)
       999 FORMAT(1X,A18,4F14.3)

   end subroutine write_particle_weights
!end module Crocus81_write_particle_weights

