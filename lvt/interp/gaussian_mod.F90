!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2024 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
! 
! !ROUTINE:
!
! !INTERFACE:
! 
! !USES:   
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP
module gaussian_mod

  implicit none
  
  PRIVATE
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!------------------------------------------------------------------------------
  PUBLIC :: gaussian_find_row
  PUBLIC :: gaussian_find_col
  PUBLIC :: gaussian_comp_lats
  PUBLIC :: gaussian_comp_lons
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!------------------------------------------------------------------------------
  PUBLIC :: gaussian_lat_array
  PUBLIC :: gaussian_lon_array

!BOP
!
! !MODULE: gaussian_mod
! 
! !DESCRIPTION:
!  The code in this file provides routines to manage a quasi-regular
!  Gaussian lat/lon grid.
!
! !REVISION HISTORY: 
!  13 Nov 2006: James Geiger ; Initial Specification
! 

   real,allocatable,dimension(:)   :: gaussian_lat_array
   real,allocatable,dimension(:)   :: gaussian_lon_array
!   real,allocatable,dimension(:,:) :: gaussian_latitudes
!   real,allocatable,dimension(:,:) :: gaussian_longitudes
   integer                     :: GAUSSIAN_NR
   integer                     :: GAUSSIAN_NC
!EOP

contains

!BOP
! 
! !ROUTINE: gaussian_find_row
! \label{gaussian_find_row}
!
! !INTERFACE:
function gaussian_find_row(lat)
! 
! !USES:
   use LVT_logMod , only : LVT_logunit, LVT_endrun

   implicit none
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This function computes the row number within the global quasi-regular 
!  Gaussian grid corresponding to the latitude given by lat.
!
!   \begin{description}
!   \item [lat]
!      latitude to search for
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP
!BOP
!
! !ARGUMENTS:
   real :: lat
!
!EOP

   integer :: gaussian_find_row

   integer :: r

   gaussian_find_row = -9999
   do r = 1, GAUSSIAN_NR
      if ( abs(gaussian_lat_array(r) - lat) < 1e-5 ) then
      !if ( gaussian_latitudes(1,r) == lat ) then
         gaussian_find_row = r
      endif
   enddo

   if ( gaussian_find_row == -9999 ) then
      write(LVT_logunit,fmt=*) 'ERR: gaussian_find_row -- Could not find lat',lat
      call LVT_endrun
   endif

end function gaussian_find_row

!BOP
! 
! !ROUTINE: gaussian_find_col
! \label{gaussian_find_col}
!
! !INTERFACE:
function gaussian_find_col(lon)
! 
! !USES:
   use LVT_logMod , only : LVT_logunit, LVT_endrun

   implicit none
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This function computes the column number within the global quasi-regular 
!  Gaussian grid corresponding to the longitude given by lon.
!
!   \begin{description}
!   \item [lon]
!      longitude to search for
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP
!BOP
!
! !ARGUMENTS:
   real :: lon
!
!EOP

   integer :: gaussian_find_col

   integer :: c

   gaussian_find_col = -9999
   do c = 1, GAUSSIAN_NC
      if ( abs(gaussian_lon_array(c) - lon) < 1e-5 ) then
      !if ( gaussian_longitudes(c,1) == lon ) then
         gaussian_find_col = c
      endif
   enddo

   if ( gaussian_find_col == -9999 ) then
      write(LVT_logunit,fmt=*) 'ERR: gaussian_find_col -- Could not find lon',lon
      call LVT_endrun
   endif

end function gaussian_find_col

#if 1
!BOP
! 
! !ROUTINE: gaussian_comp_lats
! \label{gaussian_comp_lats}
!
! !INTERFACE:
subroutine gaussian_comp_lats(jmax)
! 
! !USES:
  use LVT_logMod , only : LVT_logunit

   implicit none
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This routine computes the latitudes of the global quasi-regular Gaussian 
!  grid corresponding to the total number of latitude circles given by jmax.
!
!   \begin{description}
!   \item [jmax]
!      the total number of latitude circles
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP
!BOP
!
! !ARGUMENTS:
   integer :: jmax
!
!EOP

   real, parameter :: pi=3.14159265358979

   real,allocatable,dimension(:) :: slat, wlat
   real :: dpr
   integer :: j

   dpr =180./pi

   GAUSSIAN_NR = jmax
   allocate(gaussian_lat_array(jmax))

   allocate(slat(jmax))
   allocate(wlat(jmax))

   call gausslat(jmax,slat,wlat)

   do j = 1, jmax
      gaussian_lat_array(j) = dpr*asin(slat(jmax-j+1))
   enddo

   deallocate(slat)
   deallocate(wlat)

   write(unit=LVT_logunit,fmt=*) 'gaussian lats:', gaussian_lat_array
end subroutine gaussian_comp_lats

!BOP
! 
! !ROUTINE: gaussian_comp_lons
! \label{gaussian_comp_lons}
!
! !INTERFACE:
subroutine gaussian_comp_lons(startlon,dlon)
! 
! !USES:
   use LVT_logMod , only : LVT_logunit
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This routine computes the longitudes of the global quasi-regular Gaussian 
!  grid corresponding to starting longitude, startlon, and delta longitude,
!  dlon.
!
!   \begin{description}
!   \item [startlon]
!      starting longitude of the grid
!   \item [dlon]
!      delta longitude of the grid
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP
!BOP
!

   implicit none
! !ARGUMENTS:
   real :: startlon
   real :: dlon
!
!EOP

   integer :: i, imax

   imax = nint(360/dlon)

   GAUSSIAN_NC = imax

   allocate(gaussian_lon_array(imax))

   do i = 1, imax
      gaussian_lon_array(i) = startlon + (i-1) * dlon
      if ( gaussian_lon_array(i) > 180.0 ) then
         gaussian_lon_array(i) = gaussian_lon_array(i) - 360.0
      endif
   enddo

   write(unit=LVT_logunit,fmt=*) 'gaussian lons:', gaussian_lon_array

end subroutine gaussian_comp_lons
#endif

#if 0
!BOP
! 
! !ROUTINE: gaussian_read_grid
! \label{gaussian_read_grid}
!
! !INTERFACE:
subroutine gaussian_read_grid(imax,jmax)
! 
! !USES:
   use LVT_logMod, only : LVT_logunit

   implicit none
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This routine reads in the global quasi-regular Gaussian grid of size
!  imax,jmax.
!
!   \begin{description}
!   \item [imax]
!      total number of longitude points (along any latitude circle)
!   \item [jmax]
!      total number of latitude circles
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP
!BOP
!
! !ARGUMENTS:
   integer :: imax, jmax
!
!EOP

   real,allocatable,dimension(:,:) :: gaussian_latitudes
   real,allocatable,dimension(:,:) :: gaussian_longitudes
   integer :: i

   GAUSSIAN_NC = imax
   GAUSSIAN_NR = jmax

   allocate(gaussian_lat_array(jmax))
   allocate(gaussian_lon_array(imax))

   allocate(gaussian_latitudes(imax,jmax))
   allocate(gaussian_longitudes(imax,jmax))

   open(50,file='./GVEG/gfs_T126_lat.1gd4r',form='unformatted', &
        access='direct',recl=4*imax*jmax)
   read(50,rec=1) gaussian_latitudes
   close(50)

   open(50,file='./GVEG/gfs_T126_lon.1gd4r',form='unformatted', &
        access='direct',recl=4*imax*jmax)
   read(50,rec=1) gaussian_longitudes
   close(50)

   gaussian_lat_array = gaussian_latitudes(1,:)
   gaussian_lon_array = gaussian_longitudes(:,1)
   do i = 1, imax
      if ( gaussian_lon_array(i) > 180.0 ) then
         gaussian_lon_array(i) = gaussian_lon_array(i) - 360.0
      endif
   enddo


   !write(unit=LVT_logunit,fmt=*) 'gaussian lats:', gaussian_latitudes(1,:)
   !write(unit=LVT_logunit,fmt=*) 'gaussian lons:', gaussian_longitudes(:,1)
   write(unit=LVT_logunit,fmt=*) 'gaussian lats:', gaussian_lat_array
   write(unit=LVT_logunit,fmt=*) 'gaussian lons:', gaussian_lon_array
   

   deallocate(gaussian_latitudes)
   deallocate(gaussian_longitudes)

end subroutine gaussian_read_grid
#endif

end module gaussian_mod
