!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
!
! !ROUTINE: get_Col_de_Porte
! \label{get_Col_de_Porte}
!
! !REVISION HISTORY:
! 07 Oct 2010: David Mocko, Updated for Loobos test case
! 16 Sep 2019: Mahdi Navari, Updated for Col de Porte test case
!
! !INTERFACE:
subroutine get_Col_de_Porte(n,findex)
! !USES:
  use LIS_coreMod, only : LIS_rc
  use LIS_timeMgrMod, only : LIS_tick
  use LIS_logMod, only : LIS_logunit
  use Col_de_Porte_forcingMod, only : Col_de_Porte_struc

  implicit none
! !ARGUMENTS:
  integer, intent(in) :: n
  integer, intent(in) :: findex

!
! !DESCRIPTION:
!  Opens, reads, and interpolates the Col_de_Porte station data.
!  At the beginning of a simulation, the code reads the most
!  recent past data, and the nearest future data.  These two
!  datasets are used to temporally interpolate the data to
!  the current model timestep.
!
!  The arguments are:
!  \begin{description}
!  \item[n]
!    index of the nest
!  \end{description}
!
!  The routines invoked are:
!  \begin{description}
!  \item[LIS\_tick](\ref{LIS_tick}) \newline
!    determines the Col_de_Porte data times
!  \item[read\_Col_de_Porte](\ref{read_Col_de_Porte}) \newline
!    Interpolates the appropriate Col_de_Porte station data to LIS grid
!  \end{description}
!EOP

  integer :: doy1, yr1, mo1, da1, hr1, mn1, ss1, ts1
  integer :: doy2, yr2, mo2, da2, hr2, mn2, ss2, ts2
  real :: gmt1,gmt2
  real*8 :: timenow,time1,time2
  integer :: movetime       ! if 1=move time 2 data into time 1
  integer :: f,t
  integer :: order

  !      write(LIS_logunit,*) 'starting get_Col_de_Porte'
  movetime = 0

  yr1 = LIS_rc%yr           !current time
  mo1 = LIS_rc%mo
  da1 = LIS_rc%da
  hr1 = LIS_rc%hr
  mn1 = LIS_rc%mn
  ss1 = 0
  ts1 = 0
  !      write(LIS_logunit,*) yr1,mo1,da1,hr1,mn1,ss1
  call LIS_tick(timenow,doy1,gmt1,yr1,mo1,da1,hr1,mn1,ss1,real(ts1))

  yr1 = LIS_rc%yr
  mo1 = LIS_rc%mo
  da1 = LIS_rc%da
  hr1 = LIS_rc%hr
  mn1 = LIS_rc%mn
  ss1 = 0
  ts1 = 0
  call LIS_tick(time1,doy1,gmt1,yr1,mo1,da1,hr1,mn1,ss1,real(ts1))

  yr2 = LIS_rc%yr           !next hour
  mo2 = LIS_rc%mo
  da2 = LIS_rc%da
  hr2 = LIS_rc%hr
  mn2 = LIS_rc%mn
  ss2 = 0
  ts2 = 60*60  !MN : forcing data is available every 1 hr    ! 60*30
  call LIS_tick(time2,doy2,gmt2,yr2,mo2,da2,hr2,mn2,ss2,real(ts2))

  Col_de_Porte_struc(n)%findtime1 = 0
  Col_de_Porte_struc(n)%findtime2 = 0
  !write(LIS_logunit,*) 'timenow: ',timenow
  !write(LIS_logunit,*) 'time1: ',time1
  !write(LIS_logunit,*) 'time2: ',time2
  !write(LIS_logunit,*) 'starttime: ',Col_de_Porte_struc(n)%starttime
  if ((timenow.ge.Col_de_Porte_struc(n)%starttime).and.              &
       (.not.Col_de_Porte_struc(n)%startRead)) then
     Col_de_Porte_struc(n)%findtime1 = 1
     Col_de_Porte_struc(n)%findtime2 = 1
     Col_de_Porte_struc(n)%startRead = .true.
     movetime = 0
  endif
  if (Col_de_Porte_struc(n)%startRead) then
     if (timenow.ge.Col_de_Porte_struc(n)%Col_de_Portetime2) then
        movetime = 1
        Col_de_Porte_struc(n)%findtime2 = 1
     endif

     !Time to open file and start reading.
     !keep on reading until the obstime is reached.
     if (Col_de_Porte_struc(n)%findtime1.eq.1) then
        write(LIS_logunit,*) 'reading time1 data...'
        order = 1
        call read_Col_de_Porte(n,findex,order,1) !,222
!        call read_Col_de_Porte(n, findex, order, Col_de_Porte_filename, ferror)
        Col_de_Porte_struc(n)%Col_de_Portetime1 = time1
     endif
     if (movetime.eq.1) then 
        Col_de_Porte_struc(n)%Col_de_Portetime1 =                      &
             Col_de_Porte_struc(n)%Col_de_Portetime2
        do f = 1,11
           do t = 1,LIS_rc%ngrid(n)
              Col_de_Porte_struc(n)%metdata1(f,t) = Col_de_Porte_struc(n)%metdata2(f,t)
           enddo
        enddo
     endif

     if (Col_de_Porte_struc(n)%findtime2.eq.1) then
        write(LIS_logunit,*) 'reading time2 data...'
        order = 2
        call read_Col_de_Porte(n,findex,order,2) ! ,222
!        call read_Col_de_Porte(n, findex, order, Col_de_Porte_filename, ferror)
        Col_de_Porte_struc(n)%Col_de_Portetime2 = time2
     endif
  endif

end subroutine get_Col_de_Porte

