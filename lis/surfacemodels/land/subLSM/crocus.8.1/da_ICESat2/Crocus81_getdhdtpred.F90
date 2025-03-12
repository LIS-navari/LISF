!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.4
!
! Copyright (c) 2022 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
! !ROUTINE: Crocus81_getdhdtpred
! \label{Crocus81_getdhdtpred}
!
! !REVISION HISTORY:
! 8 Jan  2024 : Mahdi Navari ;Initial Specification for ICESat2 dhdt da
! 16 Jan 2025 : Mahdi Navari ;Updated for the height assimilation. Used ESMF 
!                             to get the ATL15 obs and added ens mean of the
!                             model at the bigining of the DA time window.
!                             It can also be done this process in pbs_Mod.F90 
!                             after call Crocus81_getdhdtpred  
!
! !INTERFACE:
subroutine Crocus81_getdhdtpred(n, k, obs_pred)

! !USES:
  use ESMF
  use LIS_coreMod, only : LIS_rc,LIS_surface,LIS_masterproc
  use LIS_logMod,  only : LIS_logunit, LIS_verify
  use noahmp401_lsmMod
  use LIS_DAobservationsMod
  use Crocus81_dhdt_DAlogMod

  implicit none
! !ARGUMENTS: 
  integer, intent(in)    :: n
  integer, intent(in)    :: k
  !real                   :: obs_pred(LIS_rc%ngrid(n),LIS_rc%nensem(n))  bug
  real                   :: obs_pred(LIS_rc%obs_ngrid(k),LIS_rc%nensem(n))
  real                   :: h_in_obs_space(LIS_rc%obs_ngrid(k),LIS_rc%nensem(n))
  real                   :: ens_mean(LIS_rc%obs_ngrid(k))
  real                   :: dh(LIS_rc%npatch(n,LIS_rc%lsm_index))
  real                   ::  h(LIS_rc%npatch(n,LIS_rc%lsm_index)) 
  type(ESMF_State)       :: OBS_State
  type(ESMF_Field)       :: dhdtfield
  real,    pointer       :: obsl(:)
  integer                :: status
  integer                      :: patch_index
  real                         :: pvar(LIS_rc%npatch(n,LIS_rc%lsm_index)) !patch_index))
  real                         :: ovar(LIS_rc%obs_ngrid(k),LIS_rc%nensem(n))
!
! The arguments are:
!  \begin{description}
!   \item [n]
!     index of the current nest
!   \item [k]
!     index of the DA instance
!   \item [patch\_index]
!     index of the patch to which the variable belong to
!   \item [pvar]
!     variable in the patch space
!   \item [ovar]
!     variable in the observation ensemble space
!  \end{description}
!  
!EOP

    integer                      :: c,r,t,i,m,g,gid
    real                         :: lis_gvar(LIS_rc%lnc(n)*LIS_rc%lnr(n))
    integer                      :: nlis_gvar(LIS_rc%lnc(n)*LIS_rc%lnr(n))
    !logical*1                    :: li(LIS_rc%lnc(n)*LIS_rc%lnr(n))
    !logical*1                    :: lo(LIS_rc%obs_lnc(k)*LIS_rc%obs_lnr(k))
    real                         :: obs_gvar(LIS_rc%obs_lnc(k)*LIS_rc%obs_lnr(k))
    integer                      :: iret


!EOP

! !DESCRIPTION:
!  This routine computes the obspred term for assimilation
!  instances.

  do t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     ! To assimilate height we need model ice sheet hight not dh. 
     !dh(t) = Crocus81pred_struc(n)%model_dh(t) !Crocus81pred_struc(n)%model_dh(2,t) - Crocus81pred_struc(n)%model_dh(1,t) 
     dh(t) = Crocus81pred_struc(n)%model_h(1,t) ! This is profile height at the end of the DA time window
     h (t) = Crocus81pred_struc(n)%model_h(3,t) ! This is profile height at the begining of the DA time window
     !                                            which will be used to compte the ensemble mean of cie/snow 
     !                                            profile for each grid cell 
  enddo
if(LIS_masterproc) then
print*,'getdhdtpred'
endif
        write(LIS_logunit,fmt=24)'[INFO] Get obspred from dhdt_DAlog @: ',LIS_rc%mo,'/',LIS_rc%da,'/', &
         LIS_rc%yr,LIS_rc%hr,':',LIS_rc%mn,':',LIS_rc%ss
        24  format(a40,i2.2,a1,i2.2,a1,i4,1x,i2.2,a1,i2.2,a1,i2.2)

! ICESat-2 ATL15 data was interpolated into the model grid (i.e., MAR forcing grid)
! using a Python script. The current version of the Polar Stereographic (PS) code 
! in LIS has not been fully evaluated and differs from the PS code used in the
! ICESat-2 program. Therefore, we perform the interpolation outside of LIS and 
! adjust the LIS code to carry out the analysis without any further interpolation.

!  call LIS_convertPatchSpaceToObsEnsSpace(n,k,&
!       LIS_rc%lsm_index, &
!       dh,&
!       obs_pred)

! The following lines of code, adapted from the subroutine 
! LIS_convertPatchSpaceToObsEnsSpace, convert the variable dhdt from the patch
! space to the observation ensemble grid space without interpolation.

    patch_index = LIS_rc%lsm_index 
    ovar = LIS_rc%udef
    do m=1,LIS_rc%nensem(n)
       lis_gvar  = 0.0
       nlis_gvar = 0
       obs_gvar = LIS_rc%udef

       do i=1,LIS_rc%npatch(n,patch_index), LIS_rc%nensem(n)
          t = i+m-1
          c = LIS_surface(n,patch_index)%tile(t)%col
          r = LIS_surface(n,patch_index)%tile(t)%row
          gid = c+(r-1)*LIS_rc%lnc(n)
          lis_gvar(gid)  = lis_gvar(gid) + dh(t) ! pvar(t)
          nlis_gvar(gid) = nlis_gvar(gid) + 1
       enddo
       
       do g=1,LIS_rc%lnc(n)*LIS_rc%lnr(n)
          if(nlis_gvar(g).ne.0) then
             lis_gvar(g)  = lis_gvar(g)/ &
                  nlis_gvar(g)
          else
             lis_gvar(g) = LIS_rc%udef
          endif
       enddo

          obs_gvar = lis_gvar ! both are in the same grid 
       do r=1,LIS_rc%obs_lnr(k)
          do c=1,LIS_rc%obs_lnc(k)
             if(LIS_obs_domain(n,k)%gindex(c,r).ne.-1) then
                ovar(LIS_obs_domain(n,k)%gindex(c,r),m) = &
                     obs_gvar(c+(r-1)*LIS_rc%obs_lnc(k))
             endif
          enddo
       enddo
    enddo
    obs_pred = ovar
! convert the variable h snow/ice profile height from the patch
! space to the observation ensemble grid space without interpolation.
    patch_index = LIS_rc%lsm_index
    ovar = LIS_rc%udef
    do m=1,LIS_rc%nensem(n)
       lis_gvar  = 0.0
       nlis_gvar = 0
       obs_gvar = LIS_rc%udef

       do i=1,LIS_rc%npatch(n,patch_index), LIS_rc%nensem(n)
          t = i+m-1
          c = LIS_surface(n,patch_index)%tile(t)%col
          r = LIS_surface(n,patch_index)%tile(t)%row
          gid = c+(r-1)*LIS_rc%lnc(n)
          lis_gvar(gid)  = lis_gvar(gid) + h(t) ! pvar(t)
          nlis_gvar(gid) = nlis_gvar(gid) + 1
       enddo

       do g=1,LIS_rc%lnc(n)*LIS_rc%lnr(n)
          if(nlis_gvar(g).ne.0) then
             lis_gvar(g)  = lis_gvar(g)/ &
                  nlis_gvar(g)
          else
             lis_gvar(g) = LIS_rc%udef
          endif
       enddo

          obs_gvar = lis_gvar ! both are in the same grid 
       do r=1,LIS_rc%obs_lnr(k)
          do c=1,LIS_rc%obs_lnc(k)
             if(LIS_obs_domain(n,k)%gindex(c,r).ne.-1) then
                ovar(LIS_obs_domain(n,k)%gindex(c,r),m) = &
                     obs_gvar(c+(r-1)*LIS_rc%obs_lnc(k))
             endif
          enddo
       enddo
    enddo
    h_in_obs_space = ovar
! compute ens mean 
    do r=1,LIS_rc%obs_lnr(k)
       do c=1,LIS_rc%obs_lnc(k)
          if (LIS_obs_domain(n, k)%gindex(c, r) .ne. -1) then
             ens_mean(LIS_obs_domain(n, k)%gindex(c, r)) = &
                sum(h_in_obs_space(LIS_obs_domain(n, k)%gindex(c, r), 1:LIS_rc%nensem(n))) / LIS_rc%nensem(n)
          endif
       enddo
    enddo
 
! Update the observations by adding ensemble mean of the model profile height at the begining of the DA time window.
      !call ESMF_StateGet(OBS_State,"Observation01",dhdtfield,&
      call ESMF_StateGet(LIS_OBS_State(n,k),"Observation01",dhdtfield,&
           rc=status)
      call LIS_verify(status, 'ESMF_StateGet failed in Crocus81_getdhdtpred')

      call ESMF_FieldGet(dhdtfield,localDE=0,farrayPtr=obsl,rc=status)
      call LIS_verify(status,'ESMF_FieldGet failed in Crocus81_getdhdtpred')

    do r=1,LIS_rc%obs_lnr(k)
       do c=1,LIS_rc%obs_lnc(k)
          if ( obsl(LIS_obs_domain(n,k)%gindex(c,r)) .ne. LIS_rc%udef) then
             obsl(LIS_obs_domain(n,k)%gindex(c,r)) = &
                   obsl(LIS_obs_domain(n,k)%gindex(c,r)) + &
                   ens_mean(LIS_obs_domain(n, k)%gindex(c, r))
          end if 
       end do
    end do

end subroutine Crocus81_getdhdtpred

