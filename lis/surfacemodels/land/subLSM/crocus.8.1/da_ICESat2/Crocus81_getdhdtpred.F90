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
!
! !INTERFACE:
subroutine Crocus81_getdhdtpred(n, k, obs_pred)

! !USES:
  use ESMF
  use LIS_coreMod, only : LIS_rc,LIS_surface
  use LIS_logMod,  only : LIS_logunit
  use noahmp401_lsmMod
  use LIS_DAobservationsMod
  use Crocus81_dhdt_DAlogMod

  implicit none
! !ARGUMENTS: 
  integer, intent(in)    :: n
  integer, intent(in)    :: k
  real                   :: obs_pred(LIS_rc%ngrid(n),LIS_rc%nensem(n))
  real                   :: dh(LIS_rc%npatch(n,LIS_rc%lsm_index))

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
!  This routine computes the obspred ('Hx') term for assimilation
!  instances.

!  integer                :: t

  do t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     dh(t) = Crocus81pred_struc(n)%model_dh(t) !Crocus81pred_struc(n)%model_dh(2,t) - Crocus81pred_struc(n)%model_dh(1,t) 
  enddo
print*,'getdhdtpred'
  !Crocus81pred_struc(n)%model_dh(1,t) = 0.0
  !Crocus81pred_struc(n)%model_dh(1,t) = Crocus81pred_struc(n)%model_dh(2,t)
        write(LIS_logunit,fmt=24)'[INFO] Get obspred from dhdt_DAlog  @: ',LIS_rc%mo,'/',LIS_rc%da,'/', &
         LIS_rc%yr,LIS_rc%hr,':',LIS_rc%mn,':',LIS_rc%ss
        24  format(a40,i2.2,a1,i2.2,a1,i4,1x,i2.2,a1,i2.2,a1,i2.2)

! MN: ICESat-2 ATL15 data was interpolated into the model grid (i.e., MAR 
! forcing grid) using a Python code. The current version of the PS code in
! LIS has not been fully evaluated and differs from the PS code used in the
! ICESat-2 program. For these reasons, we interpolate the observations 
! outside of LIS and modify the LIS code to perform the analysis without any interpolation. 

!  call LIS_convertPatchSpaceToObsEnsSpace(n,k,&
!       LIS_rc%lsm_index, &
!       dh,&
!       obs_pred)

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
 
end subroutine Crocus81_getdhdtpred

