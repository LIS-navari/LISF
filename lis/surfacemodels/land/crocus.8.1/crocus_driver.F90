!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.2
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
! July 17 2019 Mahdi Navari started for implementing Crocus 
!
!1- define dimension for variables
!2- use xls tool to generate LIS related files 


SUBROUTINE crocus_driver(n,		&
			nsnow,				& 
			nimpur,				&
			!ttile,					& 
			!latitude,longitude,		&
			year,					&
			month, 				&
			day, 					&
			hour,				&
			minute,				&
                     SNOWRES_opt,			&
			!TPTIME,				&
			OMEB_BOOL,				&
 			GLACIER_BOOL,			&
			HIMPLICIT_WIND_opt,		&
			!ZP_PEW_A_COEF,		& ! coefficients for atmospheric coupling. In offline mode, they are all equal to 0
			!ZP_PEW_B_COEF,		&
			!ZP_PET_A_COEF,		& 
			!ZP_PEQ_A_COEF,		&
			!ZP_PET_B_COEF,		&
			!ZP_PEQ_B_COEF,		&
			SNOWSWE,		&
			SNOWRHO,		&
			SNOWHEAT,		&
			SNOWALB,			& 
			SNOWGRAN1,		&
			SNOWGRAN2,		&
			SNOWHIST,		& 
			SNOWAGE,		&
			!ZP_SNOWIMPUR,		&
			PTSTEP,				&
			PPS, 				&
			RRSNOW, 			&
			SRSNOW,			&
			!ZP_PSN3L,			& ! compute this using rainfall and snowfall 
 			TA,				&
 			TG,				& ! TG(:,1)
			SW_RAD,			&
			QA,				&
			!ZP_VMOD,			&  !<<<<<<<<<<<< NOTE:  If LIS provides U and V we have to compute 
									!  ZP_VMOD = (U^2 + V^2)^0.5 and correct the tool kit to get U and 
									! V as forcing >>>>>>>>>>>>>>>>
			Wind_E,			& ! Eastward Wind
			Wind_N,			& ! Northward Wind
			LW_RAD,			&
			!ZP_RHOA,			& ! compute this using presure, temp, humidity  
			UREF,				&
			!ZP_EXNS,			& !compute this using presure at surface
			!ZP_EXNA,			& !apporximate this using presure at lowest atmos. level
			SLOPE,			      & ! replace ZP_DIRCOSZW with SLOPE and compute the cosine in the driver
			ZREF,			      &
			Z0NAT,			      &
			Z0EFF,			     &
			Z0HNAT ,                &
			ALB,                         &
			SOILCOND,	            &
			D_G,				&! D_G(:,1)
			SNOWLIQ,			&
			SNOWTEMP,		&
			SNOWDZ,			&
			THRUFAL,			&
			GRNDFLUX,		&
			!ZP_EVAPCOR,			&
			!ZP_GFLXCOR,			&
			! ZP_SWNETSNOW,		&
			!ZP_SWNETSNOWS, 	&
			!ZP_LWNETSNOW,		&
			!ZP_RNSNOW,			&
			!ZP_HSNOW,			& 
			!ZP_GFLUXSNOW,		&
			!ZP_HPSNOW, 			&
			!ZP_LES3L,			&
			!ZP_LEL3L,			&
			!ZP_EVAP,				&
			SNDRIFT,			&! snowdrift only modifies the  surface snow properties 
								!    (density and microstructure) but without any mass change
								!    unless you use the  SNOWDRIFT_SUBLIM option
								!    which can activate sublimation in case of snow drift 
								!    but without any erosion or accumulation fluxes between
								!    grid points (METEO FRANCE ticket # 1592).
			RI_n,				&
			EMISNOW,			&
			CDSNOW,			&
			USTARSNOW,		&
			CHSNOW,			&
			SNOWHMASS,		&
			QS,				&
			PERMSNOWFRAC, 	& ! ZP_VEGTYPE
			!ZP_ZENITH,			& ! compute this here 
			!ZP_ANGL_ILLUM,		& ! compute this here 
			LAT,				&
 			LON,				&
			! ZP_BLOWSNW,		& ! We can not use snow_sytron it is designed for the 
								!    METEO FRANCEinternal use .(METEO FRANCE ticket # 1592).
								!    Therefore it will be defined as a local variable.
			SNOWDRIFT_opt,		& !IO%CSNOWDRIFT
			SNOWDRIFT_SUBLIM_BOOL,& ! IO%LSNOWDRIFT_SUBLIM 
			SNOW_ABS_ZENITH_BOOL,	&! IO%LSNOW_ABS_ZENITH
 			SNOWMETAMO_opt,	& !IO%CSNOWMETAMO
			SNOWRAD_opt,		&! IO%CSNOWRAD
			ATMORAD_BOOL,		&!IO%LATMORAD
			!ZP_DIR_SW, 		&
			!ZP_SCA_SW,		&
			!ZP_SPEC_ALB, 		&
			!ZP_DIFF_RATIO,	&
			IMPWET,			&
			IMPDRY,			&
			SNOWFALL_opt,       &!IO%CSNOWFALL
 			SNOWCOND_opt,     &! IO%CSNOWCOND
			SNOWHOLD_opt,     &! IO%CSNOWHOLD
			SNOWCOMP_opt,    &! IO%CSNOWCOMP
			SNOWZREF_opt,      &! IO%CSNOWZREF
			SNOWMAK_dz,        &! 
 			SNOWCOMPACT_BOOL,  &! IO%LSNOWCOMPACT_BOOL
			SNOWMAK_BOOL,    &! IO%LSNOWMAK_BOOL
			SNOWTILLER_BOOL,  &! IO%LSNOWTILLER
			SELF_PROD_BOOL,     &! IO%LSELF_PROD
			SNOWMAK_PROP_BOOL,   &! IO%LSNOWMAK_PROP
 			PRODSNOWMAK_BOOL,    &! IO%LPRODSNOWMAK)
			SLOPE_DIR)
  
  USE LIS_coreMod, only    : LIS_rc
  !USE LIS_logMod,  only    : LIS_logunit, LIS_endrun
  !USEe LIS_timeMgrMod, only : LIS_date2time, LIS_tick
 USE MODD_TYPE_DATE_SURF, ONLY: DATE_TIME! MN: declaration of temporal types.  DATE_TIME%.... (YEAR, MONTH, DAY)
 USE MODD_CSTS,         ONLY : XRD, XP00, XCPD, XG, XBOLTZ, &
								XAVOGADRO, XMD,XMV,XRD,XRV,XPI 
 USE modi_surface_cd ! for drag coefficient for momentum 
 USE modi_surface_aero_cond  ! for drag coefficient for heat 


  implicit none

TYPE(DATE_TIME) 			:: TPTIME  ! current date and time
!*      0.1    declarations of arguments
integer, intent(in) :: n			! nest id 
integer, intent(in) :: nsnow		! number of snow layers [-] 
integer, intent(in) :: nimpur		! number of impurities [-] 
!  integer, intent(in) :: ttile		! tile id
 !real,       intent(in) :: latitude		! latitude in decimal degree [-]
 !real,       intent(in) :: longitude		! longitude in decimal degree [-]
  integer, intent(in) :: year		! year of the current time step [-]
  integer, intent(in) :: month		! month of the current time step [-] 
  integer, intent(in) :: day		! day of the current time step [-]
  integer, intent(in) :: hour		! hour of the current time step [-] 
  integer, intent(in) :: minute		! minute of the current time step [-]

 CHARACTER(LEN=3), INTENT(IN)		:: SNOWRES_opt
!					SNOWRES_opt  = ISBA-SNOW3L turbulant exchange option
!					'DEF' = Default: Louis (ISBA: Noilhan and Mahfouf 1996)
!					'RIL' = Limit Richarson number under very stable
!                                              conditions (currently testing)
!					'M98'  = Martin et Lejeune 1998 : older computation for turbulent fluxes coefficents in Crocus

LOGICAL, INTENT(IN)			:: OMEB_BOOL       
! 					True = coupled to MEB. This means surface fluxes ae IMPOSED
! 					as an upper boundary condition to the explicit snow schemes. 
! 					If = False, then energy
!					budget and fluxes are computed herein.
LOGICAL, INTENT(IN)			 :: GLACIER_BOOL  
!					True = Over permanent snow and ice, 
! 					initialise WGI=WSAT,
! 					Hsnow>=10m and allow 0.8<SNOALB<0.85
!					False = No specific treatment
 CHARACTER(LEN=3), INTENT(IN)       :: HIMPLICIT_WIND_opt   
!					wind implicitation option
!					'OLD' = direct
!					'NEW' = Taylor serie, order 1
!REAL,  INTENT(IN)      :: ZP_PEW_A_COEF, ZP_PEW_B_COEF,	&
!                                        ZP_PET_A_COEF, ZP_PEQ_A_COEF, ZP_PET_B_COEF,		&
!                                        ZP_PEQ_B_COEF  
!					PPEW_A_COEF = wind coefficient (m2s/kg)
!					PPEW_B_COEF = wind coefficient (m/s)
!					PPET_A_COEF = A-air temperature coefficient
! 					PPET_B_COEF = B-air temperature coefficient
!					PPEQ_A_COEF = A-air specific humidity coefficient
! 					PPEQ_B_COEF = B-air specific humidity coefficient
REAL,  INTENT(INOUT)  :: SNOWSWE(nsnow)
!    					Z_PSNOWSWE  = Snow layer(s) liquid Water Equivalent (SWE:kg m-2)
REAL,  INTENT(INOUT)  :: SNOWRHO(nsnow)
!					SNOWRHO  = Snow layer(s) averaged density (kg/m3)
REAL,  INTENT(INOUT)  :: SNOWHEAT(nsnow)
!					SNOWHEAT = Snow layer(s) heat content (J/m2)
REAL,  INTENT(INOUT)   :: SNOWALB
!					SNOWALB = Prognostic surface snow albedo
! 						(does not include anything but
! 						the actual snow cover)
REAL,  INTENT(INOUT)  :: SNOWGRAN1(nsnow) !SNOWGRAN1 = Snow layers grain feature 1
REAL,  INTENT(INOUT)  :: SNOWGRAN2(nsnow) !SNOWGRAN2 = Snow layer grain feature 2
REAL,  INTENT(INOUT)  :: SNOWHIST(nsnow)  ! SNOWHIST  = Snow layer grain historical
! 						parameter (only for non dendritic snow)
REAL,  INTENT(INOUT)  :: SNOWAGE(nsnow)  ! Snow grain age

! REAL,  INTENT(INOUT)  :: ZP_SNOWIMPUR  
!						Snow impurity content (g) (LOCATION,LAYER,NIMPUR)) Impur type :1/BC 2/Dust
! 						CSNOWRAD='B92'  In that case, the direct-diffuse partition is not used, a 3 band 
! 						spectral fixed repartition is used from the global radiation and the impact of the 
!						deposition of light absorbing impurities is parameterized from the age of snow 
!						as detailed in Vionnet et al 2012.

REAL, INTENT(IN)      :: PTSTEP !   PTSTEP    = time step of the integration
REAL, INTENT(IN)      :: PPS ! ZP_PS   = surface pressure
REAL,  INTENT(IN)      :: RRSNOW  !  RRSNOW  = rain rate [kg/(m2 s)]
REAL,  INTENT(IN)      :: SRSNOW  !  SRSNOW   = snow rate (SWE) [kg/(m2 s)]
! REAL,  INTENT(IN)      	:: ZP_PSN3L !   ZP_PSN3L    = snow fraction
REAL,  INTENT(IN)      :: TA  !   TA   = atmospheric temperature at level za (K)
REAL,  INTENT(IN)      :: TG  !	 TG   = Surface soil temperature (effective
! 						temperature the of layer lying below snow)
REAL,  INTENT(IN)      :: SW_RAD!	SW_RAD = incoming solar radiation (W/m2)
REAL,  INTENT(IN)      :: QA   ! QA = atmospheric specific humidity at level za
!REAL,  INTENT(IN)      :: ZP_VMOD  !ZP_VMOD = modulus of the wind parallel to the orography (m/s)
REAL,  INTENT(IN)      :: Wind_E  !  Eastward Wind   
REAL,  INTENT(IN)      :: Wind_N ! 	Northward Wind
REAL,  INTENT(IN)      :: LW_RAD !	LW_RAD = atmospheric infrared radiation (W/m2)!
!REAL,  INTENT(IN)      :: ZP_RHOA! 		ZP_RHOA     = air density
REAL,  INTENT(IN)      :: UREF !		UREF      = reference height of the wind
!REAL,  INTENT(IN)      	:: ZP_EXNS!		ZP_EXNS      = Exner function at surface
!REAL,  INTENT(IN)      	:: ZP_EXNA!		ZP_EXNA     = Exner function at lowest atmos level
!REAL,  INTENT(IN)      :: ZP_DIRCOSZW!	ZP_DIRCOSZW = Cosinus of the angle between the
!						normal to the surface and the vertical
REAL,  INTENT(IN)      :: SLOPE !	SLOPE = Slope
REAL,  INTENT(IN)      :: ZREF !  		ZREF      = reference height of the first
!						atmospheric level
REAL, INTENT(IN)      :: Z0NAT ! 		Z0NAT (PZ0)       = grid box average roughness length
REAL,  INTENT(IN)      :: Z0EFF! 		Z0EFF     = roughness length for momentum      
REAL,  INTENT(IN)      :: Z0HNAT !		Z0HNAT (PZOH)      = grid box average roughness length for heat
REAL,  INTENT(IN)      :: ALB !		ALB       = soil/vegetation albedo
REAL,  INTENT(IN)      :: SOILCOND!	SOILCOND = soil thermal conductivity [W/(m K)]   
REAL,  INTENT(IN)      :: D_G!		D_G      = Assumed first soil layer thickness (m)
!						Used to calculate ground/snow heat flux
REAL,  INTENT(INOUT)    :: SNOWLIQ(nsnow)! 	SNOWLIQ  = Snow layer(s) liquid water content (m)
REAL,  INTENT(INOUT)  :: SNOWTEMP(nsnow)!	SNOWTEMP = Snow layer(s) temperature (m)
REAL,  INTENT(INOUT)    :: SNOWDZ(nsnow)!	SNOWDZ   = Snow layer(s) thickness (m)
REAL,  INTENT(OUT)    :: THRUFAL
!					THRUFAL  = rate that liquid water leaves snow pack:
! 						 paritioned into soil infiltration/runoff by ISBA [kg/(m2 s)]
REAL,  INTENT(INOUT):: GRNDFLUX 
!					GRNDFLUX = soil/snow interface heat flux (W/m2)
!REAL,  INTENT(OUT)    :: ZP_EVAPCOR
!					ZP_EVAPCOR  = evaporation/sublimation correction term:
!						extract any evaporation exceeding the
!						actual snow cover (as snow vanishes)
! 						and apply it as a surface soil water
! 						sink. [kg/(m2 s)]
!REAL,  INTENT(OUT)     :: ZP_GFLXCOR
!					ZP_GFLXCOR  = flux correction to underlying soil for vanishing snowpack
!						(to put any energy excess from snow to soil) (W/m2)
!REAL,  INTENT(INOUT)	:: ZP_SWNETSNOW
!					ZP_SWNETSNOW = net shortwave radiation entering top of snowpack 
!						(W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
!REAL,  INTENT(INOUT)	:: ZP_SWNETSNOWS
!					ZP_SWNETSNOWS= net shortwave radiation in uppermost layer of snowpack 
! 						(W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
!						Used for surface energy budget diagnostics
!REAL,  INTENT(INOUT)	:: ZP_LWNETSNOW
!					ZP_LWNETSNOW = net longwave radiation entering top of snowpack 
! 						(W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
!REAL,  INTENT(INOUT)    :: ZP_RNSNOW!	ZP_RNSNOW     = net radiative flux from snow (W/m2)
!REAL,  INTENT(INOUT)    :: ZP_HSNOW!	ZP_HSNOW      = sensible heat flux from snow (W/m2)
!REAL,  INTENT(INOUT)    :: ZP_GFLUXSNOW!ZP_GFLUXSNOW  = net heat flux from snow (W/m2)
!REAL,  INTENT(INOUT)    :: ZP_HPSNOW!	ZP_HPSNOW     = heat release from rainfall (W/m2)
!REAL,  INTENT(INOUT)    :: ZP_LES3L!	ZP_LES3L      = evaporation heat flux from snow (W/m2)
!REAL,  INTENT(INOUT)    :: ZP_LEL3L!	ZP_LEL3L      = sublimation (W/m2)
!REAL,  INTENT(INOUT)    :: ZP_EVAP!	ZP_EVAP       = total evaporative flux (kg/m2/s)
REAL,  INTENT(OUT)	    :: SNDRIFT!	SNDRIFT    = blowing snow sublimation (kg/m2/s)
REAL,  INTENT(OUT)    :: RI_n
!					RI_n = Ridcharson number (If not OMED initalized to undefined in the snow3L_isba.F90)
REAL,  INTENT(OUT)	:: EMISNOW!		EMISNOW    = snow surface emissivity
REAL,  INTENT(OUT)    :: CDSNOW!	CDSNOW     = drag coefficient for momentum over snow
REAL,  INTENT(OUT)    :: USTARSNOW!		USTARSNOW      = friction velocity over snow (m/s)
REAL,  INTENT(OUT)    :: CHSNOW!	 CHSNOW = drag coefficient for heat over snow
REAL,  INTENT(OUT)	:: SNOWHMASS
!					SNOWHMASS  = heat content change due to mass
!						changes in snowpack (J/m2): for budget calculations only.
REAL, INTENT(OUT)	:: QS
!					QS = surface humidity
REAL, INTENT(IN)		:: PERMSNOWFRAC 
!					PPERMSNOWFRAC  = fraction of permanet snow/ice
!REAL, INTENT(IN)        :: ZP_ZENITH!	 solar zenith angle
!REAL, INTENT(IN)        :: ZP_ANGL_ILLUM 
!					Effective illumination angle, Angle between the sun and the 
! 					normal to the ground (=zenith if no slope) used in TARTES
REAL, INTENT(IN)           :: LAT
REAL, INTENT(IN)           :: LON
 CHARACTER(4), INTENT(IN)            :: SNOWDRIFT_opt        ! Snowdrift scheme :
! 					Mechanical transformation of snow grain and compaction + effect of wind 
!					on falling snow properties
!						'NONE': No snowdrift scheme
! 						'DFLT': falling snow falls as purely dendritic
!						'GA01': Gallee et al 2001
!						'VI13': Vionnet et al 2013
LOGICAL, INTENT(IN)			:: SNOWDRIFT_SUBLIM_BOOL !	activate sublimation during drift
LOGICAL, INTENT(IN)			:: SNOW_ABS_ZENITH_BOOL !	activate parametrization of solar absorption for polar regions
 CHARACTER(LEN=3), INTENT(IN)			:: SNOWMETAMO_opt 
!					B92 (historical version, Brun et al 92), C13, T07, F06 (see Carmagnola et al 2014)
 CHARACTER(LEN=3), INTENT(IN)			:: SNOWRAD_opt 
!					 Radiative transfer scheme. HSNOWRAD=B92 Brun et al 1992.  
! 						HSNOWRAD=T17 (Tuzet et al. 2017) (Libois et al. 2013) 
!						TARTES with impurities content scheme
LOGICAL, INTENT(IN)		:: ATMORAD_BOOL !	activate atmotartes scheme
!REAL,  INTENT(IN)			:: ZP_DIR_SW
!REAL,  INTENT(IN)			:: ZP_SCA_SW !	direct and diffuse spectral irradiance (W/m2/um)
!REAL, INTENT(OUT)		:: ZP_SPEC_ALB 
!REAL,  INTENT(OUT)		:: ZP_DIFF_RATIO 
!					spectral albedo and diffuse to total irradiance ratio
REAL, INTENT(IN)			:: IMPWET(nimpur)  
REAL, INTENT(IN)			:: IMPDRY(nimpur) 
!					Dry and wet deposit coefficient from Forcing File(g/m²/s)
 CHARACTER(len=3), INTENT(IN)		:: SNOWFALL_opt
!					New options for multiphysics version (Cluzet et al 2016)
!					Falling snow scheme
! 					SNOWFALL_opt=V12 Vionnet et al. 2012 from Brun et al. 1989
! 					SNOWFALL_opt=A76 Anderson et al. 1976
! 					SNOWFALL_opt=S02 Lehning el al. 2002
! 					SNOWFALL_opt=P75 Pahaut 1975
! 					SNOWFALL_opt=NZE Constant density 200 kg/m3 (who knows ?)             
 CHARACTER(len=3), INTENT(IN)		:: SNOWCOND_opt
! 					Thermal conductivity scheme
! 					SNOWCOND_opt=Y81 default Crocus from Yen et al. 1981
! 					SNOWCOND_opt=I02 ISBA_ES snow conductivity parametrization (Boone et al. 2002)
!					SNOWCOND_opt=C11 Calonne et al. 2011 snow conductivity parametrization
 CHARACTER(len=3), INTENT(IN)		 :: SNOWHOLD_opt
! 					 liquid water content scheme
! 					SNOWHOLD_opt=B92 default Crocus from Brun et al. 1992 or Vionnet et al. 2012
! 					SNOWHOLD_opt=B02 ISBA_ES  parametrization (Boone et al. 2002)
! 					SNOWHOLD_opt=O04 CLM parametrization (Oleson et al 2004)
! 					SNOWHOLD_opt=S02 SNOWPACK aprametrization (Lehning et al 2002)
 CHARACTER(len=3), INTENT(IN)		:: SNOWCOMP_opt

 CHARACTER(len=3), INTENT(IN)		:: SNOWZREF_opt
! 					reference height is constant or variable from the snow surface
!					SNOWZREF_opt='CST' constant reference height from the snow surface
!					SNOWZREF_opt='VAR' variable reference height from the snow
!						surface (i.e. constant      from the ground)
REAL, INTENT(IN)		:: SNOWMAK_dz        
!					Snowmaking thickness (m)
LOGICAL, INTENT(IN)		:: SNOWCOMPACT_BOOL
LOGICAL, INTENT(IN)		:: SNOWMAK_BOOL
LOGICAL, INTENT(IN)		:: SNOWTILLER_BOOL
LOGICAL, INTENT(IN)		:: SELF_PROD_BOOL
LOGICAL, INTENT(IN)		:: SNOWMAK_PROP_BOOL
LOGICAL, INTENT(INOUT)		:: PRODSNOWMAK_BOOL
REAL, INTENT(IN)			:: SLOPE_DIR  ! typical slope aspect in the grid 
REAL :: ZP_PEW_A_COEF = 0  ! coefficients for atmospheric coupling. In offline mode, they are all equal to 0
REAL :: ZP_PEW_B_COEF = 0
REAL :: ZP_PET_A_COEF = 0
REAL :: ZP_PEQ_A_COEF = 0
REAL :: ZP_PET_B_COEF = 0	
REAL :: ZP_PEQ_B_COEF = 0 
REAL :: ZP_SWNETSNOW = 0
REAL :: ZP_SWNETSNOWS = 0
REAL :: ZP_LWNETSNOW = 0
REAL :: ZP_RNSNOW = 0
REAL :: ZP_HSNOW = 0
REAL :: ZP_GFLUXSNOW = 0  ! it has not been initialized  in the CALL_MODEL
REAL :: ZP_HPSNOW = 0 
REAL :: ZP_LES3L = 0
REAL :: ZP_LEL3L = 0
REAL :: ZP_EVAP = 0
REAL :: ZP_EVAPCOR = 0 ! it has not been initialized  in the CALL_MODEL
REAL :: ZP_GFLXCOR = 0 ! it has not been initialized  in the CALL_MODEL

! ***************************************************************************
! Local variable 
! ***************************************************************************
REAL, DIMENSION(1,nsnow)  :: SNOWSWEinout
!                                      Z_PSNOWSWE  = Snow layer(s) liquid Water Equivalent (SWE:kg m-2)
REAL, DIMENSION(1,nsnow)  :: SNOWRHOinout
!                                      SNOWRHO  = Snow layer(s) averaged density (kg/m3)
REAL, DIMENSION(1,nsnow)  :: SNOWHEATinout
!                                      SNOWHEAT = Snow layer(s) heat content (J/m2)
REAL, DIMENSION(1)            :: SNOWALBinout
!                                      SNOWALB = Prognostic surface snow albedo
!                                                 (does not include anything but
!                                                 the actual snow cover)
REAL, DIMENSION(1,nsnow)  :: SNOWGRAN1inout
!                                      SNOWGRAN1 = Snow layers grain feature 1
REAL, DIMENSION(1,nsnow)  :: SNOWGRAN2inout
!                                      SNOWGRAN2 = Snow layer grain feature 2
REAL, DIMENSION(1,nsnow)  ::SNOWHISTinout
!                                      SNOWHIST  = Snow layer grain historical
!                                                   parameter (only for non
!                                                   dendritic snow)
REAL, DIMENSION(1,nsnow)  :: SNOWAGEinout  ! Snow grain age
REAL, DIMENSION(1,nsnow,nimpur)  :: ZP_SNOWIMPURinout    
! 					Snow impurity content (g) (LOCATION,LAYER,NIMPUR)) Impur type :1/BC 2/Dust
!REAL, INTENT(IN)         :: PTSTEP 		! PTSTEP    = time step of the integration
REAL, DIMENSION(1)                      :: PPSin			! ZP_PS     = surface pressure
REAL, DIMENSION(1)                      :: ZP_PAin		! pressure at lowest atmos. level
REAL, DIMENSION(1)                      :: SRSNOWin 	! ZP_SRSNOW      = snow rate (SWE) [kg/(m2 s)]
REAL, DIMENSION(1)                      :: RRSNOWin	! ZP_RRSNOW     = rain rate [kg/(m2 s)]
REAL, DIMENSION(1)                      :: ZP_PSN3Lin		! ZP_PSN3L   = snow fraction
REAL, DIMENSION(1)                      :: TAin			! ZP_TA        = atmospheric temperature at level za (K)
REAL, DIMENSION(1)                       :: TGin			! ZP_TG        = Surface soil temperature (effective
!                                                       temperature the of layer lying below snow)
REAL, DIMENSION(1)                       :: SW_RADin	! ZP_SW_RAD = incoming solar radiation (W/m2)
REAL, DIMENSION(1)                      :: QAin		!Z_PQA = atmospheric specific humidity at level za 
REAL, DIMENSION(1)                      :: ZP_VMODin	! ZP_VMOD = modulus of the wind parallel to the orography (m/s)
REAL, DIMENSION(1)                       :: Wind_Ein
REAL, DIMENSION(1)                      :: Wind_Nin
REAL, DIMENSION(1)                      :: LW_RADin	! ZP_LW_RAD = atmospheric infrared radiation (W/m2)
REAL, DIMENSION(1)                      :: ZP_RHOAin		! ZP_RHOA  = air density
REAL, DIMENSION(1)                      :: UREFin 		! ZP_UREF  = reference height of the wind
REAL, DIMENSION(1)                      :: ZP_EXNSin		! ZP_EXNS  = Exner function at surface
REAL, DIMENSION(1)                      :: ZP_EXNAin		! ZP_EXNA  = Exner function at lowest atmos level
REAL, DIMENSION(1)                      :: ZP_DIRCOSZWin ! ZP_DIRCOSZW = Cosinus of the angle between the
!                                                  normal to the surface and the vertical
REAL, DIMENSION(1)                      :: SLOPEin ! 	ZP_SLOPE  = Slope (degree, from LDT )
REAL , DIMENSION(1)                     :: ZREFin		! ZP_ZREF  = reference height of the first atmospheric level
REAL , DIMENSION(1)                     :: Z0NATin 		! Z0NAT (PZ0)   = grid box average roughness length
REAL , DIMENSION(1)                     :: Z0EFFin		! Z0EFF  = roughness length for momentum
REAL, DIMENSION(1)                      :: Z0HNATin 	! Z0HNAT (PZOH)  = grid box average roughness length for heat                                      					 
REAL, DIMENSION(1)                       :: ALBin 		! ALB  = soil/vegetation albedo
REAL, DIMENSION(1)                       :: SOILCONDin! SOILCOND = soil thermal conductivity [W/(m K)]
REAL, DIMENSION(1)                       :: D_Gin 		! D_G  = Assumed first soil layer thickness (m)
!                                                  Used to calculate ground/snow heat flux
REAL, DIMENSION(1,nsnow)   :: SNOWLIQout	! SNOWLIQ  = Snow layer(s) liquid water content (m)
REAL, DIMENSION(1,nsnow)   :: SNOWDZout 		! SNOWDZ   = Snow layer(s) thickness (m)
REAL, DIMENSION(1,nsnow)   :: SNOWTEMPinout ! SNOWTEMP = Snow layer(s) temperature (m)
REAL, DIMENSION(1)                       :: THRUFALout	! THRUFAL  = rate that liquid water leaves snow pack:
!                                                  paritioned into soil infiltration/runoff by ISBA [kg/(m2 s)]
REAL, DIMENSION(1)                       :: GRNDFLUXinout 
!					GRNDFLUX = soil/snow interface heat flux (W/m2)
REAL, DIMENSION(1)                       :: ZP_EVAPCORout	! ZP_EVAPCOR  = evaporation/sublimation correction term:
!                                                  extract any evaporation exceeding the
!                                                  actual snow cover (as snow vanishes)
!                                                  and apply it as a surface soil water
!                                                  sink. [kg/(m2 s)]
REAL, DIMENSION(1)                       :: ZP_GFLXCORout
! 					ZP_GFLXCOR  = flux correction to underlying soil for vanishing snowpack
!                                                  (to put any energy excess from snow to soil) (W/m2)                            
REAL, DIMENSION(1)                       :: ZP_SWNETSNOWout
!					ZP_SWNETSNOW = net shortwave radiation entering top of snowpack 
!                                                  (W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
REAL , DIMENSION(1)                      :: ZP_SWNETSNOWSout
!					ZP_SWNETSNOWS= net shortwave radiation in uppermost layer of snowpack 
!                                                  (W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
!                                                   Used for surface energy budget diagnostics
REAL, DIMENSION(1)                       :: ZP_LWNETSNOWout
! 					ZP_LWNETSNOW = net longwave radiation entering top of snowpack 
!                                                  (W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
REAL, DIMENSION(1)                       :: ZP_RNSNOWout	! ZP_RNSNOW     = net radiative flux from snow (W/m2)
REAL, DIMENSION(1)                       :: ZP_HSNOWout	! ZP_HSNOW      = sensible heat flux from snow (W/m2)
REAL, DIMENSION(1)                       :: ZP_GFLUXSNOWout	! ZP_GFLUXSNOW  = net heat flux from snow (W/m2)
REAL, DIMENSION(1)                       :: ZP_HPSNOWout	! ZP_HPSNOW     = heat release from rainfall (W/m2)
REAL, DIMENSION(1)                       :: ZP_LES3Lout 		! ZP_LES3L      = sublimation (W/m2) 
REAL, DIMENSION(1)                       :: ZP_LEL3Lout	! ZP_LEL3L      = evaporation heat flux from snow (W/m2)
REAL, DIMENSION(1)                       :: ZP_EVAPout	! ZP_EVAP       = total evaporative flux (kg/m2/s)
REAL, DIMENSION(1)                       :: SNDRIFTout	! SNDRIFT    = blowing snow sublimation (kg/m2/s)
REAL, DIMENSION(1)                       :: RI_nout		! RI_n = Ridcharson number
REAL, DIMENSION(1)                       :: EMISNOWout	! EMISNOW    = snow surface emissivity
REAL, DIMENSION(1)                       :: CDSNOWout	! CDSNOW     = drag coefficient for momentum over snow
REAL, DIMENSION(1)                       :: USTARSNOWout	! USTARSNOW      = friction velocity over snow (m/s)
REAL, DIMENSION(1)                      :: CHSNOWout	! CHSNOW = drag coefficient for heat over snow
REAL, DIMENSION(1)                      :: SNOWHMASSout 
!					SNOWHMASS  = heat content change due to mass
!                                                    changes in snowpack (J/m2): for budget
!                                                    calculations only.
REAL, DIMENSION(1)                       :: QSout		! QS = surface humidity
REAL, DIMENSION(1)                       :: PERMSNOWFRACin 
!					PPERMSNOWFRAC  = fraction of permanet snow/ice
REAL, DIMENSION(1)                       :: ZP_ZENITH	! solar zenith angle
REAL, DIMENSION(1)                       :: ZP_ANGL_ILLUM
!					Effective illumination angle, Angle between the sun and the 
! 					normal to the ground (=zenith if no slope) used in TARTES
REAL, DIMENSION(1)                       :: LATin
REAL, DIMENSION(1)                       :: LONin
REAL, DIMENSION(1,4)	:: ZP_BLOWSNWin 
!					Properties of deposited blowing snow (from Sytron or Meso-NH/Crocus)
! 					1 : Deposition flux (kg/m2/s)
! 					2 : Density of deposited snow (kg/m3)
! 					3 : SGRA1 of deposited snow
! 					4 : SGRA2 of deposited snow

! CHARACTER(4)           :: SNOWDRIFT_optin        ! Snowdrift scheme :
                                      ! Mechanical transformation of snow grain and compaction + effect of wind 
                                      ! on falling snow properties
                                      !    'NONE': No snowdrift scheme
                                      !    'DFLT': falling snow falls as purely dendritic
                                      !    'GA01': Gallee et al 2001
                                      !    'VI13': Vionnet et al 2013
!LOGICAL		:: SNOWDRIFT_SUBLIM_BOOLin !	activate sublimation during drift
!LOGICAL		:: SNOW_ABS_ZENITH_BOOLin 
! 					activate parametrization of solar absorption for polar regions
! CHARACTER(3)		:: SNOWMETAMO_optin
! CHARACTER(3)		:: SNOWRADin
!LOGICAL                  	:: ATMORAD_BOOLin !		activate atmotartes scheme


! 
! see # 1599: You should prefer CSNOWRAD='B92' at the moment for the first tests. 
! In that case, the direct-diffuse partition is not used, a 3 band spectral fixed repartition
!  is used from the global radiation as detailed in Vionnet et al 2012.
! 'T17' option comes with a number of other problems (impurities management, memory issues, etc.).
REAL, DIMENSION(1,186)   :: ZP_DIR_SWin  ! dimension (spectral band: 186 bands? )
REAL, DIMENSION(1,186)   :: ZP_SCA_SWin ! dimension (spectral band: 186 bands? )
!								direct and diffuse spectral irradiance (W/m2/um)
REAL, DIMENSION(1,186)   :: ZP_SPEC_ALBout ! dimension (spectral band: 186 bands? )
REAL, DIMENSION(1,186)   :: ZP_DIFF_RATIOout ! dimension (spectral band: 186 bands? )
!								spectral albedo and diffuse to total irradiance ratio


REAL, DIMENSION(1,nimpur )  :: IMPWETin  ! dimension 
REAL, DIMENSION(1,nimpur )  :: IMPDRYin  ! dimension 
!								Dry and wet deposit coefficient from Forcing File(g/m²/s)
! 
REAL, DIMENSION(1)                         :: SNOWMAK_dzin!Snowmaking thickness (m)
REAL, DIMENSION(1)                         :: ZP_TIME     ! current time 
!REAL, DIMENSION(:)          	:: ZPZENITH		! Solar zenithal angle
REAL                        :: ZP_AZIMSOL ! Solar azimuthal angle
REAL                        :: ZP_TSUN    ! Solar time
!REAL, DIMENSION(1,1)		:: SLOPE_DIRin	! typical slope aspect in the grid 
REAL, DIMENSION(1)                     :: ZP_CDN!		 neutral drag coefficient for momentum (not used)
REAL, DIMENSION(1)                     :: ZP_AC! 		aerodynamical resistance(not used)
REAL, DIMENSION(1)                     :: ZP_RA! 		aerodynamical resistance(not used)


XP00 = 1013.25E+02
XBOLTZ      = 1.380658E-23
XAVOGADRO   = 6.0221367E+23
XMD    = 28.9644E-3
XMV    = 18.0153E-3
XG      = 9.80665
XPI         = 2.*ASIN(1.)
RI_n = 0.2 ! it has not been initialized  in the CALL_MODEL 
EMISNOW = 0.99 ! see ini_surf_csts.F90
USTARSNOW = 0 ! it has not been initialized  in the CALL_MODEL 
CHSNOW = 0 ! it has not been initialized  in the CALL_MODEL
SNOWHMASS = 0 ! it has not been initialized  in the CALL_MODEL


! ***************************************************************************
! Set up local variables for dimension match
! ***************************************************************************
SNOWSWEinout(1,:)		= SNOWSWE(:)
SNOWRHOinout(1,:)		= SNOWRHO(:) 
SNOWHEATinout(1,:)		= SNOWHEAT(:)
SNOWALBinout		       = SNOWALB
SNOWGRAN1inout(1,:)	= SNOWGRAN1(:)
SNOWGRAN2inout(1,:)	= SNOWGRAN2(:)
SNOWHISTinout(1,:)		= SNOWHIST(:)
SNOWAGEinout (1,:)		= SNOWAGE(:)
ZP_SNOWIMPURinout (1,nsnow,nimpur) = 0 ! ZP_SNOWIMPUR(:,:)  !  B92--> snowcro.F90 uses 
! 										age to compute the effect of impurity  Vionnet et al 2012 
PPSin 					= PPS
SRSNOWin 			= SRSNOW
RRSNOWin			= RRSNOW
!ZP_PSN3Lin(1,1) 				= ZP_PSN3L
TAin					= TA
TGin 					= TG
SW_RADin 			= SW_RAD
QAin 					= QA
!ZP_VMODin(1,1) 				= ZP_VMOD
Wind_Ein				= Wind_E
Wind_Nin				= Wind_N
LW_RADin 			= LW_RAD
!ZP_RHOAin(1,1) 				= ZP_RHOA
UREFin 				= UREF 
!ZP_EXNSin(1,1) 				= ZP_EXNS
!ZP_EXNAin(1,1) 				= ZP_EXNA
!ZP_DIRCOSZWin(1,1)  		= ZP_DIRCOSZW
SLOPEin 				= SLOPE
ZREFin 				= ZREF
Z0NATin 				= Z0NAT
Z0EFFin 				= Z0EFF  
Z0HNATin 			= Z0HNAT                 					 
ALBin 				       = ALB
SOILCONDin 			= SOILCOND
D_Gin 				= D_G
SNOWLIQout(1,:) 		       = SNOWLIQ
SNOWDZout(1,:) 			= SNOWDZ
SNOWTEMPinout (1,:) 	= SNOWTEMP
THRUFALout 			= THRUFAL
ZP_EVAPCORout 			= ZP_EVAPCOR
ZP_GFLXCORout 			= ZP_GFLXCOR
GRNDFLUXinout 		= GRNDFLUX
ZP_SWNETSNOWout	 	= ZP_SWNETSNOW
ZP_SWNETSNOWSout 		= ZP_SWNETSNOWS
ZP_LWNETSNOWout	 	= ZP_LWNETSNOW
ZP_RNSNOWout   			= ZP_RNSNOW
ZP_HSNOWout 			= ZP_HSNOW
ZP_GFLUXSNOWout 		= ZP_GFLUXSNOW
ZP_HPSNOWout 			= ZP_HPSNOW
ZP_LES3Lout 				= ZP_LES3L
ZP_LEL3Lout 				= ZP_LEL3L
ZP_EVAPout  				= ZP_EVAP
SNDRIFTout 			= SNDRIFT
RI_nout  				       = RI_n
EMISNOWout 			= EMISNOW
!CDSNOWout (1,1) 		= CDSNOW ! will be computed 
USTARSNOWout 		= USTARSNOW
!CHSNOWinout(1,1) 		= CHSNOW ! will be computed 
SNOWHMASSout   		= SNOWHMASS
QSout 				       = QS
PERMSNOWFRACin  	= PERMSNOWFRAC
!ZP_ZENITHin (1,1) 			= ZP_ZENITH
!ZP_ANGL_ILLUMin (1,1) 		= ZP_ANGL_ILLUM
LATin 				       = LAT
LONin 				= LON

! T17 --> 186 band , B92 --> the direct-diffuse partition is not used, a 3 band spectral fixed repartition
!  is used from the global radiation as detailed in Vionnet et al 2012. 
ZP_DIR_SWin(1,:) = 0 ! ZP_DIR_SW(:) 
ZP_SCA_SWin(1,:) = 0 ! ZP_SCA_SW(:) 
!ZP_SPEC_ALBout (1,:,1) = ZP_SPEC_ALB(:) ! (spectral band: T17 --> 186 bands )
!ZP_DIFF_RATIOout(1,:,1) = ZP_DIFF_RATIO(:) ! dimension (spectral band: T17--> 186 bands )
!										 spectral albedo and diffuse to total irradiance ratio

IMPWETin(1,:) = IMPWET(:)  ! dimension  (1,nimpur,1) 
IMPDRYin(1,:) = IMPDRY(:)  ! dimension   (1,nimpur,1)
!										Dry and wet deposit coefficient from Forcing File(g/m²/s)

SNOWMAK_dzin	= SNOWMAK_dz
!SLOPE_DIRin (1,1)	= SLOPE_DIR

ZP_BLOWSNWin (1,1:4) = 0 ! No snow drift; set the value to zero
! ***************************************************************************
! Compute variables 
! ***************************************************************************

! Compute wind speed
! --------------------------------------------
ZP_VMODin = sqrt( Wind_Ein * Wind_Ein + Wind_Nin * Wind_Nin )

! Compute cosine of Slope 
! ---------------------------------------------------
ZP_DIRCOSZWin = COS(SLOPEin)

! Compute the snow fraction 
! -------------------------------------------------------
ZP_PSN3Lin= SRSNOWin /(SRSNOWin +RRSNOWin)

! Compute air density
! -----------------------------------------
XRD    = XAVOGADRO * XBOLTZ / XMD
XRV    = XAVOGADRO * XBOLTZ / XMV
! XRHOA (:) = ZPS(:,1) / (XRD * ZTA(:,1) * ( 1.+((XRV/XRD)-1.)*ZQA(:,1) ) + XG * XZREF )
ZP_RHOAin = PPSin / (XRD * TAin * ( 1.+((XRV/XRD)-1.)*QAin ) + XG * ZREFin )

! Compute the Exner functions
! -------------------------------------------------------------
ZP_EXNSin = (PPSin/XP00)**(XRD/XCPD) ! Exner function at surface 

! For ZP_EXNAin we need pressure at lowest atmos. level
! Assistance #1599 --> XPA = XPS - XRHOA * XZREF * XG
ZP_PAin = PPSin - ZP_RHOAin * ZREFin * XG 
ZP_EXNAin =  (ZP_PAin/XP00)**(XRD/XCPD) ! Exner function at lowest atmos. level 

! Compute ZP_ZENITH, ZP_AZIMSOL
! ------------------------------------------------------------------------------
ZP_TIME = LIS_rc%hr*3600 + LIS_rc%mn*60 + LIS_rc%ss ! current time 
 CALL SUNPOS (YEAR, MONTH, DAY, ZP_TIME, &
                         LONin, LATin, ZP_TSUN, ZP_ZENITH, ZP_AZIMSOL)

! Compute the effective illumination angle 
! ----------------------------------------------------------------------------------    
! JJ --> (1,1)
! ZP_ZENITH & ZP_AZIMSOL from SUBROUTINE SUNPOS
! SLOPE_DIR  ! direction of S.S.O. (deg from N clockwise) 

ZP_ANGL_ILLUM = ACOS((COS(ZP_ZENITH)*COS(ACOS(ZP_DIRCOSZWin)))+ &
    (SIN(ZP_ZENITH)*SIN(ACOS(ZP_DIRCOSZWin)*COS(ZP_AZIMSOL-(SLOPE_DIR*XPI/180))))) 


! Compute  (Recharson number)
! ---------------------------------------------------------------
! RI (Recharson number) >0.2-0.25 (a threshold beyond which the turbulence is suppressed)
! Crocus : Ri,crit, is set to 0.2. user manual Page = 170
! I the CALL_MODEL the Ri has not been initialized and uninitalized variable passed into the snowcro.F90
! If Ri needed to be computed we can use the following subroutine 
! CALL SURFACE_RI(PTS, PQSAT, PEXNS, PEXNA, PTA, PQA,  &
!                 PZREF, PUREF, PDIRCOSZW, PVMOD, ZRI  ) 


! Compute drag coefficient for momentum  
! -----------------------------------------------------------------------------------
! see modi_surface_cd.F90
! NOTE: we need to compute RI_ninout(1,1), ZREFin(1,1), UREFin(1,1), Z0EFFin(1,1), Z0HNATin(1,1)
! PUREF (UREFin)    ! reference height of the wind
! PZ0EFF (Z0EFFin)   ! roughness length for momentum with subgrid-scale orography
! PZ0H (Z0HNATin)  ! roughness length for heat
! NOTE:  MODI_SURFACE_CD_SUB.F90 need 1D variable. 
 CALL  MODI_SURFACE_CD_SUB(RI_nout, ZREFin, UREFin, Z0EFFin, Z0HNATin,   &
                              CDSNOWout , ZP_CDN)  


! Compute drag coefficient for heat 
! ---------------------------------------------------------------------
! see modi_surface_aero_cond.F90
! PZREF (ZREFin)   ! reference height of the first atmospheric level
! ZP_AC  (out)    ! aerodynamical conductance
! ZP_RA  (out)    ! aerodynamical resistance

 CALL MODI_SURFACE_AERO_COND_SUB(RI_nout, ZREFin, UREFin, ZP_VMODin, Z0NATin ,&
                                     Z0HNATin, ZP_AC, ZP_RA, CHSNOWout ,SNOWRES_opt  )

TPTIME%TDATE%YEAR = year
TPTIME%TDATE%MONTH = month
TPTIME%TDATE%DAY = day
TPTIME%TIME = hour*3600 + minute*60 

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!1- add alocation for variable that has more than 1D
!2- do we need to have these local variables for logical and character variables? 

! call model physics here 
      CALL SNOWCRO(SNOWRES_opt, &
		TPTIME, 		&
		OMEB_BOOL,			&
		GLACIER_BOOL, 	&
		HIMPLICIT_WIND_opt,	&
		ZP_PEW_A_COEF, 	&
		ZP_PEW_B_COEF, 	&
		ZP_PET_A_COEF, 	&
		ZP_PEQ_A_COEF, 	&
		ZP_PET_B_COEF, 	&
		ZP_PEQ_B_COEF,		&
		SNOWSWEinout, 	&
		SNOWRHOinout, 	&
		SNOWHEATinout, &
		SNOWALBinout, 	&
		SNOWGRAN1inout,&
		SNOWGRAN2inout,&
		SNOWHISTinout, 	&
		SNOWAGEinout,	&
		ZP_SNOWIMPURinout,&
		PTSTEP, 				&
		PPSin,          		&
		SRSNOWin,		&
		RRSNOWin,		&
		ZP_PSN3Lin,			&
		TAin, TGin,	&
		SW_RADin,		&
		QAin,			&
		ZP_VMODin,			&
		LW_RADin, 		&
		ZP_RHOAin, 			&
		UREFin,			&
		ZP_EXNSin,			&
		ZP_EXNAin, 			&
		ZP_DIRCOSZWin,		&
		ZREFin, 			&
		Z0NATin, 		&
		Z0EFFin, 			&
		Z0HNATin, 		&
		ALBin, 			&
		SOILCONDin, 	&
		D_Gin, 			&
		SNOWLIQout, 	&
		SNOWTEMPinout,	&
		SNOWDZout, 		&
		THRUFALout, 	&
		GRNDFLUXinout,	&
		ZP_EVAPCORout,		&
		ZP_GFLXCORout, 	&
		ZP_SWNETSNOWout, 	&
		ZP_SWNETSNOWSout,	&
		ZP_LWNETSNOWout,	&
		ZP_RNSNOWout,		&
		ZP_HSNOWout, 		& 
		ZP_GFLUXSNOWout, 	&
		ZP_HPSNOWout,	&
		ZP_LES3Lout, 		&
		ZP_LEL3Lout,		&
		ZP_EVAPout, 		&
		SNDRIFTout,		&
		RI_nout,			&
		EMISNOWout,	&
 		CDSNOWout,		&
		USTARSNOWout,	&
		CHSNOWout,		&
 		SNOWHMASSout, 	&
		QSout,				&
		PERMSNOWFRACin,	&
		ZP_ZENITH,        		&
		ZP_ANGL_ILLUM,		&
		LATin, 				&
		LONin, 				&
		ZP_BLOWSNWin, 		&
		SNOWDRIFT_opt, 		&
		SNOWDRIFT_SUBLIM_BOOL,&
		SNOW_ABS_ZENITH_BOOL,	&
		SNOWMETAMO_opt,		&
		SNOWRAD_opt,			&
		ATMORAD_BOOL,			&
		ZP_DIR_SWin,			&
		ZP_SCA_SWin,			&
		ZP_SPEC_ALBout, 		&
		ZP_DIFF_RATIOout,		&
		IMPWETin,			&
		IMPDRYin,			&
		SNOWFALL_opt,			&
		SNOWCOND_opt, 		&
		SNOWHOLD_opt, 		&
		SNOWCOMP_opt,			&
		SNOWZREF_opt,			&
		SNOWMAK_dzin,			&
 		SNOWCOMPACT_BOOL, &
		SNOWMAK_BOOL,	&
		SNOWTILLER_BOOL,		&
		SELF_PROD_BOOL,			&
		SNOWMAK_PROP_BOOL,	&
		PRODSNOWMAK_BOOL)
!
  ZP_GFLXCORout= 0.0  ! see snow3L_isba.F90

! --------------------------------------------------------------------------------------------------------------
SNOWHEAT(:) = SNOWHEATinout(1,:)
SNOWRHO(:)  = SNOWRHOinout(1,:) 
SNOWSWE(:)  = SNOWSWEinout(1,:) 
SNOWALB  = SNOWALBinout(1)
SNOWGRAN1(:)= SNOWGRAN1inout(1,:)
SNOWGRAN2(:)= SNOWGRAN2inout(1,:)
SNOWHIST(:) = SNOWHISTinout(1,:) 
SNOWAGE(:)  = SNOWAGEinout(1,:)
!ZP_SNOWIMPUR	= ZP_SNOWIMPURinout(1,nsnow,nimpur)   
!PPS 				= PPSin(1,1)
!SRSNOW 		= SRSNOWin(1,1) 
!RRSNOW 		= RRSNOWin(1,1)

!ZP_PSN3L 			= ZP_PSN3Lin(1,1)
!TA 				= TAin(1,1)!
!TG 				= TGin(1,1)
!SW_RAD 		= SW_RADin(1,1)
!QA 				= QAin(1,1)
!ZP_VMOD 			= ZP_VMODin(1,1)
!LW_RAD		= LW_RADin(1,1) 
!ZP_RHOA 			= ZP_RHOAin(1,1)
!UREF 			= UREFin(1,1) 
!ZP_EXNS 			= ZP_EXNSin(1,1)
!ZP_EXNA 			= ZP_EXNAin(1,1)
!ZP_DIRCOSZW  		= ZP_DIRCOSZWin(1,1)
!ZREF 			= ZREFin(1,1)
!Z0NAT 			= Z0NATin(1,1)
!Z0EFF 			= Z0EFFin(1,1)  
!Z0HNAT 		= Z0HNATin(1,1)                 					 
!ALB 			= ALBin(1,1)
!SOILCOND 		= SOILCONDin(1,1)
!D_G 			= D_Gin(1,1)
SNOWLIQ(:) = SNOWLIQout(1,:)
SNOWDZ (:) = SNOWDZout(1,:)
SNOWTEMP(:) = SNOWTEMPinout(1,:) 
THRUFAL    = THRUFALout(1)
ZP_EVAPCOR    = ZP_EVAPCORout(1)
ZP_GFLXCOR    = ZP_GFLXCORout(1)
GRNDFLUX   = GRNDFLUXinout(1)
ZP_SWNETSNOW  = ZP_SWNETSNOWout(1)
ZP_SWNETSNOWS = ZP_SWNETSNOWSout(1)
ZP_LWNETSNOW  = ZP_LWNETSNOWout(1)
ZP_RNSNOW     = ZP_RNSNOWout  (1)
ZP_HSNOW      = ZP_HSNOWout(1)
ZP_GFLUXSNOW  = ZP_GFLUXSNOWout(1)
ZP_HPSNOW     = ZP_HPSNOWout(1)
ZP_LES3L      = ZP_LES3Lout(1)
ZP_LEL3L      = ZP_LEL3Lout(1)
ZP_EVAP       = ZP_EVAPout(1) 
SNDRIFT    = SNDRIFTout(1)
RI_n         = RI_nout(1)
EMISNOW    = EMISNOWout(1)
CDSNOW     = CDSNOWout(1)
USTARSNOW  = USTARSNOWout(1) 
CHSNOW     = CHSNOWout (1)
SNOWHMASS  = SNOWHMASSout(1)
QS         = QSout(1)
!PERMSNOWFRAC= PERMSNOWFRACin(1,1)
!ZP_ZENITH			= ZP_ZENITHin(1,1)
!ZP_ANGL_ILLUM	= ZP_ANGL_ILLUMin(1,1)
!LAT				= LATin(1,1)
!LON				= LONin(1,1)
!ZP_BLOWSNW (1,nsnow,1)	= ZP_BLOWSNWin
! 
!ZP_DIR_SW			= ZP_DIR_SWin(1,186,1) 
!ZP_SCA_SW			= ZP_SCA_SWin(1,186,1) 
!ZP_SPEC_ALB 		= ZP_SPEC_ALBout (1,:,1) 
!ZP_DIFF_RATIO 		= ZP_DIFF_RATIOout(1,:,1) 
!					spectral albedo and diffuse to total irradiance ratio
!IMPWET 		= IMPWETin(1,nimpur) 
!IMPDRY 		= IMPDRYin(1,nimpur)  ! Dry and wet deposit coefficient from Forcing File(g/m²/s)
!SNOWMAK_dz 		= SNOWMAK_dzin(1,1)

END SUBROUTINE crocus_driver




