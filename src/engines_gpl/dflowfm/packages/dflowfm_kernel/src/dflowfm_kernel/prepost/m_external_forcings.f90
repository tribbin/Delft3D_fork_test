!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
! 
module m_external_forcings
implicit none

integer, parameter :: max_registered_item_id = 128
integer            :: max_ext_bnd_items      = 64  ! Starting size, will grow dynamically when needed.
character(len=max_registered_item_id), allocatable :: registered_items(:)
integer            :: num_registered_items = 0

interface
   module subroutine flow_setexternalforcingsonboundaries(tim,iresult)
   double precision, intent(in)    :: tim     !< (s)
   integer,          intent(out)   :: iresult !< Integer error status: DFM_NOERR==0 if succesful.
   end subroutine flow_setexternalforcingsonboundaries
end interface

interface
   module function init_external_forcings(external_force_file_name) result(res)
   character(len=*), intent(in) :: external_force_file_name  !< file name for new external forcing boundary blocks
   logical                      :: res
   end function init_external_forcings
end interface

interface
   module function flow_initexternalforcings() result(iresult)
   integer                      :: iresult
   end function flow_initexternalforcings
end interface

abstract interface
   subroutine fill_open_boundary_cells_with_inner_values_any(number_of_links, link2cell)
   integer, intent(in) :: number_of_links      !< number of links
   integer, intent(in) :: link2cell(:,:)       !< indices of cells connected by links
   end subroutine
end interface

public :: set_external_forcings
public :: calculate_wind_stresses

procedure(fill_open_boundary_cells_with_inner_values_any), pointer :: fill_open_boundary_cells_with_inner_values !< boundary update routine to be called

contains

!> set field oriented boundary conditions
subroutine set_external_forcings(time_in_seconds, initialization, iresult)
   use timers,                 only : timstrt, timstop
   use m_flowtimes
   use m_flowgeom
   use m_flow
   use m_meteo
   use m_calbedform
   use m_bedform
   use dfm_error
   use m_calibration,          only: calibration_backup_frcu
   use unstruc_channel_flow
   use time_class
   use m_longculverts
   use m_nearfield,            only : nearfield_mode, NEARFIELD_UPDATED, addNearfieldData
   use m_airdensity,           only : get_airdensity
   use dfm_error
   use m_lateral, only : numlatsg

   double precision, intent(in   ) :: time_in_seconds  !< Time in seconds
   logical,          intent(in   ) :: initialization   !< initialization phase
   integer,          intent(  out) :: iresult          !< Integer error status: DFM_NOERR==0 if succesful.

   integer, parameter              :: HUMIDITY_AIRTEMPERATURE_CLOUDINESS = 1
   integer, parameter              :: HUMIDITY_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION = 2
   integer, parameter              :: DEWPOINT_AIRTEMPERATURE_CLOUDINESS = 3
   integer, parameter              :: DEWPOINT_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION = 4
   integer, parameter              :: DEWPOINT = 5

   integer                         :: ierr             !< error flag
   logical                         :: l_set_frcu_mor = .false.
   logical                         :: first_time_wind

   logical, external               :: flow_initwaveforcings_runtime, flow_trachy_needs_update
   character(len=255)              :: tmpstr
   type(c_time)                    :: ecTime           !< Time in EC-module

   ! variables for processing the pump with levels, SOBEK style
   logical                         :: success_copy

   call timstrt('External forcings', handle_ext)

   success = .true.

   if (allocated(patm)) then
      ! To prevent any pressure jumps at the boundary, set (initial) patm in interior to PavBnd.
      ! May of course be overridden later by spatially varying patm values.
      patm = PavBnd
   end if

   call retrieve_icecover()
   
   if (ja_airdensity > 0) then
      call get_timespace_value_by_item_array_consider_success_value(item_airdensity, airdensity)
   end if
   if (ja_computed_airdensity==1) then 
      call get_timespace_value_by_item_array_consider_success_value(item_atmosphericpressure, patm)
      call get_timespace_value_by_item_array_consider_success_value(item_airtemperature, tair)
      call get_timespace_value_by_item_array_consider_success_value(item_humidity, rhum)
      call get_airdensity(patm, tair, rhum, airdensity, ierr)
   end if


   if (update_wind_stress_each_time_step == 0) then ! Update wind in set_external_forcing (each user timestep)
      call calculate_wind_stresses(time_in_seconds, iresult)
      if (iresult /= DFM_NOERR) then
         return
      end if
   end if

   if (jatem > 1) then
      call set_temperature_models()
   end if 

   if (ja_friction_coefficient_time_dependent > 0) then
       call set_friction_coefficient()
   end if

   call ecTime%set4(time_in_seconds, irefdate, tzone, ecSupportTimeUnitConversionFactor(tunit))

   call set_wave_parameters()

   call retrieve_rainfall()

   if (ncdamsg > 0) then
      call get_timespace_value_by_item_array_consider_success_value(item_damlevel, zcdam)
   end if

   if (ncgensg > 0) then
      call get_timespace_value_by_item_array_consider_success_value(item_generalstructure, zcgen)
      call update_zcgen_widths_and_heights() ! TODO: replace by Jan's LineStructure from channel_flow
   end if

   if (npumpsg > 0) then
      call get_timespace_value_by_item_array_consider_success_value(item_pump, qpump)
   end if

   call update_network_data()

   if (nlongculverts > 0) then
       call get_timespace_value_by_item_and_consider_success_value(item_longculvert_valve_relative_opening)
   end if

   if (nvalv > 0) then
       call get_timespace_value_by_item_and_consider_success_value(item_valve1D)
   end if

   if (jatidep > 0 .or. jaselfal > 0) then
      call flow_settidepotential(time_in_seconds/60d0)
   end if

   if (numlatsg > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_lateraldischarge, irefdate, tzone, tunit, time_in_seconds) ! 'lateral(_)discharge'
   end if

   !Pump with levels, outside OpenMP region
   if (nPumpsWithLevels > 0) then
      call update_pumps_with_levels()
   end if

   if (numsrc > 0) then
      ! qstss must be an argument when calling ec_gettimespacevalue.
      ! It might be reallocated after initialization (when coupled to Cosumo).
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_discharge_salinity_temperature_sorsin, irefdate, tzone, tunit, time_in_seconds, qstss)
   end if

   if (jasubsupl > 0) then
     call update_subsidence_and_uplift_data()
   end if

   if (nearfield_mode == NEARFIELD_UPDATED) then
      call addNearfieldData()
   end if

   call timstop(handle_ext)

   if (.not. success) then
      iresult = DFM_EXTFORCERROR
      call print_error_message(time_in_seconds)
      return
   end if

   if (jatem > 1 .and. jaheat_eachstep == 0) then
      call heatu(time_in_seconds/3600d0)
   end if

   if (bfm_included .and. .not. initialization) then
      if (bfmpar%lfbedfrm) then
          call fm_calbf()            ! JRE+BJ to check: see with which timestep we update this?
       end if
   end if

   if (bfmpar%lfbedfrmrou .and. .not. initialization)  then     ! .true. if van rijn 2004 or trachy contains ripple roughness
      call fm_calksc()
   end if

   if ((jacali == 1) .and. initialization) then
      ! Make backup of roughness factor after initialisation of frcu
      call calibration_backup_frcu()
   end if

   if (jatrt == 1) then
       if (flow_trachy_needs_update(time1)) then
           call flow_trachyupdate()                            ! perform a trachy update step
           l_set_frcu_mor = .true.
       end if
   end if

   if (jacali == 1) then
       ! update calibration definitions and factors on links
       call calibration_update()
       l_set_frcu_mor = .true.
   end if

   if (stm_included) then
       if ((jased>0) .and. l_set_frcu_mor) then
           call set_frcu_mor(1)     !otherwise frcu_mor is set in getprof_1d()
           call set_frcu_mor(2)
       end if
   end if

   ! Update nudging temperature (and salinity)
   if (item_nudge_tem /= ec_undef_int .and. janudge > 0 ) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_nudge_tem, irefdate, tzone, tunit, time_in_seconds)
   end if

   iresult = DFM_NOERR

contains


!> get_timespace_value_by_item_and_array_and_consider_success_value
subroutine get_timespace_value_by_item_array_consider_success_value(item, array)

    integer,          intent(in   ) :: item      !< Item for getting values
    double precision, intent(inout) :: array(:)  !< Array that stores the values

    success = success .and. ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds, array)

end subroutine get_timespace_value_by_item_array_consider_success_value

!> set_temperature_models
subroutine set_temperature_models()

    logical :: foundtempforcing

    ! Update arrays rhum, tair and clou in a single method call.
    ! Nothing happens in case quantity 'humidity_airtemperature_cloudiness' has never been added through ec_addtimespacerelation.
    select case (itempforcingtyp)
    case (HUMIDITY_AIRTEMPERATURE_CLOUDINESS)
        call get_timespace_value_by_name_and_consider_success_value('humidity_airtemperature_cloudiness')
    case (HUMIDITY_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION)
        call get_timespace_value_by_name_and_consider_success_value('humidity_airtemperature_cloudiness_solarradiation')
    case (DEWPOINT_AIRTEMPERATURE_CLOUDINESS)
        call get_timespace_value_by_name_and_consider_success_value('dewpoint_airtemperature_cloudiness')
    case (DEWPOINT_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION)
        call get_timespace_value_by_name_and_consider_success_value('dewpoint_airtemperature_cloudiness_solarradiation')
    case (DEWPOINT)
        call get_timespace_value_by_name_and_consider_success_value('dewpoint')
    end select

    foundtempforcing = (itempforcingtyp >= 1 .and. itempforcingtyp <= 4)

    if (btempforcingtypH) then
        call get_timespace_value_by_item_and_consider_success_value(item_humidity)
        foundtempforcing = .true.
    end if
    if (btempforcingtypA) then
        call get_timespace_value_by_item_and_consider_success_value(item_airtemperature)
        foundtempforcing = .true.
    end if
    if (btempforcingtypS) then
        call get_timespace_value_by_item_and_consider_success_value(item_solarradiation)
        foundtempforcing = .true.
    end if
    if (btempforcingtypC) then
        call get_timespace_value_by_item_and_consider_success_value(item_cloudiness)
        foundtempforcing = .true.
    end if
    if (btempforcingtypL) then
        call get_timespace_value_by_item_and_consider_success_value(item_longwaveradiation)
        foundtempforcing = .true.
    end if

    if (.not. foundtempforcing ) then
        call mess(LEVEL_WARN,&
    'No humidity, airtemperature, cloudiness and solar radiation forcing found, setting temperature model [physics:Temperature] = 1 (Only transport)')
        jatem = 1
    end if

end subroutine set_temperature_models

!> set friction coefficient values at this time moment
subroutine set_friction_coefficient()

   call get_timespace_value_by_item_and_array(item_frcu, frcu)

end subroutine set_friction_coefficient

!> get_timespace_value_by_name_and_consider_success_value
subroutine get_timespace_value_by_name_and_consider_success_value(name)

    character(*), intent(in) :: name

    success = success .and. ec_gettimespacevalue(ecInstancePtr, name, time_in_seconds)

end subroutine get_timespace_value_by_name_and_consider_success_value

!> get_timespace_value_by_item_and_consider_success_value
subroutine get_timespace_value_by_item_and_consider_success_value(item)

    integer, intent(in) :: item

    success = success .and. ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)

end subroutine get_timespace_value_by_item_and_consider_success_value

!> set_wave_parameters
subroutine set_wave_parameters()
   !
   logical :: all_wave_variables !< flag indicating whether _all_ wave variables should be mirrored at the boundary
   
   if (jawave == 3 .or. jawave == 6 .or. jawave == 7) then
       
       if (.not. initialization) then
           !
           if (     jawave == 7 .and. waveforcing == 1 ) then
               !
               call set_parameters_for_radiation_stress_driven_forces()
               !
           elseif ( jawave == 7 .and. waveforcing == 2 ) then
               !
               call set_parameters_for_dissipation_driven_forces()
               !
           elseif ( jawave == 7 .and. waveforcing == 3 ) then
               !
               call set_parameters_for_3d_dissipation_driven_forces()               
           else
               !
               call set_all_wave_parameters()
           end if
           !
       end if
       
       ! NB: choose whether to keep if(.not. initialization) hidden in initialize_wave_parameters or in set_wave_parameters
       
      if (.not. success) then
         !
         ! success = .false. : Most commonly, WAVE data has not been written to the com-file yet:
         ! - Print a warning
         ! - Continue with the calculation
         ! - Just try it the next timestep again
         ! - success must be set to .true., otherwise the calculation is aborted
         !
         message = dumpECMessageStack(LEVEL_WARN,callback_msg)
         success = .true.
      end if

      if(jawave == 7) then
          phiwav = convert_wave_direction_from_nautical_to_cartesian(phiwav)
      end if

      ! SWAN data used via module m_waves
      !    Data from FLOW 2 SWAN: s1 (water level), bl (bottom level), ucx (vel. x), ucy (vel. y), FlowElem_xcc, FlowElem_ycc, wx, wy
      !          NOTE: all variables defined @ cell circumcentre of unstructured grid
      !                different from Delft3D. There all variables are defined on the velocity points.
      !    Data from SWAN 2 FLOW:  wavefx, wavefy, hrms (or 0.5*sqrt(2)*hm0), rtp, tp/tps/rtp, phi (= wavedirmean), Uorb, wlen
      !          NOTE:
      !                not necessary are; tmean (Tm01), urms, wavedirpeak
      !
      ! For badly converged SWAN sums, dwcap and dsurf can be NaN. Put these to 0d0,
      ! as they cause saad errors as a result of NaNs in the turbulence model
      if (.not. flowwithoutwaves) then
          if(allocated(dsurf) .and. allocated(dwcap)) then
              if (any(isnan(dsurf)) .or. any(isnan(dwcap))) then
                  write (msgbuf, '(a)') 'Surface dissipation fields from SWAN contain NaN values, which have been converted to 0d0. &
                                       & Check the correctness of the wave results before running the coupling.'
                  call warn_flush() ! No error, just warning and continue
                  !
                  where (isnan(dsurf))
                      dsurf = 0d0
                  end where
                  !
                  where (isnan(dwcap))
                      dwcap = 0d0
                  end where
              end if
          end if

         all_wave_variables = .not.(jawave == 7 .and. waveforcing /= 3)
         call select_wave_variables_subgroup(all_wave_variables)
         
         ! In MPI case, partition ghost cells are filled properly already, open boundaries are not
         !
         ! velocity boundaries
         call fill_open_boundary_cells_with_inner_values(nbndu, kbndu)
         !
         ! waterlevel boundaries
         call fill_open_boundary_cells_with_inner_values(nbndz, kbndz)
         !
         !  normal-velocity boundaries
         call fill_open_boundary_cells_with_inner_values(nbndn, kbndn)
         !
         !  tangential-velocity boundaries
         call fill_open_boundary_cells_with_inner_values(nbndt, kbndt)
      end if
   
      if (jawave>0) then
         ! this call  is needed for bedform updates with van Rijn 2007 (cal_bf, cal_ksc below)
         ! These subroutines need uorb, rlabda
         call compute_wave_parameters()
      end if
   
   end if

end subroutine set_wave_parameters


subroutine get_values_and_consider_jawave6(item)

    integer, intent(in) :: item

    success_copy = success
    success = success .and. ecGetValues(ecInstancePtr, item, ecTime)
    if (jawave == 6) success = success_copy

end subroutine get_values_and_consider_jawave6


!> set wave parameters for jawave==3 (online wave coupling) and jawave==6 (SWAN data for D-WAQ)
subroutine set_all_wave_parameters()
    ! This part must be skipped during initialization
    if (jawave == 3) then
        ! Finally the delayed external forcings can be initialized
        success = flow_initwaveforcings_runtime()
    end if

    if ( allocated (hwavcom) ) then
        success = success .and. ecGetValues(ecInstancePtr, item_hrms, ecTime)
    end if
    if ( allocated (twav)    ) then
        success = success .and. ecGetValues(ecInstancePtr, item_tp, ecTime)
    end if
    if ( allocated (phiwav)  ) then
        call get_values_and_consider_jawave6(item_dir)
    end if
    if ( allocated (sxwav)   ) then
        call get_values_and_consider_jawave6(item_fx)
    end if
    if ( allocated (sywav)   ) then
        call get_values_and_consider_jawave6(item_fy)
    end if
    if ( allocated (sbxwav)  ) then
        call get_values_and_consider_jawave6(item_wsbu)
    end if
    if ( allocated (sbywav)  ) then
        call get_values_and_consider_jawave6(item_wsbv)
    end if
    if ( allocated (mxwav)   ) then
        call get_values_and_consider_jawave6(item_mx)
    end if
    if ( allocated (mywav)   ) then
        call get_values_and_consider_jawave6(item_my)
    end if
    if ( allocated (uorbwav) ) then
        call get_values_and_consider_jawave6(item_ubot)
    end if
    if ( allocated (dsurf)   ) then
        call get_values_and_consider_jawave6(item_dissurf)
    end if
    if ( allocated (dwcap)   ) then
        call get_values_and_consider_jawave6(item_diswcap)
    end if

end subroutine set_all_wave_parameters 

!> set wave parameters for jawave == 7 (offline wave coupling) and waveforcing == 1 (wave forces via radiation stress)
subroutine set_parameters_for_radiation_stress_driven_forces()

    success = success .and. ecGetValues(ecInstancePtr, item_hrms, ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_tp  , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_dir , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_fx  , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_fy  , ecTime)
    mxwav  (:) = 0d0
    mywav  (:) = 0d0
    uorbwav(:) = 0d0

    call mess(LEVEL_WARN, 'Incomplete functionality. Wave forces set to zero when Wavemodelnr = 7.')

end subroutine set_parameters_for_radiation_stress_driven_forces
!> set wave parameters for jawave == 7 (offline wave coupling) and waveforcing == 2 (wave forces via averaged dissipation) 
subroutine set_parameters_for_dissipation_driven_forces()

    success = success .and. ecGetValues(ecInstancePtr, item_hrms  , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_tp    , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_dir   , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_distot, ecTime)
    sxwav  (:) = 0d0
    sywav  (:) = 0d0
    mxwav  (:) = 0d0
    mywav  (:) = 0d0
    uorbwav(:) = 0d0

    call mess(LEVEL_WARN, 'Incomplete functionality. Wave forces set to zero when Wavemodelnr = 7.')

end subroutine set_parameters_for_dissipation_driven_forces

!> set wave parameters for jawave == 7 (offline wave coupling) and waveforcing == 3 (wave forces via 3D dissipation)
subroutine set_parameters_for_3d_dissipation_driven_forces()


success = success .and. ecGetValues(ecInstancePtr, item_hrms   , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_tp     , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_dir    , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_fx     , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_fy     , ecTime)
    success = success .and. ecGetValues(ecInstancePtr, item_dissurf, ecTime) 
    success = success .and. ecGetValues(ecInstancePtr, item_diswcap, ecTime)
    sbxwav (:) = 0d0
    sbywav (:) = 0d0
    mxwav  (:) = 0d0
    mywav  (:) = 0d0
    uorbwav(:) = 0d0

    call mess(LEVEL_WARN, 'Incomplete functionality. Wave forces set to zero when Wavemodelnr = 7.')

end subroutine set_parameters_for_3d_dissipation_driven_forces
    
  

!> convert wave direction [degrees] from nautical to cartesian meteorological convention
elemental function convert_wave_direction_from_nautical_to_cartesian(nautical_wave_direction) result(cartesian_wave_direction)

    double precision, intent(in) :: nautical_wave_direction  !< wave direction [degrees] in nautical  convention
    double precision             :: cartesian_wave_direction !< wave direction [degrees] in cartesian convention

    double precision, parameter  :: MAX_RANGE_IN_DEGREES            = 360d0
    double precision, parameter  :: CONVERSION_PARAMETER_IN_DEGREES = 270d0

    cartesian_wave_direction = modulo(CONVERSION_PARAMETER_IN_DEGREES - nautical_wave_direction, MAX_RANGE_IN_DEGREES)

end function convert_wave_direction_from_nautical_to_cartesian

!> retrieve icecover
subroutine retrieve_icecover()
   use m_fm_icecover, only: ja_icecover, ice_af, ice_h, ICECOVER_EXT

   if (ja_icecover == ICECOVER_EXT) then
      ice_af = 0.d0
      ice_h  = 0.d0
      if (item_sea_ice_area_fraction /= ec_undef_int) then
         call get_timespace_value_by_item_and_consider_success_value(item_sea_ice_area_fraction)
      endif
      if (item_sea_ice_thickness /= ec_undef_int) then
         call get_timespace_value_by_item_and_consider_success_value(item_sea_ice_thickness)
      endif
   endif

end subroutine retrieve_icecover

!> retrieve_rainfall
subroutine retrieve_rainfall()

   ! Retrieve rainfall for ext-file quantity 'rainfall'.
   if (jarain > 0) then
      if (item_rainfall /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'rainfall', time_in_seconds)
      end if
      if (item_rainfall_rate /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'rainfall_rate', time_in_seconds)
      end if
   end if

end subroutine retrieve_rainfall

!> update_network_data
subroutine update_network_data()

   logical :: success_previous

   success_previous = success

   if (network%sts%numPumps > 0) then
      call get_timespace_value_by_item(item_pump_capacity)
   end if

   if (network%sts%numCulverts > 0) then
      call get_timespace_value_by_item(item_culvert_valveOpeningHeight)
   end if

   if (network%sts%numWeirs > 0) then
      call get_timespace_value_by_item(item_weir_crestLevel)
   end if

   if (network%sts%numOrifices > 0) then
      call get_timespace_value_by_item(item_orifice_crestLevel)
      call get_timespace_value_by_item(item_orifice_gateLowerEdgeLevel)
   end if

   if (network%sts%numGeneralStructures > 0) then
      call get_timespace_value_by_item(item_general_structure_crestLevel)
      call get_timespace_value_by_item(item_general_structure_gateLowerEdgeLevel)
      call get_timespace_value_by_item(item_general_structure_crestWidth)
      call get_timespace_value_by_item(item_general_structure_gateOpeningWidth)
   end if

end subroutine update_network_data

!> update_subsidence_and_uplift_data
subroutine update_subsidence_and_uplift_data()

 if (.not. sdu_first) then
         ! preserve the previous 'bedrock_surface_elevation' for computing the subsidence/uplift rate
         subsupl_tp = subsupl
      end if
      if (item_subsiduplift /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'bedrock_surface_elevation', time_in_seconds)
      end if
      if (sdu_first) then
         ! preserve the first 'bedrock_surface_elevation' field as the initial field
         subsupl_tp = subsupl
         subsupl_t0 = subsupl
         sdu_first  = .false.
      end if

end subroutine update_subsidence_and_uplift_data

!> get_timespace_value_by_item_and_array
subroutine get_timespace_value_by_item_and_array(item, array)

    integer,          intent(in   ) :: item     !< Item for getting values
    double precision, intent(inout) :: array(:) !< Array that stores the values

    success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds, array)

end subroutine get_timespace_value_by_item_and_array

!> get_timespace_value_by_item
subroutine get_timespace_value_by_item(item)

    integer, intent(in) :: item !< Item for getting values

    success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)

end subroutine get_timespace_value_by_item


end subroutine set_external_forcings




!> print_error_message
subroutine print_error_message(time_in_seconds)
   use m_ec_message, only: dumpECMessageStack
   use MessageHandling, only: LEVEL_WARN, mess
   use unstruc_messages, only: callback_msg

   double precision, intent(in) :: time_in_seconds !< Current time when doing this action

   character(len=255)           :: tmpstr

   write(tmpstr,'(f22.11)') time_in_seconds
   call mess(LEVEL_WARN, 'Error while updating meteo/structure forcing at time=' // trim(tmpstr))
   tmpstr = dumpECMessageStack(LEVEL_WARN,callback_msg)
end subroutine print_error_message

!> prepare_wind_model_data
subroutine prepare_wind_model_data(time_in_seconds, iresult)
   use m_wind
   use m_flowparameters, only: jawave, flowWithoutWaves
   use m_flow, only: wind_speed_factor
   use m_meteo
   use m_flowgeom, only: ln, lnx, ndx
   use precision_basics
   use m_flowparameters, only: eps10
   use dfm_error

   double precision, intent(in   ) :: time_in_seconds !< Current time when setting wind data
   integer,          intent(  out) :: iresult         !< Error indicator

   double precision, parameter  :: SEA_LEVEL_PRESSURE = 101325d0
   integer                      :: ec_item_id, first, last, link, i, k
   logical                      :: first_time_wind

   wx = 0.d0
   wy = 0.d0
   wdsu_x = 0.d0
   wdsu_y = 0.d0
   wcharnock = 0.d0
   call initialize_array_with_zero(ec_pwxwy_x)
   call initialize_array_with_zero(ec_pwxwy_y)

   first_time_wind = (id_last_wind < 0)
   if (first_time_wind) then
      first = 1
      last  = get_ec_number_of_items()
   else
      first = id_first_wind
      last  = id_last_wind
   end if
   do i = first, last
      ec_item_id = get_ec_item_id(i)
      ! Retrieve wind's x- and y-component for ext-file quantity 'windxy'.
      if (ec_item_id == item_windxy_x .and. item_windxy_y /= ec_undef_int) then
         call get_timespace_value_by_item(item_windxy_x)
      ! Retrieve stress's x- and y-component for ext-file quantity 'stressxy'.
      elseif (ec_item_id == item_stressxy_x .and. item_stressxy_y /= ec_undef_int) then
         call get_timespace_value_by_item(item_stressxy_x)
      ! Retrieve wind's p-, x- and y-component for ext-file quantity 'airpressure_windx_windy'.
      else if (ec_item_id == item_apwxwy_p .and. item_apwxwy_x /= ec_undef_int .and. item_apwxwy_y /= ec_undef_int) then
         if (item_apwxwy_c /= ec_undef_int) then
            call get_timespace_value_by_name('airpressure_windx_windy_charnock')
         else
            call get_timespace_value_by_name('airpressure_windx_windy')
         end if
      ! Retrieve wind's charnock-component for ext-file quantity 'charnock'.
      else if (ec_item_id == item_charnock) then
         call get_timespace_value_by_item(item_charnock)
      ! Retrieve wind's x-component for ext-file quantity 'windx'.
      else if (ec_item_id == item_windx) then
         call get_timespace_value_by_item(item_windx)
      ! Retrieve wind's y-component for ext-file quantity 'windy'.
      else if (ec_item_id == item_windy) then
         call get_timespace_value_by_item(item_windy)
      ! Retrieve stress's x-component for ext-file quantity 'stressx'.
      else if (ec_item_id == item_stressx) then
         call get_timespace_value_by_item(item_stressx)
      ! Retrieve stress's y-component for ext-file quantity 'stressy'.
      else if (ec_item_id == item_stressy) then
         call get_timespace_value_by_item(item_stressy)
      ! Retrieve wind's p-component for ext-file quantity 'atmosphericpressure'.
      else if (ec_item_id == item_atmosphericpressure) then
         call get_timespace_value_by_item(item_atmosphericpressure)
      else
         cycle  ! avoid updating id_first_wind and id_last_wind
      end if
      if (.not. success) then
         iresult = DFM_EXTFORCERROR
         call print_error_message(time_in_seconds)
         return
      end if
      if (first_time_wind) then
         id_first_wind = min(i, id_first_wind)
         id_last_wind  = max(i, id_last_wind)
      end if
   end do

   if (jawindstressgiven > 0) then 
      if (item_stressx /= ec_undef_int .and. item_stressy /= ec_undef_int) then
         call get_timespace_value_by_item_and_array(item_stressx, wdsu_x)
         call get_timespace_value_by_item_and_array(item_stressy, wdsu_y)
      else if (item_stressxy_x /= ec_undef_int .and. item_stressxy_y /= ec_undef_int) then
         call get_timespace_value_by_item_and_array(item_stressxy_x, wdsu_x)
         call get_timespace_value_by_item_and_array(item_stressxy_y, wdsu_y)
      end if
   end if

   if (allocated(ec_pwxwy_x) .and. allocated( ec_pwxwy_y)) then
      if (jawindstressgiven == 1) then 
         call perform_additional_spatial_interpolation(wdsu_x, wdsu_y)
      else
         call perform_additional_spatial_interpolation(wx, wy)
      end if
      if (allocated(ec_pwxwy_c)) then
         do link  = 1, lnx
            wcharnock(link) = wcharnock(link) + 0.5d0*( ec_pwxwy_c(ln(1,link)) + ec_pwxwy_c(ln(2,link)) )
         end do
      end if
   end if
   if (allocated(ec_charnock)) then
      do link  = 1, lnx
         wcharnock(link) = wcharnock(link) + 0.5d0*( ec_charnock(ln(1,link)) + ec_charnock(ln(2,link)) )
      end do
   end if

   if (ja_wind_speed_factor > 0) then
      do link = 1, lnx
         if (wind_speed_factor(link) /= dmiss) then
            wx(link) = wx(link) * wind_speed_factor(link)
            wy(link) = wy(link) * wind_speed_factor(link)
         end if
      end do
   end if

   if (item_atmosphericpressure /= ec_undef_int) then
      do k = 1, ndx
         if (comparereal(patm(k), dmiss, eps10) == 0) then
            patm(k) = SEA_LEVEL_PRESSURE
         end if
      end do
   end if

   if (jawave == 1 .or. jawave == 2 .and. .not. flowWithoutWaves) then
      call tauwavefetch(time_in_seconds)
   end if

   iresult = DFM_NOERR
contains

!> get_ec_item_id
integer function get_ec_item_id(i)
   integer, intent(in) :: i !< Input index

   get_ec_item_id = ecInstancePtr%ecItemsPtr(i)%ptr%id

end function get_ec_item_id

!> ec_number_of_items
integer function get_ec_number_of_items()

   get_ec_number_of_items = ecInstancePtr%nItems

end function get_ec_number_of_items

!> get_timespace_value_by_name
subroutine get_timespace_value_by_name(name)

   character(*), intent(in) :: name !< Input name

   success = ec_gettimespacevalue(ecInstancePtr, name, time_in_seconds)

end subroutine get_timespace_value_by_name

!> get_timespace_value_by_item_and_array
subroutine get_timespace_value_by_item_and_array(item, array)
   use m_flowtimes, only: irefdate, tzone, tunit

   integer,          intent(in   ) :: item             !< Input item
   double precision, intent(inout) :: array(:)         !< Array that stores the obatained values

   success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds, array)

end subroutine get_timespace_value_by_item_and_array

!> perform_additional_spatial_interpolation, the size of array_x and array_y is lnx.
subroutine perform_additional_spatial_interpolation(array_x, array_y)

   double precision, intent(inout) :: array_x(:) !< Array of X-components for interpolation
   double precision, intent(inout) :: array_y(:) !< Array of Y-components for interpolation

   do link  = 1, lnx
       array_x(link) = array_x(link) + 0.5d0*( ec_pwxwy_x(ln(1,link)) + ec_pwxwy_x(ln(2,link)) )
       array_y(link) = array_y(link) + 0.5d0*( ec_pwxwy_y(ln(1,link)) + ec_pwxwy_y(ln(2,link)) )
   end do

end subroutine perform_additional_spatial_interpolation

!> get_timespace_value_by_item
subroutine get_timespace_value_by_item(item)
   use m_flowtimes, only: irefdate, tzone, tunit
   integer, intent(in) :: item !< Input item

   success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)

end subroutine get_timespace_value_by_item

!> initialize_array_with_zero
subroutine initialize_array_with_zero(array)

   double precision, allocatable, intent(inout) :: array(:) !< Array that will be initialized

   if (allocated(array)) then
        array(:) = 0.d0
   end if

end subroutine initialize_array_with_zero

end subroutine prepare_wind_model_data

!> Gets windstress (and air pressure) from input files, and sets the windstress
subroutine calculate_wind_stresses(time_in_seconds, iresult)
   use m_wind, only: jawind, japatm
   use dfm_error, only: DFM_NOERR

   double precision, intent(in   ) :: time_in_seconds !< Current time when getting and applying winds
   integer,          intent(  out) :: iresult         !< Error indicator
   if (jawind == 1 .or. japatm > 0) then
      call prepare_wind_model_data(time_in_seconds, iresult)
      if (iresult /= DFM_NOERR) then
         return
      end if
   end if

   if (jawind > 0) then
      call setwindstress()
   end if

   iresult = DFM_NOERR

end subroutine calculate_wind_stresses



!> select_wave_variables_subgroup
!! select routine depending on whether all or a subgroup of wave variables are allocated
subroutine select_wave_variables_subgroup(all_wave_variables)
    
    logical, intent(in) :: all_wave_variables
    
    if (all_wave_variables) then
        fill_open_boundary_cells_with_inner_values => fill_open_boundary_cells_with_inner_values_all
    else
        fill_open_boundary_cells_with_inner_values => fill_open_boundary_cells_with_inner_values_fewer
    end if
    
end subroutine select_wave_variables_subgroup

!> fill_open_boundary_cells_with_inner_values_all
subroutine fill_open_boundary_cells_with_inner_values_all(number_of_links, link2cell)
    use m_waves

    integer, intent(in) :: number_of_links      !< number of links
    integer, intent(in) :: link2cell(:,:)       !< indices of cells connected by links
    
    integer             :: link !< link counter
    integer             :: kb   !< cell index of boundary cell
    integer             :: ki   !< cell index of internal cell

    do link = 1, number_of_links
        kb   = link2cell(1,link)
        ki   = link2cell(2,link)
        hwavcom(kb) = hwavcom(ki)
        twav(kb)    = twav(ki)
        phiwav(kb)  = phiwav(ki)
        uorbwav(kb) = uorbwav(ki)
        sxwav(kb)   = sxwav(ki)
        sywav(kb)   = sywav(ki)
        mxwav(kb)   = mxwav(ki)
        mywav(kb)   = mywav(ki)
        sbxwav(kb)  = sbxwav(ki)
        sbywav(kb)  = sbywav(ki)
        dsurf(kb)   = dsurf(ki)
        dwcap(kb)   = dwcap(ki)
    end do

end subroutine fill_open_boundary_cells_with_inner_values_all

!> fill_open_boundary_cells_with_inner_values_fewer
subroutine fill_open_boundary_cells_with_inner_values_fewer(number_of_links, link2cell)
    use m_waves
    use m_flowparameters, only : jawave, waveforcing

    integer, intent(in) :: number_of_links      !< number of links
    integer, intent(in) :: link2cell(:,:)       !< indices of cells connected by links
    
    integer             :: link !< link counter
    integer             :: kb   !< cell index of boundary cell
    integer             :: ki   !< cell index of internal cell

    
    if (jawave == 7 .and. waveforcing == 2 ) then
        do link = 1,number_of_links
            kb   = link2cell(1,link)
            ki   = link2cell(2,link)
            distot(kb) = distot(ki)
        end do
    endif
    do link = 1, number_of_links
        kb   = link2cell(1,link)
        ki   = link2cell(2,link)
        hwavcom(kb) = hwavcom(ki)
        twav(kb)    = twav(ki)
        phiwav(kb)  = phiwav(ki)
        uorbwav(kb) = uorbwav(ki)
        sxwav(kb)   = sxwav(ki)
        sywav(kb)   = sywav(ki)
        mxwav(kb)   = mxwav(ki)
        mywav(kb)   = mywav(ki)
    end do
 
end subroutine fill_open_boundary_cells_with_inner_values_fewer

subroutine findexternalboundarypoints()             ! find external boundary points
 use m_netw
 use m_flow, filetype_hide => filetype               ! Two stages: 1 = collect elsets for which data is provided
 use m_flowgeom                                      !             2 = add relations between elsets and their providers
 use unstruc_model                                   ! This routine is based upon the network admin only,
 use timespace                                       ! not on the flow admin.
 use m_sferic
 use m_alloc
 use unstruc_messages
 use m_ship
 use properties
 use m_transport
 use m_sobekdfm
 use m_sediment
 use m_partitioninfo
 use system_utils, only: split_filename
 use unstruc_files, only: resolvePath

 implicit none

 character(len=256)    :: filename
 integer               :: filetype
 integer, allocatable  :: kce(:)             ! kc edges (numl)
 integer, allocatable  :: ke(:)              ! kc edges (numl)
 logical               :: jawel
 integer               :: ja_ext_force
 logical               :: ext_force_bnd_used
 integer               :: ierr, method
 double precision      :: return_time
 integer               :: numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf
 integer               :: nx
 integer               :: ierror
 integer               :: num_bc_ini_blocks
 character(len=64)     :: varname

 jatimespace = 1

 return_time = 0
 ja_ext_force = 0
 ext_force_bnd_used = .false.

 if (len(trim(md_extfile)) > 0) then
    inquire (file = trim(md_extfile), exist = jawel)
    if (jawel) then
       if (mext /= 0) then
          ! Close first, if left open after prior flow_geominit().
          ! NOTE: AvD: this if-check relies on the fact that mext is *not* set to 0 in default_fm_external_forcing_data(), when reinitializing an already initialized model.
          call doclose(mext)
       end if

       call oldfil(mext,md_extfile)
       call split_filename(md_extfile, md_extfile_dir, filename) ! Remember base dir for this ext file
       ja_ext_force = 1
    else
       call qnerror( 'External forcing file '''//trim(md_extfile)//''' not found.', '  ', ' ')
       write(msgbuf, '(a,a,a)') 'External forcing file ''', trim(md_extfile), ''' not found.'
       call err_flush()
    endif
 endif
 if (len(trim(md_extfile_new)) > 0) then
    inquire (file = trim(md_extfile_new), exist = jawel)
    if (jawel) then
       ext_force_bnd_used = .true.
    else
       call qnerror( 'Boundary external forcing file '''//trim(md_extfile_new)//''' not found.', '  ', ' ')
       write(msgbuf, '(a,a,a)') 'Boundary external forcing file ''', trim(md_extfile_new), ''' not found.'
       call err_flush()
    endif
 endif

! if (ja_ext_force == 0 .and. .not. ext_force_bnd_used) then
!    return
! endif

 if ( allocated (xe) ) deallocate(xe, ye, xyen)     ! centre points of all net links, also needed for opening closed boundaries

 !mx1Dend = 0                                        ! count MAX nr of 1D endpoints
 !do L = 1,numl1D
 !   if ( kn(3,L) == 1) then                         ! zeker weten
 !      k1 = kn(1,L) ; k2 = kn(2,L)
 !      if (nmk(k1) == 1 .and. nmk(k2) == 2 .and. lne(1,L) < 0 .or. &
 !          nmk(k2) == 1 .and. nmk(k1) == 2 .and. lne(2,L) < 0 ) then
 !          mx1Dend = mx1Dend + 1
 !      endif
 !   endif
 !enddo
 !
 !
 !nx = numl + mx1Dend

! count number of 2D links and 1D endpoints
 call count_links(mx1Dend, Nx)


 allocate ( xe (nx) ,     stat=ierr ) ; xe = 0      ! used in findexternalboundarypoints
 call aerr('xe (nx)',     ierr, nx)
 allocate ( ye (nx) ,     stat=ierr ) ; ye = 0
 call aerr('ye (nx)',     ierr, nx)
 allocate ( xyen(2, nx) , stat=ierr ) ; xyen = 0d0
 call aerr('xyen(2, nx)', ierr, nx)

                                                    ! some temp arrays

 if (allocated(kez)) then
    ! If flow_geominit was called separately from a flow_modelinit:
    deallocate (             kez,     keu,     kes,     ketm,     kesd,     keuxy,     ket,     ken,     ke1d2d,     keg,     ked,     kep,     kedb,     keklep,     kevalv,     kegs,     kegen,     itpez,     itpenz,     itpeu,      itpenu,     kew)
 end if
 if (allocated(ftpet) ) then
    deallocate(ftpet)
 end if
 allocate ( kce(nx), ke(nx), kez(nx), keu(nx), kes(nx), ketm(nx), kesd(nx), keuxy(nx), ket(nx), ken(nx), ke1d2d(nx), keg(nx), ked(nx), kep(nx), kedb(nx), keklep(nx), kevalv(nx), kegs(nx), kegen(nx), itpez(nx), itpenz(nx), itpeu(nx) , itpenu(nx), kew(nx), ftpet(nx), stat=ierr )
 call aerr('kce(nx), ke(nx), kez(nx), keu(nx), kes(nx), ketm(nx), kesd(nx), keuxy(nx), ket(nx), ken(nx), ke1d2d(nx), keg(nx), ked(nx), kep(nx), kedb(nx), keklep(nx), kevalv(nx), kegs(nx), kegen(nx), itpez(nx), itpenz(nx), itpeu(nx) , itpenu(nx), kew(nx), ftpet(nx)',ierr, 17*nx)
            kce = 0; ke = 0; kez = 0; keu = 0; kes = 0; ketm = 0; kesd = 0; keuxy = 0; ket = 0; ken = 0; ke1d2d = 0; keg = 0; ked = 0; kep=  0; kedb=0  ; keklep=0  ; kevalv=0  ; kegen= 0; itpez = 0; itpenz = 0; itpeu = 0 ; itpenu = 0 ; kew = 0; ftpet = 1d6

 if (allocated(ketr) ) deallocate(ketr)
 allocate ( ketr(nx,1), stat = ierr )
 call aerr('ketr(nx,1)', ierr, nx)
            ketr = 0

 if ( allocated(nbndtr) ) deallocate(nbndtr)
 allocate ( nbndtr(1), stat = ierr )
 call aerr('nbndtr(1)', ierr, 1 )
            nbndtr = 0

 if ( allocated(trnames) ) deallocate(trnames)
 allocate ( trnames(1), stat = ierr )
 call aerr('trnames(1)', ierr, 1 )
            trnames(1) = ''
 numtracers = 0

 if (allocated(kesf) ) deallocate(kesf)
 allocate ( kesf(1,nx), stat = ierr )   ! would have been nice to have stmpar%lsedsus,
 call aerr('kesf(1,nx)', ierr, nx)      ! but no can do, jammer de bammer...
 kesf = 0

 if ( allocated(nbndsf) ) deallocate(nbndsf)
 allocate ( nbndsf(1), stat = ierr )
 call aerr('nbndsf(1)', ierr, 1 )
 nbndsf = 0

 if ( allocated(sfnames) ) deallocate(sfnames)
 allocate ( sfnames(1), stat = ierr )
 call aerr('sfnames(1)', ierr, 1 )
 sfnames = ''
 numfracs = 0

 call make_mirrorcells(Nx, xe, ye, xyen, kce, ke, ierror)

 if ( jampi.eq.1 ) then
! disable mirror cells that are not mirror cells in the whole model by setting kce=0
    call partition_reduce_mirrorcells(Nx, kce, ke, ierror)
 end if

 nbndz = 0                                           ! startindex waterlevel bnds
 nbndu = 0                                           ! startindex velocity   bnds
 nbnds = 0                                           ! startindex salinity   bnds
 nbndtm = 0                                          ! startindex temperature bnds
 nbndt = 0                                           ! startindex tangential vel. bnds
 nbnduxy = 0                                         ! startindex uxuy vel. bnds
 nbndn = 0                                           ! startindex normal     vel. bnds
 nbnd1d2d = 0                                        ! startindex 1d2d bnds
 ngate = 0                                           ! startindex gate links
 ncdam = 0                                           ! startindex cdam links
 npump = 0                                           ! startindex pump links
 nbndw  = 0                                          ! startindex wave energy bnds

 nqbnd   = 0                                         ! nr of q sections   or specified q bnd's
 nqhbnd  = 0                                         ! nr of qh boundary sections or specified qh bnd's
 ngatesg = 0                                         ! nr of gate signals or specified gates ! not in loop below because flow links not ready yet
 ncdamsg = 0                                         ! nr of controllable dam signals
 npumpsg = 0                                         ! nr of pump signals
 nshiptxy = 0                                        ! nr of ship xyt signals
 nwbnd    = 0                                        ! nr of wave-energy boundaries


 num_bc_ini_blocks = 0
 if (ext_force_bnd_used) then
    ! first read the bc file (new file format for boundary conditions)
    call readlocationfilesfromboundaryblocks(trim(md_extfile_new), nx, kce, num_bc_ini_blocks, &
                                         numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf)
 endif

 do while (ja_ext_force .eq. 1)                      ! read *.ext file

    call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja_ext_force,varname)
    call resolvePath(filename, md_extfile_dir)

    if (num_bc_ini_blocks > 0 .and. qid(len_trim(qid)-2:len_trim(qid)) == 'bnd') then
       write(msgbuf, '(a)') 'Boundaries in BOTH external forcing and bound.ext.force file is not allowed'
       call msg_flush()
       call qnerror( 'Boundaries in two files: ', trim(md_extfile_new), ' and ' // trim(md_extfile) )
        ja_ext_force = 0
    endif

    if (ja_ext_force == 1) then

        jatimespace = 1                              ! module is to be used

        call processexternalboundarypoints(qid, filename, filetype, return_time,  nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, 1d0, transformcoef)

    endif

 enddo

 deallocate(kce)
 deallocate(ke)

 if (mext /= 0) then
    rewind (mext)                                      ! prepare input file
 end if
 numbnp = nbndz + nbndu + nbnd1d2d                             ! nr of boundary points =

end subroutine findexternalboundarypoints



subroutine readlocationfilesfromboundaryblocks(filename, nx, kce, num_bc_ini_blocks, &
                                                numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf)
 use properties
 use timespace
 use tree_data_types
 use tree_structures
 use messageHandling
 use m_flowgeom, only: rrtol
 use fm_external_forcings_data, only: transformcoef
 use system_utils
 use unstruc_files, only: resolvePath
 use m_alloc
 use string_module, only: strcmpi
 use unstruc_model, only: ExtfileNewMajorVersion, ExtfileNewMinorVersion
 use m_missing, only: dmiss

 implicit none

 character(len=*)      , intent(in)    :: filename
 integer               , intent(in)    :: nx
 integer, dimension(nx), intent(inout) :: kce
 integer               , intent(out)   :: num_bc_ini_blocks
 integer               , intent(inout) :: numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf

 type(tree_data), pointer     :: bnd_ptr             !< tree of extForceBnd-file's [boundary] blocks
 type(tree_data), pointer     :: node_ptr            !
 integer                      :: filetype            !< possible values POLY_TIM: use polygon file as location reference, or NODE_ID: use nodeId as a location reference
 integer                      :: istat               !
 integer, parameter           :: ini_key_len   = 32  !
 integer, parameter           :: ini_value_len = 256 !
 character(len=ini_key_len)   :: groupname           !
 character(len=ini_value_len) :: quantity            !
 character(len=ini_value_len) :: locationfile        !< contains either the name of the polygon file (.pli) or the nodeId
 character(len=ini_value_len) :: forcingfile         !
 double precision             :: return_time         !
 double precision             :: tr_ws               ! Tracer fall velocity
 double precision             :: tr_decay_time       ! Tracer decay time
 double precision             :: rrtolb              ! Local, optional boundary tolerance value.
 double precision             :: width1D             ! Local, optional custom 1D boundary width
 double precision             :: blDepth             ! Local, optional custom boundary bed level depth below initial water level

 integer                      :: i                   !
 integer                      :: num_items_in_file   !
 logical                      :: file_ok             !
 logical                      :: group_ok            !
 logical                      :: property_ok         !
 character(len=256)           :: basedir, fnam
 integer                      :: major, minor

 call tree_create(trim(filename), bnd_ptr)
 call prop_file('ini',trim(filename),bnd_ptr,istat)
 if (istat /= 0) then
     call qnerror( 'Boundary external forcing file ', trim(filename), ' could not be read' )
     return
 end if

 ! check FileVersion
 major = 1
 minor = 0
 call prop_get_version_number(bnd_ptr, major = major, minor = minor, success = file_ok)
 if ((major /= ExtfileNewMajorVersion .and. major /= 1) .or. minor > ExtfileNewMinorVersion) then
    write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of new external forcing file detected in '''//trim(filename)//''': v', major, minor, '. Current format: v',ExtfileNewMajorVersion,ExtfileNewMinorVersion,'. Ignoring this file.'
    call err_flush()
    return
 end if

 call split_filename(filename, basedir, fnam) ! Remember base dir of input file, to resolve all refenced files below w.r.t. that base dir.

 num_items_in_file = 0
 if (associated(bnd_ptr%child_nodes)) then
     num_items_in_file = size(bnd_ptr%child_nodes)
 endif

 file_ok = .true.
 do i=1,num_items_in_file
    node_ptr => bnd_ptr%child_nodes(i)%node_ptr
    groupname = tree_get_name(bnd_ptr%child_nodes(i)%node_ptr)
    if (strcmpi(groupname, 'Boundary')) then
       quantity = ''
       locationfile = ''
       forcingfile = ''
       return_time = 0.0

       group_ok = .true.

       ! todo: read multiple quantities
       call prop_get_string(node_ptr, '', 'quantity', quantity, property_ok)
       if (.not. property_ok) then
          call qnerror( 'Expected property' , 'quantity', ' for boundary definition' )
       end if

       group_ok = group_ok .and. property_ok

       call prop_get_string(node_ptr, '', 'nodeId', locationfile, property_ok)
       if (property_ok)  then
          filetype = node_id
       else
          call prop_get_string(node_ptr, '', 'locationFile', locationfile, property_ok)
          filetype = poly_tim
       endif

       if (property_ok)  then
          call resolvePath(locationfile, basedir)
       else
          call qnerror( 'Expected property' , 'locationFile', ' for boundary definition' )
       end if

       group_ok = group_ok .and. property_ok

       call prop_get_string(node_ptr, '', 'forcingFile ', forcingfile , property_ok)
       if (property_ok)  then
          call resolvePath(forcingfile, basedir)
       else
          call qnerror( 'Expected property' , 'forcingFile', ' for boundary definition' )
       end if

       group_ok = group_ok .and. property_ok

       call prop_get_double(node_ptr, '', 'returnTime', return_time )
       call prop_get_double(node_ptr, '', 'return_time', return_time ) ! UNST-2386: Backwards compatibility reading.

       tr_ws = 0d0
       call prop_get_double(node_ptr, '', 'tracerFallVelocity', tr_ws)
       transformcoef(4) = tr_ws

       tr_decay_time = 0d0
       call prop_get_double(node_ptr, '', 'tracerDecayTime', tr_decay_time)
       transformcoef(5) = tr_decay_time

       rrtolb = 0d0
       call prop_get_double(node_ptr, '', 'openBoundaryTolerance', rrtolb)

       width1D = dmiss
       call prop_get_double(node_ptr, '', 'bndWidth1D', width1D)

       blDepth = dmiss
       call prop_get_double(node_ptr, '', 'bndBlDepth', blDepth)

       if (group_ok) then
          if (rrtolb > 0d0) then
             call processexternalboundarypoints(quantity, locationfile, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, rrtolrel = (1+2*rrtolb)/(1+2*rrtol), tfc = transformcoef, width1D = width1D, blDepth = blDepth)
          else
             call processexternalboundarypoints(quantity, locationfile, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, rrtolrel = 1d0, tfc = transformcoef, width1D = width1D, blDepth = blDepth)
          end if
          num_bc_ini_blocks = num_bc_ini_blocks + 1
       endif

       file_ok = file_ok .and. group_ok

    else
       ! warning: unknown group
    endif

 end do

 call tree_destroy(bnd_ptr)

end subroutine readlocationfilesfromboundaryblocks

subroutine appendrettime(qidfm, nbnd, rettime)

 use fm_external_forcings_data
 use m_alloc

 implicit none

 character(len=256), intent(in)  :: qidfm ! constituent index
 integer, intent(in)             :: nbnd    ! boundary cell index
 double precision, intent(in)    :: rettime ! return time (h)
 integer                         :: thrtlen ! temp array length

 if (allocated(thrtt)) then
    thrtlen = size(thrtt) + 1
 else
    thrtlen = 1
 endif

 call realloc(thrtq, thrtlen, keepExisting=.true., fill='')
 thrtq(thrtlen) = qidfm

 call realloc(thrtn, thrtlen, keepExisting=.true., fill=0)
 thrtn(thrtlen) = nbnd

 call realloc(thrtt, thrtlen, keepExisting=.true., fill=0d0)
 thrtt(thrtlen) = rettime
end subroutine appendrettime


!> helper routine finding external boundary points, called for both old and new-type ext file.
!! Also used for some none-boundary quantities that also need counting total nr of elements, *prior* to flow_initexternalforcings.
!! Two stages: 1 = collect elsets for which data is provided         <-- findexternalboundarypoints + processexternalboundarypoints
!!             2 = add relations between elsets and their providers  <-- flow_initexternalforcings
!! This routine is based upon the network admin only, not on the flow admin.
subroutine processexternalboundarypoints(qid, filename, filetype, return_time, nx, kce, &
                                         numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, &
                                         numqh, numw, numtr, numsf, rrtolrel, tfc, &
                                         width1D, blDepth) ! helper for finding external boundary points
 use m_netw
 use m_flow, qid_flow => qid, filetype_flow => filetype
 use m_flowgeom
 use unstruc_model
 use timespace
 use m_sferic
 use m_alloc
 use unstruc_messages
 use m_ship
 use properties
 use m_transport
 use m_meteo, qid_meteo => qid, filetype_meteo => filetype
 use m_sobekdfm
 use m_flowparameters, only: jawave
 use string_module
 use m_strucs, only: NUMGENERALKEYWRD
 use m_missing, only: dmiss

 implicit none

 character(len=256)    , intent(in)    :: qid                                 !
 character(len=256)    , intent(in)    :: filename                            !
 integer               , intent(in)    :: filetype
 integer               , intent(in)    :: nx                                  !
 integer, dimension(nx), intent(inout) :: kce                                 !
 double precision      , intent(in)    :: return_time
 integer               , intent(inout) :: numz, numu, nums, numtm, numsd, &   !
                                          numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf      !
 double precision      , intent(in)    :: rrtolrel !< To enable a more strict rrtolerance value than the global rrtol. Measured w.r.t. global rrtol.

 double precision, dimension(NUMGENERALKEYWRD), optional, intent(in) :: tfc
 double precision, optional, intent(in) :: width1D !< Optional custom width for boundary flow link.
 double precision, optional, intent(in) :: blDepth !< Optional custom bed level depths below water level boundaries's initial value for boundary points.

 character(len=256)                    :: qidfm                               !
 integer                               :: itpbn
 character (len=NAMTRACLEN)            :: tracnam, sfnam, qidnam
 character(len=20)                     :: tracunit
 integer                               :: itrac, isf
 integer, external                     :: findname
 integer                               :: janew
 character(len=:),allocatable          :: pliname

! call bndname_to_fm(qid,qidfm)
  qidfm = qid
  if (qidfm == 'waterlevelbnd'    .or. qidfm == 'neumannbnd'  .or. qidfm == 'riemannbnd' .or. qidfm == 'outflowbnd' .or. qidfm == 'qhbnd') then

     if (allocated(pliname)) deallocate(pliname)
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kez(nbndz+1:nx), numz, usemask=.true., pliname=pliname) !numz=number cells found, plname=pliname
     write(msgbuf,'(a,x,a,i8,a)') trim (qid), trim( filename), numz, ' nr of open bndcells' ; call msg_flush()
     nzbnd = nzbnd + 1

     if (qidfm == 'waterlevelbnd')  itpbn = 1
     if (qidfm == 'neumannbnd'   )  itpbn = 2
     if (qidfm == 'riemannbnd'  )   then
        itpbn = 5
        if (present(tfc)) then
           ftpet(nbndz+1:nbndz+numz) = tfc(7)    ! relaxation time riemann from ext file
        end if
     end if
     if (qidfm == 'outflowbnd'   )  itpbn = 6

     if (qidfm == 'qhbnd') then
         itpbn = 7
         nqhbnd = nqhbnd + 1
         numqh  = numz
         if (filetype == poly_tim) then
         call realloc(qhpliname,nqhbnd)  ; qhpliname(nqhbnd) = pliname
         end if

         call realloc(L1qhbnd,nqhbnd) ; L1qhbnd(nqhbnd) = nbndz + 1
         call realloc(L2qhbnd,nqhbnd) ; L2qhbnd(nqhbnd) = nbndz + numz
         call realloc(atqh_all,nqhbnd); atqh_all(nqhbnd) = 0d0
         call realloc(atqh_sum,nqhbnd); atqh_sum(nqhbnd) = 0d0
         call realloc(qhbndz,nqhbnd)  ; qhbndz(nqhbnd)   = 0d0
         call realloc(qh_gamma, nqhbnd)
         qh_gamma = 0d0
         call realloc(qhbndz_min, nqhbnd)
         qhbndz_min = 0d0
         call realloc(qhbndz_plus, nqhbnd)
         qhbndz_plus = 0d0
         call realloc(q_org, nqhbnd)
         q_org = 0d0
     end if
     itpez(nbndz+1:nbndz+numz) =  itpbn

     call addopenbndsection(numz, kez(nbndz+1:nbndz+numz), filename, IBNDTP_ZETA)

     ! When present, set custom geometry for open boundaries (bed level for bndz and/or width1D for 1D bndz/u).
     ! Only for z:
     if (present(blDepth)) then
        call realloc(bndBlDepth, size(openbndtype), fill = dmiss)
        bndBlDepth(nopenbndsect) = blDepth
     end if
     ! For z and u:
     if (present(width1D)) then
        call realloc(bndWidth1D, size(openbndtype), fill = dmiss)
        bndWidth1D(nopenbndsect) = width1D
     end if

     itpenz(nbndz+1:nbndz+numz) = nopenbndsect
     nbndz = nbndz + numz

  else if (qidfm == 'velocitybnd' .or. qidfm == 'dischargebnd' .or. qidfm == 'qhubnd'.or. &
           qidfm == 'criticaloutflowbnd' .or. qidfm == 'weiroutflowbnd' .or. qidfm == 'absgenbnd') then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, keu(nbndu+1:nx), numu, usemask=.true., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim (qid), trim( filename), numu, ' nr of open bndcells' ; call msg_flush()
     nubnd = nubnd + 1

     if (qidfm == 'velocitybnd' ) then
        itpbn = 3
     else if (qidfm == 'dischargebnd') then
        itpbn = 4
        nqbnd = nqbnd + 1
        call realloc(L1qbnd,nqbnd) ; L1qbnd(nqbnd) = nbndu + 1
        call realloc(L2qbnd,nqbnd) ; L2qbnd(nqbnd) = nbndu + numu
        call realloc(at_all,nqbnd);  at_all(nqbnd) = 0d0
        call realloc(at_sum,nqbnd);  at_sum(nqbnd) = 0d0
        call realloc(wwssav_all,(/2,nqbnd/), keepExisting=.true., fill=0d0)
        call realloc(wwssav_sum,(/2,nqbnd/), keepExisting=.true., fill=0d0)
        call realloc(huqbnd,L2qbnd(nqbnd)); huqbnd(L1qbnd(nqbnd):L2qbnd(nqbnd)) = 0d0
     else if ( qidfm == 'absgenbnd') then
        if (.not. (jawave.eq.4)) then                 ! Safety to avoid allocation errors later on
           call qnerror( 'Absorbing-generating boundary defined without activating surfbeat model. Please use appropriate wave model, or change the boundary condition type.', '  ', ' ')
           write(msgbuf, '(a)') 'Absorbing-generating boundary defined without activating surfbeat model. Please use appropriate wave model, or change the boundary condition type.'
           call err_flush()
        end if
        itpbn = 5
     else if ( qidfm == 'qhubnd') then
        itpbn = 6
     else if ( qidfm == 'criticaloutflowbnd') then
        itpbn = 8
     else if ( qidfm == 'weiroutflowbnd') then
        itpbn = 9
     endif

     itpeu(nbndu+1:nbndu+numu) = itpbn

     call addopenbndsection(numu, keu(nbndu+1:nbndu+numu), filename, IBNDTP_U)

     ! When present, set custom geometry for open boundaries (width1D for 1D bndz/u).
     ! For z and u:
     if (present(width1D)) then
        call realloc(bndWidth1D, size(openbndtype), fill = dmiss)
        bndWidth1D(nopenbndsect) = width1D
     end if

     itpenu(nbndu+1:nbndu+numu) = nopenbndsect
     nbndu = nbndu + numu

  else if (qidfm == 'salinitybnd' .and. jasal>0 ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kes(nbnds+1:nx), nums, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename), nums, ' nr of salinity bndcells' ; call msg_flush()
     if (nums>0) then
        call appendrettime(qidfm, nbnds + 1, return_time)
        nbnds = nbnds + nums
     end if

  else if (qidfm == 'waveenergybnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kew(nbndw+1:nx), numw, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename), numw, ' nr of wave energy bndcells' ; call msg_flush()

     nwbnd = nwbnd + 1
     
     call realloc(L1wbnd,nwbnd) ; L1wbnd(nwbnd) = nbndw + 1
     call realloc(L2wbnd,nwbnd) ; L2wbnd(nwbnd) = nbndw + numw
     
     nbndw = nbndw + numw
     call realloc(fnamwbnd,nwbnd,fill='')
     fnamwbnd(nwbnd) = trim(filename)
     
  else if (qidfm == 'temperaturebnd' .and. jatem > 0 ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ketm(nbndtm+1:nx), numtm, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename), numtm, ' nr of temperature bndcells' ; call msg_flush()
     if (numtm>0) then
        call appendrettime(qidfm, nbndtm + 1, return_time)
        nbndtm = nbndtm + numtm
     end if

  else if (qidfm == 'sedimentbnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kesd(nbndsd+1:nx), numsd, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename), numsd, ' nr of sediment bndcells' ; call msg_flush()
     if (numsd>0) then
        call appendrettime(qidfm, nbndsd + 1, return_time)
        nbndsd = nbndsd + numsd
     end if

  else if (qidfm(1:9) == 'tracerbnd' ) then
     call get_tracername(qidfm, tracnam, qidnam)
     tracunit = " "
     call add_bndtracer(tracnam, tracunit, itrac, janew)

     if ( janew.eq.1 ) then
!       realloc ketr
        call realloc(ketr, (/ Nx, numtracers /), keepExisting=.true., fill=0 )
     end if
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ketr(nbndtr(itrac)+1:,itrac), numtr, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , numtr, ' nr of tracer bndcells' ; call msg_flush()
     if (numtr>0) then
        call appendrettime(qidfm, nbndtr(itrac) + 1, return_time)
        nbndtr(itrac) = nbndtr(itrac) + numtr
        nbndtr_all = maxval(nbndtr(1:numtracers))
     end if

  else if (qid(1:13) == 'initialtracer' ) then
     call get_tracername(qid, tracnam, qidnam)
     tracunit = " "
     call add_bndtracer(tracnam, tracunit, itrac, janew)

     if ( janew.eq.1 ) then
!       realloc ketr
        call realloc(ketr, (/ Nx, numtracers /), keepExisting=.true., fill=0 )
     end if

  else if (qidfm(1:10) == 'sedfracbnd' .and. stm_included) then
     call get_sedfracname(qidfm, sfnam, qidnam)
     isf = findname(numfracs, sfnames, sfnam)

     if ( isf.eq.0) then   ! add

        numfracs = numfracs+1
!       realloc
        call realloc(kesf, (/Nx, numfracs/), keepExisting=.true., fill=0 )
        call realloc(nbndsf, numfracs, keepExisting=.true., fill=0 )
        call realloc(sfnames, numfracs, keepExisting=.true., fill='')

        sfnames(numfracs) = trim(sfnam)
        isf = numfracs

     end if

     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kesf(nbndsf(isf)+1:,isf), numsf, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(3a,i8,a)') trim(qid), ' ', trim(filename) , numsf, ' nr of sedfrac bndcells' ; call msg_flush()
     if (numsf > 0) then
        call appendrettime(qidfm, nbndsf(isf) + 1, return_time)
        nbndsf(isf) = nbndsf(isf) + numsf
        nbndsf_all = maxval(nbndsf(1:numfracs))
     endif

  else if (qidfm == 'tangentialvelocitybnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ket(nbndt+1:nx), numt, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , numt, ' nr of tangentialvelocity bndcells' ; call msg_flush()

     nbndt = nbndt + numt

  else if (qidfm == 'uxuyadvectionvelocitybnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, keuxy(nbnduxy+1:nx), numuxy, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , numuxy, ' nr of uxuyadvectionvelocity bndcells' ; call msg_flush()

     nbnduxy = nbnduxy + numuxy

  else if (qidfm == 'normalvelocitybnd' ) then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ken(nbndn+1:nx), numn, usemask=.false., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , numn, ' nr of normalvelocity bndcells' ; call msg_flush()

     nbndn = nbndn + numn

  else if (qidfm == '1d2dbnd' ) then ! SOBEK1D-FM2D
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ke1d2d(nbnd1d2d+1:nx), num1d2d, usemask=.true., rrtolrel=rrtolrel)
     write(msgbuf,'(a,x,a,i8,a)') trim(qid), trim(filename) , num1d2d, ' nr of SOBEK1D-FM2D bndcells' ; call msg_flush()

     call addopenbndsection(num1d2d, ke1d2d(nbnd1d2d+1:nbnd1d2d+num1d2d), filename, IBNDTP_1D2D)
     nbnd1d2d = nbnd1d2d + num1d2d

  else if (qidfm == 'shiptxy' ) then

     nshiptxy = nshiptxy + 1

  else if (qidfm == 'nudgetime' .or. qidfm == 'nudgerate' .or. qidfm == 'nudge_salinity_temperature' ) then

     janudge = 1

 endif

 end subroutine processexternalboundarypoints


!> Calls the ec_addtimespacerelation with all proper unstruc-specific target arrays and element set masks.
function addtimespacerelation_boundaries(qid, filename, filetype, method, operand, forcingfile, targetindex) result(success)
   use fm_external_forcings_data, no1=>qid, no2=>filetype, no3=>operand, no4 => success
   use m_meteo, no5=>qid, no6=>filetype, no7=>operand, no8 => success
   use m_flowparameters, only: jawave
   use m_flowtimes, only: dt_nodal

   implicit none

   character(len=*),            intent(inout) :: qid         !< Identifier of current quantity (i.e., 'waterlevelbnd')
   character(len=*),            intent(in)    :: filename    !< Name of data file for current quantity.
   integer,                     intent(in)    :: filetype    !< File type of current quantity.
   integer,                     intent(in)    :: method      !< Time-interpolation method for current quantity.
   character(len=1),            intent(in)    :: operand     !< Operand w.r.t. previous data ('O'verride or '+'Append)
   character(len=*),  optional, intent(in)    :: forcingfile !< Optional forcings file, if it differs from the filename (i.e., if filename=*.pli, and forcingfile=*.bc)
   integer,           optional, intent(in)    :: targetIndex !< target position or rank of (complete!) vector in target array

   logical                       :: success
   character(len=256)            :: tracnam, sfnam, qidnam
   integer                       :: itrac, isf
   integer, external             :: findname
   double precision, dimension(:), pointer     :: pzmin, pzmax

   success = .true.   ! initialization

   ! Special forcingsfile: if name equals 'REALTIME', do not do an ec_addtimespacerelation, just leave it to the external caller to fill zbnd* target value array.
   ! TODO: AVD: we now leave it to caller to fill array with length(zbnd*),
   ! instead of the number of polyline points. Cleaner alternative is to create
   ! a poly_tim provider, with the *underlying* point child providers being REALTIME.
   if (present(forcingfile)) then
      if (trim(forcingfile) == 'REALTIME') then
         call mess(LEVEL_DEBUG, 'addtimespacerelation_boundaries: leave empty timespacerelation for '''//trim(qid)//''' from locationfile '''//trim(filename)//''' (REALTIME data).')
         return
      end if
   end if

   kx = 1
   if (nbndz > 0 .and. (qid == 'waterlevelbnd' .or. qid == 'neumannbnd' .or. qid == 'riemannbnd' .or. qid == 'outflowbnd')) then
      success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcingfile, dtnodal=dt_nodal, targetindex=targetindex)

   else if (nbndz > 0 .and. nqhbnd > 0 .and. (qid == 'qhbnd')) then
      success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbndu > 0 .and. (qid == 'dischargebnd' .or. qid == 'criticaloutflowbnd' .or. qid == 'weiroutflowbnd' .or. qid == 'absgenbnd' ) ) then
      if ( qid.eq.'absgenbnd' ) then
         jawave = 4
      end if
      success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand, xy2bndu, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbndu > 0 .and. qid == 'velocitybnd' ) then
      pzmin => zminmaxu(1:nbndu)
      pzmax => zminmaxu(nbndu+1:2*nbndu)
      success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand,   &
                                           xy2bndu, z=sigmabndu, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbnds > 0 .and. qid == 'salinitybnd' ) then ! 2D
      pzmin => zminmaxs(1:nbnds)
      pzmax => zminmaxs(nbnds+1:2*nbnds)
      success = ec_addtimespacerelation(qid, xbnds, ybnds, kds, kx, filename, filetype, method, operand, xy2bnds,    &
                                           z=sigmabnds, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbndTM > 0 .and. qid == 'temperaturebnd') then
      pzmin => zminmaxtm(1:nbndTM)
      pzmax => zminmaxtm(nbndTM+1:2*nbndTM)
      success = ec_addtimespacerelation(qid, xbndTM, ybndTM, kdtm, kx, filename, filetype, method, operand, xy2bndtm,   &
                                           z=sigmabndtm, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbndsd > 0 .and. (qid == 'sedimentbnd')) then
      pzmin => zminmaxsd(1:nbndsd)
      pzmax => zminmaxsd(nbndsd+1:2*nbndsd)
      success = ec_addtimespacerelation(qid, xbndsd, ybndsd, kdsd, kx, filename, filetype, method, operand, xy2bndsd,   &
                                           z=sigmabndsd, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile, targetindex=targetindex)

   else if ( numtracers > 0 .and. (qid(1:9) == 'tracerbnd') ) then
      ! get tracer boundary condition number
      call get_tracername(qid, tracnam, qidnam)
      itrac = findname(numtracers, trnames, tracnam)

! for parallel runs, we always need to add the tracer, even if this subdomain has no tracer boundary conditions defined
!      call add_tracer(tracnam, iconst)
!      update: all tracers are counted first and allocated later

      if ( nbndtr(itrac).gt.0 ) then
         pzmin => bndtr(itrac)%zminmax(1:nbndtr(itrac))
         pzmax => bndtr(itrac)%zminmax(nbndtr(itrac)+1:2*nbndtr(itrac))
         success = ec_addtimespacerelation(qid, bndtr(itrac)%x, bndtr(itrac)%y, bndtr(itrac)%kd, kx, filename, filetype, method, operand, bndtr(itrac)%xy2,    &
                                           z=bndtr(itrac)%sigma, forcingfile=forcingfile, pzmin=pzmin, pzmax=pzmax, targetindex=targetindex)
      else
         success = .true.
      end if

   else if ( numfracs > 0 .and. (qid(1:10) == 'sedfracbnd') .and. stm_included) then

      call get_sedfracname(qid, sfnam, qidnam)
      isf = findname(numfracs, sfnames, sfnam)

      if (isf > 0) then
         if ( nbndsf(isf).gt.0 ) then
            pzmin => bndsf(isf)%zminmax(1:nbndsf(isf))
            pzmax => bndsf(isf)%zminmax(nbndsf(isf)+1:2*nbndsf(isf))
            success = ec_addtimespacerelation(qid, bndsf(isf)%x, bndsf(isf)%y, bndsf(isf)%kd, kx, filename, filetype, method, operand, bndsf(isf)%xy2,    &
                                              z=bndsf(isf)%sigma, forcingfile=forcingfile, pzmin=pzmin, pzmax=pzmax, targetindex=targetindex)
         else
            success = .true.
         end if
      else
         call mess(LEVEL_WARN, 'Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.')
         call qnerror('Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.',' ',' ')
      end if

   else if (nbndt > 0 .and. (qid == 'tangentialvelocitybnd')) then
      success = ec_addtimespacerelation(qid, xbndt, ybndt, kdt, kx, filename, filetype, method, operand, xy2bndt, forcingfile=forcingfile, targetindex=targetindex)

   else if (nbnduxy > 0 .and. (qid == 'uxuyadvectionvelocitybnd')) then
      kx = 2
      pzmin => zminmaxuxy(1:nbnduxy)
      pzmax => zminmaxuxy(nbnduxy+1:2*nbnduxy)
      success = ec_addtimespacerelation(qid, xbnduxy, ybnduxy, kduxy, kx, filename, filetype, method, operand, xy2bnduxy,   &
                                        z=sigmabnduxy, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile)

   else if (nbndn > 0 .and. (qid == 'normalvelocitybnd')) then
      success = ec_addtimespacerelation(qid, xbndn, ybndn, kdn, kx, filename, filetype, method, operand, xy2bndn, forcingfile=forcingfile, targetindex=targetindex)

   else !There is some boundary that is not detected or recognized
!      success = .false.
! SPvdP: this is not an error, especially for parallel runs
   end if
end function addtimespacerelation_boundaries

!> Initializes memory for laterals on flow nodes.
subroutine ini_alloc_laterals()
   use m_lateral, only : qqlat, kclat, nnlat
   use m_flowgeom, only: ndx2d, ndxi, ndx
   use m_alloc
   use m_flow, only: kmx
   integer :: ierr
   integer :: nlatndguess

   if (.not. allocated(QQlat) ) then                      ! just once
      nlatndguess = ndx2d+2*(ndxi-ndx2d)  ! first guess: all 2D + twice all 1D, nnlat *might* be bigger.
      allocate ( QQLat(max(1,kmx),ndx) , stat=ierr)
      call aerr('QQLAT(ndx)', ierr, ndx)
      QQLat = 0d0
      allocate ( nnLat(nlatndguess) , stat=ierr)
      call aerr('nnLat(nlatndguess)', ierr, nlatndguess)
      nnLat = 0
   endif
   if (.not. allocated(kcLat) ) then
      allocate ( kcLat(ndx) , stat=ierr)                  ! only if needed
      call aerr('kcLat(ndx)', ierr, ndx)
   endif
end subroutine ini_alloc_laterals


!> Prepare the 'kclat' mask array for a specific type of lateral.
subroutine prepare_lateral_mask(kc, ilattype)
   use m_flowgeom
   use m_lateral, only : ILATTP_1D, ILATTP_2D, ILATTP_ALL
   implicit none

   integer         , intent(inout) :: kc(:) !< (ndx) The mask array that is to be filled.
   integer         , intent(in)    :: ilattype !< Type of the new lateral (one of ILATTP_1D|2D|1D2D)

   integer                         :: L, k1, k2

   kc = 0
   select case (ilattype)
   case (ILATTP_1D)       ! in everything 1D
      do L = 1,lnx1D
         !if (abs(prof1D(3,L)) .ne. 1 .and. prof1D(3,L) > 0 ) then ! no pipes pos or neg, others only if pos
            k1 = ln(1,L)
            if (k1 > ndx2d) then
               kc(k1) = 1
            end if
            k2 = ln(2,L)
            if (k2 > ndx2d) then
               kc(k2) = 1
            end if
         !endif
      enddo
   case (ILATTP_2D)       ! in everything 2D
      do L = lnx1D+1,lnxi
         k1 = ln(1,L) ; kc(k1) = 1
         k2 = ln(2,L) ; kc(k2) = 1
      enddo
   case (ILATTP_ALL)      ! both to everything 2D, and 1D, except to 1D pipes
      do L = 1,lnx1D
         ! When is lateral allowed?
         ! * (X)YZ profiles pointering to profiles number: always allow
         ! * direct profiles (rect/circle, etc.):no pipes pos or neg, others only if pos (==non-closed)
         if (prof1D(1,L) < 0 .or. (abs(prof1D(3,L)) .ne. 1 .and. prof1D(3,L) > 0) ) then
            k1 = ln(1,L) ; kc(k1) = 1
            k2 = ln(2,L) ; kc(k2) = 1
         else
            continue
         endif
      enddo
      do L = lnx1D+1,lnxi
         k1 = ln(1,L) ; kc(k1) = 1
         k2 = ln(2,L) ; kc(k2) = 1
      enddo
   end select
end subroutine prepare_lateral_mask


!> Calls the ec_addtimespacerelation with all proper dflowfm-specific
!! target arrays and element set masks for object parameters with
!! spatially uniform time series.
!! Also handles inside one function the old-style *.ext quantities and
!! the new style *.ext and structures.ini quantities.
function adduniformtimerelation_objects(qid, locationfile, objtype, objid, paramname, paramvalue, targetindex, vectormax, targetarray) result(success)
   !use fm_external_forcings_data, no1=>qid, no2=>filetype, no3=>operand, no4 => success
   use m_meteo, no5=>qid, no6=>filetype, no7=>operand, no8 => success
   use string_module, only: strcmpi
   use timespace_parameters, only: uniform, bcascii, spaceandtime
   use unstruc_messages

   implicit none

   character(len=*), intent(in)    :: qid            !< Identifier of current quantity (i.e., 'waterlevelbnd')
   character(len=*), intent(in)    :: locationfile   !< Name of location file (*.pli or *.pol) for current quantity (leave empty when valuestring contains value or filename).
   character(len=*), intent(in)    :: objtype        !< Type name of the object for which this relation is set (e.g., 'lateral', for prettyprinting only).
   character(len=*), intent(in)    :: objid          !< Id of the object for which this relation is set (for prettyprinting only).
   character(len=*), intent(in)    :: paramname      !< Name of the parameter that is set in this relation (e.g., 'discharge', for prettyprinting only).
   character(len=*), intent(in)    :: paramvalue     !< String containing the parameter value (either a scalar double, or 'REALTIME', or a filename)
   integer,          intent(in)    :: targetindex    !< Target index in target value array (typically, the current count of this object type, e.g. numlatsg).
   integer,          intent(in)    :: vectormax      !< The number of values per object ('kx'), typically 1.
   logical                         :: success        !< Return value. Whether relation was added successfully.
   double precision, intent(inout), target :: targetarray(:) !< The target array in which the value(s) will be stored. Either now with scalar, or later via ec_gettimespacevalue() calls.

   character(len=256) :: valuestring, fnam
   double precision   :: valuedble
   double precision   :: xdum(1), ydum(1)
   integer            :: kdum(1)
   integer            :: ierr, L
   double precision, pointer  :: targetarrayptr(:)
   double precision, pointer  :: dbleptr(:)
   integer            :: tgtitem
   integer, pointer   :: intptr, multuniptr
   logical            :: file_exists

   success = .true.   ! initialization
   xdum = 1d0 ; ydum = 1d0; kdum = 1

   if (len_trim(paramvalue) > 0) then
      valuestring = paramvalue
   else if (len_trim(locationfile) > 0) then
      ! Old-style *.ext:
      ! Prepare time series relation, if the .pli file has an associated .tim file.
      L = index(locationfile,'.', back=.true.) - 1
      valuestring = locationfile(1:L)//'_0001.tim'
      inquire(file=valuestring, exist=file_exists)
      if ( .not. file_exists ) then
          valuestring = locationfile(1:L)//'.tim'
          inquire(file=valuestring, exist=file_exists)
          if ( .not. file_exists ) then
             call mess(LEVEL_WARN, 'Files '''//trim(valuestring)//''' and file '''//trim(locationfile(1:L)//'_0001.tim')//''' do not exist.')
          end if
      end if

   else
      ! TODO: AvD: error msg?
      success = .false.
   end if

   ! Now check the valuestring for either scalar/REALTIME/.tim filename
   read(valuestring, *, iostat = ierr) valuedble
   targetarrayptr => targetarray
   tgtitem = ec_undef_int

   if (ierr /= 0 .or. index(valuestring,'/') == 1) then ! No number or a string starting with '/': check for timeseries filename
      if (strcmpi(trim(valuestring), 'REALTIME')) then
         success = .true.
         ! targetarray(targetindex) should be filled via DLL's API
         write(msgbuf, '(a,a,a,a,a)') 'Control for ', trim(objtype), '''' // trim(objid) // ''', ', paramname, ' set to REALTIME.'
         call dbg_flush()
      else
         if (fm_ext_force_name_to_ec_item('','','', qid,multuniptr,intptr,intptr,intptr,dbleptr,dbleptr,dbleptr,dbleptr)) then
            success = .true.
         else
            success = .false.
            write(msgbuf, '(a)') 'Unknown quantity '''//trim(qid)//'''.'
            call warn_flush()
            return
         end if

         fnam = trim(valuestring)
         ! Time-interpolated value will be placed in target array (e.g., qplat(n)) when calling ec_gettimespacevalue.
         if (index(trim(fnam)//'|','.tim|')>0) then
            ! uniform=single time series vectormax = 1
            success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, vectormax, fnam,    &
                                               filetype    = uniform,                     &
                                               method      = spaceandtime,                &
                                               operand     = 'O',                         &
                                               tgt_data1   = targetarrayptr,              &
                                               tgt_item1   = tgtitem,                     &
                                               multuni1    = multuniptr,                  &
                                               targetIndex = targetindex)
         elseif (index(trim(fnam)//'|','.bc|')>0) then
            ! uniform=single time series vectormax = 1

            success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, vectormax, objid,   &
                                               filetype    = bcascii,                     &
                                               method      = spaceandtime,                &
                                               operand     = 'O',                         &
                                               tgt_data1   = targetarrayptr,              &
                                               tgt_item1   = tgtitem,                     &
                                               multuni1    = multuniptr,                  &
                                               targetIndex = targetindex,                 &
                                               forcingfile = fnam)
         endif
      end if
   else
      targetarray(targetindex) = valuedble ! Constant value for always, set it now already.
   end if
end function adduniformtimerelation_objects


subroutine register_quantity_pli_combination(quantity, locationfile)
   use m_alloc
   implicit none
   character(len=*), intent(in)          :: quantity
   character(len=*), intent(in)          :: locationfile
   character(len=max_registered_item_id) :: item_id

   item_id = trim(quantity) // '-' // trim(locationfile)

   if (num_registered_items >= max_ext_bnd_items) then
      max_ext_bnd_items = ceiling(1.2*num_registered_items)
      call realloc(registered_items, max_ext_bnd_items, keepExisting = .true., fill='')
   end if

   num_registered_items = num_registered_items + 1
   registered_items(num_registered_items) = item_id

end subroutine register_quantity_pli_combination

subroutine init_registered_items()
   implicit none
   num_registered_items = 0

   max_ext_bnd_items = 64 ! Default start size.
   if (allocated(registered_items)) deallocate(registered_items)
   allocate(registered_items(max_ext_bnd_items))

   registered_items(1:max_ext_bnd_items) = ''

end subroutine

function quantity_pli_combination_is_registered(quantity, locationfile) result(is_registered)
   implicit none
   logical                               :: is_registered
   character(len=*),intent(in)           :: quantity
   character(len=*),intent(in)           :: locationfile
   integer                               :: i
   character(len=max_registered_item_id) :: item_id

   item_id = trim(quantity) // '-' // trim(locationfile)

   is_registered = .false.

   do i = 1, num_registered_items
      if (item_id == registered_items(i)) then
         is_registered = .true.
         return
      endif
   enddo

end function quantity_pli_combination_is_registered

subroutine init_threttimes()

 use m_flow
 use m_flowgeom
 use fm_external_forcings_data
 use m_transport
 use m_sediment, only: stm_included
 use unstruc_messages
 use m_missing

 implicit none

 integer             :: thrtlen, i, j, nseg, itrac, ifrac, iconst, n, ierr
 character(len=256)  :: qidfm, tracnam, sedfracnam, qidnam
 integer, external   :: findname

 
 ! deallocation of TH arrays
 if(allocated(threttim)) then
    deallocate(threttim)
 endif

 if(nopenbndsect==0) then
    return
 endif

 allocate(threttim(NUMCONST,nopenbndsect), stat=ierr)
 call aerr('threttim(NUMCONST,nopenbndsect)', ierr, nopenbndsect)
 threttim = 0

 ! assign return times using temp arrays
 thrtlen = size(thrtt)
 do i = 1, thrtlen
    qidfm = thrtq(i)
    if(qidfm == 'salinitybnd' .and. allocated(kbnds)) then
       nseg = kbnds(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' salinity boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       threttim(ISALT,nseg) = thrtt(i)
    else if(qidfm == 'temperaturebnd' .and. allocated(kbndtm)) then
       nseg = kbndtm(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' temperature boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       threttim(ITEMP,nseg) = thrtt(i)
    else if(qidfm == 'sedimentbnd' .and. allocated(kbndsd) .and. .not. stm_included) then
       nseg = kbndsd(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' sediment boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       do j = ISED1, ISEDN
         threttim(j,nseg) = thrtt(i)
       enddo
    else if(qidfm(1:9) == 'tracerbnd') then
       call get_tracername(qidfm, tracnam, qidnam)
       itrac = findname(numtracers, trnames, tracnam)
       if (allocated(bndtr).and.thrtn(i)<=nbndtr(itrac) ) then
          nseg = bndtr(itrac)%k(5,thrtn(i))
          if (nseg /=i) cycle
          if (nseg == 0 .or. nseg > nopenbndsect) then
             write(msgbuf,'(i8,a)') thrtn(i), ' tracer boundary point is assigned to incorrect boundary segment' ; call err_flush()
             cycle
          endif
          iconst = itrac2const(itrac)
          threttim(iconst,nseg) = thrtt(i)
       endif
    else if(qidfm(1:10) == 'sedfracbnd') then
       ierr = 0
       call get_sedfracname(qidfm, sedfracnam, qidnam)
       ifrac = findname(numfracs, sfnames, sedfracnam)
       if (allocated(bndsf).and.thrtn(i)<=nbndsf(ifrac)) then      ! i      = no of TH boundaries (i.e. 1 per fraction bnd)
                                                                   ! thrtn  = no of boundaries per fraction
                                                                   ! nbndsf = total no of bnd links per fractions
          nseg = bndsf(ifrac)%k(5,thrtn(i))  ! 5, has open bnd section where TH bnd applies
          !if (nseg /=i) cycle
          if (nseg == 0 .or. nseg > nopenbndsect) then
             ierr = 1
          endif
          iconst = ifrac2const(ifrac)
          if (iconst==0) cycle
          threttim(iconst,nseg) = thrtt(i)
       else
           ierr = 1
       endif
       if( ierr /= 0 ) then
           write(msgbuf,'(i8,a)') thrtn(i), ' sedfrac boundary point is assigned to incorrect boundary segment' ; call err_flush()
           cycle
       endif
    endif
 enddo

 if(allocated(thtbnds)) deallocate(thtbnds)
 if(allocated(thzbnds)) deallocate(thzbnds)
 if(allocated(thtbndtm)) deallocate(thtbndtm)
 if(allocated(thzbndtm)) deallocate(thzbndtm)
 if(allocated(thtbndsd)) deallocate(thtbndsd)
 if(allocated(thzbndsd)) deallocate(thzbndsd)

 allocate(thtbnds(nbnds), thzbnds(nbnds*kmxd), thtbndtm(nbndtm), thzbndtm(nbndtm*kmxd), thtbndsd(nbndsd), thzbndsd(nbndsd*kmxd), stat=ierr)
 call aerr('thtbnds(nbnds), thzbnds(nbnds*kmxd), thtbndtm(nbndtm), thzbndtm(nbndtm*kmxd), thtbndsd(nbndsd), thzbndsd(nbndsd*kmxd)', ierr, (kmxd+1)*(nbnds+nbndtm+nbndsd))
 thzbnds = DMISS

 do i = 1,nbnds
    thtbnds(i) = threttim(ISALT,kbnds(5,i))
 enddo

 do i = 1,nbndtm
    thtbndtm(i) = threttim(ITEMP,kbndtm(5,i))
 enddo

 do i = 1,nbndsd
    thtbndsd(i) = threttim(ISED1,kbndsd(5,i))
 enddo

 if (allocated(bndtr)) then
    do itrac = 1, numtracers
       iconst = itrac2const(itrac)

       if(allocated(bndtr(itrac)%tht)) deallocate(bndtr(itrac)%tht)
       if(allocated(bndtr(itrac)%thz)) deallocate(bndtr(itrac)%thz)

       n = nbndtr(itrac)

       allocate (bndtr(itrac)%tht(n), bndtr(itrac)%thz(n*kmxd), stat=ierr)
       call aerr('bndtr(itrac)%tht(n), bndtr(itrac)%thz(n*kmxd)', ierr, n*(kmxd+1))

       bndtr(itrac)%thz = dmiss
       do i = 1,n
         bndtr(itrac)%tht(i) = threttim(iconst,bndtr(itrac)%k(5,i))
       enddo
    enddo
 endif

 if (allocated(bndsf)) then
    do ifrac = 1, numfracs
       if(allocated(bndsf(ifrac)%tht)) deallocate(bndsf(ifrac)%tht)
       if(allocated(bndsf(ifrac)%thz)) deallocate(bndsf(ifrac)%thz)

       n = nbndsf(ifrac)

       allocate(bndsf(ifrac)%tht(n), bndsf(ifrac)%thz(n*kmxd), stat=ierr)
       call aerr('bndsf(ifrac)%tht(n), bndsf(ifrac)%thz(n*kmxd)', ierr, n*(kmxd+1))

       bndsf(ifrac)%thz = dmiss
       ! mapping to constituents, just in case fracs do not map sequentially to ised1 and so on
       iconst = ifrac2const(ifrac)
       if (iconst==0) then
          bndsf(ifrac)%tht = 0d0
       else
          do i = 1,n
             bndsf(ifrac)%tht(i) = threttim(iconst,bndsf(ifrac)%k(5,i))
          enddo
       end if
    enddo
 endif

end subroutine init_threttimes

subroutine allocatewindarrays()
   use m_wind
   use m_flow
   use m_flowgeom

   implicit none

   integer:: ierr

   if (.not. allocated (wx) ) then
      allocate  ( wx(lnx), wy(lnx), wdsu(lnx), wdsu_x(lnx), wdsu_y(lnx) , stat=ierr)
      call aerr ('wx(lnx), wy(lnx), wdsu(lnx), wdsu_x(lnx), wdsu_y(lnx)', ierr, lnx)
      wx     = 0d0
      wy     = 0d0
      wdsu   = 0d0
      wdsu_x = 0d0
      wdsu_y = 0d0
   endif 
 
end subroutine allocatewindarrays

end module m_external_forcings
