 !!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!> This module implements the statistical output in D-Flow FM.
module m_statistical_output
   use MessageHandling
   use m_output_config
   
private

   public realloc
   public dealloc
   public update_statistical_output

   !> Realloc memory cross-section definition or cross-sections
   interface realloc
      module procedure realloc_stat_output
   end interface

   !> Free the memory of cross-section definition or cross-sections
   interface dealloc
      module procedure dealloc_stat_output
   end interface dealloc

   integer, parameter :: SO_CURRENT = 1
   integer, parameter :: SO_AVERAGE = 2
   integer, parameter :: SO_MAX     = 3
   integer, parameter :: SO_MIN     = 4
   
   integer :: window_size !< The size of the moving average window, in samples.
   
   !> Derived type for the statistical output items. 
   type, public :: t_output_variable_item
      type(t_output_quantity_config), pointer   :: output_config        !< Pointer to output configuration item.
      integer                                   :: operation_id         !< Specifies the kind of operation to perform on the output variable.
      integer                                   :: current_step         !< Latest entry in the work array. MOD(current_step+1,total_steps_count) is the next 
      integer                                   :: total_steps_count
      !< item to remove.   
      double precision, pointer, dimension(:)   :: stat_output          !< Array that is to be written to the Netcdf file. In case the current values are
                                                                        !< required this variable points to the basic variable (e.g. s1).
                                                                        !< Otherwise during the simulation the intermediate results are stored.
      double precision, pointer, dimension(:)   :: stat_input           !< In case a statistical operation is requested. This pointer points to the
                                                                        !< basic variable.
      double precision, pointer, dimension(:)   :: source_input         !< pointer to the basic variable
      double precision, pointer, dimension(:,:) :: samples              !< In case a moving average is requested. This pointer points to the
                                                                        !< work array, where the different samples are stored.
      double precision, pointer, dimension(:)   :: moving_average_sum !< In case a moving average is requested. This pointer points to the
                                                                        !< actual average values.
      double precision                          :: timestep_sum         !< sum of timesteps (for moving average/ average calculation)
      
      double precision, pointer, dimension(:)   :: timesteps            !< array of timesteps belonging to samples in samples array

   end type t_output_variable_item

   !> Derived type to store the cross-section set
   type, public :: t_output_variable_set
      integer                                                :: size = 0                  !< 
      integer                                                :: growsby = 200             !< 
      integer                                                :: count= 0                  !< 
      type(t_output_variable_item), pointer, dimension(:)    :: statout                   !< 
   end type t_output_variable_set

contains

   subroutine realloc_stat_output(statoutput)
      ! Modules
      use m_alloc

      implicit none
      ! Input/output parameters
      type(t_output_variable_set), intent(inout)   :: statoutput !< Current cross-section definition
      
   
      ! Local variables
      integer                   :: ierr
      type(t_output_variable_item), pointer, dimension(:)    :: oldstats
   
      ! Program code
   
      if (statoutput%size > 0) then
         oldstats=>statoutput%statout
      endif
   
      if (statoutput%growsBy <=0) then
         statoutput%growsBy = 200
      endif
      allocate(statoutput%statout(statoutput%size+statoutput%growsBy),stat=ierr)
      call aerr('statoutput%statout(statoutput%size+statoutput%growsBy)',ierr,statoutput%size+statoutput%growsBy)
   
      if (statoutput%size > 0) then
         statoutput%statout(1:statoutput%size) = oldstats(1:statoutput%size)
         deallocate(oldstats)
      endif
      statoutput%size = statoutput%size+statoutput%growsBy
   end subroutine realloc_stat_output

   subroutine dealloc_stat_output(statoutput)
      implicit none
      ! Input/output parameters
      type(t_output_variable_set), intent(inout)   :: statoutput !< Current cross-section definition

      if (statoutput%size> 0) then
         deallocate(statoutput%statout)
      endif
   end subroutine dealloc_stat_output

   !> updates the moving average of a stat_out_item by removing the oldest and adding the newest value
   elemental subroutine update_moving_average(i)

      type(t_output_variable_item), intent(inout) :: i !< statistical output item to update
      
      integer :: jnew, jold !< Index to newest and oldest timestep in samples array

      jnew = i%current_step
      jold = MOD(i%current_step,i%total_steps_count)+1
   
      !when timestep < windowsize, no samples need to be removed. The timesteps array and samples array will be initialized to 0 so that we can keep the same expression.
      i%moving_average_sum = i%moving_average_sum - i%samples(:,jold)*i%timesteps(jold) + i%samples(:,jnew)*i%timesteps(jnew)
      i%timestep_sum = i%timestep_sum - i%timesteps(jold) + i%timesteps(jnew)
      i%stat_input = i%moving_average_sum/i%timestep_sum
   
   end subroutine update_moving_average

   !> adds a new sample, and its timestep to the samples array. Only needed for moving average calculation.
   elemental subroutine add_statistical_output_sample(i,timestep)

      type(t_output_variable_item), intent(inout) :: i         !< statistical output item to update
      double precision, intent(in)                :: timestep  !< this is usually dts

      i%timesteps(i%current_step) = timestep
      i%samples(:,i%current_step) = i%source_input

   end subroutine add_statistical_output_sample

   !> updates a stat_output_item using the stat_input array, depending on the operation_id
   !  stat_input is filled elsewhere and can be a moving average or a pointer to an input variable.
   elemental subroutine update_statistical_output(i,dts)
      
      type(t_output_variable_item), intent(inout) :: i   !< statistical output item to update
      double precision,             intent(in)    :: dts !< current timestep

      if (i%operation_id == SO_MIN .or. i%operation_id == SO_MAX ) then ! max/min of moving average requested
         call add_statistical_output_sample(i,dts)
         call update_moving_average(i)
         i%current_step = mod(i%current_step+1,i%total_steps_count)
      endif

      select case (i%operation_id)
      case (SO_CURRENT)
         continue
      case (SO_AVERAGE) 
         i%stat_output = i%stat_output + i%stat_input * dts
         i%timestep_sum = i%timestep_sum + dts
      case (SO_MAX) 
         i%stat_output = max(i%stat_output,i%stat_input)
      case (SO_MIN) 
         i%stat_output = min(i%stat_output,i%stat_input)
      case default
         return
      end select

   end subroutine update_statistical_output

   !> in case average is requested, a division is required before the average can be written
   subroutine finalize_SO_AVERAGE(i) 

      type(t_output_variable_item), intent(inout) :: i !>

      if (i%operation_id == SO_AVERAGE) then 
         i%stat_output = i%stat_output/i%timestep_sum
      endif

   end subroutine finalize_SO_AVERAGE

   ! every output interval the stat_output needs to be reset.
   subroutine reset_statistical_output(i)

      type(t_output_variable_item), intent(inout) :: i !< statistical output item to reset
      
      select case (i%operation_id)
      case (SO_CURRENT)
         continue
      case (SO_AVERAGE)
         i%stat_output = 0 
         i%timestep_sum = 0 !new sum every output interval
      case (SO_MAX)
         i%stat_output = -huge
      case (SO_MIN)
         i%stat_output = huge
      case default 
         call mess(LEVEL_ERROR, 'update_statistical_output: invalid operation_id')
      end select

   end subroutine reset_statistical_output
   
   subroutine flow_init_statistical_output_his(output_config,output_set)
   use m_flow
   use m_flowexternalforcings
   
      type(t_output_variable_set),    intent(inout)   :: output_set    !> output set that items need to be added to
      type(t_output_quantity_config_set), intent(in)  :: output_config !> output config for which an output set is needed.
      if (jahisbal > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VOLTOT                     ),voltot(1:1)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_STOR                       ),voltot(2:2)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VOLERR                     ),voltot(3:3)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDIN                      ),voltot(4:4)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDOUT                     ),voltot(5:5)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDTOT                     ),voltot(6:6)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHIN                     ),voltot(7:7)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHOUT                    ),voltot(8:8)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHTOT                    ),voltot(9:9)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PRECIP_TOTAL               ),voltot(10:10)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EVAP                       ),voltot(11:11)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOUR                       ),voltot(12:12)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INTERNALTIDESDISSIPATION   ),voltot(13:13)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GravInput                  ),voltot(14:14)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SalInput                   ),voltot(15:15)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SalInput2                  ),voltot(16:16)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWIN                      ),voltot(17:17)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWOUT                     ),voltot(18:18)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWTOT                     ),voltot(19:19)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN                      ),voltot(20:20)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT                     ),voltot(21:21)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT                     ),voltot(22:22)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN1D                    ),voltot(23:23)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT1D                   ),voltot(24:24)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT1D                   ),voltot(25:25)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN2D                    ),voltot(26:26)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT2D                   ),voltot(27:27)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT2D                   ),voltot(28:28)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN                      ),voltot(29:29)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT                     ),voltot(30:30)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT                     ),voltot(31:31)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN1D                    ),voltot(32:32)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT1D                   ),voltot(33:33)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT1D                   ),voltot(34:34)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN2D                    ),voltot(35:35)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT2D                   ),voltot(36:36)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT2D                   ),voltot(37:37)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ICEPT                      ),voltot(38:38)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EVAP_ICEPT                 ),voltot(39:39)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PRECIP_GROUND              ),voltot(40:40)                                  )
      endif
      !if (jahissourcesink > 0 .and. numsrc > 0) then
      !   call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_DISCHARGE                          )
      !   call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_SALINITY_INCREMENT                 )
      !   call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_TEMPERATURE_INCREMENT              )
      !   call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_CURRENT_DISCHARGE                             )
      !   call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_CUMULATIVE_VOLUME                ),vsrccum         )
      !   call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_DISCHARGE_AVERAGE                             )
      !endif
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_CREST_LEVEL                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_LOWER_EDGE_LEVEL                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_WIDTH                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1UP                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1DN                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_HEAD                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_CREST_WIDTH                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_THROUGH_GATE_OPENING          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_OVER_GATE                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_UNDER_GATE                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_HEIGHT                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_UPPER_EDGE_LEVEL                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_THROUGH_GATE_OPENING           )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_OVER_GATE                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_UNDER_GATE                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_IN_GATE_OPENING               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_OVER_GATE                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_UNDER_GATE                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_STATE                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1_ON_CREST                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FORCE_DIFFERENCE                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_DISCHARGE                                            )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_CREST_LEVEL                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_S1UP                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_S1DN                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_STRUCTURE_DISCHARGE                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_CAPACITY                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_DISCHARGE_DIR                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1UP                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1DN                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_STRUCTURE_HEAD                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_ACTUAL_STAGE                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_HEAD                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_REDUCTION_FACTOR                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1_DELIVERY_SIDE                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1_SUCTION_SIDE                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_DISCHARGE                                            )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_LOWER_EDGE_LEVEL                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_S1UP                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_S1DN                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_DISCHARGE                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_CREST_LEVEL                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_CREST_WIDTH                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_GATE_LOWER_EDGE_LEVEL                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_FLOW_THROUGH_HEIGHT                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_GATE_OPENING_WIDTH                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_S1UP                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_S1DN                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_DISCHARGE                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_CREST_LEVEL                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_CREST_WIDTH                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1UP                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1DN                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_STRUCTURE_HEAD                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_VELOCITY                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_FLOW_AREA                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_STATE                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_FORCE_DIFFERENCE                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1_ON_CREST                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_DISCHARGE                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_CREST_LEVEL                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_CREST_WIDTH                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_GATE_LOWER_EDGE_LEVEL                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1UP                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1DN                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_GATE_OPENING_HEIGHT                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_HEAD                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_FLOW_AREA                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_STATE                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1_ON_CREST                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_VELOCITY                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_FORCE_DIFFERENCE                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_DISCHARGE                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_S1UP                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_S1DN                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_HEAD                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_FLOW_AREA                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_VELOCITY                                           )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BLUP                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BLDN                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BL_ACTUAL                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_DISCHARGE                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_CREST_LEVEL                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_GATE_LOWER_EDGE_LEVEL                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_S1UP                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_S1DN                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_GATE_OPENING_HEIGHT                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_HEAD                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_FLOW_AREA                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_VELOCITY                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_STATE                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_CREST_WIDTH                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_S1UP                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_S1DN                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_DISCHARGE                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CUMULATIVE_DISCHARGE                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_VELOCITY                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_HEAD                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_FLOW_AREA                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CREST_LEVEL                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CREST_WIDTH                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_BREACH_WIDTH_TIME_DERIVATIVE                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_WATER_LEVEL_JUMP                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_DISCHARGE                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_CREST_LEVEL                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_S1UP                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_S1DN                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_HEAD                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_FLOW_AREA                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_VELOCITY                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_DISCHARGE                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_S1UP                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_S1DN                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_HEAD                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_FLOW_AREA                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_VELOCITY                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_DISCHARGE                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_S1UP                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_S1DN                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_HEAD                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_FLOW_AREA                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_VELOCITY                                      )
      !call add_stat_output_item(output_set, output_config%statout(LONGCULVERT_VALVE_RELATIVE_OPENING                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TKE                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VICWW                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EPS                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDX                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDX_SFERIC                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDY                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDY_SFERIC                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RAIN                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INFILTRATION_CAP                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INFILTRATION_INFILTRATION_ACTUAL                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TEMPERATURE                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WIND                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAIR                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RHUM                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CLOU                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QSUN                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QEVA                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QCON                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QLONG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QFREVA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QFRCON                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QTOT                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SALINITY                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_POTENTIAL_DENSITY                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DENSITY                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRUNT_VAISALA_N2                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WATERLEVEL                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BEDLEVEL                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WATERDEPTH                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_HWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_HWAV_SIG                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PHIWAV                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RLABDA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UORB                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_USTOKES                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VSTOKES                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_X_VELOCITY                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Y_VELOCITY                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Z_VELOCITY                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DEPTH_AVERAGED_X_VELOCITY                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DEPTH_AVERAGED_Y_VELOCITY                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SED                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WS                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SEDDIF                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CONSTITUENTS                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_INSTANTANEOUS                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_AVERAGE                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_REALIZED_DISCHARGE_INSTANTANEOUS                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_REALIZED_DISCHARGE_AVERAGE                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAUSX                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAUSY                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DISCHARGE_MAGNITUDE                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RICH                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_S0                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_S1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_POTEVAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_ACTEVAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_PRESCREVAP                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VOL1                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATERDEPTH                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HU                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NEGDPT                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NEGDPT_CUM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NOITER                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NOITER_CUM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_LIMTSTEP                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_LIMTSTEP_CUM                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_COURANT                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_AU                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_U1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_U0                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCXQ_EULERIAN                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCYQ_EULERIAN                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCXQ                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCYQ                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAG_EULER                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAGA_GLM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAGA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WW1                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RHO                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VIU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DIU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Q1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Q1_MAIN                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FIXED_WEIR_ENERGY_LOSS                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SPIRCRV                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SPIRINT                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUMLIMDT                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSX                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSY                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUS                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSMAX                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Z0UCUR                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Z0UROU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SA1                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CZS                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CZU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFUTYP                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TEM1                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CONST                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MORS                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TURKIN1                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VICWWU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1_3                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1_4                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_0                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_1                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_2                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFCL                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RAINFALL_RATE                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_INTERCEPTION_WATERDEPTH                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_PATM                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDX                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDY                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDXU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDYU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDX_SFERIC                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDY_SFERIC                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDXU_SFERIC                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDYU_SFERIC                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSX                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSY                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSX_SFERIC                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSY_SFERIC                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAIR                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RHUM                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CLOU                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QSUN                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QEVA                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QCON                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QLONG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QFREVA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QFRCON                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QTOT                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TIDALPOTENTIAL                                            )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SALPOTENTIAL                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_INTERNAL_TIDES_DISSIPATION                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TNUDGE                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_TEM                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_SAL                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_DTEM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_DSAL                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HWAV_SIG                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TP                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DIR                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXBWAV                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYBWAV                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MX                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MY                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DISSURF                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DISWCAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UORB                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_E                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_R                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DR                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_D                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DF                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXX                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYY                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXY                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CGWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SIGMWAV                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_KWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CTHETA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_L1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SWE                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SWT                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UST_CC                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VST_CC                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_USTOKES                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VSTOKES                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_THETAMEAN                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FX                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FY                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WAVFU                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WAVFV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DTCELL                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TIME_WATER_ON_GROUND                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FREEBOARD                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATERDEPTH_ON_GROUND                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VOLUME_ON_GROUND                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CURRENT_TOTAL_NET_INFLOW_1D2D                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_1D2D                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CURRENT_TOTAL_NET_INFLOW_LATERAL                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_LATERAL                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATER_LEVEL_GRADIENT                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QIN                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_S1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_WATERDEPTH                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCMAG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCMAG_EULER                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCDIR                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCDIR_EULER                                               )
      !
      
      call initialize_statistical_output(output_set)
         
   end subroutine flow_init_statistical_output_his
   
   subroutine add_stat_output_item(output_set, output_config, data_pointer)
   
      type(t_output_variable_set), intent(inout) :: output_set             !> output set that items need to be added to
      type(t_output_quantity_config), pointer, intent(in) :: output_config          !> output quantity config linked to output item
      double precision, pointer, dimension(:), intent(in) :: data_pointer  !> pointer to output quantity data
      
      type(t_output_variable_item) :: item !> new item to be added
      
      output_set%count = output_set%count + 1
      if (output_set%count > output_set%size) then
         call realloc_stat_output(output_set)
      endif
      
      item%output_config => output_config
      item%operation_id = set_operation_id(output_config)
      item%source_input => data_pointer
      
      output_set%statout(output_set%count) = item
      
   end subroutine add_stat_output_item
   
   integer function set_operation_id(output_config)
   use string_module, only: str_tolower
   
      type(t_output_quantity_config), intent(in) :: output_config          !> output quantity config linked to output item
      
      set_operation_id = -1
      
      if      (trim(str_tolower(output_config%input_value)) == 'current' ) then
         set_operation_id = SO_CURRENT
      else if (trim(str_tolower(output_config%input_value)) == 'average' ) then
         set_operation_id = SO_AVERAGE
      else if (trim(str_tolower(output_config%input_value)) == 'max' ) then
         set_operation_id = SO_MAX
      else if (trim(str_tolower(output_config%input_value)) == 'min' ) then
         set_operation_id = SO_MIN
      endif
         
   end function set_operation_id
      
   !> For every item in output_set, allocate arrays depending on operation id
   subroutine initialize_statistical_output(output_set)
   
      type(t_output_variable_set), intent(inout) :: output_set !> output set that needs to be initialized

      type(t_output_variable_item), pointer  :: i
      integer :: j, inputsize
      logical :: success
      
      do j = 1, output_set%count
         i => output_set%statout(j)
         input_size = size(i%source_input)
         
         !call set_statistical_output_pointer(i,success)
         if (success) then
            select case (i%operation_id)
            case (SO_CURRENT)
               i%stat_output => i%stat_input
            case (SO_AVERAGE)
               allocate(i%stat_output(input_size))
               i%stat_input => i%source_input
            case (SO_MIN, SO_MAX)
               allocate(i%stat_output(input_size),i%moving_average_sum(input_size), &
                        i%samples(input_size,window_size),i%timesteps(window_size),i%stat_input(input_size))

               i%moving_average_sum = 0
               i%samples = 0
               i%timesteps = 0
               i%timestep_sum = 0
            case default
               call mess(LEVEL_ERROR, 'update_statistical_output: invalid operation_id')
            end select

            call reset_statistical_output(i)
         endif
      enddo

   end subroutine initialize_statistical_output
   

end module
