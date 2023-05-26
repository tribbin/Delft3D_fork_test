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
      integer                                                :: size = 0                  !< Actual size of cross-section set
      integer                                                :: growsby = 200             !< Increment for cross-section set
      integer                                                :: count= 0                  !< Actual number of cross-section sets
      type(t_output_variable_item), pointer, dimension(:)    :: statout                   !< Current cross-section
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
   
   subroutine flow_init_satistical_output(output_set,output_config)

      type(t_output_variable_set), intent(inout) :: output_set !> output set that items need to be added to
         
      
      call initialize_statistical_output(output_set)
         
   end subroutine check_and_add_stat_output_item
   
   subroutine add_stat_output_item(output_set,data_pointer)
   
         type(t_output_variable_set), intent(inout) :: output_set !> output set that items need to be added to
         
         output_set%count = output_set%count + 1
         if (output_set%count > output_set%size) then
            call realloc_stat_output(output_set)
         endif
         
         
            
         
         type(t_output_variable_item) :: item !> new output item to be created
      
   
   end subroutine add_stat_output_item(output_set)
      
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
