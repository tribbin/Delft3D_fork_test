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
!

module m_setrho
   use precision_basics, only: dp
   use m_densfm, only: RHO_MIN, RHO_MAX

   implicit none

   private

   public :: set_potential_density, set_pressure_dependent_density, setrhofixedp, get_sal_and_temp

contains

   !> Fill potential density of one column
   subroutine set_potential_density(potential_density, cell_index_2d)
      use m_densfm, only: densfm, add_sediment_effect_to_density
      use precision, only: dp
      use m_flow, only: rho, kmxn
      use m_get_kbot_ktop, only: getkbotktop

      real(kind=dp), dimension(:), intent(out) :: potential_density !< Potential density of fluid
      integer, intent(in) :: cell_index_2d !< Horizontal cell index (1:ndx)

      real(kind=dp) :: salinity, temperature
      integer :: k_bot, k_top
      integer :: cell_index_3d ! vertical cell index (e.g., k_bot:k_top)

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (k_top < k_bot) then
         return
      end if

      do cell_index_3d = k_bot, k_top
         call get_sal_and_temp(cell_index_3d, salinity, temperature)
         potential_density(cell_index_3d) = densfm(salinity, temperature)
         call add_sediment_effect_to_density(potential_density(cell_index_3d), cell_index_3d)
         ! check overshoots at thin water layers
         potential_density(cell_index_3d) = min(potential_density(cell_index_3d), RHO_MAX)
         potential_density(cell_index_3d) = max(potential_density(cell_index_3d), RHO_MIN)
      end do

      do cell_index_3d = k_top + 1, k_bot + kmxn(cell_index_2d) - 1
         potential_density(cell_index_3d) = potential_density(k_top)
      end do

   end subroutine set_potential_density

   !> Fill in-situ density of one column
   subroutine set_pressure_dependent_density(in_situ_density, cell_index_2d)
      use precision, only: dp
      use m_flow, only: rho, density_is_pressure_dependent, kmxn, zws
      use m_get_kbot_ktop, only: getkbotktop
      use m_physcoef, only: Maxitpresdens, ag
      use m_densfm, only: densfm, add_sediment_effect_to_density

      real(kind=dp), dimension(:), intent(out) :: in_situ_density !< Pressure dependent density of fluid
      integer, intent(in) :: cell_index_2d !< Horizontal cell index (1:ndx)

      real(kind=dp) :: salinity, temperature, cell_pressure_upper_interface, cell_pressure_lower_interface, dz
      integer :: k_bot, k_top, i
      integer :: cell_index_3d ! vertical cell index (e.g., k_bot:k_top)

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (k_top < k_bot) then
         return
      end if

      cell_pressure_upper_interface = 0.0_dp ! surface value is 0 bar in unesco, not 1 bar
      do cell_index_3d = k_top, k_bot, -1
         call get_sal_and_temp(cell_index_3d, salinity, temperature)
         dz = zws(cell_index_3d) - zws(cell_index_3d - 1)
         do i = 1, Maxitpresdens
            cell_pressure_lower_interface = cell_pressure_upper_interface + ag * dz * in_situ_density(cell_index_3d)
            in_situ_density(cell_index_3d) = densfm(salinity, temperature, 0.5_dp * (cell_pressure_lower_interface + cell_pressure_upper_interface))
         end do
         cell_pressure_upper_interface = cell_pressure_lower_interface
         call add_sediment_effect_to_density(in_situ_density(cell_index_3d), cell_index_3d)
         ! Check overshoots at thin water layers
         in_situ_density(cell_index_3d) = min(in_situ_density(cell_index_3d), RHO_MAX)
         in_situ_density(cell_index_3d) = max(in_situ_density(cell_index_3d), RHO_MIN)
      end do

      do cell_index_3d = k_top + 1, k_bot + kmxn(cell_index_2d) - 1
         in_situ_density(cell_index_3d) = in_situ_density(k_top)
      end do
   end subroutine set_pressure_dependent_density

   real(kind=dp) function setrhofixedp(cell_index_3d, p0)
      use precision, only: dp
      use m_densfm, only: densfm, add_sediment_effect_to_density

      implicit none

      integer, intent(in) :: cell_index_3d !< cell number
      real(kind=dp), intent(in) :: p0 !< some given pressure

      real(kind=dp) :: salinity, temperature

      call get_sal_and_temp(cell_index_3d, salinity, temperature)

      setrhofixedp = densfm(salinity, temperature, p0)

      call add_sediment_effect_to_density(setrhofixedp, cell_index_3d)
   end function setrhofixedp

   subroutine get_sal_and_temp(cell_index_3d, salinity, temperature)
      use precision, only: dp
      use m_flow, only: jasal, jatem, backgroundsalinity, backgroundwatertemperature
      use m_transport, only: isalt, itemp, constituents

      implicit none

      integer, intent(in) :: cell_index_3d !< cell index
      real(kind=dp), intent(out) :: salinity, temperature

      if (jasal > 0) then
         salinity = max(0.0_dp, constituents(isalt, cell_index_3d))
      else
         salinity = backgroundsalinity
      end if

      if (jatem > 0) then
         temperature = max(-5.0_dp, constituents(itemp, cell_index_3d))
      else
         temperature = backgroundwatertemperature
      end if
   end subroutine get_sal_and_temp

end module m_setrho
