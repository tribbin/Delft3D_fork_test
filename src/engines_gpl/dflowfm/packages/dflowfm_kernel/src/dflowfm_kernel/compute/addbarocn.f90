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

module m_add_baroclinic_pressure_cell
   use precision, only: dp

   implicit none

   private

   real(kind=dp), parameter :: MIN_DENSITY_ANOMALY = 1e-10_dp ! Small value (to avoid zero) at cells with small waterdepth

   public :: add_baroclinic_pressure_cell, add_baroclinic_pressure_cell_interface, add_baroclinic_pressure_cell_use_rho_directly

contains

   !> Computes baroclinic pressure gradients across layers for a horizontal cell.
   !! Density is based on linear interpolation of density at vertical interfaces.
   subroutine add_baroclinic_pressure_cell(cell_index_2d)
      use m_turbulence, only: rho, baroclinic_pressure_term, vertical_density_anomaly
      use m_flowparameters, only: epshu
      use m_flow, only: zws
      use m_physcoef, only: rhomean
      use m_get_kbot_ktop, only: getkbotktop

      integer, intent(in) :: cell_index_2d !< horizontal cell index

      integer :: cell_index_3d, k_bot, k_top
      real(kind=dp) :: pressure_up, pressure_down, gr, layer_tichkness
      real(kind=dp) :: fuu, fud, fdu, fdd, dzu, dzd, rho_up, rho_down, layer_density_anomaly

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (zws(k_top) - zws(k_bot - 1) < epshu) then
         baroclinic_pressure_term(k_bot:k_top) = 0.0_dp
         vertical_density_anomaly(k_bot:k_top) = MIN_DENSITY_ANOMALY
         return
      end if

      baroclinic_pressure_term(k_top) = 0.0_dp
      vertical_density_anomaly(k_top) = 0.0_dp
      pressure_down = 0.0_dp
      do cell_index_3d = k_top, k_bot, -1
         layer_tichkness = zws(cell_index_3d) - zws(cell_index_3d - 1)
         if (k_bot == k_top) then
            rho_up = rho(cell_index_3d) - rhomean
            rho_down = rho(cell_index_3d) - rhomean
         else if (cell_index_3d > k_bot .and. cell_index_3d < k_top) then
            dzu = zws(cell_index_3d + 1) - zws(cell_index_3d)
            dzd = zws(cell_index_3d - 1) - zws(cell_index_3d - 2)
            fuu = dzu / (dzu + layer_tichkness)
            fud = 1.0_dp - fuu
            fdu = layer_tichkness / (dzd + layer_tichkness)
            fdd = 1.0_dp - fdu
            rho_up = fuu * rho(cell_index_3d + 1) + fud * rho(cell_index_3d) - rhomean
            rho_down = fdu * rho(cell_index_3d) + fdd * rho(cell_index_3d - 1) - rhomean
         else if (cell_index_3d == k_bot) then
            dzu = zws(cell_index_3d + 1) - zws(cell_index_3d)
            fuu = dzu / (dzu + layer_tichkness)
            fud = 1.0_dp - fuu
            rho_up = fuu * rho(cell_index_3d + 1) + fud * rho(cell_index_3d) - rhomean
            rho_down = 2.0_dp * (rho(cell_index_3d) - rhomean) - rho_up
         else if (cell_index_3d == k_top) then
            dzd = zws(cell_index_3d - 1) - zws(cell_index_3d - 2)
            fdu = layer_tichkness / (dzd + layer_tichkness)
            fdd = 1.0_dp - fdu
            rho_down = fdu * rho(cell_index_3d) + fdd * rho(cell_index_3d - 1) - rhomean
            rho_up = 2.0_dp * (rho(cell_index_3d) - rhomean) - rho_down
         end if
         layer_density_anomaly = 0.5_dp * (rho_up + rho_down) * layer_tichkness
         pressure_up = pressure_down
         pressure_down = pressure_up + layer_density_anomaly
         vertical_density_anomaly(cell_index_3d) = pressure_down
         gr = pressure_up * layer_tichkness + 0.5_dp * ((2.0_dp * rho_up + rho_down) / 3.0_dp) * layer_tichkness * layer_tichkness ! your left  wall
         baroclinic_pressure_term(cell_index_3d) = gr
      end do
   end subroutine add_baroclinic_pressure_cell

   !> Computes baroclinic pressure gradients across layers for a horizontal cell.
   !! Density is based on linear interpolation of recomputed density (from salinity, temperature (and pressure)) at vertical interfaces.
   subroutine add_baroclinic_pressure_cell_interface(cell_index_2d)
      use m_turbulence, only: baroclinic_pressure_term, vertical_density_anomaly, kmxx, rhosww, rho
      use m_flowparameters, only: epshu
      use m_flow, only: zws
      use m_transport, only: ISALT, ITEMP, constituents
      use m_physcoef, only: rhomean, max_iterations_pressure_density, apply_thermobaricity, ag
      use m_get_kbot_ktop, only: getkbotktop
      use m_density, only: calculate_density

      integer, intent(in) :: cell_index_2d !< horizontal cell index

      integer :: cell_index_3d, k_bot, k_top, i
      real(kind=dp) :: salinity_at_interface(0:kmxx), temperature_at_interface(0:kmxx) ! salinity and temperature at pressure point layer interfaces
      real(kind=dp) :: fzu, fzd, pressure_up, pressure_down, layer_tichkness, p0d, pdb, rhosk

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (zws(k_top) - zws(k_bot - 1) < epshu) then
         baroclinic_pressure_term(k_bot:k_top) = 0.0_dp
         vertical_density_anomaly(k_bot:k_top) = MIN_DENSITY_ANOMALY
         return
      end if

      if (k_top > k_bot) then
         do cell_index_3d = k_bot, k_top - 1
            fzu = (zws(cell_index_3d + 1) - zws(cell_index_3d)) / (zws(cell_index_3d + 1) - zws(cell_index_3d - 1))
            fzd = 1.0_dp - fzu
            salinity_at_interface(cell_index_3d - k_bot + 1) = fzu * constituents(isalt, cell_index_3d + 1) + fzd * constituents(isalt, cell_index_3d)
            temperature_at_interface(cell_index_3d - k_bot + 1) = fzu * constituents(itemp, cell_index_3d + 1) + fzd * constituents(itemp, cell_index_3d)
         end do
         salinity_at_interface(0) = 2.0_dp * constituents(isalt, k_bot) - salinity_at_interface(1)
         temperature_at_interface(0) = 2.0_dp * constituents(itemp, k_bot) - temperature_at_interface(1)
         salinity_at_interface(k_top - k_bot + 1) = 2.0_dp * constituents(isalt, k_top) - salinity_at_interface(k_top - k_bot)
         temperature_at_interface(k_top - k_bot + 1) = 2.0_dp * constituents(itemp, k_top) - temperature_at_interface(k_top - k_bot)
      else
         salinity_at_interface(0) = constituents(isalt, k_bot)
         temperature_at_interface(0) = constituents(itemp, k_bot)
         salinity_at_interface(1) = salinity_at_interface(0)
         temperature_at_interface(1) = temperature_at_interface(0)
      end if

      pressure_down = 0.0_dp ! baroclinic pressure/ag
      pdb = 0.0_dp ! barotropic pressure/ag

      rhosww(k_top) = calculate_density(salinity_at_interface(k_top - k_bot + 1), temperature_at_interface(k_top - k_bot + 1)) - rhomean ! rho at interface

      do cell_index_3d = k_top, k_bot, -1
         layer_tichkness = zws(cell_index_3d) - zws(cell_index_3d - 1)
         pressure_up = pressure_down
         if (.not. apply_thermobaricity) then
            rhosww(cell_index_3d - 1) = calculate_density(salinity_at_interface(cell_index_3d - k_bot), temperature_at_interface(cell_index_3d - k_bot)) - rhomean
         else
            pdb = pdb + rhomean * layer_tichkness
            do i = 1, max_iterations_pressure_density
               pressure_down = pressure_up + 0.5_dp * (rhosww(cell_index_3d) + rhosww(cell_index_3d - 1)) * layer_tichkness ! start with previous step estimate
               p0d = ag * (pressure_down + pdb) ! total pressure
               rhosww(cell_index_3d - 1) = calculate_density(salinity_at_interface(cell_index_3d - k_bot), temperature_at_interface(cell_index_3d - k_bot), p0d) - rhomean
            end do
         end if
         rhosk = 0.5_dp * (rhosww(cell_index_3d) + rhosww(cell_index_3d - 1))
         rho(cell_index_3d) = rhomean + rhosk
         pressure_down = pressure_up + layer_tichkness * rhosk
         vertical_density_anomaly(cell_index_3d) = pressure_down
         baroclinic_pressure_term(cell_index_3d) = (pressure_up + 0.5_dp * layer_tichkness * (2.0_dp * rhosww(cell_index_3d) + rhosww(cell_index_3d - 1)) / 3.0_dp) * layer_tichkness ! wall contribution
      end do
   end subroutine add_baroclinic_pressure_cell_interface

   !> Computes baroclinic pressure gradients across layers for a horizontal cell.
   !! Cell density (i.e. rho(cell_index_3d)) is used
   subroutine add_baroclinic_pressure_cell_use_rho_directly(cell_index_2d)
      use m_turbulence, only: rho, baroclinic_pressure_term, vertical_density_anomaly
      use m_flowparameters, only: epshu
      use m_flow, only: zws
      use m_physcoef, only: rhomean
      use m_get_kbot_ktop, only: getkbotktop

      integer, intent(in) :: cell_index_2d !< horizontal cell index

      integer :: cell_index_3d, k_bot, k_top
      real(kind=dp) :: pressure_up, pressure_down, gr, layer_tichkness, layer_density_anomaly

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (zws(k_top) - zws(k_bot - 1) < epshu) then
         baroclinic_pressure_term(k_bot:k_top) = 0.0_dp
         vertical_density_anomaly(k_bot:k_top) = MIN_DENSITY_ANOMALY
         return
      end if

      baroclinic_pressure_term(k_top) = 0.0_dp
      vertical_density_anomaly(k_top) = 0.0_dp
      pressure_down = 0.0_dp
      do cell_index_3d = k_top, k_bot, -1
         layer_tichkness = zws(cell_index_3d) - zws(cell_index_3d - 1)
         layer_density_anomaly = (rho(cell_index_3d) - rhomean) * layer_tichkness
         pressure_up = pressure_down
         pressure_down = pressure_up + layer_density_anomaly
         vertical_density_anomaly(cell_index_3d) = pressure_down
         gr = pressure_up * layer_tichkness + 0.5_dp * (rho(cell_index_3d) - rhomean) * layer_tichkness * layer_tichkness ! your left  wall
         baroclinic_pressure_term(cell_index_3d) = gr
      end do
   end subroutine add_baroclinic_pressure_cell_use_rho_directly
end module m_add_baroclinic_pressure_cell
