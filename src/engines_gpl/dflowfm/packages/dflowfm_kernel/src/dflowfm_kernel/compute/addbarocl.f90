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

module m_add_baroclinic_pressure_link
   use precision, only: dp

   implicit none

   private

   real(kind=dp), parameter :: MIN_LAYER_THICKNESS = 0.1_dp ! Minimum layer thickness for baroclinic pressure calculation

   public :: add_baroclinic_pressure_link, add_baroclinic_pressure_link_interface, add_baroclinic_pressure_link_use_rho_directly

contains

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Density is based on linear interpolation of density at vertical interfaces.
   subroutine add_baroclinic_pressure_link(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rho, rhou, vertical_density_anomaly, baroclinic_pressure_term
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop
      use m_flowparameters, only: jarhoxu
      use m_physcoef, only: rhomean

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d, k1, k2, k1t, k2t, cell_index_3d, k_top, kz, ktz, insigpart, morelayersleft
      real(kind=dp) :: baroclinic_pressure_gradient(kmxx), volume_averaged_density(kmxx), gr3
      real(kind=dp) :: vertical_density_anomaly1, rv2, gr1, gr2, layer_density_anomaly, grk, fzu, fzd, layer_tichkness, rhow0, rhow1

      baroclinic_pressure_gradient(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      insigpart = 0
      if (numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            insigpart = 1 ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         morelayersleft = 1
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         morelayersleft = 2
      else
         morelayersleft = 0
      end if

      do link_index_3d = l_top, l_bot, -1
         k1 = ln(1, link_index_3d)
         k1t = k1
         k2 = ln(2, link_index_3d)
         k2t = k2
         if (link_index_3d == l_top) then
            k1t = ktop(ln(1, link_index_2d))
            k2t = ktop(ln(2, link_index_2d))
         end if

         volume_averaged_density(link_index_3d - l_bot + 1) = 0.5_dp * ((zws(k1t) - zws(k1 - 1)) * rho(k1) + (zws(k2t) - zws(k2 - 1)) * rho(k2))
         if (jarhoxu > 0) then
            rhou(link_index_3d) = volume_averaged_density(link_index_3d - l_bot + 1) / (0.5_dp * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)))
         end if
         volume_averaged_density(link_index_3d - l_bot + 1) = volume_averaged_density(link_index_3d - l_bot + 1) * dx(link_index_2d)

         vertical_density_anomaly1 = vertical_density_anomaly(k1)
         rv2 = vertical_density_anomaly(k2)
         gr1 = baroclinic_pressure_term(k1)
         gr2 = baroclinic_pressure_term(k2)

         if (link_index_3d == l_bot .and. morelayersleft /= 0) then ! extrapolate at 'bed' layer of deepest side

            if (morelayersleft == 1) then ! cell_index_3d=deep side, kz=shallow side
               cell_index_3d = k1
               k_top = ktop(ln(1, link_index_2d))
               kz = k2
               ktz = ktop(ln(2, link_index_2d))
            else
               cell_index_3d = k2
               k_top = ktop(ln(2, link_index_2d))
               kz = k1
               ktz = ktop(ln(1, link_index_2d))
            end if

            if (ktz - kz > 0) then ! shallow side extrapolates, coeffs based on shallow side:
               fzu = (zws(kz + 1) - zws(kz)) / (zws(kz + 1) - zws(kz - 1))
               fzd = 1.0_dp - fzu
               rhow1 = fzu * rho(cell_index_3d + 1) + fzd * rho(cell_index_3d)
               rhow0 = 2.0_dp * rho(cell_index_3d) - rhow1
            else ! one layerr
               rhow1 = rho(cell_index_3d)
               rhow0 = rhow1
            end if

            rhow1 = rhow1 - rhomean
            rhow0 = rhow0 - rhomean
            if (insigpart == 0) then
               layer_tichkness = zws(kz) - zws(kz - 1) ! shallow side

               volume_averaged_density(1) = layer_tichkness * 0.5_dp * (rho(cell_index_3d) + rho(kz)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (rho(cell_index_3d) + rho(kz))
               end if

            else
               layer_tichkness = zws(cell_index_3d) - zws(cell_index_3d - 1) ! deep side
            end if

            layer_density_anomaly = vertical_density_anomaly(cell_index_3d + 1) + 0.5_dp * layer_tichkness * (rhow1 + rhow0)
            grk = (vertical_density_anomaly(cell_index_3d + 1) + 0.5_dp * layer_tichkness * (2.0_dp * rhow1 + rhow0) / 3.0_dp) * layer_tichkness

            if (morelayersleft == 1) then ! k1=deepest
               vertical_density_anomaly1 = layer_density_anomaly
               gr1 = grk
            else
               rv2 = layer_density_anomaly
               gr2 = grk
            end if

            if (insigpart == 0) then
               gr3 = 0.0_dp ! no skewness for zlay jump at bed
            else
               gr3 = 0.5_dp * (vertical_density_anomaly1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            gr3 = 0.5_dp * (vertical_density_anomaly1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         baroclinic_pressure_gradient(link_index_3d - l_bot + 1) = baroclinic_pressure_gradient(link_index_3d - l_bot + 1) + gr1 - gr2 + gr3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradient(link_index_3d - l_bot) = baroclinic_pressure_gradient(link_index_3d - l_bot) - gr3 ! ceiling of ff# downstairs neighbours
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradient, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Density is based on linear interpolation of recomputed density (from salinity, temperature (and pressure)) at vertical interfaces.
   subroutine add_baroclinic_pressure_link_interface(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rho, rhou, vertical_density_anomaly, baroclinic_pressure_term, rhosww
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop
      use m_flowparameters, only: jarhoxu
      use m_transport, only: ISALT, ITEMP, constituents
      use m_physcoef, only: rhomean, max_iterations_pressure_density, ag, apply_thermobaricity
      use m_density, only: calculate_density

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d, k1, k2, k1t, k2t, cell_index_3d, k_top, kz, ktz, insigpart, morelayersleft, i
      real(kind=dp) :: baroclinic_pressure_gradient(kmxx), volume_averaged_density(kmxx), gr3
      real(kind=dp) :: vertical_density_anomaly1, rv2, gr1, gr2, layer_density_anomaly, grk, salinity_at_interface0, salinity_at_interface1, temperature_at_interface0, temperature_at_interface1, fzu, fzd, layer_tichkness, rhow0, rhow1, pdb, p0d

      baroclinic_pressure_gradient(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      insigpart = 0
      if (numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            insigpart = 1 ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         morelayersleft = 1
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         morelayersleft = 2
      else
         morelayersleft = 0
      end if

      do link_index_3d = l_top, l_bot, -1
         k1 = ln(1, link_index_3d)
         k1t = k1
         k2 = ln(2, link_index_3d)
         k2t = k2
         if (link_index_3d == l_top) then
            k1t = ktop(ln(1, link_index_2d))
            k2t = ktop(ln(2, link_index_2d))
         end if

         volume_averaged_density(link_index_3d - l_bot + 1) = 0.5_dp * ((zws(k1t) - zws(k1 - 1)) * rho(k1) + (zws(k2t) - zws(k2 - 1)) * rho(k2))
         if (jarhoxu > 0) then
            rhou(link_index_3d) = volume_averaged_density(link_index_3d - l_bot + 1) / (0.5_dp * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)))
         end if
         volume_averaged_density(link_index_3d - l_bot + 1) = volume_averaged_density(link_index_3d - l_bot + 1) * dx(link_index_2d)

         vertical_density_anomaly1 = vertical_density_anomaly(k1)
         rv2 = vertical_density_anomaly(k2)
         gr1 = baroclinic_pressure_term(k1)
         gr2 = baroclinic_pressure_term(k2)

         if (link_index_3d == l_bot .and. morelayersleft /= 0) then ! extrapolate at 'bed' layer of deepest side

            if (morelayersleft == 1) then ! cell_index_3d=deep side, kz=shallow side
               cell_index_3d = k1
               k_top = ktop(ln(1, link_index_2d))
               kz = k2
               ktz = ktop(ln(2, link_index_2d))
            else
               cell_index_3d = k2
               k_top = ktop(ln(2, link_index_2d))
               kz = k1
               ktz = ktop(ln(1, link_index_2d))
            end if

            if (ktz - kz > 0) then ! shallow side extrapolates, coeffs based on shallow side:
               fzu = (zws(kz + 1) - zws(kz)) / (zws(kz + 1) - zws(kz - 1))
               fzd = 1.0_dp - fzu
               rhow1 = fzu * rho(cell_index_3d + 1) + fzd * rho(cell_index_3d)
               rhow0 = 2.0_dp * rho(cell_index_3d) - rhow1
            else ! one layer
               rhow1 = rho(cell_index_3d)
               rhow0 = rhow1
            end if

            rhow1 = rhow1 - rhomean
            rhow0 = rhow0 - rhomean
            if (insigpart == 0) then
               layer_tichkness = zws(kz) - zws(kz - 1) ! shallow side

               volume_averaged_density(1) = layer_tichkness * 0.5_dp * (rho(cell_index_3d) + rho(kz)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (rho(cell_index_3d) + rho(kz))
               end if

            else
               layer_tichkness = zws(cell_index_3d) - zws(cell_index_3d - 1) ! deep side
            end if

            salinity_at_interface1 = fzu * constituents(isalt, cell_index_3d + 1) + fzd * constituents(isalt, cell_index_3d)
            temperature_at_interface1 = fzu * constituents(itemp, cell_index_3d + 1) + fzd * constituents(itemp, cell_index_3d)
            salinity_at_interface0 = 2.0_dp * constituents(isalt, cell_index_3d) - salinity_at_interface1
            temperature_at_interface0 = 2.0_dp * constituents(itemp, cell_index_3d) - temperature_at_interface1

            if (.not. apply_thermobaricity) then
               rhow0 = calculate_density(salinity_at_interface0, temperature_at_interface0) - rhomean
            else
               pdb = (zws(ktz) - zws(kz - 1)) * rhomean
               layer_density_anomaly = vertical_density_anomaly(cell_index_3d + 1) + 0.5_dp * layer_tichkness * (rhosww(cell_index_3d) + rhosww(cell_index_3d - 1))
               do i = 1, max_iterations_pressure_density
                  p0d = ag * (layer_density_anomaly + pdb) ! total pressure
                  rhow0 = calculate_density(salinity_at_interface0, temperature_at_interface0, p0d) - rhomean
                  layer_density_anomaly = vertical_density_anomaly(cell_index_3d + 1) + 0.5_dp * layer_tichkness * (rhosww(cell_index_3d) + rhow0)
               end do
            end if

            layer_density_anomaly = vertical_density_anomaly(cell_index_3d + 1) + 0.5_dp * layer_tichkness * (rhosww(cell_index_3d) + rhow0)
            grk = (vertical_density_anomaly(cell_index_3d + 1) + 0.5_dp * layer_tichkness * (2.0_dp * rhosww(cell_index_3d) + rhow0) / 3.0_dp) * layer_tichkness

            if (morelayersleft == 1) then ! k1=deepest
               vertical_density_anomaly1 = layer_density_anomaly
               gr1 = grk
            else
               rv2 = layer_density_anomaly
               gr2 = grk
            end if

            if (insigpart == 0) then
               gr3 = 0.0_dp ! no skewness for zlay jump at bed
            else
               gr3 = 0.5_dp * (vertical_density_anomaly1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            gr3 = 0.5_dp * (vertical_density_anomaly1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         baroclinic_pressure_gradient(link_index_3d - l_bot + 1) = baroclinic_pressure_gradient(link_index_3d - l_bot + 1) + gr1 - gr2 + gr3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradient(link_index_3d - l_bot) = baroclinic_pressure_gradient(link_index_3d - l_bot) - gr3 ! ceiling of ff# downstairs neighbours
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradient, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link_interface

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Cell density (i.e. rho(cell_index_3d)) is used
   subroutine add_baroclinic_pressure_link_use_rho_directly(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rho, rhou, vertical_density_anomaly, baroclinic_pressure_term
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop
      use m_flowparameters, only: jarhoxu
      use m_physcoef, only: rhomean

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d, k1, k2, k1t, k2t, cell_index_3d, k_top, kz, ktz, insigpart, morelayersleft
      real(kind=dp) :: baroclinic_pressure_gradient(kmxx), volume_averaged_density(kmxx), gr3
      real(kind=dp) :: vertical_density_anomaly1, rv2, gr1, gr2, layer_density_anomaly, grk, layer_tichkness

      baroclinic_pressure_gradient(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      insigpart = 0
      if (numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            insigpart = 1 ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         morelayersleft = 1
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         morelayersleft = 2
      else
         morelayersleft = 0
      end if

      do link_index_3d = l_top, l_bot, -1
         k1 = ln(1, link_index_3d)
         k1t = k1
         k2 = ln(2, link_index_3d)
         k2t = k2
         if (link_index_3d == l_top) then
            k1t = ktop(ln(1, link_index_2d))
            k2t = ktop(ln(2, link_index_2d))
         end if

         volume_averaged_density(link_index_3d - l_bot + 1) = 0.5_dp * ((zws(k1t) - zws(k1 - 1)) * rho(k1) + (zws(k2t) - zws(k2 - 1)) * rho(k2))
         if (jarhoxu > 0) then
            rhou(link_index_3d) = volume_averaged_density(link_index_3d - l_bot + 1) / (0.5_dp * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)))
         end if
         volume_averaged_density(link_index_3d - l_bot + 1) = volume_averaged_density(link_index_3d - l_bot + 1) * dx(link_index_2d)

         vertical_density_anomaly1 = vertical_density_anomaly(k1)
         rv2 = vertical_density_anomaly(k2)
         gr1 = baroclinic_pressure_term(k1)
         gr2 = baroclinic_pressure_term(k2)

         if (link_index_3d == l_bot .and. morelayersleft /= 0) then ! extrapolate at 'bed' layer of deepest side

            if (morelayersleft == 1) then ! cell_index_3d=deep side, kz=shallow side
               cell_index_3d = k1
               k_top = ktop(ln(1, link_index_2d))
               kz = k2
               ktz = ktop(ln(2, link_index_2d))
            else
               cell_index_3d = k2
               k_top = ktop(ln(2, link_index_2d))
               kz = k1
               ktz = ktop(ln(1, link_index_2d))
            end if

            if (insigpart == 0) then
               layer_tichkness = zws(kz) - zws(kz - 1) ! shallow side

               volume_averaged_density(1) = layer_tichkness * 0.5_dp * (rho(cell_index_3d) + rho(kz)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (rho(cell_index_3d) + rho(kz))
               end if

            else
               layer_tichkness = zws(cell_index_3d) - zws(cell_index_3d - 1) ! deep side
            end if

            layer_density_anomaly = vertical_density_anomaly(cell_index_3d + 1) + layer_tichkness * (rho(cell_index_3d) - rhomean)
            grk = (vertical_density_anomaly(cell_index_3d + 1) + layer_tichkness * (rho(cell_index_3d) - rhomean)) * layer_tichkness

            if (morelayersleft == 1) then ! k1=deepest
               vertical_density_anomaly1 = layer_density_anomaly
               gr1 = grk
            else
               rv2 = layer_density_anomaly
               gr2 = grk
            end if

            if (insigpart == 0) then
               gr3 = 0.0_dp ! no skewness for zlay jump at bed
            else
               gr3 = 0.5_dp * (vertical_density_anomaly1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            gr3 = 0.5_dp * (vertical_density_anomaly1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         baroclinic_pressure_gradient(link_index_3d - l_bot + 1) = baroclinic_pressure_gradient(link_index_3d - l_bot + 1) + gr1 - gr2 + gr3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradient(link_index_3d - l_bot) = baroclinic_pressure_gradient(link_index_3d - l_bot) - gr3 ! ceiling of ff# downstairs neighbours
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradient, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link_use_rho_directly

   !> Applies time integration of baroclinic pressure gradients and updates the momentum terms.
   subroutine baroclinic_pressure_link_time_integration(baroclinic_pressure_gradient, volume_averaged_density, link_index_2d, l_bot, l_top)
      use m_flow, only: adve, kmxL
      use m_flowtimes, only: dts, dtprev
      use m_turbulence, only: kmxx, dpbdx0
      use m_physcoef, only: ag

      real(kind=dp), dimension(1:kmxx), intent(in) :: baroclinic_pressure_gradient !> Baroclinic pressure gradient for each layer
      real(kind=dp), dimension(1:kmxx), intent(in) :: volume_averaged_density !> Volume-averaged density for each layer

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d
      real(kind=dp) :: baroclinic_force, time_weight_factor


      time_weight_factor = 0.5_dp * dts / dtprev
      do link_index_3d = l_bot, l_top
         if (volume_averaged_density(link_index_3d - l_bot + 1) > 0.0_dp) then
            baroclinic_force = ag * baroclinic_pressure_gradient(link_index_3d - l_bot + 1) / volume_averaged_density(link_index_3d - l_bot + 1)
            if (dpbdx0(link_index_3d) /= 0.0_dp) then
               adve(link_index_3d) = adve(link_index_3d) - (1.0_dp + time_weight_factor) * baroclinic_force + time_weight_factor * dpbdx0(link_index_3d)
            else
               adve(link_index_3d) = adve(link_index_3d) - baroclinic_force
            end if
            dpbdx0(link_index_3d) = baroclinic_force
         end if
      end do

      do link_index_3d = l_top + 1, l_bot + kmxL(link_index_2d) - 1
         dpbdx0(link_index_3d) = 0.0_dp
      end do
   end subroutine baroclinic_pressure_link_time_integration
end module m_add_baroclinic_pressure_link
