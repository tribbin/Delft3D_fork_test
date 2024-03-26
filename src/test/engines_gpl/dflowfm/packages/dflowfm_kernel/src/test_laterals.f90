!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module test_lateral
   use ftnunit
   use precision, only: hp
   use dfm_error, only: DFM_NOERR, DFM_GENERICERROR
   use m_lateral

   implicit none

   real(hp), parameter :: tolerance = 1.0e-10_hp

   contains
!
!
!==============================================================================
!   
subroutine tests_lateral_loads
   call test( test_get_lateral_loads, 'Test computation of sinks and sources due to laterals.')
end subroutine tests_lateral_loads
!
!==============================================================================
!> tests computation of sinks and sources due to laterals
subroutine test_get_lateral_loads

   integer :: numconst                 !< number of constituents
   integer :: ierr                     !< error flag

   ierr = 0
   numlatsg = 1
   numconst = 2
   call initialize_lateraldata(numconst, ierr)

   call get_lateral_loads(ierr)
   call assert_equal(ierr, DFM_NOERR, 'Something wrong in call get_lateral_loads().')

   call dealloc_lateraldata()

end subroutine test_get_lateral_loads
!
end module test_lateral
