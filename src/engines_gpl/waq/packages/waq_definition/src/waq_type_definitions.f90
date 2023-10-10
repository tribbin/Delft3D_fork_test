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

module m_waq_type_definitions
!
!  module declarations
      use, intrinsic :: iso_fortran_env
      implicit none
      
      ! f77: integer*1, f90: integer(kind=1)
      integer, parameter :: int_8 = INT8
      ! f77: integer*2, f90: integer(kind=2)
      integer, parameter :: int_16 = INT16
      ! f77: integer*4, f90: integer(kind=4) this is the default value if not mentioned.
      integer, parameter :: int_32 = INT32
      ! f77: integer*8, f90: integer(kind=8)
      integer, parameter :: int_64 = INT64
      
      integer, parameter :: sp = REAL32
      integer, parameter :: dp = REAL64
      integer, parameter :: qp = REAL128
end module waq_precision
