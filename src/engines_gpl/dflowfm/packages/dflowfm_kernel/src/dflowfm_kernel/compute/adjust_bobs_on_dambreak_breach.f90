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

!> Calculate the links affected by the dam break and sets bobs accordingly
module m_adjust_bobs_on_dambreak_breach
   use precision, only: dp

   implicit none

   private

   integer, parameter, public :: DBW_SYMM = 1 !< symmetrical dambreak widening (limited width in case of asymmetric starting link placement)
   integer, parameter, public :: DBW_PROP = 2 !< dambreak wideining proportional to left/right dam length
   integer, parameter, public :: DBW_SYMM_ASYMM = 3 !< symmetrical dambreak widening until left/right runs out of space then continues one sided
   integer, public :: dambreakWidening = DBW_SYMM_ASYMM !< method for dambreak widening


end module m_adjust_bobs_on_dambreak_breach
