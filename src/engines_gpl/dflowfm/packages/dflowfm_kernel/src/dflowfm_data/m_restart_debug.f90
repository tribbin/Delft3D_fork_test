!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

module m_restart_debug
 implicit none

 double precision, pointer      :: r_bodsed(:,:)   !< [kg m-2] Available sediment in the bed in flow cell center.            {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "ndx"], "internal": "stmpar%morlyr%state%bodsed"}
 double precision, pointer      :: r_dpsed(:)      !< [m] Sediment thickness in the bed in flow cell center.                 {"location": "face", "shape": ["ndx"], "internal": "stmpar%morlyr%state%dpsed"}
 double precision, pointer      :: r_msed(:,:,:)   !< [kg m-2] Available sediment in a layer of the bed in flow cell center. {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "stmpar%morlyr%settings%nlyr", "ndx"], "internal": "stmpar%morlyr%state%msed"}
 double precision, pointer      :: r_thlyr(:)      !< [m] Thickness of a layer of the bed in flow cell center.               {"location": "face", "shape": ["stmpar%morlyr%settings%nlyr","ndx"], "internal": "stmpar%morlyr%state%thlyr"}

 !type(sedtra_type), target         :: sedtra        !< All sediment-transport-morphology fields.
 !double precision      :: rsedeq(:,:)   !< [kg m-3] Equilibrium sediment concentration. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%rsedeq"}
 !double precision      :: sbcx(:,:)     !< [kg s-1 m-1] bed load transport due to currents, x-component.       {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbcx"}
 !double precision      :: sbcy(:,:)     !< [kg s-1 m-1] bed load transport due to currents, y-component.       {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbcy"}
 !double precision      :: sbwx(:,:)     !< [kg s-1 m-1] bed load transport due to waves, x-component.          {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbwx"}
 !double precision      :: sbwy(:,:)     !< [kg s-1 m-1] bed load transport due to waves, y-component.          {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbwy"}
 !double precision      :: sscx(:,:)     !< [kg s-1 m-1] suspended load transport due to currents, x-component. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sscx"}
 !double precision      :: sscy(:,:)     !< [kg s-1 m-1] suspended load transport due to currents, y-component. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sscy"}
 !double precision      :: sswx(:,:)     !< [kg s-1 m-1] suspended load transport due to waves, x-component.    {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sswx"}
 !double precision      :: sswy(:,:)     !< [kg s-1 m-1] suspended load transport due to waves, y-component.    {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sswy"}
 !double precision      :: taucr(:)      !< [kg s-2 m-1] dimensional critical shear stress taucr.               {"location": "face", "shape": ["stmpar%lsedtot"], "internal": "stmpar%sedpar%taucr"}
 !double precision      :: tetacr(:)     !< [-] dimensionless critical shear stress tetacr.                     {"location": "face", "shape": ["stmpar%lsedtot"], "internal": "stmpar%sedpar%tetacr"}

end module m_restart_debug
