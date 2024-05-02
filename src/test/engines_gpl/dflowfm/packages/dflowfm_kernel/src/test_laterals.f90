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
   use stdlib_kinds, only: dp
   use dfm_error, only: DFM_NOERR, DFM_GENERICERROR
   use m_lateral

   implicit none

   real(dp), parameter :: tolerance = 1.0e-10_dp

   contains
!
!
!==============================================================================
!   
subroutine tests_lateral
   call test( test_get_lateral_load_and_discharge, 'Test computation of sinks and sources due to laterals.')
end subroutine tests_lateral
!
!==============================================================================
!> Test computation of sinks and sources (discharge and transport load per cell) due to laterals
subroutine test_get_lateral_load_and_discharge
   use m_flow, only: vol1, hs
   use m_flowtimes, only: dts
   use m_partitioninfo, only: jampi
   use m_transportdata, only: numconst
   use m_cell_geometry, only: ba
   use m_flowgeom, only: ndxi
   
   real(kind=dp), allocatable, dimension(:,:)   :: lateral_discharge_in                !< Lateral discharge going into the model (source)
   real(kind=dp), allocatable, dimension(:,:)   :: lateral_discharge_out               !< Lateral discharge extracted out of the model (sink)
   real(kind=dp), allocatable, dimension(:,:)   :: reference_lateral_discharge_in      !< Reference lateral discharge going into the model (source)
   real(kind=dp), allocatable, dimension(:,:)   :: reference_lateral_discharge_out     !< Reference lateral discharge extracted out of the model (sink)
   real(kind=dp), allocatable, dimension(:,:)   :: transport_load                      !< Load being transported into domain
   real(kind=dp), allocatable, dimension(:,:)   :: transport_sink                      !< Load being transported out 
   
   integer :: ierr                            ! error flag
   integer :: i_cell, i_const, i_lateral, k1  ! loop counters

   ierr = 0
   numlatsg = 2
   numconst = 3
   ndxi = 10
   jampi = 0
   nlatnd = 5
   call initialize_lateraldata(numconst, ierr)
   allocate(lateral_discharge_in(numlatsg,ndxi),stat=ierr)
   allocate(lateral_discharge_out(numlatsg,ndxi),stat=ierr)
   allocate(reference_lateral_discharge_in(numlatsg,ndxi),stat=ierr)
   allocate(reference_lateral_discharge_out(numlatsg,ndxi),stat=ierr)
   allocate(n1latsg(numlatsg),stat=ierr)
   allocate(n2latsg(numlatsg),stat=ierr)
   allocate(nnlat(nlatnd),stat=ierr)
   allocate(qplat(numlatsg),stat=ierr)
   allocate(ba(ndxi),stat=ierr)
   allocate(balat(numlatsg),stat=ierr)
   allocate(hs(ndxi),stat=ierr)
   allocate(vol1(ndxi),stat=ierr)
   allocate(transport_load(numconst,ndxi),stat=ierr)
   allocate(transport_sink(numconst,ndxi),stat=ierr)
      
   n1latsg(1) = 1
   n2latsg(1) = 3
   n1latsg(2) = 4
   n2latsg(2) = 5
   nnlat = (/1,2,3,5,8/)
   ba = 1d0
   do i_lateral = 1,numlatsg
      balat(i_lateral) = 0d0
      do k1=n1latsg(i_lateral),n2latsg(i_lateral)
         i_cell = nnlat(k1)
         balat(i_lateral) = balat(i_lateral) + ba(i_cell)
      enddo
   enddo
   qplat = (/9d0,-10d0/)
   hs = 2d0
   vol1 = 1d0
   dts = 1d0
   
   reference_lateral_discharge_in = 0d0
   reference_lateral_discharge_in(1,nnlat(1)) = 3d0
   reference_lateral_discharge_in(1,nnlat(2)) = 3d0
   reference_lateral_discharge_in(1,nnlat(3)) = 3d0
   reference_lateral_discharge_out = 0d0
   reference_lateral_discharge_out(2,nnlat(4)) = 0.5d0
   reference_lateral_discharge_out(2,nnlat(5)) = 0.5d0
   
   call get_lateral_discharge(lateral_discharge_in,lateral_discharge_out)
   
   do i_lateral = 1,numlatsg
      do i_cell=1,ndxi
         call assert_comparable(lateral_discharge_in(i_lateral,i_cell), reference_lateral_discharge_in(i_lateral,i_cell), tolerance, "get_lateral_discharge() output lateral_discharge_in is not correct" )
         call assert_comparable(lateral_discharge_out(i_lateral,i_cell), reference_lateral_discharge_out(i_lateral,i_cell), tolerance, "get_lateral_discharge() output lateral_discharge_out is not correct" )         
      enddo
   enddo
   
   transport_load(:,:) = 0d0
   transport_sink(:,:) = 0d0
   incoming_lat_concentration(1,:,1) = (/31d0,20d0,0.23d0/)
   incoming_lat_concentration(1,:,2) = 25d0
   
   call add_lateral_load_and_sink(transport_load,transport_sink,lateral_discharge_in,lateral_discharge_out,vol1,tolerance)
   
   i_lateral = 1 ! only the first lateral is a source
   do i_const = 1,numconst
      do i_cell=1,ndxi
         call assert_comparable(transport_load(i_const,i_cell), incoming_lat_concentration(1,i_const,i_lateral) * reference_lateral_discharge_in(i_lateral,i_cell), tolerance, "get_lateral_load is not correct" )
      enddo
   enddo

   call dealloc_lateraldata()
   deallocate(lateral_discharge_in)
   deallocate(lateral_discharge_out)
   deallocate(reference_lateral_discharge_in)
   deallocate(reference_lateral_discharge_out)
   deallocate(n1latsg)
   deallocate(n2latsg)
   deallocate(nnlat)
   deallocate(qplat)
   deallocate(ba)
   deallocate(balat)
   deallocate(hs)
   deallocate(vol1)
   deallocate(transport_load)
 
end subroutine test_get_lateral_load_and_discharge
!
end module test_lateral
