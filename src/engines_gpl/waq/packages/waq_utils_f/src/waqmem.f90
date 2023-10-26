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

MODULE waqmem
use m_waq_type_definitions


!     Deltares Software Centre

!     Function            : This is the new memory allocation area.
!                           It is like a huge unnamed common block.
!                           This invites to include it in every routine and make unstructured
!                           use of everything everywhere
!                           Delwaq group should strictly maintain the policy to only include it
!                           in the dlwqn$ routines and have the remainder of the source code
!                           driven by its parameter lists.
!                           The allocation should be reserved to the routines dhmmja, dhmmra and
!                           dhmmca where also the total memory demand is printed

!     Created             : January 2010 by Leo Postma

!      wasteloads

   integer(kind=int_32),  pointer     :: iwstkind(:)      ! steers flow-concentration processing
   integer(kind=int_32)                  ::ftype   (50)     ! copy of filtype in delwaq2

!      general solvers

   real(kind=sp), allocatable ::surface (:)      ! horizontal surface
   real(kind=dp), allocatable ::volume0 (:)      ! begin volume of a time step
   real(kind=dp), allocatable ::volume1 (:)      ! end   volume of a time step
   real(kind=sp), allocatable ::mixlen  (:)      ! standard mixing flow m3/s
   real(kind=sp), allocatable ::wdrawal (:)      ! withdrawal term
   integer(kind=int_32), allocatable ::rowpnt  (:)      ! start of each row in the matrix (0:n)-array
   integer(kind=int_32), allocatable ::fmat    (:)      ! pointer from(iq) in matrix
   integer(kind=int_32), allocatable ::tmat    (:)      ! pointer to  (iq) in matrix
   integer(kind=int_32),  pointer     :: iexseg  (:,:)    ! zero if volume is explicit
   integer(kind=int_32),  pointer     :: iknmkv  (:,:)    ! time variable feature array (for drying/flooding)
   integer(kind=int_32), allocatable ::isegcol (:)      ! pointer from segment to top of column

!      solver  6, 7 and 10 only

   real(kind=sp), allocatable ::rhs     (:,:)    ! delmat right hand side

!      solver 11, 12, 13, 14 and 24 only

   real(kind=dp), allocatable ::arhs    (:,:)    ! right hand side vertically implicit schemes
   real(kind=dp), allocatable ::adiag   (:,:)    ! diagonal filled with volumes vertically implicit schemes
   real(kind=dp), allocatable ::acodia  (:,:)    ! workarray under codiagonal vertical transport
   real(kind=dp), allocatable ::bcodia  (:,:)    ! workarray upper codiagonal vertical transport

!      solver 15 and 16 only

   real(kind=dp), allocatable ::gm_rhs  (:,:)    ! gmres right hand side
   real(kind=dp), allocatable ::gm_sol  (:,:)    ! gmres solution
   real(kind=dp), allocatable ::gm_work (:,:)    ! gmres workspace
   real(kind=dp), allocatable ::gm_hess (:,:)    ! gmres Hessenberg matrix
   real(kind=dp), allocatable ::gm_amat (:,:)    ! gmres off-diagonal entries of matrix
   real(kind=dp), allocatable ::gm_diag (:,:)    ! gmres diagonal entries of matrix
   real(kind=dp), allocatable ::gm_diac (:,:)    ! gmres unscaled copy of diagonal entries
   real(kind=dp), allocatable ::gm_trid (:,:)    ! gmres tridiagonal matrix vertical

!      if regular grid is provided (for future incorporation PART)

   integer(kind=int_32), allocatable ::cellpnt (:)      ! backpointer from noseg to mnmaxk
   integer(kind=int_32), allocatable ::flowpnt (:)      ! backpointer from noq to 3*mnmaxk - mnmax
   real(kind=sp), allocatable ::cell_x  (:,:)    ! x-values at the corner points of the grid
   real(kind=sp), allocatable ::cell_y  (:,:)    ! y-values at the corner points of the grid

!      solver 21 and 22 only

   real(kind=sp), allocatable ::theta   (:,:)    ! theta per exchange per processor
   real(kind=sp), allocatable ::thetaseg(:,:)    ! theta per volume per processor
   real(kind=sp), allocatable ::flowtot (:,:)    ! flow per processor
   real(kind=sp), allocatable ::disptot (:,:)    ! dispersion per processor
   real(kind=sp), allocatable ::flux    (:,:)    ! flux corrections
   real(kind=sp), allocatable ::lim     (:,:)    ! limiter
   real(kind=sp), allocatable ::maxi    (:,:)
   real(kind=sp), allocatable ::mini    (:,:)
   real(kind=sp), allocatable ::l1      (:,:)
   real(kind=sp), allocatable ::l2      (:,:)
   real(kind=sp), allocatable ::m1      (:,:)
   real(kind=sp), allocatable ::m2      (:,:)
   real(kind=sp), allocatable ::n1      (:,:)
   real(kind=sp), allocatable ::n2      (:,:)

!      solver 24 only

   real(kind=dp), allocatable ::dwork   (:,:)    ! work array self adjusting step
   real(kind=dp), allocatable ::volint  (:)      ! interpolation array for volumes
   real(kind=dp), allocatable ::dconc2  (:,:)    ! first guess array concentrations
   integer(kind=int_32), allocatable ::ibas    (:)      ! administrative arrays for the self
   integer(kind=int_32), allocatable ::ibaf    (:)      ! adjusting time step procedure
   integer(kind=int_32), allocatable ::iords   (:)      ! id.
   integer(kind=int_32), allocatable ::iordf   (:)      ! id.
   integer(kind=int_32), allocatable ::nvert   (:,:)    ! id.
   integer(kind=int_32), allocatable ::ivert   (:)      ! id.

   contains

   subroutine waqmem_deallocate

      implicit none

!      wasteloads

       if (associated(iwstkind)) deallocate(iwstkind)

!      general solvers

       if (allocated(volume0)) deallocate(volume0)
       if (allocated(volume1)) deallocate(volume1)
       if (allocated(mixlen))  deallocate(mixlen)
       if (allocated(rowpnt))  deallocate(rowpnt)
       if (allocated(fmat))    deallocate(fmat)
       if (allocated(tmat))    deallocate(tmat)
       if (associated(iexseg))  deallocate(iexseg)
       if (associated(iknmkv))  deallocate(iknmkv)
       if (allocated(isegcol)) deallocate(isegcol)

!      solver  6, 7 and 10 only

       if (allocated(rhs)) deallocate(rhs)

!      solver 15 and 16 only

       if (allocated(gm_rhs))  deallocate(gm_rhs)
       if (allocated(gm_sol))  deallocate(gm_sol)
       if (allocated(gm_work)) deallocate(gm_work)
       if (allocated(gm_hess)) deallocate(gm_hess)
       if (allocated(gm_amat)) deallocate(gm_amat)
       if (allocated(gm_diag)) deallocate(gm_diag)
       if (allocated(gm_diac)) deallocate(gm_diac)
       if (allocated(gm_trid)) deallocate(gm_trid)

!      solver 21 only

       if (allocated(theta)) deallocate(theta)
       if (allocated(thetaseg)) deallocate(thetaseg)
       if (allocated(flowtot)) deallocate(flowtot)
       if (allocated(disptot)) deallocate(disptot)
       if (allocated(flux)) deallocate(flux)
       if (allocated(lim)) deallocate(lim)
       if (allocated(maxi)) deallocate(maxi)
       if (allocated(mini)) deallocate(mini)
       if (allocated(l1)) deallocate(l1)
       if (allocated(l2)) deallocate(l2)
       if (allocated(m1)) deallocate(m1)
       if (allocated(m2)) deallocate(m2)
       if (allocated(n1)) deallocate(n1)
       if (allocated(n2)) deallocate(n2)

   end subroutine waqmem_deallocate


END MODULE waqmem
