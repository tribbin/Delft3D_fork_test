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

!! Module DELWAQ2_DATA:
!! Define the derived type holding all the information
!!
      module delwaq2_data

      use m_waq_type_definitions
      use hydroset
      use dlwq_hyd_data
      use m_waq_data_buffer
      use m_operation_data
      use dlwqgrid_mod

      integer(kind=int_32), parameter, private                  ::iisize = 21  ! from sysi.inc
      integer(kind=int_32), parameter, private                  ::insize = 72  ! from sysn.inc

      type delwaq_data
          type(waq_data_buffer)                       :: buffer
          integer(kind=int_32), dimension(:) , pointer              :: iwstkind   ! steers flow-concentration processing
          integer(kind=int_32), dimension(:,:) , pointer            :: iexseg     ! zero if volume is explicit
          integer(kind=int_32), dimension(:,:) , pointer            :: iknmkv     ! time variable feature array (for drying/flooding)

          integer(kind=int_32), dimension(insize)                   ::in
          integer(kind=int_32), dimension(iisize)                   ::ii
          integer(kind=int_32) ::itime
          integer(kind=int_32) ::ifflag
          integer(kind=int_32) ::iaflag
          integer(kind=int_32) ::ibflag
          integer(kind=int_32) ::nddim
          integer(kind=int_32) ::nvdim
          integer(kind=int_32) ::nosss
          integer(kind=int_32) ::noqtt
          integer(kind=int_32) ::noqt
          integer(kind=int_32) ::nopred
          integer(kind=int_32) ::itimel
          integer(kind=int_32) ::ithandl = 0 ! needs to be zero at the start
          integer(kind=int_32) ::inwtyp
          integer(kind=int_32) ::nowarn
          integer(kind=int_32) ::ioptzb
          integer(kind=int_32) ::lleng
          logical                                     :: lstrec
          logical                                     :: forester
          logical                                     :: updatr
          logical                                     :: litrep
          logical                                     :: ldummy
          type(operation_data), dimension(:), pointer :: operation => null()
          integer(kind=int_32) ::number_operations = 0
          real(kind=dp)                       :: tol
          real(kind=dp)                       :: otime
          real(kind=dp)                       :: deltim
          real(kind=dp)                       :: tscale
          logical                                     :: set_timer = .false.
          logical                                     :: inopenda  = .false.

          !
          ! Components from syst.inc
          !
          logical                                     :: bndset
          logical                                     :: wstset
          logical                                     :: funset
          logical                                     :: othset
          integer(kind=int_32) ::ibndmx
          integer(kind=int_32) ::iwstmx
          integer(kind=int_32) ::ifunmx

          !
          ! Components for dealing with the time-dependent data from files
          !
          type(FilePropColl)          :: PropColl
          type(FileUseDefColl)        :: UseDefColl
          type(FileUseDefCollColl)    :: CollColl

          !
          ! Temporary component for dealing with OpenDA multiple instances
          !
          type(FilePropColl), pointer, dimension(:) :: PropCollArray => null()
          !
          ! All the process parameters data from file
          !
          type(t_dlwqdatacoll)     :: proc_pars

          !
          ! Collection of all grid definitions
          !
          type(GridPointerColl)    :: GridPs
      end type delwaq_data

      contains

      ! copy_time_data
      !     Routine to copy to and from the syst time data
      !
      subroutine copy_time_data( dlwqd, todlwqd )

      use m_syst

      type(delwaq_data), intent(inout) :: dlwqd
      logical                          :: todlwqd

      if ( todlwqd ) then
          dlwqd%bndset = bndset
          dlwqd%wstset = wstset
          dlwqd%funset = funset
          dlwqd%othset = othset
          dlwqd%ibndmx = ibndmx
          dlwqd%iwstmx = iwstmx
          dlwqd%ifunmx = ifunmx
      else
          bndset = dlwqd%bndset
          wstset = dlwqd%wstset
          funset = dlwqd%funset
          othset = dlwqd%othset
          ibndmx = dlwqd%ibndmx
          iwstmx = dlwqd%iwstmx
          ifunmx = dlwqd%ifunmx
      endif
      end subroutine copy_time_data

      end module delwaq2_data
