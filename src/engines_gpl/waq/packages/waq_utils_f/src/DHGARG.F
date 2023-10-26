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
      module m_dhgarg
      use m_waq_type_definitions


      implicit none

      contains


      SUBROUTINE DHGARG ( IARG  , LINE  )
!
!     Returns the IARG'th argument from the command line
!
      USE DHCOMMAND

      IMPLICIT NONE
!
!     Arguments
!
      INTEGER(kind=int_32) ::NARG
      INTEGER(kind=int_32) ::IARG
      CHARACTER*(*) LINE
!
!     Local
!
      INTEGER(kind=int_16) ::N, STATUS
      CHARACTER*256 BUFFER
      INTEGER(kind=int_32) ::COUNT
      INTEGER(kind=int_32) ::STORED
      LOGICAL       OPENED, EXISTS
      INTEGER(kind=int_32) ::IERR
      INTEGER(kind=int_32) ::LUN
      INTEGER(kind=int_32) ::I

!
!     Any stored arguments?
!
      STORED = DHSTORED_NUMBER_ARGS()
      IF ( STORED > 0 ) THEN
          LINE = DHSTORED_ARG(IARG)
      ELSE
!
!         Call system routine
!
          NARG = IARGC() + 1
          N = IARG
          CALL GETARG( N     , BUFFER)
!
!         Store result
!
          LINE = BUFFER

          !
          ! Read from file?
          !
          INQUIRE( FILE = 'delwaq.options', EXIST = EXISTS )
          IF ( EXISTS ) THEN
              IF ( NARG .GT. 0 ) THEN
                  COUNT = IARG - NARG
              ELSE
                  COUNT = IARG - 1
              ENDIF

              OPEN( NEWUNIT = LUN, FILE = 'delwaq.options' )
              DO
                  READ( LUN, '(A)', IOSTAT = IERR ) LINE
                  IF ( IERR .NE. 0 ) THEN
                      EXIT
                  ENDIF
                  IF ( LINE .NE. ' ' ) THEN
                      IF ( COUNT .EQ. 0 ) THEN
                          EXIT
                      ENDIF
                      COUNT = COUNT - 1
                  ENDIF
              ENDDO
              CLOSE( LUN )
          ENDIF
      ENDIF
!
      RETURN
      END
      end module m_dhgarg
