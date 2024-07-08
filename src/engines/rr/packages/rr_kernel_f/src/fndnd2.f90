! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE FNDND2 (INDEX, ID)
! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0                  Date: March 1996
! *********************************************************************
! *** Last update:  March 1996       By : Geert Prinsen
! *********************************************************************
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    Zoek string ID in array ID_NOD;
! ***    Voor testen eerst alles in UPPERCASE zetten
! ***    Gevonden op positie INDEX. (-1 = not found)
! *********************************************************************
!
      USE CONF_FIL
      USE CONF_ARR
      use Network

      use Hash

      Integer index
      LOGICAL allownotfound

      CHARACTER(CharIdLength) ID

!
!new: use hashing

      INDEX = -1
      ALLOWNOTFOUND = .TRUE.
      index = Hashsearch (id, ALLOWNOTFOUND)

      RETURN
      END
