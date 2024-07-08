! Last changed 
! by:               $Author:: Schrier           $
! at:               $Modtime:: 15-08-97 11:26a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE SPLFIL (IN, STRING)

      use ReadLib
! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Spool input file to first record next event
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! ***  STRING = character string used in error messages
! *********************************************************************

      USE CONF_FIL
      USE CONF_ARR
      use Messages

!
      INTEGER       IN
      LOGICAL       ENDFIL
      CHARACTER*(*) STRING
      Integer iDebug
!
      iDebug = ConfFil_get_iDebug()
      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' SPLFIL')
!
! *********************************************************************
! *** spool file
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL, 'ODS ')
      IF (ENDFIL) call ErrMsgStandard (911, 0, '  Splfil', STRING)
!
      RETURN
      END
