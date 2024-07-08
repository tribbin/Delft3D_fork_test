      SUBROUTINE INITAR (NAME, RESNOW, RESULTS, NLOC, NHIS, NTIMS, IDEBUG)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC  version 1.0.                   Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Initialise arrays (at -9999.)
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***   NAME   = naam/indicatie soort resultaten (Sobek/3B/etc.)
! ***   RESNOW = array with results current timestep
! ***   RESULTS= array with results all timesteps
! ***   NLOC   = max. number of locations
! ***   NHIS   = max. number of series from HIS file
! ***   NTIMS  = max. number of timesteps
! ***   IDEBUG = unit nr. van debugfile
! *********************************************************************
!
      CHARACTER*6 NAME
      INTEGER     NLOC, NHIS, NTIMS, IDEBUG
      Double Precision RESNOW(NLOC,NHIS), RESULTS (NLOC,NHIS,NTIMS)
      INTEGER     ILOC, IPAR, IT
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1) NAME, NLOC, NHIS, NTIMS
    1 FORMAT (' Initar result arrays ',A,3I4)
!
! *********************************************************************
! ** Initialisatie Current and Previous results
! ********************************************************************

      DO ILOC=1,NLOC
        DO IPAR=1,NHIS
          RESNOW(ILOC,IPAR) = -9999.
          DO IT=1,NTIMS
            RESULTS(ILOC,IPAR,IT) = -9999.
          ENDDO
        ENDDO
      ENDDO


      RETURN
      END
