      SUBROUTINE NXTSTP (IDEBUG, IYEAR, IMO, IDAY, IHOUR, IMIN, ISEC, RSEC, IDHR, IDMIN, RDSEC)

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
! ***   Zet nieuwe waarden tijdstap
! *********************************************************************
!
      INTEGER IDEBUG, IYEAR, IMO, IDAY, IHOUR, IMIN, IDHR, IDMIN, ISEC
      DOUBLE PRECISION RSEC, RDSEC
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' NXTSTP')
!
      IHOUR = IHOUR + IDHR
      IMIN  = IMIN  + IDMIN
      RSEC  = RSEC  + RDSEC
      IF (RSEC .GE. 60D0) THEN
         RSEC = RSEC - 60D0
         IMIN = IMIN + 1
      ENDIF
      ISEC = INT(RSEC)
      IF (IMIN .GE. 60) THEN
         IMIN = IMIN - 60
         IHOUR= IHOUR + 1
      ENDIF
      IF (IHOUR .GE. 24) THEN
         IHOUR = IHOUR - 24
         CALL NXTDAY (IDEBUG, IYEAR, IMO, IDAY)
      ENDIF
!
      RETURN
      END
