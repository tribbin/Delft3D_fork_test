      SUBROUTINE RTC_WRLOGO (Iscren, ISTEP, ISTOP, NEVENT, Ibar0, SBKMODE)

! *********************************************************************
! *** WRLOGO voor WINDOWS
! **********************************************************************
! *** Parameters :
! ***
! *** Name       Type   Size      In/Out    Description
! *** ------     ------ -------   ------    ----------------------------
! *** ISTEP      INT              IN        PRESENT STEP IN RUNNING
! *** ISTOP      INT              IN        MAX.NUMBER OF STEPS
! *** NEVENT     INT              IN        MAX.NUMBER OF EVENTS
! *** SOBEKMODE  LOGICAL          IN        Flag for SOBEKMODE
! **********************************************************************

      use rtc_version_module
   
      implicit none

      CHARACTER  BANNER*60, BAR*60
      CHARACTER  VERSTR*20
      INTEGER    IScren, ISTEP, ISTOP, INUM1, INUM2, IBAR, NEVENT,IBar0
      LOGICAL    SBKMODE
!
      DATA BANNER /'------------------------------------------------------------'/
      DATA BAR    /'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'/
!
!

      IF (ISTEP .EQ. 0 .AND. ISTOP .EQ. 0) THEN
! schrijf header
       call getversionnumberstring_RTC(VERSTR)
       WRITE(IScren,1) trim(VERSTR)
    1  FORMAT(//,                                                       &
        10X,' ',58(' '),' ',/,                                          &
        10X,'    Simulation started .....',26X,' ',/,                   &
        10X,' ',58(' '),' ',/,                                          &
        /,                                                              &
        10X,' ',58(' '),' ',/,                                          &
        10X,'    Copyright (c) 2017     ',9X,                           &
                                      'DELTARES         ',4X,' ',/,   &
        10X,'    RTC-module           ',11X,                            &
                                      'Version ',A,4X,' ',/,  &
        10X,' ',58(' '),' ',/)
!
      ELSEIF (ISTEP .EQ. 0 .AND. ISTOP .GT. 0) THEN
        IF (SBKMODE) THEN
          INUM1 = NINT ( 0.5 * FLOAT(ISTOP))
          INUM2 = NINT (       FLOAT(ISTOP))
        ELSE
          INUM1 = NINT ( 0.25 * FLOAT(ISTOP))
          INUM2 = NINT ( 0.5  * FLOAT(ISTOP))
        ENDIF

        IF (NEVENT .EQ. 1) THEN
          WRITE (IScren, 200)
200       FORMAT (/,10X,'Timestep :')
          WRITE (IScren, 201 ) INUM1, INUM2
201       FORMAT (10X,'0',22X,I7,23X,I7)
          WRITE (IScren, 202 ) BANNER
202       FORMAT (10X,A60 )
        ELSE
          WRITE (IScren, 206 )
206       FORMAT (/,10X,'Event :')
          WRITE (IScren, 201 ) INUM1, INUM2
          WRITE (IScren, 202 ) BANNER
        ENDIF
! Digital
!!      WRITE (*,'(''          '',\)')
      ELSE
! laat balk voortschrijden
        IBAR = INT ( 60 * FLOAT(ISTEP)/ISTOP )
        IBAR0 = max ( 0, IBar0)
        IBAR0 = min ( 60, IBar0)
        IF ( IBAR.GT. IBar0 .AND. IBAR .LE. 60)  WRITE(IScren,'(''+         '',A)') BAR(1:IBAR)
! Powerstation     WRITE(*,'(''+         '',A)') BAR(1:IBAR)
! Digital
!     *             WRITE(*,'(A,\)')  BAR(IBar0+1:IBAR)
        IBar0 = IBar
      ENDIF
!
!
      RETURN
      END
