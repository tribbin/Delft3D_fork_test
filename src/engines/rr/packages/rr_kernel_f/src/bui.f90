      Subroutine Bui (RR_IEvent)


!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: March 2000
!*** Module  :
!*********************************************************************
!*** Created    : Maart 2000                      By : Geert Prinsen
!*********************************************************************
!*********************************************************************
!*** Subroutine to do calculations for event Ievent
!*********************************************************************

     use RRModule

     implicit none

! Simulatie van 1 bui, nodig voor Unix versie
! Voorlopig even met dummy RR_RunId

  Integer RR_Ievent, RR_RunId, RR_NTimestepEvent, RR_Timestep, ReturnCode

  RR_RunId = 1

  ReturnCode = RRInitializeEvent(RR_RunId, RR_Ievent,RR_NTimestepEvent)
  If (ReturnCode .ne. 0) goto 9999

  Do RR_Timestep=1,RR_NTimestepEvent
     ReturnCode = RRPerformTimestep (RR_RunId,RR_Ievent,RR_Timestep)
     If (ReturnCode .ne. 0) goto 9999
  Enddo

  Returncode = RRFinalizeEvent(RR_Runid,RR_Ievent)

  9999 Continue
  if (.not. dimr_mode .and. crashed) Call CrashCt(IdCnt, .false. )

  Return
  End Subroutine Bui

