! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 19-08-97 4:36p   $
!
! current revision: $Revision:: 4               $


      SUBROUTINE TestDate (ChYear, ChMonth, ChDay, RdYear, RdMonth, RdDay, IExit)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 2000
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Test if date read (RdYear .. RdDay) equal to Check Date (ChYear .. ChDay)
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  Iexit  = 90   read date before check date, so read next record
! ***  Iexit  = 999  read date equal to check date, so found
! ***  Iexit  = 915  read date after check date, so check date can not be found (stop error)
! *********************************************************************
!
      INTEGER        ChYear, ChMonth, ChDay
      INTEGER        RdYear, RdMonth, RdDay
      Integer        Iexit


      IF  (ChYear .GT. Rdyear) THEN
          Iexit = 90
      ELSEIF (ChYear .eq. Rdyear .AND. ChMonth .gt. Rdmonth) THEN
          IExit = 90
      ELSEIF (ChYear .eq. Rdyear .AND. ChMonth .eq. Rdmonth .AND. &
                                                ChDay .gt. Rdday) THEN
          IExit = 90
      ELSEIF (ChYear .eq. Rdyear .AND. ChMonth .eq. Rdmonth .AND. &
                                                ChDay .eq. Rdday) THEN
          IExit = 999
      ELSE
          IExit = 915
      ENDIF

      RETURN
      END
