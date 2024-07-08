      Subroutine CmpWindTablePrediction (Ievent, Itmstp, idebug, Iout1)

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use MeasureModule
      Use OtherData
      Use NewTables


      Integer Ievent, ItmStp, Idebug, Iout1

      Integer Iloc, Ipar,  IExt
      INTEGER SYEAR, SMO, SDAY, SHOUR, SMIN, SSEC
      DOUBLE PRECISION RSSEC

      Integer Rownr, TabelNr
      logical DateTimeOutsideTable, UseLastIndex

      type (Date) currentDate
      type (Time) currentTime

!
      If (IDEBUG .GT. 0) Write(IDEBUG,*) ' CmpWindTablePrediction'

! *********************************************************************
! *** Compute prediction wind
! *********************************************************************
!
      If (NWIND .GT. 0) Then
        If (USEW .AND. IMODEW .EQ. 0) Then
          If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Upd_wind timestep',ITMSTP,NWIND,NTIMHW
! Loop over External locations (wind) and related wind station
          Do IExt=1,NEXTD
            ILoc = WindLoc2Stat(IExt)
            If (IDEBUG .GT. 0) Write(Idebug,*) ' Ext and stat',IExt,ILoc
! wind direction
            If (ConsWD(ILoc)) Then
              Do IPAR=1,NTIMHW
                 RESEXT(IExt,IPAR) = VALCWD(ILoc)
              EndDo
              If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Constant wind direction', &
                                VALCWD(ILoc), (RESEXT(iExt,IPAR),IPAR=1,NTIMHW)
            Else
              SYEAR = IfYEAR
              SMO   = IfMO
              SDAY  = IfDAY
              SHOUR = IfHOUR
              SMIN  = IfMIN
              SSEC  = IfSEC
              RSSEC = SSEC
              UseLastIndex = .false.
              Do IPAR=1,NTIMHW
                If (IPAR .GT. 1) Then
                   Call NXTSTP (IDEBUG, SYEAR,SMO,SDAY,SHOUR, SMIN, SSEC, RSSEC, IDHR, IDMIN, RDSEC)
                   UseLastIndex = .true.
                EndIf
                currentDate%year   = SYear
                currentDate%month  = SMo
                currentDate%day    = SDay
                currentTime%hour   = SHour
                currentTime%minute = SMin
                currentTime%second = SSec
                TabelNr = WindTable(Iloc,2)
                RowNr = -1
                ResExt(Iext,Ipar) = GetNewValue(RTCTableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                                Idebug, iout1, DateTimeOutsideTable, UseLastIndex)
                If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Current Day/Hour/Min, direction',Sday,Shour,Smin,RESEXT(IExt,IPAR)
              EndDo
              If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Wind Direction ResExt', (RESEXT(IExt,IPAR),IPAR=1,NTIMHW)
           EndIf
! wind velocity
           If (ConsWV(Iloc)) Then
              Do IPAR=1,NTIMHW
                 RESEXT(IExt,NTIMHW+IPAR) = VALCWV(ILoc)
              EndDo
              If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Constand Wind velocity',  &
                           VALCWV(Iloc), (RESEXT(IExt,NTIMHW+IPAR),IPAR=1,NTIMHW)
           Else
              SYEAR = IfYEAR
              SMO   = IfMO
              SDAY  = IfDAY
              SHOUR = IfHOUR
              SMIN  = IfMIN
              SSEC  = IfSEC
              RSSEC = SSEC
              UseLastIndex = .false.
              Do IPAR=1,NTIMHW
                If (IPAR .GT. 1) Then
                   Call NXTSTP (IDEBUG, SYEAR,SMO,SDAY,SHOUR, SMIN, SSEC, RSSEC, IDHR, IDMIN, RDSEC)
                   UseLastIndex = .true.
                EndIf
                currentDate%year   = SYear
                currentDate%month  = SMo
                currentDate%day    = SDay
                currentTime%hour   = SHour
                currentTime%minute = SMin
                currentTime%second = SSec
                RowNr = -1
                TabelNr = WindTable(Iloc,1)
                ResExt(iext,nTimHw+Ipar) = GetNewValue(RTCTableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                                       Idebug, iout1,DateTimeOutsideTable, UseLastIndex)
                If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Current Day/Hour/Min, velocity ',Sday,Shour,Smin,RESEXT(IExt,NTimHw+IPAR)
              EndDo
              If (IDEBUG .GT. 0) Write(IDEBUG,*) ' Wind Velocity ResExt', (RESEXT(IExt,NTIMHW+IPAR),IPAR=1,NTIMHW)
            EndIf
          EndDo
        EndIf
      EndIf

      Return
      END
