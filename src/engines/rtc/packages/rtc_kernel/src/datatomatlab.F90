      function DataToMatlab (IDEBUG, IDUM , IOUT1, IflRtnRtc) result(RetVal)

! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! ***  IDUM   = time step number, note: idum-timeshift>=1.
! *********************************************************************
      use OtherData, only: MATUSE, MATDBG
      use ParameterModule, only: CharIdLength
      use m_alloc, only: realloc
      use rtc_matlab_module, only: rtc_matlab_instance, send_inputpar_names, send_outputpar_names, send_inputpar_values

      IMPLICIT NONE

      integer :: IDEBUG
      integer :: IDUM
      integer :: IOUT1
      integer :: IflRtnRtc
      integer :: RetVal

      if (MATDBG>0) write(IDEBUG,'(A)') 'Entering DataToMatlab'

      RetVal = 0

#if (defined(USE_MATLAB))
      if (MATUSE) then
         if (IDUM==1) then
            call send_inputpar_names(rtc_matlab_instance,IDEBUG,MATDBG)
            call send_outputpar_names(rtc_matlab_instance,IDEBUG,MATDBG)
         endif
         call send_inputpar_values(rtc_matlab_instance,IDUM,IDEBUG,MATDBG)
      endif
#endif

      end function DataToMatlab
