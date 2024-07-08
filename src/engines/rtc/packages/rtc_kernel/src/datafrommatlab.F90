      Function DataFromMatlab  (IDEBUG, IOUT1, ITMSTP, IflRtnRtc) result(RetVal)

! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      use OtherData, only: MATUSE,MATDBG
      use rtc_matlab_module, only: rtc_matlab_instance, receive_outputpar_values

      IMPLICIT NONE

      integer :: RetVal

      integer :: IDEBUG
      integer :: IOUT1
      integer :: IflRtnRtc
      integer :: ITMSTP
      
      integer :: istat
      integer :: unit = 22
      character(1024) :: errorline
      
      RetVal = 0

      if (MATDBG>0) write(IDEBUG,'(A)') 'Entering DataFromMatlab'

#if (defined(USE_MATLAB))
      if (MATUSE) then

         if (receive_outputpar_values(rtc_matlab_instance,IDEBUG,MATDBG)/=0) then
            ! An error occurred in MATLAB. Write error messages and stop.
            write(IOUT1,'(A)') 'An error occurred in MATLAB:'
            ! Copy the error message from the file that MATLAB wrote
            open(unit=unit, file='rtc_matlab_error.txt', status='old', action='read')
            do while (.true.)
               read(unit,'(A)',iostat=istat) errorline
               if (istat/=0) exit
               write(IOUT1,'(2A)') 'MATLAB> ',trim(errorline)
            enddo
            close(unit)
            call ErrMsg (954, 0, ' DataFromMatlab ', '[DUMMY]', IOUT1) ! writes "Error getting data from Matlab" to IOUT1
            RetVal = 954
         endif

      endif

#endif

      END Function DataFromMatlab
