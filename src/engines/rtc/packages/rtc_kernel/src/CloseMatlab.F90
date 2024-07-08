      subroutine CloseMatlab (IDEBUG)

      use OtherData, only : MATUSE, MATDBG
      use rtc_matlab_module, only: stop_matlab, rtc_matlab_instance

      IMPLICIT NONE
      
      integer IDEBUG

      integer stat

      if (MATDBG>0) write(IDEBUG,'(A)') 'Entering CloseMatlab'

#if (defined(USE_MATLAB))
      if (MATUSE) then
          stat = stop_matlab(rtc_matlab_instance,IDEBUG,MATDBG)
      endif
#endif

      end
