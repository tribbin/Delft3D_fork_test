module m_rrerror

public rrError

contains
   subroutine rrError(errorLevel, message)
   
      use MessageHandling
      use globals
      
      implicit none
      
      integer, intent(in)          :: errorLevel
      character(len=*), intent(in) :: message
      
      if (errorLevel >= LEVEL_FATAL) then
         if (.not. in_f90_runner) then
            call THROWEXCEPTION()
         else
            stop   
         endif
      endif
      
   end subroutine rrError
end module m_rrerror

