      function OpenMatlab (IDEBUG) result(RetVal)

      use OtherData, only : MATUSE, MATDIR, MATFIL, MATDBG, MATEXE
      use system_utils, only: FILESEP
      use rtc_matlab_module, only : rtc_matlab_instance, start_matlab

      IMPLICIT NONE
      !
      integer :: IDEBUG
      integer :: Retval
      integer :: iud
      !
      integer        :: istat
      logical        :: error
      character(256) :: pathd
      character(8)   :: date
      character(10)  :: time
      character(256) :: diofolder
      character(256) :: rtcfolder
      character(256) :: userscript
      character(256) :: channel
          
      RetVal = 0 

      if (MATDBG>0) write(IDEBUG,'(A)') 'Entering OpenMatlab'

#if (defined(USE_MATLAB))
      if (MATUSE) then
         !
         ! provide attach opportunity for debugging ...
         !
!10       CALL GPSLEEP (1)
!         open(newunit=iud,file='d:\waitfile.txt',err=10,status='OLD')
!         close(iud,status='DELETE')
         !
         call getexedir (error, pathd)
         diofolder  = trim(pathd)//'matlab'//FILESEP//'delftio'//FILESEP//'progsrc'
         rtcfolder  = trim(pathd)//'matlab'//FILESEP//'rtc'//FILESEP//'progsrc'
         userscript = trim(MATDIR)//FILESEP//MATFIL
         !
         ! create unique channel ID for shared memory communication with MATLAB
         !
         call DATE_AND_TIME(date,time)
         channel    = date//time
         !
         RetVal = start_matlab(rtc_matlab_instance, rtcfolder, diofolder, userscript, MATEXE, channel, IDEBUG, MATDBG)
         !
      endif
#endif

      end function OpenMatlab
