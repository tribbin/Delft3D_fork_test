! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE OPENFL (iunit, NAME, IForm, ICheck)

      Use Dio_plt_rw, prop_file_unused => prop_file

! *********************************************************************
! *** DELFT_3B model
! *** LAST UPDATE : March 1995          BY : Geert Prinsen
! **********************************************************************
! *** Check existence of inputfile
! *** if exists: Open file at unit IN with name NAME
!      iform = 1: open as ASCII file
!              2: open as unformatted file
!              3: open as Binary file
!      ICheck = 1 file should be existing (file status = old)
!               2 file status is unknown
! **********************************************************************

      use Messages
      use globals

      INTEGER        iunit, IForm, iCheck
      CHARACTER*(*)  NAME
      LOGICAL        FNMEXT
!
      INQUIRE (FILE = NAME, EXIST = FNMEXT)
      IF (.NOT. FNMEXT .and. Icheck .eq. 1) call ErrMsgStandard (904, 0, '  Openfl', NAME)

      iunit = 0

!     OPEN(IN, FILE=NAME, STATUS='OLD')
! extra MODE and SHARE option added; Microsoft specific!

#if defined(WIN32)
      if (icheck .eq. 1) then
         if (iform .eq. 1) then
           OPEN (newunit=iunit, FILE=Name, STATUS='Old',MODE='READ',SHARE='DENYWR')
         elseif (iform .eq. 2) then
           open (newunit=iunit, File=Name, Status='Old',mode='Read', Form='Unformatted')
         else
           open (newunit=iunit, File=Name, Status='Old',mode='Read', Form='Binary')
         endif
      else
         if (iform .eq. 1) then
            open (newunit=iunit, File=Name, Status='Unknown', ACTION='READWRITE', SHARE='DENYWR')
         elseif (iform .eq. 2) then
            open (newunit=iunit, File=Name, Status='Unknown', Form='Unformatted')
         elseif (iform .eq. 3) then
            open (newunit=iunit, File=Name, Status='Unknown', Form='Binary')
         endif
      endif
!
#elif defined(X64)
      if (icheck .eq. 1) then
         if (iform .eq. 1) then
            OPEN (newunit=iunit, FILE=Name, STATUS='Old',ACTION='READ')
         elseif (iform .eq. 2) then
           open (newunit=iunit, File=Name, Status='Old',mode='Read', Form='Unformatted')
         else
           open (newunit=iunit, File=Name, Status='Old',mode='Read', Form='Binary')
         endif
      else
         if (iform .eq. 1) then
            OPEN (newunit=iunit, FILE=Name, STATUS='Unknown',ACTION='READWRITE')
         elseif (iform .eq. 2) then
            open (newunit=iunit, File=Name, Status='Unknown', Form='Unformatted')
         elseif (iform .eq. 3) then
            open (newunit=iunit, File=Name, Status='Unknown', Form='Binary')
         endif
      endif
!
#else
      if (icheck .eq. 1) then
         if (iform .eq. 1) then
           OPEN (newunit=iunit, FILE=Name, STATUS='Old',ACTION='READ')
         elseif (iform .eq. 2) then
           open (newunit=iunit, File=Name, Status='Old',action='Read', Form='Unformatted')
         else
           open (newunit=iunit, File=Name, Status='Old',action='Read', Form='Unformatted')
         endif
      else
         if (iform .eq. 1) then
            open (newunit=iunit, File=Name, Status='Unknown')
         elseif (iform .eq. 2) then
            open (newunit=iunit, File=Name, Status='Unknown', Form='Unformatted')
         elseif (iform .eq. 3) then
            open (newunit=iunit, File=Name, Status='Unknown', Form='Unformatted')
         endif
      endif
!
#endif

!      write(*,*) 'Opening ', iunit, Name

      if (iunit .ne. 0) then
         maxFileUnitNumber = max(maxFileUnitNumber, iunit)
         minFileUnitNumber = min(minFileUnitNumber, iunit)
      endif

     Return
     END Subroutine OpenFl




      Subroutine CloseGP (Iunit)
      Integer iunit

!     Write (*,*) ' Closing file unit', iunit
      Close (Iunit)

      Return
      END subroutine CloseGP
