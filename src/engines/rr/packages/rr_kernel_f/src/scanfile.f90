      Subroutine ScanFile (In, FileName, Iout1, IOpt, KeyWord, NrRecords, Minimaal1)

! *********************************************************************
! *** Sobek_RTC
! *** LAST UPDATE : March 2000          BY : Geert Prinsen
! **********************************************************************
! *** Precondition: file FileName exists
! *** Scan number of records with keyword KEYWORD in file FILENAME on unit IN
! *** Output: NrRecords (altijd >= 1)
! **********************************************************************

      implicit none
      
      Integer       In, IOut1, IOpt
      Character*(*) FileName
      Character*4   KeyWord, String
      Integer       NrRecords
      Logical       Eof, Minimaal1


      Eof = .false.
      NrRecords = 0

      Call OpenFl(In, FileName, Iout1, IOpt)
      Do While (.not. eof)
         Read (In,'(A4)',Err=999,End=999) String
         If (String(1:4) .eq. KeyWord(1:4)) NrRecords = NrRecords + 1
      Enddo
 999  Eof = .true.

      Close(IN)

      If (Minimaal1) NrRecords = max (1, NrRecords)

      RETURN
      END Subroutine ScanFile
