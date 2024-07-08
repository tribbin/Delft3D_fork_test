module globals

   logical                             :: dll_mode  = .false.  ! Can be set back to false if old boundaries are used.
   logical                             :: dimr_mode = .false.  ! Will be set to true if running under DIMR
   
   double precision                    :: julStart
   
   integer                             :: maxFileUnitNumber = -214748300
   integer                             :: minFileUnitNumber = 214748300
   
   logical, public                     :: in_f90_runner = .false.   !< Set to true when running as rr.exe or from flow1d_runner
                                                                    !  (f90, can not handle exceptions)
   
   logical                             :: isMessLevelSet = .false.
   integer                             :: messLevelSet   = 0
   
   character(len=256)                  :: rr_version_string

end module globals
