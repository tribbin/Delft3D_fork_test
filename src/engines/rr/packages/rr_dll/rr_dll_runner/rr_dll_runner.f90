module m_rr_dll_runner

   public run
   
contains
   
subroutine run(conf_file_name)

   use iso_c_binding

   character(len=*), intent(in) :: conf_file_name       ! sobek_3b.fnm

   character(kind=C_CHAR) :: c_configfile(256)          ! iso c version of file name

   logical(C_INT), external :: initialize, finalize
   integer(C_INT), external :: update
   integer(C_INT)           :: ret_val

   double precision :: base_time
   double precision :: start_time
   double precision :: end_time
   double precision :: current_time
   double precision :: deltaTime

   c_configfile = string_to_char_array(conf_file_name, LEN_TRIM(conf_file_name))

   call disable_exceptions()

   ret_val = initialize(c_configfile)
   if (ret_val < 0) then
      stop 'error in initialize'
   endif
   
   call get_start_time(start_time)
   base_time = start_time
   
   call get_end_time(end_time)
      
   call get_current_time(current_time)
   
   end_time = end_time - base_time
   start_time = 0.0d0
 
   do while (current_time < (end_time - 1.0d-4))
      deltaTime = 0.0d0
      ret_val = update(deltaTime)
      if (ret_val < 0) then
         stop 'error in update'
      endif
      call get_current_time(current_time)
   enddo
   
   ret_val = finalize()

end subroutine run

pure function string_to_char_array(string, length) result(char_array)
    use iso_c_binding

    ! pass only trimmed strings to this one
    integer         , intent(in) :: length
    character(len=*), intent(in) :: string
    character(kind=C_CHAR,len=1) :: char_array(256)
    integer :: i
    do i = 1, length
       char_array(i) = string(i:i)
    enddo
    char_array(length+1) = C_NULL_CHAR
  end function string_to_char_array

end module m_rr_dll_runner
   
   
program main

   use m_rr_dll_runner

   integer                :: num_arg              ! #program arguments
   character(Len=256)     :: conf_file_name       ! md1d file name (fortran)

   num_arg = COMMAND_ARGUMENT_COUNT()
   if (num_arg == 1) then
      call GET_COMMAND_ARGUMENT(1, conf_file_name)
   else
      stop 'usage: rr_dll_runner <SOBEK_3B.FNM-file>'
   endif

   call run(conf_file_name)

end program main


