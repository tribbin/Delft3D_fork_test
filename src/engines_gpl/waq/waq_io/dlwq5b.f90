!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_dlwq5b

    use timers
    implicit none

    interface shift_subarray
    module procedure shift_int_subarray, shift_char_subarray
    end interface


    contains


    subroutine dlwq5b(lunut, iposr , npos , cchar , car    ,&
                      iar  , icmax , iimax, aname , atype  ,&
                      ntitm, nttype, noitm, noits , chkflg ,&
                      caller, ilun  , lch  , lstack, itype  ,&
                      rar  , nconst, itmnr, parsed_str , ioutpt, &
                      error_ind , iwar)

    use m_string_utils, only: index_in_array, join_strings
    use m_movint
    use m_movchr
    !use timers

!   Arguments
    integer, intent(in   )       :: icmax        !< Max. Char workspace dimension
    integer, intent(in   )       :: iimax        !< Max. Int. Workspace dimension
    integer, intent(in   )       :: chkflg       !< Check on input or add items
    integer, intent(in   )       :: lunut        !< Unit Formatted Output File
    integer, intent(inout)       :: iposr        !< Start position on input line
    integer, intent(in   )       :: npos         !< Nr of significant characters
    integer, intent(  out)       :: iar(:)       !< Integer workspace
    integer, intent(inout)       :: ntitm        !< Number of bounds/wastes
    integer, intent(in   )       :: nttype       !< Number of bound/waste types
    integer, intent(  out)       :: noitm        !< Number of items read
    integer, intent(  out)       :: noits        !< Number of items for scale
    integer, intent(inout)       :: ilun(lstack) !< Unitnumb include stack
    integer, intent(in   )       :: lstack       !< Include file stack size
    integer, intent(  out)       :: itype        !< Type of the token read ('at exit')
    integer, intent(  out)       :: nconst       !< Number of values in rar
    integer, intent(in   )       :: ioutpt       !< Output file option
    integer, intent(  out)       :: error_ind    !< Error indicator
    integer, intent(  out)       :: iwar         !< Cumulative warning count

    real, intent(  out)          :: rar(:)       !< Array with real values

    character(1),  intent(in   ) :: cchar        !< Comment character
    character(*),  intent(  out) :: car(*)       !< Character workspace
    character(*),  intent(inout) :: aname(*)     !< Id's of the boundaries/wastes
    character(*),  intent(in   ) :: atype(*)     !< Types of the boundaries/wastes
    character(10), intent(in   ) :: caller       !< Calling subject
    character(*),  intent(inout) :: lch(lstack)  !< File name stack, 4 deep
    character(*),  intent(  out) :: parsed_str   !< Input string at end of routine

!   Local variables
    logical    :: usefor_on, substitution_on, can_compute, operator_on
    logical    :: must_log
    logical    :: show_must_go_on
    integer(4) :: ithndl = 0
    integer    :: i, parsed_int, ifound, i2, name_index, icm
    integer    :: itmnr, ioff, ioffc, ioffi
    real       :: parsed_real
    character(*), parameter :: operations(6) = ['*', '/', '+', '-', 'MIN', 'MAX']
    character(*), parameter :: keywords(24) = ['BLOCK'        ,&
                                              'BINARY_FILE'   ,&
                                              'CONCENTRATIONS',&
                                              'CONSTANTS'     ,&
                                              'DATA'          ,&
                                              'DATA_ITEM'     ,&
                                              'LINEAR'        ,&
                                              'ITEM'          ,&
                                              'IDENTICALITEM' ,&
                                              'USEDATA_ITEM'  ,&
                                              'FORITEM'       ,&
                                              'TIME_DELAY'    ,&
                                              'ODS_FILE'      ,&
                                              'ABSOLUTE'      ,&
                                              'TIME'          ,&
                                              'HARMONICS'     ,&
                                              'FOURIERS'      ,&
                                              'SCALE'         ,&
                                              'DEFAULTS'      ,&
                                              'ALL'           ,&
                                              'SEGMENTS'      ,&
                                              'PARAMETERS'    ,&
                                              'FUNCTIONS'     ,&
                                              'SEG_FUNCTIONS'  ]


    if (timon) call timstrt("dlwq5b", ithndl)

    !
    ! some initialisations
    usefor_on = .false.
    substitution_on = .false.
    can_compute = .false.
    operator_on = .false.
    show_must_go_on = .true.
    must_log = (ioutpt >= 3)
    noitm  = 0
    noits  = 0
    itmnr  = 0
    ioff   = 0
    ioffc  = 0
    ioffi  = 0
    nconst = 0
    !
    ! Get a token string (and return if any error was found)
    do
        itype = -3
        if (operator_on .or. (usefor_on .and. substitution_on)) itype = 0
        call rdtok1(lunut, ilun, lch  , lstack, cchar,&
                    iposr, npos, parsed_str, parsed_int , parsed_real,&
                    itype, error_ind)
        if (error_ind .ne. 0) then
            call finish(ithndl)
            return
        end if

        scenarios : block
            ! Scenario: type==1 and a keyword was met
            if (abs(itype) == 1 .and. (any(keywords == trim(parsed_str)))) then
                if (usefor_on) then
                    write (lunut, 1035) parsed_str
                    call error_and_finish(error_ind, ithndl)
                    return
                else
                    call finish(ithndl)
                    return
                end if
            end if
        
            ! Scenario: type==1 and computation was met
            if (abs(itype) == 1 .and. (any(operations == trim(parsed_str)))) then
                if (.not. can_compute) then
                    write (lunut , 1070)
                    call error_and_finish(error_ind, ithndl)
                    return
                end if
                if (operator_on) then
                    write (lunut , '(A)') ' ERROR: arithmetics should be separated by items !'
                    call error_and_finish(error_ind, ithndl)
                    return
                end if
                noitm = noitm + 1
                noits = noits + 1
                call movint(iar   , itmnr + noitm , itmnr + noitm * 2)
                iar(itmnr + noitm + noitm) = 0
                select case(parsed_str)
                    case ('*')
                        iar(itmnr + noitm) = -1000000
                    case ('/')
                        iar(itmnr + noitm) = -10000000
                    case ('+')
                        iar(itmnr + noitm) = -100000000
                    case ('-')
                        iar(itmnr + noitm) = -1000000000
                    case ('MIN')
                        iar(itmnr + noitm) = -1100000000
                    case ('MAX')
                        iar(itmnr + noitm) = -1200000000
                end select
                operator_on = .true.
                !goto 10
                exit scenarios
            end if
        
            ! Scenario: an item used in computations
            if (abs(itype) == 1 .and. operator_on) then
                do i=1, itmnr-1
                    if (iar(i) == -1300000000) cycle
                    ifound = index_in_array(parsed_str, car(i+ioff:i+ioff))
                    if (ifound == 1) then
                        noits = noits - 1
                        i2 = iar(itmnr + noitm)
                        call log_item_number_name(i2, lunut, i, parsed_str)
                        iar(itmnr + noitm) = i2 + i
                        car(itmnr + noitm + ioff) = '&$&$SYSTEM_NAME&$&$!'
                        operator_on = .false.
                        ! goto 10
                        exit scenarios
                    end if
                end do
                i2 = iar(itmnr + noitm)
                call log_local_substitution(i2, lunut, parsed_str)
                iar (itmnr + noitm + noitm) = noits
                car (itmnr + noitm + ioff) = parsed_str
                operator_on = .false.
                ! goto 10
                exit scenarios
            end if
        
            ! Scenario: a number (int, 2, or real, 3) is used in computations
            if (  (abs(itype) == 2 .or. abs(itype) == 3) .and. &
                  (substitution_on .or. operator_on) ) then
                nconst = nconst + 1
                rar(nconst) = parsed_real
                noits = noits - 1
                i2 = iar(itmnr + noitm)
                car(itmnr + noitm + ioff) = '&$&$SYSTEM_NAME&$&$!'
                if (operator_on) then
                    if (must_log) then
                        call log_number_in_operation(i2, lunut, parsed_real)
                    end if
                    iar(itmnr + noitm) = i2 - nconst
                    operator_on = .false.
                end if
                if (substitution_on) then
                    name_index = iar(itmnr)
                    if (must_log) then
                        call log_name_substitution(name_index, lunut, aname, caller, itmnr, atype, parsed_real, parsed_str, .true.)
                    end if
                    iar(itmnr + noitm) =  -nconst
                    iar(itmnr + noitm + noitm) = 0
                    usefor_on       = .false.
                    substitution_on = .false.
                    can_compute     = .true.
                end if
                !goto 10
                exit scenarios
            end if
        
            ! Scenario: a local redirection of the name of an item or substance
            if (abs(itype) == 1 .and. parsed_str == 'USEFOR') then
                if (usefor_on) then
                    write (lunut , 1035) parsed_str
                    call error_and_finish(error_ind, ithndl)
                    return
                else
                    usefor_on = .true.
                    substitution_on = .false.
                    !goto 10
                    exit scenarios
                end if
            end if

            ! Scenario:
            if (itype == 1) then
                if (usefor_on .and. substitution_on) then
                    name_index = iar(itmnr)
                    if (must_log) then
                        call log_name_substitution(name_index, lunut, aname, caller, itmnr, atype, parsed_real, parsed_str, .false.)
                    end if
                    iar(itmnr + noitm + noitm) = noits
                    car(itmnr + noitm + ioff) = parsed_str
                    usefor_on = .false.
                    substitution_on = .false.
                    ! it is now possible to compute
                    can_compute = .true.
                    !goto 10
                    exit scenarios
                end if
                ! fill in a string value if an empty string is provided
                if (chkflg == -1 .and. parsed_str(1:20) == repeat(' ', 20)) then
                    parsed_str = 'Item-'
                    write (parsed_str(6:12) , '(I7)') noitm+1
                end if
                ! FLOW is only valid as CONCENTR. and item number is 0
                ifound = index_in_array(parsed_str, ['FLOW                '])
                if (ifound == 1 .and. caller == 'CONCENTR. ') then
                    call update_counters(noitm, noits, itmnr)
                    icm = itmnr + noitm + ioff
                    call movint(iar, itmnr      , itmnr + noitm*2)
                    call movint(iar, itmnr + noitm, itmnr + noitm*2)
                    call movchr(car, itmnr+ioff , icm)
                    iar (itmnr) =  0
                    iar (itmnr + noitm) = itmnr
                    iar (itmnr + noitm + noitm) = noits
                    car (itmnr + ioff) = parsed_str
                    car (itmnr + noitm + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (must_log .and. .not. usefor_on) then
                        write (lunut , 1020) caller , itmnr , caller , 0 , 'FLOW'
                    end if
                    !goto 10
                    exit scenarios
                end if
            
                ! parsed_str == item-NAME
                ifound = index_in_array(parsed_str(1:len(aname(1))), aname(1:ntitm))
                if (ifound >= 1) then
                    call update_counters(noitm, noits, itmnr)
                    icm = itmnr + noitm + ioff
                    call movint(iar, itmnr        , itmnr + noitm*2)
                    call movint(iar, itmnr + noitm, itmnr + noitm*2)
                    call movchr(car, itmnr+ioff , icm)
                    iar(itmnr) =  ifound
                    iar(itmnr + noitm) = itmnr
                    iar(itmnr + noitm + noitm) = noits
                    car(itmnr + ioff) = parsed_str
                    car(itmnr + noitm + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (must_log .and. .not. usefor_on) then
                        write (lunut , 1020) caller, itmnr, caller, ifound, aname(ifound)
                    end if
                    !goto 10
                    exit scenarios
                end if
            
                ! parsed_str == item-TYPE. IAR now is negative.
                ifound = index_in_array(parsed_str(1:len(atype(1))),atype(1:nttype))
                if (ifound >= 1) then
                    call update_counters(noitm, noits, itmnr)
                    icm = itmnr + noitm + ioff
                    call movint(iar, itmnr      , itmnr + noitm*2)
                    call movint(iar, itmnr + noitm, itmnr + noitm*2)
                    call movchr(car, itmnr+ioff , icm)
                    iar(itmnr) = -ifound
                    iar(itmnr + noitm) = itmnr
                    iar(itmnr + noitm + noitm) = noits
                    car(itmnr + ioff) = parsed_str
                    car(itmnr + noitm + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (must_log .and. .not. usefor_on) then
                        write (lunut , 1030) caller, itmnr, caller, ifound, atype(ifound)
                    end if
                    !goto 10
                    exit scenarios
                end if
            
                ! If only existing names or types are allowed then
                !        this is the place for an error massage
                ! JVB stick to just a warning keep on reading IAR = 0?, or used for flow??
            
                if (chkflg == 1) then
                    call update_counters(noitm, noits, itmnr)
                    icm = itmnr + noitm + ioff
                    call shift_subarray(iar, itmnr      , itmnr + noitm*2)
                    !call movint(iar, itmnr      , itmnr + noitm*2)
                    call shift_subarray(iar, itmnr + noitm, itmnr + noitm*2)
                    !call movint(iar, itmnr + noitm, itmnr + noitm*2)
                    call shift_subarray(car(:icm+1), itmnr+ioff , icm) 
                    !call movchr(car, itmnr+ioff , icm)
                    iar (itmnr) = -1300000000
                    iar (itmnr + noitm) = 1300000000
                    iar (itmnr + noitm + noitm) = noits
                    car (itmnr + ioff) = parsed_str
                    car (itmnr + noitm + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    write(lunut , 1040) caller, itmnr, parsed_str
                    iwar = iwar + 1
                    !goto 10
                    exit scenarios
                else
                    ! Now a new name is added to the list of names
                    !        the rest is moved upward since it is all 1 array
                    ntitm = ntitm + 1
                    ioff  = ioff  + 1
                    icm   = icmax + ntitm
                    call movchr(aname, ntitm, icm)
                    aname(ntitm) = parsed_str
                    ! plus normal procedure
                    noitm = noitm + 1
                    noits = noits + 1
                    itmnr = itmnr + 1
                    icm = itmnr + noitm + ioff
                    call movint(iar, itmnr      , itmnr + noitm*2)
                    call movint(iar, itmnr + noitm, itmnr + noitm*2)
                    call movchr(car, itmnr+ioff , icm)
                    iar(itmnr) = ntitm
                    iar(itmnr + noitm) = itmnr
                    iar(itmnr + noitm + noitm) = noits
                    car(itmnr + ioff) = parsed_str
                    car(itmnr + noitm + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (must_log .and. .not. usefor_on) then
                        write (lunut , 1020) caller, itmnr, caller, ntitm, aname(ntitm)
                    end if
                    !goto 10
                    exit scenarios
                end if
            end if

            ! Scenario: no item name was given, but an item number
            if (itype == 2) then
                if (parsed_int <=  ntitm .and. parsed_int >= -nttype) then
                    call update_counters(noitm, noits, itmnr)
                    icm = itmnr + noitm + ioff
                    call movint(iar, itmnr      , itmnr + noitm*2)
                    call movint(iar, itmnr + noitm, itmnr + noitm*2)
                    call movchr(car, itmnr+ioff , icm)
                    iar (itmnr) = parsed_int
                    iar (itmnr + noitm) = itmnr
                    iar (itmnr + noitm + noitm) = noits
                    if (caller == 'segment') then
                        if (parsed_int <= 0) then
                            write (lunut , 1060) parsed_int
                            call error_and_finish(error_ind, ithndl)
                            return
                        end if
                        if (must_log .and. .not. usefor_on) then
                            write (lunut , 1015) caller, itmnr, caller,  parsed_int
                        end if
                        write (parsed_str , '(''Segment '',I8)') parsed_int
                    else if (parsed_int == 0 .and. caller .ne. 'CONCENTR. ') then
                        write (lunut , 1060) parsed_int
                        call error_and_finish(error_ind, ithndl)
                        return
                    else if (parsed_int > 0) then
                        if (must_log .and. .not. usefor_on) then
                            write (lunut , 1020) caller, itmnr, caller,  parsed_int,&
                                                                 aname(parsed_int)
                        end if
                        parsed_str = aname(parsed_int)
                    else if (parsed_int == 0 .and. caller == 'CONCENTR. ') then
                        if (must_log .and. .not. usefor_on) then
                            write (lunut , 1020) caller, itmnr, caller, parsed_int,&
                                                                  'FLOW'
                        end if
                        parsed_str = 'FLOW'
                    else
                        if (must_log .and. .not. usefor_on) then
                            write (lunut , 1030) caller, itmnr, caller, -parsed_int,&
                                             atype(-parsed_int)
                        end if
                        parsed_str = atype(-parsed_int)
                    end if
                    car (itmnr + ioff) = parsed_str
                    car (itmnr + noitm + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    !goto 10
                    exit scenarios
                else
                    write (lunut , 1060) parsed_int
                    call error_and_finish(error_ind, ithndl)
                    return
                end if
            end if
        end block scenarios
    end do
!
! 1000 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',&
!               A20,' and local substitution: ',A20)
! 1001 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',&
!               A20,' and local substitution: ',E15.6)
! 1010 format(' Input ',A,' nr:',I5,' is ',A,' type:',I5,&
!               ' with type: ',A20,' and local substitution: ',A20)
! 1011 format(' Input ',A,' nr:',I5,' is ',A,' type:',I5,&
!               ' with type: ',A20,' and local substitution: ',E15.6)
 1015 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5)
 1020 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',&
               A20)
 1030 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with type: ',&
               A20)
 1035 format(' ERROR: no reserved keyword expected: ', A20)
 1040 format(' WARNING: Input ',A,' nr:',I5,' with name: ',A20,&
               ' is not a valid ID, data ignored')
 1050 format(' ERROR: string is no valid item ID: ',A)
 1060 format(' ERROR: number: ',I5,' is not a valid item number !')
 1070 format(' ERROR: multiplication is only allowed in USEFOR',&
               ' context !')
 !1080 format(' ERROR: arithmetics should be separated by items !')
 1169 format(' Substituted by: ',E15.6)

    end subroutine dlwq5b

    subroutine update_counters(counter_a, counter_b, counter_c)
        integer, intent(inout) :: counter_a, counter_b, counter_c
        
        integer i

        counter_a = counter_a + 1
        counter_b = counter_b + 1
        counter_c = counter_c + 1
    end subroutine update_counters

    subroutine shift_int_subarray(total_array, start_shift, end_shift)
        integer, dimension(:), intent(inout) :: total_array
        integer, intent(in) :: start_shift, end_shift

        integer i
        do i = end_shift, start_shift, -1
            total_array(i+1) = total_array(i)
        end do
    end subroutine shift_int_subarray

    subroutine shift_char_subarray(total_array, start_shift, end_shift)
        character(len=*), dimension(:), intent(inout) :: total_array
        integer, intent(in) :: start_shift, end_shift

        integer i
        do i = end_shift, start_shift, -1
            total_array(i+1) = total_array(i)
        end do
    end subroutine shift_char_subarray

    subroutine log_local_substitution(index, log_unit, string)
        integer, intent(in) :: index
        integer, intent(in) :: log_unit 
        character(20), intent(in) :: string

        character(20) :: message_start

        select case(index)
            case (-1000000)
                message_start = ' Multiplied by'
            case (-10000000)
                message_start = ' Divided by'
            case (-100000000)
                message_start = ' Added with'
            case (-1000000000)
                message_start = ' Subtracted by'
            case (-1100000000)
                message_start = ' Minimum value is'
            case (-1200000000)
                message_start = ' Maximum value is'
        end select
        write(log_unit, '(2A,A20)') trim(message_start), ' local substitution: ', string
    end subroutine log_local_substitution

    subroutine log_item_number_name(index, log_unit, item_number, string)
        integer, intent(in) :: index
        integer, intent(in) :: log_unit
        integer, intent(in) :: item_number
        character(20), intent(in) :: string

        character(20) :: message_start

        select case(index)
            case (-1000000)
                message_start = ' Multiplied by'
            case (-10000000)
                message_start = ' Divided by'
            case (-100000000)
                message_start = ' Added with'
            case (-1000000000)
                message_start = ' Subtracted by'
            case (-1100000000)
                message_start = ' Minimum value is'
            case (-1200000000)
                message_start = ' Maximum value is'
        end select
        write(log_unit, '(2A,I6,A7, A20)') trim(message_start), ' item nr: ', item_number, ' Name: ', string
    end subroutine log_item_number_name

    subroutine log_number_in_operation(index, log_unit, number)
        integer, intent(in) :: index
        integer, intent(in) :: log_unit 
        real, intent(in)    :: number

        character(20) :: message_start

        select case(index)
            case (-1000000)
                message_start = ' Multiplied by: '
            case (-10000000)
                message_start = ' Divided by: '
            case (-100000000)
                message_start = ' Added with: '
            case (-1000000000)
                message_start = ' Subtracted by: '
            case (-1100000000)
                message_start = ' Minimum value is: '
            case (-1200000000)
                message_start = ' Maximum value is: '
        end select
        write(log_unit, '(A,E15.6,A20)') trim(message_start), number
    end subroutine log_number_in_operation

    subroutine log_name_substitution(index, log_unit, all_names, caller, itmnr, all_types, target_real, target_char, real_substitution)
        integer, intent(in) :: index
        integer, intent(in) :: log_unit
        integer, intent(in) :: itmnr
        real, intent(in)    :: target_real
        character(*), intent(in) :: caller
        character(*), intent(in) :: target_char
        logical, intent(in) :: real_substitution
        character(*),  intent(inout) :: all_names(*)     !< Names (id's) of the boundaries/wastes
        character(*),  intent(in   ) :: all_types(*)     !< Types of the boundaries/wastes

        character(20) :: name
        character(28) :: substitution
        character(4)  :: id_or_type
        integer :: name_index

        id_or_type = 'ID  '
        name_index = index
        select case(index)
            case(1:)
                name = all_names(name_index)
            case(0)
                name = 'FLOW'
            case(-1300000000)
                name = 'Ignored'
            case default
                id_or_type = 'type'
                name_index = -index
                name = all_types(name_index)
        end select
        if (real_substitution) then
            write(substitution, '(A20)') target_real
        else
            substitution = target_char
        end if
        write(log_unit, '(3A,I5,2A,I5,3A,A20,A,A20)') ' Input ', caller, ' nr:', itmnr, ' is ', caller, &
                        name_index , ' with ', trim(id_or_type), '  : ', name , ' and local substitution: ',  substitution
    end subroutine log_name_substitution

    subroutine error_and_finish(error_ind, ithndl)
        integer, intent(inout) :: error_ind !< Error index
        integer, intent(in   ) :: ithndl    !< Handle to stop timing

        error_ind = 1
        call finish(ithndl)
    end subroutine error_and_finish

    subroutine finish(ithndl)
        integer, intent(in   ) :: ithndl    !< Handle to stop timing

        if (timon) call timstop(ithndl)
    end subroutine finish



end module m_dlwq5b
