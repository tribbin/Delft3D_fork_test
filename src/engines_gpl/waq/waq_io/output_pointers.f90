!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module output_pointers
    use m_waq_precision

    implicit none

    private
    public :: get_output_pointers

contains


    subroutine get_output_pointers(noutp, nrvar, nrvarm, dlwnam, iopoin, &
            nmis, notot, syname, nocons, coname, &
            nopa, paname, nofun, funame, nosfun, &
            sfname, lurep)

        !! Sets the pointers for all extra vars

        use timers       !   performance timers

        integer(kind = int_wp), intent(in) :: noutp                  !< Number of output files
        integer(kind = int_wp), intent(in) :: nrvar (noutp)         !< No of output vars per file
        integer(kind = int_wp), intent(in) :: nrvarm                 !< Maximum of output variables p.p.
        integer(kind = int_wp), intent(out) :: nmis                   !< Number of missing input vars
        character*(*), intent(in) :: dlwnam(nrvarm, noutp)  !< Name of input variables
        integer(kind = int_wp), intent(out) :: iopoin(nrvarm, noutp)   !< Number of missing input vars
        integer(kind = int_wp), intent(in) :: notot                  !< Total number of substances
        integer(kind = int_wp), intent(in) :: nopa                   !< Number of parameters
        integer(kind = int_wp), intent(in) :: nosfun                 !< Number of segment functions
        character(20), intent(in) :: syname(notot)         !< Names of systems
        integer(kind = int_wp), intent(in) :: nocons                 !< Number of constants used
        integer(kind = int_wp), intent(in) :: nofun                  !< Number of functions ( user )
        character(20), intent(in) :: coname(nocons)        !< Constant names
        character(20), intent(in) :: paname(nopa)        !< Parameter names
        character(20), intent(in) :: funame(nofun)        !< Function names
        character(20), intent(in) :: sfname(nosfun)        !< Segment function names
        integer(kind = int_wp), intent(in) :: lurep                  !< Unit nr. report file

        character(20) varnam            ! Name of variable to be identified
        integer(kind = int_wp) :: ivarip             ! Pointer in the SSA
        integer(kind = int_wp) :: iout               ! loop variable of outputs
        integer(kind = int_wp) :: inrv               ! loop variable output number
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("get_output_pointers", ithndl)

        write(lurep, *)
        write(lurep, *) ' Determining the place of the output variables'
        write(lurep, *)

        nmis = 0

        do iout = 1, noutp
            do inrv = 1, nrvar(iout)
                varnam = dlwnam(inrv, iout)
                if (varnam == ' ') then
                    ivarip = 0
                else
                    call set_output_pointers (notot, nopa, nosfun, syname, nocons, &
                            nofun, coname, paname, funame, sfname, &
                            varnam, ivarip, lurep)
                    if (ivarip == -1) then
                        nmis = nmis + 1
                        write(lurep, '(3a)') '   INFO:', varnam, &
                                '; NOT FOUND, delwaq will detect variables from process library'
                    endif
                endif
                iopoin(inrv, iout) = ivarip
            end do
        end do

        if (timon) call timstop(ithndl)
        return
    end subroutine get_output_pointers

    subroutine set_output_pointers(notot, nopa, nosfun, syname, nocons, &
            nofun, coname, paname, funame, sfname, &
            varnam, ivarip, lurep)

        !! sets pointers for output variables
        !!     Logical unitnumbers : lurep   - report file

        use timers       !   performance timers
        use m_string_utils

        integer(kind = int_wp), intent(in) :: notot              !< Total number of substances
        integer(kind = int_wp), intent(in) :: nopa               !< Number of parameters
        integer(kind = int_wp), intent(in) :: nosfun             !< Number of segment functions
        character(20), intent(in) :: syname(notot)     !< Names of systems
        integer(kind = int_wp), intent(in) :: nocons             !< Number of constants used
        integer(kind = int_wp), intent(in) :: nofun              !< Number of functions ( user )
        character(20), intent(in) :: coname(nocons)    !< Constant names
        character(20), intent(in) :: paname(nopa)    !< Parameter names
        character(20), intent(in) :: funame(nofun)    !< Function names
        character(20), intent(in) :: sfname(nosfun)    !< Segment function names
        character(20), intent(in) :: varnam            !< Name of variable to be identified
        integer(kind = int_wp), intent(out) :: ivarip             !< Pointer in the SSA
        integer(kind = int_wp), intent(in) :: lurep              !< Unit nr. report file

        !     Local

        integer(kind = int_wp), parameter :: nopred = 6
        integer(kind = int_wp) :: indx            !  index in array of names
        character(20) predef(3)
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("set_output_pointers", ithndl)

        predef(1) = 'volume'
        predef(2) = 'itime'
        predef(3) = 'idt'

        !     determine how VAL is modelled

        indx = index_in_array(varnam, predef)
        if (indx == 1) then
            write(lurep, *) '       ', varnam, '; Using DELWAQ VOLUME'
            ivarip = 1
            goto 800
        endif
        if (indx == 2) then
            write(lurep, *) '       ', varnam, '; Using DELWAQ ITIME'
            ivarip = 2
            goto 800
        endif
        if (indx == 3) then
            write(lurep, *) '       ', varnam, '; Using DELWAQ IDT'
            ivarip = 3
            goto 800
        endif

        !     as model variable ?

        indx = index_in_array(varnam, syname)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using substance nr', indx
            ivarip = nopred + nocons + nopa + nofun + nosfun + indx
            goto 800
        endif

        !     as segment function ?

        indx = index_in_array(varnam, sfname)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using segment function nr', indx
            ivarip = nopred + nocons + nopa + nofun + indx
            goto 800
        endif

        !     as function ?

        indx = index_in_array(varnam, funame)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using function nr', indx
            ivarip = nopred + nocons + nopa + indx
            goto 800
        endif

        !     as parameter ?

        indx = index_in_array(varnam, paname)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using parameter nr', indx
            ivarip = nopred + nocons + indx
            goto 800
        endif

        !     as constant ?

        indx = index_in_array(varnam, coname)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using constant nr', indx
            ivarip = nopred + indx
            goto 800
        endif

        !     not found

        ivarip = -1

        800 continue

        if (timon) call timstop(ithndl)
        return
    end subroutine set_output_pointers

end module output_pointers
