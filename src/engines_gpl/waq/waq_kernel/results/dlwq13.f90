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
module m_dlwq13
    use m_waq_precision

    implicit none

contains


    !> gives a complete system dump
    subroutine dlwq13 (lun, lchar, conc, itime, mname, &
            sname, notot, noseg)

        use m_open_waq_files
        use timers

        integer(kind = int_wp), intent(in) :: lun(*)             !< logical unit numbers of output files
        character(len=*), intent(in)       :: lchar (*)          !< names of output files
        real(kind = real_wp),intent(in)    :: conc(notot, noseg) !< concentration values
        integer(kind=int_wp), intent(in)   :: itime              !< present time in clock units
        character(len=40), intent(in)      :: mname (*)          !< model identification
        character(len=20), intent(in)      :: sname (*)          !< names of substances
        integer(kind=int_wp), intent(in)   :: notot              !< total number of systems
        integer(kind=int_wp), intent(in)   :: noseg              !< total number of segments or cells


        character(len=255) lcharmap
        integer(kind = int_wp) :: i, j, k
        integer(kind = int_wp) :: nonan, ierr
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwq13", ithandl)
        !
        !      check for NaNs
        nonan = 0
        do j = 1, noseg
            do i = 1, notot
                if (conc(i, j) /= conc(i, j)) then
                    conc(i, j) = 0.0
                    nonan = nonan + 1
                endif
            enddo
        enddo

        if (nonan /= 0) then
            write (lun(19), *) ' Corrected concentrations as written to the restart file:'
            write (lun(19), *) ' Number of values reset from NaN to zero: ', nonan
            write (lun(19), *) ' Total amount of numbers in the array: ', notot * noseg
            write (lun(19), *) ' This may indicate that the computation was unstable'
        endif

        !
        !     write restart file in .map format
        lcharmap = ' '
        lcharmap(1:248) = lchar(23)(1:248)
        do i = 248, 1, -1
            if (lcharmap(i:i) == '.') then
                lcharmap(i:i + 7) = "_res.map"
                goto 20
            endif
        end do
        write (*, *) ' Invalid name of restart MAP file !'
        write (*, *) ' Restart file written to restart_temporary.map !'
        write (lun(19), *) ' Invalid name of restart MAP file !'
        write (lun(19), *) ' Restart file written to restart_temporary.map !'
        lcharmap = 'restart_temporary.map'

        20 call open_waq_files (lun(23), lcharmap, 23, 1, ierr)
        write (lun(23)) (mname(k), k = 1, 4)
        write (lun(23))   notot, noseg
        write (lun(23)) (sname(k), k = 1, notot)
        write (lun(23)) itime, conc
        close (lun(23))
        !
        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwq13
