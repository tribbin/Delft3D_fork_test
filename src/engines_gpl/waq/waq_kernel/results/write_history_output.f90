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
module m_write_history_output
    use m_waq_precision

    implicit none

    private
    public :: write_binary_history_output

contains


    SUBROUTINE write_binary_history_output(history_file_unit, NAMFIH, ITIME, MONAME, NODUMP, &
            IDUMP, DUNAME, NOTOT1, SYNAM1, CONC1, &
            NOTOT2, SYNAM2, CONC2, INIT)
        ! Writes history output


        !
        !     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     history_file_unit   INTEGER  1           INPUT   unit number output file
        !     NAMFIH  CHAR*(*) 1           INPUT   name output file
        !     ITIME   INTEGER  1           INPUT   present time in clock units
        !     MONAME  CHAR*40  4           INPUT   model identhification
        !     NODUMP  INTEGER  1           INPUT   number of dump locations
        !     IDUMP   INTEGER  NODUMP      INPUT   dump segment numbers
        !     DUNAME  CHAR*20  NODUMP      INPUT   names of dump locations
        !     NOTOT1  INTEGER  1           INPUT   number of vars in CONC1
        !     SYNAM1  CHAR*20  NOTOT1      INPUT   names of vars in CONC1
        !     CONC1   REAL     NOTOT1*?    INPUT   values
        !     NOTOT2  INTEGER  1           INPUT   number of extra output vars
        !     SYNAM2  CHAR*20  NOTOT       INPUT   names of extra vars
        !     CONC2   REAL    NOTOT2,NX*NY INPUT   values for extra vars
        !     INIT    INTEGER  1           IN/OUT  Initialize flag

        use timers

        integer(kind = int_wp) :: history_file_unit, itime, nodump, notot1, notot2, init
        integer(kind = int_wp) :: idump(*)
        character(len = *) moname(4), namfih
        character(len = *) duname(*), synam1(*), synam2(*)
        real(kind = real_wp) :: conc1(*), conc2(*)
        integer(kind = int_wp) :: i, k1, k2, j

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("write_binary_history_output", ithandl)

        ! Initialize file
        if (init == 1) then
            init = 0
            write (history_file_unit) (moname(i), i = 1, 4)
            write (history_file_unit)  notot1 + notot2, nodump
            write (history_file_unit) (synam1(i), i = 1, notot1), (synam2(i), i = 1, notot2)
            write (history_file_unit) (i, duname(i), i = 1, nodump)
        endif

        ! Perform output
        write (history_file_unit) itime, (&
                (conc1(k1 + (idump(j) - 1) * notot1), k1 = 1, notot1), &
                (conc2(k2 + (j - 1) * notot2), k2 = 1, notot2), &
                j = 1, nodump)
        if (timon) call timstop (ithandl)

    end subroutine write_binary_history_output

end module m_write_history_output
