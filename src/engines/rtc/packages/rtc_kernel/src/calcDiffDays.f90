! function returns the number of days between two dates
! uses function julian()
! precondition: firstEvent occurs before secondEvent

Double Precision function calcDiffDays(firstEvent, secondEvent, evstrt, Nevnt)

  implicit none

  Integer firstEvent, secondEvent, date1, date2, time1, time2, NEvnt
  INTEGER      EVSTRT(NEVNT,6)

  double precision julian, jDate1, jDate2, diff

  date1 = (evstrt(firstEvent, 1) * 10000) + &
          (evstrt(firstEvent, 2) * 100) + &
           evstrt(firstEvent, 3)
  time1 =  evstrt(firstEvent, 4) * 10000. + &
           evstrt(firstEvent, 5) * 100.

  date2 = (evstrt(secondEvent, 1) * 10000) + &
          (evstrt(secondEvent, 2) * 100) + &
           evstrt(secondEvent, 3)
  time2 =  evstrt(secondEvent, 4) * 10000. + &
           evstrt(secondEvent, 5) * 100.

  jDate1 = julian(date1, time1)
  jDate2 = julian(date2, time2)
  diff = jDate2 - jDate1
! write(*,*) ' CalcdiffDays', firstevent, secondevent, diff
! write(*,*) ' CalcdiffDays', DATE1, DATE2

  calcDiffDays = diff
  return

end function calcDiffDays
