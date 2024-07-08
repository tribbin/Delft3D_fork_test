      FUNCTION IFREAL(VAL1, VAL2, EPS)
!C
!C---- COMPARES TWO REAL POINT NUMBERS
!C
!C     RETURN VALUE: -1 IF VAL1 < VAL2
!C                    0 IF VAL1 = VAL2
!C                   +1 IF VAL1 > VAL2
!C
      Integer Ifreal, IfFlt
      Real Val1, Val2, Eps, Value

      IF (ABS(VAL1) .LT. 1.0  .OR.  ABS(VAL2) .LT. 1.0) THEN
         VALUE = VAL1 - VAL2
      ELSE
         VALUE = VAL1 / VAL2 - 1.0
      ENDIF
!C
!CGP  IF (ABS(VALUE) .LT. EPS) THEN
!CGP  ook eps=0 toestaan
      IF (ABS(VALUE) .LE. EPS) THEN
         IFFLT = 0
      ELSE
         IF (VAL1 .LT. VAL2) THEN
            IFFLT = -1
         ELSE
            IFFLT = 1
         ENDIF
      ENDIF
      IfReal = IfFlt
!C
      Return
      END



      FUNCTION D_IFREAL(VAL1, VAL2, EPS)
!C
!C---- COMPARES TWO Double precision POINT NUMBERS
!C
!C     RETURN VALUE: -1 IF VAL1 < VAL2
!C                    0 IF VAL1 = VAL2
!C                   +1 IF VAL1 > VAL2
!C
      Integer          D_Ifreal, IfFlt
      Double precision Val1, Val2, Eps, Value

      IF (ABS(VAL1) .LT. 1.0  .OR.  ABS(VAL2) .LT. 1.0) THEN
         VALUE = VAL1 - VAL2
      ELSE
         VALUE = VAL1 / VAL2 - 1.0
      ENDIF
!C
!CGP  IF (ABS(VALUE) .LT. EPS) THEN
!CGP  ook eps=0 toestaan
      IF (ABS(VALUE) .LE. EPS) THEN
         IFFLT = 0
      ELSE
         IF (VAL1 .LT. VAL2) THEN
            IFFLT = -1
         ELSE
            IFFLT = 1
         ENDIF
      ENDIF
!     write(*,*) ' D_Ifreal val1 val2 eps result'
!     write(*,*) val1, val2, eps, ifflt
      D_IfReal = IfFlt
!C
      Return
      END
