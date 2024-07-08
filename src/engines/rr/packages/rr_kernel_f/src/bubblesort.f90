      SUBROUTINE BubbleSort (NA,A,B,N,Option)

! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    This subroutines performs sorting of an array A in increasing
! ***    order, using Bubble sort (not a very good algorithm though).
! ***    Optionally also array B is included in the sorting alg.
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! *** NA = dimension of integer array A
! *** A  = array A with integer values
! *** B  = array B with values, optionally also to be sorted using array A
! *** N  = number of elements in array A
! *** Option  = yes/no sort B also
! *********************************************************************
! *********************************************************************

      INTEGER   ::  NA, N
      Real      ::  A(NA), B(NA)
      Logical   ::  Option

      INTEGER       J,R,L,K
      Real          X

!
! Een array van lengte 1 of kleiner hoeft niet gesorteerd te worden
      if (NA .le. 1 .or. n .le. 1) goto 999

      L = 2
      R = N
      K = N
  100 IF ( L.GT.R ) THEN
!
         RETURN
      ELSE
         DO 150 J = R , L , -1
            IF ( A(J-1).GT.A(J) ) THEN
               X = A(J-1)
               A(J-1) = A(J)
               A(J) = X
               If (option) then
                 X = B(J-1)
                 B(J-1) = B(J)
                 B(J) = X
               Endif
               K = J
            END IF
  150    CONTINUE
         L = K + 1
         DO 200 J = L , R
            IF ( A(J-1).GT.A(J) ) THEN
               X = A(J-1)
               A(J-1) = A(J)
               A(J) = X
               If (option) then
                  X = B(J-1)
                  B(J-1) = B(J)
                  B(J) = X
               Endif
               K = J
            END IF
  200    CONTINUE
         R = K - 1
         GOTO 100
      END IF

  999 Continue
      Return
      END
