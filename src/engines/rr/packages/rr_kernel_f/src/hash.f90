! Last changed
! by:               $Author:: Prinsen           $
! at:               $Modtime:: 26-02-98 11:53a  $
!
! current revision: $Revision:: 1               $

      Module Hash

      USE CONF_ARR
      USE Network
      Use DH_Alloc
      Use ReadLib

      implicit none
      
      INTEGER    hashcon
      PARAMETER  (hashcon = 1009)
      INTEGER, Pointer, SAVE ::  hashfirst(:), hashnext(:)

      contains

!--------------------------------------------------------------------
!     HASHFUN.FOR: Hashing function
!--------------------------------------------------------------------

      INTEGER FUNCTION HASHFUN (STRING)

      implicit none
      
      CHARACTER(CharIdLength) STRING

      INTEGER result, lengte, i

      result = 0
      LENGTE = Len_Trim(STRING)

      DO I=1, LENGTE
         result = result + ichar (STRING(I:I))
      ENDDO

      result = MOD (result, hashcon)
! ARS 1120: prevent result=0
      if (result .eq. 0) result = hashcon

      HASHFUN = result

      RETURN
      END function Hashfun



!--------------------------------------------------------------------
!     HASHINIT.FOR: INIT hashing arrays
!--------------------------------------------------------------------

      Subroutine HASHINIT

      implicit none
      
      Logical success

      Success = DH_AllocInit (Hashcon, HashFirst, 0)
      Success = Success .and. DH_AllocInit (NcNode, HashNext, 0)
!      ALLOCATE ( hashfirst(hashcon), hashnext(ncnode), Stat=Allocation_Error )
      If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                              ' HashInit'  )

!     Vactor/Array initialisation
!      HashFirst = 0
!      HashNext  = 0

      RETURN
      END subroutine HashInit



!--------------------------------------------------------------------
!     HASHFILL.FOR: Fill hashing arrays
!--------------------------------------------------------------------

      subroutine HASHFILL (id, nodenr )

      implicit none
      
      character(CharIdLength) id, locid
      integer nodenr
      INTEGER hashcode, inr, next

      locid = id
      Call upperc(locid)
      hashcode = hashfun(locid)

!      write(*,*) ' Hashfill ', id,' ', hashcode

      if (hashfirst(hashcode) .eq. 0) then
          hashfirst(hashcode) = nodenr
          hashnext (nodenr)= 0
      else
          inr  = hashfirst(hashcode)
          next = hashnext (inr)
  10      if (next .ne. 0) then
              inr  = next
              next = hashnext (inr)
              goto 10
          endif
          hashnext(inr) = nodenr
      endif


      RETURN
      END subroutine HashFill

!--------------------------------------------------------------------
!     HASHsearch.FOR: search in hashing arrays
!--------------------------------------------------------------------

      integer function HASHsearch (id, allownotfound)

      implicit none
      
      character(CharIdLength) id, locid, idtest
      INTEGER hashcode, inr, next, ifound
      LOGICAL allownotfound

      ifound = -1


      locid = id
      Call upperc (locid)
      hashcode = hashfun(locid)

!     write(*,*) ' Hashsearch', id, hashcode

      if (hashfirst(hashcode) .eq. 0) then
          ! not found
      else
          inr  = hashfirst(hashcode)
          next = inr
 10       if (next .ne. 0) then
              idtest = Id_Nod(next)
              call upperc (idtest)
              if (locid .ne. idtest) then
                inr  = next
                next = hashnext (inr)
                goto 10
              else
                ifound = next
              endif
          endif
      endif

      if ( (ifound .le. 0) .and. (.not. allownotfound) ) then
          call ErrMsgStandard (951, 0, '  FNDNOD', id)
      endif

      Hashsearch = ifound


      RETURN
      END function HashSearch



      End Module Hash
