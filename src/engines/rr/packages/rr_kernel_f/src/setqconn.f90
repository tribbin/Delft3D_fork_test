

  Subroutine SetQConn(iConn, Flow)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    vull Qconn array
    ! *********************************************************************

   use RRConnectionBifurcationNodes

   Implicit none
   Integer IConn
   Real    Flow

   QConn(IConn) = QConn(Iconn) + Flow

  Return
  END subroutine SetQConn



