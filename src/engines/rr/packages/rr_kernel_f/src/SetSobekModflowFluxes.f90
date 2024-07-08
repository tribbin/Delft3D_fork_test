    Subroutine SetSobekModflowFluxes (ModflowIds, ModflowFluxes, NLocf)

    use Conf_Arr
    use Network
    use Unpaved
    use OpenWater

    implicit none
    
    Integer       NLocf
    Character(20) ModflowIds (NLocf)
    Real          ModflowFluxes (NLocf)
    Integer       i, imap


    IMap = 2
    Do i=1, nLcMap(iMap)
       ModflowIds(i) = Id_Nod(iiNode(IxlMap(imap,i)))
       ModflowFluxes(i) = (Kwel(i)-WegZg(i))*NrsDay*1000.
    Enddo
    IMap = 4
    Do i=1, nLcMap(iMap)
       ModflowIds(ncovhg+i) = Id_Nod(iiNode(IxlMap(imap,i)))
       ModflowFluxes(ncovhg+i) = (OwKwel(i)-OwWegZ(i))*NrsDay*1000.
    Enddo



    Return
  END subroutine SetSobekModflowFluxes


