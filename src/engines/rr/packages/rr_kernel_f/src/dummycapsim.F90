#if (defined(HAVE_CONFIG_H))
  subroutine readroot()
  return
  end

  subroutine readunsa()
  return
  end

  subroutine simgro_ovz()
  return
  end
#else
! do nothing, just 1 fake routine
  subroutine dummycapsim()
  return
  end
#endif
