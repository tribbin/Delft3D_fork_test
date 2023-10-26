!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      module m_dhgvar
      use m_waq_type_definitions
      use m_monsys


      implicit none

      contains


      SUBROUTINE DHGVAR ( IAR_NR, INDX  , IVAR  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : Jan van Beek
!
!     FUNCTION            : Initialisation of Variables structure
!
!     SUBROUTINES CALLED  :
!
!     FILES               :
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IAR_NR  INTEGER       1     INPUT   Array number
!     INDX    INTEGER       1     INPUT   Index number variable in array
!     IVAR    INTEGER       1     OUTPUT  Variable number, else -1
!
!     Declaration of arguments

      use m_sysn          ! System characteristics
!
      INTEGER(kind=int_32) ::IAR_NR, INDX  , IVAR

      INTEGER(kind=int_32) ::IIVOL  
      INTEGER(kind=int_32) ::IIAREA 
      INTEGER(kind=int_32) ::IIFLOW 
      INTEGER(kind=int_32) ::IILENG 
      INTEGER(kind=int_32) ::IIDISP 
      INTEGER(kind=int_32) ::IICONC 
      INTEGER(kind=int_32) ::IIMASS 
      INTEGER(kind=int_32) ::IIDERV 
      INTEGER(kind=int_32) ::IIBOUN 
      INTEGER(kind=int_32) ::IIBSET 
      INTEGER(kind=int_32) ::IIBSAV 
      INTEGER(kind=int_32) ::IIWSTE 
      INTEGER(kind=int_32) ::IICONS 
      INTEGER(kind=int_32) ::IIPARM 
      INTEGER(kind=int_32) ::IIFUNC 
      INTEGER(kind=int_32) ::IISFUN 
      INTEGER(kind=int_32) ::IIDNEW 
      INTEGER(kind=int_32) ::IIDIFF 
      INTEGER(kind=int_32) ::IIVNEW 
      INTEGER(kind=int_32) ::IIVELO 
      INTEGER(kind=int_32) ::IIHARM 
      INTEGER(kind=int_32) ::IIFARR 
      INTEGER(kind=int_32) ::IIMAS2 
      INTEGER(kind=int_32) ::IITIMR 
      INTEGER(kind=int_32) ::IIVOL2 
      INTEGER(kind=int_32) ::IITRAC 
      INTEGER(kind=int_32) ::IIGWRK 
      INTEGER(kind=int_32) ::IIGHES 
      INTEGER(kind=int_32) ::IIGSOL 
      INTEGER(kind=int_32) ::IIGDIA 
      INTEGER(kind=int_32) ::IIGTRI 
      INTEGER(kind=int_32) ::IISMAS 
      INTEGER(kind=int_32) ::IIPLOC 
      INTEGER(kind=int_32) ::IIDEFA 
      INTEGER(kind=int_32) ::IIFLUX 
      INTEGER(kind=int_32) ::IISTOC 
      INTEGER(kind=int_32) ::IIFLXD 
      INTEGER(kind=int_32) ::IIFLXI 
      INTEGER(kind=int_32) ::IIRIOB 
      INTEGER(kind=int_32) ::IIDSPX 
      INTEGER(kind=int_32) ::IIVELX 
      INTEGER(kind=int_32) ::IILOCX 
      INTEGER(kind=int_32) ::IIDSTO 
      INTEGER(kind=int_32) ::IIVSTO 
      INTEGER(kind=int_32) ::IIDMPQ 
      INTEGER(kind=int_32) ::IIDMPS 
      INTEGER(kind=int_32) ::IITRRA 
      INTEGER(kind=int_32) ::IINRSP 
      INTEGER(kind=int_32) ::IIVOLL 
      INTEGER(kind=int_32) ::IIVOL3 
      INTEGER(kind=int_32) ::IIR1   
      INTEGER(kind=int_32) ::IIQXK  
      INTEGER(kind=int_32) ::IIQYK  
      INTEGER(kind=int_32) ::IIQZK  
      INTEGER(kind=int_32) ::IIDIFX 
      INTEGER(kind=int_32) ::IIDIFY 
      INTEGER(kind=int_32) ::IIDIFZ 
      INTEGER(kind=int_32) ::IIVOLA 
      INTEGER(kind=int_32) ::IIVOLB 
      INTEGER(kind=int_32) ::IIGUV  
      INTEGER(kind=int_32) ::IIGVU  
      INTEGER(kind=int_32) ::IIGZZ  
      INTEGER(kind=int_32) ::IIAAK  
      INTEGER(kind=int_32) ::IIBBK  
      INTEGER(kind=int_32) ::IICCK  
      INTEGER(kind=int_32) ::IIBD3X 
      INTEGER(kind=int_32) ::IIBDDX 
      INTEGER(kind=int_32) ::IIBDX  
      INTEGER(kind=int_32) ::IIBU3X 
      INTEGER(kind=int_32) ::IIBUUX 
      INTEGER(kind=int_32) ::IIBUX  
      INTEGER(kind=int_32) ::IIWRK1 
      INTEGER(kind=int_32) ::IIWRK2 
      INTEGER(kind=int_32) ::IIAAKL 
      INTEGER(kind=int_32) ::IIBBKL 
      INTEGER(kind=int_32) ::IICCKL 
      INTEGER(kind=int_32) ::IIDDKL 
      
      INTEGER(kind=int_32) ::IVVOL
      INTEGER(kind=int_32) ::IVARE
      INTEGER(kind=int_32) ::IVFLO
      INTEGER(kind=int_32) ::IVLEN
      INTEGER(kind=int_32) ::IVCNS
      INTEGER(kind=int_32) ::IVPAR
      INTEGER(kind=int_32) ::IVFUN
      INTEGER(kind=int_32) ::IVSFU
      INTEGER(kind=int_32) ::IVCNC
      INTEGER(kind=int_32) ::IVMAS
      INTEGER(kind=int_32) ::IVDER
      INTEGER(kind=int_32) ::IVDSP
      INTEGER(kind=int_32) ::IVVEL
      INTEGER(kind=int_32) ::IVDEF
      INTEGER(kind=int_32) ::IVLOC
      INTEGER(kind=int_32) ::IVDSX
      INTEGER(kind=int_32) ::IVVLX
      INTEGER(kind=int_32) ::IVLCX
      INTEGER(kind=int_32) ::IVFLX
      INTEGER(kind=int_32) ::LUNREP
!
!     Just take the used array's in the right order
!
      IIVOL  =  1
      IIAREA =  2
      IIFLOW =  3
      IILENG =  4
      IIDISP =  5
      IICONC =  6
      IIMASS =  7
      IIDERV =  8
      IIBOUN =  9
      IIBSET = 10
      IIBSAV = 11
      IIWSTE = 12
      IICONS = 13
      IIPARM = 14
      IIFUNC = 15
      IISFUN = 16
      IIDNEW = 17
      IIDIFF = 18
      IIVNEW = 19
      IIVELO = 20
      IIHARM = 21
      IIFARR = 22
      IIMAS2 = 23
      IITIMR = 24
      IIVOL2 = 25
      IITRAC = 26
      IIGWRK = 27
      IIGHES = 28
      IIGSOL = 29
      IIGDIA = 30
      IIGTRI = 31
      IISMAS = 32
      IIPLOC = 33
      IIDEFA = 34
      IIFLUX = 35
      IISTOC = 36
      IIFLXD = 37
      IIFLXI = 38
      IIRIOB = 39
      IIDSPX = 40
      IIVELX = 41
      IILOCX = 42
      IIDSTO = 43
      IIVSTO = 44
      IIDMPQ = 45
      IIDMPS = 46
      IITRRA = 47
      IINRSP = 48
      IIVOLL = 49
      IIVOL3 = 50
      IIR1   = 51
      IIQXK  = 52
      IIQYK  = 53
      IIQZK  = 54
      IIDIFX = 55
      IIDIFY = 56
      IIDIFZ = 57
      IIVOLA = 58
      IIVOLB = 59
      IIGUV  = 60
      IIGVU  = 61
      IIGZZ  = 62
      IIAAK  = 63
      IIBBK  = 64
      IICCK  = 65
      IIBD3X = 66
      IIBDDX = 67
      IIBDX  = 68
      IIBU3X = 69
      IIBUUX = 70
      IIBUX  = 71
      IIWRK1 = 72
      IIWRK2 = 73
      IIAAKL = 74
      IIBBKL = 75
      IICCKL = 76
      IIDDKL = 77
!
      IVVOL = 1
      IVARE = IVVOL + 1
      IVFLO = IVARE + 1
      IVLEN = IVFLO + 1
      IVCNS = IVLEN + 2
      IVPAR = IVCNS + NOCONS
      IVFUN = IVPAR + NOPA
      IVSFU = IVFUN + NOFUN
      IVCNC = IVSFU + NOSFUN
      IVMAS = IVCNC + NOTOT
      IVDER = IVMAS + NOTOT
      IVDSP = IVDER + NOTOT
      IVVEL = IVDSP + NODISP
      IVDEF = IVVEL + NOVELO
      IVLOC = IVDEF + NODEF
      IVDSX = IVLOC + NOLOC
      IVVLX = IVDSX + NDSPX
      IVLCX = IVVLX + NVELX
      IVFLX = IVLCX + NLOCX
!
      IVAR = -1
!
      IF ( IAR_NR .EQ. IIVOL ) THEN
         IF ( INDX .GT. 1 ) GOTO 900
         IVAR = IVVOL + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIAREA ) THEN
         IF ( INDX .GT. 1 ) GOTO 900
         IVAR = IVARE + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIFLOW ) THEN
         IF ( INDX .GT. 1 ) GOTO 900
         IVAR = IVFLO + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IILENG ) THEN
         IF ( INDX .GT. 2 ) GOTO 900
         IVAR = IVLEN + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IICONS ) THEN
         IF ( INDX .GT. NOCONS ) GOTO 900
         IVAR = IVCNS + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIPARM ) THEN
         IF ( INDX .GT. NOPA ) GOTO 900
         IVAR = IVPAR + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIFUNC ) THEN
         IF ( INDX .GT. NOFUN ) GOTO 900
         IVAR = IVFUN + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IISFUN ) THEN
         IF ( INDX .GT. NOSFUN ) GOTO 900
         IVAR = IVSFU + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IICONC ) THEN
         IF ( INDX .GT. NOTOT ) GOTO 900
         IVAR = IVCNC + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIMASS ) THEN
         IF ( INDX .GT. NOTOT ) GOTO 900
         IVAR = IVMAS + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIDERV ) THEN
         IF ( INDX .GT. NOTOT ) GOTO 900
         IVAR = IVDER + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIDISP ) THEN
         IF ( INDX .GT. NODISP ) GOTO 900
         IVAR = IVDSP + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIVELO ) THEN
         IF ( INDX .GT. NOVELO ) GOTO 900
         IVAR = IVVEL + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIDEFA ) THEN
         IF ( INDX .GT. NODEF ) GOTO 900
         IVAR = IVDEF + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIPLOC ) THEN
         IF ( INDX .GT. NOLOC ) GOTO 900
         IVAR = IVLOC + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIDSPX  ) THEN
         IF ( INDX .GT. NDSPX ) GOTO 900
         IVAR = IVDSX + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIVELX  ) THEN
         IF ( INDX .GT. NVELX ) GOTO 900
         IVAR = IVVLX + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IILOCX  ) THEN
         IF ( INDX .GT. NLOCX ) GOTO 900
         IVAR = IVLCX + INDX - 1
      ENDIF
!
      IF ( IAR_NR .EQ. IIFLUX  ) THEN
         IF ( INDX .GT. NFLUX ) GOTO 900
         IVAR = IVFLX + INDX - 1
      ENDIF
!
      IF ( IVAR .EQ. -1 ) GOTO 900
!
      RETURN
!
  900 CONTINUE
      CALL GETMLU(LUNREP)
      WRITE(LUNREP,2000) IAR_NR,INDX
      RETURN
 2000 FORMAT (' WARNING in DHGVAR, array or index out of range',I10,I10)
!
      END
      end module m_dhgvar
