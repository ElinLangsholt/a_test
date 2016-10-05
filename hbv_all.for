c------------------------------------------------------------------
c     KARMEN VER 3.15
c     Administrative rutiner
c     Nils Roar Sælthun, Hydrologisk avdeling
c     95/09/19
C
C     Modified output on LULIST
c         ind(16) = 1: Snow content in all levels
c         ind(16) = 2: water flows
c         ind(16) = 3: states
c         ind(16) = 4: snowcover
c         ind(16) = 5: glacier mass balance
c         ind(16) = 6: diverse data, (evapotranspiration etc)  
c
c     Increased number of precipitation stations -> 25         
c     Increased number of temperature stations   -> 4
c     Separate climate change file (CLIMCHA.DAT)
c     Separate vegetation type file (VEGTYPE.DAT)
c     New snow distribution
c     interception evaporation
c     lake temperature
c     lake component
c
c    Nils Roar Sælthun, 94/05/31
c
c version 3.14:
c	improved handling of missing data
c	height correction of data to reference altitude
c	NRS, 95/01/29
c version 3.14a:
c	minor check on consistency of altitude parameters
c	NRS, 95/02/26
c version 3.15:
c	mainly code cleansing
c	NRS, 95/09/07
C ----------------------------------------------------------------------
      BLOCK DATA KARINI
C ----------------------------------------------------------------------

c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c ----------------------------------------------------------------------
c     common inimag - initial states
c	 ism	   soil moisture
c	 iuz	   upper zone
c	 ilz	   lower zone
c	 ilakeh    lake level
c	 ismmag	   snow per altitude level
c	 iuzmag    upper zone per altitude level
c	 ilzmag    lower zone per altitude level
c	 ispd	   snow distribution per altitude level and vegetation zone
c	 iwcd	   water content distribution per altitude level and veg.
c	 ilaket    lake temperature per altitude level
c	 iaindex   albedo index
c	 irmag	   routing states
c	 iicmag    interception content
c	 isp	   snow per altitude level
c	 iwc	   snow water content per altitude level

      real ism,iuz,ilz,ilakeh,ismmag,iuzmag,ilzmag,ispd,iwcd,
     *		   ilaket,iaindex,irmag,iicmag,isp,iwc

      common /inimag/ ism,iuz,ilz,ilakeh,ismmag(10,2),
     *             iuzmag(10,2),ilzmag(10,2),
     *             ispd(10,2,0:9),iwcd(10,2,0:9),
     *             ilaket(10),iaindex(10),irmag(5),
     *             iicmag(10,2),isp(10),iwc(10)

c -------------------------------------------------------------------
c     common mag - actual state
c        as inimag, +
c        ba(10)    snow free area per altitude zone

      real sm,uz,lz,lakeh,smmag,uzmag,lzmag,spd,wcd,ba,
     *		   laket,aindex,rmag,icmag,sp,wc

      common /mag/ sm,uz,lz,lakeh,smmag(10,2),
     *             uzmag(10,2),lzmag(10,2),
     *             spd(10,2,0:9),wcd(10,2,0:9),
     *             ba(10),laket(10),aindex(10),rmag(5),
     *             icmag(10,2),sp(10),wc(10)

c -------------------------------------------------------------------
c     common akku - accumulators
c	 akk(20)      accumulators
c	 bmass(0:10)  glacier mass balance, per level
c		      0: average

      real akk,bmass
      common /akku/ akk(20),bmass(0:10)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common aar -    year array of daily data
c	 tdgn	      temperature at weighted station altitude
c	 pdgn	      precipitation at weighted station altitude
c	 qo	      observed runoff
c	 qs	      simulated runoff
c	 wt	      basin mean temperature
c	 wp	      basin mean precipitation
c	 wdi	      difference between observed and simulated runoff
c	 ssm	      snow melt
c	 snd	      snow cover

      real tdgn,pdgn,qo,qs,wp,wt,wdi,ssm,snd

      common /aar/
     *     tdgn(366),pdgn(366),qo(366),qs(366),wp(366),wt(366),wdi(366),
     *	   ssm(366),snd(366)

c -------------------------------------------------------------------
c     common grad -   temperature gradients
c	 tdiff	      temperature per altitude, month, and dry/wet days

      real tdiff

      COMMON/GRAD/TDIFF(0:11,12,2)

c -------------------------------------------------------------------
c     common vegtype - parameters per vegetation type
c	 icmax	       interception storage
c	 cxrel	       melt factor (relative)
c	 tsdiff        offset of zero melt temperature
c	 cvsnow        variation coefficient of snow distribution
c	 fcveg	       field capacity
c	 lpveg	       full evapotranspiration level (rel. to fc)
c	 nostype       number of vegetation types

      real  icmax,cxrel,tsdiff,cvsnow,fcveg,lpveg,epvar
      integer nostype

      common /vegtype/ icmax(15),cxrel(15),tsdiff(15),cvsnow(15),
     *                 fcveg(15),lpveg(15),epvar(15),nostype

c -------------------------------------------------------------------
c     common zones - parameters per altitude zone
c	 hoh	       mean altitude
c	 zarea	       area fraction within zone
c	 brepro        part of zone area that is glacier covered
c	 tcorr	       actual temperature correction
c	 pcorr	       actual precipitation correction
c	 vegt	       vegetation types
c	 vega	       vegetation areas
c	 nvz	       number of vegetation zones (one or two)
c	 lake	       lake area
c	 sdist	       snow distribution
c	 pact	       actual precipitation
c	 noszone       number of altitude zones

      real  hoh,zarea,brepro,pcorr,tcorr,vega,lake,sdist,pact
      integer vegt,nvz,noszone

      common /zones/ hoh(10),zarea(10),brepro(10),pcorr(10),tcorr(10),
     *                 vegt(10,2),vega(10,2),nvz(10),lake(10),
     -		       sdist(10,2,9),pact(10),noszone

c -------------------------------------------------------------------
c     common climcha - climatic change parameters
c	 ktkorr        temperature offset per month
c	 kpkorr        precipitation change (relative)

      real ktkorr,kpkorr

      common /climcha/ ktkorr(12),kpkorr(12)

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

      DATA IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA
     *           /5,6,16,3,2,21,63,4/
      data ism,iuz,ilz,ilakeh /4*0.0/
      data ismmag,iuzmag,ilzmag,
     *     iicmag,iaindex,isp,iwc /110*0.0/
      data spd,wcd /400*0.0/
      data rmag /5*0.0/

c     set normal distribution parameters
c      data snofrac /0.01,0.04,0.1,0.2,0.3,0.2,0.1,0.04,0.01/
c      data xnorm/2.7,1.88,1.3,0.85,0,-0.85,-1.3,-1.88,-2.7/

      END

C ----------------------------------------------------------------------
      PROGRAM KARMEN
C ----------------------------------------------------------------------
C
C     PROGRAM FOR MODELL-SIMULERINGER
C

c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

c -------------------------------------------------------------------
c     local variables
c -------------------------------------------------------------------
      INTEGER STATUS,simno,indef,lmod
      INTEGER MODELL,ANALYS,INTER,FEILF
      INTEGER j,idum,mind(30)
      REAL    f(5), dum

      LOGICAL STND,NYST,START,filex
      CHARACTER FELT*4,sfelt*4,text*8,deffile*30,prgver*4,prgdat*8

c --------------- 
c     Functions
c----------------
      INTEGER IJANEI



      EQUIVALENCE (INTER,IND(18))
      EQUIVALENCE (MODELL,IND(13)),(ANALYS,IND(14)),(FEILF,IND(15))
      DATA INDEF/62/
      data prgver/"3.15"/, prgdat/"95/09/19"/
C
      STND=.FALSE.
      IND(13) = 1
      IND(14) = 1
      IND(16) = 21
      IND(18) = 1
      IND(22) = 1

      INQUIRE(FILE='DEFAULT.DAT',EXIST=FILEX)
      IF (.NOT.FILEX) then
         INQUIRE(FILE='default.dat',EXIST=FILEX)
         deffile = 'default.dat'
      else
         deffile = 'DEFAULT.DAT'
      endif

      OPEN(INDEF,FILE=deffile,STATUS='UNKNOWN')
      READ (INDEF,1000,END=5092)TEXT,IND
 1000 FORMAT (1A8,I4,2(I4,3I2) ,I2,2X,20I2)
      IF (IND(2).GT.0) GOTO 6000
 5092 CONTINUE
      CLOSE(INDEF)
      OPEN(LULIST,FILE='PRTFIL.RES',STATUS='UNKNOWN')

      FELT='    '
      NYST=.TRUE.

c      CALL BLANK5(IN)

      call header(prgver,prgdat)

      WRITE (*,*) ' '
   10 CONTINUE
      SFELT=FELT
      LMOD=MODELL
      START=.TRUE.
      WRITE(*, 5001)
5001  FORMAT(' Simulation for catchment (enter 4 first ch',
     *    ' - case sensitive):')
      read (*,'(A)') felt
      if (felt .eq. ' ') then
	 write (*,'(A)') ' Try again ...'
	 goto 10
      endif

      IF (FELT.NE.SFELT) NYST=.TRUE.

 5298 WRITE (*,5299)
 5299 FORMAT(' Enter identification no for simulation:')
      read (*,*,err=5298) simno
      if (simno .lt. 1 .or. simno .gt. 999) then
	 write (*,'(A)') ' Try again ...'
	 goto 5298
      endif

5098  continue
      WRITE(*, 5099)
5099  FORMAT(' Enter data file name:')
      read (*,'(A)') filn
      if (filn .eq. ' ') then
	 write (*,'(A)') ' Try again ...'
	 goto 5098
      endif

      INQUIRE(FILE=filn,EXIST=FILEX)
      IF (.NOT.FILEX)then
	 write (*,'(3a)') ' **** finner ikke ',filn,' ****'
	 goto 5098
      endif

      STND=.FALSE.
C     GOTO (5060,5200,10)
C    -     IJANEI(OUT,'$VIL DU FØLGE STANDARDPROSEDYRE ?')
C5060 STND=.TRUE.
      GOTO 5202
 5200 CONTINUE
      WRITE (*,5201)
 5201 FORMAT(/,' Choose model variant: '//
     *         '      1 = HBV3 model, ballistic run',/,
     *         '     11 = HBV3 model with feedback'//)
      CALL INTIN(OUT,IND(13),'$Choose:',1,11,1,*10)
      IF((IND(13) .NE. 1) .AND. (IND(13) .NE. 11)) GOTO 5200
      IND(20)=IND(13)/10
      IND(13)=IND(13)-IND(20)*10
      IF (MODELL.NE.LMOD) NYST=.TRUE.
      IF (NYST) THEN
	 CALL PARINN(FELT,STATUS)
	 IF (STATUS.EQ.1) GOTO 10
	 IF (STATUS.GE.2) GOTO 999
      ENDIF
 5202 CONTINUE
c      CALL BLANK5(IN)
      WRITE(*, 5005)
5005  FORMAT (/' Simulation type; 1 = Single run',/
     *         18X,'2 = Error topography',/,
     *         18X,'3 = Optimization, Rosenbrock',/,
     *         18X,'4 = Optimization, bottom')
c nrs     *         15X,'5 = OPPDATERING (INNLEGGING AV DATA OG SIMULERING)',
c nrs     *' / PROGNOSE',/,
c nrs     *         15X,'6 = BARE PROGNOSE',/,
c nrs     *         14X,'10 = SLETTING AV DATA PÅ KARMENS DATAFIL',/
c nrs     *        14X,'11 = INNLEGGING AV DATA PÅ KARMENS DATAFIL',///)
      CALL INTIN(OUT,IND(14),'$Choose:',1,4,1,*5202)
 5068 CONTINUE

      IF (ANALYS.EQ.10) THEN
C         CALL SLETT(LUDATA,IND(1),IN,OUT)
	  write (*,'(A)') 'Sletting ikke implementert'
	 GOTO 10
      ENDIF
      IF (ANALYS.EQ.11) THEN
c nrs          CALL OPPDAT(.FALSE.,STATUS,.FALSE.)
	 write (*,'(A)') 'Oppdatering ikke implementert'
	 GOTO 10
      ENDIF
      IF (IND(14).NE.3.AND. IND(14).NE.4) GOTO 5007
      WRITE(*, 5006)
5006  FORMAT
     *    (' Error function:  1 = sum squares of relative deviations',/,
     *     '                  2 = sum squares ',/,
     *     '                  3 = sum abs. deviations')
      CALL INTIN(OUT,IND(15),'$Choose:',1,3,2,*5202)
      if (ind(15) .eq. 1) ind(15) = 0
      if (ind(15) .eq. 3) ind(15) = 4
      GOTO 6000

5007  CONTINUE
6000  CONTINUE

      IF (NYST) THEN
	 CALL PARINN(FELT,STATUS)
	 IF (STATUS.EQ.1) GOTO 10
	 IF (STATUS.GE.2) GOTO 999
      ENDIF
      
 100  CONTINUE
      
      IF (START) THEN
	 DO 41 J=1,30
 41      MIND(J)=IND(J)
      ELSE
	 DO 42 J=10,30
 42      IND(J)=MIND(J)
	 GOTO (43,5302,100)
     -   IJANEI(OUT,'$Finished with this catchment?')
 43      CONTINUE
	 GOTO (10,999,100)
     -   IJANEI(OUT,'$More runs?')
      ENDIF
 5302 START=.FALSE.
C
C        INNLEGGING AV DATOER
C

 5091 WRITE(*,5002)
 5002 FORMAT (/,' Start simulation; day, month, year : ')
      READ (*,*,END=5072) IND(4),IND(3),IND(2)
      IF (IND(2) .LT. 100) IND(2)=IND(2)+1900
      IF (IND(3).GT.12.OR.IND(4).GT.31) THEN
	 WRITE(*,5073)
 5073    FORMAT('**** Error in date, try again ****')
	 GOTO 5091
      ENDIF
 5072 CONTINUE

      IF (ANALYS.GE.6) GOTO 5076

 145  WRITE(*,5003)
 5003 FORMAT (/,' End simulation; day, month, year :')
      READ (*,*,END=5075) IND(8),IND(7),IND(6)
      IF (IND(7).GT.12.OR.IND(8).GT.31) THEN
	 WRITE(*,5073)
	 GOTO 145
      ENDIF
      IF (IND(6) .LT. 100) IND(6)=IND(6)+1900
      GOTO 5076
 5075 CONTINUE
      GOTO 145

 5076 CONTINUE
C
      CALL INIT(0,IDUM,DUM)
      CALL PARTAB(.FALSE.)
C
      GO TO (20,30,40,40) ANALYS
C
C     ENKELTKJØRING
C
  20  CONTINUE
C

      CALL INIT (0,IDUM,DUM)
C
c      CALL BLANK5(IN)
C
      CALL SIMUL(simno,F,STATUS,1)
C
C      simno = simno+1
      IF(STATUS.NE.0) GO TO 10
      GO TO 100
C... FEILFUNKSJONSTOPOGRAFI
  30  CONTINUE
      IND(16)=0
      IND(17)=0
      STATUS =0
C     WRITE(*,*) 'FEILFUNKSJONSTOPOLOGI ER TATT UT'
      CALL TOPO(IN,OUT,LULIST,INTER,STATUS)
      IF (STATUS .EQ. 3) GOTO 100
      IF (STATUS .EQ. 2) GOTO 10
      IF(STATUS.NE.0) GO TO 999
      GO TO 30
C... OPTIMERING
  40  CONTINUE
C... NULLER UT SKRIVE- OG PLOTTEOPSJONER
      IND(16)=0
      IND(17)=0
      IF (ANALYS.EQ.3) THEN
	 CALL OPTIMA(IN,OUT,INTER,FEILF,PAR,120,STATUS,TEXT)
C	  WRITE(*,*) 'DENNE OPSJON ER TATT UT'
      ELSE
	 CALL OPTIMB(IN,OUT,INTER,FEILF,PAR,120,STATUS,TEXT)
c	  WRITE(*,*) 'DENNE OPSJON ER TATT UT'
      ENDIF
      IF (STATUS .EQ. 3) GOTO 100
      IF (STATUS .EQ. 2) GOTO 10
      IF(STATUS.NE.0) GO TO 999
      GOTO 100
C
C ... TERMINERING AV KJØRING
C
 999  CONTINUE
      CLOSE(LUSIM)
      IF (INTER .EQ. 0) GOTO 9997
9990  CONTINUE

      goto 9993

      WRITE(*,9991)
9991  FORMAT(/' You can now store states at the end of the period'/
     *        ' and later start new simulation with these states'/)
      GOTO (9992,9993,9990) IJANEI(OUT,'$Do you want to do this?')
9992  CONTINUE
      WRITE(*,*) ' OK - storing states on TILST.DAT'
      CALL MAGUT(STATUS)
9993  CONTINUE
      WRITE(*,9994)
9994  FORMAT (/,' Result printout is stored (PRTFIL.RES)'/)
      GOTO (9997,9996,999) IJANEI(OUT,'$Do you want to keep the file?')
9997  CONTINUE
      WRITE(*,*) ' OK - file is stored '
      CLOSE(LULIST,STATUS='KEEP')
      GOTO 9999
9996  CONTINUE
      WRITE(*,*) ' OK - file is deleted '
      CLOSE(LULIST,STATUS='DELETE')
9999  CONTINUE
      END


C ----------------------------------------------------------------------
      SUBROUTINE DAGNR(DAG,MND,AAR,NR)                                  DAGNR
C ----------------------------------------------------------------------
C
C    KONVERTERER ANGITT DATO TIL DAGNR ELLER ANGITT DAGNR TIL DATO      DAGNR
C    HVIS DAG ELLER MND ER LIK NULL.                                    DAGNR
      INTEGER DAG,MND,NR,DAGER,AAR                                      DAGNR
      INTEGER is,n,iskudd,k,mdag,i
c--------------
c     Functions
c--------------
      INTEGER isk

      DIMENSION DAGER(13)                                               DAGNR
      DATA DAGER/0,31,59,90,120,151,181,212,243,273,304,                DAGNR
     * 334,365/                                                         DAGNR
      IS=ISK(AAR)                                                       DAGNR
      IF(DAG.EQ.0.OR.MND.EQ.0) GO TO 10                                 DAGNR
      NR=DAGER(MND)+DAG                                                 DAGNR
      IF(MND.GT.2)NR=NR+IS                                              DAGNR
      RETURN                                                            DAGNR
  10  CONTINUE                                                          DAGNR
      N=NR                                                              DAGNR
      IF (N.GT.365+IS) N=N-365-IS                                       DAGNR

      iskudd = 0     
      DO 20 I=2,13                                                      DAGNR
	 MDAG=DAGER(I)+iskudd
	 K=I-1
	 IF (N.LE.MDAG) GOTO 30
	 iskudd = is
  20  CONTINUE

  30  MND=K                                                             DAGNR
      DAG=N-DAGER(K)                                                    DAGNR
      IF (MND.GT.2) DAG=DAG-IS                                          DAGNR
      RETURN                                                            DAGNR
      END                                                               DAGNR

      INTEGER FUNCTION ISK(IA)                                          
C                                                                       ISK
C     FUNKSJON                                                          ISK
C        & TESTE P& OM &RET IA ER SKUDD&R I HH. TIL DEN GREGORIANSKE    ISK
C        KALENDEREN.                                                    ISK
C        HVIS JA , SETTES ISK=1                                         ISK
C        HVIS NEI, SETTES ISK=0                                         ISK
C                                                                       ISK
      INTEGER ia
      IF(IA-IA/4*4)3,1,3                                                ISK
    1 IF(IA-IA/100*100)4,2,4                                            ISK
    2 IF(IA-IA/400*400)3,4,3                                            ISK
    3 ISK=0                                                             ISK
      RETURN                                                            ISK
    4 ISK=1                                                             ISK
      RETURN                                                            ISK
      END                                                               ISK


C ----------------------------------------------------------------------
      SUBROUTINE PARINN (FELT,STATUS)
C ----------------------------------------------------------------------
C
c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

c -------------------------------------------------------------------
c     common vegtype - parameters per vegetation type
c	 icmax	       interception storage
c	 cxrel	       melt factor (relative)
c	 tsdiff        offset of zero melt temperature
c	 cvsnow        variation coefficient of snow distribution
c	 fcveg	       field capacity
c	 lpveg	       full evapotranspiration level (rel. to fc)
c	 nostype       number of vegetation types

      real  icmax,cxrel,tsdiff,cvsnow,fcveg,lpveg,epvar
      integer nostype

      common /vegtype/ icmax(15),cxrel(15),tsdiff(15),cvsnow(15),
     *                 fcveg(15),lpveg(15),epvar(15),nostype

c -------------------------------------------------------------------
c     common zones - parameters per altitude zone
c	 hoh	       mean altitude
c	 zarea	       area fraction within zone
c	 brepro        part of zone area that is glacier covered
c	 tcorr	       actual temperature correction
c	 pcorr	       actual precipitation correction
c	 vegt	       vegetation types
c	 vega	       vegetation areas
c	 nvz	       number of vegetation zones (one or two)
c	 lake	       lake area
c	 sdist	       snow distribution
c	 pact	       actual precipitation
c	 noszone       number of altitude zones

      real  hoh,zarea,brepro,pcorr,tcorr,vega,lake,sdist,pact
      integer vegt,nvz,noszone

      common /zones/ hoh(10),zarea(10),brepro(10),pcorr(10),tcorr(10),
     *                 vegt(10,2),vega(10,2),nvz(10),lake(10),
     -		       sdist(10,2,9),pact(10),noszone

c -------------------------------------------------------------------
c     common climcha - climatic change parameters
c	 ktkorr        temperature offset per month
c	 kpkorr        precipitation change (relative)

      real ktkorr,kpkorr

      common /climcha/ ktkorr(12),kpkorr(12)

c -------------------------------------------------------------------
c     local variables
c -------------------------------------------------------------------
      integer inter,modell,itype
      real sjopro, magpro,xhoh,vegpar(7)

      INTEGER STATUS
      LOGICAL FILEX
      CHARACTER TEXT*80,SISTID*8,NAVNID*8,FELT*4
      CHARACTER FELTS*4,parfile*30,clifile*30,vegfile*30
      integer nrf,nr,ined,i,j,k,itemp,itils,inn

      real parbuf(200), parv
      EQUIVALENCE (IND(18),INTER),(IND(13),MODELL)
      equivalence (par(3),sjopro), (par(4),magpro)
      DATA SISTID/'FINIS'/,NAVNID/'START'/
      data nedid /25*0/,tempid /25*0/, tilsid /4*0/
      data snofrac /0.01,0.04,0.1,0.2,0.3,0.2,0.1,0.04,0.01/
      data xnorm/2.7,1.88,1.3,0.85,0,-0.85,-1.3,-1.88,-2.7/
      data par/120*0.0/, parbuf/200*0.0/
C
      STATUS=0
      NRF=0
      MODELL = 1
      INQUIRE(FILE='PARAM.DAT',EXIST=FILEX)
      IF (.NOT.FILEX) then
         INQUIRE(FILE='param.dat',EXIST=FILEX)
         if (filex) then
            parfile = 'param.dat'
         else
            goto 8888
         endif
      else
         parfile = 'PARAM.DAT'
      endif

 5300 CONTINUE
      OPEN(LUPAR,FILE=parfile,ACCESS='SEQUENTIAL',STATUS='OLD',
     * FORM='FORMATTED',IOSTAT=STATUS,ERR=8888)
      REWIND LUPAR

C...  Leser evt. kommentarer først på parameterfilen.
C nrs      CALL LESKOM (OUT,LUPAR,'PARAM.DAT',1,STSTR,SLSTR,.FALSE.,34,IFEIL)

C...  SØKER PARAMETERFIL.
 5025 READ(LUPAR,5030,ERR=5031,END=5032) TEXT,NR,FELTN
 5039 FORMAT(1X,I4,1X,A4,1X,I1,1X,I1,F8.2,7(1X,I4,1X,F3.1))
 5030 FORMAT(A5,I3,A12)

      IF(TEXT(1:5).NE.NAVNID) GO TO 5025
C
C      NYTT FELT PÅ PARAMETERFIL
C
      FELTS=FELTN(1:4)
      write (*,5039) NR,FELTS
      NRF=NRF+1
      IF(FELT.NE.FELTS) GOTO 5025
C
C     RETT FELT OG MODELL
C
      IND(1)=NR
C
C     reads header information
c
      xhoh = 0
      read (lupar,'(8x,i13)') ined          
      do 1100 j = 1,ined
	 read (lupar,'(8x,A13)') text
	 read (lupar,'(8x,F13.0)') nedhoh(j)
	 read (lupar,'(8x,F13.0)') nedv(j)
c         if (text.eq.' ') goto 1100
	 xhoh = xhoh + nedhoh(j)*nedv(j)
	 nedid(j) = j
 1100 continue
      refp = xhoh
      xhoh = 0
      read (lupar,'(8x,i13)') itemp          
      do 1110 j = 1,itemp
	 read (lupar,'(8x,A13)') text
	 read (lupar,'(8x,F13.0)') tmphoh(j)
	 read (lupar,'(8x,F13.0)') tempv(j)
c         if (text.eq.' ') goto 1110
	 xhoh = xhoh + tmphoh(j)*tempv(j)
	 tempid(j) = j
 1110 continue
      reft = xhoh
      read (lupar,'(8x,i13)') itils
      do 1115 j = 1,itils
	 read (lupar,'(8x,A13)') text
	 read (lupar,'(8x,F13.0)') tilsv(j)
	 tilsid(j) = j
 1115 continue
      read (lupar,'(8x,F13.0)') par(1)
 1000 continue
 5678 READ(LUPAR,'(A)',end=5031) text
      IF(TEXT(1:5) .EQ. SISTID) GO TO 1010
c      write (*,'(a)') text
      read (text,'(3x,i5,f13.0)') inn, parv
      if (inn .lt. 1) goto 5678
      if (inn .gt. 160) goto 5031
      parbuf(inn) = parv
      goto 1000

 1010 continue

      do 1001 j = 3,120
      par(j) = parbuf(j)
 1001 continue

	
c---> convert hypsographic curve

      do 1008 j = 1,10
	 if (parbuf(5+j).lt. 1.0) goto 1005
	 hoh(j) = (parbuf(4+j)+parbuf(5+j))*0.5
	 zarea(j)  = parbuf(16+j)-parbuf(15+j)
	 if (zarea(j).gt.0.01) then
	    brepro(j) = (parbuf(27+j)-parbuf(26+j))/zarea(j)
	 else
	    brepro(j) = 0.0
	 endif
	 if (brepro(j).gt.1.0) brepro(j)=1.0
	 noszone = j
 1008 continue

 1005 continue
      
c---> set vegetation type etc

      sjopro = 0.0

      do 1002 i=1,noszone
	 vegt(i,1) = ifix(parbuf(121+(i-1)*4)+0.5) 
	 vegt(i,2) = ifix(parbuf(122+(i-1)*4)+0.5)
	 vega(i,2) = parbuf(123+(i-1)*4)
	 lake(i)   = parbuf(124+(i-1)*4)
	 if (vegt(i,2).eq.0) vega(i,2) = 0.0
	 if (vega(i,2).eq. 0.0) then
	    nvz(i) = 1
	 else
	    nvz(i) = 2
	 endif
	 vega(i,1) = 1.0 - vega(i,2) - lake(i)
	 sjopro = sjopro + lake(i)*zarea(i)
 1002 continue

      if (magpro .gt. sjopro) magpro = sjopro
c
c     setter FCDEL til 1 dersom den ikke er satt i parameterfil
c
      if (par(80).eq.0.0) par(80) = 1.0
c
c     setter default sjøparametre (lineært reservoir)
c
      if (sjopro .gt. 0.0 .and. par(56) .eq. 0.0) then
c	 par(56) = par(89)*1000.0/86.4
	 par(56) = par(89)*par(1)*1000.0/86.4
	 par(57) = 0.0
	 par(58) = 1.0
	 par(59) = 0.0
      endif

      close(lupar)

c---> read vegetation types
      INQUIRE(FILE='VEGTYPE.DAT',EXIST=FILEX)
      IF (.NOT.FILEX) then
         INQUIRE(FILE='vegtype.dat',EXIST=FILEX)
         if (filex) then
            vegfile = 'vegtype.dat'
         else
            write (*,'(A)') ' **** file VEGTYPE.DAT missing ****'
	    modell = 0
         endif
      else
         vegfile = 'VEGTYPE.DAT'
      endif

      if (modell.ge.0) then
      OPEN(LUPAR,FILE=vegfile,ACCESS='SEQUENTIAL',STATUS='OLD',
     * FORM='FORMATTED',IOSTAT=STATUS,ERR=8888)
	 REWIND LUPAR

c--->    skip comment line
      
	 read (lupar,'(1x)',end=9999)
	 do 1003 j=1,15
	    read (lupar,'(10x,i3,7F8.0)',end=1004,err=9999)
     *           itype,(vegpar(k),k=1,7) 
     
	    icmax(itype) = vegpar(1)
	    if (vegpar(2).gt.0.0) then
	       cxrel(itype) = vegpar(2)
	    else
	       cxrel(itype) = 1.0
	    endif
	    tsdiff(itype) = vegpar(3)
	    if (vegpar(4).gt.0.0) then
	       cvsnow(itype) = vegpar(4)
	    else
	       cvsnow(itype) = 0.5
	    endif
	    cvsnow(itype) = vegpar(4)
	    if (vegpar(5).gt.0.0) then
	       fcveg(itype) = vegpar(5)*par(79)
	    else
	       fcveg(itype) = par(79)
	    endif
	    if (vegpar(6).gt.0.0) then
	       lpveg(itype) = vegpar(6)
	    else
	       lpveg(itype) = par(80)
	    endif
	    if (vegpar(7).gt.0.0) then
	       epvar(itype) = vegpar(7)
	    else
	       epvar(itype) = 1.0
	    endif

	    nostype = j
 1003    continue
 1004    continue       
	 close(lupar)
      else
	 icmax(1) = 0.0
	 cxrel(1) = 1.0
	 tsdiff(1) = 0.0
	 cvsnow(1) = 0.5
	 fcveg(1) = par(79)
	 lpveg(1) = par(80)
	 epvar(1) = 1.0
	 nostype =1
      endif

c---> read climate change profile
      INQUIRE(FILE='CLIMCHA.DAT',EXIST=FILEX)
      IF (.NOT.FILEX) then
         INQUIRE(FILE='climcha.dat',EXIST=FILEX)
         if (filex) then
            clifile = 'climcha.dat'
         endif
      ELSE
         clifile ='CLIMCHA.DAT'
      ENDIF
      IF (.NOT.FILEX) then
	 write (*,'(A)') ' **** file CLIMCHA.DAT missing ****'
	 write (*,'(A)') '   can not make climate change runs  '
	 pause
	 do 2222 j=1,12
	    ktkorr(j) = 0.0
	    kpkorr(j) = 1.0
 2222	 continue
      endif

      OPEN(LUPAR,FILE=clifile,ACCESS='SEQUENTIAL',STATUS='OLD',
     *   FORM='FORMATTED',IOSTAT=STATUS,ERR=8888)
	    REWIND LUPAR

c--->    skip comment line
      
	 read (lupar,'(1x)',end=9998)
	 do 1009 j=1,12
	   read (lupar,'(3x,2(F8.0))',end=9998,err=9998)
     *           ktkorr(j),kpkorr(j)
 1009    continue
	 close(lupar)

c      write (*,'(A)') 'exit parinn'
      RETURN

 5031 WRITE(*,5033)
 5033 FORMAT(' **** Reading error on parameter file ****')
      CLOSE (LUPAR)
      STATUS=2
      RETURN

 9999 continue
      write (*,'(A)') ' **** Reading error on VEGTYPE.DAT ****'
      close (lupar)
      status = 2 
      RETURN

 9998 continue
      write (*,'(A)') ' **** Reading error on CLIMCHA.DAT ****'
      close (lupar)
      status = 2 
      RETURN

5032  continue
      WRITE(*,5034) FELT
 5034 FORMAT(' *** Cant find ',A4,' on parameter file PARAM.DAT ***')
      CLOSE (LUPAR)
   10 STATUS=1
      RETURN

 8888 continue
      WRITE(*,8889) STATUS
8889  FORMAT ('  **** FEIL PÅ PARAMETERFIL, STATUS=',I4,' ****')
  999 STATUS=2
120   CONTINUE

      CLOSE(LUPAR)
      RETURN
      END


C ----------------------------------------------------------------------
      SUBROUTINE INIT(N,IX,X)
C ----------------------------------------------------------------------
C
C... INITIERING AV PARAMETERVERDIER F\R KALL P] SIMULER
c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c ----------------------------------------------------------------------
c     common inimag - initial states
c	 ism	   soil moisture
c	 iuz	   upper zone
c	 ilz	   lower zone
c	 ilakeh    lake level
c	 ismmag	   snow per altitude level
c	 iuzmag    upper zone per altitude level
c	 ilzmag    lower zone per altitude level
c	 ispd	   snow distribution per altitude level and vegetation zone
c	 iwcd	   water content distribution per altitude level and veg.
c	 ilaket    lake temperature per altitude level
c	 iaindex   albedo index
c	 irmag	   routing states
c	 iicmag    interception content
c	 isp	   snow per altitude level
c	 iwc	   snow water content per altitude level

      real ism,iuz,ilz,ilakeh,ismmag,iuzmag,ilzmag,ispd,iwcd,
     *		   ilaket,iaindex,irmag,iicmag,isp,iwc

      common /inimag/ ism,iuz,ilz,ilakeh,ismmag(10,2),
     *             iuzmag(10,2),ilzmag(10,2),
     *             ispd(10,2,0:9),iwcd(10,2,0:9),
     *             ilaket(10),iaindex(10),irmag(5),
     *             iicmag(10,2),isp(10),iwc(10)

c -------------------------------------------------------------------
c     common mag - actual state
c        as inimag, +
c        ba(10)    snow free area per altitude zone

      real sm,uz,lz,lakeh,smmag,uzmag,lzmag,spd,wcd,ba,
     *		   laket,aindex,rmag,icmag,sp,wc

      common /mag/ sm,uz,lz,lakeh,smmag(10,2),
     *             uzmag(10,2),lzmag(10,2),
     *             spd(10,2,0:9),wcd(10,2,0:9),
     *             ba(10),laket(10),aindex(10),rmag(5),
     *             icmag(10,2),sp(10),wc(10)

c -------------------------------------------------------------------
c     common akku - accumulators
c	 akk(20)      accumulators
c	 bmass(0:10)  glacier mass balance, per level
c		      0: average

      real akk,bmass
      common /akku/ akk(20),bmass(0:10)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common aar -    year array of daily data
c	 tdgn	      temperature at weighted station altitude
c	 pdgn	      precipitation at weighted station altitude
c	 qo	      observed runoff
c	 qs	      simulated runoff
c	 wt	      basin mean temperature
c	 wp	      basin mean precipitation
c	 wdi	      difference between observed and simulated runoff
c	 ssm	      snow melt
c	 snd	      snow cover

      real tdgn,pdgn,qo,qs,wp,wt,wdi,ssm,snd

      common /aar/
     *     tdgn(366),pdgn(366),qo(366),qs(366),wp(366),wt(366),wdi(366),
     *	   ssm(366),snd(366)

c -------------------------------------------------------------------
c     common grad -   temperature gradients
c	 tdiff	      temperature per altitude, month, and dry/wet days

      real tdiff

      COMMON/GRAD/TDIFF(0:11,12,2)

c -------------------------------------------------------------------
c     common vegtype - parameters per vegetation type
c	 icmax	       interception storage
c	 cxrel	       melt factor (relative)
c	 tsdiff        offset of zero melt temperature
c	 cvsnow        variation coefficient of snow distribution
c	 fcveg	       field capacity
c	 lpveg	       full evapotranspiration level (rel. to fc)
c	 nostype       number of vegetation types

      real  icmax,cxrel,tsdiff,cvsnow,fcveg,lpveg,epvar
      integer nostype

      common /vegtype/ icmax(15),cxrel(15),tsdiff(15),cvsnow(15),
     *                 fcveg(15),lpveg(15),epvar(15),nostype

c -------------------------------------------------------------------
c     common zones - parameters per altitude zone
c	 hoh	       mean altitude
c	 zarea	       area fraction within zone
c	 brepro        part of zone area that is glacier covered
c	 tcorr	       actual temperature correction
c	 pcorr	       actual precipitation correction
c	 vegt	       vegetation types
c	 vega	       vegetation areas
c	 nvz	       number of vegetation zones (one or two)
c	 lake	       lake area
c	 sdist	       snow distribution
c	 pact	       actual precipitation
c	 noszone       number of altitude zones

      real  hoh,zarea,brepro,pcorr,tcorr,vega,lake,sdist,pact
      integer vegt,nvz,noszone

      common /zones/ hoh(10),zarea(10),brepro(10),pcorr(10),tcorr(10),
     *                 vegt(10,2),vega(10,2),nvz(10),lake(10),
     -		       sdist(10,2,9),pact(10),noszone

c -------------------------------------------------------------------
c     common climcha - climatic change parameters
c	 ktkorr        temperature offset per month
c	 kpkorr        precipitation change (relative)

      real ktkorr,kpkorr

      common /climcha/ ktkorr(12),kpkorr(12)

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

c----------------------------------------------------------------------------
c    local variables
c---------------------------------------------------------------------------

      INTEGER      N,IX(10)
      REAL         X(10), tgrad(12)
      real         pro,sjopro,magpro

      integer      modell,klikor
      real         dw
      real         htkorr,vtkorr,pgrad,gradalt,pgrad1,pkorr,tk,xvol,
     *		   spdist

      integer      i,j,k,jmnd,iveg
    

      EQUIVALENCE (IND(13),MODELL),(IND(28),KLIKOR)
      equivalence (par(3),sjopro),(par(4),magpro),(par(44),dw)
      EQUIVALENCE (PAR(101),TGRAD(1))

C     BEREGNER NEDBØR- OG TEMPERATURKORREKSJONER
c      write (*,'(a)') ' inn i init'

      if (ism .eq. 0.0) then
	 if (par(114).eq.0.0) then
	    ism = par(79)*0.7
	 else
	    ism = par(114)
	 endif
	 iuz = par(115)
	 ilz = par(116)
	 ilakeh = par(117)
      endif

      PRO=DW*100.

      if (tgrad(1).eq.0.0) then
	 do 100 j=1,12
	    tgrad(j) = -0.6
 100     continue
      endif
c      write (*,*) tgrad

      HTKORR=PAR(63)
      VTKORR=PAR(64)
      PGRAD =PAR(65)
      GRADALT = par(47)
      PGRAD1 =  par(48)
      PKORR = par(45)
c      write (*,*) reft,refp,htkorr,vtkorr,afelt
      DO 801 JMND=1,12
	 TDIFF(0,JMND,1)=0.0
	 TDIFF(0,JMND,2)=0.0
	 TDIFF(11,JMND,2)=(REFP-REFT)/100.*TGRAD(JMND)*VTKORR/(-0.6)
801   CONTINUE

      DO 800 I=1,noszone
	 TK=HTKORR*(hoh(i)-REFT)/100.
	 DO 802 JMND=1,12
	   TDIFF(I,JMND,1)=TGRAD(JMND)*(hoh(i)-REFT)/100*HTKORR/(-0.6)
	   TDIFF(I,JMND,2)=TGRAD(JMND)*(hoh(i)-REFT)/100*VTKORR/(-0.6)
	   TDIFF(0,JMND,1)=TDIFF(0,JMND,1)+TDIFF(I,JMND,1)/noszone
	   TDIFF(0,JMND,2)=TDIFF(0,JMND,2)+TDIFF(I,JMND,2)/noszone
 802     CONTINUE
      
      if (gradalt.eq.0.0 .or. hoh(i).lt.gradalt) then
c-->     below gradient change point
	 PCORR(i) = (hoh(i)-REFP)*pgrad/100.0 + 1.0
      else
	 pcorr(i) = (gradalt-refp)*pgrad/100.0 + 
     *    (hoh(i)-gradalt)*pgrad1/100 + 1.0
      endif
      
      pcorr(i) = pcorr(i)*pkorr
      
800   CONTINUE

c-->  check ice melt factor
      if (par(66).eq.0.0) par(66) = 1.0

c-->  calculate snow distribution
     
      do 810 i = 1,noszone
	 do 811 j = 1,nvz(i)
	    iveg = vegt(i,j)
	    if (iveg .gt. 0) then
	       if (iveg.gt.nostype) then
		   write (*,'(a)') 
     *                ' **** inconsistency between parameter',
     *                '  and vegetation file ****'
		   stop
	       endif
	       xvol = 0.0
	       do 812 k = 1,9
		  sdist(i,j,k) = exp(xnorm(k)*cvsnow(iveg))
		  xvol = xvol + sdist(i,j,k)*snofrac(k) 
  812          continue
	       do 813 k = 1,9
		  sdist(i,j,k) = sdist(i,j,k)/xvol
  813          continue  
	    endif
  811    continue
C
C      SJEKKER OM SN\MAGASINET SKAL FORDELES
C
c        write (*,'(A)') ' sjekker sne'
	 IF (SP(i).GT.0.0.AND.SPD(i,1,0).EQ.0.0
     *     .AND.SDIST(i,1,9).GT.0.0)
     *     CALL SNDIST(SP,WC,BA,SPD,WCD,10,i,nvz(i),
     *     SDIST,PRO,SPDIST,snofrac)
  810 continue
 

C... GI STARTINNHOLD TIL MAGASINER

      sm = ism
      uz = iuz
      lz = ilz
      if (sjopro.gt.0.0.and.par(58).eq.1.0.and.ilakeh .eq.0.0) then
c     linear reservoir
c nrs	 if (sjopro .gt. magpro) ilakeh = lz*1000./(sjopro - magpro)
	 if (sjopro .gt. magpro) ilakeh = lz/1000./(sjopro - magpro)
      endif

      lakeh = ilakeh

      DO 25 I=1,10
	
      aindex(i) = iaindex(i)
	 laket (i) = ilaket(i)
	 sp(i)     = isp(i)
	 wc(i)     = iwc(i)
	 do 252 j = 1,nvz(i) 
	    smmag(i,j) = ismmag(i,j)
	    uzmag(i,j) = iuzmag(i,j)
	    lzmag(i,j) = ilzmag(i,j)
	    icmag(i,j) = iicmag(i,j)
	    do 251 k = 0,9
	       spd(i,j,k) =ispd(i,j,k)
	       wcd(i,j,k) =iwcd(i,j,k)
 251        continue
 252     continue
  25  continue  

      do 259 j=1,5
  259 rmag(j) = irmag(j)
c ->  sjekker fordelte magasin

      do 26 i = 1,10
	 do 261 j=1,nvz(i)
	 if (smmag(i,j).eq.0.0) smmag(i,j)=sm
	 if (lzmag(i,j).eq.0.0) lzmag(i,j)=lz
 261    continue
   26  continue

      DO 20 I=1,20
20    AKK(I)=0.0

C... GI NYE VERDIER TIL PARAMETERE SOM SKA OPTIMERES
C... HVIS INIT ER KALLT FRA OPTIMA.

      IF(N.EQ.0) GO TO 40
      DO 30 I=1,N
      J=IX(I)
  30  PAR(J)=X(I)
  40  CONTINUE
      IF (sm.GT.PAR(79)) sm=PAR(79)
c      write (*,'(A)') ' exit init'
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE SNDIST(SP,WC,BA,SPD,WCD,IDIM,IFELT,nvz,SDIST,
     *                  PRO,SPDIST,snofrac)
C ----------------------------------------------------------------------
C
C     REFORDELER JEVNT SNØDEKKE TIL FORDELT
C
      integer idim,ifelt
      REAL SP(IDIM),WC(IDIM),SPD(IDIM,2,0:9),WCD(IDIM,2,0:9),
     *   SDIST(idim,2,9),snofrac(9),ba(idim),pro,spdist
      INTEGER nvz(idim)

      integer i,k,ivz
      real xvol,snod,snoe,wm
      
      DO 100 I=1,IFELT
      if (sp(i).gt.spdist) then
	 snod = SP(I)-SPDIST
	 snoe = SPDIST
      else
	 snod = 0.0
	 snoe = SP(i)
      endif

      do 222 ivz = 1,nvz(i)
	 do 223 k = 1,9
	    SPD(I,ivz,k)=snod*SDIST(i,ivz,k)+snoe
 223     continue
	 spd(i,ivz,0) =sp(i)
	 BA(I)=0.0

C        FORDELER EVT. SMELTEVANN
	 xvol = 0.0
	 DO 201 k=1,9
	    WM=SPD(I,ivz,k)*PRO/100.0
	    IF (WC(I).LE.WM) THEN
	       WCD(I,ivz,k) = WC(I)
	    else
	       WCD(I,ivz,k) = WM       
	    ENDIF
	    xvol = xvol + wcd(i,ivz,k)*snofrac(k)
  201    continue     
	 wcd(i,ivz,0) = xvol
  222 continue
  100 CONTINUE
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE RESULT
C ----------------------------------------------------------------------
C
C      SKRIVER SLUTTRESULTATER
C
c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c ----------------------------------------------------------------------
c     common inimag - initial states
c	 ism	   soil moisture
c	 iuz	   upper zone
c	 ilz	   lower zone
c	 ilakeh    lake level
c	 ismmag	   snow per altitude level
c	 iuzmag    upper zone per altitude level
c	 ilzmag    lower zone per altitude level
c	 ispd	   snow distribution per altitude level and vegetation zone
c	 iwcd	   water content distribution per altitude level and veg.
c	 ilaket    lake temperature per altitude level
c	 iaindex   albedo index
c	 irmag	   routing states
c	 iicmag    interception content
c	 isp	   snow per altitude level
c	 iwc	   snow water content per altitude level

      real ism,iuz,ilz,ilakeh,ismmag,iuzmag,ilzmag,ispd,iwcd,
     *		   ilaket,iaindex,irmag,iicmag,isp,iwc

      common /inimag/ ism,iuz,ilz,ilakeh,ismmag(10,2),
     *             iuzmag(10,2),ilzmag(10,2),
     *             ispd(10,2,0:9),iwcd(10,2,0:9),
     *             ilaket(10),iaindex(10),irmag(5),
     *             iicmag(10,2),isp(10),iwc(10)

c -------------------------------------------------------------------
c     common mag - actual state
c        as inimag, +
c        ba(10)    snow free area per altitude zone

      real sm,uz,lz,lakeh,smmag,uzmag,lzmag,spd,wcd,ba,
     *		   laket,aindex,rmag,icmag,sp,wc

      common /mag/ sm,uz,lz,lakeh,smmag(10,2),
     *             uzmag(10,2),lzmag(10,2),
     *             spd(10,2,0:9),wcd(10,2,0:9),
     *             ba(10),laket(10),aindex(10),rmag(5),
     *             icmag(10,2),sp(10),wc(10)

c -------------------------------------------------------------------
c     common akku - accumulators
c	 akk(20)      accumulators
c	 bmass(0:10)  glacier mass balance, per level
c		      0: average

      real akk,bmass
      common /akku/ akk(20),bmass(0:10)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common aar -    year array of daily data
c	 tdgn	      temperature at weighted station altitude
c	 pdgn	      precipitation at weighted station altitude
c	 qo	      observed runoff
c	 qs	      simulated runoff
c	 wt	      basin mean temperature
c	 wp	      basin mean precipitation
c	 wdi	      difference between observed and simulated runoff
c	 ssm	      snow melt
c	 snd	      snow cover

      real tdgn,pdgn,qo,qs,wp,wt,wdi,ssm,snd

      common /aar/
     *     tdgn(366),pdgn(366),qo(366),qs(366),wp(366),wt(366),wdi(366),
     *	   ssm(366),snd(366)

c -------------------------------------------------------------------
c     common grad -   temperature gradients
c	 tdiff	      temperature per altitude, month, and dry/wet days

      real tdiff

      COMMON/GRAD/TDIFF(0:11,12,2)

c -------------------------------------------------------------------
c     common vegtype - parameters per vegetation type
c	 icmax	       interception storage
c	 cxrel	       melt factor (relative)
c	 tsdiff        offset of zero melt temperature
c	 cvsnow        variation coefficient of snow distribution
c	 fcveg	       field capacity
c	 lpveg	       full evapotranspiration level (rel. to fc)
c	 nostype       number of vegetation types

      real  icmax,cxrel,tsdiff,cvsnow,fcveg,lpveg,epvar
      integer nostype

      common /vegtype/ icmax(15),cxrel(15),tsdiff(15),cvsnow(15),
     *                 fcveg(15),lpveg(15),epvar(15),nostype

c -------------------------------------------------------------------
c     common zones - parameters per altitude zone
c	 hoh	       mean altitude
c	 zarea	       area fraction within zone
c	 brepro        part of zone area that is glacier covered
c	 tcorr	       actual temperature correction
c	 pcorr	       actual precipitation correction
c	 vegt	       vegetation types
c	 vega	       vegetation areas
c	 nvz	       number of vegetation zones (one or two)
c	 lake	       lake area
c	 sdist	       snow distribution
c	 pact	       actual precipitation
c	 noszone       number of altitude zones

      real  hoh,zarea,brepro,pcorr,tcorr,vega,lake,sdist,pact
      integer vegt,nvz,noszone

      common /zones/ hoh(10),zarea(10),brepro(10),pcorr(10),tcorr(10),
     *                 vegt(10,2),vega(10,2),nvz(10),lake(10),
     -		       sdist(10,2,9),pact(10),noszone

c -------------------------------------------------------------------
c     common climcha - climatic change parameters
c	 ktkorr        temperature offset per month
c	 kpkorr        precipitation change (relative)

      real ktkorr,kpkorr

      common /climcha/ ktkorr(12),kpkorr(12)

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

c --------------------------------------------------------------------
c     local variables
c --------------------------------------------------------------------

      CHARACTER MODEL*4,DATO*9
      INTEGER ANALYS,modell
      logical bre
      integer      i,j,afelt
      real         sjopro,magpro,fmag,smag,fsnmag,ssnmag,dmag,dsnmag,
     *             feil,runbal,evabal,strbal,snobal,snoadj
      real tsnmag(10),sncov(10),bmout(10)

      EQUIVALENCE (IND(14),ANALYS),(IND(13),MODELL)
CV    DATO=DATE()

c      call blank5(in)
      CALL DATE(DATO)
      MODEL='HBV3'

      bre = .false.
      do 1000 j=1,noszone
 1000 if (brepro(j).gt.0.0) bre = .true.       

      WRITE(LULIST,2000) MODEL,FELTN,DATO
 2000 FORMAT (///,10X,'SIMULATION RESULTS WITH THE ',A4,
     *'-MODEL FOR CATCHMENT:',A12,', RUN ',A9)
CV   *'-MODELLEN  FOR FELT:',A12,', KJØRT ',A10)
      WRITE(*, 2013) AKK(1),AKK(11),AKK(12),
     *    AKK(4),AKK(5),akk(18),akk(3),AKK(2),AKK(7)
      WRITE(LULIST, 2013) AKK(1),AKK(11),AKK(12),
     *    AKK(4),AKK(5),akk(18),AKK(3),AKK(2),AKK(7)
 2013 FORMAT( /,10X,'Accumulated volumes, mm :',/,10X,'Precip.   ',F10.1
     *,'  obs precip.:',F7.1,' of this snow: ',F7.1
     * ,/,10X,'Evapotr. :',F10.1,21X,'     snowmelt: ',F7.1
     * ,/,51X,                        ' glacier melt: ',F7.1
     * ,/,9X,
     *' sim runoff:',F9.1,'  obs runoff: ',F7.1,'   difference: ',F7.1)

      AFELT=noszone
      
      SJOPRO=PAR(3)
      magpro = par(4)

      FMAG = (ism+iuz+ilz)*(1-sjopro)+ilakeh*(sjopro-magpro)*1000.0
      SMAG = (sm+uz+lz)*(1-sjopro) + lakeh*(sjopro-magpro)*1000.0
      FSNMAG=0.0
      SSNMAG=0.0

      DO 501 I=1,noszone
	 FSNMAG=FSNMAG + (isp(i)+ iwc(i))*zarea(i)
	 SSNMAG=SSNMAG + (sp(i) + wc(i))*zarea(i)
	 TSNMAG(I) = sp(i) + wc(i)
	 SNCOV(I)=(1.0-BA(I))*100.0
 501  continue

      FSNMAG=FSNMAG*(1.0-SJOPRO)
      SSNMAG=SSNMAG*(1.0-SJOPRO)

      DMAG=SMAG-FMAG
      DSNMAG=SSNMAG-FSNMAG
      WRITE(*, 2010) IND(4),IND(3),IND(2),IND(8),IND(7),IND(6)
      WRITE(LULIST, 2010) IND(4),IND(3),IND(2),IND(8),IND(7),IND(6)
2010  FORMAT (/,10X,'Initial states:    end states:   change:',/,
     *          15X,I2,'/',I2,1X,I4,5X,I2,'/',I2,1X,I4)
      WRITE(*, 2011)FMAG,SMAG,DMAG
      WRITE(LULIST, 2011)FMAG,SMAG,DMAG
2011  FORMAT(10X,'ground+lake',3F10.1)
      WRITE(*, 2012)FSNMAG,SSNMAG,DSNMAG
      WRITE(LULIST, 2012)FSNMAG,SSNMAG,DSNMAG
 2012 FORMAT (10X,'eff. snow  ',3F10.1)
      if (bre) then
	 write (*,2051) bmass(0),bmass(0)
	 write (lulist,2051) bmass(0),bmass(0)
      endif
 2051 format (10X,'mass bal.  ',10x,2f10.1)

      IF (SP(noszone) .EQ. 0.0 .and. .not. bre) GO TO 2021
      WRITE(*,2017)IND(8),IND(7),IND(6)
      WRITE(LULIST,2017)IND(8),IND(7),IND(6)
 2017 FORMAT (/,4X,'Snow distribution',1X,I2,'/',I2,1X,I4)
      WRITE(*,2018) (hoh(I),I=1,noszone)
      WRITE(LULIST,2018) (hoh(I),I=1,noszone)
      WRITE(LULIST,2055) (zarea(I),I=1,noszone)
 2018 FORMAT (' m asl.  ',10F7.0)
 2055 format (' area    ',10F7.2)
      WRITE(*,2019) (TSNMAG(I),I = 1,noszone)
      WRITE(LULIST,2019) (TSNMAG(I),I = 1,noszone)
 2019 FORMAT (' Tot snow',10F7.0)
      WRITE(*,2020) (wc(I),I=1,noszone)
      WRITE(LULIST,2020) (wc(I),I=1,noszone)
 2020 FORMAT (' Liq. wat',10F7.0)

      WRITE (*,2035) (SNCOV(I),I=1,noszone)
      WRITE (LULIST,2035) (SNCOV(I),I=1,noszone)

      if (bre) then
	 do 2052 i = 1,noszone
	    if (brepro(i) .le. 0.0) then
	       bmout(i) = 0.0
	    else
	       bmout(i) = bmass(i)
	    endif
 2052    continue 
	 write (lulist,2053) (brepro(i),i=1,noszone)
	 write (*,2050) (bmout(i),i=1,noszone)
	 write (lulist,2050) (bmout(i),i=1,noszone)
      endif

 2050 format(' Mass bal',10F7.0)
 2035 FORMAT(' Snow cov',10F7.0)
 2053 format(' Glac.ar.',10f7.2)
 
 2021 continue

C     FINNER BEREGNINGSFEIL FRA TOTALBALANSEN

      FEIL=FMAG+FSNMAG+AKK(1)+akk(18)-SMAG-SSNMAG-AKK(3)-AKK(4)-akk(19)
C nrs IF (ABS(FEIL).gt. 0.5) then
C nrs	   WRITE(*, 2016)FEIL
           WRITE(LULIST, 2016)FEIL
           runbal = -akk(3)
           evabal = -akk(4)
           strbal = -dmag
           snobal = -dsnmag
           snoadj = - akk(19)
           write (lulist, 2054) akk(1),akk(18),evabal,runbal,
     *     strbal,snobal,snoadj
C nrs endif
2016  FORMAT (/,10X,'Comp err. ',F10.2)
2054  FORMAT (/,10X,'Precip.   ',F10.2,
     *        /,10X,'Glac. melt',F10.2,
     *        /,10X,'Evapotr.  ',F10.2,
     *        /,10X,'Runoff    ',F10.2,
     *        /,10X,'Stor.ch.  ',F10.2,
     *        /,10X,'Snow ch   ',F10.2,
     *        /,10X,'Snow adj. ',F10.2)

500   CONTINUE
      IF (ANALYS .GE. 5) RETURN
      WRITE(*, 2014)(AKK(I),I=6,10)
      WRITE(LULIST, 2014)(AKK(I),I=6,10)
2014  FORMAT (/,9X,'Error functions:',/,10X,'rel.dif**2',F10.2,/,10X,
     * 'difference',F10.2,/,10X,'F2-value  '
     *,F10.2,/,10X,'R2-value  ',F10.2,/,10X,'R2-log    ',F10.2,/)
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PARTAB(STND)
C ----------------------------------------------------------------------
C... TABULERER PARAMETERVERDIER O.L.

c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c ----------------------------------------------------------------------
c     common inimag - initial states
c	 ism	   soil moisture
c	 iuz	   upper zone
c	 ilz	   lower zone
c	 ilakeh    lake level
c	 ismmag	   snow per altitude level
c	 iuzmag    upper zone per altitude level
c	 ilzmag    lower zone per altitude level
c	 ispd	   snow distribution per altitude level and vegetation zone
c	 iwcd	   water content distribution per altitude level and veg.
c	 ilaket    lake temperature per altitude level
c	 iaindex   albedo index
c	 irmag	   routing states
c	 iicmag    interception content
c	 isp	   snow per altitude level
c	 iwc	   snow water content per altitude level

      real ism,iuz,ilz,ilakeh,ismmag,iuzmag,ilzmag,ispd,iwcd,
     *		   ilaket,iaindex,irmag,iicmag,isp,iwc

      common /inimag/ ism,iuz,ilz,ilakeh,ismmag(10,2),
     *             iuzmag(10,2),ilzmag(10,2),
     *             ispd(10,2,0:9),iwcd(10,2,0:9),
     *             ilaket(10),iaindex(10),irmag(5),
     *             iicmag(10,2),isp(10),iwc(10)

c -------------------------------------------------------------------
c     common mag - actual state
c        as inimag, +
c        ba(10)    snow free area per altitude zone

      real sm,uz,lz,lakeh,smmag,uzmag,lzmag,spd,wcd,ba,
     *		   laket,aindex,rmag,icmag,sp,wc

      common /mag/ sm,uz,lz,lakeh,smmag(10,2),
     *             uzmag(10,2),lzmag(10,2),
     *             spd(10,2,0:9),wcd(10,2,0:9),
     *             ba(10),laket(10),aindex(10),rmag(5),
     *             icmag(10,2),sp(10),wc(10)

c -------------------------------------------------------------------
c     common akku - accumulators
c	 akk(20)      accumulators
c	 bmass(0:10)  glacier mass balance, per level
c		      0: average

      real akk,bmass
      common /akku/ akk(20),bmass(0:10)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common grad -   temperature gradients
c	 tdiff	      temperature per altitude, month, and dry/wet days

      real tdiff

      COMMON/GRAD/TDIFF(0:11,12,2)

c -------------------------------------------------------------------
c     common vegtype - parameters per vegetation type
c	 icmax	       interception storage
c	 cxrel	       melt factor (relative)
c	 tsdiff        offset of zero melt temperature
c	 cvsnow        variation coefficient of snow distribution
c	 fcveg	       field capacity
c	 lpveg	       full evapotranspiration level (rel. to fc)
c	 nostype       number of vegetation types

      real  icmax,cxrel,tsdiff,cvsnow,fcveg,lpveg,epvar
      integer nostype

      common /vegtype/ icmax(15),cxrel(15),tsdiff(15),cvsnow(15),
     *                 fcveg(15),lpveg(15),epvar(15),nostype

c -------------------------------------------------------------------
c     common zones - parameters per altitude zone
c	 hoh	       mean altitude
c	 zarea	       area fraction within zone
c	 brepro        part of zone area that is glacier covered
c	 tcorr	       actual temperature correction
c	 pcorr	       actual precipitation correction
c	 vegt	       vegetation types
c	 vega	       vegetation areas
c	 nvz	       number of vegetation zones (one or two)
c	 lake	       lake area
c	 sdist	       snow distribution
c	 pact	       actual precipitation
c	 noszone       number of altitude zones

      real  hoh,zarea,brepro,pcorr,tcorr,vega,lake,sdist,pact
      integer vegt,nvz,noszone

      common /zones/ hoh(10),zarea(10),brepro(10),pcorr(10),tcorr(10),
     *                 vegt(10,2),vega(10,2),nvz(10),lake(10),
     -		       sdist(10,2,9),pact(10),noszone

c -------------------------------------------------------------------
c     common climcha - climatic change parameters
c	 ktkorr        temperature offset per month
c	 kpkorr        precipitation change (relative)

      real ktkorr,kpkorr

      common /climcha/ ktkorr(12),kpkorr(12)

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,FILN
c ------------------------------------------------------------------
c     local variables
c-------------------------------------------------------------------

      LOGICAL STND
      INTEGER modell,ANALYS,linut,mbut,inter
      integer i,i1,i2,i3,ii,j,nr,idum,iverdi,ivz,iveg,k
      real sjopro,tkonst,verdi,dum,xx,bavz,pro,spdist
      real ep(10),sncov(10),smn(10)

      CHARACTER DATO*9
      character*4 typek(6),feilf(5),model,ut(5)

c--------------
c     function
c--------------
      integer ijanei

      EQUIVALENCE (I1,IND(10)),(I2,IND(14)),(I3,IND(15)),
     *    (MODELL,IND(13)),(LINUT,IND(16)),(MBUT,IND(17)),
     *    (INTER,IND(18)),(ANALYS,IND(14))
      equivalence (par(3),sjopro),(par(67),ep(1))
      DATA TYPEK/'SIMU','TOPO','OPTI','OPTI','OPPD','PROG'/
      DATA FEILF/'RELA','TOTA','F2  ','R2  ','ABSA'/
      DATA tkonst /0.0/
CV    DATO=DATE()
      

888   CONTINUE
      CALL DATE(DATO)
      MODEL='HBV3'
     
C
C     FINNER SNØDEKKE
C
      DO 700 I=1,noszone
	 SNCOV(I)=(1.0-BA(I))*100.0
  700 CONTINUE

      II=I3+1

      DO 10 I=1,5
	 UT(I)=' NEI'
  10  CONTINUE

      IF (LINUT .EQ. 1)  UT(1)='  JA'
      IF (LINUT .EQ. 2)  UT(2)='  JA'
      IF (LINUT .EQ. 3)  UT(5)='  JA'
      IF (LINUT .GT.10)  UT(4)='  JA'
      IF (INTER.EQ.0) GOTO 1999
      IF (STND) GOTO 1999
C
C     DISPLAY-BASERT UTSKRIFT
C
900   CONTINUE
      GOTO (1001,1999,900)
     *  IJANEI(OUT,'$Do you want to inspect the parameters?')
 1001 continue
c      call blank5(in)
      WRITE(*, 1000) MODEL,DATO
 1000 FORMAT (/,' PARAMETER VALUES FOR SIMULATIONS WITH ',A4,
     * '-MODEL, RUN DATE: ',A9,/,1X,68(1H*))

      write (*,'(2x,10i7)') (j,j=1,10)
      do 2010 j=1,111,10
      write (*,'(i4,a,10F7.2)') j,':',(par(i),i=j,j+9)
2010  continue

C     EVENTUELL FORANDRINGER

1100  CONTINUE
      GOTO (1110,1800,1100) IJANEI(OUT,'$Change parameters?')
 1110 CONTINUE
C
C     LES INN NYE PARAMETERVERDIER I SUBROUTINE CHGPAR
C
C     CALL CHGPAR
C     CALL blank5(in)

C      GAMMEL INNLESEPROSEDYRE! (BEHOLDES ENNÅ)
      WRITE(*,1111)
1111  FORMAT (/,' Parameter no and new value: ')
      READ(*,*,END=1112,ERR=1110) NR,VERDI
      IF (NR .EQ. 0) GOTO 888
c      if (indpar(nr).eq.0) then
c      write (*,'(A)') ' Parameter not implemented or not accessible'
c      goto 1100
c      endif

      PAR(nr) = verdi
      CALL INIT (0,IDUM,DUM)
      GOTO 1001

 1112 CONTINUE
      GOTO 888
1800  continue
c      call blank5(in)
      WRITE(*, 1801)FELTN,(IND(I),I=2,9)
1801  FORMAT (//,1X,'Simulation for ',A12,5X,'period:',
     *   I5,3I3,' to ',I5,3I3)
 1999 CONTINUE
      IF (ANALYS.GE.5) GOTO 2222
      if (linut.gt.0) goto 2200
      IF (MBUT .EQ. 1) UT(3)='  JA'
      IF(MBUT .EQ. 2) UT(4)='  JA'
      WRITE(LULIST, 2000) MODEL,DATO
 2000 FORMAT(1H1///9X,'PARAMETERVERDIER VED SIMULERING MED ',A4,
     *'-MODELLEN KJØRT DATO: ',A9,/,9X,74(1H*))
      WRITE(LULIST, 2001)
2001  FORMAT( /9X,'INITIALINNHOLD I MAGASINER :'/)
      WRITE(LULIST, 2002)
2002  FORMAT (18X,  'SM         UZ         LZ',/)

      WRITE(LULIST, 2004) ism,iuz,ilz
2004  FORMAT(9X,10F11.2)
      WRITE(LULIST, 2005)
2005  FORMAT(' Parameter values :')

      write (lulist,'(2x,10i7)') (j,j=1,10)
      do 2040 j=1,111,10
      write (lulist,'(i4,a,10F7.2)') j,':',(par(i),i=j,j+9)
2040  continue

      WRITE (LULIST,2033) (isp(j),j=1,noszone)
 2033 FORMAT (' Dry snow:',10F7.0)

      WRITE (LULIST,2034) (iwc(j),j=1,noszone)
 2034 FORMAT (' Sno wat.:',10F7.0)
     
 2035 FORMAT (9X,'SNØDEKKE  ',10F8.0)
 2037 CONTINUE
      
      WRITE(LULIST, 2041)
2041  FORMAT (/,' Evapotranspiration profile:')
      WRITE(LULIST, 2042) (ep(j),j=1,12)
2042  FORMAT (12F6.2)

2200  CONTINUE
      WRITE(LULIST,2009)FELTN,IND(4),IND(3),IND(2),IND(8),IND(7),IND(6)
2009  FORMAT(//9X,'Simulation for ',A12,'   period: ',
     C I3,'/',I2,I5,' - ',I2,'/',I2,I5)
C
C     UTLISTING/NY INNLESNING AV INDEKSOPPSETT
C
2222  CONTINUE
c      call blank5(in)
      WRITE(*,2016)FELTN
 2016 FORMAT(/,10X,A12)
      WRITE(*, 2017) PAR(1)
2017  FORMAT( /10X,'Catchment area',F8.2,' km2')
      IF(I2.EQ.3) WRITE(*,2012) FEILF(II)
2012  FORMAT( /10X,'Error functions used in optimalization ',A4)
      WRITE(*, 2013) TYPEK(I2)
2013  FORMAT( /10X,'Run of type ',A4)
      IF (.NOT.STND) WRITE(*,2114)(I,I=1,10),(IND(J),J=1,10),
     *                              (i,i=11,30),(ind(j),j=11,30)            
 2114 FORMAT(' Index values:',2(/,I3,2(I5,3I3),i3),/,2(/,20I3))
      IF (INTER .EQ. 0) RETURN
      IF (STND) GOTO 3000
C
C     SKAL INDEKSER ENDRES?
C
2100  CONTINUE
      GOTO (2102,3000,2100) IJANEI(OUT,'$Change index?')
2102  CONTINUE
      WRITE(*,2103)
2103  FORMAT (/,' Index no and new value: ')
      READ(*,*,END=2104,ERR=2100) NR,IVERDI
      IF (NR .EQ. 0) GOTO 3000
      IND(NR)=IVERDI
      GOTO 2222
 2104 CONTINUE
C
C     UTSKRIFT OG EVT. INNLESNING AV NYE STARTMAGASINVERDIER
C
 3000 CONTINUE
c      call blank5(in)
     
      WRITE(*,3001) ism,PAR(79)
 3001 FORMAT (/,' Initial states: soil water (1):',F5.0,' (max',F5.0,
     * ' mm)')
      WRITE(*,3002) iuz,PAR(86)
 3002 FORMAT (17X,'upper zone (2):',F5.0,' (threshold',F5.0,' mm)')
      WRITE(*,3003) ilz
 3003 FORMAT (17X,'lower zone (3):',F5.0,' mm')
      write (*,3004) ilakeh
 3004 format (17x,'lake level (4):',f5.2,' m')

 3100 IF (STND) GOTO 3010

      GOTO (3006,3010,3100) IJANEI(OUT,'$Change states?')
 3006 CONTINUE
      WRITE(*,3005)
 3005 FORMAT (/,' State no and new value: ')
      READ(*,*,END=3007,ERR=3100) NR,VERDI
      goto (3101,3102,3103,3104,3100) nr
 3101 ism = verdi
      goto 3000
 3102 iuz = verdi
      goto 3000
 3103 ilz =verdi
      goto 3000
 3104 ilakeh=verdi
      goto 3000
C
C     FERDIG MED INNLESNING AV STARTMAGASIN
C
 3007 CONTINUE
 3010 CONTINUE
C
C     SJEKKER SNØMAGASIN I AKTUELLE MÅNEDER
C
      IF (IND(3).GT. 6 .AND. IND(3) .LT. 10) RETURN
C     OKTOBER - JUNI
C
C     CALL NEWPAG(4,'KARMEN',0,0,1)
c      call blank5(in)
 3300 WRITE(*,3011)
 3011 FORMAT (/,' Initial states, snow:')
      WRITE(*,3012) (i,I=1,noszone)
 3012 FORMAT ('      level no:',I5,9I6)
      WRITE(*,3013) (hoh(i),i=1,noszone)
 3013 FORMAT ('      m asl.  :',10F6.0)
      WRITE(*,3014)(isp(i),i=1,noszone)
 3014 FORMAT ('      dry snow:',10F6.0)
      WRITE(*,3015)(iwc(i),i=1,noszone)
 3015 FORMAT ('      liq wat.:',10F6.0)
      WRITE(*,3009)(ba(i),i=1,noszone)
 3009 FORMAT ('      snow cov:',10F6.0)
      IF (STND) RETURN
  202 CONTINUE
C
C     INNLESNING AV NYE TILSTANDER I SNØMAGASINET ?
C
      GOTO(3016,201,202) IJANEI(OUT,'$Change snow states?')
 3016 WRITE(*,3017) noszone
 3017 FORMAT(/,' Enter new snow states in mm for',I3,' altitude levels'
     *	     /,' start from lower end: ')
      READ (*,*,END=3018,ERR=3016) (SMN(J),J=1,noszone)
      GOTO 3019
 3018 CONTINUE
      GOTO 202
 3019 DO 3020 i=1,noszone
      IF (SMN(i).EQ.0.0) THEN
	 SP(i)=0.0
	 WC(i)=0.0
	 BA(i)=1.0
	 do 3022 j=1,nvz(i)
	 do 3022 K=0,9
	   SPD(I,J,K)=0.0
	   WCD(I,J,K)=0.0
 3022    continue 
      else
	 ba(i) = 0.0
	 do 3023 ivz = 1,nvz(i)
	    iveg = vegt(i,ivz)
 3021    continue
	 IF (ABS(SMN(i)-WC(i)-SP(i)).LT.1.0) GOTO 3024
	 CALL SNO(0.0,SIGN(1.0,SMN(i)-WC(i)-SP(i)),0.0,XX,
     *     bavz,SPD,WCD,10,i,PRO,TKONST,SPDIST,SDIST,snofrac,
     *     ivz)
	 GOTO 3021
 3024    continue
	 ba(i) = ba(i)+bavz*vega(i,ivz)
 3023    continue
      endif
 3020 CONTINUE
      
C
C     FINNER SNØDEKKE
C
      DO 3030 I=1,noszone
      
      SNCOV(I)=0.0
      IF (SP(I).LE.0.0) GOTO 3030
      SNCOV(I)=100.0
      IF (SPD(I,ivz,0).LE.0.0) GOTO 3030
      IF (SPD(I,ivz,5).LE.0.0) SNCOV(I)=(1.0-BA(I))*100.0
 3030 CONTINUE
      GOTO 3300
 201  RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLOTAR (plofil,I1,I2,START,SLUTT,IAAR,FNAVN,Q,QOBS,
     *	WSM,WP,WT,WSN,WDI,WSSM,WMV,WGRV,
     *	MDAG,NDAG,MNAVN,AREAL,BRE,GRV)
C ----------------------------------------------------------------------
C
C     Skriver ut data på file xxxxxx.PLT
C     Data kan senere leses inn og plottes av plotteprogrammet PLOKAR
C
C
                                                                        
      INTEGER   I1,I2,START,SLUTT,DAG,MND,IAAR
      LOGICAL   BRE,GRV
      INTEGER   IND
      CHARACTER FNAVN*(*) ,MNAVN*(*), plofil*(*)
      REAL	Q(366),QOBS(366),WT(366),WP(366),WSN(366),
     *          WDI(366),WSSM(366),WSM(366),WMV(366),WGRV(366)          
      integer   mdag,ndag,ioutp,ifil,idev
      real      areal
      COMMON/INDEX/IND(30)
      save ioutp

      data ioutp/0/
C
C ......................................................................
C
      DAG = 0
      IDEV = 1
      IFIL = 23
      call dagnr(dag,mnd,iaar,mdag)

      IF(IOUTP .EQ. 0) THEN
	 OPEN(IFIL,FILE=plofil)
	 IOUTP = 1
      END IF

C     Korrigerer Device nr for HP7475 plotter (Fra 2004 til 4)
      IF (IDEV .EQ. 2004) IDEV = 4
C
      WRITE(IFIL,*) IDEV,' Device nr'
      WRITE(IFIL,602) I1,I2,IAAR,MND,DAG,START,SLUTT
 602  FORMAT(I6,I5,I6,I3,I3,I4,I4)
      WRITE(IFIL,*) ' IND array'
      WRITE(IFIL,'(10I8)') IND
      WRITE(IFIL,*) FNAVN
  601 FORMAT(A12/37(10F9.2/))
      WRITE(IFIL,601) ' Simulert   Q ',Q
      WRITE(IFIL,601) ' Observert  Q ',QOBS
      WRITE(IFIL,601) ' Snømagasin   ',WSM
      WRITE(IFIL,601) ' Nedbør       ',WP
      WRITE(IFIL,601) ' Temperatur   ',WT
      WRITE(IFIL,601) ' Snødekning   ',WSN
      WRITE(IFIL,601) ' Akk. diff.   ',WDI
      WRITE(IFIL,601) ' Smelting     ',WSSM
      WRITE(IFIL,601) ' Markvann     ',WMV
      WRITE(IFIL,601) ' Grunnvann    ',WGRV
      WRITE(IFIL,*) MDAG,NDAG
      WRITE(IFIL,'(A4)') MNAVN
      WRITE(IFIL,*) BRE,GRV
      WRITE(IFIL,*) AREAL
C
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE LES(filn,AAR,faar,saar,ndat,STATUS)
C ----------------------------------------------------------------------
C
C     LESER EN ÅRSBLOKK FRA MODELLFILEN
C
c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common aar -    year array of daily data
c	 tdgn	      temperature at weighted station altitude
c	 pdgn	      precipitation at weighted station altitude
c	 qo	      observed runoff
c	 qs	      simulated runoff
c	 wt	      basin mean temperature
c	 wp	      basin mean precipitation
c	 wdi	      difference between observed and simulated runoff
c	 ssm	      snow melt
c	 snd	      snow cover

      real tdgn,pdgn,qo,qs,wp,wt,wdi,ssm,snd

      common /aar/
     *     tdgn(366),pdgn(366),qo(366),qs(366),wp(366),wt(366),wdi(366),
     *	   ssm(366),snd(366)

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

      integer idat,ined,itemp,itils,i,j,iaar,imnd,idag,analys
      integer aar,faar,saar,ndat,status,it,ip
      character filn*30

      EQUIVALENCE (IND(14),ANALYS)
      REAL xdat(40)
      REAL xp,xt,htkorr,vtkorr,pgrad,pp,tt,tgrad

      HTKORR=PAR(63)
      VTKORR=PAR(64)
      PGRAD =PAR(65)

      do 11 j=1,366
	 tdgn(j) = -10000.
	 pdgn(j) = -10000.
	 qo(j) = -10000.
 11   continue

C...  ÅPNER MODELLFILE, VERSION 1

      idat = 0
      ined = 0
      itemp = 0
      itils = 0
      do 1 j =1,25
	 if (nedid(j).eq.0) goto 2
	 idat = idat + 1
	 ined = ined + 1
    1 continue
    2 do 3 j =1,25
	 if (tempid(j).eq.0) goto 4
	 idat = idat + 1
	 itemp = itemp + 1
    3 continue
    4 continue
      do 5 j=1,4
	 if (tilsid(j).eq.0) goto 6
	 idat = idat+1
	 itils = itils+1
    5 continue
    6 continue
      if (aar.eq.faar)
     *      OPEN(LUDATA,FILE=filn,
     -       STATUS='OLD',err=9999)

      i = 1
 200  continue
      read (ludata,*,end=1000) iaar, imnd, idag, (xdat(j),j=1,idat)
c      write (*,*) iaar,imnd,idag,(xdat(j),j=1,dat)
      if (iaar.lt.aar) goto 200
      if (iaar.gt.aar) goto 999
c
c    riktig år
c
      call dagnr(idag,imnd,iaar,i)

 100  continue
c      write (*,*) iaar,imnd,idag,i,ndat
      pp = 0
      ip = 0
      xp = 0.0
      do 101 j=1,ined
	 if (xdat(j).gt.-9999.0) then
	 ip = ip+1
	 pp = pp+xdat(j)*(1.0-(nedhoh(j)-refp)*pgrad/100.0)*nedv(j)
	 xp = xp+nedv(j)
      endif
  101 continue

      if (xp .gt. 0.0) then
	 pdgn(i) = pp/xp
      else
	 pdgn(i) = -10000.0
      endif

      tt = 0
      it = 0
      xt = 0.0

      if (pdgn(i) .gt.0.0) then
	 tgrad = vtkorr
      else
	 tgrad = htkorr
      endif

      do 102 j=1,itemp
	 if (xdat(ined+j).gt.-9999.0) then
	    it = it+1
	    tt = tt+(xdat(ined+j)-(tmphoh(j)-reft)*tgrad/100.0)*tempv(j)
	    xt = xt + tempv(j)
	 endif
  102 continue

      if (xt .gt. 0.0) then
	 tdgn(i) = tt/xt
      else
	 tdgn(i) = -10000.0
      endif

      pp = 0
      ip = 0

      do 105 j=1,itils
	 if (xdat(ined+itemp+j).gt.-9999.0) then
	    ip = ip+1
	    pp = pp+xdat(ined+itemp+j)*tilsv(j)
	 endif
  105 continue

      if (ip .gt. 0) then
	 qo(i) = pp*itils/ip
      else
	 qo(i) = -10000.0
      endif
c     write (*,*) i,pdgn(i),tdgn(i),qo(i)

      if (i .lt. ndat) then
	 i = i+1
	 read (ludata,*,end=1000)
     *      iaar, imnd, idag, (xdat(j),j=1,idat)
	 goto 100
      endif

 1000 continue

      status = 0
      if (aar.eq.saar) CLOSE(LUDATA)
      RETURN

  999 WRITE(*,113) IAAR, IMND, IDAG
  113 FORMAT(//' **** error when reading data',3i5, '****'//)
      STATUS=1
      CLOSE(LUDATA)
      RETURN
 9999 continue
      write (*,'(3A)') '**** Error when opening file ',filn,' ****'
      status = 1
      RETURN
      END
C ----------------------------------------------------------------------
      SUBROUTINE MODEL(I1,I2,IAAR,MND,DAG,START,SLUTT,F,N,plofil,nsim)
C ----------------------------------------------------------------------
C
C     MODELL OG OVERFØRER VARIABLE(VAR) OG AKKUMULERTE VERDIER(AKK)
C     TIL TABULERING ELLER LINJEPLOTT.
C        PARAMETER       TYPE       BETYDNING
C        I1               I,INN      INDEKS I DATACOMMON FOR FØRSTE OBSE
C        I2               I,INN      INDEKS I DATACOMMON FOR SISTEE OBSE
C        IAAR             I,INN      ÅRNR
C        MND              I,INN/UT   MÅNEDNR
C        DAG              I,INN/UT   DAGNR I ÅR
C        START            LOG,INN    FØRSTE BLOKK: FØRSTE DAG I BLOKKA,
C        SLUTT            LOG,INN    SISTE BLOKK: SISTE DAG I BLOKKA,
C        F                R,UT       AKKUMULERT FEILFUNKSJON
C        N                I,IN       Error function no
C        PLOFIL           C,IN       name of datafile for plot
C        NSIM             I,IN       Simulation number 

      integer i1,i2,iaar,mnd,dag,start,slutt,n,nsim
      real f(5)
      character plofil*12

c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c ----------------------------------------------------------------------
c     common inimag - initial states
c	 ism	   soil moisture
c	 iuz	   upper zone
c	 ilz	   lower zone
c	 ilakeh    lake level
c	 ismmag	   snow per altitude level
c	 iuzmag    upper zone per altitude level
c	 ilzmag    lower zone per altitude level
c	 ispd	   snow distribution per altitude level and vegetation zone
c	 iwcd	   water content distribution per altitude level and veg.
c	 ilaket    lake temperature per altitude level
c	 iaindex   albedo index
c	 irmag	   routing states
c	 iicmag    interception content
c	 isp	   snow per altitude level
c	 iwc	   snow water content per altitude level

      real ism,iuz,ilz,ilakeh,ismmag,iuzmag,ilzmag,ispd,iwcd,
     *		   ilaket,iaindex,irmag,iicmag,isp,iwc

      common /inimag/ ism,iuz,ilz,ilakeh,ismmag(10,2),
     *             iuzmag(10,2),ilzmag(10,2),
     *             ispd(10,2,0:9),iwcd(10,2,0:9),
     *             ilaket(10),iaindex(10),irmag(5),
     *             iicmag(10,2),isp(10),iwc(10)

c -------------------------------------------------------------------
c     common mag - actual state
c        as inimag, +
c        ba(10)    snow free area per altitude zone

      real sm,uz,lz,lakeh,smmag,uzmag,lzmag,spd,wcd,ba,
     *		   laket,aindex,rmag,icmag,sp,wc

      common /mag/ sm,uz,lz,lakeh,smmag(10,2),
     *             uzmag(10,2),lzmag(10,2),
     *             spd(10,2,0:9),wcd(10,2,0:9),
     *             ba(10),laket(10),aindex(10),rmag(5),
     *             icmag(10,2),sp(10),wc(10)

c -------------------------------------------------------------------
c     common akku - accumulators
c	 akk(20)      accumulators
c	 bmass(0:10)  glacier mass balance, per level
c		      0: average

      real akk,bmass
      common /akku/ akk(20),bmass(0:10)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common aar -    year array of daily data
c	 tdgn	      temperature at weighted station altitude
c	 pdgn	      precipitation at weighted station altitude
c	 qo	      observed runoff
c	 qs	      simulated runoff
c	 wt	      basin mean temperature
c	 wp	      basin mean precipitation
c	 wdi	      difference between observed and simulated runoff
c	 ssm	      snow melt
c	 snd	      snow cover

      real tdgn,pdgn,qo,qs,wp,wt,wdi,ssm,snd

      common /aar/
     *     tdgn(366),pdgn(366),qo(366),qs(366),wp(366),wt(366),wdi(366),
     *	   ssm(366),snd(366)

c -------------------------------------------------------------------
c     common grad -   temperature gradients
c	 tdiff	      temperature per altitude, month, and dry/wet days

      real tdiff

      COMMON/GRAD/TDIFF(0:11,12,2)

c -------------------------------------------------------------------
c     common vegtype - parameters per vegetation type
c	 icmax	       interception storage
c	 cxrel	       melt factor (relative)
c	 tsdiff        offset of zero melt temperature
c	 cvsnow        variation coefficient of snow distribution
c	 fcveg	       field capacity
c	 lpveg	       full evapotranspiration level (rel. to fc)
c	 nostype       number of vegetation types

      real  icmax,cxrel,tsdiff,cvsnow,fcveg,lpveg,epvar
      integer nostype

      common /vegtype/ icmax(15),cxrel(15),tsdiff(15),cvsnow(15),
     *                 fcveg(15),lpveg(15),epvar(15),nostype

c -------------------------------------------------------------------
c     common zones - parameters per altitude zone
c	 hoh	       mean altitude
c	 zarea	       area fraction within zone
c	 brepro        part of zone area that is glacier covered
c	 tcorr	       actual temperature correction
c	 pcorr	       actual precipitation correction
c	 vegt	       vegetation types
c	 vega	       vegetation areas
c	 nvz	       number of vegetation zones (one or two)
c	 lake	       lake area
c	 sdist	       snow distribution
c	 pact	       actual precipitation
c	 noszone       number of altitude zones

      real  hoh,zarea,brepro,pcorr,tcorr,vega,lake,sdist,pact
      integer vegt,nvz,noszone

      common /zones/ hoh(10),zarea(10),brepro(10),pcorr(10),tcorr(10),
     *                 vegt(10,2),vega(10,2),nvz(10),lake(10),
     -		       sdist(10,2,9),pact(10),noszone

c -------------------------------------------------------------------
c     common climcha - climatic change parameters
c	 ktkorr        temperature offset per month
c	 kpkorr        precipitation change (relative)

      real ktkorr,kpkorr

      common /climcha/ ktkorr(12),kpkorr(12)

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

c --------------------------------------------------------------------
c     local variables
c -------------------------------------------------------------------

      real rpar(5),var(12)
      real areal,adiff,scq,deld,qobs,tmax,tmin,p,t,dagar,eal,rnet,sout,
     -        quu,qul,aperc,ql,glacm,diff,qmid,spmid,pmag,sne,qbre,q,e,
     -        xlqobs,qsim
      integer iterm,iprt,inter,nskala,ioppd,ired,itvar,ihyd,idamp,
     -        iscale,igrplt,irout,idg,jardg,mdag,i,ii,j,idag,
     -        modell,feilf,iardg,ndag

      REAL tsc(10),f0
      REAL SNOWCV,INSOIL
      REAL QQ(366),WM(366),WG(366),wsm(366)
      CHARACTER MNAVN*4,TXT*40,varnam(10)*10,stanam(10)*10,
     *          divnam(10)*10
      INTEGER ANALYS
      LOGICAL TABU,OPPD,BRE,GRV

c --------------------------------------------------------------------
c     functions
c -------------------------------------------------------------------
      integer isk

      EQUIVALENCE (PAR(90),RPAR(1))
  
      EQUIVALENCE (AREAL,PAR(1))
      EQUIVALENCE (MODELL,IND(13)),(ANALYS,IND(14)),(FEILF,IND(15)),
     * (ITERM,IND(16)),(IPRT,IND(17)),(INTER,IND(18)),(NSKALA,IND(19)),
     * (IOPPD,IND(20)),(IRED,IND(21)),(ITVAR,IND(23)),(IHYD,IND(24)),
     * (IDAMP,IND(25)),(ISCALE,IND(26)),(IGRPLT,IND(27)),
     * (irout,ind(22))
          
      SAVE
      data varnam /'  prec  ','   temp  ','net infl  ','soil out  ',
     -  'upper up','upper lo','deep perc','lower out','   qsim',
     -  '  qobs'/
      data stanam /'  prec  ','   temp  ','  evap  ',' snowres  ',
     -	' snowcov   ',
     -	'soilmoist','upper zn','lower zn ','	qsim  ',
     -	'   qobs'/
      data divnam /' laketemp',' lakeevap','   evap  ',7*' '/

      GRV=.FALSE.

      dag = i1

      IF (IGRPLT.EQ.1) GRV=.TRUE.
      IF (I1.EQ.START) THEN
	 TABU=.true.
         IF (ANALYS.EQ.2.OR.ANALYS.EQ.3.OR.ANALYS.EQ.4.OR.ANALYS.EQ.8)
     *      TABU=.FALSE.
	 OPPD=.FALSE.
         IF (IOPPD.GT.0.AND.ANALYS.LT.6) OPPD=.TRUE.


         DELD=1.0
C
C        SCQ SKALERER FRA 1/10 M3/S TIL MM/DØGN
C
         SCQ=86.4/AREAL*DELD
         JARDG=365+ISK(IAAR)
         IF (ISK(IAAR).EQ.1.AND.DAG.LT.60) JARDG=JARDG-1
	 IDG=DAG-(243+ISK(IAAR))*IHYD
	 IF (IDG.LT.0) IDG=IDG+JARDG
	 IDG=IDG-DELD
         MNAVN='HBV '

         MDAG=IDG+DELD
         ADIFF=0.0
         BMASS(0)=0.0
         BRE=.FALSE.
         DO 16 J=1,10
           IF (BREPRO(J).NE.0.0) BRE=.TRUE.
  16       BMASS(J)=0.0
         VAR(11)=0.0
         VAR(12)=0.0
 2347    format (16x,10(f10.0,','))
 2348    format (19x,10(1x,a10))
         if (tabu .and. ind(16).eq. 1) then
            write (lulist,'(9x,a)') 'Snowdepth in mm wat. equiv'
            write (lulist,2347) 
     -         (hoh(j),j=1,noszone)
         endif
         if (tabu .and. ind(16).eq. 2) then
            write (lulist,'(9x,a)') 'Water flows'
            write (lulist,2348) 
     -         (varnam(j),j=1,10)     
         endif
         if (tabu .and. ind(16).eq. 3) then
            write (lulist,'(9x,a)') 'States'
            write (lulist,2348) 
     -         (stanam(j),j=1,10)     
         endif
         if (tabu .and. ind(16).eq. 4) then
            write (lulist,'(9x,a)') 'Snowcover in per cent'
            write (lulist,2347) 
     -         (hoh(j),j=1,noszone)
         endif
         if (tabu .and. ind(16).eq. 5) then
            write (lulist,'(9x,a)') 
     -            'Glacier mass balance in mm wat. equiv'
            write (lulist,2347) 
     -         (hoh(j),j=1,noszone)
         endif
         if (tabu .and. ind(16).eq. 6) then
            write (lulist,'(9x,a)') 'Diverse data'
            write (lulist,2348) 
     -         (divnam(j),j=1,3)     
         endif
      ENDIF
      DAGAR= FLOAT(DAG) -DELD
      IARDG=365+ISK(IAAR)-122*IHYD

C     SIMULERER FOR EN BLOKKPERIODE

      DO 200 I=I1,I2
      N=N+1
      II=I
      TMAX=TDGN(I)
      TMIN=TDGN(I)
      P=PDGN(I)
      QOBS=QO(I)*SCQ
      T=0.5*(TMAX+TMIN)
      DAGAR=DAGAR+DELD
      DAG=IFIX(DAGAR)
      IDG=IDG+DELD
	 if (idg.gt.366) write (*,*) dag,iaar,isk(iaar),ihyd,deld,idg
      MND=0
      CALL DAGNR(IDAG,MND,IAAR,DAG)
      IF (P .GT. -1.0 .AND. T .GT. -99.0) GOTO 300
C     MANGLENDE OBSERVASJONER
C      QQ(DAG)=MANGL
      VAR(1)=P
      VAR(2)=QOBS
      VAR(3)=MANGL
      VAR(4)=MANGL
      VAR(5)=T
      VAR(6)=MANGL
      VAR(7)=MANGL
      VAR(8)=MANGL
      VAR(9)=MANGL
      VAR(10)=MANGL
      VAR(11)=MANGL
      VAR(12)=MANGL
      QS(IDG)=MANGL
      QQ(IDG)=QOBS
      WM(IDG)=MANGL
      WSM(IDG)=MANGL
      WG(IDG)=MANGL
      WT(IDG)=T
      WP(IDG)=P
      SND(IDG)=MANGL
      WDI(IDG)=ADIFF
      SSM(IDG)=MANGL
      IF (ANALYS.LT.7) GOTO 210
      DO 201 J=1,10
 201  AKK(J)=MANGL
      GOTO 210
300   CONTINUE
      AKK(11)=AKK(11)+P
C
C-KONTROLL-
C
      IF (IDAG .EQ. 1 .and. analys .lt. 3) THEN
         WRITE (TXT,'(A20,3I6)') '  ',nsim,IAAR,MND
         CALL POSTXT(12,30,TXT)
      END IF
C
      CALL NETTO(DAG,mnd,P,TMAX,TMIN,T,INSOIL,SPMID,SNOWCV,PMAG,
     *   ITVAR,SNE,DELD,
     *   qbre,bmass,bre,Q,e,eal,rnet,sout,quu,qul,aperc,ql,glacm,
     *   akk(19))

      if (irout .gt. 0) CALL ROUT(Q,RPAR,RMAG,AREAL,IND(11),irout)

900   CONTINUE
      QSIM=Q*AREAL/86.4

      IF(ANALYS.EQ.6) QOBS=0.0

c++ går galt, 28/01/94 nrs	IF (AKK(1).EQ.MANGL) GOTO 202

      AKK(1)=AKK(1)+P
      AKK(3)=AKK(3)+Q
      AKK(4)=AKK(4)+E
      AKK(12)=AKK(12)+SNE                

      if (qobs.ge.0.0) then
         DIFF=Q-QOBS
         QMID=0.5*(ABS(QOBS)+Q)
         IF (QMID .LT. 0.01*DELD) QMID=100.0
         AKK(2)=AKK(2)+QOBS
         AKK(6)=AKK(6)+DIFF*DIFF/(QMID*QMID)
         AKK(7)=AKK(7)+DIFF
         AKK(8)=AKK(8)+DIFF*DIFF
         AKK(9)=AKK(9)+QOBS*QOBS
         AKK(10)=AKK(10)+ABS(DIFF)
         akk(15) = akk(15)+1
         if (qobs.gt.0.0 .or. q.gt.0.0) then
            xlqobs = log(qobs+0.001)
            akk(13) = akk(13) + xlqobs**2
            akk(14) = akk(14) + (xlqobs-log(q+0.001))**2
            akk(16) = akk(16)+1
            akk(17) = akk(17) + xlqobs
         endif
         akk(18) = akk(18) + glacm
      endif

      IF (QOBS.LT.-10.0) THEN
         QOBS=Q
         VAR(2)=MANGL
      ELSE
         VAR(2)=QOBS
      ENDIF

  202 CONTINUE
      VAR(1)= P
      VAR(2)=QOBS
      VAR(3)=Q
      VAR(4)=E
      VAR(5)=T
      VAR(6)=INSOIL-P
      IF (VAR(6) .LT. 0.0) VAR(6)=0
      AKK(5)=AKK(5)+VAR(6)
      VAR(7)=SNOWCV
      VAR(8)=SPMID
      VAR(9)=sm
      VAR(10)=uz+lz
      QS(IDG)=Q
      QQ(IDG)=QOBS
      WM(IDG)=sm
      WSM(IDG)=spmid
      WG(IDG)=uz+lz
      WT(IDG)=T
      WP(IDG)=P
      SND(IDG)=SNOWCV
      ADIFF=ADIFF+DIFF
      WDI(IDG)=ADIFF
      SSM(IDG)=VAR(6)

   10 IF (.NOT.TABU) GOTO 210
      if (ind(16).eq.1) then
C     write snow content
      do 2345 j=1,noszone
         tsc(j) = sp(j)+wc(j)
 2345 continue
      write (lulist,2346)
     -    iaar,mnd,idag,(tsc(j),j=1,noszone),var(7),var(6)
      else if (ind(16).eq.2) then
C     write water flows
 2346 format (1x,3(i5,','),12(F10.3,','))
      write (lulist,2346)  
     -    iaar,mnd,idag,p,t,rnet,sout,quu,qul,aperc,ql,q,qobs
      else if (ind(16).eq.3) then
C     write states
      write (lulist,2346)
     -	  iaar,mnd,idag,p,t,e,spmid,snowcv,sm,uz,lz,
     -	  q,qobs
      else if (ind(16).eq.4) then
C     write snow cover
      write (lulist,2346)  
     -    iaar,mnd,idag,(100.0*(1.0-ba(j)),j=1,noszone)
      else if (ind(16).eq.5) then
C     write glacier mass balance
      write (lulist,2346)  
     -    iaar,mnd,idag,(bmass(j),j=1,noszone)
      else if (ind(16).eq.6) then
C     write diverse data
      write (lulist,2346)  
     -    iaar,mnd,idag,laket(1),eal,e
      endif

  210 IF (DAG.EQ.IARDG.OR.I.EQ.SLUTT) THEN
         NDAG=IDG
         IF (analys.eq.1)
     *     CALL PLOTAR(plofil,I1,I2,START,SLUTT,IAAR,FELTN,
     *     QS,QQ,wsm,WP,WT,SND,WDI,SSM,WM,WG,
     *     MDAG,NDAG,MNAVN,AREAL,BRE,GRV)
         IF(IPRT.GT.4 .and. analys.eq.1)
     *     CALL PLOTAR(plofil,I1,I2,START,SLUTT,IAAR,FELTN,
     *     QS,QQ,wsm,WP,WT,SND,WDI,SSM,WM,WG,
     *     MDAG,NDAG,MNAVN,AREAL,BRE,GRV)
         MDAG=1
         IDG=1-DELD
         ADIFF=0.0
      ENDIF
 200  CONTINUE

      IF (I2.NE.SLUTT) RETURN

c      if (analys.lt.3) call blank5(in)
C      WRITE(*,*) 'Data is written to file ',plofil

C     DANNER FEILFUNKSJONER
      IF (ANALYS .EQ. 6) GOTO 910
      F(1)=AKK(6)
      F(2)=ABS(AKK(7))
      F(3)=AKK(8)
      if (akk(15).gt. 0.0) then
         F0=AKK(9)-AKK(2)*AKK(2)/akk(15)
         IF (F0.GT.0.0001) F(4)=(F0-AKK(8))/F0
      else
         F(4) = 0.0
      endif

      if (akk(16).gt. 0.0) then
         F0=AKK(13)-AKK(17)*AKK(17)/akk(16)
         IF (F0.GT.0.0001) F(5)=(F0-AKK(14))/F0
      else
         F(5) = 0.0
      endif

C     F(5)=AKK(10)
      AKK(9) = F(4)
      akk(10) = F(5)
910   CONTINUE
      if (analys.eq.1) CALL RESULT
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE SIMUL(simno,F,STATUS,nsim)
C ----------------------------------------------------------------------
C
C... ADMINISTRERER INN- OG UT-DATA SAMT KALLER SUBROUTINE MODEL
c     SIMNO       I,IN         Simulation no
c     F           R,OUT        error functions
c     STATUS      I,OUT        status variable
c     nsim        I,IN         total number of simulations

      integer simno,status,nsim
      real f(5)
 
c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c ----------------------------------------------------------------------
c     common inimag - initial states
c	 ism	   soil moisture
c	 iuz	   upper zone
c	 ilz	   lower zone
c	 ilakeh    lake level
c	 ismmag	   snow per altitude level
c	 iuzmag    upper zone per altitude level
c	 ilzmag    lower zone per altitude level
c	 ispd	   snow distribution per altitude level and vegetation zone
c	 iwcd	   water content distribution per altitude level and veg.
c	 ilaket    lake temperature per altitude level
c	 iaindex   albedo index
c	 irmag	   routing states
c	 iicmag    interception content
c	 isp	   snow per altitude level
c	 iwc	   snow water content per altitude level

      real ism,iuz,ilz,ilakeh,ismmag,iuzmag,ilzmag,ispd,iwcd,
     *		   ilaket,iaindex,irmag,iicmag,isp,iwc

      common /inimag/ ism,iuz,ilz,ilakeh,ismmag(10,2),
     *             iuzmag(10,2),ilzmag(10,2),
     *             ispd(10,2,0:9),iwcd(10,2,0:9),
     *             ilaket(10),iaindex(10),irmag(5),
     *             iicmag(10,2),isp(10),iwc(10)

c -------------------------------------------------------------------
c     common mag - actual state
c        as inimag, +
c        ba(10)    snow free area per altitude zone

      real sm,uz,lz,lakeh,smmag,uzmag,lzmag,spd,wcd,ba,
     *		   laket,aindex,rmag,icmag,sp,wc

      common /mag/ sm,uz,lz,lakeh,smmag(10,2),
     *             uzmag(10,2),lzmag(10,2),
     *             spd(10,2,0:9),wcd(10,2,0:9),
     *             ba(10),laket(10),aindex(10),rmag(5),
     *             icmag(10,2),sp(10),wc(10)

c -------------------------------------------------------------------
c     common akku - accumulators
c	 akk(20)      accumulators
c	 bmass(0:10)  glacier mass balance, per level
c		      0: average

      real akk,bmass
      common /akku/ akk(20),bmass(0:10)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common aar -    year array of daily data
c	 tdgn	      temperature at weighted station altitude
c	 pdgn	      precipitation at weighted station altitude
c	 qo	      observed runoff
c	 qs	      simulated runoff
c	 wt	      basin mean temperature
c	 wp	      basin mean precipitation
c	 wdi	      difference between observed and simulated runoff
c	 ssm	      snow melt
c	 snd	      snow cover

      real tdgn,pdgn,qo,qs,wp,wt,wdi,ssm,snd

      common /aar/
     *     tdgn(366),pdgn(366),qo(366),qs(366),wp(366),wt(366),wdi(366),
     *	   ssm(366),snd(366)

c -------------------------------------------------------------------
c     common grad -   temperature gradients
c	 tdiff	      temperature per altitude, month, and dry/wet days

      real tdiff

      COMMON/GRAD/TDIFF(0:11,12,2)

c -------------------------------------------------------------------
c     common vegtype - parameters per vegetation type
c	 icmax	       interception storage
c	 cxrel	       melt factor (relative)
c	 tsdiff        offset of zero melt temperature
c	 cvsnow        variation coefficient of snow distribution
c	 fcveg	       field capacity
c	 lpveg	       full evapotranspiration level (rel. to fc)
c	 nostype       number of vegetation types

      real  icmax,cxrel,tsdiff,cvsnow,fcveg,lpveg,epvar
      integer nostype

      common /vegtype/ icmax(15),cxrel(15),tsdiff(15),cvsnow(15),
     *                 fcveg(15),lpveg(15),epvar(15),nostype

c -------------------------------------------------------------------
c     common zones - parameters per altitude zone
c	 hoh	       mean altitude
c	 zarea	       area fraction within zone
c	 brepro        part of zone area that is glacier covered
c	 tcorr	       actual temperature correction
c	 pcorr	       actual precipitation correction
c	 vegt	       vegetation types
c	 vega	       vegetation areas
c	 nvz	       number of vegetation zones (one or two)
c	 lake	       lake area
c	 sdist	       snow distribution
c	 pact	       actual precipitation
c	 noszone       number of altitude zones

      real  hoh,zarea,brepro,pcorr,tcorr,vega,lake,sdist,pact
      integer vegt,nvz,noszone

      common /zones/ hoh(10),zarea(10),brepro(10),pcorr(10),tcorr(10),
     *                 vegt(10,2),vega(10,2),nvz(10),lake(10),
     -		       sdist(10,2,9),pact(10),noszone

c -------------------------------------------------------------------
c     common climcha - climatic change parameters
c	 ktkorr        temperature offset per month
c	 kpkorr        precipitation change (relative)

      real ktkorr,kpkorr

      common /climcha/ ktkorr(12),kpkorr(12)

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

c --------------------------------------------------------------------
c     local variables
c -------------------------------------------------------------------
      INTEGER AAR,MND,DAG,NAAR,BLNR,n,stnr,faar,fmnd,saar,smnd,sdag,
     *        ldag,mdag,n1,n2,i,ndag,ndat,i1,i2,fdag
      INTEGER ANALYS,MBUT,START,SLUTT,irout
      CHARACTER plofil*12,sno*3
      real rpar(5)

c --------------------------------------------------------------------
c     functions
c -------------------------------------------------------------------
      integer isk

      EQUIVALENCE(IND(1),STNR),(IND(2),FAAR),(IND(3),FMND),(IND(4),FDAG)
     *,(IND(6),SAAR),(IND(7),SMND),(IND(8),SDAG),(IND(14),ANALYS),
     * (IND(17),MBUT),(ind(22),irout)
      equivalence (par(90),rpar(1))

      write (sno,'(i3.3)') simno
      plofil = feltn(1:4)//sno//'.plt'
c      if (analys.lt.3) CALL BLANK5(IN)
      N=0
      CALL DAGNR(SDAG,SMND,SAAR,LDAG)
      call dagnr(fdag,fmnd,faar,mdag)

*-----------------------------
*     check routing parameters
*-----------------------------

      if (irout .eq. 1) then
          if (rpar(1).lt.0.01.or.rpar(2).lt.0.01.or.rpar(4).lt.0.01)
     +       irout = 0
      else if (irout .gt. 1) then
          if (rpar(1).lt.0.01.and.rpar(2).lt.0.01.and.rpar(3).lt.0.01
     +       .and. rpar(4).lt.0.01 .and. rpar(5).lt.0.01) irout = 0
      endif
      NAAR=SAAR-FAAR+1
      N1=1
      N2=366
      DO 110 I=1,NAAR
      START=0
      SLUTT=0
      AAR=FAAR-1+I
      NDAG=365+ISK(AAR)
      ndat = ndag
      if (aar.eq.saar) ndat = ldag
      IF(ANALYS.EQ.5.OR.ANALYS.EQ.6) GO TO 105
      BLNR=0
      CALL LES(filn,AAR,faar,saar,ndat,STATUS)
      IF (STATUS.NE.0) RETURN
C
C     INNLESNING FERDIG
 105  CONTINUE
C
      I1=N1
      I2=NDAG
      MND=1
      DAG=1
      IF (I.EQ. 1) THEN
         MND=FMND
         CALL DAGNR(FDAG,FMND,FAAR,DAG)
         I1=DAG
         START=I1
      ENDIF
      IF (I.EQ.NAAR) THEN
         I2=LDAG
         SLUTT=I2
      ENDIF
      CALL MODEL(I1,I2,AAR,MND,DAG,START,SLUTT,F,N,plofil,nsim)
      IF (STATUS.NE.0) RETURN
      AAR=AAR+1
 110  CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
c     KARMEN VER 3.0
c     This file contains hydrological routines fot the HBV model
c     version 2.01 is an English translation of the code of version 2.0
c     Comments and some of the variables are translated
c
c     version 2.10 contains distributed soil moisture routine
c
c     version 3.10 contains distributed HBV model with vegetation zones
c                  and lake routines
c      
c     Nils Roar Sælthun, Hydrological Department,
c     Norwegian Water Resources and Energy Administration
c     May 1994
c
c
c
C----------------------------------------------------------------------
      SUBROUTINE NETTO(Day,MND,P1,TMAX,TMIN,T1,INSOIL,SPMID,SNOWCV,
     *   plow,ITVAR,SNEF,DELD,
     *   QGLAC,BMASS,GLAC,Q,EA,eal,rnet,sout,quu,qul,aperc,ql,glacm,
     *   adjust)
C----------------------------------------------------------------------
C     Nils Roar Sælthun, Hydrological Department
C
C     Calculates snowmelt, snow accumulation, and changes in snowcover,
C     and input to soil moisture.
C
C     The routine is called once per timestep, and runs through all
C     altitude levels
C
C     Modified for variable albedo and variable temperature index
C
C     This modifications includes the following new parameters:
C      ALDR;  aging factor for albedo, default value:       0.02
C      CRAD;  weight for radiation component, default:         0
C      CCONV; weight for convection component, default:        1
C      CCOND; weight for condensaton component, default:       0
C
C     NRS, Mar 1989
c
C     Modified for increase in melt on glacier ice (snow free areas)
C     ISKORR; correction factor for ice melt, default         1
C
C     Option for evaluation of effects of climatic change included.
C     Activated by setting IND(28) = 1
C     Temperature dependent evapotranspiration should be used (IND(25)=1)
C     for such runs
C
C     NRS, Jan 1990
C
C
C        PARAMETER       TYPE       COMMENT
C
C        P1               R,INN/UT   Observed precipitation in/mean precipitation out
C        T1               R,INN/UT   Observed temperature in/mean temperature out
C        INSOIL           R,UT       Water percolation into soil moisture zone
C        SPMID            R,UT       Water equivalent of snow
C        Plow             R,UT       Precipitation in lowest altitude interval
C        SNOWCV           R          Snow covered area - per cent
C        TKORR            R          Temperature correction for altitude intervals
C        PKORR            R          Precipitation correction for altitude intervals
C        TKORR1           R          Mean temperature correction
C        PKORR1           R          Mean rain correction
C        SKORR            R          Snow precipitation correction
C        SP               R          Snow water equivalent in altitude intervals
C        WC               R          Liquid watercontent in altitude intervals
C        TX               R          Snow/rain treshold
C        TS               R          Zero melt treshold
C        TSN              R          Zero melt treshold, lower part of catchment (if defined)
C        CX               R          Temperature index
C        CXN              R          Temperature index, lower part of catchment
C        CFR              R          Adjustment factor for temperature index for refreeze
C        PRO              R          Max liquid water content in snow (per cent)
C        AFELT            R          Number of altitude intervals
C        XFELT            R          Number of altitude intervals defining lower part
C                                    of catchment (default 0)
C        TKR              R          Adjustment of temperatur gradient on days with precip
C        AINDEX           R          Albedo index (1:10)
C        CRAD             R          Radiation component weight (default 0)
C        CCONV            R          Convection component weight (default 1)
C        CCOND            R          Condensation component weight (default 1)
C        LAT              R          Latitude
C        KTKORR           R          Monthly temperature correction (climate change)
C        KPKORR           R          Monthly precipitation correction (climate change)
C        KLIKOR           I          Indicator for climate change calculations (IND(28))
C        nday             I          Day no for mass balance datum
C        ea               O,R        actual evapotranspiration
C        rnet             O,R        net inflow soil moisture zone
C        sout             O,R        net outflow soil moisture zone
C        quu              O,R        outflow upper zone upper level
C        qul              O,R        outflow upper zone lower level
C        aperc            O,R        actual percolation
C        ql               O,R        outflow lower zone
C
      real bmass(0:10)

c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c ----------------------------------------------------------------------
c     common inimag - initial states
c	 ism	   soil moisture
c	 iuz	   upper zone
c	 ilz	   lower zone
c	 ilakeh    lake level
c	 ismmag	   snow per altitude level
c	 iuzmag    upper zone per altitude level
c	 ilzmag    lower zone per altitude level
c	 ispd	   snow distribution per altitude level and vegetation zone
c	 iwcd	   water content distribution per altitude level and veg.
c	 ilaket    lake temperature per altitude level
c	 iaindex   albedo index
c	 irmag	   routing states
c	 iicmag    interception content
c	 isp	   snow per altitude level
c	 iwc	   snow water content per altitude level

      real ism,iuz,ilz,ilakeh,ismmag,iuzmag,ilzmag,ispd,iwcd,
     *		   ilaket,iaindex,irmag,iicmag,isp,iwc

      common /inimag/ ism,iuz,ilz,ilakeh,ismmag(10,2),
     *             iuzmag(10,2),ilzmag(10,2),
     *             ispd(10,2,0:9),iwcd(10,2,0:9),
     *             ilaket(10),iaindex(10),irmag(5),
     *             iicmag(10,2),isp(10),iwc(10)

c -------------------------------------------------------------------
c     common mag - actual state
c        as inimag, +
c        ba(10)    snow free area per altitude zone

      real sm,uz,lz,lakeh,smmag,uzmag,lzmag,spd,wcd,ba,
     *		   laket,aindex,rmag,icmag,sp,wc

      common /mag/ sm,uz,lz,lakeh,smmag(10,2),
     *             uzmag(10,2),lzmag(10,2),
     *             spd(10,2,0:9),wcd(10,2,0:9),
     *             ba(10),laket(10),aindex(10),rmag(5),
     *             icmag(10,2),sp(10),wc(10)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common aar -    year array of daily data
c	 tdgn	      temperature at weighted station altitude
c	 pdgn	      precipitation at weighted station altitude
c	 qo	      observed runoff
c	 qs	      simulated runoff
c	 wt	      basin mean temperature
c	 wp	      basin mean precipitation
c	 wdi	      difference between observed and simulated runoff
c	 ssm	      snow melt
c	 snd	      snow cover

      real tdgn,pdgn,qo,qs,wp,wt,wdi,ssm,snd

      common /aar/
     *     tdgn(366),pdgn(366),qo(366),qs(366),wp(366),wt(366),wdi(366),
     *	   ssm(366),snd(366)

c -------------------------------------------------------------------
c     common grad -   temperature gradients
c	 tdiff	      temperature per altitude, month, and dry/wet days

      real tdiff

      COMMON/GRAD/TDIFF(0:11,12,2)

c -------------------------------------------------------------------
c     common vegtype - parameters per vegetation type
c	 icmax	       interception storage
c	 cxrel	       melt factor (relative)
c	 tsdiff        offset of zero melt temperature
c	 cvsnow        variation coefficient of snow distribution
c	 fcveg	       field capacity
c	 lpveg	       full evapotranspiration level (rel. to fc)
c	 nostype       number of vegetation types

      real  icmax,cxrel,tsdiff,cvsnow,fcveg,lpveg,epvar
      integer nostype

      common /vegtype/ icmax(15),cxrel(15),tsdiff(15),cvsnow(15),
     *                 fcveg(15),lpveg(15),epvar(15),nostype

c -------------------------------------------------------------------
c     common zones - parameters per altitude zone
c	 hoh	       mean altitude
c	 zarea	       area fraction within zone
c	 brepro        part of zone area that is glacier covered
c	 tcorr	       actual temperature correction
c	 pcorr	       actual precipitation correction
c	 vegt	       vegetation types
c	 vega	       vegetation areas
c	 nvz	       number of vegetation zones (one or two)
c	 lake	       lake area
c	 sdist	       snow distribution
c	 pact	       actual precipitation
c	 noszone       number of altitude zones

      real  hoh,zarea,brepro,pcorr,tcorr,vega,lake,sdist,pact
      integer vegt,nvz,noszone

      common /zones/ hoh(10),zarea(10),brepro(10),pcorr(10),tcorr(10),
     *                 vegt(10,2),vega(10,2),nvz(10),lake(10),
     -		       sdist(10,2,9),pact(10),noszone

c -------------------------------------------------------------------
c     common climcha - climatic change parameters
c	 ktkorr        temperature offset per month
c	 kpkorr        precipitation change (relative)

      real ktkorr,kpkorr

      common /climcha/ ktkorr(12),kpkorr(12)

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

c -------------------------------------------------------------------
c     local variables
c -------------------------------------------------------------------

      REAL INSOIL,ISOILX,MW,LAT,RVEKT,COSV,ALFAR,DEKLR
      REAL LATR,QGLAC,MWGLAC,ISKORR,glacm
      real klz,perc,xpar(120),magpro,infmax,iceday,cevpl
      real kuz,kuz1,isoilz(2),baz(2),mwglaz,einter(2),eared(2)
      DIMENSION EP(12)
      LOGICAL GLAC,KORR,NYMASS
      INTEGER Day, ses, soidis

      real tx,ts,cx,cfr,dw,rkorr,skorr,hpkorr,ered,htkorr,
     -	   ptkorr,draw,ce,aldr,crad,cconv,ccond,fc,
     -	   beta,uz1,klake,nlake,delh,delf,sjopro,areal,
     -     spdist,ep,cradb,cxx,cxb,tsx,cglac,epot,pthrough,
     -     cradx,pr,ps,sncovx,glacmx,qglacx,ealx,soutx,quux,
     -     qulx,apercx,qlx,qx,earedt,qinn,qut,eax
      real p1,tmax,tmin,t1,spmid,snowcv,plow,snef,deld,q,ea,eal,
     -     rnet,sout,quu,qul,aperc,ql,adjust,tkonst,totglac,fors,
     -     delt,afelt,aimin,aimax,bmassx,pro,pt,plake,cuz,olz,
     -     asm,tn,ceff,tmaxx,tminx,px,spx,wcx,bax,t2,aiminx,ccondx
      integer idamp,klikor,mnd,itvar,nday,inedb,i,iveg,ivz

      EQUIVALENCE (PAR(40),TX),(PAR(41),TS),(PAR(42),CX),(PAR(43),CFR),
     * (PAR(44),DW),(PAR(45),RKORR),(PAR(46),SKORR),(PAR(65),HPKORR),
     * (PAR(113),SPDIST),
     * (PAR(67),EP(1)),(par(61),ered),
     * (PAR(63),HTKORR),(PAR(64),PTKORR),(PAR(99),DRAW),
     * (PAR(98),CE)

      EQUIVALENCE (PAR(49),ALDR),(PAR(50),CRAD),(PAR(51),CCONV),
     * (PAR(52),CCOND),(par(60),cevpl),(par(62),iceday),
     * (par(79),fc),(par(81),beta),(par(82),infmax),
     * (par(85),kuz1),(par(86),uz1),(par(87),kuz),(par(88),perc),
     * (par(89),klz),(par(99),draw),
     * (PAR(100),LAT),(PAR(66),ISKORR),
     * (par(56),klake),(par(57),delh),(par(58),nlake),(par(59),delf)

      equivalence (par(3),sjopro),(par(4),magpro)

      EQUIVALENCE
     *  (ind(25),idamp),(ind(27),soidis),(IND(28),KLIKOR),(ind(29),ses)
     
      equivalence (xpar(1),par(1)) ,(par(1),areal)
      SAVE NYMASS
      DATA NYMASS/.FALSE./
      DATA TKONST/1.0/
c------------------------------------------------------------------------
c     initializations
c------------------------------------------------------------------------

      totglac = par(27+noszone)
      FORS=0.0
      SM=0.0
      UZ=0.0
      LZ=0.0
      rnet = 0.0
      sout = 0.0
      q = 0.0
      ea = 0.0
      quu = 0.0
      qul = 0.0
      aperc = 0.0
      ql = 0.0
      DELT = DELD*24

      afelt = float(noszone)
      nday = ifix(par(39)+0.5)
      AIMIN=0.2
      AIMAX=0.8
      QGLAC= 0.0
      glacm = 0.0
      BMASSX=0.0
      IF (NYMASS) THEN
c--->    Glacier mass balance zeroing
      NYMASS=.FALSE.
c      DO 2345 J=0,10
c 2345      BMASS(J)=0.0
      ENDIF

      SNEF=0.0

      T1=(TMAX+TMIN)/2.0
      PRO=DW*100.0
      PT=0.0
      INSOIL=0.0
      eal = 0.0
      plake = 0.0
      CUZ = 0.0
      OLZ = 0.0
      ASM = 0.0
      SNOWCV=0.0
      SPMID=0.0
      RVEKT = 1.0
      IF (P1.GT.0.01) THEN
	 INEDB = 2
      ELSE
	 INEDB = 1
      ENDIF

      Plow=P1*pcorr(1)
      TN=T1+TDIFF(11,MND,2)
      IF (TN .LT. TX) THEN
	SNEF=P1
	P1=P1*SKORR
      ENDIF
      IF (KLIKOR.EQ.1) THEN
	 T1=T1+KTKORR(MND)
	 TMAX=TMAX+KTKORR(MND)
	 TMIN=TMIN+KTKORR(MND)
	 P1=P1*(1.0+(KPKORR(MND)-1.0))
c        print *,mnd,kpkorr(mnd),p1
      ENDIF

c------------------------------------------------------------------------
c     start calculations
c------------------------------------------------------------------------

 10   CONTINUE
c------------------------------------------------------------------------
c     Snow calculations
c------------------------------------------------------------------------
      IF (ses.eq.0) GOTO 11
c------------------------------------------------------------------------
c     albedo estimation etc
c------------------------------------------------------------------------
c---> calculate declination of sun
      DEKLR = -(2*3.14*23.27/360.)*COS(2*3.14*(Day+10.0)/365.)
      LATR = 2*3.14*LAT/360.0
c---> calculate astronomic hour angle for sun
      COSV = -TAN(LATR)*TAN(DEKLR)
      IF (COSV.LE.1.0) THEN
c---> sun is above horizon
	 IF (COSV.GT.-1.0) THEN
	    ALFAR = ACOS(COSV)
	 ELSE
c--->       midnight sun
	    ALFAR = 3.1416
	 ENDIF
c--->    calculate mean short wave radiation factor   (Kanestrom p 85)
	 RVEKT = COS(LATR)*COS(DEKLR)*SIN(ALFAR) +
     *           ALFAR*SIN(LATR)*SIN(DEKLR)
      ELSE
C--->    sun is below horizon
	 RVEKT = 0.0
      ENDIF
C-----------------------------------------------------------------------
C     Typical values for RVEKT on 60 deg lat (1.05 rad)
C      DAG         DEKLR         ALFAR              RVEKT
C      1. JAN      -0.4          0.75          0.08  0.05
C      1. MAR      -0.15         1.30          0.09  0.31
C      1. MAI       0.25         2.00          0.23  0.88
C      1. JUL       0.4          2.40          0.48  1.12
C     15. MAI       0.32         2.20          0.32  0.98
C-----------------------------------------------------------------------
C---> Normalize to 1.0 at 60 deg N, 15. May,
      RVEKT = RVEKT/0.98
C
  11  CONTINUE
      CEFF = 0.0

      DO 100 I=1,noszone
c------------------------------------------------------------------------
c     Run through altitude intervals
c------------------------------------------------------------------------
C---> Adjust precipitation and temperature

      TMAXX = TMAX+TDIFF(I,MND,INEDB)
      TMINX = TMIN+TDIFF(I,MND,INEDB)
      PX = P1*pcorr(I)
      pact(i) = px
      tcorr(i) = TDIFF(I,MND,INEDB)
      SPX = SP(I)
      WCX = WC(I)
      BAX = BA(I)
      T2 = (TMAXX+TMINX)/2.0
C
C     "SMHI version":
C
C      IF (T2 .LT. TX) THEN
C        PX=PX*SKORR
C      ENDIF

C---> Albedo calculations etc
      AIMINX = AIMIN
      CCONDX = 0.0
c++     IF (T2 .GE. TX .AND. SPX .LT. 0.001 .AND..NOT.GLAC) THEN
C--->    Any precipitation is rain, and no snow in interval
c++      ISOILX=PX
c++      AINDEX(I)=0.0
c++     ELSE
c------------------------------------------------------------------------
c     calculate new albedo index
c------------------------------------------------------------------------
C--->    limiting minimum value
      IF (T2 .LE. TX) THEN
        IF (SPX.GT.0.00) THEN
           AIMINX=0.6-(0.6-AIMIN)*WCX/SPX/DW
        ELSE IF (PX.GT.0.0) THEN
C--->          First snow
           AINDEX(I)=AIMAX
        ELSE
           AINDEX(I)=0.0
           GOTO 130
        ENDIF
        IF (PX.GT.5.0) THEN
C--->          New snow
           AINDEX(I)=AIMAX-(AIMAX-0.5)*WCX/(SPX+PX)/DW
        ELSE IF (T2.GT.-10.) THEN
           AINDEX(I)=
     *               AIMINX+(AINDEX(I)-AIMINX)*(1.0-ALDR*(10+T2)*0.05)
        ENDIF
      ELSE
        AINDEX(I)=AIMINX+(AINDEX(I)-AIMINX)*(1.0-ALDR*T2)
        IF (PX.GE.0.1) THEN
           CCONDX=CCOND
        ELSE
           CCONDX=0.0
        ENDIF
      ENDIF

 130  CONTINUE

      if (ses .eq. 1) then
	 CRADX=CRAD*RVEKT*(1.0-AINDEX(I))/0.5*EXP(-PX*0.1)
C--->    Radiation component for glacier ice
	 CRADB=CRAD*RVEKT*(1.0-AIMIN)/0.5*EXP(-PX*0.1)
	 CXX=CX*(CRADX+CCONV+CCONDX)
	 cxb=cx*(cradb+cconv+ccondx)
      else
	CXX = CX
	cxb = cx
      endif

      tsx = ts
      cglac = cxb*iskorr

c     write (*,*) i,px,tmaxx,tminx,aindex(i)

      spx = 0.0
      wcx = 0.0
      bax = 0.0
      isoilx = 0.0 
      mwglac = 0.0

      do 1600 ivz = 1,nvz(i)

c---> vegetation zones
      iveg = vegt(i,ivz)

c---> evapotranspiration

      IF (IDAMP.EQ.1) THEN
        EPOT=CE*T2*(1.0 + (ep(mnd)-1.0)*epvar(iveg))
        IF (T2.LT.0) EPOT=0.0
      ELSE
        EPOT=EP(MND)
      ENDIF

c---> interception storage

      iicmag(i,ivz) = iicmag(i,ivz) + px
      if (iicmag(i,ivz) .gt. icmax(iveg)) then
         pthrough = iicmag(i,ivz) - icmax(iveg)
         iicmag(i,ivz) = icmax(iveg)
      else
         pthrough = 0.0
      endif
     
      iicmag(i,ivz) = iicmag(i,ivz) - epot

      if (iicmag(i,ivz) .lt. 0.0) then
        einter(ivz) = epot + iicmag(i,ivz)
        eared(ivz) = einter(ivz)*ered
        iicmag(i,ivz) = 0.0 
      else
        einter(ivz) = epot
        eared(ivz) = epot*ered
      endif
 
      CALL NEDB(pthrough,TMAXX,TMINX,TX,TSX+tsdiff(iveg),
     *      CXX*cxrel(iveg),
     *      CFR,PR,PS,MW,ITVAR,CGLAC,MWGLAz)
      CALL SNO(PR,PS,MW,ISOILz(ivz),BAZ(ivz),SPD,WCD,10,I,PRO,
     *   TKONST,SPDIST,SDIST,snofrac,ivz)
      if (lake(i).lt.1.0) then
        spx = spx + spd(i,ivz,0)*vega(i,ivz)/(1.0-lake(i))
        wcx = wcx + wcd(i,ivz,0)*vega(i,ivz)/(1.0-lake(i))
        bax = bax + baz(ivz)*vega(i,ivz)/(1.0-lake(i))
        isoilx = isoilx + isoilz(ivz)*vega(i,ivz)/(1.0-lake(i))
        ceff = ceff + 
     *      cxx*cxrel(iveg)*vega(i,ivz)/(1.0-lake(i))*(1.0-bax)*zarea(i)
        mwglac = mwglac + mwglaz*vega(i,ivz)/(1.0-lake(i))
      endif
 1600 continue

c------------------------------------------------------------------------
c        End snow calculations
c------------------------------------------------------------------------

c++     ENDIF

c++ 130 CONTINUE

c---> Interval update
      PT=PT+PX*zarea(i)
      IF (SPX.GT.0.0) THEN
	 SNCOVX=(1.0-BAX)*100.
	 SNOWCV=SNOWCV+SNCOVX*zarea(i)
      else
	 sncovx = 0.0
      ENDIF

C     IF (SNCOVX.GT.1.0) WRITE (16,'(2I5,10F6.2)')
C    * Day,I,PX,T2,
C    * SNCOVX,AINDEX(I),RVEKT,CRADX,CCONV,CCONDX,CXX
      rnet = rnet + isoilx*zarea(i)
      glacmx = mwglac*(100-sncovx)/100.0*brepro(i)
      QGLACX = mwglac*(100-sncovx)/100.0 + ISOILX

      BMASS(I) =
     *    BMASS(I)+ PS - MW*SNCOVX/100.-MWGLAC*(100.0-SNCOVX)/100.0
      BMASSX=
     *   BMASSX+
     *  (PS-MW*SNCOVX/100.-MWGLAC*(100.0-SNCOVX)/100.0)*brepro(I)
     *  *zarea(i)

C---> Calculate new lake temperature
      if (iceday. gt. 0) then
	 laket(i) = (1.0-1.0/iceday)*laket(i) + t2/iceday
      else
	 laket(i) = t2
      endif

C---> Calculate lake evaporation

      if (idamp.eq.0) then
	 ealx = cevpl*epot*bax
      else
	 ealx = ce*cevpl*laket(i)
	 if (ealx .lt. 0.0) ealx = 0.0
      endif

      eal = eal + ealx*zarea(i)*lake(i)
      plake = plake + px*zarea(i)*lake(i)
 
c      print *,idamp,ce,cevpl,laket(i),ealx,zarea(i),lake(i),eal

      if (soidis.eq.1) then
c----------------------------------------------------------------------
c    distributed HBV
c----------------------------------------------------------------------

      do 1700 ivz = 1,nvz(i)
     
c---> Vegetation zones
      if (vega(i,ivz) .eq. 0.0) goto 1700
      iveg = vegt(i,ivz)
c      write(*,*)'dette i,ivz: ',i,ivz,smmag(i,ivz)
      call hbv
     *   (delt,infmax,fcveg(iveg),fcveg(iveg)*lpveg(iveg),
     *   beta,kuz1,uz1,kuz,perc,draw,
     *   klz,1.0-baz(ivz),isoilz(ivz),qglacx,brepro(i),epot,eared(ivz),
     *   smmag(i,ivz),uzmag(i,ivz),lzmag(i,ivz),
     *   eax,soutx,quux,qulx,apercx,qlx,qx)

      sm = sm + smmag(i,ivz)*vega(i,ivz)/(1.0-lake(i))*zarea(i)
     *          *(1.0-brepro(i))
      uz = uz + uzmag(i,ivz)*vega(i,ivz)/(1.0-lake(i))*zarea(i)
      lz = lz + lzmag(i,ivz)*vega(i,ivz)/(1.0-lake(i))*zarea(i)
      ea = ea + (eax+einter(ivz))*vega(i,ivz)*zarea(i)
     *          *(1.0-brepro(i))
      q  = q  + qx*vega(i,ivz)*zarea(i)
      quu = quu + quux*vega(i,ivz)/(1.0-lake(i))*zarea(i)
      qul = qul + qulx*vega(i,ivz)/(1.0-lake(i))*zarea(i)
      aperc = aperc +apercx*vega(i,ivz)/(1.0-lake(i))*zarea(i)
      ql = ql + qlx*vega(i,ivz)/(1.0-lake(i))*zarea(i)
      sout = sout + soutx*vega(i,ivz)/(1.0-lake(i))*zarea(i)

 1700 continue
      
      endif
       
      INSOIL = INSOIL+ISOILX*zarea(i)
      if (brepro(i).gt.0.0) then
	 QGLAC	= QGLAC + QGLACX*zarea(i)*brepro(i)/totglac
	 glacm	= glacm + glacmx*zarea(i)
      endif
      SP(I) = SPX
      WC(I) = WCX
      BA(I) = BAX
      SPMID = SPMID+(WCX+SPX)*zarea(i)
     
c      write (*,'(a,i5,2f10.2)') 'netto:',i,qx,q
100   CONTINUE
   
c------------------------------------------------------------------------
c     End altitude interval loop
c------------------------------------------------------------------------
c---> Catchment update
      IF (TOTGLAC.GT.0.0) BMASS(0) = BMASS(0) + bmassx
      IF (SNOWCV.GT.1.00) CEFF = CEFF/SNOWCV
C     WRITE (16,'(I5,10F6.2)') Day,COSV,DEKLR,ALFAR,RVEKT,SNOWCV,CEFF
      P1=PT
      T1=T1+TDIFF(0,MND,INEDB)

c      WRITE (*,'(I5,10F6.2)') Day,P1,T1,SNOWCV,spmid,insoil
c      write (*,'(A,F10.2)') 'netto insoil',insoil
      if (soidis .ne. 1) then
c----------------------------------------------------------------------
c    lumped HBV
c----------------------------------------------------------------------
	 IF (IDAMP.EQ.1) THEN
	    EPOT=CE*T1*EP(mnd)
	    IF (T1.LT.0) EPOT=0.0
	 ELSE
	    EPOT=EP(MND)
	 ENDIF
	 call hbv
     *   (delt,infmax,fc,fc*par(80),beta,kuz1,uz1,kuz,perc,draw,
     *	  klz,snowcv/100.0,insoil,qglac,totglac,epot,earedt,sm,uz,lz,
     *	  ea,sout,quu,qul,aperc,ql,q)
C      else
C        xmag(1) = sm
C        xmag(2) = uz
C        xmag(3) = lz
      endif

      if (sjopro .gt. 0.0) then
	 qinn = q*delf + (plake-eal)*(sjopro-magpro)/sjopro
	 if (sjopro.gt.magpro) then
           call laker
     *          (qinn,lakeh,qut,delt,areal,areal*(sjopro-magpro),
     *           klake,delh,nlake,1.0)
	 else
	    qut = qinn
	 endif
	 q = q*(1.0-delf) + qut + (plake-eal)*magpro/sjopro
	 ea = ea + eal
      endif
 
 1000 CONTINUE
c------------------------------------------------------------------------
c     Adjust glacier mass balance (zero) day no nday
c------------------------------------------------------------------------
      KORR=.FALSE.
      IF (Day.EQ.nday .and. glac) THEN
	 spmid = 0.0
	 DO 200 I=1,noszone
	    IF (SP(I).GT.SPDIST) THEN
	       adjust = adjust + (sp(i)-spdist)*zarea(i)
	       KORR=.TRUE.
	       SP(I)=SPDIST
	       if (wc(i).gt.sp(i)*pro/100.) then
		  adjust = adjust + (wc(i)-sp(i)*pro/100.)*zarea(i)
		  WC(I)=SP(i)*PRO/100.
	       endif
	    ENDIF
	    SPMID= spmid+(sp(i)+wc(i))*zarea(i)
 200	 CONTINUE
	 IF (KORR) CALL SNDIST(SP,WC,BA,SPD,WCD,10,noszone,nvz,
     *        SDIST,PRO,SPDIST,snofrac)
	 NYMASS=.TRUE.
      ENDIF
      RETURN
      E N D

C----------------------------------------------------------------------
      SUBROUTINE NEDB(P,TMAX,TMIN,TX,TS,CX,CFR,PR,PS,MW,ITVAR,
     * CGLAC,MWGLAC)
C----------------------------------------------------------------------
C      Nils Roar Sælthun, Hydrological Dept
C
C      Calculates precipitation as rain (PR)
C                 precipitation as snow (PS)
C                 potential melt (full snow cover) (MW)
C                 potential melt on glaciers (MWGLAC)
C
C           Run once per time step and altitude level
C
C     P:    precipitation in mm
C     TMAX: maximum temperature
C     TMIN: minimum temperature
C     TX:   treshold temperature rain/snow
C     TS:   treshold temperature melt/no melt
C     CX:   temperature index for melt
C     CFR:  correction factor for temperature index on refreeze
C     PR:   precipitation as rain (mm, output)
C     PS:   precipitation as snow (mm, output)
C     MW:   potential melt/refreeze (negative) (mm, output)
C
C     ITVAR:1: melt computation based on mean temperature
C           0:  "       "         "    " linear variation of temperature
C                                        over time step
C     DELD: time step in days
C     CGLAC:correction of temperature index for glacier ice
C

      REAL P,TMAX,TMIN,TX,TS,CX,CFR,PR,PS,MW,CGLAC,MWGLAC
      INTEGER ITVAR

c-------------------------------------------------------------------------
c     Local variables
c-------------------------------------------------------------------------
      real t,tids,tidf,smx,sfx

C-------------------------------------------------------------------------
C     Rain/snow determination
C------------------------------------------------------------------------
      T=(TMAX+TMIN)/2.0
      IF (T.GT.TX) THEN
	 PR=P
	 PS=0.0
      ELSE
	 PS=P
	 PR=0.0
      ENDIF
      IF (TMAX.GT.TS.AND.TMIN.LT.TS.AND.ITVAR.EQ.0) GOTO 100

C-------------------------------------------------------------------------
C     Calculaton based on linear variation of temperature
C-------------------------------------------------------------------------

      MW=CX*(T-TS)
      MWGLAC  = CGLAC*(T-TS)
      IF (T.LT.TS) THEN
	MW=MW*CFR
	MWGLAC = 0.0
      ENDIF
      RETURN

  100 CONTINUE

C-------------------------------------------------------------------------
C     Determine fraction of timestep with melt (TIDS) and refreeze (TIDF)
C-------------------------------------------------------------------------
      TIDS=(TMAX-TS)/(TMAX-TMIN)
      TIDF=1.0-TIDS
      SMX=CX*(TMAX-TS)*TIDS/2.0
      SFX=CX*CFR*(TS-TMIN)*TIDF/2.0
      MW=SMX-SFX
      MWGLAC = SMX*CGLAC/CX
      RETURN
      E N D

C----------------------------------------------------------------------
      SUBROUTINE SNO(PR,PS,MW,ISOILX,bax,SPD,WCD,IDIM,I,
     *               PRO,TKONST,SPDIST,SDIST,snofrac,ivz)
C----------------------------------------------------------------------
C     Nils Roar Sælthun, Hydrologisk avdeling
C
C     Calculates new snow distribution (SPD,WCD)
C                new mean snow and snow water values (SPX,WCX)
C                and water production (to soil moisture) (ISOILX)
C                in altitude interval I
C                with distributed snow cover (BAX.NE.1.0)
C                alternatively uniform snow cover
C
C     Input data:
C     PR         precipitation as rain
C     PS         precipitation as snow
C     MW         potential melt (full snow cover)
C     MWGLAC     potential melt on glaciers
C     BAX        area with uniform snowcover or no snow
C     I          altitude interval no
C     NFELT      number of altitude levels in lower part of catchment
C     PRO        max water content in snow (in per cent)
 
C
C     NILS ROAR SÆLTHUN, jan 1994
C
C
      REAL SPD(IDIM,2,0:9),WCD(IDIM,2,0:9),SDIST(idim,2,9),snofrac(9)
      REAL PR,PS,MW,SPX,WCX,ISOILX,BAX,PRO,TKONST,SPDIST
      INTEGER I,IVZ,IDIM

c-------------------------------------------------------------------------
c     Local variables
c-------------------------------------------------------------------------
      real xmw,xmwa,totsn,spt,wct,psk,wm,wf1
      integer k

      if (spd(i,ivz,0) .eq. 0.0 .and. ps .eq. 0.0) then
	 bax = 1.0
	 isoilx = pr
	 return
      else
	 spx = spd(i,ivz,0)
	 wcx = wcd(i,ivz,0)
      
	 XMW = MW
	 XMWA = XMW
	 TOTSN=SPX+WCX
     
C---------------------------------------------------------------------------
C     Distributed (uneven) snow cover
C---------------------------------------------------------------------------
	 bax = 0.0
	 SPT = 0.0
	 WCT = 0.0

C---> Runs through distribution, adds snow, subtracts snow melt

      do 101 k = 1,9
C--->    Distribute snow fall
	if (ps.gt.0.0.and.spx.gt.spdist) then
	   psk = ps*sdist(i,ivz,k)
	else
	   psk = ps
	endif

	IF (-XMWA.GT.WCD(I,ivz,k)) XMWA = -WCD(I,ivz,k)
	SPD(I,ivz,k) = SPD(I,ivz,k)+psk-XMWA

	IF (SPD(I,ivz,k).le.0.001) THEN
	   XMWA = XMWA+SPD(I,ivz,k)
	   SPD(I,ivz,k) = 0.0
	   bax = bax + snofrac(k)
	ENDIF

c         write (*,'(3i3,4f5.2)') 
c     *     i,ivz,k,psk,spd(i,ivz,k),snofrac(k),bax
c         read (*,'(a)') tt

C--->    Alter liquid water content
	WCD(i,ivz,k) = WCD(i,ivz,k)+PR+XMWA
	IF (WCD(i,ivz,k).LT.0.0) WCD(i,ivz,k)=0.0
	WM = SPD(i,ivz,k)*PRO/100.0
	WF1 = WCD(i,ivz,k)
C--->    Delays snow melt (percolation)
	IF (WF1.GT.WM) WCD(I,ivz,k)=WF1-(WF1-WM)*TKONST
	IF (WCD(I,ivz,k).GT.SPD(i,ivz,k)*0.20)
     *       WCD(I,ivz,k) = SPD(i,ivz,k)*0.20
	WCT = WCT + wcd(i,ivz,k)*snofrac(k)
	SPT = SPT + spd(i,ivz,k)*snofrac(k)
 101  continue

 200  CONTINUE
C---------------------------------------------------------------------------
C     Calculate new mean snow and liquid water content, and percolation to
C     soil moisture zone (INSOILX)
C---------------------------------------------------------------------------
    
      spd(i,ivz,0) = SPT
      wcd(i,ivz,0) = WCT

      ISOILX=TOTSN+PS+PR-spt-wct

C     PRINT 222,(SPD(I,ivz,J),J=0,9)
C     PRINT 222,(WCD(I,ivz,J),J=0,9)
C     PRINT 222,BAX,tots,ISOILX
 222  FORMAT(10F8.2)
      RETURN
      endif
      E N D


C----------------------------------------------------------------------
      SUBROUTINE HBV
     *  (delt,infmax,fc,lp,beta,kuz1,uz1,kuz,perc,draw,
     *  klz,sncov,insoil,qglac,brepro,epot,eared,
     *  sm,uz,lz,ea,sout,quu,qul,aperc,ql,q)

C----------------------------------------------------------------------
C    Nils Roar Sælthun, Hydrological Department
C
C        Dynamic runoff part of the HBV model
C        Runs once per time step
C----------------------------------------------------------------------
C        Parameter    Type      Description
C        DELT         R,I       time step                          [h]
C        INFMAX       R,I       max infiltration                   [mm/h]
C        FC           R         Soil moisture field cappacity (mm)
C        BETA         R         Nonlinearity parameter soil moisture (1)
C        LP           R         Soil moisture level for potential evap
C        KUZ1         R         Quick response, upper zone (1/D)
C        UZ1          R         Treshold, quick response upper zone
C        KUZ          R         Slow response upper zone (1/D)
C        PERC         R         Percolation parameter upper zone to lower
c        DRAW         R         "draw up" parameter to soil moisture zone
c                                flux i mm/d when lower zone is filled and
c                                soil moisture is at half field capacity
C        KLZ          R         Response lower zone
C        SNCOV        R,I       Snow covered area                  [1]                     
C        INSOIL       R,I       Water input (rain/snow melt)       [mm]
C        QGLAC        R,I       Water in from glaciers             [mm]
C        PAREA        R,I       Precipitation                      [mm]
C        EPOT         R,I       Potential evapotranspiration       [mm/d]
C        SM           R,I/O     Soil moisture                      [mm]
C        UZ           R,I/O     Water content upper zone           [mm]
C        LZ           R,I/O     Water content lower zone           [mm]
C        EA           R,O       Actual evapotranspiration          [mm]
C        SOUT         R,O       From soil moisture + glaciers      [mm]
C        QUU          R,O       Outflow upper level upper zone     [mm]
C        QUL          R,O       Outflow lower level upper zone     [mm]
C        APERC        R,O       Actual percolation to lower zone   [mm]
C        QL           R,O       Outflow lower zone                 [mm]
C        Q            R,O       Outflow                            [mm]
C----------------------------------------------------------------------
C
      REAL INSOIL,SM,FC,LP,UZ,UZ1,LZ,KUZ,KUZ1,KLZ,EPOT,PERC
      REAL DELT,BETA,INFMAX,DRAW,SNCOV,QGLAC,BREPRO,EARED,
     -     EA,SOUT,QUU,QUL,APERC,QL,Q

c-------------------------------------------------------------------------
c     Local variables
c-------------------------------------------------------------------------
      real cuz,out,olz,uq,lzmax

      LZMAX = PERC/KLZ

c----------------------------------------------------------------------
c    soil moisture zone
c----------------------------------------------------------------------
c---> max infiltration

      IF (INSOIL.GT.INFMAX*delt) THEN
	 CUZ=INSOIL-INFMAX*delt
	 INSOIL =INFMAX*delt
      ELSE
	 CUZ=0.0
      ENDIF 
c      write (*,'(a,3f10.2)') ' hbv: insoil sm draw', insoil,sm,draw
      call soil
     *   (fc,lp,beta,epot,eared,delt,draw,lz,lzmax,
     *    insoil,sncov,sm,out,ea,olz)   
     
c      write (*,'(a,4f10.2)') 'insoil sm cuz ea olz',insoil,sm,out,ea

c---> precip on land area

       CUZ = (CUZ + OUT)*(1-brepro) + QGLAC*brepro

       sout = cuz
   
c---> water to upper zone
      
      call upzon (kuz1,uz1,kuz,perc,delt,cuz,aperc,uz,quu,qul,uq)
c      write (*,'(a,3f10.2)') ' cuz uz uq',cuz,uz,uq
c      write (*,'(a,5f10.2)') 
c     *   ' aperc olz sjopro sncov',aperc,olz,sjopro,sncov,epot
      aperc = aperc - olz 
      call lozon (klz,delt,aperc,lz,ql)
c      write (*,'(a,3f10.2)') ' aperc lz lq', aperc,lz,ql
   
      Q = uq + ql

c      write (*,'(A,3f10.2)') ' hbv:',sm,uz,lz
      RETURN
      E N D
    
    
C ----------------------------------------------------------------------
      SUBROUTINE ROUT (Q,PAR,QN,FAREAL,TIDOP,indeks)
C----------------------------------------------------------------------
C    Nils Roar Sælthun, Hydrological Departement
C
C        Routing module
C        Runs once per time step
C----------------------------------------------------------------------
C        Parameter    Type      Description
C
C        QN           R, I/O    Runoff present timestep
C        PAR          R, I      Routing parameters
C        QN           R, I/O    Runoff stored to next timesteps
C        FAREAL       R, I      Catchment area
C        TIDOP        I, I      Time step (days)
C        indeks       I, I      routingvelger
C---------------------------------------------------------------------
C        Three routing methods are implemented:
C
C        Method 1: Routing through lake with known level-discharge
C                  relationship
C        Method 2: Fixed weights for up to 6 timesteps
C                  W6 = 1.0 - W1 - W2 - W3 - W4 - W5
C        Method 3: Triangular "unit hydrograph", discharge dependent
C                  time base
C
C        indeks = 1: method 1
C        indeks = 2: method 2
C        indeks = 3: method 3
C
C
C        PAR(1)       W1        meth 1: 1th weight
C                     Q1        meth 2: disharge level for T1
C                     SAREAL    meth 3: lake area
C        PAR(2)       W2        1: 2nd weight
C                     T1        2: time base for triangular UH
C                     K         3: rating curve constant                  
C        PAR(3)       W3        1: 3rd weight
C                     Q2        2: discharge level for T2
C                     H0        3: saddle point          
C        PAR(4)       W4        1: 4th weight
C                     T2        2: time base for triangular UH
C                     N         3: exponent in rating curve
C        PAR(5)       W5        1: 5nd weight
C                               2:                            
C                     DELF      3: part of catchment draining through lake
C---------------------------------------------------------------------
      REAL Q,PAR(5),QN(5)      
      INTEGER TIDOP, INDEKS 

c---------------------------------------------------------------------
c     Local variables
c---------------------------------------------------------------------

      REAL K,N,DELH,fareal,q1,t1,q2,t2,qinn,delt,sareal,delf,h1,
     -     dqh,h,h2,qut,t,tvekt,triang,qt 
      INTEGER it,it2,i           
      REAL VEKT(6)
      DATA VEKT/6*0.0/                                                  ROUT
      Q1=PAR(1)                                                         ROUT
      T1=PAR(2)                                                         ROUT
      Q2=PAR(3)                                                         ROUT
      T2=PAR(4)                                                         ROUT
      IF (indeks .eq. 0) RETURN                                         ROUT
      IF (Q .LT. -9999.) RETURN                                         ROUT
      IF (indeks .eq. 3) GOTO 8                                         ROUT
      IF (indeks .eq. 2) GOTO 2                                         ROUT
*-    ---------------------------------------------------------------*         
*     Lake routing                                                   *         
*-    ---------------------------------------------------------------*         
      QINN   = Q                                                               
      DELT   = TIDOP*24
      SAREAL = PAR(1)                                                         
      K      = PAR(2)                                                         
      DELH   = PAR(3)                                                         
      N      = PAR(4)                                                         
      DELF   = PAR(5)                                                         

      H1     = QN(1)                                                           

      IF (H1.LT.-DELH) H1 = -DELH                                              
      DQH = QINN*FAREAL*DELF/SAREAL/1000.                                      
*-    ---------------------------------------------------------------*         
*     lake level                                                     *         
*-    ---------------------------------------------------------------*         
                                           
      H = H1+DQH/2.0-K*86.4*DELT/24.0*(H1+DELH)**N/(2.0*SAREAL*1000.)          
                                           
*-    ---------------------------------------------------------------*         
*     lake outflow in mm                                             *         
*-    ---------------------------------------------------------------*         
                                           
      IF (H.LT.-DELH) H=-DELH                                                  
      QT = K*86.4*DELT/24.0*(H+DELH)**N/(FAREAL*DELF)                          
      H2 = H1+DQH-QT*FAREAL*DELF/SAREAL/1000.                                  
      QN(1)= H2                                                                
      QUT = QINN*(1.0-DELF)+QT*DELF                                            
      Q   = QUT
                                           
      R E T U R N                                                       

  2   CONTINUE                                                         
*-    ---------------------------------------------------------------*         
*     fixed weights                                                  *         
*-    ---------------------------------------------------------------*         
      VEKT(1)=PAR(1)                                                    
      VEKT(2)=PAR(2)                                                   
      VEKT(3)=PAR(3)                                                    
      VEKT(4)=PAR(4)                                                    
      VEKT(5)=PAR(5)                                                    
      VEKT(6)=1 - par(1) - par(2) - par(3) - par(4) - par(5)           
      GOTO 100                                                         

 8    CONTINUE                                                         
*-    ---------------------------------------------------------------*         
*     UH with variable time base                                     *         
*-    ---------------------------------------------------------------*         
C---> calculate actual time base
      T=(T2-(-T1))*(Q-(-Q1))/(-Q2-(-Q1))+(-T1)                         
C---> calculate weights                                                 ROUT
      IF (T .GT. 1.0)   GOTO 5                                          ROUT
      VEKT(1)=1.0                                                       ROUT
      GOTO 100

  5   CONTINUE                                                          ROUT
      IF (T .GT. 2.0) GOTO 6                                            ROUT
      VEKT(2)=2.0*(T-1.0)*(T-1.0)/(T*T)                                 ROUT
      VEKT(1)=1.0-VEKT(2)                                               ROUT
      GOTO 100                                                          ROUT
  6   CONTINUE                                                          ROUT
      IF(T.GT.5) T=5.0                                                  ROUT
      T2=T*0.5                                                          ROUT
      IT=IFIX(T)                                                        ROUT
      IT2=IFIX(T2)                                                      ROUT
      TVEKT=0.0                                                         ROUT
      H=4.0/T                                                           ROUT
      DO 7 I=1,IT                                                       ROUT
	 TRIANG=0.5*I*H*I/T
	 VEKT(I)=TRIANG-TVEKT
	 TVEKT=TRIANG
	 IF (I .GT.IT2) VEKT(I)=H-VEKT(I)
  7   CONTINUE                                                           ROUT

      VEKT(IT2+1)=VEKT(IT2+1)-0.5*H*(1.0-IT2/T2)*(T2-IT2)               ROUT
      VEKT(IT+1)=H*(T-IT) - (2.0-TVEKT)                                 ROUT

 100  CONTINUE                                                          ROUT
*-    ---------------------------------------------------------------*         
*     calculate discharge                                            *         
*-    ---------------------------------------------------------------*         
      QT=QN(1)+Q*VEKT(1)                                                ROUT
      VEKT(1)=0.0                                                       ROUT

      DO 10 I=2,5                                                       ROUT
	QN(I-1)=QN(I)+Q*VEKT(I)
	VEKT(I)=0.0
 10   CONTINUE                                                          ROUT

      QN(5)=Q*VEKT(6)
      VEKT(6)=0.0                                                       ROUT
      Q=QT                                                              ROUT
      R E T U R N                                                       ROUT
      E N D                                                             ROUT

C ----------------------------------------------------------------------
      SUBROUTINE MAGINN(ANALYS,STATUS)
C ----------------------------------------------------------------------
C
      integer analys,status

c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)

c--------------------------------------------------------------------
c     Local variables
c--------------------------------------------------------------------
      real BUFFER(200)
      integer IBUFF(200),IUT(3,50)
      integer nbl,nsist,isist,laar,ldag,j1,j2,naar,ndag,n,i,
     -        nrbl,nr,k
c--------------------------------------------------------------------
c     Functions
c--------------------------------------------------------------------
      integer isk,ijanei

      EQUIVALENCE(BUFFER(1),IBUFF(1))
C     LESER MAGASINDATA FRA TILST
      STATUS = 0
      OPEN(LUTIL,FILE='TILST.DAT',ACCESS='DIRECT',
     * RECL=200*4,STATUS='OLD',
     * FORM='UNFORMATTED',IOSTAT=STATUS,ERR=999)
      NBL=IND(1)+1
      READ(LUTIL,REC=NBL,IOSTAT=STATUS,ERR=999) IBUFF
      IF(STATUS.NE.0) GO TO 999
      NSIST=IBUFF(1)
      if (nsist.eq.0) return
      ISIST=NSIST*3-1
      LAAR=IBUFF(ISIST)
      LDAG=IBUFF(ISIST+1)
      NRBL=IBUFF(ISIST+2)
      J1=0
      J2=0
      CALL DAGNR(J1,J2,LAAR,LDAG)
      IF (ANALYS.NE.5) GOTO 123
  120 WRITE(OUT,122) J1,J2,LAAR
  122 FORMAT (/,' SISTE OPPDATERING GIKK FRAM TIL',3I5,//)
      GOTO (123,201,120)
     -IJANEI(OUT,'$VIL DU VELGE EN TIDLIGERE STARTDATO ? ')
  201 NAAR = LAAR
      NDAG = LDAG+1
      IF (NDAG .GT. 365+ISK(NAAR)) NDAG = 1
      IF (NDAG .EQ. 1) NAAR = NAAR+1
      IND(3) = 0
      IND(4) = 0
      CALL DAGNR(IND(4),IND(3),NAAR,NDAG)
      IND(2) = NAAR
      GO TO 125
  123 CONTINUE
      WRITE(OUT,124)
  124 FORMAT (/,' Choose initial state: ')
      DO 126 N = 1,NSIST
      NAAR = IBUFF(N*3-1)
      NDAG = IBUFF(N*3)+1
      IF (NDAG .GT. 365+ISK(NAAR)) NDAG = 1
      IF (NDAG .EQ. 1) NAAR = NAAR+1
      IUT (1,N) = 0
      IUT (2,N) = 0
      CALL DAGNR(IUT(1,N),IUT(2,N),NAAR,NDAG)
      IUT(3,N) = NAAR
  126 CONTINUE
      WRITE(OUT,127) (N,(IUT(I,N),I = 1,3),N=1,NSIST)
  127 FORMAT (3(5X,I2,' :',2I3,I5))
      CALL INTIN(OUT,NR,'$Enter state no',1,NSIST,1,*123)
      IND(2) = IUT(3,NR)
      IND(3) = IUT(2,NR)
      IND(4) = IUT(1,NR)
      NRBL=IBUFF(NR*3+1)
C
C     LESER MAGASINDATA
C
  125 READ(LUTIL,REC=NRBL,IOSTAT=STATUS,ERR=999) BUFFER
      IF(STATUS.NE.0) GO TO 999
C
C     SJEKKER DATO OG STASJONSNUMMER
C
      CALL DAGNR(IND(4),IND(3),IND(2),NDAG)
      LDAG=NDAG-1
      LAAR=IND(2)
      IF(LDAG .EQ. 0) LAAR = LAAR-1
      IF(LDAG .EQ. 0) LDAG = 365+ISK(LAAR)
      IF (IBUFF(1) .NE. IND(1) .OR. IBUFF(2) .NE. LAAR
     * .OR. IBUFF(3) .NE. LDAG) WRITE(OUT,129) (IBUFF(I),I= 1,3)
  129 FORMAT (/,' **** Error in reading state data file:',3I5,' ****')
      DO 130 K = 1,147
c hele rutinen må korrigeres    130 IMAG(K)=BUFFER(K+3)
  130 continue
  135 CLOSE(LUTIL)
      RETURN
  999 WRITE(OUT,998) STATUS
  998 FORMAT('**** LESEFEIL PÅ TILSTANDSFIL,STATUS =',I4,' ****',/)
      CLOSE(LUTIL)
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE MAGUT(STATUS)
C ----------------------------------------------------------------------

      integer status

c -------------------------------------------------------------------
c     common index - simulation options
c        ind(30)     options array

      integer ind
      COMMON/INDEX/IND(30)

c -------------------------------------------------------------------
c     common lu -  file units etc
c	 in	   standard input
c	 out	   standard output
c	 lulist    list file
c	 lupar	   parameter file
c	 lutil	   states file
c	 ludata    data file
c	 lusim	   simulated results
c	 indata
c	 mangl	   missing data value

      integer in,out,lulist,lupar,lutil,ludata,lusim,indata,mangl

      COMMON /LU/    IN,OUT,LULIST,LUPAR,LUTIL,LUDATA,LUSIM,INDATA,MANGL

c -------------------------------------------------------------------
c     common id - station identifications and weights
c	 tilsid(4)     runoff stations
c	 tilsv(4)      runoff station weights
c	 tempid(25)     temperature stations
c	 tempv(25)      temperature station weights
c	 nedid(25)     precipitation station weights
c	 nedv(25)      precipitation station weights
c	 nedhoh(25)    precipitation station altitudes
c	 temphoh(25)    temperature station altitudes

      real tilsv,tempv,nedv,nedhoh,tmphoh
      integer tilsid, tempid,nedid

      COMMON/ID/TILSID(4),TILSV(4),tempid(25),tempv(25),
     * nedid(25),nedv(25),nedhoh(25),tmphoh(25)
c--------------------------------------------------------------------
c     Local variables
c--------------------------------------------------------------------
      real BUFFER(200)
      integer IBUFF(200)
      integer nsist,isist,ndag,n,i,nrbl,maxbl
      LOGICAL FINNES

c--------------------------------------------------------------------
c     Functions
c--------------------------------------------------------------------


      EQUIVALENCE (BUFFER(1),IBUFF(1))
      DATA IBUFF/200*0/
C
C     LEGGER UT MAGASINDATA PÅ TILSTANDSFIL
C
      INQUIRE(FILE='TILST.DAT',EXIST=FINNES)
      IF (.NOT.FINNES) GOTO 201
      OPEN(LUTIL,FILE='TILST.DAT',ACCESS='DIRECT',
     * RECL=200*4,STATUS='OLD',
     * FORM='UNFORMATTED',IOSTAT=STATUS,ERR=999)
      READ(LUTIL,REC=1,IOSTAT=STATUS,ERR=999) IBUFF
      MAXBL=IBUFF(1)+1
      NRBL=MAXBL
      GOTO 10
  201 CONTINUE
C
C     INITIERER FILE
C
      OPEN(LUTIL,FILE='TILST.DAT',ACCESS='DIRECT',
     * RECL=200*4,STATUS='NEW',
     * FORM='UNFORMATTED',IOSTAT=STATUS,ERR=999)
      IBUFF(1)=31
      DO 11 N = 1,31
      WRITE(LUTIL,REC=N,ERR=999,IOSTAT=STATUS) IBUFF
      IBUFF(1)=0
   11 CONTINUE
      NRBL = 32
      MAXBL = 32
   10 CONTINUE
      READ(LUTIL,REC=IND(1)+1,ERR=999,IOSTAT=STATUS) IBUFF
      NSIST = IBUFF(1)+1
      IF (NSIST .LT. 19) GO TO 20
C
C     RULLERER FILE
C
      MAXBL = MAXBL-1
      NRBL  = IBUFF(4)
      NSIST = NSIST -1
      DO 21 N = 1,18
      I=N*3-1
      IBUFF(I) = IBUFF(I+3)
      IBUFF(I+1) = IBUFF(I+4)
      IBUFF(I+2) = IBUFF(I+5)
   21 CONTINUE
   20 CONTINUE
      CALL DAGNR(IND(8),IND(7),IND(6),NDAG)
      IBUFF(1)=NSIST
      ISIST = NSIST*3-1
      IBUFF(ISIST) = IND(6)
      IBUFF(ISIST+1)=NDAG
      IBUFF(ISIST+2)=NRBL
      WRITE(LUTIL,REC=IND(1)+1,IOSTAT=STATUS,ERR=999) IBUFF
      IBUFF(1) = IND(1)
      IBUFF(2) = IND(6)
      IBUFF(3) = NDAG
c      DO 30 K = 1,147
c  30  BUFFER(K+3)=MAG(K)
      WRITE(LUTIL,REC=NRBL,IOSTAT=STATUS,ERR=999) BUFFER
      IBUFF(1)=MAXBL
      WRITE(LUTIL,REC=1,IOSTAT=STATUS,ERR=999) IBUFF
      CLOSE(LUTIL)
      RETURN
  999 WRITE(*,998) STATUS
  998 FORMAT('**** LESE/SKRIVEFEIL PÅ MAGASINFILE, STATUS=',I4)
      CLOSE(LUTIL)
      RETURN
      E N D

      subroutine soil 
     * (fc,lp,beta,ep,eared,
     * delt,draw,lz,lzmax,insoil,sncov,sm,out,ea,up)
C---------------------------------------------------------------------
C
C     soil moisture routine for the HBV model
C
C     Nils Roar Sælthun, NVE
C
C     Parameters:
C
C       FC      I       R       max soil moisture content
C       LP      I       R       Ea = EP when SM > LP
C       BETA    I       R       non-linearity parameter
C       EP      I       R       potential evapotranspiration, mm/d
C       DELT    I       R       time step, hours
C       DRAW    I       R       draw up parameter [mm/d]
C       LZ      I       R       lower zone content [mm]
C       LZMAX   I       R       max lower zone content [mm]
C       INSOIL  I       R       rain/snowmelt, mm
C       SNCOV   I       R       snowcovered area, [1]
C       SM      I,O     R       soil moisture content, mm
C       OUT     O       R       outflow, mm     
C       EA      O       R       actual evapotranspiration, mm
C       UP      O       R       draw up from lower zone
C
C---------------------------------------------------------------------
      real fc, lp, beta, ep, delt, insoil, draw, sm, out, ea, up
      real lz, lzmax, eared, sncov

C---------------------------------------------------------------------
C     local variables
C---------------------------------------------------------------------

      real intim, ysm, cuz
      integer i, itim

c----------------------------------------------------------------------
c    calculate "drawup" from lower zone to soil moisture zone
c----------------------------------------------------------------------
 
c      write(*,*)'soil0:sm ',sm
      up = 2.0*draw*lz/lzmax*(fc-sm)/fc*delt/24.0      

      out = 0.0 
      sm = sm + up
c      write(*,*) 'soil1 up,sm:', up,sm,draw,lz,lzmax,fc,delt,lp
c      write(*,*)'ep,sncov: ',ep,sncov,eared

C---------------------------------------------------------------------
C     actual evapotranspiration
C---------------------------------------------------------------------
      if (sm .gt. lp) then
	ea = delt*ep*(1.0-sncov)/24.0
      else
	ea = delt*ep*sm*(1.0-sncov)/lp/24.0
      endif
C---------------------------------------------------------------------
C     reduced evapotranspiration due to interception
C---------------------------------------------------------------------
      ea = ea - eared
      if (ea .lt. 0.0) ea = 0.0

      sm = sm - ea
      if (sm .lt. 0.0) sm=0.0
c      write(*,*) 'soil2 ea,sm:', ea,sm
c      write (*,*) ep,eared,ea,sm,delt,sncov,lp
c      read (*,'(A)') tt      
      if (insoil .eq. 0.0) RETURN

C---------------------------------------------------------------------
C     distribute water to soil moisture zone and upper zone
C---------------------------------------------------------------------
      intim = insoil/delt
      itim = ifix(delt+0.1)
      ysm  = sm

      do 100 i = 1,itim
	cuz = intim*(sm/fc)**beta
	sm  = sm + intim - cuz
	if (sm.gt.fc) then
	   cuz = sm - fc
	   sm = fc
	endif
	out = out + cuz
  100 continue
      
      RETURN
      E N D              

      subroutine upzon 
     * (kuz1,uz1,kuz,perc,delt,cuz,aperc,uz,uqu,uql,uq)
C---------------------------------------------------------------------
C
C     upper zone subroutine for the HBV model
C
C     Nils Roar Sælthun, NVE
C
C     Parameters:
C
C       KUZ1    I       R       res. const upper level     [1/d]
C       UZ1     I       R       threshold                  [mm]
C       KUZ     I       R       res const lower level      [1/d]
C       PERC    I       R       percolation constant       [mm/d]   
C       DELT    I       R       time step, hours
C       CUZ     I       R       inflow                     [mm]
C       UZ      I,O     R       upper zone content         [mm]
C       APERC   O       R       actual percolation         [mm]
C       uqu     O       R       outflow upper leve         [mm]
C       uql     O       R       outflow lower level        [mm]
C       UQ      I       R       outflow                    [mm]
C
C---------------------------------------------------------------------
      real kuz1, uz1, kuz, perc, delt, cuz, uz, aperc, uqu, uql, uq

C---------------------------------------------------------------------
C     local variables
C---------------------------------------------------------------------
      real up

c----------------------------------------------------------------------
c    upper zone
c----------------------------------------------------------------------
      APERC=PERC*DELT/24.0
      UP=UZ+0.5*(CUZ-APERC)
      IF (UP .LT. UZ1) THEN
c--->    no discharge from upper level of upper zone
	 IF (APERC .GT. UZ+CUZ) THEN
	    APERC=UZ+CUZ
	    uq=0.0
	    uql = 0.0
	    uqu = 0.0
	 ELSE
	    uql=UP*2.0*KUZ*DELT/24.0/(2.0+KUZ*DELT/24.0)
	    uqu=0.0
	    IF (UQ .LT. 0) THEN
c--->	       upper zone emptying
	       uql=0.0
	       APERC=UZ+CUZ
	    ENDIF
	 ENDIF
      ELSE
c--->    discharge from upper level
	 uqu = (UP-UZ1)*2.0*KUZ1*DELT/24.0/(2.0+KUZ1*DELT/24.0)
	 uql = UZ1*KUZ*DELT/24.0
      ENDIF
c--->
      uq = uql + uqu
      UZ=UZ+CUZ-APERC-uq
      IF (UZ .LT. 0.0) THEN
	 APERC=APERC+UZ
	 UZ=0.0
      ENDIF
      RETURN
      E N D

      subroutine lozon (klz,delt,aperc,lz,lq)
C---------------------------------------------------------------------
C
C     upper zone subroutine for the HBV model
C
C     Nils Roar Sælthun, NVE
C
C     Parameters:
C
C       KLZ     I       R       res. const                 [1/d]
C       DELT    I       R       time step, hours
C       APERC   I       R       actual percolation         [mm]
C       LZ      I       R       lower zone content         [mm]
C       LQ      I       R       outflow                    [mm]
C
C---------------------------------------------------------------------
      real klz, delt, aperc, lz, lq

C---------------------------------------------------------------------
C     local variables
C---------------------------------------------------------------------

c----------------------------------------------------------------------
c    from upper zone
c----------------------------------------------------------------------
      LZ=LZ+APERC

      IF (LZ .LT. 0.0) LZ=0.0

c----------------------------------------------------------------------
c    calculate discharge from lower zone, update zone contents
c----------------------------------------------------------------------
      LQ=KLZ*LZ*DELT/24.0
      LZ=LZ-LQ
      RETURN
      E N D

      subroutine laker 
     *   (qinn,hlake,qut,delt,fareal,sareal,k,delh,n,delf)

C---------------------------------------------------------------------
C
C     lake subroutine for the HBV model
C
C     Nils Roar Sælthun, NVE
C
C     Parameters:
C
C       qinn    I       R       inflow                     [mm]
C       hlake   I/O     R       lake level                 [m]
C       qut     O       R       outflow (from catchment)   [mm]
C       DELT    I       R       time step, hours
C       fareal  I       R       catchment area             [km2]
C       sareal  I       R       lake area                  [km2]
C       k       I       R       rating curve constant
C       delh    I       R       saddle point               [m]
C       n       I       R       rating curve exponent  
C       delf    I       R       part of catchment draining
C                               through lake               [1] 
C
C---------------------------------------------------------------------
      real qinn,hlake,qut,delt,fareal,sareal,k,delh,n,delf

C---------------------------------------------------------------------
C     local variables
C---------------------------------------------------------------------
      real h1, dqh, h, qt, h2

*-    ---------------------------------------------------------------*         
*     Lake routing                                                   *         
*-    ---------------------------------------------------------------*         
                                  
      H1     =  hlake                    

      IF (H1.LT.-DELH) H1 = -DELH                                              
      DQH = QINN*FAREAL*DELF/SAREAL/1000.

*-    ---------------------------------------------------------------*         
*     lake level                                                     *         
*-    ---------------------------------------------------------------*         
                                           
      H = H1+DQH/2.0-K*86.4*DELT/24.0*(H1+DELH)**N/(2.0*SAREAL*1000.)          
                                           
*-    ---------------------------------------------------------------*         
*     lake outflow in mm                                             *         
*-    ---------------------------------------------------------------*         
                                           
      IF (H.LT.-DELH) H=-DELH                                                  
      QT = K*86.4*DELT/24.0*(H+DELH)**N/(FAREAL*DELF)                          
      H2 = H1+DQH-QT*FAREAL*DELF/SAREAL/1000.                                  
      hlake = H2                                                                
      QUT = QINN*(1.0-DELF)+QT*DELF                                            
                                           
      R E T U R N                                                        
    
      E N D
      integer function ijanei(lu,text)
      integer lu,n
      character*(*) text
      if (text(1:1).eq.'$') then
	 write (*,'($,1x,a,a)') text(2:),' '
      else
         write (*,'(a)') text
      endif 
      call sp5(n)
      if (n.eq.1) then
	 ijanei =2
	 return
      else if (n.eq.2) then
	 ijanei = 1
	 return
      else
	 ijanei = 3
	 return
      endif

      end

      subroutine postxt(x,y,string)
      integer x,y
      character string*(*)
      character*10 rows,cols
      write (*,'(a)') "[H[J"
      write (cols,'(a,i2,a)') '(',x,'(/))'
      write (rows,'(a,i2,a)') '(',y,'X,a,$)'
c      write (*,cols) 
      write (*,rows) string
      call flush(6)
      return
      end

      subroutine blank5(lu)
      integer lu
      write (*,'(a)') "[H[J"
      return
      end

      subroutine header(prgver,prgdat)
      character prgver*4,prgdat*8
      write (*,'(2A)')
     *' --------------------------------------------------------------',
     *'--------------'
      write (*,'(2A)')
     *' |                                                             ',
     *'             |'
      WRITE (*,'(2A)')
     *' |                          RAINFALL/RUNOFF MODEL HBV          ',
     *'             |'
      WRITE (*,'(2A)')
     *' |               Originally developed at SMHI by Sten Bergstr”m',
     *'             |'
      WRITE (*,'(2A)')
     *' |             This version made at NVE/UiO by Nils Roar S‘lthu',
     *'n            |'
      WRITE (*,'(2A)')
     *' |                                                             ',
     *'             |'
      WRITE (*,'(5A)')
     *' |             ver ',prgver,'	                	       '
     *,prgdat,'            |'
      write (*,'(2A)')
     *' |                                                             ',
     *'             |'
      write (*,'(2A)')
     *' --------------------------------------------------------------',
     *'--------------'
      return
      end

      subroutine intin(lu,n,text,min,max,default,*)
      integer lu,n,min,max,default
      character*(*) text
      integer val(40)
      character instr*80
      integer num
      if (text(1:1).eq.'$') then
	 write (*,'($,1x,a,a)') text(2:),' '
      else
         write (*,'(a)') text
      endif 
      read (*,'(a)') instr
      call sp80ng(instr,val,num)
      if (num .eq. 0) then
	 n = default
	 return
      endif
      n = val(1)
      if (n.lt.min) return 1
      if (n.gt.max) return 1
      return
      end

      subroutine pcinit
      return
      end

      SUBROUTINE SP5(N)
***********************************************************************
*   Library:   SUBIB                                                  *
*                                                                     *
*   Description:                                                      *
*   This routine reads and interprete a character answer to a yes/no  *
*   question from the keyboard                                        *
*                                                                     *
*   This routine is called from several of the old programs based on  *
*   rolling menues and must be kept operational until the old user    *
*   interface has been replaced all through the system.               *
*                                                                     *
*   Fortran standard:                                                 *
*      Fortran 77 without extensions.                                 *
*                                                                     *
*   Subroutines being called:                                         *
*      None                                                           *
*---------------------------------------------------------------------*
*   Programmer: LAR/HD                                Date: -     1979*
*   Revised   : LAR/HD                                Date: 30/07-1993*
*   Modifications:                                                    *
*   Upgrading to Fortran 77 from Fortran 66. Recognition of answers in*
*   English as well as Norwegian                                      *
**********************************************************************

*---------------------------------------------------------------------*
*   Declarations                                                      *
*---------------------------------------------------------------------*

      IMPLICIT NONE

*---------------------------------------------------------------------*
*   The formal parameters                                             *
*---------------------------------------------------------------------*

      INTEGER      N           !  Code for the answer
			       !  = 0  blank (default in calling prog.)
			       !  = 1   nei/no
			       !  = 2  ja/yes
			       !  = 3  vet ikke/don't know
			       !  = 4  slutt/end/quit
			       !  = 5  tilbakehopp
			       !  = 6  rett/correct
			       !  = 7  bryt/break
			       !  = 8  hjelp/help

*---------------------------------------------------------------------*
*   Local variables                                                   *
*---------------------------------------------------------------------*

      INTEGER      I
      CHARACTER*10 SVAR

*---------------------------------------------------------------------*
*   The following character variables contains all permitted answers  *
*---------------------------------------------------------------------*
 
      CHARACTER*1 RESP(27)
      DATA RESP/' ','J','j','Y','y','N','n','V','v','D','d','S','s',
     +'E','e','Q','q','T','t','R','r','C','c','B','b','H','h'/    

*--------------------------------------------------------------------*
*   Initiate                                                         *
*--------------------------------------------------------------------*

      I=0
      N=0

*--------------------------------------------------------------------*
*   Read an answer from the keyboard                                 *
*--------------------------------------------------------------------*

    1 READ (*,2) SVAR
      IF(SVAR(1:1).EQ.RESP(1)) RETURN
    2 FORMAT(A10)

*--------------------------------------------------------------------*
*   Interprete the answer                                            *
*   The intepretation is based on the first character of the answer  *
*--------------------------------------------------------------------*

      IF(SVAR(1:1).EQ.RESP(2).OR.SVAR(1:1).EQ.RESP(3).
     +OR.SVAR(1:1).EQ.RESP(4).OR.SVAR(1:1).EQ.RESP(5))THEN
	 I=2
      ELSE IF(SVAR(1:1).EQ.RESP(8).OR.SVAR(1:1).EQ.RESP(9).
     +OR.SVAR(1:1).EQ.RESP(10).OR.SVAR(1:1).EQ.RESP(11))THEN
	 IF(I.EQ.3)THEN
	    N=N+1
	 ELSE
	    N=0
	 END IF
	 I=3
	 IF(N.GT.3)GO TO 4
	 PRINT *,'Hvis du ikke vet det, hvordan kan da jeg vite det?'
	 GO TO 1
      ELSE IF(SVAR(1:1).EQ.RESP(6).OR.SVAR(1:1).EQ.RESP(7))THEN
	 I=1
      ELSE IF(SVAR(1:1).EQ.RESP(12).OR.SVAR(1:1).EQ.RESP(13).
     +OR.SVAR(1:1).EQ.RESP(14).OR.SVAR(1:1).EQ.RESP(15).
     +OR.SVAR(1:1).EQ.RESP(16).OR.SVAR(1:1).EQ.RESP(17))THEN
	 I=4
      ELSE IF(SVAR(1:1).EQ.RESP(18).OR.SVAR(1:1).EQ.RESP(19))THEN
	 I=5
      ELSE IF(SVAR(1:1).EQ.RESP(20).OR.SVAR(1:1).EQ.RESP(21).
     +OR.SVAR(1:1).EQ.RESP(22).OR.SVAR(1:1).EQ.RESP(23))THEN
	 I=6
      ELSE IF(SVAR(1:1).EQ.RESP(24).OR.SVAR(1:1).EQ.RESP(25))THEN
	 I=7
	 STOP
      ELSE IF(SVAR(1:1).EQ.RESP(26).OR.SVAR(1:1).EQ.RESP(27))THEN
	 I=8
      ELSE

*---------------------------------------------------------------------*
*        Undefined answer - three attempts are allowed                *
*---------------------------------------------------------------------*
 
	 N=N+1
	 WRITE(*,6)SVAR
    6    FORMAT('Du har svart ',A10,'som jeg ikke forstar.', 
     +   'Svar pa nytt.')
	 CALL FORTS(0) 
	 IF(N.LT.3)GO TO 1
	 GO TO 4
      END IF
      GO TO 5
    4 PRINT *,'Siden du bare svarer tull, velger jeg a oppfatte svaret'
      PRINT *,'som nei.'        
      I=1
      CALL FORTS(0)
    5 N=I 
      RETURN
      END

********************************************************************************

       SUBROUTINE  Sp80Ng(String,Value,Num)

********************************************************************************
* Beskrivelse:
*    Delar upp en teckenstr{ng i integers som sinsemellan {r avskiljda med
*    icke numeriska tecken.
*    OBS! - ("minus")rett foran et tall tolkes som fortegn til tallet.
*-----------------------------------------------------------------------------
* Skrevet av:
*    19900110, Eva Johansson, SMHI, Kopierat fr}n Split80
* [ndringar:
*
*-----------------------------------------------------------------------------
* Inn-parametrer:
* Namn                  Typ             Beskrivning
*    String             80-tecken       textstr{ng
*
* Ut-parametrer:
* Namn                  Typ             Beskrivning
*    Value              integer array   Inneh}ller de integers som fanns
*    Num                integer         Antal integers som fanns i String
*                                       Num = -1 betyr at sist leste tall
*                                       best}r av mer enn 10 tegn som er max
*
*-------------------------------------------------------------------------------
* Lokalt deklarerte variabler:
* Namn                  Typ             Beskrivning
*    I,J                integers        Counters
*    Number             integer         Antal tecken i varje separat integer
*    Dummy,Dummy2       character       Hj{lp-parametrar vid convertering
*                                       fr}n character till integer.
*    IsNeg              boolean         Talar om om talet {r positivt eller ej
******************************************************************************
       IMPLICIT NONE

       INTEGER Number,Num,I,J
       INTEGER Value(40)

       CHARACTER*10 Dummy,Dummy2
       CHARACTER*80 String

       LOGICAL IsNeg

c      ** Nollst{llning av r{kne-parametrar **
       Number=0
       Num=0
       IsNeg = .false.

c      ** Loopa igenom tecken f|r tecken och se om det {r en integer **
       DO 100 I=1,80
c        ** Om en integer hittas sparas den , tills n{sta icke integer hittats *
	 IF ((ICHAR(String(I:I)) .ge. 48) .and.
     +       (ICHAR(String(I:I)) .le. 57)) THEN
	    Number=Number+1
	    IF (Number .eq. 11) THEN
	       NUM = -1
	       GOTO 110
	    ENDIF
	    Dummy(Number:Number)=String(I:I)
	 ELSE
c           ** Om Number {r st|rre {n 0 har en integer hittats **
	    IF (Number .gt. 0) THEN
		Num=Num+1
		Dummy2(1:10)='0000000000'
c               ** L{gg integern sist i str{ngen (h|gst 10 tecken) med 0:or f|re
		Do 50 J=1,Number
		   Dummy2(J+10-Number:J+10-Number)=Dummy(J:J)
 50             CONTINUE
		READ(Dummy2(1:10),'(I10)')Value(Num)
c               ** Om ett minus uppt{cktes direkt innan talet, {r detta negativt
		IF (IsNeg) THEN
		   Value(Num)=-Value(Num)
		ENDIF
c               ** Nollst{ll inf|r n{sta omg}ng **
		Number=0
	     ENDIF
c            ** Kolla om tecknet {r ett minustecken **
	     IF (ICHAR(String(I:I)) .eq. 45) THEN
		IsNeg=.true.
	     ELSE
		IsNeg=.false.
	     ENDIF
	  ENDIF
 100   CONTINUE
 110   Continue

       RETURN

       END

*************************************************************************

      subroutine forts(i)

*************************************************************************
*  Library:           subib                                             *
*                                                                       *
*  Description:       wait for CR                                       *
*                                                                       *
*  Fortran standard:  ANSI FORTRAN 77 (X3.9-1978)                       *
*        Extensions:  implicit none                                     *
*                     (other extensions must be mentioned)              *
*                                                                       *
*  Subroutines being called: none                                       *
*                                                                       *
*-----------------------------------------------------------------------*
*  Programmer: Svein Taksdal NVE, HD                  Date: 20.11.1991  *
*                                                                       *
*  Revised by:   --- " ---                            Date:             *
*  Changes:                                                             *

*-----------------------------------------------------------------------*
*  In-parameters:                                                       *
*  Name       Type     Description                                      *
*   i         INT      Indicates text or not on the screen              *
*                                 0 = no text                           *
*                                 1 = User is asked to press CR         *
*-----------------------------------------------------------------------*
*  Locally specified variables:                                         *
*  Name       Type     Description                                      *
*                                                                       *
*                                                                       *
*************************************************************************

      implicit none

*-----------------------------------------------------------------------*
*  Specifications:                                                      *
*-----------------------------------------------------------------------*

       integer         i

C      real
      character       DUM*1
C      logical

*-----------------------------------------------------------------------*
*  Initiations:                                                         *
*-----------------------------------------------------------------------*
      IF (I .EQ. 1) THEN
	 write(*,90)
 90      format($,' Trykk CR for † fortsette : ')
      ENDIF

      READ(*,100)DUM
 100  FORMAT(A1)

      RETURN
      END
      SUBROUTINE TOPO(IN,OUT,LU,INTER,STATUS)           
C... CALCULATION AND    PRINTING  OF ERROR FUNCTIONS IN 2 DIMENSIONS  
C        A :  VALUE OF PARAMETER A                                    
C       IA :  PARAMETER NO. IN COMMON-BLOCK                           
C       NA :  NUMBER OF DIFFERENT A-VALUES TO BE TESTED (MAX 11)      
C     AMIN :  MINIMUM VALUE OF PARAMETER A                            
C     AMAX :  MAXIMUM VALUE OF PARAMETER A                            
C       AI :  CALCULATION-INTERVAL                                    
C        B :  VALUE OF PARAMETER B                                    
C       IB :  PARAMETER NO. IN COMMON-BLOCK                           
C       NB :  NUMBER OF DIFFERENT B-VALUES TO BE TESTED (MAX 11)      
C     BMIN :  MINIMUM VALUE OF PARAMETER B                            
C     BMAX :  MAXIMUM VALUE OF PARAMETER B                            
C       BI :  CALCULATION-INTERVAL                                    
C        F :  VALUE OF ERROR FUNCTION RETURNED FROM MODEL             
C        E :  VALUES OF ERROR FUNCTIONS TO BE PRINTED                 
C      PAR :  COMMON-BLOCK FOR PARAMETER-VALUES
 
      INTEGER IN,OUT,LU,INTER,STATUS                      

c -------------------------------------------------------------------
c     common param - model parameters
c	 par(120)	parameter array
c	 reft		reference height for temperature (weighted mean
c			of stations)
c	 refp		reference height for precipitation (weighted mean
c			of stations)
c	 snofrac(9)	snow distribution fractions
c	 xnorm(9)	snow distribution

      real par,reft,refp,snofrac,xnorm
      COMMON /PARAM/ PAR(120),reft,refp,snofrac(9),xnorm(9)

c--------------------------------------------------------------------
c     Local variables
c--------------------------------------------------------------------
      INTEGER IA,NA,IB,NB,iai,ibi,i,j,k,m,idum,mf 
      REAL A,AMIN,AMAX,AI,B,BMIN,BMAX,BI,aa,bb,dum,skala                    
      CHARACTER chdum*1
      CHARACTER*4 FEILF(5)   
      real  E(11,11,5),EA(11),EB(11),F(5)                         
      integer INT(11)
c--------------------------------------------------------------------
c     Functions
c--------------------------------------------------------------------
      integer ijanei

      DATA FEILF/'RELA','TOTA','F2  ','R2  ','R2LG'/

 
 3100 continue                                                        
      GOTO (993,201,3100)
     -   IJANEI(OUT,'$Do you want to return to parameter list? ')        
 992  STATUS=2                                                        
      RETURN                                                          
 993  STATUS=3                                                        
      RETURN                                                          
  201 STATUS=0                                                        
  202 WRITE(OUT, 1001)                                                
1001  FORMAT ('$Topography, first parameter:')                         
      WRITE(OUT,1004)                                                 
1004  FORMAT ('$Enter parameter no, nos of steps, low and high value:')   
      READ(IN,*,END=204,ERR=202) IA,NA,AMIN,AMAX                      
c      if (indpar(ia).eq.0) then
c         write (*,'(A)') ' Parameter not implemented or not accessible'
c         goto 202
c      endif
      iai = ia
      GOTO 203                                                        
CV204 CLOSE(IN)                                                       
  204 CONTINUE
CV    OPEN(IN,FILE='INPUT')                                           
      GOTO 202                                                        
  203 WRITE(OUT, 1003)                                                
1003  FORMAT ('$Second parameter:')                                   
      WRITE(OUT, 1004)                                                
      READ(IN,*,END=203,ERR=203) IB,NB,BMIN,BMAX                      
c      if (indpar(ib).eq.0) then
c         write (*,'(A)') ' Parameter not implemented or not accessible'
c         goto 203
c      endif
      ibi = ib
      AA=PAR(IAI)                                                      
      BB=PAR(IBI)                                                      
      AI=AMAX-AMIN                                                    
      IF (NA.GT.1) AI=AI/(NA-1)                                       
      BI=BMAX-BMIN                                                    
      IF (NB.GT.1) BI=BI/(NB-1)                                       
      A=AMIN-AI                                                       
      B=BMIN-BI                                                       
      DO 10 K=1,NB                                                    
         B=B+BI                                                          
  10     EB(K)=B                                                         
      DO 30 I=1,NA                                                    
         A=A+AI                                                          
         B=BMIN-BI                                                       
         EA(I)=A                                                         
         DO 20 J=1,NB                                                    
            B=B+BI                                                          
            PAR(IAI)=A                                                       
            PAR(IBI)=B                                                       
            CALL INIT (0,IDUM,DUM)                                          
            CALL SIMUL(0,F,STATUS,1)
            IF(STATUS.NE.0) RETURN                                          
            DO 19 M=1,5                                                     
  19           E(I,J,M)=F(M)                                                   
  20     CONTINUE                                                        
  30  CONTINUE                                                        
C     SKRIVER UT FEILFUNKSJONSMATRISENE                               
      IF (INTER .EQ. 0) GOTO 60                                       
      DO 70 M=1,5                                                     
         MF=M-1                                                          
c         call blank5(0)
         WRITE(OUT, 3001)IA,IB,feilf(m)                                  
3001     FORMAT (/,' Error function topography for parameters #',I3,      
     1    ' and',I3,' -  error function ',A4,/,1X,79(1H*))              
         WRITE(OUT, 3002)IA,IB,(EB(K),K=1,NB)                            
3002     FORMAT (/,1X,I5,3H / ,I2,11F6.2)                                
         SKALA=1.0                                                       
         IF (M .EQ. 4 .or. M.eq.5) SKALA=100.0                           
         DO 71 I=1,NA                                                    
            DO 72 K=1,NB                                                    
72             INT(K)=IFIX(E(I,K,M)*SKALA+0.5)                                 
            WRITE(OUT, 3003)EA(I),(INT(K),K=1,NB)                           
3003        FORMAT (/,1X,F6.2,4X,11I6)                                      
71       CONTINUE   
         read (*,'(a)') chdum                                                                
70    CONTINUE                                                        
60    CONTINUE                                                        
      DO 50 M=1,5                                                     
         MF=M-1                                                          
         WRITE(LU,3001) IA,IB,feilf(m)                                   
         WRITE(LU, 3002)IB,IA,(EB(K),K=1,NB)                             
         DO 55 I=1,NA                                                    
         DO 52 K=1,NB                                                    
  52        INT(K)=IFIX(E(I,K,M)*SKALA+0.5)                                 
  55        WRITE(LU,3003) EA(I),(INT(J),J=1,NB)                            
  50  CONTINUE                                                        
80    CONTINUE                                                        
      PAR(IA)=AA                                                      
      PAR(IB)=BB                                                      
      RETURN                                                          
  40  STATUS=1                                                        
      RETURN                                                          
      E N D                                                           

      SUBROUTINE OPTIMA(IN,OUT,INTER,FEILFN,PAR,IDIM,STATUS,TEXT)       
C     ROSENBROCKS AUTOMATIC METHOD FOR FINDING THE LEAST VALUE OF AN    
C     ERROR FUNCTION AND ESTIMATING THE CORESPONDING PARAMETER VALUES.  
C     THREE AUXILIARY SUBROUTINES ARE CALLED FROM OPTIMA:               
C        AUG :  ADDS AN INCREMENT TO THE VARIABLES TO BE FITTED.        
C     SCRIBE :  TESTS IF THERE IS ANY PROGRESS IN THE FITTING.          
C       INIT :  GIVES INITIAL VALUES TO STORAGE PARAMETERS BEFORE EACH  
C               NEW CALL ON THE MODEL.                                  
C           N :  NUMBER OF PARAMETERS TO BE FITTED. MAXIMUM 10          
C           X :  PARAMETERS TO BE FITTED                                
C           A :  ORTHOGONALISATION MATRIX                               
C           Z :                                                         
C           F :  ERROR FUNCTION VALUE FROM LAST CALL ON MODEL           
C         F1 :  BEST VALUE OF F IN FITTING RUN                          
C         F2 :                                                          
C         AA :                                                          
C           B :                                                         
C        TOP :                                                          
C        BOT :                                                          
C           D :                                                         
C        ICS :  =0 IF FURTHER PROGRESS IN FITTING RUN, =2 OTHERWISE     
C        ISP :  =1 FOR THE FIRST FITTING RUN, =2 THEREAFTER             
C      FKEEP :  BEST VALUE OF F FROM LAST FITTING RUN                   
C                                                                       
      INTEGER IN,OUT,INTER,FEILFN,IDIM,STATUS
      REAL PAR(idim)
      CHARACTER TEXT*8

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c-----------------------------------------------------------------------
c    Local variables
c-----------------------------------------------------------------------
      INTEGER i,j,k,l,N,ics,isp,nfcc,idum
      CHARACTER OPTIID*8,RETUID*8
      CHARACTER*4 FEILF(5)                                              
      REAL F,fkeep,f1,f2,b,top,bot,d,dum,aa
      REAL A(10,10),Z(3,10),X(10),FAR(5)
      INTEGER IX(10),ixi(10)

c-----------------------------------------------------------------------
c     Functions
c-----------------------------------------------------------------------
      integer ijanei                                     
                                                                        
      DATA FEILF/'RELA','TOTA','F2  ','R2  ','ABSA'/                    
      DATA OPTIID/'OPTIMA  '/,RETUID/'RETUR   '/                        
C... LES OPTIMERINGSSPESIFIKASJONER OG GI STARTVERDIER TIL X(N).        
      STATUS=1                                                          
      FKEEP=10E10                                                       
                                            
 3100 continue                                                          
      GOTO (993,201,3100)                                               
     -   IJANEI(OUT,'$Do you want to return to parameter list? ')       
                                                                        
 992  STATUS=2                                                          
      RETURN                                                            
 993  STATUS=3                                                          
      RETURN                                                            
  201 STATUS=0                                                          
C      WRITE(OUT, 1001)                                                 
1001  FORMAT (/,'$Optimization: Enter nos of parameters: ')             
      CALL INTIN                                                        
     * (OUT,N,'$Optimization: Enter nos of parameters: ',1,5,1,*992)    
C      CALL INTIN(OUT,N,' ',1,5,1,*992)                                 
C      CALL ININT(IN,*992,*992,N)                                       
  202 WRITE(OUT,1002) N                                                 
 1002 FORMAT (/,' Enter ',I3,' parameters:')                            
      READ(IN,*,END=202,ERR=202)(IXi(I),I=1,N)                          
c      do 1003 i = 1,n                                                  
c      if (indpar(ix(i)).eq.0) then                                     
c         write (*,'(A)') ' Parameter not implemented or not accessible'
c         goto 202                                                      
c      endif                                                            
c      ixi(i) = indpar(ix(i))                                           
1005  CONTINUE                                                          
      STATUS=0                                                          
      DO 5 I=1,N                                                        
      J=IXI(I)                                                          
   5  X(I)=PAR(J)                                                       
      ICS=0                                                             
      ISP=1                                                             
c      call blank5(0)                                                       
      IF (INTER .GE. 1) WRITE(OUT, 2002)FEILF(feilfn+1),(IXi(K),K=1,N)  
2002  FORMAT (' Parameter optimization, Rosenbrock ',/,1X,              
     2     34(1H*),//,'   Error function ',A4,5X,'Parameters: ',//      
     3     9X,'Error  ',10I7)                                           
      IF (INTER .EQ. 0) WRITE(OUT,2001)(IXi(K),K=1,N)                   
2001  FORMAT(1H1///20X,'ROSENBROCKS AUTOMATIC METHOD FOR FINDING THE LEA
     CST VALUE OF AN'/20X,'ERROR FUNCTION AND ESTIMATING THE CORRESPONDI
     CNG PARAMETER VALUES'//5X,'ERROR',14X,'PARAMETER VALUES'/4X,'FUNCTI
     CON',9X,10(3X,I3,4X)//)                                            
      NFCC = 1                                                          
C     TEST IF ANY OF THE STARTING VALUE HAVE BEEN SET EQUAL TO ZERO     
      DO 10 I=1,N                                                       
         IF(X(I).EQ.0.) X(I)=0.0001                                     
         IF(ABS(X(I)).LT.0.0001) X(I)=SIGN(0.0001,X(I))                 
         IF(ABS(X(I)).LT.0.01) X(I)=SIGN(0.01,X(I))                     
 10   CONTINUE                                                          
      DO 20 I=1,N                                                       
         Z(1,I)=0.04                                                    
         Z(1,I)=0.1                                                     
         Z(2,I)=1.0                                                     
         Z(3,I)=X(I)                                                    
         DO 15 J=1,N                                                    
 15        A(J,I)=0.                                                    
 20      A(I,I)=1.                                                      
C     GET THE FIRST VALUE OF F TO PUT IN F1                             
      CALL INIT(N,IXi,X)                                                
      CALL SIMUL(0,FAR,STATUS,1)
      IF (STATUS .NE. 0) RETURN                                         
      F=FAR(FEILFN+1)                                                   
      F1=F                                                              
      WRITE(OUT, 2003) nfcc,F1,(X(I),I=1,N)                              
 30   DO 100 L=1,N                                                      
         AA=0.5*Z(1,L)                                                  
         B=0.                                                           
         CALL AUG(AA,N,X,Z,A,L)                                         
         CALL INIT(N,IXi,X)                                             
         CALL SIMUL(0,FAR,STATUS,1)                                       
         IF (STATUS .NE. 0) RETURN                                      
         F=FAR(FEILFN+1)                                                
         IF(F1.GT.F) GO TO 55                                           
         CALL AUG(-2*AA,N,X,Z,A,L)                                      
         F2=F                                                           
         CALL INIT(N,IXi,X)                                             
         CALL SIMUL (0,FAR,STATUS,1)                                      
         IF (STATUS .NE. 0) RETURN                                      
         F=FAR(FEILFN+1)                                                
         IF(F1.GT.F) GO TO 50                                           
         CALL AUG(AA,N,X,Z,A,L)                                         
C        SET UP FOR INTERPOLATION                                       
         TOP=F-F2                                                       
         BOT=2.*(F+F2-2.*F1)                                            
         GO TO 65                                                       
 50      AA=-AA                                                         
C        SUCCESS                                                        
 55      F2=F1                                                          
         F1=F                                                           
         B=B+AA                                                         
         AA=1.5*AA                                                      
         CALL AUG(AA,N,X,Z,A,L)                                         
         CALL INIT(N,IXi,X)                                             
         CALL SIMUL (0,FAR,STATUS,1)
         IF (STATUS .NE. 0) RETURN                                      
         F=FAR(FEILFN+1)                                                
         IF(F1.GT.F) GO TO 55                                           
C        FAILURE PROCEDURE STARTS HERE                                  
         CALL AUG(-AA,N,X,Z,A,L)                                        
         AA=AA/1.5                                                      
         Z(1,L)=AA                                                      
         BOT=3.*F2-5.*F1+2.*F                                           
         TOP=2.25*F2-1.25*F1-F                                          
 65      IF(BOT.GT.1.E-10) GO TO 70                                     
         GO TO 85                                                       
 70      D=AA*TOP/BOT                                                   
         CALL AUG(D,N,X,Z,A,L)                                          
         CALL INIT(N,IXi,X)                                             
         CALL SIMUL (0,FAR,STATUS,1)
         IF (STATUS .NE. 0) RETURN                                      
         F=FAR(FEILFN+1)                                                
         IF(F.GT.F1) GO TO 80                                           
C        ACCEPT THE PREDICTED POINT                                     
         Z(1,L)=AA+D                                                    
         F1=F                                                           
         B=B+D                                                          
         GO TO 90                                                       
 80      CALL AUG(-D,N,X,Z,A,L)                                         
 85      IF(B.NE.0.) GO TO 90                                           
C        THIS IS NECESSERY TO PRESERVE THE N INDEPENDENT DIRECTIONS     
         B=AA/10.                                                       
 90      DO 95 J=1,N                                                    
 95         A(J,L)=B*A(J,L)                                            
100   CONTINUE                                                          
C     TESTS IF THERE IS ANY PROGRESS IN THE FITTING                     
                                                                        
      WRITE(OUT, 2003)nfcc,F1,(X(I),I=1,N)                              
2003  FORMAT (i3,1x,F10.1,2X,10F7.2)                                        
      IF(ISP.EQ.1) GO TO 104                                            
      IF (FKEEP-F1 .LE. 0.00001*F1) ICS=2                               
 104  FKEEP=F1                                                          
      ISP=2                                                             
      IF(ICS.EQ.2) RETURN                                               
      IF (INTER .EQ. 0. .or. nfcc/5*5 .ne. nfcc ) GOTO 2010             
 3400 continue                                                          
      GOTO (2010,2012,3400)                                             
     -   IJANEI(OUT,'$Continue iteration?')                             
                                                                        
 2012 RETURN                                                            
2010  CONTINUE                                                          
      CALL INIT (0,IDUM,DUM)                                            
C     THE ORTHOGONALISATION LOOP STARTS HERE                            
      DO 110 J=2,N                                                      
         L=N-J+1                                                        
         DO 110 I=1,N                                                   
110   A(I,L)=A(I,L)+A(I,L+1)                                            
      DO 150 J=1,N                                                      
         IF(J.EQ.1) GO TO 125                                           
         I=J-1                                                          
         DO 120 L=1,I                                                   
            AA=0.                                                       
            DO 115 K=1,N                                                
115            AA=A(K,L)*A(K,J)+AA                                      
            DO 120 K=1,N                                                
120         A(K,J)=A(K,J)-AA*A(K,L)                                     
125      AA=0.                                                          
         DO 130 K=1,N                                                   
130         AA=A(K,J)**2+AA                                             
C        SCALE VECTORS OF THE DIRECTIONS                                
         AA=1./SQRT(AA)                                                 
         DO 150 K=1,N                                                   
150      A(K,J)=AA*A(K,J)                                               
C     END OF ORTHOGONALISATION LOOP                                     
      NFCC = NFCC+1                                                     
      GOTO 30                                                           
      E N D                                                             
                                                                        
      SUBROUTINE AUG(H,N,X,Z,A,L)                                       
C     ADDS AN INCREMENT TO THE VARIABLES TO BE FITTED                   
      real X(10),A(10,10),Z(3,10)
      real h
      integer i,l,n
                                  
      DO 10 I=1,N                                                       
      Z(2,I)=Z(2,I)+A(I,L)*H                                            
  10  X(I)=Z(2,I)*Z(3,I)                                                
      RETURN                                                            
      E N D                                                             
                                                                        
      SUBROUTINE BOTM(LU,IX,X,E,IDIM,N,EF,ESCALE,FUNC,JPRINT,MAXIT,     
     *        NPROB,IFEIL)                                              
C                                                                       
C     ******************************************************************
C     PURPOSE                                                           
C        TO MINIMIZE THE RESIDUALS SUM OF SQUARES FUNCTION              
C                                                                       
C     SOURCE                                                            
C          /NR                                                          
C                                                                       
C     ******************************************************************
      INTEGER LU,IDIM,N,JPRINT,MAXIT,NPROB,IFEIL
      INTEGER IX(idim)
      REAL X(idim),E(idim),W(460)
      REAL EF,ESCALE

      EXTERNAL FUNC

c-----------------------------------------------------------------------
c     Local variables
c---------------------------------------------------------------------- 
      integer i,iprint,icon,k,j,jj,jjj,nfcc,ind,inn,iterc,isgrad,
     -        itone,ixp,idirn,iline,is,jil
      real ddmag,scer,f,fkeep,fp,sum,dmax,dacc,dmag,ddmax,dl,d,fprev,
     -        fa,da,dd,fb,db,fc,dc,fhold,a,b,di,fi,aaa
c-----------------------------------------------------------------------
c     Functions
c---------------------------------------------------------------------- 
      real func

      IPRINT=JPRINT                                                     
      NPROB=0                                                           
      IF(IPRINT.NE.2)WRITE(LU,1) ' BOTM',N,ESCALE,JPRINT,MAXIT,NPROB    
    1 FORMAT(A,I5,F10.5,3I5)                                            
       IF(IPRINT.NE.2)WRITE(LU,561)(E(I),I=1,N)                         
      IF(N-20)120,120,122                                               
  122 WRITE (LU,121)N                                                   
  121 FORMAT(1H0,35HBOTM ERROR: THE NUMBER OF VARIABLES,I3,37H EXCEEDS T
     1HE CAPACITY OF THE ROUTINE   )                                    
      STOP                                                              
  120 ICON=1                                                            
      IF(MAXIT.EQ.0)MAXIT=100                                           
      IPRINT=JPRINT                                                     
      IF(IPRINT.EQ.1)IPRINT=2                                           
      DDMAG=0.1*ESCALE                                                  
      SCER=0.05/ESCALE                                                  
      JJ=N*(N+1)                                                        
      JJJ=JJ+N                                                          
      K=N+1                                                             
CV    NFCC=IND=INN=1                                                    
      NFCC=1                                                            
      IND=1                                                             
      INN=1                                                             
      DO 4 I=1,N                                                        
      W(I)=ESCALE                                                       
      DO 4 J=1,N                                                        
      W(K)=0.                                                           
      IF(I-J)4,3,4                                                      
    3 W(K)=ABS(E(I))                                                    
    4 K=K+1                                                             
      ITERC=1                                                           
      ISGRAD=2                                                          
      NPROB=NPROB+1                                                     
      F=FUNC(X,IX,N,IFEIL,JPRINT)                                       
      IF (NPROB.EQ.1) WRITE(LU,52) NFCC,F,(X(I),I=1,N)                  
      IF(IPRINT.NE.2)WRITE(LU,1)NPROB                                   
      FKEEP=2.*ABS(F)                                                   
      IF(IPRINT.NE.2)WRITE(LU,561)F,FKEEP,DDMAG,SCER
      call flush                    
  561 FORMAT(1X,10E12.5)                                                
    5 ITONE=1                                                           
      FP=F                                                              
      SUM=0.                                                            
      IXP=JJ                                                            
      DO 6 I=1,N                                                        
      IXP=IXP+1                                                         
    6 W(IXP)=X(I)                                                       
      IDIRN=N+1                                                         
      ILINE=1                                                           
    7 DMAX=W(ILINE)                                                     
      DACC=DMAX*SCER                                                    
      DMAG=AMIN1(DDMAG,0.1*DMAX)                                        
      DMAG=AMAX1(DMAG,20.*DACC)                                         
      DDMAX=10.*DMAG                                                    
      GO TO(70,70,71)ITONE                                              
   70 DL=0.                                                             
      D=DMAG                                                            
      FPREV=F                                                           
      IS=5                                                              
      FA=FPREV                                                          
      DA=DL                                                             
    8 DD=D-DL                                                           
      DL=D                                                              
      IF(IPRINT.NE.2)WRITE(LU,561)DL,D,DA,FA,FPREV                      
   58 K=IDIRN                                                           
      DO 9 I=1,N                                                        
      X(I)=X(I)+DD*W(K)                                                 
       IF(IPRINT.NE.2)WRITE(LU,561)X(I),DD,W(K)                         
    9 K=K+1                                                             
      NPROB=NPROB+1                                                     
      F=FUNC(X,IX,N,IFEIL,JPRINT)                                       
      NFCC=NFCC+1                                                       
      IF(IPRINT.NE.2)WRITE(LU,561)D,DMAX,F,FA,FKEEP                     
      IF(IPRINT.NE.2)WRITE(LU,561)(X(I),I=1,N)                          
      GO TO(10,11,12,13,14,96)IS                                        
   14 IF(F-FA)15,16,24                                                  
   16 IF(ABS(D)-DMAX)17,17,18                                           
   17 D=D+D                                                             
      GO TO 8                                                           
   18 WRITE(LU,19)                                                      
   19 FORMAT(82H0BOTM WARNING: MAXIMUM PERMITTED CHANGE IN THE VARIABLES
     1 DOES NOT ALTER FUNCTION    /)                                    
      GO TO 20                                                          
   15 FB=F                                                              
      DB=D                                                              
      GO TO 21                                                          
   24 FB=FA                                                             
      DB=DA                                                             
      FA=F                                                              
      DA=D                                                              
   21 GO TO(83,23)ISGRAD                                                
   23 D=DB+DB-DA                                                        
      IS=1                                                              
      GO TO 8                                                           
   83 D=0.5*(DA+DB-(FA-FB)/(DA-DB))                                     
      IS=4                                                              
      IF((DA-D)*(D-DB))25,8,8                                           
   25 IS=1                                                              
      IF(ABS(D-DB)-DDMAX)8,8,26                                         
   26 D=DB+SIGN(DDMAX,DB-DA)                                            
      IS=1                                                              
      DDMAX=DDMAX+DDMAX                                                 
      DDMAG=DDMAG+DDMAG                                                 
      IF(DDMAX-DMAX)8,8,27                                              
   27 DDMAX=DMAX                                                        
      GO TO 8                                                           
   13 IF(F-FA)28,23,23                                                  
   28 FC=FB                                                             
      DC=DB                                                             
   29 FB=F                                                              
      DB=D                                                              
      GO TO 30                                                          
   12 IF(F-FB)28,28,31                                                  
   31 FA=F                                                              
      DA=D                                                              
      GO TO 30                                                          
   11 IF(F-FB)32,10,10                                                  
   32 FA=FB                                                             
      DA=DB                                                             
      GO TO 29                                                          
   71 DL=1                                                              
      DDMAX=5                                                           
      FA=FP                                                             
      DA=-1.                                                            
      FB=FHOLD                                                          
      DB=0.                                                             
      D=1.                                                              
   10 FC=F                                                              
      DC=D                                                              
   30 A=(DB-DC)*(FA-FC)                                                 
      B=(DC-DA)*(FB-FC)                                                 
      IF((A+B)*(DA-DC))33,33,34                                         
   33 FA=FB                                                             
      DA=DB                                                             
      FB=FC                                                             
      DB=DC                                                             
      GO TO 26                                                          
   34 D=0.5*(A*(DB+DC)+B*(DA+DC))/(A+B)                                 
      DI=DB                                                             
      FI=FB                                                             
      IF(FB-FC)44,44,43                                                 
   43 DI=DC                                                             
      FI=FC                                                             
   44 GO TO(86,86,85)ITONE                                              
   85 ITONE=2                                                           
      GO TO 45                                                          
   86 IF(ABS(D-DI)-DACC)41,41,93                                        
   93 IF(ABS(D-DI)-0.03*ABS(D))41,41,45                                 
   45 IF((DA-DC)*(DC-D))47,46,46                                        
   46 FA=FB                                                             
      DA=DB                                                             
      FB=FC                                                             
      DB=DC                                                             
      GO TO 25                                                          
   47 IS=2                                                              
      IF((DB-D)*(D-DC))48,8,8                                           
   48 IS=3                                                              
      GO TO 8                                                           
   41 F=FI                                                              
      D=DI-DL                                                           
      DD=SQRT((DC-DB)*(DC-DA)*(DA-DB)/(A+B))                            
      DO 49 I=1,N                                                       
      X(I)=X(I)+D*W(IDIRN)                                              
      W(IDIRN)=DD*W(IDIRN)                                              
   49 IDIRN=IDIRN+1                                                     
      W(ILINE)=W(ILINE)/DD                                              
      ILINE=ILINE+1                                                     
      IF(IPRINT-1)51,50,51                                              
   50 WRITE(LU,52)NFCC,F,(X(I),I=1,N)                                   
   52 FORMAT(I4,2X,F7.2,10F7.3)                                         
      GO TO(51,53)IPRINT                                                
   51 GO TO(55,38)ITONE                                                 
   55 IF(FPREV-F-SUM)94,95,95                                           
   95 SUM=FPREV-F                                                       
      JIL=ILINE                                                         
   94 IF(IDIRN-JJ)7,7,84                                                
   84 GO TO(92,72)IND                                                   
   92 FHOLD=F                                                           
      IS=6                                                              
      IXP=JJ                                                            
      DO 59 I=1,N                                                       
      IXP=IXP+1                                                         
   59 W(IXP)=X(I)-W(IXP)                                                
      DD=1.                                                             
      GO TO 58                                                          
   96 GO TO (112,87)IND                                                 
  112 IF(FP-F)37,37,91                                                  
   91 D=2.*(FP+F-2.*FHOLD)/(FP-F)**2                                    
      IF(D*(FP-FHOLD-SUM)**2-SUM)87,37,37                               
   87 J=JIL*N+1                                                         
      IF(J-JJ)60,60,61                                                  
   60 DO 62 I=J,JJ                                                      
      K=I-N                                                             
   62 W(K)=W(I)                                                         
      DO 97 I=JIL,N                                                     
   97 W(I-1)=W(I)                                                       
   61 IDIRN=IDIRN-N                                                     
      ITONE=3                                                           
      K=IDIRN                                                           
      IXP=JJ                                                            
      AAA=0.                                                            
      DO 67 I=1,N                                                       
      IXP=IXP+1                                                         
      W(K)=W(IXP)                                                       
      IF(AAA-ABS(W(K)/E(I)))66,67,67                                    
   66 AAA=ABS(W(K)/E(I))                                                
   67 K=K+1                                                             
      DDMAG=1.                                                          
      W(N)=ESCALE/AAA                                                   
      ILINE=N                                                           
      GO TO 7                                                           
   37 IXP=JJ                                                            
      AAA=0.                                                            
      F=FHOLD                                                           
      DO 99 I=1,N                                                       
      IXP=IXP+1                                                         
      X(I)=X(I)-W(IXP)                                                  
      IF(AAA*ABS(E(I))-ABS(W(IXP)))98,99,99                             
   98 AAA=ABS(W(IXP)/E(I))                                              
   99 CONTINUE                                                          
      GO TO 72                                                          
   38 AAA=AAA*(1.+DI)                                                   
      GO TO(72,106)IND                                                  
   72 IF(IPRINT-2)53,50,50                                              
   53 GO TO(109,88)IND                                                  
  109 IF(AAA-0.1)89,89,76                                               
   89 GO TO(20,116)ICON                                                 
  116 IND=2                                                             
      GO TO(100,101)INN                                                 
  100 INN=2                                                             
      K=JJJ                                                             
      DO 102 I=1,N                                                      
      K=K+1                                                             
      W(K)=X(I)                                                         
  102 X(I)=X(I)+10.*E(I)                                                
      FKEEP=F                                                           
      NPROB=NPROB+1                                                     
      F=FUNC(X,IX,N,IFEIL,JPRINT)                                       
      NFCC=NFCC+1                                                       
      DDMAG=0.                                                          
      GO TO 108                                                         
   76 IF(F-FP)35,78,78                                                  
   78  WRITE(LU,80)                                                     
   80 FORMAT(68H0BOTH MESSAGE: ACCURACY LIMITED BY ROUNDING ERRORS IN TH
     1E FUNCTION   /)                                                   
      GO TO 20                                                          
   88 IND=1                                                             
   35 DDMAG=0.4*SQRT(FP-F)                                              
      ISGRAD=1                                                          
  108 ITERC=ITERC+1                                                     
      IF(ITERC-MAXIT)5,5,81                                             
   81 WRITE(LU,82)MAXIT                                                 
   82 FORMAT(52H0BOTH WARNING: SPECIFIED ACCURACY NOT ATTAINED AFTER ,  
     1I4,11H ITERATIONS  /)                                             
      IF(F-FKEEP)20,20,110                                              
  110 F=FKEEP                                                           
      DO 111 I=1,N                                                      
      JJJ=JJJ+1                                                         
  111 X(I)=W(JJJ)                                                       
      GO TO 20                                                          
  101 JIL=1                                                             
      FP=FKEEP                                                          
      IF(F-FKEEP)105,78,104                                             
  104 JIL=2                                                             
      FP=F                                                              
      F=FKEEP                                                           
  105 IXP=JJ                                                            
      DO 113 I=1,N                                                      
      IXP=IXP+1                                                         
      K=IXP+N                                                           
      GO TO(114,115)JIL                                                 
  114 W(IXP)=W(K)                                                       
      GO TO 113                                                         
  115 W(IXP)=X(I)                                                       
      X(I)=W(K)                                                         
  113 CONTINUE                                                          
      JIL=2                                                             
      GO TO 92                                                          
  106 IF(AAA-0.1)20,20,107                                              
   20 EF=F                                                              
      RETURN                                                            
  107 INN=1                                                             
      GO TO 35                                                          
      E N D                                                             
                                                                        
      SUBROUTINE OPTIMB(IN,OUT,INTER,FEILFN,PAR,IDIM,STATUS,TEXT)       
C                                                                       
C     OPTIMALISERER PARAMETRE VED HJELP AV BOTM                         
C     

      INTEGER IN,OUT,INTER,FEILFN,IDIM,STATUS
      REAL PAR(idim)
      CHARACTER TEXT*8

c -------------------------------------------------------------------
c     common char - names
c	 feltn	       catchment name
c	 filn	       file name

      character feltn*12, filn*30

      COMMON/CHAR/FELTN,filn

c-----------------------------------------------------------------------
c    Local variables
c-----------------------------------------------------------------------
      INTEGER i,j,k,N,nprob
      CHARACTER OPTIID*8,RETUID*8
      CHARACTER*4 FEILF(5)
      real X(10),E(10),f1
      integer ix(10),ixi(10)   

c-----------------------------------------------------------------------
c     Functions
c-----------------------------------------------------------------------
      integer ijanei                                     
                                                                  
      EXTERNAL FEIL                                                                                    
      DATA FEILF/'RELA','TOTA','F2  ','R2  ','ABSA'/                    
      DATA OPTIID/'OPTIMA  '/,RETUID/'RETUR   '/                        
C... LES OPTIMERINGSSPESIFIKASJONER OG GI STARTVERDIER TIL X(N).        
      STATUS=1                                                          
                                            
 3100 continue                                                          
      GOTO (993,201,3100)                                               
     -   IJANEI(OUT,'$Do you want to return to parameter list? ')       
                                                                        
 992  STATUS=2                                                          
      RETURN                                                            
 993  STATUS=3                                                          
      RETURN                                                            
  201 STATUS=0                                                          
c      WRITE(OUT, 1001)                                                 
1001  FORMAT (/,'$Optimization: Enter nos of parameters: ')             
      CALL INTIN                                                        
     * (OUT,N,'$Optimization: Enter nos of parameters: ',1,5,1,*992)    
C      CALL ININT(IN,*992,*992,N)                                       
  202 WRITE(OUT,1002) N                                                 
 1002 FORMAT (/,' Enter ',I3,' parameters:')                            
      READ(IN,*,END=202,ERR=202)(IXi(I),I=1,N)                          
c      do 1003 i = 1,n                                                  
c      if (indpar(ix(i)).eq.0) then                                     
c         write (*,'(A)') ' Parameter not implemented or not accessible'
c         goto 202                                                      
c      endif                                                            
c      ixi(i) = indpar(ix(i))                                           

1005  CONTINUE                                                          
      STATUS=0                                                          
      DO 5 I=1,N                                                        
      J=IXi(I)                                                          
   5  X(I)=PAR(J)                                                       
c      call blank5(0)                                                       
      IF (INTER .GE. 1) WRITE(OUT, 2002)FEILF(feilfn+1),(IXI(K),K=1,N)  
2002  FORMAT (' Parameter optimization, Bottom     ',/,1X,              
     2     30(1H*),//,' Error function ',A4,5X,'Parameters: ',//        
     3     6X,'Error',10I7)
      call flush                                             
 2000 DO 2001 J=1,N                                                     
      E(J)=0.01                                                         
 2001 CONTINUE                                                          
      CALL BOTM(OUT,IXi,X,E,10,N,F1,1000.,FEIL,2,3,NPROB,FEILFN)        
      IF (INTER .EQ. 0) GOTO 2010                                       
 3400 continue                                                          
      GOTO (2000,2012,3400)                                             
     -   IJANEI(OUT,'$Continue iteration?')                             
                                                                        
 2012 RETURN                                                            
2010  CONTINUE                                                          
      RETURN                                                            
      E N D                                                             
                                                                        
      FUNCTION FEIL(X,IX,N,FEILF,IDUM)                                  
      INTEGER FEILF, STATUS,N,IDUM                                             
      REAL X(10),FAR(5)
      INTEGER IX(10) 
      real feil
                                    
      CALL INIT(N,IX,X)                                                 
      CALL SIMUL (0,FAR,STATUS,1)                                         
      FEIL=FAR(FEILF+1)                                                 
      RETURN                                                            
      END
