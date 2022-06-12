' *********************************************************
' ** READ64.BAS ** LECTOR DEL SECTOR 1 PISTA 64 DEL DISC **
' *********************************************************
' ***** (C)1991,92 GABI MARTI I FUENTES (GAMMA SOFT) ******
' *********************************************************
'
' ABANS DE COMPILAR S'HA DE CARREGAR EL Quick Basic
' AMB ELS SEGÅENTS PARAMETRES:  QB /L QB.QLB
'

TYPE REGTYPEX
     AX    AS INTEGER
     BX    AS INTEGER
     CX    AS INTEGER
     DX    AS INTEGER
     BP    AS INTEGER
     SI    AS INTEGER
     DI    AS INTEGER
     FLAGS AS INTEGER
     DS    AS INTEGER
     ES    AS INTEGER
END TYPE
'
DECLARE SUB INTERRUPTX (intnum AS INTEGER, INREG AS REGTYPEX, OUTREG AS REGTYPEX)
'
DIM INREG AS REGTYPEX      ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX     ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)         ' SECTOR

CLS
COLOR 0, 7
PRINT "                                        "
PRINT "  READ64 v1.0 ‚     (c)1992 Gabi Marti  "
PRINT " ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ "
COLOR 7, 0
PRINT

unitat% = 0

INREG.AX = &H201             ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H4001            ' CILINDRE 64 (40h), SECTOR 1
INREG.DX = unitat%           ' UNITAT, CAPÄAL 0
ZINREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES

PRINT " Llegint Pista 64 Sector 1 de Unitat "; HEX$(unitat%); " ..."
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)

PRINT " Guardant Sector al fitxer SECTOR.DAT ..."
OPEN "SECTOR.DAT" FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR I% = 1 TO 512
   LSET BYTE2$ = MKI$(SEC%(I%))
   PUT #1, I%
NEXT I%
CLOSE #1
END




















