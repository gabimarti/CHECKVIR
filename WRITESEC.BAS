' **************************************************
' ** WRITESEC.BAS ** LECTOR DEL SECTOR 0 DEL DISC **
' **************************************************
' *** (C)1991 GABI MARTI I FUENTES (GAMMA SOFT)  ***
' **************************************************
'
' ABANS DE COMPILAR S'HA DE CARREGAR EL Quick Basic
' AMB ELS SEGENTS PARAMETRES:  QB /L QB.QLB
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
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

CLS
COLOR 0, 7
PRINT "                                                                             "
PRINT "  WRITESEC v1.0 ç     (c)1991  MICRO. Inform…tica Calella S.L. & Gabi Marti  "
PRINT " ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ "
COLOR 7, 0
PRINT
IF UCASE$(COMMAND$) = "" THEN GOTO MERR
IF UCASE$(COMMAND$) = "?" THEN GOTO INFO

unitat% = VAL(COMMAND$)

INREG.AX = &H301             ' FUNCIO 3  - ESCRIURE 1 SECTOR
INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
INREG.DX = unitat%           ' CAP€AL 0, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES

PRINT " Leyendo sector 1 del fichero CHECKVIR.TPA ..."
PUN% = 1
OPEN "CHECKVIR.TPA" FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR I% = 1 TO 512
   GET #1, I%
   SEC%(I%) = CVI(BYTE2$)
NEXT I%
CLOSE #1

PRINT " Escribiendo sector 1 de Unidad "; HEX$(unitat%); " ..."
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)

END

MERR:
   PRINT CHR$(7)
   PRINT " Esta utilidad debe ser usada conjuntamente con Checkvir.EXE. "
   PRINT
   END

INFO:
   PRINT CHR$(7)
   PRINT " FORMATO: "
   PRINT "                           ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
   PRINT "   WRITESEC n§ unidad      ³  Esta utilidad lee la imagen del   ³"
   PRINT "            ÄÄÄÄÂÄÄÄÄ      ³  sector 1 del disco duro que est…  ³"
   PRINT "                ³          ³  en el fichero 'CHECKVIR.TPA' y la ³"
   PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄ¿    ³  reescribe en dicho sector del     ³"
   PRINT " ³Debe estar expresado³    ³  disco para eliminar modificaciones³"
   PRINT " ³en Hexadecimal y con³    ³  hechas por algun virus.           ³"
   PRINT " ³el siguiente formato³    ³                                    ³"
   PRINT " ³                    ³    ³  Previamente se debe haber guarda- ³"
   PRINT " ³      &Hnumero      ³    ³  do la informaci¢n de dicho sector ³"
   PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³    ³  con el fichero READSEC.EXE, y     ³"
   PRINT " ³ ejemplo para la    ³    ³  estando la Tabla de Parcticiones  ³"
   PRINT " ³ unidad C (HD):     ³    ³  en buen estado.                   ³"
   PRINT " ³                    ³    ³  --------------------------------  ³"
   PRINT " ³       &H80         ³    ³                                    ³"
   PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
   PRINT
   END




















