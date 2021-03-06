' *************************************************
' ** READSEC.BAS ** LECTOR DEL SECTOR 0 DEL DISC **
' *************************************************
' *** (C)1991 GABI MARTI I FUENTES (GAMMA SOFT) ***
' *************************************************
'
' ABANS DE COMPILAR S'HA DE CARREGAR EL Quick Basic
' AMB ELS SEG�ENTS PARAMETRES:  QB /L QB.QLB
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
PRINT "                                                                            "
PRINT "  READSEC v1.0 �     (c)1991  MICRO. Inform�tica Calella S.L. & Gabi Marti  "
PRINT " �������������������������������������������������������������������������� "
COLOR 7, 0
PRINT
IF UCASE$(COMMAND$) = "" THEN GOTO MERR
IF UCASE$(COMMAND$) = "?" THEN GOTO INFO

unitat% = VAL(COMMAND$)

INREG.AX = &H201             ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
INREG.DX = unitat%           ' CAP�AL 0, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES

PRINT " Leyendo sector 1 de Unidad "; HEX$(unitat%); " ..."
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)

PRINT " Guardando sector 1 en fichero CHECKVIR.TPA ..."
OPEN "CHECKVIR.TPA" FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR I% = 1 TO 512
   LSET BYTE2$ = MKI$(SEC%(I%))
   PUT #1, I%
NEXT I%
CLOSE #1
END

MERR:
   PRINT CHR$(7)
   PRINT " Esta utilidad debe ser usada conjuntamente con Checkvir.EXE. "
   PRINT
   END

INFO:
   PRINT CHR$(7)
   PRINT " FORMATO: "
   PRINT "                           ������������������������������������Ŀ"
   PRINT "    READSEC n� unidad      �  Esta utilidad lee el sector 1 del �"
   PRINT "            ���������      �  disco duro, que �s el que corres- �"
   PRINT "                �          �  ponde a la tabla de particiones y �"
   PRINT " ��������������������Ŀ    �  guarda la informaci�n en el fi-   �"
   PRINT " �Debe estar expresado�    �  chero 'CHECKVIR.TPA'.             �"
   PRINT " �en Hexadecimal y con�    �                                    �"
   PRINT " �el siguiente formato�    �  Esta imagen de la tabla de par-   �"
   PRINT " �                    �    �  ticiones se puede restaurar de    �"
   PRINT " �      &Hnumero      �    �  nuevo en caso de infecci�n por    �"
   PRINT " ��������������������ĳ    �  virus, con la utilidad            �"
   PRINT " � ejemplo para la    �    �  'WRITESEC.EXE'.                   �"
   PRINT " � unidad C (HD):     �    �                                    �"
   PRINT " �                    �    �  --------------------------------  �"
   PRINT " �       &H80         �    �                                    �"
   PRINT " ����������������������    ��������������������������������������"
   PRINT
   END




















