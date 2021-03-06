' **************************************************
' ** WRITESEC.BAS ** LECTOR DEL SECTOR 0 DEL DISC **
' **************************************************
' *** (C)1991 GABI MARTI I FUENTES (GAMMA SOFT)  ***
' **************************************************
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
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

CLS
COLOR 0, 7
PRINT "                                                                             "
PRINT "  WRITESEC v1.0 �     (c)1991  MICRO. Inform�tica Calella S.L. & Gabi Marti  "
PRINT " ��������������������������������������������������������������������������� "
COLOR 7, 0
PRINT
IF UCASE$(COMMAND$) = "" THEN GOTO MERR
IF UCASE$(COMMAND$) = "?" THEN GOTO INFO

unitat% = VAL(COMMAND$)

INREG.AX = &H301             ' FUNCIO 3  - ESCRIURE 1 SECTOR
INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
INREG.DX = unitat%           ' CAP�AL 0, UNITAT 80 (C:)
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
   PRINT "                           ������������������������������������Ŀ"
   PRINT "   WRITESEC n� unidad      �  Esta utilidad lee la imagen del   �"
   PRINT "            ���������      �  sector 1 del disco duro que est�  �"
   PRINT "                �          �  en el fichero 'CHECKVIR.TPA' y la �"
   PRINT " ��������������������Ŀ    �  reescribe en dicho sector del     �"
   PRINT " �Debe estar expresado�    �  disco para eliminar modificaciones�"
   PRINT " �en Hexadecimal y con�    �  hechas por algun virus.           �"
   PRINT " �el siguiente formato�    �                                    �"
   PRINT " �                    �    �  Previamente se debe haber guarda- �"
   PRINT " �      &Hnumero      �    �  do la informaci�n de dicho sector �"
   PRINT " ��������������������ĳ    �  con el fichero READSEC.EXE, y     �"
   PRINT " � ejemplo para la    �    �  estando la Tabla de Parcticiones  �"
   PRINT " � unidad C (HD):     �    �  en buen estado.                   �"
   PRINT " �                    �    �  --------------------------------  �"
   PRINT " �       &H80         �    �                                    �"
   PRINT " ����������������������    ��������������������������������������"
   PRINT
   END




















