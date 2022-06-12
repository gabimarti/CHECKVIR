DECLARE SUB AYUDA ()
DECLARE SUB WRITESEC (UNIDAD%, CARA%)
DECLARE SUB READSEC (UNIDAD%, CARA%)
'
' ***************************************
' ***          MBRBOOT.BAS            ***
' *** (C)1992 GABRIEL MARTI I FUENTES ***
' ***************************************
'
' Antes de compilar se debe cargar el Quick Basic
' de la siguiente manera:   QB /L
' para que carge la libreria QB.QLB
'
'$INCLUDE: 'QB.BI'
'$DYNAMIC

  UNIDAD% = &H80          ' Unidad por defecto C:, si se desea que
			  ' funcione para la unidad D: debe cambiarse
			  ' el &H80 por &H81.

			  ' VER PROCEDIMIENTOS READSEC / WRITESEC

  MBR% = 0                ' Cuando deseamos leer/escribir la MBR, el
			  ' valor del byte alto de DX debe ser 0.

  BOOT% = 1               ' Cuando deseamos leer/escribir el BOOT, el
			  ' valor del byte alto de DX debe ser 256.
			  ' El 1 es el factor multiplicador.

INICIO:
  CLS
  PRINT "зддддддддддддддддддддддддддддддддддддддддд©"
  PRINT "Ё Lectura / Escritura de la MBR y el BOOT Ё"
  PRINT "цддддддддддддддддддддддддддддддддддддддддд╢"
  PRINT "Ё (c)Febrero 1992  Gabi Mart║ i Fuentes.  Ё"
  PRINT "юддддддддддддддддддддддддддддддддддддддддды"
  PRINT
  IF INSTR(UCASE$(COMMAND$), "/L") > 0 THEN
     CALL READSEC(UNIDAD%, MBR%)
     CALL READSEC(UNIDAD%, BOOT%)
     END
  END IF
  IF INSTR(UCASE$(COMMAND$), "/E") > 0 THEN
     CALL WRITESEC(UNIDAD%, MBR%)
     CALL WRITESEC(UNIDAD%, BOOT%)
     END
  END IF
  CALL AYUDA
END

REM $STATIC
SUB AYUDA
  PRINT "  здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд© "
  PRINT "  Ё  FORMATO:     MBRBOOT [opcion]                               Ё "
  PRINT "  цдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢ "
  PRINT "  Ё  OPCIONES:                                                   Ё "
  PRINT "  Ё               /L   Lee la MBR y el BOOT guardando la         Ё "
  PRINT "  Ё                    informaci╒n en los ficheros con extensi╒n Ё "
  PRINT "  Ё                    .MBR y .BOT respectivamente.              Ё "
  PRINT "  Ё                                                              Ё "
  PRINT "  Ё               /E   Escribe la informaci╒n de los ficheros    Ё "
  PRINT "  Ё                    con extension .MBR y .BOT en el disco.    Ё "
  PRINT "  ЁддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддЁ "
  PRINT "  Ё  Atenci╒n!!!  El nombre del fichero ┌s siempre MBRBOOT.ext   Ё "
  PRINT "  юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды "
END SUB

SUB READSEC (UNIDAD%, CARA%)
  DIM INREG AS RegTypeX             ' REGISTROS DE ENTRADA
  DIM OUTREG AS RegTypeX            ' REGISTROS DE SALIDA
  DIM SEC%(1 TO 512)                ' SECTOR

  INREG.ax = &H201                  ' FUNCION 2  - LECTURA DE 1 SECTOR
  INREG.cx = &H1                    ' CILINDRO 0, SECTOR 1
  INREG.dx = 256 * CARA% + UNIDAD%  ' CARA%, UNIDAD%
  INREG.es = VARSEG(SEC%(1))        ' SEGMENTO DE DATOS
  INREG.bx = VARPTR(SEC%(1))        ' OFFSET DE DATOS

  IF CARA% = 0 THEN
     L$ = "(MBR)": FICH$ = "MBRBOOT.MBR"
  ELSE
     L$ = "(BOOT)": FICH$ = "MBRBOOT.BOT"
  END IF

  PRINT " Leyendo "; L$; " de Unidad "; HEX$(UNIDAD%); " ...";
  CALL INTERRUPTX(&H13, INREG, OUTREG)
 
  PRINT " Guardando "; L$; " en fichero "; FICH$; " ..."; CHR$(7);
  OPEN FICH$ FOR RANDOM AS #1 LEN = 2
  FIELD #1, 2 AS BYTE2$
  FOR I% = 1 TO 512
     LSET BYTE2$ = MKI$(SEC%(I%))
     PUT #1, I%
  NEXT I%
  CLOSE #1

END SUB

SUB WRITESEC (UNIDAD%, CARA%)
  DIM INREG AS RegTypeX               ' REGISTROS DE ENTRADA
  DIM OUTREG AS RegTypeX              ' REGISTROS DE SALIDA
  DIM SEC%(1 TO 512)                  ' SECTOR
 
  INREG.ax = &H301                    ' FUNCION 3  - ESCRITURA DE 1 SECTOR
  INREG.cx = &H1                      ' CILINDRO 0, SECTOR 1
  INREG.dx = 256 * CARA% + UNIDAD%    ' CARA%, UNIDAD%
  INREG.es = VARSEG(SEC%(1))          ' SEGMENTO DE DATOS
  INREG.bx = VARPTR(SEC%(1))          ' OFFSET DE DATOS
 
  IF CARA% = 0 THEN
     L$ = "(MBR)": FICH$ = "MBRBOOT.MBR"
  ELSE
     L$ = "(BOOT)": FICH$ = "MBRBOOT.BOT"
  END IF
 
  PRINT " Leyendo "; L$; " del fichero "; FICH$; " ...";
  OPEN FICH$ FOR RANDOM AS #1 LEN = 2
  FIELD #1, 2 AS BYTE2$
  IF LOF(1) = 0 THEN
     PRINT " Error: Fichero "; FICH$; " inexistente."
     CLOSE #1
     KILL FICH$
     EXIT SUB
  END IF
  FOR I% = 1 TO 512
     GET #1, I%
     SEC%(I%) = CVI(BYTE2$)
  NEXT I%
  CLOSE #1
 
  PRINT " Escribiendo "; L$; " en Unidad "; HEX$(UNIDAD%); " ..."; CHR$(7);
  CALL INTERRUPTX(&H13, INREG, OUTREG)

END SUB

