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
  PRINT "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�"
  PRINT "� Lectura / Escritura de la MBR y el BOOT �"
  PRINT "쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�"
  PRINT "� (c)Febrero 1992  Gabi Mart� i Fuentes.  �"
  PRINT "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�"
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
  PRINT "  旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커 "
  PRINT "  �  FORMATO:     MBRBOOT [opcion]                               � "
  PRINT "  쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑 "
  PRINT "  �  OPCIONES:                                                   � "
  PRINT "  �               /L   Lee la MBR y el BOOT guardando la         � "
  PRINT "  �                    informaci줻 en los ficheros con extensi줻 � "
  PRINT "  �                    .MBR y .BOT respectivamente.              � "
  PRINT "  �                                                              � "
  PRINT "  �               /E   Escribe la informaci줻 de los ficheros    � "
  PRINT "  �                    con extension .MBR y .BOT en el disco.    � "
  PRINT "  냐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캐 "
  PRINT "  �  Atenci줻!!!  El nombre del fichero 굎 siempre MBRBOOT.ext   � "
  PRINT "  읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸 "
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

