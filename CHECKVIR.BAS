
   ' ===============================================================
   '  CHECKVIR -- ver 6.00 (c) 1991-1997 Gabi Marti i Fuentes
   ' ===============================================================
   '
   ' Programa que compara el espai de memoria cada vegada
   ' que arrenca per evitar infeccions de virus.
   '
   ' HISTORIC DE VERSIONS
   '
   ' A partir de la versi¢ 2.00 compara tamb‚ la taula de Interrupci¢ns
   '
   ' A partir de la versi¢ 3.00 compara la taula de partici¢ns del HD
   ' o Master Boot Record (MBR), i reescriu la informaci¢ en cas de
   ' modificaci¢ d'aquesta.
   '
   ' A partir de la versi¢ 4.00 compara el BOOT, i reescriu la informaci¢
   ' en cas de modificaci¢ d'aquest.
   '
   ' A partir de la versi¢ 4.10 s'afegeixen els comanaments de linia i
   ' ajuda, ( opci¢ /? ¢ /H ).
   '
   ' A partir de la versi¢ 4.11 no restaura la taula de particions ni el
   ' sector d'arranc si no es possa el parametre /R. Quan restaura la MBR
   ' i el BOOT, guarda els anteriors en els fitxers CHECKANT.TPA i
   ' CHECKANT.BOT.
   '
   ' A la versi¢ 4.12 es corregeix el error de que no restaurava possant
   ' el par…metre /R.
   '
   ' A la versi¢ 4.13 s'inclou la opci¢ /I que genera un informe de totes
   ' les comprovacions sobre el fitxer CHECKVIR.NFO
   '
   ' A la versi¢ 4.14 es millora la presentaci¢ de la ajuda.
   '
   ' A la versi¢ 4.15 es cambia la presentaci¢ de la pantalla, i s'afegeix
   ' la comprovaci¢ de la capacitat de memoria extendida.
   '
   ' La versi¢ 4.16 es la mateixa que la 4.15. No te cap diferencia en
   ' el codi. Simplement s'han caviat algunes descripcions del codi
   ' font per m‚s claredat.
   '
   ' La versi¢ 5.00 es una revisi¢ i adaptaci¢ de la 4.16 feta el
   ' 29/7/96 per instalarla a MD Soluciones de Etiquetado, que
   ' es la nova feina d'en Gabi desde el 15/7/96.
   '
   ' 23/Jun/97 - Versi¢ 6.00a
   ' Es canvien alguns textos de l'ajut. S'actualitza l'adre‡a Internet.
   ' Quan no hi ha cap error al sistema al cap de 10 segons el programa
   ' surt autom…ticament sense esperar que l'usuari pulsi una tecla.
   ' S'afegeixen algunes definicions pels colors i es canvien alguns colors.
   '
   ' ATENCIO!! necessita trobar el COMMAND.COM al mateix directori; en cas
   ' contrari treur… un missatge indicant que no l'ha trobat.
   '
   ' 25/Jun/97 - Versi¢ 6.00b
   ' Utilitzo variable COMSPEC per buscar el COMMAND.COM, sin¢ el busca
   ' directament a l'arrel de la unitat C:, aix¡ CHECKVIR pot estar a un
   ' altre directori, i l'interpret de comandes pot ser un altre diferent
   ' del COMMAND.COM (per exemple NDOS.COM).
   ' Canvio alguns colors i textos.
   '
   '
   '
   ' ABANS DE COMPILAR S'HA DE CARREGAR EL Quick Basic
   ' AMB ELS SEGENTS PARAMETRES:  QB /L QB.QLB
   '
   '$DYNAMIC
   '

'
' Estructura de registres del processador
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
DECLARE FUNCTION EXTENDIDA! ()
DECLARE SUB SALVASEC (UNITAT%, CARA%)
DECLARE SUB AYUDA ()
DECLARE FUNCTION PAUSA$ (TIC)
DECLARE SUB LINEDISPLAY (TEXTE$, LIN%, COL%, NUM%)
DECLARE SUB WRITESEC (UNITAT%, CARA%)
DECLARE FUNCTION RESTAURA! (UNITAT%, CARA%)
DECLARE FUNCTION COMPBOOT! (UNITAT%)
DECLARE FUNCTION COMPTPA (UNITAT%)
DECLARE SUB READSEC (UNITAT%, CARA%)
DECLARE SUB INTERRUPTX (intnum AS INTEGER, INREG AS REGTYPEX, OUTREG AS REGTYPEX)
DECLARE SUB PANTALLA (MEN$)
DECLARE SUB ColorNormal ()
DECLARE SUB ColorSistema ()
DECLARE SUB ColorInvers ()
DECLARE SUB ColorBrillant ()
DECLARE SUB ColorApagat ()
DECLARE SUB ColorTitol ()
DECLARE FUNCTION TamanyCOMMAND! ()

'
' VARIABLES GLOBALS
'
   UNITAT% = &H80    ' Unitat de disc a comprovar
   LPRO = 69800      ' LONGITUD EN BYTES DEL PROGRAMA CHECKVIR.EXE
   '
   ' DEFINICIO VARIABLES CAP€ALERA
   '
   VER$ = "6.00b "
   PRO$ = "Gabi Marti.    (gabi@ati.es)    "
   AUT$ = "Gabriel Mart¡ i Fuentes         "
   '
   ' AUTO-PROTECCIO CONTRA MODIFICACIONS DE LITERALS
   '
   IF LCASE$(LEFT$(AUT$, 1)) <> "g" THEN GOTO ERRR
   IF MID$(AUT$, 6, 1) <> "e" THEN GOTO ERRR
   IF MID$(AUT$, 14, 1) <> " " THEN GOTO ERRR
   '
   IF LEFT$(PRO$, 1) <> "G" OR MID$(PRO$, 10, 1) <> "i" THEN GOTO ERRR
   IF MID$(PRO$, 19, 1) <> "b" OR MID$(PRO$, 21, 1) <> "@" THEN GOTO ERRR
   IF RIGHT$(PRO$, 6) <> "s)    " OR MID$(PRO$, 17, 1) <> "g" THEN GOTO ERRR
   '
   ' COMPROVACIO DE SI S'HA ACTIVAT LA OPCIO D'AJUDA
   '
   IF INSTR(COMMAND$, "/?") > 0 OR INSTR(UCASE$(COMMAND$), "/H") > 0 THEN
      CALL AYUDA
      END
   END IF
   '
   ' COMPROVACIO DE SI S'HA ACTIVAT LA OPCIO DE GENERAR INFORME
   '
   IF INSTR(UCASE$(COMMAND$), "/I") > 0 THEN
      INFO% = 1
   ELSE
      INFO% = 0
   END IF
   '
   ' COMPROVACIO DE SI S'HA ACTIVAT LA OPCIO DE RESTAURAR MBR I BOOT
   '
   IF INSTR(UCASE$(COMMAND$), "/R") > 0 THEN
      REST% = 1
   ELSE
      REST% = 0
   END IF
   '
   ' COMPROVACIO DE SI S'HA ACTIVAT LA OPCIO DE VISUALITZAR NOM D'AUTOR
   '
   IF INSTR(UCASE$(COMMAND$), "/A") > 0 THEN
      CALL PANTALLA(PRO$)
   ELSE
      CALL PANTALLA(AUT$)
   END IF
   '
   ' MIRO SI S'HA ENTRAT EL PARAMETRE /D unitat PER SELùLECCIONAR UNA
   ' UNITAT DIFERENTE DE LA &H80 (C:)
   '
   P% = INSTR(UCASE$(COMMAND$), "/D")
   IF P% > 0 THEN
      UNITAT% = VAL(MID$(COMMAND$, P% + 2))
   END IF

   LIMEN% = 7    ' Linia de missatges
   '
   ' COS PRINCIPAL
   '
   IF INFO% = 1 THEN
      OPEN "CHECKVIR.NFO" FOR OUTPUT AS #5
      PRINT #5, "  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   "
      PRINT #5, "  ³ ® CHECKVIR ¯  ver. " + VER$ + "  -  I N F O R M E  - ³Þ  "
      PRINT #5, "  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÞ  "
      PRINT #5, "   ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß  "
      PRINT #5, " "
      PRINT #5, "  COMPROBACION            V. ESPERADO   V. DETECTADO  DIAGNOSTICO           "
      PRINT #5, " ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
         NFO$ = " \                     \ #,###,###,### #,###,###,### \                     \"
         NFH$ = " \                     \  \          \  \          \ \                     \"
   END IF
   '
   GOSUB FITXER.COMPROBACIO
   '
   DEF SEG = 0
   MKB = PEEK(&H413) + 256 * PEEK(&H414)   ' MEMORIA BASE
   MKS = EXTENDIDA                         ' MEMORIA EXTENDIDA
   CR1 = FRE(0)                            ' CRC1
   CR2 = FRE(-1)                           ' CRC2
   '
   ' Numero de errors trobats
   '
   VIRUS% = 0
   '
   ' Tamany COMMAND.COM
   '
   LCO = TamanyCOMMAND
   IF LCO = 0 THEN
      LOCATE LIMEN%, 56: PRINT "COMMAND.COM NO EXISTE"
      LIMEN% = LIMEN% + 1
   END IF
   '
   ' Tamany CHECKVIR.EXE
   '
   OPEN "CHECKVIR.EXE" FOR INPUT AS #1
    LCH = LOF(1)
   CLOSE #1
   '
   ' comprovaci¢ de MEMORIA BASE
   '
   IF MEMKB = MKB THEN
      OK1$ = " CORRECTO"
   ELSE
      OK1$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "CAPACIDAD MEMORIA RAM"
      LIMEN% = LIMEN% + 1
      LOCATE LIMEN%, 56: PRINT "HA SIDO MODIFICADA. "
      LIMEN% = LIMEN% + 1
      VIRUS% = 1
      SOUND 4000, .1: SOUND 1660, .1: SOUND 500, .15
   END IF
   IF INFO% = 1 THEN PRINT #5, USING NFO$; "MEMORIA RAM BASE"; MEMKB; MKB; OK1$
   '
   ' comprovaci¢ MEMORIA EXTENDIDA
   '
   IF MEM1M = MKS THEN
      OK4$ = " CORRECTO"
   ELSE
      OK4$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "LA MEMORIA EXTENDIDA"
      LIMEN% = LIMEN% + 1
      LOCATE LIMEN%, 56: PRINT "HA SIDO MODIFICADA. "
      LIMEN% = LIMEN% + 1
      VIRUS% = 1
      SOUND 4000, .1: SOUND 1660, .1: SOUND 500, .15
   END IF
   IF INFO% = 1 THEN PRINT #5, USING NFO$; "MEMORIA EXTENDIDA"; MEM1M; MKS; OK4$
   '
   ' comprovaci¢ CRC-1
   '
   BADCRC% = 0
   IF CRC1 = CR1 THEN
      OK2$ = " CORRECTO"
   ELSE
      OK2$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "MEMORIA ALTERADA."
      LIMEN% = LIMEN% + 1
      VIRUS% = VIRUS% + 1
      BADCRC% = 1
   END IF
   IF INFO% = 1 THEN PRINT #5, USING NFO$; "CRC #1"; CRC1; CR1; OK2$
   '
   ' comprovaci¢ CRC-2
   '
   IF CRC2 = CR2 THEN
      OK3$ = " CORRECTO "
   ELSE
      OK3$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "MEMORIA ALTERADA."
      LIMEN% = LIMEN% + 1
      VIRUS% = VIRUS% + 1
      BADCRC% = 1
   END IF
   IF INFO% = 1 THEN PRINT #5, USING NFO$; "CRC #2"; CRC2; CR2; OK3$
   '
   ' comprovaci¢ COMMAND.COM
   '
   IF LCOM = LCO THEN
      OK5$ = " CORRECTO "
   ELSE
      OK5$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "COMMAND.COM INFECTADO."
      LIMEN% = LIMEN% + 1
      VIRUS% = VIRUS% + 1
   END IF
   IF INFO% = 1 THEN PRINT #5, USING NFO$; "LONGITUD COMMAND.COM"; LCOM; LCO; OK5$
   IF INFO% = 1 AND LCO = 0 THEN PRINT #5, USING NFH$; " ÀÄÄÄÄÄÄ>"; ""; ""; "COMMAND.COM NO ENCONTRADO"
   '
   ' comprovaci¢ CHECKVIR
   '
   IF LPRO = LCH THEN
      OK6$ = " CORRECTO "
   ELSE
      OK6$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "CHECKVIR.EXE INFECTADO."
      LIMEN% = LIMEN% + 1
      VIRUS% = VIRUS% + 1
   END IF
   IF INFO% = 1 THEN PRINT #5, USING NFO$; "LONGITUD CHECKVIR.EXE"; LPRO; LCH; OK6$
   '
   ' Resultat comprovaci¢ns MEMORIA, COMMAND.COM, CHECKVIR
   '
   LOCATE 7, 33: PRINT USING "###,###"; MKB; : PRINT " Kb"; : LOCATE 7, 45: PRINT OK1$
   LOCATE 8, 33: PRINT USING "###,###"; EXTENDIDA; : PRINT " Kb ": LOCATE 8, 45: PRINT OK4$
   LOCATE 9, 33: PRINT USING "###,###"; CR2 + CR1;
   LOCATE 9, 45
   IF BADCRC% = 0 THEN
      PRINT " CORRECTO "
   ELSE
      PRINT "INCORRECTO"
   END IF
   LOCATE 10, 33: PRINT USING "###,###"; LCO; : LOCATE 10, 45: PRINT OK5$
   LOCATE 11, 33: PRINT USING "###,###"; LCH; : LOCATE 11, 45: PRINT OK6$
   '
   ' comprovaci¢ dels VECTORS DE INTERRUPCIO
   '
   GOSUB COMPROVA.INT
   '
   '
   '
   CORRE1% = 0
   CORRE2% = 0
   CORRE1% = COMPTPA(UNITAT%)
   CORRE2% = COMPBOOT(UNITAT%)
   COLOR 1, 7
   LOCATE 24, 1: PRINT SPACE$(80);
   '
   ' Missatge final
   '
   IF VIRUS% > 0 THEN
      M$ = "*** Posible infecci¢n v¡rica. ***          "
      M$ = M$ + "Para m s informaci¢n compruebe el archivo CHECKVIR.NFO."
      M$ = M$ + "    PULSE [RETURN] PARA TERMINAR ...     "
      CALL LINEDISPLAY(M$, 24, 1, 80)
   ELSE
      LOCATE 24, 5: PRINT " Sistema correcto.   Pulse una tecla para salir ... ";
      K$ = PAUSA$(10)
   END IF
   ColorSistema
   CLS
   END
ERRR:
   CLS
   PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
   PRINT "³  Fichero corrompido !!!                       ³"
   PRINT "³          Posible copia ilegal o pirateada...  ³"
   PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
   PRINT CHR$(7); CHR$(7); CHR$(7); CHR$(7);
   END
'
' comprova si existeixen els fitxers de control
'
FITXER.COMPROBACIO:
'
   OPEN "CHECKVIR.BOT" FOR RANDOM AS #1
   IF LOF(1) = 0 THEN
      CLOSE #1
      CALL READSEC(UNITAT%, 1)
   END IF
   CLOSE #1
   OPEN "CHECKVIR.TPA" FOR RANDOM AS #1
   IF LOF(1) = 0 THEN
      CLOSE #1
      CALL READSEC(UNITAT%, 0)
   END IF
   CLOSE #1
   OPEN "CHECKVIR.CRC" FOR RANDOM AS #1
   IF LOF(1) = 0 THEN GOSUB CREA.CRC
   CLOSE #1
   OPEN "CHECKVIR.CRC" FOR INPUT AS #1
    INPUT #1, MEMKB   ' MEMORIA BASE
    INPUT #1, MEM1M   ' MEMORIA EXTENDIDAD
    INPUT #1, CRC1
    INPUT #1, CRC2
    INPUT #1, LCOM    ' LONGITUD COMMAND.COM
   CLOSE #1
   OPEN "CHECKVIR.INT" FOR RANDOM AS #1
   IF LOF(1) = 0 THEN GOSUB CREA.INT
   CLOSE #1
RETURN
'
' comprova els VECTORS DE INTERRUPCIO
'
COMPROVA.INT:
   RESTORE
   ColorInvers
   LOCATE 24, 1: PRINT SPACE$(80);
   ColorNormal
   CLOSE #1
   IF INFO% = 1 THEN
      PRINT #5, " "
      PRINT #5, USING NFH$; "VECTORES INTERRUPCION"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"
   END IF
   OPEN "CHECKVIR.INT" FOR INPUT AS #1
   LIN% = 15
   FOR i% = &H0 TO &H77
       IF LIN% > 19 THEN
          FOR X% = 15 TO 19
              LOCATE X%, 3: PRINT SPACE$(28)
              LOCATE X%, 33: PRINT SPACE$(8)
              LOCATE X%, 45: PRINT SPACE$(10)
          NEXT X%
          LIN% = 15
       END IF
       INPUT #1, FC1%
       INPUT #1, FC2%
       INPUT #1, FC3%
       INPUT #1, FC4%
       DEF SEG = 0
       VL1% = PEEK(i%)
       VL2% = PEEK(i% + 1)
       VL3% = PEEK(i% + 2)
       VL4% = PEEK(i% + 3)
       READ IN$
       LOCATE LIN%, 3: PRINT HEX$(i%); " "; IN$
       LOCATE LIN%, 33: PRINT HEX$(FC1%); HEX$(FC2%); HEX$(FC3%); HEX$(FC4%)
       IF VL1% <> FC1% OR VL2% <> FC2% OR VL3% <> FC3% OR VL4% <> FC4% THEN
           OKI$ = "INCORRECTO"
           SOUND 3600, .1: SOUND 660, .1
           VIRUS% = VIRUS% + 1
           IF LIMEN% < 20 THEN
             LOCATE LIMEN%, 56: PRINT HEX$(i%); " "; IN$;
             LIMEN% = LIMEN% + 1
           END IF
       ELSE
           OKI$ = " CORRECTO "
       END IF
       LOCATE LIN%, 45: PRINT OKI$;
       IF INFO% = 1 THEN PRINT #5, USING NFH$; "  " + IN$; HEX$(FC1%) + HEX$(FC2%) + HEX$(FC3%) + HEX$(FC4%); HEX$(VL1%) + HEX$(VL2%) + HEX$(VL3%) + HEX$(VL4%); OKI$
       LIN% = LIN% + 1
   NEXT i%
   CLOSE #1
   IF INFO% = 1 THEN PRINT #5, USING NFH$; "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"
RETURN
'
' crea el fitxer de control dels VECTORS DE INTERRUPCIO
'
CREA.INT:
   CLOSE #1
   ColorInvers
   LOCATE 24, 1: PRINT SPACE$(80);
   LOCATE 24, 1: PRINT " Creaci¢n fichero vectores interrupci¢n ..."; CHR$(7);
   ColorNormal
   OPEN "CHECKVIR.INT" FOR OUTPUT AS #1
   FOR i% = &H0 TO &H77
       ColorInvers
       LOCATE 24, 65: PRINT HEX$(i%);
       ColorNormal
       DEF SEG = 0
       VL1% = PEEK(i%)
       VL2% = PEEK(i% + 1)
       VL3% = PEEK(i% + 2)
       VL4% = PEEK(i% + 3)
       PRINT #1, VL1%
       PRINT #1, VL2%
       PRINT #1, VL3%
       PRINT #1, VL4%
   NEXT i%
   CLOSE #1
RETURN
'
' crea el fitxer de CRC
'
CREA.CRC:
   CLOSE #1
   ColorInvers
   LOCATE 24, 1: PRINT SPACE$(80);
   LOCATE 24, 1: PRINT " Creaci¢n del fichero de comprobaci¢n ..."; CHR$(7);
   ColorNormal
   DEF SEG = 0
   MEMKB = PEEK(&H413) + 256 * PEEK(&H414)
   MEM1M = EXTENDIDA
   CRC1 = FRE(0)
   CRC2 = FRE(-1)

   LCOM = TamanyCOMMAND
   IF LCOM = 0 THEN
      LOCATE LIMEN%, 56: PRINT "COMMAND.COM NO EXISTE"
      LIMEN% = LIMEN% + 1
   END IF

   OPEN "CHECKVIR.CRC" FOR OUTPUT AS #1
    PRINT #1, MEMKB
    PRINT #1, MEM1M
    PRINT #1, CRC1
    PRINT #1, CRC2
    PRINT #1, LCOM
   CLOSE #1
RETURN
'
' Descripcions de les adreces dels VECTORS DE INTERRUPCIO
'
DATA DIVISION POR CERO, PASO A PASO, NMI, RUPTURA, DESBORDAMIENTO, TECLA PrtSc
DATA RESERVADA, RESERVADA, IRQ0, IRQ1, IRQ2, IRQ3, IRQ4, IRQ5, IRQ6, IRQ7
DATA VIDEO BIOS, EQUIPO BIOS, TAMA¥O BIOS, DISCO BIOS, COMUNICACIONES BIOS
DATA E/S BIOS, TECLADO BIOS, IMPRESORA BIOS, BASIC, LOAD BIOS
DATA HORA BIOS, BREAK BIOS, TEMPORIZADOR BIOS, PARAMETROS BIOS, FLOPPY BIOS
DATA CARACTERES BIOS, FIN PROCESO DOS, FUNCIONES DOS, ADR. FIN DOS
DATA CTRL-C DOS, ERROR DOS, LECTURA DISCO DOS, ESCRITURA DISCO DOS
DATA RESIDENTE DOS, ESPERA DOS, RESERVADA DOS, RED DOS
DATA RESERVADA DOS, RESERVADA DOS, RESERVADA DOS, RESERVADA DOS
DATA MULTIPLEX DOS
DATA RESERVADA DOS, RESERVADA DOS, RESERVADA DOS, RESERVADA DOS
DATA RESERVADA DOS, RESERVADA DOS, RESERVADA DOS, RESERVADA DOS
DATA RESERVADA DOS, RESERVADA DOS, RESERVADA DOS, RESERVADA DOS
DATA RESERVADA DOS, RESERVADA DOS, RESERVADA DOS, RESERVADA DOS
DATA FLOPPY BIOS, HARD DISK BIOS, EGA BIOS
DATA TABLA EGA BIOS, CARACTERES BIOS, N/D, PARM. HD BIOS
DATA N/D, N/D, N/D, ALARMA BIOS
DATA N/D, N/D, N/D, N/D, N/D
DATA N/D, N/D, N/D, N/D, N/D
DATA N/D, N/D, N/D, N/D, N/D
DATA ADAPTADOR BLOQUES, PROGRAMA BLOQUES
DATA N/D, N/D, N/D, N/D
DATA USUARIO, USUARIO, USUARIO, USUARIO, USUARIO, USUARIO, USUARIO
DATA DRIVER LIM EMS
DATA N/D, N/D, N/D, N/D, N/D, N/D, N/D, N/D
DATA IRQ8, IRQ9 A IRQ2, IRQ10, IRQ11, IRQ12
DATA IRQ13, IRQ14, RESERVADO IRQ15

REM $STATIC
'
' AJUDA DE CHECKVIR
'
SUB AYUDA
    SHARED VER$
    K$ = ""
    PAG = 1
    DO WHILE K$ <> CHR$(27)
       ' PgDn
       IF K$ = CHR$(0) + CHR$(&H51) THEN PAG = PAG + 1
       ' PgUp
       IF K$ = CHR$(0) + CHR$(&H49) THEN PAG = PAG - 1
       ' CONTROL OVERFLOW
       IF PAG < 1 THEN PAG = 1
       IF PAG > 4 THEN PAG = 4
       COLOR 15, 3
       ON PAG GOSUB PANTALLA1, PANTALLA2, PANTALLA3, PANTALLA4
       COLOR 9, 7
       LOCATE 24, 1
       PRINT "     [Esc] - Salir    [PgDn] - P gina Siguiente    [PgUp] - P gina Anterior     ";
       ColorNormal
       K$ = ""
       DO WHILE K$ = ""
          K$ = INKEY$
       LOOP
    LOOP
    CLS
    EXIT SUB
PANTALLA1:
    CLS
    PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
    PRINT " ³  CHECKVIR  ³ *************** A Y U D A **************          P gina 1/4  ³ "
    PRINT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ "
    PRINT " ³  (c)1991-97    Gabriel Marti i Fuentes         ( Internet : gabi@ati.es )  ³ "
    PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³ Checkvir es una utilidad NO RESIDENTE que avisa de la intrusi¢n de virus o ³ "
    PRINT " ³ programas extra¤os en el sistema, basandose en la comprobaci¢n de unos     ³ "
    PRINT " ³ ficheros patr¢n que se crean la primera vez que se ejecuta Checkvir.       ³ "
    PRINT " ³ Por lo tanto es imprescindible que el sistema est‚ 'limpio' la primera vez ³ "
    PRINT " ³ que se ejecute Checkvir, sin¢ no servir  de nada su instalaci¢n.           ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³ Para que su funcionamiento sea correcto debe instalarse en el AUTOEXEC.BAT ³ "
    PRINT " ³ siendo ademas recomendable que sea la primera instrucci¢n que se ejecute,  ³ "
    PRINT " ³ as¡ si se modifican los diferentes programas que se cargan en el AUTOEXEC, ³ "
    PRINT " ³ no afectar  a la detecci¢n de Checkvir, al cargar o descargar programas    ³ "
    PRINT " ³ residentes, que pueden dar falsos errores.                                 ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³ No debe de ejecutarse desde la linea de comandos del DOS, pu‚s nos daria   ³ "
    PRINT " ³ errores en las lineas de CRC, i en algunos Vectores de Interrupci¢n, por lo³ "
    PRINT " ³ tanto para saber si ha habido intrusi¢n de virus se debe reinicializar el  ³ "
    PRINT " ³ ordenador haciendo un reset, o par rlo mediante el interruptor.            ³ "
    PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ "
RETURN

PANTALLA2:
    CLS
    PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
    PRINT " ³  CHECKVIR  ³ *************** A Y U D A **************          P gina 2/4  ³ "
    PRINT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ "
    PRINT " ³  RELACION DE FICHEROS QUE CREA Y UTILIZA:                                  ³ "
    PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³  CHECKVIR.CRC : Contiene las capacidades de memoria, los CRC's y la lon-   ³ "
    PRINT " ³                 gitud de los ficheros vitales del sistema.                 ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³  CHECKVIR.INT : Tiene las direcciones de los Vectores de Interrupci¢n.     ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³  CHECKVIR.TPA : Contiene la MBR, ¢ Tabla de Particiones.                   ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³  CHECKVIR.BOT : Contiene el BOOT, o sea el Arranque del DOS.               ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³  CHECKANT.TPA : Contiene la MBR anterior cuando se restaura la original.   ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³  CHECKANT.BOT : Contiene el BOOT anterior cuando se restaura el original.  ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³  CHECKVIR.NFO : Contiene el Informe de las comprobaciones cuando se utiliza³ "
    PRINT " ³                 la opci¢n /I.                                              ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ "
RETURN

PANTALLA3:
    CLS
    PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
    PRINT " ³  CHECKVIR  ³ *************** A Y U D A **************          P gina 3/4  ³ "
    PRINT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ "
    PRINT " ³  FORMATO:   CHECKVIR [opciones]                                            ³ "
    PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³ "
    PRINT " ³  OPCIONES:                                                                 ³ "
    PRINT " ³     /? ¢ /H   : Muestra la Ayuda.                                          ³ "
    PRINT " ³     /D unidad : Donde la unidad es por defecto &H80 que corresponde al     ³ "
    PRINT " ³                 primer disco duro.                                         ³ "
    PRINT " ³                 UNIDAD:                                                    ³ "
    PRINT " ³                         &H80 - PRIMER DISCO DURO  (* valor por defecto *)  ³ "
    PRINT " ³                         &H81 - SEGUNDO DISCO DURO                          ³ "
    PRINT " ³     /R        : Restaura la Tabla de Particiones (MBR) y el Sector de      ³ "
    PRINT " ³                 Arranque (BOOT), en caso de que sean erroneos.             ³ "
    PRINT " ³     /I        : Genera el fichero CHECKVIR.NFO con el resultado de todas   ³ "
    PRINT " ³                 las comprobaciones.                                        ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ "
RETURN

PANTALLA4:
    CLS
    PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
    PRINT " ³  CHECKVIR  ³ *************** A Y U D A **************          P gina 4/4  ³ "
    PRINT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ "
    PRINT " ³                                                                            ³ "
    PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³     Programado enteramente en Quick Basic ver. 4.5                         ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³     Programaci¢n y dise¤o:                                                 ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³              GABRIEL MARTI I FUENTES                                       ³ "
    PRINT " ³              ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ                                       ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³     (c)1991-97  Gamma Software.                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³        Versi¢n " + VER$ + " - 23/Junio/1997                                      ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ "
RETURN

PANTPATRO:
    CLS
    PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
    PRINT " ³  CHECKVIR  ³ *************** A Y U D A **************          P gina 5/5  ³ "
    PRINT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ "
    PRINT " ³  (c)1991-97  Gabriel Marti i Fuentes                                       ³ "
    PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³                                                                            ³ "
    PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³ "
    PRINT " ³ PANTALLA RESERVADA PARA VERSIONES POSTERIORES ...                          ³ "
    PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ "
RETURN
END SUB

SUB ColorApagat
    COLOR 8, 7
END SUB

SUB ColorBrillant
    COLOR 15, 7
END SUB

SUB ColorInvers
    COLOR 0, 7
END SUB

SUB ColorNormal
    COLOR 0, 3
END SUB

SUB ColorSistema
    COLOR 7, 0
END SUB

SUB ColorTitol
    COLOR 4, 7
END SUB

'
' COMPARA EL BOOT DE LA UNITAT SELúLECCIONADA
'
FUNCTION COMPBOOT (UNITAT%)
SHARED LIMEN%, INFO%, NFH$
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

INREG.AX = &H201             ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
INREG.DX = 256 + UNITAT%     ' CAP€AL 1, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Leyendo (BOOT) de Unidad "; HEX$(UNITAT%); " ...";
ColorNormal
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Comparando (BOOT) con fichero CHECKVIR.BOT ...";
ColorNormal
OPEN "CHECKVIR.BOT" FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
MAL% = 0
FOR i% = 1 TO 512
   GET #1, i%
   IF SEC%(i%) <> CVI(BYTE2$) THEN MAL% = MAL% + 1
NEXT i%
CLOSE #1
OK% = 1
IF MAL% > 0 THEN
   OK$ = "INCORRECTO"
   LOCATE LIMEN%, 56: PRINT "BOOT ERRONEO"
   LIMEN% = LIMEN% + 1
   LOCATE 22, 45: PRINT OK$
   OK% = RESTAURA(UNITAT%, 1)
ELSE
   OK$ = " CORRECTO "
   LOCATE 22, 45: PRINT OK$
END IF
IF INFO% = 1 THEN
   PRINT #5, USING NFH$; "SECTOR DE ARRANQUE"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"; OK$
   IF MAL% > 0 THEN
     IF OK% = 1 THEN
        PRINT #5, USING NFH$; " ÀÄÄÄÄÄÄ>"; ""; ""; "CORREGIDO"
     ELSE
        PRINT #5, USING NFH$; " ÀÄÄÄÄÄÄ>"; ""; ""; "NO CORREGIDO"
     END IF
   END IF
END IF
COMPBOOT = OK%
END FUNCTION

'
' COMPARA LA TAULA DE PARTICIONS DE LA UNITAT SELúLECCIONADA
'
FUNCTION COMPTPA (UNITAT%)
SHARED LIMEN%, INFO%, NFH$
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

INREG.AX = &H201             ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
INREG.DX = UNITAT%           ' CAP€AL 0, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Leyendo (MBR) de Unidad "; HEX$(UNITAT%); " ...";
ColorNormal
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Comparando (MBR) con fichero CHECKVIR.TPA ...";
ColorNormal
OPEN "CHECKVIR.TPA" FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
MAL% = 0
FOR i% = 1 TO 512
   GET #1, i%
   IF SEC%(i%) <> CVI(BYTE2$) THEN MAL% = MAL% + 1
NEXT i%
CLOSE #1
OK% = 1
IF MAL% > 0 THEN
   OK$ = "INCORRECTO"
   LOCATE LIMEN%, 56: PRINT "MBR ERRONEO"
   LIMEN% = LIMEN% + 1
   LOCATE 21, 45: PRINT OK$
   OK% = RESTAURA(UNITAT%, 0)
ELSE
   OK$ = " CORRECTO "
   LOCATE 21, 45: PRINT OK$
END IF
IF INFO% = 1 THEN
   PRINT #5, USING NFH$; "MASTER BOOT RECORD"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"; "ÄÄÄÄÄÄÄÄÄÄÄÄ"; OK$
   IF MAL% > 0 THEN
     IF OK% = 1 THEN
        PRINT #5, USING NFH$; " ÀÄÄÄÄÄÄ>"; ""; ""; "CORREGIDO"
     ELSE
        PRINT #5, USING NFH$; " ÀÄÄÄÄÄÄ>"; ""; ""; "NO CORREGIDO"
     END IF
   END IF
END IF
COMPTPA = OK%
END FUNCTION

'
' RETORNA LA QUANTITAT DE MEMORIA EXTENDIDA LLIURE DEL SISTEMA
'
FUNCTION EXTENDIDA
  DIM INREG AS REGTYPEX      ' REGISTRES DE ENTRADA
  DIM OUTREG AS REGTYPEX     ' REGISTRES DE SORTIDA
  INREG.AX = &H8800          ' FUNCIO 88 - CALCULAR TAMANY MEMORIA EXTENDIDA
  CALL INTERRUPTX(&H15, INREG, OUTREG)
  IF OUTREG.AX < 0 THEN
     EXTENDIDA = 0
  ELSE
     EXTENDIDA = OUTREG.AX
  END IF
END FUNCTION

'
' VISUALITZA UN MISSATGE AMB SCROLL LATERAL
'
SUB LINEDISPLAY (TEXTE$, LIN%, COL%, NUM%)
    X$ = ""
    TEXTE$ = SPACE$(NUM% - 1) + TEXTE$
    L% = LEN(TEXTE$)
    P% = 1
    SOUND 3000, .15
    DO
       LOCATE LIN%, COL%
       D$ = MID$(TEXTE$, P%, NUM%)
       IF LEN(D$) < NUM% THEN
          D$ = D$ + SPACE$(NUM% - LEN(D$))
       END IF
       LOCATE LIN%, COL%: PRINT D$;
       P% = P% + 1
       IF P% > L% THEN P% = 1: SOUND 3000, .15
       SOUND 2000, .05
       X$ = PAUSA$(.152)
    LOOP UNTIL X$ = CHR$(13)
    EXIT SUB
END SUB

'
' DIBUIX DE LA PANTALLA PRINCIPAL
'
SUB PANTALLA (MEN$)
   SHARED VER$
   CLS
   ColorBrillant
   LOCATE 1, 1:  PRINT "ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß"
   ColorTitol
   LOCATE 2, 1:  PRINT "   CHECKVIR   v " + VER$ + "               (c)1991-97 " + MEN$ + " "
   ColorApagat
   LOCATE 3, 1:  PRINT "ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ"
   COLOR 14, 4
   LOCATE 5, 1:  PRINT "                                  Valores     Estado     Mensajes / Errores     "
   ColorNormal
   LOCATE 6, 1:  PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
   LOCATE 7, 1:  PRINT " ³ Memoria RAM Base úúúúúúúúú ³            ³          ³                       ³ "
   LOCATE 8, 1:  PRINT " ³ Memoria Extendida úúúúúúúú ³            ³          ³                       ³ "
   LOCATE 9, 1:  PRINT " ³ CRC #1 + #2 úúúúúúúúúúúúúú ³            ³          ³                       ³ "
   LOCATE 10, 1: PRINT " ³ Interprete de Comandos úúú ³            ³          ³                       ³ "
   LOCATE 11, 1: PRINT " ³ Longitud CHECKVIR.EXE úúúú ³            ³          ³                       ³ "
   LOCATE 12, 1: PRINT " ³ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ³            ³          ³                       ³ "
   LOCATE 13, 1: PRINT " ³ Tabla Vectores Interrupcion³            ³          ³                       ³ "
   LOCATE 14, 1: PRINT " ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³            ³          ³                       ³ "
   LOCATE 15, 1: PRINT " ³                            ³            ³          ³                       ³ "
   LOCATE 16, 1: PRINT " ³                            ³            ³          ³                       ³ "
   LOCATE 17, 1: PRINT " ³                            ³            ³          ³                       ³ "
   LOCATE 18, 1: PRINT " ³                            ³            ³          ³                       ³ "
   LOCATE 19, 1: PRINT " ³                            ³            ³          ³                       ³ "
   LOCATE 20, 1: PRINT " ³ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ³            ³          ³                       ³ "
   LOCATE 21, 1: PRINT " ³ Tabla de Particiones (MBR) ³            ³          ³                       ³ "
   LOCATE 22, 1: PRINT " ³ Sector de Arranque (BOOT)  ³            ³          ³                       ³ "
   LOCATE 23, 1: PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ "
   ColorInvers
   LOCATE 24, 1: PRINT SPACE$(80);
   ColorNormal
END SUB

'
' EXECUTA UNA PAUSA EXPRESADA EN TICS DE RELLOTGE
'
FUNCTION PAUSA$ (TIC)
    INIT = TIMER
    K$ = ""
    DO
       K$ = INKEY$
    LOOP UNTIL (TIMER > INIT + TIC) OR (K$ <> "")
    PAUSA$ = K$
END FUNCTION

'
' LLEGEIX UN SECTOR DEL DISC
'
SUB READSEC (UNITAT%, CARA%)
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

INREG.AX = &H201                  ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1                    ' CILINDRE 0, SECTOR 1
INREG.DX = 256 * CARA% + UNITAT%  ' CAP€AL CARA%, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))        ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))        ' OFFSET DE DADES
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
IF CARA% = 0 THEN
   L$ = "(MBR)": FICH$ = "CHECKVIR.TPA"
ELSE
   L$ = "(BOOT)": FICH$ = "CHECKVIR.BOT"
END IF
LOCATE 24, 1: PRINT " Leyendo "; L$; " de Unidad "; HEX$(UNITAT%); " ...";
ColorNormal
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Guardando "; L$; " en fichero "; FICH$; " ..."; CHR$(7);
ColorNormal
OPEN FICH$ FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR i% = 1 TO 512
   LSET BYTE2$ = MKI$(SEC%(i%))
   PUT #1, i%
NEXT i%
CLOSE #1
END SUB

'
' RESTAURA LA MBR O EL BOOT
'
FUNCTION RESTAURA (UNITAT%, CARA%)
    SHARED REST%
    ColorInvers
    LOCATE 24, 1: PRINT SPACE$(80);
    IF REST% = 1 THEN
      IF CARA% = 0 THEN
         LOCATE 24, 1: PRINT " Restaurar TABLA DE PARTICIONES (MBR) [s/n]? ..."; CHR$(7);
      ELSE
         LOCATE 24, 1: PRINT " Restaurar SECTOR DE ARRANQUE  (BOOT) [s/n]? ..."; CHR$(7);
      END IF
      ColorNormal
      SOUND 2000, .2
      SOUND 1300, .15
      K$ = ""
      DO WHILE K$ <> "S" AND K$ <> "N" AND K$ <> CHR$(27)
         K$ = UCASE$(INKEY$)
      LOOP
    ELSE
      K$ = "N"
    END IF
    ColorNormal
    IF K$ = "S" THEN
       CALL SALVASEC(UNITAT%, CARA%)
       CALL WRITESEC(UNITAT%, CARA%)
       IF CARA% = 0 THEN
          LOCATE 20, 56: PRINT "MBR CORREGIDO     "
       ELSE
          LOCATE 21, 56: PRINT "BOOT CORREGIDO    "
       END IF
       RESTAURA = 1
    ELSE
       IF CARA% = 0 THEN
          LOCATE 20, 56: PRINT "MBR NO CORREGIDO  "
       ELSE
          LOCATE 21, 56: PRINT "BOOT NO CORREGIDO "
       END IF
       RESTAURA = 0
    END IF
END FUNCTION

'
' SALVA LA INFORMACIO ANTERIOR DE LA MBR I/O DEL BOOT
'
SUB SALVASEC (UNITAT%, CARA%)
DIM INREG AS REGTYPEX             ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX            ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)                ' SECTOR

INREG.AX = &H201                  ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1                    ' CILINDRE 0, SECTOR 1
INREG.DX = 256 * CARA% + UNITAT%  ' CAP€AL CARA%, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))        ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))        ' OFFSET DE DADES
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
IF CARA% = 0 THEN
   L$ = "(MBR)": FICH$ = "CHECKANT.TPA"
ELSE
   L$ = "(BOOT)": FICH$ = "CHECKANT.BOT"
END IF
LOCATE 24, 1: PRINT " Leyendo "; HEX$(UNITAT%); " ...";
ColorNormal
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Salvando " + L$ + " en fichero " + FICH$ + " ..."; CHR$(7);
ColorNormal
OPEN FICH$ FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR i% = 1 TO 512
   LSET BYTE2$ = MKI$(SEC%(i%))
   PUT #1, i%
NEXT i%
CLOSE #1
END SUB

'
' Torna el Tamany del COMMAND.COM
'
' Mira si esta definida la variable del sistema COMSPEC
' y obte el tamany del fitxer expecificat si existeix.
' Sino, busca el fitxer COMMAND.COM a la arrel de la unitat C:.
'
FUNCTION TamanyCOMMAND
   COMMANDCOM$ = ENVIRON$("COMSPEC")
  
   IF COMMANDCOM$ = "" THEN COMMANDCOM$ = "C:\COMMAND.COM"

   OPEN COMMANDCOM$ FOR RANDOM ACCESS READ AS #1
   IF LOF(1) <> 0 THEN
      CLOSE #1
      OPEN COMMANDCOM$ FOR INPUT AS #1
      LCO = LOF(1)
      CLOSE #1
   ELSE
      CLOSE #1
      LCO = 0
      KILL COMMANDCOM$
   END IF

   TamanyCOMMAND = LCO
END FUNCTION

'
' ESCRIU LA INFORMACIO DE UN SECTOR
'
SUB WRITESEC (UNITAT%, CARA%)
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

INREG.AX = &H301                    ' FUNCIO 3  - ESCRIURE 1 SECTOR
INREG.CX = &H1                      ' CILINDRE 0, SECTOR 1
INREG.DX = 256 * CARA% + UNITAT%    ' CAP€AL CARA%, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))          ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))          ' OFFSET DE DADES
IF CARA% = 0 THEN
   L$ = "(MBR)": FICH$ = "CHECKVIR.TPA"
ELSE
   L$ = "(BOOT)": FICH$ = "CHECKVIR.BOT"
END IF
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Leyendo "; L$; " del fichero "; FICH$; " ...";
ColorNormal
PUN% = 1
OPEN FICH$ FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR i% = 1 TO 512
   GET #1, i%
   SEC%(i%) = CVI(BYTE2$)
NEXT i%
CLOSE #1
ColorInvers
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Escribiendo "; L$; " en Unidad "; HEX$(UNITAT%); " ..."; CHR$(7);
ColorNormal
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
END SUB

