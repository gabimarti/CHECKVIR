   ' =======================================================
   '   CHKBOOT -- ver 1.00 â  (c)1992 GABI MARTI I FUENTES
   ' =======================================================
   '
   ' Antes de compilar se tiene que cargar el Quick Basic
   ' con los siguientes parametros:  QB /L QB.QLB
   '
   '$DYNAMIC
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
DECLARE SUB SALVASEC (UNITAT%, CARA%)
DECLARE SUB AYUDA ()
DECLARE SUB PAUSA (TIC)
DECLARE SUB LINEDISPLAY (TEXTE$, LIN%, COL%, NUM%)
DECLARE SUB WRITESEC (UNITAT%, CARA%)
DECLARE FUNCTION RESTAURA! (UNITAT%, CARA%)
DECLARE FUNCTION COMPBOOT! (UNITAT%)
DECLARE FUNCTION COMPTPA (UNITAT%)
DECLARE SUB READSEC (UNITAT%, CARA%)
DECLARE SUB INTERRUPTX (intnum AS INTEGER, INREG AS REGTYPEX, OUTREG AS REGTYPEX)
DECLARE SUB PANTALLA (MEN$)
   '
   UNITAT% = &H80
   ' LONGITUD EN BYTES DEL PROGRAMA CHKBOOT.EXE
   LPRO = 60980
   ' DEFINICIO VARIABLES CAP€ALERA
   PRO$ = "GABI MARTI I FUENTES.           "
   ' COMPROVACIO DE SI S'HA ACTIVAT LA OPCIO D'AJUDA
   IF INSTR(COMMAND$, "/?") > 0 OR INSTR(UCASE$(COMMAND$), "/H") > 0 THEN
      CALL AYUDA
      END
   END IF
   ' MIRO SI SE HA ENTRAT EL PARAMETRE /D unitat PER SELùLECCIONAR UNA
   ' UNITAT DIFERENTE DE LA &H80 (C:)
   P% = INSTR(UCASE$(COMMAND$), "/D")
   IF P% > 0 THEN
      UNITAT% = VAL(MID$(COMMAND$, P% + 2))
   END IF
   LIMEN% = 6
   IF LCASE$(LEFT$(AUT$, 1)) <> "m" THEN GOTO ERRR
   IF MID$(AUT$, 6, 1) <> "." THEN GOTO ERRR
   IF MID$(AUT$, 14, 1) <> "…" THEN GOTO ERRR
   IF LEFT$(PRO$, 1) <> "G" OR MID$(PRO$, 12, 1) <> "I" THEN GOTO ERRR
   IF RIGHT$(PRO$, 6) <> "      " OR MID$(PRO$, 17, 1) <> "N" THEN GOTO ERRR
   CALL PANTALLA(PRO$)
   GOSUB FITXER.COMPROBACIO
   DEF SEG = 0
   MKB = PEEK(&H413) + 256 * PEEK(&H414)
   OPEN "COMMAND.COM" FOR RANDOM ACCESS READ AS #1
   IF LOF(1) <> 0 THEN
      CLOSE #1
      OPEN "COMMAND.COM" FOR INPUT AS #1
      LCO = LOF(1)
      CLOSE #1
   ELSE
      CLOSE #1
      LCO = 0
      LOCATE LIMEN%, 56: PRINT "COMMAND.COM NO EXISTE"
      LIMEN% = LIMEN% + 1
      KILL "COMMAND.COM"
   END IF
   OPEN "CHKBOOT.EXE" FOR INPUT AS #1
    LCH = LOF(1)
   CLOSE #1
   VIRUS% = 0
   IF MEMKB = MKB THEN
      OK1$ = " CORRECTO"
   ELSE
      OK1$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "CAPACIDAD MEMORIA RAM"
      LIMEN% = LIMEN% + 1
      LOCATE LIMEN%, 56: PRINT "HA SIDO MODIFICADA. "
      LIMEN% = LIMEN% + 1
      VIRUS% = 1
   END IF
   IF CRC1 = CR1 THEN
      OK2$ = " CORRECTO"
   ELSE
      OK2$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "MEMORIA ALTERADA."
      LIMEN% = LIMEN% + 1
      VIRUS% = VIRUS% + 1
   END IF
   IF CRC2 = CR2 THEN
      OK3$ = " CORRECTO "
   ELSE
      OK3$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "MEMORIA ALTERADA."
      LIMEN% = LIMEN% + 1
      VIRUS% = VIRUS% + 1
   END IF
   IF LCOM = LCO THEN
      OK5$ = " CORRECTO "
   ELSE
      OK5$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "COMMAND.COM INFECTADO."
      LIMEN% = LIMEN% + 1
      VIRUS% = VIRUS% + 1
   END IF
   IF LPRO = LCH THEN
      OK6$ = " CORRECTO "
   ELSE
      OK6$ = "INCORRECTO"
      LOCATE LIMEN%, 56: PRINT "CHECKVIR.EXE INFECTADO."
      LIMEN% = LIMEN% + 1
      VIRUS% = VIRUS% + 1
   END IF
   LOCATE 6, 33: PRINT USING "###,###"; MKB; : PRINT " Kb"; : LOCATE 6, 45: PRINT OK1$
   LOCATE 9, 33: PRINT USING "###,###"; LCO; : LOCATE 9, 45: PRINT OK5$
   LOCATE 10, 33: PRINT USING "###,###"; LCH; : LOCATE 10, 45: PRINT OK6$
   GOSUB COMPROVA.INT
   CORRE1% = 0
   CORRE2% = 0
   CORRE1% = COMPTPA(UNITAT%)
   CORRE2% = COMPBOOT(UNITAT%)
   COLOR 0, 7
   LOCATE 24, 1: PRINT SPACE$(80);
   IF VIRUS% > 0 THEN
      M$ = "*** Posible infecci¢n v¡rica. ***          "
      M$ = M$ + "    PULSE [RETURN] PARA TERMINAR ...     "
      CALL LINEDISPLAY(M$, 24, 1, 80)
   ELSE
      LOCATE 24, 5: PRINT " Sistema correcto.   Pulse una tecla para salir ... ";
      K$ = INPUT$(1)
   END IF
   COLOR 7, 0
   PRINT
   END
ERRR:
   CLS
   PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
   PRINT "³ Packed file is corrupt ... ³"
   PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
   PRINT CHR$(7); CHR$(7); CHR$(7); CHR$(7);
   END
'
'
FITXER.COMPROBACIO:
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
    INPUT #1, MEMKB
    INPUT #1, CRC1
    INPUT #1, CRC2
    INPUT #1, LCOM
   CLOSE #1
   OPEN "CHECKVIR.INT" FOR RANDOM AS #1
   IF LOF(1) = 0 THEN GOSUB CREA.INT
   CLOSE #1
RETURN
'
COMPROVA.INT:
   RESTORE
   COLOR 0, 7
   LOCATE 24, 1: PRINT SPACE$(80);
   COLOR 7, 0
   CLOSE #1
   OPEN "CHECKVIR.INT" FOR INPUT AS #1
   LIN% = 14
   FOR i% = &H0 TO &H77
       IF LIN% > 18 THEN
          FOR X% = 14 TO 18
              LOCATE X%, 3: PRINT SPACE$(28)
              LOCATE X%, 33: PRINT SPACE$(8)
              LOCATE X%, 45: PRINT SPACE$(10)
          NEXT X%
          LIN% = 14
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
       LOCATE LIN%, 45
       IF VL1% <> FC1% OR VL2% <> FC2% OR VL3% <> FC3% OR VL4% <> FC4% THEN
           PRINT "INCORRECTO"; : SOUND 3600, .1: SOUND 600, .2
           IF LIMEN% < 20 THEN
              LOCATE LIMEN%, 56: PRINT HEX$(i%); " "; IN$;
              LIMEN% = LIMEN% + 1
           END IF
           VIRUS% = VIRUS% + 1
       ELSE
           PRINT " CORRECTO "
       END IF
       LIN% = LIN% + 1
   NEXT i%
   CLOSE #1
RETURN
'
'
CREA.INT:
   CLOSE #1
   COLOR 0, 7
   LOCATE 24, 1: PRINT SPACE$(80);
   LOCATE 24, 1: PRINT " Creaci¢n fichero vectores interrupci¢n ..."; CHR$(7);
   COLOR 7, 0
   OPEN "CHECKVIR.INT" FOR OUTPUT AS #1
   FOR i% = &H0 TO &H77
       COLOR 0, 7
       LOCATE 24, 65: PRINT HEX$(i%);
       COLOR 7, 0
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
CREA.CRC:
   CLOSE #1
   COLOR 0, 7
   LOCATE 24, 1: PRINT SPACE$(80);
   LOCATE 24, 1: PRINT " Creaci¢n del fichero de comprobaci¢n ..."; CHR$(7);
   COLOR 7, 0
   DEF SEG = 0
   MEMKB = PEEK(&H413) + 256 * PEEK(&H414)
   CRC1 = FRE(0)
   CRC2 = FRE(-1)
   OPEN "COMMAND.COM" FOR RANDOM ACCESS READ AS #1
   IF LOF(1) <> 0 THEN
      CLOSE #1
      OPEN "COMMAND.COM" FOR INPUT AS #1
      LCOM = LOF(1)
      CLOSE #1
   ELSE
      CLOSE #1
      LCOM = 0
      LOCATE LIMEN%, 56: PRINT "COMMAND.COM NO EXISTE"
      LIMEN% = LIMEN% + 1
      KILL "COMMAND.COM"
   END IF
   OPEN "CHECKVIR.CRC" FOR OUTPUT AS #1
    PRINT #1, MEMKB
    PRINT #1, CRC1
    PRINT #1, CRC2
    PRINT #1, LCOM
   CLOSE #1
RETURN
'

REM $STATIC
SUB AYUDA
    CLS
    PRINT "       ÚÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
    PRINT "       ³  CHECKVIR  ³  *************** A Y U D A **************  ³"
    PRINT "       ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
    PRINT "       ³  FORMATO:   CHECKVIR [opciones]                         ³"
    PRINT "       ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³"
    PRINT "       ³  OPCIONES:                                              ³"
    PRINT "       ³     /? ¢ /H   : Muestra esta pantalla de Ayuda          ³"
    PRINT "       ³     /D unidad : Donde la unidad es por defecto &H80 que ³"
    PRINT "       ³                 corresponde al primer disco duro.       ³"
    PRINT "       ³                 UNIDAD:                                 ³"
    PRINT "       ³                         &H0  - PRIMERA DISKETERA        ³"
    PRINT "       ³                         &H1  - SEGUNDA DISKETERA        ³"
    PRINT "       ³                         &H80 - PRIMER DISCO DURO (DEF.) ³"
    PRINT "       ³                         &H81 - SEGUNDO DISCO DURO       ³"
    PRINT "       ³     /R        : Restaura la Tabla de Particiones (MBR)  ³"
    PRINT "       ³                 y el Sector de Arranque (BOOT), en caso ³"
    PRINT "       ³                 de que sean erroneos.                   ³"
    PRINT "       ³                                                         ³"
    PRINT "       ³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³"
    PRINT "       ³                                                         ³"
    PRINT "       ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
    PRINT "                (c)1991 MICRO. Inform…tica Calella S.L.           "
    END SUB

FUNCTION COMPBOOT (UNITAT%)
SHARED LIMEN%
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

INREG.AX = &H201             ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
INREG.DX = 256 + UNITAT%     ' CAP€AL 1, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Leyendo (BOOT) de Unidad "; HEX$(UNITAT%); " ...";
COLOR 7, 0
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Comparando (BOOT) con fichero CHECKVIR.BOT ...";
COLOR 7, 0
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
   LOCATE 21, 45: PRINT "INCORRECTO"
   LOCATE LIMEN%, 56: PRINT "BOOT ERRONEO"
   LIMEN% = LIMEN% + 1
   OK% = RESTAURA(UNITAT%, 1)
ELSE
   LOCATE 21, 45: PRINT " CORRECTO "
END IF
COMPBOOT = OK%
END FUNCTION

FUNCTION COMPTPA (UNITAT%)
SHARED LIMEN%
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

INREG.AX = &H201             ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
INREG.DX = UNITAT%           ' CAP€AL 0, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Leyendo (MBR) de Unidad "; HEX$(UNITAT%); " ...";
COLOR 7, 0
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Comparando (MBR) con fichero CHECKVIR.TPA ...";
COLOR 7, 0
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
   LOCATE 20, 45: PRINT "INCORRECTO"
   LOCATE LIMEN%, 56: PRINT "MBR ERRONEO"
   LIMEN% = LIMEN% + 1
   OK% = RESTAURA(UNITAT%, 0)
ELSE
   LOCATE 20, 45: PRINT " CORRECTO "
END IF
COMPTPA = OK%
END FUNCTION

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
       PAUSA (.152)
       X$ = INKEY$
    LOOP UNTIL X$ = CHR$(13)
    EXIT SUB
END SUB

SUB PANTALLA (MEN$)
   CLS
   LOCATE 1, 1: PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
   LOCATE 2, 1: PRINT "³"; : COLOR 0, 7
   PRINT " Checkvir   v 4.11 ç                  (c)1991 " + MEN$;
   COLOR 7, 0: PRINT "³"
   LOCATE 3, 1:  PRINT "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
   LOCATE 4, 1:  PRINT "³                                 Valores     Estado     Mensajes / Errores    ³"
   LOCATE 5, 1:  PRINT "³ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿³"
   LOCATE 6, 1:  PRINT "³³ MEMORIA RAM TOTAL úúúúúúúú ³            ³          ³                       ³³"
   LOCATE 7, 1:  PRINT "³³ CRC #1 úúúúúúúúúúúúúúúúúúú ³            ³          ³                       ³³"
   LOCATE 8, 1:  PRINT "³³ CRC #2 úúúúúúúúúúúúúúúúúúú ³            ³          ³                       ³³"
   LOCATE 9, 1:  PRINT "³³ LONGITUD COMMAND.COM úúúúú ³            ³          ³                       ³³"
   LOCATE 10, 1: PRINT "³³ LONGITUD CHECKVIR.EXE úúúú ³            ³          ³                       ³³"
   LOCATE 11, 1: PRINT "³³ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ³            ³          ³                       ³³"
   LOCATE 12, 1: PRINT "³³ TABLA VECTORES INTERRUPCION³            ³          ³                       ³³"
   LOCATE 13, 1: PRINT "³³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³            ³          ³                       ³³"
   LOCATE 14, 1: PRINT "³³                            ³            ³          ³                       ³³"
   LOCATE 15, 1: PRINT "³³                            ³            ³          ³                       ³³"
   LOCATE 16, 1: PRINT "³³                            ³            ³          ³                       ³³"
   LOCATE 17, 1: PRINT "³³                            ³            ³          ³                       ³³"
   LOCATE 18, 1: PRINT "³³                            ³            ³          ³                       ³³"
   LOCATE 19, 1: PRINT "³³ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ³            ³          ³                       ³³"
   LOCATE 20, 1: PRINT "³³ TABLA DE PARTICIONES (MBR) ³            ³          ³                       ³³"
   LOCATE 21, 1: PRINT "³³ SECTOR DE ARRANQUE (BOOT)  ³            ³          ³                       ³³"
   LOCATE 22, 1: PRINT "³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³"
   LOCATE 23, 1: PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ";
   COLOR 0, 7
   LOCATE 24, 1: PRINT SPACE$(80);
   COLOR 7, 0
END SUB

SUB PAUSA (TIC)
    INIT = TIMER
    DO
     REM
    LOOP UNTIL (TIMER > INIT + TIC)
END SUB

SUB READSEC (UNITAT%, CARA%)
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

INREG.AX = &H201                  ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1                    ' CILINDRE 0, SECTOR 1
INREG.DX = 256 * CARA% + UNITAT%  ' CAP€AL CARA%, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))        ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))        ' OFFSET DE DADES
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
IF CARA% = 0 THEN
   L$ = "(MBR)": FICH$ = "CHECKVIR.TPA"
ELSE
   L$ = "(BOOT)": FICH$ = "CHECKVIR.BOT"
END IF
LOCATE 24, 1: PRINT " Leyendo "; L$; " de Unidad "; HEX$(UNITAT%); " ...";
COLOR 7, 0
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Guardando "; L$; " en fichero "; FICH$; " ..."; CHR$(7);
COLOR 7, 0
OPEN FICH$ FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR i% = 1 TO 512
   LSET BYTE2$ = MKI$(SEC%(i%))
   PUT #1, i%
NEXT i%
CLOSE #1
END SUB

FUNCTION RESTAURA (UNITAT%, CARA%)
    COLOR 0, 7
    LOCATE 24, 1: PRINT SPACE$(80);
    IF REST% = 1 THEN
      IF CARA% = 0 THEN
         LOCATE 24, 1: PRINT " Restaurar TABLA DE PARTICIONES (MBR) [s/n]? ..."; CHR$(7);
      ELSE
         LOCATE 24, 1: PRINT " Restaurar SECTOR DE ARRANQUE  (BOOT) [s/n]? ..."; CHR$(7);
      END IF
      COLOR 7, 0
      SOUND 2000, .2
      SOUND 1300, .15
      K$ = ""
      DO WHILE K$ <> "S" AND K$ <> "N" AND K$ <> CHR$(27)
         K$ = UCASE$(INKEY$)
      LOOP
    ELSE
      K$ = "N"
    END IF
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

SUB SALVASEC (UNITAT%, CARA%)
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

INREG.AX = &H201                  ' FUNCIO 2  - LLEGIR 1 SECTOR
INREG.CX = &H1                    ' CILINDRE 0, SECTOR 1
INREG.DX = 256 * CARA% + UNITAT%  ' CAP€AL CARA%, UNITAT 80 (C:)
INREG.ES = VARSEG(SEC%(1))        ' SEGMENT DE DADES
INREG.BX = VARPTR(SEC%(1))        ' OFFSET DE DADES
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
IF CARA% = 0 THEN
   L$ = "(MBR)": FICH$ = "SALVASEC.TPA"
ELSE
   L$ = "(BOOT)": FICH$ = "SALVASEC.BOT"
END IF
LOCATE 24, 1: PRINT " Leyendo "; HEX$(UNITAT%); " ...";
COLOR 7, 0
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Salvando ..."; CHR$(7);
COLOR 7, 0
OPEN FICH$ FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR i% = 1 TO 512
   LSET BYTE2$ = MKI$(SEC%(i%))
   PUT #1, i%
NEXT i%
CLOSE #1
END SUB

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
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Leyendo "; L$; " del fichero "; FICH$; " ...";
COLOR 7, 0
PUN% = 1
OPEN FICH$ FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS BYTE2$
FOR i% = 1 TO 512
   GET #1, i%
   SEC%(i%) = CVI(BYTE2$)
NEXT i%
CLOSE #1
COLOR 0, 7
LOCATE 24, 1: PRINT SPACE$(80);
LOCATE 24, 1: PRINT " Escribiendo "; L$; " en Unidad "; HEX$(UNITAT%); " ..."; CHR$(7);
COLOR 7, 0
CALL INTERRUPTX(&H13, INREG, OUTREG)
CALL INTERRUPTX(&H13, INREG, OUTREG)
END SUB

