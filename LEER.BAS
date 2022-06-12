DECLARE SUB ABOUT (VER$, FECHA$)
DECLARE SUB PAUSA (TIC%)
DECLARE FUNCTION ENTRADA$ (NUMCAR%, INI$, CURSORS%, tinta1%, fons1%)
DECLARE SUB IMPRIMIR (X$)
DECLARE FUNCTION BUSCAR$ (BU$)
DECLARE SUB FINESTRA (F1%, C1%, F2%, C2%, TI%, FO%, SO%)
DECLARE SUB AJUDA ()
' **********************************
' ** LEER.BAS  (C)1991 GABI MARTI **
' **********************************
'
 VER$ = "1.03 ç"
 FECHA$ = "21/08/1991"
 ON ERROR GOTO DISERROR
 CO$ = UCASE$(COMMAND$)
 CLS
 IF INSTR(CO$, " ") > 0 THEN
    X$ = LEFT$(CO$, INSTR(CO$, " ") - 1)
 ELSE
    X$ = CO$
 END IF
 IF INSTR(CO$, "/NF") > 0 THEN FILT% = 0 ELSE FILT% = 1
 IF X$ = "" THEN BEEP: GOTO INFO
 OPEN X$ FOR RANDOM AS #1
 L = LOF(1)
 CLOSE
 IF L = 0 THEN GOTO VACIO
 LOCATE 2, 1: PRINT STRING$(80, "Ä");
 ' CONTADOR DE LINIAS DE TEXTE
 D% = 0
 LOCATE 1, 2: PRINT X$;
 OPEN X$ FOR INPUT AS #1
 LOCATE 1, 30: PRINT "CONTANDO LINEA ... ";
 DO WHILE NOT EOF(1)
    LINE INPUT #1, D$
    D% = D% + 1
    LOCATE 1, 50: PRINT USING "##,###"; D%
 LOOP
 CLOSE
 ' CARREGADOR DEL TEXTE
 OPEN X$ FOR INPUT AS #1
 DIM LIN$(1 TO D%)
 C% = 1
 AM% = 0
 LOCATE 1, 30: PRINT "CARGANDO LINEA ... ";
 DO WHILE NOT EOF(1)
    LOCATE 1, 50: PRINT USING "##,###"; C%;
    PRINT "      MEM. ";
    PRINT USING "###,###"; FRE("");
    IF FRE("") < 256 THEN EXIT DO
    LINE INPUT #1, LIN$(C%)
    IF FILT% = 1 THEN
       R$ = ""
       FOR I% = 1 TO LEN(LIN$(C%))
           CH$ = MID$(LIN$(C%), I%, 1)
           IF ASC(CH$) >= 32 AND ASC(CH$) <> 127 THEN R$ = R$ + CH$
       NEXT I%
       LIN$(C%) = R$
       IF LEN(LIN$(C%)) > AM% THEN AM% = LEN(LIN$(C%))
    END IF
    C% = C% + 1
 LOOP
 CLOSE
 COLOR 0, 7
 LOCATE 1, 1: PRINT SPACE$(80);
 LOCATE 1, 1: PRINT " "; X$;
100 REM
 COLOR 7, 0
 ' VISUALITZAR EL TEXTE
 LOCATE 2, 79: PRINT "Â";
 LOCATE 23, 1: PRINT STRING$(80, "Ä");
 LOCATE 23, 79: PRINT "Á";
 COLOR 0, 7
 LOCATE 1, 65: PRINT "[F1] = Ayuda ";
 LOCATE 3, 80: PRINT CHR$(24);
 LOCATE 22, 80: PRINT CHR$(25);
 LOCATE 24, 1: PRINT CHR$(27);
 LOCATE 24, 80: PRINT CHR$(26);
 COLOR 7, 0
 BU$ = ""
 INI% = 1
 CL% = 1
 DO
   COLOR 0, 7
   LOCATE 1, 25: PRINT "LINEA ";
   PRINT USING "#,### #,###"; INI%; D%
   LOCATE 1, 45: PRINT "COLUMNA ";
   PRINT USING "#,###"; CL%;
   COLOR 7, 0
   PUN% = INI%
   FOR X% = 3 TO 22
       IF (PUN% + X% - 3) <= D% THEN
          L$ = MID$(LIN$(PUN% + X% - 3), CL%, 78)
          L$ = L$ + SPACE$(78 - LEN(L$))
          LOCATE X%, 1: PRINT L$; " ";
       ELSE
          LOCATE X%, 1: PRINT SPACE$(78);
       END IF
       SELECT CASE X%
          CASE 3
            LOCATE X%, 79: PRINT "³";
          CASE 22
            LOCATE X%, 79: PRINT "³";
          CASE ELSE
            LOCATE X%, 79: PRINT "³°";
       END SELECT
       LOCATE (INI% * 17 / D%) + 4, 80: PRINT "²";
       LOCATE 24, 2: PRINT STRING$(78, "°");
       LOCATE 24, (CL% * 78 / AM%) + 1: PRINT "²";
   NEXT X%
   DO
     K$ = INKEY$
   LOOP UNTIL K$ <> ""
   ' AMUNT
   IF K$ = CHR$(0) + CHR$(72) THEN INI% = INI% - 1: IF INI% < 1 THEN INI% = 1
   ' AVALL
   IF K$ = CHR$(0) + CHR$(80) THEN INI% = INI% + 1: IF INI% > D% THEN INI% = D%
   ' DRETA
   IF K$ = CHR$(0) + CHR$(77) THEN CL% = CL% + 5: IF CL% > AM% THEN CL% = AM%
   ' ESQUERRA
   IF K$ = CHR$(0) + CHR$(75) THEN CL% = CL% - 5: IF CL% < 1 THEN CL% = 1
   ' PAG. AVALL
   IF K$ = CHR$(0) + CHR$(81) THEN INI% = INI% + 19: IF INI% > D% THEN INI% = D%
   ' PAG. AMUNT
   IF K$ = CHR$(0) + CHR$(73) THEN INI% = INI% - 19: IF INI% < 1 THEN INI% = 1
   ' INICI
   IF K$ = CHR$(0) + CHR$(71) THEN INI% = 1
   ' FI
   IF K$ = CHR$(0) + CHR$(79) THEN INI% = D%
   ' F1 - AJUDA
   IF K$ = CHR$(0) + CHR$(59) THEN CALL AJUDA
   ' F7 - IMPRIMIR
   IF K$ = CHR$(0) + CHR$(65) THEN CALL IMPRIMIR(X$)
   ' F8 - BUSCAR
   IF K$ = CHR$(0) + CHR$(66) THEN BU$ = BUSCAR$(BU$)
   ' F10 - SOBRE EL PROGRAMA
   IF K$ = CHR$(0) + CHR$(&H44) THEN CALL ABOUT(VER$, FECHA$)
 LOOP UNTIL K$ = CHR$(27)
 CLS
 IF INSTR(CO$, "/NM") = 0 THEN
    PRINT " LEER "; VER$; " - " + FECHA$
    PRINT " Gracias por haberme utilizado. "
    PRINT
    PRINT " (c)1991 Gabi Marti i Fuentes "
    PRINT
 END IF
 END

VACIO:
  PRINT " ERROR: "
  PRINT " Fichero no encontrado "
  KILL X$
  END
INFO:
  COLOR 0, 7
  PRINT "  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿                                                 "
  PRINT "  ³ LEER  "; VER$; " ÃÄ¿  Copyright 1991     Gabriel Marti i Fuentes.  "
  PRINT "  ÀÂÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ³                                               "
  PRINT "   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ                                               "
  COLOR 7, 0
  PRINT
  PRINT " Formato: "
  PRINT
  PRINT " LEER nomarchivo [opciones]"
  PRINT
  PRINT " opciones: "
  PRINT
  PRINT " /NM    - No visualiza ningun mensaje al finalizar el programa. "
  PRINT "          Es £til para enlazar desde dentro de otros programas. "
  PRINT
  PRINT " /NF    - No filtra los caracteres de control. "
  PRINT
  END

DISERROR:
  IF ERR = 24 OR ERR = 25 OR ERR = 27 THEN
     CLOSE
     LOCATE 8, 7: PRINT "IMPRESORA NO PREPARADA"
     BEEP
     K$ = INPUT$(1)
     RESUME 100
  ELSE
     COLOR 7, 0
     CLS
     PRINT " ERROR "; ERR; " EN LA LINEA "; ERL
     END
  END IF

SUB ABOUT (VER$, FECHA$)
    CALL FINESTRA(5, 5, 21, 60, 0, 7, 0)
    LOCATE 5, 7: PRINT " Sobre el programa "
    LOCATE 7, 10: PRINT "LEER " + VER$ + " - " + FECHA$
    LOCATE 9, 10: PRINT "Realizado enteramente en Quick Basic ver. 4.5 "
    LOCATE 11, 10: PRINT "Programado por : "
    LOCATE 12, 30: PRINT "Gabriel Marti i Fuentes"
    LOCATE 13, 30: PRINT "c/ San Juan, 22. Atico"
    LOCATE 14, 30: PRINT "08370 - Calella (BARNA)"
    LOCATE 15, 30: PRINT "Ä*Ä*Ä*Ä*Ä*Ä*Ä*Ä*Ä*Ä*Ä*Ä"
    LOCATE 17, 10: PRINT "Esta versi¢n ‚s de DOMINIO PUBLICO, y puede"
    LOCATE 18, 10: PRINT "ser copiada libremente sin permiso previo  "
    LOCATE 19, 10: PRINT "del autor. "
    K$ = INPUT$(1)
END SUB

SUB AJUDA
'    CALL FINESTRA(5, 5, 20, 74, 0, 7, 0)
    CALL FINESTRA(5, 5, 20, 35, 0, 7, 0)
    LOCATE 5, 7: PRINT " Ayuda ";
    LOCATE 7, 7:  PRINT " Movimiento "; " "; CHR$(24); " "; CHR$(25); " "; CHR$(26); " "; CHR$(27); SPACE$(6);
    LOCATE 9, 7:  PRINT " Home =  Principio Texto   "
    LOCATE 10, 7: PRINT "  End =  Final de Texto    "
    LOCATE 11, 7: PRINT " PgUp =  P gina Arriba     "
    LOCATE 12, 7: PRINT " PgDn =  P gina Abajo      "
    LOCATE 14, 7: PRINT "   F7 =  Imprimir          "
    LOCATE 15, 7: PRINT "   F8 =  Buscar            "
    LOCATE 16, 7: PRINT "  F10 =  Sobre el programa "
    LOCATE 18, 7: PRINT "  Esc =  Salir             "
    K$ = INPUT$(1)
END SUB

FUNCTION BUSCAR$ (T$)
    SHARED INI%, CL%, D%, LIN$()
    INI$ = T$
    CALL FINESTRA(5, 5, 9, 30, 0, 7, 0)
    LOCATE 5, 7: PRINT " Buscar ";
    LOCATE 7, 6: PRINT CHR$(16); : T$ = ENTRADA$(23, T$, 0, 0, 7)
    IF T$ <> CHR$(27) THEN
       BUSCAR = T$
       T$ = RTRIM$(LTRIM$(T$))
       FOR I% = INI% TO D%
           P% = INSTR(LIN$(I%), T$)
           IF P% > 0 THEN INI% = I%: CL% = P%: EXIT FOR
       NEXT I%
    ELSE
       BUSCAR = INI$
    END IF
END FUNCTION

FUNCTION ENTRADA$ (NUMCAR%, INI$, CURSORS%, tinta1%, fons1%)
    Y% = POS(0)
    X% = CSRLIN
    PC% = 1
    K$ = ""
    CAR$ = INI$
    COLOR tinta1%, fons1%
    LOCATE X%, Y%: PRINT STRING$(NUMCAR%, 32);
    LOCATE X%, Y%: PRINT CAR$; STRING$(NUMCAR% - LEN(CAR$), 32);
    COLOR fons1% + 16, tinta1%
    LOCATE X%, Y% + PC% - 1
    IF PC% > LEN(CAR$) THEN
       IF PC% <= NUMCAR% THEN PRINT CHR$(32);
    ELSE
       PRINT MID$(CAR$, PC%, 1);
    END IF
    COLOR tinta1%, fons1%
    WHILE K$ <> CHR$(13) AND K$ <> CHR$(27) AND K$ <> CHR$(1) AND K$ <> CHR$(4)
          DO
            K$ = INKEY$
          LOOP WHILE K$ = ""
          'HOME
           IF K$ = CHR$(0) + CHR$(71) THEN PC% = 1
          'END
           IF K$ = CHR$(0) + CHR$(79) THEN PC% = LEN(CAR$) + 1: IF PC% > NUMCAR% THEN PC% = NUMCAR%
          'ESQERR.
           IF K$ = CHR$(0) + CHR$(75) THEN PC% = PC% - 1
          'DRTA.
           IF K$ = CHR$(0) + CHR$(77) THEN PC% = PC% + 1
           IF PC% > LEN(CAR$) THEN PC% = LEN(CAR$) + 1
           'AMUNT.
            IF CURSORS% = 1 THEN IF K$ = CHR$(0) + CHR$(72) THEN K$ = CHR$(1)
           'AVALL.
            IF CURSORS% = 1 THEN IF K$ = CHR$(0) + CHR$(80) THEN K$ = CHR$(4)
           'CTRL + L
           IF K$ = CHR$(12) THEN
             CAR$ = ""
             PC% = 1
          END IF
          'DEL
          IF K$ = CHR$(0) + CHR$(83) THEN
                CAR$ = LEFT$(CAR$, PC% - 1) + MID$(CAR$, PC% + 1)
          END IF
          'INS
          IF K$ = CHR$(0) + CHR$(82) THEN
                CAR$ = LEFT$(CAR$, PC% - 1) + " " + MID$(CAR$, PC%)
                IF LEN(CAR$) > NUMCAR% THEN CAR$ = LEFT$(CAR$, NUMCAR%)
          END IF
          'BACKSPACE
          IF K$ = CHR$(8) THEN
             IF LEN(CAR$) > 0 AND PC% > 1 THEN
                CAR$ = LEFT$(CAR$, PC% - 2) + MID$(CAR$, PC%): PC% = PC% - 1
             ELSE
                SOUND 500, .12
                SOUND 600, .12
             END IF
          END IF
          '
          IF K$ >= CHR$(32) AND K$ <= CHR$(255) AND LEN(CAR$) <= NUMCAR% THEN
             IF PC% <= LEN(CAR$) THEN
                MID$(CAR$, PC%, 1) = K$
             ELSE
               IF LEN(CAR$) < NUMCAR% THEN CAR$ = CAR$ + K$
             END IF
             PC% = PC% + 1
          END IF
RETORNO:
          IF PC% > NUMCAR% THEN PC% = NUMCAR%: SOUND 1600, .15
          IF PC% < 1 THEN PC% = 1: SOUND 600, .15
          LOCATE X%, Y%: PRINT CAR$; STRING$(NUMCAR% - LEN(CAR$), 32);
          COLOR fons1% + 16, tinta1%
          LOCATE X%, Y% + PC% - 1
          IF PC% > LEN(CAR$) THEN
             IF PC% <= NUMCAR% THEN PRINT CHR$(32);
          ELSE
             PRINT MID$(CAR$, PC%, 1);
          END IF
          COLOR tinta1%, fons1%
    WEND
    IF K$ = CHR$(27) OR K$ = CHR$(1) OR K$ = CHR$(4) THEN ENTRADA$ = K$ ELSE ENTRADA$ = CAR$
    LOCATE X%, Y%: PRINT CAR$ + SPACE$(NUMCAR% - LEN(CAR$))
    EXIT FUNCTION
END FUNCTION

SUB FINESTRA (F1%, C1%, F2%, C2%, TI%, FO%, SO%)
    COLOR TI%, FO%
    LOCATE F1%, C1%: PRINT "Ú"
    LOCATE F1%, C2%: PRINT "¿"
    LOCATE F2%, C1%: PRINT "À"
    LOCATE F2%, C2%: PRINT "Ù"
    LOCATE F1%, C1% + 1: PRINT STRING$(C2% - C1% - 1, "Ä")
    LOCATE F2%, C1% + 1: PRINT STRING$(C2% - C1% - 1, "Ä")
    FOR I% = F1% + 1 TO F2% - 1
        LOCATE I%, C1%: PRINT "³"
        LOCATE I%, C1% + 1: PRINT SPACE$(C2% - C1% - 1)
        LOCATE I%, C2%: PRINT "³"
    NEXT I%
    IF SO% > 0 THEN
       FOR I% = F1% + 1 TO F2%
           LOCATE I%, C2% + 1: PRINT CHR$(SO%)
       NEXT I%
       LOCATE F2% + 1, C1% + 1: PRINT STRING$(C2% - C1% + 1, SO%)
    END IF
END SUB

SUB IMPRIMIR (X$)
    CALL FINESTRA(5, 5, 9, 30, 0, 7, 0)
    LOCATE 5, 7: PRINT " Imprimir ";
    OPEN X$ FOR INPUT AS #1
    LOCATE 7, 10: PRINT "Linea ... ";
    C% = 1
    WIDTH LPRINT 132
    DO WHILE NOT EOF(1)
       LOCATE 7, 19: PRINT USING "##,###"; C%;
       LINE INPUT #1, R$
       LPRINT R$
       C% = C% + 1
    LOOP
    CLOSE
END SUB

SUB PAUSA (TIC%)
    INIT = TIMER
    DO
     REM
    LOOP UNTIL (TIMER > INIT + TIC%)
END SUB

