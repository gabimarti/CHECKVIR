DECLARE SUB MOSTRPART (PART() AS ANY)
DECLARE SUB MOSTRAVAL (PART() AS ANY)
DECLARE SUB EDITA ()
DECLARE SUB WRITEPART (UNITAT%)
DECLARE SUB READPART (UNITAT%)
DECLARE FUNCTION SELEUNITAT% (UNITAT%)
DECLARE SUB caratula (TI%, FO%)
DECLARE FUNCTION TCENTRE$ (TEXTE$, LO%)
DECLARE FUNCTION NOESP$ (CAD$)
DECLARE FUNCTION MENU% (TITOL$, TABLA$(), F1%, C1%, F2%, C2%, TI%, FO%)
DECLARE SUB OMPLE (F1%, C1%, F2%, C2%, CH%)
DECLARE SUB FINESTRA (F1%, C1%, F2%, C2%, TI%, FO%, SO%)
DECLARE FUNCTION ENTRADA$ (NUMCAR%, INI$, CURSORS%, tinta1%, fons1%)
'$INCLUDE: 'QB.BI'

TYPE WORD
     HIGH AS STRING * 1
     LOW  AS STRING * 1
END TYPE

TYPE BYTES16
     INDICADOR AS STRING * 1
     INICABEZA AS STRING * 1
     INISECTOR AS STRING * 1
     INICILIND AS STRING * 1
     SISTEMA   AS STRING * 1
     FINCABEZA AS STRING * 1
     FINSECTOR AS STRING * 1
     FINCILIND AS STRING * 1
     SECRELAT  AS STRING * 4
     NUMSECPA  AS STRING * 4
END TYPE

UNITAT% = &H80
INICIO:
CLS
CALL caratula(7, 0)
COLOR 0, 7
CALL OMPLE(1, 1, 24, 80, 178)
LOCATE 1, 1: PRINT TCENTRE$("Editor de la taula de Partici¢ns del Disc Dur", 80);
LOCATE 24, 1: PRINT TCENTRE$("(c)1991 Gabi Mart° i Fuentes", 80);
DIM OP$(1 TO 5)
OP$(1) = " 1. SEL˘LECCIONAR UNITAT FISICA "
OP$(2) = " 2. LLEGIR TAULA PARTICIONS     "
OP$(3) = " 3. EDITAR TAULA PARTICIONS     "
OP$(4) = " 4. ESCRIURE TAULA PARTICIONS   "
OP$(5) = " 5. SORTIR DE PROGRAMA          "
T$ = "EDIPART v1.00"
DO WHILE OP% <> 5
OP% = MENU(T$, OP$(), 3, 21, 11, 54, 0, 7)
  SELECT CASE OP%
       CASE 1
	UNITAT% = SELEUNITAT(UNITAT%)
       CASE 2
	CALL READPART(UNITAT%)
       CASE 3
	CALL EDITA
	COLOR 0, 7
	CALL OMPLE(1, 1, 24, 80, 178)
	LOCATE 1, 1: PRINT TCENTRE$("Editor de la taula de Partici¢ns del Disc Dur", 80);
	LOCATE 24, 1: PRINT TCENTRE$("(c)1991 Gabi Mart° i Fuentes", 80);
       CASE 4
	CALL WRITEPART(UNITAT%)
       CASE ELSE
	SOUND 2000, .1
	SOUND 1000, .15
  END SELECT
LOOP

SUB caratula (TI%, FO%)
    COLOR TI%, FO%
    CLS
    PRINT "   ≥        ≥   €€  ≥        ≥        ≥         ≥   "
    PRINT " ƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒ≈ƒƒ "
    PRINT "   €€€€€€€€€€€  €   €€€€€€€€ ≥€€€€€€  €€€€€€€€  ≥   "
    PRINT "   €    €   ≥€  €   €      € ≥€       €      €  ≥   "
    PRINT "   €€   €€  ≥€  €€  €€       ≥€€      €€     €  ≥   "
    PRINT " ƒƒ€€ƒƒƒ€€ƒƒ≈€ƒƒ€€ƒƒ€€ƒƒƒƒƒ€ƒ≈€€ƒƒƒƒƒƒ€€ƒƒƒƒƒ€ƒƒ≈ƒƒ "
    PRINT "   €€   €€  ≥€  €€  €€€€€€€€ ≥€€      €€€€€€€€  €€  "
    PRINT "   ≥        ≥       ≥        ≥        ≥         ≥   "
    PRINT
    COLOR FO%, TI%
    PRINT "   I n f o r m Ö t i c a   C a l e l l a   S . L .  "
    COLOR TI%, FO%
    PRINT
    PRINT "       Sant Antoni, 93  -  08370 CALELLA       "
    PRINT "       Tel. 769 43 12   -  Fax 766 00 00       "
    LOCATE 20, 1
    PRINT " Programa de £s exclusiu per als täcnics de MICRO. InformÖtica Calella S.L."
    PRINT
    PRINT " Programmed by Gabriel Mart° i Fuentes.    (c)1991 Agost"
    K$ = INPUT$(1)
END SUB

SUB EDITA
    DIM SEC$(1 TO 1024)          ' SECTOR
    DIM FIRMA AS WORD            ' 2 BYTES DE LA FIRMA (55AA)
    DIM PART(1 TO 4) AS BYTES16  ' DADES DE LES 4 PARTICIONS
    CLS
    CALL FINESTRA(2, 1, 23, 80, 7, 0, 0)
    NOMFICH$ = "NONAME  "
    COLOR 0, 7
    LOCATE 1, 1: PRINT SPACE$(80);
    LOCATE 1, 22: PRINT "EDITAR EL FITXER ...         .TPA "
    LOCATE 1, 43: NOMFICH$ = ENTRADA(8, NOMFICH$, 0, 0, 7)
    COLOR 7, 0
    IF NOMFICH$ <> CHR$(27) THEN
       NOMFICH$ = NOESP(NOMFICH$) + ".TPA"
       OPEN NOMFICH$ FOR RANDOM AS #1
       IF LOF(1) > 0 THEN
	  CLOSE #1
       ELSE
	  CLOSE #1
	  KILL NOMFICH$
	  EXIT SUB
       END IF
       OPEN NOMFICH$ FOR BINARY AS #1
       GET #1, &H1BF, PART(1)
       GET #1, &H1CF, PART(2)
       GET #1, &H1DF, PART(3)
       GET #1, &H1EF, PART(4)
       GET #1, , FIRMA
       CLOSE
       CALL MOSTRPART(PART())
       LOCATE 8, 4: PRINT "FIRMA ... ";
       PRINT HEX$(ASC(FIRMA.HIGH));
       PRINT HEX$(ASC(FIRMA.LOW));
       LOCATE 9, 2: PRINT STRING$(78, "Õ");
       COLOR 0, 7
       LOCATE 10, 2: PRINT " MARCA ≥MARCA  ≥------- INICI --------≥------- FINAL --------≥RELATIU≥NUMERO  "
       LOCATE 11, 2: PRINT " ARRANC≥SISTEMA≥CILINDRE SECTOR CAPÄAL≥CILINDRE SECTOR CAPÄAL≥SECTORS≥SECTORS "
       COLOR 7, 0
       LOCATE 12, 2: PRINT STRING$(78, "ƒ");
       LOCATE 17, 2: PRINT STRING$(78, "ƒ");
EN1:  
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(1).INDICADOR)))
       X$ = X$ + SPACE$(3 - LEN(X$))
       LOCATE 13, 5: X$ = ENTRADA(3, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      SOUND 2000, .05
	      GOTO EN1
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN2:    
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(2).INDICADOR)))
       X$ = X$ + SPACE$(3 - LEN(X$))
       LOCATE 14, 5: X$ = ENTRADA(3, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN1
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN3:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(3).INDICADOR)))
       X$ = X$ + SPACE$(3 - LEN(X$))
       LOCATE 15, 5: X$ = ENTRADA(3, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN2
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN4:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(4).INDICADOR)))
       X$ = X$ + SPACE$(3 - LEN(X$))
       LOCATE 16, 5: X$ = ENTRADA(3, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN3
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT

EN5:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(1).SISTEMA)))
       X$ = X$ + SPACE$(3 - LEN(X$))
       LOCATE 13, 13: X$ = ENTRADA(3, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN4
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN6:  
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(2).SISTEMA)))
       X$ = X$ + SPACE$(3 - LEN(X$))
       LOCATE 14, 13: X$ = ENTRADA(3, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN5
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN7:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(3).SISTEMA)))
       X$ = X$ + SPACE$(3 - LEN(X$))
       LOCATE 15, 13: X$ = ENTRADA(3, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN6
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN8:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(4).SISTEMA)))
       X$ = X$ + SPACE$(3 - LEN(X$))
       LOCATE 16, 13: X$ = ENTRADA(3, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN7
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN9:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(1).INICILIND) + (4 * ((ASC(PART(1).INISECTOR)) AND &HC0))))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 13, 20: PRINT SPACE$(5);
       LOCATE 13, 21: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN8
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN10:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(2).INICILIND) + (4 * ((ASC(PART(2).INISECTOR)) AND &HC0))))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 14, 20: PRINT SPACE$(5);
       LOCATE 14, 21: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN9
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN11:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(3).INICILIND) + (4 * ((ASC(PART(3).INISECTOR)) AND &HC0))))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 15, 20: PRINT SPACE$(5);
       LOCATE 15, 21: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN10
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN12:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(4).INICILIND) + (4 * ((ASC(PART(4).INISECTOR)) AND &HC0))))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 16, 20: PRINT SPACE$(5);
       LOCATE 16, 21: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN11
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN13:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(1).INISECTOR) AND &H3F))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 13, 27: PRINT SPACE$(5);
       LOCATE 13, 28: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN12
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN14:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(2).INISECTOR) AND &H3F))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 14, 27: PRINT SPACE$(5);
       LOCATE 14, 28: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN13
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN15:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(3).INISECTOR) AND &H3F))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 15, 27: PRINT SPACE$(5);
       LOCATE 15, 28: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN14
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN16:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(4).INISECTOR) AND &H3F))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 16, 27: PRINT SPACE$(5);
       LOCATE 16, 28: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN15
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT
EN17:
       CALL MOSTRAVAL(PART())
       X$ = NOESP$(STR$(ASC(PART(1).INICABEZA)))
       X$ = X$ + SPACE$(4 - LEN(X$))
       LOCATE 13, 37: PRINT SPACE$(5);
       LOCATE 13, 38: X$ = ENTRADA(4, X$, 1, 7, 0)
       SELECT CASE X$
	 CASE CHR$(27)
	      GOTO FINAL
	 CASE CHR$(1)
	      GOTO EN15
	 CASE CHR$(4)
	 CASE ELSE
       END SELECT


   '        B4 = ASC(PART(I%).INICABEZA)
   '        B6 = ASC(PART(I%).FINCILIND) + (4 * ((ASC(PART(I%).FINSECTOR)) AND &HC0))
   '        B7 = ASC(PART(I%).FINSECTOR) AND &H3F
   '        B8 = ASC(PART(I%).FINCABEZA)
   '        B9 = CVL(PART(I%).SECRELAT)
   '        B10 = CVL(PART(I%).NUMSECPA)
      
       K$ = INPUT$(1)
    END IF
FINAL:

END SUB

FUNCTION ENTRADA$ (NUMCAR%, INI$, CURSORS%, tinta1%, fons1%)
    y% = POS(0)
    X% = CSRLIN
    PC% = 1
    K$ = ""
    CAR$ = INI$
    COLOR tinta1%, fons1%
    LOCATE X%, y%: PRINT STRING$(NUMCAR%, 32);
    LOCATE X%, y%: PRINT CAR$; STRING$(NUMCAR% - LEN(CAR$), 32);
    COLOR fons1% + 16, tinta1%
    LOCATE X%, y% + PC% - 1
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
	  LOCATE X%, y%: PRINT CAR$; STRING$(NUMCAR% - LEN(CAR$), 32);
	  COLOR fons1% + 16, tinta1%
	  LOCATE X%, y% + PC% - 1
	  IF PC% > LEN(CAR$) THEN
	     IF PC% <= NUMCAR% THEN PRINT CHR$(32);
	  ELSE
	     PRINT MID$(CAR$, PC%, 1);
	  END IF
	  COLOR tinta1%, fons1%
    WEND
    IF K$ = CHR$(27) OR K$ = CHR$(1) OR K$ = CHR$(4) THEN ENTRADA$ = K$ ELSE ENTRADA$ = CAR$
    LOCATE X%, y%: PRINT CAR$ + SPACE$(NUMCAR% - LEN(CAR$))
    EXIT FUNCTION
END FUNCTION

SUB FINESTRA (F1%, C1%, F2%, C2%, TI%, FO%, SO%)
    COLOR TI%, FO%
    LOCATE F1%, C1%: PRINT "⁄"
    LOCATE F1%, C2%: PRINT "ø"
    LOCATE F2%, C1%: PRINT "¿"
    LOCATE F2%, C2%: PRINT "Ÿ"
    LOCATE F1%, C1% + 1: PRINT STRING$(C2% - C1% - 1, "ƒ")
    LOCATE F2%, C1% + 1: PRINT STRING$(C2% - C1% - 1, "ƒ")
    FOR I% = F1% + 1 TO F2% - 1
	LOCATE I%, C1%: PRINT "≥"
	LOCATE I%, C1% + 1: PRINT SPACE$(C2% - C1% - 1)
	LOCATE I%, C2%: PRINT "≥"
    NEXT I%
    IF SO% > 0 THEN
       FOR I% = F1% + 1 TO F2%
	   LOCATE I%, C2% + 1: PRINT CHR$(SO%)
       NEXT I%
       LOCATE F2% + 1, C1% + 1: PRINT STRING$(C2% - C1% + 1, SO%)
    END IF
END SUB

FUNCTION MENU% (TITOL$, TABLA$(), F1%, C1%, F2%, C2%, TI%, FO%)
	 ELE% = UBOUND(TABLA$)
	 COLOR TI%, FO%
	 CALL FINESTRA(F1%, C1%, F2%, C2%, TI%, FO%, 219)
	 COLOR FO%, TI%
	 LOCATE F1% + 1, C1% + 1: PRINT TCENTRE$(TITOL$, C2% - C1% - 1);
	 COLOR TI%, FO%
	 LOCATE F1% + 2, C1%: PRINT "√";
	 LOCATE F1% + 2, C1% + 1: PRINT STRING$(C2% - C1% - 1, "ƒ");
	 LOCATE F1% + 2, C2%: PRINT "¥";
	 FOR I% = 1 TO ELE%
	     LOCATE F1% + 2 + I%, C1% + 1: PRINT TABLA$(I%);
	 NEXT I%
	 LOCATE F1% + ELE% + 3, C1% + 1: PRINT STRING$(C2% - C1% - 1, "ƒ")
	 TEC$ = ""
	 ANT% = 1
	 PUN% = 1
	 DO WHILE (TEC$ <> CHR$(13) AND TEC$ <> CHR$(27))
	    GOSUB VISUALITZA
	    ANT% = PUN%
	    DO
	      TEC$ = INKEY$
	    LOOP WHILE TEC$ = ""
	   'AMUNT.
	    IF TEC$ = CHR$(0) + CHR$(72) OR TEC$ = CHR$(9) THEN
	       PUN% = PUN% - 1: IF PUN% < 1 THEN PUN% = ELE%
	    END IF
	   'AVALL.
	    IF TEC$ = CHR$(0) + CHR$(80) OR TEC$ = CHR$(32) THEN
	       PUN% = PUN% + 1: IF PUN% > ELE% THEN PUN% = 1
	    END IF
	    'NUMERO
	    FOR I% = 1 TO ELE%
		IF VAL(TEC$) = I% THEN
		   PUN% = I%
		   GOSUB VISUALITZA
		   ANT% = PUN%
		   TEC$ = CHR$(13)
		END IF
	    NEXT I%
	    MENU% = PUN%
	    IF TEC$ = CHR$(27) THEN MENU% = 0
	 LOOP
	 EXIT FUNCTION
VISUALITZA:
	COLOR TI%, FO%
	LOCATE F1% + 2 + ANT%, C1% + 1: PRINT TABLA$(ANT%)
	COLOR FO%, TI%
	LOCATE F1% + 2 + PUN%, C1% + 1: PRINT TABLA$(PUN%)
	RETURN
END FUNCTION

SUB MOSTRAVAL (PART() AS BYTES16)
       FOR I% = 1 TO 4
	   B1 = ASC(PART(I%).INDICADOR)
	   B2 = ASC(PART(I%).INICILIND) + (4 * ((ASC(PART(I%).INISECTOR)) AND &HC0))
	   B3 = ASC(PART(I%).INISECTOR) AND &H3F
	   B4 = ASC(PART(I%).INICABEZA)
	   B5 = ASC(PART(I%).SISTEMA)
	   B6 = ASC(PART(I%).FINCILIND) + (4 * ((ASC(PART(I%).FINSECTOR)) AND &HC0))
	   B7 = ASC(PART(I%).FINSECTOR) AND &H3F
	   B8 = ASC(PART(I%).FINCABEZA)
	   B9 = CVL(PART(I%).SECRELAT)
	   B10 = CVL(PART(I%).NUMSECPA)
	   LOCATE I% + 12, 2
	   REM       1       2      3      4      5        6      7      8       9      10
	   F$ = "   ###     ###   ##,### ##,### ##,###   ##,### ##,### ##,### ###,### ###,###"
	   PRINT USING F$; B1; B5; B2; B3; B4; B6; B7; B8; B9; B10
       NEXT I%
END SUB

SUB MOSTRPART (PART() AS BYTES16)
       FOR I% = 1 TO 4
	   B1$ = HEX$(ASC(PART(I%).INDICADOR))
	   IF LEN(B1$) = 1 THEN B1$ = "0" + B1$
	   B2$ = HEX$(ASC(PART(I%).INICILIND))
	   IF LEN(B2$) = 1 THEN B2$ = "0" + B2$
	   B3$ = HEX$(ASC(PART(I%).INISECTOR))
	   IF LEN(B3$) = 1 THEN B3$ = "0" + B3$
	   B4$ = HEX$(ASC(PART(I%).INICABEZA))
	   IF LEN(B4$) = 1 THEN B4$ = "0" + B4$
	   B5$ = HEX$(ASC(PART(I%).SISTEMA))
	   IF LEN(B5$) = 1 THEN B5$ = "0" + B5$
	   B6$ = HEX$(ASC(PART(I%).FINCILIND))
	   IF LEN(B6$) = 1 THEN B6$ = "0" + B6$
	   B7$ = HEX$(ASC(PART(I%).FINSECTOR))
	   IF LEN(B7$) = 1 THEN B7$ = "0" + B7$
	   B8$ = HEX$(ASC(PART(I%).FINCABEZA))
	   IF LEN(B8$) = 1 THEN B8$ = "0" + B8$
	   B9$ = ""
	   FOR B% = 1 TO LEN(PART(I%).SECRELAT)
	       B9$ = B9$ + HEX$(ASC(MID$(PART(I%).SECRELAT, B%, 1)))
	   NEXT B%
	   IF LEN(B9$) < 8 THEN B9$ = STRING$(8 - LEN(B9$), "0") + B9$
	   B10$ = ""
	   FOR B% = 1 TO LEN(PART(I%).NUMSECPA)
	       B10$ = B10$ + HEX$(ASC(MID$(PART(I%).NUMSECPA, B%, 1)))
	   NEXT B%
	   IF LEN(B10$) < 8 THEN B10$ = STRING$(8 - LEN(B10$), "0") + B10$
	   LOCATE I% + 2, 4
	   PRINT B1$; " "; B2$; " "; B3$; " "; B4$; " "; B5$; " "; B6$; " "; B7$; " "; B8$; " "; B9$; " "; B10$
       NEXT I%
END SUB

FUNCTION NOESP$ (CAD$)
    NOESP$ = RTRIM$(LTRIM$(CAD$))
END FUNCTION

SUB OMPLE (F1%, C1%, F2%, C2%, CH%)
    FOR I% = F1% TO F2%
	LOCATE I%, C1%: PRINT STRING$(C2% - C1% + 1, CH%);
    NEXT I%
END SUB

SUB READPART (UNITAT%)
DIM INREG AS REGTYPEX      ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX     ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)         ' SECTOR

CALL FINESTRA(14, 21, 22, 55, 7, 0, 0)
NOMFICH$ = "NONAME  "
COLOR 0, 7
LOCATE 15, 22: PRINT TCENTRE$("LECTURA TAULA PARTICIONS", 33)
COLOR 7, 0
LOCATE 16, 22: PRINT STRING$(33, "ƒ")
LOCATE 17, 23: PRINT "AL FITXER ...         .TPA "
LOCATE 17, 37: NOMFICH$ = ENTRADA(8, NOMFICH$, 0, 7, 0)
IF NOMFICH$ <> CHR$(27) THEN
   NOMFICH$ = NOESP(NOMFICH$) + ".TPA"
   INREG.AX = &H201             ' FUNCIO 2  - LLEGIR 1 SECTOR
   INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
   INREG.DX = UNITAT%           ' CAPÄAL 0, UNITAT 80 (C:)
   INREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
   INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES
   LOCATE 19, 22: PRINT "Llegint sector 1, Unitat "; HEX$(UNITAT%);
   CALL INTERRUPTX(&H13, INREG, OUTREG)
   CALL INTERRUPTX(&H13, INREG, OUTREG)
   LOCATE 21, 22: PRINT "Escribint al fitxer "; NOMFICH$
   OPEN NOMFICH$ FOR RANDOM AS #1 LEN = 2
   FIELD #1, 2 AS BYTE2$
   FOR I% = 1 TO 512
      LSET BYTE2$ = MKI$(SEC%(I%))
      PUT #1, I%
   NEXT I%
   CLOSE #1
END IF
COLOR 0, 7
CALL OMPLE(14, 21, 22, 55, 178)
COLOR 7, 0
END SUB

FUNCTION SELEUNITAT% (UNITAT%)
    DIM T$(1 TO 2)
    T$(1) = " &H80  PRIMER DISC DUR "
    T$(2) = " &H81  SEGON DISC DUR  "
    V% = MENU("UNITAT FISICA", T$(), 15, 35, 20, 59, 0, 7)
    IF V% = 1 THEN UNITAT% = &H80 ELSE UNITAT% = &H81
    SELEUNITAT% = UNITAT%
    COLOR 0, 7
    CALL OMPLE(15, 35, 21, 60, 178)
    COLOR 7, 0
END FUNCTION

FUNCTION TCENTRE$ (TEXTE$, LO%)
      TEXTE$ = NOESP$(TEXTE$)
      IF LEN(TEXTE$) > LO% THEN
	 TEXTE$ = LEFT$(TEXTE$, LO%)
      ELSE
	 S% = (LO% - LEN(TEXTE$)) \ 2
	 TEXTE$ = SPACE$(S%) + TEXTE$ + SPACE$(S%)
	 IF LEN(TEXTE$) < LO% THEN TEXTE$ = TEXTE$ + " "
      END IF
      TCENTRE$ = TEXTE$
END FUNCTION

SUB WRITEPART (UNITAT%)
DIM INREG AS REGTYPEX        ' REGISTRES DE ENTRADA
DIM OUTREG AS REGTYPEX       ' REGISTRES DE SORTIDA
DIM SEC%(1 TO 512)           ' SECTOR

CALL FINESTRA(14, 21, 22, 55, 7, 0, 0)
NOMFICH$ = "NONAME  "
COLOR 0, 7
LOCATE 15, 22: PRINT TCENTRE$("ESCRITURA TAULA PARTICIONS", 33)
COLOR 7, 0
LOCATE 16, 22: PRINT STRING$(33, "ƒ")
LOCATE 17, 23: PRINT "DEL FITXER ..         .TPA "
LOCATE 17, 37: NOMFICH$ = ENTRADA(8, NOMFICH$, 0, 7, 0)
IF NOMFICH$ <> CHR$(27) THEN
   NOMFICH$ = NOESP(NOMFICH$) + ".TPA"
   INREG.AX = &H301             ' FUNCIO 3  - ESCRIURE 1 SECTOR
   INREG.CX = &H1               ' CILINDRE 0, SECTOR 1
   INREG.DX = UNITAT%           ' CAPÄAL 0, UNITAT 80 (C:)
   INREG.ES = VARSEG(SEC%(1))   ' SEGMENT DE DADES
   INREG.BX = VARPTR(SEC%(1))   ' OFFSET DE DADES
   LOCATE 19, 22
   PRINT "Llegint del fitxer " + NOMFICH$
   PUN% = 1
   OPEN NOMFICH$ FOR RANDOM AS #1 LEN = 2
   FIELD #1, 2 AS BYTE2$
   FOR I% = 1 TO 512
     GET #1, I%
     SEC%(I%) = CVI(BYTE2$)
   NEXT I%
   CLOSE #1
   LOCATE 21, 22
   PRINT " Escribint a la Unitat "; HEX$(UNITAT%);
   CALL INTERRUPTX(&H13, INREG, OUTREG)
   CALL INTERRUPTX(&H13, INREG, OUTREG)
   SOUND 3000, 1
   PRINT " Ok."
   K$ = INPUT$(1)
END IF
COLOR 0, 7
CALL OMPLE(14, 21, 22, 55, 178)
COLOR 7, 0

END SUB

