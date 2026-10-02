/* REXX ***************************************************************/
/* EXEC     : MBKDTRX                                                 */
/* TITLE    : BUSINESS DATE ROLL                                      */
/* LIBRARY  : MBK.PRD.REXX                                            */
/* JCL      : MBKDTROL                                                */
/*--------------------------------------------------------------------*/
/* READS DD DATECARD (MBK.PRD.CNTL(BUSDATE)) AND DD BANKHOL, AND      */
/* REWRITES THE DATE CARD SO THAT:                                    */
/*   NEW BUSDATE  = OLD NEXTDATE                                      */
/*   NEW PREVDATE = OLD BUSDATE                                       */
/*   NEW NEXTDATE = FIRST BUSINESS DAY AFTER NEW BUSDATE              */
/*   NEW RUN      = MMDD OF NEW BUSDATE + 'A'                         */
/* RC 0 OK, RC 8 DATE CARD INVALID, RC 12 I/O ERROR                   */
/*--------------------------------------------------------------------*/
/* THE HOLIDAY LIST IS A SECOND COPY OF THE TABLE IN DTEUTIL -        */
/* BOTH MUST BE UPDATED (PIR 2006/102).                               */
/*--------------------------------------------------------------------*/
/* 1999-01-11 MLW  ORIGINAL (REPLACES MANUAL DATE CARD UPDATE)        */
/* 2003-07-21 SAT  RUN ID                                             */
/**********************************************************************/
TRACE O

"EXECIO 1 DISKR DATECARD (STEM CARD. FINIS"
IF RC <> 0 THEN DO
   SAY 'MBKDTRX - UNABLE TO READ DATECARD, RC='RC
   EXIT 12
END

PARSE VAR CARD.1 'BUSDATE=' OLDBUS ' PREVDATE=' OLDPREV ,
                 ' NEXTDATE=' OLDNEXT ' RUN=' OLDRUN .

IF DATATYPE(OLDBUS,'W') = 0 | DATATYPE(OLDNEXT,'W') = 0 THEN DO
   SAY 'MBKDTRX - DATE CARD INVALID: ' CARD.1
   EXIT 8
END

"EXECIO * DISKR BANKHOL (STEM HOL. FINIS"
IF RC <> 0 THEN DO
   SAY 'MBKDTRX - UNABLE TO READ BANKHOL, RC='RC
   EXIT 12
END
HOLIDAYS = ''
DO I = 1 TO HOL.0
   IF LEFT(HOL.I,1) = '*' THEN ITERATE
   HOLIDAYS = HOLIDAYS WORD(HOL.I,1)
END

NEWBUS  = OLDNEXT
NEWPREV = OLDBUS
NEWNEXT = NEXT_BUSINESS_DAY(NEWBUS)
NEWRUN  = SUBSTR(NEWBUS,5,4)'A'

NEWCARD = 'BUSDATE='NEWBUS' PREVDATE='NEWPREV ,
          'NEXTDATE='NEWNEXT' RUN='NEWRUN
CARD.1  = LEFT(NEWCARD,80)

"EXECIO 1 DISKW DATECARD (STEM CARD. FINIS"
IF RC <> 0 THEN DO
   SAY 'MBKDTRX - UNABLE TO WRITE DATECARD, RC='RC
   EXIT 12
END

SAY 'MBKDTRX - OLD: BUSDATE='OLDBUS' NEXTDATE='OLDNEXT
SAY 'MBKDTRX - NEW: 'NEWCARD
EXIT 0

/*--------------------------------------------------------------------*/
/* FIRST MONDAY-FRIDAY NOT IN THE HOLIDAY LIST AFTER THE GIVEN DATE   */
/*--------------------------------------------------------------------*/
NEXT_BUSINESS_DAY: PROCEDURE EXPOSE HOLIDAYS
   ARG D
   BASE = DATE('B', D, 'S')
   DO FOREVER
      BASE = BASE + 1
      CAND = DATE('S', BASE, 'B')
      DOW  = DATE('W', BASE, 'B')
      IF DOW = 'Saturday' | DOW = 'Sunday' THEN ITERATE
      IF WORDPOS(CAND, HOLIDAYS) > 0 THEN ITERATE
      RETURN CAND
   END
