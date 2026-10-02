/* REXX ***************************************************************/
/* EXEC     : MBKAUDIT                                                */
/* TITLE    : ISPF DIALOG AUDIT LOG - WRITE AND BROWSE                */
/* LIBRARY  : MBK.PRD.REXX                                            */
/* PANELS   : MBKAUD1 (BROWSE LIST)  MBKAUD2 (ENTRY DETAIL)           */
/*--------------------------------------------------------------------*/
/* EVERY ACTION TAKEN THROUGH THE MBKMENU DIALOGS IS LOGGED TO        */
/*   <HLQ>.ISPF.AUDITLOG   FB 200, DISP=MOD                           */
/* THE LOG IS ARCHIVED WEEKLY TO <HLQ>.ISPF.AUDITLOG.ARCH(+1) BY JOB  */
/* MBKAUDAR (SUNDAY 06:00) AND KEPT FOR 7 YEARS (FSA RECORD KEEPING). */
/*                                                                    */
/* RECORD LAYOUT                                                      */
/*   COLS   1-8   DATE CCYYMMDD                                       */
/*   COLS  10-17  TIME HH:MM:SS                                       */
/*   COLS  19-26  TSO USER ID                                         */
/*   COLS  28-30  FUNCTION (PRD RT FX CGR GLS BRV AML MNU)            */
/*   COLS  32-39  ACTION   (REQUEST APPROVE REJECT RELEASE SUBMIT     */
/*                          AMEND DELETE DENIED ERROR SIGNON ...)     */
/*   COLS  41-60  KEY      (CHANGE REF, CURRENCY, SUSPENSE ID ...)    */
/*   COLS  62-200 FREE TEXT                                           */
/*                                                                    */
/* CALLS                                                              */
/*   CALL MBKAUDIT 'LOG', func, action, key, text   (RESULT 0/8)      */
/*   CALL MBKAUDIT 'BROWSE' [func]                  (FROM MBKMENU L)  */
/*                                                                    */
/* A LOGGING FAILURE IS NEVER SILENT: THE CALLER IS TOLD (RESULT 8)   */
/* AND IS EXPECTED TO STOP - AN UNLOGGED APPROVAL FAILS IA-08-017.    */
/* THE ALLOCATION IS RETRIED 5 TIMES (THE LOG IS ENQUEUED EXCLUSIVE   */
/* FOR THE DURATION OF EACH WRITE BY DISP=MOD).                       */
/*--------------------------------------------------------------------*/
/* 2007-02-12 JNB  ORIGINAL                                           */
/* 2008-06-23 KLM  RETRY ON ALLOCATION FAILURE                        */
/* 2010-11-29 AOS  BROWSE OPTION FOR AUDIT (IA-10-044)                */
/* 2014-10-06 PDQ  FILTER BY USER / DATE ON THE BROWSE LIST           */
/**********************************************************************/
TRACE O
/* CALLED AS A FUNCTION/SUBROUTINE THE ARGUMENTS ARE SEPARATE; FROM  */
/* THE MENU (SELECT CMD(%MBKAUDIT BROWSE PRD)) THEY ARE ONE STRING.  */
IF ARG() > 1 THEN DO
   REQ = ARG(1); P1 = ARG(2); P2 = ARG(3)
   P3 = ARG(4);  P4 = ARG(5)
END
ELSE DO
   PARSE ARG REQ P1 .
   P2 = ''; P3 = ''; P4 = ''
END
REQ = TRANSLATE(STRIP(REQ))
IF REQ = '' THEN REQ = 'BROWSE'

ISPF = (SYSVAR('SYSISPF') = 'ACTIVE')
MBKHLQ = ''
IF ISPF THEN DO
   ADDRESS ISPEXEC "CONTROL ERRORS RETURN"
   ADDRESS ISPEXEC "VGET (MBKHLQ) SHARED"
END
IF MBKHLQ = '' THEN MBKHLQ = 'MBK.PRD'
LOGDSN = MBKHLQ'.ISPF.AUDITLOG'

SELECT
   WHEN REQ = 'LOG'    THEN RETURN WRITE_LOG(P1, P2, P3, P4)
   WHEN REQ = 'BROWSE' THEN DO
      IF \ISPF THEN DO
         SAY 'MBKAUDIT - BROWSE NEEDS ISPF'
         RETURN 8
      END
      CALL BROWSE_LOG TRANSLATE(STRIP(P1))
      RETURN 0
   END
   OTHERWISE DO
      SAY 'MBKAUDIT - INVALID REQUEST "'REQ'"'
      RETURN 8
   END
END

/*--------------------------------------------------------------------*/
/* WRITE ONE AUDIT RECORD                                             */
/*--------------------------------------------------------------------*/
WRITE_LOG: PROCEDURE EXPOSE LOGDSN ISPF
   PARSE ARG FUNC, ACTION, KEY, TEXT
   REC = LEFT(DATE('S'),8) LEFT(TIME('N'),8) LEFT(USERID(),8),
         LEFT(TRANSLATE(FUNC),3) LEFT(TRANSLATE(ACTION),8),
         LEFT(KEY,20) LEFT(TEXT,139)
   REC = LEFT(REC, 200)
   OUT.1 = REC

   DD = 'MBKAL'RIGHT(TIME('S'),3,'0')
   ARC = 99
   DO TRY = 1 TO 5
      CALL OUTTRAP 'TRAP.'
      ADDRESS TSO "ALLOC F("DD") DA('"LOGDSN"') MOD REUSE"
      ARC = RC
      CALL OUTTRAP 'OFF'
      IF ARC = 0 THEN LEAVE
      /* LOG IN USE BY ANOTHER WRITER - WAIT A MOMENT AND RETRY */
      CALL PAUSE 1
   END
   IF ARC <> 0 THEN DO
      CALL ERRMSG 'AUDIT LOG UNAVAILABLE',,
         'MBKAUDIT - CANNOT ALLOCATE' LOGDSN 'RC='ARC '- ACTION NOT',
         'LOGGED. CONTACT CORE SYSTEMS SUPPORT.'
      RETURN 8
   END
   ADDRESS TSO "EXECIO 1 DISKW "DD" (STEM OUT. FINIS"
   WRC = RC
   ADDRESS TSO "FREE F("DD")"
   IF WRC <> 0 THEN DO
      CALL ERRMSG 'AUDIT WRITE FAILED',,
         'MBKAUDIT - EXECIO WRITE TO' LOGDSN 'RC='WRC
      RETURN 8
   END
   RETURN 0

/*--------------------------------------------------------------------*/
/* BROWSE - LAST 2000 RECORDS, FILTERED, IN AN ISPF TABLE             */
/*--------------------------------------------------------------------*/
BROWSE_LOG:
   PARSE ARG AFUNC
   AUFUNC = AFUNC
   AUUSER = ''
   AUDATE = ''
   AUACT  = ''
   ADDRESS ISPEXEC "VGET (AUUSER AUDATE AUACT) PROFILE"
   IF AFUNC <> '' THEN AUFUNC = AFUNC

   IF SYSDSN("'"LOGDSN"'") <> 'OK' THEN DO
      CALL ERRMSG 'NO AUDIT LOG', 'MBKAUDIT -' LOGDSN 'NOT FOUND'
      RETURN
   END
   DD = 'MBKAB'RIGHT(TIME('S'),3,'0')
   ADDRESS TSO "ALLOC F("DD") DA('"LOGDSN"') SHR REUSE"
   IF RC <> 0 THEN DO
      CALL ERRMSG 'ALLOC FAILED',,
         'MBKAUDIT - CANNOT ALLOCATE' LOGDSN 'RC='RC
      RETURN
   END
   ADDRESS TSO "EXECIO * DISKR "DD" (STEM LOG. FINIS"
   ERC = RC
   ADDRESS TSO "FREE F("DD")"
   IF ERC <> 0 THEN DO
      CALL ERRMSG 'READ FAILED', 'MBKAUDIT - READ' LOGDSN 'RC='ERC
      RETURN
   END

   TABLE = 'MBKAUDT'
   DO FOREVER
      ADDRESS ISPEXEC "TBEND "TABLE
      ADDRESS ISPEXEC "TBCREATE "TABLE" NOWRITE REPLACE",
         "NAMES(AUDT AUTM AUUS AUFN AUAC AUKY AUTX)"
      FIRST = MAX(1, LOG.0 - 1999)
      SHOWN = 0
      /* NEWEST FIRST */
      DO I = LOG.0 TO FIRST BY -1
         PARSE VALUE LOG.I WITH 1 AUDT 9 . 10 AUTM 18 . 19 AUUS 27 .,
            28 AUFN 31 . 32 AUAC 40 . 41 AUKY 61 . 62 AUTX
         AUUS = STRIP(AUUS); AUFN = STRIP(AUFN); AUAC = STRIP(AUAC)
         IF AUFUNC <> '' & AUFN <> AUFUNC THEN ITERATE
         IF AUUSER <> '' & AUUS <> AUUSER THEN ITERATE
         IF AUACT  <> '' & ABBREV(AUAC, AUACT) = 0 THEN ITERATE
         IF AUDATE <> '' & AUDT < AUDATE THEN ITERATE
         AUTX = STRIP(AUTX, 'T')
         ADDRESS ISPEXEC "TBADD "TABLE
         SHOWN = SHOWN + 1
      END
      AUCOUNT = SHOWN
      ADDRESS ISPEXEC "TBTOP "TABLE

      DROP_OUT = 0
      DO FOREVER
         ZTDSELS = 0
         ADDRESS ISPEXEC "TBDISPLAY "TABLE" PANEL(MBKAUD1)"
         DRC = RC
         IF DRC >= 8 THEN DO
            DROP_OUT = 1
            LEAVE
         END
         ADDRESS ISPEXEC "VPUT (AUUSER AUDATE AUACT) PROFILE"
         /* FILTER FIELDS CHANGED - REBUILD THE LIST */
         IF AUREFR = 'Y' THEN LEAVE
         DO WHILE ZTDSELS > 0
            IF TRANSLATE(AUSEL) = 'S' THEN DO
               ADDRESS ISPEXEC "ADDPOP ROW(4) COLUMN(4)"
               ADDRESS ISPEXEC "DISPLAY PANEL(MBKAUD2)"
               ADDRESS ISPEXEC "REMPOP"
            END
            IF ZTDSELS > 1 THEN ADDRESS ISPEXEC "TBDISPLAY "TABLE
            ELSE ZTDSELS = 0
         END
      END
      IF DROP_OUT THEN LEAVE
   END
   ADDRESS ISPEXEC "TBEND "TABLE
   RETURN

/*--------------------------------------------------------------------*/
/* WAIT N SECONDS WITHOUT A WAIT SERVICE (TSO/E HAS NO SLEEP) - THE   */
/* CPU COST OF THIS LOOP WAS AGREED WITH CAPACITY PLANNING AS         */
/* NEGLIGIBLE GIVEN IT ONLY RUNS ON AN ALLOCATION CONFLICT.           */
/*--------------------------------------------------------------------*/
PAUSE: PROCEDURE
   PARSE ARG SECS
   CALL TIME 'R'
   DO WHILE TIME('E') < SECS
      NOP
   END
   RETURN

ERRMSG: PROCEDURE EXPOSE ISPF
   PARSE ARG SHORT, LONG
   IF ISPF THEN DO
      ZEDSMSG = SHORT
      ZEDLMSG = LONG
      ADDRESS ISPEXEC "SETMSG MSG(ISRZ001)"
   END
   ELSE SAY LONG
   RETURN
