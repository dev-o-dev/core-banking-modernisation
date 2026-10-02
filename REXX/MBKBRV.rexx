/* REXX ***************************************************************/
/* EXEC     : MBKBRV                                                  */
/* TITLE    : BEREAVEMENT INSTRUCTIONS - CONTINGENCY KEYING           */
/* LIBRARY  : MBK.PRD.REXX                                            */
/* INVOKE   : MBKMENU OPTION 6 (BEREAVEMENT SUPPORT TEAM)             */
/*            %MBKBRV LIST     (QUEUED INSTRUCTIONS - VIEW / DELETE)  */
/*            %MBKBRV RELEASE  (MBKMENU OPTION R)                     */
/* PANELS   : MBKBRV1  INSTRUCTION ENTRY                              */
/*            MBKBRV2  QUEUED INSTRUCTIONS                            */
/*            MBKBRV3  RELEASE CONFIRMATION                           */
/*            MBKBRVH  HELP                                           */
/* BATCH    : CFDEC01 / CFDEC010 (CA-7 19:00, BEFORE PSEOD01)         */
/*--------------------------------------------------------------------*/
/* OUTPUT   : <HLQ>.CIF.BEREAVE.IN(+1)  FB 200  (DD BEREAVE)          */
/*            LAYOUT CFDEC010 BRV-INSTRUCTION:                        */
/*  COLS   1      BRV-ACTION       N NOTIFY P PROBATE X SETTLED       */
/*                                 R REVERSAL                         */
/*  COLS   2-13   BRV-CIF-NO                                          */
/*  COLS  14-25   BRV-REF                                             */
/*  COLS  26-33   BRV-DATE-OF-DEATH                                   */
/*  COLS  34-73   BRV-NOTIFIER-NAME                                   */
/*  COLS  74-83   BRV-NOTIFIER-RELATION                               */
/*  COLS  84-89   BRV-EXEC-SORT-CODE                                  */
/*  COLS  90-97   BRV-EXEC-ACCT-NO                                    */
/*  COLS  98-132  BRV-EXEC-NAME                                       */
/*  COLS 133      BRV-INDEMNITY-IND                                   */
/*  COLS 134      BRV-FREEZE-INT-IND                                  */
/*  COLS 135-142  BRV-USER-ID                                         */
/*  COLS 143-200  FILLER                                              */
/*--------------------------------------------------------------------*/
/* INSTRUCTIONS NORMALLY COME FROM THE BEREAVEMENT WORKFLOW TOOL      */
/* (BEREAVEMENT NOTIFICATION SERVICE FEED, CONNECT:DIRECT). THIS      */
/* DIALOG IS THE CONTINGENCY WHEN THE TOOL IS DOWN, AND FOR CASES     */
/* THE TOOL CANNOT HANDLE (JOINT ACCOUNTS WITH A BUSINESS PARTY).     */
/* BOTH WRITE THE SAME GDG - THE TOOL'S FILE ARRIVES AS A GENERATION  */
/* AT 18:30, SO THIS DIALOG ALWAYS MERGES WITH TODAY'S GENERATION IF  */
/* ONE EXISTS (IT CANNOT TELL WHICH GENERATIONS THE TOOL WROTE).      */
/*                                                                    */
/* NO FOUR EYES IN CFDEC010, BUT:                                     */
/*   - THE CIF NUMBER IS KEYED TWICE (WRONG-CUSTOMER NOTIFICATIONS    */
/*     WERE THE MAIN FOS COMPLAINT THEME - 2011 REVIEW)               */
/*   - REVERSAL (R) IS LIMITED TO TEAM LEADERS (LEVEL A)              */
/*   - PROBATE WITH INDEMNITY (SMALL ESTATE, NO GRANT) NEEDS LEVEL A  */
/*     BECAUSE THE TEAM LEADER SIGNS THE INDEMNITY                    */
/* CUT-OFF 18:45 - LATER INSTRUCTIONS WAIT FOR TOMORROW'S RUN.        */
/*--------------------------------------------------------------------*/
/* 2011-02-14 AOS  ORIGINAL                                           */
/* 2013-01-21 PDQ  PENDING QUEUE VIA MBKPEND, RELEASE OPTION          */
/* 2013-09-02 PDQ  FREEZE INTEREST INDICATOR (LOAN PAUSE)             */
/* 2015-06-08 PDQ  SMALL ESTATE WARNING 25,000                        */
/* 2018-11-12 RKS  CIF DOUBLE KEYING (FOS THEME REVIEW)               */
/**********************************************************************/
TRACE O
NUMERIC DIGITS 18
PARSE UPPER ARG MODE .
IF MODE = '' THEN MODE = 'NEW'
FUNC = 'BRV'

IF SYSVAR('SYSISPF') <> 'ACTIVE' THEN DO
   SAY 'MBKBRV - RUN FROM THE MBKMENU DIALOG'
   EXIT 12
END
ADDRESS ISPEXEC
"CONTROL ERRORS RETURN"
"VGET (MBKHLQ MBKBDATE) SHARED"
IF MBKHLQ = '' | MBKBDATE = '' THEN DO
   CALL MSG 'START FROM MBKMENU', 'MBKBRV MUST BE STARTED FROM MBKMENU'
   EXIT 12
END
OUTDSN = MBKHLQ'.CIF.BEREAVE.IN'
USER   = USERID()
CUTOFF = '1845'
SMALL_ESTATE = 25000
RELATIONS = 'SPOUSE PARTNER CHILD PARENT SIBLING EXECUTOR SOLICITOR',
            'FUNERAL OTHER'

LV = MBKAUTH('LEVELS', FUNC)
IF LV = '' THEN DO
   CALL AUDIT 'DENIED', '', 'BEREAVEMENT - NO AUTHORITY'
   CALL MSG 'NOT AUTHORISED', 'YOU ARE NOT AUTHORISED FOR',
      'BEREAVEMENT INSTRUCTIONS.'
   EXIT 8
END
IS_TL = (POS('A', LV) > 0)

SELECT
   WHEN MODE = 'NEW'     THEN CALL NEW_INSTRUCTION
   WHEN MODE = 'LIST'    THEN CALL QUEUE_LIST
   WHEN MODE = 'RELEASE' THEN CALL RELEASE
   OTHERWISE CALL MSG 'INVALID MODE', 'MBKBRV - MODE' MODE
END
EXIT 0

/*====================================================================*/
/* KEY AN INSTRUCTION                                                 */
/*====================================================================*/
NEW_INSTRUCTION:
   IF POS('K', LV) = 0 THEN DO
      CALL MSG 'NOT AUTHORISED', 'YOU MAY NOT KEY INSTRUCTIONS.'
      RETURN
   END
   CALL BLANK_FIELDS
   NOW = SPACE(TRANSLATE(LEFT(TIME('N'),5),' ',':'),0)
   IF NOW >= CUTOFF THEN BVCUT = 'AFTER 18:45 - FOR TOMORROW''S RUN'
   ELSE BVCUT = ''
   DO FOREVER
      "DISPLAY PANEL(MBKBRV1)"
      IF RC >= 8 THEN LEAVE
      IF TRANSLATE(STRIP(ZCMD)) = 'LIST' THEN DO
         ZCMD = ''
         CALL QUEUE_LIST
         ITERATE
      END
      E = VALIDATE_INSTRUCTION()
      IF E <> '' THEN DO
         CALL MSG 'NOT SAVED', E
         ITERATE
      END
      IF BVWARN <> '' & BVACK <> 'Y' THEN DO
         CALL MSG 'PLEASE CONFIRM', BVWARN '- ENTER Y IN CONFIRM.'
         ITERATE
      END
      IF SAVE_INSTRUCTION() = 0 THEN CALL BLANK_FIELDS
   END
   RETURN

BLANK_FIELDS:
   BVACT = ''; BVCIF = ''; BVCIF2 = ''; BVREF = ''; BVDOD = ''
   BVNNAME = ''; BVNREL = ''; BVESORT = ''; BVEACCT = ''
   BVENAME = ''; BVINDEM = 'N'; BVFREEZE = 'N'; BVWARN = ''
   BVACK = ''; ZCMD = ''
   RETURN

VALIDATE_INSTRUCTION:
   BVWARN = ''
   IF WORDPOS(BVACT, 'N P X R') = 0 THEN,
      RETURN 'ACTION MUST BE N, P, X OR R.'
   BVCIF = STRIP(BVCIF); BVCIF2 = STRIP(BVCIF2)
   IF \DATATYPE(BVCIF,'W') | LENGTH(BVCIF) > 12 THEN,
      RETURN 'CIF NUMBER IS UP TO 12 DIGITS.'
   BVCIF = RIGHT(BVCIF, 12, '0')
   IF RIGHT(BVCIF2, 12, '0') <> BVCIF THEN DO
      BVCIF2 = ''
      RETURN 'THE TWO CIF NUMBERS DO NOT MATCH - KEY THEM AGAIN FROM',
         'THE DEATH CERTIFICATE / CUSTOMER RECORD.'
   END
   IF \CIF_CHECK_DIGIT(BVCIF) THEN,
      RETURN 'CIF NUMBER CHECK DIGIT IS WRONG.'
   BVREF = TRANSLATE(STRIP(BVREF))
   SELECT
      WHEN BVACT = 'N' THEN DO
         IF BVREF <> '' THEN RETURN 'LEAVE THE REFERENCE BLANK FOR A',
            'NOTIFICATION - IT IS ALLOCATED.'
         IF \VALID_DATE(BVDOD) THEN,
            RETURN 'DATE OF DEATH MUST BE A VALID CCYYMMDD DATE.'
         IF BVDOD > MBKBDATE THEN,
            RETURN 'DATE OF DEATH CANNOT BE AFTER TODAY.'
         IF STRIP(BVNNAME) = '' THEN RETURN 'NOTIFIER NAME REQUIRED',
            '(THE LETTER IS ADDRESSED TO THEM).'
         BVNREL = TRANSLATE(STRIP(BVNREL))
         IF WORDPOS(BVNREL, RELATIONS) = 0 THEN,
            RETURN 'RELATIONSHIP MUST BE ONE OF' RELATIONS'.'
         IF WORDPOS(BVFREEZE, 'Y N') = 0 THEN,
            RETURN 'FREEZE INTEREST MUST BE Y OR N.'
         IF DATE('B',MBKBDATE,'S') - DATE('B',BVDOD,'S') > 365 THEN,
            BVWARN = 'DATE OF DEATH IS OVER A YEAR AGO'
      END
      WHEN BVACT = 'P' THEN DO
         IF LEFT(BVREF,3) <> 'BRV' THEN,
            RETURN 'KEY THE BEREAVEMENT REF FROM THE NOTIFICATION.'
         IF \DATATYPE(BVESORT,'W') | LENGTH(BVESORT) <> 6 THEN,
            RETURN 'EXECUTOR SORT CODE IS 6 DIGITS.'
         IF \DATATYPE(BVEACCT,'W') | LENGTH(BVEACCT) <> 8 THEN,
            RETURN 'EXECUTOR ACCOUNT NUMBER IS 8 DIGITS.'
         IF STRIP(BVENAME) = '' THEN,
            RETURN 'EXECUTOR / ADMINISTRATOR NAME REQUIRED.'
         IF WORDPOS(BVINDEM, 'Y N') = 0 THEN,
            RETURN 'INDEMNITY MUST BE Y OR N.'
         IF BVINDEM = 'Y' & \IS_TL THEN,
            RETURN 'RELEASE ON INDEMNITY NEEDS A TEAM LEADER (LEVEL A).'
         IF BVINDEM = 'Y' THEN BVWARN = 'SMALL ESTATE - CREDIT',
            'BALANCES MUST TOTAL UNDER' SMALL_ESTATE,
            'OR CFDEC010 REJECTS'
         IF LEFT(BVESORT,2) <> '40' THEN DO
            IF BVWARN = '' THEN BVWARN = 'EXECUTOR ACCOUNT IS AT',
               'ANOTHER BANK - PAY-OUT WILL BE BY CHAPS (KEYED BY',
               'THE TEAM)'
         END
      END
      WHEN BVACT = 'X' THEN DO
         IF LEFT(BVREF,3) <> 'BRV' THEN,
            RETURN 'KEY THE BEREAVEMENT REF FROM THE NOTIFICATION.'
      END
      WHEN BVACT = 'R' THEN DO
         IF \IS_TL THEN,
            RETURN 'REVERSALS ARE LIMITED TO TEAM LEADERS (LEVEL A).'
         IF LEFT(BVREF,3) <> 'BRV' THEN,
            RETURN 'KEY THE BEREAVEMENT REF BEING REVERSED.'
         BVWARN = 'REVERSAL DOES NOT REINSTATE STANDING ORDERS OR',
            'DIRECT DEBITS - PHONE THE CUSTOMER'
      END
   END
   IF QUEUED_FOR(BVCIF, BVACT) THEN,
      RETURN 'AN INSTRUCTION' BVACT 'FOR THIS CIF IS ALREADY QUEUED.'
   RETURN ''

MAKE_CARD:
   C = LEFT(BVACT, 1)          ||,   /*   1      ACTION           */
       RIGHT(BVCIF, 12, '0')   ||,   /*   2-13   CIF-NO           */
       LEFT(BVREF, 12)         ||,   /*  14-25   REF              */
       RIGHT(BVDOD, 8, '0')    ||,   /*  26-33   DATE-OF-DEATH    */
       LEFT(BVNNAME, 40)       ||,   /*  34-73   NOTIFIER-NAME    */
       LEFT(BVNREL, 10)        ||,   /*  74-83   NOTIFIER-REL     */
       LEFT(BVESORT, 6)        ||,   /*  84-89   EXEC-SORT-CODE   */
       LEFT(BVEACCT, 8)        ||,   /*  90-97   EXEC-ACCT-NO     */
       LEFT(BVENAME, 35)       ||,   /*  98-132  EXEC-NAME        */
       LEFT(BVINDEM, 1)        ||,   /* 133      INDEMNITY-IND    */
       LEFT(BVFREEZE, 1)       ||,   /* 134      FREEZE-INT-IND   */
       LEFT(USER, 8)           ||,   /* 135-142  USER-ID          */
       LEFT('', 58)                  /* 143-200  FILLER           */
   RETURN C

SAVE_INSTRUCTION:
   /* NO SECOND USER - STRAIGHT TO THE RELEASE QUEUE (A-MEMBER) */
   MEM = MBKPEND('NEWMEM', FUNC, 'A')
   IF MEM = '' THEN RETURN 8
   IF BVACT = 'N' THEN,
      BVREF = 'BRV'SUBSTR(DATE('S'),3,2)SUBSTR(MEM,2,7)
   IF BVACT <> 'N' THEN BVDOD = 0
   CARD = MAKE_CARD()
   IF LENGTH(CARD) <> 200 THEN DO
      CALL MSG 'INTERNAL ERROR', 'CARD LENGTH' LENGTH(CARD)
      RETURN 8
   END
   HDR = LEFT('*PEND',6)LEFT(FUNC,4)LEFT(BVREF,13)LEFT(USER,9),
      || LEFT(DATE('S'),9)LEFT(SPACE(TRANSLATE(TIME('N'),' ',':'),0),7),
      || LEFT(USER,9)LEFT(BVACT BVCIF, 23)
   QUEUE CARD
   IF MBKPEND('WRITE', FUNC, MEM, HDR) <> 0 THEN RETURN 8
   WTXT = ''
   IF BVWARN <> '' THEN WTXT = 'CONFIRMED:' LEFT(BVWARN, 40)
   CALL AUDIT 'REQUEST', BVREF, 'ACTION' BVACT 'CIF' BVCIF WTXT
   CALL MSG 'SAVED' BVREF, 'INSTRUCTION' BVACT 'FOR CIF' BVCIF,
      'SAVED AS' BVREF'. QUOTE THIS REFERENCE IN THE LETTER LOG.'
   RETURN 0

QUEUED_FOR:
   PARSE ARG QCIF, QACT
   N = MBKPEND('LIST', FUNC, 'A*')
   IF \DATATYPE(N,'W') | N < 1 THEN RETURN 0
   DO I = 1 TO N; PARSE PULL QM.I; END
   DO I = 1 TO N
      C = MBKPEND('READ', FUNC, QM.I)
      IF C < 1 THEN ITERATE
      PULL .
      PARSE PULL D
      DO C - 1; PULL .; END
      IF LEFT(D,13) = QACT || QCIF THEN RETURN 1
   END
   RETURN 0

/*====================================================================*/
/* QUEUED INSTRUCTIONS - VIEW / DELETE BEFORE CUT-OFF                 */
/*====================================================================*/
QUEUE_LIST:
   DO FOREVER
      "TBCREATE BRVQ NOWRITE REPLACE",
         "NAMES(QSEL QMEM QREF QUSER QTIME QACT QCIF QDOD QNAME)"
      N = MBKPEND('LIST', FUNC, 'A*')
      IF \DATATYPE(N,'W') | N < 0 THEN RETURN
      DO I = 1 TO N; PARSE PULL QM.I; END
      DO I = 1 TO N
         C = MBKPEND('READ', FUNC, QM.I)
         IF C < 1 THEN ITERATE
         PARSE PULL H
         PARSE PULL D
         DO C - 1; PULL .; END
         QMEM = QM.I
         PARSE VAR H 11 QREF 23 . 24 QUSER 32 . 42 QTIME 48 .
         PARSE VAR D 1 QACT 2 QCIF 14 . 26 QDOD 34 QNAME 74 .
         IF QACT = 'P' THEN QNAME = SUBSTR(D, 98, 35)
         QTIME = LEFT(QTIME,2)':'SUBSTR(QTIME,3,2)
         QSEL = ''
         "TBADD BRVQ"
      END
      QCOUNT = N
      "TBTOP BRVQ"
      "TBDISPLAY BRVQ PANEL(MBKBRV2)"
      IF RC >= 8 THEN DO
         "TBEND BRVQ"
         RETURN
      END
      DO WHILE ZTDSELS > 0
         IF TRANSLATE(QSEL) = 'D' THEN CALL DELETE_QUEUED
         IF ZTDSELS > 1 THEN "TBDISPLAY BRVQ"
         ELSE ZTDSELS = 0
      END
      "TBEND BRVQ"
   END
   RETURN

DELETE_QUEUED:
   IF STRIP(QUSER) <> USER & \IS_TL THEN DO
      CALL MSG 'NOT YOURS', 'ONLY THE USER WHO KEYED' QREF 'OR A TEAM',
         'LEADER MAY DELETE IT.'
      RETURN
   END
   IF MBKPEND('DELETE', FUNC, QMEM) = 0 THEN DO
      CALL AUDIT 'DELETE', STRIP(QREF), 'QUEUED' QACT 'CIF' QCIF,
         'DELETED BEFORE RELEASE'
      CALL MSG 'DELETED', STRIP(QREF) 'DELETED FROM THE QUEUE.'
   END
   RETURN

/*====================================================================*/
/* RELEASE TO TONIGHT'S CFDEC01                                       */
/*====================================================================*/
RELEASE:
   IF POS('R', LV) = 0 THEN DO
      CALL AUDIT 'DENIED', '', 'RELEASE - NO LEVEL R'
      CALL MSG 'NOT AUTHORISED', 'RELEASE NEEDS LEVEL R.'
      RETURN
   END
   NOW = SPACE(TRANSLATE(LEFT(TIME('N'),5),' ',':'),0)
   IF NOW >= CUTOFF | NOW < '0530' THEN DO
      CALL MSG 'AFTER CUT-OFF', 'CFDEC01 RUNS AT 19:00 - NO RELEASE',
         'AFTER 18:45. THE INSTRUCTIONS WILL GO TOMORROW.'
      RETURN
   END
   N = MBKPEND('LIST', FUNC, 'A*')
   IF \DATATYPE(N,'W') | N < 0 THEN RETURN
   IF N = 0 THEN DO
      CALL MSG 'NOTHING TO RELEASE', 'NO INSTRUCTIONS ARE QUEUED.'
      RETURN
   END
   DO I = 1 TO N; PARSE PULL RM.I; END
   NEWC.0 = 0
   RLN = 0; RLP = 0; RLX = 0; RLR = 0
   DO I = 1 TO N
      C = MBKPEND('READ', FUNC, RM.I)
      IF C < 1 THEN ITERATE
      PULL .
      PARSE PULL D
      DO C - 1; PULL .; END
      K = NEWC.0 + 1
      NEWC.K = LEFT(D, 200); NEWM.K = RM.I; NEWC.0 = K
      INTERPRET 'RL'LEFT(D,1) '= RL'LEFT(D,1) '+ 1'
   END
   RLCOUNT = NEWC.0
   /* MERGE WITH ANY GENERATION ALREADY CREATED TODAY (BY US OR BY  */
   /* THE WORKFLOW TOOL'S 18:30 FEED)                               */
   ALL.0 = 0
   RLMERGE = 'N'
   X = LISTDSI("'"OUTDSN"(0)'" 'NORECALL')
   IF X = 0 THEN DO
      IF JUL2STD(SYSCREATE) = DATE('S') THEN RLMERGE = 'Y'
   END
   RLCONF = ''
   "DISPLAY PANEL(MBKBRV3)"
   IF RC >= 8 | RLCONF <> 'Y' THEN RETURN

   IF RLMERGE = 'Y' THEN DO
      CALL OUTTRAP 'X.'
      ADDRESS TSO "ALLOC F(BRVOLD) DA('"OUTDSN"(0)') OLD REUSE"
      ARC = RC
      CALL OUTTRAP 'OFF'
      IF ARC <> 0 THEN DO
         CALL MSG 'IN USE', OUTDSN'(0) IS IN USE - TRY AGAIN.'
         RETURN
      END
      ADDRESS TSO "EXECIO * DISKR BRVOLD (STEM ALL. FINIS"
      ADDRESS TSO "FREE F(BRVOLD)"
   END
   BASE = ALL.0
   DO I = 1 TO NEWC.0
      K = ALL.0 + 1; ALL.K = NEWC.I; ALL.0 = K
   END
   ADDRESS TSO "ALLOC F(BRVNEW) DA('"OUTDSN"(+1)') NEW CATALOG",
      "UNIT(SYSDA) SPACE(1 1) TRACKS RELEASE",
      "RECFM(F B) LRECL(200) BLKSIZE(0) REUSE"
   IF RC <> 0 THEN DO
      CALL MSG 'ALLOCATION FAILED', 'CANNOT CREATE' OUTDSN'(+1)'
      RETURN
   END
   ADDRESS TSO "EXECIO" ALL.0 "DISKW BRVNEW (STEM ALL. FINIS"
   WRC = RC
   X = LISTDSI('BRVNEW' 'FILE')
   GEN = SYSDSNAME
   ADDRESS TSO "FREE F(BRVNEW)"
   IF WRC <> 0 THEN DO
      CALL AUDIT 'ERROR', '', 'RELEASE WRITE' GEN 'RC='WRC
      CALL MSG 'WRITE FAILED', 'WRITE TO' GEN 'FAILED RC='WRC
      RETURN
   END
   DO I = 1 TO NEWC.0
      CALL MBKPEND 'DELETE', FUNC, NEWM.I
   END
   CALL AUDIT 'RELEASE', RIGHT(GEN,8), 'N='RLN 'P='RLP 'X='RLX,
      'R='RLR '(+'BASE 'ALREADY IN TODAY''S GENERATION)'
   CALL MSG 'RELEASED', NEWC.0 'INSTRUCTION(S) WRITTEN TO' GEN,
      'FOR CFDEC01 AT 19:00.'
   RETURN

/*====================================================================*/
/* UTILITIES                                                          */
/*====================================================================*/
/* CIF NUMBERS CARRY A MODULUS 7 CHECK DIGIT IN THE LAST POSITION   */
/* (CFNUM010 SINCE 1987)                                            */
CIF_CHECK_DIGIT: PROCEDURE
   PARSE ARG CIF
   T = 0
   DO I = 1 TO 11
      T = T + SUBSTR(CIF, I, 1) * (13 - I)
   END
   RETURN (T // 7) = RIGHT(CIF, 1)

JUL2STD: PROCEDURE
   PARSE ARG Y '/' D
   IF \DATATYPE(Y,'W') | \DATATYPE(D,'W') THEN RETURN '00000000'
   RETURN DATE('S', RIGHT(Y,2)RIGHT(D,3,'0'), 'J')

VALID_DATE: PROCEDURE
   PARSE ARG D
   IF \DATATYPE(D,'W') | LENGTH(D) <> 8 THEN RETURN 0
   Y = LEFT(D,4); M = SUBSTR(D,5,2); DD = RIGHT(D,2)
   IF Y < 1900 | M < 1 | M > 12 | DD < 1 THEN RETURN 0
   ML = WORD('31 28 31 30 31 30 31 31 30 31 30 31', M)
   IF M = 2 & (Y // 4 = 0 & (Y // 100 <> 0 | Y // 400 = 0)) THEN,
      ML = 29
   RETURN DD <= ML

AUDIT:
   PARSE ARG AACT, AKEY, ATXT
   CALL MBKAUDIT 'LOG', FUNC, AACT, AKEY, ATXT
   IF RESULT <> 0 THEN,
      CALL MSG 'AUDIT FAILURE', 'THE AUDIT LOG COULD NOT BE WRITTEN',
         '- STOP AND CALL CORE SYSTEMS SUPPORT.'
   RETURN

MSG: PROCEDURE
   PARSE ARG SHORT, LONG
   ZEDSMSG = SHORT
   ZEDLMSG = LONG
   ADDRESS ISPEXEC "SETMSG MSG(ISRZ001)"
   RETURN
