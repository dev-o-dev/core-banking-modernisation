/* REXX ***************************************************************/
/* EXEC     : MBKAMLRP                                                */
/* TITLE    : AML TRANSACTION MONITORING - RULE PARAMETER CARDS       */
/* LIBRARY  : MBK.PRD.REXX                                            */
/* INVOKE   : MBKMENU OPTION 7 (FINANCIAL CRIME / MLRO OFFICE)        */
/* PANELS   : MBKAML1  RULE TABLE (DEFAULT / IN FORCE / NEW)          */
/*            MBKAML2  CONFIRM - REASON AND MLRO APPROVAL REFERENCE   */
/*            MBKAMLH  HELP                                           */
/* BATCH    : RGAML01 / RGAML010 (CA-7 MBKEOD, AFTER PSEOD01)         */
/*--------------------------------------------------------------------*/
/* OUTPUT   : <HLQ>.CNTL(AMLRULES)  FB 80  (DD RULEPARM)              */
/*            PREVIOUS VERSION KEPT AS <HLQ>.CNTL(AMLRPREV)           */
/*            LAYOUT RGAML010 RULEPARM-RECORD:                        */
/*  COLS  1-3   RPM-RULE-ID       R01 - R07                           */
/*  COL   4     SPACE                                                 */
/*  COLS  5-14  'THRESHOLD='                                          */
/*  COLS 15-27  RPM-THRESHOLD     9(11)V99 ZONED (10000.00 =          */
/*                                0000001000000)                      */
/*  COL  28     SPACE                                                 */
/*  COLS 29-34  'COUNT='                                              */
/*  COLS 35-37  RPM-COUNT         9(3)                                */
/*  COL  38     SPACE                                                 */
/*  COLS 39-45  'ACTIVE='                                             */
/*  COL  46     RPM-ACTIVE        Y / N                               */
/*  COLS 47-80  FILLER                                                */
/*  '*' IN COLUMN 1 IS A COMMENT (RGAML010 SKIPS IT)                  */
/*--------------------------------------------------------------------*/
/* RGAML010 DEFAULTS ARE COPIED BELOW (WS-RULE-VALUES) SO THE SCREEN  */
/* CAN SHOW WHAT EACH OVERRIDE IS OVERRIDING. KEEP IN STEP.           */
/*                                                                    */
/* CHANGES TO THRESHOLDS ARE A MONEY LAUNDERING REPORTING OFFICER     */
/* DECISION (MLR 2007 REG 20 - POLICIES AND PROCEDURES). ANY USER     */
/* WITH AML AUTHORITY MAY VIEW THE TABLE; ONLY LEVEL M (THE MLRO AND  */
/* NOMINATED DEPUTY) MAY CHANGE IT, AND MUST QUOTE THE FINANCIAL      */
/* CRIME COMMITTEE MINUTE OR MLRO DECISION REFERENCE. SWITCHING A     */
/* RULE OFF, OR RAISING A THRESHOLD BY MORE THAN 50% OF THE DEFAULT,  */
/* NEEDS A SECOND CONFIRMATION.                                       */
/*                                                                    */
/* THE MEMBER IS WRITTEN WITH ISPF LIBRARY SERVICES (LMPUT/LMMREP) SO */
/* THE PDS DIRECTORY IS ENQUEUED CORRECTLY AND THE ISPF STATISTICS    */
/* SHOW WHO LAST CHANGED IT.                                          */
/*--------------------------------------------------------------------*/
/* 2009-09-14 AOS  ORIGINAL (RULEPARM CARDS INTRODUCED)               */
/* 2010-08-16 JNB  RGAML01 JOB - CNTL MEMBER INSTEAD OF INSTREAM      */
/* 2012-04-23 PDQ  PREVIOUS VERSION KEPT AS AMLRPREV (FCA VISIT -     */
/*                 COULD NOT SHOW THRESHOLDS IN FORCE ON A GIVEN DAY) */
/* 2017-06-26 RKS  MLR 2017 - DECISION REFERENCE MANDATORY            */
/**********************************************************************/
TRACE O
NUMERIC DIGITS 18
FUNC = 'AML'

IF SYSVAR('SYSISPF') <> 'ACTIVE' THEN DO
   SAY 'MBKAMLRP - RUN FROM THE MBKMENU DIALOG'
   EXIT 12
END
ADDRESS ISPEXEC
"CONTROL ERRORS RETURN"
"VGET (MBKHLQ MBKBDATE) SHARED"
IF MBKHLQ = '' | MBKBDATE = '' THEN DO
   CALL MSG 'START FROM MBKMENU', 'MBKAMLRP MUST BE STARTED FROM',
      'MBKMENU'
   EXIT 12
END
CNTLDSN = MBKHLQ'.CNTL'
MEMBER  = 'AMLRULES'
PREVMEM = 'AMLRPREV'
USER    = USERID()

LV = MBKAUTH('LEVELS', FUNC)
IF LV = '' THEN DO
   CALL AUDIT 'DENIED', '', 'AML RULES - NO AUTHORITY'
   CALL MSG 'NOT AUTHORISED', 'YOU ARE NOT AUTHORISED TO VIEW THE',
      'AML RULE PARAMETERS.'
   EXIT 8
END
IS_MLRO = (POS('M', LV) > 0)
IF IS_MLRO THEN AMMODE = 'CHANGE'
ELSE AMMODE = 'VIEW ONLY'

CALL LOAD_DEFAULTS
IF READ_CURRENT() <> 0 THEN EXIT 12
CALL AUDIT 'VIEW', MEMBER, 'RULE TABLE DISPLAYED' AMMODE

/*--------------------------------------------------------------------*/
/* RULE TABLE DIALOG                                                  */
/*--------------------------------------------------------------------*/
"TBCREATE AMLRULE NOWRITE REPLACE",
   "NAMES(RID RDESC RSEV DTHR DCNT DACT CTHR CCNT CACT NTHR NCNT",
   "NACT RMSG)"
DO I = 1 TO 7
   RID   = DEF.I.ID
   RDESC = DEF.I.DESC
   RSEV  = DEF.I.SEV
   DTHR  = FMT_AMT(DEF.I.THR)
   DCNT  = DEF.I.CNT
   DACT  = DEF.I.ACT
   CTHR  = FMT_AMT(CUR.I.THR)
   CCNT  = CUR.I.CNT
   CACT  = CUR.I.ACT
   NTHR  = ''; NCNT = ''; NACT = ''; RMSG = ''
   "TBADD AMLRULE"
END
AMUPD = CUR.UPDATED
DO FOREVER
   "TBTOP AMLRULE"
   ZTDSELS = 0
   "TBDISPLAY AMLRULE PANEL(MBKAML1)"
   DRC = RC
   IF DRC >= 8 THEN LEAVE
   DO WHILE ZTDSELS > 0
      RMSG = ''
      "TBPUT AMLRULE"
      IF ZTDSELS > 1 THEN "TBDISPLAY AMLRULE"
      ELSE ZTDSELS = 0
   END
   CMD = TRANSLATE(STRIP(ZCMD)); ZCMD = ''
   SELECT
      WHEN CMD = '' THEN ITERATE
      WHEN CMD = 'CANCEL' THEN LEAVE
      WHEN CMD = 'RESET' THEN CALL RESET_NEW
      WHEN CMD = 'SAVE' THEN DO
         IF \IS_MLRO THEN DO
            CALL AUDIT 'DENIED', MEMBER, 'SAVE ATTEMPTED - NOT MLRO'
            CALL MSG 'NOT AUTHORISED', 'ONLY THE MLRO (LEVEL M) MAY',
               'CHANGE AML RULE PARAMETERS.'
            ITERATE
         END
         IF SAVE_CHANGES() = 0 THEN LEAVE
      END
      WHEN CMD = 'RERUN' THEN DO
         SRULEDSN = CNTLDSN'('MEMBER')'
         "VPUT (SRULEDSN) SHARED"
         CALL MBKSUBMT 'RGAML01S', FUNC, MEMBER, 'AML RULES RERUN'
      END
      OTHERWISE CALL MSG 'INVALID COMMAND', 'ENTER SAVE, RESET,',
         'RERUN OR CANCEL.'
   END
END
"TBEND AMLRULE"
EXIT 0

/*====================================================================*/
/* VALIDATE THE "NEW" COLUMNS AND WRITE THE MEMBER                    */
/*====================================================================*/
SAVE_CHANGES:
   ERRS = 0; CHANGES = 0; RISKY = ''
   "TBTOP AMLRULE"
   DO I = 1 TO 7
      "TBSKIP AMLRULE"
      IF RC <> 0 THEN LEAVE
      RMSG = ''
      NEW.I.THR = UNFMT(CTHR)
      NEW.I.CNT = CCNT
      NEW.I.ACT = CACT
      IF STRIP(NTHR) <> '' THEN DO
         V = SPACE(TRANSLATE(NTHR, ' ', ','), 0)
         IF \DATATYPE(V,'N') | V < 0 | V > 99999999999.99 |,
            DECS(V) > 2 THEN RMSG = 'THRESHOLD INVALID'
         ELSE NEW.I.THR = V
      END
      IF STRIP(NCNT) <> '' & RMSG = '' THEN DO
         IF \DATATYPE(NCNT,'W') | NCNT < 1 | NCNT > 999 THEN,
            RMSG = 'COUNT 1-999'
         ELSE NEW.I.CNT = NCNT + 0
      END
      IF STRIP(NACT) <> '' & RMSG = '' THEN DO
         IF WORDPOS(NACT, 'Y N') = 0 THEN RMSG = 'ACTIVE Y OR N'
         ELSE NEW.I.ACT = NACT
      END
      /* RULE-SPECIFIC SENSE CHECKS */
      IF RMSG = '' THEN DO
         SELECT
            WHEN RID = 'R07' & NEW.I.THR <> 0 THEN,
               RMSG = 'R07 USES COUNT ONLY'
            WHEN RID = 'R01' & NEW.I.CNT <> 1 THEN,
               RMSG = 'R01 COUNT MUST BE 1'
            WHEN RID <> 'R07' & NEW.I.THR = 0 THEN,
               RMSG = 'ZERO WOULD ALERT ON ALL'
            WHEN RID = 'R02' & NEW.I.CNT < 2 THEN,
               RMSG = 'R02 NEEDS COUNT >= 2'
            OTHERWISE NOP
         END
      END
      IF RMSG <> '' THEN ERRS = ERRS + 1
      ELSE DO
         IF NEW.I.THR <> UNFMT(CTHR) | NEW.I.CNT <> CCNT |,
            NEW.I.ACT <> CACT THEN DO
            CHANGES = CHANGES + 1
            RMSG = 'CHANGED'
         END
         IF NEW.I.ACT = 'N' & CACT = 'Y' THEN RISKY = RISKY RID'-OFF'
         IF DEF.I.THR > 0 THEN,
            IF NEW.I.THR > DEF.I.THR * 1.5 THEN RISKY = RISKY RID'-THR'
      END
      "TBPUT AMLRULE"
   END
   /* R02 STRUCTURING IS "BELOW THE R01 THRESHOLD" - KEEP IT BELOW */
   IF ERRS = 0 & NEW.2.THR > NEW.1.THR * NEW.2.CNT THEN DO
      CALL MSG 'R02 INCONSISTENT', 'R02 TOTAL THRESHOLD CANNOT BE',
         'REACHED BY' NEW.2.CNT 'DEPOSITS EACH BELOW THE R01',
         'THRESHOLD.'
      RETURN 8
   END
   IF ERRS > 0 THEN DO
      CALL MSG ERRS 'ERROR(S)', 'CORRECT THE RULES MARKED IN THE',
         'MESSAGE COLUMN.'
      RETURN 8
   END
   IF CHANGES = 0 THEN DO
      CALL MSG 'NO CHANGES', 'NOTHING HAS BEEN CHANGED.'
      RETURN 8
   END

   AMCHG = CHANGES
   AMRISK = STRIP(RISKY)
   AMREF = ''; AMREASON = ''; AMCONF = ''; AMCONF2 = ''
   DO FOREVER
      "DISPLAY PANEL(MBKAML2)"
      IF RC >= 8 THEN RETURN 8
      IF AMCONF <> 'Y' THEN RETURN 8
      IF AMRISK <> '' & AMCONF2 <> 'Y' THEN DO
         CALL MSG 'SECOND CONFIRMATION', 'RULES SWITCHED OFF OR RAISED',
            'OVER 150% OF DEFAULT:' AMRISK '- CONFIRM AGAIN.'
         ITERATE
      END
      LEAVE
   END

   /* BUILD THE NEW MEMBER */
   OUT.0 = 0
   CALL ADDLINE '* AMLRULES - RGAML010 RULE THRESHOLD OVERRIDES'
   CALL ADDLINE '* WRITTEN BY MBKAMLRP (ISPF) - DO NOT EDIT BY HAND'
   CALL ADDLINE '* UPDATED' DATE('S') TIME('N') 'BY' USER
   CALL ADDLINE '* DECISION REF:' LEFT(AMREF, 20)
   CALL ADDLINE '* REASON:' LEFT(AMREASON, 60)
   CALL ADDLINE '* RNN THRESHOLD=NNNNNNNNNNNVV COUNT=NNN ACTIVE=Y'
   DO I = 1 TO 7
      CARD = LEFT(DEF.I.ID, 3)                         ||,  /* 1-3  */
             ' '                                       ||,  /* 4    */
             'THRESHOLD='                              ||,  /* 5-14 */
             RIGHT(TRUNC(NEW.I.THR * 100), 13, '0')    ||,  /*15-27 */
             ' '                                       ||,  /* 28   */
             'COUNT='                                  ||,  /*29-34 */
             RIGHT(NEW.I.CNT, 3, '0')                  ||,  /*35-37 */
             ' '                                       ||,  /* 38   */
             'ACTIVE='                                 ||,  /*39-45 */
             NEW.I.ACT                                      /* 46   */
      CALL ADDLINE CARD
   END
   IF WRITE_MEMBER() <> 0 THEN RETURN 8

   DO I = 1 TO 7
      IF NEW.I.THR <> CUR.I.THR | NEW.I.CNT <> CUR.I.CNT |,
         NEW.I.ACT <> CUR.I.ACT THEN,
         CALL AUDIT 'AMEND', DEF.I.ID 'REF' LEFT(AMREF,12),,
            'THR' FMT_AMT(CUR.I.THR) '->' FMT_AMT(NEW.I.THR),
            'CNT' CUR.I.CNT '->' NEW.I.CNT,
            'ACT' CUR.I.ACT '->' NEW.I.ACT
   END
   CALL AUDIT 'AMEND', MEMBER, CHANGES 'RULE(S) CHANGED. REASON:',
      LEFT(AMREASON, 80)
   CALL MSG 'RULES SAVED', CHANGES 'RULE(S) CHANGED. THEY APPLY FROM',
      'TONIGHT''S RGAML01. PREVIOUS VERSION KEPT AS' PREVMEM'.'
   RETURN 0

ADDLINE:
   PARSE ARG L
   K = OUT.0 + 1
   OUT.K = LEFT(L, 80)
   OUT.0 = K
   RETURN

RESET_NEW:
   "TBTOP AMLRULE"
   DO FOREVER
      "TBSKIP AMLRULE"
      IF RC <> 0 THEN LEAVE
      NTHR = ''; NCNT = ''; NACT = ''; RMSG = ''
      "TBPUT AMLRULE"
   END
   RETURN

/*====================================================================*/
/* READ THE MEMBER IN FORCE (DEFAULTS IF IT IS MISSING OR EMPTY)      */
/*====================================================================*/
READ_CURRENT:
   DO I = 1 TO 7
      CUR.I.THR = DEF.I.THR
      CUR.I.CNT = DEF.I.CNT
      CUR.I.ACT = DEF.I.ACT
   END
   CUR.UPDATED = 'DEFAULTS (NO OVERRIDES)'
   IF SYSDSN("'"CNTLDSN"("MEMBER")'") <> 'OK' THEN RETURN 0
   ADDRESS TSO "ALLOC F(AMLIN) DA('"CNTLDSN"("MEMBER")') SHR REUSE"
   IF RC <> 0 THEN DO
      CALL MSG 'ALLOC FAILED', 'CANNOT ALLOCATE' CNTLDSN'('MEMBER')'
      RETURN 8
   END
   ADDRESS TSO "EXECIO * DISKR AMLIN (STEM IN. FINIS"
   ERC = RC
   ADDRESS TSO "FREE F(AMLIN)"
   IF ERC <> 0 THEN DO
      CALL MSG 'READ FAILED', 'CANNOT READ' MEMBER 'RC='ERC
      RETURN 8
   END
   DO J = 1 TO IN.0
      L = IN.J
      IF LEFT(L,1) = '*' THEN DO
         IF POS('UPDATED', L) > 0 THEN,
            CUR.UPDATED = STRIP(SUBSTR(L, POS('UPDATED', L) + 8))
         ITERATE
      END
      ID = LEFT(L, 3)
      I = WHICH_RULE(ID)
      IF I = 0 THEN ITERATE
      T = SUBSTR(L, 15, 13); C = SUBSTR(L, 35, 3); A = SUBSTR(L, 46, 1)
      /* SAME RULES AS RGAML010 2000-LOAD-RULES: A FIELD THAT IS    */
      /* NOT NUMERIC / NOT Y-N IS IGNORED AND THE DEFAULT STANDS    */
      IF DATATYPE(T,'W') THEN CUR.I.THR = T / 100
      IF DATATYPE(C,'W') THEN CUR.I.CNT = C + 0
      IF A = 'Y' | A = 'N' THEN CUR.I.ACT = A
   END
   RETURN 0

WHICH_RULE: PROCEDURE EXPOSE DEF.
   PARSE ARG ID
   DO I = 1 TO 7
      IF DEF.I.ID = ID THEN RETURN I
   END
   RETURN 0

/*====================================================================*/
/* WRITE THE MEMBER WITH LM SERVICES, KEEPING THE OLD ONE AS PREVMEM  */
/*====================================================================*/
WRITE_MEMBER:
   "LMINIT DATAID(AMLID) DATASET('"CNTLDSN"') ENQ(SHRW)"
   IF RC <> 0 THEN DO
      CALL MSG 'LMINIT FAILED', 'LMINIT' CNTLDSN 'RC='RC
      RETURN 8
   END
   /* KEEP THE CURRENT VERSION: DELETE OLD PREV, RENAME CURRENT */
   IF SYSDSN("'"CNTLDSN"("MEMBER")'") = 'OK' THEN DO
      "LMMDEL DATAID("AMLID") MEMBER("PREVMEM")"
      "LMMREN DATAID("AMLID") MEMBER("MEMBER") NEWNAME("PREVMEM")"
      IF RC <> 0 THEN DO
         "LMFREE DATAID("AMLID")"
         CALL MSG 'BACKUP FAILED', 'COULD NOT RENAME' MEMBER 'TO',
            PREVMEM '(RC='RC') - NOTHING CHANGED.'
         RETURN 8
      END
   END
   "LMOPEN DATAID("AMLID") OPTION(OUTPUT)"
   IF RC <> 0 THEN DO
      ORC = RC
      CALL RESTORE_PREV
      CALL MSG 'IN USE', CNTLDSN 'IS IN USE (RC='ORC') - TRY AGAIN.'
      RETURN 8
   END
   DO I = 1 TO OUT.0
      REC = OUT.I
      "LMPUT DATAID("AMLID") MODE(INVAR) DATALOC(REC) DATALEN(80)"
      IF RC <> 0 THEN LEAVE
   END
   PRC = RC
   IF PRC = 0 THEN "LMMADD DATAID("AMLID") MEMBER("MEMBER") STATS(YES)"
   ARC = RC
   "LMCLOSE DATAID("AMLID")"
   IF PRC <> 0 | ARC <> 0 THEN DO
      CALL RESTORE_PREV
      CALL AUDIT 'ERROR', MEMBER, 'WRITE FAILED LMPUT' PRC 'LMMADD' ARC
      CALL MSG 'WRITE FAILED', 'THE NEW MEMBER COULD NOT BE WRITTEN',
         '- THE PREVIOUS VERSION HAS BEEN RESTORED.'
      RETURN 8
   END
   "LMFREE DATAID("AMLID")"
   RETURN 0

RESTORE_PREV:
   "LMMREN DATAID("AMLID") MEMBER("PREVMEM") NEWNAME("MEMBER")"
   "LMFREE DATAID("AMLID")"
   RETURN

/*====================================================================*/
/* RGAML010 WS-RULE-VALUES                                            */
/*====================================================================*/
LOAD_DEFAULTS:
   CALL DEF 1, 'R01', 10000.00, 1,  'Y', 'H',,
      'LARGE CASH DEPOSIT'
   CALL DEF 2, 'R02',  9000.00, 3,  'Y', 'H',,
      'POSSIBLE STRUCTURING OF CASH'
   CALL DEF 3, 'R03',  5000.00, 1,  'Y', 'M',,
      'RAPID MOVEMENT OF FUNDS'
   CALL DEF 4, 'R04',  2000.00, 1,  'Y', 'M',,
      'HIGH RISK CUSTOMER ACTIVITY'
   CALL DEF 5, 'R05',  1000.00, 1,  'Y', 'M',,
      'DEBITS AFTER DORMANT REACTIVATE'
   CALL DEF 6, 'R06', 15000.00, 1,  'Y', 'L',,
      'HIGH VALUE OVERSEAS PAYMENTS'
   CALL DEF 7, 'R07',        0, 20, 'Y', 'L',,
      'HIGH VELOCITY FPS PAYMENTS OUT'
   RETURN

DEF:
   PARSE ARG N, ID, THR, CNT, ACT, SEV, DESC
   DEF.N.ID = ID; DEF.N.THR = THR; DEF.N.CNT = CNT
   DEF.N.ACT = ACT; DEF.N.SEV = SEV; DEF.N.DESC = DESC
   RETURN

/*====================================================================*/
/* UTILITIES                                                          */
/*====================================================================*/
FMT_AMT: PROCEDURE
   PARSE ARG A
   IF \DATATYPE(A,'N') THEN RETURN A
   A = FORMAT(A,,2)
   PARSE VAR A W '.' D
   OUT = ''
   DO WHILE LENGTH(W) > 3
      OUT = ','RIGHT(W,3) || OUT
      W = LEFT(W, LENGTH(W) - 3)
   END
   RETURN W || OUT'.'D

UNFMT: PROCEDURE
   PARSE ARG A
   RETURN SPACE(TRANSLATE(A, ' ', ','), 0) + 0

DECS: PROCEDURE
   PARSE ARG N
   IF POS('.', N) = 0 THEN RETURN 0
   RETURN LENGTH(STRIP(SUBSTR(N, POS('.', N) + 1), 'T', '0'))

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
