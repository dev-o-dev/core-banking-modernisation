/* REXX ***************************************************************/
/* EXEC     : MBKPRDCH                                                */
/* TITLE    : PRODUCT PARAMETER CHANGE SETS - KEY AND APPROVE         */
/* LIBRARY  : MBK.PRD.REXX                                            */
/* INVOKE   : MBKMENU OPTION 1  (OR %MBKPRDCH APPROVE FROM OPTION A)  */
/* PANELS   : MBKPRD1  CHANGE SET HEADER (PRODUCT, ACTION, DATE)      */
/*            MBKPRD2  FIELD CHANGES (TABLE, UP TO 59 FIELDS)         */
/*            MBKPRD3  CONFIRM / RATE WARNING                         */
/*            MBKPRD4  FIELD NAME LIST (POP-UP)                       */
/*            MBKPRD5  APPROVAL LIST                                  */
/*            MBKPRD6  CHANGE SET REVIEW (CARD IMAGES)                */
/*            MBKREJ1  REJECTION REASON (POP-UP, SHARED)              */
/*            MBKPRDH  HELP                                           */
/* BATCH    : PRMNT01 / PRMNT010  (SKELETON PRMNT01S)                 */
/*--------------------------------------------------------------------*/
/* OUTPUT   : <HLQ>.PRD.CHANGES(+1)  FB 120 - COPYBOOK PRCHGREC       */
/*                                                                    */
/*   COLS   1-12  PCH-CHANGE-REF       COLS  52-81  PCH-NEW-VALUE     */
/*   COLS  13-15  PCH-CARD-SEQ         COLS  82-89  PCH-EFFECTIVE-DATE*/
/*   COLS  16-25  PCH-PROD-CD          COLS  90-97  PCH-REQUESTED-BY  */
/*   COLS     26  PCH-ACTION           COLS  98-105 PCH-APPROVED-BY   */
/*   COLS  27-50  PCH-FIELD-NAME       COLS 106-120 FILLER            */
/*   COLS     51  PCH-OCCURRENCE                                      */
/*--------------------------------------------------------------------*/
/* FLOW                                                               */
/*  1. PRODUCT CONTROL (LEVEL K) KEYS A CHANGE SET. EVERY CARD IS     */
/*     VALIDATED HERE WITH THE SAME RULES AS PRMNT010 2500-APPLY-     */
/*     FIELD SO THE SET IS NOT REJECTED OVERNIGHT FOR A TYPING ERROR. */
/*     THE SET IS SAVED IN <HLQ>.ISPF.PEND.PRD AS MEMBER Pdhhmmss.    */
/*  2. A SECOND USER (LEVEL A, NOT THE REQUESTER) REVIEWS AND         */
/*     APPROVES. THE APPROVER'S ID IS STAMPED ON EVERY CARD AND THE   */
/*     SET IS MERGED INTO A NEW GENERATION OF THE CHANGES GDG         */
/*     TOGETHER WITH ANY CARDS CARRIED FORWARD BY THE LAST RUN (THE   */
/*     CURRENT GENERATION), SORTED BY CHANGE REF + SEQ AS PRMNT010    */
/*     EXPECTS.                                                       */
/*  3. THE APPROVER MAY SUBMIT PRMNT01 (LEVEL S) OR LEAVE THE SET FOR */
/*     THE NEXT MBKEOD RUN.                                           */
/*--------------------------------------------------------------------*/
/* KEEP THE FIELD TABLE BELOW IN STEP WITH PRMNT010 WS-FIELD-VALUES.  */
/* MAXIMUM LENGTHS ARE NOT IN PRMNT010 (IT TRUNCATES SILENTLY) - THEY */
/* ARE THE PRDPARRC PICTURE SIZES.                                    */
/*                                                                    */
/* RATE REDUCTIONS ON SAVINGS PRODUCTS MUST GO THROUGH MBKRTCH SO THE */
/* CUSTOMER NOTICES ARE PRODUCED (PIR 2009/301). THIS DIALOG WARNS    */
/* AND ASKS FOR A REASON IF CR-TIER-RATE OR CR-BONUS-RATE IS KEYED    */
/* ON AN EXISTING PRODUCT.                                            */
/*--------------------------------------------------------------------*/
/* 2007-02-12 JNB  ORIGINAL - REPLACES PAPER FORM PC/17 KEYED BY      */
/*                 OPERATIONS INTO MBK.PRD.PRD.CHANGES                */
/* 2008-09-15 JNB  DORMANCY FIELDS                                    */
/* 2009-10-01 AOS  NOTIFICATION DAYS >= 14 CHECK, RATE WARNING        */
/*                 (PIR 2009/301)                                     */
/* 2010-06-14 JNB  MERGE WITH CARRIED-FORWARD GENERATION - SETS WERE  */
/*                 LOST WHEN A NEW GENERATION WAS CREATED OVER THE    */
/*                 CARRY FORWARD (INC 2010/0412)                      */
/* 2013-01-21 PDQ  PENDING QUEUE VIA MBKPEND, APPROVE FROM OPTION A   */
/* 2017-08-14 RKS  MAXIMUM VALUE LENGTHS                              */
/**********************************************************************/
TRACE O
NUMERIC DIGITS 18
PARSE UPPER ARG MODE .
IF MODE = '' THEN MODE = 'NEW'
FUNC = 'PRD'
PRREF = ''
PRRSN = ''

IF SYSVAR('SYSISPF') <> 'ACTIVE' THEN DO
   SAY 'MBKPRDCH - RUN FROM THE MBKMENU DIALOG'
   EXIT 12
END
ADDRESS ISPEXEC
"CONTROL ERRORS RETURN"
"VGET (MBKHLQ MBKBDATE MBKNDATE) SHARED"
IF MBKHLQ = '' | MBKBDATE = '' THEN DO
   CALL MSG 'START FROM MBKMENU',,
      'MBKPRDCH MUST BE STARTED FROM THE MBKMENU DIALOG (TSO %MBKMENU)'
   EXIT 12
END
CHGDSN = MBKHLQ'.PRD.CHANGES'
USER   = USERID()
MAXCARDS = 60

CALL LOAD_FIELD_TABLE

SELECT
   WHEN MODE = 'NEW'     THEN CALL NEW_CHANGE_SET
   WHEN MODE = 'APPROVE' THEN CALL APPROVE_LIST
   OTHERWISE DO
      CALL MSG 'INVALID MODE', 'MBKPRDCH - MODE' MODE 'NOT KNOWN'
      EXIT 12
   END
END
EXIT 0

/*====================================================================*/
/* KEY A NEW CHANGE SET                                               */
/*====================================================================*/
NEW_CHANGE_SET:
   IF MBKAUTH('CHECK', FUNC, 'K') <> 0 THEN DO
      CALL AUDIT 'DENIED', '', 'KEY CHANGE SET - NO LEVEL K'
      CALL MSG 'NOT AUTHORISED',,
         'YOU ARE NOT AUTHORISED TO KEY PRODUCT CHANGE SETS.'
      RETURN
   END
   PRPROD = ''; PRACT = 'U'; PRNAME = ''; PRDESC = ''
   PREFF  = MBKNDATE
   "VGET (PRPROD) PROFILE"
   PRBDATE = MBKBDATE
   CALL CREATE_FIELD_TABLE 12

   DO FOREVER
      "DISPLAY PANEL(MBKPRD1)"
      IF RC >= 8 THEN DO
         "TBEND PRDFLDS"
         RETURN
      END
      "VPUT (PRPROD) PROFILE"
      /* CROSS-FIELD CHECKS NOT POSSIBLE IN THE PANEL */
      IF \VALID_DATE(PREFF) THEN DO
         CALL MSG 'INVALID DATE', 'EFFECTIVE DATE' PREFF 'IS NOT A',
            'VALID CCYYMMDD DATE.'
         ITERATE
      END
      IF PREFF < MBKBDATE THEN DO
         CALL MSG 'DATE IN THE PAST', 'THE EFFECTIVE DATE MAY NOT BE',
            'BEFORE THE BUSINESS DATE' MBKBDATE'.'
         ITERATE
      END
      IF PRACT = 'W' | PRACT = 'C' THEN DO
         CALL BUILD_STATUS_SET
         IF RESULT = 0 THEN LEAVE
         ITERATE
      END
      /* A OR U - FIELD CHANGES */
      CALL FIELD_CHANGES
      IF RESULT = 0 THEN LEAVE
      IF RESULT = 4 THEN ITERATE  /* BACK TO HEADER */
      "TBEND PRDFLDS"
      RETURN                      /* CANCELLED */
   END
   "TBEND PRDFLDS"

   /* CONFIRM, THEN SAVE TO THE PENDING QUEUE */
   PRCARDS = CARD.0
   PRCONF = ''
   "DISPLAY PANEL(MBKPRD3)"
   IF RC >= 8 | PRCONF <> 'Y' THEN DO
      CALL AUDIT 'CANCEL', PRPROD, 'CHANGE SET NOT SAVED'
      CALL MSG 'NOT SAVED', 'THE CHANGE SET WAS NOT SAVED.'
      RETURN
   END
   CALL SAVE_PENDING
   RETURN

/*--------------------------------------------------------------------*/
/* WITHDRAW / CLOSE TO NEW - ONE CARD, NO FIELDS                      */
/*--------------------------------------------------------------------*/
BUILD_STATUS_SET:
   CARD.0 = 1
   CARD.1 = MAKE_CARD(1, PRACT, '', 0, '')
   PRRATEW = 'N'
   RETURN 0

/*--------------------------------------------------------------------*/
/* FIELD CHANGE TABLE DIALOG - RESULT 0 BUILT, 4 BACK, 8 CANCEL       */
/*--------------------------------------------------------------------*/
FIELD_CHANGES:
   "TBTOP PRDFLDS"
   ZCMD = ''
   DO FOREVER
      ZTDSELS = 0
      "TBDISPLAY PRDFLDS PANEL(MBKPRD2)"
      DRC = RC
      IF DRC >= 8 THEN RETURN 4
      /* SAVE ANY ROWS THE USER OVERTYPED */
      DO WHILE ZTDSELS > 0
         FNAME = TRANSLATE(STRIP(FNAME))
         FOCC  = STRIP(FOCC)
         FMSG  = ''
         "TBPUT PRDFLDS"
         IF ZTDSELS > 1 THEN "TBDISPLAY PRDFLDS"
         ELSE ZTDSELS = 0
      END
      CMD = TRANSLATE(STRIP(ZCMD))
      ZCMD = ''
      SELECT
         WHEN CMD = 'CANCEL' THEN DO
            CALL AUDIT 'CANCEL', PRPROD, 'CHANGE SET ABANDONED'
            RETURN 8
         END
         WHEN CMD = 'MORE' THEN DO
            "TBQUERY PRDFLDS ROWNUM(NR)"
            IF NR + 12 > MAXCARDS - 1 THEN DO
               CALL MSG 'SET TOO LARGE', 'A CHANGE SET MAY HAVE AT',
                  'MOST' MAXCARDS 'CARDS (PRMNT010 WS-SET-TABLE).'
               ITERATE
            END
            CALL ADD_BLANK_ROWS 12
         END
         WHEN CMD = 'FIELDS' THEN CALL SHOW_FIELD_LIST
         WHEN CMD = 'SAVE' | CMD = '' THEN DO
            IF CMD = '' & DRC = 0 THEN ITERATE
            CALL VALIDATE_FIELDS
            IF RESULT = 0 THEN RETURN 0
            "TBTOP PRDFLDS"
         END
         OTHERWISE CALL MSG 'INVALID COMMAND', 'ENTER SAVE, MORE,',
            'FIELDS OR CANCEL (PF3 RETURNS TO THE HEADER).'
      END
   END

/*--------------------------------------------------------------------*/
/* VALIDATE EVERY NON-BLANK ROW AND BUILD THE CARD STEM               */
/*--------------------------------------------------------------------*/
VALIDATE_FIELDS:
   CARD.0 = 0
   ERRS = 0
   SEQ = 0
   SEEN. = 0
   CHANGED. = ''
   PRRATEW = 'N'
   IF PRACT = 'A' THEN DO
      IF STRIP(PRNAME) = '' THEN DO
         CALL MSG 'NAME REQUIRED', 'A NEW PRODUCT NEEDS A SHORT NAME',
            '(ACTION A CARD VALUE).'
         RETURN 8
      END
      SEQ = 1
      CARD.1 = MAKE_CARD(1, 'A', 'PRODUCT', 0, PRNAME)
      CARD.0 = 1
   END
   "TBTOP PRDFLDS"
   DO FOREVER
      "TBSKIP PRDFLDS"
      IF RC <> 0 THEN LEAVE
      IF STRIP(FNAME) = '' & STRIP(FVAL) = '' THEN DO
         IF FMSG <> '' THEN DO; FMSG = ''; "TBPUT PRDFLDS"; END
         ITERATE
      END
      FMSG = CHECK_FIELD(FNAME, FOCC, FVAL)
      IF FMSG = '' THEN DO
         KEYX = FNAME'.'FOCC
         IF SEEN.KEYX THEN FMSG = 'DUPLICATE FIELD IN THIS SET'
         SEEN.KEYX = 1
      END
      "TBPUT PRDFLDS"
      IF FMSG <> '' THEN DO
         ERRS = ERRS + 1
         ITERATE
      END
      SEQ = SEQ + 1
      CARD.SEQ = MAKE_CARD(SEQ, 'U', FNAME, FOCC, NORMVAL)
      CARD.0 = SEQ
      CHANGED.KEYX = NORMVAL
      IF PRACT = 'U' & (FNAME = 'CR-TIER-RATE' |,
         FNAME = 'CR-BONUS-RATE') THEN PRRATEW = 'Y'
   END
   IF ERRS > 0 THEN DO
      CALL MSG ERRS 'ERROR(S)', 'CORRECT THE ROWS MARKED IN THE',
         'MESSAGE COLUMN.'
      RETURN 8
   END
   IF CARD.0 = 0 | (PRACT = 'A' & CARD.0 = 1) THEN DO
      CALL MSG 'NO CHANGES', 'KEY AT LEAST ONE FIELD CHANGE.'
      RETURN 8
   END
   IF CARD.0 > MAXCARDS THEN DO
      CALL MSG 'SET TOO LARGE', 'MAXIMUM' MAXCARDS 'CARDS PER SET.'
      RETURN 8
   END
   /* CROSS-CHECKS PRMNT010 WILL MAKE AFTER APPLYING THE SET - ONLY */
   /* THOSE THAT CAN BE SEEN FROM THE SET ITSELF ARE CHECKED HERE  */
   XMSG = CROSS_CHECK()
   IF XMSG <> '' THEN DO
      CALL MSG 'CROSS-CHECK FAILED', XMSG
      RETURN 8
   END
   RETURN 0

/*--------------------------------------------------------------------*/
/* ONE FIELD - RETURNS '' OR AN ERROR TEXT; SETS NORMVAL              */
/*--------------------------------------------------------------------*/
CHECK_FIELD:
   PARSE ARG CNAME, COCC, CVAL
   CNAME = TRANSLATE(STRIP(CNAME))
   CVAL  = STRIP(CVAL)
   NORMVAL = CVAL
   IX = FT_INDEX.CNAME
   IF IX = 0 THEN RETURN 'UNKNOWN FIELD - ENTER FIELDS FOR A LIST'
   IF FT_OCC.IX > 0 THEN DO
      IF \DATATYPE(COCC,'W') THEN RETURN 'OCCURRENCE 1-'FT_OCC.IX,
         'REQUIRED'
      IF COCC < 1 | COCC > FT_OCC.IX THEN,
         RETURN 'OCCURRENCE MUST BE 1-'FT_OCC.IX
   END
   ELSE DO
      IF COCC <> '' & COCC <> '0' THEN,
         RETURN 'FIELD DOES NOT TAKE AN OCCURRENCE'
      FOCC = 0
   END
   IF CVAL = '' THEN RETURN 'NEW VALUE REQUIRED'
   SELECT
      WHEN POS(FT_TYPE.IX, 'RAN') > 0 THEN DO
         V = SPACE(TRANSLATE(CVAL, ' ', ','), 0)
         IF \DATATYPE(V,'N') | VERIFY(V, '0123456789.') > 0 THEN,
            RETURN 'NOT A VALID NUMBER (NO SIGN, NO E)'
         IF V < 0 THEN RETURN 'NEGATIVE VALUES ARE NOT ALLOWED'
         SELECT
            WHEN FT_TYPE.IX = 'R' THEN DO
               IF V > 99.9999 THEN RETURN 'RATE MUST BE 0.0000-99.9999'
               IF DECS(V) > 4 THEN RETURN 'RATE HAS MAX 4 DECIMALS'
               NORMVAL = FORMAT(V,,4)
            END
            WHEN FT_TYPE.IX = 'A' THEN DO
               IF DECS(V) > 2 THEN RETURN 'AMOUNT HAS MAX 2 DECIMALS'
               IF V > 9999999999999.99 THEN RETURN 'AMOUNT TOO LARGE'
               NORMVAL = FORMAT(V,,2)
            END
            OTHERWISE DO
               IF \DATATYPE(V,'W') THEN RETURN 'WHOLE NUMBER REQUIRED'
               IF LENGTH(V+0) > FT_LEN.IX THEN,
                  RETURN 'MAXIMUM' FT_LEN.IX 'DIGITS'
               NORMVAL = V + 0
            END
         END
      END
      WHEN FT_TYPE.IX = 'C' THEN DO
         CVAL = TRANSLATE(CVAL)
         IF LENGTH(CVAL) <> 1 THEN RETURN 'ONE CHARACTER CODE REQUIRED'
         IF POS(CVAL, FT_CODES.IX) = 0 THEN,
            RETURN 'CODE MUST BE ONE OF' FT_CODES.IX
         NORMVAL = CVAL
      END
      OTHERWISE DO
         IF LENGTH(CVAL) > FT_LEN.IX THEN,
            RETURN 'MAXIMUM' FT_LEN.IX 'CHARACTERS'
         IF LEFT(CNAME,3) = 'GL-' | CNAME = 'COST-CENTRE' THEN DO
            IF \DATATYPE(CVAL,'A') THEN RETURN 'ALPHANUMERIC ONLY'
            CVAL = TRANSLATE(CVAL)
         END
         NORMVAL = CVAL
      END
   END
   RETURN ''

/* VALUE KEYED FOR A FIELD IN THIS SET ('' IF NOT IN THE SET) - THE */
/* FIELD NAMES CONTAIN '-' SO CANNOT BE WRITTEN AS COMPOUND TAILS   */
CHG: PROCEDURE EXPOSE CHANGED.
   PARSE ARG NM, OC
   K = NM'.'OC
   RETURN CHANGED.K

DECS: PROCEDURE
   PARSE ARG N
   IF POS('.', N) = 0 THEN RETURN 0
   RETURN LENGTH(STRIP(SUBSTR(N, POS('.', N) + 1), 'T', '0'))

/*--------------------------------------------------------------------*/
/* CROSS-CHECKS WITHIN THE SET (PRMNT010 2600-CROSS-CHECK)            */
/*--------------------------------------------------------------------*/
CROSS_CHECK:
   ARR = CHG('DR-RATE-ARRANGED',0)
   UNA = CHG('DR-RATE-UNARRANGED',0)
   IF ARR <> '' & UNA <> '' THEN,
      IF UNA < ARR THEN RETURN 'UNARRANGED RATE MUST BE >= ARRANGED',
         'RATE.'
   NDY = CHG('CHG-NOTIFY-DAYS',0)
   IF NDY <> '' THEN IF NDY < 14 THEN,
      RETURN 'CHG-NOTIFY-DAYS MUST BE AT LEAST 14 (PSR 2009).'
   T1 = CHG('CR-TIER-THRESHOLD',1)
   IF T1 <> '' THEN IF T1 <> 0 THEN,
      RETURN 'CR-TIER-THRESHOLD(1) MUST BE ZERO.'
   PREV = -1
   DO T = 1 TO 5
      TV = CHG('CR-TIER-THRESHOLD',T)
      IF TV = '' THEN ITERATE
      IF TV <= PREV & TV <> 0 THEN,
         RETURN 'CREDIT TIER THRESHOLDS MUST ASCEND (TIER' T').'
      PREV = TV
   END
   IF PRACT = 'A' THEN DO
      /* A NEW PRODUCT THAT PAYS OR CHARGES INTEREST NEEDS GL CODES */
      IF CHG('CR-INT-IND',0) = 'Y' & CHG('GL-INT-EXPENSE',0) = '' THEN,
         RETURN 'NEW PRODUCT PAYS INTEREST - GL-INT-EXPENSE REQUIRED.'
      IF CHG('DR-INT-IND',0) = 'Y' & CHG('GL-INT-INCOME',0) = '' THEN,
         RETURN 'NEW PRODUCT CHARGES INTEREST - GL-INT-INCOME REQUIRED.'
      IF CHG('GL-BALANCE-CR',0) = '' THEN,
         RETURN 'NEW PRODUCT - GL-BALANCE-CR REQUIRED.'
   END
   RETURN ''

/*--------------------------------------------------------------------*/
/* BUILD ONE 120-BYTE CARD (PRCHGREC)                                 */
/*--------------------------------------------------------------------*/
MAKE_CARD: PROCEDURE EXPOSE PRREF PRPROD PREFF USER
   PARSE ARG SEQ, ACT, FLD, OCC, VAL
   IF \DATATYPE(OCC,'W') THEN OCC = 0
   C = LEFT(PRREF, 12)          ||,   /*   1-12  CHANGE REF       */
       RIGHT(SEQ, 3, '0')       ||,   /*  13-15  CARD SEQ         */
       LEFT(PRPROD, 10)         ||,   /*  16-25  PRODUCT CODE     */
       LEFT(ACT, 1)             ||,   /*  26     ACTION           */
       LEFT(FLD, 24)            ||,   /*  27-50  FIELD NAME       */
       RIGHT(OCC, 1)            ||,   /*  51     OCCURRENCE       */
       LEFT(VAL, 30)            ||,   /*  52-81  NEW VALUE        */
       RIGHT(PREFF, 8, '0')     ||,   /*  82-89  EFFECTIVE DATE   */
       LEFT(USER, 8)            ||,   /*  90-97  REQUESTED BY     */
       LEFT('', 8)              ||,   /*  98-105 APPROVED BY      */
       LEFT('', 15)                   /* 106-120 FILLER           */
   RETURN C

/*--------------------------------------------------------------------*/
/* SAVE TO THE PENDING QUEUE                                          */
/*--------------------------------------------------------------------*/
SAVE_PENDING:
   MEM = MBKPEND('NEWMEM', FUNC, 'P')
   IF MEM = '' THEN RETURN
   PRREF = 'P'SUBSTR(DATE('S'),3,4)SUBSTR(MEM,2,7)
   /* REBUILD THE CARDS NOW THE REFERENCE IS KNOWN */
   DO I = 1 TO CARD.0
      CARD.I = OVERLAY(LEFT(PRREF,12), CARD.I, 1)
   END
   HDR = LEFT('*PEND',6)LEFT(FUNC,4)LEFT(PRREF,13)LEFT(USER,9),
      || LEFT(DATE('S'),9)LEFT(SPACE(TRANSLATE(TIME('N'),' ',':'),0),7),
      || LEFT('',9)LEFT(PRPROD PRACT PRDESC, 23)
   DO I = 1 TO CARD.0
      QUEUE CARD.I
   END
   IF MBKPEND('WRITE', FUNC, MEM, HDR) <> 0 THEN DO
      CALL AUDIT 'ERROR', PRREF, 'PENDING WRITE FAILED' MEM
      RETURN
   END
   RTXT = ''
   IF PRRATEW = 'Y' THEN RTXT = 'RATE CHG REASON:' PRRSN
   CALL AUDIT 'REQUEST', PRREF, PRPROD 'ACTION' PRACT CARD.0,
      'CARD(S) EFF' PREFF RTXT
   CALL MSG 'SET' PRREF 'SAVED', 'CHANGE SET' PRREF 'FOR' PRPROD,
      '('CARD.0 'CARDS) IS WAITING FOR APPROVAL BY A SECOND USER.'
   RETURN

/*====================================================================*/
/* APPROVAL                                                           */
/*====================================================================*/
APPROVE_LIST:
   LV = MBKAUTH('LEVELS', FUNC)
   IF POS('A', LV) = 0 THEN DO
      CALL AUDIT 'DENIED', '', 'APPROVAL LIST - NO LEVEL A'
      CALL MSG 'NOT AUTHORISED',,
         'YOU ARE NOT AN APPROVER FOR PRODUCT CHANGE SETS.'
      RETURN
   END
   DO FOREVER
      "TBCREATE PRDAPPR NOWRITE REPLACE",
         "NAMES(ASEL AMEM AREF AUSER ADATE ATIME APROD ADESC ACNT)"
      N = MBKPEND('LIST', FUNC, 'P*')
      IF \DATATYPE(N,'W') | N < 0 THEN RETURN
      DO I = 1 TO N
         PARSE PULL M.I
      END
      DO I = 1 TO N
         AMEM = M.I
         C = MBKPEND('READ', FUNC, AMEM)
         IF \DATATYPE(C,'W') | C < 0 THEN ITERATE
         PARSE PULL H
         DO C; PULL .; END
         PARSE VAR H 7 . 11 AREF 23 . 24 AUSER 32 . 33 ADATE 41 .,
            42 ATIME 48 . 58 ADESC
         PARSE VAR ADESC APROD .
         ACNT = C
         ASEL = ''
         "TBADD PRDAPPR"
      END
      "TBSORT PRDAPPR FIELDS(ADATE,C,A,ATIME,C,A)"
      "TBTOP PRDAPPR"
      ACOUNT = N
      "TBDISPLAY PRDAPPR PANEL(MBKPRD5)"
      IF RC >= 8 THEN DO
         "TBEND PRDAPPR"
         RETURN
      END
      DO WHILE ZTDSELS > 0
         SEL = TRANSLATE(ASEL)
         SELECT
            WHEN SEL = 'S' THEN CALL REVIEW_SET AMEM
            WHEN SEL = 'A' THEN CALL APPROVE_SET AMEM
            WHEN SEL = 'R' THEN CALL REJECT_SET AMEM
            OTHERWISE NOP
         END
         IF ZTDSELS > 1 THEN "TBDISPLAY PRDAPPR"
         ELSE ZTDSELS = 0
      END
      "TBEND PRDAPPR"
   END
   RETURN

/*--------------------------------------------------------------------*/
/* REVIEW - SHOW THE CARDS                                            */
/*--------------------------------------------------------------------*/
REVIEW_SET:
   PARSE ARG RMEM
   IF LOAD_SET(RMEM) <> 0 THEN RETURN
   "TBCREATE PRDREVW NOWRITE REPLACE",
      "NAMES(RSEQ RACT RFLD ROCC RVAL REFF)"
   DO I = 1 TO SET.0
      PARSE VALUE SET.I WITH 13 RSEQ 16 . 26 RACT 27 RFLD 51 ROCC 52,
         RVAL 82 REFF 90 .
      IF ROCC = '0' THEN ROCC = ''
      "TBADD PRDREVW"
   END
   "TBTOP PRDREVW"
   "TBDISPLAY PRDREVW PANEL(MBKPRD6)"
   "TBEND PRDREVW"
   RETURN

/*--------------------------------------------------------------------*/
/* APPROVE - FOUR EYES, STAMP, MERGE INTO THE GDG, OFFER SUBMIT       */
/*--------------------------------------------------------------------*/
APPROVE_SET:
   PARSE ARG RMEM
   IF LOAD_SET(RMEM) <> 0 THEN RETURN
   FE = MBKAUTH('FOUREYES', FUNC, SREQ)
   IF FE <> 0 THEN DO
      IF FE = 4 THEN TXT = 'YOU KEYED THIS SET - A DIFFERENT USER',
         'MUST APPROVE IT (FOUR EYES).'
      ELSE TXT = 'YOU ARE NOT AN APPROVER FOR PRODUCT CHANGES.'
      CALL AUDIT 'DENIED', SREF, 'APPROVE - FOUREYES RC' FE
      CALL MSG 'CANNOT APPROVE', TXT
      RETURN
   END
   /* EVERY CARD MUST STILL BELONG TO THIS REQUESTER AND HAVE NO    */
   /* APPROVER - GUARDS AGAINST AN EDITED PENDING MEMBER            */
   DO I = 1 TO SET.0
      IF SUBSTR(SET.I,90,8) <> LEFT(SREQ,8) |,
         SUBSTR(SET.I,98,8) <> '' THEN DO
         CALL AUDIT 'ERROR', SREF, 'PENDING MEMBER' RMEM 'TAMPERED'
         CALL MSG 'SET INVALID', 'CHANGE SET' SREF 'HAS BEEN ALTERED',
            'OUTSIDE THE DIALOG - REJECT IT AND REPORT TO SECURITY.'
         RETURN
      END
      SET.I = OVERLAY(LEFT(USER,8), SET.I, 98)
   END
   IF SEFF < MBKBDATE THEN DO
      CALL MSG 'EFFECTIVE DATE PASSED', 'SET' SREF 'WAS FOR' SEFF,
         'WHICH IS BEFORE THE BUSINESS DATE - REJECT AND REKEY.'
      RETURN
   END

   IF MERGE_INTO_GDG() <> 0 THEN RETURN

   IF MBKPEND('DELETE', FUNC, RMEM) <> 0 THEN,
      CALL AUDIT 'ERROR', SREF, 'APPROVED BUT PENDING MEMBER' RMEM,
         'NOT DELETED - DELETE MANUALLY'
   CALL AUDIT 'APPROVE', SREF, SPROD SET.0 'CARD(S) REQUESTED BY',
      SREQ 'EFF' SEFF 'GEN' NEWGEN
   "TBDELETE PRDAPPR"

   /* NOTIFY THE REQUESTER IF LOGGED ON */
   CALL OUTTRAP 'X.'
   ADDRESS TSO "SEND 'MBK: PRODUCT CHANGE SET" SREF "APPROVED BY",
      USER"' USER("STRIP(SREQ)") LOGON"
   CALL OUTTRAP 'OFF'

   /* OFFER TO SUBMIT PRMNT01 NOW - TODAY-DATED SETS ONLY */
   IF SEFF = MBKBDATE THEN DO
      SCHGDSN = CHGDSN
      SREFS   = SREF
      "VPUT (SCHGDSN SREFS) SHARED"
      SRC = MBKSUBMT('PRMNT01S', FUNC, SREF, 'PRODUCT PARAMETERS')
      IF SRC = 0 THEN RETURN
   END
   CALL MSG 'SET' SREF 'APPROVED', 'CHANGE SET' SREF 'APPROVED AND',
      'ADDED TO' CHGDSN 'GENERATION' NEWGEN'. IT WILL BE APPLIED BY',
      'PRMNT01 ON OR AFTER' SEFF'.'
   RETURN

/*--------------------------------------------------------------------*/
/* REJECT - REASON, DELETE, AUDIT, TELL THE REQUESTER                 */
/*--------------------------------------------------------------------*/
REJECT_SET:
   PARSE ARG RMEM
   IF LOAD_SET(RMEM) <> 0 THEN RETURN
   IF MBKAUTH('CHECK', FUNC, 'A') <> 0 & SREQ <> USER THEN DO
      CALL MSG 'NOT AUTHORISED', 'ONLY AN APPROVER OR THE REQUESTER',
         'MAY REJECT A CHANGE SET.'
      RETURN
   END
   RJKEY = SREF
   RJTEXT = ''
   "ADDPOP ROW(6) COLUMN(10)"
   "DISPLAY PANEL(MBKREJ1)"
   PRC = RC
   "REMPOP"
   IF PRC >= 8 THEN RETURN
   IF MBKPEND('DELETE', FUNC, RMEM) <> 0 THEN RETURN
   CALL AUDIT 'REJECT', SREF, SPROD 'BY' USER':' RJTEXT
   "TBDELETE PRDAPPR"
   IF SREQ <> USER THEN DO
      CALL OUTTRAP 'X.'
      ADDRESS TSO "SEND 'MBK: CHANGE SET" SREF "REJECTED BY" USER,
         "-" LEFT(RJTEXT,40)"' USER("STRIP(SREQ)") LOGON"
      CALL OUTTRAP 'OFF'
   END
   CALL MSG 'SET' SREF 'REJECTED', 'CHANGE SET' SREF 'REJECTED AND',
      'REMOVED FROM THE QUEUE.'
   RETURN

/*--------------------------------------------------------------------*/
/* READ A PENDING SET INTO STEM SET. AND HEADER FIELDS                */
/*--------------------------------------------------------------------*/
LOAD_SET:
   PARSE ARG LMEM
   C = MBKPEND('READ', FUNC, LMEM)
   IF \DATATYPE(C,'W') | C <= 0 THEN DO
      CALL MSG 'SET NOT FOUND', 'PENDING MEMBER' LMEM 'IS EMPTY OR',
         'WAS PROCESSED BY ANOTHER USER.'
      RETURN 8
   END
   PARSE PULL H
   DO I = 1 TO C
      PARSE PULL SET.I
   END
   SET.0 = C
   PARSE VAR H 11 SREF 23 . 24 SREQ 32 .
   SREF = STRIP(SREF); SREQ = STRIP(SREQ)
   SPROD = STRIP(SUBSTR(SET.1, 16, 10))
   SEFF  = SUBSTR(SET.1, 82, 8)
   RETURN 0

/*--------------------------------------------------------------------*/
/* MERGE THE SET INTO A NEW GENERATION OF THE CHANGES GDG             */
/* (0) IS ALLOCATED OLD SO WE FAIL CLEANLY IF PRMNT01 IS RUNNING      */
/*--------------------------------------------------------------------*/
MERGE_INTO_GDG:
   OLD.0 = 0
   CALL OUTTRAP 'X.'
   ADDRESS TSO "ALLOC F(PRDOLD) DA('"CHGDSN"(0)') OLD REUSE"
   ARC = RC
   CALL OUTTRAP 'OFF'
   IF ARC <> 0 THEN DO
      CALL MSG 'CHANGES FILE IN USE', CHGDSN'(0) IS IN USE (PRMNT01',
         'RUNNING?) OR HAS NO GENERATIONS. TRY AGAIN LATER.'
      RETURN 8
   END
   ADDRESS TSO "EXECIO * DISKR PRDOLD (STEM OLD. FINIS"
   IF RC <> 0 THEN DO
      ADDRESS TSO "FREE F(PRDOLD)"
      CALL MSG 'READ ERROR', 'CANNOT READ' CHGDSN'(0) RC='RC
      RETURN 8
   END
   /* A SET MUST NOT ALREADY BE THERE (DOUBLE APPROVAL) */
   DO I = 1 TO OLD.0
      IF LEFT(OLD.I,12) = LEFT(SREF,12) THEN DO
         ADDRESS TSO "FREE F(PRDOLD)"
         CALL MSG 'ALREADY APPROVED', 'SET' SREF 'IS ALREADY IN',
            CHGDSN'(0).'
         RETURN 8
      END
   END
   /* COMBINE AND SORT BY CHANGE REF + SEQ (COLS 1-15) */
   N = 0
   DO I = 1 TO OLD.0
      N = N + 1; ALL.N = LEFT(OLD.I, 120)
   END
   DO I = 1 TO SET.0
      N = N + 1; ALL.N = LEFT(SET.I, 120)
   END
   ALL.0 = N
   CALL SORT_CARDS
   ADDRESS TSO "ALLOC F(PRDNEW) DA('"CHGDSN"(+1)') NEW CATALOG",
      "UNIT(SYSDA) SPACE(1 1) CYLINDERS RELEASE",
      "RECFM(F B) LRECL(120) BLKSIZE(0) REUSE"
   IF RC <> 0 THEN DO
      ADDRESS TSO "FREE F(PRDOLD)"
      CALL MSG 'ALLOCATION FAILED', 'CANNOT CREATE' CHGDSN'(+1)',
         'RC='RC
      RETURN 8
   END
   ADDRESS TSO "EXECIO" ALL.0 "DISKW PRDNEW (STEM ALL. FINIS"
   WRC = RC
   X = LISTDSI('PRDNEW' 'FILE')
   NEWGEN = SYSDSNAME
   ADDRESS TSO "FREE F(PRDNEW)"
   ADDRESS TSO "FREE F(PRDOLD)"
   IF WRC <> 0 THEN DO
      CALL AUDIT 'ERROR', SREF, 'WRITE TO' NEWGEN 'RC='WRC
      CALL MSG 'WRITE FAILED', 'WRITE TO' NEWGEN 'FAILED RC='WRC,
         '- CALL CORE SYSTEMS SUPPORT BEFORE PRMNT01 RUNS.'
      RETURN 8
   END
   NEWGEN = RIGHT(NEWGEN, 8)
   RETURN 0

/* INSERTION SORT - SETS ARE SMALL (A FEW HUNDRED CARDS AT MOST) */
SORT_CARDS:
   DO I = 2 TO ALL.0
      T = ALL.I
      K = LEFT(T, 15)
      J = I - 1
      DO WHILE J > 0
         IF LEFT(ALL.J, 15) <= K THEN LEAVE
         J1 = J + 1
         ALL.J1 = ALL.J
         J = J - 1
      END
      J1 = J + 1
      ALL.J1 = T
   END
   RETURN

/*====================================================================*/
/* TABLES AND UTILITIES                                               */
/*====================================================================*/
CREATE_FIELD_TABLE:
   PARSE ARG ROWS
   "TBCREATE PRDFLDS NOWRITE REPLACE NAMES(FNAME FOCC FVAL FMSG)"
   CALL ADD_BLANK_ROWS ROWS
   RETURN

ADD_BLANK_ROWS:
   PARSE ARG ROWS
   "TBBOTTOM PRDFLDS"
   DO R = 1 TO ROWS
      FNAME = ''; FOCC = ''; FVAL = ''; FMSG = ''
      "TBADD PRDFLDS"
   END
   "TBTOP PRDFLDS"
   RETURN

SHOW_FIELD_LIST:
   "TBCREATE PRDFLIST NOWRITE REPLACE NAMES(LNAME LTYPE LOCC LCODES)"
   DO I = 1 TO FT.0
      LNAME = FT_NAME.I
      LTYPE = WORD('RATE AMOUNT NUMBER CODE TEXT',,
         POS(FT_TYPE.I, 'RANCT'))
      LOCC = ''
      IF FT_OCC.I > 0 THEN LOCC = '1-'FT_OCC.I
      LCODES = FT_CODES.I
      IF FT_TYPE.I = 'T' THEN LCODES = 'MAX' FT_LEN.I 'CHARS'
      "TBADD PRDFLIST"
   END
   "TBTOP PRDFLIST"
   "ADDPOP ROW(2) COLUMN(20)"
   "TBDISPLAY PRDFLIST PANEL(MBKPRD4)"
   "REMPOP"
   "TBEND PRDFLIST"
   RETURN

/*--------------------------------------------------------------------*/
/* FIELD TABLE - NAME TYPE MAXOCC CODES MAXLEN (FROM PRMNT010)        */
/* TYPES R RATE  A AMOUNT  N NUMBER  C CODE  T TEXT                   */
/*--------------------------------------------------------------------*/
LOAD_FIELD_TABLE:
   FT_INDEX. = 0
   FT.0 = 0
   CALL FT 'DESCRIPTION        T 0 -       40'
   CALL FT 'SHORT-NAME         T 0 -       18'
   CALL FT 'STATUS             C 0 ANW      1'
   CALL FT 'CUST-TYPE          C 0 PBA      1'
   CALL FT 'MIN-AGE            N 0 -        3'
   CALL FT 'MAX-AGE            N 0 -        3'
   CALL FT 'MAX-PER-CUST       N 0 -        3'
   CALL FT 'MIN-OPEN-DEPOSIT   A 0 -       15'
   CALL FT 'MAX-BALANCE        A 0 -       15'
   CALL FT 'KYC-REQUIRED       C 0 YN       1'
   CALL FT 'OPEN-BRANCH-IND    C 0 YN       1'
   CALL FT 'OPEN-IBANK-IND     C 0 YN       1'
   CALL FT 'OPEN-TELE-IND      C 0 YN       1'
   CALL FT 'CR-INT-IND         C 0 YN       1'
   CALL FT 'TIER-METHOD        C 0 WB       1'
   CALL FT 'CR-TIER-THRESHOLD  A 5 -       15'
   CALL FT 'CR-TIER-RATE       R 5 -        7'
   CALL FT 'CR-BONUS-RATE      R 0 -        7'
   CALL FT 'CR-BONUS-MONTHS    N 0 -        3'
   CALL FT 'CR-MIN-BAL         A 0 -       15'
   CALL FT 'CR-DAY-BASIS       N 0 -        3'
   CALL FT 'CR-CAP-FREQ        C 0 MQHAT    1'
   CALL FT 'CR-CAP-DAY         N 0 -        2'
   CALL FT 'CR-CAP-MONTH       N 0 -        2'
   CALL FT 'TAX-DEDUCT-IND     C 0 YN       1'
   CALL FT 'DR-INT-IND         C 0 YN       1'
   CALL FT 'DR-RATE-ARRANGED   R 0 -        7'
   CALL FT 'DR-RATE-UNARRANGED R 0 -        7'
   CALL FT 'DR-CAP-FREQ        C 0 MQ       1'
   CALL FT 'DR-CAP-DAY         N 0 -        2'
   CALL FT 'DR-INT-FREE-AMT    A 0 -       15'
   CALL FT 'DR-MIN-CHARGE      A 0 -       15'
   CALL FT 'OD-MAX-LIMIT       A 0 -       15'
   CALL FT 'CHG-IND            C 0 YN       1'
   CALL FT 'CHG-CYCLE-DAY      N 0 -        2'
   CALL FT 'CHG-NOTIFY-DAYS    N 0 -        3'
   CALL FT 'MONTHLY-FEE        A 0 -       15'
   CALL FT 'FREE-BANKING-BAL   A 0 -       15'
   CALL FT 'UNARR-OD-DAILY-FEE A 0 -       15'
   CALL FT 'UNARR-OD-MAX-DAYS  N 0 -        3'
   CALL FT 'UNARR-OD-BUFFER    A 0 -       15'
   CALL FT 'UNPAID-ITEM-FEE    A 0 -       15'
   CALL FT 'PAID-ITEM-FEE      A 0 -       15'
   CALL FT 'MONTHLY-CHG-CAP    A 0 -       15'
   CALL FT 'DORMANT-MONTHS     N 0 -        3'
   CALL FT 'RECLAIM-YEARS      N 0 -        2'
   CALL FT 'DEFAULT-STMT-CYCLE C 0 MQA      1'
   CALL FT 'DEFAULT-STMT-DAY   N 0 -        2'
   CALL FT 'GL-BALANCE-CR      T 0 -       10'
   CALL FT 'GL-BALANCE-DR      T 0 -       10'
   CALL FT 'GL-INT-EXPENSE     T 0 -       10'
   CALL FT 'GL-INT-INCOME      T 0 -       10'
   CALL FT 'GL-FEE-INCOME      T 0 -       10'
   CALL FT 'COST-CENTRE        T 0 -        6'
   RETURN

FT:
   PARSE ARG FN FTY FOC FCD FLN .
   I = FT.0 + 1
   FT.0 = I
   FT_NAME.I  = FN
   FT_TYPE.I  = FTY
   FT_OCC.I   = FOC
   FT_CODES.I = STRIP(FCD, 'B', '-')
   FT_LEN.I   = FLN
   FT_INDEX.FN = I
   RETURN

/*--------------------------------------------------------------------*/
/* CCYYMMDD VALIDITY (NO DATE() CONVERSION - IT RAISES A SYNTAX       */
/* ERROR ON BAD INPUT AND ENDS THE DIALOG)                            */
/*--------------------------------------------------------------------*/
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
