/* REXX ***************************************************************/
/* EXEC     : MBKAUTH                                                 */
/* TITLE    : ISPF DIALOG AUTHORITY AND FOUR-EYES CHECKS              */
/* LIBRARY  : MBK.PRD.REXX  (SYSEXEC CONCATENATION FOR MBKMENU)       */
/* CALLED BY: MBKMENU MBKPRDCH MBKRTCH FXMNT MBKCGREF MBKGLSUS        */
/*            MBKBRV MBKAMLRP                                         */
/*--------------------------------------------------------------------*/
/* EXTERNAL FUNCTION. READS THE DIALOG AUTHORITY TABLE                */
/*   <HLQ>.ISPF.AUTHTAB  (FB 80, ONE LINE PER USER / FUNCTION)        */
/*                                                                    */
/*   COLS  1-8   USER ID                                              */
/*   COLS 10-12  FUNCTION  PRD RT  FX  CGR GLS BRV AML ADM            */
/*   COLS 14-20  LEVELS    K  KEY / REQUEST                           */
/*                         A  APPROVE (FOUR EYES SECOND USER)         */
/*                         2  LEVEL 2 APPROVE (CHARGE REFUNDS)        */
/*                         M  MLRO (AML RULE THRESHOLDS)              */
/*                         R  RELEASE TO BATCH (CUT-OFF)              */
/*                         S  SUBMIT JOBS                             */
/*   COLS 22-29  EXPIRY CCYYMMDD (BLANK = NO EXPIRY)                  */
/*   COLS 31-80  COMMENT (WHO AUTHORISED THE ENTRY - FORM SEC/09)     */
/*   '*' IN COLUMN 1 IS A COMMENT.                                    */
/*                                                                    */
/* CALLS                                                              */
/*   MBKAUTH('CHECK',func,levels[,user])                              */
/*        0  USER HAS ALL OF THE LEVELS FOR THE FUNCTION              */
/*        4  NOT AUTHORISED                                           */
/*        8  TABLE MISSING / UNREADABLE                               */
/*   MBKAUTH('LEVELS',func[,user])     -> STRING OF LEVELS, OR ''     */
/*   MBKAUTH('FUNCS'[,user])           -> FUNCTIONS USER MAY USE      */
/*   MBKAUTH('FOUREYES',func,requester[,user])                        */
/*        0  USER IS AN APPROVER AND IS NOT THE REQUESTER             */
/*        4  USER IS THE REQUESTER                                    */
/*        6  USER IS NOT AN APPROVER FOR THE FUNCTION                 */
/*        8  TABLE ERROR                                              */
/*   MBKAUTH('REFRESH')                -> DROPS THE CACHED TABLE      */
/*                                                                    */
/* THE TABLE IS CACHED IN THE ISPF SHARED POOL (MBKAUTB1-MBKAUTBN)    */
/* FOR THE LIFE OF THE DIALOG SO IT IS ONLY READ ONCE PER SESSION.    */
/*--------------------------------------------------------------------*/
/* THE PROPER ANSWER IS RACF FACILITY PROFILES MBK.ISPF.<FUNC>.<LVL>  */
/* (PIR 2012/044) - NOT FUNDED. UNTIL THEN SECURITY ADMIN MAINTAIN    */
/* THE TABLE AND IT IS RECONCILED QUARTERLY (AUDIT POINT IA-11-031).  */
/* THE TABLE ITSELF IS RACF PROTECTED - READ FOR ALL DIALOG USERS,    */
/* UPDATE FOR SECADM ONLY.                                            */
/*--------------------------------------------------------------------*/
/* 2007-02-12 JNB  ORIGINAL (FOUR EYES FOR PRODUCT CHANGE SETS)       */
/* 2008-04-07 JNB  LEVEL 2 FOR CHARGE REFUNDS                         */
/* 2008-06-23 KLM  GLS FUNCTION                                       */
/* 2009-09-14 AOS  AML / MLRO LEVEL                                   */
/* 2011-02-14 AOS  BRV FUNCTION                                       */
/* 2011-05-09 JNB  FX FUNCTION                                        */
/* 2012-03-19 PDQ  EXPIRY DATE COLUMN (TEMPORARY COVER)               */
/* 2014-10-06 PDQ  CACHE IN SHARED POOL - TABLE WAS READ ON EVERY     */
/*                 KEYSTROKE OF THE APPROVAL LIST                     */
/**********************************************************************/
TRACE O
PARSE ARG REQ, P1, P2, P3
REQ = TRANSLATE(STRIP(REQ))
P1  = TRANSLATE(STRIP(P1))
P2  = TRANSLATE(STRIP(P2))
P3  = TRANSLATE(STRIP(P3))

VALID_FUNCS  = 'PRD RT FX CGR GLS BRV AML ADM'
VALID_LEVELS = 'KA2MRS'

TODAY = DATE('S')

ISPF = (SYSVAR('SYSISPF') = 'ACTIVE')
IF ISPF THEN DO
   ADDRESS ISPEXEC "CONTROL ERRORS RETURN"
   ADDRESS ISPEXEC "VGET (MBKHLQ) SHARED"
   IF RC <> 0 | MBKHLQ = '' THEN MBKHLQ = 'MBK.PRD'
END
ELSE MBKHLQ = 'MBK.PRD'

AUTHDSN = MBKHLQ'.ISPF.AUTHTAB'

SELECT
   WHEN REQ = 'REFRESH' THEN DO
      CALL DROP_CACHE
      RETURN 0
   END
   WHEN REQ = 'CHECK' THEN DO
      IF WORDPOS(P1, VALID_FUNCS) = 0 THEN RETURN 8
      IF P2 = '' THEN RETURN 8
      IF VERIFY(P2, VALID_LEVELS) > 0 THEN RETURN 8
      USER = P3
      IF USER = '' THEN USER = USERID()
      IF LOAD_TABLE() <> 0 THEN RETURN 8
      HAVE = USER_LEVELS(USER, P1)
      DO I = 1 TO LENGTH(P2)
         IF POS(SUBSTR(P2,I,1), HAVE) = 0 THEN RETURN 4
      END
      RETURN 0
   END
   WHEN REQ = 'LEVELS' THEN DO
      IF WORDPOS(P1, VALID_FUNCS) = 0 THEN RETURN ''
      USER = P2
      IF USER = '' THEN USER = USERID()
      IF LOAD_TABLE() <> 0 THEN RETURN ''
      RETURN USER_LEVELS(USER, P1)
   END
   WHEN REQ = 'FUNCS' THEN DO
      USER = P1
      IF USER = '' THEN USER = USERID()
      IF LOAD_TABLE() <> 0 THEN RETURN ''
      OUT = ''
      DO F = 1 TO WORDS(VALID_FUNCS)
         FN = WORD(VALID_FUNCS, F)
         IF USER_LEVELS(USER, FN) <> '' THEN OUT = OUT FN
      END
      RETURN STRIP(OUT)
   END
   WHEN REQ = 'FOUREYES' THEN DO
      IF WORDPOS(P1, VALID_FUNCS) = 0 THEN RETURN 8
      REQUESTER = P2
      USER = P3
      IF USER = '' THEN USER = USERID()
      IF REQUESTER = '' THEN RETURN 8
      IF USER = REQUESTER THEN RETURN 4
      IF LOAD_TABLE() <> 0 THEN RETURN 8
      HAVE = USER_LEVELS(USER, P1)
      IF POS('A', HAVE) = 0 & POS('2', HAVE) = 0 &,
         POS('M', HAVE) = 0 THEN RETURN 6
      RETURN 0
   END
   OTHERWISE DO
      CALL ERRMSG 'MBKAUTH - INVALID REQUEST "'REQ'"'
      RETURN 8
   END
END
RETURN 8

/*--------------------------------------------------------------------*/
/* LOAD THE TABLE - FROM THE SHARED POOL IF ALREADY CACHED, ELSE      */
/* FROM THE DATASET. TABLE LINES GO INTO STEM AT. AS                  */
/*    'USERID FUNC LEVELS EXPIRY'                                     */
/*--------------------------------------------------------------------*/
LOAD_TABLE:
   AT.0 = 0
   IF ISPF THEN DO
      ADDRESS ISPEXEC "VGET (MBKAUTBN) SHARED"
      IF RC = 0 & DATATYPE(MBKAUTBN,'W') & MBKAUTBN > 0 THEN DO
         DO K = 1 TO MBKAUTBN
            VN = 'MBKAUT'RIGHT(K,2,'0')
            ADDRESS ISPEXEC "VGET ("VN") SHARED"
            IF RC <> 0 THEN LEAVE
            CHUNK = VALUE(VN)
            DO WHILE CHUNK <> ''
               PARSE VAR CHUNK ENTRY ';' CHUNK
               IF ENTRY = '' THEN ITERATE
               N = AT.0 + 1
               AT.N = ENTRY
               AT.0 = N
            END
         END
         IF AT.0 > 0 THEN RETURN 0
      END
   END

   IF SYSDSN("'"AUTHDSN"'") <> 'OK' THEN DO
      CALL ERRMSG 'MBKAUTH - AUTHORITY TABLE' AUTHDSN 'NOT FOUND'
      RETURN 8
   END
   DD = 'MBKAU'RIGHT(TIME('S'),3,'0')
   ADDRESS TSO "ALLOC F("DD") DA('"AUTHDSN"') SHR REUSE"
   IF RC <> 0 THEN DO
      CALL ERRMSG 'MBKAUTH - UNABLE TO ALLOCATE' AUTHDSN 'RC='RC
      RETURN 8
   END
   ADDRESS TSO "EXECIO * DISKR "DD" (STEM RAW. FINIS"
   ERC = RC
   ADDRESS TSO "FREE F("DD")"
   IF ERC <> 0 THEN DO
      CALL ERRMSG 'MBKAUTH - READ ERROR ON' AUTHDSN 'RC='ERC
      RETURN 8
   END

   DO I = 1 TO RAW.0
      LINE = RAW.I
      IF LEFT(LINE,1) = '*' | STRIP(LINE) = '' THEN ITERATE
      U  = TRANSLATE(STRIP(SUBSTR(LINE, 1, 8)))
      FN = TRANSLATE(STRIP(SUBSTR(LINE,10, 3)))
      LV = TRANSLATE(STRIP(SUBSTR(LINE,14, 7)))
      EX = STRIP(SUBSTR(LINE,22, 8))
      IF U = '' | FN = '' | LV = '' THEN ITERATE
      IF WORDPOS(FN, VALID_FUNCS) = 0 THEN ITERATE
      LV = SPACE(LV, 0)
      IF VERIFY(LV, VALID_LEVELS) > 0 THEN ITERATE
      IF EX = '' THEN EX = '99991231'
      IF \DATATYPE(EX,'W') | LENGTH(EX) <> 8 THEN ITERATE
      N = AT.0 + 1
      AT.N = U FN LV EX
      AT.0 = N
   END

   IF ISPF THEN CALL SAVE_CACHE
   RETURN 0

/*--------------------------------------------------------------------*/
/* SAVE THE PARSED TABLE IN THE SHARED POOL - VARIABLES ARE LIMITED   */
/* TO 32K SO THE TABLE IS SPLIT INTO CHUNKS OF ABOUT 4000 BYTES       */
/*--------------------------------------------------------------------*/
SAVE_CACHE:
   CN = 0
   CHUNK = ''
   DO I = 1 TO AT.0
      IF LENGTH(CHUNK) + LENGTH(AT.I) + 1 > 4000 THEN DO
         CN = CN + 1
         VN = 'MBKAUT'RIGHT(CN,2,'0')
         INTERPRET VN '= CHUNK'
         ADDRESS ISPEXEC "VPUT ("VN") SHARED"
         CHUNK = ''
      END
      CHUNK = CHUNK || AT.I || ';'
   END
   IF CHUNK <> '' THEN DO
      CN = CN + 1
      VN = 'MBKAUT'RIGHT(CN,2,'0')
      INTERPRET VN '= CHUNK'
      ADDRESS ISPEXEC "VPUT ("VN") SHARED"
   END
   MBKAUTBN = CN
   ADDRESS ISPEXEC "VPUT (MBKAUTBN) SHARED"
   RETURN

DROP_CACHE:
   IF \ISPF THEN RETURN
   MBKAUTBN = 0
   ADDRESS ISPEXEC "VPUT (MBKAUTBN) SHARED"
   RETURN

/*--------------------------------------------------------------------*/
/* ALL LEVELS HELD BY A USER FOR A FUNCTION (UNEXPIRED ENTRIES ONLY). */
/* ADM IN THE TABLE GIVES K AND R FOR EVERY FUNCTION BUT NEVER A,     */
/* 2 OR M - AN ADMINISTRATOR CANNOT APPROVE (IA-11-031 FINDING 4).    */
/*--------------------------------------------------------------------*/
USER_LEVELS: PROCEDURE EXPOSE AT. TODAY
   PARSE ARG UID, FUNC
   OUT = ''
   DO I = 1 TO AT.0
      PARSE VAR AT.I U FN LV EX .
      IF U <> UID THEN ITERATE
      IF EX < TODAY THEN ITERATE
      IF FN = FUNC THEN OUT = OUT || LV
      ELSE IF FN = 'ADM' & FUNC <> 'ADM' THEN DO
         IF POS('K', LV) > 0 THEN OUT = OUT || 'K'
         IF POS('R', LV) > 0 THEN OUT = OUT || 'R'
      END
   END
   /* REMOVE DUPLICATE LEVEL LETTERS */
   CLEAN = ''
   DO I = 1 TO LENGTH(OUT)
      C = SUBSTR(OUT, I, 1)
      IF POS(C, CLEAN) = 0 THEN CLEAN = CLEAN || C
   END
   RETURN CLEAN

/*--------------------------------------------------------------------*/
/* ERRORS GO TO THE ISPF LONG MESSAGE WHEN IN ISPF, ELSE TO SAY       */
/*--------------------------------------------------------------------*/
ERRMSG: PROCEDURE EXPOSE ISPF
   PARSE ARG TEXT
   IF ISPF THEN DO
      ZEDSMSG = 'AUTHORITY ERROR'
      ZEDLMSG = TEXT
      ADDRESS ISPEXEC "SETMSG MSG(ISRZ001)"
   END
   ELSE SAY TEXT
   RETURN
