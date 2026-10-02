/* REXX ***************************************************************/
/* EXEC     : MBKMENU                                                 */
/* TITLE    : MEGABANK OPERATIONS DIALOGS - MAIN MENU DRIVER          */
/* LIBRARY  : MBK.PRD.REXX                                            */
/* INVOKE   : TSO %MBKMENU            (FROM ISPF OPTION 6 OR ANY      */
/*                                     COMMAND LINE: TSO MBKMENU)     */
/*            TSO %MBKMENU ENV(UAT)   (TEST LIBRARIES MBK.UAT.*)      */
/* PANELS   : MBKMENU MBKMENUH MBKAPRQ MBKRELQ                        */
/*--------------------------------------------------------------------*/
/* SETS UP THE DIALOG ENVIRONMENT AND DISPLAYS THE MAIN MENU:         */
/*   - LIBDEFS FOR PANELS (ISPPLIB) AND SKELETONS (ISPSLIB), ALTLIB   */
/*     FOR THE EXECS, ALL STACKED AND REMOVED ON EXIT                 */
/*   - BUSINESS DATE FROM <HLQ>.CNTL(BUSDATE) INTO THE SHARED POOL    */
/*     (MBKBDATE MBKPDATE MBKNDATE MBKNBDAY)                          */
/*   - THE USER'S FUNCTIONS FROM THE AUTHORITY TABLE (MBKAUTH)        */
/*   - OPTION A  APPROVAL QUEUE - PENDING ITEMS FOR EVERY FUNCTION    */
/*               THE USER CAN APPROVE (EXCLUDING THEIR OWN)           */
/*   - OPTION R  CUT-OFF RELEASE - APPROVED ITEMS WAITING TO GO TO    */
/*               TONIGHT'S BATCH (CGR GLS BRV)                        */
/*   - OPTION L  AUDIT LOG                                            */
/* EVERY SELECTION AND DENIAL IS AUDIT LOGGED.                        */
/*--------------------------------------------------------------------*/
/* RC 0 NORMAL, 8 NOT AUTHORISED FOR ANY FUNCTION, 12 SETUP FAILED    */
/*--------------------------------------------------------------------*/
/* 2007-02-12 JNB  ORIGINAL - PRODUCT CHANGE SETS (FORM PC/17         */
/*                 REPLACED BY PANEL MBKPRD1)                         */
/* 2008-04-07 JNB  CHARGE REFUNDS                                     */
/* 2008-06-23 KLM  GL SUSPENSE CLEARING                               */
/* 2009-09-14 AOS  AML RULE PARAMETERS                                */
/* 2010-04-12 AOS  RATE CHANGES (MBKRTCH) MOVED UNDER THIS MENU       */
/* 2011-02-14 AOS  BEREAVEMENT (CONTINGENCY FOR THE WORKFLOW TOOL)    */
/* 2011-05-09 JNB  FX CURRENCY SET-UP (FXMNT)                         */
/* 2013-01-21 PDQ  APPROVAL QUEUE AND CUT-OFF RELEASE OPTIONS         */
/* 2014-10-06 PDQ  ENV() PARAMETER FOR UAT                            */
/* 2019-05-13 RKS  ALTLIB INSTEAD OF SYSEXEC CONCATENATION IN LOGON   */
/*                 PROC (TSO LOGON PROC RATIONALISATION)              */
/**********************************************************************/
TRACE O
PARSE UPPER ARG PARMS
ENV = 'PRD'
MODE = 'MENU'
IF POS('ENV(', PARMS) > 0 THEN PARSE VAR PARMS 'ENV(' ENV ')' .
IF WORDPOS('APPRQ', PARMS) > 0 THEN MODE = 'APPRQ'
IF WORDPOS('RELQ',  PARMS) > 0 THEN MODE = 'RELQ'
ENV = STRIP(ENV)
IF WORDPOS(ENV, 'PRD UAT SYS') = 0 THEN DO
   SAY 'MBKMENU - ENV MUST BE PRD, UAT OR SYS'
   EXIT 12
END

/*--------------------------------------------------------------------*/
/* START ISPF IF WE WERE CALLED FROM NATIVE TSO                       */
/*--------------------------------------------------------------------*/
IF SYSVAR('SYSISPF') <> 'ACTIVE' THEN DO
   ADDRESS TSO "ISPSTART CMD(%MBKMENU "PARMS") NEWAPPL(MBK)"
   EXIT RC
END

ADDRESS ISPEXEC
"CONTROL ERRORS RETURN"

/* A NESTED CALL (APPRQ / RELQ FROM THE MENU) REUSES THE ENVIRONMENT */
IF MODE <> 'MENU' THEN DO
   "VGET (MBKHLQ MBKBDATE) SHARED"
   IF MODE = 'APPRQ' THEN CALL APPROVAL_QUEUE
   ELSE CALL RELEASE_QUEUE
   EXIT 0
END

MBKHLQ = 'MBK.'ENV
"VPUT (MBKHLQ) SHARED"

/*--------------------------------------------------------------------*/
/* LIBRARIES                                                          */
/*--------------------------------------------------------------------*/
LIBS_OK = 1
DO L = 1 TO 2
   LIBTYPE = WORD('ISPPLIB ISPSLIB', L)
   LIBDSN  = MBKHLQ'.'LIBTYPE
   IF SYSDSN("'"LIBDSN"'") <> 'OK' THEN DO
      SAY 'MBKMENU -' LIBDSN 'NOT FOUND'
      LIBS_OK = 0
      ITERATE
   END
   "LIBDEF "LIBTYPE" DATASET ID('"LIBDSN"') STACK"
   IF RC <> 0 THEN DO
      SAY 'MBKMENU - LIBDEF' LIBTYPE 'FAILED RC='RC
      LIBS_OK = 0
   END
END
IF \LIBS_OK THEN EXIT 12
ADDRESS TSO "ALTLIB ACTIVATE APPLICATION(EXEC)",
   "DATASET('"MBKHLQ".REXX')"
IF RC <> 0 THEN DO
   CALL CLEANUP
   SAY 'MBKMENU - ALTLIB FOR' MBKHLQ'.REXX FAILED RC='RC
   EXIT 12
END

/* FORCE A FRESH READ OF THE AUTHORITY TABLE FOR THIS SESSION */
CALL MBKAUTH 'REFRESH'

IF GET_BUSDATE() <> 0 THEN DO
   CALL CLEANUP
   EXIT 12
END

MNFUNCS = MBKAUTH('FUNCS')
IF MNFUNCS = '' THEN DO
   CALL MBKAUDIT 'LOG', 'MNU', 'DENIED', '', 'NO DIALOG AUTHORITY'
   ZEDSMSG = 'NOT AUTHORISED'
   ZEDLMSG = 'YOU HAVE NO ENTRIES IN' MBKHLQ'.ISPF.AUTHTAB. ASK',
      'YOUR TEAM LEADER TO RAISE FORM SEC/09.'
   "SETMSG MSG(ISRZ001)"
   CALL CLEANUP
   EXIT 8
END
CALL MBKAUDIT 'LOG', 'MNU', 'SIGNON', ENV, 'FUNCTIONS:' MNFUNCS

/*--------------------------------------------------------------------*/
/* MAIN MENU LOOP                                                     */
/*--------------------------------------------------------------------*/
OPTS = '1 2 3 4 5 6 7'
FUNCS = 'PRD RT FX CGR GLS BRV AML'
EXECS = 'MBKPRDCH MBKRTCH FXMNT MBKCGREF MBKGLSUS MBKBRV MBKAMLRP'
DESCS = 'PRODUCT_CHANGES RATE_CHANGES FX_CURRENCY CHARGE_REFUNDS',
        'GL_SUSPENSE BEREAVEMENT AML_RULES'

CALL SET_MENU_FLAGS
ZCMD = ''
DO FOREVER
   MNUSER = USERID()
   MNTIME = LEFT(TIME('N'),5)
   "DISPLAY PANEL(MBKMENU)"
   IF RC >= 8 THEN LEAVE
   OPT = TRANSLATE(STRIP(ZCMD))
   ZCMD = ''
   PARSE VAR OPT OPT '.' REST
   SELECT
      WHEN OPT = '' THEN ITERATE
      WHEN OPT = 'X' THEN LEAVE
      WHEN WORDPOS(OPT, OPTS) > 0 THEN DO
         N = WORDPOS(OPT, OPTS)
         FN = WORD(FUNCS, N)
         EX = WORD(EXECS, N)
         IF WORDPOS(FN, MNFUNCS) = 0 THEN DO
            CALL MBKAUDIT 'LOG', 'MNU', 'DENIED', FN,
               'OPTION' OPT 'NOT AUTHORISED'
            ZEDSMSG = 'NOT AUTHORISED'
            ZEDLMSG = 'YOU ARE NOT AUTHORISED FOR' ,
               TRANSLATE(WORD(DESCS,N),' ','_')'.'
            "SETMSG MSG(ISRZ001)"
            ITERATE
         END
         PRM = ''
         IF REST <> '' THEN PRM = REST
         "SELECT CMD(%"EX PRM")"
      END
      WHEN OPT = 'A' THEN CALL APPROVAL_QUEUE
      WHEN OPT = 'R' THEN CALL RELEASE_QUEUE
      WHEN OPT = 'L' THEN DO
         /* NON-ADMINISTRATORS SEE THEIR FIRST FUNCTION BY DEFAULT */
         /* (THE FILTER ON MBKAUD1 CAN BE CHANGED - THE LOG IS NOT */
         /* CONFIDENTIAL WITHIN OPERATIONS, IA-10-044 AGREED)      */
         IF REST = '' & WORDPOS('ADM', MNFUNCS) = 0 THEN,
            REST = WORD(MNFUNCS, 1)
         "SELECT CMD(%MBKAUDIT BROWSE "REST")"
      END
      WHEN OPT = 'T' THEN DO
         "SELECT PGM(ISPTUTOR) PARM(MBKMENUH)"
      END
      OTHERWISE DO
         ZEDSMSG = 'INVALID OPTION'
         ZEDLMSG = 'OPTION "'OPT'" IS NOT VALID - ENTER 1-7, A, R,',
            'L, T OR X.'
         "SETMSG MSG(ISRZ001)"
      END
   END
   CALL SET_MENU_FLAGS
END

CALL MBKAUDIT 'LOG', 'MNU', 'SIGNOFF', ENV, ''
CALL CLEANUP
EXIT 0

/*--------------------------------------------------------------------*/
/* MENU FLAGS - '*' NEXT TO OPTIONS THE USER CANNOT USE, AND THE      */
/* NUMBER OF ITEMS WAITING FOR THEIR APPROVAL                         */
/*--------------------------------------------------------------------*/
SET_MENU_FLAGS:
   DO I = 1 TO 7
      FN = WORD(FUNCS, I)
      IF WORDPOS(FN, MNFUNCS) > 0 THEN FLAG = ' '
      ELSE FLAG = '*'
      INTERPRET 'MNF'I '= FLAG'
   END
   MNPEND = 0
   DO I = 1 TO WORDS('PRD RT CGR GLS')
      FN = WORD('PRD RT CGR GLS', I)
      LV = MBKAUTH('LEVELS', FN)
      IF VERIFY(LV, 'A2', 'M') = 0 THEN ITERATE
      C = MBKPEND('COUNT', FN, 'P*')
      IF DATATYPE(C,'W') THEN IF C > 0 THEN MNPEND = MNPEND + C
   END
   IF MNPEND = 0 THEN MNPTXT = ''
   ELSE MNPTXT = MNPEND 'ITEM(S) AWAITING APPROVAL - OPTION A'
   RETURN

/*--------------------------------------------------------------------*/
/* BUSINESS DATE CARD                                                 */
/*--------------------------------------------------------------------*/
GET_BUSDATE:
   DCDSN = MBKHLQ'.CNTL(BUSDATE)'
   ADDRESS TSO "ALLOC F(MBKDTC) DA('"DCDSN"') SHR REUSE"
   IF RC <> 0 THEN DO
      SAY 'MBKMENU - CANNOT ALLOCATE' DCDSN
      RETURN 12
   END
   ADDRESS TSO "EXECIO 1 DISKR MBKDTC (STEM DC. FINIS"
   ERC = RC
   ADDRESS TSO "FREE F(MBKDTC)"
   IF ERC <> 0 THEN DO
      SAY 'MBKMENU - CANNOT READ' DCDSN 'RC='ERC
      RETURN 12
   END
   PARSE VAR DC.1 'BUSDATE=' MBKBDATE ' PREVDATE=' MBKPDATE,
      ' NEXTDATE=' MBKNDATE ' RUN=' MBKRUNID .
   IF \DATATYPE(MBKBDATE,'W') | LENGTH(MBKBDATE) <> 8 THEN DO
      SAY 'MBKMENU - DATE CARD INVALID:' DC.1
      RETURN 12
   END
   /* TODAY IS A NON-BUSINESS DAY IF THE CALENDAR DATE IS NOT THE   */
   /* BUSINESS DATE (WEEKENDS AND BANK HOLIDAYS DO NOT ROLL IT)     */
   IF DATE('S') <> MBKBDATE THEN MBKNBDAY = 'Y'
   ELSE MBKNBDAY = 'N'
   MNBDATE = LEFT(MBKBDATE,4)'-'SUBSTR(MBKBDATE,5,2)'-',
      || RIGHT(MBKBDATE,2)
   "VPUT (MBKBDATE MBKPDATE MBKNDATE MBKNBDAY MBKRUNID) SHARED"
   RETURN 0

/*--------------------------------------------------------------------*/
/* APPROVAL QUEUE - ONE ROW PER FUNCTION WITH PENDING ITEMS           */
/*--------------------------------------------------------------------*/
APPROVAL_QUEUE:
   QFUNCS = 'PRD RT CGR GLS'
   QEXECS = 'MBKPRDCH MBKRTCH MBKCGREF MBKGLSUS'
   QDESCS = 'PRODUCT_PARAMETER_CHANGE_SETS INTEREST_RATE_CHANGES',
            'CHARGE_REFUNDS GL_SUSPENSE_WRITE-OFFS'
   DO FOREVER
      "TBCREATE MBKAPRQ NOWRITE REPLACE",
         "NAMES(AQSEL AQFUNC AQDESC AQCOUNT AQLEVEL AQEXEC)"
      ROWS = 0
      DO I = 1 TO WORDS(QFUNCS)
         AQFUNC = WORD(QFUNCS, I)
         AQLEVEL = MBKAUTH('LEVELS', AQFUNC)
         IF VERIFY(AQLEVEL, 'A2', 'M') = 0 THEN ITERATE
         AQCOUNT = MBKPEND('COUNT', AQFUNC, 'P*')
         IF AQCOUNT < 0 THEN AQCOUNT = '?'
         AQDESC = TRANSLATE(WORD(QDESCS, I), ' ', '_')
         AQEXEC = WORD(QEXECS, I)
         AQSEL  = ''
         "TBADD MBKAPRQ"
         ROWS = ROWS + 1
      END
      IF ROWS = 0 THEN DO
         "TBEND MBKAPRQ"
         ZEDSMSG = 'NO APPROVAL AUTHORITY'
         ZEDLMSG = 'YOU ARE NOT AN APPROVER FOR ANY FUNCTION WITH A',
            'PENDING QUEUE.'
         "SETMSG MSG(ISRZ001)"
         RETURN
      END
      "TBTOP MBKAPRQ"
      "TBDISPLAY MBKAPRQ PANEL(MBKAPRQ)"
      IF RC >= 8 THEN DO
         "TBEND MBKAPRQ"
         RETURN
      END
      PICKED = ''
      DO WHILE ZTDSELS > 0
         IF TRANSLATE(AQSEL) = 'S' THEN PICKED = PICKED AQEXEC
         IF ZTDSELS > 1 THEN "TBDISPLAY MBKAPRQ"
         ELSE ZTDSELS = 0
      END
      "TBEND MBKAPRQ"
      DO P = 1 TO WORDS(PICKED)
         "SELECT CMD(%"WORD(PICKED,P)" APPROVE)"
      END
      IF PICKED = '' THEN RETURN
   END
   RETURN

/*--------------------------------------------------------------------*/
/* CUT-OFF RELEASE QUEUE - APPROVED ITEMS NOT YET IN A BATCH FILE     */
/*--------------------------------------------------------------------*/
RELEASE_QUEUE:
   RFUNCS = 'CGR GLS BRV'
   REXECS = 'MBKCGREF MBKGLSUS MBKBRV'
   RJOBS  = 'CGREF01 GLSUS01 CFDEC01'
   RDESCS = 'CHARGE_REFUNDS GL_SUSPENSE_CLEARING BEREAVEMENT'
   DO FOREVER
      "TBCREATE MBKRELQ NOWRITE REPLACE",
         "NAMES(RQSEL RQFUNC RQDESC RQCOUNT RQJOB RQEXEC)"
      ROWS = 0
      DO I = 1 TO WORDS(RFUNCS)
         RQFUNC = WORD(RFUNCS, I)
         IF MBKAUTH('CHECK', RQFUNC, 'R') <> 0 THEN ITERATE
         RQCOUNT = MBKPEND('COUNT', RQFUNC, 'A*')
         IF RQCOUNT < 0 THEN RQCOUNT = '?'
         RQDESC = TRANSLATE(WORD(RDESCS, I), ' ', '_')
         RQJOB  = WORD(RJOBS, I)
         RQEXEC = WORD(REXECS, I)
         RQSEL  = ''
         "TBADD MBKRELQ"
         ROWS = ROWS + 1
      END
      IF ROWS = 0 THEN DO
         "TBEND MBKRELQ"
         ZEDSMSG = 'NO RELEASE AUTHORITY'
         ZEDLMSG = 'CUT-OFF RELEASE NEEDS LEVEL R FOR CGR, GLS OR BRV.'
         "SETMSG MSG(ISRZ001)"
         RETURN
      END
      "TBTOP MBKRELQ"
      "TBDISPLAY MBKRELQ PANEL(MBKRELQ)"
      IF RC >= 8 THEN DO
         "TBEND MBKRELQ"
         RETURN
      END
      PICKED = ''
      DO WHILE ZTDSELS > 0
         IF TRANSLATE(RQSEL) = 'S' THEN PICKED = PICKED RQEXEC
         IF ZTDSELS > 1 THEN "TBDISPLAY MBKRELQ"
         ELSE ZTDSELS = 0
      END
      "TBEND MBKRELQ"
      DO P = 1 TO WORDS(PICKED)
         "SELECT CMD(%"WORD(PICKED,P)" RELEASE)"
      END
      IF PICKED = '' THEN RETURN
   END
   RETURN

/*--------------------------------------------------------------------*/
/* REMOVE LIBDEFS AND ALTLIB                                          */
/*--------------------------------------------------------------------*/
CLEANUP:
   ADDRESS TSO "ALTLIB DEACTIVATE APPLICATION(EXEC)"
   "LIBDEF ISPPLIB"
   "LIBDEF ISPSLIB"
   RETURN
