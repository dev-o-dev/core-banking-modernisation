/* REXX ***************************************************************/
/* EXEC     : MBKSUBMT                                                */
/* TITLE    : ISPF DIALOGS - TAILOR AND SUBMIT A BATCH JOB            */
/* LIBRARY  : MBK.PRD.REXX                                            */
/* PANELS   : MBKSUB1 (CONFIRM / REVIEW)                              */
/* SKELETONS: MBK.PRD.ISPSLIB - MBKJOBC (JOB CARD) IS IMBEDDED BY     */
/*            EVERY JOB SKELETON                                      */
/*--------------------------------------------------------------------*/
/* CALL:  MBKSUBMT(skel, func, key, description)                      */
/*   SKEL  ISPSLIB MEMBER (PRMNT01S INRAT01S FXMNT01S CGREF01S        */
/*         GLSUS01S CFDEC01S RGAML01S)                                */
/*   FUNC  DIALOG FUNCTION FOR THE AUTHORITY CHECK (LEVEL S) AND THE  */
/*         AUDIT LOG                                                  */
/*   KEY   CHANGE REF ETC. FOR THE AUDIT LOG                          */
/* RESULT  0 SUBMITTED (JOB ID IN SHARED VARIABLE MBKJOBID)           */
/*         4 CANCELLED BY THE USER                                    */
/*         8 NOT AUTHORISED / OUTSIDE THE SUBMISSION WINDOW           */
/*        12 TAILORING OR SUBMIT FAILED                               */
/*                                                                    */
/* THE CALLER MUST VPUT ANY SKELETON VARIABLES TO THE SHARED POOL     */
/* BEFORE CALLING - THIS EXEC HAS ITS OWN FUNCTION POOL.              */
/*--------------------------------------------------------------------*/
/* SUBMISSION WINDOW: THE ON-REQUEST JOBS (PRMNT01, INRAT01, FXMNT01) */
/* UPDATE FILES READ BY THE END OF DAY SUITE. THEY MAY NOT BE         */
/* SUBMITTED BETWEEN 21:30 AND 05:30 (MBKEOD) OR AT ALL ON A NON-     */
/* BUSINESS DAY UNLESS THE USER HAS AN OPERATIONS OVERRIDE (ADM R).   */
/* THE CA-7 SCHEDULED JOBS (CGREF01 GLSUS01 CFDEC01 RGAML01) ARE      */
/* NORMALLY LEFT TO CA-7 - SUBMITTING THEM HERE IS FOR RERUNS ONLY    */
/* AND NEEDS THE OPERATIONS OVERRIDE.                                 */
/*--------------------------------------------------------------------*/
/* 2007-02-12 JNB  ORIGINAL (PRMNT01 ONLY)                            */
/* 2010-04-12 AOS  INRAT01                                            */
/* 2011-05-09 JNB  FXMNT01, TYPRUN=HOLD OPTION                        */
/* 2012-09-10 PDQ  SUBMISSION WINDOW (INC 2012/0877 - PRMNT01 RAN     */
/*                 DURING PSEOD01 AND PRDPARM WAS ENQUEUED)           */
/* 2019-05-13 RKS  JOB ID CAPTURED FOR THE AUDIT LOG                  */
/**********************************************************************/
TRACE O
PARSE ARG SKEL, FUNC, KEY, JDESC
SKEL  = TRANSLATE(STRIP(SKEL))
FUNC  = TRANSLATE(STRIP(FUNC))
KEY   = STRIP(KEY)
JDESC = STRIP(JDESC)

IF SYSVAR('SYSISPF') <> 'ACTIVE' THEN DO
   SAY 'MBKSUBMT - MUST RUN UNDER ISPF'
   RETURN 12
END
ADDRESS ISPEXEC
"CONTROL ERRORS RETURN"
"VGET (MBKHLQ MBKBDATE MBKNBDAY) SHARED"
IF MBKHLQ = '' THEN MBKHLQ = 'MBK.PRD'

ONREQ_JOBS = 'PRMNT01S INRAT01S FXMNT01S'
CA7_JOBS   = 'CGREF01S GLSUS01S CFDEC01S RGAML01S'
IF WORDPOS(SKEL, ONREQ_JOBS CA7_JOBS) = 0 THEN DO
   CALL MSG 'UNKNOWN SKELETON',,
      'MBKSUBMT - SKELETON' SKEL 'IS NOT A DIALOG JOB SKELETON'
   RETURN 12
END

/*--------------------------------------------------------------------*/
/* AUTHORITY - LEVEL S FOR THE FUNCTION, OVERRIDE = ADM LEVEL R       */
/*--------------------------------------------------------------------*/
IF MBKAUTH('CHECK', FUNC, 'S') <> 0 THEN DO
   CALL MBKAUDIT 'LOG', FUNC, 'DENIED', KEY, 'SUBMIT' SKEL,
      '- NO SUBMIT AUTHORITY'
   CALL MSG 'NOT AUTHORISED',,
      'YOU ARE NOT AUTHORISED TO SUBMIT' FUNC 'JOBS. THE ITEM IS',
      'SAVED - ASK A COLLEAGUE WITH SUBMIT AUTHORITY OR WAIT FOR',
      'THE SCHEDULED RUN.'
   RETURN 8
END
OVERRIDE = (MBKAUTH('CHECK', 'ADM', 'R') = 0)

NOW = SPACE(TRANSLATE(LEFT(TIME('N'),5),' ',':'),0)
IN_EOD = (NOW >= '2130' | NOW < '0530')
NONBUS = (MBKNBDAY = 'Y')

IF WORDPOS(SKEL, CA7_JOBS) > 0 & \OVERRIDE THEN DO
   CALL MSG 'CA-7 SCHEDULED JOB',,
      'JOB' LEFT(SKEL,7) 'RUNS UNDER CA-7 (MBKEOD). A MANUAL SUBMIT',
      'NEEDS AN OPERATIONS OVERRIDE - RAISE A CA-7 DEMAND INSTEAD.'
   RETURN 8
END
IF (IN_EOD | NONBUS) & \OVERRIDE THEN DO
   CALL MBKAUDIT 'LOG', FUNC, 'DENIED', KEY, 'SUBMIT' SKEL,
      '- OUTSIDE SUBMISSION WINDOW' NOW
   CALL MSG 'OUTSIDE WINDOW',,
      'ON-REQUEST JOBS CANNOT BE SUBMITTED BETWEEN 21:30 AND 05:30',
      'OR ON A NON-BUSINESS DAY. THE ITEM IS SAVED AND WILL BE',
      'PICKED UP BY THE NEXT RUN.'
   RETURN 8
END

/*--------------------------------------------------------------------*/
/* JOB CARD VARIABLES (USED BY SKELETON MBKJOBC)                      */
/*--------------------------------------------------------------------*/
SJOBNAME = LEFT(SKEL, 7)
SJDESC   = LEFT(JDESC, 20)
SUSER    = USERID()
SHOLD    = 'N'
SMSGCLS  = 'X'
SNOTIFY  = 'Y'
SACT     = 'S'
SKEY     = KEY
"VGET (SMSGCLS SNOTIFY) PROFILE"
IF SMSGCLS = '' THEN SMSGCLS = 'X'
IF SNOTIFY = '' THEN SNOTIFY = 'Y'
SOVR = ''
IF IN_EOD | NONBUS THEN SOVR = 'OPERATIONS OVERRIDE IN USE'

DO FOREVER
   "DISPLAY PANEL(MBKSUB1)"
   IF RC >= 8 THEN DO
      CALL MBKAUDIT 'LOG', FUNC, 'CANCEL', KEY, 'SUBMIT' SKEL,
         'CANCELLED AT CONFIRMATION'
      RETURN 4
   END
   "VPUT (SMSGCLS SNOTIFY) PROFILE"
   SELECT
      WHEN SACT = 'C' THEN DO
         CALL MBKAUDIT 'LOG', FUNC, 'CANCEL', KEY, 'SUBMIT' SKEL,
            'CANCELLED BY USER'
         RETURN 4
      END
      WHEN SACT = 'B' THEN DO
         IF TAILOR() <> 0 THEN RETURN 12
         /* VIEW, NOT EDIT - THE JCL MUST BE SUBMITTED AS TAILORED */
         "VIEW DATASET('"ZTEMPF"')"
         SACT = 'S'
      END
      WHEN SACT = 'S' THEN LEAVE
      OTHERWISE NOP
   END
END

IF TAILOR() <> 0 THEN RETURN 12

/*--------------------------------------------------------------------*/
/* SUBMIT AND CAPTURE THE JOB ID FROM THE IKJ56250I MESSAGE           */
/*--------------------------------------------------------------------*/
CALL OUTTRAP 'SUB.'
ADDRESS TSO "SUBMIT '"ZTEMPF"'"
SRC = RC
CALL OUTTRAP 'OFF'
JOBID = ''
DO I = 1 TO SUB.0
   IF POS('IKJ56250I', SUB.I) > 0 THEN DO
      PARSE VAR SUB.I . 'JOB ' JN '(' JOBID ')' .
      LEAVE
   END
END
IF SRC <> 0 | JOBID = '' THEN DO
   TXT = ''
   IF SUB.0 > 0 THEN TXT = SUB.1
   CALL MBKAUDIT 'LOG', FUNC, 'ERROR', KEY, 'SUBMIT' SKEL 'RC='SRC,
      LEFT(TXT, 60)
   CALL MSG 'SUBMIT FAILED',,
      'MBKSUBMT - SUBMIT OF' SKEL 'FAILED RC='SRC'.' TXT
   RETURN 12
END

MBKJOBID = JN'('JOBID')'
"VPUT (MBKJOBID) SHARED"
HOLDTXT = ''
IF SHOLD = 'Y' THEN HOLDTXT = 'TYPRUN=HOLD'
OVTXT = ''
IF SOVR <> '' THEN OVTXT = 'OVERRIDE'
CALL MBKAUDIT 'LOG', FUNC, 'SUBMIT', KEY, MBKJOBID SJDESC HOLDTXT,
   OVTXT
CALL MSG 'JOB' JOBID 'SUBMITTED',,
   'JOB' MBKJOBID 'SUBMITTED FOR' KEY'.' HOLDTXT,
   'CHECK THE OUTPUT IN SDSF (OWNER' SUSER').'
RETURN 0

/*--------------------------------------------------------------------*/
/* FILE TAILOR THE SKELETON INTO THE ISPF TEMPORARY DATASET           */
/*--------------------------------------------------------------------*/
TAILOR:
   SHOLDTXT = ''
   IF SHOLD = 'Y' THEN SHOLDTXT = ',TYPRUN=HOLD'
   SNOTTXT = ''
   IF SNOTIFY = 'Y' THEN SNOTTXT = ',NOTIFY=&SYSUID'
   SHLQ = MBKHLQ
   SDATE = DATE('S')
   STIME = TIME('N')
   "FTOPEN TEMP"
   IF RC <> 0 THEN DO
      CALL MSG 'FTOPEN FAILED', 'MBKSUBMT - FTOPEN RC='RC
      RETURN 12
   END
   "FTINCL "SKEL
   FRC = RC
   "FTCLOSE"
   IF FRC <> 0 THEN DO
      CALL MSG 'TAILORING FAILED',,
         'MBKSUBMT - FTINCL' SKEL 'RC='FRC '- SKELETON MISSING OR',
         'A VARIABLE IS UNDEFINED. CALL CORE SYSTEMS SUPPORT.'
      RETURN 12
   END
   "VGET (ZTEMPF) SHARED"
   RETURN 0

MSG: PROCEDURE
   PARSE ARG SHORT, LONG
   ZEDSMSG = SHORT
   ZEDLMSG = LONG
   ADDRESS ISPEXEC "SETMSG MSG(ISRZ001)"
   RETURN
