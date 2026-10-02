/* REXX ***************************************************************/
/* EXEC     : MBKPEND                                                 */
/* TITLE    : ISPF DIALOG PENDING QUEUE SERVICES                      */
/* LIBRARY  : MBK.PRD.REXX                                            */
/*--------------------------------------------------------------------*/
/* EACH DIALOG KEEPS WORK IN PROGRESS IN A PDS, ONE MEMBER PER ITEM:  */
/*   <HLQ>.ISPF.PEND.<FUNC>   RECFM FB, LRECL = THE BATCH CARD LRECL  */
/*        PRD 120  RT 100  CGR 120  GLS 120  BRV 200                  */
/* MEMBER NAMES                                                       */
/*   Pdhhmmss   AWAITING APPROVAL (SECOND USER)                       */
/*   Adhhmmss   APPROVED, AWAITING CUT-OFF RELEASE TO BATCH           */
/*   D = DAY OF MONTH AS A LETTER (A=1 ... Z=26, 0-4 = 27-31)         */
/* RECORD 1 OF EVERY MEMBER IS A HEADER, THE REST ARE CARD IMAGES     */
/* EXACTLY AS THEY WILL BE WRITTEN FOR THE BATCH PROGRAM:             */
/*   COLS  1-5  '*PEND'          COLS 33-40  DATE KEYED               */
/*   COLS  7-9  FUNCTION         COLS 42-47  TIME KEYED               */
/*   COLS 11-22 CHANGE REF / KEY COLS 49-56  APPROVER (WHEN APPROVED) */
/*   COLS 24-31 REQUESTER        COLS 58-80  DESCRIPTION              */
/*                                                                    */
/* DATA IS PASSED ON THE TSO DATA STACK (CARD IMAGES CAN CONTAIN      */
/* PACKED DECIMAL SO THEY CANNOT GO IN ISPF VARIABLES SAFELY).        */
/*                                                                    */
/* CALLS (RESULT 0 OK, 4 NOT FOUND / EMPTY, 8 ERROR - EXCEPT READ,    */
/* LIST AND COUNT WHICH RETURN A COUNT, -1 NOT FOUND OR -8 ERROR,     */
/* AND NEWMEM WHICH RETURNS '' ON ERROR)                              */
/*   MBKPEND('NEWMEM',func,prefix)  -> FREE MEMBER NAME               */
/*   MBKPEND('WRITE',func,mem,hdr)  CARDS ON THE STACK (QUEUED, FIFO) */
/*                                  ALL QUEUED LINES ARE CONSUMED     */
/*   MBKPEND('READ',func,mem)       QUEUES HEADER THEN CARDS, RESULT  */
/*                                  IS THE NUMBER OF CARD LINES       */
/*   MBKPEND('LIST',func,pattern)   QUEUES ONE MEMBER NAME PER LINE,  */
/*                                  RESULT IS THE COUNT               */
/*   MBKPEND('COUNT',func,pattern)  COUNT ONLY                        */
/*   MBKPEND('RENAME',func,old,new)                                   */
/*   MBKPEND('DELETE',func,mem)                                       */
/*   MBKPEND('LRECL',func)          -> LRECL OF THE PENDING PDS       */
/*--------------------------------------------------------------------*/
/* WHY NOT A DB2 TABLE? ASKED IN 2007 - THE DIALOGS HAD TO WORK WHEN  */
/* DB2P IS DOWN FOR THE SUNDAY REORGS. STILL TRUE.                    */
/*--------------------------------------------------------------------*/
/* 2007-02-12 JNB  ORIGINAL (PRODUCT CHANGE SETS ONLY)                */
/* 2008-04-07 JNB  GENERALISED FOR CHARGE REFUNDS                     */
/* 2008-06-23 KLM  GLS                                                */
/* 2011-02-14 AOS  BRV (LRECL 200)                                    */
/* 2013-01-21 PDQ  A-PREFIX MEMBERS FOR CUT-OFF RELEASE (WAS A        */
/*                 SEPARATE READY PDS PER FUNCTION)                   */
/* 2017-08-14 RKS  LMMLIST OPTION(FREE) - LIST WAS LEFT OPEN ON ERROR */
/**********************************************************************/
TRACE O
PARSE ARG REQ, FUNC, A1, A2
REQ  = TRANSLATE(STRIP(REQ))
FUNC = TRANSLATE(STRIP(FUNC))

IF SYSVAR('SYSISPF') <> 'ACTIVE' THEN DO
   SAY 'MBKPEND - MUST RUN UNDER ISPF'
   RETURN 8
END
ADDRESS ISPEXEC
"CONTROL ERRORS RETURN"
"VGET (MBKHLQ) SHARED"
IF RC <> 0 | MBKHLQ = '' THEN MBKHLQ = 'MBK.PRD'

SELECT
   WHEN REQ = 'NEWMEM' THEN BADRC = ''
   WHEN WORDPOS(REQ, 'READ LIST COUNT') > 0 THEN BADRC = -8
   OTHERWISE BADRC = 8
END
IF WORDPOS(FUNC, 'PRD RT CGR GLS BRV') = 0 THEN DO
   CALL ERR 'MBKPEND - FUNCTION "'FUNC'" HAS NO PENDING QUEUE'
   RETURN BADRC
END
PDSDSN = MBKHLQ'.ISPF.PEND.'FUNC

IF SYSDSN("'"PDSDSN"'") <> 'OK' THEN DO
   CALL ERR 'MBKPEND - PENDING LIBRARY' PDSDSN 'NOT FOUND'
   RETURN BADRC
END

SELECT
   WHEN REQ = 'NEWMEM' THEN RETURN NEW_MEMBER(A1)
   WHEN REQ = 'WRITE'  THEN RETURN WRITE_MEMBER(A1, A2)
   WHEN REQ = 'READ'   THEN RETURN READ_MEMBER(A1)
   WHEN REQ = 'LIST'   THEN RETURN LIST_MEMBERS(A1, 'Q')
   WHEN REQ = 'COUNT'  THEN RETURN LIST_MEMBERS(A1, 'C')
   WHEN REQ = 'RENAME' THEN RETURN RENAME_MEMBER(A1, A2)
   WHEN REQ = 'DELETE' THEN RETURN DELETE_MEMBER(A1)
   WHEN REQ = 'LRECL'  THEN RETURN GET_LRECL()
   OTHERWISE DO
      CALL ERR 'MBKPEND - INVALID REQUEST "'REQ'"'
      RETURN 8
   END
END
RETURN 8

/*--------------------------------------------------------------------*/
/* FIND A FREE MEMBER NAME  <PREFIX><DAY LETTER><HHMMSS>              */
/*--------------------------------------------------------------------*/
NEW_MEMBER: PROCEDURE EXPOSE PDSDSN
   PARSE ARG PFX
   PFX = LEFT(TRANSLATE(STRIP(PFX)),1)
   IF PFX = '' THEN PFX = 'P'
   DAYS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ01234'
   DD   = SUBSTR(DATE('S'),7,2)
   DL   = SUBSTR(DAYS, DD, 1)
   HMS  = SPACE(TRANSLATE(TIME('N'),' ',':'),0)
   DO TRY = 1 TO 500
      MEM = PFX || DL || HMS
      /* CHECK BOTH PREFIXES - THE SAME SUFFIX MUST NOT BE REUSED */
      /* WHILE AN APPROVED COPY IS WAITING FOR RELEASE            */
      IF SYSDSN("'"PDSDSN"(P"DL||HMS")'") <> 'OK' &,
         SYSDSN("'"PDSDSN"(A"DL||HMS")'") <> 'OK' THEN RETURN MEM
      HMS = RIGHT(HMS + 1, 6, '0')
      IF RIGHT(HMS,2) > 59 THEN,
         HMS = RIGHT(LEFT(HMS,4) + 1, 4, '0')'00'
   END
   CALL ERR 'MBKPEND - NO FREE MEMBER NAME IN' PDSDSN
   RETURN ''

/*--------------------------------------------------------------------*/
/* WRITE A MEMBER - HEADER PLUS EVERY LINE CURRENTLY QUEUED           */
/*--------------------------------------------------------------------*/
WRITE_MEMBER:
   PARSE ARG MEM, HDR
   LRECL = GET_LRECL()
   IF LRECL = 0 THEN RETURN 8
   CALL LM_OPEN 'OUTPUT', 'SHRW'
   IF RESULT <> 0 THEN RETURN 8
   REC = LEFT(HDR, LRECL)
   "LMPUT DATAID("PDID") MODE(INVAR) DATALOC(REC)",
      "DATALEN("LRECL")"
   IF RC <> 0 THEN SIGNAL WRITE_FAIL
   DO WHILE QUEUED() > 0
      PARSE PULL REC
      REC = LEFT(REC, LRECL)
      "LMPUT DATAID("PDID") MODE(INVAR) DATALOC(REC)",
         "DATALEN("LRECL")"
      IF RC <> 0 THEN SIGNAL WRITE_FAIL
   END
   "LMMREP DATAID("PDID") MEMBER("MEM")"
   IF RC > 8 THEN SIGNAL WRITE_FAIL
   CALL LM_CLOSE
   RETURN 0
WRITE_FAIL:
   WRC = RC
   DO WHILE QUEUED() > 0; PULL .; END
   CALL LM_CLOSE
   CALL ERR 'MBKPEND - WRITE OF' PDSDSN'('MEM') FAILED RC='WRC
   RETURN 8

/*--------------------------------------------------------------------*/
/* READ A MEMBER ONTO THE STACK                                       */
/*--------------------------------------------------------------------*/
READ_MEMBER:
   PARSE ARG MEM
   MEM = TRANSLATE(STRIP(MEM))
   LRECL = GET_LRECL()
   IF LRECL = 0 THEN RETURN -8
   CALL LM_OPEN 'INPUT', 'SHR'
   IF RESULT <> 0 THEN RETURN -8
   "LMMFIND DATAID("PDID") MEMBER("MEM")"
   IF RC = 8 THEN DO
      CALL LM_CLOSE
      RETURN -1
   END
   IF RC <> 0 THEN DO
      CALL LM_CLOSE
      CALL ERR 'MBKPEND - LMMFIND' MEM 'RC='RC
      RETURN -8
   END
   N = 0
   DO FOREVER
      "LMGET DATAID("PDID") MODE(INVAR) DATALOC(REC)",
         "DATALEN(RLEN) MAXLEN("LRECL")"
      IF RC = 8 THEN LEAVE
      IF RC <> 0 THEN DO
         CALL LM_CLOSE
         CALL ERR 'MBKPEND - LMGET' MEM 'RC='RC
         RETURN -8
      END
      QUEUE LEFT(REC, LRECL)
      N = N + 1
   END
   CALL LM_CLOSE
   IF N = 0 THEN RETURN -1
   RETURN N - 1

/*--------------------------------------------------------------------*/
/* LIST MEMBERS MATCHING A PATTERN (QUEUE) OR COUNT THEM              */
/*--------------------------------------------------------------------*/
LIST_MEMBERS:
   PARSE ARG PAT, HOW
   PAT = TRANSLATE(STRIP(PAT))
   IF PAT = '' THEN PAT = '*'
   CALL LM_INIT 'SHR'
   IF RESULT <> 0 THEN RETURN -8
   "LMOPEN DATAID("PDID") OPTION(INPUT)"
   IF RC <> 0 THEN DO
      "LMFREE DATAID("PDID")"
      CALL ERR 'MBKPEND - LMOPEN' PDSDSN 'RC='RC
      RETURN -8
   END
   CNT = 0
   LMEM = ''
   DO FOREVER
      "LMMLIST DATAID("PDID") OPTION(LIST) MEMBER(LMEM)",
         "PATTERN("PAT") STATS(NO)"
      IF RC <> 0 THEN LEAVE
      CNT = CNT + 1
      IF HOW = 'Q' THEN QUEUE STRIP(LMEM)
   END
   "LMMLIST DATAID("PDID") OPTION(FREE)"
   "LMCLOSE DATAID("PDID")"
   "LMFREE DATAID("PDID")"
   RETURN CNT

/*--------------------------------------------------------------------*/
/* RENAME / DELETE - DATASET MUST BE ENQ SHRW AND NOT OPEN            */
/*--------------------------------------------------------------------*/
RENAME_MEMBER:
   PARSE ARG OLDM, NEWM
   OLDM = TRANSLATE(STRIP(OLDM)); NEWM = TRANSLATE(STRIP(NEWM))
   CALL LM_INIT 'SHRW'
   IF RESULT <> 0 THEN RETURN 8
   "LMMREN DATAID("PDID") MEMBER("OLDM") NEWNAME("NEWM")"
   RRC = RC
   "LMFREE DATAID("PDID")"
   SELECT
      WHEN RRC = 0 THEN RETURN 0
      WHEN RRC = 8 THEN DO
         CALL ERR 'MBKPEND - RENAME' OLDM '->' NEWM,
            '- MEMBER MISSING OR NEW NAME EXISTS'
         RETURN 4
      END
      OTHERWISE DO
         CALL ERR 'MBKPEND - LMMREN' OLDM 'RC='RRC
         RETURN 8
      END
   END

DELETE_MEMBER:
   PARSE ARG MEM
   MEM = TRANSLATE(STRIP(MEM))
   CALL LM_INIT 'SHRW'
   IF RESULT <> 0 THEN RETURN 8
   "LMMDEL DATAID("PDID") MEMBER("MEM")"
   DRC = RC
   "LMFREE DATAID("PDID")"
   IF DRC = 0 THEN RETURN 0
   IF DRC = 8 THEN RETURN 4
   CALL ERR 'MBKPEND - LMMDEL' MEM 'RC='DRC
   RETURN 8

/*--------------------------------------------------------------------*/
/* LIBRARY MANAGEMENT HELPERS                                         */
/*--------------------------------------------------------------------*/
LM_INIT:
   PARSE ARG ENQ
   PDID = ''
   "LMINIT DATAID(PDID) DATASET('"PDSDSN"') ENQ("ENQ")"
   IF RC <> 0 THEN DO
      CALL ERR 'MBKPEND - LMINIT' PDSDSN 'RC='RC
      RETURN 8
   END
   RETURN 0

LM_OPEN:
   PARSE ARG OPT, ENQ
   CALL LM_INIT ENQ
   IF RESULT <> 0 THEN RETURN 8
   "LMOPEN DATAID("PDID") OPTION("OPT")"
   IF RC <> 0 THEN DO
      ORC = RC
      "LMFREE DATAID("PDID")"
      IF ORC = 8 THEN,
         CALL ERR 'MBKPEND -' PDSDSN 'IS IN USE - TRY AGAIN'
      ELSE CALL ERR 'MBKPEND - LMOPEN' PDSDSN 'RC='ORC
      RETURN 8
   END
   RETURN 0

LM_CLOSE:
   "LMCLOSE DATAID("PDID")"
   "LMFREE DATAID("PDID")"
   RETURN

GET_LRECL:
   X = LISTDSI("'"PDSDSN"'" 'NORECALL')
   IF X > 4 THEN DO
      CALL ERR 'MBKPEND - LISTDSI' PDSDSN SYSMSGLVL2
      RETURN 0
   END
   IF LEFT(SYSRECFM,1) <> 'F' THEN DO
      CALL ERR 'MBKPEND -' PDSDSN 'MUST BE RECFM FB, IS' SYSRECFM
      RETURN 0
   END
   RETURN SYSLRECL

ERR: PROCEDURE
   PARSE ARG TEXT
   ZEDSMSG = 'PENDING QUEUE ERROR'
   ZEDLMSG = TEXT
   ADDRESS ISPEXEC "SETMSG MSG(ISRZ001)"
   RETURN
