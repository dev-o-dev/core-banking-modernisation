       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CHROUTER.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2000-06-19.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CHROUTER                                            *
      * TITLE    : CHANNEL SERVICE ROUTER                              *
      * TYPE     : CICS DPL SERVER PROGRAM (NO TERMINAL)               *
      * REGION   : CICSPRD1 (AOR)                                      *
      * COMMAREA : CHCOMMAR (4000 BYTES)                               *
      *----------------------------------------------------------------*
      * SINGLE ENTRY POINT FOR THE REMOTE CHANNELS. INVOKED BY:        *
      *   - CICS TRANSACTION GATEWAY ECI (MIRROR TRANSACTION CHMI)     *
      *   - MQ-CICS DPL BRIDGE (TRANSACTION CKBP, QUEUE                *
      *     MBK.CICS.BRIDGE.REQ)                                       *
      *                                                                *
      * VALIDATES THE COMMAREA HEADER, LINKS TO THE SERVICE PROGRAM    *
      * FOR THE REQUESTED SERVICE CODE AND WRITES ONE AUDIT RECORD     *
      * PER CALL TO TRANSIENT DATA QUEUE CHLG (EXTRAPARTITION,         *
      * DSN MBK.PRD.CICS.CHLG - SWEPT TO SMF BY JOB CHLGSWP1).         *
      *                                                                *
      * THE CALL IS SYNCONRETURN - THE UNIT OF WORK IS COMMITTED WHEN  *
      * THIS PROGRAM RETURNS, OR BACKED OUT IF THE TASK ABENDS.        *
      *----------------------------------------------------------------*
      * ADDING A SERVICE: ADD AN ENTRY TO WS-SERVICE-TABLE, INCREASE   *
      * THE OCCURS, DEFINE THE PROGRAM IN CSD GROUP MBKCORE AND ADD    *
      * THE REQUEST/RESPONSE LAYOUT TO CHCOMMAR.                       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2000-06-19 DKP  ORIGINAL VERSION                               *
      * 2001-11-05 DKP  MEMOPOST                                       *
      * 2003-02-17 DKP  HANDLE ABEND - RETURN RC 0012 INSTEAD OF       *
      *                 ABENDING THE MIRROR (PIR 2003/009)             *
      * 2006-06-30 AOS  CHANNEL WHITELIST (AUDIT FINDING IA-06-14)     *
      * 2008-05-27 AOS  PAYINIT                                        *
      * 2009-10-02 JNB  MORTINQ (STUB)                                 *
      * 2010-03-08 AOS  SOMAINT, DDMANDT, ACCTOPEN, ACCTCLOS, ACCTHOLD,*
      *                 TXNHIST, CIFMAINT, POSTREV (LAYOUTS IN THE     *
      *                 NEW COPYBOOK CHCOMMA2 - CHCOMMAR UNCHANGED)    *
      * 2010-08-02 JNB  CARDAUTH AND THE CARDS CHANNEL. THE CARDS      *
      *                 CHANNEL MAY ONLY CALL CARDAUTH                 *
      * 2010-10-04 JNB  LOANENQ                                        *
      * 2011-05-16 JNB  FXQUOTE                                        *
      * 2012-09-03 AOS  CHQSTOP, ODLIMIT, STMTREQ, CUSTPREF (LAYOUTS   *
      *                 IN CHCOMMA2, CSD GROUP MBKCOR3)                *
      * 2013-06-24 PDQ  SWITCHRQ (CASS SWITCH-IN REQUESTS)             *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CHROUTER'.
      *
      *----------------------------------------------------------------*
      * SERVICE ROUTING TABLE                                          *
      *----------------------------------------------------------------*
       01  WS-SERVICE-TABLE-VALUES.
           05  FILLER  PIC X(16) VALUE 'CIFINQ  CFINQ01 '.
           05  FILLER  PIC X(16) VALUE 'ACCTINQ DDINQ01 '.
           05  FILLER  PIC X(16) VALUE 'ACCTLISTDDLST01 '.
           05  FILLER  PIC X(16) VALUE 'MEMOPOSTPSMEMO1 '.
           05  FILLER  PIC X(16) VALUE 'PAYINIT PYINIT1 '.
           05  FILLER  PIC X(16) VALUE 'MORTINQ MGSTUB1 '.
           05  FILLER  PIC X(16) VALUE 'SOMAINT SOMNT01 '.
           05  FILLER  PIC X(16) VALUE 'DDMANDT DMSVC01 '.
           05  FILLER  PIC X(16) VALUE 'ACCTOPENDDOPN01 '.
           05  FILLER  PIC X(16) VALUE 'ACCTCLOSDDCLS01 '.
           05  FILLER  PIC X(16) VALUE 'ACCTHOLDDDHLD01 '.
           05  FILLER  PIC X(16) VALUE 'TXNHIST DDTXN01 '.
           05  FILLER  PIC X(16) VALUE 'CIFMAINTCFMNT01 '.
           05  FILLER  PIC X(16) VALUE 'POSTREV PSREV01 '.
           05  FILLER  PIC X(16) VALUE 'CARDAUTHCRDAUT01'.
           05  FILLER  PIC X(16) VALUE 'LOANENQ LNSVC01 '.
           05  FILLER  PIC X(16) VALUE 'FXQUOTE FXSVC01 '.
           05  FILLER  PIC X(16) VALUE 'CHQSTOP CQSTP01 '.
           05  FILLER  PIC X(16) VALUE 'ODLIMIT ODLIM01 '.
           05  FILLER  PIC X(16) VALUE 'STMTREQ STREQ01 '.
           05  FILLER  PIC X(16) VALUE 'CUSTPREFCFPRF01 '.
           05  FILLER  PIC X(16) VALUE 'SWITCHRQCSSVC01 '.
       01  WS-SERVICE-TABLE REDEFINES WS-SERVICE-TABLE-VALUES.
           05  WS-SERVICE-ENTRY            OCCURS 22 TIMES
                                           INDEXED BY SVC-IDX.
               10  WS-SVC-CODE             PIC X(08).
               10  WS-SVC-PROGRAM          PIC X(08).
      *
      *----------------------------------------------------------------*
      * CHANNELS PERMITTED TO USE THIS INTERFACE                       *
      *----------------------------------------------------------------*
       01  WS-CHANNEL-VALUES.
           05  FILLER  PIC X(06) VALUE 'IBANK '.
           05  FILLER  PIC X(06) VALUE 'TELE  '.
           05  FILLER  PIC X(06) VALUE 'WAP   '.
           05  FILLER  PIC X(06) VALUE 'BRANCH'.
           05  FILLER  PIC X(06) VALUE 'CARDS '.
       01  WS-CHANNEL-TABLE REDEFINES WS-CHANNEL-VALUES.
           05  WS-CHANNEL-ENTRY            PIC X(06)
                                           OCCURS 5 TIMES
                                           INDEXED BY CHN-IDX.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP2                    PIC S9(08) COMP.
           05  WS-TARGET-PGM               PIC X(08) VALUE SPACES.
           05  WS-ABCODE                   PIC X(04) VALUE SPACES.
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-START-ABSTIME            PIC S9(15) COMP-3.
           05  WS-ELAPSED-MS               PIC S9(09) COMP-3.
           05  WS-RESP-DISPLAY             PIC 9(08).
      *
      *----------------------------------------------------------------*
      * AUDIT LOG RECORD - TDQ CHLG (RECFM=FB LRECL=160)               *
      *----------------------------------------------------------------*
       01  WS-LOG-RECORD.
           05  LOG-DATE                    PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-TIME                    PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-TRANID                  PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-TASKNO                  PIC 9(07).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-SERVICE-CD              PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-CHANNEL-CD              PIC X(06).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-USER-ID                 PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-REQUEST-ID              PIC X(24).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-PROGRAM                 PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-RETURN-CD               PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-REASON-CD               PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-ELAPSED-MS              PIC 9(07).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LOG-ABCODE                  PIC X(04).
           05  FILLER                      PIC X(46) VALUE SPACES.
      *
           COPY CHCOMMAR.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(4000).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           EXEC CICS HANDLE ABEND
                     LABEL(9000-ABEND-HANDLER)
           END-EXEC

           EXEC CICS ASKTIME
                     ABSTIME(WS-START-ABSTIME)
           END-EXEC

      *    NO (OR SHORT) COMMAREA - NOTHING TO RETURN AN ERROR IN.
           IF EIBCALEN NOT = LENGTH OF CH-COMMAREA
               EXEC CICS ABEND
                         ABCODE('CHR1')
                         NODUMP
               END-EXEC
           END-IF

           MOVE DFHCOMMAREA                TO CH-COMMAREA

           PERFORM 1000-VALIDATE-HEADER

           IF CH-RC-OK
               PERFORM 2000-ROUTE-REQUEST
           END-IF

           PERFORM 8000-WRITE-AUDIT-LOG

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-HEADER.
      *================================================================*
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT

           IF NOT CH-EYECATCHER-OK
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'EYEC'                 TO CH-REASON-CD
               MOVE 'INVALID COMMAREA EYECATCHER'
                                           TO CH-MESSAGE-TEXT
           ELSE
           IF CH-VERSION NOT = '01'
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'VERS'                 TO CH-REASON-CD
               MOVE 'UNSUPPORTED COMMAREA VERSION'
                                           TO CH-MESSAGE-TEXT
           END-IF
           END-IF

           IF CH-RC-OK
               SET CHN-IDX                 TO 1
               SEARCH WS-CHANNEL-ENTRY
                   AT END
                       MOVE '0016'         TO CH-RETURN-CD
                       MOVE 'CHNX'         TO CH-REASON-CD
                       MOVE 'CHANNEL NOT AUTHORISED FOR THIS INTERFACE'
                                           TO CH-MESSAGE-TEXT
                   WHEN WS-CHANNEL-ENTRY (CHN-IDX) = CH-CHANNEL-CD
                       CONTINUE
               END-SEARCH
           END-IF

           IF CH-RC-OK
               SET SVC-IDX                 TO 1
               SEARCH WS-SERVICE-ENTRY
                   AT END
                       MOVE '0016'         TO CH-RETURN-CD
                       MOVE 'SVCX'         TO CH-REASON-CD
                       MOVE 'UNKNOWN SERVICE CODE'
                                           TO CH-MESSAGE-TEXT
                   WHEN WS-SVC-CODE (SVC-IDX) = CH-SERVICE-CD
                       MOVE WS-SVC-PROGRAM (SVC-IDX)
                                           TO WS-TARGET-PGM
               END-SEARCH
           END-IF

      *    THE CARD GATEWAY USES A SHARED TECHNICAL USER - IT MUST NOT
      *    REACH ANY CUSTOMER SERVICE OTHER THAN CARDAUTH.
           IF CH-RC-OK
           AND CH-CHANNEL-CD = 'CARDS '
           AND CH-SERVICE-CD NOT = 'CARDAUTH'
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'CHNX'                 TO CH-REASON-CD
               MOVE 'CARDS CHANNEL MAY ONLY USE CARDAUTH'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       2000-ROUTE-REQUEST.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM(WS-TARGET-PGM)
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(PGMIDERR)
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'PGMX'             TO CH-REASON-CD
                   MOVE 'SERVICE PROGRAM NOT AVAILABLE'
                                           TO CH-MESSAGE-TEXT
               WHEN DFHRESP(NOTAUTH)
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'AUTH'             TO CH-REASON-CD
                   MOVE 'NOT AUTHORISED TO SERVICE PROGRAM'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE WS-RESP            TO WS-RESP-DISPLAY
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'LINK'             TO CH-REASON-CD
                   STRING 'LINK FAILED - EIBRESP '
                          WS-RESP-DISPLAY
                          DELIMITED BY SIZE
                          INTO CH-MESSAGE-TEXT
                   END-STRING
           END-EVALUATE
           .
      *================================================================*
       8000-WRITE-AUDIT-LOG.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(LOG-DATE)
                     DATESEP('-')
                     TIME(LOG-TIME)
                     TIMESEP(':')
           END-EXEC

           COMPUTE WS-ELAPSED-MS = WS-ABSTIME - WS-START-ABSTIME
           IF WS-ELAPSED-MS > 9999999
               MOVE 9999999                TO WS-ELAPSED-MS
           END-IF

           MOVE EIBTRNID                   TO LOG-TRANID
           MOVE EIBTASKN                   TO LOG-TASKNO
           MOVE CH-SERVICE-CD              TO LOG-SERVICE-CD
           MOVE CH-CHANNEL-CD              TO LOG-CHANNEL-CD
           MOVE CH-USER-ID                 TO LOG-USER-ID
           MOVE CH-REQUEST-ID              TO LOG-REQUEST-ID
           MOVE WS-TARGET-PGM              TO LOG-PROGRAM
           MOVE CH-RETURN-CD               TO LOG-RETURN-CD
           MOVE CH-REASON-CD               TO LOG-REASON-CD
           MOVE WS-ELAPSED-MS              TO LOG-ELAPSED-MS
           MOVE WS-ABCODE                  TO LOG-ABCODE

      *    AUDIT LOG FAILURE MUST NOT FAIL THE CUSTOMER REQUEST.
           EXEC CICS WRITEQ TD
                     QUEUE('CHLG')
                     FROM(WS-LOG-RECORD)
                     LENGTH(LENGTH OF WS-LOG-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           .
      *================================================================*
       9000-ABEND-HANDLER.
      *================================================================*
      *    A SERVICE PROGRAM (OR THIS PROGRAM) HAS ABENDED. BACK OUT
      *    ANY RECOVERABLE UPDATES AND RETURN RC 0012 TO THE CHANNEL
      *    SO THE GATEWAY CONNECTION IS NOT LOST.
           EXEC CICS HANDLE ABEND
                     CANCEL
           END-EXEC

           EXEC CICS ASSIGN
                     ABCODE(WS-ABCODE)
           END-EXEC

           EXEC CICS SYNCPOINT ROLLBACK
                     RESP(WS-RESP)
           END-EXEC

           MOVE '0012'                     TO CH-RETURN-CD
           MOVE 'ABND'                     TO CH-REASON-CD
           MOVE SPACES                     TO CH-MESSAGE-TEXT
           STRING 'SERVICE ABENDED - CODE '
                  WS-ABCODE
                  ' - PLEASE RETRY LATER'
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING

           PERFORM 8000-WRITE-AUDIT-LOG

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
