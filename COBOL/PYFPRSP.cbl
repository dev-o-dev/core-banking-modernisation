       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PYFPRSP.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2008-03-10.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PYFPRSP                                             *
      * TITLE    : FASTER PAYMENTS GATEWAY RESPONSE PROCESSOR          *
      * TYPE     : CICS / DB2 / MQ - TRANSACTION PYFR                  *
      *            STARTED BY THE MQ TRIGGER MONITOR (CKTI) WHEN A     *
      *            MESSAGE ARRIVES ON MBK.FPS.RESPONSE (TRIGGER FIRST) *
      * PLAN     : MBKCICS1                                            *
      * TABLES   : MBK.PAYINSTR (SELECT / UPDATE)                      *
      * LINKS TO : PSMEMO1 (REVERSAL OF REJECTED PAYMENTS)             *
      *----------------------------------------------------------------*
      * DRAINS THE RESPONSE QUEUE. FOR EACH GATEWAY RESPONSE:          *
      *   ACSC  PAYMENT ACCEPTED  -> PAY_STATUS 'ACK', FPS ID STORED   *
      *   RJCT  PAYMENT REJECTED  -> PAY_STATUS 'REJ', CUSTOMER        *
      *                              RE-CREDITED (PRINCIPAL + CHARGE)  *
      * ONE UNIT OF WORK PER MESSAGE (MQGET UNDER SYNCPOINT).          *
      *                                                                *
      * A RESPONSE FOR AN UNKNOWN PAYMENT, OR ONE NOT AT STATUS 'SNT', *
      * IS LOGGED TO CSMT AND DISCARDED. PAYMENTS OPERATIONS RECONCILE *
      * THESE FROM THE GATEWAY END-OF-DAY REPORT (PROCEDURE PO-FPS-07).*
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2008-03-10 AOS  ORIGINAL VERSION                               *
      * 2008-07-02 AOS  REVERSAL INCLUDES CHARGE (PIR 2008/066)        *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PYFPRSP '.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-MSG-COUNT                PIC 9(07) VALUE ZERO.
           05  WS-ACK-COUNT                PIC 9(07) VALUE ZERO.
           05  WS-REJ-COUNT                PIC 9(07) VALUE ZERO.
           05  WS-IGN-COUNT                PIC 9(07) VALUE ZERO.
           05  WS-SQLCODE-DISP             PIC -9(09).
           05  WS-REASON-DISP              PIC 9(04).
           05  WS-REVERSAL-AMT             PIC S9(13)V99 COMP-3.
           05  WS-END-OF-QUEUE-IND         PIC X(01) VALUE 'N'.
               88  WS-END-OF-QUEUE             VALUE 'Y'.
      *
       01  WS-CSMT-MESSAGE.
           05  FILLER                      PIC X(09) VALUE 'PYFPRSP: '.
           05  WS-CSMT-TEXT                PIC X(111).
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  WS-PAY-ID                       PIC X(16).
       01  WS-PAY-STATUS                   PIC X(03).
       01  WS-DR-SORT-CODE                 PIC X(06).
       01  WS-DR-ACCT-NO                   PIC X(08).
       01  WS-DR-AMT                       PIC S9(13)V99 COMP-3.
       01  WS-CHARGE-AMT                   PIC S9(13)V99 COMP-3.
       01  WS-BENE-NAME                    PIC X(35).
       01  WS-SCHEME-REF                   PIC X(35).
       01  WS-REJECT-CD                    PIC X(04).
      *
      *---- MQ --------------------------------------------------------*
       01  MQ-CONSTANTS.
           COPY CMQV.
       01  MQM-TRIGGER-MESSAGE.
           COPY CMQTMV.
       01  MQM-OBJECT-DESCRIPTOR.
           COPY CMQODV.
       01  MQM-MESSAGE-DESCRIPTOR.
           COPY CMQMDV.
       01  MQM-GET-MESSAGE-OPTIONS.
           COPY CMQGMOV.
       01  WS-MQ-FIELDS.
           05  WS-HCONN                    PIC S9(09) BINARY VALUE 0.
           05  WS-HOBJ                     PIC S9(09) BINARY.
           05  WS-OPTIONS                  PIC S9(09) BINARY.
           05  WS-BUFFLEN                  PIC S9(09) BINARY.
           05  WS-DATALEN                  PIC S9(09) BINARY.
           05  WS-COMPCODE                 PIC S9(09) BINARY.
           05  WS-REASON                   PIC S9(09) BINARY.
      *
           COPY PYFPSMSG.
      *
           COPY CHCOMMAR.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           PERFORM 1000-OPEN-QUEUE

           PERFORM 2000-PROCESS-MESSAGE
               UNTIL WS-END-OF-QUEUE

           PERFORM 3000-CLOSE-QUEUE

           MOVE SPACES                     TO WS-CSMT-TEXT
           STRING 'COMPLETE - MSGS ' WS-MSG-COUNT
                  ' ACK ' WS-ACK-COUNT
                  ' REJ ' WS-REJ-COUNT
                  ' IGNORED ' WS-IGN-COUNT
                  DELIMITED BY SIZE
                  INTO WS-CSMT-TEXT
           END-STRING
           PERFORM 8000-WRITE-CSMT

           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-OPEN-QUEUE.
      *================================================================*
      *    THE TRIGGER MONITOR PASSES AN MQTM - TAKE THE QUEUE NAME
      *    FROM IT. IF STARTED MANUALLY, DEFAULT TO THE LIVE QUEUE.
           EXEC CICS RETRIEVE
                     INTO(MQM-TRIGGER-MESSAGE)
                     LENGTH(LENGTH OF MQM-TRIGGER-MESSAGE)
                     RESP(WS-RESP)
           END-EXEC

           MOVE MQOT-Q                     TO MQOD-OBJECTTYPE
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE MQTM-QNAME             TO MQOD-OBJECTNAME
           ELSE
               MOVE 'MBK.FPS.RESPONSE'     TO MQOD-OBJECTNAME
           END-IF

           COMPUTE WS-OPTIONS = MQOO-INPUT-SHARED
                              + MQOO-FAIL-IF-QUIESCING

           CALL 'MQOPEN' USING WS-HCONN
                               MQM-OBJECT-DESCRIPTOR
                               WS-OPTIONS
                               WS-HOBJ
                               WS-COMPCODE
                               WS-REASON

           IF WS-COMPCODE NOT = MQCC-OK
               MOVE WS-REASON              TO WS-REASON-DISP
               MOVE SPACES                 TO WS-CSMT-TEXT
               STRING 'MQOPEN FAILED - REASON ' WS-REASON-DISP
                      DELIMITED BY SIZE
                      INTO WS-CSMT-TEXT
               END-STRING
               PERFORM 8000-WRITE-CSMT
               EXEC CICS ABEND
                         ABCODE('PYF1')
               END-EXEC
           END-IF
           .
      *================================================================*
       2000-PROCESS-MESSAGE.
      *================================================================*
           MOVE MQMI-NONE                  TO MQMD-MSGID
           MOVE MQCI-NONE                  TO MQMD-CORRELID
           COMPUTE MQGMO-OPTIONS = MQGMO-SYNCPOINT
                                 + MQGMO-WAIT
                                 + MQGMO-CONVERT
                                 + MQGMO-FAIL-IF-QUIESCING
           MOVE 5000                       TO MQGMO-WAITINTERVAL
           MOVE LENGTH OF FPR-RESPONSE     TO WS-BUFFLEN
           MOVE SPACES                     TO FPR-RESPONSE

           CALL 'MQGET' USING WS-HCONN
                              WS-HOBJ
                              MQM-MESSAGE-DESCRIPTOR
                              MQM-GET-MESSAGE-OPTIONS
                              WS-BUFFLEN
                              FPR-RESPONSE
                              WS-DATALEN
                              WS-COMPCODE
                              WS-REASON

           EVALUATE TRUE
               WHEN WS-COMPCODE = MQCC-OK
                   ADD 1                   TO WS-MSG-COUNT
                   PERFORM 2100-APPLY-RESPONSE
                   EXEC CICS SYNCPOINT
                   END-EXEC
               WHEN WS-REASON = MQRC-NO-MSG-AVAILABLE
                   SET WS-END-OF-QUEUE     TO TRUE
               WHEN OTHER
                   MOVE WS-REASON          TO WS-REASON-DISP
                   MOVE SPACES             TO WS-CSMT-TEXT
                   STRING 'MQGET FAILED - REASON ' WS-REASON-DISP
                          DELIMITED BY SIZE
                          INTO WS-CSMT-TEXT
                   END-STRING
                   PERFORM 8000-WRITE-CSMT
                   EXEC CICS SYNCPOINT ROLLBACK
                   END-EXEC
                   SET WS-END-OF-QUEUE     TO TRUE
           END-EVALUATE
           .
      *================================================================*
       2100-APPLY-RESPONSE.
      *================================================================*
           IF NOT FPR-SIP-RESPONSE
               MOVE SPACES                 TO WS-CSMT-TEXT
               STRING 'UNEXPECTED MESSAGE TYPE ' FPR-MSG-TYPE
                      ' ID ' FPR-MSG-ID ' - DISCARDED'
                      DELIMITED BY SIZE
                      INTO WS-CSMT-TEXT
               END-STRING
               PERFORM 8000-WRITE-CSMT
               ADD 1                       TO WS-IGN-COUNT
           ELSE
               MOVE FPR-MSG-ID             TO WS-PAY-ID
               EXEC SQL
                   SELECT PAY_STATUS, DR_SORT_CODE, DR_ACCT_NO,
                          DR_AMT, CHARGE_AMT, BENE_NAME
                     INTO :WS-PAY-STATUS, :WS-DR-SORT-CODE,
                          :WS-DR-ACCT-NO, :WS-DR-AMT,
                          :WS-CHARGE-AMT, :WS-BENE-NAME
                     FROM MBK.PAYINSTR
                    WHERE PAY_ID = :WS-PAY-ID
                     WITH RS USE AND KEEP UPDATE LOCKS
               END-EXEC

               EVALUATE TRUE
                   WHEN SQLCODE = 100
                   WHEN SQLCODE = 0 AND WS-PAY-STATUS NOT = 'SNT'
                       MOVE SPACES         TO WS-CSMT-TEXT
                       STRING 'RESPONSE FOR ' WS-PAY-ID
                              ' IGNORED - NOT FOUND OR NOT AT SNT'
                              DELIMITED BY SIZE
                              INTO WS-CSMT-TEXT
                       END-STRING
                       PERFORM 8000-WRITE-CSMT
                       ADD 1               TO WS-IGN-COUNT
                   WHEN SQLCODE NOT = 0
                       PERFORM 9100-SQL-ERROR
                   WHEN FPR-ACCEPTED
                       PERFORM 2200-MARK-ACCEPTED
                   WHEN FPR-REJECTED
                       PERFORM 2300-MARK-REJECTED
                   WHEN OTHER
                       MOVE SPACES         TO WS-CSMT-TEXT
                       STRING 'UNKNOWN STATUS ' FPR-STATUS
                              ' FOR ' WS-PAY-ID ' - IGNORED'
                              DELIMITED BY SIZE
                              INTO WS-CSMT-TEXT
                       END-STRING
                       PERFORM 8000-WRITE-CSMT
                       ADD 1               TO WS-IGN-COUNT
               END-EVALUATE
           END-IF
           .
      *================================================================*
       2200-MARK-ACCEPTED.
      *================================================================*
           MOVE FPR-FPS-TXN-ID             TO WS-SCHEME-REF
           EXEC SQL
               UPDATE MBK.PAYINSTR
                  SET PAY_STATUS = 'ACK',
                      SCHEME_REF = :WS-SCHEME-REF,
                      STATUS_TS  = CURRENT TIMESTAMP
                WHERE PAY_ID = :WS-PAY-ID
           END-EXEC
           IF SQLCODE NOT = 0
               PERFORM 9100-SQL-ERROR
           ELSE
               ADD 1                       TO WS-ACK-COUNT
           END-IF
           .
      *================================================================*
       2300-MARK-REJECTED.
      *================================================================*
           MOVE FPR-REJECT-RSN             TO WS-REJECT-CD
           MOVE FPR-FPS-TXN-ID             TO WS-SCHEME-REF
           EXEC SQL
               UPDATE MBK.PAYINSTR
                  SET PAY_STATUS = 'REJ',
                      REJECT_CD  = :WS-REJECT-CD,
                      SCHEME_REF = :WS-SCHEME-REF,
                      STATUS_TS  = CURRENT TIMESTAMP
                WHERE PAY_ID = :WS-PAY-ID
           END-EXEC
           IF SQLCODE NOT = 0
               PERFORM 9100-SQL-ERROR
           ELSE
               PERFORM 2400-REVERSE-DEBIT
               ADD 1                       TO WS-REJ-COUNT
           END-IF
           .
      *================================================================*
       2400-REVERSE-DEBIT.
      *================================================================*
      *    RE-CREDIT PRINCIPAL AND CHARGE AS ONE REVERSAL POSTING.
      *    THE EXTERNAL REFERENCE IS PAY ID + 'REV' SO A REDELIVERED
      *    RESPONSE CANNOT CREDIT THE CUSTOMER TWICE.
           COMPUTE WS-REVERSAL-AMT = WS-DR-AMT + WS-CHARGE-AMT

           MOVE SPACES                     TO CH-COMMAREA
           MOVE 'MBKC'                     TO CH-EYECATCHER
           MOVE '01'                       TO CH-VERSION
           MOVE 'MEMOPOST'                 TO CH-SERVICE-CD
           MOVE 'BATCH '                   TO CH-CHANNEL-CD
           MOVE 'PYFR    '                 TO CH-USER-ID
           MOVE WS-PAY-ID                  TO CH-REQUEST-ID
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE WS-DR-SORT-CODE            TO CHMP-SORT-CODE
           MOVE WS-DR-ACCT-NO              TO CHMP-ACCT-NO
           MOVE 'REV '                     TO CHMP-TXN-CODE
           MOVE 'C'                        TO CHMP-DR-CR-IND
           MOVE WS-REVERSAL-AMT            TO CHMP-AMOUNT
           MOVE WS-BENE-NAME               TO CHMP-NARRATIVE-1
           STRING 'RETURNED FPS ' WS-PAY-ID
                  DELIMITED BY SIZE
                  INTO CHMP-NARRATIVE-2
           END-STRING
           MOVE WS-PAY-ID                  TO CHMP-EXT-REF
           MOVE 'REV'                      TO CHMP-EXT-REF (17:3)
           MOVE ZERO                       TO CHMP-VALUE-DATE
           MOVE 'N'                        TO CHMP-OVERRIDE-IND

           EXEC CICS LINK
                     PROGRAM('PSMEMO1')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

      *    DUPL IS ACCEPTABLE - THE REVERSAL IS ALREADY POSTED.
           IF WS-RESP NOT = DFHRESP(NORMAL)
           OR (NOT CH-RC-OK AND CH-REASON-CD NOT = 'DUPL')
               MOVE SPACES                 TO WS-CSMT-TEXT
               STRING 'REVERSAL FAILED FOR ' WS-PAY-ID
                      ' RC ' CH-RETURN-CD ' ' CH-REASON-CD
                      ' - MESSAGE BACKED OUT'
                      DELIMITED BY SIZE
                      INTO WS-CSMT-TEXT
               END-STRING
               PERFORM 8000-WRITE-CSMT
               EXEC CICS ABEND
                         ABCODE('PYF2')
               END-EXEC
           END-IF
           .
      *================================================================*
       3000-CLOSE-QUEUE.
      *================================================================*
           MOVE MQCO-NONE                  TO WS-OPTIONS
           CALL 'MQCLOSE' USING WS-HCONN
                                WS-HOBJ
                                WS-OPTIONS
                                WS-COMPCODE
                                WS-REASON
           .
      *================================================================*
       8000-WRITE-CSMT.
      *================================================================*
           EXEC CICS WRITEQ TD
                     QUEUE('CSMT')
                     FROM(WS-CSMT-MESSAGE)
                     LENGTH(LENGTH OF WS-CSMT-MESSAGE)
                     RESP(WS-RESP)
           END-EXEC
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
      *    BACK OUT THE MESSAGE (IT RETURNS TO THE QUEUE) AND ABEND SO
      *    THE TRIGGER IS NOT RE-FIRED IN A LOOP. OPERATIONS RESTART
      *    PYFR AFTER THE DB2 PROBLEM IS RESOLVED.
           MOVE SQLCODE                    TO WS-SQLCODE-DISP
           MOVE SPACES                     TO WS-CSMT-TEXT
           STRING 'SQL ERROR ' WS-SQLCODE-DISP
                  ' PROCESSING ' WS-PAY-ID
                  DELIMITED BY SIZE
                  INTO WS-CSMT-TEXT
           END-STRING
           PERFORM 8000-WRITE-CSMT
           EXEC CICS ABEND
                     ABCODE('PYF3')
           END-EXEC
           .
