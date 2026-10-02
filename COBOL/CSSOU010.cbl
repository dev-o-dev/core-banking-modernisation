       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CSSOU010.
       AUTHOR.        P D QUINN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2013-06-24.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CSSOU010                                            *
      * TITLE    : CURRENT ACCOUNT SWITCH SERVICE - OUTBOUND MESSAGES  *
      *            AND OLD-BANK SWITCH PROCESSING                      *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CSSDLY01 STEP CS020 (AFTER CSSIN010)                *
      *----------------------------------------------------------------*
      * INPUT    : CIFMAST  - CIF MASTER (RANDOM)                      *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : SWITCHES - CASS SWITCH CONTROL (VSAM, DYNAMIC)      *
      *            ACCTMAST - DDA MASTER (RANDOM)                      *
      *            SOMAST   - STANDING ORDERS (DYNAMIC)                *
      *            DDMAND   - DIRECT DEBIT MANDATES (DYNAMIC)          *
      * OUTPUT   : CASSOUT  - TODAY'S MESSAGES TO CASS (CSSRECS)       *
      *            CSSPOST  - CLOSING BALANCE DEBITS FOR PSEOD01 (MOD) *
      *            REPORT   - CASS OUTBOUND REPORT (FBA 133)           *
      *----------------------------------------------------------------*
      * THE SWITCH TIMETABLE (CASS SCHEME RULES - 7 WORKING DAYS):     *
      *   DAY 1     NEW BANK SENDS SWRQ                                *
      *   DAY 2     OLD BANK ACCEPTS (SWAK) OR REJECTS (SWRJ)          *
      *   DAY 3-5   OLD BANK SENDS SOTR / DDTR FOR EVERY ACTIVE        *
      *             STANDING ORDER AND MANDATE                         *
      *   DAY 7     SWITCH DATE: OLD BANK SENDS BALT (CLOSING BALANCE) *
      *             AND SWCP, CLOSES THE ACCOUNT, REDIRECTS FOR 36     *
      *             MONTHS                                             *
      * THIS PROGRAM WALKS EVERY SWITCH RECORD AND SENDS WHATEVER IS   *
      * DUE FOR ITS STATUS:                                            *
      *  INCOMING ('I', MEGABANK = NEW BANK)                           *
      *   RQ, NOT YET SENT (CSW-REQUEST-DATE ZERO) -> SWRQ             *
      *  OUTGOING ('O', MEGABANK = OLD BANK)                           *
      *   RQ WITH A REJECT REASON -> SWRJ, STATUS RJ                   *
      *   RQ WITHOUT                -> SWAK, STATUS AK                 *
      *   AK AND WITHIN WS-TRANSFER-LEAD DAYS OF THE SWITCH DATE ->    *
      *      SOTR / DDTR FOR EACH ACTIVE SO / LIVE MANDATE; THE SO IS  *
      *      CANCELLED (REASON 'CASS') AND THE MANDATE MARKED 'T'      *
      *      TRANSFERRED WITH ADDACS SUPPRESSED (CASS INFORMS THE      *
      *      ORIGINATOR); STATUS TR                                    *
      *   TR AND THE SWITCH DATE HAS COME -> BALT + SWCP, DEBIT THE    *
      *      CLOSING BALANCE, REQUEST CLOSURE (DDCLS010 CLOSES THE     *
      *      ACCOUNT TOMORROW), STATUS RD, REDIRECT END = SWITCH DATE  *
      *      + 36 MONTHS                                               *
      *   RD AND THE REDIRECT PERIOD IS OVER -> STATUS EN              *
      * KNOWN ISSUE PIR 2013/207 OPEN: CREDITS ARRIVING FOR A SWITCHED *
      * ACCOUNT DURING REDIRECTION ARE RETURNED BY PYINB010 (ACCOUNT   *
      * CLOSED) INSTEAD OF BEING FORWARDED - CASS REDIRECTION IS DONE  *
      * BY THE SCHEME FOR BACS ONLY; FPS CREDITS ARE AFFECTED.         *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2013-06-24 PDQ  ORIGINAL VERSION (CASS GO-LIVE 16/09/2013)     *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SWITCHES-FILE    ASSIGN TO SWITCHES
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS SWF-KEY
                                   FILE STATUS IS WS-SWITCHES-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT SOMAST-FILE      ASSIGN TO SOMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS SOM-KEY
                                   FILE STATUS IS WS-SOMAST-STATUS.
           SELECT DDMAND-FILE      ASSIGN TO DDMAND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS DMM-KEY
                                   FILE STATUS IS WS-DDMAND-STATUS.
           SELECT CASSOUT-FILE     ASSIGN TO CASSOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CASSOUT-STATUS.
           SELECT CSSPOST-FILE     ASSIGN TO CSSPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CSSPOST-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SWITCHES-FILE.
       01  SWITCHES-RECORD.
           05  SWF-KEY                     PIC X(18).
           05  FILLER                      PIC X(232).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  SOMAST-FILE.
           COPY SOMSTREC.
       FD  DDMAND-FILE.
           COPY DMMNDREC.
       FD  CASSOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CASSOUT-RECORD                  PIC X(300).
       FD  CSSPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CSSPOST-RECORD                  PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CSSOU010'.
      *
       01  WS-CONSTANTS.
           05  WS-TRANSFER-LEAD            PIC S9(03) COMP-3 VALUE +4.
           05  WS-REDIRECT-MONTHS          PIC S9(03) COMP-3 VALUE +36.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00013000.
      *
       01  WS-FILE-STATUSES.
           05  WS-SWITCHES-STATUS          PIC X(02).
               88  WS-SWITCHES-OK              VALUE '00'.
               88  WS-SWITCHES-EOF             VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-SOMAST-STATUS            PIC X(02).
               88  WS-SOMAST-OK                VALUE '00'.
           05  WS-DDMAND-STATUS            PIC X(02).
               88  WS-DDMAND-OK                VALUE '00'.
           05  WS-CASSOUT-STATUS           PIC X(02).
           05  WS-CSSPOST-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-SWITCHES          VALUE 'Y'.
           05  WS-CHANGED-IND              PIC X(01).
               88  WS-SWITCH-CHANGED           VALUE 'Y'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-DAYS-TO-SWITCH           PIC S9(07) COMP-3.
           05  WS-ACTION                   PIC X(10).
           05  WS-NOTE                     PIC X(40).
           05  WS-MSGS-WRITTEN             PIC 9(09) VALUE ZERO.
           05  WS-FILE-SEQ                 PIC 9(06) VALUE 1.
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-SAVED-SWITCH-KEY         PIC X(18).
           05  WS-MA-DATE                  PIC 9(08).
           05  WS-MA-DATE-R REDEFINES WS-MA-DATE.
               10  WS-MA-CCYY              PIC 9(04).
               10  WS-MA-MM                PIC 9(02).
               10  WS-MA-DD                PIC 9(02).
           05  WS-MA-TOTAL                 PIC S9(07) COMP-3.
      *
           COPY CSSRECS.
      *
       01  WS-COUNTERS.
           05  WS-SWITCHES-READ            PIC S9(07) COMP-3 VALUE 0.
           05  WS-SWRQ-SENT                PIC S9(07) COMP-3 VALUE 0.
           05  WS-SWAK-SENT                PIC S9(07) COMP-3 VALUE 0.
           05  WS-SWRJ-SENT                PIC S9(07) COMP-3 VALUE 0.
           05  WS-SOTR-SENT                PIC S9(07) COMP-3 VALUE 0.
           05  WS-DDTR-SENT                PIC S9(07) COMP-3 VALUE 0.
           05  WS-BALT-SENT                PIC S9(07) COMP-3 VALUE 0.
           05  WS-ENDED                    PIC S9(07) COMP-3 VALUE 0.
           05  WS-BALT-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-OVERDRAWN                PIC S9(07) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CSSOU010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - CASS OUTBOUND / OLD BANK SWITCHES'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(20) VALUE 'SWITCH ID'.
           05  FILLER            PIC X(04) VALUE 'DIR'.
           05  FILLER            PIC X(04) VALUE 'ST'.
           05  FILLER            PIC X(17) VALUE 'OLD ACCOUNT'.
           05  FILLER            PIC X(10) VALUE 'SW DATE'.
           05  FILLER            PIC X(12) VALUE 'ACTION'.
           05  FILLER            PIC X(65) VALUE 'NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SWITCH-ID                PIC X(18).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DIR                      PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-STATUS                   PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-OLD-SC                   PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-OLD-ACCT                 PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SW-DATE                  PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
           05  FILLER                      PIC X(25) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY PSTXNREC.
      *
           COPY DATECARD.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-SWITCH THRU 2000-EXIT
               UNTIL WS-END-OF-SWITCHES
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CIFMAST-FILE
                       DATECARD-FILE
                I-O    SWITCHES-FILE
                       ACCTMAST-FILE
                       SOMAST-FILE
                       DDMAND-FILE
                OUTPUT CASSOUT-FILE
                       REPORT-FILE
                EXTEND CSSPOST-FILE
           IF NOT WS-SWITCHES-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-SOMAST-OK
           OR NOT WS-DDMAND-OK
           OR WS-CASSOUT-STATUS NOT = '00'
           OR WS-CSSPOST-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
           OR DTC-BUSINESS-DATE NOT NUMERIC
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           COMPUTE WS-BUS-DATE-INT =
                   FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING

      *---- FILE HEADER -----------------------------------------------*
           MOVE SPACES                     TO CSI-CASS-MESSAGE
           SET CSI-HEADER                  TO TRUE
           MOVE WS-BUS-DATE                TO CSI-FILE-DATE
           MOVE WS-FILE-SEQ                TO CSI-FILE-SEQ
           MOVE ZERO                       TO CSI-RECORD-COUNT
           WRITE CASSOUT-RECORD            FROM CSI-CASS-MESSAGE

           MOVE LOW-VALUES                 TO SWF-KEY
           START SWITCHES-FILE KEY NOT < SWF-KEY
           IF NOT WS-SWITCHES-OK
               SET WS-END-OF-SWITCHES      TO TRUE
           ELSE
               PERFORM 8000-READ-SWITCH
           END-IF
           .
      *================================================================*
       2000-PROCESS-SWITCH.
      *================================================================*
           MOVE 'N'                        TO WS-CHANGED-IND
           MOVE SPACES                     TO WS-ACTION WS-NOTE
           IF CSW-COMPLETE OR CSW-ENDED OR CSW-REJECTED
               PERFORM 8000-READ-SWITCH
               GO TO 2000-EXIT
           END-IF
           MOVE CSW-SWITCH-ID              TO WS-SAVED-SWITCH-KEY
           IF CSW-SWITCH-DATE NUMERIC AND CSW-SWITCH-DATE > ZERO
               COMPUTE WS-DAYS-TO-SWITCH =
                   FUNCTION INTEGER-OF-DATE (CSW-SWITCH-DATE)
                   - WS-BUS-DATE-INT
           ELSE
               MOVE 999                    TO WS-DAYS-TO-SWITCH
           END-IF

           IF CSW-INCOMING
               IF CSW-REQUESTED AND CSW-REQUEST-DATE = ZERO
                   PERFORM 3000-SEND-SWRQ
               END-IF
           ELSE
               EVALUATE TRUE
                   WHEN CSW-REQUESTED
                       PERFORM 4000-ANSWER-REQUEST
                   WHEN CSW-ACCEPTED
                   AND  WS-DAYS-TO-SWITCH <= WS-TRANSFER-LEAD
                       PERFORM 5000-TRANSFER-ARRANGEMENTS
                   WHEN CSW-TRANSFERRING
                   AND  WS-DAYS-TO-SWITCH <= ZERO
                       PERFORM 6000-CLOSE-OLD-ACCOUNT THRU 6000-EXIT
                   WHEN CSW-REDIRECTING
                   AND  CSW-REDIRECT-END < WS-BUS-DATE
                       SET CSW-ENDED       TO TRUE
                       SET WS-SWITCH-CHANGED TO TRUE
                       ADD 1               TO WS-ENDED
                       MOVE 'ENDED'        TO WS-ACTION
                       MOVE 'REDIRECTION PERIOD OVER' TO WS-NOTE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF

           IF WS-SWITCH-CHANGED
      *        THE SO / DD BROWSES MOVED OTHER FILES ONLY - THE SWITCH
      *        RECORD AREA IS STILL CURRENT FOR THE REWRITE.
               MOVE WS-BUS-DATE            TO CSW-LAST-MAINT-DATE
               MOVE WS-PROGRAM-ID          TO CSW-LAST-MAINT-PGM
               REWRITE SWITCHES-RECORD     FROM CSW-SWITCH-RECORD
               IF NOT WS-SWITCHES-OK
                   MOVE '2000-PROCESS-SWITCH' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SWITCHES REWRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-SWITCHES-STATUS TO ABN-FILE-STATUS
                   MOVE WS-SAVED-SWITCH-KEY TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
           END-IF
           IF WS-ACTION NOT = SPACES
               PERFORM 7100-REPORT-DETAIL
           END-IF
           PERFORM 8000-READ-SWITCH
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-SEND-SWRQ.
      *================================================================*
           PERFORM 7500-INIT-MESSAGE
           SET CSI-SWITCH-REQUEST          TO TRUE
           MOVE CSW-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           IF WS-CIFMAST-OK
               MOVE CIF-LAST-NM            TO CSI-CUST-SURNAME
               MOVE CIF-FIRST-NM (1:1)     TO CSI-CUST-INITIALS (1:1)
               MOVE CIF-MIDDLE-NM (1:1)    TO CSI-CUST-INITIALS (2:1)
               MOVE CIF-DOB                TO CSI-CUST-DOB
               MOVE CIF-POST-CD            TO CSI-CUST-POST-CD
           ELSE
               MOVE ZERO                   TO CSI-CUST-DOB
               MOVE 'CIF NOT FOUND - REQUEST SENT WITHOUT DETAILS'
                                           TO WS-NOTE
           END-IF
           MOVE 'Y'                        TO CSI-FULL-SWITCH
           PERFORM 7600-WRITE-MESSAGE
           MOVE WS-BUS-DATE                TO CSW-REQUEST-DATE
           SET WS-SWITCH-CHANGED           TO TRUE
           ADD 1                           TO WS-SWRQ-SENT
           MOVE 'SWRQ SENT'                TO WS-ACTION
           .
      *================================================================*
       4000-ANSWER-REQUEST.
      *================================================================*
           PERFORM 7500-INIT-MESSAGE
           IF CSW-REJECT-REASON NOT = SPACES
               SET CSI-SWITCH-REJECTED     TO TRUE
               MOVE CSW-REJECT-REASON      TO CSI-REASON
               SET CSW-REJECTED            TO TRUE
               ADD 1                       TO WS-SWRJ-SENT
               MOVE 'SWRJ SENT'            TO WS-ACTION
               MOVE CSW-REJECT-REASON      TO WS-NOTE
           ELSE
               SET CSI-SWITCH-ACCEPTED     TO TRUE
               SET CSW-ACCEPTED            TO TRUE
               MOVE WS-BUS-DATE            TO CSW-ACCEPT-DATE
               ADD 1                       TO WS-SWAK-SENT
               MOVE 'SWAK SENT'            TO WS-ACTION
           END-IF
           PERFORM 7600-WRITE-MESSAGE
           SET WS-SWITCH-CHANGED           TO TRUE
           .
      *================================================================*
       5000-TRANSFER-ARRANGEMENTS.
      *================================================================*
           MOVE ZERO                       TO CSW-SO-COUNT CSW-DD-COUNT
      *---- STANDING ORDERS -------------------------------------------*
           MOVE CSW-OLD-SORT-CODE          TO SOM-PAYER-SORT-CODE
           MOVE CSW-OLD-ACCT-NO            TO SOM-PAYER-ACCT-NO
           MOVE 1                          TO SOM-SO-SEQ
           START SOMAST-FILE KEY NOT < SOM-KEY
           MOVE 'N'                        TO WS-END-BROWSE-IND
           IF NOT WS-SOMAST-OK
               SET WS-END-BROWSE           TO TRUE
           END-IF
           PERFORM UNTIL WS-END-BROWSE
               READ SOMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-SOMAST-OK
                   WHEN SOM-PAYER-SORT-CODE NOT = CSW-OLD-SORT-CODE
                   WHEN SOM-PAYER-ACCT-NO NOT = CSW-OLD-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN SOM-CONTROL-REC
                       CONTINUE
                   WHEN SOM-ACTIVE OR SOM-SUSPENDED
                       PERFORM 5100-SEND-SOTR
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM

      *---- DIRECT DEBIT MANDATES -------------------------------------*
           MOVE LOW-VALUES                 TO DMM-KEY
           MOVE CSW-OLD-SORT-CODE          TO DMM-PAYER-SORT-CODE
           MOVE CSW-OLD-ACCT-NO            TO DMM-PAYER-ACCT-NO
           START DDMAND-FILE KEY NOT < DMM-KEY
           MOVE 'N'                        TO WS-END-BROWSE-IND
           IF NOT WS-DDMAND-OK
               SET WS-END-BROWSE           TO TRUE
           END-IF
           PERFORM UNTIL WS-END-BROWSE
               READ DDMAND-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-DDMAND-OK
                   WHEN DMM-PAYER-SORT-CODE NOT = CSW-OLD-SORT-CODE
                   WHEN DMM-PAYER-ACCT-NO NOT = CSW-OLD-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN DMM-LIVE OR DMM-DORMANT
                       PERFORM 5200-SEND-DDTR
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM

           SET CSW-TRANSFERRING            TO TRUE
           SET WS-SWITCH-CHANGED           TO TRUE
           MOVE 'TRANSFER'                 TO WS-ACTION
           STRING CSW-SO-COUNT ' SO / ' CSW-DD-COUNT ' DD SENT'
                  DELIMITED BY SIZE
                  INTO WS-NOTE
           END-STRING
           .
      *================================================================*
       5100-SEND-SOTR.
      *================================================================*
           PERFORM 7500-INIT-MESSAGE
           SET CSI-SO-TRANSFER             TO TRUE
           MOVE SOM-BENE-NAME              TO CSI-SO-BENE-NAME
           MOVE SOM-BENE-SORT-CODE         TO CSI-SO-BENE-SORT-CODE
           MOVE SOM-BENE-ACCT-NO           TO CSI-SO-BENE-ACCT-NO
           MOVE SOM-BENE-REF               TO CSI-SO-BENE-REF
           MOVE SOM-FREQ-CD                TO CSI-SO-FREQ-CD
           MOVE SOM-NEXT-DUE-DATE          TO CSI-SO-NEXT-DATE
           MOVE SOM-REGULAR-AMT            TO CSI-SO-AMOUNT
           IF SOM-UNTIL-FURTHER-NOTICE
               MOVE ZERO                   TO CSI-SO-FINAL-DATE
           ELSE
               MOVE SOM-FINAL-PAY-DATE     TO CSI-SO-FINAL-DATE
           END-IF
           PERFORM 7600-WRITE-MESSAGE
      *    THE ORDER NOW RUNS AT THE NEW BANK - CANCEL IT HERE.
           SET SOM-CANCELLED               TO TRUE
           MOVE WS-BUS-DATE                TO SOM-CANCEL-DATE
                                              SOM-AMEND-DATE
           MOVE 'CASS'                     TO SOM-CANCEL-RSN
           MOVE 'CASS  '                   TO SOM-AMEND-CHANNEL
           MOVE WS-PROGRAM-ID              TO SOM-AMEND-USER
           REWRITE SOM-SO-RECORD
           ADD 1                           TO CSW-SO-COUNT
                                              WS-SOTR-SENT
           .
      *================================================================*
       5200-SEND-DDTR.
      *================================================================*
           PERFORM 7500-INIT-MESSAGE
           SET CSI-DD-TRANSFER             TO TRUE
           MOVE DMM-ORIG-SUN               TO CSI-DD-ORIG-SUN
           MOVE DMM-ORIG-NAME              TO CSI-DD-ORIG-NAME
           MOVE DMM-PAYER-REF              TO CSI-DD-PAYER-REF
           MOVE DMM-LAST-COLL-DATE         TO CSI-DD-LAST-COLL-DATE
           MOVE DMM-LAST-COLL-AMT          TO CSI-DD-LAST-COLL-AMT
           PERFORM 7600-WRITE-MESSAGE
      *    TRANSFERRED - CASS TELLS THE ORIGINATOR, SO DMADC010 MUST
      *    NOT SEND AN ADDACS (SENT DATE SET NOW).
           SET DMM-TRANSFERRED             TO TRUE
           MOVE WS-BUS-DATE                TO DMM-CANCEL-DATE
                                              DMM-ADDACS-SENT-DATE
                                              DMM-LAST-MAINT-DATE
           MOVE 'CASS  '                   TO DMM-CANCEL-CHANNEL
           MOVE WS-PROGRAM-ID              TO DMM-CANCEL-USER
                                              DMM-LAST-MAINT-PGM
           MOVE '3'                        TO DMM-ADDACS-REASON
           REWRITE DMM-MANDATE-RECORD
           ADD 1                           TO CSW-DD-COUNT
                                              WS-DDTR-SENT
           .
      *================================================================*
       6000-CLOSE-OLD-ACCOUNT.
      *================================================================*
           MOVE CSW-OLD-SORT-CODE          TO ACM-SORT-CODE
           MOVE CSW-OLD-ACCT-NO            TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF NOT WS-ACCTMAST-OK
               MOVE 'ERROR'                TO WS-ACTION
               MOVE 'OLD ACCOUNT NOT FOUND AT SWITCH DATE' TO WS-NOTE
               GO TO 6000-EXIT
           END-IF

      *---- CLOSING BALANCE ------------------------------------------*
           PERFORM 7500-INIT-MESSAGE
           SET CSI-BALANCE-TRANSFER        TO TRUE
           IF ACM-LEDGER-BAL < ZERO
               COMPUTE CSI-BAL-AMOUNT = ZERO - ACM-LEDGER-BAL
               MOVE '-'                    TO CSI-BAL-SIGN
               ADD 1                       TO WS-OVERDRAWN
               MOVE 'OVERDRAWN - DEBT STAYS HERE, ACCOUNT RESTRICTED'
                                           TO WS-NOTE
           ELSE
               MOVE ACM-LEDGER-BAL         TO CSI-BAL-AMOUNT
               MOVE '+'                    TO CSI-BAL-SIGN
           END-IF
           PERFORM 7600-WRITE-MESSAGE
           ADD 1                           TO WS-BALT-SENT

           IF ACM-LEDGER-BAL > ZERO
               MOVE ACM-LEDGER-BAL         TO WS-AMOUNT
               MOVE WS-AMOUNT              TO CSW-BALANCE-AMT
               ADD WS-AMOUNT               TO WS-BALT-TOTAL
               MOVE SPACES                 TO PST-POSTING-RECORD
               MOVE CSW-OLD-SORT-CODE      TO PST-SORT-CODE
               MOVE CSW-OLD-ACCT-NO        TO PST-ACCT-NO
               MOVE WS-BUS-DATE            TO PST-POST-DATE
                                              PST-VALUE-DATE
               MOVE WS-POST-TIME-BASE      TO PST-POST-TIME
               ADD 1                       TO WS-POST-SEQ
               MOVE WS-POST-SEQ            TO PST-SEQ-NO
               MOVE 'TFR '                 TO PST-TXN-CODE
               SET PST-DEBIT               TO TRUE
               MOVE WS-AMOUNT              TO PST-AMOUNT
               MOVE 'GBP'                  TO PST-CCY-CD
               MOVE 'CURRENT ACCOUNT SWITCH' TO PST-NARRATIVE-1
               STRING 'TO ' CSW-NEW-SORT-CODE ' ' CSW-NEW-ACCT-NO
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
               MOVE 'BATCH '               TO PST-CHANNEL-CD
               MOVE CSW-SWITCH-ID          TO PST-EXT-REF
               SET PST-HARD-POST           TO TRUE
               SET PST-PENDING             TO TRUE
               MOVE WS-PROGRAM-ID          TO PST-USER-ID
               WRITE CSSPOST-RECORD        FROM PST-POSTING-RECORD
           END-IF

      *---- CLOSE (OR RESTRICT IF OVERDRAWN) -------------------------*
           IF ACM-LEDGER-BAL < ZERO
               SET ACM-NO-DEBITS           TO TRUE
               MOVE ZERO                   TO ACM-OD-LIMIT
           ELSE
               SET ACM-PENDING-CLOSE       TO TRUE
               MOVE WS-BUS-DATE            TO ACM-CLOSE-REQ-DATE
               MOVE 'CASS'                 TO ACM-CLOSE-RSN
               MOVE CSW-NEW-SORT-CODE      TO ACM-CLOSE-XFER-SORT-CODE
               MOVE CSW-NEW-ACCT-NO        TO ACM-CLOSE-XFER-ACCT-NO
           END-IF
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER
           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '6000-CLOSE-OLD-ACCOUNT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

      *---- SWITCH COMPLETE ------------------------------------------*
           PERFORM 7500-INIT-MESSAGE
           SET CSI-SWITCH-COMPLETE         TO TRUE
           PERFORM 7600-WRITE-MESSAGE

           SET CSW-REDIRECTING             TO TRUE
           MOVE WS-BUS-DATE                TO CSW-COMPLETE-DATE
           MOVE CSW-SWITCH-DATE            TO WS-MA-DATE
           COMPUTE WS-MA-TOTAL = WS-MA-CCYY * 12 + WS-MA-MM - 1
                               + WS-REDIRECT-MONTHS
           COMPUTE WS-MA-CCYY = WS-MA-TOTAL / 12
           COMPUTE WS-MA-MM = WS-MA-TOTAL - WS-MA-CCYY * 12 + 1
           IF WS-MA-DD > 28
               MOVE 28                     TO WS-MA-DD
           END-IF
           MOVE WS-MA-DATE                 TO CSW-REDIRECT-END
           SET WS-SWITCH-CHANGED           TO TRUE
           MOVE 'SWITCHED'                 TO WS-ACTION
           .
       6000-EXIT.
           EXIT.
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= 55
               PERFORM 7200-REPORT-HEADINGS
           END-IF
           MOVE CSW-SWITCH-ID              TO DL-SWITCH-ID
           MOVE CSW-DIRECTION              TO DL-DIR
           MOVE CSW-STATUS                 TO DL-STATUS
           MOVE CSW-OLD-SORT-CODE          TO DL-OLD-SC
           MOVE CSW-OLD-ACCT-NO            TO DL-OLD-ACCT
           MOVE CSW-SWITCH-DATE            TO DL-SW-DATE
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           WRITE REPORT-RECORD             FROM WS-HEADING-2
           MOVE 3                          TO WS-LINE-COUNT
           .
      *================================================================*
       7500-INIT-MESSAGE.
      *================================================================*
           MOVE SPACES                     TO CSI-CASS-MESSAGE
           MOVE CSW-SWITCH-ID              TO CSI-SWITCH-ID
           MOVE CSW-SWITCH-DATE            TO CSI-SWITCH-DATE
           MOVE CSW-OLD-SORT-CODE          TO CSI-OLD-SORT-CODE
           MOVE CSW-OLD-ACCT-NO            TO CSI-OLD-ACCT-NO
           MOVE CSW-NEW-SORT-CODE          TO CSI-NEW-SORT-CODE
           MOVE CSW-NEW-ACCT-NO            TO CSI-NEW-ACCT-NO
           .
      *================================================================*
       7600-WRITE-MESSAGE.
      *================================================================*
           WRITE CASSOUT-RECORD            FROM CSI-CASS-MESSAGE
           IF WS-CASSOUT-STATUS NOT = '00'
               MOVE '7600-WRITE-MESSAGE'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CASSOUT WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CASSOUT-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-MSGS-WRITTEN
           .
      *================================================================*
       8000-READ-SWITCH.
      *================================================================*
           READ SWITCHES-FILE NEXT RECORD INTO CSW-SWITCH-RECORD
           EVALUATE TRUE
               WHEN WS-SWITCHES-OK
                   ADD 1                   TO WS-SWITCHES-READ
               WHEN WS-SWITCHES-EOF
                   SET WS-END-OF-SWITCHES  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-SWITCH' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SWITCHES READ FAILED' TO ABN-MESSAGE
                   MOVE WS-SWITCHES-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           MOVE SPACES                     TO CSI-CASS-MESSAGE
           SET CSI-TRAILER                 TO TRUE
           MOVE WS-BUS-DATE                TO CSI-FILE-DATE
           MOVE WS-FILE-SEQ                TO CSI-FILE-SEQ
           MOVE WS-MSGS-WRITTEN            TO CSI-RECORD-COUNT
           WRITE CASSOUT-RECORD            FROM CSI-CASS-MESSAGE

           PERFORM 7200-REPORT-HEADINGS
           MOVE 'SWITCH RECORDS READ'      TO TL-LABEL
           MOVE WS-SWITCHES-READ           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SWRQ SENT (WE ARE NEW BANK)' TO TL-LABEL
           MOVE WS-SWRQ-SENT               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SWAK SENT'                TO TL-LABEL
           MOVE WS-SWAK-SENT               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SWRJ SENT'                TO TL-LABEL
           MOVE WS-SWRJ-SENT               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'STANDING ORDERS TRANSFERRED' TO TL-LABEL
           MOVE WS-SOTR-SENT               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MANDATES TRANSFERRED'     TO TL-LABEL
           MOVE WS-DDTR-SENT               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'BALANCES SENT'            TO TL-LABEL
           MOVE WS-BALT-SENT               TO TL-COUNT
           MOVE WS-BALT-TOTAL              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH OVERDRAWN'     TO TL-LABEL
           MOVE WS-OVERDRAWN               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REDIRECTIONS ENDED'       TO TL-LABEL
           MOVE WS-ENDED                   TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MESSAGES WRITTEN'         TO TL-LABEL
           MOVE WS-MSGS-WRITTEN            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE SWITCHES-FILE ACCTMAST-FILE CIFMAST-FILE SOMAST-FILE
                 DDMAND-FILE CASSOUT-FILE CSSPOST-FILE REPORT-FILE
           DISPLAY 'CSSOU010 - MESSAGES ' WS-MSGS-WRITTEN
           IF WS-OVERDRAWN > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
