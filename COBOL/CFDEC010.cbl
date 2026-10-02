       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFDEC010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-02-14.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFDEC010                                            *
      * TITLE    : BEREAVEMENT - DECEASED CUSTOMER PROCESSING          *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CFDEC01 (DAILY 19:00, BEFORE THE EOD SUITE)         *
      *----------------------------------------------------------------*
      * INPUT    : BEREAVE  - BEREAVEMENT TEAM INSTRUCTIONS (FB 200)   *
      *                       KEYED ON THE BEREAVEMENT WORKFLOW TOOL   *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : CIFMAST  - CIF MASTER (RANDOM)                      *
      *            ACCTMAST - DDA MASTER (DYNAMIC, ALTERNATE KEY CIF)  *
      *            SOMAST   - STANDING ORDERS (DYNAMIC)                *
      *            DDMAND   - DIRECT DEBIT MANDATES (DYNAMIC)          *
      *            LOANMAST - LOANS (DYNAMIC, ALTERNATE KEY CIF)       *
      * OUTPUT   : DECPOST  - PAY-OUT TRANSFERS TO MEGABANK EXECUTOR   *
      *                       ACCOUNTS FOR PSEOD01 (PSTXNREC)          *
      *            PAYOUT   - PAY-OUTS TO OTHER BANKS - KEYED BY THE   *
      *                       BEREAVEMENT TEAM AS CHAPS (FB 200)       *
      *            LETTERS  - LETTERS TO THE NOTIFIER / EXECUTOR       *
      *            REPORT   - BEREAVEMENT CONTROL REPORT (FBA 133)     *
      *----------------------------------------------------------------*
      * INSTRUCTION TYPES (BRV-ACTION):                                *
      *  N  NOTIFICATION OF DEATH                                      *
      *     - CIF STATUS DECEASED, DATE OF DEATH, BEREAVEMENT REF      *
      *     - EVERY OPEN ACCOUNT: NO DEBITS (CREDITS STILL ACCEPTED -  *
      *       PENSIONS ARE RECLAIMED BY DWP SEPARATELY)                *
      *     - STANDING ORDERS CANCELLED (REASON 'DCSD')                *
      *     - DIRECT DEBIT MANDATES CANCELLED, ADDACS REASON '2'       *
      *       (PAYER DECEASED) - SENT BY DMADC010 TONIGHT              *
      *     - LOANS: COLLECTIONS PAUSED FOR 90 DAYS (FORBEARANCE WITH  *
      *       ZERO PAYMENT); INTEREST CONTINUES UNLESS BRV-FREEZE-IND  *
      *     - LETTER 'BRV1' WITH BALANCES AT DATE OF DEATH             *
      *  P  PROBATE / LETTERS OF ADMINISTRATION RECEIVED               *
      *     - EVERY ACCOUNT WITH A CREDIT BALANCE IS PAID TO THE       *
      *       EXECUTOR ACCOUNT AND CLOSURE REQUESTED (DDCLS010)        *
      *     - ACCOUNTS IN DEBIT AND LOANS ARE LISTED FOR THE CLAIM     *
      *       AGAINST THE ESTATE (NOT CLOSED)                          *
      *     - SMALL ESTATES (TOTAL CREDIT BALANCES UNDER WS-SMALL-     *
      *       ESTATE) MAY BE RELEASED WITHOUT PROBATE ON AN INDEMNITY  *
      *       - THE TEAM SENDS 'P' WITH BRV-INDEMNITY-IND 'Y'          *
      *     - LETTER 'BRV2'                                            *
      *  X  ESTATE SETTLED - CIF CLOSED IF NO OPEN ACCOUNTS OR LOANS   *
      *  R  REVERSAL - NOTIFICATION IN ERROR (WRONG CUSTOMER). CIF     *
      *     BACK TO ACTIVE, DEBIT RESTRICTIONS LIFTED. CANCELLED SOS / *
      *     MANDATES ARE NOT REINSTATED - THE TEAM PHONES THE CUSTOMER *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-02-14 AOS  ORIGINAL VERSION (REPLACES THE BRANCH          *
      *                 BEREAVEMENT CHECKLIST - FOS COMPLAINT THEMES)  *
      * 2013-09-02 PDQ  LOAN COLLECTIONS PAUSE                         *
      * 2015-06-08 PDQ  SMALL ESTATE LIMIT RAISED 15,000 -> 25,000     *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BEREAVE-FILE     ASSIGN TO BEREAVE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-BEREAVE-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS ACM-KEY
                                   ALTERNATE RECORD KEY IS ACM-CIF-NO
                                       WITH DUPLICATES
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
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
           SELECT LOANMAST-FILE    ASSIGN TO LOANMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS LN-KEY
                                   ALTERNATE RECORD KEY IS LN-CIF-NO
                                       WITH DUPLICATES
                                   FILE STATUS IS WS-LOANMAST-STATUS.
           SELECT DECPOST-FILE     ASSIGN TO DECPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DECPOST-STATUS.
           SELECT PAYOUT-FILE      ASSIGN TO PAYOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PAYOUT-STATUS.
           SELECT LETTERS-FILE     ASSIGN TO LETTERS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LETTERS-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  BEREAVE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BEREAVE-RECORD                  PIC X(200).
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  SOMAST-FILE.
           COPY SOMSTREC.
       FD  DDMAND-FILE.
           COPY DMMNDREC.
       FD  LOANMAST-FILE.
           COPY LNACCREC.
       FD  DECPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DECPOST-RECORD                  PIC X(200).
       FD  PAYOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PAYOUT-RECORD                   PIC X(200).
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(250).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFDEC010'.
      *
       01  WS-CONSTANTS.
           05  WS-SMALL-ESTATE             PIC S9(13)V99 COMP-3
                                           VALUE +25000.00.
           05  WS-LOAN-PAUSE-DAYS          PIC S9(03) COMP-3 VALUE +90.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00010000.
      *
      *---- BEREAVEMENT INSTRUCTION -----------------------------------*
       01  BRV-INSTRUCTION.
           05  BRV-ACTION                  PIC X(01).
               88  BRV-NOTIFY                  VALUE 'N'.
               88  BRV-PROBATE                 VALUE 'P'.
               88  BRV-SETTLED                 VALUE 'X'.
               88  BRV-REVERSE                 VALUE 'R'.
           05  BRV-CIF-NO                  PIC X(12).
           05  BRV-REF                     PIC X(12).
           05  BRV-DATE-OF-DEATH           PIC 9(08).
           05  BRV-NOTIFIER-NAME           PIC X(40).
           05  BRV-NOTIFIER-RELATION       PIC X(10).
           05  BRV-EXEC-SORT-CODE          PIC X(06).
           05  BRV-EXEC-ACCT-NO            PIC X(08).
           05  BRV-EXEC-NAME               PIC X(35).
           05  BRV-INDEMNITY-IND           PIC X(01).
           05  BRV-FREEZE-INT-IND          PIC X(01).
           05  BRV-USER-ID                 PIC X(08).
           05  FILLER                      PIC X(58).
      *
       01  WS-FILE-STATUSES.
           05  WS-BEREAVE-STATUS           PIC X(02).
               88  WS-BEREAVE-OK               VALUE '00'.
               88  WS-BEREAVE-EOF              VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00' '02'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-SOMAST-STATUS            PIC X(02).
               88  WS-SOMAST-OK                VALUE '00'.
           05  WS-DDMAND-STATUS            PIC X(02).
               88  WS-DDMAND-OK                VALUE '00'.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00' '02'.
               88  WS-LOANMAST-NOTFND          VALUE '23'.
           05  WS-DECPOST-STATUS           PIC X(02).
           05  WS-PAYOUT-STATUS            PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-EXEC-INTERNAL-IND        PIC X(01).
               88  WS-EXEC-INTERNAL            VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-REJECT-TEXT              PIC X(50).
           05  WS-ACCT-COUNT               PIC S9(04) COMP.
           05  WS-OPEN-COUNT               PIC S9(04) COMP.
           05  WS-SO-COUNT                 PIC S9(04) COMP.
           05  WS-DD-COUNT                 PIC S9(04) COMP.
           05  WS-LOAN-COUNT               PIC S9(04) COMP.
           05  WS-CREDIT-TOTAL             PIC S9(13)V99 COMP-3.
           05  WS-DEBIT-TOTAL              PIC S9(13)V99 COMP-3.
           05  WS-LOAN-TOTAL               PIC S9(13)V99 COMP-3.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-SAVED-ACCT-KEY           PIC X(14).
           05  WS-ACCT-KEYS.
               10  WS-ACCT-KEY-ENTRY       PIC X(14) OCCURS 50 TIMES.
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-EXEC-KEY.
               10  WS-EXEC-SORT-CODE       PIC X(06).
               10  WS-EXEC-ACCT-NO         PIC X(08).
      *
      *---- LETTER RECORD ---------------------------------------------*
       01  WS-LETTER.
           05  LT-LETTER-CD                PIC X(04).
           05  LT-ISSUE-DATE               PIC 9(08).
           05  LT-BEREAVE-REF              PIC X(12).
           05  LT-CIF-NO                   PIC X(12).
           05  LT-DECEASED-NAME            PIC X(60).
           05  LT-DATE-OF-DEATH            PIC 9(08).
           05  LT-ADDRESSEE                PIC X(40).
           05  LT-ACCOUNT-COUNT            PIC 9(03).
           05  LT-CREDIT-TOTAL             PIC 9(11)V99.
           05  LT-DEBIT-TOTAL              PIC 9(11)V99.
           05  LT-LOAN-TOTAL               PIC 9(11)V99.
           05  LT-SO-CANCELLED             PIC 9(03).
           05  LT-DD-CANCELLED             PIC 9(03).
           05  LT-EXEC-SORT-CODE           PIC X(06).
           05  LT-EXEC-ACCT-NO             PIC X(08).
           05  FILLER                      PIC X(37).
      *
      *---- EXTERNAL PAY-OUT INSTRUCTION ------------------------------*
       01  WS-PAYOUT.
           05  PO-BEREAVE-REF              PIC X(12).
           05  PO-FROM-SORT-CODE           PIC X(06).
           05  PO-FROM-ACCT-NO             PIC X(08).
           05  PO-AMOUNT                   PIC 9(11)V99.
           05  PO-TO-SORT-CODE             PIC X(06).
           05  PO-TO-ACCT-NO               PIC X(08).
           05  PO-TO-NAME                  PIC X(35).
           05  PO-DATE                     PIC 9(08).
           05  PO-SCHEME                   PIC X(04) VALUE 'CHAP'.
           05  FILLER                      PIC X(100).
      *
       01  WS-COUNTERS.
           05  WS-INSTR-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOTIFICATIONS            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PROBATES                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-SETTLEMENTS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-REVERSALS                PIC S9(09) COMP-3 VALUE 0.
           05  WS-REJECTED                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOT-ACCTS-STOPPED        PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOT-SO-CANCELLED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOT-DD-CANCELLED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOT-LOANS-PAUSED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOT-PAID-INTERNAL        PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOT-PAID-EXTERNAL        PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CFDEC010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - BEREAVEMENT PROCESSING'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(04) VALUE 'ACT'.
           05  FILLER            PIC X(14) VALUE 'CUSTOMER'.
           05  FILLER            PIC X(14) VALUE 'BEREAVE REF'.
           05  FILLER            PIC X(06) VALUE 'ACCTS'.
           05  FILLER            PIC X(04) VALUE 'SO'.
           05  FILLER            PIC X(04) VALUE 'DD'.
           05  FILLER            PIC X(06) VALUE 'LOANS'.
           05  FILLER            PIC X(16) VALUE '        CREDITS'.
           05  FILLER            PIC X(16) VALUE '         DEBITS'.
           05  FILLER            PIC X(48) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-ACTION                   PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-REF                      PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACCTS                    PIC ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-SO                       PIC ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DD                       PIC ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-LOANS                    PIC ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CREDITS                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DEBITS                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(50).
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
           PERFORM 2000-PROCESS-INSTRUCTION THRU 2000-EXIT
               UNTIL WS-END-OF-INPUT
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  BEREAVE-FILE
                       DATECARD-FILE
                I-O    CIFMAST-FILE
                       ACCTMAST-FILE
                       SOMAST-FILE
                       DDMAND-FILE
                       LOANMAST-FILE
                OUTPUT DECPOST-FILE
                       PAYOUT-FILE
                       LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-BEREAVE-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-SOMAST-OK
           OR NOT WS-DDMAND-OK
           OR NOT WS-LOANMAST-OK
           OR WS-DECPOST-STATUS NOT = '00'
           OR WS-PAYOUT-STATUS NOT = '00'
           OR WS-LETTERS-STATUS NOT = '00'
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
           PERFORM 8000-READ-INSTRUCTION
           .
      *================================================================*
       2000-PROCESS-INSTRUCTION.
      *================================================================*
           MOVE SPACES                     TO WS-REJECT-TEXT
           MOVE ZERO                       TO WS-ACCT-COUNT
                                              WS-OPEN-COUNT
                                              WS-SO-COUNT WS-DD-COUNT
                                              WS-LOAN-COUNT
                                              WS-CREDIT-TOTAL
                                              WS-DEBIT-TOTAL
                                              WS-LOAN-TOTAL

           MOVE BRV-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-CIFMAST-OK
                   MOVE 'CUSTOMER NOT ON CIF' TO WS-REJECT-TEXT
               WHEN NOT (BRV-NOTIFY OR BRV-PROBATE
                         OR BRV-SETTLED OR BRV-REVERSE)
                   MOVE 'UNKNOWN ACTION CODE' TO WS-REJECT-TEXT
               WHEN BRV-NOTIFY AND CIF-DECEASED
                   MOVE 'ALREADY NOTIFIED' TO WS-REJECT-TEXT
               WHEN BRV-NOTIFY
               AND (BRV-DATE-OF-DEATH NOT NUMERIC
                    OR BRV-DATE-OF-DEATH > WS-BUS-DATE)
                   MOVE 'DATE OF DEATH INVALID' TO WS-REJECT-TEXT
               WHEN NOT BRV-NOTIFY AND NOT CIF-DECEASED
                   MOVE 'CUSTOMER NOT RECORDED AS DECEASED'
                                           TO WS-REJECT-TEXT
               WHEN NOT BRV-NOTIFY AND BRV-REF NOT = CIF-BEREAVE-REF
                   MOVE 'BEREAVEMENT REFERENCE DOES NOT MATCH'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-TEXT NOT = SPACES
               ADD 1                       TO WS-REJECTED
               PERFORM 7100-REPORT-DETAIL
               PERFORM 8000-READ-INSTRUCTION
               GO TO 2000-EXIT
           END-IF

           EVALUATE TRUE
               WHEN BRV-NOTIFY
                   PERFORM 2100-NOTIFICATION
               WHEN BRV-PROBATE
                   PERFORM 2500-PROBATE THRU 2500-EXIT
               WHEN BRV-SETTLED
                   PERFORM 2800-SETTLED THRU 2800-EXIT
               WHEN BRV-REVERSE
                   PERFORM 2900-REVERSE
           END-EVALUATE
           PERFORM 7100-REPORT-DETAIL
           PERFORM 8000-READ-INSTRUCTION
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-NOTIFICATION.
      *================================================================*
           SET CIF-DECEASED                TO TRUE
           MOVE BRV-DATE-OF-DEATH          TO CIF-DATE-OF-DEATH
           MOVE BRV-REF                    TO CIF-BEREAVE-REF
           SET CIF-BRV-ACCOUNTS-STOPPED    TO TRUE
           MOVE 'N'                        TO CIF-MKTG-EMAIL-IND
                                              CIF-MKTG-POST-IND
           PERFORM 6000-REWRITE-CIF

           PERFORM 5000-COLLECT-ACCOUNTS THRU 5000-EXIT
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-ACCT-COUNT
               MOVE WS-ACCT-KEY-ENTRY (WS-SUB) TO ACM-KEY
               READ ACCTMAST-FILE
               IF WS-ACCTMAST-OK AND NOT ACM-CLOSED
                   PERFORM 2200-STOP-ACCOUNT
                   PERFORM 2300-CANCEL-SOS THRU 2300-EXIT
                   PERFORM 2400-CANCEL-MANDATES THRU 2400-EXIT
               END-IF
           END-PERFORM
           PERFORM 2450-PAUSE-LOANS THRU 2450-EXIT

           MOVE 'BRV1'                     TO LT-LETTER-CD
           MOVE BRV-NOTIFIER-NAME          TO LT-ADDRESSEE
           PERFORM 6200-WRITE-LETTER
           ADD 1                           TO WS-NOTIFICATIONS
           MOVE 'NOTIFIED - ACCOUNTS STOPPED' TO WS-REJECT-TEXT
           .
      *================================================================*
       2200-STOP-ACCOUNT.
      *================================================================*
           ADD 1                           TO WS-OPEN-COUNT
           IF ACM-LEDGER-BAL >= ZERO
               ADD ACM-LEDGER-BAL          TO WS-CREDIT-TOTAL
           ELSE
               SUBTRACT ACM-LEDGER-BAL     FROM WS-DEBIT-TOTAL
           END-IF
           IF NOT ACM-NO-POSTINGS
               SET ACM-NO-DEBITS           TO TRUE
           END-IF
      *    NO OVERDRAFT FOR AN ESTATE - THE LIMIT EXPIRES TODAY.
           IF ACM-OD-LIMIT > ZERO
               MOVE WS-BUS-DATE            TO ACM-OD-EXPIRY-DATE
           END-IF
           PERFORM 6100-REWRITE-ACCOUNT
           ADD 1                           TO WS-TOT-ACCTS-STOPPED
           .
      *================================================================*
       2300-CANCEL-SOS.
      *================================================================*
           MOVE ACM-SORT-CODE              TO SOM-PAYER-SORT-CODE
           MOVE ACM-ACCT-NO                TO SOM-PAYER-ACCT-NO
           MOVE 1                          TO SOM-SO-SEQ
           START SOMAST-FILE KEY NOT < SOM-KEY
           IF NOT WS-SOMAST-OK
               GO TO 2300-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ SOMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-SOMAST-OK
                   WHEN SOM-PAYER-SORT-CODE NOT = ACM-SORT-CODE
                   WHEN SOM-PAYER-ACCT-NO NOT = ACM-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN SOM-CONTROL-REC
                       CONTINUE
                   WHEN SOM-ACTIVE OR SOM-SUSPENDED
                       SET SOM-CANCELLED   TO TRUE
                       MOVE WS-BUS-DATE    TO SOM-CANCEL-DATE
                       MOVE 'DCSD'         TO SOM-CANCEL-RSN
                       REWRITE SOM-SO-RECORD
                       ADD 1               TO WS-SO-COUNT
                                              WS-TOT-SO-CANCELLED
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2400-CANCEL-MANDATES.
      *================================================================*
           MOVE LOW-VALUES                 TO DMM-KEY
           MOVE ACM-SORT-CODE              TO DMM-PAYER-SORT-CODE
           MOVE ACM-ACCT-NO                TO DMM-PAYER-ACCT-NO
           START DDMAND-FILE KEY NOT < DMM-KEY
           IF NOT WS-DDMAND-OK
               GO TO 2400-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ DDMAND-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-DDMAND-OK
                   WHEN DMM-PAYER-SORT-CODE NOT = ACM-SORT-CODE
                   WHEN DMM-PAYER-ACCT-NO NOT = ACM-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN DMM-LIVE OR DMM-DORMANT
                       SET DMM-CANC-BY-PAYER TO TRUE
                       MOVE WS-BUS-DATE    TO DMM-CANCEL-DATE
                                              DMM-LAST-MAINT-DATE
                       MOVE 'BATCH '       TO DMM-CANCEL-CHANNEL
                       MOVE BRV-USER-ID    TO DMM-CANCEL-USER
      *                ADDACS REASON 2 - PAYER DECEASED (DMADC010
      *                KEEPS AN EXPLICIT REASON).
                       MOVE '2'            TO DMM-ADDACS-REASON
                       MOVE WS-PROGRAM-ID  TO DMM-LAST-MAINT-PGM
                       REWRITE DMM-MANDATE-RECORD
                       ADD 1               TO WS-DD-COUNT
                                              WS-TOT-DD-CANCELLED
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
       2400-EXIT.
           EXIT.
      *================================================================*
       2450-PAUSE-LOANS.
      *================================================================*
           MOVE BRV-CIF-NO                 TO LN-CIF-NO
           START LOANMAST-FILE KEY NOT < LN-CIF-NO
           IF NOT WS-LOANMAST-OK
               GO TO 2450-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ LOANMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-LOANMAST-OK
                   WHEN LN-CIF-NO NOT = BRV-CIF-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN LN-OPEN-STATUS
                       ADD 1               TO WS-LOAN-COUNT
                       COMPUTE WS-LOAN-TOTAL = WS-LOAN-TOTAL
                           + LN-PRINCIPAL-BAL + LN-ACCRUED-INT
                           + LN-FEES-OUTSTANDING
                       IF LN-ACTIVE OR LN-DEFAULTED
                           MOVE 'Y'        TO LN-FORBEARANCE-IND
                           MOVE ZERO       TO LN-FORBEARANCE-PAYMENT
                           COMPUTE LN-FORBEARANCE-END =
                               FUNCTION DATE-OF-INTEGER
                               (WS-BUS-DATE-INT + WS-LOAN-PAUSE-DAYS)
                           IF BRV-FREEZE-INT-IND = 'Y'
                               MOVE 'Y'    TO LN-INT-FROZEN-IND
                           END-IF
                           MOVE WS-BUS-DATE TO LN-LAST-MAINT-DATE
                           MOVE WS-PROGRAM-ID TO LN-LAST-MAINT-USER
                           REWRITE LN-LOAN-RECORD
                           ADD 1           TO WS-TOT-LOANS-PAUSED
                       END-IF
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
       2450-EXIT.
           EXIT.
      *================================================================*
       2500-PROBATE.
      *================================================================*
           IF BRV-EXEC-SORT-CODE NOT NUMERIC
           OR BRV-EXEC-ACCT-NO NOT NUMERIC
               ADD 1                       TO WS-REJECTED
               MOVE 'EXECUTOR ACCOUNT MISSING' TO WS-REJECT-TEXT
               GO TO 2500-EXIT
           END-IF
           PERFORM 5000-COLLECT-ACCOUNTS THRU 5000-EXIT

      *---- FIRST PASS: TOTALS (SMALL ESTATE CHECK) -------------------*
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-ACCT-COUNT
               MOVE WS-ACCT-KEY-ENTRY (WS-SUB) TO ACM-KEY
               READ ACCTMAST-FILE
               IF WS-ACCTMAST-OK AND NOT ACM-CLOSED
                   ADD 1                   TO WS-OPEN-COUNT
                   IF ACM-LEDGER-BAL > ZERO
                       ADD ACM-LEDGER-BAL  TO WS-CREDIT-TOTAL
                   ELSE
                       SUBTRACT ACM-LEDGER-BAL FROM WS-DEBIT-TOTAL
                   END-IF
               END-IF
           END-PERFORM
           IF BRV-INDEMNITY-IND = 'Y'
           AND WS-CREDIT-TOTAL > WS-SMALL-ESTATE
               ADD 1                       TO WS-REJECTED
               MOVE 'ESTATE OVER SMALL ESTATE LIMIT - PROBATE NEEDED'
                                           TO WS-REJECT-TEXT
               GO TO 2500-EXIT
           END-IF

      *---- IS THE EXECUTOR ACCOUNT A MEGABANK ACCOUNT? ---------------*
           MOVE 'N'                        TO WS-EXEC-INTERNAL-IND
           MOVE BRV-EXEC-SORT-CODE         TO WS-EXEC-SORT-CODE
           MOVE BRV-EXEC-ACCT-NO           TO WS-EXEC-ACCT-NO
           MOVE WS-EXEC-KEY                TO ACM-KEY
           READ ACCTMAST-FILE
           IF WS-ACCTMAST-OK AND ACM-OPEN
           AND NOT (ACM-NO-CREDITS OR ACM-NO-POSTINGS)
               SET WS-EXEC-INTERNAL        TO TRUE
           END-IF

      *---- SECOND PASS: PAY OUT AND CLOSE CREDIT ACCOUNTS ------------*
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-ACCT-COUNT
               MOVE WS-ACCT-KEY-ENTRY (WS-SUB) TO ACM-KEY
               READ ACCTMAST-FILE
               IF WS-ACCTMAST-OK AND NOT ACM-CLOSED
               AND ACM-LEDGER-BAL >= ZERO
                   PERFORM 2600-PAY-OUT-ACCOUNT
               END-IF
           END-PERFORM
           PERFORM 2450-COUNT-LOANS-ONLY THRU 2450-COUNT-EXIT

           SET CIF-BRV-PROBATE-RECEIVED    TO TRUE
           PERFORM 6000-REWRITE-CIF
           MOVE 'BRV2'                     TO LT-LETTER-CD
           MOVE BRV-EXEC-NAME              TO LT-ADDRESSEE
           PERFORM 6200-WRITE-LETTER
           ADD 1                           TO WS-PROBATES
           IF WS-DEBIT-TOTAL > ZERO OR WS-LOAN-COUNT > ZERO
               MOVE 'PAID OUT - DEBTS LISTED FOR CLAIM ON ESTATE'
                                           TO WS-REJECT-TEXT
           ELSE
               MOVE 'PAID OUT - CLOSURES REQUESTED' TO WS-REJECT-TEXT
           END-IF
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       2450-COUNT-LOANS-ONLY.
      *================================================================*
           MOVE BRV-CIF-NO                 TO LN-CIF-NO
           START LOANMAST-FILE KEY NOT < LN-CIF-NO
           IF NOT WS-LOANMAST-OK
               GO TO 2450-COUNT-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ LOANMAST-FILE NEXT RECORD
               IF NOT WS-LOANMAST-OK OR LN-CIF-NO NOT = BRV-CIF-NO
                   SET WS-END-BROWSE       TO TRUE
               ELSE
                   IF LN-OPEN-STATUS
                       ADD 1               TO WS-LOAN-COUNT
                       COMPUTE WS-LOAN-TOTAL = WS-LOAN-TOTAL
                           + LN-PRINCIPAL-BAL + LN-ACCRUED-INT
                           + LN-FEES-OUTSTANDING
                   END-IF
               END-IF
           END-PERFORM
           .
       2450-COUNT-EXIT.
           EXIT.
      *================================================================*
       2600-PAY-OUT-ACCOUNT.
      *================================================================*
           MOVE ACM-LEDGER-BAL             TO WS-AMOUNT
           IF WS-AMOUNT > ZERO
               IF WS-EXEC-INTERNAL
                   MOVE SPACES             TO PST-POSTING-RECORD
                   MOVE ACM-SORT-CODE      TO PST-SORT-CODE
                   MOVE ACM-ACCT-NO        TO PST-ACCT-NO
                   SET PST-DEBIT           TO TRUE
                   MOVE 'ESTATE PAYMENT TO EXECUTOR' TO PST-NARRATIVE-1
                   MOVE BRV-REF            TO PST-NARRATIVE-2
                   PERFORM 6300-WRITE-POSTING
                   MOVE SPACES             TO PST-POSTING-RECORD
                   MOVE WS-EXEC-SORT-CODE  TO PST-SORT-CODE
                   MOVE WS-EXEC-ACCT-NO    TO PST-ACCT-NO
                   SET PST-CREDIT          TO TRUE
                   MOVE 'ESTATE OF CUSTOMER' TO PST-NARRATIVE-1
                   MOVE BRV-REF            TO PST-NARRATIVE-2
                   PERFORM 6300-WRITE-POSTING
                   ADD WS-AMOUNT           TO WS-TOT-PAID-INTERNAL
               ELSE
      *            THE DEBIT IS RAISED WHEN THE TEAM KEYS THE CHAPS.
                   MOVE BRV-REF            TO PO-BEREAVE-REF
                   MOVE ACM-SORT-CODE      TO PO-FROM-SORT-CODE
                   MOVE ACM-ACCT-NO        TO PO-FROM-ACCT-NO
                   MOVE WS-AMOUNT          TO PO-AMOUNT
                   MOVE WS-EXEC-SORT-CODE  TO PO-TO-SORT-CODE
                   MOVE WS-EXEC-ACCT-NO    TO PO-TO-ACCT-NO
                   MOVE BRV-EXEC-NAME      TO PO-TO-NAME
                   MOVE WS-BUS-DATE        TO PO-DATE
                   WRITE PAYOUT-RECORD     FROM WS-PAYOUT
                   ADD WS-AMOUNT           TO WS-TOT-PAID-EXTERNAL
               END-IF
           END-IF
           IF WS-EXEC-INTERNAL OR WS-AMOUNT = ZERO
               SET ACM-PENDING-CLOSE       TO TRUE
               MOVE WS-BUS-DATE            TO ACM-CLOSE-REQ-DATE
               MOVE 'DCSD'                 TO ACM-CLOSE-RSN
               MOVE WS-EXEC-SORT-CODE      TO ACM-CLOSE-XFER-SORT-CODE
               MOVE WS-EXEC-ACCT-NO        TO ACM-CLOSE-XFER-ACCT-NO
               PERFORM 6100-REWRITE-ACCOUNT
           END-IF
           .
      *================================================================*
       2800-SETTLED.
      *================================================================*
           PERFORM 5000-COLLECT-ACCOUNTS THRU 5000-EXIT
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-ACCT-COUNT
               MOVE WS-ACCT-KEY-ENTRY (WS-SUB) TO ACM-KEY
               READ ACCTMAST-FILE
               IF WS-ACCTMAST-OK AND NOT ACM-CLOSED
                   ADD 1                   TO WS-OPEN-COUNT
               END-IF
           END-PERFORM
           PERFORM 2450-COUNT-LOANS-ONLY THRU 2450-COUNT-EXIT
           IF WS-OPEN-COUNT > ZERO OR WS-LOAN-COUNT > ZERO
               ADD 1                       TO WS-REJECTED
               MOVE 'ACCOUNTS OR LOANS STILL OPEN - NOT SETTLED'
                                           TO WS-REJECT-TEXT
               GO TO 2800-EXIT
           END-IF
           SET CIF-BRV-SETTLED             TO TRUE
           SET CIF-CLOSED                  TO TRUE
           PERFORM 6000-REWRITE-CIF
           ADD 1                           TO WS-SETTLEMENTS
           MOVE 'ESTATE SETTLED - CIF CLOSED' TO WS-REJECT-TEXT
           .
       2800-EXIT.
           EXIT.
      *================================================================*
       2900-REVERSE.
      *================================================================*
           SET CIF-ACTIVE                  TO TRUE
           MOVE ZERO                       TO CIF-DATE-OF-DEATH
           MOVE SPACES                     TO CIF-BEREAVE-REF
                                              CIF-BEREAVE-STAGE
           PERFORM 6000-REWRITE-CIF
           PERFORM 5000-COLLECT-ACCOUNTS THRU 5000-EXIT
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-ACCT-COUNT
               MOVE WS-ACCT-KEY-ENTRY (WS-SUB) TO ACM-KEY
               READ ACCTMAST-FILE
               IF WS-ACCTMAST-OK AND ACM-OPEN AND ACM-NO-DEBITS
                   SET ACM-NO-RESTRICT     TO TRUE
                   PERFORM 6100-REWRITE-ACCOUNT
                   ADD 1                   TO WS-OPEN-COUNT
               END-IF
           END-PERFORM
           ADD 1                           TO WS-REVERSALS
           MOVE 'REVERSED - SO/DD NOT REINSTATED, CALL CUSTOMER'
                                           TO WS-REJECT-TEXT
           .
      *================================================================*
       5000-COLLECT-ACCOUNTS.
      *================================================================*
      *    THE CUSTOMER'S ACCOUNT KEYS ARE TABLED FIRST SO THAT THE
      *    ALTERNATE-KEY BROWSE IS NOT DISTURBED BY THE UPDATES.
           MOVE ZERO                       TO WS-ACCT-COUNT
           MOVE BRV-CIF-NO                 TO ACM-CIF-NO
           START ACCTMAST-FILE KEY NOT < ACM-CIF-NO
           IF NOT WS-ACCTMAST-OK
               GO TO 5000-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ ACCTMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-ACCTMAST-OK
                   WHEN ACM-CIF-NO NOT = BRV-CIF-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN WS-ACCT-COUNT >= 50
                       SET WS-END-BROWSE   TO TRUE
                       MOVE 'MORE THAN 50 ACCOUNTS - CHECK MANUALLY'
                                           TO WS-REJECT-TEXT
                   WHEN OTHER
                       ADD 1               TO WS-ACCT-COUNT
                       MOVE ACM-KEY TO WS-ACCT-KEY-ENTRY (WS-ACCT-COUNT)
               END-EVALUATE
           END-PERFORM
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       6000-REWRITE-CIF.
      *================================================================*
           MOVE WS-BUS-DATE                TO CIF-LAST-MAINT-DATE
           MOVE ZERO                       TO CIF-LAST-MAINT-TIME
           MOVE BRV-USER-ID                TO CIF-LAST-MAINT-USER
           SET CIF-MAINT-BATCH             TO TRUE
           REWRITE CIF-CUSTOMER-RECORD
           IF NOT WS-CIFMAST-OK
               MOVE '6000-REWRITE-CIF'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CIFMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CIFMAST-STATUS      TO ABN-FILE-STATUS
               MOVE CIF-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       6100-REWRITE-ACCOUNT.
      *================================================================*
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER
           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '6100-REWRITE-ACCOUNT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       6200-WRITE-LETTER.
      *================================================================*
           MOVE WS-BUS-DATE                TO LT-ISSUE-DATE
           MOVE BRV-REF                    TO LT-BEREAVE-REF
           MOVE CIF-NO                     TO LT-CIF-NO
           MOVE SPACES                     TO LT-DECEASED-NAME
           STRING CIF-TITLE     DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-FIRST-NM  DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-LAST-NM   DELIMITED BY '  '
                  INTO LT-DECEASED-NAME
           END-STRING
           MOVE CIF-DATE-OF-DEATH          TO LT-DATE-OF-DEATH
           MOVE WS-OPEN-COUNT              TO LT-ACCOUNT-COUNT
           MOVE WS-CREDIT-TOTAL            TO LT-CREDIT-TOTAL
           MOVE WS-DEBIT-TOTAL             TO LT-DEBIT-TOTAL
           MOVE WS-LOAN-TOTAL              TO LT-LOAN-TOTAL
           MOVE WS-SO-COUNT                TO LT-SO-CANCELLED
           MOVE WS-DD-COUNT                TO LT-DD-CANCELLED
           MOVE BRV-EXEC-SORT-CODE         TO LT-EXEC-SORT-CODE
           MOVE BRV-EXEC-ACCT-NO           TO LT-EXEC-ACCT-NO
           WRITE LETTERS-RECORD            FROM WS-LETTER
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '6200-WRITE-LETTER'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       6300-WRITE-POSTING.
      *================================================================*
           MOVE WS-BUS-DATE                TO PST-POST-DATE
                                              PST-VALUE-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'TFR '                     TO PST-TXN-CODE
           MOVE WS-AMOUNT                  TO PST-AMOUNT
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           STRING 'BRV' BRV-REF WS-BUS-DATE
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE BRV-USER-ID                TO PST-USER-ID
           WRITE DECPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-DECPOST-STATUS NOT = '00'
               MOVE '6300-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'DECPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-DECPOST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'INSTRUCTIONS READ'        TO TL-LABEL
           MOVE WS-INSTR-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NOTIFICATIONS'            TO TL-LABEL
           MOVE WS-NOTIFICATIONS           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  ACCOUNTS STOPPED'       TO TL-LABEL
           MOVE WS-TOT-ACCTS-STOPPED       TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  STANDING ORDERS CANCELLED' TO TL-LABEL
           MOVE WS-TOT-SO-CANCELLED        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  MANDATES CANCELLED (ADDACS 2)' TO TL-LABEL
           MOVE WS-TOT-DD-CANCELLED        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  LOAN COLLECTIONS PAUSED' TO TL-LABEL
           MOVE WS-TOT-LOANS-PAUSED        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PROBATE / PAY-OUTS'       TO TL-LABEL
           MOVE WS-PROBATES                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  PAID TO MEGABANK EXECUTOR ACCOUNTS' TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOT-PAID-INTERNAL       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  CHAPS TO BE KEYED (PAYOUT FILE)' TO TL-LABEL
           MOVE WS-TOT-PAID-EXTERNAL       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ESTATES SETTLED'          TO TL-LABEL
           MOVE WS-SETTLEMENTS             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REVERSALS'                TO TL-LABEL
           MOVE WS-REVERSALS               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REJECTED INSTRUCTIONS'    TO TL-LABEL
           MOVE WS-REJECTED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE BEREAVE-FILE CIFMAST-FILE ACCTMAST-FILE SOMAST-FILE
                 DDMAND-FILE LOANMAST-FILE DECPOST-FILE PAYOUT-FILE
                 LETTERS-FILE REPORT-FILE
           DISPLAY 'CFDEC010 - INSTRUCTIONS ' WS-INSTR-READ
           DISPLAY 'CFDEC010 - REJECTED     ' WS-REJECTED
           IF WS-REJECTED > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       7000-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           WRITE REPORT-RECORD             FROM WS-HEADING-2
           WRITE REPORT-RECORD             FROM WS-HEADING-3
           MOVE 4                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE BRV-ACTION                 TO DL-ACTION
           MOVE BRV-CIF-NO                 TO DL-CIF-NO
           MOVE BRV-REF                    TO DL-REF
           MOVE WS-OPEN-COUNT              TO DL-ACCTS
           MOVE WS-SO-COUNT                TO DL-SO
           MOVE WS-DD-COUNT                TO DL-DD
           MOVE WS-LOAN-COUNT              TO DL-LOANS
           MOVE WS-CREDIT-TOTAL            TO DL-CREDITS
           MOVE WS-DEBIT-TOTAL             TO DL-DEBITS
           MOVE WS-REJECT-TEXT             TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-INSTRUCTION.
      *================================================================*
           READ BEREAVE-FILE INTO BRV-INSTRUCTION
           EVALUATE TRUE
               WHEN WS-BEREAVE-OK
                   ADD 1                   TO WS-INSTR-READ
               WHEN WS-BEREAVE-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-INSTRUCTION' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'BEREAVE READ FAILED' TO ABN-MESSAGE
                   MOVE WS-BEREAVE-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
