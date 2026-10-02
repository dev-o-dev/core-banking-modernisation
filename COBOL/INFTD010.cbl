       IDENTIFICATION DIVISION.
       PROGRAM-ID.    INFTD010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-11-01.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : INFTD010                                            *
      * TITLE    : FIXED TERM DEPOSITS - MATURITY NOTICES AND ACTIONS  *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : INFTD01 (DAILY, AFTER PSEOD01)                      *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (TERM, RATES)         *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, DYNAMIC - *
      *                       SEQUENTIAL BROWSE + RANDOM READ OF THE   *
      *                       NOMINATED ACCOUNT)                       *
      * OUTPUT   : MATPOST  - PAY-AWAY TRANSFERS FOR PSEOD01           *
      *            NOTICES  - PRE-MATURITY AND MATURITY LETTERS        *
      *                       (FB 200)                                 *
      *            REPORT   - MATURITY REPORT (FBA 133)                *
      *----------------------------------------------------------------*
      * RUNS AFTER END OF DAY POSTING SO THAT THE INTEREST PAID AT     *
      * MATURITY BY INCAP010 (CAPITALISATION FREQUENCY 'T') IS ALREADY *
      * IN THE LEDGER BALANCE. IT THEREFORE ACTS ON DEPOSITS WHOSE     *
      * MATURITY DATE IS BEFORE TODAY'S BUSINESS DATE.                 *
      *                                                                *
      * 1. PRE-MATURITY NOTICE (BCOBS 4.2 / SAVINGS CODE): DEPOSITS    *
      *    MATURING WITHIN WS-NOTICE-DAYS GET A LETTER REMINDING THE   *
      *    CUSTOMER OF THEIR INSTRUCTION. ACM-MATURITY-NOTICE-DATE     *
      *    STOPS A SECOND LETTER.                                      *
      * 2. MATURED - ACT ON ACM-MATURITY-INSTR:                        *
      *    R  RENEW FOR THE SAME TERM AT THE PRODUCT'S CURRENT RATE    *
      *       (FIRST TIER). NEW MATURITY DATE = OLD + TERM MONTHS.     *
      *    P  PAY AWAY THE LEDGER BALANCE TO THE NOMINATED MEGABANK    *
      *       ACCOUNT (ACM-CLOSE-XFER-SORT-CODE / ACCT-NO) AND REQUEST *
      *       CLOSURE (DDCLS010 CLOSES THE ZERO-BALANCE ACCOUNT).      *
      *       AN EXTERNAL OR INVALID NOMINATED ACCOUNT IS TREATED AS   *
      *       NO INSTRUCTION AND REPORTED TO SAVINGS OPERATIONS.       *
      *    N  NO INSTRUCTION - FUNDS STAY ON THE ACCOUNT AT THE        *
      *       MATURED DEPOSIT RATE (INSTANT ACCESS). THE ACCOUNT TYPE  *
      *       STAYS 'FTD ' WITH MATURITY DATE ZERO - CUSTOMER MAY      *
      *       WITHDRAW AT ANY TIME.                                    *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-11-01 JNB  ORIGINAL VERSION (REPLACES MANUAL MATURITY     *
      *                 LIST WORKED BY SAVINGS OPERATIONS)             *
      * 2014-01-20 PDQ  MATURED RATE FROM PRDPARM BONUS RATE IF SET    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT MATPOST-FILE     ASSIGN TO MATPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-MATPOST-STATUS.
           SELECT NOTICES-FILE     ASSIGN TO NOTICES
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-NOTICES-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  MATPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  MATPOST-RECORD                  PIC X(200).
       FD  NOTICES-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  NOTICES-RECORD                  PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'INFTD010'.
      *
       01  WS-CONSTANTS.
           05  WS-NOTICE-DAYS              PIC S9(03) COMP-3 VALUE +14.
           05  WS-DEFAULT-MATURED-RATE     PIC S9(03)V9(04) COMP-3
                                           VALUE +0.2500.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00080000.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-MATPOST-STATUS           PIC X(02).
           05  WS-NOTICES-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-NOMINATED-OK-IND         PIC X(01).
               88  WS-NOMINATED-OK             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-MAT-INT                  PIC S9(09) COMP.
           05  WS-DAYS-TO-MATURITY         PIC S9(07) COMP-3.
           05  WS-TERM-MONTHS              PIC 9(03).
           05  WS-NEW-RATE                 PIC S9(03)V9(04) COMP-3.
           05  WS-MATURED-RATE             PIC S9(03)V9(04) COMP-3.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-ACTION                   PIC X(10).
           05  WS-NOTE                     PIC X(40).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-SAVED-ACCOUNT            PIC X(400).
           05  WS-SAVED-KEY                PIC X(14).
           05  WS-NOM-KEY.
               10  WS-NOM-SORT-CODE        PIC X(06).
               10  WS-NOM-ACCT-NO          PIC X(08).
      *
      *---- MONTH ARITHMETIC ------------------------------------------*
       01  WS-MONTH-ARITH.
           05  WS-MA-DATE                  PIC 9(08).
           05  WS-MA-DATE-R REDEFINES WS-MA-DATE.
               10  WS-MA-CCYY              PIC 9(04).
               10  WS-MA-MM                PIC 9(02).
               10  WS-MA-DD                PIC 9(02).
           05  WS-MA-MONTHS                PIC S9(05) COMP-3.
           05  WS-MA-DAY                   PIC 9(02).
           05  WS-MA-TOTAL-MONTHS          PIC S9(07) COMP-3.
           05  WS-MA-LAST-DAY              PIC 9(02).
       01  WS-DAYS-IN-MONTH-VALUES         PIC X(24)
                                VALUE '312831303130313130313031'.
       01  WS-DAYS-IN-MONTH-TABLE REDEFINES WS-DAYS-IN-MONTH-VALUES.
           05  WS-DIM                      PIC 9(02) OCCURS 12 TIMES.
      *
      *---- NOTICE RECORD ---------------------------------------------*
       01  WS-NOTICE.
           05  NT-NOTICE-TYPE              PIC X(04).
               88  NT-PRE-MATURITY             VALUE 'FTPM'.
               88  NT-RENEWED                  VALUE 'FTRN'.
               88  NT-PAID-AWAY                VALUE 'FTPA'.
               88  NT-MATURED-NO-INSTR         VALUE 'FTNI'.
           05  NT-ISSUE-DATE               PIC 9(08).
           05  NT-SORT-CODE                PIC X(06).
           05  NT-ACCT-NO                  PIC X(08).
           05  NT-CIF-NO                   PIC X(12).
           05  NT-PROD-CD                  PIC X(10).
           05  NT-BALANCE                  PIC 9(11)V99.
           05  NT-MATURITY-DATE            PIC 9(08).
           05  NT-NEW-MATURITY-DATE        PIC 9(08).
           05  NT-OLD-RATE                 PIC 9(03)V9(04).
           05  NT-NEW-RATE                 PIC 9(03)V9(04).
           05  NT-INSTRUCTION              PIC X(01).
           05  NT-NOM-SORT-CODE            PIC X(06).
           05  NT-NOM-ACCT-NO              PIC X(08).
           05  FILLER                      PIC X(94).
      *
       01  WS-COUNTERS.
           05  WS-FTD-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-PRE-NOTICES              PIC S9(09) COMP-3 VALUE 0.
           05  WS-RENEWED                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-AWAY                PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-INSTR                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOM-INVALID              PIC S9(09) COMP-3 VALUE 0.
           05  WS-RENEWED-AMT              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-PAID-AWAY-AMT            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-NO-INSTR-AMT             PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'INFTD010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - FIXED TERM DEPOSIT MATURITIES'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(12) VALUE 'PRODUCT'.
           05  FILLER            PIC X(10) VALUE 'MATURITY'.
           05  FILLER            PIC X(17) VALUE '         BALANCE'.
           05  FILLER            PIC X(04) VALUE 'INS'.
           05  FILLER            PIC X(12) VALUE 'ACTION'.
           05  FILLER            PIC X(58) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-MATURITY                 PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-BALANCE                  PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-INSTR                    PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACTION                   PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
           05  FILLER                      PIC X(16) VALUE SPACES.
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
           PERFORM 2000-PROCESS-ACCOUNT THRU 2000-EXIT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                OUTPUT MATPOST-FILE
                       NOTICES-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-MATPOST-STATUS NOT = '00'
           OR WS-NOTICES-STATUS NOT = '00'
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
           MOVE LOW-VALUES                 TO ACM-KEY
           START ACCTMAST-FILE KEY NOT < ACM-KEY
           IF NOT WS-ACCTMAST-OK
               SET WS-END-OF-ACCOUNTS      TO TRUE
           ELSE
               PERFORM 8000-READ-NEXT
           END-IF
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           IF NOT ACM-FIXED-TERM
           OR NOT ACM-OPEN
           OR ACM-MATURITY-DATE NOT NUMERIC
           OR ACM-MATURITY-DATE = ZERO
               PERFORM 8000-READ-NEXT
               GO TO 2000-EXIT
           END-IF
           ADD 1                           TO WS-FTD-READ
           MOVE SPACES                     TO WS-ACTION WS-NOTE
           COMPUTE WS-MAT-INT =
                   FUNCTION INTEGER-OF-DATE (ACM-MATURITY-DATE)
           COMPUTE WS-DAYS-TO-MATURITY = WS-MAT-INT - WS-BUS-DATE-INT

           EVALUATE TRUE
               WHEN WS-DAYS-TO-MATURITY < ZERO
                   PERFORM 2200-MATURED
               WHEN WS-DAYS-TO-MATURITY <= WS-NOTICE-DAYS
               AND (ACM-MATURITY-NOTICE-DATE NOT NUMERIC
                    OR ACM-MATURITY-NOTICE-DATE = ZERO
                    OR ACM-MATURITY-NOTICE-DATE < ACM-OPEN-DATE)
                   PERFORM 2100-PRE-MATURITY
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           PERFORM 8000-READ-NEXT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-PRE-MATURITY.
      *================================================================*
           MOVE SPACES                     TO WS-NOTICE
           SET NT-PRE-MATURITY             TO TRUE
           PERFORM 5000-FILL-NOTICE
           PERFORM 5100-WRITE-NOTICE
           MOVE WS-BUS-DATE                TO ACM-MATURITY-NOTICE-DATE
           PERFORM 6100-REWRITE-ACCOUNT
           ADD 1                           TO WS-PRE-NOTICES
           MOVE 'NOTICE'                   TO WS-ACTION
           IF ACM-MAT-NONE
               MOVE 'NO INSTRUCTION HELD - CUSTOMER ASKED'
                                           TO WS-NOTE
           END-IF
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2200-MATURED.
      *================================================================*
           MOVE ACM-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               MOVE PRD-TERM-MONTHS        TO WS-TERM-MONTHS
               MOVE PRD-CR-TIER-RATE (1)   TO WS-NEW-RATE
               IF PRD-CR-BONUS-RATE NUMERIC
               AND PRD-CR-BONUS-RATE > ZERO
                   MOVE PRD-CR-BONUS-RATE  TO WS-MATURED-RATE
               ELSE
                   MOVE WS-DEFAULT-MATURED-RATE TO WS-MATURED-RATE
               END-IF
           ELSE
               MOVE ZERO                   TO WS-TERM-MONTHS
               MOVE WS-DEFAULT-MATURED-RATE TO WS-MATURED-RATE
           END-IF

           EVALUATE TRUE
               WHEN ACM-MAT-RENEW
                   IF WS-TERM-MONTHS > ZERO
                   AND PRD-ACTIVE
                       PERFORM 2300-RENEW
                   ELSE
                       MOVE 'PRODUCT WITHDRAWN - CANNOT RENEW'
                                           TO WS-NOTE
                       PERFORM 2500-NO-INSTRUCTION
                   END-IF
               WHEN ACM-MAT-PAY-AWAY
                   PERFORM 2350-CHECK-NOMINATED THRU 2350-RESTORE
                   IF WS-NOMINATED-OK
                       PERFORM 2400-PAY-AWAY
                   ELSE
                       ADD 1               TO WS-NOM-INVALID
                       PERFORM 2500-NO-INSTRUCTION
                   END-IF
               WHEN OTHER
                   PERFORM 2500-NO-INSTRUCTION
           END-EVALUATE
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2300-RENEW.
      *================================================================*
           MOVE SPACES                     TO WS-NOTICE
           SET NT-RENEWED                  TO TRUE
           PERFORM 5000-FILL-NOTICE
           MOVE ACM-MATURITY-DATE          TO WS-MA-DATE
           MOVE WS-MA-DD                   TO WS-MA-DAY
           MOVE WS-TERM-MONTHS             TO WS-MA-MONTHS
           PERFORM 6000-ADD-MONTHS
           MOVE WS-MA-DATE                 TO ACM-MATURITY-DATE
                                              NT-NEW-MATURITY-DATE
           MOVE WS-NEW-RATE                TO ACM-INT-RATE-CR
                                              NT-NEW-RATE
           MOVE ZERO                       TO ACM-MATURITY-NOTICE-DATE
           PERFORM 5100-WRITE-NOTICE
           PERFORM 6100-REWRITE-ACCOUNT
           ADD 1                           TO WS-RENEWED
           ADD ACM-LEDGER-BAL              TO WS-RENEWED-AMT
           MOVE 'RENEWED'                  TO WS-ACTION
           .
      *================================================================*
       2350-CHECK-NOMINATED.
      *================================================================*
           MOVE 'N'                        TO WS-NOMINATED-OK-IND
           MOVE ACM-ACCOUNT-RECORD         TO WS-SAVED-ACCOUNT
           MOVE ACM-KEY                    TO WS-SAVED-KEY
           MOVE ACM-CLOSE-XFER-SORT-CODE   TO WS-NOM-SORT-CODE
           MOVE ACM-CLOSE-XFER-ACCT-NO     TO WS-NOM-ACCT-NO
           IF WS-NOM-KEY = SPACES OR WS-NOM-KEY = WS-SAVED-KEY
               MOVE 'NO NOMINATED ACCOUNT HELD' TO WS-NOTE
               GO TO 2350-RESTORE
           END-IF
           MOVE WS-NOM-KEY                 TO ACM-KEY
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'NOMINATED ACCOUNT EXTERNAL / NOT FOUND'
                                           TO WS-NOTE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE '2350-CHECK-NOMINATED' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST RANDOM READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               WHEN NOT ACM-OPEN
               WHEN ACM-NO-CREDITS OR ACM-NO-POSTINGS
                   MOVE 'NOMINATED ACCOUNT CANNOT TAKE CREDITS'
                                           TO WS-NOTE
               WHEN OTHER
                   SET WS-NOMINATED-OK     TO TRUE
           END-EVALUATE
           .
       2350-RESTORE.
      *    RE-ESTABLISH THE SEQUENTIAL POSITION AND THE FTD RECORD.
           MOVE WS-SAVED-KEY               TO ACM-KEY
           READ ACCTMAST-FILE
           IF NOT WS-ACCTMAST-OK
               MOVE '2350-CHECK-NOMINATED' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REPOSITION FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE WS-SAVED-KEY           TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2400-PAY-AWAY.
      *================================================================*
           MOVE ACM-LEDGER-BAL             TO WS-AMOUNT
           IF WS-AMOUNT <= ZERO
               MOVE 'ZERO BALANCE - CLOSURE ONLY' TO WS-NOTE
           ELSE
      *        DEBIT THE DEPOSIT
               MOVE SPACES                 TO PST-POSTING-RECORD
               MOVE ACM-SORT-CODE          TO PST-SORT-CODE
               MOVE ACM-ACCT-NO            TO PST-ACCT-NO
               SET PST-DEBIT               TO TRUE
               MOVE 'MATURITY PAY-AWAY'    TO PST-NARRATIVE-1
               STRING 'TO ' WS-NOM-SORT-CODE ' ' WS-NOM-ACCT-NO
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
               PERFORM 5200-WRITE-POSTING
      *        CREDIT THE NOMINATED ACCOUNT
               MOVE SPACES                 TO PST-POSTING-RECORD
               MOVE WS-NOM-SORT-CODE       TO PST-SORT-CODE
               MOVE WS-NOM-ACCT-NO         TO PST-ACCT-NO
               SET PST-CREDIT              TO TRUE
               MOVE 'FIXED TERM DEPOSIT MATURED' TO PST-NARRATIVE-1
               STRING 'FROM ' ACM-SORT-CODE ' ' ACM-ACCT-NO
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
               PERFORM 5200-WRITE-POSTING
           END-IF

           MOVE SPACES                     TO WS-NOTICE
           SET NT-PAID-AWAY                TO TRUE
           PERFORM 5000-FILL-NOTICE
           PERFORM 5100-WRITE-NOTICE

      *    CLOSURE REQUEST - DDCLS010 CLOSES WHEN THE BALANCE IS ZERO.
           SET ACM-PENDING-CLOSE           TO TRUE
           MOVE WS-BUS-DATE                TO ACM-CLOSE-REQ-DATE
           MOVE 'MATR'                     TO ACM-CLOSE-RSN
           PERFORM 6100-REWRITE-ACCOUNT
           ADD 1                           TO WS-PAID-AWAY
           ADD WS-AMOUNT                   TO WS-PAID-AWAY-AMT
           MOVE 'PAID AWAY'                TO WS-ACTION
           .
      *================================================================*
       2500-NO-INSTRUCTION.
      *================================================================*
           MOVE SPACES                     TO WS-NOTICE
           SET NT-MATURED-NO-INSTR         TO TRUE
           PERFORM 5000-FILL-NOTICE
           MOVE ZERO                       TO NT-NEW-MATURITY-DATE
           MOVE WS-MATURED-RATE            TO NT-NEW-RATE
           PERFORM 5100-WRITE-NOTICE
           MOVE WS-MATURED-RATE            TO ACM-INT-RATE-CR
           MOVE ZERO                       TO ACM-MATURITY-DATE
                                              ACM-MATURITY-NOTICE-DATE
           MOVE 'N'                        TO ACM-MATURITY-INSTR
           PERFORM 6100-REWRITE-ACCOUNT
           ADD 1                           TO WS-NO-INSTR
           ADD ACM-LEDGER-BAL              TO WS-NO-INSTR-AMT
           MOVE 'MATURED'                  TO WS-ACTION
           IF WS-NOTE = SPACES
               MOVE 'NO INSTRUCTION - MATURED RATE APPLIED'
                                           TO WS-NOTE
           END-IF
           .
      *================================================================*
       5000-FILL-NOTICE.
      *================================================================*
           MOVE WS-BUS-DATE                TO NT-ISSUE-DATE
           MOVE ACM-SORT-CODE              TO NT-SORT-CODE
           MOVE ACM-ACCT-NO                TO NT-ACCT-NO
           MOVE ACM-CIF-NO                 TO NT-CIF-NO
           MOVE ACM-PROD-CD                TO NT-PROD-CD
           IF ACM-LEDGER-BAL > ZERO
               MOVE ACM-LEDGER-BAL         TO NT-BALANCE
           ELSE
               MOVE ZERO                   TO NT-BALANCE
           END-IF
           MOVE ACM-MATURITY-DATE          TO NT-MATURITY-DATE
           MOVE ZERO                       TO NT-NEW-MATURITY-DATE
           MOVE ACM-INT-RATE-CR            TO NT-OLD-RATE
           MOVE ZERO                       TO NT-NEW-RATE
           MOVE ACM-MATURITY-INSTR         TO NT-INSTRUCTION
           MOVE ACM-CLOSE-XFER-SORT-CODE   TO NT-NOM-SORT-CODE
           MOVE ACM-CLOSE-XFER-ACCT-NO     TO NT-NOM-ACCT-NO
           .
      *================================================================*
       5100-WRITE-NOTICE.
      *================================================================*
           WRITE NOTICES-RECORD            FROM WS-NOTICE
           IF WS-NOTICES-STATUS NOT = '00'
               MOVE '5100-WRITE-NOTICE'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'NOTICES WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-NOTICES-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       5200-WRITE-POSTING.
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
           STRING 'MAT' WS-SAVED-KEY WS-BUS-DATE
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE MATPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-MATPOST-STATUS NOT = '00'
               MOVE '5200-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'MATPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-MATPOST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       6000-ADD-MONTHS.
      *================================================================*
           COMPUTE WS-MA-TOTAL-MONTHS =
                   WS-MA-CCYY * 12 + WS-MA-MM - 1 + WS-MA-MONTHS
           COMPUTE WS-MA-CCYY = WS-MA-TOTAL-MONTHS / 12
           COMPUTE WS-MA-MM =
                   WS-MA-TOTAL-MONTHS - (WS-MA-CCYY * 12) + 1
           MOVE WS-DIM (WS-MA-MM)          TO WS-MA-LAST-DAY
           IF WS-MA-MM = 2
               IF FUNCTION MOD (WS-MA-CCYY, 4) = 0
               AND (FUNCTION MOD (WS-MA-CCYY, 100) NOT = 0
                    OR FUNCTION MOD (WS-MA-CCYY, 400) = 0)
                   MOVE 29                 TO WS-MA-LAST-DAY
               END-IF
           END-IF
           IF WS-MA-DAY > WS-MA-LAST-DAY
               MOVE WS-MA-LAST-DAY         TO WS-MA-DD
           ELSE
               MOVE WS-MA-DAY              TO WS-MA-DD
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
       3000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'FIXED TERM DEPOSITS READ' TO TL-LABEL
           MOVE WS-FTD-READ                TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PRE-MATURITY NOTICES'     TO TL-LABEL
           MOVE WS-PRE-NOTICES             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RENEWED'                  TO TL-LABEL
           MOVE WS-RENEWED                 TO TL-COUNT
           MOVE WS-RENEWED-AMT             TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PAID AWAY'                TO TL-LABEL
           MOVE WS-PAID-AWAY               TO TL-COUNT
           MOVE WS-PAID-AWAY-AMT           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MATURED - NO INSTRUCTION' TO TL-LABEL
           MOVE WS-NO-INSTR                TO TL-COUNT
           MOVE WS-NO-INSTR-AMT            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH NOMINATED ACCOUNT INVALID' TO TL-LABEL
           MOVE WS-NOM-INVALID             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE ACCTMAST-FILE PRDPARM-FILE MATPOST-FILE NOTICES-FILE
                 REPORT-FILE
           DISPLAY 'INFTD010 - RENEWED   ' WS-RENEWED
           DISPLAY 'INFTD010 - PAID AWAY ' WS-PAID-AWAY
           DISPLAY 'INFTD010 - NO INSTR  ' WS-NO-INSTR
           IF WS-NOM-INVALID > ZERO
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
           MOVE ACM-SORT-CODE              TO DL-SORT-CODE
           MOVE ACM-ACCT-NO                TO DL-ACCT-NO
           MOVE ACM-PROD-CD                TO DL-PROD-CD
           MOVE NT-MATURITY-DATE           TO DL-MATURITY
           MOVE ACM-LEDGER-BAL             TO DL-BALANCE
           MOVE NT-INSTRUCTION             TO DL-INSTR
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-NEXT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   CONTINUE
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-NEXT'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
