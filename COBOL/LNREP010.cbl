       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNREP010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-09-13.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : LNREP010                                            *
      * TITLE    : PERSONAL LOANS - COLLECT MONTHLY REPAYMENTS         *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : LNREP010                                            *
      * JCL      : LNDLY01 STEP LN030 (AFTER LNINT010)                 *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (RANDOM, READ ONLY)   *
      *            PRDPARM  - PRODUCT PARAMETERS (GL CODES)            *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : LOANMAST - LOAN MASTER (VSAM KSDS, SEQUENTIAL I-O)  *
      *            MBK.LOANTXN - 'REP ' / 'MISS' / 'SETL' ROWS         *
      * OUTPUT   : LNPOST   - 'LNR ' DEBITS ON REPAYMENT ACCOUNTS      *
      *            GLJNL    - INTEREST / FEE RECLASSIFICATION JOURNAL  *
      *            LETTERS  - SETTLED / ARREARS CLEARED (LNLTRREC)     *
      *            REPORT   - COLLECTION REPORT (FBA 133)              *
      *----------------------------------------------------------------*
      * A LOAN IS PICKED UP WHEN:                                      *
      *   - LN-NEXT-DUE-DATE <= BUSINESS DATE (INSTALMENT DUE). THE    *
      *     DUE DATE MOVES ON ONE MONTH WHETHER OR NOT THE INSTALMENT  *
      *     IS PAID - THE CONTRACTUAL SCHEDULE DOES NOT SLIP. AN       *
      *     UNPAID INSTALMENT IS ADDED TO LN-ARREARS-AMT AND A RETRY   *
      *     IS SET FOR 7 DAYS LATER.                                   *
      *   - LN-RETRY-DATE <= BUSINESS DATE AND ARREARS > 0 (RETRY THE  *
      *     WHOLE ARREARS; NO PARTIAL COLLECTIONS - SEE ICG GUIDANCE). *
      * ONLY ONE RETRY PER MISSED INSTALMENT.                          *
      *                                                                *
      * FUNDS: AVAILABLE BALANCE + UNEXPIRED ARRANGED OVERDRAFT LIMIT  *
      * LESS ANYTHING ALREADY TAKEN FROM THE SAME ACCOUNT IN THIS RUN  *
      * (WS-RESERVE TABLE). LOAN REPAYMENTS NEVER USE THE UNARRANGED   *
      * BUFFER.                                                        *
      *                                                                *
      * PAYMENT ALLOCATION (CCA S81 - BORROWER MAY NOT APPROPRIATE ON  *
      * A SINGLE AGREEMENT): ACCRUED INTEREST, THEN DEFAULT FEES, THEN *
      * PRINCIPAL. THE FINAL INSTALMENT IS WHATEVER IS LEFT (PRINCIPAL *
      * + INTEREST + FEES) SO ROUNDING CLEARS TO ZERO.                 *
      *                                                                *
      * GL: GLEXT010 CREDITS THE WHOLE 'LNR ' AMOUNT TO LOANS          *
      * PRINCIPAL 13000-0010 VIA MBK.GLMAP. THIS PROGRAM RECLASSIFIES  *
      * THE INTEREST AND FEE PORTIONS:                                 *
      *   DR 13000-0010   CR PRD-GL-INT-ACCR-REC (13000-0020)          *
      *   DR 13000-0010   CR 13000-0030 LOAN FEES RECEIVABLE           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-09-13 JNB  ORIGINAL VERSION                               *
      * 2011-03-21 JNB  FORBEARANCE - COLLECT LN-FORBEARANCE-PAYMENT   *
      *                 INSTEAD OF THE CONTRACTUAL INSTALMENT          *
      * 2012-02-06 PDQ  RESERVE TABLE RAISED 2000 -> 5000 ENTRIES      *
      *                 (INC0045521 - TWO LOANS ON ONE ACCOUNT BOTH    *
      *                 PAID WHEN FUNDS COVERED ONLY ONE)              *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOANMAST-FILE    ASSIGN TO LOANMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS LN-KEY
                                   FILE STATUS IS WS-LOANMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT LNPOST-FILE      ASSIGN TO LNPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LNPOST-STATUS.
           SELECT GLJNL-FILE       ASSIGN TO GLJNL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLJNL-STATUS.
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
       FD  LOANMAST-FILE.
           COPY LNACCREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  LNPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LNPOST-RECORD                   PIC X(200).
       FD  GLJNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJNL-RECORD                    PIC X(250).
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(300).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNREP010'.
      *
       01  WS-CONSTANTS.
           05  WS-CONTROL-KEY              PIC X(10) VALUE '0000000000'.
           05  WS-RETRY-DAYS               PIC S9(03) COMP-3 VALUE +7.
           05  WS-MAX-RESERVE              PIC S9(04) COMP VALUE +5000.
           05  WS-GL-PRINCIPAL             PIC X(10) VALUE '13000-0010'.
           05  WS-GL-INT-REC-DFLT          PIC X(10) VALUE '13000-0020'.
           05  WS-GL-FEE-REC               PIC X(10) VALUE '13000-0030'.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00070000.
      *
       01  WS-FILE-STATUSES.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00'.
               88  WS-LOANMAST-EOF             VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-LNPOST-STATUS            PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-LOANS             VALUE 'Y'.
           05  WS-LOAN-CHANGED-IND         PIC X(01).
               88  WS-LOAN-CHANGED             VALUE 'Y'.
           05  WS-COLLECTED-IND            PIC X(01).
               88  WS-COLLECTED                VALUE 'Y'.
           05  WS-FINAL-IND                PIC X(01).
               88  WS-FINAL-INSTALMENT         VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-DATE-ISO                 PIC X(10).
           05  WS-COLLECTION-TYPE          PIC X(04).
               88  WS-COLLECT-INSTALMENT       VALUE 'INST'.
               88  WS-COLLECT-ARREARS          VALUE 'ARRS'.
           05  WS-AMOUNT-DUE               PIC S9(13)V99 COMP-3.
           05  WS-CONTRACT-DUE             PIC S9(13)V99 COMP-3.
           05  WS-SPENDING-POWER           PIC S9(13)V99 COMP-3.
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-REMAINING                PIC S9(13)V99 COMP-3.
           05  WS-INT-PART                 PIC S9(13)V99 COMP-3.
           05  WS-FEE-PART                 PIC S9(13)V99 COMP-3.
           05  WS-PRIN-PART                PIC S9(13)V99 COMP-3.
           05  WS-SHORTFALL                PIC S9(13)V99 COMP-3.
           05  WS-RESULT                   PIC X(10).
           05  WS-NOTE                     PIC X(36).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-TXN-SEQ                  PIC S9(09) COMP.
           05  WS-LINE-NO                  PIC 9(07) VALUE ZERO.
           05  WS-GL-INT-REC               PIC X(10).
           05  WS-COST-CENTRE              PIC X(06).
           05  WS-TIME-NOW                 PIC 9(08).
           05  WS-HOLD-ARREARS             PIC S9(13)V99 COMP-3.
      *
      *---- MONTH ARITHMETIC (AS LNBKG010) ----------------------------*
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
           05  WS-MA-RESULT                PIC 9(08).
       01  WS-DAYS-IN-MONTH-VALUES         PIC X(24)
                                VALUE '312831303130313130313031'.
       01  WS-DAYS-IN-MONTH-TABLE REDEFINES WS-DAYS-IN-MONTH-VALUES.
           05  WS-DIM                      PIC 9(02) OCCURS 12 TIMES.
      *
      *---- AMOUNTS ALREADY TAKEN FROM EACH ACCOUNT IN THIS RUN -------*
       01  WS-RESERVE-TABLE.
           05  WS-RS-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-RS-ENTRY                 OCCURS 5000 TIMES
                                           INDEXED BY RS-IDX.
               10  WS-RS-ACCOUNT           PIC X(14).
               10  WS-RS-AMOUNT            PIC S9(13)V99 COMP-3.
      *
      *---- GL RECLASS TOTALS BY INTEREST-RECEIVABLE ACCOUNT ----------*
       01  WS-GL-TABLE.
           05  WS-GT-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-GT-ENTRY                 OCCURS 20 TIMES
                                           INDEXED BY GT-IDX.
               10  WS-GT-GL-INT-REC        PIC X(10).
               10  WS-GT-COST-CENTRE       PIC X(06).
               10  WS-GT-INTEREST          PIC S9(13)V99 COMP-3.
               10  WS-GT-FEES              PIC S9(13)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-LOANS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-INSTALMENTS-DUE          PIC S9(09) COMP-3 VALUE 0.
           05  WS-INSTALMENTS-PAID         PIC S9(09) COMP-3 VALUE 0.
           05  WS-INSTALMENTS-MISSED       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETRIES                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETRIES-PAID             PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOANS-SETTLED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-REPAY-ACCT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-RESERVE-FULL             PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-COLLECTED          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-MISSED             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-INT-PART           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-FEE-PART           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-PRIN-PART          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-DR                    PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-CR                    PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-LINES                 PIC 9(09) VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'LNREP010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - LOAN REPAYMENT COLLECTION'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(12) VALUE 'LOAN NO'.
           05  FILLER            PIC X(06) VALUE 'TYPE'.
           05  FILLER            PIC X(16) VALUE 'REPAY ACCOUNT'.
           05  FILLER            PIC X(15) VALUE '        DUE'.
           05  FILLER            PIC X(15) VALUE '    ARREARS'.
           05  FILLER            PIC X(15) VALUE '  PRINCIPAL'.
           05  FILLER            PIC X(11) VALUE 'RESULT'.
           05  FILLER            PIC X(42) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-LOAN-NO                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TYPE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-REPAY-SC                 PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-REPAY-ACCT               PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DUE                      PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-ARREARS                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-PRINCIPAL                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RESULT                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTE                     PIC X(36).
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
           COPY GLJNLREC.
      *
           COPY LNLTRREC.
      *
           COPY DATECARD.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLLNTX END-EXEC.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-LOAN THRU 2000-EXIT
               UNTIL WS-END-OF-LOANS
           PERFORM 3000-WRITE-JOURNAL
           PERFORM 4000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                I-O    LOANMAST-FILE
                OUTPUT LNPOST-FILE
                       GLJNL-FILE
                       LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-LOANMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-LNPOST-STATUS NOT = '00'
           OR WS-GLJNL-STATUS NOT = '00'
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
           MOVE HD1-BUS-DATE               TO WS-DATE-ISO
           PERFORM 8000-READ-LOAN
           .
      *================================================================*
       2000-PROCESS-LOAN.
      *================================================================*
           IF LN-LOAN-NO = WS-CONTROL-KEY
           OR NOT (LN-ACTIVE OR LN-DEFAULTED)
               PERFORM 8000-READ-LOAN
               GO TO 2000-EXIT
           END-IF
           ADD 1                           TO WS-LOANS-READ
           MOVE 'N'                        TO WS-LOAN-CHANGED-IND

      *---- INSTALMENT DUE --------------------------------------------*
           IF LN-NEXT-DUE-DATE NUMERIC
           AND LN-NEXT-DUE-DATE > ZERO
           AND LN-NEXT-DUE-DATE <= WS-BUS-DATE
           AND LN-PAYMENTS-REMAINING > ZERO
               SET WS-COLLECT-INSTALMENT   TO TRUE
               ADD 1                       TO WS-INSTALMENTS-DUE
               PERFORM 2100-INSTALMENT
               SET WS-LOAN-CHANGED         TO TRUE
           ELSE
      *---- RETRY OF ARREARS ------------------------------------------*
               IF LN-ARREARS-AMT > ZERO
               AND LN-RETRY-DATE NUMERIC
               AND LN-RETRY-DATE > ZERO
               AND LN-RETRY-DATE <= WS-BUS-DATE
                   SET WS-COLLECT-ARREARS  TO TRUE
                   ADD 1                   TO WS-RETRIES
                   PERFORM 2200-RETRY
                   SET WS-LOAN-CHANGED     TO TRUE
               END-IF
           END-IF

           IF WS-LOAN-CHANGED
               MOVE WS-BUS-DATE            TO LN-LAST-MAINT-DATE
               MOVE WS-PROGRAM-ID          TO LN-LAST-MAINT-USER
               REWRITE LN-LOAN-RECORD
               IF NOT WS-LOANMAST-OK
                   MOVE '2000-PROCESS-LOAN' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'LOANMAST REWRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-LOANMAST-STATUS TO ABN-FILE-STATUS
                   MOVE LN-KEY             TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
           END-IF
           PERFORM 8000-READ-LOAN
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-INSTALMENT.
      *================================================================*
           MOVE 'N'                        TO WS-FINAL-IND
           IF LN-PAYMENTS-REMAINING = 1
               SET WS-FINAL-INSTALMENT     TO TRUE
               COMPUTE WS-CONTRACT-DUE = LN-PRINCIPAL-BAL
                       + LN-ACCRUED-INT + LN-FEES-OUTSTANDING
                       - LN-ARREARS-AMT
               IF WS-CONTRACT-DUE < ZERO
                   MOVE ZERO               TO WS-CONTRACT-DUE
               END-IF
           ELSE
               MOVE LN-MONTHLY-PAYMENT     TO WS-CONTRACT-DUE
           END-IF

           IF LN-IN-FORBEARANCE
           AND LN-FORBEARANCE-END >= WS-BUS-DATE
               MOVE LN-FORBEARANCE-PAYMENT TO WS-AMOUNT-DUE
           ELSE
               MOVE WS-CONTRACT-DUE        TO WS-AMOUNT-DUE
           END-IF

           IF WS-AMOUNT-DUE > ZERO
               PERFORM 5000-TRY-COLLECT THRU 5000-EXIT
           ELSE
               MOVE 'N'                    TO WS-COLLECTED-IND
           END-IF

           IF WS-COLLECTED
               ADD 1                       TO WS-INSTALMENTS-PAID
               PERFORM 5100-ALLOCATE
               MOVE 'PAID'                 TO WS-RESULT
      *        FORBEARANCE SHORTFALL STILL BUILDS ARREARS BUT DOES NOT
      *        COUNT AS A MISSED PAYMENT.
               IF WS-AMOUNT-DUE < WS-CONTRACT-DUE
                   COMPUTE WS-SHORTFALL =
                           WS-CONTRACT-DUE - WS-AMOUNT-DUE
                   ADD WS-SHORTFALL        TO LN-ARREARS-AMT
                   MOVE 'FORBEARANCE PAYMENT - SHORTFALL TO ARREARS'
                                           TO WS-NOTE
               END-IF
           ELSE
               IF WS-AMOUNT-DUE > ZERO
                   ADD 1                   TO WS-INSTALMENTS-MISSED
                   ADD WS-CONTRACT-DUE     TO WS-TOTAL-MISSED
               END-IF
               ADD WS-CONTRACT-DUE         TO LN-ARREARS-AMT
               ADD 1                       TO LN-MISSED-COUNT
               IF LN-ARREARS-SINCE = ZERO
                   MOVE LN-NEXT-DUE-DATE   TO LN-ARREARS-SINCE
               END-IF
               COMPUTE LN-RETRY-DATE = FUNCTION DATE-OF-INTEGER
                       (WS-BUS-DATE-INT + WS-RETRY-DAYS)
               MOVE ZERO                   TO LN-RETRY-COUNT
               MOVE 'MISSED'               TO WS-RESULT
               PERFORM 5300-HISTORY-MISSED
           END-IF

      *---- SCHEDULE MOVES ON -----------------------------------------*
           SUBTRACT 1                      FROM LN-PAYMENTS-REMAINING
           IF LN-PAYMENTS-REMAINING > ZERO
               MOVE LN-NEXT-DUE-DATE       TO WS-MA-DATE
               MOVE 1                      TO WS-MA-MONTHS
               MOVE LN-PAYMENT-DAY         TO WS-MA-DAY
               PERFORM 6000-ADD-MONTHS
               MOVE WS-MA-RESULT           TO LN-NEXT-DUE-DATE
           ELSE
               MOVE ZERO                   TO LN-NEXT-DUE-DATE
           END-IF

           PERFORM 5200-CHECK-SETTLED
           PERFORM 7100-REPORT-DETAIL THRU 7100-EXIT
           .
      *================================================================*
       2200-RETRY.
      *================================================================*
           MOVE LN-ARREARS-AMT             TO WS-AMOUNT-DUE
           MOVE LN-ARREARS-AMT             TO WS-CONTRACT-DUE
           PERFORM 5000-TRY-COLLECT THRU 5000-EXIT
           IF WS-COLLECTED
               ADD 1                       TO WS-RETRIES-PAID
               PERFORM 5100-ALLOCATE
               MOVE 'ARR PAID'             TO WS-RESULT
           ELSE
               MOVE 'ARR UNPAID'           TO WS-RESULT
               MOVE 'NO FURTHER RETRY - LNARR010 ACTION'
                                           TO WS-NOTE
           END-IF
           MOVE 1                          TO LN-RETRY-COUNT
           MOVE ZERO                       TO LN-RETRY-DATE
           PERFORM 5200-CHECK-SETTLED
           PERFORM 7100-REPORT-DETAIL THRU 7100-EXIT
           .
      *================================================================*
       5000-TRY-COLLECT.
      *================================================================*
           MOVE 'N'                        TO WS-COLLECTED-IND
           MOVE SPACES                     TO WS-NOTE
           MOVE LN-REPAY-SORT-CODE         TO ACM-SORT-CODE
           MOVE LN-REPAY-ACCT-NO           TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF NOT WS-ACCTMAST-OK
               ADD 1                       TO WS-NO-REPAY-ACCT
               MOVE 'REPAYMENT ACCOUNT NOT FOUND' TO WS-NOTE
               GO TO 5000-EXIT
           END-IF
           IF NOT ACM-OPEN
           OR ACM-NO-DEBITS OR ACM-NO-POSTINGS
               MOVE 'REPAYMENT ACCOUNT CLOSED / RESTRICTED'
                                           TO WS-NOTE
               GO TO 5000-EXIT
           END-IF

           MOVE ACM-OD-LIMIT               TO WS-OD-LIMIT
           IF ACM-OD-EXPIRY-DATE NOT = ZERO
           AND ACM-OD-EXPIRY-DATE < WS-BUS-DATE
               MOVE ZERO                   TO WS-OD-LIMIT
           END-IF
           COMPUTE WS-SPENDING-POWER = ACM-AVAIL-BAL + WS-OD-LIMIT

      *    FIND (OR ADD) THE ACCOUNT IN THE RUN RESERVE TABLE.
           SET RS-IDX                      TO 1
           SEARCH WS-RS-ENTRY
               AT END
                   ADD 1                   TO WS-RESERVE-FULL
                   SET RS-IDX              TO WS-MAX-RESERVE
               WHEN RS-IDX > WS-RS-COUNT
                   ADD 1                   TO WS-RS-COUNT
                   MOVE ACM-KEY            TO WS-RS-ACCOUNT (RS-IDX)
                   MOVE ZERO               TO WS-RS-AMOUNT (RS-IDX)
               WHEN WS-RS-ACCOUNT (RS-IDX) = ACM-KEY
                   CONTINUE
           END-SEARCH
      *    TABLE FULL: THE LAST SLOT IS SHARED, SO THE CHECK BELOW MAY
      *    BE WRONG FOR THE ACCOUNTS USING IT. HAS NEVER HAPPENED.
           IF WS-RS-ACCOUNT (RS-IDX) = ACM-KEY
               SUBTRACT WS-RS-AMOUNT (RS-IDX) FROM WS-SPENDING-POWER
           END-IF

           IF WS-AMOUNT-DUE > WS-SPENDING-POWER
               MOVE 'INSUFFICIENT FUNDS'   TO WS-NOTE
               GO TO 5000-EXIT
           END-IF

           ADD WS-AMOUNT-DUE               TO WS-RS-AMOUNT (RS-IDX)
           SET WS-COLLECTED                TO TRUE

           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE LN-REPAY-SORT-CODE         TO PST-SORT-CODE
           MOVE LN-REPAY-ACCT-NO           TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'LNR '                     TO PST-TXN-CODE
           SET PST-DEBIT                   TO TRUE
           MOVE WS-AMOUNT-DUE              TO PST-AMOUNT
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           MOVE 'MEGABANK LOAN REPAYMENT'  TO PST-NARRATIVE-1
           IF WS-COLLECT-ARREARS
               STRING 'LOAN ' LN-LOAN-NO ' ARREARS'
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
           ELSE
               STRING 'LOAN ' LN-LOAN-NO
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
           END-IF
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           STRING 'LNR' LN-LOAN-NO WS-BUS-DATE
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE LNPOST-RECORD             FROM PST-POSTING-RECORD
           IF WS-LNPOST-STATUS NOT = '00'
               MOVE '5000-TRY-COLLECT'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LNPOST WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-LNPOST-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD WS-AMOUNT-DUE               TO WS-TOTAL-COLLECTED
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       5100-ALLOCATE.
      *================================================================*
      *    INTEREST, THEN FEES, THEN PRINCIPAL. ARREARS REDUCE BY THE
      *    AMOUNT COLLECTED ON A RETRY ONLY - AN INSTALMENT PAYS THE
      *    CURRENT MONTH.
           MOVE WS-AMOUNT-DUE              TO WS-REMAINING
           MOVE ZERO                       TO WS-INT-PART
                                              WS-FEE-PART
                                              WS-PRIN-PART

           IF WS-REMAINING > LN-ACCRUED-INT
               MOVE LN-ACCRUED-INT         TO WS-INT-PART
           ELSE
               MOVE WS-REMAINING           TO WS-INT-PART
           END-IF
           SUBTRACT WS-INT-PART            FROM WS-REMAINING
                                                LN-ACCRUED-INT

           IF WS-REMAINING > LN-FEES-OUTSTANDING
               MOVE LN-FEES-OUTSTANDING    TO WS-FEE-PART
           ELSE
               MOVE WS-REMAINING           TO WS-FEE-PART
           END-IF
           SUBTRACT WS-FEE-PART            FROM WS-REMAINING
                                                LN-FEES-OUTSTANDING

           IF WS-REMAINING > LN-PRINCIPAL-BAL
      *        OVERPAYMENT - SHOULD NOT HAPPEN; LEAVE THE EXCESS ON
      *        THE REPORT FOR LENDING OPERATIONS TO REFUND.
               MOVE LN-PRINCIPAL-BAL       TO WS-PRIN-PART
               MOVE 'OVERPAYMENT - REFUND REQUIRED' TO WS-NOTE
           ELSE
               MOVE WS-REMAINING           TO WS-PRIN-PART
           END-IF
           SUBTRACT WS-PRIN-PART           FROM LN-PRINCIPAL-BAL

           ADD WS-INT-PART                 TO LN-TOTAL-INT-PAID
                                              LN-INT-PAID-TAX-YEAR
                                              WS-TOTAL-INT-PART
           ADD WS-FEE-PART                 TO LN-TOTAL-FEES-PAID
                                              WS-TOTAL-FEE-PART
           ADD WS-PRIN-PART                TO LN-TOTAL-PRIN-PAID
                                              WS-TOTAL-PRIN-PART
           MOVE WS-BUS-DATE                TO LN-LAST-PAY-DATE
           MOVE WS-AMOUNT-DUE              TO LN-LAST-PAY-AMT
           IF WS-COLLECT-INSTALMENT
               ADD 1                       TO LN-PAYMENTS-MADE
           END-IF

           IF WS-COLLECT-ARREARS
               MOVE LN-ARREARS-AMT         TO WS-HOLD-ARREARS
               SUBTRACT WS-AMOUNT-DUE      FROM LN-ARREARS-AMT
               IF LN-ARREARS-AMT <= ZERO
                   MOVE ZERO               TO LN-ARREARS-AMT
                   PERFORM 5400-ARREARS-CLEARED
               END-IF
           END-IF

           PERFORM 5150-GL-ACCUMULATE THRU 5150-EXIT
           PERFORM 5350-HISTORY-PAID
           .
      *================================================================*
       5150-GL-ACCUMULATE.
      *================================================================*
           IF WS-INT-PART = ZERO AND WS-FEE-PART = ZERO
               GO TO 5150-EXIT
           END-IF
           MOVE WS-GL-INT-REC-DFLT         TO WS-GL-INT-REC
           MOVE SPACES                     TO WS-COST-CENTRE
           MOVE LN-PROD-CD                 TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               IF PRD-GL-INT-ACCR-REC NOT = SPACES
                   MOVE PRD-GL-INT-ACCR-REC TO WS-GL-INT-REC
               END-IF
               MOVE PRD-COST-CENTRE        TO WS-COST-CENTRE
           END-IF
           SET GT-IDX                      TO 1
           SEARCH WS-GT-ENTRY
               AT END
                   MOVE '5150-GL-ACCUMULATE' TO ABN-PARAGRAPH
                   MOVE 1006               TO ABN-ABEND-CODE
                   MOVE 'GL TABLE FULL (20 RECEIVABLE ACCOUNTS)'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               WHEN GT-IDX > WS-GT-COUNT
                   ADD 1                   TO WS-GT-COUNT
                   MOVE WS-GL-INT-REC      TO WS-GT-GL-INT-REC (GT-IDX)
                   MOVE WS-COST-CENTRE     TO WS-GT-COST-CENTRE (GT-IDX)
                   MOVE ZERO               TO WS-GT-INTEREST (GT-IDX)
                                              WS-GT-FEES (GT-IDX)
               WHEN WS-GT-GL-INT-REC (GT-IDX) = WS-GL-INT-REC
               AND  WS-GT-COST-CENTRE (GT-IDX) = WS-COST-CENTRE
                   CONTINUE
           END-SEARCH
           ADD WS-INT-PART                 TO WS-GT-INTEREST (GT-IDX)
           ADD WS-FEE-PART                 TO WS-GT-FEES (GT-IDX)
           .
       5150-EXIT.
           EXIT.
      *================================================================*
       5200-CHECK-SETTLED.
      *================================================================*
           IF LN-PRINCIPAL-BAL = ZERO
           AND LN-ACCRUED-INT = ZERO
           AND LN-FEES-OUTSTANDING = ZERO
               SET LN-SETTLED              TO TRUE
               MOVE WS-BUS-DATE            TO LN-SETTLED-DATE
                                              LN-CLOSED-DATE
               MOVE LN-LAST-PAY-AMT        TO LN-SETTLED-AMT
               MOVE ZERO                   TO LN-NEXT-DUE-DATE
                                              LN-ARREARS-AMT
                                              LN-MISSED-COUNT
                                              LN-ARREARS-BUCKET
                                              LN-RETRY-DATE
               ADD 1                       TO WS-LOANS-SETTLED
               MOVE 'LOAN REPAID IN FULL - CLOSED' TO WS-NOTE
               MOVE 'SETL'                 TO LNX-LETTER-CD
               MOVE 'LNSET01'              TO LNX-TEMPLATE-ID
               PERFORM 5500-WRITE-LETTER
           END-IF
           .
      *================================================================*
       5300-HISTORY-MISSED.
      *================================================================*
           MOVE 'MISS'                     TO LNT-TXN-TYPE
           MOVE 'D'                        TO LNT-DR-CR-IND
           MOVE WS-CONTRACT-DUE            TO LNT-AMOUNT
           MOVE ZERO                       TO LNT-INT-PORTION
                                              LNT-FEE-PORTION
                                              LNT-PRIN-PORTION
           MOVE 'INSTALMENT NOT PAID'      TO LNT-NARRATIVE
           PERFORM 5390-INSERT-HISTORY
           .
      *================================================================*
       5350-HISTORY-PAID.
      *================================================================*
           MOVE 'REP '                     TO LNT-TXN-TYPE
           MOVE 'C'                        TO LNT-DR-CR-IND
           MOVE WS-AMOUNT-DUE              TO LNT-AMOUNT
           MOVE WS-INT-PART                TO LNT-INT-PORTION
           MOVE WS-FEE-PART                TO LNT-FEE-PORTION
           MOVE WS-PRIN-PART               TO LNT-PRIN-PORTION
           IF WS-COLLECT-ARREARS
               MOVE 'ARREARS COLLECTED'    TO LNT-NARRATIVE
           ELSE
               MOVE 'MONTHLY REPAYMENT'    TO LNT-NARRATIVE
           END-IF
           PERFORM 5390-INSERT-HISTORY
           .
      *================================================================*
       5390-INSERT-HISTORY.
      *================================================================*
           MOVE LN-LOAN-NO                 TO LNT-LOAN-NO
           MOVE WS-DATE-ISO                TO LNT-TXN-DATE
           MOVE LN-PRINCIPAL-BAL           TO LNT-PRINCIPAL-AFTER
           MOVE LN-ARREARS-AMT             TO LNT-ARREARS-AFTER
           MOVE WS-PROGRAM-ID              TO LNT-SOURCE-PGM
           EXEC SQL
               SELECT COALESCE(MAX(TXN_SEQ), 0) + 1
                 INTO :WS-TXN-SEQ
                 FROM MBK.LOANTXN
                WHERE LOAN_NO  = :LNT-LOAN-NO
                  AND TXN_DATE = :LNT-TXN-DATE
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '5390-INSERT-HISTORY'  TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE WS-TXN-SEQ                 TO LNT-TXN-SEQ
           EXEC SQL
               INSERT INTO MBK.LOANTXN
                     ( LOAN_NO, TXN_DATE, TXN_SEQ, TXN_TYPE,
                       DR_CR_IND, AMOUNT, INT_PORTION, FEE_PORTION,
                       PRIN_PORTION, PRINCIPAL_AFTER, ARREARS_AFTER,
                       NARRATIVE, SOURCE_PGM, INSERT_TS )
               VALUES ( :LNT-LOAN-NO, :LNT-TXN-DATE, :LNT-TXN-SEQ,
                        :LNT-TXN-TYPE, :LNT-DR-CR-IND, :LNT-AMOUNT,
                        :LNT-INT-PORTION, :LNT-FEE-PORTION,
                        :LNT-PRIN-PORTION, :LNT-PRINCIPAL-AFTER,
                        :LNT-ARREARS-AFTER, :LNT-NARRATIVE,
                        :LNT-SOURCE-PGM, CURRENT TIMESTAMP )
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '5390-INSERT-HISTORY'  TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *================================================================*
       5400-ARREARS-CLEARED.
      *================================================================*
           MOVE ZERO                       TO LN-MISSED-COUNT
                                              LN-ARREARS-SINCE
                                              LN-ARREARS-BUCKET
                                              LN-RETRY-DATE
                                              LN-RETRY-COUNT
                                              LN-FEE-CHARGED-MISSED
           IF LN-DEFAULTED
      *        DEFAULT REMEDIED (CCA S89) - BACK TO ACTIVE.
               SET LN-ACTIVE               TO TRUE
               MOVE ZERO                   TO LN-DEFAULT-DATE
                                              LN-DEFAULT-REMEDY-DATE
           END-IF
           MOVE 'ARRC'                     TO LNX-LETTER-CD
           MOVE 'LNARC01'                  TO LNX-TEMPLATE-ID
           PERFORM 5500-WRITE-LETTER
           MOVE 'ARREARS CLEARED'          TO WS-NOTE
           .
      *================================================================*
       5500-WRITE-LETTER.
      *================================================================*
           MOVE LNX-LETTER-CD              TO LN-LAST-LETTER-CD
           MOVE WS-BUS-DATE                TO LN-LAST-LETTER-DATE
                                              LNX-ISSUE-DATE
           MOVE LN-LOAN-NO                 TO LNX-LOAN-NO
           MOVE LN-CIF-NO                  TO LNX-CIF-NO
           MOVE LN-AGREEMENT-REF           TO LNX-AGREEMENT-REF
           MOVE LN-ADVANCE-AMT             TO LNX-ADVANCE-AMT
           MOVE LN-PRINCIPAL-BAL           TO LNX-PRINCIPAL-BAL
           MOVE LN-ACCRUED-INT             TO LNX-ACCRUED-INT
           MOVE LN-ARREARS-AMT             TO LNX-ARREARS-AMT
           MOVE LN-FEES-OUTSTANDING        TO LNX-FEES-AMT
           MOVE LN-MONTHLY-PAYMENT         TO LNX-MONTHLY-PAYMENT
           MOVE ZERO                       TO LNX-SETTLEMENT-AMT
                                              LNX-REMEDY-DATE
                                              LNX-PERIOD-FROM
                                              LNX-PERIOD-TO
                                              LNX-PERIOD-INT
                                              LNX-PERIOD-FEES
           MOVE LN-NOMINAL-RATE            TO LNX-NOMINAL-RATE
           MOVE LN-APR                     TO LNX-APR
           MOVE LN-TERM-MONTHS             TO LNX-TERM-MONTHS
           MOVE LN-PAYMENTS-REMAINING      TO LNX-PAYMENTS-REMAINING
           MOVE LN-MISSED-COUNT            TO LNX-MISSED-COUNT
           MOVE LN-NEXT-DUE-DATE           TO LNX-NEXT-DUE-DATE
           MOVE WS-AMOUNT-DUE              TO LNX-PERIOD-PAID
           MOVE LN-REPAY-SORT-CODE         TO LNX-REPAY-SORT-CODE
           MOVE LN-REPAY-ACCT-NO           TO LNX-REPAY-ACCT-NO
           MOVE SPACES                     TO LNX-DOC-REF
           STRING LNX-LETTER-CD LN-LOAN-NO WS-BUS-DATE (3:6)
                  DELIMITED BY SIZE
                  INTO LNX-DOC-REF
           END-STRING
           WRITE LETTERS-RECORD            FROM LNX-LETTER-RECORD
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '5500-WRITE-LETTER'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-WRITE-JOURNAL.
      *================================================================*
           ACCEPT WS-TIME-NOW              FROM TIME
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-LOANS               TO TRUE
           STRING 'LNREP' WS-BUS-DATE
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE 'LNREP'                    TO GLJH-RUN-ID
           MOVE WS-BUS-DATE                TO GLJH-CREATE-DATE
           MOVE WS-TIME-NOW (1:6)          TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3900-WRITE-GL

           PERFORM VARYING GT-IDX FROM 1 BY 1
                   UNTIL GT-IDX > WS-GT-COUNT
               IF WS-GT-INTEREST (GT-IDX) > ZERO
                   MOVE WS-GL-PRINCIPAL    TO GLJD-GL-ACCOUNT
                   MOVE 'D'                TO GLJD-DR-CR-IND
                   MOVE WS-GT-INTEREST (GT-IDX) TO GLJD-AMOUNT
                   MOVE 'RECLASS INTEREST FROM PRINCIPAL'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3100-DETAIL
                   MOVE WS-GT-GL-INT-REC (GT-IDX) TO GLJD-GL-ACCOUNT
                   MOVE 'C'                TO GLJD-DR-CR-IND
                   MOVE 'LOAN INTEREST RECEIVED'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3100-DETAIL
               END-IF
               IF WS-GT-FEES (GT-IDX) > ZERO
                   MOVE WS-GL-PRINCIPAL    TO GLJD-GL-ACCOUNT
                   MOVE 'D'                TO GLJD-DR-CR-IND
                   MOVE WS-GT-FEES (GT-IDX) TO GLJD-AMOUNT
                   MOVE 'RECLASS FEES FROM PRINCIPAL'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3100-DETAIL
                   MOVE WS-GL-FEE-REC      TO GLJD-GL-ACCOUNT
                   MOVE 'C'                TO GLJD-DR-CR-IND
                   MOVE 'LOAN FEES RECEIVED' TO GLJD-DESCRIPTION
                   PERFORM 3100-DETAIL
               END-IF
           END-PERFORM

           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-GL-LINES                TO GLJT-LINE-COUNT
           MOVE WS-GL-DR                   TO GLJT-TOTAL-DR
           MOVE WS-GL-CR                   TO GLJT-TOTAL-CR
           MOVE 1                          TO GLJT-JOURNAL-COUNT
           PERFORM 3900-WRITE-GL
           .
      *================================================================*
       3100-DETAIL.
      *================================================================*
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-LINE-NO
           MOVE WS-LINE-NO                 TO GLJD-LINE-NO
           MOVE WS-GT-COST-CENTRE (GT-IDX) TO GLJD-COST-CENTRE
           MOVE SPACES                     TO GLJD-PROD-CD
                                              GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE 'GBP'                      TO GLJD-CCY-CD
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE 'LNR '                     TO GLJD-TXN-CODE
           MOVE GLJ-JOURNAL-ID             TO GLJD-SOURCE-REF
           PERFORM 3900-WRITE-GL
           ADD 1                           TO WS-GL-LINES
           IF GLJD-DEBIT
               ADD GLJD-AMOUNT             TO WS-GL-DR
           ELSE
               ADD GLJD-AMOUNT             TO WS-GL-CR
           END-IF
           .
      *================================================================*
       3900-WRITE-GL.
      *================================================================*
           WRITE GLJNL-RECORD              FROM GLJ-JOURNAL-LINE
           IF WS-GLJNL-STATUS NOT = '00'
               MOVE '3900-WRITE-GL'        TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'GLJNL WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-GLJNL-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       4000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'LOANS READ (OPEN)'        TO TL-LABEL
           MOVE WS-LOANS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INSTALMENTS DUE'          TO TL-LABEL
           MOVE WS-INSTALMENTS-DUE         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  PAID'                   TO TL-LABEL
           MOVE WS-INSTALMENTS-PAID        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  MISSED'                 TO TL-LABEL
           MOVE WS-INSTALMENTS-MISSED      TO TL-COUNT
           MOVE WS-TOTAL-MISSED            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ARREARS RETRIES / PAID'   TO TL-LABEL
           MOVE WS-RETRIES                 TO TL-COUNT
           MOVE WS-RETRIES-PAID            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TOTAL COLLECTED'          TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-COLLECTED         TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  INTEREST'               TO TL-LABEL
           MOVE WS-TOTAL-INT-PART          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  FEES'                   TO TL-LABEL
           MOVE WS-TOTAL-FEE-PART          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  PRINCIPAL'              TO TL-LABEL
           MOVE WS-TOTAL-PRIN-PART         TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LOANS SETTLED'            TO TL-LABEL
           MOVE WS-LOANS-SETTLED           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REPAYMENT ACCOUNT NOT FOUND' TO TL-LABEL
           MOVE WS-NO-REPAY-ACCT           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RESERVE TABLE FULL'       TO TL-LABEL
           MOVE WS-RESERVE-FULL            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE LOANMAST-FILE ACCTMAST-FILE PRDPARM-FILE LNPOST-FILE
                 GLJNL-FILE LETTERS-FILE REPORT-FILE
           DISPLAY 'LNREP010 - PAID     ' WS-INSTALMENTS-PAID
           DISPLAY 'LNREP010 - MISSED   ' WS-INSTALMENTS-MISSED
           DISPLAY 'LNREP010 - SETTLED  ' WS-LOANS-SETTLED
           IF WS-GL-DR NOT = WS-GL-CR
               MOVE '4000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL RECLASS JOURNAL OUT OF BALANCE'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF WS-NO-REPAY-ACCT > ZERO OR WS-RESERVE-FULL > ZERO
               MOVE 4                      TO RETURN-CODE
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
           MOVE WS-MA-DATE                 TO WS-MA-RESULT
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
      *    PAID INSTALMENTS WITH NO NOTE ARE NOT LISTED.
           IF WS-RESULT = 'PAID' AND WS-NOTE = SPACES
               GO TO 7100-EXIT
           END-IF
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE LN-LOAN-NO                 TO DL-LOAN-NO
           MOVE WS-COLLECTION-TYPE         TO DL-TYPE
           MOVE LN-REPAY-SORT-CODE         TO DL-REPAY-SC
           MOVE LN-REPAY-ACCT-NO           TO DL-REPAY-ACCT
           MOVE WS-AMOUNT-DUE              TO DL-DUE
           MOVE LN-ARREARS-AMT             TO DL-ARREARS
           MOVE LN-PRINCIPAL-BAL           TO DL-PRINCIPAL
           MOVE WS-RESULT                  TO DL-RESULT
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
       7100-EXIT.
           EXIT.
      *================================================================*
       8000-READ-LOAN.
      *================================================================*
           READ LOANMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-LOANMAST-OK
                   CONTINUE
               WHEN WS-LOANMAST-EOF
                   SET WS-END-OF-LOANS     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-LOAN'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'LOANMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-LOANMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
