       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNBKG010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-09-06.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : LNBKG010                                            *
      * TITLE    : PERSONAL LOANS - BOOK APPROVED LOANS AND DRAW DOWN  *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : LNBKG010                                            *
      * JCL      : LNDLY01 STEP LN010                                  *
      *----------------------------------------------------------------*
      * INPUT    : APPROVED - APPROVED LOANS FROM LOS (LNORGREC)       *
      *            PRDPARM  - PRODUCT PARAMETERS (RANDOM)              *
      *            CIFMAST  - CIF CUSTOMER MASTER (RANDOM)             *
      *            ACCTMAST - DDA ACCOUNT MASTER (RANDOM, READ ONLY)   *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : LOANMAST - LOAN MASTER (VSAM KSDS, RANDOM) - NEW    *
      *                       LOANS AND THE LOAN NUMBER CONTROL RECORD *
      *            MBK.LOANTXN - 'ADV ' ROW PER LOAN                   *
      * OUTPUT   : LNPOST   - DRAWDOWN CREDITS FOR PSEOD01 (PSTXNREC)  *
      *            LETTERS  - WELCOME LETTERS (LNLTRREC)               *
      *            REJECTS  - REJECTED APPLICATIONS BACK TO LOS (FB    *
      *                       300 - LNORGREC + REASON IN COLS 281-300) *
      *            REPORT   - BOOKING REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * FOR EACH APPROVED LOAN WHOSE DRAWDOWN DATE IS ON OR BEFORE THE *
      * BUSINESS DATE (FUTURE-DATED LOANS ARE REJECTED BACK TO THE LOS *
      * WITH REASON 'FUTR' - THE LOS RESENDS THEM ON THE DAY):         *
      *   1. VALIDATE CUSTOMER, PRODUCT, PAYOUT AND REPAYMENT ACCOUNTS *
      *   2. RECALCULATE THE INSTALMENT (ANNUITY FORMULA) AND REJECT   *
      *      IF IT DIFFERS FROM THE LOS FIGURE BY MORE THAN 1P         *
      *   3. ALLOCATE THE LOAN NUMBER FROM THE CONTROL RECORD (KEY     *
      *      '0000000000', LAST NUMBER IN COLS 11-20)                  *
      *   4. WRITE THE LOAN MASTER, THE 'ADV ' HISTORY ROW AND THE     *
      *      'LND ' CREDIT TO THE PAYOUT ACCOUNT                       *
      *   5. WRITE THE WELCOME LETTER (CCA S61A COPY OF AGREEMENT IS   *
      *      SENT BY THE LOS - THIS LETTER CONFIRMS THE DATES)         *
      * THE 14-DAY RIGHT OF WITHDRAWAL (CONSUMER CREDIT DIRECTIVE,     *
      * FROM 1 FEBRUARY 2011) IS RECORDED IN LN-WITHDRAWAL-END-DATE.   *
      *                                                                *
      * GL: THE 'LND ' CREDIT IS EXTRACTED BY GLEXT010 AND MBK.GLMAP   *
      * MAPS IT TO LOANS PRINCIPAL 13000-0010.                         *
      *                                                                *
      * KNOWN ISSUE: LNO-SETTLE-LOAN-NO (CONSOLIDATION OF AN EXISTING  *
      * MEGABANK LOAN) IS NOT ACTIONED - THE OLD LOAN IS SETTLED BY    *
      * LENDING OPERATIONS BY HAND FROM THE REPORT. PIR 2010/112 OPEN. *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-09-06 JNB  ORIGINAL VERSION                               *
      * 2010-12-13 JNB  RIGHT OF WITHDRAWAL END DATE (CCD)             *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APPROVED-FILE    ASSIGN TO APPROVED
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-APPROVED-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT LOANMAST-FILE    ASSIGN TO LOANMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS LN-KEY
                                   FILE STATUS IS WS-LOANMAST-STATUS.
           SELECT LNPOST-FILE      ASSIGN TO LNPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LNPOST-STATUS.
           SELECT LETTERS-FILE     ASSIGN TO LETTERS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LETTERS-STATUS.
           SELECT REJECTS-FILE     ASSIGN TO REJECTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REJECTS-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  APPROVED-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  APPROVED-RECORD                 PIC X(300).
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  LOANMAST-FILE.
           COPY LNACCREC.
       FD  LNPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LNPOST-RECORD                   PIC X(200).
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(300).
       FD  REJECTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REJECTS-RECORD                  PIC X(300).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNBKG010'.
      *
       01  WS-CONSTANTS.
           05  WS-CONTROL-KEY              PIC X(10) VALUE '0000000000'.
           05  WS-TOLERANCE                PIC S9(01)V99 COMP-3
                                           VALUE +0.01.
           05  WS-WITHDRAWAL-DAYS          PIC S9(03) COMP-3 VALUE +14.
           05  WS-MIN-FIRST-DAYS           PIC S9(03) COMP-3 VALUE +14.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00060000.
           05  WS-MIN-AGE                  PIC 9(03) VALUE 018.
      *
       01  WS-FILE-STATUSES.
           05  WS-APPROVED-STATUS          PIC X(02).
               88  WS-APPROVED-OK              VALUE '00'.
               88  WS-APPROVED-EOF             VALUE '10'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00'.
               88  WS-LOANMAST-DUPKEY          VALUE '22'.
               88  WS-LOANMAST-NOTFND          VALUE '23'.
           05  WS-LNPOST-STATUS            PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REJECTS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-TRAILER-IND              PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-REJECT-RSN               PIC X(04).
           05  WS-REJECT-TEXT              PIC X(40).
           05  WS-LAST-LOAN-NO             PIC 9(10).
           05  WS-NEW-LOAN-NO              PIC 9(10).
           05  WS-CONTROL-DIRTY-IND        PIC X(01) VALUE 'N'.
           05  WS-AGE                      PIC 9(03).
           05  WS-CALC-PAYMENT             PIC S9(13)V99 COMP-3.
           05  WS-DIFF                     PIC S9(13)V99 COMP-3.
           05  WS-MONTHLY-RATE             COMP-2.
           05  WS-FACTOR                   COMP-2.
           05  WS-PAYMENT-FLOAT            COMP-2.
           05  WS-TOTAL-PAYABLE            PIC S9(13)V99 COMP-3.
           05  WS-FINAL-PAYMENT            PIC S9(13)V99 COMP-3.
           05  WS-FIRST-DUE                PIC 9(08).
           05  WS-FINAL-DUE                PIC 9(08).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-TS                       PIC X(26).
           05  WS-DATE-ISO                 PIC X(10).
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
           05  WS-MA-RESULT                PIC 9(08).
       01  WS-DAYS-IN-MONTH-VALUES         PIC X(24)
                                VALUE '312831303130313130313031'.
       01  WS-DAYS-IN-MONTH-TABLE REDEFINES WS-DAYS-IN-MONTH-VALUES.
           05  WS-DIM                      PIC 9(02) OCCURS 12 TIMES.
      *
       01  WS-REJECT-RECORD.
           05  WR-APPROVED                 PIC X(280).
           05  WR-REASON                   PIC X(04).
           05  WR-REJECT-DATE              PIC 9(08).
           05  FILLER                      PIC X(08).
      *
       01  WS-COUNTERS.
           05  WS-RECS-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BOOKED                   PIC S9(09) COMP-3 VALUE 0.
           05  WS-REJECTED                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-CONSOL-FLAGGED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-ADVANCE-TOTAL-IN         PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-ADVANCE-BOOKED           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-ADVANCE-REJECTED         PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'LNBKG010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - PERSONAL LOAN BOOKING'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(22) VALUE 'APPLICATION'.
           05  FILLER            PIC X(14) VALUE 'CUSTOMER'.
           05  FILLER            PIC X(12) VALUE 'LOAN NO'.
           05  FILLER            PIC X(17) VALUE '         ADVANCE'.
           05  FILLER            PIC X(05) VALUE 'TERM'.
           05  FILLER            PIC X(08) VALUE '   RATE'.
           05  FILLER            PIC X(15) VALUE '     INSTALMENT'.
           05  FILLER            PIC X(06) VALUE 'RSN'.
           05  FILLER            PIC X(33) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-APPLICATION              PIC X(20).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-LOAN-NO                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ADVANCE                  PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TERM                     PIC ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-RATE                     PIC ZZ9.9999.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-INSTALMENT               PIC ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RSN                      PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(34).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY LNORGREC.
      *
           COPY LNLTRREC.
      *
           COPY PSTXNREC.
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
           PERFORM 2000-PROCESS-RECORD
               UNTIL WS-END-OF-INPUT
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  APPROVED-FILE
                       PRDPARM-FILE
                       CIFMAST-FILE
                       ACCTMAST-FILE
                       DATECARD-FILE
                I-O    LOANMAST-FILE
                OUTPUT LNPOST-FILE
                       LETTERS-FILE
                       REJECTS-FILE
                       REPORT-FILE
           IF NOT WS-APPROVED-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-LOANMAST-OK
           OR WS-LNPOST-STATUS NOT = '00'
           OR WS-LETTERS-STATUS NOT = '00'
           OR WS-REJECTS-STATUS NOT = '00'
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

      *---- LOAN NUMBER CONTROL RECORD --------------------------------*
           MOVE WS-CONTROL-KEY             TO LN-LOAN-NO
           READ LOANMAST-FILE
           EVALUATE TRUE
               WHEN WS-LOANMAST-OK
                   MOVE LN-LOAN-RECORD (11:10) TO WS-LAST-LOAN-NO
               WHEN WS-LOANMAST-NOTFND
      *            FIRST RUN - LOAN NUMBERS START AT 7000000001
      *            (LENDPRO NUMBERS WERE 6XXXXXXXXX).
                   MOVE 7000000000         TO WS-LAST-LOAN-NO
                   MOVE SPACES             TO LN-LOAN-RECORD
                   MOVE WS-CONTROL-KEY     TO LN-LOAN-NO
                   MOVE WS-LAST-LOAN-NO    TO LN-LOAN-RECORD (11:10)
                   WRITE LN-LOAN-RECORD
               WHEN OTHER
                   MOVE '1000-INITIALISE'  TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'LOAN CONTROL RECORD READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-LOANMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE

           PERFORM 8000-READ-APPROVED
           IF WS-END-OF-INPUT OR NOT LNO-HEADER
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'APPROVED LOANS FILE HAS NO HEADER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           DISPLAY 'LNBKG010 - LOS FILE ' LNOH-FILE-DATE
                   ' SEQ ' LNOH-FILE-SEQ
           PERFORM 8000-READ-APPROVED
           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           EVALUATE TRUE
               WHEN LNO-DETAIL
                   PERFORM 2100-VALIDATE THRU 2100-EXIT
                   IF WS-REJECT-RSN = SPACES
                       PERFORM 2200-CALCULATE-SCHEDULE THRU 2200-EXIT
                   END-IF
                   IF WS-REJECT-RSN = SPACES
                       PERFORM 2300-BOOK-LOAN
                   ELSE
                       PERFORM 2800-REJECT
                   END-IF
               WHEN LNO-TRAILER
                   SET WS-TRAILER-SEEN     TO TRUE
                   IF LNOT-RECORD-COUNT NOT = WS-RECS-READ
                   OR LNOT-ADVANCE-TOTAL NOT = WS-ADVANCE-TOTAL-IN
                       MOVE '2000-PROCESS-RECORD' TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'LOS TRAILER COUNT / TOTAL MISMATCH'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           PERFORM 8000-READ-APPROVED
           .
      *================================================================*
       2100-VALIDATE.
      *================================================================*
           MOVE SPACES                     TO WS-REJECT-RSN
                                              WS-REJECT-TEXT
           EVALUATE TRUE
               WHEN LNO-ADVANCE-AMT NOT NUMERIC
               WHEN LNO-TERM-MONTHS NOT NUMERIC
               WHEN LNO-NOMINAL-RATE NOT NUMERIC
               WHEN LNO-MONTHLY-PAYMENT NOT NUMERIC
               WHEN LNO-PAYMENT-DAY NOT NUMERIC
               WHEN LNO-DRAWDOWN-DATE NOT NUMERIC
                   MOVE 'DATA'             TO WS-REJECT-RSN
                   MOVE 'NON-NUMERIC FIELD IN LOS RECORD'
                                           TO WS-REJECT-TEXT
               WHEN LNO-ADVANCE-AMT = ZERO
               WHEN LNO-TERM-MONTHS = ZERO
                   MOVE 'DATA'             TO WS-REJECT-RSN
                   MOVE 'ZERO ADVANCE OR TERM' TO WS-REJECT-TEXT
               WHEN LNO-PAYMENT-DAY < 1 OR LNO-PAYMENT-DAY > 28
                   MOVE 'PDAY'             TO WS-REJECT-RSN
                   MOVE 'PAYMENT DAY MUST BE 1-28' TO WS-REJECT-TEXT
               WHEN LNO-DRAWDOWN-DATE > WS-BUS-DATE
                   MOVE 'FUTR'             TO WS-REJECT-RSN
                   MOVE 'DRAWDOWN DATE IN THE FUTURE'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-RSN NOT = SPACES
               GO TO 2100-EXIT
           END-IF

      *---- PRODUCT ---------------------------------------------------*
           MOVE LNO-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN NOT WS-PRDPARM-OK
                   MOVE 'PROD'             TO WS-REJECT-RSN
                   MOVE 'PRODUCT NOT FOUND' TO WS-REJECT-TEXT
               WHEN NOT PRD-LOAN
                   MOVE 'PROD'             TO WS-REJECT-RSN
                   MOVE 'PRODUCT IS NOT A LOAN' TO WS-REJECT-TEXT
               WHEN PRD-WITHDRAWN
                   MOVE 'PROD'             TO WS-REJECT-RSN
                   MOVE 'PRODUCT WITHDRAWN' TO WS-REJECT-TEXT
               WHEN PRD-MAX-BALANCE > ZERO
               AND  LNO-ADVANCE-AMT > PRD-MAX-BALANCE
                   MOVE 'AMNT'             TO WS-REJECT-RSN
                   MOVE 'ADVANCE OVER PRODUCT MAXIMUM'
                                           TO WS-REJECT-TEXT
               WHEN PRD-TERM-MONTHS > ZERO
               AND  LNO-TERM-MONTHS > PRD-TERM-MONTHS
                   MOVE 'TERM'             TO WS-REJECT-RSN
                   MOVE 'TERM OVER PRODUCT MAXIMUM' TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-RSN NOT = SPACES
               GO TO 2100-EXIT
           END-IF

      *---- CUSTOMER --------------------------------------------------*
           MOVE LNO-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-CIFMAST-OK
                   MOVE 'CUST'             TO WS-REJECT-RSN
                   MOVE 'CUSTOMER NOT ON CIF' TO WS-REJECT-TEXT
               WHEN NOT CIF-ACTIVE
                   MOVE 'CUST'             TO WS-REJECT-RSN
                   MOVE 'CUSTOMER NOT ACTIVE' TO WS-REJECT-TEXT
               WHEN NOT CIF-PERSONAL
                   MOVE 'CUST'             TO WS-REJECT-RSN
                   MOVE 'PERSONAL LOANS FOR PERSONAL CUSTOMERS'
                                           TO WS-REJECT-TEXT
               WHEN NOT CIF-KYC-VERIFIED
                   MOVE 'KYC '             TO WS-REJECT-RSN
                   MOVE 'KYC NOT VERIFIED' TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-RSN NOT = SPACES
               GO TO 2100-EXIT
           END-IF
           IF CIF-DOB NUMERIC AND CIF-DOB > ZERO
               COMPUTE WS-AGE =
                   (WS-BUS-DATE - CIF-DOB) / 10000
               IF WS-AGE < WS-MIN-AGE
                   MOVE 'AGE '             TO WS-REJECT-RSN
                   MOVE 'CUSTOMER UNDER 18' TO WS-REJECT-TEXT
                   GO TO 2100-EXIT
               END-IF
           END-IF

      *---- PAYOUT ACCOUNT --------------------------------------------*
           MOVE LNO-PAYOUT-SORT-CODE       TO ACM-SORT-CODE
           MOVE LNO-PAYOUT-ACCT-NO         TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE 'PAYA'             TO WS-REJECT-RSN
                   MOVE 'PAYOUT ACCOUNT NOT FOUND' TO WS-REJECT-TEXT
               WHEN NOT ACM-OPEN
               WHEN ACM-NO-CREDITS OR ACM-NO-POSTINGS
                   MOVE 'PAYA'             TO WS-REJECT-RSN
                   MOVE 'PAYOUT ACCOUNT CANNOT TAKE CREDITS'
                                           TO WS-REJECT-TEXT
               WHEN ACM-CIF-NO NOT = LNO-CIF-NO
                   MOVE 'PAYA'             TO WS-REJECT-RSN
                   MOVE 'PAYOUT ACCOUNT NOT IN BORROWER NAME'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-RSN NOT = SPACES
               GO TO 2100-EXIT
           END-IF

      *---- REPAYMENT ACCOUNT -----------------------------------------*
           MOVE LNO-REPAY-SORT-CODE        TO ACM-SORT-CODE
           MOVE LNO-REPAY-ACCT-NO          TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE 'REPA'             TO WS-REJECT-RSN
                   MOVE 'REPAYMENT ACCOUNT NOT FOUND'
                                           TO WS-REJECT-TEXT
               WHEN NOT ACM-CURRENT
                   MOVE 'REPA'             TO WS-REJECT-RSN
                   MOVE 'REPAYMENT ACCOUNT NOT A CURRENT ACCOUNT'
                                           TO WS-REJECT-TEXT
               WHEN NOT ACM-OPEN
               WHEN ACM-NO-DEBITS OR ACM-NO-POSTINGS
                   MOVE 'REPA'             TO WS-REJECT-RSN
                   MOVE 'REPAYMENT ACCOUNT RESTRICTED'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-CALCULATE-SCHEDULE.
      *================================================================*
      *    ANNUITY: PMT = P * R / (1 - (1 + R) ** -N), R = RATE / 1200.
      *    FLOATING POINT IS USED FOR THE POWER ONLY; THE RESULT IS
      *    ROUNDED TO PENCE. THE FINAL INSTALMENT ABSORBS THE ROUNDING
      *    AND IS SET BY LNREP010 TO WHATEVER IS LEFT.
           IF LNO-NOMINAL-RATE = ZERO
               COMPUTE WS-CALC-PAYMENT ROUNDED =
                       LNO-ADVANCE-AMT / LNO-TERM-MONTHS
           ELSE
               COMPUTE WS-MONTHLY-RATE = LNO-NOMINAL-RATE / 1200
               COMPUTE WS-FACTOR =
                       (1 + WS-MONTHLY-RATE) ** LNO-TERM-MONTHS
               COMPUTE WS-PAYMENT-FLOAT =
                       LNO-ADVANCE-AMT * WS-MONTHLY-RATE * WS-FACTOR
                       / (WS-FACTOR - 1)
               COMPUTE WS-CALC-PAYMENT ROUNDED = WS-PAYMENT-FLOAT
           END-IF

           COMPUTE WS-DIFF = WS-CALC-PAYMENT - LNO-MONTHLY-PAYMENT
           IF WS-DIFF < ZERO
               COMPUTE WS-DIFF = ZERO - WS-DIFF
           END-IF
           IF WS-DIFF > WS-TOLERANCE
               MOVE 'CALC'                 TO WS-REJECT-RSN
               MOVE 'LOS INSTALMENT DOES NOT MATCH HOST'
                                           TO WS-REJECT-TEXT
               GO TO 2200-EXIT
           END-IF

           COMPUTE WS-TOTAL-PAYABLE =
                   LNO-MONTHLY-PAYMENT * LNO-TERM-MONTHS
           MOVE LNO-MONTHLY-PAYMENT        TO WS-FINAL-PAYMENT

      *---- FIRST DUE DATE: PAYMENT DAY NEXT MONTH, OR THE MONTH ------*
      *---- AFTER IF THAT IS LESS THAN 14 DAYS AWAY -------------------*
           MOVE LNO-DRAWDOWN-DATE          TO WS-MA-DATE
           MOVE 1                          TO WS-MA-MONTHS
           MOVE LNO-PAYMENT-DAY            TO WS-MA-DAY
           PERFORM 6000-ADD-MONTHS
           MOVE WS-MA-RESULT               TO WS-FIRST-DUE
           IF FUNCTION INTEGER-OF-DATE (WS-FIRST-DUE)
            - FUNCTION INTEGER-OF-DATE (LNO-DRAWDOWN-DATE)
              < WS-MIN-FIRST-DAYS
               MOVE WS-FIRST-DUE           TO WS-MA-DATE
               MOVE 1                      TO WS-MA-MONTHS
               PERFORM 6000-ADD-MONTHS
               MOVE WS-MA-RESULT           TO WS-FIRST-DUE
           END-IF
           MOVE WS-FIRST-DUE               TO WS-MA-DATE
           COMPUTE WS-MA-MONTHS = LNO-TERM-MONTHS - 1
           PERFORM 6000-ADD-MONTHS
           MOVE WS-MA-RESULT               TO WS-FINAL-DUE
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-BOOK-LOAN.
      *================================================================*
           ADD 1                           TO WS-LAST-LOAN-NO
           MOVE WS-LAST-LOAN-NO            TO WS-NEW-LOAN-NO
           MOVE 'Y'                        TO WS-CONTROL-DIRTY-IND

           MOVE SPACES                     TO LN-LOAN-RECORD
           MOVE WS-NEW-LOAN-NO             TO LN-LOAN-NO
           MOVE LNO-CIF-NO                 TO LN-CIF-NO
           MOVE LNO-PROD-CD                TO LN-PROD-CD
           SET LN-ACTIVE                   TO TRUE
           MOVE LNO-REPAY-SORT-CODE        TO LN-REPAY-SORT-CODE
           MOVE LNO-REPAY-ACCT-NO          TO LN-REPAY-ACCT-NO
           MOVE LNO-ADVANCE-AMT            TO LN-ADVANCE-AMT
           MOVE LNO-DRAWDOWN-DATE          TO LN-ADVANCE-DATE
           MOVE LNO-TERM-MONTHS            TO LN-TERM-MONTHS
           MOVE LNO-NOMINAL-RATE           TO LN-NOMINAL-RATE
           MOVE LNO-APR                    TO LN-APR
           MOVE LNO-MONTHLY-PAYMENT        TO LN-MONTHLY-PAYMENT
           MOVE WS-FINAL-PAYMENT           TO LN-FINAL-PAYMENT
           MOVE LNO-PAYMENT-DAY            TO LN-PAYMENT-DAY
           MOVE WS-FIRST-DUE               TO LN-FIRST-DUE-DATE
           MOVE WS-FINAL-DUE               TO LN-FINAL-DUE-DATE
           MOVE WS-TOTAL-PAYABLE           TO LN-TOTAL-PAYABLE
           MOVE LNO-PURPOSE-CD             TO LN-PURPOSE-CD
           MOVE LNO-AGREEMENT-REF          TO LN-AGREEMENT-REF
           MOVE LNO-APPLICATION-ID         TO LN-ORIG-SYSTEM-REF
           MOVE LNO-BRANCH-SORT-CODE       TO LN-BRANCH-SORT-CODE

           MOVE LNO-ADVANCE-AMT            TO LN-PRINCIPAL-BAL
           MOVE ZERO                       TO LN-ACCRUED-INT
                                              LN-INT-RESIDUE
                                              LN-FEES-OUTSTANDING
                                              LN-TOTAL-INT-CHARGED
                                              LN-TOTAL-INT-PAID
                                              LN-TOTAL-PRIN-PAID
                                              LN-TOTAL-FEES-PAID
                                              LN-INT-PAID-TAX-YEAR
           MOVE LNO-DRAWDOWN-DATE          TO LN-LAST-ACCRUAL-DATE

           MOVE WS-FIRST-DUE               TO LN-NEXT-DUE-DATE
           MOVE ZERO                       TO LN-PAYMENTS-MADE
                                              LN-LAST-PAY-DATE
                                              LN-LAST-PAY-AMT
                                              LN-RETRY-DATE
                                              LN-RETRY-COUNT
           MOVE LNO-TERM-MONTHS            TO LN-PAYMENTS-REMAINING

           MOVE ZERO                       TO LN-ARREARS-AMT
                                              LN-MISSED-COUNT
                                              LN-ARREARS-SINCE
                                              LN-ARREARS-BUCKET
                                              LN-WORST-BUCKET
                                              LN-NOSIA-DATE
                                              LN-NOSIA-COUNT
                                              LN-DEFAULT-DATE
                                              LN-DEFAULT-REMEDY-DATE
                                              LN-TERMINATION-DATE
                                              LN-COLLECTIONS-DATE
                                              LN-WRITE-OFF-DATE
                                              LN-WRITE-OFF-AMT
                                              LN-FORBEARANCE-END
                                              LN-FORBEARANCE-PAYMENT
                                              LN-LAST-LETTER-DATE
                                              LN-FEE-CHARGED-MISSED
           MOVE 'N'                        TO LN-FORBEARANCE-IND
                                              LN-INT-FROZEN-IND
           MOVE 'WELC'                     TO LN-LAST-LETTER-CD
           MOVE WS-BUS-DATE                TO LN-LAST-LETTER-DATE

           MOVE ZERO                       TO LN-SETTLE-QUOTE-DATE
                                              LN-SETTLE-QUOTE-AMT
                                              LN-SETTLE-VALID-TO
                                              LN-SETTLED-DATE
                                              LN-SETTLED-AMT
           MOVE LNO-DRAWDOWN-DATE          TO LN-ANNUAL-STMT-DATE
           COMPUTE LN-WITHDRAWAL-END-DATE = FUNCTION DATE-OF-INTEGER
                   (FUNCTION INTEGER-OF-DATE (LNO-SIGNED-DATE)
                    + WS-WITHDRAWAL-DAYS)
           MOVE WS-BUS-DATE                TO LN-OPENED-DATE
                                              LN-LAST-MAINT-DATE
           MOVE ZERO                       TO LN-CLOSED-DATE
           MOVE WS-PROGRAM-ID              TO LN-LAST-MAINT-USER

           WRITE LN-LOAN-RECORD
           IF NOT WS-LOANMAST-OK
               MOVE '2300-BOOK-LOAN'       TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LOANMAST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LOANMAST-STATUS     TO ABN-FILE-STATUS
               MOVE LN-KEY                 TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           PERFORM 2400-WRITE-HISTORY
           PERFORM 2500-WRITE-DRAWDOWN
           PERFORM 2600-WRITE-WELCOME

           ADD 1                           TO WS-BOOKED
           ADD LNO-ADVANCE-AMT             TO WS-ADVANCE-BOOKED
           IF LNO-SETTLE-LOAN-NO NOT = SPACES
               ADD 1                       TO WS-CONSOL-FLAGGED
               MOVE 'CONSOLIDATION - SETTLE OLD LOAN BY HAND'
                                           TO WS-REJECT-TEXT
           ELSE
               MOVE SPACES                 TO WS-REJECT-TEXT
           END-IF
           MOVE WS-NEW-LOAN-NO             TO DL-LOAN-NO
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2400-WRITE-HISTORY.
      *================================================================*
           MOVE LN-LOAN-NO                 TO LNT-LOAN-NO
           MOVE WS-DATE-ISO                TO LNT-TXN-DATE
           MOVE 1                          TO LNT-TXN-SEQ
           MOVE 'ADV '                     TO LNT-TXN-TYPE
           MOVE 'D'                        TO LNT-DR-CR-IND
           MOVE LN-ADVANCE-AMT             TO LNT-AMOUNT
                                              LNT-PRIN-PORTION
                                              LNT-PRINCIPAL-AFTER
           MOVE ZERO                       TO LNT-INT-PORTION
                                              LNT-FEE-PORTION
                                              LNT-ARREARS-AFTER
           MOVE 'LOAN ADVANCE'             TO LNT-NARRATIVE
           MOVE WS-PROGRAM-ID              TO LNT-SOURCE-PGM
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
               MOVE '2400-WRITE-HISTORY'   TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *================================================================*
       2500-WRITE-DRAWDOWN.
      *================================================================*
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE LNO-PAYOUT-SORT-CODE       TO PST-SORT-CODE
           MOVE LNO-PAYOUT-ACCT-NO         TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'LND '                     TO PST-TXN-CODE
           SET PST-CREDIT                  TO TRUE
           MOVE LNO-ADVANCE-AMT            TO PST-AMOUNT
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           MOVE 'MEGABANK PERSONAL LOAN'   TO PST-NARRATIVE-1
           STRING 'LOAN ' WS-NEW-LOAN-NO
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           MOVE LNO-AGREEMENT-REF          TO PST-EXT-REF
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE LNPOST-RECORD             FROM PST-POSTING-RECORD
           IF WS-LNPOST-STATUS NOT = '00'
               MOVE '2500-WRITE-DRAWDOWN'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LNPOST WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-LNPOST-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2600-WRITE-WELCOME.
      *================================================================*
           MOVE SPACES                     TO LNX-LETTER-RECORD
           MOVE 'WELC'                     TO LNX-LETTER-CD
           MOVE 'LNWEL01'                  TO LNX-TEMPLATE-ID
           MOVE WS-BUS-DATE                TO LNX-ISSUE-DATE
           MOVE LN-LOAN-NO                 TO LNX-LOAN-NO
           MOVE LN-CIF-NO                  TO LNX-CIF-NO
           MOVE LN-AGREEMENT-REF           TO LNX-AGREEMENT-REF
           MOVE LN-ADVANCE-AMT             TO LNX-ADVANCE-AMT
                                              LNX-PRINCIPAL-BAL
           MOVE ZERO                       TO LNX-ACCRUED-INT
                                              LNX-ARREARS-AMT
                                              LNX-FEES-AMT
                                              LNX-SETTLEMENT-AMT
                                              LNX-MISSED-COUNT
                                              LNX-PERIOD-PAID
                                              LNX-PERIOD-INT
                                              LNX-PERIOD-FEES
           MOVE LN-MONTHLY-PAYMENT         TO LNX-MONTHLY-PAYMENT
           MOVE LN-NOMINAL-RATE            TO LNX-NOMINAL-RATE
           MOVE LN-APR                     TO LNX-APR
           MOVE LN-TERM-MONTHS             TO LNX-TERM-MONTHS
                                              LNX-PAYMENTS-REMAINING
           MOVE LN-FIRST-DUE-DATE          TO LNX-NEXT-DUE-DATE
           MOVE LN-WITHDRAWAL-END-DATE     TO LNX-REMEDY-DATE
           MOVE LN-ADVANCE-DATE            TO LNX-PERIOD-FROM
           MOVE LN-FINAL-DUE-DATE          TO LNX-PERIOD-TO
           MOVE LN-REPAY-SORT-CODE         TO LNX-REPAY-SORT-CODE
           MOVE LN-REPAY-ACCT-NO           TO LNX-REPAY-ACCT-NO
           STRING 'WELC' LN-LOAN-NO WS-BUS-DATE (3:6)
                  DELIMITED BY SIZE
                  INTO LNX-DOC-REF
           END-STRING
           WRITE LETTERS-RECORD            FROM LNX-LETTER-RECORD
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '2600-WRITE-WELCOME'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2800-REJECT.
      *================================================================*
           MOVE LNO-APPROVED-LOAN (1:280)  TO WR-APPROVED
           MOVE WS-REJECT-RSN              TO WR-REASON
           MOVE WS-BUS-DATE                TO WR-REJECT-DATE
           WRITE REJECTS-RECORD            FROM WS-REJECT-RECORD
           IF WS-REJECTS-STATUS NOT = '00'
               MOVE '2800-REJECT'          TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'REJECTS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-REJECTS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-REJECTED
           IF LNO-ADVANCE-AMT NUMERIC
               ADD LNO-ADVANCE-AMT         TO WS-ADVANCE-REJECTED
           END-IF
           MOVE SPACES                     TO DL-LOAN-NO
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF NOT WS-TRAILER-SEEN
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'APPROVED LOANS FILE HAS NO TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           IF WS-CONTROL-DIRTY-IND = 'Y'
               MOVE WS-CONTROL-KEY         TO LN-LOAN-NO
               READ LOANMAST-FILE
               IF NOT WS-LOANMAST-OK
                   MOVE '3000-FINALISE'    TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'LOAN CONTROL RECORD RE-READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-LOANMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               END-IF
               MOVE WS-LAST-LOAN-NO        TO LN-LOAN-RECORD (11:10)
               REWRITE LN-LOAN-RECORD
           END-IF

           EXEC SQL COMMIT END-EXEC

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'APPROVED LOANS RECEIVED'  TO TL-LABEL
           MOVE WS-RECS-READ               TO TL-COUNT
           MOVE WS-ADVANCE-TOTAL-IN        TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LOANS BOOKED AND DRAWN DOWN' TO TL-LABEL
           MOVE WS-BOOKED                  TO TL-COUNT
           MOVE WS-ADVANCE-BOOKED          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REJECTED BACK TO LOS'     TO TL-LABEL
           MOVE WS-REJECTED                TO TL-COUNT
           MOVE WS-ADVANCE-REJECTED        TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CONSOLIDATIONS FOR MANUAL SETTLEMENT' TO TL-LABEL
           MOVE WS-CONSOL-FLAGGED          TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LAST LOAN NUMBER ALLOCATED' TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-LAST-LOAN-NO            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE APPROVED-FILE PRDPARM-FILE CIFMAST-FILE ACCTMAST-FILE
                 LOANMAST-FILE LNPOST-FILE LETTERS-FILE REJECTS-FILE
                 REPORT-FILE

           DISPLAY 'LNBKG010 - BOOKED   ' WS-BOOKED
           DISPLAY 'LNBKG010 - REJECTED ' WS-REJECTED
           IF WS-REJECTED > ZERO OR WS-CONSOL-FLAGGED > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       6000-ADD-MONTHS.
      *================================================================*
      *    WS-MA-DATE + WS-MA-MONTHS MONTHS, ON DAY WS-MA-DAY (OR THE
      *    LAST DAY OF THE MONTH IF SHORTER) -> WS-MA-RESULT.
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
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE LNO-APPLICATION-ID         TO DL-APPLICATION
           MOVE LNO-CIF-NO                 TO DL-CIF-NO
           IF LNO-ADVANCE-AMT NUMERIC
               MOVE LNO-ADVANCE-AMT        TO DL-ADVANCE
           ELSE
               MOVE ZERO                   TO DL-ADVANCE
           END-IF
           IF LNO-TERM-MONTHS NUMERIC
               MOVE LNO-TERM-MONTHS        TO DL-TERM
           ELSE
               MOVE ZERO                   TO DL-TERM
           END-IF
           IF LNO-NOMINAL-RATE NUMERIC
               MOVE LNO-NOMINAL-RATE       TO DL-RATE
           ELSE
               MOVE ZERO                   TO DL-RATE
           END-IF
           IF LNO-MONTHLY-PAYMENT NUMERIC
               MOVE LNO-MONTHLY-PAYMENT    TO DL-INSTALMENT
           ELSE
               MOVE ZERO                   TO DL-INSTALMENT
           END-IF
           MOVE WS-REJECT-RSN              TO DL-RSN
           MOVE WS-REJECT-TEXT             TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-APPROVED.
      *================================================================*
           READ APPROVED-FILE INTO LNO-APPROVED-LOAN
           EVALUATE TRUE
               WHEN WS-APPROVED-OK
                   IF LNO-DETAIL
                       ADD 1               TO WS-RECS-READ
                       IF LNO-ADVANCE-AMT NUMERIC
                           ADD LNO-ADVANCE-AMT TO WS-ADVANCE-TOTAL-IN
                       END-IF
                   END-IF
               WHEN WS-APPROVED-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-APPROVED' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'APPROVED READ FAILED' TO ABN-MESSAGE
                   MOVE WS-APPROVED-STATUS TO ABN-FILE-STATUS
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
