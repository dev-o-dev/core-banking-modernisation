       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNSTM010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-10-11.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : LNSTM010                                            *
      * TITLE    : PERSONAL LOANS - ANNUAL STATEMENTS (CCA S77A)       *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : LNSTM010                                            *
      * JCL      : LNDLY01 STEP LN050                                  *
      *----------------------------------------------------------------*
      * INPUT    : DATECARD - BUSINESS DATE CONTROL CARD               *
      *            MBK.LOANTXN - MOVEMENTS IN THE STATEMENT PERIOD     *
      * UPDATE   : LOANMAST - LOAN MASTER (VSAM KSDS, SEQUENTIAL I-O)  *
      *            MBK.LOANLTR - EVIDENCE THAT THE STATEMENT WAS SENT  *
      * OUTPUT   : LETTERS  - ANNUAL STATEMENTS (LNLTRREC, CODE ANST)  *
      *            REPORT   - STATEMENT REPORT (FBA 133)               *
      *----------------------------------------------------------------*
      * THE CONSUMER CREDIT ACT 1974 S77A (INSERTED BY CCA 2006, IN    *
      * FORCE OCTOBER 2008) REQUIRES A STATEMENT AT LEAST ONCE A YEAR  *
      * FOR FIXED-SUM CREDIT, THE FIRST WITHIN ONE YEAR OF THE         *
      * AGREEMENT. FAILURE MAKES THE AGREEMENT UNENFORCEABLE AND THE   *
      * BORROWER IS NOT LIABLE FOR INTEREST FOR THE PERIOD OF          *
      * NON-COMPLIANCE.                                                *
      *                                                                *
      * A STATEMENT IS DUE WHEN LN-ANNUAL-STMT-DATE + 1 YEAR - 7 DAYS  *
      * <= BUSINESS DATE (THE 7 DAYS ALLOW FOR PRINT AND POST). THE    *
      * STATEMENT PERIOD RUNS FROM LN-ANNUAL-STMT-DATE TO THE BUSINESS *
      * DATE AND SHOWS PAYMENTS, INTEREST AND FEES IN THE PERIOD       *
      * (SUMMED FROM MBK.LOANTXN) AND THE BALANCES TODAY.              *
      *                                                                *
      * DEFAULTED, COLLECTIONS AND WRITTEN-OFF LOANS STILL GET A       *
      * STATEMENT - THE OBLIGATION CONTINUES UNTIL THE DEBT IS PAID.   *
      * SETTLED LOANS DO NOT.                                          *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-10-11 JNB  ORIGINAL VERSION                               *
      * 2012-05-14 PDQ  WRITTEN-OFF LOANS INCLUDED (AUDIT IA-12-007 -  *
      *                 STATEMENTS HAD STOPPED AT WRITE-OFF)           *
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNSTM010'.
      *
       01  WS-CONSTANTS.
           05  WS-CONTROL-KEY              PIC X(10) VALUE '0000000000'.
           05  WS-LEAD-DAYS                PIC S9(03) COMP-3 VALUE +7.
           05  WS-OVERDUE-DAYS             PIC S9(03) COMP-3 VALUE +30.
      *
       01  WS-FILE-STATUSES.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00'.
               88  WS-LOANMAST-EOF             VALUE '10'.
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-LOANS             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-DATE-ISO                 PIC X(10).
           05  WS-ANNIV                    PIC 9(08).
           05  WS-ANNIV-R REDEFINES WS-ANNIV.
               10  WS-ANNIV-CCYY           PIC 9(04).
               10  WS-ANNIV-MMDD           PIC 9(04).
           05  WS-ANNIV-INT                PIC S9(09) COMP.
           05  WS-LATE-DAYS                PIC S9(05) COMP-3.
           05  WS-FROM-ISO                 PIC X(10).
           05  WS-NOTE                     PIC X(40).
      *
       01  HV-FROM-DATE                    PIC X(10).
       01  HV-TO-DATE                      PIC X(10).
       01  HV-PAID                         PIC S9(13)V99 COMP-3.
       01  HV-INTEREST                     PIC S9(13)V99 COMP-3.
       01  HV-FEES                         PIC S9(13)V99 COMP-3.
       01  HV-NULL-IND                     PIC S9(04) COMP OCCURS 3.
      *
       01  WS-COUNTERS.
           05  WS-LOANS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-STATEMENTS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-LATE-STATEMENTS          PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOT-DUE                  PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'LNSTM010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - LOAN ANNUAL STATEMENTS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(12) VALUE 'LOAN NO'.
           05  FILLER            PIC X(04) VALUE 'ST'.
           05  FILLER            PIC X(10) VALUE 'FROM'.
           05  FILLER            PIC X(10) VALUE 'TO'.
           05  FILLER            PIC X(15) VALUE '         PAID'.
           05  FILLER            PIC X(15) VALUE '     INTEREST'.
           05  FILLER            PIC X(15) VALUE '         FEES'.
           05  FILLER            PIC X(15) VALUE '    BALANCE'.
           05  FILLER            PIC X(36) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-LOAN-NO                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-STATUS                   PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-FROM                     PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TO                       PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PAID                     PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-INTEREST                 PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-FEES                     PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-BALANCE                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(35).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
           COPY LNLTRREC.
      *
           COPY DATECARD.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLLNLT END-EXEC.
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
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  DATECARD-FILE
                I-O    LOANMAST-FILE
                OUTPUT LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-LOANMAST-OK
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
           OR LN-SETTLED
           OR LN-PENDING-DRAWDOWN
               PERFORM 8000-READ-LOAN
               GO TO 2000-EXIT
           END-IF
           ADD 1                           TO WS-LOANS-READ

      *---- ANNIVERSARY OF THE LAST STATEMENT (29 FEB -> 28 FEB) ------*
           MOVE LN-ANNUAL-STMT-DATE        TO WS-ANNIV
           ADD 1                           TO WS-ANNIV-CCYY
           IF WS-ANNIV-MMDD = 0229
               MOVE 0228                   TO WS-ANNIV-MMDD
           END-IF
           COMPUTE WS-ANNIV-INT = FUNCTION INTEGER-OF-DATE (WS-ANNIV)
           IF WS-ANNIV-INT - WS-LEAD-DAYS > WS-BUS-DATE-INT
               ADD 1                       TO WS-NOT-DUE
               PERFORM 8000-READ-LOAN
               GO TO 2000-EXIT
           END-IF

           MOVE SPACES                     TO WS-NOTE
           COMPUTE WS-LATE-DAYS = WS-BUS-DATE-INT - WS-ANNIV-INT
           IF WS-LATE-DAYS > WS-OVERDUE-DAYS
      *        A MISSED STATEMENT - COMPLIANCE MUST ASSESS WHETHER
      *        INTEREST FOR THE PERIOD OF NON-COMPLIANCE IS REFUNDED.
               ADD 1                       TO WS-LATE-STATEMENTS
               MOVE 'LATE - REFER TO COMPLIANCE (S77A)'
                                           TO WS-NOTE
           END-IF

           PERFORM 2100-SUM-PERIOD
           PERFORM 2200-WRITE-STATEMENT
           PERFORM 2300-RECORD-NOTICE

           MOVE WS-BUS-DATE                TO LN-ANNUAL-STMT-DATE
           MOVE 'ANST'                     TO LN-LAST-LETTER-CD
           MOVE WS-BUS-DATE                TO LN-LAST-LETTER-DATE
                                              LN-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO LN-LAST-MAINT-USER
           REWRITE LN-LOAN-RECORD
           IF NOT WS-LOANMAST-OK
               MOVE '2000-PROCESS-LOAN'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LOANMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LOANMAST-STATUS     TO ABN-FILE-STATUS
               MOVE LN-KEY                 TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-STATEMENTS
           PERFORM 7100-REPORT-DETAIL
           PERFORM 8000-READ-LOAN
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-SUM-PERIOD.
      *================================================================*
           STRING LN-ANNUAL-STMT-DATE (1:4) '-'
                  LN-ANNUAL-STMT-DATE (5:2) '-'
                  LN-ANNUAL-STMT-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO WS-FROM-ISO
           END-STRING
           MOVE WS-FROM-ISO                TO HV-FROM-DATE
           MOVE WS-DATE-ISO                TO HV-TO-DATE
           MOVE LN-LOAN-NO                 TO LNL-LOAN-NO
           EXEC SQL
               SELECT SUM(CASE WHEN TXN_TYPE = 'REP '
                               THEN AMOUNT ELSE 0 END),
                      SUM(CASE WHEN TXN_TYPE = 'REP '
                               THEN INT_PORTION ELSE 0 END),
                      SUM(CASE WHEN TXN_TYPE = 'FEE '
                               THEN AMOUNT ELSE 0 END)
                 INTO :HV-PAID     :HV-NULL-IND(1),
                      :HV-INTEREST :HV-NULL-IND(2),
                      :HV-FEES     :HV-NULL-IND(3)
                 FROM MBK.LOANTXN
                WHERE LOAN_NO   = :LNL-LOAN-NO
                  AND TXN_DATE  > :HV-FROM-DATE
                  AND TXN_DATE <= :HV-TO-DATE
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2100-SUM-PERIOD'      TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           IF HV-NULL-IND (1) < 0
               MOVE ZERO                   TO HV-PAID
           END-IF
           IF HV-NULL-IND (2) < 0
               MOVE ZERO                   TO HV-INTEREST
           END-IF
           IF HV-NULL-IND (3) < 0
               MOVE ZERO                   TO HV-FEES
           END-IF
           .
      *================================================================*
       2200-WRITE-STATEMENT.
      *================================================================*
           MOVE SPACES                     TO LNX-LETTER-RECORD
           MOVE 'ANST'                     TO LNX-LETTER-CD
           MOVE 'LNANS01'                  TO LNX-TEMPLATE-ID
           MOVE WS-BUS-DATE                TO LNX-ISSUE-DATE
           MOVE LN-LOAN-NO                 TO LNX-LOAN-NO
           MOVE LN-CIF-NO                  TO LNX-CIF-NO
           MOVE LN-AGREEMENT-REF           TO LNX-AGREEMENT-REF
           MOVE LN-ADVANCE-AMT             TO LNX-ADVANCE-AMT
           MOVE LN-PRINCIPAL-BAL           TO LNX-PRINCIPAL-BAL
           MOVE LN-ACCRUED-INT             TO LNX-ACCRUED-INT
           MOVE LN-ARREARS-AMT             TO LNX-ARREARS-AMT
           MOVE LN-FEES-OUTSTANDING        TO LNX-FEES-AMT
           MOVE LN-MONTHLY-PAYMENT         TO LNX-MONTHLY-PAYMENT
           COMPUTE LNX-SETTLEMENT-AMT = LN-PRINCIPAL-BAL
                   + LN-ACCRUED-INT + LN-FEES-OUTSTANDING
           MOVE LN-NOMINAL-RATE            TO LNX-NOMINAL-RATE
           MOVE LN-APR                     TO LNX-APR
           MOVE LN-TERM-MONTHS             TO LNX-TERM-MONTHS
           MOVE LN-PAYMENTS-REMAINING      TO LNX-PAYMENTS-REMAINING
           MOVE LN-MISSED-COUNT            TO LNX-MISSED-COUNT
           MOVE LN-NEXT-DUE-DATE           TO LNX-NEXT-DUE-DATE
           MOVE ZERO                       TO LNX-REMEDY-DATE
           MOVE LN-ANNUAL-STMT-DATE        TO LNX-PERIOD-FROM
           MOVE WS-BUS-DATE                TO LNX-PERIOD-TO
           MOVE HV-PAID                    TO LNX-PERIOD-PAID
           MOVE HV-INTEREST                TO LNX-PERIOD-INT
           MOVE HV-FEES                    TO LNX-PERIOD-FEES
           MOVE LN-REPAY-SORT-CODE         TO LNX-REPAY-SORT-CODE
           MOVE LN-REPAY-ACCT-NO           TO LNX-REPAY-ACCT-NO
           STRING 'ANST' LN-LOAN-NO WS-BUS-DATE (3:6)
                  DELIMITED BY SIZE
                  INTO LNX-DOC-REF
           END-STRING
           WRITE LETTERS-RECORD            FROM LNX-LETTER-RECORD
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '2200-WRITE-STATEMENT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2300-RECORD-NOTICE.
      *================================================================*
           MOVE LN-LOAN-NO                 TO LNL-LOAN-NO
           MOVE 'ANST'                     TO LNL-LETTER-CD
           MOVE WS-DATE-ISO                TO LNL-ISSUE-DATE
           MOVE LN-CIF-NO                  TO LNL-CIF-NO
           MOVE LN-ARREARS-AMT             TO LNL-ARREARS-AMT
           MOVE LNX-SETTLEMENT-AMT         TO LNL-BALANCE-AMT
           MOVE LN-MISSED-COUNT            TO LNL-MISSED-COUNT
           MOVE 'LNANS01'                  TO LNL-TEMPLATE-ID
           MOVE LNX-DOC-REF                TO LNL-DOC-REF
           EXEC SQL
               INSERT INTO MBK.LOANLTR
                     ( LOAN_NO, LETTER_CD, ISSUE_DATE, CIF_NO,
                       ARREARS_AMT, BALANCE_AMT, MISSED_COUNT,
                       TEMPLATE_ID, DOC_REF, INSERT_TS )
               VALUES ( :LNL-LOAN-NO, :LNL-LETTER-CD, :LNL-ISSUE-DATE,
                        :LNL-CIF-NO, :LNL-ARREARS-AMT,
                        :LNL-BALANCE-AMT, :LNL-MISSED-COUNT,
                        :LNL-TEMPLATE-ID, :LNL-DOC-REF,
                        CURRENT TIMESTAMP )
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = -803
               MOVE '2300-RECORD-NOTICE'   TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'LOANS READ (NOT SETTLED)' TO TL-LABEL
           MOVE WS-LOANS-READ              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ANNUAL STATEMENTS PRODUCED' TO TL-LABEL
           MOVE WS-STATEMENTS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH LATE (OVER 30 DAYS)' TO TL-LABEL
           MOVE WS-LATE-STATEMENTS         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NOT YET DUE'              TO TL-LABEL
           MOVE WS-NOT-DUE                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE LOANMAST-FILE LETTERS-FILE REPORT-FILE
           DISPLAY 'LNSTM010 - STATEMENTS ' WS-STATEMENTS
           IF WS-LATE-STATEMENTS > ZERO
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
           MOVE LN-LOAN-NO                 TO DL-LOAN-NO
           MOVE LN-STATUS                  TO DL-STATUS
           MOVE LNX-PERIOD-FROM            TO DL-FROM
           MOVE WS-BUS-DATE                TO DL-TO
           MOVE HV-PAID                    TO DL-PAID
           MOVE HV-INTEREST                TO DL-INTEREST
           MOVE HV-FEES                    TO DL-FEES
           MOVE LNX-SETTLEMENT-AMT         TO DL-BALANCE
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
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
