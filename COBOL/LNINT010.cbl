       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNINT010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-09-06.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : LNINT010                                            *
      * TITLE    : PERSONAL LOANS - DAILY INTEREST ACCRUAL             *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : LNDLY01 STEP LN020                                  *
      *----------------------------------------------------------------*
      * INPUT    : DATECARD - BUSINESS DATE CONTROL CARD               *
      *            PRDPARM  - PRODUCT PARAMETERS (GL CODES, DAY BASIS) *
      * UPDATE   : LOANMAST - LOAN MASTER (VSAM KSDS, SEQUENTIAL I-O)  *
      * OUTPUT   : GLJNL    - LOAN INTEREST JOURNAL (GLJNLREC)         *
      *            REPORT   - ACCRUAL REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * INTEREST ACCRUES EACH CALENDAR DAY FROM LN-LAST-ACCRUAL-DATE   *
      * TO THE BUSINESS DATE (WEEKENDS AND BANK HOLIDAYS ARE CAUGHT UP *
      * ON THE NEXT BUSINESS DAY) ON THE PRINCIPAL BALANCE AT THE      *
      * NOMINAL RATE, ACTUAL/365 (PRD-DR-DAY-BASIS IF SET).            *
      *                                                                *
      * THE DAILY FIGURE IS CARRIED TO 6 DECIMAL PLACES IN             *
      * LN-INT-RESIDUE; WHOLE PENCE MOVE TO LN-ACCRUED-INT.            *
      *                                                                *
      * NO INTEREST ACCRUES WHEN:                                      *
      *   - THE LOAN IS NOT ACTIVE OR DEFAULTED (COLLECTIONS, WRITTEN  *
      *     OFF AND SETTLED LOANS ARE SKIPPED)                         *
      *   - LN-INT-FROZEN (FORBEARANCE - FSA TCF / ICG ARREARS         *
      *     GUIDANCE) - THE DAYS ARE STILL CONSUMED                    *
      *   - THE LOAN IS INSIDE ITS 14-DAY WITHDRAWAL PERIOD? NO -      *
      *     INTEREST DOES ACCRUE; IF THE CUSTOMER WITHDRAWS THEY PAY   *
      *     INTEREST FOR THE DAYS THEY HAD THE MONEY (CCD ART 14).     *
      *                                                                *
      * GL (ONE JOURNAL PER PRODUCT, PRODUCTS IN ASCENDING ORDER ON    *
      * THE MASTER IS NOT GUARANTEED SO TOTALS ARE TABLED):            *
      *   DR PRD-GL-INT-ACCR-REC   LOAN INTEREST RECEIVABLE            *
      *   CR PRD-GL-INT-INCOME     LOAN INTEREST INCOME                *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-09-06 JNB  ORIGINAL VERSION                               *
      * 2011-03-21 JNB  INTEREST FREEZE FOR FORBEARANCE                *
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
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT GLJNL-FILE       ASSIGN TO GLJNL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLJNL-STATUS.
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
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  GLJNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJNL-RECORD                    PIC X(250).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNINT010'.
      *
       01  WS-CONSTANTS.
           05  WS-CONTROL-KEY              PIC X(10) VALUE '0000000000'.
           05  WS-DEFAULT-DAY-BASIS        PIC 9(03) VALUE 365.
           05  WS-DEFAULT-GL-REC           PIC X(10) VALUE '13000-0020'.
           05  WS-DEFAULT-GL-INC           PIC X(10) VALUE '41100-0010'.
           05  WS-MAX-PRODUCTS             PIC S9(04) COMP VALUE +50.
      *
       01  WS-FILE-STATUSES.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00'.
               88  WS-LOANMAST-EOF             VALUE '10'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-GLJNL-STATUS             PIC X(02).
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
           05  WS-LAST-INT                 PIC S9(09) COMP.
           05  WS-DAYS                     PIC S9(05) COMP-3.
           05  WS-DAY-BASIS                PIC 9(03).
           05  WS-DAILY-INT                PIC S9(09)V9(06) COMP-3.
           05  WS-PERIOD-INT               PIC S9(11)V9(06) COMP-3.
           05  WS-WHOLE-PENCE              PIC S9(13)V99 COMP-3.
           05  WS-GL-REC                   PIC X(10).
           05  WS-GL-INC                   PIC X(10).
           05  WS-COST-CENTRE              PIC X(06).
           05  WS-LINE-NO                  PIC 9(07) VALUE ZERO.
           05  WS-JNL-COUNT                PIC 9(07) VALUE ZERO.
           05  WS-JNL-LINES                PIC 9(09) VALUE ZERO.
           05  WS-TIME-NOW                 PIC 9(08).
      *
      *---- PRODUCT TABLE (GL CODES AND TOTALS) -----------------------*
       01  WS-PRODUCT-TABLE.
           05  WS-PT-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-PT-ENTRY                 OCCURS 50 TIMES
                                           INDEXED BY PT-IDX.
               10  WS-PT-PROD-CD           PIC X(10).
               10  WS-PT-GL-REC            PIC X(10).
               10  WS-PT-GL-INC            PIC X(10).
               10  WS-PT-COST-CENTRE       PIC X(06).
               10  WS-PT-DAY-BASIS         PIC 9(03).
               10  WS-PT-LOANS             PIC S9(09) COMP-3.
               10  WS-PT-INTEREST          PIC S9(13)V99 COMP-3.
               10  WS-PT-PRINCIPAL         PIC S9(15)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-LOANS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOANS-ACCRUED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOANS-FROZEN             PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOANS-SKIPPED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOANS-ERROR              PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-INTEREST           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-PRINCIPAL          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-DR                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-CR                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'LNINT010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - LOAN INTEREST ACCRUAL'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(12) VALUE 'PRODUCT'.
           05  FILLER            PIC X(14) VALUE '       LOANS'.
           05  FILLER            PIC X(24) VALUE '       PRINCIPAL'.
           05  FILLER            PIC X(22) VALUE '      INTEREST TODAY'.
           05  FILLER            PIC X(12) VALUE '  GL REC'.
           05  FILLER            PIC X(48) VALUE '  GL INCOME'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-PRODUCT-LINE.
           05  PL-CC                       PIC X(01) VALUE ' '.
           05  PL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PL-LOANS                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  PL-PRINCIPAL                PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  PL-INTEREST                 PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  PL-GL-REC                   PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PL-GL-INC                   PIC X(10).
           05  FILLER                      PIC X(37) VALUE SPACES.
       01  WS-EXCEPTION-LINE.
           05  EL-CC                       PIC X(01) VALUE ' '.
           05  EL-LOAN-NO                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-TEXT                     PIC X(60).
           05  FILLER                      PIC X(60) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY GLJNLREC.
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
           PERFORM 2000-PROCESS-LOAN THRU 2000-EXIT
               UNTIL WS-END-OF-LOANS
           PERFORM 3000-WRITE-JOURNALS
           PERFORM 4000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       DATECARD-FILE
                I-O    LOANMAST-FILE
                OUTPUT GLJNL-FILE
                       REPORT-FILE
           IF NOT WS-LOANMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-GLJNL-STATUS NOT = '00'
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
           PERFORM 8000-READ-LOAN
           .
      *================================================================*
       2000-PROCESS-LOAN.
      *================================================================*
           IF LN-LOAN-NO = WS-CONTROL-KEY
               PERFORM 8000-READ-LOAN
               GO TO 2000-EXIT
           END-IF
           ADD 1                           TO WS-LOANS-READ

           IF NOT (LN-ACTIVE OR LN-DEFAULTED)
               ADD 1                       TO WS-LOANS-SKIPPED
               PERFORM 8000-READ-LOAN
               GO TO 2000-EXIT
           END-IF

           IF LN-LAST-ACCRUAL-DATE NOT NUMERIC
           OR LN-LAST-ACCRUAL-DATE = ZERO
               MOVE LN-ADVANCE-DATE        TO LN-LAST-ACCRUAL-DATE
           END-IF
           COMPUTE WS-LAST-INT =
                   FUNCTION INTEGER-OF-DATE (LN-LAST-ACCRUAL-DATE)
           COMPUTE WS-DAYS = WS-BUS-DATE-INT - WS-LAST-INT
           IF WS-DAYS <= ZERO
      *        ALREADY ACCRUED (RERUN) OR A FUTURE-DATED ADVANCE.
               PERFORM 8000-READ-LOAN
               GO TO 2000-EXIT
           END-IF
           IF WS-DAYS > 10
               MOVE LN-LOAN-NO             TO EL-LOAN-NO
               MOVE 'MORE THAN 10 DAYS SINCE LAST ACCRUAL - CHECK'
                                           TO EL-TEXT
               PERFORM 7200-EXCEPTION
           END-IF

           PERFORM 2100-FIND-PRODUCT

           IF LN-INT-FROZEN
               ADD 1                       TO WS-LOANS-FROZEN
           ELSE
               IF LN-PRINCIPAL-BAL > ZERO
                   COMPUTE WS-DAILY-INT =
                       LN-PRINCIPAL-BAL * LN-NOMINAL-RATE
                       / 100 / WS-DAY-BASIS
                   COMPUTE WS-PERIOD-INT =
                       WS-DAILY-INT * WS-DAYS + LN-INT-RESIDUE
                   MOVE WS-PERIOD-INT      TO WS-WHOLE-PENCE
                   COMPUTE LN-INT-RESIDUE =
                       WS-PERIOD-INT - WS-WHOLE-PENCE
                   ADD WS-WHOLE-PENCE      TO LN-ACCRUED-INT
                                              LN-TOTAL-INT-CHARGED
                   ADD WS-WHOLE-PENCE      TO WS-PT-INTEREST (PT-IDX)
                                              WS-TOTAL-INTEREST
                   ADD 1                   TO WS-LOANS-ACCRUED
               END-IF
           END-IF
           ADD 1                           TO WS-PT-LOANS (PT-IDX)
           ADD LN-PRINCIPAL-BAL            TO WS-PT-PRINCIPAL (PT-IDX)
                                              WS-TOTAL-PRINCIPAL

           MOVE WS-BUS-DATE                TO LN-LAST-ACCRUAL-DATE
           MOVE WS-BUS-DATE                TO LN-LAST-MAINT-DATE
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
           PERFORM 8000-READ-LOAN
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-FIND-PRODUCT.
      *================================================================*
           SET PT-IDX                      TO 1
           SEARCH WS-PT-ENTRY
               AT END
                   PERFORM 2150-ADD-PRODUCT
               WHEN PT-IDX > WS-PT-COUNT
                   PERFORM 2150-ADD-PRODUCT
               WHEN WS-PT-PROD-CD (PT-IDX) = LN-PROD-CD
                   CONTINUE
           END-SEARCH
           MOVE WS-PT-DAY-BASIS (PT-IDX)   TO WS-DAY-BASIS
           .
      *================================================================*
       2150-ADD-PRODUCT.
      *================================================================*
           IF WS-PT-COUNT >= WS-MAX-PRODUCTS
               MOVE '2150-ADD-PRODUCT'     TO ABN-PARAGRAPH
               MOVE 1006                   TO ABN-ABEND-CODE
               MOVE 'MORE THAN 50 LOAN PRODUCTS - TABLE FULL'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-PT-COUNT
           SET PT-IDX                      TO WS-PT-COUNT
           MOVE LN-PROD-CD                 TO WS-PT-PROD-CD (PT-IDX)
           MOVE ZERO                       TO WS-PT-LOANS (PT-IDX)
                                              WS-PT-INTEREST (PT-IDX)
                                              WS-PT-PRINCIPAL (PT-IDX)
           MOVE WS-DEFAULT-GL-REC          TO WS-PT-GL-REC (PT-IDX)
           MOVE WS-DEFAULT-GL-INC          TO WS-PT-GL-INC (PT-IDX)
           MOVE SPACES                     TO WS-PT-COST-CENTRE (PT-IDX)
           MOVE WS-DEFAULT-DAY-BASIS       TO WS-PT-DAY-BASIS (PT-IDX)
           MOVE LN-PROD-CD                 TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               IF PRD-GL-INT-ACCR-REC NOT = SPACES
                   MOVE PRD-GL-INT-ACCR-REC TO WS-PT-GL-REC (PT-IDX)
               END-IF
               IF PRD-GL-INT-INCOME NOT = SPACES
                   MOVE PRD-GL-INT-INCOME  TO WS-PT-GL-INC (PT-IDX)
               END-IF
               MOVE PRD-COST-CENTRE        TO WS-PT-COST-CENTRE (PT-IDX)
               IF PRD-DR-DAY-BASIS = 360 OR 365
                   MOVE PRD-DR-DAY-BASIS   TO WS-PT-DAY-BASIS (PT-IDX)
               END-IF
           ELSE
               MOVE LN-LOAN-NO             TO EL-LOAN-NO
               MOVE 'PRODUCT NOT ON PRDPARM - DEFAULT GL CODES USED'
                                           TO EL-TEXT
               PERFORM 7200-EXCEPTION
           END-IF
           .
      *================================================================*
       3000-WRITE-JOURNALS.
      *================================================================*
           ACCEPT WS-TIME-NOW              FROM TIME
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-LOANS               TO TRUE
           STRING 'LNINT' WS-BUS-DATE
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE 'LNINT'                    TO GLJH-RUN-ID
           MOVE WS-BUS-DATE                TO GLJH-CREATE-DATE
           MOVE WS-TIME-NOW (1:6)          TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3900-WRITE-GL

           PERFORM VARYING PT-IDX FROM 1 BY 1
                   UNTIL PT-IDX > WS-PT-COUNT
               IF WS-PT-INTEREST (PT-IDX) > ZERO
                   PERFORM 3100-PRODUCT-LINES
                   ADD 1                   TO WS-JNL-COUNT
               END-IF
               PERFORM 7100-PRODUCT-LINE
           END-PERFORM

           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-JNL-LINES               TO GLJT-LINE-COUNT
           MOVE WS-TOTAL-DR                TO GLJT-TOTAL-DR
           MOVE WS-TOTAL-CR                TO GLJT-TOTAL-CR
           MOVE WS-JNL-COUNT               TO GLJT-JOURNAL-COUNT
           PERFORM 3900-WRITE-GL
           .
      *================================================================*
       3100-PRODUCT-LINES.
      *================================================================*
           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-LINE-NO
           MOVE WS-LINE-NO                 TO GLJD-LINE-NO
           MOVE WS-PT-GL-REC (PT-IDX)      TO GLJD-GL-ACCOUNT
           MOVE WS-PT-COST-CENTRE (PT-IDX) TO GLJD-COST-CENTRE
           MOVE WS-PT-PROD-CD (PT-IDX)     TO GLJD-PROD-CD
           MOVE 'GBP'                      TO GLJD-CCY-CD
           SET GLJD-DEBIT                  TO TRUE
           MOVE WS-PT-INTEREST (PT-IDX)    TO GLJD-AMOUNT
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE 'LOAN INTEREST ACCRUED - RECEIVABLE'
                                           TO GLJD-DESCRIPTION
           MOVE 'INT '                     TO GLJD-TXN-CODE
           MOVE GLJ-JOURNAL-ID             TO GLJD-SOURCE-REF
           PERFORM 3900-WRITE-GL
           ADD WS-PT-INTEREST (PT-IDX)     TO WS-TOTAL-DR
           ADD 1                           TO WS-JNL-LINES

           ADD 1                           TO WS-LINE-NO
           MOVE WS-LINE-NO                 TO GLJD-LINE-NO
           MOVE WS-PT-GL-INC (PT-IDX)      TO GLJD-GL-ACCOUNT
           SET GLJD-CREDIT                 TO TRUE
           MOVE 'LOAN INTEREST ACCRUED - INCOME'
                                           TO GLJD-DESCRIPTION
           PERFORM 3900-WRITE-GL
           ADD WS-PT-INTEREST (PT-IDX)     TO WS-TOTAL-CR
           ADD 1                           TO WS-JNL-LINES
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
           MOVE 'LOANS READ'               TO TL-LABEL
           MOVE WS-LOANS-READ              TO TL-COUNT
           MOVE WS-TOTAL-PRINCIPAL         TO TL-AMOUNT
           PERFORM 7300-TOTAL
           MOVE 'LOANS ACCRUED / INTEREST' TO TL-LABEL
           MOVE WS-LOANS-ACCRUED           TO TL-COUNT
           MOVE WS-TOTAL-INTEREST          TO TL-AMOUNT
           PERFORM 7300-TOTAL
           MOVE 'INTEREST FROZEN (FORBEARANCE)' TO TL-LABEL
           MOVE WS-LOANS-FROZEN            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7300-TOTAL
           MOVE 'NOT ACCRUING (SETTLED/COLLECTIONS/W-O)'
                                           TO TL-LABEL
           MOVE WS-LOANS-SKIPPED           TO TL-COUNT
           PERFORM 7300-TOTAL
           MOVE 'GL JOURNAL DEBITS'        TO TL-LABEL
           MOVE WS-JNL-LINES               TO TL-COUNT
           MOVE WS-TOTAL-DR                TO TL-AMOUNT
           PERFORM 7300-TOTAL
           MOVE 'GL JOURNAL CREDITS'       TO TL-LABEL
           MOVE WS-JNL-COUNT               TO TL-COUNT
           MOVE WS-TOTAL-CR                TO TL-AMOUNT
           PERFORM 7300-TOTAL

           CLOSE LOANMAST-FILE PRDPARM-FILE GLJNL-FILE REPORT-FILE
           DISPLAY 'LNINT010 - LOANS ACCRUED ' WS-LOANS-ACCRUED
           DISPLAY 'LNINT010 - INTEREST      ' WS-TOTAL-INTEREST
           IF WS-TOTAL-DR NOT = WS-TOTAL-CR
               MOVE '4000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL JOURNAL OUT OF BALANCE' TO ABN-MESSAGE
               PERFORM Z999-ABEND
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
       7100-PRODUCT-LINE.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE WS-PT-PROD-CD (PT-IDX)     TO PL-PROD-CD
           MOVE WS-PT-LOANS (PT-IDX)       TO PL-LOANS
           MOVE WS-PT-PRINCIPAL (PT-IDX)   TO PL-PRINCIPAL
           MOVE WS-PT-INTEREST (PT-IDX)    TO PL-INTEREST
           MOVE WS-PT-GL-REC (PT-IDX)      TO PL-GL-REC
           MOVE WS-PT-GL-INC (PT-IDX)      TO PL-GL-INC
           WRITE REPORT-RECORD             FROM WS-PRODUCT-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-EXCEPTION.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-EXCEPTION-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7300-TOTAL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE - 2
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           ADD 2                           TO WS-LINE-COUNT
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
      *
           COPY ABNDPROC.
