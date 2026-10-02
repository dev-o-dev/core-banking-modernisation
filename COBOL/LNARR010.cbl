       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNARR010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-09-20.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : LNARR010                                            *
      * TITLE    : PERSONAL LOANS - ARREARS, DEFAULT AND COLLECTIONS   *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : LNARR010                                            *
      * JCL      : LNDLY01 STEP LN040 (AFTER LNREP010)                 *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (LATE PAYMENT FEE)    *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : LOANMAST - LOAN MASTER (VSAM KSDS, SEQUENTIAL I-O)  *
      *            MBK.LOANTXN - 'FEE ' / 'WOFF' ROWS                  *
      *            MBK.LOANLTR - EVIDENCE OF REGULATORY NOTICES        *
      * OUTPUT   : LETTERS  - ARREARS LETTERS (LNLTRREC)               *
      *            COLLECT  - PLACEMENTS FOR THE DEBT COLLECTION       *
      *                       AGENCIES (FB 250, ONE FILE PER AGENCY IS *
      *                       SPLIT OUT BY SORT IN STEP LN045)         *
      *            GLJNL    - FEE AND WRITE-OFF JOURNAL (GLJNLREC)     *
      *            REPORT   - ARREARS REPORT AND AGEING (FBA 133)      *
      *----------------------------------------------------------------*
      * ACTIONS, IN ORDER, FOR EACH LOAN WITH ARREARS:                 *
      *   1. AGE THE ARREARS (DAYS SINCE LN-ARREARS-SINCE) INTO A      *
      *      BUCKET (SEE LNACCREC) AND TRACK THE WORST EVER BUCKET.    *
      *   2. LATE PAYMENT FEE - ONCE PER MISSED INSTALMENT, AFTER THE  *
      *      RETRY HAS FAILED (PRD-UNPAID-ITEM-FEE, CAPPED AT 12.00    *
      *      FOLLOWING THE OFT DEFAULT CHARGES STATEMENT 2006). NO FEE *
      *      WHILE IN FORBEARANCE.                                     *
      *   3. FIRST MISSED PAYMENT - INFORMAL REMINDER 'MISS'.          *
      *   4. TWO OR MORE MISSED - NOTICE OF SUMS IN ARREARS 'NSIA'     *
      *      (CCA S86B), THEN EVERY 6 MONTHS WHILE STILL IN ARREARS.   *
      *   5. THREE OR MORE MISSED AND NOT IN FORBEARANCE - DEFAULT     *
      *      NOTICE 'DFLT' (CCA S87/S88). REMEDY DATE = 14 DAYS + 7    *
      *      FOR POSTAL SERVICE. STATUS BECOMES DEFAULTED.             *
      *   6. DEFAULTED AND REMEDY DATE PASSED WITH ARREARS STILL       *
      *      OUTSTANDING - TERMINATE ('TERM'), STATUS COLLECTIONS, THE *
      *      WHOLE BALANCE BECOMES DUE AND THE LOAN IS PLACED WITH AN  *
      *      AGENCY. INTEREST STOPS (LNINT010 SKIPS STATUS C).         *
      *   7. COLLECTIONS FOR MORE THAN WS-WRITE-OFF-DAYS - WRITE OFF.  *
      *      THE DEBT REMAINS OWED; THE AGENCY CONTINUES TO COLLECT.   *
      * EXPIRED FORBEARANCE ARRANGEMENTS ARE ENDED AND INTEREST        *
      * RESTARTS.                                                      *
      *                                                                *
      * EVERY REGULATORY NOTICE (NSIA, DFLT, TERM) IS RECORDED ON      *
      * MBK.LOANLTR - A NOTICE THAT WAS NOT SENT MAKES THE AGREEMENT   *
      * UNENFORCEABLE FOR THE PERIOD OF NON-COMPLIANCE (S86D) SO THE   *
      * EVIDENCE MUST SURVIVE THE LETTER FILE.                         *
      *                                                                *
      * GL:                                                            *
      *   FEES       DR 13000-0030 LOAN FEES RECEIVABLE                *
      *              CR PRD-GL-FEE-INCOME (DEFAULT 42100-0010)         *
      *   WRITE-OFF  DR 54000-0010 LOAN IMPAIRMENT                     *
      *              CR 13000-0010 / 13000-0020 / 13000-0030           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-09-20 JNB  ORIGINAL VERSION                               *
      * 2011-03-21 JNB  FORBEARANCE (NO FEES, NO DEFAULT NOTICE)       *
      * 2012-05-14 PDQ  WRITE-OFF MOVED HERE FROM THE MONTH-END        *
      *                 SPREADSHEET PROCESS (AUDIT IA-12-007)          *
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
           SELECT LETTERS-FILE     ASSIGN TO LETTERS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LETTERS-STATUS.
           SELECT COLLECT-FILE     ASSIGN TO COLLECT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-COLLECT-STATUS.
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
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(300).
       FD  COLLECT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  COLLECT-RECORD                  PIC X(250).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNARR010'.
      *
       01  WS-CONSTANTS.
           05  WS-CONTROL-KEY              PIC X(10) VALUE '0000000000'.
           05  WS-FEE-CAP                  PIC S9(05)V99 COMP-3
                                           VALUE +12.00.
           05  WS-DEFAULT-FEE              PIC S9(05)V99 COMP-3
                                           VALUE +12.00.
           05  WS-NSIA-REPEAT-DAYS         PIC S9(05) COMP-3 VALUE +182.
           05  WS-REMEDY-DAYS              PIC S9(05) COMP-3 VALUE +21.
           05  WS-WRITE-OFF-DAYS           PIC S9(05) COMP-3 VALUE +180.
           05  WS-MISS-LETTER-GAP          PIC S9(05) COMP-3 VALUE +25.
           05  WS-GL-PRINCIPAL             PIC X(10) VALUE '13000-0010'.
           05  WS-GL-INT-REC               PIC X(10) VALUE '13000-0020'.
           05  WS-GL-FEE-REC               PIC X(10) VALUE '13000-0030'.
           05  WS-GL-FEE-INC-DFLT          PIC X(10) VALUE '42100-0010'.
           05  WS-GL-IMPAIRMENT            PIC X(10) VALUE '54000-0010'.
      *
      *---- DEBT COLLECTION AGENCIES - ROUND ROBIN --------------------*
       01  WS-AGENCY-VALUES.
           05  FILLER                      PIC X(04) VALUE 'DCA1'.
           05  FILLER                      PIC X(04) VALUE 'DCA2'.
           05  FILLER                      PIC X(04) VALUE 'DCA3'.
       01  WS-AGENCY-TABLE REDEFINES WS-AGENCY-VALUES.
           05  WS-AGENCY                   PIC X(04) OCCURS 3 TIMES.
       01  WS-AGENCY-NEXT                  PIC S9(04) COMP VALUE +1.
      *
       01  WS-FILE-STATUSES.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00'.
               88  WS-LOANMAST-EOF             VALUE '10'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-COLLECT-STATUS           PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-LOANS             VALUE 'Y'.
           05  WS-CHANGED-IND              PIC X(01).
               88  WS-LOAN-CHANGED             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-DATE-ISO                 PIC X(10).
           05  WS-DAYS-IN-ARREARS          PIC S9(07) COMP-3.
           05  WS-DAYS-SINCE               PIC S9(07) COMP-3.
           05  WS-FEE                      PIC S9(13)V99 COMP-3.
           05  WS-GL-FEE-INC               PIC X(10).
           05  WS-COST-CENTRE              PIC X(06).
           05  WS-ACTION                   PIC X(08).
           05  WS-TXN-SEQ                  PIC S9(09) COMP.
           05  WS-LINE-NO                  PIC 9(07) VALUE ZERO.
           05  WS-TIME-NOW                 PIC 9(08).
           05  WS-TOTAL-OWED               PIC S9(13)V99 COMP-3.
           05  WS-NEW-BUCKET               PIC 9(01).
           05  WS-BKT                      PIC S9(04) COMP.
      *
      *---- COLLECTIONS PLACEMENT RECORD ------------------------------*
       01  WS-COLLECT-RECORD.
           05  CP-AGENCY                   PIC X(04).
           05  CP-PLACEMENT-DATE           PIC 9(08).
           05  CP-LOAN-NO                  PIC X(10).
           05  CP-CIF-NO                   PIC X(12).
           05  CP-AGREEMENT-REF            PIC X(16).
           05  CP-ADVANCE-DATE             PIC 9(08).
           05  CP-ADVANCE-AMT              PIC 9(11)V99.
           05  CP-PRINCIPAL                PIC 9(11)V99.
           05  CP-INTEREST                 PIC 9(11)V99.
           05  CP-FEES                     PIC 9(11)V99.
           05  CP-TOTAL-OWED               PIC 9(11)V99.
           05  CP-LAST-PAY-DATE            PIC 9(08).
           05  CP-LAST-PAY-AMT             PIC 9(11)V99.
           05  CP-DEFAULT-DATE             PIC 9(08).
           05  CP-TERMINATION-DATE         PIC 9(08).
           05  CP-REPAY-SORT-CODE          PIC X(06).
           05  CP-REPAY-ACCT-NO            PIC X(08).
           05  FILLER                      PIC X(76).
      *
      *---- AGEING TABLE (BUCKETS 0-5 AS 1-6) -------------------------*
       01  WS-AGEING.
           05  WS-AGE-ENTRY                OCCURS 6 TIMES.
               10  WS-AGE-COUNT            PIC S9(09) COMP-3.
               10  WS-AGE-ARREARS          PIC S9(15)V99 COMP-3.
               10  WS-AGE-BALANCE          PIC S9(15)V99 COMP-3.
       01  WS-BUCKET-NAMES-VALUES.
           05  FILLER  PIC X(14) VALUE 'UP TO DATE'.
           05  FILLER  PIC X(14) VALUE '1-29 DAYS'.
           05  FILLER  PIC X(14) VALUE '30-59 DAYS'.
           05  FILLER  PIC X(14) VALUE '60-89 DAYS'.
           05  FILLER  PIC X(14) VALUE '90-179 DAYS'.
           05  FILLER  PIC X(14) VALUE '180+ DAYS'.
       01  WS-BUCKET-NAMES REDEFINES WS-BUCKET-NAMES-VALUES.
           05  WS-BUCKET-NAME              PIC X(14) OCCURS 6 TIMES.
      *
       01  WS-COUNTERS.
           05  WS-LOANS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-IN-ARREARS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-FEES-CHARGED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-MISS-LETTERS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-NSIA-LETTERS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DEFAULT-NOTICES          PIC S9(09) COMP-3 VALUE 0.
           05  WS-TERMINATIONS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-WRITE-OFFS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-FORBEARANCE-ENDED        PIC S9(09) COMP-3 VALUE 0.
           05  WS-FEE-TOTAL                PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-WOFF-PRIN                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-WOFF-INT                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-WOFF-FEES                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-DR                    PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-CR                    PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-LINES                 PIC 9(09) VALUE 0.
      *
      *---- FEE INCOME BY GL ACCOUNT ----------------------------------*
       01  WS-FEE-GL-TABLE.
           05  WS-FG-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-FG-ENTRY                 OCCURS 20 TIMES
                                           INDEXED BY FG-IDX.
               10  WS-FG-GL-ACCOUNT        PIC X(10).
               10  WS-FG-COST-CENTRE       PIC X(06).
               10  WS-FG-AMOUNT            PIC S9(13)V99 COMP-3.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'LNARR010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - LOAN ARREARS MANAGEMENT'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(12) VALUE 'LOAN NO'.
           05  FILLER            PIC X(14) VALUE 'CUSTOMER'.
           05  FILLER            PIC X(04) VALUE 'ST'.
           05  FILLER            PIC X(05) VALUE 'MISS'.
           05  FILLER            PIC X(06) VALUE ' DAYS'.
           05  FILLER            PIC X(04) VALUE 'BKT'.
           05  FILLER            PIC X(15) VALUE '     ARREARS'.
           05  FILLER            PIC X(15) VALUE '   PRINCIPAL'.
           05  FILLER            PIC X(10) VALUE 'ACTION'.
           05  FILLER            PIC X(43) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-LOAN-NO                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-STATUS                   PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-MISSED                   PIC ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DAYS                     PIC ZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-BUCKET                   PIC 9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ARREARS                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-PRINCIPAL                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
       01  WS-AGEING-LINE.
           05  AL-CC                       PIC X(01) VALUE ' '.
           05  AL-BUCKET                   PIC X(14).
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  AL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  AL-ARREARS                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  AL-BALANCE                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(59) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY LNLTRREC.
      *
           COPY GLJNLREC.
      *
           COPY DATECARD.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLLNTX END-EXEC.
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
           PERFORM 3000-WRITE-JOURNAL
           PERFORM 4000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       DATECARD-FILE
                I-O    LOANMAST-FILE
                OUTPUT LETTERS-FILE
                       COLLECT-FILE
                       GLJNL-FILE
                       REPORT-FILE
           IF NOT WS-LOANMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-LETTERS-STATUS NOT = '00'
           OR WS-COLLECT-STATUS NOT = '00'
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
           MOVE HD1-BUS-DATE               TO WS-DATE-ISO
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 6
               MOVE ZERO                   TO WS-AGE-COUNT (WS-BKT)
                                              WS-AGE-ARREARS (WS-BKT)
                                              WS-AGE-BALANCE (WS-BKT)
           END-PERFORM
      *    ROTATE THE STARTING AGENCY BY DAY NUMBER SO THAT SMALL
      *    DAILY VOLUMES ARE SPREAD EVENLY.
           COMPUTE WS-AGENCY-NEXT =
                   FUNCTION MOD (WS-BUS-DATE-INT, 3) + 1
           PERFORM 8000-READ-LOAN
           .
      *================================================================*
       2000-PROCESS-LOAN.
      *================================================================*
           IF LN-LOAN-NO = WS-CONTROL-KEY
           OR NOT LN-OPEN-STATUS
               PERFORM 8000-READ-LOAN
               GO TO 2000-EXIT
           END-IF
           ADD 1                           TO WS-LOANS-READ
           MOVE 'N'                        TO WS-CHANGED-IND
           MOVE SPACES                     TO WS-ACTION
                                              DL-NOTE

      *---- FORBEARANCE EXPIRY ----------------------------------------*
           IF LN-IN-FORBEARANCE
           AND LN-FORBEARANCE-END < WS-BUS-DATE
               MOVE 'N'                    TO LN-FORBEARANCE-IND
                                              LN-INT-FROZEN-IND
               MOVE ZERO                   TO LN-FORBEARANCE-PAYMENT
               ADD 1                       TO WS-FORBEARANCE-ENDED
               SET WS-LOAN-CHANGED         TO TRUE
               MOVE 'FBR END'              TO WS-ACTION
               MOVE 'FORBEARANCE EXPIRED - INTEREST RESTARTS'
                                           TO DL-NOTE
           END-IF

      *---- AGEING ----------------------------------------------------*
           IF LN-ARREARS-AMT > ZERO AND LN-ARREARS-SINCE > ZERO
               COMPUTE WS-DAYS-IN-ARREARS = WS-BUS-DATE-INT
                   - FUNCTION INTEGER-OF-DATE (LN-ARREARS-SINCE)
               EVALUATE TRUE
                   WHEN WS-DAYS-IN-ARREARS < 30
                       MOVE 1              TO WS-NEW-BUCKET
                   WHEN WS-DAYS-IN-ARREARS < 60
                       MOVE 2              TO WS-NEW-BUCKET
                   WHEN WS-DAYS-IN-ARREARS < 90
                       MOVE 3              TO WS-NEW-BUCKET
                   WHEN WS-DAYS-IN-ARREARS < 180
                       MOVE 4              TO WS-NEW-BUCKET
                   WHEN OTHER
                       MOVE 5              TO WS-NEW-BUCKET
               END-EVALUATE
               ADD 1                       TO WS-IN-ARREARS
           ELSE
               MOVE ZERO                   TO WS-NEW-BUCKET
                                              WS-DAYS-IN-ARREARS
           END-IF
           IF WS-NEW-BUCKET NOT = LN-ARREARS-BUCKET
               MOVE WS-NEW-BUCKET          TO LN-ARREARS-BUCKET
               SET WS-LOAN-CHANGED         TO TRUE
           END-IF
           IF LN-ARREARS-BUCKET > LN-WORST-BUCKET
               MOVE LN-ARREARS-BUCKET      TO LN-WORST-BUCKET
           END-IF
           COMPUTE WS-BKT = LN-ARREARS-BUCKET + 1
           ADD 1                           TO WS-AGE-COUNT (WS-BKT)
           ADD LN-ARREARS-AMT              TO WS-AGE-ARREARS (WS-BKT)
           ADD LN-PRINCIPAL-BAL            TO WS-AGE-BALANCE (WS-BKT)

           EVALUATE TRUE
               WHEN LN-COLLECTIONS
                   PERFORM 2700-CHECK-WRITE-OFF THRU 2700-EXIT
               WHEN LN-DEFAULTED
                   PERFORM 2600-CHECK-TERMINATE THRU 2600-EXIT
               WHEN LN-ARREARS-AMT > ZERO
                   PERFORM 2200-LATE-FEE THRU 2200-EXIT
                   PERFORM 2300-ARREARS-LETTERS THRU 2300-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

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
           IF WS-ACTION NOT = SPACES
               PERFORM 7100-REPORT-DETAIL
           END-IF
           PERFORM 8000-READ-LOAN
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2200-LATE-FEE.
      *================================================================*
      *    ONE FEE PER MISSED INSTALMENT, ONLY ONCE THE RETRY HAS
      *    BEEN MADE (LN-RETRY-COUNT = 1) OR THE RETRY DATE HAS GONE.
           IF LN-IN-FORBEARANCE
           OR LN-FEE-CHARGED-MISSED >= LN-MISSED-COUNT
               GO TO 2200-EXIT
           END-IF
           IF LN-RETRY-COUNT = ZERO
           AND LN-RETRY-DATE > ZERO
           AND LN-RETRY-DATE >= WS-BUS-DATE
               GO TO 2200-EXIT
           END-IF

           MOVE WS-DEFAULT-FEE             TO WS-FEE
           MOVE WS-GL-FEE-INC-DFLT         TO WS-GL-FEE-INC
           MOVE SPACES                     TO WS-COST-CENTRE
           MOVE LN-PROD-CD                 TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               IF PRD-UNPAID-ITEM-FEE NUMERIC
                   MOVE PRD-UNPAID-ITEM-FEE TO WS-FEE
               END-IF
               IF PRD-GL-FEE-INCOME NOT = SPACES
                   MOVE PRD-GL-FEE-INCOME  TO WS-GL-FEE-INC
               END-IF
               MOVE PRD-COST-CENTRE        TO WS-COST-CENTRE
           END-IF
           IF WS-FEE > WS-FEE-CAP
               MOVE WS-FEE-CAP             TO WS-FEE
           END-IF
           IF WS-FEE <= ZERO
               MOVE LN-MISSED-COUNT        TO LN-FEE-CHARGED-MISSED
               SET WS-LOAN-CHANGED         TO TRUE
               GO TO 2200-EXIT
           END-IF

           ADD WS-FEE                      TO LN-FEES-OUTSTANDING
           ADD WS-FEE                      TO LN-ARREARS-AMT
           MOVE LN-MISSED-COUNT            TO LN-FEE-CHARGED-MISSED
           SET WS-LOAN-CHANGED             TO TRUE
           ADD 1                           TO WS-FEES-CHARGED
           ADD WS-FEE                      TO WS-FEE-TOTAL

           SET FG-IDX                      TO 1
           SEARCH WS-FG-ENTRY
               AT END
                   MOVE '2200-LATE-FEE'    TO ABN-PARAGRAPH
                   MOVE 1006               TO ABN-ABEND-CODE
                   MOVE 'FEE GL TABLE FULL' TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               WHEN FG-IDX > WS-FG-COUNT
                   ADD 1                   TO WS-FG-COUNT
                   MOVE WS-GL-FEE-INC      TO WS-FG-GL-ACCOUNT (FG-IDX)
                   MOVE WS-COST-CENTRE     TO WS-FG-COST-CENTRE (FG-IDX)
                   MOVE ZERO               TO WS-FG-AMOUNT (FG-IDX)
               WHEN WS-FG-GL-ACCOUNT (FG-IDX) = WS-GL-FEE-INC
               AND  WS-FG-COST-CENTRE (FG-IDX) = WS-COST-CENTRE
                   CONTINUE
           END-SEARCH
           ADD WS-FEE                      TO WS-FG-AMOUNT (FG-IDX)

           MOVE 'FEE '                     TO LNT-TXN-TYPE
           MOVE 'D'                        TO LNT-DR-CR-IND
           MOVE WS-FEE                     TO LNT-AMOUNT
                                              LNT-FEE-PORTION
           MOVE ZERO                       TO LNT-INT-PORTION
                                              LNT-PRIN-PORTION
           MOVE 'LATE PAYMENT FEE'         TO LNT-NARRATIVE
           PERFORM 5000-INSERT-HISTORY
           MOVE 'FEE'                      TO WS-ACTION
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-ARREARS-LETTERS.
      *================================================================*
      *---- DEFAULT NOTICE --------------------------------------------*
           IF LN-MISSED-COUNT >= 3
           AND LN-DEFAULT-DATE = ZERO
           AND NOT LN-IN-FORBEARANCE
               SET LN-DEFAULTED            TO TRUE
               MOVE WS-BUS-DATE            TO LN-DEFAULT-DATE
               COMPUTE LN-DEFAULT-REMEDY-DATE = FUNCTION DATE-OF-INTEGER
                       (WS-BUS-DATE-INT + WS-REMEDY-DAYS)
               SET WS-LOAN-CHANGED         TO TRUE
               ADD 1                       TO WS-DEFAULT-NOTICES
               MOVE 'DFLT'                 TO LNX-LETTER-CD
               MOVE 'LNDFT01'              TO LNX-TEMPLATE-ID
               PERFORM 5500-WRITE-LETTER
               PERFORM 5600-RECORD-NOTICE
               MOVE 'DEFAULT'              TO WS-ACTION
               GO TO 2300-EXIT
           END-IF

      *---- NOTICE OF SUMS IN ARREARS ---------------------------------*
           IF LN-MISSED-COUNT >= 2
               IF LN-NOSIA-DATE = ZERO
                   MOVE ZERO               TO WS-DAYS-SINCE
               ELSE
                   COMPUTE WS-DAYS-SINCE = WS-BUS-DATE-INT
                       - FUNCTION INTEGER-OF-DATE (LN-NOSIA-DATE)
               END-IF
               IF LN-NOSIA-DATE = ZERO
               OR WS-DAYS-SINCE >= WS-NSIA-REPEAT-DAYS
                   MOVE WS-BUS-DATE        TO LN-NOSIA-DATE
                   ADD 1                   TO LN-NOSIA-COUNT
                   SET WS-LOAN-CHANGED     TO TRUE
                   ADD 1                   TO WS-NSIA-LETTERS
                   MOVE 'NSIA'             TO LNX-LETTER-CD
                   MOVE 'LNNSA01'          TO LNX-TEMPLATE-ID
                   PERFORM 5500-WRITE-LETTER
                   PERFORM 5600-RECORD-NOTICE
                   MOVE 'NOSIA'            TO WS-ACTION
               END-IF
               GO TO 2300-EXIT
           END-IF

      *---- FIRST MISSED PAYMENT REMINDER -----------------------------*
           IF LN-MISSED-COUNT = 1
               IF LN-LAST-LETTER-CD = 'MISS'
               AND LN-LAST-LETTER-DATE > ZERO
                   COMPUTE WS-DAYS-SINCE = WS-BUS-DATE-INT
                       - FUNCTION INTEGER-OF-DATE (LN-LAST-LETTER-DATE)
               ELSE
                   MOVE 999                TO WS-DAYS-SINCE
               END-IF
               IF WS-DAYS-SINCE > WS-MISS-LETTER-GAP
                   SET WS-LOAN-CHANGED     TO TRUE
                   ADD 1                   TO WS-MISS-LETTERS
                   MOVE 'MISS'             TO LNX-LETTER-CD
                   MOVE 'LNMIS01'          TO LNX-TEMPLATE-ID
                   PERFORM 5500-WRITE-LETTER
                   MOVE 'REMINDER'         TO WS-ACTION
               END-IF
           END-IF
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2600-CHECK-TERMINATE.
      *================================================================*
           IF LN-ARREARS-AMT = ZERO
      *        REMEDIED (LNREP010 NORMALLY RESETS THE STATUS).
               SET LN-ACTIVE               TO TRUE
               MOVE ZERO                   TO LN-DEFAULT-DATE
                                              LN-DEFAULT-REMEDY-DATE
               SET WS-LOAN-CHANGED         TO TRUE
               MOVE 'REMEDIED'             TO WS-ACTION
               GO TO 2600-EXIT
           END-IF
           IF LN-DEFAULT-REMEDY-DATE >= WS-BUS-DATE
      *        STILL INSIDE THE REMEDY PERIOD - FEES CONTINUE.
               PERFORM 2200-LATE-FEE THRU 2200-EXIT
               GO TO 2600-EXIT
           END-IF

           SET LN-COLLECTIONS              TO TRUE
           MOVE WS-BUS-DATE                TO LN-TERMINATION-DATE
                                              LN-COLLECTIONS-DATE
           MOVE WS-AGENCY (WS-AGENCY-NEXT) TO LN-COLLECTIONS-AGENCY
           ADD 1                           TO WS-AGENCY-NEXT
           IF WS-AGENCY-NEXT > 3
               MOVE 1                      TO WS-AGENCY-NEXT
           END-IF
           MOVE ZERO                       TO LN-NEXT-DUE-DATE
                                              LN-RETRY-DATE
           COMPUTE LN-ARREARS-AMT = LN-PRINCIPAL-BAL
                   + LN-ACCRUED-INT + LN-FEES-OUTSTANDING
           SET WS-LOAN-CHANGED             TO TRUE
           ADD 1                           TO WS-TERMINATIONS

           MOVE 'TERM'                     TO LNX-LETTER-CD
           MOVE 'LNTRM01'                  TO LNX-TEMPLATE-ID
           PERFORM 5500-WRITE-LETTER
           PERFORM 5600-RECORD-NOTICE

           MOVE SPACES                     TO WS-COLLECT-RECORD
           MOVE LN-COLLECTIONS-AGENCY      TO CP-AGENCY
           MOVE WS-BUS-DATE                TO CP-PLACEMENT-DATE
           MOVE LN-LOAN-NO                 TO CP-LOAN-NO
           MOVE LN-CIF-NO                  TO CP-CIF-NO
           MOVE LN-AGREEMENT-REF           TO CP-AGREEMENT-REF
           MOVE LN-ADVANCE-DATE            TO CP-ADVANCE-DATE
           MOVE LN-ADVANCE-AMT             TO CP-ADVANCE-AMT
           MOVE LN-PRINCIPAL-BAL           TO CP-PRINCIPAL
           MOVE LN-ACCRUED-INT             TO CP-INTEREST
           MOVE LN-FEES-OUTSTANDING        TO CP-FEES
           MOVE LN-ARREARS-AMT             TO CP-TOTAL-OWED
           MOVE LN-LAST-PAY-DATE           TO CP-LAST-PAY-DATE
           MOVE LN-LAST-PAY-AMT            TO CP-LAST-PAY-AMT
           MOVE LN-DEFAULT-DATE            TO CP-DEFAULT-DATE
           MOVE LN-TERMINATION-DATE        TO CP-TERMINATION-DATE
           MOVE LN-REPAY-SORT-CODE         TO CP-REPAY-SORT-CODE
           MOVE LN-REPAY-ACCT-NO           TO CP-REPAY-ACCT-NO
           WRITE COLLECT-RECORD            FROM WS-COLLECT-RECORD
           IF WS-COLLECT-STATUS NOT = '00'
               MOVE '2600-CHECK-TERMINATE' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'COLLECT WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-COLLECT-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           MOVE 'TERMINATE'                TO WS-ACTION
           STRING 'PLACED WITH ' LN-COLLECTIONS-AGENCY
                  DELIMITED BY SIZE
                  INTO DL-NOTE
           END-STRING
           .
       2600-EXIT.
           EXIT.
      *================================================================*
       2700-CHECK-WRITE-OFF.
      *================================================================*
           IF LN-COLLECTIONS-DATE = ZERO
               GO TO 2700-EXIT
           END-IF
           COMPUTE WS-DAYS-SINCE = WS-BUS-DATE-INT
                   - FUNCTION INTEGER-OF-DATE (LN-COLLECTIONS-DATE)
           IF WS-DAYS-SINCE < WS-WRITE-OFF-DAYS
               GO TO 2700-EXIT
           END-IF

           COMPUTE WS-TOTAL-OWED = LN-PRINCIPAL-BAL
                   + LN-ACCRUED-INT + LN-FEES-OUTSTANDING
           SET LN-WRITTEN-OFF              TO TRUE
           MOVE WS-BUS-DATE                TO LN-WRITE-OFF-DATE
           MOVE WS-TOTAL-OWED              TO LN-WRITE-OFF-AMT
           ADD LN-PRINCIPAL-BAL            TO WS-WOFF-PRIN
           ADD LN-ACCRUED-INT              TO WS-WOFF-INT
           ADD LN-FEES-OUTSTANDING         TO WS-WOFF-FEES
           ADD 1                           TO WS-WRITE-OFFS
           SET WS-LOAN-CHANGED             TO TRUE

           MOVE 'WOFF'                     TO LNT-TXN-TYPE
           MOVE 'C'                        TO LNT-DR-CR-IND
           MOVE WS-TOTAL-OWED              TO LNT-AMOUNT
           MOVE LN-ACCRUED-INT             TO LNT-INT-PORTION
           MOVE LN-FEES-OUTSTANDING        TO LNT-FEE-PORTION
           MOVE LN-PRINCIPAL-BAL           TO LNT-PRIN-PORTION
           MOVE 'WRITTEN OFF - DEBT REMAINS DUE'
                                           TO LNT-NARRATIVE
           PERFORM 5000-INSERT-HISTORY
           MOVE 'WRITE-OFF'                TO WS-ACTION
           .
       2700-EXIT.
           EXIT.
      *================================================================*
       5000-INSERT-HISTORY.
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
               MOVE '5000-INSERT-HISTORY'  TO ABN-PARAGRAPH
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
               MOVE '5000-INSERT-HISTORY'  TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
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
           COMPUTE LNX-SETTLEMENT-AMT = LN-PRINCIPAL-BAL
                   + LN-ACCRUED-INT + LN-FEES-OUTSTANDING
           MOVE LN-NOMINAL-RATE            TO LNX-NOMINAL-RATE
           MOVE LN-APR                     TO LNX-APR
           MOVE LN-TERM-MONTHS             TO LNX-TERM-MONTHS
           MOVE LN-PAYMENTS-REMAINING      TO LNX-PAYMENTS-REMAINING
           MOVE LN-MISSED-COUNT            TO LNX-MISSED-COUNT
           MOVE LN-NEXT-DUE-DATE           TO LNX-NEXT-DUE-DATE
           MOVE LN-DEFAULT-REMEDY-DATE     TO LNX-REMEDY-DATE
           MOVE LN-ARREARS-SINCE           TO LNX-PERIOD-FROM
           MOVE WS-BUS-DATE                TO LNX-PERIOD-TO
           MOVE ZERO                       TO LNX-PERIOD-PAID
                                              LNX-PERIOD-INT
                                              LNX-PERIOD-FEES
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
       5600-RECORD-NOTICE.
      *================================================================*
           MOVE LN-LOAN-NO                 TO LNL-LOAN-NO
           MOVE LNX-LETTER-CD              TO LNL-LETTER-CD
           MOVE WS-DATE-ISO                TO LNL-ISSUE-DATE
           MOVE LN-CIF-NO                  TO LNL-CIF-NO
           MOVE LN-ARREARS-AMT             TO LNL-ARREARS-AMT
           COMPUTE LNL-BALANCE-AMT = LN-PRINCIPAL-BAL
                   + LN-ACCRUED-INT + LN-FEES-OUTSTANDING
           MOVE LN-MISSED-COUNT            TO LNL-MISSED-COUNT
           MOVE LNX-TEMPLATE-ID            TO LNL-TEMPLATE-ID
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
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
      *            RERUN ON THE SAME DAY - NOTICE ALREADY RECORDED.
                   CONTINUE
               WHEN OTHER
                   MOVE '5600-RECORD-NOTICE' TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       3000-WRITE-JOURNAL.
      *================================================================*
           ACCEPT WS-TIME-NOW              FROM TIME
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-LOANS               TO TRUE
           STRING 'LNARR' WS-BUS-DATE
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE 'LNARR'                    TO GLJH-RUN-ID
           MOVE WS-BUS-DATE                TO GLJH-CREATE-DATE
           MOVE WS-TIME-NOW (1:6)          TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3900-WRITE-GL

      *---- LATE PAYMENT FEES -----------------------------------------*
           PERFORM VARYING FG-IDX FROM 1 BY 1
                   UNTIL FG-IDX > WS-FG-COUNT
               MOVE WS-GL-FEE-REC          TO GLJD-GL-ACCOUNT
               MOVE 'D'                    TO GLJD-DR-CR-IND
               MOVE WS-FG-AMOUNT (FG-IDX)  TO GLJD-AMOUNT
               MOVE WS-FG-COST-CENTRE (FG-IDX) TO GLJD-COST-CENTRE
               MOVE 'LOAN LATE PAYMENT FEES RAISED'
                                           TO GLJD-DESCRIPTION
               MOVE 'FEE '                 TO GLJD-TXN-CODE
               PERFORM 3100-DETAIL
               MOVE WS-FG-GL-ACCOUNT (FG-IDX) TO GLJD-GL-ACCOUNT
               MOVE 'C'                    TO GLJD-DR-CR-IND
               MOVE 'LOAN LATE PAYMENT FEE INCOME'
                                           TO GLJD-DESCRIPTION
               PERFORM 3100-DETAIL
           END-PERFORM

      *---- WRITE-OFFS ------------------------------------------------*
           IF WS-WRITE-OFFS > ZERO
               MOVE SPACES                 TO GLJD-COST-CENTRE
               MOVE 'WOFF'                 TO GLJD-TXN-CODE
               MOVE WS-GL-IMPAIRMENT       TO GLJD-GL-ACCOUNT
               MOVE 'D'                    TO GLJD-DR-CR-IND
               COMPUTE GLJD-AMOUNT = WS-WOFF-PRIN + WS-WOFF-INT
                                   + WS-WOFF-FEES
               MOVE 'LOAN WRITE-OFFS'      TO GLJD-DESCRIPTION
               PERFORM 3100-DETAIL
               IF WS-WOFF-PRIN > ZERO
                   MOVE WS-GL-PRINCIPAL    TO GLJD-GL-ACCOUNT
                   MOVE 'C'                TO GLJD-DR-CR-IND
                   MOVE WS-WOFF-PRIN       TO GLJD-AMOUNT
                   MOVE 'WRITE-OFF PRINCIPAL' TO GLJD-DESCRIPTION
                   PERFORM 3100-DETAIL
               END-IF
               IF WS-WOFF-INT > ZERO
                   MOVE WS-GL-INT-REC      TO GLJD-GL-ACCOUNT
                   MOVE 'C'                TO GLJD-DR-CR-IND
                   MOVE WS-WOFF-INT        TO GLJD-AMOUNT
                   MOVE 'WRITE-OFF INTEREST' TO GLJD-DESCRIPTION
                   PERFORM 3100-DETAIL
               END-IF
               IF WS-WOFF-FEES > ZERO
                   MOVE WS-GL-FEE-REC      TO GLJD-GL-ACCOUNT
                   MOVE 'C'                TO GLJD-DR-CR-IND
                   MOVE WS-WOFF-FEES       TO GLJD-AMOUNT
                   MOVE 'WRITE-OFF FEES'   TO GLJD-DESCRIPTION
                   PERFORM 3100-DETAIL
               END-IF
           END-IF

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
           MOVE SPACES                     TO GLJD-PROD-CD
                                              GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE 'GBP'                      TO GLJD-CCY-CD
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
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
           MOVE '0'                        TO AL-CC
           MOVE 'ARREARS AGEING'           TO AL-BUCKET
           MOVE ZERO                       TO AL-COUNT AL-ARREARS
                                              AL-BALANCE
           WRITE REPORT-RECORD             FROM WS-AGEING-LINE
           MOVE ' '                        TO AL-CC
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 6
               MOVE WS-BUCKET-NAME (WS-BKT) TO AL-BUCKET
               MOVE WS-AGE-COUNT (WS-BKT)  TO AL-COUNT
               MOVE WS-AGE-ARREARS (WS-BKT) TO AL-ARREARS
               MOVE WS-AGE-BALANCE (WS-BKT) TO AL-BALANCE
               WRITE REPORT-RECORD         FROM WS-AGEING-LINE
           END-PERFORM

           MOVE 'OPEN LOANS READ'          TO TL-LABEL
           MOVE WS-LOANS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'IN ARREARS'               TO TL-LABEL
           MOVE WS-IN-ARREARS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LATE PAYMENT FEES'        TO TL-LABEL
           MOVE WS-FEES-CHARGED            TO TL-COUNT
           MOVE WS-FEE-TOTAL               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REMINDERS (MISS)'         TO TL-LABEL
           MOVE WS-MISS-LETTERS            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NOTICES OF SUMS IN ARREARS' TO TL-LABEL
           MOVE WS-NSIA-LETTERS            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'DEFAULT NOTICES'          TO TL-LABEL
           MOVE WS-DEFAULT-NOTICES         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TERMINATED / PLACED WITH AGENCY' TO TL-LABEL
           MOVE WS-TERMINATIONS            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'WRITTEN OFF'              TO TL-LABEL
           MOVE WS-WRITE-OFFS              TO TL-COUNT
           COMPUTE TL-AMOUNT = WS-WOFF-PRIN + WS-WOFF-INT
                             + WS-WOFF-FEES
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'FORBEARANCE ARRANGEMENTS ENDED' TO TL-LABEL
           MOVE WS-FORBEARANCE-ENDED       TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE LOANMAST-FILE PRDPARM-FILE LETTERS-FILE COLLECT-FILE
                 GLJNL-FILE REPORT-FILE
           DISPLAY 'LNARR010 - IN ARREARS ' WS-IN-ARREARS
           DISPLAY 'LNARR010 - DEFAULTS   ' WS-DEFAULT-NOTICES
           DISPLAY 'LNARR010 - TERMINATED ' WS-TERMINATIONS
           IF WS-GL-DR NOT = WS-GL-CR
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
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE LN-LOAN-NO                 TO DL-LOAN-NO
           MOVE LN-CIF-NO                  TO DL-CIF-NO
           MOVE LN-STATUS                  TO DL-STATUS
           MOVE LN-MISSED-COUNT            TO DL-MISSED
           MOVE WS-DAYS-IN-ARREARS         TO DL-DAYS
           MOVE LN-ARREARS-BUCKET          TO DL-BUCKET
           MOVE LN-ARREARS-AMT             TO DL-ARREARS
           MOVE LN-PRINCIPAL-BAL           TO DL-PRINCIPAL
           MOVE WS-ACTION                  TO DL-ACTION
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
