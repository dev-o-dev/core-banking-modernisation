       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GLREC010.
       AUTHOR.        K L MORGAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2002-06-03.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : GLREC010                                            *
      * TITLE    : GENERAL LEDGER - FEED VALIDATION AND SUB-LEDGER     *
      *            RECONCILIATION                                      *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : GLREC010                                            *
      * JCL      : GLFEED01 STEP GL030                                 *
      *----------------------------------------------------------------*
      * INPUT    : GLFEED   - CONCATENATED GL JOURNALS FOR THE DAY     *
      *                       (ACCR + ICAP + CHRG + CREF + CLOS + MOVT)*
      *            GLBALIN  - GL CLOSING BALANCES FROM ORACLE FOR THE  *
      *                       PREVIOUS BUSINESS DATE (RETURN FILE,     *
      *                       ARRIVES 06:00 VIA CONNECT:DIRECT)        *
      *            ACCTMAST - DDA ACCOUNT MASTER (SEQUENTIAL)          *
      *            PRDPARM  - PRODUCT PARAMETERS (RANDOM)              *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : MBK.GLCTLBAL (INSERT / UPDATE)                      *
      * OUTPUT   : FEEDOUT  - VALIDATED FEED (COPY OF GLFEED) FOR C:D  *
      *            REPORT   - FEED CONTROL + RECONCILIATION (FBA 133)  *
      *----------------------------------------------------------------*
      * PART 1 - FEED VALIDATION                                       *
      *   EVERY JOURNAL IN THE FEED MUST HAVE A HEADER, DETAILS AND A  *
      *   TRAILER; THE TRAILER COUNTS AND TOTALS MUST MATCH THE        *
      *   DETAILS; DEBITS MUST EQUAL CREDITS. IF ANY JOURNAL FAILS,    *
      *   FEEDOUT IS LEFT EMPTY AND THE STEP ENDS RC 8 - THE C:D STEP  *
      *   IS SKIPPED AND FINANCE SYSTEMS RECEIVE NOTHING. A PARTIAL    *
      *   FEED IS NEVER SENT (FINANCE REQUIREMENT FIN-GL-004).         *
      *                                                                *
      * PART 2 - SUB-LEDGER RECONCILIATION                             *
      *   THE ACCOUNT MASTER LEDGER BALANCES ARE TOTALLED BY PRODUCT   *
      *   AND COMPARED WITH THE GL BALANCE OF THE PRODUCT'S BALANCE GL *
      *   ACCOUNT. THE GL FILE IS ONE DAY BEHIND, SO TODAY'S MOVEMENTS *
      *   (THE MOVT JOURNAL JUST VALIDATED) ARE ADDED TO THE GL SIDE   *
      *   BEFORE COMPARING. DIFFERENCES ABOVE WS-TOLERANCE ARE BREAKS. *
      *   ONE ROW PER PRODUCT IS WRITTEN TO MBK.GLCTLBAL.              *
      *   OVERDRAWN ACCOUNTS ARE INCLUDED (NET BALANCE) BECAUSE THE GL *
      *   ALSO HOLDS THE NET UNTIL THE MONTHLY RECLASSIFICATION.       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2002-06-03 KLM  ORIGINAL VERSION                               *
      * 2004-01-12 KLM  INTERNAL CLEARING CHECK                        *
      * 2007-07-30 KLM  RECONCILIATION RESULTS TO DB2 (GLCTLBAL) FOR   *
      *                 THE FINANCE MI DASHBOARD                       *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GLFEED-FILE      ASSIGN TO GLFEED
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLFEED-STATUS.
           SELECT FEEDOUT-FILE     ASSIGN TO FEEDOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-FEEDOUT-STATUS.
           SELECT GLBALIN-FILE     ASSIGN TO GLBALIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLBALIN-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  GLFEED-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLFEED-RECORD                   PIC X(250).
       FD  FEEDOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FEEDOUT-RECORD                  PIC X(250).
       FD  GLBALIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLBALIN-RECORD.
           05  GBI-GL-ACCOUNT              PIC X(10).
           05  GBI-PROD-CD                 PIC X(10).
           05  GBI-CCY-CD                  PIC X(03).
           05  GBI-BALANCE-DATE            PIC 9(08).
           05  GBI-BALANCE                 PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
           05  FILLER                      PIC X(51).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'GLREC010'.
      *
       01  WS-CONSTANTS.
           05  WS-TOLERANCE                PIC S9(05)V99 COMP-3
                                           VALUE +0.00.
           05  WS-MAX-FEED-RECS            PIC S9(09) COMP
                                           VALUE +500000.
      *
       01  WS-FILE-STATUSES.
           05  WS-GLFEED-STATUS            PIC X(02).
               88  WS-GLFEED-OK                VALUE '00'.
               88  WS-GLFEED-EOF               VALUE '10'.
           05  WS-FEEDOUT-STATUS           PIC X(02).
           05  WS-GLBALIN-STATUS           PIC X(02).
               88  WS-GLBALIN-OK               VALUE '00'.
               88  WS-GLBALIN-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-FEED-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-FEED              VALUE 'Y'.
           05  WS-IN-JOURNAL-IND           PIC X(01) VALUE 'N'.
               88  WS-IN-JOURNAL               VALUE 'Y'.
           05  WS-FEED-VALID-IND           PIC X(01) VALUE 'Y'.
               88  WS-FEED-VALID               VALUE 'Y'.
           05  WS-ACCT-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-GLB-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-END-OF-GL-BALANCES       VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-DB2             PIC X(10).
           05  WS-PREV-BUS-DATE            PIC 9(08).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-ERROR-TEXT               PIC X(60).
      *
      *---- CURRENT JOURNAL -------------------------------------------*
       01  WS-JOURNAL-FIELDS.
           05  WS-JNL-ID                   PIC X(16).
           05  WS-JNL-SOURCE               PIC X(04).
           05  WS-JNL-LINES                PIC S9(09) COMP-3.
           05  WS-JNL-DR                   PIC S9(15)V99 COMP-3.
           05  WS-JNL-CR                   PIC S9(15)V99 COMP-3.
      *
      *---- FEED BUFFER - THE FEED IS HELD UNTIL VALIDATED ------------*
      *    THE FEED IS WRITTEN TO FEEDOUT ONLY AFTER ALL JOURNALS PASS.
      *    IT IS RE-READ FROM GLFEED FOR THE COPY (TWO PASSES) RATHER
      *    THAN HELD IN STORAGE.
       01  WS-FEED-COUNTS.
           05  WS-FEED-RECS                PIC S9(09) COMP VALUE ZERO.
           05  WS-JOURNAL-COUNT            PIC S9(07) COMP-3 VALUE 0.
           05  WS-BAD-JOURNAL-COUNT        PIC S9(07) COMP-3 VALUE 0.
           05  WS-FEED-TOTAL-DR            PIC S9(17)V99 COMP-3
                                           VALUE 0.
           05  WS-FEED-TOTAL-CR            PIC S9(17)V99 COMP-3
                                           VALUE 0.
           05  WS-COPIED-RECS              PIC S9(09) COMP VALUE ZERO.
      *
      *---- RECONCILIATION TABLE BY BALANCE GL + PRODUCT -------------*
       01  WS-RECON-TABLE.
           05  WS-RC-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-RC-ENTRY                 OCCURS 400 TIMES
                                           INDEXED BY RC-IDX.
               10  WS-RC-GL-ACCOUNT        PIC X(10).
               10  WS-RC-PROD-CD           PIC X(10).
               10  WS-RC-CCY-CD            PIC X(03).
               10  WS-RC-SUB-LEDGER        PIC S9(15)V99 COMP-3.
               10  WS-RC-ACCT-COUNT        PIC S9(09) COMP-3.
               10  WS-RC-GL-OPENING        PIC S9(15)V99 COMP-3.
               10  WS-RC-GL-MOVEMENT       PIC S9(15)V99 COMP-3.
               10  WS-RC-GL-HAVE-IND       PIC X(01).
      *
      *---- PRODUCT -> BALANCE GL CACHE -------------------------------*
       01  WS-PROD-CACHE.
           05  WS-PC-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-PC-ENTRY                 OCCURS 300 TIMES
                                           INDEXED BY PC-IDX.
               10  WS-PC-PROD-CD           PIC X(10).
               10  WS-PC-GL-ACCOUNT        PIC X(10).
      *
       01  WS-RECON-FIELDS.
           05  WS-GL-EXPECTED              PIC S9(15)V99 COMP-3.
           05  WS-DIFFERENCE               PIC S9(15)V99 COMP-3.
           05  WS-ABS-DIFFERENCE           PIC S9(15)V99 COMP-3.
           05  WS-BREAK-COUNT              PIC S9(07) COMP-3 VALUE 0.
           05  WS-NO-GL-COUNT              PIC S9(07) COMP-3 VALUE 0.
           05  WS-ACCT-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-SUB-LEDGER         PIC S9(17)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-GL                 PIC S9(17)V99 COMP-3
                                           VALUE 0.
           05  WS-THIS-GL                  PIC X(10).
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
           05  WS-REPORT-PART              PIC X(30).
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'GLREC010'.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  HD1-PART                    PIC X(30).
           05  FILLER                      PIC X(45)
               VALUE 'MEGABANK PLC - GENERAL LEDGER CONTROL'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-FEED-HEADING.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(18) VALUE 'JOURNAL ID'.
           05  FILLER            PIC X(06) VALUE 'SRCE'.
           05  FILLER            PIC X(11) VALUE '     LINES'.
           05  FILLER            PIC X(22) VALUE '            DEBITS'.
           05  FILLER            PIC X(22) VALUE '           CREDITS'.
           05  FILLER            PIC X(53) VALUE '  RESULT'.
       01  WS-RECON-HEADING.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(12) VALUE 'BALANCE GL'.
           05  FILLER            PIC X(12) VALUE 'PRODUCT'.
           05  FILLER            PIC X(10) VALUE '  ACCOUNTS'.
           05  FILLER            PIC X(21) VALUE '        SUB-LEDGER'.
           05  FILLER            PIC X(21) VALUE '     GL (ADJUSTED)'.
           05  FILLER            PIC X(21) VALUE '        DIFFERENCE'.
           05  FILLER            PIC X(35) VALUE '  STATUS'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-FEED-LINE.
           05  FL-CC                       PIC X(01) VALUE ' '.
           05  FL-JNL-ID                   PIC X(16).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  FL-SOURCE                   PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  FL-LINES                    PIC ZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FL-DR                       PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  FL-CR                       PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FL-RESULT                   PIC X(51).
       01  WS-RECON-LINE.
           05  RL-CC                       PIC X(01) VALUE ' '.
           05  RL-GL-ACCOUNT               PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RL-ACCOUNTS                 PIC Z,ZZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  RL-SUB-LEDGER               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  RL-GL                       PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  RL-DIFFERENCE               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  RL-STATUS                   PIC X(30).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLGLCB END-EXEC.
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
           PERFORM 2000-VALIDATE-FEED
           IF WS-FEED-VALID
               PERFORM 2500-COPY-FEED
           END-IF
           PERFORM 3000-SUM-SUB-LEDGER
           PERFORM 3500-LOAD-GL-BALANCES
           PERFORM 4000-RECONCILE
           PERFORM 5000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  GLFEED-FILE
                       GLBALIN-FILE
                       ACCTMAST-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                OUTPUT FEEDOUT-FILE
                       REPORT-FILE
           IF NOT WS-GLFEED-OK
           OR NOT WS-GLBALIN-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-FEEDOUT-STATUS NOT = '00'
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
           MOVE DTC-PREV-BUS-DATE          TO WS-PREV-BUS-DATE
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-BUS-DATE-DB2
           END-STRING
           MOVE WS-BUS-DATE-DB2            TO HD1-BUS-DATE
           .
      *================================================================*
       2000-VALIDATE-FEED.
      *================================================================*
           MOVE 'PART 1 - FEED VALIDATION'  TO HD1-PART
           PERFORM 7000-FEED-HEADINGS
           PERFORM 8000-READ-FEED
           PERFORM UNTIL WS-END-OF-FEED
               EVALUATE TRUE
                   WHEN GLJ-HEADER
                       PERFORM 2100-JOURNAL-HEADER
                   WHEN GLJ-DETAIL
                       PERFORM 2200-JOURNAL-DETAIL
                   WHEN GLJ-TRAILER
                       PERFORM 2300-JOURNAL-TRAILER THRU 2300-EXIT
                   WHEN OTHER
                       MOVE 'INVALID RECORD TYPE IN FEED'
                                           TO WS-ERROR-TEXT
                       PERFORM 2900-JOURNAL-ERROR
               END-EVALUATE
               PERFORM 8000-READ-FEED
           END-PERFORM

           IF WS-IN-JOURNAL
               MOVE 'FEED ENDS INSIDE A JOURNAL (NO TRAILER)'
                                           TO WS-ERROR-TEXT
               PERFORM 2900-JOURNAL-ERROR
           END-IF
           IF WS-JOURNAL-COUNT = ZERO
               MOVE 'N'                    TO WS-FEED-VALID-IND
               DISPLAY 'GLREC010 E001 GL FEED IS EMPTY'
           END-IF
           IF WS-FEED-TOTAL-DR NOT = WS-FEED-TOTAL-CR
               MOVE 'N'                    TO WS-FEED-VALID-IND
               DISPLAY 'GLREC010 E002 FEED DEBITS NOT = CREDITS'
           END-IF
           .
      *================================================================*
       2100-JOURNAL-HEADER.
      *================================================================*
           IF WS-IN-JOURNAL
               MOVE 'HEADER WITHOUT TRAILER FOR PREVIOUS JOURNAL'
                                           TO WS-ERROR-TEXT
               PERFORM 2900-JOURNAL-ERROR
           END-IF
           SET WS-IN-JOURNAL               TO TRUE
           MOVE GLJ-JOURNAL-ID             TO WS-JNL-ID
           MOVE GLJ-SOURCE-SYSTEM          TO WS-JNL-SOURCE
           MOVE ZERO                       TO WS-JNL-LINES
                                              WS-JNL-DR
                                              WS-JNL-CR
           IF GLJ-BUSINESS-DATE NOT = WS-BUS-DATE
               MOVE 'JOURNAL BUSINESS DATE IS NOT TODAY'
                                           TO WS-ERROR-TEXT
               PERFORM 2900-JOURNAL-ERROR
           END-IF
           .
      *================================================================*
       2200-JOURNAL-DETAIL.
      *================================================================*
           IF NOT WS-IN-JOURNAL
               MOVE 'DETAIL OUTSIDE A JOURNAL'
                                           TO WS-ERROR-TEXT
               PERFORM 2900-JOURNAL-ERROR
           END-IF
           IF GLJ-JOURNAL-ID NOT = WS-JNL-ID
               MOVE 'DETAIL JOURNAL ID DIFFERS FROM HEADER'
                                           TO WS-ERROR-TEXT
               PERFORM 2900-JOURNAL-ERROR
           END-IF
           IF GLJD-AMOUNT NOT NUMERIC
           OR GLJD-GL-ACCOUNT = SPACES
               MOVE 'DETAIL HAS NO GL ACCOUNT OR BAD AMOUNT'
                                           TO WS-ERROR-TEXT
               PERFORM 2900-JOURNAL-ERROR
           ELSE
               ADD 1                       TO WS-JNL-LINES
               IF GLJD-DEBIT
                   ADD GLJD-AMOUNT         TO WS-JNL-DR
               ELSE
                   ADD GLJD-AMOUNT         TO WS-JNL-CR
               END-IF
      *        TODAY'S CUSTOMER MOVEMENTS ADJUST THE GL SIDE OF THE
      *        RECONCILIATION (THE GL FILE IS AS AT YESTERDAY).
               IF GLJ-SRC-MOVEMENTS OR GLJ-SRC-CAPITALISE
               OR GLJ-SRC-CHARGES
                   PERFORM 2250-NOTE-MOVEMENT
               END-IF
           END-IF
           .
      *================================================================*
       2250-NOTE-MOVEMENT.
      *================================================================*
      *    ONLY LINES ON A PRODUCT BALANCE GL ARE RELEVANT. THE SIGN
      *    CONVENTION FOR A LIABILITY: CREDIT INCREASES THE BALANCE.
           PERFORM 2260-FIND-RECON-ENTRY
           IF GLJD-CREDIT
               ADD GLJD-AMOUNT         TO WS-RC-GL-MOVEMENT (RC-IDX)
           ELSE
               SUBTRACT GLJD-AMOUNT    FROM WS-RC-GL-MOVEMENT (RC-IDX)
           END-IF
           .
      *================================================================*
       2260-FIND-RECON-ENTRY.
      *================================================================*
           SET RC-IDX                      TO 1
           SEARCH WS-RC-ENTRY
               AT END
                   PERFORM 2270-NEW-RECON-ENTRY
               WHEN RC-IDX > WS-RC-COUNT
                   PERFORM 2270-NEW-RECON-ENTRY
               WHEN WS-RC-GL-ACCOUNT (RC-IDX) = GLJD-GL-ACCOUNT
               AND  WS-RC-PROD-CD (RC-IDX) = GLJD-PROD-CD
                   CONTINUE
           END-SEARCH
           .
      *================================================================*
       2270-NEW-RECON-ENTRY.
      *================================================================*
           IF WS-RC-COUNT >= 400
               MOVE '2270-NEW-RECON-ENTRY' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'RECONCILIATION TABLE FULL'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-RC-COUNT
           SET RC-IDX                      TO WS-RC-COUNT
           MOVE GLJD-GL-ACCOUNT            TO WS-RC-GL-ACCOUNT (RC-IDX)
           MOVE GLJD-PROD-CD               TO WS-RC-PROD-CD (RC-IDX)
           MOVE GLJD-CCY-CD                TO WS-RC-CCY-CD (RC-IDX)
           MOVE ZERO                       TO WS-RC-SUB-LEDGER (RC-IDX)
                                              WS-RC-ACCT-COUNT (RC-IDX)
                                              WS-RC-GL-OPENING (RC-IDX)
                                              WS-RC-GL-MOVEMENT (RC-IDX)
           MOVE 'N'                        TO WS-RC-GL-HAVE-IND (RC-IDX)
           .
      *================================================================*
       2300-JOURNAL-TRAILER.
      *================================================================*
           IF NOT WS-IN-JOURNAL
               MOVE 'TRAILER WITHOUT HEADER'
                                           TO WS-ERROR-TEXT
               PERFORM 2900-JOURNAL-ERROR
               GO TO 2300-EXIT
           END-IF
           MOVE WS-JNL-ID                  TO FL-JNL-ID
           MOVE WS-JNL-SOURCE              TO FL-SOURCE
           MOVE WS-JNL-LINES               TO FL-LINES
           MOVE WS-JNL-DR                  TO FL-DR
           MOVE WS-JNL-CR                  TO FL-CR
           EVALUATE TRUE
               WHEN GLJT-LINE-COUNT NOT = WS-JNL-LINES
                   MOVE 'FAILED - TRAILER LINE COUNT MISMATCH'
                                           TO FL-RESULT
                   MOVE 'N'                TO WS-FEED-VALID-IND
                   ADD 1                   TO WS-BAD-JOURNAL-COUNT
               WHEN GLJT-TOTAL-DR NOT = WS-JNL-DR
               WHEN GLJT-TOTAL-CR NOT = WS-JNL-CR
                   MOVE 'FAILED - TRAILER TOTALS MISMATCH'
                                           TO FL-RESULT
                   MOVE 'N'                TO WS-FEED-VALID-IND
                   ADD 1                   TO WS-BAD-JOURNAL-COUNT
               WHEN WS-JNL-DR NOT = WS-JNL-CR
                   MOVE 'FAILED - JOURNAL DOES NOT BALANCE'
                                           TO FL-RESULT
                   MOVE 'N'                TO WS-FEED-VALID-IND
                   ADD 1                   TO WS-BAD-JOURNAL-COUNT
               WHEN OTHER
                   MOVE 'OK'               TO FL-RESULT
           END-EVALUATE
           PERFORM 7100-FEED-DETAIL
           ADD 1                           TO WS-JOURNAL-COUNT
           ADD WS-JNL-DR                   TO WS-FEED-TOTAL-DR
           ADD WS-JNL-CR                   TO WS-FEED-TOTAL-CR
           MOVE 'N'                        TO WS-IN-JOURNAL-IND
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2900-JOURNAL-ERROR.
      *================================================================*
           MOVE 'N'                        TO WS-FEED-VALID-IND
           ADD 1                           TO WS-BAD-JOURNAL-COUNT
           MOVE GLJ-JOURNAL-ID             TO FL-JNL-ID
           MOVE GLJ-SOURCE-SYSTEM          TO FL-SOURCE
           MOVE ZERO                       TO FL-LINES FL-DR FL-CR
           MOVE WS-ERROR-TEXT              TO FL-RESULT
           PERFORM 7100-FEED-DETAIL
           .
      *================================================================*
       2500-COPY-FEED.
      *================================================================*
      *    SECOND PASS - COPY THE VALIDATED FEED FOR CONNECT:DIRECT.
           CLOSE GLFEED-FILE
           OPEN INPUT GLFEED-FILE
           MOVE 'N'                        TO WS-FEED-EOF-IND
           READ GLFEED-FILE INTO GLJ-JOURNAL-LINE
           PERFORM UNTIL NOT WS-GLFEED-OK
               WRITE FEEDOUT-RECORD        FROM GLJ-JOURNAL-LINE
               IF WS-FEEDOUT-STATUS NOT = '00'
                   MOVE '2500-COPY-FEED'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'FEEDOUT WRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-FEEDOUT-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               END-IF
               ADD 1                       TO WS-COPIED-RECS
               READ GLFEED-FILE INTO GLJ-JOURNAL-LINE
           END-PERFORM
           IF WS-COPIED-RECS NOT = WS-FEED-RECS
               MOVE '2500-COPY-FEED'       TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'FEED COPY COUNT DIFFERS FROM VALIDATION COUNT'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-SUM-SUB-LEDGER.
      *================================================================*
           PERFORM 8100-READ-ACCOUNT
           PERFORM UNTIL WS-END-OF-ACCOUNTS
               IF NOT ACM-CLOSED
                   PERFORM 3100-GET-BALANCE-GL
                   MOVE WS-THIS-GL         TO GLJD-GL-ACCOUNT
                   MOVE ACM-PROD-CD        TO GLJD-PROD-CD
                   MOVE ACM-CCY-CD         TO GLJD-CCY-CD
                   PERFORM 2260-FIND-RECON-ENTRY
                   ADD ACM-LEDGER-BAL  TO WS-RC-SUB-LEDGER (RC-IDX)
                   ADD 1               TO WS-RC-ACCT-COUNT (RC-IDX)
                   ADD ACM-LEDGER-BAL  TO WS-TOTAL-SUB-LEDGER
               END-IF
               PERFORM 8100-READ-ACCOUNT
           END-PERFORM
           .
      *================================================================*
       3100-GET-BALANCE-GL.
      *================================================================*
           SET PC-IDX                      TO 1
           SEARCH WS-PC-ENTRY
               AT END
                   PERFORM 3110-LOAD-PRODUCT
               WHEN PC-IDX > WS-PC-COUNT
                   PERFORM 3110-LOAD-PRODUCT
               WHEN WS-PC-PROD-CD (PC-IDX) = ACM-PROD-CD
                   CONTINUE
           END-SEARCH
           MOVE WS-PC-GL-ACCOUNT (PC-IDX)  TO WS-THIS-GL
           .
      *================================================================*
       3110-LOAD-PRODUCT.
      *================================================================*
           IF WS-PC-COUNT >= 300
               MOVE '3110-LOAD-PRODUCT'    TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'PRODUCT CACHE FULL'   TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-PC-COUNT
           SET PC-IDX                      TO WS-PC-COUNT
           MOVE ACM-PROD-CD                TO WS-PC-PROD-CD (PC-IDX)
                                              PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               MOVE PRD-GL-BALANCE-CR      TO WS-PC-GL-ACCOUNT (PC-IDX)
           ELSE
               MOVE '29900-0001'           TO WS-PC-GL-ACCOUNT (PC-IDX)
           END-IF
           .
      *================================================================*
       3500-LOAD-GL-BALANCES.
      *================================================================*
           PERFORM 8200-READ-GL-BALANCE
           PERFORM UNTIL WS-END-OF-GL-BALANCES
               IF GBI-BALANCE-DATE NOT = WS-PREV-BUS-DATE
                   DISPLAY 'GLREC010 W003 GL BALANCE DATED '
                           GBI-BALANCE-DATE ' EXPECTED '
                           WS-PREV-BUS-DATE ' FOR ' GBI-GL-ACCOUNT
               END-IF
               MOVE GBI-GL-ACCOUNT         TO GLJD-GL-ACCOUNT
               MOVE GBI-PROD-CD            TO GLJD-PROD-CD
               MOVE GBI-CCY-CD             TO GLJD-CCY-CD
               PERFORM 2260-FIND-RECON-ENTRY
               ADD GBI-BALANCE             TO WS-RC-GL-OPENING (RC-IDX)
               MOVE 'Y'                    TO WS-RC-GL-HAVE-IND (RC-IDX)
               PERFORM 8200-READ-GL-BALANCE
           END-PERFORM
           .
      *================================================================*
       4000-RECONCILE.
      *================================================================*
           MOVE 'PART 2 - SUB-LEDGER RECON'  TO HD1-PART
           PERFORM 7200-RECON-HEADINGS

           PERFORM VARYING RC-IDX FROM 1 BY 1
                   UNTIL RC-IDX > WS-RC-COUNT
      *        ONLY ENTRIES THAT ARE PRODUCT BALANCE ACCOUNTS (HAVE A
      *        SUB-LEDGER COUNT) ARE RECONCILED. CONTRA, INCOME AND
      *        EXPENSE LINES PICKED UP FROM THE FEED ARE IGNORED.
               IF WS-RC-ACCT-COUNT (RC-IDX) > ZERO
                   PERFORM 4100-RECONCILE-ONE
               END-IF
           END-PERFORM
           .
      *================================================================*
       4100-RECONCILE-ONE.
      *================================================================*
           COMPUTE WS-GL-EXPECTED = WS-RC-GL-OPENING (RC-IDX)
                                  + WS-RC-GL-MOVEMENT (RC-IDX)
           COMPUTE WS-DIFFERENCE = WS-RC-SUB-LEDGER (RC-IDX)
                                 - WS-GL-EXPECTED
           IF WS-DIFFERENCE < ZERO
               COMPUTE WS-ABS-DIFFERENCE = 0 - WS-DIFFERENCE
           ELSE
               MOVE WS-DIFFERENCE          TO WS-ABS-DIFFERENCE
           END-IF
           ADD WS-GL-EXPECTED              TO WS-TOTAL-GL

           MOVE WS-RC-GL-ACCOUNT (RC-IDX)  TO RL-GL-ACCOUNT
           MOVE WS-RC-PROD-CD (RC-IDX)     TO RL-PROD-CD
           MOVE WS-RC-ACCT-COUNT (RC-IDX)  TO RL-ACCOUNTS
           MOVE WS-RC-SUB-LEDGER (RC-IDX)  TO RL-SUB-LEDGER
           MOVE WS-GL-EXPECTED             TO RL-GL
           MOVE WS-DIFFERENCE              TO RL-DIFFERENCE

           EVALUATE TRUE
               WHEN WS-RC-GL-HAVE-IND (RC-IDX) NOT = 'Y'
                   MOVE 'NO GL BALANCE RECEIVED'   TO RL-STATUS
                   MOVE 'G'                        TO GCB-RECON-STATUS
                   ADD 1                           TO WS-NO-GL-COUNT
               WHEN WS-ABS-DIFFERENCE > WS-TOLERANCE
                   MOVE '*** BREAK ***'            TO RL-STATUS
                   MOVE 'B'                        TO GCB-RECON-STATUS
                   ADD 1                           TO WS-BREAK-COUNT
               WHEN OTHER
                   MOVE 'RECONCILED'               TO RL-STATUS
                   MOVE 'R'                        TO GCB-RECON-STATUS
           END-EVALUATE
           PERFORM 7300-RECON-DETAIL
           PERFORM 4200-STORE-RESULT
           .
      *================================================================*
       4200-STORE-RESULT.
      *================================================================*
           MOVE WS-BUS-DATE-DB2            TO GCB-BUSINESS-DATE
           MOVE WS-RC-GL-ACCOUNT (RC-IDX)  TO GCB-GL-ACCOUNT
           MOVE WS-RC-PROD-CD (RC-IDX)     TO GCB-PROD-CD
           MOVE WS-RC-CCY-CD (RC-IDX)      TO GCB-CCY-CD
           MOVE WS-GL-EXPECTED             TO GCB-GL-BALANCE
           MOVE WS-RC-SUB-LEDGER (RC-IDX)  TO GCB-SUB-LEDGER-BAL
           MOVE WS-DIFFERENCE              TO GCB-DIFFERENCE
           MOVE WS-RC-ACCT-COUNT (RC-IDX)  TO GCB-ACCOUNT-COUNT

      *    A RERUN REPLACES THE DAY'S ROW.
           EXEC SQL
               UPDATE MBK.GLCTLBAL
                  SET GL_BALANCE     = :GCB-GL-BALANCE,
                      SUB_LEDGER_BAL = :GCB-SUB-LEDGER-BAL,
                      DIFFERENCE     = :GCB-DIFFERENCE,
                      RECON_STATUS   = :GCB-RECON-STATUS,
                      ACCOUNT_COUNT  = :GCB-ACCOUNT-COUNT,
                      RECON_TS       = CURRENT TIMESTAMP
                WHERE BUSINESS_DATE = :GCB-BUSINESS-DATE
                  AND GL_ACCOUNT    = :GCB-GL-ACCOUNT
                  AND PROD_CD       = :GCB-PROD-CD
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   EXEC SQL
                       INSERT INTO MBK.GLCTLBAL
                             (BUSINESS_DATE, GL_ACCOUNT, PROD_CD,
                              CCY_CD, GL_BALANCE, SUB_LEDGER_BAL,
                              DIFFERENCE, RECON_STATUS, ACCOUNT_COUNT,
                              LOADED_TS, RECON_TS)
                       VALUES
                             (:GCB-BUSINESS-DATE, :GCB-GL-ACCOUNT,
                              :GCB-PROD-CD, :GCB-CCY-CD,
                              :GCB-GL-BALANCE, :GCB-SUB-LEDGER-BAL,
                              :GCB-DIFFERENCE, :GCB-RECON-STATUS,
                              :GCB-ACCOUNT-COUNT, CURRENT TIMESTAMP,
                              CURRENT TIMESTAMP)
                   END-EXEC
                   IF SQLCODE NOT = 0
                       MOVE '4200-STORE-RESULT' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
                   END-IF
               WHEN OTHER
                   MOVE '4200-STORE-RESULT' TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       5000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC

           MOVE 'JOURNALS IN FEED'         TO TL-LABEL
           MOVE WS-JOURNAL-COUNT           TO TL-COUNT
           MOVE WS-FEED-TOTAL-DR           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'JOURNALS FAILED'          TO TL-LABEL
           MOVE WS-BAD-JOURNAL-COUNT       TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'FEED RECORDS COPIED FOR C:D' TO TL-LABEL
           MOVE WS-COPIED-RECS             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS IN SUB-LEDGER'   TO TL-LABEL
           MOVE WS-ACCT-READ               TO TL-COUNT
           MOVE WS-TOTAL-SUB-LEDGER        TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'GL (ADJUSTED) TOTAL'      TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-GL                TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RECONCILIATION BREAKS'    TO TL-LABEL
           MOVE WS-BREAK-COUNT             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PRODUCTS WITH NO GL BALANCE' TO TL-LABEL
           MOVE WS-NO-GL-COUNT             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE GLFEED-FILE
                 FEEDOUT-FILE
                 GLBALIN-FILE
                 ACCTMAST-FILE
                 PRDPARM-FILE
                 REPORT-FILE

           EVALUATE TRUE
               WHEN NOT WS-FEED-VALID
                   DISPLAY 'GLREC010 E003 GL FEED REJECTED - NOT SENT'
                   MOVE 8                  TO RETURN-CODE
               WHEN WS-BREAK-COUNT > ZERO
               WHEN WS-NO-GL-COUNT > ZERO
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       7000-FEED-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           WRITE REPORT-RECORD             FROM WS-FEED-HEADING
           WRITE REPORT-RECORD             FROM WS-HEADING-3
           MOVE 4                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-FEED-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-FEED-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-FEED-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-RECON-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           WRITE REPORT-RECORD             FROM WS-RECON-HEADING
           WRITE REPORT-RECORD             FROM WS-HEADING-3
           MOVE 4                          TO WS-LINE-COUNT
           .
      *================================================================*
       7300-RECON-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7200-RECON-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-RECON-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-FEED.
      *================================================================*
           READ GLFEED-FILE INTO GLJ-JOURNAL-LINE
           EVALUATE TRUE
               WHEN WS-GLFEED-OK
                   ADD 1                   TO WS-FEED-RECS
                   IF WS-FEED-RECS > WS-MAX-FEED-RECS
                       MOVE '8000-READ-FEED' TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'GL FEED UNEXPECTEDLY LARGE - CHECK INPUT'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
               WHEN WS-GLFEED-EOF
                   SET WS-END-OF-FEED      TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-FEED'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'GLFEED READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-GLFEED-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8100-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-ACCT-READ
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8100-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8200-READ-GL-BALANCE.
      *================================================================*
           READ GLBALIN-FILE
           EVALUATE TRUE
               WHEN WS-GLBALIN-OK
                   CONTINUE
               WHEN WS-GLBALIN-EOF
                   SET WS-END-OF-GL-BALANCES TO TRUE
               WHEN OTHER
                   MOVE '8200-READ-GL-BALANCE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'GLBALIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-GLBALIN-STATUS  TO ABN-FILE-STATUS
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
