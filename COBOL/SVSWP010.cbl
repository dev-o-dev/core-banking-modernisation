       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SVSWP010.
       AUTHOR.        R K MISTRY.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2015-03-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : SVSWP010                                            *
      * TITLE    : SAVINGS SWEEP - CURRENT ACCOUNT TO SAVINGS AND BACK *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : SVSWP01 STEP SW010 (DAILY, BEFORE PSEOD01)          *
      *----------------------------------------------------------------*
      * INPUT    : PENDPOST - TONIGHT'S BATCH DEBITS ALREADY QUEUED    *
      *                       FOR PSEOD01 (SO, DD, CHEQUES, CARDS,     *
      *                       LOAN REPAYMENTS), SORTED BY ACCOUNT BY   *
      *                       STEP SW005 (PSTXNREC LAYOUT)             *
      *            PRDPARM  - PRODUCT PARAMETERS (MAX BALANCE,         *
      *                       NOTICE DAYS)                             *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : SWPINST  - SWEEP INSTRUCTIONS (VSAM KSDS,           *
      *                       SEQUENTIAL I-O) - SVSWPREC               *
      *            ACCTMAST - DDA ACCOUNT MASTER (RANDOM)              *
      * OUTPUT   : SWPPOST  - SWEEP TRANSFERS FOR TONIGHT'S PSEOD01    *
      *                       (PSTXNREC, FEED - DISP=MOD, EXTEND)      *
      *            REPORT   - SWEEP REPORT (FBA 133)                   *
      *----------------------------------------------------------------*
      * EVERY ACTIVE SWEEP INSTRUCTION IN DATE IS EVALUATED AGAINST    *
      * THE PROJECTED END OF DAY BALANCE OF THE CURRENT ACCOUNT:       *
      *                                                                *
      *   PROJECTED = ACM-AVAIL-BAL (CLEARED FUNDS, INCLUDING TODAY'S  *
      *               MEMO POSTINGS)                                   *
      *             - DEBITS FOR THE ACCOUNT ALREADY IN TONIGHT'S      *
      *               BATCH FEEDS (PENDPOST)                           *
      *                                                                *
      * PENDING CREDITS IN THE FEEDS ARE IGNORED ON PURPOSE - A        *
      * CREDIT CAN STILL BE RETURNED (UNPAID CHEQUE, RECALLED BACS)    *
      * AND WE MUST NOT SWEEP AWAY MONEY THE CUSTOMER DOES NOT HAVE.   *
      * THE ARRANGED OVERDRAFT LIMIT IS NOT FUNDS FOR SWEEP PURPOSES.  *
      *                                                                *
      *   SWEEP OUT  : PROJECTED > SWP-THRESHOLD. THE EXCESS, ROUNDED  *
      *                DOWN TO A MULTIPLE OF SWP-SWEEP-UNIT, IS MOVED  *
      *                TO SAVINGS IF IT IS AT LEAST SWP-MIN-SWEEP-AMT. *
      *                CAPPED SO THE SAVINGS ACCOUNT DOES NOT EXCEED   *
      *                PRD-MAX-BALANCE FOR ITS PRODUCT.                *
      *   SWEEP BACK : PROJECTED < SWP-FLOOR-BAL. ENOUGH IS RETURNED   *
      *                TO BRING THE CURRENT ACCOUNT BACK TO            *
      *                SWP-RETURN-TARGET (ROUNDED UP TO A MULTIPLE OF  *
      *                THE SWEEP UNIT), LIMITED TO THE AVAILABLE       *
      *                BALANCE OF THE SAVINGS ACCOUNT (PART SWEEP).    *
      *                                                                *
      * EACH SWEEP IS A 'TFR ' DEBIT AND A 'TFR ' CREDIT WRITTEN TO    *
      * THE SWPPOST FEED. THE AVAILABLE BALANCE OF THE ACCOUNT DEBITED *
      * IS REDUCED HERE (AS A MEMO WOULD) - SAME APPROACH AS INNTC010, *
      * SO PIR 2012/019 (PSRCN010 MEMO BREAK) APPLIES HERE TOO.        *
      *                                                                *
      * RERUN: AN INSTRUCTION ALREADY SWEPT ON THE BUSINESS DATE       *
      * (SWP-LAST-SWEEP-DATE) IS SKIPPED, AND THE EXTERNAL REFERENCE   *
      * CARRIES THE DATE SO PSEOD010 REJECTS ANY DUPLICATE.            *
      *                                                                *
      * FAILURES (ACCOUNT CLOSED, RESTRICTED, CIF MISMATCH, NO FUNDS   *
      * TO SWEEP BACK ...) ARE REPORTED AND COUNTED ON THE             *
      * INSTRUCTION. AFTER WS-MAX-FAILS CONSECUTIVE FAILURES THE       *
      * INSTRUCTION IS SUSPENDED AND SAVINGS OPERATIONS CONTACT THE    *
      * CUSTOMER (REPORT SECTION 'SUSPENDED'). RC 4 IF ANY FAILED.     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2015-03-02 RKM  ORIGINAL VERSION (SWEEP PROPOSITION CR 15/0022)*
      * 2015-06-29 RKM  PENDING BATCH DEBITS INCLUDED IN THE PROJECTED *
      *                 BALANCE (INC0061734 - SWEEP OUT FOLLOWED BY AN *
      *                 UNPAID DIRECT DEBIT THE SAME NIGHT)            *
      * 2015-11-16 RKM  AUTO SUSPEND AFTER 5 CONSECUTIVE FAILURES      *
      * 2016-04-11 HLT  SAVINGS PRODUCT MAXIMUM BALANCE CAP            *
      * 2017-02-06 HLT  EXPIRED INSTRUCTIONS SET TO CANCELLED          *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SWPINST-FILE     ASSIGN TO SWPINST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS SWP-KEY
                                   FILE STATUS IS WS-SWPINST-STATUS.
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
           SELECT PENDPOST-FILE    ASSIGN TO PENDPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PENDPOST-STATUS.
           SELECT SWPPOST-FILE     ASSIGN TO SWPPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-SWPPOST-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SWPINST-FILE.
           COPY SVSWPREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  PENDPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PENDPOST-RECORD                 PIC X(200).
       FD  SWPPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SWPPOST-RECORD                  PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'SVSWP010'.
      *
       01  WS-CONSTANTS.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00015000.
           05  WS-MAX-FAILS                PIC 9(03) VALUE 005.
      *
       01  WS-FILE-STATUSES.
           05  WS-SWPINST-STATUS           PIC X(02).
               88  WS-SWPINST-OK               VALUE '00'.
               88  WS-SWPINST-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-NOTFND           VALUE '23'.
           05  WS-PENDPOST-STATUS          PIC X(02).
               88  WS-PENDPOST-OK              VALUE '00'.
               88  WS-PENDPOST-EOF             VALUE '10'.
           05  WS-SWPPOST-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-SWP-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-END-OF-SWEEPS            VALUE 'Y'.
           05  WS-PND-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-END-OF-PENDING           VALUE 'Y'.
           05  WS-SWP-CHANGED-IND          PIC X(01) VALUE 'N'.
               88  WS-SWP-CHANGED              VALUE 'Y'.
               88  WS-SWP-UNCHANGED            VALUE 'N'.
           05  WS-PRINT-IND                PIC X(01) VALUE 'N'.
               88  WS-PRINT-LINE               VALUE 'Y'.
               88  WS-NO-PRINT                 VALUE 'N'.
      *
      *---- PENDING POSTING (PSTXNREC POSITIONS 1-49) -----------------*
      *    PSTXNREC IS ALSO USED FOR OUTPUT, SO THE INPUT IS MAPPED
      *    HERE RATHER THAN BY A SECOND COPY OF THE COPYBOOK.
       01  WS-PEND-RECORD.
           05  PND-KEY.
               10  PND-SORT-CODE           PIC X(06).
               10  PND-ACCT-NO             PIC X(08).
           05  PND-POST-DATE               PIC 9(08).
           05  PND-POST-TIME               PIC 9(08).
           05  PND-SEQ-NO                  PIC 9(06).
           05  PND-TXN-CODE                PIC X(04).
           05  PND-DR-CR-IND               PIC X(01).
               88  PND-DEBIT                   VALUE 'D'.
               88  PND-CREDIT                  VALUE 'C'.
           05  PND-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  FILLER                      PIC X(151).
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-SWEEP-KEY                PIC X(14).
           05  WS-PREV-PND-KEY             PIC X(14) VALUE LOW-VALUES.
           05  WS-FAIL-RSN                 PIC X(04).
           05  WS-NOTE                     PIC X(41).
           05  WS-DIRECTION                PIC X(01).
               88  WS-DIR-OUT                  VALUE 'O'.
               88  WS-DIR-BACK                 VALUE 'I'.
               88  WS-DIR-NONE                 VALUE ' '.
           05  WS-SWEEP-AMT                PIC S9(13)V99 COMP-3.
           05  WS-PROJECTED                PIC S9(13)V99 COMP-3.
           05  WS-PENDING-DR               PIC S9(13)V99 COMP-3.
           05  WS-PENDING-CR               PIC S9(13)V99 COMP-3.
           05  WS-EXCESS                   PIC S9(13)V99 COMP-3.
           05  WS-NEEDED                   PIC S9(13)V99 COMP-3.
           05  WS-TARGET                   PIC S9(13)V99 COMP-3.
           05  WS-HEADROOM                 PIC S9(13)V99 COMP-3.
           05  WS-UNITS                    PIC S9(13)    COMP-3.
           05  WS-SAV-MAX-BAL              PIC S9(13)V99 COMP-3.
           05  WS-SAV-NOTICE-DAYS          PIC 9(03).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-FROM-KEY.
               10  WS-FROM-SORT-CODE       PIC X(06).
               10  WS-FROM-ACCT-NO         PIC X(08).
           05  WS-TO-KEY.
               10  WS-TO-SORT-CODE         PIC X(06).
               10  WS-TO-ACCT-NO           PIC X(08).
           05  WS-EXT-REF.
               10  WS-XR-PREFIX            PIC X(03) VALUE 'SWP'.
               10  WS-XR-KEY               PIC X(14).
               10  WS-XR-DIR               PIC X(01).
               10  WS-XR-DATE              PIC X(06).
      *
      *    BOTH ACCOUNTS ARE HELD HERE WHILE THE INSTRUCTION IS
      *    PROCESSED - THE FD RECORD AREA IS SHARED.
       01  WS-CUR-ACCOUNT                  PIC X(400) VALUE SPACES.
       01  WS-SAV-ACCOUNT                  PIC X(400) VALUE SPACES.
      *
       01  WS-COUNTERS.
           05  WS-SWP-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-INACTIVE             PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-NOT-STARTED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-EXPIRED              PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-ALREADY              PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-NO-ACTION            PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-OUT                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-BACK                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-PART-BACK            PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-CAPPED               PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-FAILED               PIC S9(09) COMP-3 VALUE 0.
           05  WS-SWP-SUSPENDED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PND-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-PND-MATCHED              PIC S9(09) COMP-3 VALUE 0.
           05  WS-POSTINGS-WRITTEN         PIC S9(09) COMP-3 VALUE 0.
           05  WS-OUT-TOTAL                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-BACK-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-PND-DR-TOTAL             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-POST-DR-TOTAL            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-POST-CR-TOTAL            PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'SVSWP010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - NIGHTLY SAVINGS SWEEP'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(16) VALUE 'CURRENT ACCOUNT'.
           05  FILLER            PIC X(16) VALUE 'SAVINGS ACCOUNT'.
           05  FILLER            PIC X(03) VALUE 'D'.
           05  FILLER            PIC X(18) VALUE '     PROJECTED BAL'.
           05  FILLER            PIC X(16) VALUE '    PENDING DR'.
           05  FILLER            PIC X(16) VALUE '     SWEPT AMT'.
           05  FILLER            PIC X(06) VALUE 'RSN'.
           05  FILLER            PIC X(41) VALUE 'NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SAV-SC                   PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-SAV-ACCT                 PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DIR                      PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PROJECTED                PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  DL-PENDING                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RSN                      PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(41).
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
           PERFORM 2000-PROCESS-SWEEP THRU 2000-EXIT
               UNTIL WS-END-OF-SWEEPS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       PENDPOST-FILE
                       DATECARD-FILE
                I-O    SWPINST-FILE
                       ACCTMAST-FILE
                EXTEND SWPPOST-FILE
                OUTPUT REPORT-FILE
           IF NOT WS-SWPINST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-PENDPOST-OK
           OR WS-SWPPOST-STATUS NOT = '00'
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
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           MOVE WS-BUS-DATE (3:6)          TO WS-XR-DATE

           PERFORM 8100-READ-PENDING
           PERFORM 8000-READ-SWEEP
           .
      *================================================================*
       2000-PROCESS-SWEEP.
      *================================================================*
           MOVE SPACES                     TO WS-FAIL-RSN WS-NOTE
                                              WS-DIRECTION
                                              WS-CUR-ACCOUNT
                                              WS-SAV-ACCOUNT
           MOVE ZERO                       TO WS-SWEEP-AMT
                                              WS-PROJECTED
                                              WS-PENDING-DR
                                              WS-PENDING-CR
           SET WS-SWP-UNCHANGED            TO TRUE
           SET WS-NO-PRINT                 TO TRUE

      *    PENDING POSTINGS FOR THIS ACCOUNT (AND SKIP THOSE FOR
      *    ACCOUNTS WITH NO SWEEP INSTRUCTION)
           PERFORM 2050-ACCUM-PENDING
               UNTIL WS-END-OF-PENDING
                  OR PND-KEY > WS-SWEEP-KEY

           EVALUATE TRUE
               WHEN NOT SWP-ACTIVE
                   ADD 1                   TO WS-SWP-INACTIVE
                   PERFORM 8000-READ-SWEEP
                   GO TO 2000-EXIT
               WHEN SWP-START-DATE > WS-BUS-DATE
                   ADD 1                   TO WS-SWP-NOT-STARTED
                   PERFORM 8000-READ-SWEEP
                   GO TO 2000-EXIT
               WHEN SWP-END-DATE NOT = ZERO
               AND  SWP-END-DATE < WS-BUS-DATE
                   PERFORM 2080-EXPIRE
               WHEN SWP-LAST-SWEEP-DATE = WS-BUS-DATE
                   ADD 1                   TO WS-SWP-ALREADY
                   PERFORM 8000-READ-SWEEP
                   GO TO 2000-EXIT
               WHEN OTHER
                   PERFORM 2100-CHECK-ACCOUNTS THRU 2100-EXIT
                   IF WS-FAIL-RSN = SPACES
                       PERFORM 2200-CALCULATE
                   END-IF
                   EVALUATE TRUE
                       WHEN WS-FAIL-RSN NOT = SPACES
                           PERFORM 2600-FAIL
                       WHEN WS-SWEEP-AMT > ZERO
                           PERFORM 2300-SWEEP
                       WHEN OTHER
                           ADD 1           TO WS-SWP-NO-ACTION
                   END-EVALUATE
           END-EVALUATE

           IF WS-SWP-CHANGED
               PERFORM 2700-UPDATE-INSTRUCTION
           END-IF
           IF WS-PRINT-LINE
               PERFORM 7100-REPORT-DETAIL
           END-IF
           PERFORM 8000-READ-SWEEP
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2050-ACCUM-PENDING.
      *================================================================*
           IF PND-KEY = WS-SWEEP-KEY
               ADD 1                       TO WS-PND-MATCHED
               IF PND-DEBIT
                   ADD PND-AMOUNT          TO WS-PENDING-DR
                   ADD PND-AMOUNT          TO WS-PND-DR-TOTAL
               ELSE
                   ADD PND-AMOUNT          TO WS-PENDING-CR
               END-IF
           END-IF
           PERFORM 8100-READ-PENDING
           .
      *================================================================*
       2080-EXPIRE.
      *================================================================*
      *    END DATE PASSED - CANCEL THE INSTRUCTION SO IT DROPS OUT OF
      *    THE IBANK 'MY SWEEPS' LIST. NO SWEEP TONIGHT.
           SET SWP-CANCELLED               TO TRUE
           SET WS-SWP-CHANGED              TO TRUE
           SET WS-PRINT-LINE               TO TRUE
           MOVE 'EXPD'                     TO WS-FAIL-RSN
           MOVE 'END DATE PASSED - INSTRUCTION CANCELLED'
                                           TO WS-NOTE
           ADD 1                           TO WS-SWP-EXPIRED
           .
      *================================================================*
       2100-CHECK-ACCOUNTS.
      *================================================================*
      *---- SAVINGS ACCOUNT -------------------------------------------*
           MOVE SWP-SAV-SORT-CODE          TO ACM-SORT-CODE
           MOVE SWP-SAV-ACCT-NO            TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'SVNF'             TO WS-FAIL-RSN
                   MOVE 'SAVINGS ACCOUNT NOT FOUND' TO WS-NOTE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE '2100-CHECK-ACCOUNTS' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               WHEN NOT ACM-SAVINGS
                   MOVE 'SVTY'             TO WS-FAIL-RSN
                   MOVE 'NOMINATED ACCOUNT IS NOT INSTANT SAVINGS'
                                           TO WS-NOTE
               WHEN ACM-CLOSED OR ACM-PENDING-CLOSE
                   MOVE 'SVCL'             TO WS-FAIL-RSN
                   MOVE 'SAVINGS ACCOUNT CLOSED / CLOSING'
                                           TO WS-NOTE
               WHEN ACM-FROZEN OR ACM-DORMANT
                   MOVE 'SVFZ'             TO WS-FAIL-RSN
                   MOVE 'SAVINGS ACCOUNT FROZEN OR DORMANT'
                                           TO WS-NOTE
               WHEN ACM-CIF-NO NOT = SWP-CIF-NO
                   MOVE 'CIFM'             TO WS-FAIL-RSN
                   MOVE 'SAVINGS ACCOUNT HELD BY ANOTHER CUSTOMER'
                                           TO WS-NOTE
               WHEN OTHER
                   MOVE ACM-ACCOUNT-RECORD TO WS-SAV-ACCOUNT
           END-EVALUATE
           IF WS-FAIL-RSN NOT = SPACES
               GO TO 2100-EXIT
           END-IF

      *---- SAVINGS PRODUCT RULES -------------------------------------*
           MOVE ZERO                       TO WS-SAV-MAX-BAL
                                              WS-SAV-NOTICE-DAYS
           MOVE ACM-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN WS-PRDPARM-OK
                   IF PRD-MAX-BALANCE NUMERIC
                       MOVE PRD-MAX-BALANCE TO WS-SAV-MAX-BAL
                   END-IF
                   IF PRD-NOTICE-DAYS NUMERIC
                       MOVE PRD-NOTICE-DAYS TO WS-SAV-NOTICE-DAYS
                   END-IF
               WHEN WS-PRDPARM-NOTFND
                   DISPLAY 'SVSWP010 W001 NO PRDPARM FOR PRODUCT '
                           ACM-PROD-CD ' - NO MAXIMUM APPLIED'
               WHEN OTHER
                   MOVE '2100-CHECK-ACCOUNTS' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PRDPARM READ FAILED' TO ABN-MESSAGE
                   MOVE WS-PRDPARM-STATUS  TO ABN-FILE-STATUS
                   MOVE PRD-PROD-CD        TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           IF WS-SAV-NOTICE-DAYS > ZERO
               MOVE 'SVNT'                 TO WS-FAIL-RSN
               MOVE 'SAVINGS ACCOUNT IS A NOTICE ACCOUNT'
                                           TO WS-NOTE
               GO TO 2100-EXIT
           END-IF

      *---- CURRENT ACCOUNT -------------------------------------------*
           MOVE SWP-SORT-CODE              TO ACM-SORT-CODE
           MOVE SWP-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'CANF'             TO WS-FAIL-RSN
                   MOVE 'CURRENT ACCOUNT NOT FOUND' TO WS-NOTE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE '2100-CHECK-ACCOUNTS' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               WHEN NOT ACM-CURRENT
                   MOVE 'CATY'             TO WS-FAIL-RSN
                   MOVE 'SWEEP ACCOUNT IS NOT A CURRENT ACCOUNT'
                                           TO WS-NOTE
               WHEN ACM-CLOSED OR ACM-PENDING-CLOSE
                   MOVE 'CACL'             TO WS-FAIL-RSN
                   MOVE 'CURRENT ACCOUNT CLOSED / CLOSING'
                                           TO WS-NOTE
               WHEN ACM-FROZEN OR ACM-DORMANT
                   MOVE 'CAFZ'             TO WS-FAIL-RSN
                   MOVE 'CURRENT ACCOUNT FROZEN OR DORMANT'
                                           TO WS-NOTE
               WHEN ACM-CIF-NO NOT = SWP-CIF-NO
                   MOVE 'CIFM'             TO WS-FAIL-RSN
                   MOVE 'CURRENT ACCOUNT HELD BY ANOTHER CUSTOMER'
                                           TO WS-NOTE
               WHEN ACM-CCY-CD NOT = 'GBP'
                   MOVE 'CCYX'             TO WS-FAIL-RSN
                   MOVE 'CURRENT ACCOUNT IS NOT STERLING'
                                           TO WS-NOTE
               WHEN OTHER
                   MOVE ACM-ACCOUNT-RECORD TO WS-CUR-ACCOUNT
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-CALCULATE.
      *================================================================*
      *    ACM-ACCOUNT-RECORD HOLDS THE CURRENT ACCOUNT (READ LAST).
           COMPUTE WS-PROJECTED = ACM-AVAIL-BAL - WS-PENDING-DR
           EVALUATE TRUE
               WHEN SWP-SWEEPS-OUT
               AND  WS-PROJECTED > SWP-THRESHOLD
                   PERFORM 2210-CALC-OUT THRU 2210-EXIT
               WHEN SWP-SWEEPS-BACK
               AND  WS-PROJECTED < SWP-FLOOR-BAL
                   PERFORM 2220-CALC-BACK THRU 2220-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       2210-CALC-OUT.
      *================================================================*
           IF ACM-NO-DEBITS OR ACM-NO-POSTINGS
               MOVE 'CARS'                 TO WS-FAIL-RSN
               MOVE 'CURRENT ACCOUNT - DEBITS RESTRICTED'
                                           TO WS-NOTE
               GO TO 2210-EXIT
           END-IF
           MOVE WS-SAV-ACCOUNT             TO ACM-ACCOUNT-RECORD
           IF ACM-NO-CREDITS OR ACM-NO-POSTINGS
               MOVE 'SVRS'                 TO WS-FAIL-RSN
               MOVE 'SAVINGS ACCOUNT - CREDITS RESTRICTED'
                                           TO WS-NOTE
               GO TO 2210-EXIT
           END-IF

           COMPUTE WS-EXCESS = WS-PROJECTED - SWP-THRESHOLD
           PERFORM 2230-ROUND-DOWN

      *    PRODUCT MAXIMUM BALANCE ON THE SAVINGS ACCOUNT
           IF WS-SAV-MAX-BAL > ZERO
               COMPUTE WS-HEADROOM = WS-SAV-MAX-BAL - ACM-LEDGER-BAL
               IF WS-HEADROOM NOT > ZERO
                   MOVE 'SVMX'             TO WS-FAIL-RSN
                   MOVE 'SAVINGS ACCOUNT AT PRODUCT MAXIMUM'
                                           TO WS-NOTE
                   GO TO 2210-EXIT
               END-IF
               IF WS-EXCESS > WS-HEADROOM
                   MOVE WS-HEADROOM        TO WS-EXCESS
                   PERFORM 2230-ROUND-DOWN
                   MOVE 'CAPPED AT SAVINGS PRODUCT MAXIMUM'
                                           TO WS-NOTE
                   ADD 1                   TO WS-SWP-CAPPED
               END-IF
           END-IF

           IF WS-EXCESS NOT > ZERO
           OR WS-EXCESS < SWP-MIN-SWEEP-AMT
               MOVE ZERO                   TO WS-SWEEP-AMT
               GO TO 2210-EXIT
           END-IF
           SET WS-DIR-OUT                  TO TRUE
           MOVE WS-EXCESS                  TO WS-SWEEP-AMT
           .
       2210-EXIT.
           EXIT.
      *================================================================*
       2220-CALC-BACK.
      *================================================================*
           IF ACM-NO-CREDITS OR ACM-NO-POSTINGS
               MOVE 'CARS'                 TO WS-FAIL-RSN
               MOVE 'CURRENT ACCOUNT - CREDITS RESTRICTED'
                                           TO WS-NOTE
               GO TO 2220-EXIT
           END-IF
           MOVE WS-SAV-ACCOUNT             TO ACM-ACCOUNT-RECORD
           IF ACM-NO-DEBITS OR ACM-NO-POSTINGS
               MOVE 'SVRS'                 TO WS-FAIL-RSN
               MOVE 'SAVINGS ACCOUNT - DEBITS RESTRICTED'
                                           TO WS-NOTE
               GO TO 2220-EXIT
           END-IF

           MOVE SWP-RETURN-TARGET          TO WS-TARGET
           IF WS-TARGET < SWP-FLOOR-BAL
               MOVE SWP-FLOOR-BAL          TO WS-TARGET
           END-IF
           COMPUTE WS-NEEDED = WS-TARGET - WS-PROJECTED

      *    ROUND UP TO A WHOLE NUMBER OF SWEEP UNITS
           IF SWP-SWEEP-UNIT > ZERO
               COMPUTE WS-UNITS = WS-NEEDED / SWP-SWEEP-UNIT
               IF WS-UNITS * SWP-SWEEP-UNIT < WS-NEEDED
                   ADD 1                   TO WS-UNITS
               END-IF
               COMPUTE WS-NEEDED = WS-UNITS * SWP-SWEEP-UNIT
           END-IF

           IF ACM-AVAIL-BAL NOT > ZERO
               MOVE 'SFND'                 TO WS-FAIL-RSN
               MOVE 'NO FUNDS IN SAVINGS TO SWEEP BACK'
                                           TO WS-NOTE
               GO TO 2220-EXIT
           END-IF
           IF WS-NEEDED > ACM-AVAIL-BAL
               MOVE ACM-AVAIL-BAL          TO WS-NEEDED
               MOVE 'PART SWEEP - SAVINGS BALANCE TOO LOW'
                                           TO WS-NOTE
               ADD 1                       TO WS-SWP-PART-BACK
           END-IF
           SET WS-DIR-BACK                 TO TRUE
           MOVE WS-NEEDED                  TO WS-SWEEP-AMT
           .
       2220-EXIT.
           EXIT.
      *================================================================*
       2230-ROUND-DOWN.
      *================================================================*
           IF SWP-SWEEP-UNIT > ZERO
           AND WS-EXCESS > ZERO
               COMPUTE WS-UNITS = WS-EXCESS / SWP-SWEEP-UNIT
               COMPUTE WS-EXCESS = WS-UNITS * SWP-SWEEP-UNIT
           END-IF
           .
      *================================================================*
       2300-SWEEP.
      *================================================================*
           IF WS-DIR-OUT
               MOVE SWP-KEY                TO WS-FROM-KEY
               MOVE SWP-SAVINGS-ACCOUNT    TO WS-TO-KEY
           ELSE
               MOVE SWP-SAVINGS-ACCOUNT    TO WS-FROM-KEY
               MOVE SWP-KEY                TO WS-TO-KEY
           END-IF
           MOVE SWP-KEY                    TO WS-XR-KEY
           MOVE WS-DIRECTION               TO WS-XR-DIR

      *---- DEBIT ----------------------------------------------------*
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE WS-FROM-SORT-CODE          TO PST-SORT-CODE
           MOVE WS-FROM-ACCT-NO            TO PST-ACCT-NO
           SET PST-DEBIT                   TO TRUE
           MOVE 'TFR '                     TO PST-TXN-CODE
           MOVE WS-SWEEP-AMT               TO PST-AMOUNT
           IF WS-DIR-OUT
               MOVE 'SAVINGS SWEEP OUT'    TO PST-NARRATIVE-1
           ELSE
               MOVE 'SAVINGS SWEEP RETURN' TO PST-NARRATIVE-1
           END-IF
           STRING 'TO ' WS-TO-SORT-CODE ' ' WS-TO-ACCT-NO
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           PERFORM 5000-WRITE-POSTING
           ADD WS-SWEEP-AMT                TO WS-POST-DR-TOTAL

      *---- CREDIT ---------------------------------------------------*
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE WS-TO-SORT-CODE            TO PST-SORT-CODE
           MOVE WS-TO-ACCT-NO              TO PST-ACCT-NO
           SET PST-CREDIT                  TO TRUE
           MOVE 'TFR '                     TO PST-TXN-CODE
           MOVE WS-SWEEP-AMT               TO PST-AMOUNT
           IF WS-DIR-OUT
               MOVE 'SAVINGS SWEEP IN'     TO PST-NARRATIVE-1
           ELSE
               MOVE 'SWEEP BACK FROM SAVINGS'
                                           TO PST-NARRATIVE-1
           END-IF
           STRING 'FROM ' WS-FROM-SORT-CODE ' ' WS-FROM-ACCT-NO
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           PERFORM 5000-WRITE-POSTING
           ADD WS-SWEEP-AMT                TO WS-POST-CR-TOTAL

      *---- REDUCE AVAILABLE ON THE ACCOUNT DEBITED (SEE HEADER) -----*
           IF WS-DIR-OUT
               MOVE WS-CUR-ACCOUNT         TO ACM-ACCOUNT-RECORD
           ELSE
               MOVE WS-SAV-ACCOUNT         TO ACM-ACCOUNT-RECORD
           END-IF
           SUBTRACT WS-SWEEP-AMT           FROM ACM-AVAIL-BAL
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER
           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '2300-SWEEP'           TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

      *---- INSTRUCTION ----------------------------------------------*
           MOVE WS-BUS-DATE                TO SWP-LAST-SWEEP-DATE
           MOVE WS-DIRECTION               TO SWP-LAST-SWEEP-DIR
           MOVE WS-SWEEP-AMT               TO SWP-LAST-SWEEP-AMT
           MOVE ZERO                       TO SWP-FAIL-COUNT
           MOVE SPACES                     TO SWP-LAST-FAIL-RSN
           SET WS-SWP-CHANGED              TO TRUE
           SET WS-PRINT-LINE               TO TRUE
           IF WS-DIR-OUT
               ADD 1                       TO WS-SWP-OUT
               ADD WS-SWEEP-AMT            TO WS-OUT-TOTAL
           ELSE
               ADD 1                       TO WS-SWP-BACK
               ADD WS-SWEEP-AMT            TO WS-BACK-TOTAL
           END-IF
           .
      *================================================================*
       2600-FAIL.
      *================================================================*
           ADD 1                           TO WS-SWP-FAILED
           ADD 1                           TO SWP-FAIL-COUNT
           MOVE WS-FAIL-RSN                TO SWP-LAST-FAIL-RSN
           IF SWP-FAIL-COUNT NOT < WS-MAX-FAILS
               SET SWP-SUSPENDED           TO TRUE
               MOVE 'SUSPENDED - REPEATED FAILURES'
                                           TO WS-NOTE
               ADD 1                       TO WS-SWP-SUSPENDED
               DISPLAY 'SVSWP010 W002 SWEEP SUSPENDED ' SWP-KEY
                       ' REASON ' WS-FAIL-RSN
           END-IF
           SET WS-SWP-CHANGED              TO TRUE
           SET WS-PRINT-LINE               TO TRUE
           .
      *================================================================*
       2700-UPDATE-INSTRUCTION.
      *================================================================*
           MOVE WS-BUS-DATE                TO SWP-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO SWP-LAST-MAINT-USER
           REWRITE SWP-SWEEP-RECORD
           IF NOT WS-SWPINST-OK
               MOVE '2700-UPDATE-INSTRUCTION' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SWPINST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-SWPINST-STATUS      TO ABN-FILE-STATUS
               MOVE SWP-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       5000-WRITE-POSTING.
      *================================================================*
           MOVE WS-BUS-DATE                TO PST-POST-DATE
                                              PST-VALUE-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           MOVE WS-EXT-REF                 TO PST-EXT-REF
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE SWPPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-SWPPOST-STATUS NOT = '00'
               MOVE '5000-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SWPPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-SWPPOST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-POSTINGS-WRITTEN
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
      *    THE FEED MUST BALANCE - EVERY DEBIT HAS ITS CREDIT
           IF WS-POST-DR-TOTAL NOT = WS-POST-CR-TOTAL
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'SWPPOST DEBITS NOT EQUAL TO CREDITS'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'SWEEP INSTRUCTIONS READ'  TO TL-LABEL
           MOVE WS-SWP-READ                TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  NOT ACTIVE (SUSPENDED/CANCELLED)'
                                           TO TL-LABEL
           MOVE WS-SWP-INACTIVE            TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  NOT YET STARTED'        TO TL-LABEL
           MOVE WS-SWP-NOT-STARTED         TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  EXPIRED - NOW CANCELLED' TO TL-LABEL
           MOVE WS-SWP-EXPIRED             TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  ALREADY SWEPT TODAY (RERUN)' TO TL-LABEL
           MOVE WS-SWP-ALREADY             TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  NO SWEEP NEEDED'        TO TL-LABEL
           MOVE WS-SWP-NO-ACTION           TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'SWEPT OUT TO SAVINGS'     TO TL-LABEL
           MOVE WS-SWP-OUT                 TO TL-COUNT
           MOVE WS-OUT-TOTAL               TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  OF WHICH CAPPED AT PRODUCT MAXIMUM'
                                           TO TL-LABEL
           MOVE WS-SWP-CAPPED              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'SWEPT BACK TO CURRENT ACCOUNT' TO TL-LABEL
           MOVE WS-SWP-BACK                TO TL-COUNT
           MOVE WS-BACK-TOTAL              TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  OF WHICH PART SWEEPS'   TO TL-LABEL
           MOVE WS-SWP-PART-BACK           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'FAILED'                   TO TL-LABEL
           MOVE WS-SWP-FAILED              TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  OF WHICH NOW SUSPENDED' TO TL-LABEL
           MOVE WS-SWP-SUSPENDED           TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'PENDING FEED POSTINGS READ' TO TL-LABEL
           MOVE WS-PND-READ                TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  ON SWEEP ACCOUNTS (DEBITS VALUE)'
                                           TO TL-LABEL
           MOVE WS-PND-MATCHED             TO TL-COUNT
           MOVE WS-PND-DR-TOTAL            TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'SWPPOST RECORDS WRITTEN'  TO TL-LABEL
           MOVE WS-POSTINGS-WRITTEN        TO TL-COUNT
           MOVE WS-POST-DR-TOTAL           TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL

           CLOSE SWPINST-FILE ACCTMAST-FILE PRDPARM-FILE PENDPOST-FILE
                 SWPPOST-FILE REPORT-FILE
           DISPLAY 'SVSWP010 - OUT ' WS-SWP-OUT ' BACK ' WS-SWP-BACK
                   ' FAILED ' WS-SWP-FAILED
           IF WS-SWP-FAILED > ZERO
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
           MOVE 3                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SWP-SORT-CODE              TO DL-SORT-CODE
           MOVE SWP-ACCT-NO                TO DL-ACCT-NO
           MOVE SWP-SAV-SORT-CODE          TO DL-SAV-SC
           MOVE SWP-SAV-ACCT-NO            TO DL-SAV-ACCT
           MOVE WS-DIRECTION               TO DL-DIR
           MOVE WS-PROJECTED               TO DL-PROJECTED
           MOVE WS-PENDING-DR              TO DL-PENDING
           MOVE WS-SWEEP-AMT               TO DL-AMOUNT
           MOVE WS-FAIL-RSN                TO DL-RSN
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '7100-REPORT-DETAIL'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'REPORT WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-REPORT-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-WRITE-TOTAL.
      *================================================================*
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           .
      *================================================================*
       8000-READ-SWEEP.
      *================================================================*
           READ SWPINST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-SWPINST-OK
                   ADD 1                   TO WS-SWP-READ
                   MOVE SWP-KEY            TO WS-SWEEP-KEY
               WHEN WS-SWPINST-EOF
                   SET WS-END-OF-SWEEPS    TO TRUE
                   MOVE HIGH-VALUES        TO WS-SWEEP-KEY
               WHEN OTHER
                   MOVE '8000-READ-SWEEP'  TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SWPINST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-SWPINST-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8100-READ-PENDING.
      *================================================================*
      *    PENDPOST IS SORTED ON PST-KEY BY STEP SW005, SO IT IS IN
      *    ACCOUNT ORDER LIKE THE SWEEP FILE. A KEY LOWER THAN THE
      *    PREVIOUS ONE MEANS THE SORT STEP WAS BYPASSED.
           READ PENDPOST-FILE INTO WS-PEND-RECORD
           EVALUATE TRUE
               WHEN WS-PENDPOST-OK
                   ADD 1                   TO WS-PND-READ
                   IF PND-KEY < WS-PREV-PND-KEY
                       MOVE '8100-READ-PENDING' TO ABN-PARAGRAPH
                       MOVE 1006           TO ABN-ABEND-CODE
                       MOVE 'PENDPOST NOT IN ACCOUNT ORDER'
                                           TO ABN-MESSAGE
                       MOVE PND-KEY        TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
                   MOVE PND-KEY            TO WS-PREV-PND-KEY
               WHEN WS-PENDPOST-EOF
                   SET WS-END-OF-PENDING   TO TRUE
                   MOVE HIGH-VALUES        TO PND-KEY
               WHEN OTHER
                   MOVE '8100-READ-PENDING' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PENDPOST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-PENDPOST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
