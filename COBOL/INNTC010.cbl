       IDENTIFICATION DIVISION.
       PROGRAM-ID.    INNTC010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-07-04.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : INNTC010                                            *
      * TITLE    : NOTICE SAVINGS - PAY MATURED WITHDRAWAL NOTICES     *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : INNTC01 (DAILY, BEFORE PSEOD01)                     *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (NOTICE / PENALTY)    *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : SAVNTC   - WITHDRAWAL NOTICES (VSAM KSDS, DYNAMIC)  *
      *            ACCTMAST - DDA ACCOUNT MASTER (RANDOM)              *
      * OUTPUT   : NTCPOST  - WITHDRAWAL TRANSFERS AND PENALTY DEBITS  *
      *                       FOR TONIGHT'S PSEOD01 (PSTXNREC, MOD)    *
      *            LETTERS  - CONFIRMATION / FAILURE LETTERS (FB 150)  *
      *            REPORT   - NOTICE PAYMENT REPORT (FBA 133)          *
      *----------------------------------------------------------------*
      * EVERY PENDING NOTICE WHOSE DUE DATE IS ON OR BEFORE THE        *
      * BUSINESS DATE (EARLY WITHDRAWALS ARE DUE THE DAY THEY ARE      *
      * GIVEN) IS PAID:                                                *
      *   - 'N' NOTICE    : THE NOTIFIED AMOUNT, OR THE AVAILABLE      *
      *                     BALANCE IF LOWER (THE CUSTOMER MAY HAVE    *
      *                     WITHDRAWN ON ANOTHER NOTICE MEANWHILE)     *
      *   - 'E' EARLY     : THE AMOUNT LESS A PENALTY OF               *
      *                     PRD-CR-PENALTY-DAYS DAYS' INTEREST AT THE  *
      *                     ACCOUNT RATE. THE PENALTY IS A SEPARATE    *
      *                     'CHG ' DEBIT SO THE STATEMENT SHOWS IT.    *
      *   - 'C' CLOSURE   : THE WHOLE AVAILABLE BALANCE, AND A CLOSURE *
      *                     REQUEST FOR DDCLS010.                      *
      * THE PAYMENT IS A 'TFR ' DEBIT FROM THE NOTICE ACCOUNT AND A    *
      * 'TFR ' CREDIT TO THE NOMINATED MEGABANK ACCOUNT. NOMINATED     *
      * ACCOUNTS AT OTHER BANKS ARE NOT ALLOWED ON THIS PRODUCT (THE   *
      * BRANCH SCREEN ENFORCES IT - THIS PROGRAM FAILS SUCH NOTICES).  *
      *                                                                *
      * THE AVAILABLE BALANCE IS REDUCED HERE (AS A MEMO WOULD) SO A   *
      * SECOND NOTICE DUE THE SAME DAY SEES THE REDUCED FUNDS.         *
      * PIR 2012/019 OPEN: PSRCN010 REPORTS THE DIFFERENCE AS A MEMO   *
      * BREAK BECAUSE NO MEMO LOG RECORD IS WRITTEN.                   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-07-04 AOS  ORIGINAL VERSION                               *
      * 2012-02-20 PDQ  CLOSURE NOTICES ('C')                          *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SAVNTC-FILE      ASSIGN TO SAVNTC
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS NTC-KEY
                                   FILE STATUS IS WS-SAVNTC-STATUS.
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
           SELECT NTCPOST-FILE     ASSIGN TO NTCPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-NTCPOST-STATUS.
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
       FD  SAVNTC-FILE.
           COPY NTCWDREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  NTCPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  NTCPOST-RECORD                  PIC X(200).
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(150).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'INNTC010'.
      *
       01  WS-CONSTANTS.
           05  WS-DEFAULT-PENALTY-DAYS     PIC 9(03) VALUE 090.
           05  WS-DAY-BASIS                PIC 9(03) VALUE 365.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00011000.
      *
       01  WS-FILE-STATUSES.
           05  WS-SAVNTC-STATUS            PIC X(02).
               88  WS-SAVNTC-OK                VALUE '00'.
               88  WS-SAVNTC-EOF               VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-NTCPOST-STATUS           PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-NOTICES           VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-PAY-AMOUNT               PIC S9(13)V99 COMP-3.
           05  WS-PENALTY                  PIC S9(13)V99 COMP-3.
           05  WS-PENALTY-DAYS             PIC 9(03).
           05  WS-NOTICE-DAYS              PIC 9(03).
           05  WS-FAIL-RSN                 PIC X(04).
           05  WS-NOTE                     PIC X(40).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-SAVED-ACCOUNT            PIC X(400) VALUE SPACES.
           05  WS-SAVED-NTC-KEY            PIC X(22).
           05  WS-TO-KEY.
               10  WS-TO-SORT-CODE         PIC X(06).
               10  WS-TO-ACCT-NO           PIC X(08).
      *
       01  WS-LETTER.
           05  LT-LETTER-CD                PIC X(04).
           05  LT-ISSUE-DATE               PIC 9(08).
           05  LT-SORT-CODE                PIC X(06).
           05  LT-ACCT-NO                  PIC X(08).
           05  LT-CIF-NO                   PIC X(12).
           05  LT-NOTICE-TYPE              PIC X(01).
           05  LT-GIVEN-DATE               PIC 9(08).
           05  LT-REQUESTED                PIC 9(11)V99.
           05  LT-PAID                     PIC 9(11)V99.
           05  LT-PENALTY                  PIC 9(11)V99.
           05  LT-TO-SORT-CODE             PIC X(06).
           05  LT-TO-ACCT-NO               PIC X(08).
           05  LT-FAIL-REASON              PIC X(04).
           05  FILLER                      PIC X(46).
      *
       01  WS-COUNTERS.
           05  WS-NOTICES-READ             PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOT-DUE                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID                     PIC S9(09) COMP-3 VALUE 0.
           05  WS-PART-PAID                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EARLY                    PIC S9(09) COMP-3 VALUE 0.
           05  WS-CLOSURES                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-FAILED                   PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-PENALTY-TOTAL            PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'INNTC010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - NOTICE ACCOUNT WITHDRAWALS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(16) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(03) VALUE 'T'.
           05  FILLER            PIC X(10) VALUE 'GIVEN'.
           05  FILLER            PIC X(10) VALUE 'DUE'.
           05  FILLER            PIC X(15) VALUE '     REQUESTED'.
           05  FILLER            PIC X(15) VALUE '          PAID'.
           05  FILLER            PIC X(13) VALUE '     PENALTY'.
           05  FILLER            PIC X(17) VALUE 'TO ACCOUNT'.
           05  FILLER            PIC X(33) VALUE 'NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TYPE                     PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-GIVEN                    PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DUE                      PIC 9(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-REQUESTED                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-PAID                     PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-PENALTY                  PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TO-SC                    PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-TO-ACCT                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(30).
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
           PERFORM 2000-PROCESS-NOTICE THRU 2000-EXIT
               UNTIL WS-END-OF-NOTICES
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       DATECARD-FILE
                I-O    SAVNTC-FILE
                       ACCTMAST-FILE
                EXTEND NTCPOST-FILE
                OUTPUT LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-SAVNTC-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-NTCPOST-STATUS NOT = '00'
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
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           MOVE LOW-VALUES                 TO NTC-KEY
           START SAVNTC-FILE KEY NOT < NTC-KEY
           IF NOT WS-SAVNTC-OK
               SET WS-END-OF-NOTICES       TO TRUE
           ELSE
               PERFORM 8000-READ-NOTICE
           END-IF
           .
      *================================================================*
       2000-PROCESS-NOTICE.
      *================================================================*
           IF NOT NTC-PENDING
               PERFORM 8000-READ-NOTICE
               GO TO 2000-EXIT
           END-IF
           IF NTC-NOTICE AND NTC-DUE-DATE > WS-BUS-DATE
               ADD 1                       TO WS-NOT-DUE
               PERFORM 8000-READ-NOTICE
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES                     TO WS-FAIL-RSN WS-NOTE
           MOVE ZERO                       TO WS-PAY-AMOUNT WS-PENALTY
           MOVE NTC-KEY                    TO WS-SAVED-NTC-KEY

           PERFORM 2100-CHECK-ACCOUNTS THRU 2100-EXIT
           IF WS-FAIL-RSN = SPACES
               PERFORM 2200-CALCULATE
           END-IF
           IF WS-FAIL-RSN = SPACES
               PERFORM 2300-PAY
           ELSE
               PERFORM 2400-FAIL
           END-IF

           MOVE WS-BUS-DATE                TO NTC-PAID-DATE
           REWRITE NTC-NOTICE-RECORD
           IF NOT WS-SAVNTC-OK
               MOVE '2000-PROCESS-NOTICE'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SAVNTC REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-SAVNTC-STATUS       TO ABN-FILE-STATUS
               MOVE NTC-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           PERFORM 2500-LETTER
           PERFORM 7100-REPORT-DETAIL
           PERFORM 8000-READ-NOTICE
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-CHECK-ACCOUNTS.
      *================================================================*
      *    NOMINATED ACCOUNT FIRST, THEN THE NOTICE ACCOUNT - SO THAT
      *    ACM-ACCOUNT-RECORD HOLDS THE NOTICE ACCOUNT AFTERWARDS.
           MOVE NTC-TO-SORT-CODE           TO WS-TO-SORT-CODE
           MOVE NTC-TO-ACCT-NO             TO WS-TO-ACCT-NO
           MOVE WS-TO-KEY                  TO ACM-KEY
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'NOMA'             TO WS-FAIL-RSN
                   MOVE 'NOMINATED ACCOUNT NOT A MEGABANK ACCOUNT'
                                           TO WS-NOTE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE '2100-CHECK-ACCOUNTS' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               WHEN NOT ACM-OPEN
               WHEN ACM-NO-CREDITS OR ACM-NO-POSTINGS
                   MOVE 'NOMC'             TO WS-FAIL-RSN
                   MOVE 'NOMINATED ACCOUNT CANNOT TAKE CREDITS'
                                           TO WS-NOTE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-FAIL-RSN NOT = SPACES
               GO TO 2100-EXIT
           END-IF

           MOVE NTC-SORT-CODE              TO ACM-SORT-CODE
           MOVE NTC-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE 'NACC'             TO WS-FAIL-RSN
                   MOVE 'NOTICE ACCOUNT NOT FOUND' TO WS-NOTE
               WHEN ACM-CLOSED
                   MOVE 'NCLS'             TO WS-FAIL-RSN
                   MOVE 'NOTICE ACCOUNT CLOSED' TO WS-NOTE
               WHEN ACM-FROZEN OR ACM-NO-DEBITS OR ACM-NO-POSTINGS
                   MOVE 'NRST'             TO WS-FAIL-RSN
                   MOVE 'NOTICE ACCOUNT RESTRICTED' TO WS-NOTE
               WHEN ACM-AVAIL-BAL NOT > ZERO
                   MOVE 'NFND'             TO WS-FAIL-RSN
                   MOVE 'NO FUNDS AVAILABLE' TO WS-NOTE
               WHEN OTHER
                   MOVE ACM-ACCOUNT-RECORD TO WS-SAVED-ACCOUNT
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-CALCULATE.
      *================================================================*
           MOVE WS-DEFAULT-PENALTY-DAYS    TO WS-PENALTY-DAYS
           MOVE ACM-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               IF PRD-CR-PENALTY-DAYS NUMERIC
                   MOVE PRD-CR-PENALTY-DAYS TO WS-PENALTY-DAYS
               END-IF
               MOVE PRD-NOTICE-DAYS        TO WS-NOTICE-DAYS
           END-IF

           EVALUATE TRUE
               WHEN NTC-CLOSURE
                   MOVE ACM-AVAIL-BAL      TO WS-PAY-AMOUNT
               WHEN NTC-AMOUNT > ACM-AVAIL-BAL
                   MOVE ACM-AVAIL-BAL      TO WS-PAY-AMOUNT
                   MOVE 'PART PAID - BALANCE LOWER THAN NOTICE'
                                           TO WS-NOTE
                   ADD 1                   TO WS-PART-PAID
               WHEN OTHER
                   MOVE NTC-AMOUNT         TO WS-PAY-AMOUNT
           END-EVALUATE

           IF NTC-EARLY
      *        PENALTY = AMOUNT * RATE * DAYS / 365, TAKEN FROM THE
      *        AMOUNT PAID (NOT AN ADDITIONAL DEBIT FROM THE BALANCE).
               COMPUTE WS-PENALTY ROUNDED = WS-PAY-AMOUNT
                       * ACM-INT-RATE-CR / 100
                       * WS-PENALTY-DAYS / WS-DAY-BASIS
               IF WS-PENALTY > WS-PAY-AMOUNT
                   MOVE WS-PAY-AMOUNT      TO WS-PENALTY
               END-IF
               SUBTRACT WS-PENALTY         FROM WS-PAY-AMOUNT
               ADD 1                       TO WS-EARLY
           END-IF
           .
      *================================================================*
       2300-PAY.
      *================================================================*
      *---- DEBIT THE NOTICE ACCOUNT ----------------------------------*
           IF WS-PAY-AMOUNT > ZERO
               MOVE SPACES                 TO PST-POSTING-RECORD
               MOVE NTC-SORT-CODE          TO PST-SORT-CODE
               MOVE NTC-ACCT-NO            TO PST-ACCT-NO
               SET PST-DEBIT               TO TRUE
               MOVE 'TFR '                 TO PST-TXN-CODE
               MOVE WS-PAY-AMOUNT          TO PST-AMOUNT
               MOVE 'NOTICE WITHDRAWAL'    TO PST-NARRATIVE-1
               STRING 'TO ' NTC-TO-SORT-CODE ' ' NTC-TO-ACCT-NO
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
               PERFORM 5000-WRITE-POSTING
      *---- CREDIT THE NOMINATED ACCOUNT ------------------------------*
               MOVE SPACES                 TO PST-POSTING-RECORD
               MOVE NTC-TO-SORT-CODE       TO PST-SORT-CODE
               MOVE NTC-TO-ACCT-NO         TO PST-ACCT-NO
               SET PST-CREDIT              TO TRUE
               MOVE 'TFR '                 TO PST-TXN-CODE
               MOVE WS-PAY-AMOUNT          TO PST-AMOUNT
               MOVE 'FROM NOTICE ACCOUNT'  TO PST-NARRATIVE-1
               STRING NTC-SORT-CODE ' ' NTC-ACCT-NO
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
               PERFORM 5000-WRITE-POSTING
           END-IF
      *---- EARLY WITHDRAWAL PENALTY ----------------------------------*
           IF WS-PENALTY > ZERO
               MOVE SPACES                 TO PST-POSTING-RECORD
               MOVE NTC-SORT-CODE          TO PST-SORT-CODE
               MOVE NTC-ACCT-NO            TO PST-ACCT-NO
               SET PST-DEBIT               TO TRUE
               MOVE 'CHG '                 TO PST-TXN-CODE
               MOVE WS-PENALTY             TO PST-AMOUNT
               MOVE 'EARLY WITHDRAWAL - INTEREST LOST'
                                           TO PST-NARRATIVE-1
               STRING WS-PENALTY-DAYS ' DAYS INTEREST'
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
               PERFORM 5000-WRITE-POSTING
           END-IF

      *---- REDUCE AVAILABLE NOW (SEE HEADER) -------------------------*
           MOVE WS-SAVED-ACCOUNT           TO ACM-ACCOUNT-RECORD
           COMPUTE ACM-AVAIL-BAL = ACM-AVAIL-BAL
                                 - WS-PAY-AMOUNT - WS-PENALTY
           IF NTC-CLOSURE
               SET ACM-PENDING-CLOSE       TO TRUE
               MOVE WS-BUS-DATE            TO ACM-CLOSE-REQ-DATE
               MOVE 'NTCE'                 TO ACM-CLOSE-RSN
               MOVE NTC-TO-SORT-CODE       TO ACM-CLOSE-XFER-SORT-CODE
               MOVE NTC-TO-ACCT-NO         TO ACM-CLOSE-XFER-ACCT-NO
               ADD 1                       TO WS-CLOSURES
               MOVE 'CLOSURE REQUESTED'    TO WS-NOTE
           END-IF
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER
           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '2300-PAY'             TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

           SET NTC-PAID                    TO TRUE
           MOVE WS-PAY-AMOUNT              TO NTC-PAID-AMOUNT
           MOVE WS-PENALTY                 TO NTC-PENALTY-AMT
           ADD 1                           TO WS-PAID
           ADD WS-PAY-AMOUNT               TO WS-PAID-TOTAL
           ADD WS-PENALTY                  TO WS-PENALTY-TOTAL
           .
      *================================================================*
       2400-FAIL.
      *================================================================*
           SET NTC-FAILED                  TO TRUE
           MOVE WS-FAIL-RSN                TO NTC-FAIL-REASON
           MOVE ZERO                       TO NTC-PAID-AMOUNT
                                              NTC-PENALTY-AMT
           ADD 1                           TO WS-FAILED
           .
      *================================================================*
       2500-LETTER.
      *================================================================*
           MOVE SPACES                     TO WS-LETTER
           IF NTC-PAID
               MOVE 'NTCP'                 TO LT-LETTER-CD
           ELSE
               MOVE 'NTCF'                 TO LT-LETTER-CD
           END-IF
           MOVE WS-BUS-DATE                TO LT-ISSUE-DATE
           MOVE NTC-SORT-CODE              TO LT-SORT-CODE
           MOVE NTC-ACCT-NO                TO LT-ACCT-NO
           IF WS-SAVED-ACCOUNT NOT = SPACES
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               MOVE ACM-CIF-NO             TO LT-CIF-NO
           END-IF
           MOVE NTC-TYPE                   TO LT-NOTICE-TYPE
           MOVE NTC-GIVEN-DATE             TO LT-GIVEN-DATE
           MOVE NTC-AMOUNT                 TO LT-REQUESTED
           MOVE NTC-PAID-AMOUNT            TO LT-PAID
           MOVE NTC-PENALTY-AMT            TO LT-PENALTY
           MOVE NTC-TO-SORT-CODE           TO LT-TO-SORT-CODE
           MOVE NTC-TO-ACCT-NO             TO LT-TO-ACCT-NO
           MOVE NTC-FAIL-REASON            TO LT-FAIL-REASON
           WRITE LETTERS-RECORD            FROM WS-LETTER
           MOVE SPACES                     TO WS-SAVED-ACCOUNT
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
           STRING 'NTC' NTC-KEY
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE NTCPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-NTCPOST-STATUS NOT = '00'
               MOVE '5000-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'NTCPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-NTCPOST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'PENDING NOTICES READ'     TO TL-LABEL
           MOVE WS-NOTICES-READ            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  NOT YET DUE'            TO TL-LABEL
           MOVE WS-NOT-DUE                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PAID'                     TO TL-LABEL
           MOVE WS-PAID                    TO TL-COUNT
           MOVE WS-PAID-TOTAL              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  PART PAID'              TO TL-LABEL
           MOVE WS-PART-PAID               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  EARLY / PENALTIES'      TO TL-LABEL
           MOVE WS-EARLY                   TO TL-COUNT
           MOVE WS-PENALTY-TOTAL           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  CLOSURES REQUESTED'     TO TL-LABEL
           MOVE WS-CLOSURES                TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'FAILED'                   TO TL-LABEL
           MOVE WS-FAILED                  TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE SAVNTC-FILE ACCTMAST-FILE PRDPARM-FILE NTCPOST-FILE
                 LETTERS-FILE REPORT-FILE
           DISPLAY 'INNTC010 - PAID ' WS-PAID ' FAILED ' WS-FAILED
           IF WS-FAILED > ZERO
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
           MOVE NTC-SORT-CODE              TO DL-SORT-CODE
           MOVE NTC-ACCT-NO                TO DL-ACCT-NO
           MOVE NTC-TYPE                   TO DL-TYPE
           MOVE NTC-GIVEN-DATE             TO DL-GIVEN
           MOVE NTC-DUE-DATE               TO DL-DUE
           MOVE NTC-AMOUNT                 TO DL-REQUESTED
           MOVE NTC-PAID-AMOUNT            TO DL-PAID
           MOVE NTC-PENALTY-AMT            TO DL-PENALTY
           MOVE NTC-TO-SORT-CODE           TO DL-TO-SC
           MOVE NTC-TO-ACCT-NO             TO DL-TO-ACCT
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-NOTICE.
      *================================================================*
           READ SAVNTC-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-SAVNTC-OK
                   IF NTC-PENDING
                       ADD 1               TO WS-NOTICES-READ
                   END-IF
               WHEN WS-SAVNTC-EOF
                   SET WS-END-OF-NOTICES   TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-NOTICE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SAVNTC READ FAILED' TO ABN-MESSAGE
                   MOVE WS-SAVNTC-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
