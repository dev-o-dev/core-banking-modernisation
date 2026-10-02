       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CQCLR010.
       AUTHOR.        R J HALLIDAY.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1990-06-11.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CQCLR010                                            *
      * TITLE    : CHEQUES - INWARD CLEARING (PAYING BANK DECISIONS)   *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CQCLR01 STEP CQ020                                  *
      *----------------------------------------------------------------*
      * INPUT    : INWARD   - CHEQUES DRAWN ON MEGABANK ACCOUNTS,      *
      *                       SORTED BY ACCOUNT (STEP CQ010) - CHQRECS *
      *            STOPS    - CHEQUE STOPS (VSAM KSDS, DYNAMIC)        *
      *            PRDPARM  - PRODUCT PARAMETERS (BUFFER, ITEM FEES)   *
      *            CIFMAST  - CIF (DECEASED CHECK)                     *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (RANDOM) - COUNTERS   *
      * OUTPUT   : CHQPOST  - CHEQUE DEBITS FOR PSEOD01 - PSTXNREC     *
      *            RETURNS  - UNPAID CHEQUES FOR THE CLEARING AGENT    *
      *            LETTERS  - UNPAID CHEQUE LETTERS (FB 150)           *
      *            REPORT   - INWARD CLEARING REPORT (FBA 133)         *
      *----------------------------------------------------------------*
      * EACH CHEQUE IS PAID (A 'CHQ ' DEBIT POSTING) OR RETURNED       *
      * UNPAID. THE DECISION MUST BE MADE BY THE END OF THE SECOND     *
      * DAY OF THE CLEARING CYCLE - THIS PROGRAM RUNS ON THE DAY THE   *
      * ITEMS ARRIVE SO THERE IS ONE DAY'S MARGIN.                     *
      *                                                                *
      * ORDER OF CHECKS:                                               *
      *   NO ACCOUNT, CLOSED, DECEASED, STOPPED, OUT OF DATE (CHEQUE   *
      *   DATED MORE THAN 6 MONTHS AGO), FROZEN / RESTRICTED, FUNDS.   *
      * THE FUNDS CHECK USES THE SAME RULE AS DMCOL010 (AVAILABLE +    *
      * LIMIT + BUFFER, LESS ITEMS ALREADY PAID IN THIS RUN). CHEQUES  *
      * ON THE SAME ACCOUNT ARE TAKEN IN SERIAL NUMBER ORDER.          *
      *                                                                *
      * A STOP CAN COVER A RANGE OF SERIAL NUMBERS. A STOP WITH AN     *
      * AMOUNT ONLY MATCHES A CHEQUE FOR THAT AMOUNT.                  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1990-06-11 RJH  ORIGINAL VERSION (DD0610)                      *
      * 1998-11-30 MLW  Y2K - CHEQUE DATE CCYYMMDD                     *
      * 2004-08-09 SAT  BUFFER / PAID ITEM COUNTER (OFT)               *
      * 2009-10-01 AOS  UNPAID ITEM COUNTER FOR CGCAL010               *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INWARD-FILE      ASSIGN TO INWARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-INWARD-STATUS.
           SELECT STOPS-FILE       ASSIGN TO STOPS
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS CQS-KEY
                                   FILE STATUS IS WS-STOPS-STATUS.
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
           SELECT CHQPOST-FILE     ASSIGN TO CHQPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CHQPOST-STATUS.
           SELECT RETURNS-FILE     ASSIGN TO RETURNS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RETURNS-STATUS.
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
       FD  INWARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  INWARD-RECORD                   PIC X(150).
       FD  STOPS-FILE.
       01  STOPS-RECORD.
           05  STOPS-KEY                   PIC X(20).
           05  FILLER                      PIC X(80).
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CHQPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHQPOST-RECORD                  PIC X(200).
       FD  RETURNS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RETURNS-RECORD                  PIC X(150).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CQCLR010'.
      *
       01  WS-CONSTANTS.
           05  WS-STALE-DAYS               PIC S9(05) COMP VALUE +183.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00030000.
      *
       01  WS-FILE-STATUSES.
           05  WS-INWARD-STATUS            PIC X(02).
               88  WS-INWARD-OK                VALUE '00'.
               88  WS-INWARD-EOF               VALUE '10'.
           05  WS-STOPS-STATUS             PIC X(02).
               88  WS-STOPS-OK                 VALUE '00'.
               88  WS-STOPS-EOF                VALUE '10'.
               88  WS-STOPS-NOTFND             VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-CHQPOST-STATUS           PIC X(02).
           05  WS-RETURNS-STATUS           PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-PAYER-FOUND-IND          PIC X(01).
               88  WS-PAYER-FOUND              VALUE 'Y'.
           05  WS-PAYER-CHANGED-IND        PIC X(01).
               88  WS-PAYER-CHANGED            VALUE 'Y'.
           05  WS-DECEASED-IND             PIC X(01).
               88  WS-DECEASED                 VALUE 'Y'.
           05  WS-STOP-HIT-IND             PIC X(01).
               88  WS-STOP-HIT                 VALUE 'Y'.
           05  WS-END-STOPS-IND            PIC X(01).
               88  WS-END-STOPS                VALUE 'Y'.
           05  WS-TRAILER-IND              PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-CHQ-DATE-INT             PIC S9(09) COMP.
           05  WS-CURR-PAYER               PIC X(14) VALUE LOW-VALUES.
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-SPENDING-POWER           PIC S9(13)V99 COMP-3.
           05  WS-BUFFER                   PIC S9(13)V99 COMP-3.
           05  WS-RESERVED                 PIC S9(13)V99 COMP-3.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-UNPAID-FEE               PIC S9(13)V99 COMP-3.
           05  WS-RETURN-RSN               PIC X(02).
           05  WS-RESULT-TEXT              PIC X(30).
           05  WS-PAID-ITEM-IND            PIC X(01).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-SAVED-ACCOUNT            PIC X(400).
      *
       01  WS-LETTER.
           05  LTR-SORT-CODE               PIC X(06).
           05  LTR-ACCT-NO                 PIC X(08).
           05  LTR-CIF-NO                  PIC X(12).
           05  LTR-SERIAL-NO               PIC X(06).
           05  LTR-AMOUNT                  PIC 9(11)V99.
           05  LTR-PAYEE                   PIC X(35).
           05  LTR-REASON                  PIC X(02).
           05  LTR-REASON-TEXT             PIC X(30).
           05  LTR-FEE                     PIC 9(07)V99.
           05  FILLER                      PIC X(29).
      *
       01  WS-COUNTERS.
           05  WS-ITEMS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ITEMS-PAID               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ITEMS-RETURNED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-ITEMS-STOPPED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-ITEMS-BUFFER        PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-IN                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-PAID               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-RETURNED           PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CQCLR010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - INWARD CHEQUE CLEARING'.
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
           05  FILLER            PIC X(08) VALUE 'SERIAL'.
           05  FILLER            PIC X(17) VALUE '          AMOUNT'.
           05  FILLER            PIC X(10) VALUE 'CHQ DATE'.
           05  FILLER            PIC X(08) VALUE 'PRES BK'.
           05  FILLER            PIC X(10) VALUE 'RESULT'.
           05  FILLER            PIC X(04) VALUE 'RSN'.
           05  FILLER            PIC X(56) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SERIAL                   PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CHQ-DATE                 PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PRES-BANK                PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RESULT                   PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RSN                      PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(30).
           05  FILLER                      PIC X(16) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY CHQRECS.
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
           PERFORM 2000-PROCESS-ITEM
               UNTIL WS-END-OF-INPUT
           PERFORM 2900-END-PAYER
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  INWARD-FILE
                       PRDPARM-FILE
                       CIFMAST-FILE
                       DATECARD-FILE
                I-O    STOPS-FILE
                       ACCTMAST-FILE
                OUTPUT CHQPOST-FILE
                       RETURNS-FILE
                       LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-INWARD-OK
           OR NOT WS-STOPS-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-CHQPOST-STATUS NOT = '00'
           OR WS-RETURNS-STATUS NOT = '00'
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

           PERFORM 8000-READ-ITEM
           IF WS-END-OF-INPUT OR NOT CQI-HEADER
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'INWARD CLEARING FILE HAS NO HEADER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           DISPLAY 'CQCLR010 - FILE ' CQIH-FILE-ID
                   ' CLEARING DATE ' CQIH-CLEARING-DATE
           PERFORM 8000-READ-ITEM
           .
      *================================================================*
       2000-PROCESS-ITEM.
      *================================================================*
           EVALUATE TRUE
               WHEN CQI-ITEM
                   IF CQI-SORT-CODE NOT = WS-CURR-PAYER (1:6)
                   OR CQI-ACCT-NO NOT = WS-CURR-PAYER (7:8)
                       PERFORM 2900-END-PAYER
                       PERFORM 2050-START-PAYER
                   END-IF
                   PERFORM 2100-DECIDE-ITEM
               WHEN CQI-TRAILER
                   SET WS-TRAILER-SEEN     TO TRUE
                   IF CQIT-ITEM-COUNT NOT = WS-ITEMS-READ
                   OR CQIT-TOTAL-AMT NOT = WS-TOTAL-IN
                       MOVE '2000-PROCESS-ITEM' TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'INWARD TRAILER COUNT / AMOUNT MISMATCH'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           PERFORM 8000-READ-ITEM
           .
      *================================================================*
       2050-START-PAYER.
      *================================================================*
           MOVE CQI-SORT-CODE              TO WS-CURR-PAYER (1:6)
           MOVE CQI-ACCT-NO                TO WS-CURR-PAYER (7:8)
           MOVE ZERO                       TO WS-RESERVED
                                              WS-SPENDING-POWER
                                              WS-BUFFER WS-UNPAID-FEE
           MOVE 'N'                        TO WS-PAYER-FOUND-IND
                                              WS-PAYER-CHANGED-IND
                                              WS-DECEASED-IND
           MOVE CQI-SORT-CODE              TO ACM-SORT-CODE
           MOVE CQI-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   SET WS-PAYER-FOUND      TO TRUE
                   MOVE ACM-ACCOUNT-RECORD TO WS-SAVED-ACCOUNT
                   MOVE ACM-OD-LIMIT       TO WS-OD-LIMIT
                   IF ACM-OD-EXPIRY-DATE NOT = ZERO
                   AND ACM-OD-EXPIRY-DATE < WS-BUS-DATE
                       MOVE ZERO           TO WS-OD-LIMIT
                   END-IF
                   COMPUTE WS-SPENDING-POWER =
                           ACM-AVAIL-BAL + WS-OD-LIMIT
                   MOVE ACM-PROD-CD        TO PRD-PROD-CD
                   READ PRDPARM-FILE
                   IF WS-PRDPARM-OK
                       MOVE PRD-UNARR-OD-BUFFER TO WS-BUFFER
                       MOVE PRD-UNPAID-ITEM-FEE TO WS-UNPAID-FEE
                   END-IF
                   MOVE ACM-CIF-NO         TO CIF-NO
                   READ CIFMAST-FILE
                   IF WS-CIFMAST-OK AND CIF-DECEASED
                       SET WS-DECEASED     TO TRUE
                   END-IF
               WHEN WS-ACCTMAST-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE '2050-START-PAYER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2100-DECIDE-ITEM.
      *================================================================*
           MOVE CQI-AMOUNT                 TO WS-AMOUNT
           MOVE SPACES                     TO WS-RETURN-RSN
                                              WS-RESULT-TEXT
           MOVE 'N'                        TO WS-PAID-ITEM-IND
           IF WS-PAYER-FOUND
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
           END-IF

           EVALUATE TRUE
               WHEN NOT WS-PAYER-FOUND
                   MOVE 'NA'               TO WS-RETURN-RSN
                   MOVE 'NO ACCOUNT'       TO WS-RESULT-TEXT
               WHEN ACM-CLOSED OR ACM-PENDING-CLOSE
                   MOVE 'AC'               TO WS-RETURN-RSN
                   MOVE 'ACCOUNT CLOSED'   TO WS-RESULT-TEXT
               WHEN WS-DECEASED
                   MOVE 'DE'               TO WS-RETURN-RSN
                   MOVE 'DRAWER DECEASED'  TO WS-RESULT-TEXT
               WHEN OTHER
                   PERFORM 2200-CHECK-STOPS THRU 2200-EXIT
                   IF WS-STOP-HIT
                       MOVE 'PS'           TO WS-RETURN-RSN
                       MOVE 'PAYMENT STOPPED' TO WS-RESULT-TEXT
                       ADD 1               TO WS-ITEMS-STOPPED
                   END-IF
           END-EVALUATE

           IF WS-RETURN-RSN = SPACES
               IF CQI-CHEQUE-DATE NUMERIC
               AND CQI-CHEQUE-DATE > ZERO
                   COMPUTE WS-CHQ-DATE-INT =
                       FUNCTION INTEGER-OF-DATE (CQI-CHEQUE-DATE)
                   IF WS-BUS-DATE-INT - WS-CHQ-DATE-INT > WS-STALE-DAYS
                       MOVE 'OD'           TO WS-RETURN-RSN
                       MOVE 'OUT OF DATE'  TO WS-RESULT-TEXT
                   END-IF
               END-IF
           END-IF

           IF WS-RETURN-RSN = SPACES
               IF ACM-FROZEN OR ACM-DORMANT
               OR ACM-NO-DEBITS OR ACM-NO-POSTINGS
                   MOVE 'FR'               TO WS-RETURN-RSN
                   MOVE 'ACCOUNT RESTRICTED' TO WS-RESULT-TEXT
               END-IF
           END-IF

           IF WS-RETURN-RSN = SPACES
               EVALUATE TRUE
                   WHEN WS-AMOUNT <= WS-SPENDING-POWER - WS-RESERVED
                       CONTINUE
                   WHEN WS-AMOUNT <= WS-SPENDING-POWER - WS-RESERVED
                                   + WS-BUFFER
                       MOVE 'Y'            TO WS-PAID-ITEM-IND
                   WHEN OTHER
                       MOVE 'RD'           TO WS-RETURN-RSN
                       MOVE 'REFER TO DRAWER' TO WS-RESULT-TEXT
               END-EVALUATE
           END-IF

           IF WS-RETURN-RSN = SPACES
               PERFORM 2300-PAY-ITEM
           ELSE
               PERFORM 2400-RETURN-ITEM
           END-IF
           .
      *================================================================*
       2200-CHECK-STOPS.
      *================================================================*
      *    STOPS ARE KEYED BY ACCOUNT + FIRST SERIAL. BROWSE FROM THE
      *    START OF THE ACCOUNT AND CHECK EACH RANGE.
           MOVE 'N'                        TO WS-STOP-HIT-IND
                                              WS-END-STOPS-IND
           MOVE CQI-SORT-CODE              TO CQS-SORT-CODE
           MOVE CQI-ACCT-NO                TO CQS-ACCT-NO
           MOVE LOW-VALUES                 TO CQS-SERIAL-FROM
           MOVE CQS-KEY                    TO STOPS-KEY
           START STOPS-FILE KEY NOT < STOPS-KEY
           IF NOT WS-STOPS-OK
               GO TO 2200-EXIT
           END-IF
           PERFORM UNTIL WS-END-STOPS OR WS-STOP-HIT
               READ STOPS-FILE NEXT RECORD INTO CQS-STOP-RECORD
               EVALUATE TRUE
                   WHEN NOT WS-STOPS-OK
                   WHEN CQS-SORT-CODE NOT = CQI-SORT-CODE
                   WHEN CQS-ACCT-NO NOT = CQI-ACCT-NO
                       SET WS-END-STOPS    TO TRUE
                   WHEN NOT CQS-ACTIVE
                       CONTINUE
                   WHEN CQI-SERIAL-NO < CQS-SERIAL-FROM
                   WHEN CQI-SERIAL-NO > CQS-SERIAL-TO
                       CONTINUE
                   WHEN CQS-AMOUNT > ZERO
                   AND  CQS-AMOUNT NOT = WS-AMOUNT
                       CONTINUE
                   WHEN OTHER
                       SET WS-STOP-HIT     TO TRUE
      *                A SINGLE-CHEQUE STOP IS USED UP; A RANGE STAYS.
                       IF CQS-SERIAL-FROM = CQS-SERIAL-TO
                           SET CQS-HIT     TO TRUE
                           MOVE WS-BUS-DATE TO CQS-HIT-DATE
                           REWRITE STOPS-RECORD FROM CQS-STOP-RECORD
                       END-IF
               END-EVALUATE
           END-PERFORM
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-PAY-ITEM.
      *================================================================*
           ADD WS-AMOUNT                   TO WS-RESERVED
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE CQI-SORT-CODE              TO PST-SORT-CODE
           MOVE CQI-ACCT-NO                TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'CHQ '                     TO PST-TXN-CODE
           SET PST-DEBIT                   TO TRUE
           MOVE WS-AMOUNT                  TO PST-AMOUNT
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           STRING 'CHEQUE ' CQI-SERIAL-NO
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-1
           END-STRING
           MOVE CQI-PAYEE-NAME             TO PST-NARRATIVE-2
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           MOVE CQI-ITEM-REF               TO PST-EXT-REF
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE CHQPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-CHQPOST-STATUS NOT = '00'
               MOVE '2300-PAY-ITEM'        TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHQPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CHQPOST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           IF WS-PAID-ITEM-IND = 'Y'
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               IF ACM-PAID-ITEMS NOT NUMERIC
                   MOVE ZERO               TO ACM-PAID-ITEMS
               END-IF
               IF ACM-PAID-ITEMS < 999
                   ADD 1                   TO ACM-PAID-ITEMS
               END-IF
               MOVE ACM-ACCOUNT-RECORD     TO WS-SAVED-ACCOUNT
               SET WS-PAYER-CHANGED        TO TRUE
               ADD 1                       TO WS-PAID-ITEMS-BUFFER
               MOVE 'PAID INTO BUFFER'     TO WS-RESULT-TEXT
           END-IF
           ADD 1                           TO WS-ITEMS-PAID
           ADD WS-AMOUNT                   TO WS-TOTAL-PAID
           MOVE 'PAID'                     TO DL-RESULT
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2400-RETURN-ITEM.
      *================================================================*
           MOVE SPACES                     TO CQR-RETURN-ITEM
           MOVE CQI-SORT-CODE              TO CQR-SORT-CODE
           MOVE CQI-ACCT-NO                TO CQR-ACCT-NO
           MOVE CQI-SERIAL-NO              TO CQR-SERIAL-NO
           MOVE CQI-AMOUNT                 TO CQR-AMOUNT
           MOVE CQI-PRESENTING-BANK        TO CQR-PRESENTING-BANK
           MOVE CQI-ITEM-REF               TO CQR-ITEM-REF
           MOVE WS-RETURN-RSN              TO CQR-RETURN-RSN
           MOVE WS-BUS-DATE                TO CQR-RETURN-DATE
           WRITE RETURNS-RECORD            FROM CQR-RETURN-ITEM
           IF WS-RETURNS-STATUS NOT = '00'
               MOVE '2400-RETURN-ITEM'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'RETURNS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-RETURNS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

      *    REFER TO DRAWER AND RESTRICTED: UNPAID ITEM FEE AND LETTER.
           IF (WS-RETURN-RSN = 'RD' OR 'FR')
           AND WS-PAYER-FOUND
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               IF ACM-UNPAID-ITEMS NOT NUMERIC
                   MOVE ZERO               TO ACM-UNPAID-ITEMS
               END-IF
               IF ACM-UNPAID-ITEMS < 999
                   ADD 1                   TO ACM-UNPAID-ITEMS
               END-IF
               MOVE ACM-ACCOUNT-RECORD     TO WS-SAVED-ACCOUNT
               SET WS-PAYER-CHANGED        TO TRUE
               MOVE SPACES                 TO WS-LETTER
               MOVE CQI-SORT-CODE          TO LTR-SORT-CODE
               MOVE CQI-ACCT-NO            TO LTR-ACCT-NO
               MOVE ACM-CIF-NO             TO LTR-CIF-NO
               MOVE CQI-SERIAL-NO          TO LTR-SERIAL-NO
               MOVE CQI-AMOUNT             TO LTR-AMOUNT
               MOVE CQI-PAYEE-NAME         TO LTR-PAYEE
               MOVE WS-RETURN-RSN          TO LTR-REASON
               MOVE WS-RESULT-TEXT         TO LTR-REASON-TEXT
               MOVE WS-UNPAID-FEE          TO LTR-FEE
               WRITE LETTERS-RECORD        FROM WS-LETTER
           END-IF

           ADD 1                           TO WS-ITEMS-RETURNED
           ADD WS-AMOUNT                   TO WS-TOTAL-RETURNED
           MOVE 'RETURNED'                 TO DL-RESULT
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2900-END-PAYER.
      *================================================================*
           IF WS-PAYER-CHANGED
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '2900-END-PAYER'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               END-IF
               MOVE 'N'                    TO WS-PAYER-CHANGED-IND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF NOT WS-TRAILER-SEEN
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'INWARD CLEARING FILE HAS NO TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'CHEQUES PRESENTED'        TO TL-LABEL
           MOVE WS-ITEMS-READ              TO TL-COUNT
           MOVE WS-TOTAL-IN                TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PAID'                     TO TL-LABEL
           MOVE WS-ITEMS-PAID              TO TL-COUNT
           MOVE WS-TOTAL-PAID              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH INTO BUFFER'   TO TL-LABEL
           MOVE WS-PAID-ITEMS-BUFFER       TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RETURNED UNPAID'          TO TL-LABEL
           MOVE WS-ITEMS-RETURNED          TO TL-COUNT
           MOVE WS-TOTAL-RETURNED          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH STOPPED'       TO TL-LABEL
           MOVE WS-ITEMS-STOPPED           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE INWARD-FILE STOPS-FILE PRDPARM-FILE CIFMAST-FILE
                 ACCTMAST-FILE CHQPOST-FILE RETURNS-FILE LETTERS-FILE
                 REPORT-FILE

           DISPLAY 'CQCLR010 - PRESENTED ' WS-ITEMS-READ
           DISPLAY 'CQCLR010 - PAID      ' WS-ITEMS-PAID
           DISPLAY 'CQCLR010 - RETURNED  ' WS-ITEMS-RETURNED
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
           MOVE CQI-SORT-CODE              TO DL-SORT-CODE
           MOVE CQI-ACCT-NO                TO DL-ACCT-NO
           MOVE CQI-SERIAL-NO              TO DL-SERIAL
           MOVE CQI-AMOUNT                 TO DL-AMOUNT
           MOVE CQI-CHEQUE-DATE            TO DL-CHQ-DATE
           MOVE CQI-PRESENTING-BANK        TO DL-PRES-BANK
           MOVE WS-RETURN-RSN              TO DL-RSN
           MOVE WS-RESULT-TEXT             TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-ITEM.
      *================================================================*
           READ INWARD-FILE INTO CQI-INWARD-ITEM
           EVALUATE TRUE
               WHEN WS-INWARD-OK
                   IF CQI-ITEM
                       ADD 1               TO WS-ITEMS-READ
                       ADD CQI-AMOUNT      TO WS-TOTAL-IN
                   END-IF
               WHEN WS-INWARD-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-ITEM'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'INWARD READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-INWARD-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
