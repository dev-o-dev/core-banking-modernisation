       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CQUNC010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2007-11-19.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CQUNC010                                            *
      * TITLE    : CHEQUES - DEPOSITED CHEQUE CLEARANCE (2-4-6)        *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CQCLR01 STEP CQ040                                  *
      *----------------------------------------------------------------*
      * INPUT    : RETIN    - CHEQUES PAID IN AT MEGABANK THAT THE     *
      *                       DRAWEE BANK RETURNED UNPAID (CHQRECS,    *
      *                       CQR- LAYOUT) - MAY BE EMPTY              *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : UNCLEAR  - UNCLEARED DEPOSITS (VSAM KSDS, DYNAMIC)  *
      *            ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM)   *
      * OUTPUT   : UNCPOST  - DEBITS FOR RETURNED CHEQUES - PSTXNREC   *
      *            LETTERS  - RETURNED CHEQUE LETTERS (FB 150)         *
      *            REPORT   - CLEARANCE REPORT (FBA 133)               *
      *----------------------------------------------------------------*
      * THE TELLER SYSTEM CREDITS A DEPOSITED CHEQUE TO THE LEDGER AT  *
      * END OF DAY (TXN 'CHD ') BUT HOLDS THE AMOUNT AS UNCLEARED      *
      * (ACM-UNCLEARED-AMT, NOT IN ACM-AVAIL-BAL). UNDER THE OFT 2-4-6 *
      * TIMESCALES (2007) THE BANK MUST:                               *
      *   DAY 2  START PAYING CREDIT INTEREST (NOT MODELLED - INTEREST *
      *          IS ON THE LEDGER BALANCE FROM DAY 0, WHICH IS MORE    *
      *          GENEROUS)                                             *
      *   DAY 4  ALLOW WITHDRAWAL - THIS PROGRAM MOVES THE AMOUNT FROM *
      *          UNCLEARED TO AVAILABLE ON CQD-WITHDRAW-DATE           *
      *   DAY 6  CERTAINTY OF FATE - AFTER CQD-CERTAINTY-DATE THE      *
      *          CHEQUE CANNOT BE TAKEN BACK EVEN IF RETURNED (FRAUD   *
      *          EXCEPTED)                                             *
      *                                                                *
      * RETURNED CHEQUES (RETIN) BEFORE THE CERTAINTY DATE ARE DEBITED *
      * BACK ('UNP ' HARD POSTING). IF THE AMOUNT WAS STILL UNCLEARED  *
      * THE UNCLEARED TOTAL IS REDUCED; IF IT HAD ALREADY BEEN RELEASED*
      * THE DEBIT MAY TAKE THE ACCOUNT OVERDRAWN. RETURNS AFTER THE    *
      * CERTAINTY DATE ARE REPORTED FOR THE FRAUD TEAM ONLY.           *
      *                                                                *
      * CLEARED RECORDS ARE DELETED 30 DAYS AFTER THE CERTAINTY DATE.  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2007-11-19 JNB  ORIGINAL VERSION (2-4-6 GO-LIVE NOV 2007)      *
      * 2010-08-02 JNB  'UNP ' TXN CODE (WAS 'CHG ' WITH NARRATIVE)    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RETIN-FILE       ASSIGN TO RETIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RETIN-STATUS.
           SELECT UNCLEAR-FILE     ASSIGN TO UNCLEAR
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS UNC-KEY
                                   FILE STATUS IS WS-UNCLEAR-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT UNCPOST-FILE     ASSIGN TO UNCPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-UNCPOST-STATUS.
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
       FD  RETIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RETIN-RECORD                    PIC X(150).
       FD  UNCLEAR-FILE.
       01  UNCLEAR-RECORD.
           05  UNC-KEY                     PIC X(28).
           05  FILLER                      PIC X(92).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  UNCPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  UNCPOST-RECORD                  PIC X(200).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CQUNC010'.
      *
       01  WS-CONSTANTS.
           05  WS-PURGE-DAYS               PIC S9(05) COMP VALUE +30.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00040000.
      *
       01  WS-FILE-STATUSES.
           05  WS-RETIN-STATUS             PIC X(02).
               88  WS-RETIN-OK                 VALUE '00'.
               88  WS-RETIN-EOF                VALUE '10'.
           05  WS-UNCLEAR-STATUS           PIC X(02).
               88  WS-UNCLEAR-OK               VALUE '00'.
               88  WS-UNCLEAR-EOF              VALUE '10'.
               88  WS-UNCLEAR-NOTFND           VALUE '23'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-UNCPOST-STATUS           PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-RET-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-END-OF-RETURNS           VALUE 'Y'.
           05  WS-UNC-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-END-OF-UNCLEARED         VALUE 'Y'.
           05  WS-FOUND-IND                PIC X(01).
               88  WS-DEPOSIT-FOUND            VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-CERT-INT                 PIC S9(09) COMP.
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-ACTION                   PIC X(10).
           05  WS-NOTE                     PIC X(40).
      *
       01  WS-LETTER.
           05  LTR-SORT-CODE               PIC X(06).
           05  LTR-ACCT-NO                 PIC X(08).
           05  LTR-CIF-NO                  PIC X(12).
           05  LTR-DEPOSIT-DATE            PIC 9(08).
           05  LTR-AMOUNT                  PIC 9(11)V99.
           05  LTR-DRAWN-ON                PIC X(06).
           05  LTR-REASON                  PIC X(02).
           05  LTR-OVERDRAWN-IND           PIC X(01).
           05  FILLER                      PIC X(94).
      *
       01  WS-COUNTERS.
           05  WS-UNC-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-RELEASED                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-CLEARED                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-PURGED                   PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETURNS-READ             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETURNS-DEBITED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETURNS-LATE             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETURNS-UNMATCHED        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RELEASED-AMT             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-RETURNED-AMT             PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CQUNC010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - DEPOSITED CHEQUE CLEARANCE'.
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
           05  FILLER            PIC X(10) VALUE 'DEPOSITED'.
           05  FILLER            PIC X(17) VALUE '          AMOUNT'.
           05  FILLER            PIC X(10) VALUE 'WITHDRAW'.
           05  FILLER            PIC X(10) VALUE 'CERTAIN'.
           05  FILLER            PIC X(11) VALUE 'ACTION'.
           05  FILLER            PIC X(55) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DEPOSIT-DATE             PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-WITHDRAW                 PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CERTAIN                  PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
           05  FILLER                      PIC X(13) VALUE SPACES.
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
           PERFORM 2000-PROCESS-RETURNS
           PERFORM 3000-RELEASE-AND-PURGE THRU 3000-EXIT
           PERFORM 4000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  RETIN-FILE
                       DATECARD-FILE
                I-O    UNCLEAR-FILE
                       ACCTMAST-FILE
                OUTPUT UNCPOST-FILE
                       LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-RETIN-OK
           OR NOT WS-UNCLEAR-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-UNCPOST-STATUS NOT = '00'
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
           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       2000-PROCESS-RETURNS.
      *================================================================*
      *    THE RETURN CARRIES THE CREDITED ACCOUNT, THE AMOUNT AND THE
      *    DEPOSIT REFERENCE (CQR-ITEM-REF = DEPOSIT DATE + SEQUENCE
      *    AS WRITTEN BY THE TELLER ON THE CHEQUE ENCODING).
           READ RETIN-FILE INTO CQR-RETURN-ITEM
           PERFORM UNTIL NOT WS-RETIN-OK
               ADD 1                       TO WS-RETURNS-READ
               PERFORM 2100-MATCH-DEPOSIT THRU 2100-EXIT
               READ RETIN-FILE INTO CQR-RETURN-ITEM
           END-PERFORM
           .
      *================================================================*
       2100-MATCH-DEPOSIT.
      *================================================================*
           MOVE 'N'                        TO WS-FOUND-IND
           MOVE CQR-SORT-CODE              TO CQD-SORT-CODE
           MOVE CQR-ACCT-NO                TO CQD-ACCT-NO
           MOVE CQR-ITEM-REF (1:8)         TO CQD-DEPOSIT-DATE
           MOVE CQR-ITEM-REF (9:6)         TO CQD-SEQ
           MOVE CQD-KEY                    TO UNC-KEY
           READ UNCLEAR-FILE INTO CQD-DEPOSIT-RECORD
           EVALUATE TRUE
               WHEN WS-UNCLEAR-OK
                   SET WS-DEPOSIT-FOUND    TO TRUE
               WHEN WS-UNCLEAR-NOTFND
                   ADD 1                   TO WS-RETURNS-UNMATCHED
                   MOVE 'UNMATCHED'        TO WS-ACTION
                   MOVE 'NO DEPOSIT RECORD - REFER TO CLEARING OPS'
                                           TO WS-NOTE
                   PERFORM 7100-RETURN-DETAIL
                   GO TO 2100-EXIT
               WHEN OTHER
                   MOVE '2100-MATCH-DEPOSIT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'UNCLEAR READ FAILED' TO ABN-MESSAGE
                   MOVE WS-UNCLEAR-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE

           IF CQD-RETURNED
               MOVE 'DUPLICATE'            TO WS-ACTION
               MOVE 'DEPOSIT ALREADY RETURNED' TO WS-NOTE
               PERFORM 7200-DEPOSIT-DETAIL
               GO TO 2100-EXIT
           END-IF

           IF CQD-CERTAINTY-DATE < WS-BUS-DATE
               ADD 1                       TO WS-RETURNS-LATE
               MOVE 'LATE'                 TO WS-ACTION
               MOVE 'RETURNED AFTER CERTAINTY - REFER TO FRAUD'
                                           TO WS-NOTE
               PERFORM 7200-DEPOSIT-DETAIL
               GO TO 2100-EXIT
           END-IF

           PERFORM 2200-DEBIT-BACK THRU 2200-EXIT
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-DEBIT-BACK.
      *================================================================*
           MOVE CQD-SORT-CODE              TO ACM-SORT-CODE
           MOVE CQD-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF NOT WS-ACCTMAST-OK
               MOVE 'NO ACCOUNT'           TO WS-ACTION
               MOVE 'CREDITED ACCOUNT NOT FOUND' TO WS-NOTE
               PERFORM 7200-DEPOSIT-DETAIL
               GO TO 2200-EXIT
           END-IF

      *    IF STILL UNCLEARED THE AMOUNT NEVER REACHED AVAILABLE -
      *    REDUCE UNCLEARED AND RAISE AVAILABLE BY THE SAME SO THAT
      *    PSEOD010'S HARD DEBIT (WHICH LOWERS BOTH LEDGER AND
      *    AVAILABLE) LEAVES AVAILABLE UNCHANGED.
           IF CQD-UNCLEARED
               SUBTRACT CQD-AMOUNT         FROM ACM-UNCLEARED-AMT
               ADD CQD-AMOUNT              TO ACM-AVAIL-BAL
               IF ACM-UNCLEARED-AMT < ZERO
                   MOVE ZERO               TO ACM-UNCLEARED-AMT
               END-IF
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
           END-IF

           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE CQD-SORT-CODE              TO PST-SORT-CODE
           MOVE CQD-ACCT-NO                TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'UNP '                     TO PST-TXN-CODE
           SET PST-DEBIT                   TO TRUE
           MOVE CQD-AMOUNT                 TO PST-AMOUNT
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           MOVE 'RETURNED CHEQUE'          TO PST-NARRATIVE-1
           STRING 'PAID IN ' CQD-DEPOSIT-DATE ' REASON '
                  CQR-RETURN-RSN
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           STRING 'UNP' CQD-KEY (1:20)
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE UNCPOST-RECORD            FROM PST-POSTING-RECORD

           MOVE SPACES                     TO WS-LETTER
           MOVE CQD-SORT-CODE              TO LTR-SORT-CODE
           MOVE CQD-ACCT-NO                TO LTR-ACCT-NO
           MOVE ACM-CIF-NO                 TO LTR-CIF-NO
           MOVE CQD-DEPOSIT-DATE           TO LTR-DEPOSIT-DATE
           MOVE CQD-AMOUNT                 TO LTR-AMOUNT
           MOVE CQD-DRAWN-ON-SORT          TO LTR-DRAWN-ON
           MOVE CQR-RETURN-RSN             TO LTR-REASON
           IF ACM-AVAIL-BAL - CQD-AMOUNT < ZERO
               MOVE 'Y'                    TO LTR-OVERDRAWN-IND
           ELSE
               MOVE 'N'                    TO LTR-OVERDRAWN-IND
           END-IF
           WRITE LETTERS-RECORD            FROM WS-LETTER

           SET CQD-RETURNED                TO TRUE
           REWRITE UNCLEAR-RECORD          FROM CQD-DEPOSIT-RECORD
           IF NOT WS-UNCLEAR-OK
               MOVE '2200-DEBIT-BACK'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'UNCLEAR REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-UNCLEAR-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-RETURNS-DEBITED
           ADD CQD-AMOUNT                  TO WS-RETURNED-AMT
           MOVE 'DEBITED'                  TO WS-ACTION
           MOVE 'RETURNED CHEQUE DEBITED BACK' TO WS-NOTE
           PERFORM 7200-DEPOSIT-DETAIL
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       3000-RELEASE-AND-PURGE.
      *================================================================*
           MOVE LOW-VALUES                 TO UNC-KEY
           START UNCLEAR-FILE KEY NOT < UNC-KEY
           IF NOT WS-UNCLEAR-OK
               GO TO 3000-EXIT
           END-IF
           PERFORM UNTIL WS-END-OF-UNCLEARED
               READ UNCLEAR-FILE NEXT RECORD INTO CQD-DEPOSIT-RECORD
               EVALUATE TRUE
                   WHEN WS-UNCLEAR-EOF
                       SET WS-END-OF-UNCLEARED TO TRUE
                   WHEN NOT WS-UNCLEAR-OK
                       MOVE '3000-RELEASE-AND-PURGE' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'UNCLEAR READ FAILED' TO ABN-MESSAGE
                       MOVE WS-UNCLEAR-STATUS TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
                   WHEN OTHER
                       ADD 1               TO WS-UNC-READ
                       PERFORM 3100-PROCESS-DEPOSIT
               END-EVALUATE
           END-PERFORM
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-PROCESS-DEPOSIT.
      *================================================================*
           EVALUATE TRUE
               WHEN CQD-UNCLEARED
               AND  CQD-WITHDRAW-DATE <= WS-BUS-DATE
                   PERFORM 3200-RELEASE-FUNDS
               WHEN CQD-AVAILABLE
               AND  CQD-CERTAINTY-DATE < WS-BUS-DATE
                   SET CQD-CLEARED         TO TRUE
                   REWRITE UNCLEAR-RECORD  FROM CQD-DEPOSIT-RECORD
                   ADD 1                   TO WS-CLEARED
               WHEN CQD-CLEARED OR CQD-RETURNED
                   COMPUTE WS-CERT-INT =
                       FUNCTION INTEGER-OF-DATE (CQD-CERTAINTY-DATE)
                   IF WS-BUS-DATE-INT - WS-CERT-INT > WS-PURGE-DAYS
                       DELETE UNCLEAR-FILE RECORD
                       ADD 1               TO WS-PURGED
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       3200-RELEASE-FUNDS.
      *================================================================*
           MOVE CQD-SORT-CODE              TO ACM-SORT-CODE
           MOVE CQD-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF WS-ACCTMAST-OK
               IF CQD-AMOUNT > ACM-UNCLEARED-AMT
      *            THE TELLER TOTAL AND THE DEPOSIT RECORDS DISAGREE -
      *            RELEASE WHAT IS THERE AND REPORT.
                   ADD ACM-UNCLEARED-AMT   TO ACM-AVAIL-BAL
                   MOVE ZERO               TO ACM-UNCLEARED-AMT
                   MOVE 'UNCLEARED TOTAL SHORT - REFER DDA OPS'
                                           TO WS-NOTE
               ELSE
                   SUBTRACT CQD-AMOUNT     FROM ACM-UNCLEARED-AMT
                   ADD CQD-AMOUNT          TO ACM-AVAIL-BAL
                   MOVE SPACES             TO WS-NOTE
               END-IF
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '3200-RELEASE-FUNDS' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               END-IF
           ELSE
               MOVE 'ACCOUNT NOT FOUND - RECORD CLOSED' TO WS-NOTE
           END-IF
           SET CQD-AVAILABLE               TO TRUE
           REWRITE UNCLEAR-RECORD          FROM CQD-DEPOSIT-RECORD
           ADD 1                           TO WS-RELEASED
           ADD CQD-AMOUNT                  TO WS-RELEASED-AMT
           MOVE 'RELEASED'                 TO WS-ACTION
           PERFORM 7200-DEPOSIT-DETAIL
           .
      *================================================================*
       4000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'DEPOSIT RECORDS READ'     TO TL-LABEL
           MOVE WS-UNC-READ                TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RELEASED TO AVAILABLE (DAY 4)' TO TL-LABEL
           MOVE WS-RELEASED                TO TL-COUNT
           MOVE WS-RELEASED-AMT            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CERTAIN (DAY 6)'          TO TL-LABEL
           MOVE WS-CLEARED                 TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RETURNED CHEQUES RECEIVED' TO TL-LABEL
           MOVE WS-RETURNS-READ            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  DEBITED BACK'           TO TL-LABEL
           MOVE WS-RETURNS-DEBITED         TO TL-COUNT
           MOVE WS-RETURNED-AMT            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  AFTER CERTAINTY (FRAUD)' TO TL-LABEL
           MOVE WS-RETURNS-LATE            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  UNMATCHED'              TO TL-LABEL
           MOVE WS-RETURNS-UNMATCHED       TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PURGED'                   TO TL-LABEL
           MOVE WS-PURGED                  TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE RETIN-FILE UNCLEAR-FILE ACCTMAST-FILE UNCPOST-FILE
                 LETTERS-FILE REPORT-FILE

           DISPLAY 'CQUNC010 - RELEASED ' WS-RELEASED
           DISPLAY 'CQUNC010 - RETURNED ' WS-RETURNS-DEBITED
           IF WS-RETURNS-LATE > ZERO OR WS-RETURNS-UNMATCHED > ZERO
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
       7100-RETURN-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE CQR-SORT-CODE              TO DL-SORT-CODE
           MOVE CQR-ACCT-NO                TO DL-ACCT-NO
           MOVE ZERO                       TO DL-DEPOSIT-DATE
                                              DL-WITHDRAW DL-CERTAIN
           MOVE CQR-AMOUNT                 TO DL-AMOUNT
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-DEPOSIT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE CQD-SORT-CODE              TO DL-SORT-CODE
           MOVE CQD-ACCT-NO                TO DL-ACCT-NO
           MOVE CQD-DEPOSIT-DATE           TO DL-DEPOSIT-DATE
           MOVE CQD-AMOUNT                 TO DL-AMOUNT
           MOVE CQD-WITHDRAW-DATE          TO DL-WITHDRAW
           MOVE CQD-CERTAINTY-DATE         TO DL-CERTAIN
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *
           COPY ABNDPROC.
