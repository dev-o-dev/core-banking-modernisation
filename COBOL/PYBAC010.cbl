       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PYBAC010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2008-09-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PYBAC010                                            *
      * TITLE    : BACS OUTBOUND - BUILD STANDARD 18 SUBMISSION        *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : PYBAC010                                            *
      * JCL      : PYBACS01 STEP PB010                                 *
      *----------------------------------------------------------------*
      * INPUT    : MBK.PAYINSTR - BACS PAYMENTS AT STATUS 'VAL' WHOSE  *
      *                           VALUE DATE IS ON OR BEFORE THE ENTRY *
      *                           DATE OF THIS SUBMISSION              *
      *            DATECARD     - BUSINESS DATE CONTROL CARD           *
      * OUTPUT   : BACSOUT      - STANDARD 18 FILE (BACS18RC)          *
      * UPDATE   : MBK.PAYINSTR - STATUS 'SNT', OUT_FILE_ID, SENT_TS   *
      *----------------------------------------------------------------*
      * BACS 3-DAY CYCLE                                               *
      *   DAY 1  INPUT DAY      - THIS JOB RUNS (BUSINESS DATE)        *
      *   DAY 2  PROCESSING DAY - UHL1 PROCESSING DATE                 *
      *   DAY 3  ENTRY DAY      - FUNDS REACH THE BENEFICIARY          *
      * THE CUSTOMER WAS DEBITED WHEN THE PAYMENT WAS TAKEN (PYINIT1). *
      * A SINGLE CONTRA DEBITS THE BANK'S BACS SETTLEMENT ACCOUNT FOR  *
      * THE FILE TOTAL.                                                *
      *                                                                *
      * THE ORIGINATOR NAME IS NOT HELD ON PAYINSTR, SO EVERY PAYMENT  *
      * GOES OUT AS "MEGABANK PLC" WITH THE CUSTOMER'S REFERENCE -     *
      * BENEFICIARIES SEE THE BANK'S NAME, NOT THE PAYER'S (PIR        *
      * 2009/078 - OPEN).                                              *
      *----------------------------------------------------------------*
      * RESTART: SINGLE UNIT OF WORK - ON ABEND DB2 ROLLS BACK AND THE *
      * JOB IS RERUN FROM THE TOP (THE OUTPUT GDG IS NOT CATALOGUED).  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2008-09-15 AOS  ORIGINAL VERSION                               *
      * 2009-03-02 AOS  LATE PAYMENTS (VALUE DATE MISSED) INCLUDED     *
      *                 WITH A WARNING INSTEAD OF BEING LEFT BEHIND    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BACSOUT-FILE     ASSIGN TO BACSOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-BACSOUT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  BACSOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BACSOUT-RECORD                  PIC X(100).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PYBAC010'.
      *
       01  WS-CONSTANTS.
           05  WS-SUN                      PIC X(06) VALUE '412345'.
           05  WS-SETTLE-SORT-CODE         PIC X(06) VALUE '401234'.
           05  WS-SETTLE-ACCT-NO           PIC X(08) VALUE '90000001'.
           05  WS-BANK-NAME                PIC X(18)
                                           VALUE 'MEGABANK PLC'.
      *
       01  WS-FILE-STATUSES.
           05  WS-BACSOUT-STATUS           PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-PAYMENTS          VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-ENTRY-DATE               PIC 9(08).
           05  WS-ENTRY-DATE-DB2           PIC X(10).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-CREATE-JULIAN            PIC X(06).
           05  WS-PROCESS-JULIAN           PIC X(06).
           05  WS-OUT-FILE-ID.
               10  FILLER                  PIC X(02) VALUE 'BS'.
               10  WS-OFI-YYMMDD           PIC X(06).
           05  WS-SUBMISSION-SERIAL        PIC X(06).
           05  WS-PENCE                    PIC 9(11).
           05  WS-SENT-TS                  PIC X(26).
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURR-DATE            PIC 9(08).
               10  WS-CURR-TIME            PIC 9(06).
               10  FILLER                  PIC X(07).
      *
       01  WS-COUNTERS.
           05  WS-CREDIT-COUNT             PIC 9(07) VALUE 0.
           05  WS-LATE-COUNT               PIC 9(07) VALUE 0.
           05  WS-CREDIT-TOTAL-PENCE       PIC 9(13) VALUE 0.
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLPAYI END-EXEC.
      *
           EXEC SQL DECLARE BACSCSR CURSOR WITH HOLD FOR
               SELECT PAY_ID, BENE_NAME, BENE_SORT_CODE, BENE_ACCT_NO,
                      PAY_AMT, REMIT_REF, VALUE_DATE
                 FROM MBK.PAYINSTR
                WHERE PAY_SCHEME = 'BACS'
                  AND PAY_STATUS = 'VAL'
                  AND VALUE_DATE <= :WS-ENTRY-DATE-DB2
                ORDER BY BENE_SORT_CODE, BENE_ACCT_NO, PAY_ID
                FOR UPDATE OF PAY_STATUS, OUT_FILE_ID, SENT_TS,
                              STATUS_TS
           END-EXEC.
      *
           COPY BACS18RC.
      *
           COPY DTEPARM.
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
           PERFORM 2000-WRITE-HEADER-LABELS
           PERFORM 3000-PROCESS-PAYMENT
               UNTIL WS-END-OF-PAYMENTS
           PERFORM 4000-WRITE-CONTRA
           PERFORM 5000-WRITE-TRAILER-LABELS
           PERFORM 6000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  DATECARD-FILE
                OUTPUT BACSOUT-FILE

           IF WS-BACSOUT-STATUS NOT = '00'
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
           OR DTC-NEXT-BUS-DATE NOT NUMERIC
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE

           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA

      *    ENTRY DATE (DAY 3) = BUSINESS DATE + 2 BUSINESS DAYS
           MOVE 'A'                        TO DTE-FUNCTION
           MOVE DTC-BUSINESS-DATE          TO DTE-DATE-IN
           MOVE +2                         TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           IF NOT DTE-OK
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'DTEUTIL FAILED CALCULATING ENTRY DATE'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           MOVE DTE-DATE-OUT               TO WS-ENTRY-DATE
           MOVE WS-ENTRY-DATE              TO WS-DATE-WORK
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-ENTRY-DATE-DB2
           END-STRING

      *    JULIAN DATES FOR THE LABELS
           MOVE 'J'                        TO DTE-FUNCTION
           MOVE DTC-BUSINESS-DATE          TO DTE-DATE-IN
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-JULIAN                 TO WS-CREATE-JULIAN
           MOVE DTC-NEXT-BUS-DATE          TO DTE-DATE-IN
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-JULIAN                 TO WS-PROCESS-JULIAN

           MOVE DTC-BUSINESS-DATE          TO WS-DATE-WORK
           MOVE WS-DATE-WORK-X (3:6)       TO WS-OFI-YYMMDD
           MOVE WS-DATE-WORK-X (3:6)       TO WS-SUBMISSION-SERIAL

           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD '-'
                  WS-CURR-TIME (1:2) '.' WS-CURR-TIME (3:2) '.'
                  WS-CURR-TIME (5:2) '.000000'
                  DELIMITED BY SIZE
                  INTO WS-SENT-TS
           END-STRING

           DISPLAY 'PYBAC010 - INPUT DAY  ' DTC-BUSINESS-DATE
                   ' ENTRY DAY ' WS-ENTRY-DATE
                   ' FILE ' WS-OUT-FILE-ID

           EXEC SQL OPEN BACSCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           PERFORM 8000-FETCH-PAYMENT
           .
      *================================================================*
       2000-WRITE-HEADER-LABELS.
      *================================================================*
      *---- VOL1 ------------------------------------------------------*
           MOVE SPACES                     TO B18-LABEL-RECORD
           MOVE 'VOL1'                     TO B18-LABEL-ID
           MOVE WS-SUBMISSION-SERIAL       TO B18V-SERIAL-NO
           MOVE '0'                        TO B18V-ACCESSIBILITY
           MOVE SPACES                     TO B18V-OWNER-ID
           MOVE WS-SUN                     TO B18V-OWNER-ID (5:6)
           MOVE '1'                        TO B18V-LABEL-STD
           PERFORM 8100-WRITE-LABEL

      *---- HDR1 ------------------------------------------------------*
           MOVE SPACES                     TO B18-LABEL-RECORD
           MOVE 'HDR1'                     TO B18-LABEL-ID
           STRING 'A' WS-SUN 'S  ' WS-SUN
                  DELIMITED BY SIZE
                  INTO B18H1-FILE-ID
           END-STRING
           MOVE WS-SUBMISSION-SERIAL       TO B18H1-SET-ID
           MOVE '0001'                     TO B18H1-FILE-SECTION
           MOVE '0001'                     TO B18H1-FILE-SEQ
           MOVE WS-CREATE-JULIAN           TO B18H1-CREATE-DATE
           MOVE WS-CREATE-JULIAN           TO B18H1-EXPIRY-DATE
           MOVE ' '                        TO B18H1-ACCESSIBILITY
           MOVE '000000'                   TO B18H1-BLOCK-COUNT
           PERFORM 8100-WRITE-LABEL

      *---- HDR2 ------------------------------------------------------*
           MOVE SPACES                     TO B18-LABEL-RECORD
           MOVE 'HDR2'                     TO B18-LABEL-ID
           MOVE 'F'                        TO B18H2-RECORD-FORMAT
           MOVE '02000'                    TO B18H2-BLOCK-LENGTH
           MOVE '00100'                    TO B18H2-RECORD-LENGTH
           MOVE '00'                       TO B18H2-BUFFER-OFFSET
           PERFORM 8100-WRITE-LABEL

      *---- UHL1 ------------------------------------------------------*
           MOVE SPACES                     TO B18-LABEL-RECORD
           MOVE 'UHL1'                     TO B18-LABEL-ID
           MOVE WS-PROCESS-JULIAN          TO B18U-PROCESS-DATE
           MOVE '999999'                   TO B18U-RECEIVING-ID
           MOVE '00'                       TO B18U-CURRENCY-CD
           MOVE '000000'                   TO B18U-COUNTRY-CD
           MOVE '1 DAILY  '                TO B18U-WORK-CODE
           MOVE '001'                      TO B18U-FILE-NO
           PERFORM 8100-WRITE-LABEL
           .
      *================================================================*
       3000-PROCESS-PAYMENT.
      *================================================================*
           IF PYI-VALUE-DATE < WS-ENTRY-DATE-DB2
               ADD 1                       TO WS-LATE-COUNT
               DISPLAY 'PYBAC010 W001 LATE PAYMENT ' PYI-PAY-ID
                       ' VALUE DATE ' PYI-VALUE-DATE
           END-IF

           COMPUTE WS-PENCE = PYI-PAY-AMT * 100

           MOVE SPACES                     TO B18-DATA-RECORD
           MOVE PYI-BENE-SORT-CODE         TO B18D-DEST-SORT-CODE
           MOVE PYI-BENE-ACCT-NO           TO B18D-DEST-ACCT-NO
           MOVE '0'                        TO B18D-DEST-ACCT-TYPE
           SET B18D-CREDIT                 TO TRUE
           MOVE WS-SETTLE-SORT-CODE        TO B18D-ORIG-SORT-CODE
           MOVE WS-SETTLE-ACCT-NO          TO B18D-ORIG-ACCT-NO
           MOVE WS-PENCE                   TO B18D-AMOUNT-PENCE
           MOVE WS-BANK-NAME               TO B18D-ORIG-NAME
           MOVE PYI-REMIT-REF              TO B18D-REFERENCE
           MOVE FUNCTION UPPER-CASE (PYI-BENE-NAME-TEXT (1:18))
                                           TO B18D-DEST-ACCT-NAME
           WRITE BACSOUT-RECORD            FROM B18-DATA-RECORD
           PERFORM 8200-CHECK-WRITE

           ADD 1                           TO WS-CREDIT-COUNT
           ADD WS-PENCE                    TO WS-CREDIT-TOTAL-PENCE

           EXEC SQL
               UPDATE MBK.PAYINSTR
                  SET PAY_STATUS  = 'SNT',
                      OUT_FILE_ID = :WS-OUT-FILE-ID,
                      SENT_TS     = :WS-SENT-TS,
                      STATUS_TS   = CURRENT TIMESTAMP
                WHERE CURRENT OF BACSCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '3000-PROCESS-PAYMENT' TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           PERFORM 8000-FETCH-PAYMENT
           .
      *================================================================*
       4000-WRITE-CONTRA.
      *================================================================*
           IF WS-CREDIT-COUNT > ZERO
               MOVE SPACES                 TO B18-DATA-RECORD
               MOVE WS-SETTLE-SORT-CODE    TO B18D-DEST-SORT-CODE
               MOVE WS-SETTLE-ACCT-NO      TO B18D-DEST-ACCT-NO
               MOVE '0'                    TO B18D-DEST-ACCT-TYPE
               SET B18D-DEBIT-CONTRA       TO TRUE
               MOVE WS-SETTLE-SORT-CODE    TO B18D-ORIG-SORT-CODE
               MOVE WS-SETTLE-ACCT-NO      TO B18D-ORIG-ACCT-NO
               MOVE WS-CREDIT-TOTAL-PENCE  TO B18D-AMOUNT-PENCE
               MOVE 'MBK CUST PAYMENTS'    TO B18D-ORIG-NAME
               MOVE 'CONTRA'               TO B18D-REFERENCE
               MOVE WS-BANK-NAME           TO B18D-DEST-ACCT-NAME
               WRITE BACSOUT-RECORD        FROM B18-DATA-RECORD
               PERFORM 8200-CHECK-WRITE
           END-IF
           .
      *================================================================*
       5000-WRITE-TRAILER-LABELS.
      *================================================================*
      *---- EOF1 / EOF2 REPEAT HDR1 / HDR2 ----------------------------*
           MOVE SPACES                     TO B18-LABEL-RECORD
           MOVE 'EOF1'                     TO B18-LABEL-ID
           STRING 'A' WS-SUN 'S  ' WS-SUN
                  DELIMITED BY SIZE
                  INTO B18H1-FILE-ID
           END-STRING
           MOVE WS-SUBMISSION-SERIAL       TO B18H1-SET-ID
           MOVE '0001'                     TO B18H1-FILE-SECTION
           MOVE '0001'                     TO B18H1-FILE-SEQ
           MOVE WS-CREATE-JULIAN           TO B18H1-CREATE-DATE
           MOVE WS-CREATE-JULIAN           TO B18H1-EXPIRY-DATE
           MOVE '000000'                   TO B18H1-BLOCK-COUNT
           PERFORM 8100-WRITE-LABEL

           MOVE SPACES                     TO B18-LABEL-RECORD
           MOVE 'EOF2'                     TO B18-LABEL-ID
           MOVE 'F'                        TO B18H2-RECORD-FORMAT
           MOVE '02000'                    TO B18H2-BLOCK-LENGTH
           MOVE '00100'                    TO B18H2-RECORD-LENGTH
           MOVE '00'                       TO B18H2-BUFFER-OFFSET
           PERFORM 8100-WRITE-LABEL

      *---- UTL1 - VALUE AND COUNT TOTALS -----------------------------*
           MOVE SPACES                     TO B18-LABEL-RECORD
           MOVE 'UTL1'                     TO B18-LABEL-ID
           MOVE WS-CREDIT-TOTAL-PENCE      TO B18T-DEBIT-TOTAL
           MOVE WS-CREDIT-TOTAL-PENCE      TO B18T-CREDIT-TOTAL
           IF WS-CREDIT-COUNT > ZERO
               MOVE 1                      TO B18T-DEBIT-COUNT
           ELSE
               MOVE ZERO                   TO B18T-DEBIT-COUNT
           END-IF
           MOVE WS-CREDIT-COUNT            TO B18T-CREDIT-COUNT
           PERFORM 8100-WRITE-LABEL
           .
      *================================================================*
       6000-FINALISE.
      *================================================================*
           EXEC SQL CLOSE BACSCSR END-EXEC
           EXEC SQL COMMIT END-EXEC

           CLOSE BACSOUT-FILE

           DISPLAY 'PYBAC010 - CREDITS WRITTEN   ' WS-CREDIT-COUNT
           DISPLAY 'PYBAC010 - TOTAL (PENCE)     ' WS-CREDIT-TOTAL-PENCE
           DISPLAY 'PYBAC010 - LATE PAYMENTS     ' WS-LATE-COUNT

      *    RC 4 = NOTHING TO SEND. THE TRANSMISSION STEP IS BYPASSED.
           IF WS-CREDIT-COUNT = ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       8000-FETCH-PAYMENT.
      *================================================================*
           EXEC SQL
               FETCH BACSCSR
                INTO :PYI-PAY-ID, :PYI-BENE-NAME,
                     :PYI-BENE-SORT-CODE, :PYI-BENE-ACCT-NO,
                     :PYI-PAY-AMT, :PYI-REMIT-REF, :PYI-VALUE-DATE
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   SET WS-END-OF-PAYMENTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-FETCH-PAYMENT' TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       8100-WRITE-LABEL.
      *================================================================*
           WRITE BACSOUT-RECORD            FROM B18-LABEL-RECORD
           PERFORM 8200-CHECK-WRITE
           .
      *================================================================*
       8200-CHECK-WRITE.
      *================================================================*
           IF WS-BACSOUT-STATUS NOT = '00'
               MOVE '8200-CHECK-WRITE'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'BACSOUT WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-BACSOUT-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
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
