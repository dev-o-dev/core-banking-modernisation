       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PYSWF010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2008-09-22.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PYSWF010                                            *
      * TITLE    : INTERNATIONAL PAYMENTS - BUILD SWIFT MT103 FILE     *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : PYSWF010                                            *
      * JCL      : PYSWFT01 STEP PS010                                 *
      *----------------------------------------------------------------*
      * INPUT    : MBK.PAYINSTR - SWIFT PAYMENTS AT STATUS 'VAL'       *
      *            ACCTMAST     - FOR THE PAYER'S IBAN                 *
      *            CIFMAST      - FOR THE PAYER'S NAME AND ADDRESS     *
      *                           (FIELD 50K - EC REG 1781/2006        *
      *                           COMPLETE PAYER INFORMATION)          *
      *            DATECARD     - BUSINESS DATE CONTROL CARD           *
      * OUTPUT   : SWIFTOUT     - MT103 MESSAGES IN SWIFT ALLIANCE     *
      *                           ACCESS RJE FORMAT, '$' BETWEEN       *
      *                           MESSAGES. FB LRECL=80, ONE FIELD     *
      *                           LINE PER RECORD. PICKED UP BY SAA    *
      *                           FILE TRANSFER INTERFACE (FTA).       *
      * UPDATE   : MBK.PAYINSTR - STATUS 'SNT', OUT_FILE_ID, SENT_TS   *
      *----------------------------------------------------------------*
      * RUNS 4 TIMES A DAY (10:00, 12:00, 14:00, 15:30) FROM CA-7 -    *
      * NOT PART OF THE EOD NETWORK.                                   *
      *                                                                *
      * ONLY FIELDS 20, 23B, 32A, 50K, 57A, 59, 70 AND 71A ARE         *
      * GENERATED. NO 33B/36 - THE FX CONVERSION IS DONE BY TREASURY   *
      * WHEN THE NOSTRO IS FUNDED, NOT IN THE MESSAGE.                 *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2008-09-22 AOS  ORIGINAL VERSION                               *
      * 2008-12-15 AOS  FULL PAYER NAME/ADDRESS IN 50K (AUDIT FINDING  *
      *                 IA-08-22, EC REG 1781/2006)                    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SWIFTOUT-FILE    ASSIGN TO SWIFTOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-SWIFTOUT-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SWIFTOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SWIFTOUT-RECORD                 PIC X(80).
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PYSWF010'.
      *
       01  WS-CONSTANTS.
           05  WS-OUR-LT-ADDRESS           PIC X(12)
                                           VALUE 'MEGAGB2LAXXX'.
      *
       01  WS-FILE-STATUSES.
           05  WS-SWIFTOUT-STATUS          PIC X(02).
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-PAYMENTS          VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-LINE                     PIC X(80).
           05  WS-RECEIVER-ADDRESS         PIC X(12).
           05  WS-AMOUNT-EDIT              PIC Z(12)9.99.
           05  WS-AMOUNT-TEXT              PIC X(16).
           05  WS-LEAD-SPACES              PIC S9(04) COMP.
           05  WS-SWIFT-VALUE-DATE         PIC X(06).
           05  WS-PAYER-NAME               PIC X(35).
           05  WS-TOWN-PCODE               PIC X(35).
           05  WS-OUT-FILE-ID.
               10  FILLER                  PIC X(02) VALUE 'SW'.
               10  WS-OFI-DDHHMM           PIC X(06).
           05  WS-SENT-TS                  PIC X(26).
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURR-DATE            PIC X(08).
               10  WS-CURR-TIME            PIC X(06).
               10  FILLER                  PIC X(07).
      *
       01  WS-COUNTERS.
           05  WS-MSG-COUNT                PIC 9(07) VALUE 0.
           05  WS-HELD-COUNT               PIC 9(07) VALUE 0.
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLPAYI END-EXEC.
      *
           EXEC SQL DECLARE SWFTCSR CURSOR WITH HOLD FOR
               SELECT PAY_ID, DR_SORT_CODE, DR_ACCT_NO, BENE_NAME,
                      BENE_ACCT_NO, BENE_IBAN, BENE_BIC, PAY_CCY_CD,
                      PAY_AMT, CHARGE_BEARER, REMIT_REF, VALUE_DATE
                 FROM MBK.PAYINSTR
                WHERE PAY_SCHEME = 'SWFT'
                  AND PAY_STATUS = 'VAL'
                ORDER BY PAY_ID
                FOR UPDATE OF PAY_STATUS, OUT_FILE_ID, SENT_TS,
                              STATUS_TS
           END-EXEC.
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
           PERFORM 2000-PROCESS-PAYMENT
               UNTIL WS-END-OF-PAYMENTS
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       CIFMAST-FILE
                       DATECARD-FILE
                OUTPUT SWIFTOUT-FILE

           IF WS-SWIFTOUT-STATUS NOT = '00'
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE

           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA
      *    FILE ID IS UNIQUE PER RUN: SW + DD + HHMM
           STRING WS-CURR-DATE (7:2) WS-CURR-TIME (1:4)
                  DELIMITED BY SIZE
                  INTO WS-OFI-DDHHMM
           END-STRING
           STRING WS-CURR-DATE (1:4) '-' WS-CURR-DATE (5:2) '-'
                  WS-CURR-DATE (7:2) '-'
                  WS-CURR-TIME (1:2) '.' WS-CURR-TIME (3:2) '.'
                  WS-CURR-TIME (5:2) '.000000'
                  DELIMITED BY SIZE
                  INTO WS-SENT-TS
           END-STRING

           EXEC SQL OPEN SWFTCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           PERFORM 8000-FETCH-PAYMENT
           .
      *================================================================*
       2000-PROCESS-PAYMENT.
      *================================================================*
           PERFORM 2100-GET-PAYER-DETAILS

      *    WITHOUT COMPLETE PAYER DETAILS THE PAYMENT MAY NOT BE SENT
      *    (EC REG 1781/2006). LEAVE IT AT 'VAL' FOR PAYMENTS
      *    OPERATIONS TO INVESTIGATE (REPORT PYSWFHLD).
           IF WS-PAYER-NAME = SPACES
               ADD 1                       TO WS-HELD-COUNT
               DISPLAY 'PYSWF010 W001 PAYMENT HELD - NO PAYER '
                       'DETAILS: ' PYI-PAY-ID
           ELSE
               PERFORM 2200-WRITE-MT103
               EXEC SQL
                   UPDATE MBK.PAYINSTR
                      SET PAY_STATUS  = 'SNT',
                          OUT_FILE_ID = :WS-OUT-FILE-ID,
                          SENT_TS     = :WS-SENT-TS,
                          STATUS_TS   = CURRENT TIMESTAMP
                    WHERE CURRENT OF SWFTCSR
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE '2000-PROCESS-PAYMENT'
                                           TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
               END-IF
               ADD 1                       TO WS-MSG-COUNT
           END-IF

           PERFORM 8000-FETCH-PAYMENT
           .
      *================================================================*
       2100-GET-PAYER-DETAILS.
      *================================================================*
           MOVE SPACES                     TO WS-PAYER-NAME
                                              WS-TOWN-PCODE
           MOVE PYI-DR-SORT-CODE           TO ACM-SORT-CODE
           MOVE PYI-DR-ACCT-NO             TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF WS-ACCTMAST-OK
               MOVE ACM-CIF-NO             TO CIF-NO
               READ CIFMAST-FILE
               IF WS-CIFMAST-OK
                   STRING CIF-TITLE     DELIMITED BY SPACE
                          ' '           DELIMITED BY SIZE
                          CIF-FIRST-NM  DELIMITED BY SPACE
                          ' '           DELIMITED BY SIZE
                          CIF-LAST-NM   DELIMITED BY SPACE
                          INTO WS-PAYER-NAME
                   END-STRING
                   STRING CIF-TOWN      DELIMITED BY '  '
                          ' '           DELIMITED BY SIZE
                          CIF-POST-CD   DELIMITED BY '  '
                          INTO WS-TOWN-PCODE
                   END-STRING
               END-IF
           END-IF
           .
      *================================================================*
       2200-WRITE-MT103.
      *================================================================*
      *---- RECEIVER LOGICAL TERMINAL: BIC8 + 'X' + BRANCH ------------*
           MOVE PYI-BENE-BIC (1:8)         TO WS-RECEIVER-ADDRESS (1:8)
           MOVE 'X'                        TO WS-RECEIVER-ADDRESS (9:1)
           IF PYI-BENE-BIC (9:3) = SPACES
               MOVE 'XXX'                  TO WS-RECEIVER-ADDRESS (10:3)
           ELSE
               MOVE PYI-BENE-BIC (9:3)     TO WS-RECEIVER-ADDRESS (10:3)
           END-IF

      *---- 32A: YYMMDD + CCY + AMOUNT WITH DECIMAL COMMA -------------*
           STRING PYI-VALUE-DATE (3:2) PYI-VALUE-DATE (6:2)
                  PYI-VALUE-DATE (9:2)
                  DELIMITED BY SIZE
                  INTO WS-SWIFT-VALUE-DATE
           END-STRING
           MOVE PYI-PAY-AMT                TO WS-AMOUNT-EDIT
           MOVE ZERO                       TO WS-LEAD-SPACES
           INSPECT WS-AMOUNT-EDIT TALLYING WS-LEAD-SPACES
                   FOR LEADING SPACES
           MOVE WS-AMOUNT-EDIT (WS-LEAD-SPACES + 1:)
                                           TO WS-AMOUNT-TEXT
           INSPECT WS-AMOUNT-TEXT REPLACING ALL '.' BY ','

      *---- BASIC, APPLICATION AND TEXT BLOCKS ------------------------*
           MOVE SPACES                     TO WS-LINE
           STRING '{1:F01' WS-OUR-LT-ADDRESS '0000000000}'
                  '{2:I103' WS-RECEIVER-ADDRESS 'N}'
                  '{4:'
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM 8100-WRITE-LINE

           STRING ':20:' PYI-PAY-ID
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 8100-WRITE-LINE

           MOVE ':23B:CRED'                TO WS-LINE
           PERFORM 8100-WRITE-LINE

           STRING ':32A:' WS-SWIFT-VALUE-DATE PYI-PAY-CCY-CD
                  WS-AMOUNT-TEXT
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 8100-WRITE-LINE

           STRING ':50K:/' ACM-IBAN
                  DELIMITED BY SPACE INTO WS-LINE
           END-STRING
           PERFORM 8100-WRITE-LINE
           MOVE WS-PAYER-NAME              TO WS-LINE
           PERFORM 8100-WRITE-LINE
           MOVE CIF-ADDR-LINE1 (1:35)      TO WS-LINE
           PERFORM 8100-WRITE-LINE
           MOVE WS-TOWN-PCODE              TO WS-LINE
           PERFORM 8100-WRITE-LINE

           STRING ':57A:' PYI-BENE-BIC
                  DELIMITED BY SPACE INTO WS-LINE
           END-STRING
           PERFORM 8100-WRITE-LINE

           IF PYI-BENE-IBAN NOT = SPACES
               STRING ':59:/' PYI-BENE-IBAN
                      DELIMITED BY SPACE INTO WS-LINE
               END-STRING
           ELSE
               STRING ':59:/' PYI-BENE-ACCT-NO
                      DELIMITED BY SPACE INTO WS-LINE
               END-STRING
           END-IF
           PERFORM 8100-WRITE-LINE
           MOVE PYI-BENE-NAME-TEXT (1:35)  TO WS-LINE
           PERFORM 8100-WRITE-LINE

           IF PYI-REMIT-REF NOT = SPACES
               STRING ':70:' PYI-REMIT-REF
                      DELIMITED BY SIZE INTO WS-LINE
               END-STRING
               PERFORM 8100-WRITE-LINE
           END-IF

           STRING ':71A:' PYI-CHARGE-BEARER
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 8100-WRITE-LINE

           MOVE '-}'                       TO WS-LINE
           PERFORM 8100-WRITE-LINE
           MOVE '$'                        TO WS-LINE
           PERFORM 8100-WRITE-LINE
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL CLOSE SWFTCSR END-EXEC
           EXEC SQL COMMIT END-EXEC

           CLOSE SWIFTOUT-FILE
                 ACCTMAST-FILE
                 CIFMAST-FILE

           DISPLAY 'PYSWF010 - MT103 WRITTEN     ' WS-MSG-COUNT
           DISPLAY 'PYSWF010 - PAYMENTS HELD     ' WS-HELD-COUNT
           DISPLAY 'PYSWF010 - FILE ID           ' WS-OUT-FILE-ID

           EVALUATE TRUE
               WHEN WS-HELD-COUNT > ZERO
                   MOVE 4                  TO RETURN-CODE
               WHEN WS-MSG-COUNT = ZERO
                   MOVE 4                  TO RETURN-CODE
           END-EVALUATE
           .
      *================================================================*
       8000-FETCH-PAYMENT.
      *================================================================*
           EXEC SQL
               FETCH SWFTCSR
                INTO :PYI-PAY-ID, :PYI-DR-SORT-CODE, :PYI-DR-ACCT-NO,
                     :PYI-BENE-NAME, :PYI-BENE-ACCT-NO,
                     :PYI-BENE-IBAN, :PYI-BENE-BIC, :PYI-PAY-CCY-CD,
                     :PYI-PAY-AMT, :PYI-CHARGE-BEARER,
                     :PYI-REMIT-REF, :PYI-VALUE-DATE
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
       8100-WRITE-LINE.
      *================================================================*
           WRITE SWIFTOUT-RECORD           FROM WS-LINE
           IF WS-SWIFTOUT-STATUS NOT = '00'
               MOVE '8100-WRITE-LINE'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SWIFTOUT WRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-SWIFTOUT-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           MOVE SPACES                     TO WS-LINE
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
