       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PYINIT1.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2008-02-04.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PYINIT1                                             *
      * TITLE    : PAYMENT INITIATION - CHANNEL SERVICE PAYINIT        *
      * TYPE     : CICS / DB2 / MQ - LINKED FROM CHROUTER              *
      * PLAN     : MBKCICS1  (DB2ENTRY MBKPAY, THREADS PROTECTED 10)   *
      * TABLES   : MBK.PAYINSTR  (SELECT / INSERT)                     *
      *            MBK.PAYID_SEQ (SEQUENCE)                            *
      * QUEUES   : MBK.FPS.OUTBOUND, MBK.CHAPS.OUTBOUND (MQPUT1)       *
      * LINKS TO : PSMEMO1 (DEBIT AND CHARGE POSTINGS)                 *
      *----------------------------------------------------------------*
      * 1. VALIDATE THE INSTRUCTION FOR THE REQUESTED SCHEME           *
      * 2. REJECT DUPLICATES (SAME CHANNEL + CHANNEL PAYMENT REF)      *
      * 3. ALLOCATE HOST PAYMENT ID  PY + CCYYMMDD + NNNNNN            *
      * 4. DEBIT THE CUSTOMER (AND ANY CHARGE) VIA PSMEMO1             *
      * 5. INSERT MBK.PAYINSTR                                         *
      * 6. FPS / CHAPS: PUT TO THE GATEWAY QUEUE UNDER SYNCPOINT       *
      *    BACS / SWIFT: LEFT AT STATUS 'VAL' FOR THE BATCH SENDERS    *
      *    (PYBAC010 / PYSWF010)                                       *
      * THE DEBIT, THE PAYINSTR ROW AND THE MQ MESSAGE ARE ONE UNIT OF *
      * WORK - ANY FAILURE ROLLS BACK ALL THREE.                       *
      *----------------------------------------------------------------*
      * SCHEME RULES                                                   *
      *   FPS   GBP ONLY, MAX 100,000.00, VALUE DATE = TODAY           *
      *   CHAP  GBP ONLY, CUT-OFF 15:40, VALUE DATE = TODAY            *
      *   BACS  GBP ONLY, VALUE DATE >= TODAY + 2 BUSINESS DAYS        *
      *   SWFT  ANY CCY, BIC REQUIRED, VALUE DATE DEFAULT SPOT (T+2)   *
      *         FX RATE AND STERLING AMOUNT ARE TAKEN FROM THE CHANNEL *
      *         AND CHECKED AGAINST THE TREASURY RATE BY FXSVC01       *
      *         (SERVICE FXQUOTE, FUNCTION V) - REJECT FXRT IF OUTSIDE *
      *         TOLERANCE. PIR 2009/201 CLOSED 2011-05-23.             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2008-02-04 AOS  ORIGINAL VERSION (FPS)                         *
      * 2008-09-15 AOS  BACS AND SWIFT (REPLACES IBANK OVERNIGHT FILE) *
      * 2009-04-06 AOS  CHAPS VIA PAYMENTS GATEWAY                     *
      * 2009-11-30 JNB  CHARGE POSTED SEPARATELY (CR 09/0311)          *
      * 2011-05-23 JNB  SWIFT FX RATE VALIDATED VIA FXSVC01            *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PYINIT1 '.
      *
       01  WS-CONSTANTS.
           05  WS-FPS-SCHEME-LIMIT         PIC S9(13)V99 COMP-3
                                           VALUE +100000.00.
           05  WS-CHAPS-CUTOFF             PIC 9(06) VALUE 154000.
           05  WS-FPS-QUEUE                PIC X(48)
                                           VALUE 'MBK.FPS.OUTBOUND'.
           05  WS-CHAPS-QUEUE              PIC X(48)
                                           VALUE 'MBK.CHAPS.OUTBOUND'.
           05  WS-OUR-NAME                 PIC X(35)
                                           VALUE 'MEGABANK CUSTOMER'.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TIME-HHMMSS              PIC 9(06).
           05  WS-TIME-HHMMSS-X REDEFINES WS-TIME-HHMMSS
                                           PIC X(06).
           05  WS-VALUE-DATE               PIC 9(08).
           05  WS-VALUE-DATE-R REDEFINES WS-VALUE-DATE.
               10  WS-VD-CCYY              PIC X(04).
               10  WS-VD-MM                PIC X(02).
               10  WS-VD-DD                PIC X(02).
           05  WS-EARLIEST-DATE            PIC 9(08).
           05  WS-DR-AMT                   PIC S9(13)V99 COMP-3.
           05  WS-CHARGE-AMT               PIC S9(13)V99 COMP-3.
           05  WS-PAY-SEQ                  PIC S9(09) COMP.
           05  WS-PAY-SEQ-DISP             PIC 9(06).
           05  WS-HOST-PAY-ID.
               10  FILLER                  PIC X(02) VALUE 'PY'.
               10  WS-PAY-ID-DATE          PIC 9(08).
               10  WS-PAY-ID-SEQ           PIC 9(06).
           05  WS-TXN-CODE                 PIC X(04).
           05  WS-NEW-STATUS               PIC X(03).
           05  WS-NEW-AVAIL-BAL            PIC S9(13)V99 COMP-3.
           05  WS-REVERSED-NAME            PIC X(35).
           05  WS-TRAILING-SPACES          PIC S9(04) COMP.
           05  WS-SQLCODE-DISP             PIC -9(09).
           05  WS-TIMESTAMP                PIC X(26).
           05  WS-MEMO-SORT-CODE           PIC X(06).
           05  WS-MEMO-ACCT-NO             PIC X(08).
           05  WS-MEMO-BENE-NAME           PIC X(35).
           05  WS-MEMO-SCHEME              PIC X(04).
           05  WS-MEMO-RC                  PIC X(04).
           05  WS-MEMO-REASON              PIC X(04).
           05  WS-MEMO-MESSAGE             PIC X(80).
           05  WS-ERROR-IND                PIC X(01) VALUE 'N'.
               88  WS-ERROR                    VALUE 'Y'.
      *
      *    CHANNEL COMMAREA IS SAVED HERE WHILE CH-COMMAREA IS REUSED
      *    TO CALL PSMEMO1.
       01  WS-SAVED-COMMAREA               PIC X(4000).
       01  WS-FX-SAVE                      PIC X(4000).
       01  WS-FX-FIELDS.
           05  WS-FX-CCY                   PIC X(03).
           05  WS-FX-CCY-AMT               PIC S9(13)V99 COMP-3.
           05  WS-FX-RATE                  PIC 9(05)V9(06).
           05  WS-FX-RC                    PIC X(04).
           05  WS-FX-REASON                PIC X(04).
           05  WS-FX-MESSAGE               PIC X(80).
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLPAYI END-EXEC.
      *
       01  WS-DUP-PAY-ID                   PIC X(16).
       01  WS-DUP-PAY-STATUS               PIC X(03).
       01  WS-DUP-SCHEME-REF               PIC X(35).
       01  WS-SENT-TS-IND                  PIC S9(04) COMP.
      *
      *---- MQ --------------------------------------------------------*
       01  MQ-CONSTANTS.
           COPY CMQV.
       01  MQM-OBJECT-DESCRIPTOR.
           COPY CMQODV.
       01  MQM-MESSAGE-DESCRIPTOR.
           COPY CMQMDV.
       01  MQM-PUT-MESSAGE-OPTIONS.
           COPY CMQPMOV.
       01  WS-MQ-FIELDS.
           05  WS-HCONN                    PIC S9(09) BINARY VALUE 0.
           05  WS-BUFFLEN                  PIC S9(09) BINARY.
           05  WS-COMPCODE                 PIC S9(09) BINARY.
           05  WS-REASON                   PIC S9(09) BINARY.
           05  WS-REASON-DISP              PIC 9(04).
      *
           COPY PYFPSMSG.
      *
           COPY DTEPARM.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(4000).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE DFHCOMMAREA                TO CH-COMMAREA
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
                                              CHPI-HOST-PAY-ID
                                              CHPI-PAY-STATUS
                                              CHPI-SCHEME-REF
           MOVE ZERO                       TO CHPI-NEW-AVAIL-BAL

           PERFORM 0100-GET-DATE-TIME
           PERFORM 1000-VALIDATE-INSTRUCTION

           IF CH-RC-OK
               PERFORM 2000-CHECK-DUPLICATE
           END-IF
           IF CH-RC-OK
               PERFORM 3000-ALLOCATE-PAY-ID
           END-IF
           IF CH-RC-OK
               PERFORM 4000-DEBIT-CUSTOMER
           END-IF
           IF CH-RC-OK
               PERFORM 5000-INSERT-PAYMENT
           END-IF
           IF CH-RC-OK
           AND (CHPI-FPS OR CHPI-CHAPS)
               PERFORM 6000-SEND-TO-GATEWAY
           END-IF

           IF CH-RC-OK
               MOVE WS-HOST-PAY-ID         TO CHPI-HOST-PAY-ID
               MOVE WS-NEW-STATUS          TO CHPI-PAY-STATUS
               MOVE WS-NEW-AVAIL-BAL       TO CHPI-NEW-AVAIL-BAL
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       0100-GET-DATE-TIME.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC

      *    DB2 TIMESTAMP FORMAT YYYY-MM-DD-HH.MM.SS.NNNNNN
           MOVE SPACES                     TO WS-TIMESTAMP
           STRING WS-TODAY-X (1:4) '-' WS-TODAY-X (5:2) '-'
                  WS-TODAY-X (7:2) '-'
                  WS-TIME-HHMMSS-X (1:2) '.'
                  WS-TIME-HHMMSS-X (3:2) '.'
                  WS-TIME-HHMMSS-X (5:2) '.000000'
                  DELIMITED BY SIZE
                  INTO WS-TIMESTAMP
           END-STRING
           .
      *================================================================*
       1000-VALIDATE-INSTRUCTION.
      *================================================================*
      *---- COMMON FIELDS ---------------------------------------------*
           EVALUATE TRUE
               WHEN CHPI-EXT-REF = SPACES OR LOW-VALUES
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'CHANNEL PAYMENT REFERENCE NOT SUPPLIED'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT (CHPI-FPS OR CHPI-CHAPS
                         OR CHPI-BACS OR CHPI-SWIFT)
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'PAYMENT SCHEME INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-DR-SORT-CODE NOT NUMERIC
               WHEN CHPI-DR-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'DEBIT ACCOUNT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-BENE-NAME = SPACES
                   MOVE 'INVN'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY NAME NOT SUPPLIED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-PAY-AMT NOT NUMERIC
               WHEN CHPI-DR-AMT NOT NUMERIC
               WHEN CHPI-CHARGE-AMT NOT NUMERIC
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'AMOUNT FIELDS MUST BE NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-PAY-AMT NOT > ZERO
               WHEN CHPI-CHARGE-AMT < ZERO
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'PAYMENT AMOUNT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD = SPACES
               IF CHPI-VALUE-DATE NUMERIC
                   MOVE CHPI-VALUE-DATE    TO WS-VALUE-DATE
               ELSE
                   MOVE ZERO               TO WS-VALUE-DATE
               END-IF
               MOVE CHPI-CHARGE-AMT        TO WS-CHARGE-AMT

               EVALUATE TRUE
                   WHEN CHPI-FPS
                       PERFORM 1100-VALIDATE-FPS
                   WHEN CHPI-CHAPS
                       PERFORM 1200-VALIDATE-CHAPS
                   WHEN CHPI-BACS
                       PERFORM 1300-VALIDATE-BACS
                   WHEN CHPI-SWIFT
                       PERFORM 1400-VALIDATE-SWIFT
               END-EVALUATE
           END-IF

           IF CH-REASON-CD NOT = SPACES
               IF CH-REASON-CD (1:3) = 'INV'
                   MOVE '0016'             TO CH-RETURN-CD
               ELSE
                   MOVE '0008'             TO CH-RETURN-CD
               END-IF
           END-IF
           .
      *================================================================*
       1100-VALIDATE-FPS.
      *================================================================*
           PERFORM 1500-VALIDATE-UK-BENEFICIARY
           IF CH-REASON-CD = SPACES
               IF CHPI-PAY-AMT > WS-FPS-SCHEME-LIMIT
                   MOVE 'LIMT'             TO CH-REASON-CD
                   MOVE 'AMOUNT EXCEEDS FASTER PAYMENTS SCHEME LIMIT'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           MOVE WS-TODAY                   TO WS-VALUE-DATE
           MOVE CHPI-PAY-AMT               TO WS-DR-AMT
           MOVE 'FPO '                     TO WS-TXN-CODE
           .
      *================================================================*
       1200-VALIDATE-CHAPS.
      *================================================================*
           PERFORM 1500-VALIDATE-UK-BENEFICIARY
           IF CH-REASON-CD = SPACES
               IF WS-TIME-HHMMSS > WS-CHAPS-CUTOFF
                   MOVE 'COFF'             TO CH-REASON-CD
                   MOVE 'CHAPS CUT-OFF (15:40) HAS PASSED'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           IF CH-REASON-CD = SPACES
               MOVE 'B'                    TO DTE-FUNCTION
               MOVE WS-TODAY               TO DTE-DATE-IN
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               IF DTE-BUS-DAY-IND NOT = 'Y'
                   MOVE 'NBDY'             TO CH-REASON-CD
                   MOVE 'CHAPS NOT AVAILABLE - NOT A BUSINESS DAY'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           MOVE WS-TODAY                   TO WS-VALUE-DATE
           MOVE CHPI-PAY-AMT               TO WS-DR-AMT
           MOVE 'CHP '                     TO WS-TXN-CODE
           .
      *================================================================*
       1300-VALIDATE-BACS.
      *================================================================*
           PERFORM 1500-VALIDATE-UK-BENEFICIARY

      *    3-DAY CYCLE: EARLIEST ENTRY DATE IS TODAY + 2 BUSINESS DAYS.
           MOVE 'A'                        TO DTE-FUNCTION
           MOVE WS-TODAY                   TO DTE-DATE-IN
           MOVE +2                         TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO WS-EARLIEST-DATE

           IF CH-REASON-CD = SPACES
               IF WS-VALUE-DATE = ZERO
                   MOVE WS-EARLIEST-DATE   TO WS-VALUE-DATE
               ELSE
                   MOVE 'B'                TO DTE-FUNCTION
                   MOVE WS-VALUE-DATE      TO DTE-DATE-IN
                   CALL 'DTEUTIL' USING DTE-PARM-BLOCK
                   IF NOT DTE-OK
                   OR DTE-BUS-DAY-IND NOT = 'Y'
                   OR WS-VALUE-DATE < WS-EARLIEST-DATE
                       MOVE 'VDTE'         TO CH-REASON-CD
                       MOVE 'BACS VALUE DATE INVALID OR TOO EARLY'
                                           TO CH-MESSAGE-TEXT
                   END-IF
               END-IF
           END-IF
           MOVE CHPI-PAY-AMT               TO WS-DR-AMT
           MOVE 'BAC '                     TO WS-TXN-CODE
           .
      *================================================================*
       1400-VALIDATE-SWIFT.
      *================================================================*
           EVALUATE TRUE
               WHEN CHPI-BENE-IBAN = SPACES
               AND  CHPI-BENE-ACCT-NO = SPACES
                   MOVE 'INVB'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY IBAN OR ACCOUNT REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-BENE-BIC (1:8) = SPACES
               WHEN CHPI-BENE-BIC (9:3) NOT = SPACES
               AND  CHPI-BENE-BIC (11:1) = SPACE
                   MOVE 'INVB'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY BIC MUST BE 8 OR 11 CHARACTERS'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-BENE-CTRY-CD = SPACES
                   MOVE 'INVB'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY BANK COUNTRY REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-PAY-CCY-CD = SPACES
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'PAYMENT CURRENCY REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-CHARGE-BEARER NOT = 'SHA' AND 'OUR'
                                                   AND 'BEN'
                   MOVE 'INVG'             TO CH-REASON-CD
                   MOVE 'CHARGE BEARER MUST BE SHA, OUR OR BEN'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-PAY-CCY-CD NOT = 'GBP'
               AND  (CHPI-FX-RATE NOT NUMERIC
                     OR CHPI-FX-RATE = ZERO
                     OR CHPI-DR-AMT NOT > ZERO)
                   MOVE 'INVX'             TO CH-REASON-CD
                   MOVE 'FX RATE AND STERLING AMOUNT REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD = SPACES
           AND CHPI-PAY-CCY-CD NOT = 'GBP'
               PERFORM 1450-VALIDATE-FX-RATE
           END-IF

           IF CHPI-PAY-CCY-CD = 'GBP'
               MOVE CHPI-PAY-AMT           TO WS-DR-AMT
           ELSE
               MOVE CHPI-DR-AMT            TO WS-DR-AMT
           END-IF

           IF CH-REASON-CD = SPACES AND WS-VALUE-DATE = ZERO
               MOVE 'A'                    TO DTE-FUNCTION
               MOVE WS-TODAY               TO DTE-DATE-IN
               MOVE +2                     TO DTE-DAYS
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               MOVE DTE-DATE-OUT           TO WS-VALUE-DATE
           END-IF
           MOVE 'SWO '                     TO WS-TXN-CODE
           .
      *================================================================*
       1450-VALIDATE-FX-RATE.
      *================================================================*
      *    THE PAYINIT AREA AND THE FXQUOTE AREA SHARE CH-DATA, SO THE
      *    REQUEST IS SAVED, THE FX CALL MADE, AND THE REQUEST PUT BACK.
           MOVE CHPI-PAY-CCY-CD            TO WS-FX-CCY
           MOVE CHPI-PAY-AMT               TO WS-FX-CCY-AMT
           MOVE CHPI-FX-RATE               TO WS-FX-RATE
           MOVE CH-COMMAREA                TO WS-FX-SAVE

           MOVE 'FXQUOTE '                 TO CH-SERVICE-CD
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
                                              CH-DATA
           SET CHFX-VALIDATE               TO TRUE
           MOVE WS-FX-CCY                  TO CHFX-CCY-CD
           SET CHFX-BANK-SELLS             TO TRUE
           MOVE ZERO                       TO CHFX-GBP-AMOUNT
           MOVE WS-FX-CCY-AMT              TO CHFX-CCY-AMOUNT
           MOVE WS-FX-RATE                 TO CHFX-CHANNEL-RATE
           EXEC CICS LINK
                     PROGRAM('FXSVC01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO WS-FX-RC
               MOVE 'FXNA'                 TO WS-FX-REASON
               MOVE 'FX RATE SERVICE UNAVAILABLE - TRY LATER'
                                           TO WS-FX-MESSAGE
           ELSE
               MOVE CH-RETURN-CD           TO WS-FX-RC
               MOVE CH-REASON-CD           TO WS-FX-REASON
               MOVE CH-MESSAGE-TEXT        TO WS-FX-MESSAGE
           END-IF
           MOVE WS-FX-SAVE                 TO CH-COMMAREA

      *    WARNINGS (HELD / STALE RATE) ARE ACCEPTED.
           IF WS-FX-RC NOT = '0000' AND WS-FX-RC NOT = '0004'
               IF WS-FX-REASON = 'FXNA'
                   MOVE 'FXNA'             TO CH-REASON-CD
               ELSE
                   MOVE 'FXRT'             TO CH-REASON-CD
               END-IF
               MOVE WS-FX-MESSAGE          TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       1500-VALIDATE-UK-BENEFICIARY.
      *================================================================*
      *    NB. NO VOCALINK MODULUS CHECK IS PERFORMED HERE - THE CHANNEL
      *    IS EXPECTED TO HAVE DONE IT (IA-IBK-002 SECTION 5).
           EVALUATE TRUE
               WHEN CHPI-BENE-SORT-CODE NOT NUMERIC
               WHEN CHPI-BENE-ACCT-NO NOT NUMERIC
                   MOVE 'INVB'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY SORT CODE / ACCOUNT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPI-PAY-CCY-CD NOT = 'GBP'
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'DOMESTIC SCHEMES ACCEPT GBP ONLY'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       2000-CHECK-DUPLICATE.
      *================================================================*
           EXEC SQL
               SELECT PAY_ID, PAY_STATUS, SCHEME_REF
                 INTO :WS-DUP-PAY-ID,
                      :WS-DUP-PAY-STATUS,
                      :WS-DUP-SCHEME-REF
                 FROM MBK.PAYINSTR
                WHERE CHANNEL_CD = :CH-CHANNEL-CD
                  AND EXT_REF    = :CHPI-EXT-REF
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'DUPL'             TO CH-REASON-CD
                   MOVE 'DUPLICATE PAYMENT REFERENCE - NOT REPROCESSED'
                                           TO CH-MESSAGE-TEXT
                   MOVE WS-DUP-PAY-ID      TO CHPI-HOST-PAY-ID
                   MOVE WS-DUP-PAY-STATUS  TO CHPI-PAY-STATUS
                   MOVE WS-DUP-SCHEME-REF  TO CHPI-SCHEME-REF
               WHEN 100
                   CONTINUE
               WHEN OTHER
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       3000-ALLOCATE-PAY-ID.
      *================================================================*
           EXEC SQL
               SELECT NEXT VALUE FOR MBK.PAYID_SEQ
                 INTO :WS-PAY-SEQ
                 FROM SYSIBM.SYSDUMMY1
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 9100-SQL-ERROR
           ELSE
               MOVE WS-TODAY               TO WS-PAY-ID-DATE
               MOVE WS-PAY-SEQ             TO WS-PAY-ID-SEQ
           END-IF
           .
      *================================================================*
       4000-DEBIT-CUSTOMER.
      *================================================================*
      *    KEEP WHAT WE NEED FROM THE CHANNEL REQUEST - CH-COMMAREA IS
      *    OVERLAID WITH THE MEMOPOST REQUEST FOR EACH PSMEMO1 CALL.
           MOVE CH-COMMAREA                TO WS-SAVED-COMMAREA
           MOVE CHPI-DR-SORT-CODE          TO WS-MEMO-SORT-CODE
           MOVE CHPI-DR-ACCT-NO            TO WS-MEMO-ACCT-NO
           MOVE CHPI-BENE-NAME             TO WS-MEMO-BENE-NAME
           MOVE CHPI-SCHEME                TO WS-MEMO-SCHEME

      *---- PRINCIPAL DEBIT -------------------------------------------*
           PERFORM 4100-SET-MEMO-REQUEST
           MOVE WS-TXN-CODE                TO CHMP-TXN-CODE
           MOVE WS-DR-AMT                  TO CHMP-AMOUNT
           MOVE WS-HOST-PAY-ID             TO CHMP-EXT-REF
           MOVE 'PAY'                      TO CHMP-EXT-REF (17:3)
           MOVE WS-MEMO-BENE-NAME          TO CHMP-NARRATIVE-1
           PERFORM 4200-CALL-PSMEMO1

      *---- CHARGE (IF ANY) - SEPARATE STATEMENT ENTRY ---------------*
           IF NOT WS-ERROR AND WS-CHARGE-AMT > ZERO
               PERFORM 4100-SET-MEMO-REQUEST
               MOVE 'CHG '                 TO CHMP-TXN-CODE
               MOVE WS-CHARGE-AMT          TO CHMP-AMOUNT
               MOVE WS-HOST-PAY-ID         TO CHMP-EXT-REF
               MOVE 'CHG'                  TO CHMP-EXT-REF (17:3)
               MOVE 'PAYMENT CHARGE'       TO CHMP-NARRATIVE-1
               PERFORM 4200-CALL-PSMEMO1
      *        PRINCIPAL IS ALREADY DEBITED - BACK IT OUT.
               IF WS-ERROR
                   EXEC CICS SYNCPOINT ROLLBACK
                   END-EXEC
               END-IF
           END-IF

           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA

           IF WS-ERROR
               MOVE WS-MEMO-RC             TO CH-RETURN-CD
               MOVE WS-MEMO-REASON         TO CH-REASON-CD
               MOVE WS-MEMO-MESSAGE        TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       4100-SET-MEMO-REQUEST.
      *================================================================*
      *    CH-HEADER (EYECATCHER, CHANNEL, USER) IS KEPT FROM THE
      *    CHANNEL REQUEST SO THE POSTING IS ATTRIBUTED CORRECTLY.
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA
           MOVE 'MEMOPOST'                 TO CH-SERVICE-CD
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
                                              CH-DATA
           MOVE WS-MEMO-SORT-CODE          TO CHMP-SORT-CODE
           MOVE WS-MEMO-ACCT-NO            TO CHMP-ACCT-NO
           MOVE 'D'                        TO CHMP-DR-CR-IND
           MOVE WS-VALUE-DATE              TO CHMP-VALUE-DATE
           MOVE 'N'                        TO CHMP-OVERRIDE-IND
           STRING WS-MEMO-SCHEME
                  ' '
                  WS-HOST-PAY-ID
                  DELIMITED BY SIZE
                  INTO CHMP-NARRATIVE-2
           END-STRING
           .
      *================================================================*
       4200-CALL-PSMEMO1.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM('PSMEMO1')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Y'                    TO WS-ERROR-IND
               MOVE '0012'                 TO WS-MEMO-RC
               MOVE 'PGMX'                 TO WS-MEMO-REASON
               MOVE 'POSTING SERVICE UNAVAILABLE'
                                           TO WS-MEMO-MESSAGE
           ELSE
               IF CH-RC-OK OR CH-RC-WARNING
                   MOVE CHMP-NEW-AVAIL-BAL TO WS-NEW-AVAIL-BAL
               ELSE
                   MOVE 'Y'                TO WS-ERROR-IND
                   MOVE CH-RETURN-CD       TO WS-MEMO-RC
                   MOVE CH-REASON-CD       TO WS-MEMO-REASON
                   MOVE CH-MESSAGE-TEXT    TO WS-MEMO-MESSAGE
               END-IF
           END-IF
           .
      *================================================================*
       5000-INSERT-PAYMENT.
      *================================================================*
           MOVE WS-HOST-PAY-ID             TO PYI-PAY-ID
           MOVE CHPI-EXT-REF               TO PYI-EXT-REF
           MOVE CH-CHANNEL-CD              TO PYI-CHANNEL-CD
           MOVE CHPI-SCHEME                TO PYI-PAY-SCHEME
           MOVE CHPI-DR-SORT-CODE          TO PYI-DR-SORT-CODE
           MOVE CHPI-DR-ACCT-NO            TO PYI-DR-ACCT-NO

           MOVE CHPI-BENE-NAME             TO PYI-BENE-NAME-TEXT
           MOVE FUNCTION REVERSE (CHPI-BENE-NAME)
                                           TO WS-REVERSED-NAME
           MOVE ZERO                       TO WS-TRAILING-SPACES
           INSPECT WS-REVERSED-NAME
                   TALLYING WS-TRAILING-SPACES FOR LEADING SPACES
           COMPUTE PYI-BENE-NAME-LEN = 35 - WS-TRAILING-SPACES

           MOVE CHPI-BENE-SORT-CODE        TO PYI-BENE-SORT-CODE
           MOVE CHPI-BENE-ACCT-NO          TO PYI-BENE-ACCT-NO
           MOVE CHPI-BENE-IBAN             TO PYI-BENE-IBAN
           MOVE CHPI-BENE-BIC              TO PYI-BENE-BIC
           MOVE CHPI-BENE-CTRY-CD          TO PYI-BENE-CTRY-CD
           MOVE CHPI-PAY-CCY-CD            TO PYI-PAY-CCY-CD
           MOVE CHPI-PAY-AMT               TO PYI-PAY-AMT
           IF CHPI-FX-RATE NUMERIC
               MOVE CHPI-FX-RATE           TO PYI-FX-RATE
           ELSE
               MOVE ZERO                   TO PYI-FX-RATE
           END-IF
           MOVE WS-DR-AMT                  TO PYI-DR-AMT
           MOVE WS-CHARGE-AMT              TO PYI-CHARGE-AMT
           MOVE CHPI-CHARGE-BEARER         TO PYI-CHARGE-BEARER
           MOVE CHPI-REMIT-REF             TO PYI-REMIT-REF
           MOVE CHPI-PURPOSE-CD            TO PYI-PURPOSE-CD
           STRING WS-VD-CCYY '-' WS-VD-MM '-' WS-VD-DD
                  DELIMITED BY SIZE
                  INTO PYI-VALUE-DATE
           END-STRING
           MOVE SPACES                     TO PYI-SCHEME-REF
                                              PYI-REJECT-CD
                                              PYI-OUT-FILE-ID
           MOVE CH-USER-ID                 TO PYI-USER-ID

      *    FPS/CHAPS ARE SENT IN THIS UOW (STEP 6); BACS/SWIFT WAIT
      *    FOR THE BATCH SENDERS.
           IF CHPI-FPS OR CHPI-CHAPS
               MOVE 'SNT'                  TO WS-NEW-STATUS
               MOVE WS-TIMESTAMP           TO PYI-SENT-TS
               MOVE ZERO                   TO WS-SENT-TS-IND
           ELSE
               MOVE 'VAL'                  TO WS-NEW-STATUS
               MOVE SPACES                 TO PYI-SENT-TS
               MOVE -1                     TO WS-SENT-TS-IND
           END-IF
           MOVE WS-NEW-STATUS              TO PYI-PAY-STATUS

           EXEC SQL
               INSERT INTO MBK.PAYINSTR
                     (PAY_ID, EXT_REF, CHANNEL_CD, PAY_SCHEME,
                      PAY_STATUS, DR_SORT_CODE, DR_ACCT_NO,
                      BENE_NAME, BENE_SORT_CODE, BENE_ACCT_NO,
                      BENE_IBAN, BENE_BIC, BENE_CTRY_CD,
                      PAY_CCY_CD, PAY_AMT, FX_RATE, DR_AMT,
                      CHARGE_AMT, CHARGE_BEARER, REMIT_REF,
                      PURPOSE_CD, VALUE_DATE, SCHEME_REF,
                      REJECT_CD, CAPTURE_TS, SENT_TS, STATUS_TS,
                      OUT_FILE_ID, USER_ID)
               VALUES
                     (:PYI-PAY-ID, :PYI-EXT-REF, :PYI-CHANNEL-CD,
                      :PYI-PAY-SCHEME, :PYI-PAY-STATUS,
                      :PYI-DR-SORT-CODE, :PYI-DR-ACCT-NO,
                      :PYI-BENE-NAME, :PYI-BENE-SORT-CODE,
                      :PYI-BENE-ACCT-NO, :PYI-BENE-IBAN,
                      :PYI-BENE-BIC, :PYI-BENE-CTRY-CD,
                      :PYI-PAY-CCY-CD, :PYI-PAY-AMT, :PYI-FX-RATE,
                      :PYI-DR-AMT, :PYI-CHARGE-AMT,
                      :PYI-CHARGE-BEARER, :PYI-REMIT-REF,
                      :PYI-PURPOSE-CD, :PYI-VALUE-DATE,
                      :PYI-SCHEME-REF, :PYI-REJECT-CD,
                      CURRENT TIMESTAMP,
                      :PYI-SENT-TS :WS-SENT-TS-IND,
                      CURRENT TIMESTAMP,
                      :PYI-OUT-FILE-ID, :PYI-USER-ID)
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *================================================================*
       6000-SEND-TO-GATEWAY.
      *================================================================*
           MOVE SPACES                     TO FPM-REQUEST
           IF CHPI-FPS
               SET FPM-SINGLE-IMMEDIATE    TO TRUE
               MOVE WS-FPS-QUEUE           TO MQOD-OBJECTNAME
           ELSE
               SET FPM-CHAPS-PAYMENT       TO TRUE
               MOVE WS-CHAPS-QUEUE         TO MQOD-OBJECTNAME
           END-IF
           MOVE '01'                       TO FPM-VERSION
           MOVE WS-HOST-PAY-ID             TO FPM-MSG-ID
           MOVE WS-TIMESTAMP               TO FPM-CREATE-TS
           MOVE CHPI-DR-SORT-CODE          TO FPM-ORIG-SORT-CODE
           MOVE CHPI-DR-ACCT-NO            TO FPM-ORIG-ACCT-NO
           MOVE WS-OUR-NAME                TO FPM-ORIG-NAME
           MOVE CHPI-BENE-SORT-CODE        TO FPM-BENE-SORT-CODE
           MOVE CHPI-BENE-ACCT-NO          TO FPM-BENE-ACCT-NO
           MOVE CHPI-BENE-NAME             TO FPM-BENE-NAME
           MOVE CHPI-PAY-AMT               TO FPM-AMOUNT
           MOVE 'GBP'                      TO FPM-CCY-CD
           MOVE CHPI-REMIT-REF             TO FPM-REFERENCE
           MOVE WS-VALUE-DATE              TO FPM-SETTLEMENT-DATE

           MOVE MQOT-Q                     TO MQOD-OBJECTTYPE
           MOVE MQMT-DATAGRAM              TO MQMD-MSGTYPE
           MOVE MQPER-PERSISTENT           TO MQMD-PERSISTENCE
           MOVE MQFMT-STRING               TO MQMD-FORMAT
           MOVE MQMI-NONE                  TO MQMD-MSGID
           MOVE MQCI-NONE                  TO MQMD-CORRELID
           MOVE WS-HOST-PAY-ID             TO MQMD-CORRELID
           COMPUTE MQPMO-OPTIONS = MQPMO-SYNCPOINT
                                 + MQPMO-NEW-MSG-ID
                                 + MQPMO-FAIL-IF-QUIESCING
           MOVE LENGTH OF FPM-REQUEST      TO WS-BUFFLEN

           CALL 'MQPUT1' USING WS-HCONN
                               MQM-OBJECT-DESCRIPTOR
                               MQM-MESSAGE-DESCRIPTOR
                               MQM-PUT-MESSAGE-OPTIONS
                               WS-BUFFLEN
                               FPM-REQUEST
                               WS-COMPCODE
                               WS-REASON

           IF WS-COMPCODE NOT = MQCC-OK
      *        GATEWAY QUEUE UNAVAILABLE - BACK OUT THE DEBIT AND THE
      *        PAYINSTR ROW. THE CUSTOMER MUST RETRY.
               EXEC CICS SYNCPOINT ROLLBACK
               END-EXEC
               MOVE WS-REASON              TO WS-REASON-DISP
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'MQER'                 TO CH-REASON-CD
               MOVE SPACES                 TO CH-MESSAGE-TEXT
               STRING 'PAYMENT GATEWAY UNAVAILABLE - MQ REASON '
                      WS-REASON-DISP
                      DELIMITED BY SIZE
                      INTO CH-MESSAGE-TEXT
               END-STRING
           END-IF
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO WS-SQLCODE-DISP
           EXEC CICS SYNCPOINT ROLLBACK
           END-EXEC
           MOVE '0012'                     TO CH-RETURN-CD
           MOVE 'SQLE'                     TO CH-REASON-CD
           MOVE SPACES                     TO CH-MESSAGE-TEXT
           STRING 'PAYMENTS DATABASE ERROR - SQLCODE '
                  WS-SQLCODE-DISP
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
