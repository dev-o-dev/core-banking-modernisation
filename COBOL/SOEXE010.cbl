       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SOEXE010.
       AUTHOR.        R J HALLIDAY.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1988-07-11.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : SOEXE010                                            *
      * TITLE    : STANDING ORDERS - DAILY EXECUTION                   *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : SOEXE010                                            *
      * JCL      : SOEXE01 STEP SO020                                  *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (STOPPED SO FEE)      *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : SOMAST   - STANDING ORDER MASTER (VSAM KSDS, SEQ)   *
      *            ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM)   *
      *                       - UNPAID ITEM COUNTER ONLY               *
      *            MBK.PAYINSTR (INSERT) - EXTERNAL SO PAYMENTS        *
      * OUTPUT   : SOPOST   - POSTINGS FOR PSEOD01 - PSTXNREC          *
      *            FPSOUT   - FPS STANDING ORDER FILE - SOFPSREC       *
      *            EXCEPTS  - EXCEPTIONS / LETTERS - SOEXCREC          *
      *            REPORT   - EXECUTION REPORT (FBA 133)               *
      *----------------------------------------------------------------*
      * RUNS IN THE EOD NETWORK BEFORE PSEOD01. EVERY ACTIVE STANDING  *
      * ORDER WITH A NEXT DUE DATE ON OR BEFORE TODAY IS ATTEMPTED.    *
      *                                                                *
      * FUNDS CHECK: AVAILABLE BALANCE + EFFECTIVE OD LIMIT, LESS THE  *
      * STANDING ORDERS ALREADY PAID FROM THE ACCOUNT IN THIS RUN      *
      * (THE MASTER IS NOT UPDATED HERE - PSEOD010 DOES THAT TONIGHT). *
      * STANDING ORDERS FOR AN ACCOUNT ARE TAKEN IN SO SEQUENCE ORDER; *
      * THERE IS NO PRIORITISATION BY AMOUNT (PIR 1997/012 - OPEN).    *
      *                                                                *
      * RETRY RULES (DDA OPERATIONS MANUAL 7.4):                       *
      *   1ST FAILURE  - RETRY NEXT BUSINESS DAY (NEXT DUE UNCHANGED)  *
      *   2ND FAILURE  - THIS PAYMENT IS MISSED; MOVE TO THE NEXT DUE  *
      *                  DATE; UNPAID ITEM FEE COUNTED; LETTER SENT    *
      *   3 CONSECUTIVE MISSED PAYMENTS - SO SUSPENDED; LETTER SENT    *
      *                                                                *
      * ROUTING                                                        *
      *   I INTERNAL  - DEBIT PAYER + CREDIT BENEFICIARY (BOTH 'SO  ') *
      *   F FPS       - DEBIT PAYER, PAYINSTR ROW (SCHEME 'FPS ',      *
      *                 STATUS SNT) AND A RECORD ON FPSOUT             *
      *   B BACS      - DEBIT PAYER, PAYINSTR ROW (SCHEME 'BACS',      *
      *                 STATUS VAL) - SENT BY PYBAC010 AT 20:30        *
      *                 TOMORROW (THREE-DAY CYCLE - CUSTOMER IS PAID   *
      *                 LATE. KNOWN - LEGACY SOS ONLY.)                *
      *----------------------------------------------------------------*
      * RESTART: EACH SO IS COMMITTED (SOMAST REWRITE + DB2 COMMIT)    *
      * EVERY 100 SOS. ON RESTART, SOS ALREADY ADVANCED HAVE A NEXT    *
      * DUE DATE AFTER TODAY AND ARE NOT PAID AGAIN. THE SOPOST AND    *
      * FPSOUT FILES ARE DISP=MOD ON RESTART (JCL SOEXE01 COMMENTS).   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1988-07-11 RJH  ORIGINAL VERSION (DD0500)                      *
      * 1993-05-17 GHT  RETRY NEXT DAY                                 *
      * 1998-11-30 MLW  Y2K                                            *
      * 2002-10-14 DKP  SO CREATED VIA IBANK - NO PROGRAM CHANGE       *
      * 2005-03-21 SAT  SUSPEND AFTER 3 CONSECUTIVE FAILURES           *
      * 2008-05-27 AOS  FPS ROUTING (FASTER PAYMENTS GO-LIVE)          *
      * 2009-10-01 AOS  UNPAID ITEM COUNTER FOR CGCAL010               *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT SOMAST-FILE      ASSIGN TO SOMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS SOM-KEY
                                   FILE STATUS IS WS-SOMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT SOPOST-FILE      ASSIGN TO SOPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-SOPOST-STATUS.
           SELECT FPSOUT-FILE      ASSIGN TO FPSOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-FPSOUT-STATUS.
           SELECT EXCEPTS-FILE     ASSIGN TO EXCEPTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-EXCEPTS-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
      *
       FD  SOMAST-FILE.
           COPY SOMSTREC.
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  SOPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SOPOST-RECORD                   PIC X(200).
      *
       FD  FPSOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FPSOUT-RECORD                   PIC X(250).
      *
       FD  EXCEPTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  EXCEPTS-RECORD                  PIC X(200).
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'SOEXE010'.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-RETRIES              PIC 9(01) VALUE 1.
           05  WS-SUSPEND-AFTER            PIC 9(02) VALUE 3.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00010000.
           05  WS-COMMIT-FREQ              PIC S9(04) COMP VALUE +100.
      *
       01  WS-FILE-STATUSES.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-SOMAST-STATUS            PIC X(02).
               88  WS-SOMAST-OK                VALUE '00'.
               88  WS-SOMAST-EOF               VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-SOPOST-STATUS            PIC X(02).
           05  WS-FPSOUT-STATUS            PIC X(02).
           05  WS-EXCEPTS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-SOS               VALUE 'Y'.
           05  WS-PAYER-OK-IND             PIC X(01) VALUE 'N'.
               88  WS-PAYER-FOUND              VALUE 'Y'.
           05  WS-PAYER-CHANGED-IND        PIC X(01) VALUE 'N'.
               88  WS-PAYER-CHANGED            VALUE 'Y'.
           05  WS-SO-CHANGED-IND           PIC X(01) VALUE 'N'.
               88  WS-SO-CHANGED               VALUE 'Y'.
           05  WS-BENE-OK-IND              PIC X(01) VALUE 'N'.
               88  WS-BENE-OK                  VALUE 'Y'.
      *
      *---- DATES -----------------------------------------------------*
       01  WS-DATE-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-NEXT-BUS-DATE            PIC 9(08).
           05  WS-NOMINAL                  PIC 9(08).
           05  WS-NOMINAL-R REDEFINES WS-NOMINAL.
               10  WS-NM-CCYY              PIC 9(04).
               10  WS-NM-MM                PIC 9(02).
               10  WS-NM-DD                PIC 9(02).
           05  WS-ADD-MONTHS               PIC S9(04) COMP.
           05  WS-MONTH-TOTAL              PIC S9(06) COMP.
           05  WS-DAYS-IN-MONTH            PIC 9(02).
           05  WS-LEAP-REM-4               PIC S9(04) COMP.
           05  WS-LEAP-REM-100             PIC S9(04) COMP.
           05  WS-LEAP-REM-400             PIC S9(04) COMP.
           05  WS-LEAP-QUOT                PIC S9(06) COMP.
           05  WS-NEXT-DUE                 PIC 9(08).
           05  WS-LOOP-GUARD               PIC S9(04) COMP.
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DB2                 PIC X(10).
      *
       01  WS-MONTH-DAYS-VALUES.
           05  FILLER                      PIC X(24)
               VALUE '312831303130313130313031'.
       01  WS-MONTH-DAYS-TABLE REDEFINES WS-MONTH-DAYS-VALUES.
           05  WS-MONTH-DAYS               PIC 9(02) OCCURS 12 TIMES.
      *
      *---- PAYER CONTROL ---------------------------------------------*
       01  WS-PAYER-FIELDS.
           05  WS-CURR-PAYER               PIC X(14) VALUE LOW-VALUES.
           05  WS-SPENDING-POWER           PIC S9(13)V99 COMP-3.
           05  WS-RESERVED                 PIC S9(13)V99 COMP-3.
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-STOPPED-SO-FEE           PIC S9(07)V99 COMP-3.
           05  WS-PAYER-NAME               PIC X(18).
      *
       01  WS-PAYMENT-FIELDS.
           05  WS-PAY-AMT                  PIC S9(13)V99 COMP-3.
           05  WS-EXC-CD                   PIC X(04).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-PAY-SEQ                  PIC S9(09) COMP.
           05  WS-PAY-SEQ-DISP             PIC 9(06).
           05  WS-HOST-PAY-ID              PIC X(16).
           05  WS-SINCE-COMMIT             PIC S9(04) COMP VALUE ZERO.
           05  WS-SO-KEY-DISP              PIC X(18).
           05  WS-BENE-KEY.
               10  WS-BENE-SORT-CODE       PIC X(06).
               10  WS-BENE-ACCT-NO         PIC X(08).
      *
      *---- SAVED PAYER ACCOUNT (ACCTMAST IS ALSO READ FOR BENE) ------*
       01  WS-SAVED-ACCOUNT                PIC X(400).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-DUE-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-INTERNAL            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-FPS                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-BACS                PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETRY-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-MISSED-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUSPEND-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-CANCEL-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-COMPLETE-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-BENE-FAIL-COUNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-POSTING-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-FPS-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-FPS-TOTAL                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-MISSED-TOTAL             PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'SOEXE010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - STANDING ORDER EXECUTION'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(09) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(05) VALUE ' SEQ'.
           05  FILLER            PIC X(02) VALUE 'R'.
           05  FILLER            PIC X(26) VALUE 'BENEFICIARY'.
           05  FILLER            PIC X(16) VALUE '          AMOUNT'.
           05  FILLER            PIC X(02) VALUE SPACES.
           05  FILLER            PIC X(11) VALUE 'DUE DATE'.
           05  FILLER            PIC X(11) VALUE 'NEXT DUE'.
           05  FILLER            PIC X(09) VALUE 'RESULT'.
           05  FILLER            PIC X(32) VALUE 'PAYMENT ID / NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SO-SEQ                   PIC 9(04).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-ROUTE                    PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-BENE-NAME                PIC X(25).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DUE-DATE                 PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NEXT-DUE                 PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-RESULT                   PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTE                     PIC X(30).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLPAYI END-EXEC.
      *
           COPY PSTXNREC.
      *
           COPY SOFPSREC.
      *
           COPY SOEXCREC.
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
           PERFORM 2000-PROCESS-SO
               UNTIL WS-END-OF-SOS
           PERFORM 2900-END-PAYER
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       DATECARD-FILE
                I-O    SOMAST-FILE
                       ACCTMAST-FILE
                OUTPUT SOPOST-FILE
                       FPSOUT-FILE
                       EXCEPTS-FILE
                       REPORT-FILE

           IF NOT WS-PRDPARM-OK
           OR NOT WS-SOMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-SOPOST-STATUS NOT = '00'
           OR WS-FPSOUT-STATUS NOT = '00'
           OR WS-EXCEPTS-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-PRDPARM-STATUS ' ' WS-SOMAST-STATUS ' '
                      WS-ACCTMAST-STATUS ' ' WS-SOPOST-STATUS ' '
                      WS-FPSOUT-STATUS ' ' WS-EXCEPTS-STATUS ' '
                      WS-REPORT-STATUS ' ' WS-DATECARD-STATUS
                      DELIMITED BY SIZE
                      INTO ABN-KEY-DATA
               END-STRING
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
               MOVE DATECARD-RECORD (1:40) TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           MOVE DTC-NEXT-BUS-DATE          TO WS-NEXT-BUS-DATE
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO HD1-BUS-DATE
           DISPLAY 'SOEXE010 - BUSINESS DATE ' WS-DATE-DB2
                   ' RUN ID ' DTC-RUN-ID

      *---- FPS FILE HEADER -------------------------------------------*
           MOVE SPACES                     TO SOF-FPS-RECORD
           SET SOF-HEADER                  TO TRUE
           STRING 'MBKSO' WS-BUS-DATE (3:6) '01'
                  DELIMITED BY SIZE
                  INTO SOFH-FILE-ID
           END-STRING
           MOVE WS-BUS-DATE                TO SOFH-CREATE-DATE
           MOVE WS-BUS-DATE                TO SOFH-VALUE-DATE
           MOVE 'MBKP0001'                 TO SOFH-SUBMITTER-ID
           WRITE FPSOUT-RECORD             FROM SOF-FPS-RECORD

           PERFORM 8000-READ-SO
           .
      *================================================================*
       2000-PROCESS-SO.
      *================================================================*
      *    CONTROL BREAK ON PAYER ACCOUNT.
           IF SOM-KEY (1:14) NOT = WS-CURR-PAYER
               PERFORM 2900-END-PAYER
               PERFORM 2050-START-PAYER
           END-IF

           MOVE 'N'                        TO WS-SO-CHANGED-IND

           IF NOT SOM-CONTROL-REC
           AND SOM-ACTIVE
           AND SOM-NEXT-DUE-DATE NOT > WS-BUS-DATE
               ADD 1                       TO WS-DUE-COUNT
               PERFORM 2100-ATTEMPT-PAYMENT THRU 2100-EXIT
           END-IF

           IF WS-SO-CHANGED
               PERFORM 2800-REWRITE-SO
           END-IF

           PERFORM 8000-READ-SO
           .
      *================================================================*
       2050-START-PAYER.
      *================================================================*
           MOVE SOM-KEY (1:14)             TO WS-CURR-PAYER
           MOVE ZERO                       TO WS-RESERVED
                                              WS-SPENDING-POWER
                                              WS-STOPPED-SO-FEE
           MOVE 'N'                        TO WS-PAYER-OK-IND
                                              WS-PAYER-CHANGED-IND
      *    THE CIF IS NOT READ (RUN TIME - 1993 DECISION), SO THE PAYER
      *    NAME IS NOT KNOWN. THE GATEWAY SUBSTITUTES 'MEGABANK CUST'
      *    FOR A BLANK FPS ORIGINATOR NAME. PIR 2008/198.
           MOVE SPACES                     TO WS-PAYER-NAME

           MOVE SOM-PAYER-SORT-CODE        TO ACM-SORT-CODE
           MOVE SOM-PAYER-ACCT-NO          TO ACM-ACCT-NO
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
                   PERFORM 2060-GET-SO-FEE
               WHEN WS-ACCTMAST-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE '2050-START-PAYER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE SOM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2060-GET-SO-FEE.
      *================================================================*
           MOVE ACM-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               MOVE PRD-STOPPED-SO-FEE     TO WS-STOPPED-SO-FEE
           END-IF
           .
      *================================================================*
       2100-ATTEMPT-PAYMENT.
      *================================================================*
           MOVE SPACES                     TO WS-EXC-CD
           SET WS-SO-CHANGED               TO TRUE
           MOVE WS-BUS-DATE                TO SOM-LAST-ATTEMPT-DATE

      *---- AMOUNT - FIRST / FINAL / REGULAR --------------------------*
           EVALUATE TRUE
               WHEN SOM-PAYMENTS-MADE = ZERO
               AND  SOM-FIRST-AMT > ZERO
                   MOVE SOM-FIRST-AMT      TO WS-PAY-AMT
               WHEN SOM-PAYMENTS-REMAINING = 1
               AND  SOM-FINAL-AMT > ZERO
                   MOVE SOM-FINAL-AMT      TO WS-PAY-AMT
               WHEN OTHER
                   MOVE SOM-REGULAR-AMT    TO WS-PAY-AMT
           END-EVALUATE

      *---- DATA CHECKS -----------------------------------------------*
           IF WS-PAY-AMT NOT > ZERO
           OR NOT SOM-FREQ-VALID
           OR NOT (SOM-ROUTE-INTERNAL OR SOM-ROUTE-FPS
                   OR SOM-ROUTE-BACS)
               MOVE 'BADD'                 TO WS-EXC-CD
               PERFORM 2600-SUSPEND-SO
               GO TO 2100-EXIT
           END-IF

      *---- PAYER ACCOUNT ---------------------------------------------*
           IF NOT WS-PAYER-FOUND
               MOVE 'ACLS'                 TO WS-EXC-CD
               PERFORM 2700-CANCEL-SO
               GO TO 2100-EXIT
           END-IF
           MOVE WS-SAVED-ACCOUNT           TO ACM-ACCOUNT-RECORD

           EVALUATE TRUE
               WHEN ACM-CLOSED
                   MOVE 'ACLS'             TO WS-EXC-CD
                   PERFORM 2700-CANCEL-SO
                   GO TO 2100-EXIT
               WHEN ACM-PENDING-CLOSE
               WHEN ACM-FROZEN
               WHEN ACM-DORMANT
               WHEN ACM-NO-DEBITS
               WHEN ACM-NO-POSTINGS
                   MOVE 'ARST'             TO WS-EXC-CD
                   PERFORM 2500-PAYMENT-FAILED
                   GO TO 2100-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

      *---- FUNDS -----------------------------------------------------*
           IF WS-PAY-AMT > WS-SPENDING-POWER - WS-RESERVED
               PERFORM 2500-PAYMENT-FAILED
               GO TO 2100-EXIT
           END-IF

      *---- INTERNAL BENEFICIARY MUST EXIST AND ACCEPT CREDITS --------*
           IF SOM-ROUTE-INTERNAL
               PERFORM 2150-CHECK-BENEFICIARY
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               IF NOT WS-BENE-OK
                   MOVE 'BENF'             TO WS-EXC-CD
                   ADD 1                   TO WS-BENE-FAIL-COUNT
                   PERFORM 2600-SUSPEND-SO
                   GO TO 2100-EXIT
               END-IF
           END-IF

           PERFORM 2200-MAKE-PAYMENT
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2150-CHECK-BENEFICIARY.
      *================================================================*
           MOVE 'N'                        TO WS-BENE-OK-IND
           MOVE SOM-BENE-SORT-CODE         TO ACM-SORT-CODE
           MOVE SOM-BENE-ACCT-NO           TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF WS-ACCTMAST-OK
               IF NOT ACM-CLOSED
               AND NOT ACM-NO-CREDITS
               AND NOT ACM-NO-POSTINGS
                   SET WS-BENE-OK          TO TRUE
               END-IF
           END-IF
           .
      *================================================================*
       2200-MAKE-PAYMENT.
      *================================================================*
           ADD WS-PAY-AMT                  TO WS-RESERVED

           EVALUATE TRUE
               WHEN SOM-ROUTE-INTERNAL
                   PERFORM 2210-POST-PAYER-DEBIT
                   PERFORM 2220-POST-BENE-CREDIT
                   MOVE SPACES             TO WS-HOST-PAY-ID
                   ADD 1                   TO WS-PAID-INTERNAL
               WHEN SOM-ROUTE-FPS
                   PERFORM 2250-ALLOCATE-PAY-ID
                   PERFORM 2210-POST-PAYER-DEBIT
                   PERFORM 2260-INSERT-PAYINSTR
                   PERFORM 2270-WRITE-FPS-RECORD
                   ADD 1                   TO WS-PAID-FPS
               WHEN SOM-ROUTE-BACS
                   PERFORM 2250-ALLOCATE-PAY-ID
                   PERFORM 2210-POST-PAYER-DEBIT
                   PERFORM 2260-INSERT-PAYINSTR
                   ADD 1                   TO WS-PAID-BACS
           END-EVALUATE

      *---- ADVANCE THE STANDING ORDER --------------------------------*
           MOVE WS-BUS-DATE                TO SOM-LAST-PAID-DATE
           MOVE WS-PAY-AMT                 TO SOM-LAST-PAID-AMT
           ADD WS-PAY-AMT                  TO SOM-TOTAL-PAID
           ADD 1                           TO SOM-PAYMENTS-MADE
           MOVE ZERO                       TO SOM-RETRY-COUNT
                                              SOM-CONSEC-FAILS
           MOVE SPACES                     TO SOM-LAST-FAIL-RSN
           MOVE WS-HOST-PAY-ID             TO SOM-LAST-PAY-ID

           IF NOT SOM-UNTIL-FURTHER-NOTICE
               SUBTRACT 1                  FROM SOM-PAYMENTS-REMAINING
           END-IF

           IF SOM-PAYMENTS-REMAINING = ZERO
           OR (SOM-FINAL-PAY-DATE NOT = ZERO
               AND SOM-FINAL-PAY-DATE <= WS-BUS-DATE)
               SET SOM-COMPLETED           TO TRUE
               ADD 1                       TO WS-COMPLETE-COUNT
               MOVE ZERO                   TO WS-NEXT-DUE
               MOVE 'PAID - FINAL PAYMENT' TO DL-NOTE
           ELSE
               PERFORM 2400-CALC-NEXT-DUE
               MOVE WS-NEXT-DUE            TO SOM-NEXT-DUE-DATE
               IF WS-HOST-PAY-ID = SPACES
                   MOVE 'INTERNAL TRANSFER' TO DL-NOTE
               ELSE
                   MOVE WS-HOST-PAY-ID     TO DL-NOTE
               END-IF
           END-IF

           ADD 1                           TO WS-PAID-COUNT
           ADD WS-PAY-AMT                  TO WS-PAID-TOTAL
           MOVE 'PAID'                     TO DL-RESULT
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2210-POST-PAYER-DEBIT.
      *================================================================*
           PERFORM 2290-INIT-POSTING
           MOVE SOM-PAYER-SORT-CODE        TO PST-SORT-CODE
           MOVE SOM-PAYER-ACCT-NO          TO PST-ACCT-NO
           SET PST-DEBIT                   TO TRUE
           MOVE SOM-BENE-NAME              TO PST-NARRATIVE-1
           STRING 'SO ' SOM-BENE-REF
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           STRING 'SO' SOM-PAYER-SORT-CODE SOM-PAYER-ACCT-NO
                  SOM-SO-SEQ 'D'
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           PERFORM 2295-WRITE-POSTING
           .
      *================================================================*
       2220-POST-BENE-CREDIT.
      *================================================================*
           PERFORM 2290-INIT-POSTING
           MOVE SOM-BENE-SORT-CODE         TO PST-SORT-CODE
           MOVE SOM-BENE-ACCT-NO           TO PST-ACCT-NO
           SET PST-CREDIT                  TO TRUE
           MOVE WS-PAYER-NAME              TO PST-NARRATIVE-1
           IF PST-NARRATIVE-1 = SPACES
               MOVE 'STANDING ORDER'       TO PST-NARRATIVE-1
           END-IF
           MOVE SOM-BENE-REF               TO PST-NARRATIVE-2
           STRING 'SO' SOM-PAYER-SORT-CODE SOM-PAYER-ACCT-NO
                  SOM-SO-SEQ 'C'
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           PERFORM 2295-WRITE-POSTING
           .
      *================================================================*
       2250-ALLOCATE-PAY-ID.
      *================================================================*
      *    SAME HOST PAYMENT ID FORMAT AS PYINIT1:
      *      'PY' || CCYYMMDD || 6-DIGIT SEQUENCE
           EXEC SQL
               SET :WS-PAY-SEQ = NEXT VALUE FOR MBK.PAYID_SEQ
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2250-ALLOCATE-PAY-ID' TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE WS-PAY-SEQ                 TO WS-PAY-SEQ-DISP
           MOVE SPACES                     TO WS-HOST-PAY-ID
           STRING 'PY' WS-BUS-DATE WS-PAY-SEQ-DISP
                  DELIMITED BY SIZE
                  INTO WS-HOST-PAY-ID
           END-STRING
           .
      *================================================================*
       2260-INSERT-PAYINSTR.
      *================================================================*
           MOVE WS-HOST-PAY-ID             TO PYI-PAY-ID
           MOVE SPACES                     TO PYI-EXT-REF
           STRING 'SO' SOM-PAYER-ACCT-NO SOM-SO-SEQ
                  DELIMITED BY SIZE
                  INTO PYI-EXT-REF
           END-STRING
           MOVE 'BATCH '                   TO PYI-CHANNEL-CD
           IF SOM-ROUTE-FPS
               MOVE 'FPS '                 TO PYI-PAY-SCHEME
               MOVE 'SNT'                  TO PYI-PAY-STATUS
           ELSE
               MOVE 'BACS'                 TO PYI-PAY-SCHEME
               MOVE 'VAL'                  TO PYI-PAY-STATUS
           END-IF
           MOVE SOM-PAYER-SORT-CODE        TO PYI-DR-SORT-CODE
           MOVE SOM-PAYER-ACCT-NO          TO PYI-DR-ACCT-NO
           MOVE SOM-BENE-NAME              TO PYI-BENE-NAME-TEXT
           MOVE 35                         TO PYI-BENE-NAME-LEN
           MOVE SOM-BENE-SORT-CODE         TO PYI-BENE-SORT-CODE
           MOVE SOM-BENE-ACCT-NO           TO PYI-BENE-ACCT-NO
           MOVE SPACES                     TO PYI-BENE-IBAN
                                              PYI-BENE-BIC
           MOVE 'GB'                       TO PYI-BENE-CTRY-CD
           MOVE 'GBP'                      TO PYI-PAY-CCY-CD
           MOVE WS-PAY-AMT                 TO PYI-PAY-AMT
                                              PYI-DR-AMT
           MOVE ZERO                       TO PYI-FX-RATE
                                              PYI-CHARGE-AMT
           MOVE 'SHA'                      TO PYI-CHARGE-BEARER
           MOVE SOM-BENE-REF               TO PYI-REMIT-REF
           MOVE 'STDO'                     TO PYI-PURPOSE-CD
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO PYI-VALUE-DATE
           MOVE SPACES                     TO PYI-SCHEME-REF
                                              PYI-REJECT-CD
           MOVE SPACES                     TO PYI-OUT-FILE-ID
           IF SOM-ROUTE-FPS
               STRING 'MBKSO' WS-BUS-DATE (3:6)
                      DELIMITED BY SIZE
                      INTO PYI-OUT-FILE-ID
               END-STRING
           END-IF
           MOVE WS-PROGRAM-ID              TO PYI-USER-ID

           EXEC SQL
               INSERT INTO MBK.PAYINSTR
                     (PAY_ID, EXT_REF, CHANNEL_CD, PAY_SCHEME,
                      PAY_STATUS, DR_SORT_CODE, DR_ACCT_NO,
                      BENE_NAME, BENE_SORT_CODE, BENE_ACCT_NO,
                      BENE_IBAN, BENE_BIC, BENE_CTRY_CD, PAY_CCY_CD,
                      PAY_AMT, FX_RATE, DR_AMT, CHARGE_AMT,
                      CHARGE_BEARER, REMIT_REF, PURPOSE_CD,
                      VALUE_DATE, SCHEME_REF, REJECT_CD,
                      CAPTURE_TS, SENT_TS, STATUS_TS, OUT_FILE_ID,
                      USER_ID)
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
                      CURRENT TIMESTAMP, NULL, CURRENT TIMESTAMP,
                      :PYI-OUT-FILE-ID, :PYI-USER-ID)
           END-EXEC

      *    THE CHANNEL/EXT_REF INDEX IS UNIQUE. A -803 MEANS THIS SO
      *    WAS ALREADY PAID TODAY BY A RUN THAT DIED BEFORE ADVANCING
      *    IT - SEE RESTART NOTES. TREAT AS FATAL SO OPS INVESTIGATE.
           IF SQLCODE NOT = 0
               MOVE '2260-INSERT-PAYINSTR' TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *================================================================*
       2270-WRITE-FPS-RECORD.
      *================================================================*
           MOVE SPACES                     TO SOF-FPS-RECORD
           SET SOF-PAYMENT                 TO TRUE
           MOVE WS-HOST-PAY-ID             TO SOFP-HOST-PAY-ID
           MOVE SOM-PAYER-SORT-CODE        TO SOFP-DR-SORT-CODE
           MOVE SOM-PAYER-ACCT-NO          TO SOFP-DR-ACCT-NO
           MOVE WS-PAYER-NAME              TO SOFP-DR-NAME
           MOVE SOM-BENE-SORT-CODE         TO SOFP-CR-SORT-CODE
           MOVE SOM-BENE-ACCT-NO           TO SOFP-CR-ACCT-NO
           MOVE SOM-BENE-NAME              TO SOFP-CR-NAME
           MOVE WS-PAY-AMT                 TO SOFP-AMOUNT
           MOVE SOM-BENE-REF               TO SOFP-REFERENCE
           MOVE WS-BUS-DATE                TO SOFP-VALUE-DATE
           MOVE SOM-KEY                    TO SOFP-SO-KEY
           WRITE FPSOUT-RECORD             FROM SOF-FPS-RECORD
           IF WS-FPSOUT-STATUS NOT = '00'
               MOVE '2270-WRITE-FPS-RECORD' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'FPSOUT WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-FPSOUT-STATUS       TO ABN-FILE-STATUS
               EXEC SQL ROLLBACK END-EXEC
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-FPS-COUNT
           ADD WS-PAY-AMT                  TO WS-FPS-TOTAL
           .
      *================================================================*
       2290-INIT-POSTING.
      *================================================================*
      *    STANDING ORDERS POST EARLY IN THE DAY (TIME 00:01) SO THAT,
      *    WHEN SORTED WITH THE MEMO LOG, THEY COME FIRST - THE FUNDS
      *    CHECK ABOVE WAS DONE AGAINST THE START-OF-DAY POSITION.
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'SO  '                     TO PST-TXN-CODE
           MOVE WS-PAY-AMT                 TO PST-AMOUNT
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           .
      *================================================================*
       2295-WRITE-POSTING.
      *================================================================*
           WRITE SOPOST-RECORD             FROM PST-POSTING-RECORD
           IF WS-SOPOST-STATUS NOT = '00'
               MOVE '2295-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SOPOST WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-SOPOST-STATUS       TO ABN-FILE-STATUS
               MOVE SOM-KEY                TO ABN-KEY-DATA
               EXEC SQL ROLLBACK END-EXEC
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-POSTING-COUNT
           .
      *================================================================*
       2400-CALC-NEXT-DUE.
      *================================================================*
      *    ADVANCE THE NOMINAL DUE DATE BY ONE PERIOD, THEN ROLL THE
      *    RESULT FORWARD TO A BUSINESS DAY. THE NOMINAL DATE IS KEPT
      *    SO MONTHLY ORDERS DO NOT DRIFT (E.G. 31ST -> 28TH -> 28TH).
           IF SOM-NOMINAL-DUE-DATE NUMERIC
           AND SOM-NOMINAL-DUE-DATE NOT = ZERO
               MOVE SOM-NOMINAL-DUE-DATE   TO WS-NOMINAL
           ELSE
               MOVE SOM-NEXT-DUE-DATE      TO WS-NOMINAL
           END-IF

           EVALUATE TRUE
               WHEN SOM-WEEKLY
                   MOVE 7                  TO DTE-DAYS
                   PERFORM 2410-ADD-DAYS
               WHEN SOM-FORTNIGHTLY
                   MOVE 14                 TO DTE-DAYS
                   PERFORM 2410-ADD-DAYS
               WHEN SOM-FOUR-WEEKLY
                   MOVE 28                 TO DTE-DAYS
                   PERFORM 2410-ADD-DAYS
               WHEN SOM-MONTHLY
                   MOVE 1                  TO WS-ADD-MONTHS
                   PERFORM 2420-ADD-MONTHS
               WHEN SOM-QUARTERLY
                   MOVE 3                  TO WS-ADD-MONTHS
                   PERFORM 2420-ADD-MONTHS
               WHEN SOM-HALF-YEARLY
                   MOVE 6                  TO WS-ADD-MONTHS
                   PERFORM 2420-ADD-MONTHS
               WHEN SOM-ANNUALLY
                   MOVE 12                 TO WS-ADD-MONTHS
                   PERFORM 2420-ADD-MONTHS
           END-EVALUATE

      *    CATCH UP - IF THE ORDER WAS SUSPENDED FOR A WHILE THE NEW
      *    NOMINAL DATE MAY STILL BE IN THE PAST. ONLY ONE PAYMENT IS
      *    EVER MADE PER RUN; THE MISSED PERIODS ARE NOT PAID.
           MOVE ZERO                       TO WS-LOOP-GUARD
           PERFORM UNTIL WS-NOMINAL > WS-BUS-DATE
                      OR WS-LOOP-GUARD > 60
               EVALUATE TRUE
                   WHEN SOM-WEEKLY
                       MOVE 7              TO DTE-DAYS
                       PERFORM 2410-ADD-DAYS
                   WHEN SOM-FORTNIGHTLY
                       MOVE 14             TO DTE-DAYS
                       PERFORM 2410-ADD-DAYS
                   WHEN SOM-FOUR-WEEKLY
                       MOVE 28             TO DTE-DAYS
                       PERFORM 2410-ADD-DAYS
                   WHEN OTHER
                       PERFORM 2420-ADD-MONTHS
               END-EVALUATE
               ADD 1                       TO WS-LOOP-GUARD
           END-PERFORM

           MOVE WS-NOMINAL                 TO SOM-NOMINAL-DUE-DATE
           MOVE WS-NOMINAL                 TO WS-NEXT-DUE
           PERFORM 2430-ROLL-TO-BUS-DAY
           .
      *================================================================*
       2410-ADD-DAYS.
      *================================================================*
           MOVE 'D'                        TO DTE-FUNCTION
           MOVE WS-NOMINAL                 TO DTE-DATE-IN
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           IF NOT DTE-OK
               MOVE '2410-ADD-DAYS'        TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'DTEUTIL FAILED - NOMINAL DUE DATE INVALID'
                                           TO ABN-MESSAGE
               MOVE SOM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           MOVE DTE-DATE-OUT               TO WS-NOMINAL
           .
      *================================================================*
       2420-ADD-MONTHS.
      *================================================================*
           COMPUTE WS-MONTH-TOTAL = WS-NM-MM + WS-ADD-MONTHS - 1
           DIVIDE WS-MONTH-TOTAL BY 12 GIVING WS-LEAP-QUOT
                                       REMAINDER WS-MONTH-TOTAL
           ADD WS-LEAP-QUOT                TO WS-NM-CCYY
           COMPUTE WS-NM-MM = WS-MONTH-TOTAL + 1

           MOVE WS-MONTH-DAYS (WS-NM-MM)   TO WS-DAYS-IN-MONTH
           IF WS-NM-MM = 2
               DIVIDE WS-NM-CCYY BY 4   GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-4
               DIVIDE WS-NM-CCYY BY 100 GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-100
               DIVIDE WS-NM-CCYY BY 400 GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-400
               IF (WS-LEAP-REM-4 = 0 AND WS-LEAP-REM-100 NOT = 0)
               OR WS-LEAP-REM-400 = 0
                   MOVE 29                 TO WS-DAYS-IN-MONTH
               END-IF
           END-IF

      *    USE THE ORIGINAL DAY OF MONTH, CAPPED AT THE MONTH END.
           IF SOM-NOMINAL-DAY NUMERIC
           AND SOM-NOMINAL-DAY > ZERO
               IF SOM-NOMINAL-DAY > WS-DAYS-IN-MONTH
                   MOVE WS-DAYS-IN-MONTH   TO WS-NM-DD
               ELSE
                   MOVE SOM-NOMINAL-DAY    TO WS-NM-DD
               END-IF
           ELSE
               IF WS-NM-DD > WS-DAYS-IN-MONTH
                   MOVE WS-DAYS-IN-MONTH   TO WS-NM-DD
               END-IF
           END-IF
           .
      *================================================================*
       2430-ROLL-TO-BUS-DAY.
      *================================================================*
           MOVE ZERO                       TO WS-LOOP-GUARD
           MOVE 'B'                        TO DTE-FUNCTION
           MOVE WS-NEXT-DUE                TO DTE-DATE-IN
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           PERFORM UNTIL DTE-BUS-DAY-IND = 'Y'
                      OR WS-LOOP-GUARD > 10
               MOVE 'D'                    TO DTE-FUNCTION
               MOVE WS-NEXT-DUE            TO DTE-DATE-IN
               MOVE 1                      TO DTE-DAYS
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               MOVE DTE-DATE-OUT           TO WS-NEXT-DUE
               MOVE 'B'                    TO DTE-FUNCTION
               MOVE WS-NEXT-DUE            TO DTE-DATE-IN
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               ADD 1                       TO WS-LOOP-GUARD
           END-PERFORM
           .
      *================================================================*
       2500-PAYMENT-FAILED.
      *================================================================*
      *    ON ENTRY WS-EXC-CD IS 'ARST' OR SPACES (= INSUFFICIENT FUNDS)
           MOVE WS-BUS-DATE                TO SOM-LAST-ATTEMPT-DATE

           IF SOM-RETRY-COUNT < WS-MAX-RETRIES
           AND WS-EXC-CD = SPACES
      *        FIRST FAILURE - RETRY ON THE NEXT BUSINESS DAY.
               ADD 1                       TO SOM-RETRY-COUNT
               MOVE 'NSF '                 TO SOM-LAST-FAIL-RSN
               MOVE WS-NEXT-BUS-DATE       TO SOM-NEXT-DUE-DATE
               MOVE 'NSFR'                 TO WS-EXC-CD
               ADD 1                       TO WS-RETRY-COUNT
               MOVE 'RETRY'                TO DL-RESULT
               MOVE 'WILL RETRY NEXT BUSINESS DAY' TO DL-NOTE
               MOVE SOM-NEXT-DUE-DATE      TO WS-NEXT-DUE
               PERFORM 7100-REPORT-DETAIL
               PERFORM 2550-WRITE-EXCEPTION
           ELSE
      *        FINAL FAILURE - THIS PAYMENT IS MISSED.
               IF WS-EXC-CD = SPACES
                   MOVE 'NSFF'             TO WS-EXC-CD
                   MOVE 'NSF '             TO SOM-LAST-FAIL-RSN
               ELSE
                   MOVE WS-EXC-CD          TO SOM-LAST-FAIL-RSN
               END-IF
               ADD 1                       TO SOM-CONSEC-FAILS
               MOVE ZERO                   TO SOM-RETRY-COUNT
               ADD 1                       TO WS-MISSED-COUNT
               ADD WS-PAY-AMT              TO WS-MISSED-TOTAL
               PERFORM 2560-COUNT-UNPAID-ITEM

               IF SOM-CONSEC-FAILS >= WS-SUSPEND-AFTER
                   MOVE 'SUSP'             TO WS-EXC-CD
                   PERFORM 2600-SUSPEND-SO
               ELSE
                   PERFORM 2400-CALC-NEXT-DUE
                   MOVE WS-NEXT-DUE        TO SOM-NEXT-DUE-DATE
                   MOVE 'MISSED'           TO DL-RESULT
                   IF WS-EXC-CD = 'ARST'
                       MOVE 'ACCOUNT RESTRICTED' TO DL-NOTE
                   ELSE
                       MOVE 'INSUFFICIENT FUNDS' TO DL-NOTE
                   END-IF
                   PERFORM 7100-REPORT-DETAIL
                   PERFORM 2550-WRITE-EXCEPTION
               END-IF
           END-IF
           .
      *================================================================*
       2550-WRITE-EXCEPTION.
      *================================================================*
           MOVE SPACES                     TO SOX-EXCEPTION-RECORD
           MOVE SOM-PAYER-SORT-CODE        TO SOX-PAYER-SORT-CODE
           MOVE SOM-PAYER-ACCT-NO          TO SOX-PAYER-ACCT-NO
           MOVE SOM-SO-SEQ                 TO SOX-SO-SEQ
           MOVE WS-BUS-DATE                TO SOX-BUS-DATE
           MOVE WS-EXC-CD                  TO SOX-EXC-CD
      *    NO LETTER FOR A RETRY - THE CUSTOMER MAY STILL PAY IN.
           IF SOX-NSF-RETRY
               MOVE 'N'                    TO SOX-LETTER-IND
           ELSE
               MOVE 'Y'                    TO SOX-LETTER-IND
           END-IF
           IF WS-PAYER-FOUND
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               MOVE ACM-CIF-NO             TO SOX-CIF-NO
           END-IF
           MOVE SOM-BENE-NAME              TO SOX-BENE-NAME
           MOVE SOM-BENE-SORT-CODE         TO SOX-BENE-SORT-CODE
           MOVE SOM-BENE-ACCT-NO           TO SOX-BENE-ACCT-NO
           MOVE SOM-BENE-REF               TO SOX-BENE-REF
           MOVE WS-PAY-AMT                 TO SOX-AMOUNT
           MOVE SOM-NEXT-DUE-DATE          TO SOX-DUE-DATE
           COMPUTE SOX-AVAILABLE = WS-SPENDING-POWER - WS-RESERVED
           IF SOM-ACTIVE
               MOVE SOM-NEXT-DUE-DATE      TO SOX-NEXT-DUE-DATE
           ELSE
               MOVE ZERO                   TO SOX-NEXT-DUE-DATE
           END-IF
           MOVE SOM-CONSEC-FAILS           TO SOX-CONSEC-FAILS
           IF SOX-NSF-FINAL OR SOX-SUSPENDED
               MOVE WS-STOPPED-SO-FEE      TO SOX-FEE-AMT
           ELSE
               MOVE ZERO                   TO SOX-FEE-AMT
           END-IF

           WRITE EXCEPTS-RECORD            FROM SOX-EXCEPTION-RECORD
           IF WS-EXCEPTS-STATUS NOT = '00'
               MOVE '2550-WRITE-EXCEPTION' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'EXCEPTS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-EXCEPTS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2560-COUNT-UNPAID-ITEM.
      *================================================================*
      *    A MISSED STANDING ORDER COUNTS AS AN UNPAID ITEM FOR THE
      *    CHARGING PERIOD (CGCAL010). THE ACCOUNT IS REWRITTEN AT THE
      *    END OF THE PAYER GROUP.
           IF WS-PAYER-FOUND
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               IF ACM-UNPAID-ITEMS NOT NUMERIC
                   MOVE ZERO               TO ACM-UNPAID-ITEMS
               END-IF
               IF ACM-UNPAID-ITEMS < 999
                   ADD 1                   TO ACM-UNPAID-ITEMS
               END-IF
               MOVE ACM-ACCOUNT-RECORD     TO WS-SAVED-ACCOUNT
               SET WS-PAYER-CHANGED        TO TRUE
           END-IF
           .
      *================================================================*
       2600-SUSPEND-SO.
      *================================================================*
           SET SOM-SUSPENDED               TO TRUE
           MOVE WS-EXC-CD                  TO SOM-LAST-FAIL-RSN
           SET WS-SO-CHANGED               TO TRUE
           ADD 1                           TO WS-SUSPEND-COUNT
           MOVE 'SUSPEND'                  TO DL-RESULT
           EVALUATE WS-EXC-CD
               WHEN 'SUSP'
                   MOVE '3 CONSECUTIVE PAYMENTS MISSED' TO DL-NOTE
               WHEN 'BENF'
                   MOVE 'BENEFICIARY ACCOUNT INVALID' TO DL-NOTE
               WHEN 'BADD'
                   MOVE 'INVALID SO DATA - REFER DDA OPS' TO DL-NOTE
               WHEN OTHER
                   MOVE WS-EXC-CD          TO DL-NOTE
           END-EVALUATE
           MOVE ZERO                       TO WS-NEXT-DUE
           PERFORM 7100-REPORT-DETAIL
           PERFORM 2550-WRITE-EXCEPTION
           .
      *================================================================*
       2700-CANCEL-SO.
      *================================================================*
           SET SOM-CANCELLED               TO TRUE
           MOVE WS-BUS-DATE                TO SOM-CANCEL-DATE
           MOVE 'ACLS'                     TO SOM-CANCEL-RSN
           MOVE 'BATCH '                   TO SOM-AMEND-CHANNEL
           MOVE WS-PROGRAM-ID              TO SOM-AMEND-USER
           MOVE WS-BUS-DATE                TO SOM-AMEND-DATE
           SET WS-SO-CHANGED               TO TRUE
           ADD 1                           TO WS-CANCEL-COUNT
           MOVE 'CANCEL'                   TO DL-RESULT
           MOVE 'PAYER ACCOUNT CLOSED'     TO DL-NOTE
           MOVE ZERO                       TO WS-NEXT-DUE
           PERFORM 7100-REPORT-DETAIL
           PERFORM 2550-WRITE-EXCEPTION
           .
      *================================================================*
       2800-REWRITE-SO.
      *================================================================*
           REWRITE SOM-SO-RECORD
           IF NOT WS-SOMAST-OK
               MOVE '2800-REWRITE-SO'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SOMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-SOMAST-STATUS       TO ABN-FILE-STATUS
               MOVE SOM-KEY                TO ABN-KEY-DATA
               EXEC SQL ROLLBACK END-EXEC
               PERFORM Z999-ABEND
           END-IF

           ADD 1                           TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
               EXEC SQL COMMIT END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
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
                   MOVE WS-CURR-PAYER      TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
               MOVE 'N'                    TO WS-PAYER-CHANGED-IND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC

           MOVE SPACES                     TO SOF-FPS-RECORD
           SET SOF-TRAILER                 TO TRUE
           MOVE WS-FPS-COUNT               TO SOFT-PAY-COUNT
           MOVE WS-FPS-TOTAL               TO SOFT-PAY-TOTAL
           WRITE FPSOUT-RECORD             FROM SOF-FPS-RECORD

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'STANDING ORDER RECORDS READ'  TO TL-LABEL
           MOVE WS-READ-COUNT                  TO TL-COUNT
           MOVE ZERO                           TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE 'DUE TODAY'                    TO TL-LABEL
           MOVE WS-DUE-COUNT                   TO TL-COUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE 'PAID'                         TO TL-LABEL
           MOVE WS-PAID-COUNT                  TO TL-COUNT
           MOVE WS-PAID-TOTAL                  TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE '  INTERNAL'                   TO TL-LABEL
           MOVE WS-PAID-INTERNAL               TO TL-COUNT
           MOVE ZERO                           TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE '  FASTER PAYMENTS'            TO TL-LABEL
           MOVE WS-PAID-FPS                    TO TL-COUNT
           MOVE WS-FPS-TOTAL                   TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE '  BACS'                       TO TL-LABEL
           MOVE WS-PAID-BACS                   TO TL-COUNT
           MOVE ZERO                           TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE 'RETRY TOMORROW'               TO TL-LABEL
           MOVE WS-RETRY-COUNT                 TO TL-COUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE 'MISSED'                       TO TL-LABEL
           MOVE WS-MISSED-COUNT                TO TL-COUNT
           MOVE WS-MISSED-TOTAL                TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE 'SUSPENDED'                    TO TL-LABEL
           MOVE WS-SUSPEND-COUNT               TO TL-COUNT
           MOVE ZERO                           TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE 'CANCELLED - ACCOUNT CLOSED'   TO TL-LABEL
           MOVE WS-CANCEL-COUNT                TO TL-COUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE 'COMPLETED (FINAL PAYMENT)'    TO TL-LABEL
           MOVE WS-COMPLETE-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
           MOVE 'POSTINGS WRITTEN'             TO TL-LABEL
           MOVE WS-POSTING-COUNT               TO TL-COUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE

           CLOSE PRDPARM-FILE
                 SOMAST-FILE
                 ACCTMAST-FILE
                 SOPOST-FILE
                 FPSOUT-FILE
                 EXCEPTS-FILE
                 REPORT-FILE

           DISPLAY 'SOEXE010 - SO RECORDS READ ' WS-READ-COUNT
           DISPLAY 'SOEXE010 - DUE             ' WS-DUE-COUNT
           DISPLAY 'SOEXE010 - PAID            ' WS-PAID-COUNT
           DISPLAY 'SOEXE010 - MISSED          ' WS-MISSED-COUNT
           DISPLAY 'SOEXE010 - SUSPENDED       ' WS-SUSPEND-COUNT

           IF WS-SUSPEND-COUNT > ZERO
           OR WS-BENE-FAIL-COUNT > ZERO
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
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SOM-PAYER-SORT-CODE        TO DL-SORT-CODE
           MOVE SOM-PAYER-ACCT-NO          TO DL-ACCT-NO
           MOVE SOM-SO-SEQ                 TO DL-SO-SEQ
           MOVE SOM-ROUTE-CD               TO DL-ROUTE
           MOVE SOM-BENE-NAME              TO DL-BENE-NAME
           MOVE WS-PAY-AMT                 TO DL-AMOUNT
           MOVE SOM-LAST-ATTEMPT-DATE      TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO DL-DUE-DATE
           IF WS-NEXT-DUE NUMERIC AND WS-NEXT-DUE NOT = ZERO
               MOVE WS-NEXT-DUE            TO WS-DATE-WORK
               PERFORM 8900-FORMAT-DB2-DATE
               MOVE WS-DATE-DB2            TO DL-NEXT-DUE
           ELSE
               MOVE SPACES                 TO DL-NEXT-DUE
           END-IF
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-SO.
      *================================================================*
           READ SOMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-SOMAST-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-SOMAST-EOF
                   SET WS-END-OF-SOS       TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-SO'     TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SOMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-SOMAST-STATUS   TO ABN-FILE-STATUS
                   EXEC SQL ROLLBACK END-EXEC
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8900-FORMAT-DB2-DATE.
      *================================================================*
           MOVE SPACES                     TO WS-DATE-DB2
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-DATE-DB2
           END-STRING
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           MOVE SOM-KEY                    TO ABN-KEY-DATA
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
