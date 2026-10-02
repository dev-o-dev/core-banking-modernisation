       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RGAML010.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2003-05-09.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : RGAML010                                            *
      * TITLE    : ANTI MONEY LAUNDERING - DAILY TRANSACTION RULES     *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : RGAML010                                            *
      * JCL      : RGAML01 STEP RA010                                  *
      *----------------------------------------------------------------*
      * INPUT    : MBK.TXNHIST  - TODAY'S HARD POSTINGS (CURSOR)       *
      *            ACCTMAST     - DDA ACCOUNT MASTER (RANDOM)          *
      *            CIFMAST      - CIF CUSTOMER MASTER (RANDOM)         *
      *            RULEPARM     - RULE THRESHOLD OVERRIDES (OPTIONAL)  *
      *            DATECARD     - BUSINESS DATE CONTROL CARD           *
      * OUTPUT   : MBK.AMLALERT (INSERT) - ALERTS FOR THE MLRO TEAM    *
      *            ALERTS   - ALERT FILE FOR THE CASE MANAGEMENT TOOL  *
      *                       (NORKOM) - SAME CONTENT AS THE TABLE     *
      *            REPORT   - DAILY AML RULES SUMMARY (FBA 133)        *
      *----------------------------------------------------------------*
      * MONEY LAUNDERING REGULATIONS 2007 REG 8 (ONGOING MONITORING).  *
      * THE RULES BELOW WERE AGREED WITH THE MLRO IN 2003 AND HAVE     *
      * BEEN TUNED SINCE. THRESHOLDS ARE IN WS-RULE-TABLE AND CAN BE   *
      * OVERRIDDEN WITHOUT A CODE CHANGE BY A CARD IN RULEPARM:        *
      *     RNN THRESHOLD=NNNNNNNNNNNNN COUNT=NNN ACTIVE=Y             *
      *                                                                *
      *   R01 LARGE CASH         SINGLE CASH CREDIT >= THRESHOLD       *
      *   R02 STRUCTURING        >= COUNT CASH CREDITS, EACH BELOW THE *
      *                          R01 THRESHOLD, TOTAL >= THRESHOLD     *
      *   R03 RAPID MOVEMENT     CREDITS IN >= THRESHOLD AND DEBITS    *
      *                          OUT >= 90% OF CREDITS, SAME DAY       *
      *   R04 HIGH RISK CUSTOMER ANY TRANSACTION >= THRESHOLD ON A     *
      *                          CUSTOMER RATED HIGH RISK              *
      *   R05 DORMANT REACTIVATE DEBITS >= THRESHOLD ON AN ACCOUNT     *
      *                          REACTIVATED IN THE LAST 30 DAYS       *
      *   R06 INTERNATIONAL      SWIFT OUT TOTAL >= THRESHOLD          *
      *   R07 VELOCITY           >= COUNT FASTER PAYMENTS OUT          *
      *                                                                *
      * ONE ALERT PER ACCOUNT PER RULE PER DAY. ALERTS ARE ONLY RAISED *
      * ON HARD-POSTED TRANSACTIONS - SAME-DAY ONLINE ACTIVITY IS SEEN *
      * THE NEXT MORNING. REAL-TIME SCREENING IS A SEPARATE (VENDOR)   *
      * SYSTEM ON THE PAYMENTS GATEWAY.                                *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2003-05-09 SAT  ORIGINAL VERSION - R01, R02 (MLR 2003)         *
      * 2004-11-22 SAT  R03 RAPID MOVEMENT, R04 HIGH RISK              *
      * 2008-03-10 AOS  R06, R07 (FASTER PAYMENTS FRAUD TYPOLOGIES)    *
      * 2009-02-02 JNB  R05 DORMANT REACTIVATION (MLR 2007 REVIEW)     *
      * 2009-09-14 AOS  RULE THRESHOLDS FROM RULEPARM CARDS            *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
           SELECT RULEPARM-FILE    ASSIGN TO RULEPARM
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RULEPARM-STATUS.
           SELECT ALERTS-FILE      ASSIGN TO ALERTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ALERTS-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  RULEPARM-FILE
           RECORDING MODE IS F.
       01  RULEPARM-RECORD.
           05  RPM-RULE-ID                 PIC X(03).
           05  FILLER                      PIC X(01).
           05  RPM-THRESH-KW               PIC X(10).
           05  RPM-THRESHOLD               PIC 9(11)V99.
           05  FILLER                      PIC X(01).
           05  RPM-COUNT-KW                PIC X(06).
           05  RPM-COUNT                   PIC 9(03).
           05  FILLER                      PIC X(01).
           05  RPM-ACTIVE-KW               PIC X(07).
           05  RPM-ACTIVE                  PIC X(01).
           05  FILLER                      PIC X(34).
       FD  ALERTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ALERTS-RECORD                   PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'RGAML010'.
      *
      *---- RULE TABLE (DEFAULTS) -------------------------------------*
       01  WS-RULE-VALUES.
           05  FILLER PIC X(03) VALUE 'R01'.
           05  FILLER PIC 9(11)V99 VALUE 10000.00.
           05  FILLER PIC 9(03) VALUE 001.
           05  FILLER PIC X(01) VALUE 'Y'.
           05  FILLER PIC X(01) VALUE 'H'.
           05  FILLER PIC X(40) VALUE 'LARGE CASH DEPOSIT'.
           05  FILLER PIC X(03) VALUE 'R02'.
           05  FILLER PIC 9(11)V99 VALUE 9000.00.
           05  FILLER PIC 9(03) VALUE 003.
           05  FILLER PIC X(01) VALUE 'Y'.
           05  FILLER PIC X(01) VALUE 'H'.
           05  FILLER PIC X(40) VALUE 'POSSIBLE STRUCTURING OF CASH'.
           05  FILLER PIC X(03) VALUE 'R03'.
           05  FILLER PIC 9(11)V99 VALUE 5000.00.
           05  FILLER PIC 9(03) VALUE 001.
           05  FILLER PIC X(01) VALUE 'Y'.
           05  FILLER PIC X(01) VALUE 'M'.
           05  FILLER PIC X(40) VALUE 'RAPID MOVEMENT OF FUNDS'.
           05  FILLER PIC X(03) VALUE 'R04'.
           05  FILLER PIC 9(11)V99 VALUE 2000.00.
           05  FILLER PIC 9(03) VALUE 001.
           05  FILLER PIC X(01) VALUE 'Y'.
           05  FILLER PIC X(01) VALUE 'M'.
           05  FILLER PIC X(40) VALUE 'HIGH RISK CUSTOMER ACTIVITY'.
           05  FILLER PIC X(03) VALUE 'R05'.
           05  FILLER PIC 9(11)V99 VALUE 1000.00.
           05  FILLER PIC 9(03) VALUE 001.
           05  FILLER PIC X(01) VALUE 'Y'.
           05  FILLER PIC X(01) VALUE 'M'.
           05  FILLER PIC X(40) VALUE 'DEBITS AFTER DORMANT REACTIVATE'.
           05  FILLER PIC X(03) VALUE 'R06'.
           05  FILLER PIC 9(11)V99 VALUE 15000.00.
           05  FILLER PIC 9(03) VALUE 001.
           05  FILLER PIC X(01) VALUE 'Y'.
           05  FILLER PIC X(01) VALUE 'L'.
           05  FILLER PIC X(40) VALUE 'HIGH VALUE OVERSEAS PAYMENTS'.
           05  FILLER PIC X(03) VALUE 'R07'.
           05  FILLER PIC 9(11)V99 VALUE 0.
           05  FILLER PIC 9(03) VALUE 020.
           05  FILLER PIC X(01) VALUE 'Y'.
           05  FILLER PIC X(01) VALUE 'L'.
           05  FILLER PIC X(40) VALUE 'HIGH VELOCITY FPS PAYMENTS OUT'.
       01  WS-RULE-TABLE REDEFINES WS-RULE-VALUES.
           05  WS-RULE                     OCCURS 7 TIMES
                                           INDEXED BY RL-IDX.
               10  WS-RL-ID                PIC X(03).
               10  WS-RL-THRESHOLD         PIC 9(11)V99.
               10  WS-RL-COUNT             PIC 9(03).
               10  WS-RL-ACTIVE            PIC X(01).
               10  WS-RL-SEVERITY          PIC X(01).
               10  WS-RL-DESC              PIC X(40).
       01  WS-RULE-HITS.
           05  WS-RL-HIT-COUNT             PIC S9(07) COMP-3
                                           OCCURS 7 TIMES.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-RULEPARM-STATUS          PIC X(02).
               88  WS-RULEPARM-OK              VALUE '00'.
               88  WS-RULEPARM-EOF             VALUE '10'.
           05  WS-ALERTS-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOC-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CURSOR            VALUE 'Y'.
           05  WS-ACCT-READ-IND            PIC X(01).
               88  WS-ACCT-READ                VALUE 'Y'.
           05  WS-CUST-READ-IND            PIC X(01).
               88  WS-CUST-READ                VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-DB2             PIC X(10).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-DORMANT-INT              PIC S9(09) COMP.
           05  WS-CURR-ACCT                PIC X(14) VALUE LOW-VALUES.
           05  WS-ALERT-SEQ                PIC 9(06) VALUE ZERO.
           05  WS-RULE-NO                  PIC S9(04) COMP.
           05  WS-HIT-TOTAL                PIC S9(13)V99 COMP-3.
           05  WS-HIT-COUNT                PIC S9(07) COMP-3.
           05  WS-NINETY-PCT               PIC S9(13)V99 COMP-3.
           05  WS-AMOUNT-EDIT              PIC ZZZ,ZZZ,ZZ9.99.
      *
      *---- PER-ACCOUNT ACCUMULATORS ----------------------------------*
       01  WS-ACCT-TOTALS.
           05  WS-AT-TXN-COUNT             PIC S9(07) COMP-3.
           05  WS-AT-CASH-IN-TOTAL         PIC S9(13)V99 COMP-3.
           05  WS-AT-CASH-IN-COUNT         PIC S9(07) COMP-3.
           05  WS-AT-CASH-IN-MAX           PIC S9(13)V99 COMP-3.
           05  WS-AT-CASH-UNDER-TOTAL      PIC S9(13)V99 COMP-3.
           05  WS-AT-CASH-UNDER-COUNT      PIC S9(07) COMP-3.
           05  WS-AT-CR-TOTAL              PIC S9(13)V99 COMP-3.
           05  WS-AT-DR-TOTAL              PIC S9(13)V99 COMP-3.
           05  WS-AT-SWO-TOTAL             PIC S9(13)V99 COMP-3.
           05  WS-AT-SWO-COUNT             PIC S9(07) COMP-3.
           05  WS-AT-FPO-COUNT             PIC S9(07) COMP-3.
           05  WS-AT-FPO-TOTAL             PIC S9(13)V99 COMP-3.
           05  WS-AT-MAX-TXN               PIC S9(13)V99 COMP-3.
           05  WS-AT-OVER-R04-COUNT        PIC S9(07) COMP-3.
           05  WS-AT-OVER-R04-TOTAL        PIC S9(13)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-TXN-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCOUNTS-SEEN            PIC S9(09) COMP-3 VALUE 0.
           05  WS-ALERTS-RAISED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-DUP-ALERTS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-OVERRIDES                PIC S9(05) COMP-3 VALUE 0.
      *
      *---- ALERT FILE RECORD (NORKOM INTERFACE NK-IN-04) -------------*
       01  WS-ALERT-RECORD.
           05  ALR-ALERT-ID                PIC X(16).
           05  ALR-ALERT-DATE              PIC 9(08).
           05  ALR-RULE-ID                 PIC X(04).
           05  ALR-SEVERITY                PIC X(01).
           05  ALR-SORT-CODE               PIC X(06).
           05  ALR-ACCT-NO                 PIC X(08).
           05  ALR-CIF-NO                  PIC X(12).
           05  ALR-CUST-NAME               PIC X(40).
           05  ALR-RISK-RATING             PIC X(01).
           05  ALR-TXN-COUNT               PIC 9(05).
           05  ALR-TXN-TOTAL               PIC 9(13)V99.
           05  ALR-THRESHOLD               PIC 9(13)V99.
           05  ALR-DESCRIPTION             PIC X(60).
           05  FILLER                      PIC X(09).
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'RGAML010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - AML DAILY RULES - RESTRICTED'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(18) VALUE 'ALERT ID'.
           05  FILLER            PIC X(05) VALUE 'RULE'.
           05  FILLER            PIC X(04) VALUE 'SEV'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(14) VALUE 'CIF'.
           05  FILLER            PIC X(04) VALUE 'RSK'.
           05  FILLER            PIC X(07) VALUE ' COUNT'.
           05  FILLER            PIC X(17) VALUE '           TOTAL'.
           05  FILLER            PIC X(44) VALUE '  DESCRIPTION'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-ALERT-ID                 PIC X(16).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RULE                     PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SEV                      PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RISK                     PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-COUNT                    PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TOTAL                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DESC                     PIC X(40).
           05  FILLER                      PIC X(02) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(50).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(71) VALUE SPACES.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
           EXEC SQL INCLUDE DCLAMLA END-EXEC.
      *
       01  HV-POST-DATE                    PIC X(10).
      *
           EXEC SQL DECLARE AMLCSR CURSOR FOR
               SELECT SORT_CODE, ACCT_NO, TXN_CODE, DR_CR_IND,
                      TXN_AMT
                 FROM MBK.TXNHIST
                WHERE POST_DATE = :HV-POST-DATE
                  AND TXN_CODE NOT IN ('INT ', 'TAX ', 'CHG ')
                ORDER BY SORT_CODE, ACCT_NO, POST_SEQ
                FOR FETCH ONLY
                WITH UR
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
           PERFORM 2000-SCAN-TRANSACTIONS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       CIFMAST-FILE
                       RULEPARM-FILE
                       DATECARD-FILE
                OUTPUT ALERTS-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR WS-ALERTS-STATUS NOT = '00'
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
                  INTO WS-BUS-DATE-DB2
           END-STRING
           MOVE WS-BUS-DATE-DB2            TO HD1-BUS-DATE
                                              HV-POST-DATE
           COMPUTE WS-BUS-DATE-INT =
                   FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)

           PERFORM VARYING WS-RULE-NO FROM 1 BY 1 UNTIL WS-RULE-NO > 7
               MOVE ZERO               TO WS-RL-HIT-COUNT (WS-RULE-NO)
           END-PERFORM

           PERFORM 1100-LOAD-OVERRIDES THRU 1100-EXIT
           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       1100-LOAD-OVERRIDES.
      *================================================================*
      *    RULEPARM IS OPTIONAL (DD DUMMY IF NOT USED).
           IF NOT WS-RULEPARM-OK
               GO TO 1100-EXIT
           END-IF
           READ RULEPARM-FILE
           PERFORM UNTIL NOT WS-RULEPARM-OK
               IF RPM-RULE-ID (1:1) NOT = '*'
                   SET RL-IDX              TO 1
                   SEARCH WS-RULE
                       AT END
                           DISPLAY 'RGAML010 W001 UNKNOWN RULE IN '
                                   'RULEPARM: ' RPM-RULE-ID
                       WHEN WS-RL-ID (RL-IDX) = RPM-RULE-ID
                           IF RPM-THRESHOLD NUMERIC
                               MOVE RPM-THRESHOLD
                                   TO WS-RL-THRESHOLD (RL-IDX)
                           END-IF
                           IF RPM-COUNT NUMERIC
                               MOVE RPM-COUNT TO WS-RL-COUNT (RL-IDX)
                           END-IF
                           IF RPM-ACTIVE = 'Y' OR 'N'
                               MOVE RPM-ACTIVE
                                   TO WS-RL-ACTIVE (RL-IDX)
                           END-IF
                           ADD 1           TO WS-OVERRIDES
                           DISPLAY 'RGAML010 RULE ' RPM-RULE-ID
                                   ' OVERRIDE THRESHOLD '
                                   WS-RL-THRESHOLD (RL-IDX)
                                   ' COUNT ' WS-RL-COUNT (RL-IDX)
                                   ' ACTIVE ' WS-RL-ACTIVE (RL-IDX)
                   END-SEARCH
               END-IF
               READ RULEPARM-FILE
           END-PERFORM
           CLOSE RULEPARM-FILE
           .
       1100-EXIT.
           EXIT.
      *================================================================*
       2000-SCAN-TRANSACTIONS.
      *================================================================*
           EXEC SQL OPEN AMLCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2000-SCAN-TRANSACTIONS' TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           PERFORM UNTIL WS-END-OF-CURSOR
               EXEC SQL
                   FETCH AMLCSR
                    INTO :TXH-SORT-CODE, :TXH-ACCT-NO,
                         :TXH-TXN-CODE, :TXH-DR-CR-IND,
                         :TXH-TXN-AMT
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-TXN-READ
                       IF TXH-SORT-CODE NOT = WS-CURR-ACCT (1:6)
                       OR TXH-ACCT-NO NOT = WS-CURR-ACCT (7:8)
                           PERFORM 2900-END-ACCOUNT THRU 2900-EXIT
                           PERFORM 2050-START-ACCOUNT
                       END-IF
                       PERFORM 2100-ACCUMULATE
                   WHEN +100
                       SET WS-END-OF-CURSOR TO TRUE
                   WHEN OTHER
                       MOVE '2000-SCAN-TRANSACTIONS' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
           PERFORM 2900-END-ACCOUNT THRU 2900-EXIT
           EXEC SQL CLOSE AMLCSR END-EXEC
           .
      *================================================================*
       2050-START-ACCOUNT.
      *================================================================*
           MOVE TXH-SORT-CODE              TO WS-CURR-ACCT (1:6)
           MOVE TXH-ACCT-NO                TO WS-CURR-ACCT (7:8)
           MOVE ZERO                       TO WS-AT-TXN-COUNT
                                              WS-AT-CASH-IN-TOTAL
                                              WS-AT-CASH-IN-COUNT
                                              WS-AT-CASH-IN-MAX
                                              WS-AT-CASH-UNDER-TOTAL
                                              WS-AT-CASH-UNDER-COUNT
                                              WS-AT-CR-TOTAL
                                              WS-AT-DR-TOTAL
                                              WS-AT-SWO-TOTAL
                                              WS-AT-SWO-COUNT
                                              WS-AT-FPO-COUNT
                                              WS-AT-FPO-TOTAL
                                              WS-AT-MAX-TXN
                                              WS-AT-OVER-R04-COUNT
                                              WS-AT-OVER-R04-TOTAL
           MOVE 'N'                        TO WS-ACCT-READ-IND
                                              WS-CUST-READ-IND
           ADD 1                           TO WS-ACCOUNTS-SEEN
           .
      *================================================================*
       2100-ACCUMULATE.
      *================================================================*
           ADD 1                           TO WS-AT-TXN-COUNT
           IF TXH-TXN-AMT > WS-AT-MAX-TXN
               MOVE TXH-TXN-AMT            TO WS-AT-MAX-TXN
           END-IF
           IF TXH-TXN-AMT >= WS-RL-THRESHOLD (4)
               ADD 1                       TO WS-AT-OVER-R04-COUNT
               ADD TXH-TXN-AMT             TO WS-AT-OVER-R04-TOTAL
           END-IF

           IF TXH-DR-CR-IND = 'C'
               ADD TXH-TXN-AMT             TO WS-AT-CR-TOTAL
               IF TXH-TXN-CODE = 'CSH '
                   ADD 1                   TO WS-AT-CASH-IN-COUNT
                   ADD TXH-TXN-AMT         TO WS-AT-CASH-IN-TOTAL
                   IF TXH-TXN-AMT > WS-AT-CASH-IN-MAX
                       MOVE TXH-TXN-AMT    TO WS-AT-CASH-IN-MAX
                   END-IF
                   IF TXH-TXN-AMT < WS-RL-THRESHOLD (1)
                       ADD 1               TO WS-AT-CASH-UNDER-COUNT
                       ADD TXH-TXN-AMT     TO WS-AT-CASH-UNDER-TOTAL
                   END-IF
               END-IF
           ELSE
               ADD TXH-TXN-AMT             TO WS-AT-DR-TOTAL
               EVALUATE TXH-TXN-CODE
                   WHEN 'SWO '
                       ADD 1               TO WS-AT-SWO-COUNT
                       ADD TXH-TXN-AMT     TO WS-AT-SWO-TOTAL
                   WHEN 'FPO '
                       ADD 1               TO WS-AT-FPO-COUNT
                       ADD TXH-TXN-AMT     TO WS-AT-FPO-TOTAL
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF
           .
      *================================================================*
       2900-END-ACCOUNT.
      *================================================================*
           IF WS-CURR-ACCT = LOW-VALUES
               GO TO 2900-EXIT
           END-IF

      *---- R01 LARGE CASH --------------------------------------------*
           IF WS-RL-ACTIVE (1) = 'Y'
           AND WS-AT-CASH-IN-MAX >= WS-RL-THRESHOLD (1)
               MOVE 1                      TO WS-RULE-NO
               MOVE WS-AT-CASH-IN-COUNT    TO WS-HIT-COUNT
               MOVE WS-AT-CASH-IN-TOTAL    TO WS-HIT-TOTAL
               PERFORM 4000-RAISE-ALERT THRU 4000-EXIT
           END-IF

      *---- R02 STRUCTURING -------------------------------------------*
           IF WS-RL-ACTIVE (2) = 'Y'
           AND WS-AT-CASH-UNDER-COUNT >= WS-RL-COUNT (2)
           AND WS-AT-CASH-UNDER-TOTAL >= WS-RL-THRESHOLD (2)
               MOVE 2                      TO WS-RULE-NO
               MOVE WS-AT-CASH-UNDER-COUNT TO WS-HIT-COUNT
               MOVE WS-AT-CASH-UNDER-TOTAL TO WS-HIT-TOTAL
               PERFORM 4000-RAISE-ALERT THRU 4000-EXIT
           END-IF

      *---- R03 RAPID MOVEMENT ----------------------------------------*
           IF WS-RL-ACTIVE (3) = 'Y'
           AND WS-AT-CR-TOTAL >= WS-RL-THRESHOLD (3)
               COMPUTE WS-NINETY-PCT = WS-AT-CR-TOTAL * 0.9
               IF WS-AT-DR-TOTAL >= WS-NINETY-PCT
                   MOVE 3                  TO WS-RULE-NO
                   MOVE WS-AT-TXN-COUNT    TO WS-HIT-COUNT
                   MOVE WS-AT-DR-TOTAL     TO WS-HIT-TOTAL
                   PERFORM 4000-RAISE-ALERT THRU 4000-EXIT
               END-IF
           END-IF

      *---- R04 HIGH RISK CUSTOMER ------------------------------------*
           IF WS-RL-ACTIVE (4) = 'Y'
           AND WS-AT-OVER-R04-COUNT > ZERO
               PERFORM 2910-READ-CUSTOMER THRU 2910-EXIT
               IF WS-CUST-READ AND CIF-RISK-HIGH
                   MOVE 4                  TO WS-RULE-NO
                   MOVE WS-AT-OVER-R04-COUNT TO WS-HIT-COUNT
                   MOVE WS-AT-OVER-R04-TOTAL TO WS-HIT-TOTAL
                   PERFORM 4000-RAISE-ALERT THRU 4000-EXIT
               END-IF
           END-IF

      *---- R05 DORMANT REACTIVATION ----------------------------------*
           IF WS-RL-ACTIVE (5) = 'Y'
           AND WS-AT-DR-TOTAL >= WS-RL-THRESHOLD (5)
               PERFORM 2920-READ-ACCOUNT THRU 2920-EXIT
               IF WS-ACCT-READ
               AND ACM-OPEN
               AND ACM-DORMANT-DATE NUMERIC
               AND ACM-DORMANT-DATE > ZERO
                   COMPUTE WS-DORMANT-INT =
                       FUNCTION INTEGER-OF-DATE (ACM-LAST-MAINT-DATE)
      *            REACTIVATED (STATUS BACK TO OPEN) WITHIN 30 DAYS.
                   IF WS-BUS-DATE-INT - WS-DORMANT-INT <= 30
                       MOVE 5              TO WS-RULE-NO
                       MOVE WS-AT-TXN-COUNT TO WS-HIT-COUNT
                       MOVE WS-AT-DR-TOTAL TO WS-HIT-TOTAL
                       PERFORM 4000-RAISE-ALERT THRU 4000-EXIT
                   END-IF
               END-IF
           END-IF

      *---- R06 INTERNATIONAL -----------------------------------------*
           IF WS-RL-ACTIVE (6) = 'Y'
           AND WS-AT-SWO-TOTAL >= WS-RL-THRESHOLD (6)
               MOVE 6                      TO WS-RULE-NO
               MOVE WS-AT-SWO-COUNT        TO WS-HIT-COUNT
               MOVE WS-AT-SWO-TOTAL        TO WS-HIT-TOTAL
               PERFORM 4000-RAISE-ALERT THRU 4000-EXIT
           END-IF

      *---- R07 VELOCITY ----------------------------------------------*
           IF WS-RL-ACTIVE (7) = 'Y'
           AND WS-AT-FPO-COUNT >= WS-RL-COUNT (7)
               MOVE 7                      TO WS-RULE-NO
               MOVE WS-AT-FPO-COUNT        TO WS-HIT-COUNT
               MOVE WS-AT-FPO-TOTAL        TO WS-HIT-TOTAL
               PERFORM 4000-RAISE-ALERT THRU 4000-EXIT
           END-IF
           .
       2900-EXIT.
           EXIT.
      *================================================================*
       2910-READ-CUSTOMER.
      *================================================================*
           IF WS-CUST-READ
               GO TO 2910-EXIT
           END-IF
           PERFORM 2920-READ-ACCOUNT THRU 2920-EXIT
           IF NOT WS-ACCT-READ
               GO TO 2910-EXIT
           END-IF
           MOVE ACM-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           IF WS-CIFMAST-OK
               SET WS-CUST-READ            TO TRUE
           END-IF
           .
       2910-EXIT.
           EXIT.
      *================================================================*
       2920-READ-ACCOUNT.
      *================================================================*
           IF WS-ACCT-READ
               GO TO 2920-EXIT
           END-IF
           MOVE WS-CURR-ACCT (1:6)         TO ACM-SORT-CODE
           MOVE WS-CURR-ACCT (7:8)         TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF WS-ACCTMAST-OK
               SET WS-ACCT-READ            TO TRUE
           END-IF
           .
       2920-EXIT.
           EXIT.
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'TRANSACTIONS SCANNED'     TO TL-LABEL
           MOVE WS-TXN-READ                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS WITH ACTIVITY'   TO TL-LABEL
           MOVE WS-ACCOUNTS-SEEN           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ALERTS RAISED'            TO TL-LABEL
           MOVE WS-ALERTS-RAISED           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           PERFORM VARYING WS-RULE-NO FROM 1 BY 1 UNTIL WS-RULE-NO > 7
               MOVE SPACES                 TO TL-LABEL
               STRING '  ' WS-RL-ID (WS-RULE-NO) ' '
                      WS-RL-DESC (WS-RULE-NO)
                      DELIMITED BY SIZE
                      INTO TL-LABEL
               END-STRING
               IF WS-RL-ACTIVE (WS-RULE-NO) NOT = 'Y'
                   MOVE ' (INACTIVE)'      TO TL-LABEL (40:11)
               END-IF
               MOVE WS-RL-HIT-COUNT (WS-RULE-NO) TO TL-COUNT
               WRITE REPORT-RECORD         FROM WS-TOTAL-LINE
           END-PERFORM
           MOVE 'DUPLICATE ALERTS (RERUN) SKIPPED' TO TL-LABEL
           MOVE WS-DUP-ALERTS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RULE OVERRIDES APPLIED'   TO TL-LABEL
           MOVE WS-OVERRIDES               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE ACCTMAST-FILE
                 CIFMAST-FILE
                 ALERTS-FILE
                 REPORT-FILE

           DISPLAY 'RGAML010 - TRANSACTIONS ' WS-TXN-READ
           DISPLAY 'RGAML010 - ALERTS       ' WS-ALERTS-RAISED
           .
      *================================================================*
       4000-RAISE-ALERT.
      *================================================================*
           PERFORM 2910-READ-CUSTOMER THRU 2910-EXIT

           ADD 1                           TO WS-ALERT-SEQ
           MOVE SPACES                     TO AML-ALERT-ID
           STRING 'AML' WS-BUS-DATE (3:6) WS-RL-ID (WS-RULE-NO)
                  WS-ALERT-SEQ (2:5)
                  DELIMITED BY SIZE
                  INTO AML-ALERT-ID
           END-STRING
           MOVE WS-BUS-DATE-DB2            TO AML-ALERT-DATE
           MOVE SPACES                     TO AML-RULE-ID
           MOVE WS-RL-ID (WS-RULE-NO)      TO AML-RULE-ID
           MOVE WS-RL-SEVERITY (WS-RULE-NO) TO AML-SEVERITY
           MOVE WS-CURR-ACCT (1:6)         TO AML-SORT-CODE
           MOVE WS-CURR-ACCT (7:8)         TO AML-ACCT-NO
           IF WS-ACCT-READ
               MOVE ACM-CIF-NO             TO AML-CIF-NO
           ELSE
               MOVE SPACES                 TO AML-CIF-NO
           END-IF
           IF WS-CUST-READ
               MOVE CIF-RISK-RATING        TO AML-RISK-RATING
           ELSE
               MOVE '?'                    TO AML-RISK-RATING
           END-IF
      *    HIGH-RISK CUSTOMERS RAISE THE SEVERITY BY ONE LEVEL.
           IF AML-RISK-RATING = 'H' AND AML-SEVERITY = 'L'
               MOVE 'M'                    TO AML-SEVERITY
           ELSE
               IF AML-RISK-RATING = 'H' AND AML-SEVERITY = 'M'
                   MOVE 'H'                TO AML-SEVERITY
               END-IF
           END-IF
           MOVE WS-HIT-COUNT               TO AML-TXN-COUNT
           MOVE WS-HIT-TOTAL               TO AML-TXN-TOTAL
           MOVE WS-RL-THRESHOLD (WS-RULE-NO) TO AML-THRESHOLD
           MOVE WS-RL-DESC (WS-RULE-NO)    TO AML-DESCRIPTION-TEXT
           MOVE 40                         TO AML-DESCRIPTION-LEN
           MOVE 'N'                        TO AML-ALERT-STATUS
           MOVE SPACES                     TO AML-ASSIGNED-TO
                                              AML-CASE-REF

      *    THE UNIQUE INDEX IS (ALERT_DATE, RULE_ID, SORT_CODE,
      *    ACCT_NO) - A RERUN DOES NOT DUPLICATE ALERTS.
           EXEC SQL
               INSERT INTO MBK.AMLALERT
                     (ALERT_ID, ALERT_DATE, RULE_ID, SEVERITY,
                      SORT_CODE, ACCT_NO, CIF_NO, RISK_RATING,
                      TXN_COUNT, TXN_TOTAL, THRESHOLD, DESCRIPTION,
                      ALERT_STATUS, ASSIGNED_TO, CASE_REF, CREATED_TS)
               VALUES
                     (:AML-ALERT-ID, :AML-ALERT-DATE, :AML-RULE-ID,
                      :AML-SEVERITY, :AML-SORT-CODE, :AML-ACCT-NO,
                      :AML-CIF-NO, :AML-RISK-RATING, :AML-TXN-COUNT,
                      :AML-TXN-TOTAL, :AML-THRESHOLD,
                      :AML-DESCRIPTION, :AML-ALERT-STATUS,
                      :AML-ASSIGNED-TO, :AML-CASE-REF,
                      CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
                   ADD 1                   TO WS-DUP-ALERTS
                   GO TO 4000-EXIT
               WHEN OTHER
                   MOVE '4000-RAISE-ALERT' TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE

           ADD 1                           TO WS-ALERTS-RAISED
           ADD 1                       TO WS-RL-HIT-COUNT (WS-RULE-NO)
           PERFORM 4100-WRITE-ALERT-FILE
           PERFORM 7100-REPORT-DETAIL
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-WRITE-ALERT-FILE.
      *================================================================*
           MOVE SPACES                     TO WS-ALERT-RECORD
           MOVE AML-ALERT-ID               TO ALR-ALERT-ID
           MOVE WS-BUS-DATE                TO ALR-ALERT-DATE
           MOVE AML-RULE-ID                TO ALR-RULE-ID
           MOVE AML-SEVERITY               TO ALR-SEVERITY
           MOVE AML-SORT-CODE              TO ALR-SORT-CODE
           MOVE AML-ACCT-NO                TO ALR-ACCT-NO
           MOVE AML-CIF-NO                 TO ALR-CIF-NO
           IF WS-CUST-READ
               STRING CIF-FIRST-NM DELIMITED BY SPACE
                      ' '          DELIMITED BY SIZE
                      CIF-LAST-NM  DELIMITED BY '  '
                      INTO ALR-CUST-NAME
               END-STRING
           END-IF
           MOVE AML-RISK-RATING            TO ALR-RISK-RATING
           MOVE AML-TXN-COUNT              TO ALR-TXN-COUNT
           MOVE AML-TXN-TOTAL              TO ALR-TXN-TOTAL
           MOVE AML-THRESHOLD              TO ALR-THRESHOLD
           MOVE AML-DESCRIPTION-TEXT       TO ALR-DESCRIPTION
           WRITE ALERTS-RECORD             FROM WS-ALERT-RECORD
           IF WS-ALERTS-STATUS NOT = '00'
               MOVE '4100-WRITE-ALERT-FILE' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ALERTS WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-ALERTS-STATUS       TO ABN-FILE-STATUS
               EXEC SQL ROLLBACK END-EXEC
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
           MOVE AML-ALERT-ID               TO DL-ALERT-ID
           MOVE AML-RULE-ID                TO DL-RULE
           MOVE AML-SEVERITY               TO DL-SEV
           MOVE AML-SORT-CODE              TO DL-SORT-CODE
           MOVE AML-ACCT-NO                TO DL-ACCT-NO
           MOVE AML-CIF-NO                 TO DL-CIF-NO
           MOVE AML-RISK-RATING            TO DL-RISK
           MOVE AML-TXN-COUNT              TO DL-COUNT
           MOVE AML-TXN-TOTAL              TO DL-TOTAL
           MOVE AML-DESCRIPTION-TEXT       TO DL-DESC
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           MOVE WS-CURR-ACCT               TO ABN-KEY-DATA
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
