       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IBFD0301.
       AUTHOR.        S J WHITFIELD.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2014-05-12.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : IBFD0301                                            *
      * TITLE    : IBFEED03 - HOST TO INTERNET BANKING TRANSACTION     *
      *            SHADOW TABLE FEED                                   *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : IBFD0301                                            *
      * JCL      : IBFEED03 STEP IB010                                 *
      *----------------------------------------------------------------*
      * INPUT    : MBK.TXNHIST - TODAY'S HARD POSTINGS (CURSOR)        *
      *            ACCTMAST - DDA ACCOUNT MASTER (VSAM, RANDOM)        *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : IBFEED   - TRANSACTION LOAD FILE (IBFD3REC)         *
      *            REPORT   - CONTROL REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * COMPANION TO IBFD0201 (IBFEED02). IBFEED02 IS DRIVEN BY THE    *
      * ACCOUNT MASTER AND CARRIES BALANCES; THIS FEED IS DRIVEN BY    *
      * THE TXNHIST CURSOR AND LOADS THE IBANK TRANSACTION SHADOW      *
      * TABLE IBK_TXN_SHADOW USED BY 'SEARCH MY TRANSACTIONS' AND THE  *
      * SPENDING ANALYSIS PAGES.                                       *
      *                                                                *
      * FOR EACH TXNHIST ROW POSTED ON THE BUSINESS DATE (ORDER SORT   *
      * CODE, ACCOUNT, POST SEQUENCE) ONE 'T' RECORD IS WRITTEN WITH   *
      * THE ACCOUNT ATTRIBUTES (CIF, TYPE, PRODUCT) LOOKED UP ON THE   *
      * ACCOUNT MASTER AT EACH CHANGE OF ACCOUNT, A CUSTOMER-FRIENDLY  *
      * DESCRIPTION AND A SPENDING CATEGORY FROM WS-TXN-CODE-TABLE.    *
      * AFTER EACH ACCOUNT'S ROWS AN 'S' RECORD CARRIES THE ACCOUNT'S  *
      * COUNT, DEBIT / CREDIT TOTALS, OPENING AND CLOSING BALANCE.     *
      *                                                                *
      * CONTROLS                                                       *
      *  1. BEFORE THE CURSOR IS OPENED THE ROW COUNT AND DEBIT /      *
      *     CREDIT SUMS FOR THE DATE ARE SELECTED. THE ROWS FETCHED    *
      *     MUST AGREE (U1004 IF NOT - TXNHIST CHANGED DURING THE      *
      *     EXTRACT; PSEOD01 MUST HAVE FINISHED - SEE CA-7).           *
      *  2. RUNNING BALANCE CHAIN: EACH ROW'S RUNNING_BAL MUST EQUAL   *
      *     THE PREVIOUS ROW'S PLUS THE SIGNED AMOUNT. BREAKS ARE      *
      *     REPORTED (RC 4) BUT THE ROWS ARE STILL SENT.               *
      *  3. THE LAST RUNNING BALANCE MUST EQUAL ACM-LEDGER-BAL. IF NOT *
      *     IB3S-BAL-CHECK = 'N' AND THE ACCOUNT IS REPORTED (RC 4).   *
      *  4. ROWS FOR ACCOUNTS NOT ON THE MASTER ARE NOT SENT (THE      *
      *     SHADOW TABLE HAS A FOREIGN KEY TO THE ACCOUNT) - RC 4.     *
      *  5. THE TRAILER CARRIES COUNTS, DEBIT / CREDIT TOTALS AND A    *
      *     HASH OF ACCOUNT NUMBERS FOR THE LOADER TO CHECK.           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2014-05-12 SJW  ORIGINAL VERSION (IBANK TRANSACTION SEARCH)    *
      * 2014-09-01 SJW  CONTROL TOTALS SELECTED BEFORE THE CURSOR      *
      *                 (INC0052290 - FEED BUILT WHILE A PS020 RERUN   *
      *                 WAS STILL INSERTING)                           *
      * 2015-10-05 SJW  SPENDING CATEGORY (IB3T-CATEGORY)              *
      * 2016-03-14 DKP  RUNNING BALANCE CHAIN CHECK                    *
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
           SELECT IBFEED-FILE      ASSIGN TO IBFEED
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-IBFEED-STATUS.
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
      *
       FD  IBFEED-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  IBFEED-RECORD                   PIC X(300).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'IBFD0301'.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-IBFEED-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-TXN-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-TXN-EOF                  VALUE 'Y'.
           05  WS-ACCT-STATE-IND           PIC X(01) VALUE 'X'.
               88  WS-ACCT-ON-MASTER           VALUE 'Y'.
               88  WS-ACCT-ORPHAN              VALUE 'N'.
               88  WS-NO-ACCOUNT-YET           VALUE 'X'.
           05  WS-FIRST-ROW-IND            PIC X(01).
               88  WS-FIRST-ROW-OF-ACCT        VALUE 'Y'.
      *
       01  WS-KEYS.
           05  WS-TXN-KEY.
               10  WS-TK-SORT-CODE         PIC X(06).
               10  WS-TK-ACCT-NO           PIC X(08).
           05  WS-CURR-ACCT-KEY.
               10  WS-CK-SORT-CODE         PIC X(06).
               10  WS-CK-ACCT-NO           PIC X(08).
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-DB2             PIC X(10).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-SIGNED-AMT               PIC S9(13)V99 COMP-3.
           05  WS-EXPECTED-BAL             PIC S9(13)V99 COMP-3.
           05  WS-PREV-RUNNING-BAL         PIC S9(13)V99 COMP-3.
           05  WS-ACCT-NUM                 PIC 9(08).
           05  WS-NOTE                     PIC X(43).
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURR-DATE            PIC 9(08).
               10  WS-CURR-TIME            PIC 9(06).
               10  FILLER                  PIC X(07).
      *
      *---- PER-ACCOUNT ACCUMULATORS ----------------------------------*
       01  WS-ACCOUNT-TOTALS.
           05  WS-AT-TXN-COUNT             PIC S9(07) COMP-3.
           05  WS-AT-TOTAL-DR              PIC S9(13)V99 COMP-3.
           05  WS-AT-TOTAL-CR              PIC S9(13)V99 COMP-3.
           05  WS-AT-OPENING-BAL           PIC S9(13)V99 COMP-3.
           05  WS-AT-CLOSING-BAL           PIC S9(13)V99 COMP-3.
      *
      *---- DB2 CONTROL TOTALS (SELECTED BEFORE THE CURSOR) -----------*
       01  WS-DB2-CONTROL.
           05  WS-DB2-ROW-COUNT            PIC S9(09) COMP.
           05  WS-DB2-TOTAL-DR             PIC S9(15)V99 COMP-3.
           05  WS-DB2-TOTAL-CR             PIC S9(15)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-ROWS-FETCHED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-FETCH-DR                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-FETCH-CR                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-ACCT-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-TXN-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-REC-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ORPHAN-ACCTS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ORPHAN-ROWS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CHAIN-BREAKS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-LEDGER-BREAKS            PIC S9(09) COMP-3 VALUE 0.
           05  WS-UNKNOWN-CODES            PIC S9(09) COMP-3 VALUE 0.
           05  WS-FEED-DR                  PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-FEED-CR                  PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-HASH-ACCT-NO             PIC 9(18) VALUE 0.
      *
      *---- TRANSACTION CODE DESCRIPTIONS AND CATEGORIES --------------*
      *    CATEGORIES (AGREED WITH IBANK PRODUCT, CR 15/0187):
      *      TRAN TRANSFER  PAYM PAYMENT  INCM INCOME  FEES CHARGES
      *      INTR INTEREST  BILL BILLS    CASH CASH    ADJT ADJUSTMENT
      *      CARD CARD      LOAN LOANS    OTHR OTHER
      *    THE LAST ENTRY ('????') IS USED FOR ANY CODE NOT LISTED.
       01  WS-TXN-CODE-VALUES.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'TFR '.
               10  FILLER          PIC X(20)
                                   VALUE 'TRANSFER'.
               10  FILLER          PIC X(04) VALUE 'TRAN'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'FPO '.
               10  FILLER          PIC X(20)
                                   VALUE 'FASTER PAYMENT OUT'.
               10  FILLER          PIC X(04) VALUE 'PAYM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'FPI '.
               10  FILLER          PIC X(20)
                                   VALUE 'FASTER PAYMENT IN'.
               10  FILLER          PIC X(04) VALUE 'INCM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'BGC '.
               10  FILLER          PIC X(20)
                                   VALUE 'BANK GIRO CREDIT'.
               10  FILLER          PIC X(04) VALUE 'INCM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'BAC '.
               10  FILLER          PIC X(20)
                                   VALUE 'BACS PAYMENT'.
               10  FILLER          PIC X(04) VALUE 'PAYM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'CHP '.
               10  FILLER          PIC X(20)
                                   VALUE 'CHAPS PAYMENT'.
               10  FILLER          PIC X(04) VALUE 'PAYM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'SWO '.
               10  FILLER          PIC X(20)
                                   VALUE 'OVERSEAS PAYMENT'.
               10  FILLER          PIC X(04) VALUE 'PAYM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'SWI '.
               10  FILLER          PIC X(20)
                                   VALUE 'OVERSEAS RECEIPT'.
               10  FILLER          PIC X(04) VALUE 'INCM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'CHG '.
               10  FILLER          PIC X(20)
                                   VALUE 'CHARGE'.
               10  FILLER          PIC X(04) VALUE 'FEES'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'INT '.
               10  FILLER          PIC X(20)
                                   VALUE 'INTEREST'.
               10  FILLER          PIC X(04) VALUE 'INTR'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'DD  '.
               10  FILLER          PIC X(20)
                                   VALUE 'DIRECT DEBIT'.
               10  FILLER          PIC X(04) VALUE 'BILL'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'SO  '.
               10  FILLER          PIC X(20)
                                   VALUE 'STANDING ORDER'.
               10  FILLER          PIC X(04) VALUE 'BILL'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'CSH '.
               10  FILLER          PIC X(20)
                                   VALUE 'CASH'.
               10  FILLER          PIC X(04) VALUE 'CASH'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'REV '.
               10  FILLER          PIC X(20)
                                   VALUE 'REVERSAL'.
               10  FILLER          PIC X(04) VALUE 'ADJT'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'TAX '.
               10  FILLER          PIC X(20)
                                   VALUE 'TAX DEDUCTED'.
               10  FILLER          PIC X(04) VALUE 'INTR'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'OPN '.
               10  FILLER          PIC X(20)
                                   VALUE 'OPENING DEPOSIT'.
               10  FILLER          PIC X(04) VALUE 'TRAN'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'CLS '.
               10  FILLER          PIC X(20)
                                   VALUE 'ACCOUNT CLOSURE'.
               10  FILLER          PIC X(04) VALUE 'TRAN'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'UNP '.
               10  FILLER          PIC X(20)
                                   VALUE 'RETURNED ITEM'.
               10  FILLER          PIC X(04) VALUE 'ADJT'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'CHQ '.
               10  FILLER          PIC X(20)
                                   VALUE 'CHEQUE'.
               10  FILLER          PIC X(04) VALUE 'PAYM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'CHD '.
               10  FILLER          PIC X(20)
                                   VALUE 'CHEQUE DEPOSIT'.
               10  FILLER          PIC X(04) VALUE 'INCM'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'CRD '.
               10  FILLER          PIC X(20)
                                   VALUE 'DEBIT CARD'.
               10  FILLER          PIC X(04) VALUE 'CARD'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'LNR '.
               10  FILLER          PIC X(20)
                                   VALUE 'LOAN REPAYMENT'.
               10  FILLER          PIC X(04) VALUE 'LOAN'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE 'LND '.
               10  FILLER          PIC X(20)
                                   VALUE 'LOAN ADVANCE'.
               10  FILLER          PIC X(04) VALUE 'LOAN'.
           05  FILLER.
               10  FILLER          PIC X(04) VALUE '????'.
               10  FILLER          PIC X(20)
                                   VALUE 'OTHER'.
               10  FILLER          PIC X(04) VALUE 'OTHR'.
       01  WS-TXN-CODE-TABLE REDEFINES WS-TXN-CODE-VALUES.
           05  WS-TC-ENTRY                 OCCURS 24 TIMES
                                           INDEXED BY TC-IDX.
               10  WS-TC-CODE              PIC X(04).
               10  WS-TC-DESC              PIC X(20).
               10  WS-TC-CATEGORY          PIC X(04).
       01  WS-TC-MAX                       PIC S9(04) COMP VALUE +24.
       01  WS-TXN-CODE-TOTALS.
           05  WS-TT-ENTRY                 OCCURS 24 TIMES
                                           INDEXED BY TT-IDX.
               10  WS-TT-COUNT             PIC S9(09) COMP-3.
               10  WS-TT-DR                PIC S9(15)V99 COMP-3.
               10  WS-TT-CR                PIC S9(15)V99 COMP-3.
      *
      *---- REPORT ----------------------------------------------------*
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'IBFD0301'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - IBFEED03 TRANSACTION FEED CONTROL'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(16) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(11) VALUE '   POST SEQ'.
           05  FILLER            PIC X(06) VALUE ' CODE'.
           05  FILLER            PIC X(16) VALUE '        AMOUNT'.
           05  FILLER            PIC X(20) VALUE '          EXPECTED'.
           05  FILLER            PIC X(20) VALUE '            ACTUAL'.
           05  FILLER            PIC X(43) VALUE '  EXCEPTION'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-POST-SEQ                 PIC Z(08)9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TXN-CODE                 PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-EXPECTED                 PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTUAL                   PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(43).
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(06) VALUE 'CODE'.
           05  FILLER            PIC X(22) VALUE 'DESCRIPTION'.
           05  FILLER            PIC X(14) VALUE '      COUNT'.
           05  FILLER            PIC X(25)
                                 VALUE '                 DEBITS'.
           05  FILLER            PIC X(25)
                                 VALUE '                CREDITS'.
           05  FILLER            PIC X(40) VALUE SPACES.
       01  WS-CODE-LINE.
           05  CL-CC                       PIC X(01) VALUE ' '.
           05  CL-CODE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  CL-DESC                     PIC X(20).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  CL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  CL-DR                       PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  CL-CR                       PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(42) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
           EXEC SQL DECLARE TXNCSR3 CURSOR FOR
               SELECT SORT_CODE, ACCT_NO, POST_SEQ, TXN_CODE,
                      DR_CR_IND, TXN_AMT, RUNNING_BAL, CCY_CD,
                      VALUE_DATE, NARRATIVE_1, NARRATIVE_2,
                      CHANNEL_CD, EXT_REF, POST_TYPE,
                      BATCH_RUN_ID, CHAR(INSERT_TS)
                 FROM MBK.TXNHIST
                WHERE POST_DATE = :WS-BUS-DATE-DB2
                ORDER BY SORT_CODE, ACCT_NO, POST_SEQ
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
           COPY IBFD3REC.
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
           PERFORM 2000-PROCESS-TXN THRU 2000-EXIT
               UNTIL WS-TXN-EOF
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       DATECARD-FILE
                OUTPUT IBFEED-FILE
                       REPORT-FILE

           IF NOT WS-ACCTMAST-OK
           OR WS-IBFEED-STATUS NOT = '00'
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
                                              WS-DATE-WORK
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-BUS-DATE-DB2
           END-STRING
           MOVE WS-BUS-DATE-DB2            TO HD1-BUS-DATE
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA
           INITIALIZE WS-TXN-CODE-TOTALS
           INITIALIZE WS-ACCOUNT-TOTALS
           MOVE LOW-VALUES                 TO WS-CURR-ACCT-KEY

      *---- HEADER ----------------------------------------------------*
           MOVE SPACES                     TO IB3-FEED-RECORD
           SET IB3-HEADER-REC              TO TRUE
           MOVE 'IBFEED03'                 TO IB3H-FILE-ID
           MOVE WS-BUS-DATE                TO IB3H-BUSINESS-DATE
           MOVE WS-CURR-DATE               TO IB3H-CREATE-DATE
           MOVE WS-CURR-TIME               TO IB3H-CREATE-TIME
      *    FILE SEQUENCE = DAYS SINCE 01/01/2000 (SAME AS IBFEED02)
           COMPUTE IB3H-FILE-SEQ-NO =
                   FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
                 - FUNCTION INTEGER-OF-DATE (20000101)
           MOVE 'MBKHOST '                 TO IB3H-SOURCE-SYS
           MOVE 'IBK_TXN_SHADOW'           TO IB3H-TARGET-TABLE
           PERFORM 8200-WRITE-FEED

      *---- CONTROL TOTALS (CONTROL 1) --------------------------------*
           EXEC SQL
               SELECT COUNT(*),
                      COALESCE(SUM(CASE WHEN DR_CR_IND = 'D'
                                        THEN TXN_AMT ELSE 0 END), 0),
                      COALESCE(SUM(CASE WHEN DR_CR_IND = 'C'
                                        THEN TXN_AMT ELSE 0 END), 0)
                 INTO :WS-DB2-ROW-COUNT,
                      :WS-DB2-TOTAL-DR,
                      :WS-DB2-TOTAL-CR
                 FROM MBK.TXNHIST
                WHERE POST_DATE = :WS-BUS-DATE-DB2
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           DISPLAY 'IBFD0301 - TXNHIST ROWS FOR ' WS-BUS-DATE-DB2
                   ' : ' WS-DB2-ROW-COUNT

           EXEC SQL OPEN TXNCSR3 END-EXEC
           IF SQLCODE NOT = 0
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           PERFORM 8100-FETCH-TXN
           .
      *================================================================*
       2000-PROCESS-TXN.
      *================================================================*
           IF WS-TXN-KEY NOT = WS-CURR-ACCT-KEY
               PERFORM 2100-ACCOUNT-BREAK
           END-IF

           IF WS-ACCT-ORPHAN
               ADD 1                       TO WS-ORPHAN-ROWS
               PERFORM 8100-FETCH-TXN
               GO TO 2000-EXIT
           END-IF

           PERFORM 2200-LOOKUP-CODE
           PERFORM 2300-CHECK-CHAIN
           PERFORM 2400-WRITE-TRANSACTION
           PERFORM 8100-FETCH-TXN
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-ACCOUNT-BREAK.
      *================================================================*
      *    CLOSE OFF THE PREVIOUS ACCOUNT, THEN LOOK UP THE NEW ONE.
           IF WS-ACCT-ON-MASTER
               PERFORM 2900-WRITE-SUMMARY
           END-IF

           MOVE WS-TXN-KEY                 TO WS-CURR-ACCT-KEY
           INITIALIZE WS-ACCOUNT-TOTALS
           SET WS-FIRST-ROW-OF-ACCT        TO TRUE

           MOVE WS-CURR-ACCT-KEY           TO ACM-KEY
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   SET WS-ACCT-ON-MASTER   TO TRUE
               WHEN WS-ACCTMAST-NOTFND
                   SET WS-ACCT-ORPHAN      TO TRUE
                   ADD 1                   TO WS-ORPHAN-ACCTS
                   DISPLAY 'IBFD0301 W001 TXNHIST ROWS - NO ACCOUNT '
                           WS-CURR-ACCT-KEY
                   MOVE ZERO               TO DL-AMOUNT DL-EXPECTED
                                              DL-ACTUAL
                   MOVE TXH-POST-SEQ       TO DL-POST-SEQ
                   MOVE TXH-TXN-CODE       TO DL-TXN-CODE
                   MOVE 'ACCOUNT NOT ON MASTER - ROWS NOT SENT'
                                           TO WS-NOTE
                   PERFORM 7100-REPORT-DETAIL
               WHEN OTHER
                   MOVE '2100-ACCOUNT-BREAK' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2200-LOOKUP-CODE.
      *================================================================*
           SET TC-IDX                      TO 1
           SEARCH WS-TC-ENTRY
               AT END
                   SET TC-IDX              TO WS-TC-MAX
                   ADD 1                   TO WS-UNKNOWN-CODES
                   DISPLAY 'IBFD0301 W002 UNKNOWN TXN CODE '
                           TXH-TXN-CODE ' ON ' WS-TXN-KEY
               WHEN WS-TC-CODE (TC-IDX) = TXH-TXN-CODE
                   CONTINUE
           END-SEARCH
           SET TT-IDX                      TO TC-IDX
           .
      *================================================================*
       2300-CHECK-CHAIN.
      *================================================================*
      *    SIGNED MOVEMENT: DEBITS REDUCE THE BALANCE
           IF TXH-DR-CR-IND = 'D'
               COMPUTE WS-SIGNED-AMT = ZERO - TXH-TXN-AMT
           ELSE
               MOVE TXH-TXN-AMT            TO WS-SIGNED-AMT
           END-IF

           IF WS-FIRST-ROW-OF-ACCT
      *        OPENING BALANCE IS DERIVED FROM THE FIRST ROW
               COMPUTE WS-AT-OPENING-BAL =
                       TXH-RUNNING-BAL - WS-SIGNED-AMT
               MOVE 'N'                    TO WS-FIRST-ROW-IND
           ELSE
               COMPUTE WS-EXPECTED-BAL =
                       WS-PREV-RUNNING-BAL + WS-SIGNED-AMT
               IF WS-EXPECTED-BAL NOT = TXH-RUNNING-BAL
                   ADD 1                   TO WS-CHAIN-BREAKS
                   MOVE TXH-POST-SEQ       TO DL-POST-SEQ
                   MOVE TXH-TXN-CODE       TO DL-TXN-CODE
                   MOVE TXH-TXN-AMT        TO DL-AMOUNT
                   MOVE WS-EXPECTED-BAL    TO DL-EXPECTED
                   MOVE TXH-RUNNING-BAL    TO DL-ACTUAL
                   MOVE 'RUNNING BALANCE CHAIN BREAK'
                                           TO WS-NOTE
                   PERFORM 7100-REPORT-DETAIL
               END-IF
           END-IF
           MOVE TXH-RUNNING-BAL            TO WS-PREV-RUNNING-BAL
                                              WS-AT-CLOSING-BAL
           .
      *================================================================*
       2400-WRITE-TRANSACTION.
      *================================================================*
           MOVE SPACES                     TO IB3-FEED-RECORD
           SET IB3-TXN-REC                 TO TRUE
           MOVE TXH-SORT-CODE              TO IB3T-SORT-CODE
           MOVE TXH-ACCT-NO                TO IB3T-ACCT-NO
           MOVE ACM-CIF-NO                 TO IB3T-CIF-NO
           MOVE ACM-ACCT-TYPE-CD           TO IB3T-ACCT-TYPE-CD
           MOVE ACM-PROD-CD                TO IB3T-PROD-CD
           MOVE WS-BUS-DATE                TO IB3T-POST-DATE
           MOVE TXH-POST-SEQ               TO IB3T-POST-SEQ
           MOVE TXH-TXN-CODE               TO IB3T-TXN-CODE
           MOVE WS-TC-DESC (TC-IDX)        TO IB3T-TXN-DESC
           MOVE WS-TC-CATEGORY (TC-IDX)    TO IB3T-CATEGORY
           MOVE TXH-DR-CR-IND              TO IB3T-DR-CR-IND
           MOVE TXH-TXN-AMT                TO IB3T-TXN-AMT
           MOVE TXH-RUNNING-BAL            TO IB3T-RUNNING-BAL
           MOVE TXH-CCY-CD                 TO IB3T-CCY-CD
      *    DB2 DATE 'YYYY-MM-DD' TO CCYYMMDD
           STRING TXH-VALUE-DATE (1:4)
                  TXH-VALUE-DATE (6:2)
                  TXH-VALUE-DATE (9:2)
                  DELIMITED BY SIZE
                  INTO IB3T-VALUE-DATE
           END-STRING
           IF TXH-NARRATIVE-1-LEN > ZERO
               MOVE TXH-NARRATIVE-1-TEXT (1:TXH-NARRATIVE-1-LEN)
                                           TO IB3T-NARRATIVE-1
           END-IF
           IF TXH-NARRATIVE-2-LEN > ZERO
               MOVE TXH-NARRATIVE-2-TEXT (1:TXH-NARRATIVE-2-LEN)
                                           TO IB3T-NARRATIVE-2
           END-IF
           MOVE TXH-CHANNEL-CD             TO IB3T-CHANNEL-CD
           MOVE TXH-EXT-REF                TO IB3T-EXT-REF
           MOVE TXH-POST-TYPE              TO IB3T-POST-TYPE
           MOVE TXH-BATCH-RUN-ID           TO IB3T-BATCH-RUN-ID
           MOVE TXH-INSERT-TS              TO IB3T-INSERT-TS
           PERFORM 8200-WRITE-FEED

           ADD 1                           TO WS-TXN-COUNT
                                              WS-AT-TXN-COUNT
                                              WS-TT-COUNT (TT-IDX)
           IF TXH-DR-CR-IND = 'D'
               ADD TXH-TXN-AMT             TO WS-AT-TOTAL-DR
                                              WS-FEED-DR
                                              WS-TT-DR (TT-IDX)
           ELSE
               ADD TXH-TXN-AMT             TO WS-AT-TOTAL-CR
                                              WS-FEED-CR
                                              WS-TT-CR (TT-IDX)
           END-IF
           .
      *================================================================*
       2900-WRITE-SUMMARY.
      *================================================================*
      *    ACM-ACCOUNT-RECORD STILL HOLDS THE ACCOUNT BEING CLOSED OFF
      *    (THE NEXT ACCOUNT IS NOT READ UNTIL AFTER THIS PARAGRAPH).
           MOVE SPACES                     TO IB3-FEED-RECORD
           SET IB3-SUMMARY-REC             TO TRUE
           MOVE WS-CK-SORT-CODE            TO IB3S-SORT-CODE
           MOVE WS-CK-ACCT-NO              TO IB3S-ACCT-NO
           MOVE WS-AT-TXN-COUNT            TO IB3S-TXN-COUNT
           MOVE WS-AT-TOTAL-DR             TO IB3S-TOTAL-DR
           MOVE WS-AT-TOTAL-CR             TO IB3S-TOTAL-CR
           MOVE WS-AT-OPENING-BAL          TO IB3S-OPENING-BAL
           MOVE WS-AT-CLOSING-BAL          TO IB3S-CLOSING-BAL
           MOVE ACM-LEDGER-BAL             TO IB3S-LEDGER-BAL
           IF WS-AT-CLOSING-BAL = ACM-LEDGER-BAL
               SET IB3S-BAL-AGREED         TO TRUE
           ELSE
      *        CONTROL 3 - CHANNEL HIDES THE RUNNING BALANCE COLUMN
               SET IB3S-BAL-DIFFERENT      TO TRUE
               ADD 1                       TO WS-LEDGER-BREAKS
               MOVE ZERO                   TO DL-POST-SEQ
               MOVE SPACES                 TO DL-TXN-CODE
               MOVE ZERO                   TO DL-AMOUNT
               MOVE ACM-LEDGER-BAL         TO DL-EXPECTED
               MOVE WS-AT-CLOSING-BAL      TO DL-ACTUAL
               MOVE 'CLOSING RUNNING BAL NOT EQUAL TO LEDGER'
                                           TO WS-NOTE
               PERFORM 7100-REPORT-DETAIL
           END-IF
           PERFORM 8200-WRITE-FEED

           ADD 1                           TO WS-ACCT-COUNT
           IF WS-CK-ACCT-NO NUMERIC
               MOVE WS-CK-ACCT-NO          TO WS-ACCT-NUM
               ADD WS-ACCT-NUM             TO WS-HASH-ACCT-NO
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF WS-ACCT-ON-MASTER
               PERFORM 2900-WRITE-SUMMARY
           END-IF

           EXEC SQL CLOSE TXNCSR3 END-EXEC

      *---- CONTROL 1 - FETCHED ROWS AGREE WITH THE COUNT SELECT ------*
           IF WS-ROWS-FETCHED NOT = WS-DB2-ROW-COUNT
           OR WS-FETCH-DR NOT = WS-DB2-TOTAL-DR
           OR WS-FETCH-CR NOT = WS-DB2-TOTAL-CR
               DISPLAY 'IBFD0301 E001 SELECTED ' WS-DB2-ROW-COUNT
                       ' FETCHED ' WS-ROWS-FETCHED
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'TXNHIST CHANGED DURING EXTRACT - RERUN'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

      *---- TRAILER ---------------------------------------------------*
           MOVE SPACES                     TO IB3-FEED-RECORD
           SET IB3-TRAILER-REC             TO TRUE
           MOVE WS-ACCT-COUNT              TO IB3Z-ACCT-COUNT
           MOVE WS-TXN-COUNT               TO IB3Z-TXN-COUNT
           MOVE WS-FEED-DR                 TO IB3Z-TOTAL-DR
           MOVE WS-FEED-CR                 TO IB3Z-TOTAL-CR
           MOVE WS-HASH-ACCT-NO            TO IB3Z-HASH-ACCT-NO
      *    RECORD COUNT INCLUDES THE HEADER AND THIS TRAILER
           COMPUTE IB3Z-RECORD-COUNT = WS-REC-COUNT + 1
           PERFORM 8200-WRITE-FEED

           PERFORM 7300-REPORT-TOTALS

           CLOSE ACCTMAST-FILE
                 IBFEED-FILE
                 REPORT-FILE

           DISPLAY 'IBFD0301 - ROWS FETCHED        ' WS-ROWS-FETCHED
           DISPLAY 'IBFD0301 - ACCOUNTS            ' WS-ACCT-COUNT
           DISPLAY 'IBFD0301 - TRANSACTION RECORDS ' WS-TXN-COUNT
           DISPLAY 'IBFD0301 - ORPHAN ROWS         ' WS-ORPHAN-ROWS
           DISPLAY 'IBFD0301 - CHAIN BREAKS        ' WS-CHAIN-BREAKS
           DISPLAY 'IBFD0301 - LEDGER BREAKS       ' WS-LEDGER-BREAKS

           IF WS-ORPHAN-ROWS > ZERO
           OR WS-CHAIN-BREAKS > ZERO
           OR WS-LEDGER-BREAKS > ZERO
           OR WS-UNKNOWN-CODES > ZERO
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
      *    CALLER SETS DL-POST-SEQ / CODE / AMOUNTS AND WS-NOTE
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE WS-CK-SORT-CODE            TO DL-SORT-CODE
           MOVE WS-CK-ACCT-NO              TO DL-ACCT-NO
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
       7300-REPORT-TOTALS.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'TXNHIST ROWS SELECTED (CONTROL)' TO TL-LABEL
           MOVE WS-DB2-ROW-COUNT           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'TXNHIST ROWS FETCHED'     TO TL-LABEL
           MOVE WS-ROWS-FETCHED            TO TL-COUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE '  DEBITS FETCHED'         TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-FETCH-DR                TO TL-AMOUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE '  CREDITS FETCHED'        TO TL-LABEL
           MOVE WS-FETCH-CR                TO TL-AMOUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'ACCOUNTS SENT'            TO TL-LABEL
           MOVE WS-ACCT-COUNT              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'TRANSACTIONS SENT - DEBITS' TO TL-LABEL
           MOVE WS-TXN-COUNT               TO TL-COUNT
           MOVE WS-FEED-DR                 TO TL-AMOUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'TRANSACTIONS SENT - CREDITS' TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-FEED-CR                 TO TL-AMOUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'ACCOUNTS NOT ON MASTER'   TO TL-LABEL
           MOVE WS-ORPHAN-ACCTS            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE '  ROWS NOT SENT'          TO TL-LABEL
           MOVE WS-ORPHAN-ROWS             TO TL-COUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'RUNNING BALANCE CHAIN BREAKS' TO TL-LABEL
           MOVE WS-CHAIN-BREAKS            TO TL-COUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'CLOSING BALANCE NOT EQUAL TO LEDGER' TO TL-LABEL
           MOVE WS-LEDGER-BREAKS           TO TL-COUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'UNKNOWN TRANSACTION CODES' TO TL-LABEL
           MOVE WS-UNKNOWN-CODES           TO TL-COUNT
           PERFORM 7400-WRITE-TOTAL
           MOVE 'FEED RECORDS INCLUDING HEADER/TRAILER' TO TL-LABEL
           MOVE WS-REC-COUNT               TO TL-COUNT
           PERFORM 7400-WRITE-TOTAL

      *---- ANALYSIS BY TRANSACTION CODE ------------------------------*
           WRITE REPORT-RECORD             FROM WS-HEADING-3
           PERFORM VARYING TT-IDX FROM 1 BY 1
                   UNTIL TT-IDX > WS-TC-MAX
               IF WS-TT-COUNT (TT-IDX) > ZERO
                   SET TC-IDX              TO TT-IDX
                   MOVE WS-TC-CODE (TC-IDX) TO CL-CODE
                   MOVE WS-TC-DESC (TC-IDX) TO CL-DESC
                   MOVE WS-TT-COUNT (TT-IDX) TO CL-COUNT
                   MOVE WS-TT-DR (TT-IDX)  TO CL-DR
                   MOVE WS-TT-CR (TT-IDX)  TO CL-CR
                   WRITE REPORT-RECORD     FROM WS-CODE-LINE
               END-IF
           END-PERFORM
           .
      *================================================================*
       7400-WRITE-TOTAL.
      *================================================================*
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           .
      *================================================================*
       8100-FETCH-TXN.
      *================================================================*
           EXEC SQL
               FETCH TXNCSR3
                INTO :TXH-SORT-CODE, :TXH-ACCT-NO, :TXH-POST-SEQ,
                     :TXH-TXN-CODE, :TXH-DR-CR-IND, :TXH-TXN-AMT,
                     :TXH-RUNNING-BAL, :TXH-CCY-CD, :TXH-VALUE-DATE,
                     :TXH-NARRATIVE-1, :TXH-NARRATIVE-2,
                     :TXH-CHANNEL-CD, :TXH-EXT-REF, :TXH-POST-TYPE,
                     :TXH-BATCH-RUN-ID, :TXH-INSERT-TS
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   MOVE TXH-SORT-CODE      TO WS-TK-SORT-CODE
                   MOVE TXH-ACCT-NO        TO WS-TK-ACCT-NO
                   ADD 1                   TO WS-ROWS-FETCHED
                   IF TXH-DR-CR-IND = 'D'
                       ADD TXH-TXN-AMT     TO WS-FETCH-DR
                   ELSE
                       ADD TXH-TXN-AMT     TO WS-FETCH-CR
                   END-IF
               WHEN 100
                   SET WS-TXN-EOF          TO TRUE
                   MOVE HIGH-VALUES        TO WS-TXN-KEY
               WHEN OTHER
                   MOVE '8100-FETCH-TXN'   TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       8200-WRITE-FEED.
      *================================================================*
           WRITE IBFEED-RECORD             FROM IB3-FEED-RECORD
           IF WS-IBFEED-STATUS NOT = '00'
               MOVE '8200-WRITE-FEED'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'IBFEED WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-IBFEED-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-REC-COUNT
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
