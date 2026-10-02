       IDENTIFICATION DIVISION.
       PROGRAM-ID.    INACR010.
       AUTHOR.        R J HALLIDAY.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1992-02-10.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : INACR010                                            *
      * TITLE    : DAILY INTEREST ACCRUAL - CREDIT AND DEBIT           *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : INACR01 STEP IA020                                  *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (VSAM KSDS, SEQ LOAD) *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ I-O)  *
      * OUTPUT   : ACCRUALS - ACCRUAL AUDIT FILE - INACCREC            *
      *            GLJNL    - GL ACCRUAL JOURNAL - GLJNLREC            *
      *            REPORT   - ACCRUAL SUMMARY BY PRODUCT (FBA 133)     *
      *            EXCEPTS  - ACCRUAL EXCEPTIONS REPORT (FBA 133)      *
      *----------------------------------------------------------------*
      * RUNS AFTER PSEOD01 (HARD POSTING) SO THE LEDGER BALANCE IS THE *
      * END OF DAY CLEARED POSITION. INTEREST IS ACCRUED ON THE LEDGER *
      * BALANCE FOR EVERY CALENDAR DAY FROM TODAY UP TO (BUT NOT       *
      * INCLUDING) THE NEXT BUSINESS DATE - SO FRIDAY ACCRUES FRIDAY,  *
      * SATURDAY AND SUNDAY, AND THE DAY BEFORE A BANK HOLIDAY ACCRUES *
      * THE HOLIDAY TOO.                                               *
      *                                                                *
      * CREDIT INTEREST                                                *
      *   - ONLY WHEN THE PRODUCT PAYS CREDIT INTEREST AND THE LEDGER  *
      *     BALANCE IS AT LEAST THE PRODUCT MINIMUM                    *
      *   - RATE: ACCOUNT RATE (ACM-INT-RATE-CR) IF NOT ZERO, ELSE THE *
      *     PRODUCT TIER RATE(S). BONUS RATE ADDED UNTIL THE ACCOUNT'S *
      *     BONUS END DATE                                             *
      *   - TIERS 'W' WHOLE BALANCE AT THE HIGHEST TIER REACHED, OR    *
      *     'B' BANDED - EACH SLICE OF BALANCE AT ITS OWN TIER RATE    *
      *                                                                *
      * DEBIT INTEREST                                                 *
      *   - ARRANGED PART = UP TO THE OVERDRAFT LIMIT (ZERO IF THE     *
      *     LIMIT HAS EXPIRED), LESS THE INTEREST-FREE AMOUNT          *
      *   - UNARRANGED PART = ANY EXCESS OVER THE LIMIT                *
      *   - ACCOUNT RATE (ACM-INT-RATE-DR) OVERRIDES THE PRODUCT       *
      *     ARRANGED RATE. THE UNARRANGED RATE IS ALWAYS THE PRODUCT'S *
      *                                                                *
      * ROUNDING: EACH DAY'S ACCRUAL IS CALCULATED TO 5 DECIMAL PLACES.*
      * WHOLE PENCE ARE ADDED TO ACM-ACCRUED-INT-CR/DR AND THE         *
      * SUB-PENNY RESIDUE IS CARRIED IN ACM-ACCR-CR/DR-RESIDUE TO THE  *
      * NEXT DAY. RESIDUES ARE DISCARDED AT CAPITALISATION (INCAP010). *
      *                                                                *
      * RERUN: AN ACCOUNT WITH ACM-LAST-ACCRUAL-DATE >= THE BUSINESS   *
      * DATE IS SKIPPED, SO A RERUN AFTER AN ABEND IS SAFE WITHOUT A   *
      * RESTORE. THE GL JOURNAL FROM THE FAILED RUN MUST BE DELETED    *
      * (IT IS A NEW GENERATION THAT IS NOT CATALOGUED ON ABEND).      *
      *                                                                *
      * ALSO MAINTAINS THE CHARGING PERIOD COUNTER ACM-UNARR-OD-DAYS   *
      * (BUSINESS DAYS IN UNARRANGED OVERDRAFT BEYOND THE PRODUCT      *
      * BUFFER) FOR CGCAL010.                                          *
      *----------------------------------------------------------------*
      * RETURN CODES  0 OK   4 EXCEPTIONS REPORTED (SEE EXCEPTS)       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1992-02-10 RJH  ORIGINAL VERSION (REPLACES DD0300 ASSEMBLER)   *
      * 1996-04-22 PDK  TIERED RATES (INSTANT ACCESS SAVER LAUNCH)     *
      * 1998-11-30 MLW  Y2K                                            *
      * 2001-07-16 DKP  BANDED TIERS FOR E-SAVER (IBANK ONLY PRODUCT)  *
      * 2002-06-03 KLM  GL ACCRUAL JOURNAL FOR ORACLE GL               *
      * 2004-08-09 SAT  ARRANGED / UNARRANGED DEBIT SPLIT; OD EXPIRY;  *
      *                 UNARRANGED DAY COUNTER FOR CHARGES             *
      * 2006-03-13 AOS  BONUS RATE FOR FIXED PERIOD AFTER OPENING      *
      * 2008-01-07 AOS  ACTUAL/ACTUAL BASIS (CODE 000) FOR ISA         *
      * 2010-03-08 AOS  RESIDUE AND LAST ACCRUAL DATE MOVED TO THE     *
      *                 ACCOUNT MASTER EXTENSION (WAS A SEPARATE VSAM  *
      *                 FILE MBK.PRD.INT.RESIDUE) - RERUN PROTECTION   *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT ACCRUALS-FILE    ASSIGN TO ACCRUALS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ACCRUALS-STATUS.
           SELECT GLJNL-FILE       ASSIGN TO GLJNL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLJNL-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT EXCEPTS-FILE     ASSIGN TO EXCEPTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-EXCEPTS-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  ACCRUALS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACCRUALS-RECORD                 PIC X(150).
      *
       FD  GLJNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJNL-RECORD                    PIC X(250).
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
      *
       FD  EXCEPTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  EXCEPTS-RECORD                  PIC X(133).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'INACR010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-ACCRUALS-STATUS          PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-EXCEPTS-STATUS           PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-PROD-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-PRODUCTS          VALUE 'Y'.
           05  WS-PROD-FOUND-IND           PIC X(01) VALUE 'N'.
               88  WS-PROD-FOUND               VALUE 'Y'.
           05  WS-ACCT-CHANGED-IND         PIC X(01) VALUE 'N'.
               88  WS-ACCT-CHANGED             VALUE 'Y'.
           05  WS-BONUS-IND                PIC X(01) VALUE 'N'.
               88  WS-BONUS-APPLIES            VALUE 'Y'.
      *
      *---- DATES -----------------------------------------------------*
       01  WS-DATE-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-R REDEFINES WS-BUS-DATE.
               10  WS-BD-CCYY              PIC 9(04).
               10  WS-BD-MM                PIC 9(02).
               10  WS-BD-DD                PIC 9(02).
           05  WS-NEXT-BUS-DATE            PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-NEXT-DATE-INT            PIC S9(09) COMP.
           05  WS-ACCRUAL-DAYS             PIC S9(03) COMP-3.
           05  WS-YEAR-DAYS                PIC S9(03) COMP-3.
           05  WS-LEAP-REM-4               PIC S9(04) COMP.
           05  WS-LEAP-REM-100             PIC S9(04) COMP.
           05  WS-LEAP-REM-400             PIC S9(04) COMP.
           05  WS-LEAP-QUOT                PIC S9(06) COMP.
           05  WS-DATE-DISPLAY             PIC X(10).
      *
      *---- CALCULATION FIELDS ----------------------------------------*
       01  WS-CALC-FIELDS.
           05  WS-BALANCE                  PIC S9(13)V99 COMP-3.
           05  WS-DR-BALANCE               PIC S9(13)V99 COMP-3.
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-ARR-BALANCE              PIC S9(13)V99 COMP-3.
           05  WS-UNARR-BALANCE            PIC S9(13)V99 COMP-3.
           05  WS-SLICE                    PIC S9(13)V99 COMP-3.
           05  WS-SLICE-TOP                PIC S9(13)V99 COMP-3.
           05  WS-CR-RATE                  PIC S9(03)V9(04) COMP-3.
           05  WS-DR-ARR-RATE              PIC S9(03)V9(04) COMP-3.
           05  WS-DR-UNARR-RATE            PIC S9(03)V9(04) COMP-3.
           05  WS-BASIS                    PIC S9(03) COMP-3.
           05  WS-CR-ACCRUAL               PIC S9(09)V9(05) COMP-3.
           05  WS-DR-ARR-ACCRUAL           PIC S9(09)V9(05) COMP-3.
           05  WS-DR-UNARR-ACCRUAL         PIC S9(09)V9(05) COMP-3.
           05  WS-DR-ACCRUAL               PIC S9(09)V9(05) COMP-3.
           05  WS-SLICE-ACCRUAL            PIC S9(09)V9(05) COMP-3.
           05  WS-WITH-RESIDUE             PIC S9(09)V9(05) COMP-3.
           05  WS-WHOLE-PENCE              PIC S9(09)V99 COMP-3.
           05  WS-UNARR-WHOLE-PENCE        PIC S9(09)V99 COMP-3.
           05  WS-TIER-USED                PIC 9(01).
           05  WS-TIER-NO                  PIC S9(04) COMP.
           05  WS-BUFFER                   PIC S9(13)V99 COMP-3.
      *
      *---- PRODUCT TABLE (LOADED FROM PRDPARM AT START) --------------*
      * THE PRODUCT FILE IS SMALL (UNDER 200 PRODUCTS IN 2010). THE    *
      * TABLE HOLDS ONLY THE FIELDS THIS PROGRAM NEEDS, PLUS THE       *
      * ACCUMULATORS FOR THE PRODUCT SUMMARY AND THE GL JOURNAL.       *
      *----------------------------------------------------------------*
       01  WS-PRODUCT-TABLE.
           05  WS-PROD-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-PROD-MAX                 PIC S9(04) COMP VALUE +300.
           05  WS-PROD-ENTRY               OCCURS 1 TO 300 TIMES
                                           DEPENDING ON WS-PROD-COUNT
                                           ASCENDING KEY WS-PT-PROD-CD
                                           INDEXED BY PT-IDX.
               10  WS-PT-PROD-CD           PIC X(10).
               10  WS-PT-ACCT-TYPE         PIC X(04).
               10  WS-PT-CR-INT-IND        PIC X(01).
               10  WS-PT-TIER-METHOD       PIC X(01).
               10  WS-PT-TIER              OCCURS 5 TIMES.
                   15  WS-PT-TIER-THRESHOLD
                                           PIC S9(13)V99 COMP-3.
                   15  WS-PT-TIER-RATE     PIC S9(03)V9(04) COMP-3.
               10  WS-PT-BONUS-RATE        PIC S9(03)V9(04) COMP-3.
               10  WS-PT-CR-MIN-BAL        PIC S9(13)V99 COMP-3.
               10  WS-PT-CR-BASIS          PIC S9(03) COMP-3.
               10  WS-PT-DR-INT-IND        PIC X(01).
               10  WS-PT-DR-RATE-ARR       PIC S9(03)V9(04) COMP-3.
               10  WS-PT-DR-RATE-UNARR     PIC S9(03)V9(04) COMP-3.
               10  WS-PT-DR-BASIS          PIC S9(03) COMP-3.
               10  WS-PT-DR-FREE-AMT       PIC S9(13)V99 COMP-3.
               10  WS-PT-UNARR-BUFFER      PIC S9(13)V99 COMP-3.
               10  WS-PT-GL-INT-EXPENSE    PIC X(10).
               10  WS-PT-GL-INT-INCOME     PIC X(10).
               10  WS-PT-GL-ACCR-PAY       PIC X(10).
               10  WS-PT-GL-ACCR-REC       PIC X(10).
               10  WS-PT-COST-CENTRE       PIC X(06).
               10  WS-PT-CCY-CD            PIC X(03).
      *        ACCUMULATORS
               10  WS-PT-ACCT-COUNT        PIC S9(09) COMP-3.
               10  WS-PT-CR-ACCT-COUNT     PIC S9(09) COMP-3.
               10  WS-PT-DR-ACCT-COUNT     PIC S9(09) COMP-3.
               10  WS-PT-CR-BAL-TOTAL      PIC S9(15)V99 COMP-3.
               10  WS-PT-DR-BAL-TOTAL      PIC S9(15)V99 COMP-3.
               10  WS-PT-CR-ACCR-TOTAL     PIC S9(13)V99 COMP-3.
               10  WS-PT-DR-ACCR-TOTAL     PIC S9(13)V99 COMP-3.
               10  WS-PT-UNARR-ACCR-TOTAL  PIC S9(13)V99 COMP-3.
      *
      *---- COUNTERS --------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-SKIP-CLOSED              PIC S9(09) COMP-3 VALUE 0.
           05  WS-SKIP-RERUN               PIC S9(09) COMP-3 VALUE 0.
           05  WS-SKIP-NO-PRODUCT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCRUED-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-ZERO-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-REWRITE-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUDIT-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXCEPTION-COUNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-GL-LINE-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-CR-ACCRUAL         PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-DR-ACCRUAL         PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-TOTAL-DR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-TOTAL-CR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-EXCEPTION-FIELDS.
           05  WS-EXC-CODE                 PIC X(04).
           05  WS-EXC-TEXT                 PIC X(60).
      *
      *---- REPORT LINES ----------------------------------------------*
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
           05  WS-EXC-LINE-COUNT           PIC S9(04) COMP VALUE +99.
           05  WS-EXC-PAGE-COUNT           PIC S9(04) COMP VALUE ZERO.
      *
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'INACR010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  HD1-TITLE                   PIC X(55)
               VALUE 'MEGABANK PLC - DAILY INTEREST ACCRUAL SUMMARY'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
      *
       01  WS-HEADING-2.
           05  FILLER                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(36) VALUE SPACES.
           05  FILLER                      PIC X(20)
               VALUE 'ACCRUAL DAYS      : '.
           05  HD2-DAYS                    PIC ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(20)
               VALUE 'ACCRUED TO (EXCL) : '.
           05  HD2-NEXT-DATE               PIC X(10).
           05  FILLER                      PIC X(38) VALUE SPACES.
      *
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(11) VALUE 'PRODUCT'.
           05  FILLER            PIC X(09) VALUE '  ACCTS'.
           05  FILLER            PIC X(09) VALUE ' CR ACCTS'.
           05  FILLER            PIC X(20) VALUE '     CREDIT BALANCES'.
           05  FILLER            PIC X(16) VALUE '      CR ACCRUAL'.
           05  FILLER            PIC X(09) VALUE ' DR ACCTS'.
           05  FILLER            PIC X(20) VALUE '      DEBIT BALANCES'.
           05  FILLER            PIC X(16) VALUE '      DR ACCRUAL'.
           05  FILLER            PIC X(16) VALUE '   OF WHICH UNAR'.
           05  FILLER            PIC X(06) VALUE SPACES.
       01  WS-HEADING-4.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
      *
       01  WS-SUMMARY-LINE.
           05  SL-CC                       PIC X(01) VALUE ' '.
           05  SL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  SL-ACCTS                    PIC Z,ZZZ,ZZ9.
           05  SL-CR-ACCTS                 PIC Z,ZZZ,ZZ9.
           05  SL-CR-BAL                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  SL-CR-ACCR                  PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  SL-DR-ACCTS                 PIC Z,ZZZ,ZZ9.
           05  SL-DR-BAL                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  SL-DR-ACCR                  PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  SL-UNARR-ACCR               PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(09) VALUE SPACES.
      *
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
       01  WS-EXC-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'INACR010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - INTEREST ACCRUAL EXCEPTIONS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  EH1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  EH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-EXC-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(12) VALUE 'PRODUCT'.
           05  FILLER            PIC X(06) VALUE 'CODE'.
           05  FILLER            PIC X(20) VALUE '      LEDGER BALANCE'.
           05  FILLER            PIC X(03) VALUE SPACES.
           05  FILLER            PIC X(60) VALUE 'DESCRIPTION'.
           05  FILLER            PIC X(12) VALUE SPACES.
      *
       01  WS-EXC-DETAIL.
           05  ED-CC                       PIC X(01) VALUE ' '.
           05  ED-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  ED-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  ED-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  ED-CODE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  ED-BALANCE                  PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  ED-TEXT                     PIC X(60).
           05  FILLER                      PIC X(12) VALUE SPACES.
      *
           COPY INACCREC.
      *
           COPY GLJNLREC.
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
           PERFORM 2000-PROCESS-ACCOUNT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                OUTPUT ACCRUALS-FILE
                       GLJNL-FILE
                       REPORT-FILE
                       EXCEPTS-FILE

           IF NOT WS-PRDPARM-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-ACCRUALS-STATUS NOT = '00'
           OR WS-GLJNL-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-EXCEPTS-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-PRDPARM-STATUS ' ' WS-ACCTMAST-STATUS ' '
                      WS-ACCRUALS-STATUS ' ' WS-GLJNL-STATUS ' '
                      WS-REPORT-STATUS ' ' WS-EXCEPTS-STATUS ' '
                      WS-DATECARD-STATUS
                      DELIMITED BY SIZE
                      INTO ABN-KEY-DATA
               END-STRING
               PERFORM Z999-ABEND
           END-IF

           PERFORM 1100-READ-DATE-CARD
           PERFORM 1200-CALCULATE-DAYS
           PERFORM 1300-LOAD-PRODUCTS

           CLOSE PRDPARM-FILE

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       1100-READ-DATE-CARD.
      *================================================================*
           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
           OR DTC-BUSINESS-DATE NOT NUMERIC
           OR DTC-NEXT-BUS-DATE NOT NUMERIC
               MOVE '1100-READ-DATE-CARD'  TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               MOVE DATECARD-RECORD (1:40) TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE

           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           MOVE DTC-NEXT-BUS-DATE          TO WS-NEXT-BUS-DATE

           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO WS-DATE-DISPLAY
           END-STRING
           MOVE WS-DATE-DISPLAY            TO HD1-BUS-DATE
                                              EH1-BUS-DATE

           DISPLAY 'INACR010 - BUSINESS DATE ' WS-DATE-DISPLAY
                   ' RUN ID ' DTC-RUN-ID
           .
      *================================================================*
       1200-CALCULATE-DAYS.
      *================================================================*
      *    NUMBER OF CALENDAR DAYS TO ACCRUE = NEXT BUSINESS DATE MINUS
      *    TODAY. NORMALLY 1; 3 ON A FRIDAY; 4 BEFORE A BANK HOLIDAY
      *    MONDAY; UP TO 5 OVER CHRISTMAS.
           COMPUTE WS-BUS-DATE-INT =
                   FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
           COMPUTE WS-NEXT-DATE-INT =
                   FUNCTION INTEGER-OF-DATE (WS-NEXT-BUS-DATE)
           COMPUTE WS-ACCRUAL-DAYS =
                   WS-NEXT-DATE-INT - WS-BUS-DATE-INT

           IF WS-ACCRUAL-DAYS < 1
           OR WS-ACCRUAL-DAYS > 7
               MOVE '1200-CALCULATE-DAYS'  TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'ACCRUAL DAYS OUT OF RANGE 1-7 - CHECK DATE CARD'
                                           TO ABN-MESSAGE
               MOVE DTC-DATE-CARD (1:40)   TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           MOVE WS-ACCRUAL-DAYS            TO HD2-DAYS
           STRING WS-NEXT-BUS-DATE (1:4) '-' WS-NEXT-BUS-DATE (5:2)
                  '-' WS-NEXT-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD2-NEXT-DATE
           END-STRING

      *    DAYS IN THE CURRENT YEAR FOR ACTUAL/ACTUAL BASIS.
           DIVIDE WS-BD-CCYY BY 4   GIVING WS-LEAP-QUOT
                                    REMAINDER WS-LEAP-REM-4
           DIVIDE WS-BD-CCYY BY 100 GIVING WS-LEAP-QUOT
                                    REMAINDER WS-LEAP-REM-100
           DIVIDE WS-BD-CCYY BY 400 GIVING WS-LEAP-QUOT
                                    REMAINDER WS-LEAP-REM-400
           IF (WS-LEAP-REM-4 = 0 AND WS-LEAP-REM-100 NOT = 0)
           OR WS-LEAP-REM-400 = 0
               MOVE 366                    TO WS-YEAR-DAYS
           ELSE
               MOVE 365                    TO WS-YEAR-DAYS
           END-IF

           DISPLAY 'INACR010 - ACCRUAL DAYS   ' WS-ACCRUAL-DAYS
                   ' DAYS IN YEAR ' WS-YEAR-DAYS
           .
      *================================================================*
       1300-LOAD-PRODUCTS.
      *================================================================*
      *    THE KSDS IS IN PRODUCT CODE ORDER, SO THE TABLE IS LOADED
      *    IN ASCENDING KEY ORDER READY FOR SEARCH ALL.
           MOVE ZERO                       TO WS-PROD-COUNT
           PERFORM 1310-READ-PRODUCT
           PERFORM UNTIL WS-END-OF-PRODUCTS
               IF WS-PROD-COUNT >= WS-PROD-MAX
                   MOVE '1300-LOAD-PRODUCTS' TO ABN-PARAGRAPH
                   MOVE 1004               TO ABN-ABEND-CODE
                   MOVE 'PRODUCT TABLE FULL - INCREASE WS-PROD-MAX'
                                           TO ABN-MESSAGE
                   MOVE PRD-PROD-CD        TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
               ADD 1                       TO WS-PROD-COUNT
               SET PT-IDX                  TO WS-PROD-COUNT
               PERFORM 1320-STORE-PRODUCT
               PERFORM 1310-READ-PRODUCT
           END-PERFORM

           IF WS-PROD-COUNT = ZERO
               MOVE '1300-LOAD-PRODUCTS'   TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'PRODUCT PARAMETER FILE IS EMPTY'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           DISPLAY 'INACR010 - PRODUCTS LOADED ' WS-PROD-COUNT
           .
      *================================================================*
       1310-READ-PRODUCT.
      *================================================================*
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN WS-PRDPARM-OK
                   CONTINUE
               WHEN WS-PRDPARM-EOF
                   SET WS-END-OF-PRODUCTS  TO TRUE
               WHEN OTHER
                   MOVE '1310-READ-PRODUCT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PRDPARM READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-PRDPARM-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       1320-STORE-PRODUCT.
      *================================================================*
           MOVE PRD-PROD-CD                TO WS-PT-PROD-CD (PT-IDX)
           MOVE PRD-ACCT-TYPE-CD           TO WS-PT-ACCT-TYPE (PT-IDX)
           MOVE PRD-CR-INT-IND             TO WS-PT-CR-INT-IND (PT-IDX)
           MOVE PRD-TIER-METHOD            TO WS-PT-TIER-METHOD (PT-IDX)
           PERFORM VARYING WS-TIER-NO FROM 1 BY 1
                   UNTIL WS-TIER-NO > 5
               MOVE PRD-CR-TIER-THRESHOLD (WS-TIER-NO)
                     TO WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO)
               MOVE PRD-CR-TIER-RATE (WS-TIER-NO)
                     TO WS-PT-TIER-RATE (PT-IDX, WS-TIER-NO)
           END-PERFORM
           MOVE PRD-CR-BONUS-RATE          TO WS-PT-BONUS-RATE (PT-IDX)
           MOVE PRD-CR-MIN-BAL             TO WS-PT-CR-MIN-BAL (PT-IDX)
           MOVE PRD-CR-DAY-BASIS           TO WS-PT-CR-BASIS (PT-IDX)
           MOVE PRD-DR-INT-IND             TO WS-PT-DR-INT-IND (PT-IDX)
           MOVE PRD-DR-RATE-ARRANGED       TO WS-PT-DR-RATE-ARR (PT-IDX)
           MOVE PRD-DR-RATE-UNARRANGED
                                     TO WS-PT-DR-RATE-UNARR (PT-IDX)
           MOVE PRD-DR-DAY-BASIS           TO WS-PT-DR-BASIS (PT-IDX)
           MOVE PRD-DR-INT-FREE-AMT        TO WS-PT-DR-FREE-AMT (PT-IDX)
           MOVE PRD-UNARR-OD-BUFFER
                                     TO WS-PT-UNARR-BUFFER (PT-IDX)
           MOVE PRD-GL-INT-EXPENSE
                                     TO WS-PT-GL-INT-EXPENSE (PT-IDX)
           MOVE PRD-GL-INT-INCOME    TO WS-PT-GL-INT-INCOME (PT-IDX)
           MOVE PRD-GL-INT-ACCR-PAY  TO WS-PT-GL-ACCR-PAY (PT-IDX)
           MOVE PRD-GL-INT-ACCR-REC  TO WS-PT-GL-ACCR-REC (PT-IDX)
           MOVE PRD-COST-CENTRE      TO WS-PT-COST-CENTRE (PT-IDX)
           MOVE PRD-CCY-CD           TO WS-PT-CCY-CD (PT-IDX)

           MOVE ZERO TO WS-PT-ACCT-COUNT (PT-IDX)
                        WS-PT-CR-ACCT-COUNT (PT-IDX)
                        WS-PT-DR-ACCT-COUNT (PT-IDX)
                        WS-PT-CR-BAL-TOTAL (PT-IDX)
                        WS-PT-DR-BAL-TOTAL (PT-IDX)
                        WS-PT-CR-ACCR-TOTAL (PT-IDX)
                        WS-PT-DR-ACCR-TOTAL (PT-IDX)
                        WS-PT-UNARR-ACCR-TOTAL (PT-IDX)

      *    A BASIS OF ZERO MEANS ACTUAL/ACTUAL - USE DAYS IN THIS YEAR.
           IF WS-PT-CR-BASIS (PT-IDX) = ZERO
               MOVE WS-YEAR-DAYS           TO WS-PT-CR-BASIS (PT-IDX)
           END-IF
           IF WS-PT-DR-BASIS (PT-IDX) = ZERO
               MOVE WS-YEAR-DAYS           TO WS-PT-DR-BASIS (PT-IDX)
           END-IF
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           MOVE 'N'                        TO WS-ACCT-CHANGED-IND

           EVALUATE TRUE
               WHEN ACM-CLOSED
                   ADD 1                   TO WS-SKIP-CLOSED
               WHEN ACM-LAST-ACCRUAL-DATE NUMERIC
               AND  ACM-LAST-ACCRUAL-DATE >= WS-BUS-DATE
                   ADD 1                   TO WS-SKIP-RERUN
               WHEN OTHER
                   PERFORM 2100-FIND-PRODUCT
                   IF WS-PROD-FOUND
                       PERFORM 2200-ACCRUE-ACCOUNT
                   ELSE
                       ADD 1               TO WS-SKIP-NO-PRODUCT
                       MOVE 'NPRD'         TO WS-EXC-CODE
                       MOVE 'PRODUCT NOT ON PARAMETER FILE - NO ACCRUAL'
                                           TO WS-EXC-TEXT
                       PERFORM 7500-WRITE-EXCEPTION
                   END-IF
           END-EVALUATE

           IF WS-ACCT-CHANGED
               PERFORM 2900-REWRITE-ACCOUNT
           END-IF

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       2100-FIND-PRODUCT.
      *================================================================*
           MOVE 'N'                        TO WS-PROD-FOUND-IND
           SEARCH ALL WS-PROD-ENTRY
               AT END
                   CONTINUE
               WHEN WS-PT-PROD-CD (PT-IDX) = ACM-PROD-CD
                   SET WS-PROD-FOUND       TO TRUE
           END-SEARCH
           .
      *================================================================*
       2200-ACCRUE-ACCOUNT.
      *================================================================*
           ADD 1                           TO WS-PT-ACCT-COUNT (PT-IDX)
           MOVE ACM-LEDGER-BAL             TO WS-BALANCE
           MOVE ZERO                       TO WS-CR-ACCRUAL
                                              WS-DR-ARR-ACCRUAL
                                              WS-DR-UNARR-ACCRUAL
                                              WS-DR-ACCRUAL
                                              WS-CR-RATE
                                              WS-DR-ARR-RATE
                                              WS-DR-UNARR-RATE
                                              WS-ARR-BALANCE
                                              WS-UNARR-BALANCE
                                              WS-TIER-USED
           MOVE 'N'                        TO WS-BONUS-IND

           EVALUATE TRUE
               WHEN WS-BALANCE > ZERO
                   PERFORM 2300-CREDIT-INTEREST THRU 2300-EXIT
               WHEN WS-BALANCE < ZERO
                   PERFORM 2400-DEBIT-INTEREST THRU 2400-EXIT
                   PERFORM 2500-UNARRANGED-DAYS
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           PERFORM 2600-APPLY-ACCRUALS

           IF WS-CR-ACCRUAL = ZERO
           AND WS-DR-ACCRUAL = ZERO
               ADD 1                       TO WS-ZERO-COUNT
           ELSE
               ADD 1                       TO WS-ACCRUED-COUNT
               PERFORM 2700-WRITE-AUDIT
           END-IF

      *    THE ACCRUAL DATE IS SET EVEN WHEN NOTHING WAS ACCRUED SO
      *    THAT A RERUN SKIPS THE ACCOUNT.
           MOVE WS-BUS-DATE                TO ACM-LAST-ACCRUAL-DATE
           SET WS-ACCT-CHANGED             TO TRUE
           .
      *================================================================*
       2300-CREDIT-INTEREST.
      *================================================================*
           IF WS-PT-CR-INT-IND (PT-IDX) NOT = 'Y'
               GO TO 2300-EXIT
           END-IF

           ADD 1                         TO WS-PT-CR-ACCT-COUNT (PT-IDX)
           ADD WS-BALANCE                TO WS-PT-CR-BAL-TOTAL (PT-IDX)

           IF WS-BALANCE < WS-PT-CR-MIN-BAL (PT-IDX)
               GO TO 2300-EXIT
           END-IF

           MOVE WS-PT-CR-BASIS (PT-IDX)    TO WS-BASIS

      *    BONUS PERIOD - ACM-BONUS-END-DATE IS SET AT OPENING BY
      *    DDOPN01 FROM PRD-CR-BONUS-MONTHS.
           IF  WS-PT-BONUS-RATE (PT-IDX) > ZERO
           AND ACM-BONUS-END-DATE NUMERIC
           AND ACM-BONUS-END-DATE > WS-BUS-DATE
               SET WS-BONUS-APPLIES        TO TRUE
           END-IF

           EVALUATE TRUE
      *        ACCOUNT-LEVEL NEGOTIATED RATE OVERRIDES THE TIERS.
               WHEN ACM-INT-RATE-CR NOT = ZERO
                   MOVE ACM-INT-RATE-CR    TO WS-CR-RATE
                   MOVE 9                  TO WS-TIER-USED
                   PERFORM 2310-ACCRUE-FLAT-CREDIT
               WHEN WS-PT-TIER-METHOD (PT-IDX) = 'B'
                   PERFORM 2330-ACCRUE-BANDED-CREDIT
               WHEN OTHER
                   PERFORM 2320-FIND-WHOLE-TIER
                   PERFORM 2310-ACCRUE-FLAT-CREDIT
           END-EVALUATE

           IF WS-BONUS-APPLIES
               PERFORM 2340-ACCRUE-BONUS
           END-IF
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2310-ACCRUE-FLAT-CREDIT.
      *================================================================*
           IF WS-CR-RATE > ZERO
               COMPUTE WS-CR-ACCRUAL ROUNDED =
                       WS-BALANCE * WS-CR-RATE / 100
                     * WS-ACCRUAL-DAYS / WS-BASIS
               END-COMPUTE
           END-IF
           .
      *================================================================*
       2320-FIND-WHOLE-TIER.
      *================================================================*
      *    WHOLE BALANCE METHOD: THE HIGHEST TIER WHOSE THRESHOLD THE
      *    BALANCE REACHES. TIER 1 THRESHOLD IS NORMALLY ZERO. AN
      *    UNUSED TIER HAS A ZERO THRESHOLD AND IS IGNORED AFTER TIER 1.
           MOVE ZERO                       TO WS-CR-RATE
           MOVE ZERO                       TO WS-TIER-USED
           PERFORM VARYING WS-TIER-NO FROM 1 BY 1
                   UNTIL WS-TIER-NO > 5
               IF WS-TIER-NO = 1
               OR WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO) > ZERO
                   IF WS-BALANCE >=
                      WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO)
                       MOVE WS-PT-TIER-RATE (PT-IDX, WS-TIER-NO)
                                           TO WS-CR-RATE
                       MOVE WS-TIER-NO     TO WS-TIER-USED
                   END-IF
               END-IF
           END-PERFORM
           .
      *================================================================*
       2330-ACCRUE-BANDED-CREDIT.
      *================================================================*
      *    BANDED METHOD: EACH SLICE OF THE BALANCE BETWEEN ONE TIER
      *    THRESHOLD AND THE NEXT EARNS THAT TIER'S RATE. WS-CR-RATE IS
      *    SET TO THE BLENDED (EFFECTIVE) RATE FOR THE AUDIT RECORD.
           MOVE ZERO                       TO WS-CR-ACCRUAL
           MOVE ZERO                       TO WS-TIER-USED
           PERFORM VARYING WS-TIER-NO FROM 1 BY 1
                   UNTIL WS-TIER-NO > 5
               IF WS-TIER-NO = 1
               OR WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO) > ZERO
                   IF WS-BALANCE >
                      WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO)
                       PERFORM 2335-ACCRUE-ONE-BAND
                       MOVE WS-TIER-NO     TO WS-TIER-USED
                   END-IF
               END-IF
           END-PERFORM

           IF WS-BALANCE > ZERO
           AND WS-CR-ACCRUAL > ZERO
               COMPUTE WS-CR-RATE ROUNDED =
                       WS-CR-ACCRUAL * 100 * WS-BASIS
                     / (WS-BALANCE * WS-ACCRUAL-DAYS)
               END-COMPUTE
           END-IF
           .
      *================================================================*
       2335-ACCRUE-ONE-BAND.
      *================================================================*
      *    TOP OF THIS BAND = NEXT TIER'S THRESHOLD, OR THE BALANCE IF
      *    THIS IS THE LAST USED TIER.
           MOVE WS-BALANCE                 TO WS-SLICE-TOP
           IF WS-TIER-NO < 5
               IF WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO + 1) > ZERO
               AND WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO + 1)
                   < WS-BALANCE
                   MOVE WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO + 1)
                                           TO WS-SLICE-TOP
               END-IF
           END-IF

           COMPUTE WS-SLICE = WS-SLICE-TOP
                            - WS-PT-TIER-THRESHOLD (PT-IDX, WS-TIER-NO)

           IF WS-SLICE > ZERO
               COMPUTE WS-SLICE-ACCRUAL ROUNDED =
                       WS-SLICE
                     * WS-PT-TIER-RATE (PT-IDX, WS-TIER-NO) / 100
                     * WS-ACCRUAL-DAYS / WS-BASIS
               END-COMPUTE
               ADD WS-SLICE-ACCRUAL        TO WS-CR-ACCRUAL
           END-IF
           .
      *================================================================*
       2340-ACCRUE-BONUS.
      *================================================================*
      *    BONUS IS PAID ON THE WHOLE BALANCE ON TOP OF THE TIER RATE.
           COMPUTE WS-SLICE-ACCRUAL ROUNDED =
                   WS-BALANCE * WS-PT-BONUS-RATE (PT-IDX) / 100
                 * WS-ACCRUAL-DAYS / WS-BASIS
           END-COMPUTE
           ADD WS-SLICE-ACCRUAL            TO WS-CR-ACCRUAL
           ADD WS-PT-BONUS-RATE (PT-IDX)   TO WS-CR-RATE
           .
      *================================================================*
       2400-DEBIT-INTEREST.
      *================================================================*
           COMPUTE WS-DR-BALANCE = 0 - WS-BALANCE
           ADD 1                         TO WS-PT-DR-ACCT-COUNT (PT-IDX)
           ADD WS-DR-BALANCE             TO WS-PT-DR-BAL-TOTAL (PT-IDX)

           IF WS-PT-DR-INT-IND (PT-IDX) NOT = 'Y'
      *        SAVINGS PRODUCTS SHOULD NEVER BE OVERDRAWN.
               MOVE 'DRNI'                 TO WS-EXC-CODE
               MOVE 'OVERDRAWN BUT PRODUCT HAS NO DEBIT INTEREST'
                                           TO WS-EXC-TEXT
               PERFORM 7500-WRITE-EXCEPTION
               GO TO 2400-EXIT
           END-IF

           MOVE WS-PT-DR-BASIS (PT-IDX)    TO WS-BASIS

      *    EFFECTIVE OVERDRAFT LIMIT - ZERO IF EXPIRED.
           MOVE ACM-OD-LIMIT               TO WS-OD-LIMIT
           IF ACM-OD-EXPIRY-DATE NOT = ZERO
           AND ACM-OD-EXPIRY-DATE < WS-BUS-DATE
               MOVE ZERO                   TO WS-OD-LIMIT
           END-IF

           IF WS-DR-BALANCE > WS-OD-LIMIT
               MOVE WS-OD-LIMIT            TO WS-ARR-BALANCE
               COMPUTE WS-UNARR-BALANCE = WS-DR-BALANCE - WS-OD-LIMIT
           ELSE
               MOVE WS-DR-BALANCE          TO WS-ARR-BALANCE
               MOVE ZERO                   TO WS-UNARR-BALANCE
           END-IF

      *    INTEREST-FREE OVERDRAFT AMOUNT (E.G. FIRST 250 FOR STUDENT
      *    AND GRADUATE ACCOUNTS) COMES OFF THE ARRANGED PART ONLY.
           IF WS-PT-DR-FREE-AMT (PT-IDX) > ZERO
               IF WS-ARR-BALANCE > WS-PT-DR-FREE-AMT (PT-IDX)
                   SUBTRACT WS-PT-DR-FREE-AMT (PT-IDX)
                                           FROM WS-ARR-BALANCE
               ELSE
                   MOVE ZERO               TO WS-ARR-BALANCE
               END-IF
           END-IF

           IF ACM-INT-RATE-DR NOT = ZERO
               MOVE ACM-INT-RATE-DR        TO WS-DR-ARR-RATE
           ELSE
               MOVE WS-PT-DR-RATE-ARR (PT-IDX)
                                           TO WS-DR-ARR-RATE
           END-IF
           MOVE WS-PT-DR-RATE-UNARR (PT-IDX)
                                           TO WS-DR-UNARR-RATE

           IF WS-ARR-BALANCE > ZERO AND WS-DR-ARR-RATE > ZERO
               COMPUTE WS-DR-ARR-ACCRUAL ROUNDED =
                       WS-ARR-BALANCE * WS-DR-ARR-RATE / 100
                     * WS-ACCRUAL-DAYS / WS-BASIS
               END-COMPUTE
           END-IF

           IF WS-UNARR-BALANCE > ZERO AND WS-DR-UNARR-RATE > ZERO
               COMPUTE WS-DR-UNARR-ACCRUAL ROUNDED =
                       WS-UNARR-BALANCE * WS-DR-UNARR-RATE / 100
                     * WS-ACCRUAL-DAYS / WS-BASIS
               END-COMPUTE
           END-IF

           COMPUTE WS-DR-ACCRUAL = WS-DR-ARR-ACCRUAL
                                 + WS-DR-UNARR-ACCRUAL
           .
       2400-EXIT.
           EXIT.
      *================================================================*
       2500-UNARRANGED-DAYS.
      *================================================================*
      *    COUNT ONE DAY (PER BUSINESS DAY, NOT PER CALENDAR DAY) WHEN
      *    THE ACCOUNT IS OVERDRAWN BEYOND ITS LIMIT BY MORE THAN THE
      *    PRODUCT BUFFER. CGCAL010 CHARGES PRD-UNARR-OD-DAILY-FEE PER
      *    DAY UP TO PRD-UNARR-OD-MAX-DAYS IN THE CHARGING PERIOD.
           MOVE WS-PT-UNARR-BUFFER (PT-IDX) TO WS-BUFFER
           IF WS-UNARR-BALANCE > WS-BUFFER
               IF ACM-UNARR-OD-DAYS NOT NUMERIC
                   MOVE ZERO               TO ACM-UNARR-OD-DAYS
               END-IF
               IF ACM-UNARR-OD-DAYS < 999
                   ADD 1                   TO ACM-UNARR-OD-DAYS
               END-IF
           END-IF
           .
      *================================================================*
       2600-APPLY-ACCRUALS.
      *================================================================*
           IF ACM-ACCR-CR-RESIDUE NOT NUMERIC
               MOVE ZERO                   TO ACM-ACCR-CR-RESIDUE
           END-IF
           IF ACM-ACCR-DR-RESIDUE NOT NUMERIC
               MOVE ZERO                   TO ACM-ACCR-DR-RESIDUE
           END-IF
           IF ACM-ACCR-DR-UNARR NOT NUMERIC
               MOVE ZERO                   TO ACM-ACCR-DR-UNARR
           END-IF

      *---- CREDIT ----------------------------------------------------*
           IF WS-CR-ACCRUAL > ZERO
               COMPUTE WS-WITH-RESIDUE =
                       WS-CR-ACCRUAL + ACM-ACCR-CR-RESIDUE
               COMPUTE WS-WHOLE-PENCE =
                       FUNCTION INTEGER-PART (WS-WITH-RESIDUE * 100)
                       / 100
               COMPUTE ACM-ACCR-CR-RESIDUE =
                       WS-WITH-RESIDUE - WS-WHOLE-PENCE
               ADD WS-WHOLE-PENCE          TO ACM-ACCRUED-INT-CR
               ADD WS-WHOLE-PENCE          TO WS-PT-CR-ACCR-TOTAL
                                                         (PT-IDX)
               ADD WS-WHOLE-PENCE          TO WS-TOTAL-CR-ACCRUAL
           END-IF

      *---- DEBIT -----------------------------------------------------*
           IF WS-DR-ACCRUAL > ZERO
               COMPUTE WS-WITH-RESIDUE =
                       WS-DR-ACCRUAL + ACM-ACCR-DR-RESIDUE
               COMPUTE WS-WHOLE-PENCE =
                       FUNCTION INTEGER-PART (WS-WITH-RESIDUE * 100)
                       / 100
               COMPUTE ACM-ACCR-DR-RESIDUE =
                       WS-WITH-RESIDUE - WS-WHOLE-PENCE
               ADD WS-WHOLE-PENCE          TO ACM-ACCRUED-INT-DR
               ADD WS-WHOLE-PENCE          TO WS-PT-DR-ACCR-TOTAL
                                                         (PT-IDX)
               ADD WS-WHOLE-PENCE          TO WS-TOTAL-DR-ACCRUAL

      *        THE UNARRANGED SHARE IS TRACKED SEPARATELY FOR THE
      *        STATEMENT AND THE CHARGES PRE-NOTIFICATION. IT IS
      *        ROUNDED DOWN SO IT CAN NEVER EXCEED THE TOTAL.
               IF WS-DR-UNARR-ACCRUAL > ZERO
                   COMPUTE WS-UNARR-WHOLE-PENCE =
                       FUNCTION INTEGER-PART
                               (WS-DR-UNARR-ACCRUAL * 100) / 100
                   ADD WS-UNARR-WHOLE-PENCE TO ACM-ACCR-DR-UNARR
                   ADD WS-UNARR-WHOLE-PENCE
                                 TO WS-PT-UNARR-ACCR-TOTAL (PT-IDX)
               END-IF
           END-IF
           .
      *================================================================*
       2700-WRITE-AUDIT.
      *================================================================*
           MOVE SPACES                     TO IAC-ACCRUAL-RECORD
           MOVE ACM-SORT-CODE              TO IAC-SORT-CODE
           MOVE ACM-ACCT-NO                TO IAC-ACCT-NO
           MOVE WS-BUS-DATE                TO IAC-ACCRUAL-DATE
           MOVE ACM-PROD-CD                TO IAC-PROD-CD
           MOVE WS-ACCRUAL-DAYS            TO IAC-DAYS
           MOVE WS-BALANCE                 TO IAC-BALANCE-USED
           MOVE WS-CR-RATE                 TO IAC-CR-RATE
           MOVE WS-CR-ACCRUAL              TO IAC-CR-ACCRUAL
           MOVE WS-ARR-BALANCE             TO IAC-DR-ARR-BALANCE
           MOVE WS-DR-ARR-RATE             TO IAC-DR-ARR-RATE
           MOVE WS-DR-ARR-ACCRUAL          TO IAC-DR-ARR-ACCRUAL
           MOVE WS-UNARR-BALANCE           TO IAC-DR-UNARR-BALANCE
           MOVE WS-DR-UNARR-RATE           TO IAC-DR-UNARR-RATE
           MOVE WS-DR-UNARR-ACCRUAL        TO IAC-DR-UNARR-ACCRUAL
           MOVE ACM-ACCRUED-INT-CR         TO IAC-CR-ACCRUED-TO-DATE
           MOVE ACM-ACCRUED-INT-DR         TO IAC-DR-ACCRUED-TO-DATE
           MOVE ACM-ACCR-CR-RESIDUE        TO IAC-CR-RESIDUE
           MOVE ACM-ACCR-DR-RESIDUE        TO IAC-DR-RESIDUE
           MOVE WS-TIER-USED               TO IAC-TIER-USED
           MOVE WS-BONUS-IND               TO IAC-BONUS-IND
           MOVE DTC-RUN-ID                 TO IAC-RUN-ID

           WRITE ACCRUALS-RECORD           FROM IAC-ACCRUAL-RECORD
           IF WS-ACCRUALS-STATUS NOT = '00'
               MOVE '2700-WRITE-AUDIT'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCRUALS WRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-ACCRUALS-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-AUDIT-COUNT
           .
      *================================================================*
       2900-REWRITE-ACCOUNT.
      *================================================================*
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER

           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '2900-REWRITE-ACCOUNT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-REWRITE-COUNT
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 3100-WRITE-GL-JOURNAL
           PERFORM 3200-PRODUCT-SUMMARY
           PERFORM 3300-RUN-TOTALS

           CLOSE ACCTMAST-FILE
                 ACCRUALS-FILE
                 GLJNL-FILE
                 REPORT-FILE
                 EXCEPTS-FILE

           DISPLAY 'INACR010 - ACCOUNTS READ       ' WS-READ-COUNT
           DISPLAY 'INACR010 - ACCRUED             ' WS-ACCRUED-COUNT
           DISPLAY 'INACR010 - NOTHING TO ACCRUE   ' WS-ZERO-COUNT
           DISPLAY 'INACR010 - SKIPPED CLOSED      ' WS-SKIP-CLOSED
           DISPLAY 'INACR010 - SKIPPED (RERUN)     ' WS-SKIP-RERUN
           DISPLAY 'INACR010 - SKIPPED NO PRODUCT  ' WS-SKIP-NO-PRODUCT
           DISPLAY 'INACR010 - EXCEPTIONS          ' WS-EXCEPTION-COUNT
           DISPLAY 'INACR010 - GL LINES            ' WS-GL-LINE-COUNT

           IF WS-SKIP-RERUN > ZERO
               DISPLAY 'INACR010 W001 ' WS-SKIP-RERUN
                       ' ACCOUNTS ALREADY ACCRUED - RERUN ASSUMED'
           END-IF

           IF WS-EXCEPTION-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       3100-WRITE-GL-JOURNAL.
      *================================================================*
      *    ONE JOURNAL FOR THE RUN. TWO PAIRS OF LINES PER PRODUCT:
      *      CREDIT INTEREST  DR INTEREST EXPENSE / CR ACCRUED PAYABLE
      *      DEBIT INTEREST   DR ACCRUED RECEIVABLE / CR INTEREST INCOME
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-ACCRUAL             TO TRUE
           STRING 'ACCR' WS-BUS-DATE DTC-RUN-ID (1:4)
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE DTC-RUN-ID                 TO GLJH-RUN-ID
           MOVE FUNCTION CURRENT-DATE (1:8) TO GLJH-CREATE-DATE
           MOVE FUNCTION CURRENT-DATE (9:6) TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3190-WRITE-GL-RECORD

           PERFORM VARYING PT-IDX FROM 1 BY 1
                   UNTIL PT-IDX > WS-PROD-COUNT
               IF WS-PT-CR-ACCR-TOTAL (PT-IDX) > ZERO
                   PERFORM 3110-GL-CREDIT-INTEREST
               END-IF
               IF WS-PT-DR-ACCR-TOTAL (PT-IDX) > ZERO
                   PERFORM 3120-GL-DEBIT-INTEREST
               END-IF
           END-PERFORM

           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-GL-LINE-COUNT           TO GLJT-LINE-COUNT
           MOVE WS-GL-TOTAL-DR             TO GLJT-TOTAL-DR
           MOVE WS-GL-TOTAL-CR             TO GLJT-TOTAL-CR
           MOVE 1                          TO GLJT-JOURNAL-COUNT
           PERFORM 3190-WRITE-GL-RECORD

           IF WS-GL-TOTAL-DR NOT = WS-GL-TOTAL-CR
               MOVE '3100-WRITE-GL-JOURNAL' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL ACCRUAL JOURNAL DOES NOT BALANCE'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3110-GL-CREDIT-INTEREST.
      *================================================================*
           MOVE WS-PT-GL-INT-EXPENSE (PT-IDX) TO GLJD-GL-ACCOUNT
           SET GLJD-DEBIT                  TO TRUE
           MOVE WS-PT-CR-ACCR-TOTAL (PT-IDX) TO GLJD-AMOUNT
           MOVE 'CREDIT INTEREST ACCRUED - EXPENSE'
                                           TO GLJD-DESCRIPTION
           PERFORM 3180-GL-DETAIL

           MOVE WS-PT-GL-ACCR-PAY (PT-IDX) TO GLJD-GL-ACCOUNT
           SET GLJD-CREDIT                 TO TRUE
           MOVE WS-PT-CR-ACCR-TOTAL (PT-IDX) TO GLJD-AMOUNT
           MOVE 'CREDIT INTEREST ACCRUED - PAYABLE'
                                           TO GLJD-DESCRIPTION
           PERFORM 3180-GL-DETAIL
           .
      *================================================================*
       3120-GL-DEBIT-INTEREST.
      *================================================================*
           MOVE WS-PT-GL-ACCR-REC (PT-IDX) TO GLJD-GL-ACCOUNT
           SET GLJD-DEBIT                  TO TRUE
           MOVE WS-PT-DR-ACCR-TOTAL (PT-IDX) TO GLJD-AMOUNT
           MOVE 'DEBIT INTEREST ACCRUED - RECEIVABLE'
                                           TO GLJD-DESCRIPTION
           PERFORM 3180-GL-DETAIL

           MOVE WS-PT-GL-INT-INCOME (PT-IDX) TO GLJD-GL-ACCOUNT
           SET GLJD-CREDIT                 TO TRUE
           MOVE WS-PT-DR-ACCR-TOTAL (PT-IDX) TO GLJD-AMOUNT
           MOVE 'DEBIT INTEREST ACCRUED - INCOME'
                                           TO GLJD-DESCRIPTION
           PERFORM 3180-GL-DETAIL
           .
      *================================================================*
       3180-GL-DETAIL.
      *================================================================*
      *    ON ENTRY GL ACCOUNT, DR/CR, AMOUNT AND DESCRIPTION ARE SET.
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-GL-LINE-COUNT
           MOVE WS-GL-LINE-COUNT           TO GLJD-LINE-NO
           MOVE WS-PT-COST-CENTRE (PT-IDX) TO GLJD-COST-CENTRE
           MOVE WS-PT-PROD-CD (PT-IDX)     TO GLJD-PROD-CD
           MOVE WS-PT-CCY-CD (PT-IDX)      TO GLJD-CCY-CD
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE SPACES                     TO GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE 'INT '                     TO GLJD-TXN-CODE
           MOVE GLJ-JOURNAL-ID             TO GLJD-SOURCE-REF
           IF GLJD-DEBIT
               ADD GLJD-AMOUNT             TO WS-GL-TOTAL-DR
           ELSE
               ADD GLJD-AMOUNT             TO WS-GL-TOTAL-CR
           END-IF
           PERFORM 3190-WRITE-GL-RECORD
           .
      *================================================================*
       3190-WRITE-GL-RECORD.
      *================================================================*
           WRITE GLJNL-RECORD              FROM GLJ-JOURNAL-LINE
           IF WS-GLJNL-STATUS NOT = '00'
               MOVE '3190-WRITE-GL-RECORD' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'GLJNL WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-GLJNL-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3200-PRODUCT-SUMMARY.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS

           PERFORM VARYING PT-IDX FROM 1 BY 1
                   UNTIL PT-IDX > WS-PROD-COUNT
               IF WS-PT-ACCT-COUNT (PT-IDX) > ZERO
                   IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
                       PERFORM 7000-REPORT-HEADINGS
                   END-IF
                   MOVE WS-PT-PROD-CD (PT-IDX)   TO SL-PROD-CD
                   MOVE WS-PT-ACCT-COUNT (PT-IDX) TO SL-ACCTS
                   MOVE WS-PT-CR-ACCT-COUNT (PT-IDX)
                                                 TO SL-CR-ACCTS
                   MOVE WS-PT-CR-BAL-TOTAL (PT-IDX) TO SL-CR-BAL
                   MOVE WS-PT-CR-ACCR-TOTAL (PT-IDX) TO SL-CR-ACCR
                   MOVE WS-PT-DR-ACCT-COUNT (PT-IDX)
                                                 TO SL-DR-ACCTS
                   MOVE WS-PT-DR-BAL-TOTAL (PT-IDX) TO SL-DR-BAL
                   MOVE WS-PT-DR-ACCR-TOTAL (PT-IDX) TO SL-DR-ACCR
                   MOVE WS-PT-UNARR-ACCR-TOTAL (PT-IDX)
                                                 TO SL-UNARR-ACCR
                   WRITE REPORT-RECORD     FROM WS-SUMMARY-LINE
                   ADD 1                   TO WS-LINE-COUNT
               END-IF
           END-PERFORM
           .
      *================================================================*
       3300-RUN-TOTALS.
      *================================================================*
           IF WS-LINE-COUNT > WS-LINES-PER-PAGE - 12
               PERFORM 7000-REPORT-HEADINGS
           END-IF

           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE

           MOVE 'ACCOUNTS WITH AN ACCRUAL' TO TL-LABEL
           MOVE WS-ACCRUED-COUNT           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE

           MOVE 'TOTAL CREDIT INTEREST ACCRUED'
                                           TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-CR-ACCRUAL        TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE

           MOVE 'TOTAL DEBIT INTEREST ACCRUED'
                                           TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-DR-ACCRUAL        TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE

           MOVE 'SKIPPED - CLOSED'         TO TL-LABEL
           MOVE WS-SKIP-CLOSED             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE

           MOVE 'SKIPPED - ALREADY ACCRUED (RERUN)'
                                           TO TL-LABEL
           MOVE WS-SKIP-RERUN              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE

           MOVE 'SKIPPED - PRODUCT NOT FOUND'
                                           TO TL-LABEL
           MOVE WS-SKIP-NO-PRODUCT         TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE

           MOVE 'EXCEPTIONS REPORTED'      TO TL-LABEL
           MOVE WS-EXCEPTION-COUNT         TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE

           MOVE 'GL JOURNAL LINES'         TO TL-LABEL
           MOVE WS-GL-LINE-COUNT           TO TL-COUNT
           MOVE WS-GL-TOTAL-DR             TO TL-AMOUNT
           PERFORM 7100-WRITE-TOTAL-LINE
           .
      *================================================================*
       7000-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           WRITE REPORT-RECORD             FROM WS-HEADING-2
           WRITE REPORT-RECORD             FROM WS-HEADING-3
           WRITE REPORT-RECORD             FROM WS-HEADING-4
           MOVE 6                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-WRITE-TOTAL-LINE.
      *================================================================*
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           ADD 2                           TO WS-LINE-COUNT
           .
      *================================================================*
       7500-WRITE-EXCEPTION.
      *================================================================*
      *    ON ENTRY WS-EXC-CODE AND WS-EXC-TEXT ARE SET.
           IF WS-EXC-LINE-COUNT >= WS-LINES-PER-PAGE
               ADD 1                       TO WS-EXC-PAGE-COUNT
               MOVE WS-EXC-PAGE-COUNT      TO EH1-PAGE
               WRITE EXCEPTS-RECORD        FROM WS-EXC-HEADING-1
               WRITE EXCEPTS-RECORD        FROM WS-EXC-HEADING-2
               WRITE EXCEPTS-RECORD        FROM WS-HEADING-4
               MOVE 5                      TO WS-EXC-LINE-COUNT
           END-IF

           MOVE ACM-SORT-CODE              TO ED-SORT-CODE
           MOVE ACM-ACCT-NO                TO ED-ACCT-NO
           MOVE ACM-PROD-CD                TO ED-PROD-CD
           MOVE WS-EXC-CODE                TO ED-CODE
           MOVE ACM-LEDGER-BAL             TO ED-BALANCE
           MOVE WS-EXC-TEXT                TO ED-TEXT
           WRITE EXCEPTS-RECORD            FROM WS-EXC-DETAIL
           ADD 1                           TO WS-EXC-LINE-COUNT
           ADD 1                           TO WS-EXCEPTION-COUNT
           .
      *================================================================*
       8000-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
