       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MIEXT010.
       AUTHOR.        G H TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1995-02-06.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : MIEXT010                                            *
      * TITLE    : MONTHLY MANAGEMENT INFORMATION EXTRACT - DEPOSITS   *
      * TYPE     : BATCH COBOL (NO DB2) - INTERNAL SORT                *
      * JCL      : MIEXT01 (MONTHLY, 1ST BUSINESS DAY, AFTER PSEOD01)  *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ)      *
      *            CIFMAST  - CUSTOMER MASTER (VSAM KSDS, RANDOM) FOR  *
      *                       CUSTOMER TYPE AND SEGMENT                *
      *            PRDPARM  - PRODUCT PARAMETERS (SHORT NAME)          *
      *            PARMIN   - OPTIONAL 'MONTH=CCYYMM' OVERRIDE         *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * WORK     : SORTWK01-03                                         *
      * OUTPUT   : MIFILE   - COMMA SEPARATED MI FILE FOR THE DATA     *
      *                       WAREHOUSE (FB 250, SENT BY MBKCOPY)      *
      *            REPORT   - MI SUMMARY (FBA 133) FOR FINANCE AND     *
      *                       PRODUCT MANAGEMENT                       *
      *----------------------------------------------------------------*
      * REPORTING MONTH IS THE CALENDAR MONTH BEFORE THE BUSINESS DATE *
      * UNLESS PARMIN SUPPLIES 'MONTH=CCYYMM' (RERUNS).                *
      *                                                                *
      * EACH ACCOUNT THAT WAS OPEN AT ANY TIME IN THE MONTH IS COUNTED *
      * IN THREE DIMENSIONS, BY RELEASING ONE SORT RECORD FOR EACH:    *
      *   1  PRODUCT       (ACM-PROD-CD WITHIN ACM-ACCT-TYPE-CD)       *
      *   2  BRANCH        (ACM-SORT-CODE)                             *
      *   3  SEGMENT       (CIF-SEGMENT-CD WITHIN CIF-CUST-TYPE)       *
      * THE OUTPUT PROCEDURE SUMMARISES ON CONTROL BREAK AND WRITES    *
      * ONE 'D' LINE PER GROUP. THE THREE DIMENSIONS MUST GIVE THE     *
      * SAME GRAND TOTALS - IF NOT THE RUN ABENDS U1004.               *
      *                                                                *
      * MEASURES PER GROUP:                                            *
      *   OPEN      OPEN AT MONTH END (NOT CLOSED, OR CLOSED AFTER IT) *
      *   NEW       OPENED IN THE MONTH                                *
      *   CLOSED    CLOSED IN THE MONTH                                *
      *   DORMANT   DORMANT AT RUN TIME                                *
      *   CR BAL    SUM OF CREDIT LEDGER BALANCES                      *
      *   DR BAL    SUM OF DEBIT LEDGER BALANCES (AS A POSITIVE)       *
      *   AVG BAL   NET BALANCE / OPEN ACCOUNTS                        *
      *   OD LIMIT  ARRANGED OVERDRAFT LIMITS                          *
      *   ACCRUED   CREDIT AND DEBIT INTEREST ACCRUED NOT YET APPLIED  *
      *----------------------------------------------------------------*
      * KNOWN PROBLEMS                                                 *
      * PIR 2009/101 OPEN - NON-STERLING ACCOUNTS ARE ADDED IN WITHOUT *
      *   CONVERSION. FINANCE ADJUST IN THE WAREHOUSE. THE COUNT IS    *
      *   SHOWN ON THE REPORT.                                         *
      * PIR 2011/019 OPEN - BALANCES ARE AS AT THE RUN (FIRST BUSINESS *
      *   DAY), NOT THE MONTH END, AND 'AVERAGE BALANCE' IS PER        *
      *   ACCOUNT, NOT TIME-WEIGHTED. THE WAREHOUSE HOLDS DAILY        *
      *   BALANCES FROM TXNHIST FOR THE TRUE AVERAGE.                  *
      * THE SEGMENT DIMENSION READS CIFMAST ONCE PER ACCOUNT (ONLY THE *
      * LAST CIF IS KEPT) - THE RUN TAKES ABOUT 40 MINUTES.            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1995-02-06 GHT  ORIGINAL VERSION (PRINT ONLY, MI0400)          *
      * 1998-12-14 MLW  Y2K                                            *
      * 2003-09-29 SAT  COMMA SEPARATED FILE FOR THE DATA WAREHOUSE    *
      * 2007-03-05 JNB  SEGMENT DIMENSION (CIF-SEGMENT-CD)             *
      * 2011-05-16 AOS  DIMENSION CROSS-CHECK, ACCRUED INTEREST        *
      * 2014-11-03 PDQ  MONTH= OVERRIDE CARD FOR RERUNS                *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
           SELECT SORT-FILE        ASSIGN TO SORTWK01.
           SELECT MIFILE-FILE      ASSIGN TO MIFILE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-MIFILE-STATUS.
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
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD.
           05  PARM-KEYWORD                PIC X(06).
               88  PARM-MONTH                  VALUE 'MONTH='.
           05  PARM-VALUE                  PIC X(06).
           05  FILLER                      PIC X(68).
       SD  SORT-FILE.
       01  SR-RECORD.
           05  SR-DIM                      PIC 9(01).
               88  SR-DIM-PRODUCT              VALUE 1.
               88  SR-DIM-BRANCH               VALUE 2.
               88  SR-DIM-SEGMENT              VALUE 3.
           05  SR-KEY                      PIC X(10).
           05  SR-SUB                      PIC X(04).
           05  SR-OPEN-COUNT               PIC S9(03) COMP-3.
           05  SR-NEW-COUNT                PIC S9(03) COMP-3.
           05  SR-CLOSED-COUNT             PIC S9(03) COMP-3.
           05  SR-DORMANT-COUNT            PIC S9(03) COMP-3.
           05  SR-CR-BAL                   PIC S9(13)V99 COMP-3.
           05  SR-DR-BAL                   PIC S9(13)V99 COMP-3.
           05  SR-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  SR-ACCR-CR                  PIC S9(13)V99 COMP-3.
           05  SR-ACCR-DR                  PIC S9(13)V99 COMP-3.
           05  SR-NEW-BAL                  PIC S9(13)V99 COMP-3.
           05  FILLER                      PIC X(29).
       FD  MIFILE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  MIFILE-RECORD                   PIC X(250).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'MIEXT010'.
      *
       01  WS-CONSTANTS.
           05  WS-FILE-ID                  PIC X(08) VALUE 'MBKMIDEP'.
           05  WS-LAYOUT-VERSION           PIC X(03) VALUE 'V03'.
           05  WS-HOME-CCY                 PIC X(03) VALUE 'GBP'.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-NOTFND           VALUE '23'.
           05  WS-PARMIN-STATUS            PIC X(02).
               88  WS-PARMIN-OK                VALUE '00'.
           05  WS-MIFILE-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-SORT-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-SORT              VALUE 'Y'.
           05  WS-FIRST-GROUP-IND          PIC X(01) VALUE 'Y'.
               88  WS-FIRST-GROUP              VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-MONTH-START              PIC 9(08).
           05  WS-MONTH-END                PIC 9(08).
           05  WS-PERIOD                   PIC X(06).
           05  WS-PERIOD-R REDEFINES WS-PERIOD.
               10  WS-PERIOD-CCYY          PIC 9(04).
               10  WS-PERIOD-MM            PIC 9(02).
           05  WS-LAST-DAY                 PIC 9(02).
           05  WS-LAST-CIF-NO              PIC X(12) VALUE LOW-VALUES.
           05  WS-CUST-TYPE                PIC X(01).
           05  WS-SEGMENT                  PIC X(10).
           05  WS-IN-MONTH-IND             PIC X(01).
               88  WS-IN-MONTH                 VALUE 'Y'.
           05  WS-NET-BAL                  PIC S9(15)V99 COMP-3.
           05  WS-AVG-BAL                  PIC S9(13)V99 COMP-3.
           05  WS-I                        PIC S9(04) COMP.
           05  WS-PTR                      PIC S9(04) COMP.
       01  WS-DAYS-IN-MONTH-VALUES         PIC X(24)
                                VALUE '312831303130313130313031'.
       01  WS-DAYS-IN-MONTH-TABLE REDEFINES WS-DAYS-IN-MONTH-VALUES.
           05  WS-DIM                      PIC 9(02) OCCURS 12 TIMES.
      *
      *---- CONTROL BREAK -------------------------------------------- *
       01  WS-PREV-GROUP.
           05  WS-PREV-DIM                 PIC X(01) VALUE LOW-VALUES.
           05  WS-PREV-KEY                 PIC X(10) VALUE LOW-VALUES.
           05  WS-PREV-SUB                 PIC X(04) VALUE LOW-VALUES.
      *
      *---- ACCUMULATORS: 1 = GROUP, 2-4 = DIMENSION TOTALS ---------- *
       01  WS-ACCUMULATORS.
           05  WS-ACC                      OCCURS 4 TIMES.
               10  WS-ACC-OPEN             PIC S9(09) COMP-3.
               10  WS-ACC-NEW              PIC S9(09) COMP-3.
               10  WS-ACC-CLOSED           PIC S9(09) COMP-3.
               10  WS-ACC-DORMANT          PIC S9(09) COMP-3.
               10  WS-ACC-CR-BAL           PIC S9(15)V99 COMP-3.
               10  WS-ACC-DR-BAL           PIC S9(15)V99 COMP-3.
               10  WS-ACC-OD-LIMIT         PIC S9(15)V99 COMP-3.
               10  WS-ACC-ACCR-CR          PIC S9(15)V99 COMP-3.
               10  WS-ACC-ACCR-DR          PIC S9(15)V99 COMP-3.
               10  WS-ACC-NEW-BAL          PIC S9(15)V99 COMP-3.
               10  WS-ACC-GROUPS           PIC S9(07) COMP-3.
       01  WS-DIM-IDX                      PIC S9(04) COMP.
      *
      *---- DIMENSION NAMES -----------------------------------------  *
       01  WS-DIM-NAME-VALUES.
           05  FILLER    PIC X(28) VALUE 'PRODUCT BY PRODUCT CODE     '.
           05  FILLER    PIC X(28) VALUE 'BRANCH  BY SORT CODE        '.
           05  FILLER    PIC X(28) VALUE 'SEGMENTBY CUSTOMER SEGMENT  '.
       01  WS-DIM-NAMES REDEFINES WS-DIM-NAME-VALUES.
           05  WS-DIM-ENTRY                OCCURS 3 TIMES.
               10  WS-DIM-CODE             PIC X(07).
               10  WS-DIM-TITLE            PIC X(21).
      *
      *---- CSV EDIT FIELDS -----------------------------------------  *
       01  WS-CSV-FIELDS.
           05  WS-CSV-COUNT-1              PIC 9(09).
           05  WS-CSV-COUNT-2              PIC 9(09).
           05  WS-CSV-COUNT-3              PIC 9(09).
           05  WS-CSV-COUNT-4              PIC 9(09).
           05  WS-CSV-AMT-1                PIC -9(13).99.
           05  WS-CSV-AMT-2                PIC -9(13).99.
           05  WS-CSV-AMT-3                PIC -9(13).99.
           05  WS-CSV-AMT-4                PIC -9(13).99.
           05  WS-CSV-AMT-5                PIC -9(13).99.
           05  WS-CSV-AMT-6                PIC -9(13).99.
           05  WS-CSV-AMT-7                PIC -9(13).99.
           05  WS-CSV-KEY                  PIC X(10).
           05  WS-CSV-SUB                  PIC X(04).
       01  WS-MI-LINE                      PIC X(250).
      *
       01  WS-COUNTERS.
           05  WS-ACCTS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-SELECTED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-CLOSED-EARLIER     PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-OPENED-LATER       PIC S9(09) COMP-3 VALUE 0.
           05  WS-NON-STERLING             PIC S9(09) COMP-3 VALUE 0.
           05  WS-CIF-NOT-FOUND            PIC S9(09) COMP-3 VALUE 0.
           05  WS-RELEASED                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETURNED                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-MI-RECORDS               PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'MIEXT010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(40)
               VALUE 'MEGABANK PLC - MONTHLY DEPOSIT MI'.
           05  FILLER                      PIC X(08) VALUE 'MONTH '.
           05  HD1-PERIOD                  PIC X(07).
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-DIM-HEADING.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  FILLER                      PIC X(11) VALUE 'DIMENSION '.
           05  DH-DIM                      PIC X(01).
           05  FILLER                      PIC X(03) VALUE ' - '.
           05  DH-TITLE                    PIC X(21).
           05  FILLER                      PIC X(96) VALUE SPACES.
       01  WS-COLUMN-HEADING.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(11) VALUE 'KEY'.
           05  FILLER            PIC X(05) VALUE 'SUB'.
           05  FILLER            PIC X(19) VALUE 'DESCRIPTION'.
           05  FILLER            PIC X(10) VALUE '     OPEN'.
           05  FILLER            PIC X(08) VALUE '    NEW'.
           05  FILLER            PIC X(08) VALUE ' CLOSED'.
           05  FILLER            PIC X(19) VALUE '    CREDIT BALANCE'.
           05  FILLER            PIC X(19) VALUE '     DEBIT BALANCE'.
           05  FILLER            PIC X(15) VALUE '   AVG BALANCE'.
           05  FILLER            PIC X(18) VALUE '  OD LIMITS'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-KEY                      PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SUB                      PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DESC                     PIC X(18).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-OPEN                     PIC Z,ZZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NEW                      PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CLOSED                   PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CR-BAL                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DR-BAL                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-AVG-BAL                  PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-OD-LIMIT                 PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(07) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY DATECARD.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN SECTION.
      *================================================================*
       0000-START.
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           SORT SORT-FILE
               ON ASCENDING KEY SR-DIM
                                SR-KEY
                                SR-SUB
               INPUT PROCEDURE IS 2000-SELECT-ACCOUNTS
               OUTPUT PROCEDURE IS 3000-PRODUCE-OUTPUT
           IF SORT-RETURN NOT = ZERO
               MOVE '0000-MAIN'            TO ABN-PARAGRAPH
               MOVE 1006                   TO ABN-ABEND-CODE
               MOVE 'SORT FAILED'          TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE SECTION.
      *================================================================*
       1000-START.
           OPEN INPUT  ACCTMAST-FILE
                       CIFMAST-FILE
                       PRDPARM-FILE
                       PARMIN-FILE
                       DATECARD-FILE
                OUTPUT MIFILE-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-PARMIN-OK
           OR WS-MIFILE-STATUS NOT = '00'
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

      *    DEFAULT REPORTING MONTH = MONTH BEFORE THE BUSINESS DATE.
           MOVE WS-BUS-DATE (1:6)          TO WS-PERIOD
           IF WS-PERIOD-MM = 01
               MOVE 12                     TO WS-PERIOD-MM
               SUBTRACT 1                  FROM WS-PERIOD-CCYY
           ELSE
               SUBTRACT 1                  FROM WS-PERIOD-MM
           END-IF
      *    PARMIN MAY BE EMPTY (DD DUMMY) - OVERRIDE ONLY IF PRESENT.
           READ PARMIN-FILE
           PERFORM UNTIL NOT WS-PARMIN-OK
               IF PARM-MONTH
                   IF PARM-VALUE NUMERIC
                   AND PARM-VALUE (5:2) >= '01'
                   AND PARM-VALUE (5:2) <= '12'
                       MOVE PARM-VALUE     TO WS-PERIOD
                       DISPLAY 'MIEXT010 - MONTH OVERRIDE ' WS-PERIOD
                   ELSE
                       MOVE '1000-INITIALISE' TO ABN-PARAGRAPH
                       MOVE 1005           TO ABN-ABEND-CODE
                       MOVE 'INVALID MONTH= CARD - FORMAT CCYYMM'
                                           TO ABN-MESSAGE
                       MOVE PARM-VALUE     TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
               END-IF
               READ PARMIN-FILE
           END-PERFORM
           CLOSE PARMIN-FILE
           IF WS-PERIOD >= WS-BUS-DATE (1:6)
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'REPORTING MONTH NOT YET ENDED'
                                           TO ABN-MESSAGE
               MOVE WS-PERIOD              TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

      *    MONTH START AND END DATES.
           MOVE WS-DIM (WS-PERIOD-MM)      TO WS-LAST-DAY
           IF WS-PERIOD-MM = 2
               IF FUNCTION MOD (WS-PERIOD-CCYY, 4) = 0
               AND (FUNCTION MOD (WS-PERIOD-CCYY, 100) NOT = 0
                    OR FUNCTION MOD (WS-PERIOD-CCYY, 400) = 0)
                   MOVE 29                 TO WS-LAST-DAY
               END-IF
           END-IF
           COMPUTE WS-MONTH-START =
                   WS-PERIOD-CCYY * 10000 + WS-PERIOD-MM * 100 + 1
           COMPUTE WS-MONTH-END =
                   WS-PERIOD-CCYY * 10000 + WS-PERIOD-MM * 100
                 + WS-LAST-DAY
           STRING WS-PERIOD (1:4) '-' WS-PERIOD (5:2)
                  DELIMITED BY SIZE
                  INTO HD1-PERIOD
           END-STRING
           DISPLAY 'MIEXT010 - REPORTING MONTH ' WS-MONTH-START
                   ' TO ' WS-MONTH-END

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               PERFORM 1100-CLEAR-ACC
           END-PERFORM
           PERFORM 1200-WRITE-MI-HEADER
           GO TO 1000-EXIT
           .
      *----------------------------------------------------------------*
       1100-CLEAR-ACC.
      *----------------------------------------------------------------*
           MOVE ZERO                       TO WS-ACC-OPEN (WS-I)
                                              WS-ACC-NEW (WS-I)
                                              WS-ACC-CLOSED (WS-I)
                                              WS-ACC-DORMANT (WS-I)
                                              WS-ACC-CR-BAL (WS-I)
                                              WS-ACC-DR-BAL (WS-I)
                                              WS-ACC-OD-LIMIT (WS-I)
                                              WS-ACC-ACCR-CR (WS-I)
                                              WS-ACC-ACCR-DR (WS-I)
                                              WS-ACC-NEW-BAL (WS-I)
                                              WS-ACC-GROUPS (WS-I)
           .
      *----------------------------------------------------------------*
       1200-WRITE-MI-HEADER.
      *----------------------------------------------------------------*
      *    'H' RECORD, THEN A 'C' RECORD OF COLUMN NAMES FOR THE
      *    WAREHOUSE LOADER (MAPPING MI_DEP_MONTHLY).
           MOVE SPACES                     TO WS-MI-LINE
           STRING 'H,' WS-FILE-ID ',' WS-PERIOD ',' WS-BUS-DATE ','
                  DTC-RUN-ID ',' WS-LAYOUT-VERSION
                  DELIMITED BY SIZE
                  INTO WS-MI-LINE
           END-STRING
           PERFORM 3900-WRITE-MI
           MOVE SPACES                     TO WS-MI-LINE
           STRING 'C,DIMENSION,KEY,SUBKEY,PERIOD,OPEN_CNT,NEW_CNT,'
                  'CLOSED_CNT,DORMANT_CNT,CR_BAL,DR_BAL,NET_BAL,'
                  'AVG_BAL,OD_LIMIT,ACCR_CR,ACCR_DR,NEW_BAL'
                  DELIMITED BY SIZE
                  INTO WS-MI-LINE
           END-STRING
           PERFORM 3900-WRITE-MI
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-SELECT-ACCOUNTS SECTION.
      *================================================================*
       2000-START.
           PERFORM 2900-READ-ACCOUNT
           PERFORM UNTIL WS-END-OF-ACCOUNTS
               PERFORM 2100-SELECT-ONE THRU 2100-EXIT
               PERFORM 2900-READ-ACCOUNT
           END-PERFORM
           GO TO 2000-EXIT
           .
      *----------------------------------------------------------------*
       2100-SELECT-ONE.
      *----------------------------------------------------------------*
           ADD 1                           TO WS-ACCTS-READ
      *    LOAN AND MORTGAGE TYPES ON ACCTMAST ARE SHADOW ACCOUNTS -
      *    LOAN MI COMES FROM THE LENDING EXTRACT.
           IF ACM-LOAN OR ACM-MORTGAGE
               GO TO 2100-EXIT
           END-IF
           IF ACM-OPEN-DATE NOT NUMERIC
           OR ACM-OPEN-DATE > WS-MONTH-END
               ADD 1                       TO WS-ACCTS-OPENED-LATER
               GO TO 2100-EXIT
           END-IF
           IF ACM-CLOSED
           AND ACM-CLOSE-DATE NUMERIC
           AND ACM-CLOSE-DATE < WS-MONTH-START
               ADD 1                       TO WS-ACCTS-CLOSED-EARLIER
               GO TO 2100-EXIT
           END-IF
           ADD 1                           TO WS-ACCTS-SELECTED
           IF ACM-CCY-CD NOT = WS-HOME-CCY
               ADD 1                       TO WS-NON-STERLING
           END-IF

           MOVE SPACES                     TO SR-RECORD
           MOVE ZERO                       TO SR-OPEN-COUNT
                                              SR-NEW-COUNT
                                              SR-CLOSED-COUNT
                                              SR-DORMANT-COUNT
                                              SR-CR-BAL SR-DR-BAL
                                              SR-OD-LIMIT
                                              SR-ACCR-CR SR-ACCR-DR
                                              SR-NEW-BAL
           IF ACM-CLOSED
           AND ACM-CLOSE-DATE NOT > WS-MONTH-END
               MOVE 1                      TO SR-CLOSED-COUNT
           ELSE
               MOVE 1                      TO SR-OPEN-COUNT
           END-IF
           IF ACM-OPEN-DATE NOT < WS-MONTH-START
               MOVE 1                      TO SR-NEW-COUNT
               MOVE ACM-LEDGER-BAL         TO SR-NEW-BAL
           END-IF
           IF ACM-DORMANT
               MOVE 1                      TO SR-DORMANT-COUNT
           END-IF
           IF ACM-LEDGER-BAL < ZERO
               COMPUTE SR-DR-BAL = ZERO - ACM-LEDGER-BAL
           ELSE
               MOVE ACM-LEDGER-BAL         TO SR-CR-BAL
           END-IF
           IF ACM-OD-LIMIT NUMERIC
               MOVE ACM-OD-LIMIT           TO SR-OD-LIMIT
           END-IF
           IF ACM-ACCRUED-INT-CR NUMERIC
               MOVE ACM-ACCRUED-INT-CR     TO SR-ACCR-CR
           END-IF
           IF ACM-ACCRUED-INT-DR NUMERIC
               MOVE ACM-ACCRUED-INT-DR     TO SR-ACCR-DR
           END-IF

      *    DIMENSION 1 - PRODUCT WITHIN ACCOUNT TYPE
           MOVE 1                          TO SR-DIM
           MOVE ACM-PROD-CD                TO SR-KEY
           MOVE ACM-ACCT-TYPE-CD           TO SR-SUB
           PERFORM 2800-RELEASE
      *    DIMENSION 2 - BRANCH (SORT CODE)
           MOVE 2                          TO SR-DIM
           MOVE ACM-SORT-CODE              TO SR-KEY
           MOVE SPACES                     TO SR-SUB
           PERFORM 2800-RELEASE
      *    DIMENSION 3 - CUSTOMER SEGMENT WITHIN CUSTOMER TYPE
           PERFORM 2200-GET-SEGMENT THRU 2200-EXIT
           MOVE 3                          TO SR-DIM
           MOVE WS-SEGMENT                 TO SR-KEY
           MOVE WS-CUST-TYPE               TO SR-SUB
           PERFORM 2800-RELEASE
           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-GET-SEGMENT.
      *----------------------------------------------------------------*
           IF ACM-CIF-NO = WS-LAST-CIF-NO
               GO TO 2200-EXIT
           END-IF
           MOVE ACM-CIF-NO                 TO WS-LAST-CIF-NO
                                              CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   MOVE CIF-CUST-TYPE      TO WS-CUST-TYPE
                   IF CIF-SEGMENT-CD = SPACES
      *                PRE-2007 CUSTOMERS NEVER HAD A SEGMENT SET.
                       MOVE 'UNSEGMENTD'   TO WS-SEGMENT
                   ELSE
                       MOVE CIF-SEGMENT-CD TO WS-SEGMENT
                   END-IF
               WHEN WS-CIFMAST-NOTFND
                   ADD 1                   TO WS-CIF-NOT-FOUND
                   MOVE '?'                TO WS-CUST-TYPE
                   MOVE 'NO-CIF'           TO WS-SEGMENT
               WHEN OTHER
                   MOVE '2200-GET-SEGMENT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE ACM-CIF-NO         TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2800-RELEASE.
      *----------------------------------------------------------------*
           RELEASE SR-RECORD
           ADD 1                           TO WS-RELEASED
           .
      *----------------------------------------------------------------*
       2900-READ-ACCOUNT.
      *----------------------------------------------------------------*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   CONTINUE
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '2900-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-PRODUCE-OUTPUT SECTION.
      *================================================================*
       3000-START.
           PERFORM 3800-RETURN
           IF WS-END-OF-SORT
               PERFORM 7000-PAGE-HEADING
               MOVE SPACES                 TO WS-DETAIL-LINE
               MOVE 'NO ACCOUNTS'          TO DL-DESC
               WRITE REPORT-RECORD         FROM WS-DETAIL-LINE
               GO TO 3000-EXIT
           END-IF
           PERFORM UNTIL WS-END-OF-SORT
               IF SR-DIM NOT = WS-PREV-DIM
                   IF NOT WS-FIRST-GROUP
                       PERFORM 3200-END-GROUP
                       PERFORM 3300-END-DIMENSION
                   END-IF
                   PERFORM 3100-NEW-DIMENSION
               ELSE
                   IF SR-KEY NOT = WS-PREV-KEY
                   OR SR-SUB NOT = WS-PREV-SUB
                       PERFORM 3200-END-GROUP
                   END-IF
               END-IF
               MOVE 'N'                    TO WS-FIRST-GROUP-IND
               MOVE SR-DIM                 TO WS-PREV-DIM
               MOVE SR-KEY                 TO WS-PREV-KEY
               MOVE SR-SUB                 TO WS-PREV-SUB
               PERFORM 3400-ACCUMULATE
               PERFORM 3800-RETURN
           END-PERFORM
           PERFORM 3200-END-GROUP
           PERFORM 3300-END-DIMENSION
           PERFORM 3500-CROSS-CHECK
           PERFORM 3600-WRITE-MI-TRAILER
           GO TO 3000-EXIT
           .
      *----------------------------------------------------------------*
       3100-NEW-DIMENSION.
      *----------------------------------------------------------------*
           MOVE SR-DIM                     TO DH-DIM
           MOVE SR-DIM                     TO WS-DIM-IDX
           MOVE WS-DIM-TITLE (WS-DIM-IDX)  TO DH-TITLE
           PERFORM 7000-PAGE-HEADING
           WRITE REPORT-RECORD             FROM WS-DIM-HEADING
           WRITE REPORT-RECORD             FROM WS-COLUMN-HEADING
           ADD 4                           TO WS-LINE-COUNT
           MOVE 1                          TO WS-I
           PERFORM 1100-CLEAR-ACC
           .
      *----------------------------------------------------------------*
       3200-END-GROUP.
      *----------------------------------------------------------------*
      *    GROUP LINE TO THE MI FILE AND THE REPORT, THEN ROLL THE
      *    GROUP INTO THE DIMENSION TOTAL (2 + DIMENSION - 1).
           COMPUTE WS-NET-BAL = WS-ACC-CR-BAL (1) - WS-ACC-DR-BAL (1)
           IF WS-ACC-OPEN (1) > ZERO
               COMPUTE WS-AVG-BAL ROUNDED =
                       WS-NET-BAL / WS-ACC-OPEN (1)
           ELSE
               MOVE ZERO                   TO WS-AVG-BAL
           END-IF
           MOVE WS-PREV-KEY                TO WS-CSV-KEY
           MOVE WS-PREV-SUB                TO WS-CSV-SUB
           IF WS-CSV-KEY = SPACES
               MOVE 'BLANK'                TO WS-CSV-KEY
           END-IF
           IF WS-CSV-SUB = SPACES
               MOVE '-'                    TO WS-CSV-SUB
           END-IF
           MOVE WS-ACC-OPEN (1)            TO WS-CSV-COUNT-1
           MOVE WS-ACC-NEW (1)             TO WS-CSV-COUNT-2
           MOVE WS-ACC-CLOSED (1)          TO WS-CSV-COUNT-3
           MOVE WS-ACC-DORMANT (1)         TO WS-CSV-COUNT-4
           MOVE WS-ACC-CR-BAL (1)          TO WS-CSV-AMT-1
           MOVE WS-ACC-DR-BAL (1)          TO WS-CSV-AMT-2
           MOVE WS-NET-BAL                 TO WS-CSV-AMT-3
           MOVE WS-AVG-BAL                 TO WS-CSV-AMT-4
           MOVE WS-ACC-OD-LIMIT (1)        TO WS-CSV-AMT-5
           MOVE WS-ACC-ACCR-CR (1)         TO WS-CSV-AMT-6
           MOVE WS-ACC-ACCR-DR (1)         TO WS-CSV-AMT-7
           MOVE SPACES                     TO WS-MI-LINE
           MOVE 1                          TO WS-PTR
           STRING 'D,'                     DELIMITED BY SIZE
                  WS-DIM-CODE (WS-DIM-IDX) DELIMITED BY SPACE
                  ','                      DELIMITED BY SIZE
                  WS-CSV-KEY               DELIMITED BY SPACE
                  ','                      DELIMITED BY SIZE
                  WS-CSV-SUB               DELIMITED BY SPACE
                  ','                      DELIMITED BY SIZE
                  WS-PERIOD                DELIMITED BY SIZE
                  ','                      DELIMITED BY SIZE
                  WS-CSV-COUNT-1 ',' WS-CSV-COUNT-2 ','
                  WS-CSV-COUNT-3 ',' WS-CSV-COUNT-4 ','
                                           DELIMITED BY SIZE
                  INTO WS-MI-LINE
                  WITH POINTER WS-PTR
           END-STRING
           STRING WS-CSV-AMT-1 ',' WS-CSV-AMT-2 ',' WS-CSV-AMT-3 ','
                  WS-CSV-AMT-4 ',' WS-CSV-AMT-5 ',' WS-CSV-AMT-6 ','
                  WS-CSV-AMT-7 ','
                                           DELIMITED BY SIZE
                  INTO WS-MI-LINE
                  WITH POINTER WS-PTR
           END-STRING
           MOVE WS-ACC-NEW-BAL (1)         TO WS-CSV-AMT-1
           STRING WS-CSV-AMT-1             DELIMITED BY SIZE
                  INTO WS-MI-LINE
                  WITH POINTER WS-PTR
           END-STRING
           PERFORM 3900-WRITE-MI

           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-PAGE-HEADING
               WRITE REPORT-RECORD         FROM WS-COLUMN-HEADING
               ADD 2                       TO WS-LINE-COUNT
           END-IF
           MOVE WS-PREV-KEY                TO DL-KEY
           MOVE WS-PREV-SUB                TO DL-SUB
           PERFORM 3250-GROUP-DESCRIPTION
           MOVE WS-ACC-OPEN (1)            TO DL-OPEN
           MOVE WS-ACC-NEW (1)             TO DL-NEW
           MOVE WS-ACC-CLOSED (1)          TO DL-CLOSED
           MOVE WS-ACC-CR-BAL (1)          TO DL-CR-BAL
           MOVE WS-ACC-DR-BAL (1)          TO DL-DR-BAL
           MOVE WS-AVG-BAL                 TO DL-AVG-BAL
           MOVE WS-ACC-OD-LIMIT (1)        TO DL-OD-LIMIT
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT

           COMPUTE WS-I = WS-DIM-IDX + 1
           ADD WS-ACC-OPEN (1)             TO WS-ACC-OPEN (WS-I)
           ADD WS-ACC-NEW (1)              TO WS-ACC-NEW (WS-I)
           ADD WS-ACC-CLOSED (1)           TO WS-ACC-CLOSED (WS-I)
           ADD WS-ACC-DORMANT (1)          TO WS-ACC-DORMANT (WS-I)
           ADD WS-ACC-CR-BAL (1)           TO WS-ACC-CR-BAL (WS-I)
           ADD WS-ACC-DR-BAL (1)           TO WS-ACC-DR-BAL (WS-I)
           ADD WS-ACC-OD-LIMIT (1)         TO WS-ACC-OD-LIMIT (WS-I)
           ADD WS-ACC-ACCR-CR (1)          TO WS-ACC-ACCR-CR (WS-I)
           ADD WS-ACC-ACCR-DR (1)          TO WS-ACC-ACCR-DR (WS-I)
           ADD WS-ACC-NEW-BAL (1)          TO WS-ACC-NEW-BAL (WS-I)
           ADD 1                           TO WS-ACC-GROUPS (WS-I)
           MOVE 1                          TO WS-I
           PERFORM 1100-CLEAR-ACC
           .
      *----------------------------------------------------------------*
       3250-GROUP-DESCRIPTION.
      *----------------------------------------------------------------*
           MOVE SPACES                     TO DL-DESC
           EVALUATE WS-PREV-DIM
               WHEN '1'
                   MOVE WS-PREV-KEY        TO PRD-PROD-CD
                   READ PRDPARM-FILE
                   IF WS-PRDPARM-OK
                       MOVE PRD-SHORT-NAME TO DL-DESC
                   ELSE
                       MOVE '** NOT ON PRDPARM' TO DL-DESC
                   END-IF
               WHEN '2'
                   MOVE 'BRANCH'           TO DL-DESC
               WHEN '3'
                   EVALUATE WS-PREV-SUB (1:1)
                       WHEN 'P'
                           MOVE 'PERSONAL' TO DL-DESC
                       WHEN 'B'
                           MOVE 'BUSINESS' TO DL-DESC
                       WHEN OTHER
                           MOVE 'CUSTOMER NOT ON CIF' TO DL-DESC
                   END-EVALUATE
           END-EVALUATE
           .
      *----------------------------------------------------------------*
       3300-END-DIMENSION.
      *----------------------------------------------------------------*
           COMPUTE WS-I = WS-DIM-IDX + 1
           IF WS-LINE-COUNT + 6 >= WS-LINES-PER-PAGE
               PERFORM 7000-PAGE-HEADING
           END-IF
           MOVE SPACES                     TO TL-LABEL
           STRING 'TOTAL ' WS-DIM-CODE (WS-DIM-IDX) ' - GROUPS'
                  DELIMITED BY SIZE
                  INTO TL-LABEL
           END-STRING
           MOVE WS-ACC-GROUPS (WS-I)       TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OPEN ACCOUNTS / NET BALANCE' TO TL-LABEL
           MOVE WS-ACC-OPEN (WS-I)         TO TL-COUNT
           COMPUTE TL-AMOUNT =
                   WS-ACC-CR-BAL (WS-I) - WS-ACC-DR-BAL (WS-I)
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  NEW ACCOUNTS / BALANCE' TO TL-LABEL
           MOVE WS-ACC-NEW (WS-I)          TO TL-COUNT
           MOVE WS-ACC-NEW-BAL (WS-I)      TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  CLOSED ACCOUNTS'        TO TL-LABEL
           MOVE WS-ACC-CLOSED (WS-I)       TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           ADD 8                           TO WS-LINE-COUNT
           .
      *----------------------------------------------------------------*
       3400-ACCUMULATE.
      *----------------------------------------------------------------*
           ADD 1                           TO WS-RETURNED
           ADD SR-OPEN-COUNT               TO WS-ACC-OPEN (1)
           ADD SR-NEW-COUNT                TO WS-ACC-NEW (1)
           ADD SR-CLOSED-COUNT             TO WS-ACC-CLOSED (1)
           ADD SR-DORMANT-COUNT            TO WS-ACC-DORMANT (1)
           ADD SR-CR-BAL                   TO WS-ACC-CR-BAL (1)
           ADD SR-DR-BAL                   TO WS-ACC-DR-BAL (1)
           ADD SR-OD-LIMIT                 TO WS-ACC-OD-LIMIT (1)
           ADD SR-ACCR-CR                  TO WS-ACC-ACCR-CR (1)
           ADD SR-ACCR-DR                  TO WS-ACC-ACCR-DR (1)
           ADD SR-NEW-BAL                  TO WS-ACC-NEW-BAL (1)
           .
      *----------------------------------------------------------------*
       3500-CROSS-CHECK.
      *----------------------------------------------------------------*
      *    EVERY ACCOUNT WAS RELEASED ONCE PER DIMENSION, SO THE THREE
      *    DIMENSION TOTALS MUST AGREE WITH EACH OTHER AND WITH THE
      *    SORT RECORD COUNTS.
           IF WS-RELEASED NOT = WS-RETURNED
           OR WS-ACC-OPEN (2) NOT = WS-ACC-OPEN (3)
           OR WS-ACC-OPEN (2) NOT = WS-ACC-OPEN (4)
           OR WS-ACC-CR-BAL (2) NOT = WS-ACC-CR-BAL (3)
           OR WS-ACC-CR-BAL (2) NOT = WS-ACC-CR-BAL (4)
           OR WS-ACC-DR-BAL (2) NOT = WS-ACC-DR-BAL (3)
           OR WS-ACC-DR-BAL (2) NOT = WS-ACC-DR-BAL (4)
               DISPLAY 'MIEXT010 - RELEASED ' WS-RELEASED
                       ' RETURNED ' WS-RETURNED
               DISPLAY 'MIEXT010 - OPEN BY DIM ' WS-ACC-OPEN (2)
                       ' ' WS-ACC-OPEN (3) ' ' WS-ACC-OPEN (4)
               MOVE '3500-CROSS-CHECK'     TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'DIMENSION TOTALS DO NOT AGREE'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           .
      *----------------------------------------------------------------*
       3600-WRITE-MI-TRAILER.
      *----------------------------------------------------------------*
      *    TRAILER COUNT INCLUDES THE H AND C RECORDS AND ITSELF.
           ADD 1                           TO WS-MI-RECORDS
           MOVE WS-MI-RECORDS              TO WS-CSV-COUNT-1
           MOVE WS-ACC-OPEN (2)            TO WS-CSV-COUNT-2
           COMPUTE WS-NET-BAL =
                   WS-ACC-CR-BAL (2) - WS-ACC-DR-BAL (2)
           MOVE WS-NET-BAL                 TO WS-CSV-AMT-1
           MOVE SPACES                     TO WS-MI-LINE
           STRING 'T,' WS-FILE-ID ',' WS-PERIOD ',' WS-CSV-COUNT-1
                  ',' WS-CSV-COUNT-2 ',' WS-CSV-AMT-1
                  DELIMITED BY SIZE
                  INTO WS-MI-LINE
           END-STRING
           WRITE MIFILE-RECORD             FROM WS-MI-LINE
           IF WS-MIFILE-STATUS NOT = '00'
               MOVE '3600-WRITE-MI-TRAILER' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'MIFILE WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-MIFILE-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *----------------------------------------------------------------*
       3800-RETURN.
      *----------------------------------------------------------------*
           RETURN SORT-FILE
               AT END SET WS-END-OF-SORT   TO TRUE
           END-RETURN
           .
      *----------------------------------------------------------------*
       3900-WRITE-MI.
      *----------------------------------------------------------------*
           WRITE MIFILE-RECORD             FROM WS-MI-LINE
           IF WS-MIFILE-STATUS NOT = '00'
               MOVE '3900-WRITE-MI'        TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'MIFILE WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-MIFILE-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-MI-RECORDS
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       7000-PAGE-HEADING SECTION.
      *================================================================*
       7000-START.
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           MOVE 2                          TO WS-LINE-COUNT
           .
      *================================================================*
       9000-FINALISE SECTION.
      *================================================================*
       9000-START.
           PERFORM 7000-PAGE-HEADING
           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-ACCTS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS IN MONTH (SELECTED)' TO TL-LABEL
           MOVE WS-ACCTS-SELECTED          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCLUDED - CLOSED BEFORE MONTH' TO TL-LABEL
           MOVE WS-ACCTS-CLOSED-EARLIER    TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCLUDED - OPENED AFTER MONTH END' TO TL-LABEL
           MOVE WS-ACCTS-OPENED-LATER      TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NON-STERLING (NOT CONVERTED)' TO TL-LABEL
           MOVE WS-NON-STERLING            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMER NOT ON CIF'      TO TL-LABEL
           MOVE WS-CIF-NOT-FOUND           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SORT RECORDS RELEASED'    TO TL-LABEL
           MOVE WS-RELEASED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MI FILE RECORDS (INCL H/C/T)' TO TL-LABEL
           MOVE WS-MI-RECORDS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TOTAL CREDIT BALANCES'    TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-ACC-CR-BAL (2)          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TOTAL DEBIT BALANCES'     TO TL-LABEL
           MOVE WS-ACC-DR-BAL (2)          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TOTAL ARRANGED OD LIMITS' TO TL-LABEL
           MOVE WS-ACC-OD-LIMIT (2)        TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCRUED CREDIT INTEREST'  TO TL-LABEL
           MOVE WS-ACC-ACCR-CR (2)         TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCRUED DEBIT INTEREST'   TO TL-LABEL
           MOVE WS-ACC-ACCR-DR (2)         TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE ACCTMAST-FILE CIFMAST-FILE PRDPARM-FILE MIFILE-FILE
                 REPORT-FILE
           DISPLAY 'MIEXT010 - ACCOUNTS SELECTED ' WS-ACCTS-SELECTED
           DISPLAY 'MIEXT010 - MI RECORDS        ' WS-MI-RECORDS
           IF WS-CIF-NOT-FOUND > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
       9000-EXIT.
           EXIT.
      *
       Z999-ABEND-SECTION SECTION.
           COPY ABNDPROC.
