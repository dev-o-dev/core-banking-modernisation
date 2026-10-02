       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SARXT010.
       AUTHOR.        L J OKAFOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2018-04-16.
       DATE-COMPILED.
      *================================================================
      * PROGRAM  : SARXT010                                            *
      * TITLE    : GDPR - DATA SUBJECT ACCESS REQUEST (DSAR) EXTRACT   *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : SARXT010                                            *
      * JCL      : SARXT01 (DAILY, AFTER PSEOD01 - ONLY IF REQUESTS)   *
      *----------------------------------------------------------------
      * INPUT    : SARREQ   - REQUESTS LOGGED BY THE DATA PROTECTION   *
      *                       OFFICE (ISPF PANEL MBKSAR1, FB 80)       *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      *            CIFMAST  - CIF MASTER (RANDOM)                      *
      *            ACCTMAST - DDA MASTER BY CIF (ALTERNATE KEY, DD     *
      *                       ACCTMAS1 = CIF PATH)                     *
      *            LOANMAST - LOAN MASTER BY CIF (DD LOANMAS1 = PATH)  *
      *            SOMAST   - STANDING ORDERS (BROWSE BY ACCOUNT)      *
      *            DDMAND   - DIRECT DEBIT MANDATES (BROWSE BY ACCOUNT)*
      *            MBK.TXNHIST - TRANSACTIONS, LAST SIX YEARS (CURSOR) *
      * OUTPUT   : SAROUT   - ONE PRINTED PACK PER CUSTOMER (FBA 133)  *
      *                       FOR THE DPO TO CHECK AND SEND            *
      *            REPORT   - CONTROL REPORT WITH DEADLINES (FBA 133)  *
      *----------------------------------------------------------------
      * GDPR ARTICLE 15: A CUSTOMER MAY ASK FOR A COPY OF ALL THE      *
      * PERSONAL DATA WE HOLD. DEADLINE (ARTICLE 12(3)) ONE MONTH FROM *
      * RECEIPT - THE SAME DAY NEXT MONTH, OR ITS LAST DAY IF THERE IS *
      * NO SUCH DAY - PLUS TWO MONTHS IF EXTENDED (SRQ-EXTENDED-IND).  *
      *                                                                *
      * EACH PACK HAS A CONTENTS PAGE, THEN ONE SECTION PER SOURCE,    *
      * EACH STARTING ON A NEW PAGE:                                   *
      *   1 CUSTOMER DETAILS      CIF RECORD                           *
      *   2 ACCOUNTS              ACCTMAST (OPEN AND CLOSED)           *
      *   3 LOANS                 LOANMAST                             *
      *   4 STANDING ORDERS       SOMAST, EVERY ACCOUNT IN SECTION 2   *
      *   5 DIRECT DEBITS         DDMAND, EVERY ACCOUNT IN SECTION 2   *
      *   6 TRANSACTIONS          TXNHIST FROM BUSINESS DATE - 6 YEARS *
      * PASS 1 COUNTS THE ITEMS IN EACH SECTION FOR THE CONTENTS PAGE  *
      * (SAME BROWSES IN COUNT MODE, COUNT(*) FOR TRANSACTIONS).       *
      *                                                                *
      * CONTROL REPORT: ONE LINE PER REQUEST WITH THE STATUTORY        *
      * DEADLINE AND DAYS REMAINING. RC 4 IF A REQUEST IS REJECTED     *
      * (UNKNOWN CIF) OR ALREADY PAST ITS DEADLINE.                    *
      *                                                                *
      * KNOWN ISSUES:                                                  *
      * PIR 2018/077 OPEN: JOINT ACCOUNTS PRINT FOR THE PRIMARY CIF    *
      *   ONLY (ONE CIF PER ACCOUNT) - THE DPO FINDS THE SECOND HOLDER *
      *   AND REDACTS THIRD-PARTY DATA BY HAND.                        *
      * PIR 2018/091 OPEN: MORE THAN 30 ACCOUNTS OR 10 LOANS ARE NOT   *
      *   PRINTED (REPORTED AS 'TRUNCATED' - RE-RUN BY ACCOUNT RANGE). *
      *----------------------------------------------------------------
      * HISTORY                                                        *
      * 2018-04-16 LJO  ORIGINAL VERSION (GDPR GO-LIVE 25 MAY 2018)    *
      * 2018-06-11 LJO  EXTENDED DEADLINE FLAG, OVERDUE RC 4 (DPO      *
      *                 AUDIT - 3 LATE RESPONSES IN JUNE)              *
      * 2018-09-03 PDQ  STANDING ORDERS AND MANDATES SECTIONS ADDED    *
      *================================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SARREQ-FILE      ASSIGN TO SARREQ
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-SARREQ-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS ACM-KEY
                                   ALTERNATE RECORD KEY IS ACM-CIF-NO
                                       WITH DUPLICATES
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT LOANMAST-FILE    ASSIGN TO LOANMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS LN-KEY
                                   ALTERNATE RECORD KEY IS LN-CIF-NO
                                       WITH DUPLICATES
                                   FILE STATUS IS WS-LOANMAST-STATUS.
           SELECT SOMAST-FILE      ASSIGN TO SOMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS SOM-KEY
                                   FILE STATUS IS WS-SOMAST-STATUS.
           SELECT DDMAND-FILE      ASSIGN TO DDMAND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS DMM-KEY
                                   FILE STATUS IS WS-DDMAND-STATUS.
           SELECT SAROUT-FILE      ASSIGN TO SAROUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-SAROUT-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SARREQ-FILE
           RECORDING MODE IS F.
       01  SARREQ-RECORD.
           05  SRQ-REQUEST-REF             PIC X(10).
           05  SRQ-CIF-NO                  PIC X(12).
           05  SRQ-RECEIVED-DATE           PIC 9(08).
           05  SRQ-EXTENDED-IND            PIC X(01).
               88  SRQ-EXTENDED                VALUE 'Y'.
           05  SRQ-CHANNEL                 PIC X(04).
           05  SRQ-REQUESTED-BY            PIC X(30).
           05  FILLER                      PIC X(15).
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  LOANMAST-FILE.
           COPY LNACCREC.
       FD  SOMAST-FILE.
           COPY SOMSTREC.
       FD  DDMAND-FILE.
           COPY DMMNDREC.
       FD  SAROUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SAROUT-RECORD                   PIC X(133).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'SARXT010'.
      *
       01  WS-CONSTANTS.
           05  WS-HISTORY-YEARS            PIC 9(02) VALUE 06.
           05  WS-BODY-LINES               PIC S9(04) COMP VALUE +50.
           05  WS-MAX-ACCOUNTS             PIC S9(04) COMP VALUE +30.
           05  WS-MAX-LOANS                PIC S9(04) COMP VALUE +10.
      *
       01  WS-FILE-STATUSES.
           05  WS-SARREQ-STATUS            PIC X(02).
               88  WS-SARREQ-OK                VALUE '00'.
               88  WS-SARREQ-EOF               VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00' '02'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00' '02'.
               88  WS-LOANMAST-EOF             VALUE '10'.
           05  WS-SOMAST-STATUS            PIC X(02).
               88  WS-SOMAST-OK                VALUE '00'.
           05  WS-DDMAND-STATUS            PIC X(02).
               88  WS-DDMAND-OK                VALUE '00'.
           05  WS-SAROUT-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-REQUESTS          VALUE 'Y'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-MODE-IND                 PIC X(01).
               88  WS-COUNT-MODE               VALUE 'C'.
               88  WS-PRINT-MODE               VALUE 'P'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-FROM-DATE                PIC 9(08).
           05  WS-DEADLINE                 PIC 9(08).
           05  WS-DAYS-LEFT                PIC S9(05) COMP.
           05  WS-STATUS                   PIC X(12).
           05  WS-NOTE                     PIC X(40).
           05  WS-SO-COUNT                 PIC S9(05) COMP.
           05  WS-DD-COUNT                 PIC S9(05) COMP.
           05  WS-TXN-TOTAL                PIC S9(09) COMP.
           05  WS-TXN-PRINTED              PIC S9(09) COMP.
           05  WS-TRUNCATED-IND            PIC X(01).
               88  WS-TRUNCATED                VALUE 'Y'.
           05  WS-I                        PIC S9(04) COMP.
           05  WS-S                        PIC S9(04) COMP.
           05  WS-SECT-DISP                PIC 9(01).
      *---- ADD-MONTHS / FORMAT-DATE WORK -----------------------------*
           05  WS-AM-DATE                  PIC 9(08).
           05  WS-AM-DATE-R REDEFINES WS-AM-DATE.
               10  WS-AM-YYYY              PIC 9(04).
               10  WS-AM-MM                PIC 9(02).
               10  WS-AM-DD                PIC 9(02).
           05  WS-AM-MONTHS                PIC S9(04) COMP.
           05  WS-AM-NEXT                  PIC 9(08).
           05  WS-AM-NEXT-R REDEFINES WS-AM-NEXT.
               10  WS-AM-NEXT-YYYY         PIC 9(04).
               10  WS-AM-NEXT-MM           PIC 9(02).
               10  WS-AM-NEXT-DD           PIC 9(02).
           05  WS-AM-DIM                   PIC S9(04) COMP.
           05  WS-FD-IN                    PIC 9(08).
           05  WS-FD-OUT                   PIC X(10).
           05  WS-ED-AMT                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  WS-ED-RATE                  PIC -ZZ9.9999.
           05  WS-ED-TXN                   PIC ZZZ,ZZZ,ZZ9.99.
      *
      *---- SECTIONS OF THE PACK --------------------------------------*
       01  WS-SECTION-VALUES.
           05  FILLER  PIC X(30) VALUE 'CUSTOMER DETAILS'.
           05  FILLER  PIC X(30) VALUE 'ACCOUNTS'.
           05  FILLER  PIC X(30) VALUE 'LOANS'.
           05  FILLER  PIC X(30) VALUE 'STANDING ORDERS'.
           05  FILLER  PIC X(30) VALUE 'DIRECT DEBIT INSTRUCTIONS'.
           05  FILLER  PIC X(30) VALUE 'TRANSACTIONS (LAST 6 YEARS)'.
       01  WS-SECTION-NAMES REDEFINES WS-SECTION-VALUES.
           05  WS-SECT-NAME                PIC X(30) OCCURS 6 TIMES.
       01  WS-PAGE-PLAN.
           05  WS-SECT-ITEMS               PIC S9(07) COMP
                                           OCCURS 6 TIMES.
      *
      *---- ACCOUNTS AND LOANS OF THE CURRENT CUSTOMER ----------------*
       01  WS-ACCT-TABLE.
           05  WS-ACCT-COUNT               PIC S9(04) COMP.
           05  WS-ACCT-ENTRY               OCCURS 30 TIMES.
               10  WS-AT-RECORD            PIC X(400).
               10  WS-AT-TXN-COUNT         PIC S9(09) COMP.
       01  WS-LOAN-TABLE.
           05  WS-LOAN-COUNT               PIC S9(04) COMP.
           05  WS-LOAN-ENTRY               OCCURS 10 TIMES.
               10  WS-LT-RECORD            PIC X(500).
      *
      *---- PACK PRINT CONTROL ----------------------------------------*
       01  WS-PACK-CONTROL.
           05  WS-PACK-PAGE                PIC S9(04) COMP.
           05  WS-PG-LINES                 PIC S9(04) COMP.
           05  WS-CUR-SECTION              PIC X(30).
       01  WS-PACK-HEAD-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'SARXT010'.
           05  FILLER                      PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(90) VALUE
               'MEGABANK PLC - SUBJECT ACCESS REQUEST - PERSONAL DATA'.
           05  FILLER                      PIC X(05) VALUE 'PAGE '.
           05  PH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(13) VALUE SPACES.
       01  WS-PACK-HEAD-2.
           05  FILLER                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(10) VALUE 'REQUEST'.
           05  PH2-REF                     PIC X(12).
           05  FILLER                      PIC X(10) VALUE 'CUSTOMER'.
           05  PH2-CIF                     PIC X(14).
           05  FILLER                      PIC X(10) VALUE 'PREPARED'.
           05  PH2-DATE                    PIC X(12).
           05  FILLER                      PIC X(10) VALUE 'RESPOND BY'.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  PH2-DEADLINE                PIC X(10).
           05  FILLER                      PIC X(43) VALUE SPACES.
       01  WS-PACK-HEAD-3.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  PH3-SECTION                 PIC X(60).
           05  FILLER                      PIC X(72) VALUE SPACES.
       01  WS-PRINT-LINE.
           05  PL-CC                       PIC X(01).
           05  FILLER                      PIC X(04).
           05  PL-LABEL                    PIC X(30).
           05  PL-VALUE                    PIC X(98).
       01  WS-CONTENTS-LINE.
           05  CN-CC                       PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  CN-SECT                     PIC 9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  CN-NAME                     PIC X(30).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  CN-ITEMS                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(06) VALUE ' ITEMS'.
           05  FILLER                      PIC X(79) VALUE SPACES.
      *
      *---- CONTROL REPORT --------------------------------------------*
       01  WS-COUNTERS.
           05  WS-REQUESTS                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-PRODUCED                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-REJECTED                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-OVERDUE                  PIC S9(07) COMP-3 VALUE 0.
           05  WS-TRUNCATED-PACKS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-PAGES-PRINTED            PIC S9(09) COMP-3 VALUE 0.
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'SARXT010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - SUBJECT ACCESS REQUESTS CONTROL'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(12) VALUE 'REQUEST'.
           05  FILLER            PIC X(14) VALUE 'CIF'.
           05  FILLER            PIC X(12) VALUE 'RECEIVED'.
           05  FILLER            PIC X(12) VALUE 'DEADLINE'.
           05  FILLER            PIC X(07) VALUE ' DAYS'.
           05  FILLER            PIC X(14) VALUE 'STATUS'.
           05  FILLER            PIC X(06) VALUE 'PAGES'.
           05  FILLER            PIC X(05) VALUE 'ACC'.
           05  FILLER            PIC X(09) VALUE '    TXNS'.
           05  FILLER            PIC X(41) VALUE 'NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-REF                      PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CIF                      PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RECEIVED                 PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DEADLINE                 PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DAYS                     PIC -ZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-STATUS                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PAGES                    PIC ZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACCOUNTS                 PIC ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TXNS                     PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
           05  FILLER                      PIC X(01) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
           COPY DATECARD.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
       01  HV-FROM-DATE                    PIC X(10).
       01  HV-TXN-COUNT                    PIC S9(09) COMP.
      *
           EXEC SQL DECLARE TXNCSR CURSOR FOR
               SELECT POST_DATE, POST_SEQ, TXN_CODE, DR_CR_IND,
                      TXN_AMT, RUNNING_BAL, CHANNEL_CD, NARRATIVE_1
                 FROM MBK.TXNHIST
                WHERE SORT_CODE = :TXH-SORT-CODE
                  AND ACCT_NO   = :TXH-ACCT-NO
                  AND POST_DATE >= :HV-FROM-DATE
                ORDER BY POST_DATE, POST_SEQ
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-REQUEST THRU 2000-EXIT
               UNTIL WS-END-OF-REQUESTS
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  SARREQ-FILE
                       DATECARD-FILE
                       CIFMAST-FILE
                       ACCTMAST-FILE
                       LOANMAST-FILE
                       SOMAST-FILE
                       DDMAND-FILE
                OUTPUT SAROUT-FILE
                       REPORT-FILE
           IF NOT WS-SARREQ-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-LOANMAST-OK
           OR NOT WS-SOMAST-OK
           OR NOT WS-DDMAND-OK
           OR WS-SAROUT-STATUS NOT = '00'
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
           MOVE WS-BUS-DATE                TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           MOVE WS-FD-OUT                  TO HD1-BUS-DATE PH2-DATE
      *    HISTORY FROM THE SAME DAY SIX YEARS AGO (29/2 -> 28/2)
           MOVE WS-BUS-DATE                TO WS-AM-DATE
           SUBTRACT WS-HISTORY-YEARS       FROM WS-AM-YYYY
           IF WS-AM-MM = 02 AND WS-AM-DD = 29
               MOVE 28                     TO WS-AM-DD
           END-IF
           MOVE WS-AM-DATE                 TO WS-FROM-DATE
           STRING WS-AM-YYYY '-' WS-AM-MM '-' WS-AM-DD
                  DELIMITED BY SIZE
                  INTO HV-FROM-DATE
           END-STRING
           PERFORM 7000-REPORT-HEADINGS
           PERFORM 8000-READ-REQUEST
           .
      *================================================================*
       2000-PROCESS-REQUEST.
      *================================================================*
           MOVE SPACES                     TO WS-NOTE WS-STATUS
           MOVE 'N'                        TO WS-TRUNCATED-IND
           INITIALIZE WS-PAGE-PLAN WS-ACCT-TABLE WS-LOAN-TABLE
           MOVE ZERO                       TO WS-TXN-TOTAL WS-DEADLINE
                                              WS-DAYS-LEFT WS-PACK-PAGE
           IF SRQ-RECEIVED-DATE NOT NUMERIC
           OR SRQ-RECEIVED-DATE > WS-BUS-DATE
               MOVE 'REJECTED'             TO WS-STATUS
               MOVE 'RECEIVED DATE INVALID - CORRECT ON MBKSAR1'
                                           TO WS-NOTE
               ADD 1                       TO WS-REJECTED
               PERFORM 7100-REPORT-DETAIL
               PERFORM 8000-READ-REQUEST
               GO TO 2000-EXIT
           END-IF
           PERFORM 2050-DEADLINE

           MOVE SRQ-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   CONTINUE
               WHEN WS-CIFMAST-NOTFND
                   MOVE 'REJECTED'         TO WS-STATUS
                   MOVE 'CIF NOT FOUND - CHECK IDENTITY / OLD CIF'
                                           TO WS-NOTE
                   ADD 1                   TO WS-REJECTED
                   PERFORM 7100-REPORT-DETAIL
                   PERFORM 8000-READ-REQUEST
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE '2000-PROCESS-REQUEST' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE SRQ-CIF-NO         TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           IF CIF-MERGED-INTO NOT = SPACES
           AND CIF-MERGED-INTO NOT = LOW-VALUES
               MOVE 'MERGED - ALSO RUN FOR SURVIVING CIF'
                                           TO WS-NOTE
           END-IF

      *---- PASS 1 - COUNT AND PLAN -----------------------------------*
           PERFORM 2100-LOAD-ACCOUNTS THRU 2100-EXIT
           PERFORM 2200-LOAD-LOANS THRU 2200-EXIT
           SET WS-COUNT-MODE               TO TRUE
           MOVE ZERO                       TO WS-SO-COUNT WS-DD-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
               MOVE WS-AT-RECORD (WS-I)    TO ACM-ACCOUNT-RECORD
               PERFORM 4400-BROWSE-SO THRU 4400-EXIT
               PERFORM 4500-BROWSE-DD THRU 4500-EXIT
               PERFORM 2300-COUNT-TXNS
           END-PERFORM
           PERFORM 2400-PLAN-CONTENTS

      *---- PASS 2 - PRINT --------------------------------------------*
           SET WS-PRINT-MODE               TO TRUE
           MOVE ZERO                       TO WS-PACK-PAGE
           PERFORM 3000-CONTENTS
           PERFORM 4100-CUSTOMER-SECTION
           PERFORM 4200-ACCOUNT-SECTION
           PERFORM 4300-LOAN-SECTION
           MOVE 4                          TO WS-S
           PERFORM 6000-SECTION-START
           IF WS-SO-COUNT = ZERO
               PERFORM 6200-NONE-HELD
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
               MOVE WS-AT-RECORD (WS-I)    TO ACM-ACCOUNT-RECORD
               PERFORM 4400-BROWSE-SO THRU 4400-EXIT
           END-PERFORM
           MOVE 5                          TO WS-S
           PERFORM 6000-SECTION-START
           IF WS-DD-COUNT = ZERO
               PERFORM 6200-NONE-HELD
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
               MOVE WS-AT-RECORD (WS-I)    TO ACM-ACCOUNT-RECORD
               PERFORM 4500-BROWSE-DD THRU 4500-EXIT
           END-PERFORM
           PERFORM 4600-TXN-SECTION

           ADD 1                           TO WS-PRODUCED
           ADD WS-PACK-PAGE                TO WS-PAGES-PRINTED
           IF WS-TRUNCATED
               ADD 1                       TO WS-TRUNCATED-PACKS
               MOVE 'TRUNCATED - OVER 30 ACCOUNTS / 10 LOANS'
                                           TO WS-NOTE
           END-IF
           IF WS-DAYS-LEFT < ZERO
               MOVE 'OVERDUE'              TO WS-STATUS
               ADD 1                       TO WS-OVERDUE
           ELSE
               MOVE 'PRODUCED'             TO WS-STATUS
           END-IF
           PERFORM 7100-REPORT-DETAIL
           PERFORM 8000-READ-REQUEST
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2050-DEADLINE.
      *================================================================*
      *    ARTICLE 12(3): ONE MONTH, PLUS TWO MORE IF EXTENDED.
           MOVE SRQ-RECEIVED-DATE          TO WS-AM-DATE
           IF SRQ-EXTENDED
               MOVE 3                      TO WS-AM-MONTHS
           ELSE
               MOVE 1                      TO WS-AM-MONTHS
           END-IF
           PERFORM 9500-ADD-MONTHS
           MOVE WS-AM-DATE                 TO WS-DEADLINE
           COMPUTE WS-DAYS-LEFT =
                   FUNCTION INTEGER-OF-DATE (WS-DEADLINE)
                   - WS-BUS-DATE-INT
           MOVE WS-DEADLINE                TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           MOVE WS-FD-OUT                  TO PH2-DEADLINE
           .
      *================================================================*
       2100-LOAD-ACCOUNTS.
      *================================================================*
           MOVE CIF-NO                     TO ACM-CIF-NO
           START ACCTMAST-FILE KEY = ACM-CIF-NO
           IF NOT WS-ACCTMAST-OK
               GO TO 2100-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ ACCTMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN WS-ACCTMAST-EOF
                       SET WS-END-BROWSE   TO TRUE
                   WHEN NOT WS-ACCTMAST-OK
                       MOVE '2100-LOAD-ACCOUNTS' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTMAST BROWSE FAILED' TO ABN-MESSAGE
                       MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                       MOVE CIF-NO         TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   WHEN ACM-CIF-NO NOT = CIF-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN WS-ACCT-COUNT >= WS-MAX-ACCOUNTS
                       SET WS-TRUNCATED    TO TRUE
                       SET WS-END-BROWSE   TO TRUE
                   WHEN OTHER
                       ADD 1               TO WS-ACCT-COUNT
                       MOVE ACM-ACCOUNT-RECORD
                                 TO WS-AT-RECORD (WS-ACCT-COUNT)
               END-EVALUATE
           END-PERFORM
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-LOAD-LOANS.
      *================================================================*
           MOVE CIF-NO                     TO LN-CIF-NO
           START LOANMAST-FILE KEY = LN-CIF-NO
           IF NOT WS-LOANMAST-OK
               GO TO 2200-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ LOANMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN WS-LOANMAST-EOF
                       SET WS-END-BROWSE   TO TRUE
                   WHEN NOT WS-LOANMAST-OK
                       MOVE '2200-LOAD-LOANS' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'LOANMAST BROWSE FAILED' TO ABN-MESSAGE
                       MOVE WS-LOANMAST-STATUS TO ABN-FILE-STATUS
                       MOVE CIF-NO         TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   WHEN LN-CIF-NO NOT = CIF-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN WS-LOAN-COUNT >= WS-MAX-LOANS
                       SET WS-TRUNCATED    TO TRUE
                       SET WS-END-BROWSE   TO TRUE
                   WHEN OTHER
                       ADD 1               TO WS-LOAN-COUNT
                       MOVE LN-LOAN-RECORD
                                 TO WS-LT-RECORD (WS-LOAN-COUNT)
               END-EVALUATE
           END-PERFORM
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-COUNT-TXNS.
      *================================================================*
           MOVE ACM-SORT-CODE              TO TXH-SORT-CODE
           MOVE ACM-ACCT-NO                TO TXH-ACCT-NO
           EXEC SQL
               SELECT COUNT(*)
                 INTO :HV-TXN-COUNT
                 FROM MBK.TXNHIST
                WHERE SORT_CODE = :TXH-SORT-CODE
                  AND ACCT_NO   = :TXH-ACCT-NO
                  AND POST_DATE >= :HV-FROM-DATE
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2300-COUNT-TXNS'      TO ABN-PARAGRAPH
               MOVE TXH-ACCT-NO            TO ABN-KEY-DATA
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE HV-TXN-COUNT               TO WS-AT-TXN-COUNT (WS-I)
           ADD HV-TXN-COUNT                TO WS-TXN-TOTAL
           .
      *================================================================*
       2400-PLAN-CONTENTS.
      *================================================================*
           MOVE 1                          TO WS-SECT-ITEMS (1)
           MOVE WS-ACCT-COUNT              TO WS-SECT-ITEMS (2)
           MOVE WS-LOAN-COUNT              TO WS-SECT-ITEMS (3)
           MOVE WS-SO-COUNT                TO WS-SECT-ITEMS (4)
           MOVE WS-DD-COUNT                TO WS-SECT-ITEMS (5)
           MOVE WS-TXN-TOTAL               TO WS-SECT-ITEMS (6)
           .
      *================================================================*
       3000-CONTENTS.
      *================================================================*
           MOVE 'CONTENTS'                 TO WS-CUR-SECTION
           MOVE ZERO                       TO WS-PG-LINES
           PERFORM 6050-NEW-PAGE
           MOVE SPACES                     TO WS-PRINT-LINE
           MOVE 'PREPARED FOR'             TO PL-LABEL
           STRING CIF-TITLE DELIMITED BY SPACE ' ' DELIMITED BY SIZE
                  CIF-FIRST-NM DELIMITED BY SPACE ' ' DELIMITED BY SIZE
                  CIF-LAST-NM DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE 'REQUEST RECEIVED'         TO PL-LABEL
           MOVE SRQ-RECEIVED-DATE          TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           MOVE WS-FD-OUT                  TO PL-VALUE
           PERFORM 6100-PRINT-LINE
           MOVE 'TRANSACTIONS FROM'        TO PL-LABEL
           MOVE WS-FROM-DATE               TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           MOVE WS-FD-OUT                  TO PL-VALUE
           PERFORM 6100-PRINT-LINE
           MOVE SPACES                     TO WS-PRINT-LINE
           PERFORM 6100-PRINT-LINE
           PERFORM VARYING WS-S FROM 1 BY 1 UNTIL WS-S > 6
               MOVE WS-S                   TO CN-SECT
               MOVE WS-SECT-NAME (WS-S)    TO CN-NAME
               MOVE WS-SECT-ITEMS (WS-S)   TO CN-ITEMS
               WRITE SAROUT-RECORD         FROM WS-CONTENTS-LINE
               ADD 1                       TO WS-PG-LINES
           END-PERFORM
           MOVE '0'                        TO PL-CC
           MOVE 'CARD, CALL AND ONLINE BANKING DATA ARE SENT SEPARATELY'
                                           TO PL-VALUE
           PERFORM 6100-PRINT-LINE
           .
      *================================================================*
       4100-CUSTOMER-SECTION.
      *================================================================*
           MOVE 1                          TO WS-S
           PERFORM 6000-SECTION-START
           MOVE 'CUSTOMER NUMBER / NAME'   TO PL-LABEL
           STRING CIF-NO ' ' CIF-TITLE ' '
                  CIF-FIRST-NM DELIMITED BY '  '
                  ' ' CIF-MIDDLE-NM DELIMITED BY '  '
                  ' ' CIF-LAST-NM DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE 'DATE OF BIRTH / NI NUMBER' TO PL-LABEL
           MOVE CIF-DOB                    TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           STRING WS-FD-OUT ' / ' CIF-NAT-INS-NO
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE 'ADDRESS'                  TO PL-LABEL
           MOVE CIF-ADDR-LINE1             TO PL-VALUE
           PERFORM 6100-PRINT-LINE
           MOVE CIF-ADDR-LINE2             TO PL-VALUE
           PERFORM 6100-PRINT-LINE
           STRING CIF-TOWN DELIMITED BY '  ' ' ' CIF-COUNTY
                  DELIMITED BY '  ' ' ' CIF-POST-CD ' ' CIF-CTRY-CD
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE 'TELEPHONE / EMAIL'        TO PL-LABEL
           STRING CIF-HOME-TEL ' ' CIF-MOBILE-TEL ' ' CIF-EMAIL-ADDR
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE 'EMPLOYMENT / INCOME'      TO PL-LABEL
           MOVE CIF-ANN-INCOME             TO WS-ED-AMT
           STRING CIF-EMPL-STATUS ' ' CIF-OCCUPATION ' ' WS-ED-AMT
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE 'MARKETING EMAIL/POST, SEGMENT' TO PL-LABEL
           STRING CIF-MKTG-EMAIL-IND ' / ' CIF-MKTG-POST-IND
                  '   ' CIF-SEGMENT-CD
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE 'ID CHECK / RISK / PEP'    TO PL-LABEL
           STRING 'KYC ' CIF-KYC-STATUS '  RISK ' CIF-RISK-RATING
                  '  PEP ' CIF-PEP-IND
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE 'CUSTOMER SINCE / BRANCH'  TO PL-LABEL
           MOVE CIF-CREATE-DATE            TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           STRING WS-FD-OUT ' / ' CIF-HOME-BRANCH-SC
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           .
      *================================================================*
       4200-ACCOUNT-SECTION.
      *================================================================*
           MOVE 2                          TO WS-S
           PERFORM 6000-SECTION-START
           IF WS-ACCT-COUNT = ZERO
               PERFORM 6200-NONE-HELD
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
               MOVE WS-AT-RECORD (WS-I)    TO ACM-ACCOUNT-RECORD
               MOVE 'ACCOUNT / IBAN'       TO PL-LABEL
               STRING ACM-SORT-CODE ' ' ACM-ACCT-NO ' ' ACM-ACCT-TYPE-CD
                      ' ' ACM-PROD-CD ' ' ACM-CCY-CD '  ' ACM-IBAN
                      DELIMITED BY SIZE
                      INTO PL-VALUE
               END-STRING
               PERFORM 6100-PRINT-LINE
               MOVE 'STATUS/OPENED/CLOSED/BAL' TO PL-LABEL
               MOVE ACM-ACCT-STATUS        TO PL-VALUE
               MOVE ACM-OPEN-DATE          TO WS-FD-IN
               PERFORM 9600-FORMAT-DATE
               MOVE WS-FD-OUT              TO PL-VALUE (3:10)
               MOVE ACM-CLOSE-DATE         TO WS-FD-IN
               PERFORM 9600-FORMAT-DATE
               MOVE WS-FD-OUT              TO PL-VALUE (15:10)
               MOVE ACM-LEDGER-BAL         TO WS-ED-AMT
               MOVE WS-ED-AMT              TO PL-VALUE (27:15)
               PERFORM 6100-PRINT-LINE
               MOVE 'OVERDRAFT / CR / DR RATE' TO PL-LABEL
               MOVE ACM-OD-LIMIT           TO WS-ED-AMT
               MOVE WS-ED-AMT              TO PL-VALUE
               MOVE ACM-INT-RATE-CR        TO WS-ED-RATE
               MOVE WS-ED-RATE             TO PL-VALUE (20:9)
               MOVE ACM-INT-RATE-DR        TO WS-ED-RATE
               MOVE WS-ED-RATE             TO PL-VALUE (32:9)
               PERFORM 6100-PRINT-LINE
               PERFORM 6100-PRINT-LINE
           END-PERFORM
           .
      *================================================================*
       4300-LOAN-SECTION.
      *================================================================*
           MOVE 3                          TO WS-S
           PERFORM 6000-SECTION-START
           IF WS-LOAN-COUNT = ZERO
               PERFORM 6200-NONE-HELD
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LOAN-COUNT
               MOVE WS-LT-RECORD (WS-I)    TO LN-LOAN-RECORD
               MOVE 'LOAN / AGREEMENT'     TO PL-LABEL
               STRING LN-LOAN-NO ' / ' LN-AGREEMENT-REF '  STATUS '
                      LN-STATUS '  REPAID FROM ' LN-REPAY-SORT-CODE
                      ' ' LN-REPAY-ACCT-NO '  AGENCY '
                      LN-COLLECTIONS-AGENCY
                      DELIMITED BY SIZE
                      INTO PL-VALUE
               END-STRING
               PERFORM 6100-PRINT-LINE
               MOVE 'ADVANCED / BALANCE / ARR' TO PL-LABEL
               MOVE LN-ADVANCE-AMT         TO WS-ED-AMT
               MOVE WS-ED-AMT              TO PL-VALUE
               MOVE LN-PRINCIPAL-BAL       TO WS-ED-AMT
               MOVE WS-ED-AMT              TO PL-VALUE (17:15)
               MOVE LN-ARREARS-AMT         TO WS-ED-AMT
               MOVE WS-ED-AMT              TO PL-VALUE (33:15)
               PERFORM 6100-PRINT-LINE
               MOVE 'PAYMENT / APR / ADVANCED' TO PL-LABEL
               MOVE LN-MONTHLY-PAYMENT     TO WS-ED-AMT
               MOVE WS-ED-AMT              TO PL-VALUE
               MOVE LN-APR                 TO WS-ED-RATE
               MOVE WS-ED-RATE             TO PL-VALUE (17:9)
               MOVE LN-ADVANCE-DATE        TO WS-FD-IN
               PERFORM 9600-FORMAT-DATE
               MOVE WS-FD-OUT              TO PL-VALUE (28:10)
               PERFORM 6100-PRINT-LINE
               PERFORM 6100-PRINT-LINE
           END-PERFORM
           .
      *================================================================*
       4400-BROWSE-SO.
      *================================================================*
      *    COUNT MODE ADDS TO WS-SO-COUNT; PRINT MODE PRINTS.
           MOVE ACM-SORT-CODE              TO SOM-PAYER-SORT-CODE
           MOVE ACM-ACCT-NO                TO SOM-PAYER-ACCT-NO
           MOVE ZERO                       TO SOM-SO-SEQ
           START SOMAST-FILE KEY NOT < SOM-KEY
           IF NOT WS-SOMAST-OK
               GO TO 4400-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ SOMAST-FILE NEXT RECORD
               IF NOT WS-SOMAST-OK
               OR SOM-PAYER-SORT-CODE NOT = ACM-SORT-CODE
               OR SOM-PAYER-ACCT-NO NOT = ACM-ACCT-NO
                   SET WS-END-BROWSE       TO TRUE
               ELSE
                   IF NOT SOM-CONTROL-REC
                       IF WS-COUNT-MODE
                           ADD 1           TO WS-SO-COUNT
                       ELSE
                           PERFORM 4410-PRINT-SO
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       4400-EXIT.
           EXIT.
      *================================================================*
       4410-PRINT-SO.
      *================================================================*
           MOVE 'STANDING ORDER'           TO PL-LABEL
           STRING SOM-PAYER-SORT-CODE ' ' SOM-PAYER-ACCT-NO ' NO '
                  SOM-SO-SEQ '  STATUS ' SOM-STATUS
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE '  PAID TO'                TO PL-LABEL
           STRING SOM-BENE-NAME ' ' SOM-BENE-SORT-CODE ' '
                  SOM-BENE-ACCT-NO '  REF ' SOM-BENE-REF
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE '  AMOUNT / FREQ / NEXT DUE' TO PL-LABEL
           MOVE SOM-REGULAR-AMT            TO WS-ED-AMT
           MOVE SOM-NEXT-DUE-DATE          TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           STRING WS-ED-AMT ' / ' SOM-FREQ-CD ' / ' WS-FD-OUT
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           .
      *================================================================*
       4500-BROWSE-DD.
      *================================================================*
      *    COUNT MODE ADDS TO WS-DD-COUNT; PRINT MODE PRINTS.
           MOVE LOW-VALUES                 TO DMM-KEY
           MOVE ACM-SORT-CODE              TO DMM-PAYER-SORT-CODE
           MOVE ACM-ACCT-NO                TO DMM-PAYER-ACCT-NO
           START DDMAND-FILE KEY NOT < DMM-KEY
           IF NOT WS-DDMAND-OK
               GO TO 4500-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ DDMAND-FILE NEXT RECORD
               IF NOT WS-DDMAND-OK
               OR DMM-PAYER-SORT-CODE NOT = ACM-SORT-CODE
               OR DMM-PAYER-ACCT-NO NOT = ACM-ACCT-NO
                   SET WS-END-BROWSE       TO TRUE
               ELSE
                   IF WS-COUNT-MODE
                       ADD 1               TO WS-DD-COUNT
                   ELSE
                       PERFORM 4510-PRINT-DD
                   END-IF
               END-IF
           END-PERFORM
           .
       4500-EXIT.
           EXIT.
      *================================================================*
       4510-PRINT-DD.
      *================================================================*
           MOVE 'DIRECT DEBIT TO'          TO PL-LABEL
           STRING DMM-ORIG-NAME ' SUN ' DMM-ORIG-SUN '  REF '
                  DMM-PAYER-REF '  STATUS ' DMM-STATUS
                  '  FROM ' DMM-PAYER-SORT-CODE ' ' DMM-PAYER-ACCT-NO
                  DELIMITED BY SIZE
                  INTO PL-VALUE
           END-STRING
           PERFORM 6100-PRINT-LINE
           MOVE '  SET UP / LAST PAID'     TO PL-LABEL
           MOVE DMM-SETUP-DATE             TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           MOVE WS-FD-OUT                  TO PL-VALUE
           MOVE DMM-LAST-COLL-DATE         TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           MOVE DMM-LAST-COLL-AMT          TO WS-ED-AMT
           MOVE WS-FD-OUT                  TO PL-VALUE (14:10)
           MOVE WS-ED-AMT                  TO PL-VALUE (25:15)
           PERFORM 6100-PRINT-LINE
           .
      *================================================================*
       4600-TXN-SECTION.
      *================================================================*
           MOVE 6                          TO WS-S
           PERFORM 6000-SECTION-START
           IF WS-ACCT-COUNT = ZERO
               PERFORM 6200-NONE-HELD
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
               MOVE WS-AT-RECORD (WS-I)    TO ACM-ACCOUNT-RECORD
               MOVE SPACES                 TO WS-PRINT-LINE
               PERFORM 6100-PRINT-LINE
               MOVE 'ACCOUNT'              TO PL-LABEL
               STRING ACM-SORT-CODE ' ' ACM-ACCT-NO '  ' ACM-PROD-CD
                      DELIMITED BY SIZE
                      INTO PL-VALUE
               END-STRING
               PERFORM 6100-PRINT-LINE
               MOVE 'DATE / CODE / DR-CR / AMOUNT / BALANCE / CHANNEL'
                                           TO PL-VALUE
               PERFORM 6100-PRINT-LINE
               IF WS-AT-TXN-COUNT (WS-I) = ZERO
                   MOVE '  NO TRANSACTIONS IN THE PERIOD' TO PL-VALUE
                   PERFORM 6100-PRINT-LINE
               ELSE
                   PERFORM 4610-FETCH-TXNS
               END-IF
           END-PERFORM
           .
      *================================================================*
       4610-FETCH-TXNS.
      *================================================================*
           MOVE ACM-SORT-CODE              TO TXH-SORT-CODE
           MOVE ACM-ACCT-NO                TO TXH-ACCT-NO
           MOVE ZERO                       TO WS-TXN-PRINTED
           EXEC SQL OPEN TXNCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '4610-FETCH-TXNS'      TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
      *    NO MORE ROWS THAN COUNTED IN PASS 1 (MATCHES THE CONTENTS)
           PERFORM UNTIL SQLCODE NOT = 0
                      OR WS-TXN-PRINTED >= WS-AT-TXN-COUNT (WS-I)
               EXEC SQL
                   FETCH TXNCSR
                    INTO :TXH-POST-DATE, :TXH-POST-SEQ,
                         :TXH-TXN-CODE, :TXH-DR-CR-IND,
                         :TXH-TXN-AMT, :TXH-RUNNING-BAL,
                         :TXH-CHANNEL-CD, :TXH-NARRATIVE-1
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 4620-PRINT-TXN
                   WHEN +100
                       CONTINUE
                   WHEN OTHER
                       MOVE '4610-FETCH-TXNS' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE TXNCSR END-EXEC
           .
      *================================================================*
       4620-PRINT-TXN.
      *================================================================*
      *    TXNHIST DATES ARE ISO (CCYY-MM-DD) - PRINT AS DD/MM/CCYY
           STRING TXH-POST-DATE (9:2) '/' TXH-POST-DATE (6:2) '/'
                  TXH-POST-DATE (1:4) '  ' TXH-TXN-CODE
                  DELIMITED BY SIZE
                  INTO PL-LABEL
           END-STRING
           IF TXH-DR-CR-IND = 'D'
               MOVE 'DR'                   TO PL-LABEL (19:2)
           ELSE
               MOVE 'CR'                   TO PL-LABEL (19:2)
           END-IF
           MOVE TXH-TXN-AMT                TO WS-ED-TXN
           MOVE WS-ED-TXN                  TO PL-VALUE (1:14)
           MOVE TXH-RUNNING-BAL            TO WS-ED-AMT
           MOVE WS-ED-AMT                  TO PL-VALUE (16:15)
           MOVE TXH-CHANNEL-CD             TO PL-VALUE (33:6)
           IF TXH-NARRATIVE-1-LEN > ZERO
           AND TXH-NARRATIVE-1-LEN <= 35
               MOVE TXH-NARRATIVE-1-TEXT (1:TXH-NARRATIVE-1-LEN)
                                           TO PL-VALUE (41:35)
           END-IF
           PERFORM 6100-PRINT-LINE
           ADD 1                           TO WS-TXN-PRINTED
           .
      *================================================================*
       6000-SECTION-START.
      *================================================================*
           MOVE SPACES                     TO WS-CUR-SECTION
           MOVE WS-S                       TO WS-SECT-DISP
           STRING WS-SECT-DISP ' ' WS-SECT-NAME (WS-S)
                  DELIMITED BY SIZE
                  INTO WS-CUR-SECTION
           END-STRING
      *    A SECTION ALWAYS STARTS A NEW PAGE (NOT A CONTINUATION)
           MOVE ZERO                       TO WS-PG-LINES
           PERFORM 6050-NEW-PAGE
           .
      *================================================================*
       6050-NEW-PAGE.
      *================================================================*
           ADD 1                           TO WS-PACK-PAGE
           MOVE WS-PACK-PAGE               TO PH1-PAGE
           MOVE SRQ-REQUEST-REF            TO PH2-REF
           MOVE CIF-NO                     TO PH2-CIF
           MOVE WS-CUR-SECTION             TO PH3-SECTION
           IF WS-PG-LINES > ZERO AND WS-CUR-SECTION NOT = 'CONTENTS'
               STRING WS-CUR-SECTION DELIMITED BY '  '
                      ' (CONTINUED)' DELIMITED BY SIZE
                      INTO PH3-SECTION
               END-STRING
           END-IF
           WRITE SAROUT-RECORD             FROM WS-PACK-HEAD-1
           PERFORM 6900-CHECK-SAROUT
           WRITE SAROUT-RECORD             FROM WS-PACK-HEAD-2
           WRITE SAROUT-RECORD             FROM WS-PACK-HEAD-3
           MOVE ZERO                       TO WS-PG-LINES
           .
      *================================================================*
       6100-PRINT-LINE.
      *================================================================*
           IF WS-PG-LINES >= WS-BODY-LINES
               PERFORM 6050-NEW-PAGE
           END-IF
           WRITE SAROUT-RECORD             FROM WS-PRINT-LINE
           PERFORM 6900-CHECK-SAROUT
           ADD 1                           TO WS-PG-LINES
           MOVE SPACES                     TO WS-PRINT-LINE
           .
      *================================================================*
       6200-NONE-HELD.
      *================================================================*
           MOVE '  NONE HELD'              TO PL-VALUE
           PERFORM 6100-PRINT-LINE
           .
      *================================================================*
       6900-CHECK-SAROUT.
      *================================================================*
           IF WS-SAROUT-STATUS NOT = '00'
               MOVE '6900-CHECK-SAROUT'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SAROUT WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-SAROUT-STATUS       TO ABN-FILE-STATUS
               MOVE SRQ-REQUEST-REF        TO ABN-KEY-DATA
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
           MOVE 3                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SRQ-REQUEST-REF            TO DL-REF
           MOVE SRQ-CIF-NO                 TO DL-CIF
           MOVE SRQ-RECEIVED-DATE          TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           MOVE WS-FD-OUT                  TO DL-RECEIVED
           MOVE WS-DEADLINE                TO WS-FD-IN
           PERFORM 9600-FORMAT-DATE
           MOVE WS-FD-OUT                  TO DL-DEADLINE
           MOVE WS-DAYS-LEFT               TO DL-DAYS
           MOVE WS-STATUS                  TO DL-STATUS
           MOVE WS-PACK-PAGE               TO DL-PAGES
           MOVE WS-ACCT-COUNT              TO DL-ACCOUNTS
           MOVE WS-TXN-TOTAL               TO DL-TXNS
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-REQUEST.
      *================================================================*
           READ SARREQ-FILE
           EVALUATE TRUE
               WHEN WS-SARREQ-OK
                   ADD 1                   TO WS-REQUESTS
               WHEN WS-SARREQ-EOF
                   SET WS-END-OF-REQUESTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-REQUEST' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SARREQ READ FAILED' TO ABN-MESSAGE
                   MOVE WS-SARREQ-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC
           MOVE 'REQUESTS READ'            TO TL-LABEL
           MOVE WS-REQUESTS                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PACKS PRODUCED'           TO TL-LABEL
           MOVE WS-PRODUCED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH ALREADY OVERDUE' TO TL-LABEL
           MOVE WS-OVERDUE                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  TRUNCATED (PIR 2018/091)' TO TL-LABEL
           MOVE WS-TRUNCATED-PACKS         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REQUESTS REJECTED'        TO TL-LABEL
           MOVE WS-REJECTED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE SARREQ-FILE CIFMAST-FILE ACCTMAST-FILE LOANMAST-FILE
                 SOMAST-FILE DDMAND-FILE SAROUT-FILE REPORT-FILE
           DISPLAY 'SARXT010 - PACKS ' WS-PRODUCED
                   ' REJECTED ' WS-REJECTED ' OVERDUE ' WS-OVERDUE
                   ' PAGES ' WS-PAGES-PRINTED
           IF WS-REJECTED > ZERO
           OR WS-OVERDUE > ZERO
           OR WS-TRUNCATED-PACKS > ZERO
               MOVE 4                      TO RETURN-CODE
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
      *================================================================*
       9500-ADD-MONTHS.
      *================================================================*
      *    WS-AM-DATE + WS-AM-MONTHS CALENDAR MONTHS. A DAY THAT DOES
      *    NOT EXIST IN THE TARGET MONTH BECOMES ITS LAST DAY.
           ADD WS-AM-MONTHS                TO WS-AM-MM
           PERFORM UNTIL WS-AM-MM <= 12
               SUBTRACT 12                 FROM WS-AM-MM
               ADD 1                       TO WS-AM-YYYY
           END-PERFORM
           MOVE WS-AM-YYYY                 TO WS-AM-NEXT-YYYY
           MOVE WS-AM-MM                   TO WS-AM-NEXT-MM
           MOVE 01                         TO WS-AM-NEXT-DD
           ADD 1                           TO WS-AM-NEXT-MM
           IF WS-AM-NEXT-MM > 12
               MOVE 01                     TO WS-AM-NEXT-MM
               ADD 1                       TO WS-AM-NEXT-YYYY
           END-IF
      *    DAYS IN THE TARGET MONTH = 1ST OF NEXT MONTH - 1ST OF THIS
           COMPUTE WS-AM-DIM =
                   FUNCTION INTEGER-OF-DATE (WS-AM-NEXT)
                 - FUNCTION INTEGER-OF-DATE
                   (WS-AM-YYYY * 10000 + WS-AM-MM * 100 + 1)
           IF WS-AM-DD > WS-AM-DIM
               MOVE WS-AM-DIM              TO WS-AM-DD
           END-IF
           .
      *================================================================*
       9600-FORMAT-DATE.
      *================================================================*
           IF WS-FD-IN = ZERO OR WS-FD-IN NOT NUMERIC
               MOVE SPACES                 TO WS-FD-OUT
           ELSE
               STRING WS-FD-IN (7:2) '/' WS-FD-IN (5:2) '/'
                      WS-FD-IN (1:4)
                      DELIMITED BY SIZE
                      INTO WS-FD-OUT
               END-STRING
           END-IF
           .
      *
           COPY ABNDPROC.
