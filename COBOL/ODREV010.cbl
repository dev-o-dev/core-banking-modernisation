       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ODREV010.
       AUTHOR.        S A THORNTON.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2004-11-08.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : ODREV010                                            *
      * TITLE    : ARRANGED OVERDRAFTS - EXPIRY, REVIEW AND REFERRALS  *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : ODREV01 (DAILY, AFTER PSEOD01 AND CGCAL01)          *
      *----------------------------------------------------------------*
      * INPUT    : CIFMAST  - CUSTOMER MASTER (VSAM KSDS, RANDOM)      *
      *            PRDPARM  - PRODUCT PARAMETERS (OD MAX LIMIT, FEES,  *
      *                       UNARRANGED BUFFER)                       *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, DYNAMIC - *
      *                       SEQUENTIAL BROWSE WITH REWRITE)          *
      * OUTPUT   : LETTERS  - REVIEW / EXPIRY LETTERS (ODLTRREC FB 200)*
      *            REFERRAL - ACCOUNTS REFERRED TO FINANCIAL SUPPORT   *
      *                       UNIT (FB 150)                            *
      *            REPORT   - OVERDRAFT REVIEW REPORT (FBA 133)        *
      *----------------------------------------------------------------*
      * FOR EVERY OPEN OR DORMANT CURRENT ACCOUNT:                     *
      *                                                                *
      * 1. ARRANGED LIMIT (ACM-OD-LIMIT > 0) WITH AN EXPIRY DATE       *
      *    (ACM-OD-EXPIRY-DATE). THE LIMIT IS VALID UP TO AND          *
      *    INCLUDING THE EXPIRY DATE.                                  *
      *    A. EXPIRY 30 DAYS AHEAD - REVIEW NOTICE 'ODRV' ASKING THE   *
      *       CUSTOMER TO CONTACT THE BRANCH TO RENEW.                 *
      *    B. EXPIRY 7 DAYS AHEAD - FINAL REMINDER 'ODR7'.             *
      *    C. EXPIRY DATE PASSED - THE LIMIT IS REMOVED (SET TO ZERO,  *
      *       EXPIRY CLEARED) AND LETTER 'ODEX' SENT. IF THE ACCOUNT   *
      *       IS OVERDRAWN IT IS NOW IN UNARRANGED OVERDRAFT - LETTER  *
      *       'ODXO' AND A REFERRAL 'EXOD' SO THE FINANCIAL SUPPORT    *
      *       UNIT CAN CONTACT THE CUSTOMER BEFORE CHARGES BUILD UP.   *
      *    THERE IS NO 'NOTICE SENT' FIELD ON THE ACCOUNT, SO A NOTICE *
      *    IS SENT WHEN EXPIRY MINUS N DAYS FALLS AFTER THE PREVIOUS   *
      *    BUSINESS DATE AND ON OR BEFORE TODAY. THIS COVERS WEEKENDS  *
      *    AND BANK HOLIDAYS WITHOUT SENDING A SECOND LETTER.          *
      *                                                                *
      * 2. LIMIT ABOVE THE PRODUCT MAXIMUM (PRD-OD-MAX-LIMIT) - REFER  *
      *    'XLIM' (CREDIT RISK). NOTHING IS CHANGED ON THE ACCOUNT.    *
      *                                                                *
      * 3. PERSISTENT UNARRANGED OVERDRAFT - LEDGER BALANCE BELOW THE  *
      *    ARRANGED LIMIT BY MORE THAN THE PRODUCT BUFFER AND IN       *
      *    UNARRANGED OVERDRAFT FOR WS-REFER-DAYS OR MORE DAYS IN THE  *
      *    CURRENT CHARGING PERIOD (ACM-UNARR-OD-DAYS, COUNTED BY      *
      *    CGCAL010) - REFER 'PUOD'.                                   *
      *                                                                *
      * LETTERS ARE NOT PRODUCED FOR DECEASED CUSTOMERS (BEREAVEMENT   *
      * TEAM DEALS WITH THE ESTATE) BUT THE LIMIT IS STILL REMOVED.    *
      *                                                                *
      * SAFETY: IF THE GAP BETWEEN PREVIOUS AND CURRENT BUSINESS DATE  *
      * IS MORE THAN WS-MAX-GAP-DAYS THE DATE CARD IS ASSUMED WRONG    *
      * AND THE RUN ABENDS U1005 BEFORE ANY LIMIT IS TOUCHED.          *
      *----------------------------------------------------------------*
      * KNOWN PROBLEMS                                                 *
      * PIR 2005/017 OPEN - LIMITS GRANTED BEFORE NOV 2004 HAVE NO     *
      *   EXPIRY DATE ('EVERGREEN'). THEY ARE LISTED FOR INFORMATION   *
      *   EVERY DAY AND NEVER REVIEWED. BRANCHES WERE TO BACKFILL.     *
      * PIR 2013/087 OPEN - IF A DAY'S RUN IS SKIPPED (NOT RERUN WITH  *
      *   THE SKIPPED DATE CARD) THE NOTICES FOR THAT DAY ARE LOST.    *
      * PIR 2015/022 OPEN - ACM-UNARR-OD-DAYS IS RESET AT EACH CHARGE  *
      *   PERIOD START, SO A CUSTOMER OVERDRAWN ACROSS THE PERIOD END  *
      *   MAY NOT BE REFERRED UNTIL WS-REFER-DAYS INTO THE NEW PERIOD. *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2004-11-08 SAT  ORIGINAL VERSION (OFT OVERDRAFT REVIEW - ALL   *
      *                 NEW LIMITS GRANTED FOR 12 MONTHS)              *
      * 2006-03-20 SAT  PRODUCT MAXIMUM CHECK                          *
      * 2009-10-05 AOS  FINAL REMINDER, BUSINESS TEMPLATES, DORMANT    *
      *                 ACCOUNTS INCLUDED                              *
      * 2011-06-13 JNB  BEREAVEMENT - SUPPRESS LETTERS (CFDEC010)      *
      * 2015-04-27 PDQ  REFERRAL FILE FOR FINANCIAL SUPPORT UNIT       *
      *                 (FCA CONC / TREATING CUSTOMERS FAIRLY)         *
      * 2016-02-08 KLM  DATE GAP SAFETY CHECK AFTER 2016-01-04 RERUN   *
      *                 REMOVED 1,900 LIMITS IN ERROR (INC 16/0042)    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
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
           SELECT LETTERS-FILE     ASSIGN TO LETTERS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LETTERS-STATUS.
           SELECT REFERRAL-FILE    ASSIGN TO REFERRAL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REFERRAL-STATUS.
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
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(200).
       FD  REFERRAL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REFERRAL-RECORD                 PIC X(150).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'ODREV010'.
      *
       01  WS-CONSTANTS.
           05  WS-REVIEW-DAYS              PIC S9(03) COMP-3 VALUE +30.
           05  WS-FINAL-DAYS               PIC S9(03) COMP-3 VALUE +7.
           05  WS-REFER-DAYS               PIC S9(03) COMP-3 VALUE +20.
           05  WS-MAX-GAP-DAYS             PIC S9(03) COMP-3 VALUE +7.
      *    USED WHEN THE PRODUCT RECORD IS MISSING (SEE PRMNT010)
           05  WS-DEFAULT-BUFFER           PIC S9(13)V99 COMP-3
                                           VALUE +10.00.
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
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REFERRAL-STATUS          PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-REWRITE-IND              PIC X(01).
               88  WS-REWRITE-NEEDED           VALUE 'Y'.
           05  WS-CIF-IND                  PIC X(01).
               88  WS-CIF-LOADED               VALUE 'Y'.
               88  WS-CIF-MISSING              VALUE 'M'.
               88  WS-CIF-NOT-READ             VALUE 'N'.
           05  WS-PRD-IND                  PIC X(01).
               88  WS-PRD-FOUND                VALUE 'Y'.
           05  WS-REFERRED-IND             PIC X(01).
               88  WS-ALREADY-REFERRED         VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-PREV-DATE                PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-PREV-DATE-INT            PIC S9(09) COMP.
           05  WS-EXP-INT                  PIC S9(09) COMP.
           05  WS-DAYS-TO-EXPIRY           PIC S9(07) COMP-3.
           05  WS-GAP-DAYS                 PIC S9(07) COMP-3.
           05  WS-HEADROOM                 PIC S9(13)V99 COMP-3.
           05  WS-EXCESS                   PIC S9(13)V99 COMP-3.
           05  WS-OLD-LIMIT                PIC S9(13)V99 COMP-3.
           05  WS-OLD-EXPIRY               PIC 9(08).
           05  WS-BUFFER                   PIC S9(13)V99 COMP-3.
           05  WS-MAX-LIMIT                PIC S9(13)V99 COMP-3.
           05  WS-ARR-FEE                  PIC S9(13)V99 COMP-3.
           05  WS-ARR-RATE                 PIC S9(03)V9(04) COMP-3.
           05  WS-CACHED-PROD-CD           PIC X(10) VALUE LOW-VALUES.
           05  WS-LETTER-CD                PIC X(04).
           05  WS-REFER-RSN                PIC X(04).
           05  WS-ACTION                   PIC X(10).
           05  WS-NOTE                     PIC X(40).
           05  WS-DATE-CHECK               PIC 9(08).
           05  WS-DATE-CHECK-R REDEFINES WS-DATE-CHECK.
               10  WS-DC-CCYY              PIC 9(04).
               10  WS-DC-MM                PIC 9(02).
               10  WS-DC-DD                PIC 9(02).
           05  WS-NAME-WORK                PIC X(40).
      *
      *---- REFERRAL RECORD (FINANCIAL SUPPORT UNIT WORKLIST) ---------*
       01  WS-REFERRAL.
           05  RF-REFER-DATE               PIC 9(08).
           05  RF-REASON                   PIC X(04).
               88  RF-PERSISTENT-UNARRANGED    VALUE 'PUOD'.
               88  RF-EXPIRED-OVERDRAWN        VALUE 'EXOD'.
               88  RF-OVER-PRODUCT-MAX         VALUE 'XLIM'.
           05  RF-SORT-CODE                PIC X(06).
           05  RF-ACCT-NO                  PIC X(08).
           05  RF-CIF-NO                   PIC X(12).
           05  RF-CUST-TYPE                PIC X(01).
           05  RF-CUST-NAME                PIC X(35).
           05  RF-HOME-BRANCH-SC           PIC X(06).
           05  RF-MOBILE-TEL               PIC X(20).
           05  RF-LEDGER-BAL               PIC S9(11)V99
                                           SIGN LEADING SEPARATE.
           05  RF-OD-LIMIT                 PIC 9(11)V99.
           05  RF-EXCESS-AMT               PIC 9(11)V99.
           05  RF-UNARR-DAYS               PIC 9(03).
           05  RF-UNPAID-ITEMS             PIC 9(03).
           05  FILLER                      PIC X(04).
      *
       01  WS-COUNTERS.
           05  WS-ACCTS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-CURR-EXAMINED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-LIMITS-HELD              PIC S9(09) COMP-3 VALUE 0.
           05  WS-LIMITS-AMT               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-EVERGREEN                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EVERGREEN-AMT            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-REVIEW-30                PIC S9(09) COMP-3 VALUE 0.
           05  WS-REVIEW-7                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXPIRED                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXPIRED-AMT              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-EXPIRED-OVERDRAWN        PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXPIRED-OD-AMT           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-UNARRANGED               PIC S9(09) COMP-3 VALUE 0.
           05  WS-UNARRANGED-AMT           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-REFER-PUOD               PIC S9(09) COMP-3 VALUE 0.
           05  WS-REFER-EXOD               PIC S9(09) COMP-3 VALUE 0.
           05  WS-REFER-XLIM               PIC S9(09) COMP-3 VALUE 0.
           05  WS-LETTERS-WRITTEN          PIC S9(09) COMP-3 VALUE 0.
           05  WS-LETTERS-SUPPRESSED       PIC S9(09) COMP-3 VALUE 0.
           05  WS-CIF-NOT-FOUND            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PRD-NOT-FOUND            PIC S9(09) COMP-3 VALUE 0.
           05  WS-BAD-EXPIRY               PIC S9(09) COMP-3 VALUE 0.
           05  WS-REWRITES                 PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'ODREV010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - ARRANGED OVERDRAFT REVIEW'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(08) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(14) VALUE 'CUSTOMER'.
           05  FILLER            PIC X(12) VALUE 'ACTION'.
           05  FILLER            PIC X(15) VALUE '    OLD LIMIT'.
           05  FILLER            PIC X(12) VALUE 'EXPIRY'.
           05  FILLER            PIC X(16) VALUE '       BALANCE'.
           05  FILLER            PIC X(45) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-LIMIT                    PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-EXPIRY                   PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-BALANCE                  PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
           05  FILLER                      PIC X(05) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY ODLTRREC.
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
           PERFORM 2000-PROCESS-ACCOUNT THRU 2000-EXIT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CIFMAST-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                OUTPUT LETTERS-FILE
                       REFERRAL-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-LETTERS-STATUS NOT = '00'
           OR WS-REFERRAL-STATUS NOT = '00'
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
      *    THE PREVIOUS BUSINESS DATE DRIVES THE NOTICE WINDOW. A
      *    MISSING ONE IS TREATED AS YESTERDAY.
           IF DTC-PREV-BUS-DATE NUMERIC
           AND DTC-PREV-BUS-DATE > 19000101
           AND DTC-PREV-BUS-DATE < WS-BUS-DATE
               MOVE DTC-PREV-BUS-DATE      TO WS-PREV-DATE
               COMPUTE WS-PREV-DATE-INT =
                       FUNCTION INTEGER-OF-DATE (WS-PREV-DATE)
           ELSE
               COMPUTE WS-PREV-DATE-INT = WS-BUS-DATE-INT - 1
               COMPUTE WS-PREV-DATE =
                       FUNCTION DATE-OF-INTEGER (WS-PREV-DATE-INT)
               DISPLAY 'ODREV010 - NO PREVIOUS DATE ON CARD, USING '
                       WS-PREV-DATE
           END-IF
           COMPUTE WS-GAP-DAYS = WS-BUS-DATE-INT - WS-PREV-DATE-INT
           IF WS-GAP-DAYS > WS-MAX-GAP-DAYS
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'BUSINESS DATE GAP TOO LARGE - CHECK DATE CARD'
                                           TO ABN-MESSAGE
               MOVE WS-PREV-DATE           TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           DISPLAY 'ODREV010 - BUSINESS DATE ' WS-BUS-DATE
                   ' PREVIOUS ' WS-PREV-DATE
           MOVE LOW-VALUES                 TO ACM-KEY
           START ACCTMAST-FILE KEY NOT < ACM-KEY
           IF NOT WS-ACCTMAST-OK
               SET WS-END-OF-ACCOUNTS      TO TRUE
           ELSE
               PERFORM 8000-READ-NEXT
           END-IF
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           ADD 1                           TO WS-ACCTS-READ
           IF NOT ACM-CURRENT
           OR NOT (ACM-OPEN OR ACM-DORMANT)
               PERFORM 8000-READ-NEXT
               GO TO 2000-EXIT
           END-IF
           ADD 1                           TO WS-CURR-EXAMINED
           MOVE 'N'                        TO WS-REWRITE-IND
                                              WS-CIF-IND
                                              WS-REFERRED-IND
           IF ACM-OD-LIMIT NOT NUMERIC
               MOVE ZERO                   TO ACM-OD-LIMIT
           END-IF
           IF ACM-OD-LIMIT > ZERO
               PERFORM 2100-ARRANGED-LIMIT THRU 2100-EXIT
           END-IF
           PERFORM 2500-CHECK-UNARRANGED THRU 2500-EXIT
           IF WS-REWRITE-NEEDED
               PERFORM 6100-REWRITE-ACCOUNT
           END-IF
           PERFORM 8000-READ-NEXT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-ARRANGED-LIMIT.
      *================================================================*
           ADD 1                           TO WS-LIMITS-HELD
           ADD ACM-OD-LIMIT                TO WS-LIMITS-AMT
           PERFORM 2150-GET-PRODUCT THRU 2150-EXIT
           MOVE ACM-OD-LIMIT               TO WS-OLD-LIMIT
           MOVE ACM-OD-EXPIRY-DATE         TO WS-OLD-EXPIRY

      *    LIMIT ABOVE THE PRODUCT MAXIMUM - CREDIT RISK REFERRAL.
           IF WS-MAX-LIMIT > ZERO
           AND ACM-OD-LIMIT > WS-MAX-LIMIT
               MOVE 'XLIM'                 TO WS-REFER-RSN
               COMPUTE WS-EXCESS = ACM-OD-LIMIT - WS-MAX-LIMIT
               PERFORM 4500-WRITE-REFERRAL
               ADD 1                       TO WS-REFER-XLIM
               MOVE 'REFER XLIM'           TO WS-ACTION
               MOVE 'LIMIT ABOVE PRODUCT MAXIMUM' TO WS-NOTE
               PERFORM 7100-REPORT-DETAIL
           END-IF

      *    EVERGREEN LIMIT - NO EXPIRY DATE (PIR 2005/017).
           IF ACM-OD-EXPIRY-DATE NOT NUMERIC
           OR ACM-OD-EXPIRY-DATE = ZERO
               ADD 1                       TO WS-EVERGREEN
               ADD ACM-OD-LIMIT            TO WS-EVERGREEN-AMT
               MOVE 'EVERGREEN'            TO WS-ACTION
               MOVE 'NO EXPIRY DATE - PIR 2005/017'
                                           TO WS-NOTE
               PERFORM 7100-REPORT-DETAIL
               GO TO 2100-EXIT
           END-IF

      *    BASIC VALIDITY - INTEGER-OF-DATE ABENDS ON A BAD DATE.
           MOVE ACM-OD-EXPIRY-DATE         TO WS-DATE-CHECK
           IF WS-DC-CCYY < 1987
           OR WS-DC-MM < 01 OR WS-DC-MM > 12
           OR WS-DC-DD < 01 OR WS-DC-DD > 31
               ADD 1                       TO WS-BAD-EXPIRY
               MOVE 'BAD DATE'             TO WS-ACTION
               MOVE 'EXPIRY DATE INVALID - NOT ACTIONED'
                                           TO WS-NOTE
               PERFORM 7100-REPORT-DETAIL
               GO TO 2100-EXIT
           END-IF

           COMPUTE WS-EXP-INT =
                   FUNCTION INTEGER-OF-DATE (ACM-OD-EXPIRY-DATE)
           COMPUTE WS-DAYS-TO-EXPIRY = WS-EXP-INT - WS-BUS-DATE-INT

           EVALUATE TRUE
               WHEN WS-EXP-INT < WS-BUS-DATE-INT
                   PERFORM 2200-EXPIRE-LIMIT
               WHEN WS-EXP-INT > WS-PREV-DATE-INT + WS-REVIEW-DAYS
               AND  WS-EXP-INT <= WS-BUS-DATE-INT + WS-REVIEW-DAYS
                   MOVE 'ODRV'             TO WS-LETTER-CD
                   PERFORM 2300-REVIEW-NOTICE
               WHEN WS-EXP-INT > WS-PREV-DATE-INT + WS-FINAL-DAYS
               AND  WS-EXP-INT <= WS-BUS-DATE-INT + WS-FINAL-DAYS
                   MOVE 'ODR7'             TO WS-LETTER-CD
                   PERFORM 2300-REVIEW-NOTICE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2150-GET-PRODUCT.
      *================================================================*
      *    PRDPARM IS KEPT IN THE BUFFER WHILE THE PRODUCT IS UNCHANGED
      *    (ACCOUNTS ARE IN SORT CODE ORDER, SO THIS SAVES LITTLE -
      *    BUT EACH RANDOM READ IS A VSAM I/O).
           IF ACM-PROD-CD = WS-CACHED-PROD-CD
               GO TO 2150-EXIT
           END-IF
           MOVE ACM-PROD-CD                TO WS-CACHED-PROD-CD
                                              PRD-PROD-CD
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN WS-PRDPARM-OK
                   SET WS-PRD-FOUND        TO TRUE
                   MOVE PRD-OD-MAX-LIMIT   TO WS-MAX-LIMIT
                   MOVE PRD-UNARR-OD-BUFFER TO WS-BUFFER
                   MOVE PRD-ARR-OD-MONTHLY-FEE TO WS-ARR-FEE
                   MOVE PRD-DR-RATE-ARRANGED TO WS-ARR-RATE
               WHEN WS-PRDPARM-NOTFND
                   MOVE 'N'                TO WS-PRD-IND
                   ADD 1                   TO WS-PRD-NOT-FOUND
                   MOVE ZERO               TO WS-MAX-LIMIT
                                              WS-ARR-FEE
                                              WS-ARR-RATE
                   MOVE WS-DEFAULT-BUFFER  TO WS-BUFFER
                   DISPLAY 'ODREV010 - PRODUCT NOT ON PRDPARM '
                           ACM-PROD-CD
               WHEN OTHER
                   MOVE '2150-GET-PRODUCT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PRDPARM READ FAILED' TO ABN-MESSAGE
                   MOVE WS-PRDPARM-STATUS  TO ABN-FILE-STATUS
                   MOVE ACM-PROD-CD        TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           IF WS-BUFFER NOT NUMERIC
               MOVE WS-DEFAULT-BUFFER      TO WS-BUFFER
           END-IF
           .
       2150-EXIT.
           EXIT.
      *================================================================*
       2200-EXPIRE-LIMIT.
      *================================================================*
      *    REMOVE THE LIMIT. ACM-AVAIL-BAL EXCLUDES THE LIMIT SO IT IS
      *    NOT CHANGED - SPENDING POWER (AVAIL + LIMIT) DROPS AT ONCE
      *    FOR CARD AUTHORISATION AND IBANK. THE UNDRAWN COMMITMENT
      *    MEMO (GL 91000) IS RECALCULATED BY GLEXT010 AT MONTH END.
           MOVE ZERO                       TO ACM-OD-LIMIT
                                              ACM-OD-EXPIRY-DATE
           SET WS-REWRITE-NEEDED           TO TRUE
           ADD 1                           TO WS-EXPIRED
           ADD WS-OLD-LIMIT                TO WS-EXPIRED-AMT
           IF ACM-LEDGER-BAL < ZERO
               MOVE 'ODXO'                 TO WS-LETTER-CD
               COMPUTE WS-EXCESS = ZERO - ACM-LEDGER-BAL
               ADD 1                       TO WS-EXPIRED-OVERDRAWN
               ADD WS-EXCESS               TO WS-EXPIRED-OD-AMT
               MOVE 'REMOVED-OD'           TO WS-ACTION
               MOVE 'EXPIRED WHILE OVERDRAWN - REFERRED'
                                           TO WS-NOTE
               MOVE 'EXOD'                 TO WS-REFER-RSN
               PERFORM 4500-WRITE-REFERRAL
               ADD 1                       TO WS-REFER-EXOD
               SET WS-ALREADY-REFERRED     TO TRUE
           ELSE
               MOVE 'ODEX'                 TO WS-LETTER-CD
               MOVE ZERO                   TO WS-EXCESS
               MOVE 'REMOVED'              TO WS-ACTION
               MOVE 'LIMIT EXPIRED AND REMOVED' TO WS-NOTE
           END-IF
           MOVE ZERO                       TO WS-DAYS-TO-EXPIRY
           PERFORM 4000-WRITE-LETTER THRU 4000-EXIT
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2300-REVIEW-NOTICE.
      *================================================================*
           MOVE ZERO                       TO WS-EXCESS
           IF WS-LETTER-CD = 'ODRV'
               ADD 1                       TO WS-REVIEW-30
               MOVE 'REVIEW 30'            TO WS-ACTION
               MOVE '30 DAY REVIEW NOTICE' TO WS-NOTE
           ELSE
               ADD 1                       TO WS-REVIEW-7
               MOVE 'REVIEW 7'             TO WS-ACTION
               MOVE 'FINAL REMINDER BEFORE EXPIRY' TO WS-NOTE
           END-IF
           PERFORM 4000-WRITE-LETTER THRU 4000-EXIT
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2500-CHECK-UNARRANGED.
      *================================================================*
      *    HEADROOM IS TAKEN ON THE LEDGER BALANCE (AS CGCAL010 DOES
      *    FOR THE DAILY UNARRANGED FEE) AND ON THE LIMIT AFTER ANY
      *    REMOVAL ABOVE.
           COMPUTE WS-HEADROOM = ACM-LEDGER-BAL + ACM-OD-LIMIT
           IF WS-HEADROOM NOT < ZERO
               GO TO 2500-EXIT
           END-IF
           ADD 1                           TO WS-UNARRANGED
           COMPUTE WS-EXCESS = ZERO - WS-HEADROOM
           ADD WS-EXCESS                   TO WS-UNARRANGED-AMT
           IF WS-ALREADY-REFERRED
               GO TO 2500-EXIT
           END-IF
           PERFORM 2150-GET-PRODUCT THRU 2150-EXIT
           IF ACM-UNARR-OD-DAYS NOT NUMERIC
               MOVE ZERO                   TO ACM-UNARR-OD-DAYS
           END-IF
           IF ACM-UNPAID-ITEMS NOT NUMERIC
               MOVE ZERO                   TO ACM-UNPAID-ITEMS
           END-IF
           IF WS-EXCESS > WS-BUFFER
           AND ACM-UNARR-OD-DAYS >= WS-REFER-DAYS
               MOVE 'PUOD'                 TO WS-REFER-RSN
               PERFORM 4500-WRITE-REFERRAL
               ADD 1                       TO WS-REFER-PUOD
               MOVE ACM-OD-LIMIT           TO WS-OLD-LIMIT
               MOVE ACM-OD-EXPIRY-DATE     TO WS-OLD-EXPIRY
               MOVE 'REFER PUOD'           TO WS-ACTION
               MOVE SPACES                 TO WS-NOTE
               STRING 'UNARRANGED ' ACM-UNARR-OD-DAYS
                      ' DAYS THIS PERIOD'
                      DELIMITED BY SIZE
                      INTO WS-NOTE
               END-STRING
               PERFORM 7100-REPORT-DETAIL
           END-IF
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       4000-WRITE-LETTER.
      *================================================================*
           PERFORM 4100-GET-CIF THRU 4100-EXIT
           IF WS-CIF-MISSING
               MOVE 'CIF NOT FOUND - NO LETTER' TO WS-NOTE
               GO TO 4000-EXIT
           END-IF
           IF CIF-DECEASED
           OR (CIF-DATE-OF-DEATH NUMERIC
               AND CIF-DATE-OF-DEATH > ZERO)
               ADD 1                       TO WS-LETTERS-SUPPRESSED
               MOVE 'DECEASED - LETTER SUPPRESSED' TO WS-NOTE
               GO TO 4000-EXIT
           END-IF

           MOVE SPACES                     TO ODL-LETTER-RECORD
           MOVE WS-LETTER-CD               TO ODL-LETTER-CD
           MOVE SPACES                     TO ODL-TEMPLATE-ID
           IF CIF-BUSINESS
               STRING WS-LETTER-CD 'B01' DELIMITED BY SIZE
                      INTO ODL-TEMPLATE-ID
               END-STRING
           ELSE
               STRING WS-LETTER-CD 'P01' DELIMITED BY SIZE
                      INTO ODL-TEMPLATE-ID
               END-STRING
           END-IF
           MOVE WS-BUS-DATE                TO ODL-ISSUE-DATE
           MOVE ACM-SORT-CODE              TO ODL-SORT-CODE
           MOVE ACM-ACCT-NO                TO ODL-ACCT-NO
           MOVE ACM-CIF-NO                 TO ODL-CIF-NO
           MOVE CIF-CUST-TYPE              TO ODL-CUST-TYPE
           MOVE WS-NAME-WORK               TO ODL-CUST-NAME
           MOVE CIF-POST-CD                TO ODL-POST-CD
           MOVE ACM-PROD-CD                TO ODL-PROD-CD
           MOVE WS-OLD-LIMIT               TO ODL-OD-LIMIT
           MOVE WS-OLD-EXPIRY              TO ODL-EXPIRY-DATE
           MOVE ACM-LEDGER-BAL             TO ODL-LEDGER-BAL
           MOVE WS-EXCESS                  TO ODL-EXCESS-AMT
      *    ACCOUNT-LEVEL NEGOTIATED RATE WINS OVER THE PRODUCT RATE.
           IF ACM-INT-RATE-DR NUMERIC
           AND ACM-INT-RATE-DR > ZERO
               MOVE ACM-INT-RATE-DR        TO ODL-DR-RATE
           ELSE
               MOVE WS-ARR-RATE            TO ODL-DR-RATE
           END-IF
           MOVE WS-ARR-FEE                 TO ODL-MONTHLY-FEE
           IF WS-DAYS-TO-EXPIRY > ZERO
               MOVE WS-DAYS-TO-EXPIRY      TO ODL-DAYS-TO-EXPIRY
           ELSE
               MOVE ZERO                   TO ODL-DAYS-TO-EXPIRY
           END-IF
           MOVE CIF-HOME-BRANCH-SC         TO ODL-HOME-BRANCH-SC
           WRITE LETTERS-RECORD            FROM ODL-LETTER-RECORD
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '4000-WRITE-LETTER'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-LETTERS-WRITTEN
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-GET-CIF.
      *================================================================*
           IF NOT WS-CIF-NOT-READ
               GO TO 4100-EXIT
           END-IF
           MOVE ACM-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   SET WS-CIF-LOADED       TO TRUE
               WHEN WS-CIFMAST-NOTFND
                   SET WS-CIF-MISSING      TO TRUE
                   ADD 1                   TO WS-CIF-NOT-FOUND
                   MOVE SPACES             TO CIF-CUSTOMER-RECORD
                   MOVE 'P'                TO CIF-CUST-TYPE
                   MOVE ZERO               TO CIF-DATE-OF-DEATH
                   MOVE 'CUSTOMER NOT ON CIF' TO WS-NAME-WORK
                   GO TO 4100-EXIT
               WHEN OTHER
                   MOVE '4100-GET-CIF'     TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE ACM-CIF-NO         TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
      *    LETTER NAME - 'MR J SMITH' FOR PERSONAL; BUSINESS NAMES ARE
      *    HELD IN FIRST + LAST NAME (CFMNT01 SPLITS LONG NAMES).
           MOVE SPACES                     TO WS-NAME-WORK
           IF CIF-BUSINESS
               STRING CIF-FIRST-NM   DELIMITED BY '  '
                      ' '            DELIMITED BY SIZE
                      CIF-LAST-NM    DELIMITED BY '  '
                      INTO WS-NAME-WORK
               END-STRING
           ELSE
               STRING CIF-TITLE      DELIMITED BY SPACE
                      ' '            DELIMITED BY SIZE
                      CIF-FIRST-NM (1:1) DELIMITED BY SIZE
                      ' '            DELIMITED BY SIZE
                      CIF-LAST-NM    DELIMITED BY '  '
                      INTO WS-NAME-WORK
               END-STRING
           END-IF
           .
       4100-EXIT.
           EXIT.
      *================================================================*
       4500-WRITE-REFERRAL.
      *================================================================*
           PERFORM 4100-GET-CIF THRU 4100-EXIT
           MOVE SPACES                     TO WS-REFERRAL
           MOVE WS-BUS-DATE                TO RF-REFER-DATE
           MOVE WS-REFER-RSN               TO RF-REASON
           MOVE ACM-SORT-CODE              TO RF-SORT-CODE
           MOVE ACM-ACCT-NO                TO RF-ACCT-NO
           MOVE ACM-CIF-NO                 TO RF-CIF-NO
           MOVE CIF-CUST-TYPE              TO RF-CUST-TYPE
           MOVE WS-NAME-WORK               TO RF-CUST-NAME
           IF WS-CIF-LOADED
               MOVE CIF-HOME-BRANCH-SC     TO RF-HOME-BRANCH-SC
               MOVE CIF-MOBILE-TEL         TO RF-MOBILE-TEL
           ELSE
               MOVE ACM-SORT-CODE          TO RF-HOME-BRANCH-SC
           END-IF
           MOVE ACM-LEDGER-BAL             TO RF-LEDGER-BAL
           IF WS-REFER-RSN = 'EXOD'
               MOVE WS-OLD-LIMIT           TO RF-OD-LIMIT
           ELSE
               MOVE ACM-OD-LIMIT           TO RF-OD-LIMIT
           END-IF
           MOVE WS-EXCESS                  TO RF-EXCESS-AMT
           IF ACM-UNARR-OD-DAYS NUMERIC
               MOVE ACM-UNARR-OD-DAYS      TO RF-UNARR-DAYS
           ELSE
               MOVE ZERO                   TO RF-UNARR-DAYS
           END-IF
           IF ACM-UNPAID-ITEMS NUMERIC
               MOVE ACM-UNPAID-ITEMS       TO RF-UNPAID-ITEMS
           ELSE
               MOVE ZERO                   TO RF-UNPAID-ITEMS
           END-IF
           WRITE REFERRAL-RECORD           FROM WS-REFERRAL
           IF WS-REFERRAL-STATUS NOT = '00'
               MOVE '4500-WRITE-REFERRAL'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'REFERRAL WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-REFERRAL-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       6100-REWRITE-ACCOUNT.
      *================================================================*
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER
           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '6100-REWRITE-ACCOUNT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-REWRITES
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
           MOVE ACM-SORT-CODE              TO DL-SORT-CODE
           MOVE ACM-ACCT-NO                TO DL-ACCT-NO
           MOVE ACM-CIF-NO                 TO DL-CIF-NO
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE WS-OLD-LIMIT               TO DL-LIMIT
           IF WS-OLD-EXPIRY NUMERIC
           AND WS-OLD-EXPIRY > ZERO
               STRING WS-OLD-EXPIRY (1:4) '-' WS-OLD-EXPIRY (5:2) '-'
                      WS-OLD-EXPIRY (7:2)
                      DELIMITED BY SIZE
                      INTO DL-EXPIRY
               END-STRING
           ELSE
               MOVE '  NONE'               TO DL-EXPIRY
           END-IF
           MOVE ACM-LEDGER-BAL             TO DL-BALANCE
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-TOTAL-LINE.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           ADD 2                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-NEXT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   CONTINUE
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-NEXT'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-ACCTS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'CURRENT ACCOUNTS EXAMINED' TO TL-LABEL
           MOVE WS-CURR-EXAMINED           TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'ARRANGED LIMITS HELD (BEFORE RUN)' TO TL-LABEL
           MOVE WS-LIMITS-HELD             TO TL-COUNT
           MOVE WS-LIMITS-AMT              TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE '  OF WHICH NO EXPIRY (EVERGREEN)' TO TL-LABEL
           MOVE WS-EVERGREEN               TO TL-COUNT
           MOVE WS-EVERGREEN-AMT           TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE '30 DAY REVIEW NOTICES'    TO TL-LABEL
           MOVE WS-REVIEW-30               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE '7 DAY FINAL REMINDERS'    TO TL-LABEL
           MOVE WS-REVIEW-7                TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'LIMITS EXPIRED AND REMOVED' TO TL-LABEL
           MOVE WS-EXPIRED                 TO TL-COUNT
           MOVE WS-EXPIRED-AMT             TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE '  OF WHICH NOW UNARRANGED (EXCESS)' TO TL-LABEL
           MOVE WS-EXPIRED-OVERDRAWN       TO TL-COUNT
           MOVE WS-EXPIRED-OD-AMT          TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'ACCOUNTS IN UNARRANGED OVERDRAFT' TO TL-LABEL
           MOVE WS-UNARRANGED              TO TL-COUNT
           MOVE WS-UNARRANGED-AMT          TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'REFERRALS - PERSISTENT UNARRANGED' TO TL-LABEL
           MOVE WS-REFER-PUOD              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'REFERRALS - EXPIRED WHILE OVERDRAWN' TO TL-LABEL
           MOVE WS-REFER-EXOD              TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'REFERRALS - ABOVE PRODUCT MAXIMUM' TO TL-LABEL
           MOVE WS-REFER-XLIM              TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'LETTERS WRITTEN'          TO TL-LABEL
           MOVE WS-LETTERS-WRITTEN         TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'LETTERS SUPPRESSED - DECEASED' TO TL-LABEL
           MOVE WS-LETTERS-SUPPRESSED      TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'EXCEPTION - CUSTOMER NOT ON CIF' TO TL-LABEL
           MOVE WS-CIF-NOT-FOUND           TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'EXCEPTION - PRODUCT NOT ON PRDPARM' TO TL-LABEL
           MOVE WS-PRD-NOT-FOUND           TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'EXCEPTION - INVALID EXPIRY DATE' TO TL-LABEL
           MOVE WS-BAD-EXPIRY              TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'ACCOUNT RECORDS REWRITTEN' TO TL-LABEL
           MOVE WS-REWRITES                TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           CLOSE ACCTMAST-FILE CIFMAST-FILE PRDPARM-FILE LETTERS-FILE
                 REFERRAL-FILE REPORT-FILE
           DISPLAY 'ODREV010 - REVIEW NOTICES  ' WS-REVIEW-30
           DISPLAY 'ODREV010 - FINAL REMINDERS ' WS-REVIEW-7
           DISPLAY 'ODREV010 - LIMITS REMOVED  ' WS-EXPIRED
           DISPLAY 'ODREV010 - REFERRALS PUOD  ' WS-REFER-PUOD
           DISPLAY 'ODREV010 - REFERRALS EXOD  ' WS-REFER-EXOD
           IF WS-CIF-NOT-FOUND > ZERO
           OR WS-PRD-NOT-FOUND > ZERO
           OR WS-BAD-EXPIRY > ZERO
           OR WS-REFER-XLIM > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
