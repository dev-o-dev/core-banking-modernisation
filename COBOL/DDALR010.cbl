       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDALR010.
       AUTHOR.        H L TOWNSEND.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2016-09-05.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDALR010                                            *
      * TITLE    : CURRENT ACCOUNT ALERTS - UNARRANGED OVERDRAFT,      *
      *            CHARGE WARNING AND LOW BALANCE                      *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : DDALR01 STEP AL010 (DAILY, AFTER PSEOD01)           *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (SEQUENTIAL, READ     *
      *                       ONLY - BALANCES AFTER TONIGHT'S POSTING) *
      *            CIFMAST  - CUSTOMER MASTER (RANDOM - CONTACTS)      *
      *            PRDPARM  - PRODUCT PARAMETERS (UNARRANGED OD FEE,   *
      *                       BUFFER, MAX DAYS, MONTHLY CAP)           *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ALRTPREF - ALERT PREFERENCES AND SUPPRESSION STATE  *
      *                       (VSAM KSDS, RANDOM) - ALRTPREC           *
      * OUTPUT   : ALERTOUT - SMS / EMAIL REQUESTS FOR THE ALERT       *
      *                       GATEWAY (FB 250, HEADER/DETAIL/TRAILER)  *
      *            REPORT   - ALERTS REPORT (FBA 133)                  *
      *----------------------------------------------------------------*
      * REGULATORY BACKGROUND                                          *
      *   THE CMA RETAIL BANKING ORDER 2017 (ARTICLE 15) AND THE FCA   *
      *   ALERTS RULES REQUIRE EVERY PERSONAL CURRENT ACCOUNT CUSTOMER *
      *   TO BE ENROLLED, UNLESS THEY OPT OUT, IN AN ALERT TELLING     *
      *   THEM THEY HAVE GONE INTO AN UNARRANGED OVERDRAFT AND THAT    *
      *   THEY WILL BE CHARGED UNLESS THEY PAY IN IN TIME.             *
      *                                                                *
      * ONLY OPEN CURRENT ACCOUNTS ('CURR') ARE CONSIDERED. FOR EACH:  *
      *   ARRANGED LIMIT = ACM-OD-LIMIT (ZERO IF ACM-OD-EXPIRY-DATE    *
      *                    HAS PASSED - SAME RULE AS IBFD0201)         *
      *   UNARRANGED     = ACM-AVAIL-BAL + ARRANGED LIMIT < 0          *
      *   EXCESS         = AMOUNT BEYOND THE ARRANGED LIMIT            *
      *                                                                *
      * ALERT TYPES                                                    *
      *   UNAR  WENT INTO UNARRANGED OVERDRAFT TODAY (WAS NOT AT THE   *
      *         LAST RUN). MANDATORY UNLESS ALP-UNARR-OPT-OUT = 'Y'.   *
      *   CHGW  ABOUT TO BE CHARGED: EXCESS IS ABOVE THE PRODUCT       *
      *         BUFFER (PRD-UNARR-OD-BUFFER), THE PRODUCT LEVIES A     *
      *         DAILY FEE, CHARGES ARE NOT WAIVED, AND THE ACCOUNT HAS *
      *         NOT ALREADY REACHED PRD-UNARR-OD-MAX-DAYS OR THE       *
      *         MONTHLY CAP. SENT EACH BUSINESS DAY THIS IS TRUE (THE  *
      *         CUSTOMER CAN STILL AVOID THE NEXT DAY'S FEE BY PAYING  *
      *         IN BEFORE ALRD-ACTION-BY-DATE). WHEN BOTH UNAR AND     *
      *         CHGW APPLY ON THE SAME DAY ONLY CHGW IS SENT - ITS     *
      *         TEMPLATE INCLUDES THE UNARRANGED WORDING.              *
      *   LOWB  OPTED-IN LOW BALANCE: AVAILABLE BALANCE FELL BELOW     *
      *         ALP-LOWBAL-THRESHOLD SINCE THE LAST RUN. NOT SENT IF   *
      *         A UNAR / CHGW ALERT WENT THE SAME DAY. CAPPED AT       *
      *         WS-MAX-LOWB-PER-MONTH PER ACCOUNT PER CALENDAR MONTH.  *
      *                                                                *
      * SUPPRESSION OF REPEATS                                         *
      *   ALP-UNARR-STATE / ALP-LOWBAL-STATE HOLD THE CONDITION AT THE *
      *   LAST RUN; UNAR AND LOWB GO ONLY ON THE TRANSITION FROM 'N'   *
      *   TO 'Y'. THE LAST-ALERT DATES STOP A RERUN SENDING TWICE ON   *
      *   THE SAME BUSINESS DATE (ALL THREE TYPES).                    *
      *   AN ACCOUNT WITH NO PREFERENCE RECORD GETS A DEFAULT ONE      *
      *   (SMS, LOW BALANCE OFF) THE FIRST TIME IT GOES UNARRANGED.    *
      *                                                                *
      * CONTACT: SMS TO CIF-MOBILE-TEL, EMAIL TO CIF-EMAIL-ADDR. IF    *
      * THE CHOSEN CHANNEL HAS NO CONTACT DETAIL THE OTHER IS USED;    *
      * IF NEITHER EXISTS THE ALERT IS REPORTED AS AN EXCEPTION (RC 4) *
      * FOR THE CONTACT CENTRE (FCA - "REASONABLE STEPS" TO ALERT).    *
      * NO ALERTS FOR DECEASED / CLOSED / BLOCKED CUSTOMERS - THE      *
      * STATE IS STILL MAINTAINED.                                     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2016-09-05 HLT  ORIGINAL VERSION (UNARRANGED OD ALERTS, PILOT) *
      * 2017-01-23 HLT  LOW-BALANCE ALERTS MOVED FROM IBANK TO HOST    *
      * 2017-05-15 HLT  CHARGE WARNING ALERT (CMA ORDER ART. 15 - GO   *
      *                 LIVE 02/02/2018 BROUGHT FORWARD)               *
      * 2017-08-21 RKM  CHANNEL FALLBACK AND NO-CONTACT EXCEPTIONS     *
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
           SELECT ALRTPREF-FILE    ASSIGN TO ALRTPREF
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ALP-KEY
                                   FILE STATUS IS WS-ALRTPREF-STATUS.
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
           SELECT ALERTOUT-FILE    ASSIGN TO ALERTOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ALERTOUT-STATUS.
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
       FD  ALRTPREF-FILE.
           COPY ALRTPREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  ALERTOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ALERTOUT-RECORD                 PIC X(250).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDALR010'.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-LOWB-PER-MONTH       PIC 9(05) VALUE 00031.
           05  WS-TPL-UNARRANGED           PIC X(08) VALUE 'UNAROD01'.
           05  WS-TPL-CHARGE-WARN          PIC X(08) VALUE 'UNARCH01'.
           05  WS-TPL-LOW-BALANCE          PIC X(08) VALUE 'LOWBAL01'.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-ALRTPREF-STATUS          PIC X(02).
               88  WS-ALRTPREF-OK              VALUE '00'.
               88  WS-ALRTPREF-NOTFND          VALUE '23'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-NOTFND           VALUE '23'.
           05  WS-ALERTOUT-STATUS          PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-ALP-FOUND-IND            PIC X(01).
               88  WS-ALP-FOUND                VALUE 'Y'.
               88  WS-ALP-NEW                  VALUE 'N'.
           05  WS-ALP-CHANGED-IND          PIC X(01).
               88  WS-ALP-CHANGED              VALUE 'Y'.
               88  WS-ALP-UNCHANGED            VALUE 'N'.
           05  WS-PRD-FOUND-IND            PIC X(01) VALUE 'N'.
               88  WS-PRD-FOUND                VALUE 'Y'.
           05  WS-UNARR-IND                PIC X(01).
               88  WS-IS-UNARRANGED            VALUE 'Y'.
           05  WS-LOWBAL-IND               PIC X(01).
               88  WS-IS-BELOW-THRESHOLD       VALUE 'Y'.
           05  WS-SENT-TODAY-IND           PIC X(01).
               88  WS-OD-ALERT-SENT            VALUE 'Y'.
           05  WS-CIF-OK-IND               PIC X(01).
               88  WS-CIF-CAN-ALERT            VALUE 'Y'.
               88  WS-CIF-NO-ALERT             VALUE 'N'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-MONTH                PIC 9(06).
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-SPENDING-POWER           PIC S9(13)V99 COMP-3.
           05  WS-EXCESS                   PIC S9(13)V99 COMP-3.
           05  WS-FEES-SO-FAR              PIC S9(13)V99 COMP-3.
           05  WS-ALERT-TYPE               PIC X(04).
               88  WS-ALERT-UNAR               VALUE 'UNAR'.
               88  WS-ALERT-CHGW               VALUE 'CHGW'.
               88  WS-ALERT-LOWB               VALUE 'LOWB'.
           05  WS-TEMPLATE-ID              PIC X(08).
           05  WS-THRESHOLD-SHOWN          PIC S9(13)V99 COMP-3.
           05  WS-SEND-CHANNEL             PIC X(01).
               88  WS-SEND-SMS                 VALUE 'S'.
               88  WS-SEND-EMAIL               VALUE 'E'.
           05  WS-NOTE                     PIC X(40).
           05  WS-LAST-PROD-CD             PIC X(10) VALUE SPACES.
           05  WS-ALERT-SEQ                PIC 9(09) VALUE ZERO.
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURR-DATE            PIC 9(08).
               10  WS-CURR-TIME            PIC 9(06).
               10  FILLER                  PIC X(07).
      *
      *---- PRODUCT FIELDS KEPT FROM THE LAST PRDPARM READ ------------*
       01  WS-PRODUCT-CACHE.
           05  WS-PC-DAILY-FEE             PIC S9(13)V99 COMP-3.
           05  WS-PC-BUFFER                PIC S9(13)V99 COMP-3.
           05  WS-PC-MAX-DAYS              PIC 9(03).
           05  WS-PC-MONTHLY-CAP           PIC S9(13)V99 COMP-3.
           05  WS-PC-LEVIES-CHARGES        PIC X(01).
      *
      *---- ALERT GATEWAY RECORD (ALERTOUT) ---------------------------*
       01  WS-ALERT-RECORD.
           05  ALR-REC-TYPE                PIC X(01).
               88  ALR-HEADER                  VALUE 'H'.
               88  ALR-DETAIL                  VALUE 'D'.
               88  ALR-TRAILER                 VALUE 'T'.
           05  ALR-BODY                    PIC X(249).
           05  ALR-HDR REDEFINES ALR-BODY.
               10  ALRH-FILE-ID            PIC X(08).
               10  ALRH-BUSINESS-DATE      PIC 9(08).
               10  ALRH-CREATE-DATE        PIC 9(08).
               10  ALRH-CREATE-TIME        PIC 9(06).
               10  ALRH-FILE-SEQ-NO        PIC 9(06).
               10  ALRH-SOURCE-SYS         PIC X(08).
               10  FILLER                  PIC X(205).
           05  ALR-DTL REDEFINES ALR-BODY.
               10  ALRD-ALERT-ID           PIC X(18).
               10  ALRD-ALERT-TYPE         PIC X(04).
               10  ALRD-TEMPLATE-ID        PIC X(08).
               10  ALRD-CHANNEL            PIC X(01).
               10  ALRD-MOBILE-TEL         PIC X(20).
               10  ALRD-EMAIL-ADDR         PIC X(80).
               10  ALRD-CIF-NO             PIC X(12).
               10  ALRD-TITLE              PIC X(06).
               10  ALRD-LAST-NM            PIC X(30).
               10  ALRD-SORT-CODE          PIC X(06).
               10  ALRD-ACCT-LAST4         PIC X(04).
               10  ALRD-AVAIL-BAL          PIC S9(11)V99
                                           SIGN LEADING SEPARATE.
               10  ALRD-OD-LIMIT           PIC S9(11)V99
                                           SIGN LEADING SEPARATE.
               10  ALRD-THRESHOLD          PIC S9(11)V99
                                           SIGN LEADING SEPARATE.
               10  ALRD-ACTION-BY-DATE     PIC 9(08).
               10  FILLER                  PIC X(10).
           05  ALR-TRL REDEFINES ALR-BODY.
               10  ALRT-ALERT-COUNT        PIC 9(09).
               10  ALRT-SMS-COUNT          PIC 9(09).
               10  ALRT-EMAIL-COUNT        PIC 9(09).
               10  ALRT-UNAR-COUNT         PIC 9(09).
               10  ALRT-CHGW-COUNT         PIC 9(09).
               10  ALRT-LOWB-COUNT         PIC 9(09).
               10  FILLER                  PIC X(195).
      *
       01  WS-COUNTERS.
           05  WS-ACCT-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCT-CHECKED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCT-UNARRANGED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCT-BELOW               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ALERTS-UNAR              PIC S9(09) COMP-3 VALUE 0.
           05  WS-ALERTS-CHGW              PIC S9(09) COMP-3 VALUE 0.
           05  WS-ALERTS-LOWB              PIC S9(09) COMP-3 VALUE 0.
           05  WS-ALERTS-SMS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ALERTS-EMAIL             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ALERTS-TOTAL             PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUPP-REPEAT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUPP-OPTOUT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUPP-CUSTOMER            PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUPP-CAP                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUPP-RERUN               PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-CONTACT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-PREFS-CREATED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PREFS-UPDATED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-UNARR-EXCESS-TOTAL       PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'DDALR010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - CURRENT ACCOUNT ALERTS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(16) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(06) VALUE 'TYPE'.
           05  FILLER            PIC X(04) VALUE 'CH'.
           05  FILLER            PIC X(18) VALUE '         AVAILABLE'.
           05  FILLER            PIC X(16) VALUE '        OD LIMIT'.
           05  FILLER            PIC X(16) VALUE '   THRESHOLD/BUF'.
           05  FILLER            PIC X(14) VALUE '  CIF'.
           05  FILLER            PIC X(42) VALUE '  NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TYPE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CHANNEL                  PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-AVAIL                    PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-OD-LIMIT                 PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-THRESHOLD                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
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
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-ACCOUNT THRU 2000-EXIT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       CIFMAST-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                I-O    ALRTPREF-FILE
                OUTPUT ALERTOUT-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-ALRTPREF-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-ALERTOUT-STATUS NOT = '00'
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
           OR DTC-NEXT-BUS-DATE NOT NUMERIC
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           MOVE WS-BUS-DATE (1:6)          TO WS-BUS-MONTH
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA

      *---- GATEWAY HEADER --------------------------------------------*
           MOVE SPACES                     TO WS-ALERT-RECORD
           SET ALR-HEADER                  TO TRUE
           MOVE 'DDALERTS'                 TO ALRH-FILE-ID
           MOVE WS-BUS-DATE                TO ALRH-BUSINESS-DATE
           MOVE WS-CURR-DATE               TO ALRH-CREATE-DATE
           MOVE WS-CURR-TIME               TO ALRH-CREATE-TIME
      *    SAME SEQUENCE RULE AS IBFEED02 - THE GATEWAY REJECTS A FILE
      *    WHOSE SEQUENCE IS NOT HIGHER THAN THE LAST ONE LOADED.
           COMPUTE ALRH-FILE-SEQ-NO =
                   FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
                 - FUNCTION INTEGER-OF-DATE (20000101)
           MOVE 'MBKHOST '                 TO ALRH-SOURCE-SYS
           PERFORM 8300-WRITE-ALERT-REC

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           IF NOT ACM-CURRENT
           OR NOT ACM-OPEN
               PERFORM 8000-READ-ACCOUNT
               GO TO 2000-EXIT
           END-IF
           ADD 1                           TO WS-ACCT-CHECKED
           MOVE 'N'                        TO WS-UNARR-IND
                                              WS-LOWBAL-IND
                                              WS-SENT-TODAY-IND
           SET WS-ALP-UNCHANGED            TO TRUE
           MOVE SPACES                     TO WS-CIF-OK-IND

      *---- ARRANGED LIMIT AND UNARRANGED POSITION --------------------*
           MOVE ACM-OD-LIMIT               TO WS-OD-LIMIT
           IF ACM-OD-EXPIRY-DATE NOT = ZERO
           AND ACM-OD-EXPIRY-DATE < WS-BUS-DATE
               MOVE ZERO                   TO WS-OD-LIMIT
           END-IF
           COMPUTE WS-SPENDING-POWER = ACM-AVAIL-BAL + WS-OD-LIMIT
           IF WS-SPENDING-POWER < ZERO
               SET WS-IS-UNARRANGED        TO TRUE
               COMPUTE WS-EXCESS = ZERO - WS-SPENDING-POWER
               ADD 1                       TO WS-ACCT-UNARRANGED
               ADD WS-EXCESS               TO WS-UNARR-EXCESS-TOTAL
           ELSE
               MOVE ZERO                   TO WS-EXCESS
           END-IF

      *---- PREFERENCES -----------------------------------------------*
           PERFORM 2100-GET-PREFERENCES
           IF WS-ALP-NEW
           AND NOT WS-IS-UNARRANGED
      *        NOTHING TO SAY AND NOTHING TO REMEMBER
               PERFORM 8000-READ-ACCOUNT
               GO TO 2000-EXIT
           END-IF
           IF WS-ALP-FOUND
           AND ALP-LOWBAL-OPTED-IN
           AND ACM-AVAIL-BAL < ALP-LOWBAL-THRESHOLD
               SET WS-IS-BELOW-THRESHOLD   TO TRUE
               ADD 1                       TO WS-ACCT-BELOW
           END-IF
           IF ALP-ALERT-MONTH NOT = WS-BUS-MONTH
               MOVE WS-BUS-MONTH           TO ALP-ALERT-MONTH
               MOVE ZERO                   TO ALP-ALERT-COUNT-MTD
               SET WS-ALP-CHANGED          TO TRUE
           END-IF

           PERFORM 2200-OVERDRAFT-ALERTS THRU 2200-EXIT
           PERFORM 2300-LOW-BALANCE-ALERT THRU 2300-EXIT

      *---- REMEMBER TODAY'S STATE FOR TOMORROW -----------------------*
           IF ALP-UNARR-STATE NOT = WS-UNARR-IND
               MOVE WS-UNARR-IND           TO ALP-UNARR-STATE
               SET WS-ALP-CHANGED          TO TRUE
           END-IF
           IF ALP-LOWBAL-STATE NOT = WS-LOWBAL-IND
               MOVE WS-LOWBAL-IND          TO ALP-LOWBAL-STATE
               SET WS-ALP-CHANGED          TO TRUE
           END-IF
           IF WS-ALP-CHANGED OR WS-ALP-NEW
               PERFORM 2900-SAVE-PREFERENCES
           END-IF

           PERFORM 8000-READ-ACCOUNT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-GET-PREFERENCES.
      *================================================================*
           MOVE ACM-KEY                    TO ALP-KEY
           READ ALRTPREF-FILE
           EVALUATE TRUE
               WHEN WS-ALRTPREF-OK
                   SET WS-ALP-FOUND        TO TRUE
               WHEN WS-ALRTPREF-NOTFND
      *            DEFAULT PREFERENCES - AUTO-ENROLLED IN THE
      *            MANDATORY UNARRANGED OVERDRAFT ALERTS BY SMS
                   SET WS-ALP-NEW          TO TRUE
                   MOVE SPACES             TO ALP-PREF-RECORD
                   MOVE ACM-KEY            TO ALP-KEY
                   MOVE ACM-CIF-NO         TO ALP-CIF-NO
                   SET ALP-ACTIVE          TO TRUE
                   SET ALP-BY-SMS          TO TRUE
                   MOVE 'N'                TO ALP-UNARR-OPT-OUT
                                              ALP-LOWBAL-IND
                                              ALP-UNARR-STATE
                                              ALP-LOWBAL-STATE
                   MOVE ZERO               TO ALP-LOWBAL-THRESHOLD
                                              ALP-LAST-UNARR-DATE
                                              ALP-LAST-CHGWARN-DATE
                                              ALP-LAST-LOWBAL-DATE
                                              ALP-LAST-LOWBAL-BAL
                                              ALP-ALERT-COUNT-MTD
                   MOVE WS-BUS-MONTH       TO ALP-ALERT-MONTH
                   MOVE WS-BUS-DATE        TO ALP-CREATE-DATE
                   SET ALP-CREATED-BY-BATCH TO TRUE
               WHEN OTHER
                   MOVE '2100-GET-PREFERENCES' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ALRTPREF READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ALRTPREF-STATUS TO ABN-FILE-STATUS
                   MOVE ALP-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2200-OVERDRAFT-ALERTS.
      *================================================================*
           IF NOT WS-IS-UNARRANGED
               GO TO 2200-EXIT
           END-IF
           MOVE SPACES                     TO WS-ALERT-TYPE WS-NOTE

      *---- WOULD TOMORROW'S DAILY FEE BE CHARGED? --------------------*
           PERFORM 2210-GET-PRODUCT THRU 2210-EXIT
           IF WS-PRD-FOUND
           AND WS-PC-LEVIES-CHARGES = 'Y'
           AND WS-PC-DAILY-FEE > ZERO
           AND WS-EXCESS > WS-PC-BUFFER
           AND NOT ACM-CHARGES-WAIVED
               MOVE 'CHGW'                 TO WS-ALERT-TYPE
      *        NO MORE FEES ONCE THE PERIOD MAXIMUM DAYS OR THE
      *        MONTHLY CAP IS REACHED - NO WARNING EITHER.
               IF WS-PC-MAX-DAYS > ZERO
               AND ACM-UNARR-OD-DAYS NOT < WS-PC-MAX-DAYS
                   MOVE SPACES             TO WS-ALERT-TYPE
               END-IF
               COMPUTE WS-FEES-SO-FAR =
                       ACM-UNARR-OD-DAYS * WS-PC-DAILY-FEE
               IF WS-PC-MONTHLY-CAP > ZERO
               AND WS-FEES-SO-FAR NOT < WS-PC-MONTHLY-CAP
                   MOVE SPACES             TO WS-ALERT-TYPE
               END-IF
           END-IF

      *---- OTHERWISE, IS IT A NEW UNARRANGED EPISODE? ----------------*
           IF WS-ALERT-TYPE = SPACES
           AND NOT ALP-WAS-UNARRANGED
               MOVE 'UNAR'                 TO WS-ALERT-TYPE
           END-IF
           IF WS-ALERT-TYPE = SPACES
               ADD 1                       TO WS-SUPP-REPEAT
               GO TO 2200-EXIT
           END-IF

      *---- SUPPRESSIONS ----------------------------------------------*
           EVALUATE TRUE
               WHEN ALP-UNARR-OPTED-OUT
                   ADD 1                   TO WS-SUPP-OPTOUT
                   GO TO 2200-EXIT
               WHEN ALP-ALL-SUSPENDED
                   ADD 1                   TO WS-SUPP-OPTOUT
                   GO TO 2200-EXIT
               WHEN WS-ALERT-CHGW
               AND  ALP-LAST-CHGWARN-DATE = WS-BUS-DATE
                   ADD 1                   TO WS-SUPP-RERUN
                   GO TO 2200-EXIT
               WHEN WS-ALERT-UNAR
               AND  ALP-LAST-UNARR-DATE = WS-BUS-DATE
                   ADD 1                   TO WS-SUPP-RERUN
                   GO TO 2200-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           PERFORM 2400-CHECK-CUSTOMER
           IF WS-CIF-NO-ALERT
               GO TO 2200-EXIT
           END-IF

           IF WS-ALERT-CHGW
               MOVE WS-TPL-CHARGE-WARN     TO WS-TEMPLATE-ID
               MOVE WS-PC-BUFFER           TO WS-THRESHOLD-SHOWN
               MOVE 'UNARRANGED OD - FEE DUE UNLESS PAID IN'
                                           TO WS-NOTE
           ELSE
               MOVE WS-TPL-UNARRANGED      TO WS-TEMPLATE-ID
               MOVE ZERO                   TO WS-THRESHOLD-SHOWN
               MOVE 'WENT INTO UNARRANGED OVERDRAFT'
                                           TO WS-NOTE
           END-IF
           PERFORM 2500-SEND-ALERT THRU 2500-EXIT
           IF WS-SEND-CHANNEL NOT = SPACE
               SET WS-OD-ALERT-SENT        TO TRUE
               MOVE WS-BUS-DATE            TO ALP-LAST-UNARR-DATE
               IF WS-ALERT-CHGW
                   MOVE WS-BUS-DATE        TO ALP-LAST-CHGWARN-DATE
                   ADD 1                   TO WS-ALERTS-CHGW
               ELSE
                   ADD 1                   TO WS-ALERTS-UNAR
               END-IF
               SET WS-ALP-CHANGED          TO TRUE
           END-IF
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2210-GET-PRODUCT.
      *================================================================*
      *    ACCTMAST IS IN SORT CODE / ACCOUNT ORDER SO PRODUCTS ARE
      *    MIXED - BUT MOST CURRENT ACCOUNTS ARE ON A HANDFUL OF
      *    PRODUCTS, SO KEEPING THE LAST ONE SAVES MOST READS.
           IF ACM-PROD-CD = WS-LAST-PROD-CD
               GO TO 2210-EXIT
           END-IF
           MOVE ACM-PROD-CD                TO PRD-PROD-CD
                                              WS-LAST-PROD-CD
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN WS-PRDPARM-OK
                   SET WS-PRD-FOUND        TO TRUE
                   MOVE PRD-UNARR-OD-DAILY-FEE TO WS-PC-DAILY-FEE
                   MOVE PRD-UNARR-OD-BUFFER    TO WS-PC-BUFFER
                   MOVE PRD-UNARR-OD-MAX-DAYS  TO WS-PC-MAX-DAYS
                   MOVE PRD-MONTHLY-CHG-CAP    TO WS-PC-MONTHLY-CAP
                   MOVE PRD-CHG-IND        TO WS-PC-LEVIES-CHARGES
               WHEN WS-PRDPARM-NOTFND
                   MOVE 'N'                TO WS-PRD-FOUND-IND
                   DISPLAY 'DDALR010 W001 NO PRDPARM FOR PRODUCT '
                           ACM-PROD-CD ' - NO CHARGE WARNINGS'
               WHEN OTHER
                   MOVE '2210-GET-PRODUCT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PRDPARM READ FAILED' TO ABN-MESSAGE
                   MOVE WS-PRDPARM-STATUS  TO ABN-FILE-STATUS
                   MOVE PRD-PROD-CD        TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       2210-EXIT.
           EXIT.
      *================================================================*
       2300-LOW-BALANCE-ALERT.
      *================================================================*
           IF NOT WS-IS-BELOW-THRESHOLD
               GO TO 2300-EXIT
           END-IF
           MOVE SPACES                     TO WS-NOTE
           EVALUATE TRUE
               WHEN ALP-WAS-BELOW-THRESHOLD
                   ADD 1                   TO WS-SUPP-REPEAT
                   GO TO 2300-EXIT
               WHEN ALP-LAST-LOWBAL-DATE = WS-BUS-DATE
                   ADD 1                   TO WS-SUPP-RERUN
                   GO TO 2300-EXIT
               WHEN ALP-ALL-SUSPENDED
                   ADD 1                   TO WS-SUPP-OPTOUT
                   GO TO 2300-EXIT
      *        THE OVERDRAFT ALERT ALREADY TOLD THEM TODAY
               WHEN WS-OD-ALERT-SENT
                   ADD 1                   TO WS-SUPP-REPEAT
                   GO TO 2300-EXIT
               WHEN ALP-ALERT-COUNT-MTD NOT < WS-MAX-LOWB-PER-MONTH
                   ADD 1                   TO WS-SUPP-CAP
                   GO TO 2300-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-CIF-OK-IND = SPACE
               PERFORM 2400-CHECK-CUSTOMER
           END-IF
           IF WS-CIF-NO-ALERT
               GO TO 2300-EXIT
           END-IF

           MOVE 'LOWB'                     TO WS-ALERT-TYPE
           MOVE WS-TPL-LOW-BALANCE         TO WS-TEMPLATE-ID
           MOVE ALP-LOWBAL-THRESHOLD       TO WS-THRESHOLD-SHOWN
           MOVE 'BALANCE BELOW CUSTOMER THRESHOLD' TO WS-NOTE
           PERFORM 2500-SEND-ALERT THRU 2500-EXIT
           IF WS-SEND-CHANNEL NOT = SPACE
               MOVE WS-BUS-DATE            TO ALP-LAST-LOWBAL-DATE
               MOVE ACM-AVAIL-BAL          TO ALP-LAST-LOWBAL-BAL
               ADD 1                       TO WS-ALERTS-LOWB
               SET WS-ALP-CHANGED          TO TRUE
           END-IF
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2400-CHECK-CUSTOMER.
      *================================================================*
           SET WS-CIF-CAN-ALERT            TO TRUE
           MOVE ACM-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   IF CIF-DECEASED OR CIF-CLOSED OR CIF-BLOCKED
                       SET WS-CIF-NO-ALERT TO TRUE
                       ADD 1               TO WS-SUPP-CUSTOMER
                   END-IF
               WHEN WS-CIFMAST-NOTFND
                   SET WS-CIF-NO-ALERT     TO TRUE
                   ADD 1                   TO WS-NO-CONTACT
                   MOVE SPACES             TO CIF-CUSTOMER-RECORD
                   MOVE 'NOCF'             TO WS-ALERT-TYPE
                   MOVE 'CIF NOT FOUND - ALERT NOT SENT'
                                           TO WS-NOTE
                   MOVE ZERO               TO WS-THRESHOLD-SHOWN
                   MOVE SPACE              TO WS-SEND-CHANNEL
                   PERFORM 7100-REPORT-DETAIL
               WHEN OTHER
                   MOVE '2400-CHECK-CUSTOMER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE CIF-NO             TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2500-SEND-ALERT.
      *================================================================*
      *    CHOOSE THE CHANNEL, FALLING BACK IF THE CHOSEN ONE HAS NO
      *    CONTACT DETAIL. 'B' (BOTH) SENDS SMS AND EMAIL.
           MOVE SPACE                      TO WS-SEND-CHANNEL
           EVALUATE TRUE
               WHEN ALP-BY-BOTH
                   IF CIF-MOBILE-TEL NOT = SPACES
                       SET WS-SEND-SMS     TO TRUE
                       PERFORM 2550-WRITE-DETAIL
                   END-IF
                   IF CIF-EMAIL-ADDR NOT = SPACES
                       SET WS-SEND-EMAIL   TO TRUE
                       PERFORM 2550-WRITE-DETAIL
                   END-IF
               WHEN ALP-BY-EMAIL
                   IF CIF-EMAIL-ADDR NOT = SPACES
                       SET WS-SEND-EMAIL   TO TRUE
                   ELSE
                       IF CIF-MOBILE-TEL NOT = SPACES
                           SET WS-SEND-SMS TO TRUE
                       END-IF
                   END-IF
                   IF WS-SEND-CHANNEL NOT = SPACE
                       PERFORM 2550-WRITE-DETAIL
                   END-IF
               WHEN OTHER
                   IF CIF-MOBILE-TEL NOT = SPACES
                       SET WS-SEND-SMS     TO TRUE
                   ELSE
                       IF CIF-EMAIL-ADDR NOT = SPACES
                           SET WS-SEND-EMAIL TO TRUE
                       END-IF
                   END-IF
                   IF WS-SEND-CHANNEL NOT = SPACE
                       PERFORM 2550-WRITE-DETAIL
                   END-IF
           END-EVALUATE

           IF WS-SEND-CHANNEL = SPACE
               ADD 1                       TO WS-NO-CONTACT
               MOVE 'NO MOBILE OR EMAIL - CONTACT CENTRE'
                                           TO WS-NOTE
               PERFORM 7100-REPORT-DETAIL
               GO TO 2500-EXIT
           END-IF
           ADD 1                           TO ALP-ALERT-COUNT-MTD
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       2550-WRITE-DETAIL.
      *================================================================*
           ADD 1                           TO WS-ALERT-SEQ
           MOVE SPACES                     TO WS-ALERT-RECORD
           SET ALR-DETAIL                  TO TRUE
           STRING 'A' WS-BUS-DATE WS-ALERT-SEQ
                  DELIMITED BY SIZE
                  INTO ALRD-ALERT-ID
           END-STRING
           MOVE WS-ALERT-TYPE              TO ALRD-ALERT-TYPE
           MOVE WS-TEMPLATE-ID             TO ALRD-TEMPLATE-ID
           MOVE WS-SEND-CHANNEL            TO ALRD-CHANNEL
           IF WS-SEND-SMS
               MOVE CIF-MOBILE-TEL         TO ALRD-MOBILE-TEL
               ADD 1                       TO WS-ALERTS-SMS
           ELSE
               MOVE CIF-EMAIL-ADDR         TO ALRD-EMAIL-ADDR
               ADD 1                       TO WS-ALERTS-EMAIL
           END-IF
           MOVE ACM-CIF-NO                 TO ALRD-CIF-NO
           MOVE CIF-TITLE                  TO ALRD-TITLE
           MOVE CIF-LAST-NM                TO ALRD-LAST-NM
      *    ONLY THE LAST FOUR DIGITS GO OUTSIDE THE HOST (INFOSEC
      *    STANDARD IS-07 - NO FULL ACCOUNT NUMBERS IN SMS / EMAIL)
           MOVE ACM-SORT-CODE              TO ALRD-SORT-CODE
           MOVE ACM-ACCT-NO (5:4)          TO ALRD-ACCT-LAST4
           MOVE ACM-AVAIL-BAL              TO ALRD-AVAIL-BAL
           MOVE WS-OD-LIMIT                TO ALRD-OD-LIMIT
           MOVE WS-THRESHOLD-SHOWN         TO ALRD-THRESHOLD
           MOVE DTC-NEXT-BUS-DATE          TO ALRD-ACTION-BY-DATE
           PERFORM 8300-WRITE-ALERT-REC
           ADD 1                           TO WS-ALERTS-TOTAL
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2900-SAVE-PREFERENCES.
      *================================================================*
           MOVE WS-BUS-DATE                TO ALP-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO ALP-LAST-MAINT-USER
           IF WS-ALP-NEW
               WRITE ALP-PREF-RECORD
               ADD 1                       TO WS-PREFS-CREATED
           ELSE
               REWRITE ALP-PREF-RECORD
               ADD 1                       TO WS-PREFS-UPDATED
           END-IF
           IF NOT WS-ALRTPREF-OK
               MOVE '2900-SAVE-PREFERENCES' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ALRTPREF WRITE/REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-ALRTPREF-STATUS     TO ABN-FILE-STATUS
               MOVE ALP-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
      *---- GATEWAY TRAILER -------------------------------------------*
           MOVE SPACES                     TO WS-ALERT-RECORD
           SET ALR-TRAILER                 TO TRUE
           MOVE WS-ALERTS-TOTAL            TO ALRT-ALERT-COUNT
           MOVE WS-ALERTS-SMS              TO ALRT-SMS-COUNT
           MOVE WS-ALERTS-EMAIL            TO ALRT-EMAIL-COUNT
           MOVE WS-ALERTS-UNAR             TO ALRT-UNAR-COUNT
           MOVE WS-ALERTS-CHGW             TO ALRT-CHGW-COUNT
           MOVE WS-ALERTS-LOWB             TO ALRT-LOWB-COUNT
           PERFORM 8300-WRITE-ALERT-REC

      *    CROSS-CHECK: EVERY GATEWAY DETAIL IS EITHER SMS OR EMAIL
           IF WS-ALERTS-SMS + WS-ALERTS-EMAIL NOT = WS-ALERTS-TOTAL
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'ALERT CHANNEL COUNTS DO NOT MATCH TOTAL'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-ACCT-READ               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  OPEN CURRENT ACCOUNTS CHECKED' TO TL-LABEL
           MOVE WS-ACCT-CHECKED            TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  IN UNARRANGED OVERDRAFT (EXCESS)' TO TL-LABEL
           MOVE WS-ACCT-UNARRANGED         TO TL-COUNT
           MOVE WS-UNARR-EXCESS-TOTAL      TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  BELOW LOW-BALANCE THRESHOLD' TO TL-LABEL
           MOVE WS-ACCT-BELOW              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'ALERTS SENT - UNARRANGED OVERDRAFT' TO TL-LABEL
           MOVE WS-ALERTS-UNAR             TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'ALERTS SENT - CHARGE WARNING' TO TL-LABEL
           MOVE WS-ALERTS-CHGW             TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'ALERTS SENT - LOW BALANCE' TO TL-LABEL
           MOVE WS-ALERTS-LOWB             TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'GATEWAY MESSAGES - SMS'   TO TL-LABEL
           MOVE WS-ALERTS-SMS              TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'GATEWAY MESSAGES - EMAIL' TO TL-LABEL
           MOVE WS-ALERTS-EMAIL            TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'SUPPRESSED - REPEAT / ALREADY TOLD' TO TL-LABEL
           MOVE WS-SUPP-REPEAT             TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'SUPPRESSED - OPTED OUT / SUSPENDED' TO TL-LABEL
           MOVE WS-SUPP-OPTOUT             TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'SUPPRESSED - CUSTOMER STATUS' TO TL-LABEL
           MOVE WS-SUPP-CUSTOMER           TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'SUPPRESSED - MONTHLY CAP' TO TL-LABEL
           MOVE WS-SUPP-CAP                TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'SUPPRESSED - ALREADY SENT TODAY (RERUN)'
                                           TO TL-LABEL
           MOVE WS-SUPP-RERUN              TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'EXCEPTIONS - NO CONTACT DETAILS' TO TL-LABEL
           MOVE WS-NO-CONTACT              TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'PREFERENCE RECORDS CREATED' TO TL-LABEL
           MOVE WS-PREFS-CREATED           TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'PREFERENCE RECORDS UPDATED' TO TL-LABEL
           MOVE WS-PREFS-UPDATED           TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL

           CLOSE ACCTMAST-FILE ALRTPREF-FILE CIFMAST-FILE PRDPARM-FILE
                 ALERTOUT-FILE REPORT-FILE
           DISPLAY 'DDALR010 - ALERTS ' WS-ALERTS-TOTAL
                   ' NO CONTACT ' WS-NO-CONTACT
           IF WS-NO-CONTACT > ZERO
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
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE ACM-SORT-CODE              TO DL-SORT-CODE
           MOVE ACM-ACCT-NO                TO DL-ACCT-NO
           MOVE WS-ALERT-TYPE              TO DL-TYPE
           MOVE WS-SEND-CHANNEL            TO DL-CHANNEL
           MOVE ACM-AVAIL-BAL              TO DL-AVAIL
           MOVE WS-OD-LIMIT                TO DL-OD-LIMIT
           MOVE WS-THRESHOLD-SHOWN         TO DL-THRESHOLD
           MOVE ACM-CIF-NO                 TO DL-CIF-NO
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
       7200-WRITE-TOTAL.
      *================================================================*
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           .
      *================================================================*
       8000-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-ACCT-READ
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8300-WRITE-ALERT-REC.
      *================================================================*
           WRITE ALERTOUT-RECORD           FROM WS-ALERT-RECORD
           IF WS-ALERTOUT-STATUS NOT = '00'
               MOVE '8300-WRITE-ALERT-REC' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ALERTOUT WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ALERTOUT-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *
           COPY ABNDPROC.
