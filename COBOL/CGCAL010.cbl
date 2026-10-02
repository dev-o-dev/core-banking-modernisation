       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CGCAL010.
       AUTHOR.        G H THORNTON.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1995-04-03.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CGCAL010                                            *
      * TITLE    : ACCOUNT CHARGES - PERIOD CALCULATION                *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : CGCAL010                                            *
      * JCL      : CGCHG01 STEP CG020                                  *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (VSAM KSDS, SEQ LOAD) *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      *            MBK.TXNHIST  (SELECT) - TRANSACTION COUNTS, LOWEST  *
      *                                    BALANCE IN THE PERIOD       *
      *            MBK.PAYINSTR (SELECT) - SWIFT / CHAPS PAYMENTS      *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ I-O)  *
      *            - RESETS THE CHARGING PERIOD COUNTERS               *
      * OUTPUT   : CHGPEND  - PENDING CHARGES (VSAM KSDS, RANDOM)      *
      *                       CGCHGREC                                 *
      *            REPORT   - CHARGES CALCULATED (FBA 133)             *
      *----------------------------------------------------------------*
      * AN ACCOUNT'S CHARGING PERIOD ENDS ON THE PRODUCT CHARGE CYCLE  *
      * DAY (PRD-CHG-CYCLE-DAY, CAPPED AT THE MONTH END). WHEN THE     *
      * PERIOD END FALLS BETWEEN TODAY AND THE NEXT BUSINESS DATE THE  *
      * CHARGES FOR THE PERIOD ARE CALCULATED AND WRITTEN AS PENDING.  *
      *                                                                *
      * THEY ARE NOT DEBITED HERE. UNDER THE PAYMENT SERVICES          *
      * REGULATIONS 2009 / BANKING CODE THE CUSTOMER MUST BE TOLD OF   *
      * CHARGES PRD-CHG-NOTIFY-DAYS (NORMALLY 14) BEFORE THEY ARE      *
      * TAKEN. CGNTF010 PRODUCES THE NOTIFICATIONS FROM CHGPEND AND    *
      * CGAPL010 DEBITS THEM ON CGP-APPLY-DATE.                        *
      *                                                                *
      * CHARGES CALCULATED                                             *
      *   MFEE  MONTHLY FEE - NOT CHARGED IF THE LOWEST END-OF-DAY     *
      *         BALANCE IN THE PERIOD WAS AT LEAST PRD-FREE-BANKING-BAL*
      *   AODF  ARRANGED OVERDRAFT FEE - IF THE ACCOUNT HAS A LIMIT    *
      *         AND WAS OVERDRAWN AT ANY END OF DAY IN THE PERIOD      *
      *   UODF  UNARRANGED OVERDRAFT DAILY FEE x ACM-UNARR-OD-DAYS,    *
      *         LIMITED TO PRD-UNARR-OD-MAX-DAYS                       *
      *   UNPD  UNPAID ITEM FEE x ACM-UNPAID-ITEMS                     *
      *   PDIT  PAID ITEM FEE x ACM-PAID-ITEMS                         *
      *   TXNF  PER-TRANSACTION FEES (PRD-TXN-FEE-TABLE) FOR ITEMS     *
      *         ABOVE THE FREE ALLOWANCE                               *
      *   SWFT  SWIFT PAYMENT FEE PER PAYMENT SENT IN THE PERIOD       *
      *   CHAP  CHAPS PAYMENT FEE PER PAYMENT SENT IN THE PERIOD       *
      *                                                                *
      * MONTHLY CAP: UODF + UNPD + PDIT TOGETHER MAY NOT EXCEED        *
      * PRD-MONTHLY-CHG-CAP (OFT 2004 UNDERTAKING). THE REDUCTION IS   *
      * TAKEN FROM UODF FIRST, THEN PDIT, THEN UNPD.                   *
      *                                                                *
      * AN ACCOUNT FLAGGED ACM-CHARGES-WAIVED (STAFF, VULNERABLE       *
      * CUSTOMER, COMPLAINT SETTLEMENT) HAS ITS CHARGES CALCULATED AND *
      * WRITTEN WITH STATUS 'S' (SUPPRESSED) SO THE VALUE FOREGONE IS  *
      * VISIBLE TO MANAGEMENT INFORMATION.                             *
      *----------------------------------------------------------------*
      * RERUN: A CHGPEND RECORD THAT ALREADY EXISTS (DUPLICATE KEY)    *
      * MEANS THE ACCOUNT WAS CHARGED BY AN EARLIER RUN. THE ACCOUNT   *
      * IS SKIPPED. THE PERIOD COUNTERS ARE ONLY RESET WHEN CHARGES    *
      * HAVE BEEN WRITTEN, SO A RERUN IS SAFE.                         *
      *----------------------------------------------------------------*
      * RETURN CODES  0 OK   4 EXCEPTIONS                              *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1995-04-03 GHT  ORIGINAL VERSION (REPLACES DD0700 CHARGES)     *
      * 1998-11-30 MLW  Y2K                                            *
      * 2001-07-16 DKP  MINIMUM BALANCE FROM TXNHIST (DB2) INSTEAD OF  *
      *                 THE DD0700 DAILY BALANCE FILE                  *
      * 2004-08-09 SAT  UNARRANGED BUFFER, MAX DAYS, MONTHLY CAP (OFT) *
      * 2006-11-20 JNB  SWIFT AND CHAPS FEES FROM PAYINSTR             *
      * 2009-10-01 AOS  PSR 2009 - CHARGES NO LONGER DEBITED HERE.     *
      *                 WRITTEN TO CHGPEND FOR NOTIFICATION (CGNTF010) *
      *                 AND LATER APPLICATION (CGAPL010)               *
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
           SELECT CHGPEND-FILE     ASSIGN TO CHGPEND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CGP-KEY
                                   FILE STATUS IS WS-CHGPEND-STATUS.
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
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  CHGPEND-FILE.
           COPY CGCHGREC.
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CGCAL010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-CHGPEND-STATUS           PIC X(02).
               88  WS-CHGPEND-OK               VALUE '00'.
               88  WS-CHGPEND-DUPKEY           VALUE '22'.
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-PROD-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-PRODUCTS          VALUE 'Y'.
           05  WS-PROD-FOUND-IND           PIC X(01) VALUE 'N'.
               88  WS-PROD-FOUND               VALUE 'Y'.
           05  WS-DUE-IND                  PIC X(01) VALUE 'N'.
               88  WS-PERIOD-ENDS              VALUE 'Y'.
           05  WS-ALREADY-IND              PIC X(01) VALUE 'N'.
               88  WS-ALREADY-CHARGED          VALUE 'Y'.
           05  WS-OVERDRAWN-IND            PIC X(01) VALUE 'N'.
               88  WS-WAS-OVERDRAWN            VALUE 'Y'.
      *
      *---- DATES -----------------------------------------------------*
       01  WS-DATE-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-R REDEFINES WS-BUS-DATE.
               10  WS-BD-CCYY              PIC 9(04).
               10  WS-BD-MM                PIC 9(02).
               10  WS-BD-DD                PIC 9(02).
           05  WS-NEXT-BUS-DATE            PIC 9(08).
           05  WS-PERIOD-END               PIC 9(08).
           05  WS-PERIOD-END-R REDEFINES WS-PERIOD-END.
               10  WS-PE-CCYY              PIC 9(04).
               10  WS-PE-MM                PIC 9(02).
               10  WS-PE-DD                PIC 9(02).
           05  WS-PERIOD-START             PIC 9(08).
           05  WS-NEXT-PERIOD-START        PIC 9(08).
           05  WS-APPLY-DATE               PIC 9(08).
           05  WS-CYCLE-DAY                PIC 9(02).
           05  WS-DAYS-IN-MONTH            PIC 9(02).
           05  WS-LEAP-REM-4               PIC S9(04) COMP.
           05  WS-LEAP-REM-100             PIC S9(04) COMP.
           05  WS-LEAP-REM-400             PIC S9(04) COMP.
           05  WS-LEAP-QUOT                PIC S9(06) COMP.
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DB2                 PIC X(10).
           05  WS-FROM-DB2                 PIC X(10).
           05  WS-TO-DB2                   PIC X(10).
           05  WS-LOOP-GUARD               PIC S9(04) COMP.
      *
       01  WS-MONTH-DAYS-VALUES.
           05  FILLER                      PIC X(24)
               VALUE '312831303130313130313031'.
       01  WS-MONTH-DAYS-TABLE REDEFINES WS-MONTH-DAYS-VALUES.
           05  WS-MONTH-DAYS               PIC 9(02) OCCURS 12 TIMES.
      *
      *---- CHARGE WORK AREA - ONE ENTRY PER CHARGE TYPE --------------*
       01  WS-CHARGE-WORK.
           05  WS-CHG-COUNT                PIC S9(04) COMP VALUE ZERO.
           05  WS-CHG-ENTRY                OCCURS 20 TIMES
                                           INDEXED BY CW-IDX.
               10  WS-CW-TYPE              PIC X(04).
               10  WS-CW-ITEMS             PIC 9(05).
               10  WS-CW-UNIT              PIC S9(07)V99 COMP-3.
               10  WS-CW-GROSS             PIC S9(13)V99 COMP-3.
               10  WS-CW-CAP-RED           PIC S9(13)V99 COMP-3.
               10  WS-CW-NET               PIC S9(13)V99 COMP-3.
               10  WS-CW-DESC              PIC X(35).
      *
       01  WS-CALC-FIELDS.
           05  WS-MIN-BAL                  PIC S9(13)V99 COMP-3.
           05  WS-MIN-BAL-IND              PIC S9(04) COMP.
           05  WS-TXN-COUNT                PIC S9(09) COMP.
           05  WS-PAY-COUNT                PIC S9(09) COMP.
           05  WS-CAPPED-TOTAL             PIC S9(13)V99 COMP-3.
           05  WS-EXCESS                   PIC S9(13)V99 COMP-3.
           05  WS-REDUCE                   PIC S9(13)V99 COMP-3.
           05  WS-UNARR-DAYS               PIC 9(03).
           05  WS-CHARGEABLE               PIC S9(05) COMP-3.
           05  WS-ACCOUNT-TOTAL            PIC S9(13)V99 COMP-3.
           05  WS-ITEM-SEQ                 PIC 9(06).
           05  WS-CAP-ORDER                PIC X(12)
                                           VALUE 'UODFPDITUNPD'.
           05  WS-CAP-TYPE                 PIC X(04).
           05  WS-CAP-SUB                  PIC S9(04) COMP.
           05  WS-TFEE-SUB                 PIC S9(04) COMP.
           05  WS-TFEE-CODE                PIC X(04).
           05  WS-AMOUNT-EDIT              PIC ZZ,ZZ9.99.
           05  WS-COUNT-EDIT               PIC ZZ9.
      *
      *---- PRODUCT TABLE ---------------------------------------------*
       01  WS-PRODUCT-TABLE.
           05  WS-PROD-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-PROD-MAX                 PIC S9(04) COMP VALUE +300.
           05  WS-PROD-ENTRY               OCCURS 1 TO 300 TIMES
                                           DEPENDING ON WS-PROD-COUNT
                                           ASCENDING KEY WS-PT-PROD-CD
                                           INDEXED BY PT-IDX.
               10  WS-PT-PROD-CD           PIC X(10).
               10  WS-PT-CHG-IND           PIC X(01).
               10  WS-PT-CYCLE-DAY         PIC 9(02).
               10  WS-PT-NOTIFY-DAYS       PIC 9(03).
               10  WS-PT-MONTHLY-FEE       PIC S9(13)V99 COMP-3.
               10  WS-PT-FREE-BANK-BAL     PIC S9(13)V99 COMP-3.
               10  WS-PT-ARR-OD-FEE        PIC S9(13)V99 COMP-3.
               10  WS-PT-UNARR-DAY-FEE     PIC S9(13)V99 COMP-3.
               10  WS-PT-UNARR-MAX-DAYS    PIC 9(03).
               10  WS-PT-UNPAID-FEE        PIC S9(13)V99 COMP-3.
               10  WS-PT-PAID-FEE          PIC S9(13)V99 COMP-3.
               10  WS-PT-MONTHLY-CAP       PIC S9(13)V99 COMP-3.
               10  WS-PT-TFEE              OCCURS 6 TIMES.
                   15  WS-PT-TFEE-CODE     PIC X(04).
                   15  WS-PT-TFEE-FREE     PIC 9(03).
                   15  WS-PT-TFEE-AMT      PIC S9(07)V99 COMP-3.
               10  WS-PT-SWIFT-FEE         PIC S9(07)V99 COMP-3.
               10  WS-PT-CHAPS-FEE         PIC S9(07)V99 COMP-3.
               10  WS-PT-TOTAL-CHARGED     PIC S9(13)V99 COMP-3.
               10  WS-PT-TOTAL-SUPPRESSED  PIC S9(13)V99 COMP-3.
               10  WS-PT-ACCTS-CHARGED     PIC S9(07) COMP-3.
      *
      *---- COUNTERS --------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-DUE-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CHARGED-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-NIL-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUPPRESSED-COUNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-RERUN-SKIP-COUNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-PRODUCT-COUNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-ITEMS-WRITTEN            PIC S9(09) COMP-3 VALUE 0.
           05  WS-CAPPED-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXCEPTION-COUNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-GROSS              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-CAP-RED            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-NET                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-SUPPRESSED         PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
      *---- REPORT ----------------------------------------------------*
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
      *
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CGCAL010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - ACCOUNT CHARGES CALCULATED'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(11) VALUE 'PRODUCT'.
           05  FILLER            PIC X(06) VALUE 'TYPE'.
           05  FILLER            PIC X(07) VALUE ' ITEMS'.
           05  FILLER            PIC X(11) VALUE '      UNIT'.
           05  FILLER            PIC X(14) VALUE '         GROSS'.
           05  FILLER            PIC X(14) VALUE '       CAP RED'.
           05  FILLER            PIC X(14) VALUE '           NET'.
           05  FILLER            PIC X(03) VALUE SPACES.
           05  FILLER            PIC X(11) VALUE 'APPLY DATE'.
           05  FILLER            PIC X(02) VALUE 'ST'.
           05  FILLER            PIC X(20) VALUE SPACES.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
      *
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TYPE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ITEMS                    PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-UNIT                     PIC ZZZ,ZZ9.99.
           05  DL-GROSS                    PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CAP-RED                  PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NET                      PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  DL-APPLY-DATE               PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-STATUS                   PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTE                     PIC X(20).
      *
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
       01  WS-HV-SORT-CODE                 PIC X(06).
       01  WS-HV-ACCT-NO                   PIC X(08).
       01  WS-HV-TXN-CODE                  PIC X(04).
       01  WS-HV-SCHEME                    PIC X(04).
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
                       CHGPEND-FILE
                OUTPUT REPORT-FILE

           IF NOT WS-PRDPARM-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CHGPEND-OK
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-PRDPARM-STATUS ' ' WS-ACCTMAST-STATUS ' '
                      WS-CHGPEND-STATUS ' ' WS-REPORT-STATUS ' '
                      WS-DATECARD-STATUS
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
           DISPLAY 'CGCAL010 - BUSINESS DATE ' WS-DATE-DB2
                   ' RUN ID ' DTC-RUN-ID

           PERFORM 1300-LOAD-PRODUCTS
           CLOSE PRDPARM-FILE

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       1300-LOAD-PRODUCTS.
      *================================================================*
           MOVE ZERO                       TO WS-PROD-COUNT
           PERFORM 1310-READ-PRODUCT
           PERFORM UNTIL WS-END-OF-PRODUCTS
               IF WS-PROD-COUNT >= WS-PROD-MAX
                   MOVE '1300-LOAD-PRODUCTS' TO ABN-PARAGRAPH
                   MOVE 1004               TO ABN-ABEND-CODE
                   MOVE 'PRODUCT TABLE FULL - INCREASE WS-PROD-MAX'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               END-IF
               ADD 1                       TO WS-PROD-COUNT
               SET PT-IDX                  TO WS-PROD-COUNT
               PERFORM 1320-STORE-PRODUCT
               PERFORM 1310-READ-PRODUCT
           END-PERFORM
           DISPLAY 'CGCAL010 - PRODUCTS LOADED ' WS-PROD-COUNT
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
           MOVE PRD-PROD-CD             TO WS-PT-PROD-CD (PT-IDX)
           MOVE PRD-CHG-IND             TO WS-PT-CHG-IND (PT-IDX)
           MOVE PRD-CHG-CYCLE-DAY       TO WS-PT-CYCLE-DAY (PT-IDX)
           MOVE PRD-CHG-NOTIFY-DAYS     TO WS-PT-NOTIFY-DAYS (PT-IDX)
           MOVE PRD-MONTHLY-FEE         TO WS-PT-MONTHLY-FEE (PT-IDX)
           MOVE PRD-FREE-BANKING-BAL    TO WS-PT-FREE-BANK-BAL (PT-IDX)
           MOVE PRD-ARR-OD-MONTHLY-FEE  TO WS-PT-ARR-OD-FEE (PT-IDX)
           MOVE PRD-UNARR-OD-DAILY-FEE  TO WS-PT-UNARR-DAY-FEE (PT-IDX)
           MOVE PRD-UNARR-OD-MAX-DAYS   TO WS-PT-UNARR-MAX-DAYS (PT-IDX)
           MOVE PRD-UNPAID-ITEM-FEE     TO WS-PT-UNPAID-FEE (PT-IDX)
           MOVE PRD-PAID-ITEM-FEE       TO WS-PT-PAID-FEE (PT-IDX)
           MOVE PRD-MONTHLY-CHG-CAP     TO WS-PT-MONTHLY-CAP (PT-IDX)
           PERFORM VARYING WS-TFEE-SUB FROM 1 BY 1
                   UNTIL WS-TFEE-SUB > 6
               MOVE PRD-TFEE-TXN-CODE (WS-TFEE-SUB)
                     TO WS-PT-TFEE-CODE (PT-IDX, WS-TFEE-SUB)
               MOVE PRD-TFEE-FREE-COUNT (WS-TFEE-SUB)
                     TO WS-PT-TFEE-FREE (PT-IDX, WS-TFEE-SUB)
               MOVE PRD-TFEE-AMOUNT (WS-TFEE-SUB)
                     TO WS-PT-TFEE-AMT (PT-IDX, WS-TFEE-SUB)
           END-PERFORM
           MOVE PRD-SWIFT-FEE           TO WS-PT-SWIFT-FEE (PT-IDX)
           MOVE PRD-CHAPS-FEE           TO WS-PT-CHAPS-FEE (PT-IDX)
           MOVE ZERO TO WS-PT-TOTAL-CHARGED (PT-IDX)
                        WS-PT-TOTAL-SUPPRESSED (PT-IDX)
                        WS-PT-ACCTS-CHARGED (PT-IDX)

      *    NOTIFICATION PERIOD DEFAULTS TO 14 DAYS (PSR 2009 MINIMUM
      *    AGREED WITH COMPLIANCE) IF PRODUCT CONTROL LEFT IT BLANK.
           IF WS-PT-NOTIFY-DAYS (PT-IDX) NOT NUMERIC
           OR WS-PT-NOTIFY-DAYS (PT-IDX) < 14
               MOVE 14                  TO WS-PT-NOTIFY-DAYS (PT-IDX)
           END-IF
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           MOVE 'N'                        TO WS-DUE-IND
                                              WS-ALREADY-IND

           IF ACM-OPEN OR ACM-DORMANT OR ACM-FROZEN
               PERFORM 2050-FIND-PRODUCT
               IF WS-PROD-FOUND
               AND WS-PT-CHG-IND (PT-IDX) = 'Y'
                   PERFORM 2100-CHECK-PERIOD-END
                   IF WS-PERIOD-ENDS
                       ADD 1               TO WS-DUE-COUNT
                       PERFORM 2200-CALCULATE-CHARGES
                       PERFORM 2500-APPLY-MONTHLY-CAP THRU 2500-EXIT
                       PERFORM 2600-WRITE-PENDING THRU 2600-EXIT
                       IF NOT WS-ALREADY-CHARGED
                           PERFORM 2800-RESET-PERIOD
                       END-IF
                   END-IF
               END-IF
           END-IF

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       2050-FIND-PRODUCT.
      *================================================================*
           MOVE 'N'                        TO WS-PROD-FOUND-IND
           SEARCH ALL WS-PROD-ENTRY
               AT END
                   ADD 1                   TO WS-NO-PRODUCT-COUNT
               WHEN WS-PT-PROD-CD (PT-IDX) = ACM-PROD-CD
                   SET WS-PROD-FOUND       TO TRUE
           END-SEARCH
           .
      *================================================================*
       2100-CHECK-PERIOD-END.
      *================================================================*
      *    PERIOD END THIS MONTH, OR NEXT MONTH IF THE WINDOW CROSSES A
      *    MONTH END.
           MOVE WS-PT-CYCLE-DAY (PT-IDX)   TO WS-CYCLE-DAY
           MOVE WS-BD-CCYY                 TO WS-PE-CCYY
           MOVE WS-BD-MM                   TO WS-PE-MM
           PERFORM 2110-SET-CYCLE-DAY
           IF WS-PERIOD-END >= WS-BUS-DATE
           AND WS-PERIOD-END < WS-NEXT-BUS-DATE
               SET WS-PERIOD-ENDS          TO TRUE
           ELSE
               IF WS-BD-MM = 12
                   COMPUTE WS-PE-CCYY = WS-BD-CCYY + 1
                   MOVE 1                  TO WS-PE-MM
               ELSE
                   MOVE WS-BD-CCYY         TO WS-PE-CCYY
                   COMPUTE WS-PE-MM = WS-BD-MM + 1
               END-IF
               PERFORM 2110-SET-CYCLE-DAY
               IF WS-PERIOD-END >= WS-BUS-DATE
               AND WS-PERIOD-END < WS-NEXT-BUS-DATE
                   SET WS-PERIOD-ENDS      TO TRUE
               END-IF
           END-IF

           IF WS-PERIOD-ENDS
      *        PERIOD START - FROM THE ACCOUNT, OR THE OPEN DATE FOR A
      *        NEW ACCOUNT, OR ONE MONTH BACK AS A LAST RESORT.
               EVALUATE TRUE
                   WHEN ACM-CHG-PERIOD-START NUMERIC
                   AND  ACM-CHG-PERIOD-START NOT = ZERO
                       MOVE ACM-CHG-PERIOD-START TO WS-PERIOD-START
                   WHEN ACM-OPEN-DATE > ZERO
                       MOVE ACM-OPEN-DATE  TO WS-PERIOD-START
                   WHEN OTHER
                       MOVE 'D'            TO DTE-FUNCTION
                       MOVE WS-PERIOD-END  TO DTE-DATE-IN
                       MOVE -30            TO DTE-DAYS
                       CALL 'DTEUTIL' USING DTE-PARM-BLOCK
                       MOVE DTE-DATE-OUT   TO WS-PERIOD-START
               END-EVALUATE
               MOVE 'D'                    TO DTE-FUNCTION
               MOVE WS-PERIOD-END          TO DTE-DATE-IN
               MOVE 1                      TO DTE-DAYS
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               MOVE DTE-DATE-OUT           TO WS-NEXT-PERIOD-START
           END-IF
           .
      *================================================================*
       2110-SET-CYCLE-DAY.
      *================================================================*
           MOVE WS-MONTH-DAYS (WS-PE-MM)   TO WS-DAYS-IN-MONTH
           IF WS-PE-MM = 2
               DIVIDE WS-PE-CCYY BY 4   GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-4
               DIVIDE WS-PE-CCYY BY 100 GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-100
               DIVIDE WS-PE-CCYY BY 400 GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-400
               IF (WS-LEAP-REM-4 = 0 AND WS-LEAP-REM-100 NOT = 0)
               OR WS-LEAP-REM-400 = 0
                   MOVE 29                 TO WS-DAYS-IN-MONTH
               END-IF
           END-IF
           IF WS-CYCLE-DAY = ZERO
           OR WS-CYCLE-DAY > WS-DAYS-IN-MONTH
               MOVE WS-DAYS-IN-MONTH       TO WS-PE-DD
           ELSE
               MOVE WS-CYCLE-DAY           TO WS-PE-DD
           END-IF
           .
      *================================================================*
       2200-CALCULATE-CHARGES.
      *================================================================*
           MOVE ZERO                       TO WS-CHG-COUNT
           MOVE ACM-SORT-CODE              TO WS-HV-SORT-CODE
           MOVE ACM-ACCT-NO                TO WS-HV-ACCT-NO
           MOVE WS-PERIOD-START            TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO WS-FROM-DB2
           MOVE WS-PERIOD-END              TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO WS-TO-DB2

           PERFORM 2210-LOWEST-BALANCE
           PERFORM 2220-MONTHLY-FEE
           PERFORM 2230-ARRANGED-OD-FEE
           PERFORM 2240-UNARRANGED-OD-FEE
           PERFORM 2250-ITEM-FEES
           PERFORM 2260-TRANSACTION-FEES
           PERFORM 2270-PAYMENT-FEES
           .
      *================================================================*
       2210-LOWEST-BALANCE.
      *================================================================*
      *    LOWEST RUNNING BALANCE IN THE PERIOD. IF THERE WERE NO
      *    TRANSACTIONS THE BALANCE NEVER MOVED - USE THE LEDGER.
           EXEC SQL
               SELECT MIN(RUNNING_BAL)
                 INTO :WS-MIN-BAL :WS-MIN-BAL-IND
                 FROM MBK.TXNHIST
                WHERE SORT_CODE = :WS-HV-SORT-CODE
                  AND ACCT_NO   = :WS-HV-ACCT-NO
                  AND POST_DATE BETWEEN :WS-FROM-DB2 AND :WS-TO-DB2
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE '2210-LOWEST-BALANCE'  TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           IF WS-MIN-BAL-IND < 0
               MOVE ACM-LEDGER-BAL         TO WS-MIN-BAL
           ELSE
      *        THE LEDGER TODAY MAY BE LOWER THAN ANY RUNNING BALANCE
      *        (E.G. INTEREST POSTED LAST NIGHT AT THE END OF A DAY).
               IF ACM-LEDGER-BAL < WS-MIN-BAL
                   MOVE ACM-LEDGER-BAL     TO WS-MIN-BAL
               END-IF
           END-IF

           IF WS-MIN-BAL < ZERO
               SET WS-WAS-OVERDRAWN        TO TRUE
           ELSE
               MOVE 'N'                    TO WS-OVERDRAWN-IND
           END-IF
           .
      *================================================================*
       2220-MONTHLY-FEE.
      *================================================================*
           IF WS-PT-MONTHLY-FEE (PT-IDX) > ZERO
               IF WS-PT-FREE-BANK-BAL (PT-IDX) > ZERO
               AND WS-MIN-BAL >= WS-PT-FREE-BANK-BAL (PT-IDX)
                   CONTINUE
               ELSE
                   PERFORM 2290-NEW-CHARGE
                   MOVE 'MFEE'             TO WS-CW-TYPE (CW-IDX)
                   MOVE 1                  TO WS-CW-ITEMS (CW-IDX)
                   MOVE WS-PT-MONTHLY-FEE (PT-IDX)
                                           TO WS-CW-UNIT (CW-IDX)
                   MOVE 'MONTHLY ACCOUNT FEE'
                                           TO WS-CW-DESC (CW-IDX)
                   PERFORM 2295-EXTEND-CHARGE
               END-IF
           END-IF
           .
      *================================================================*
       2230-ARRANGED-OD-FEE.
      *================================================================*
           IF WS-PT-ARR-OD-FEE (PT-IDX) > ZERO
           AND ACM-OD-LIMIT > ZERO
           AND WS-WAS-OVERDRAWN
               PERFORM 2290-NEW-CHARGE
               MOVE 'AODF'                 TO WS-CW-TYPE (CW-IDX)
               MOVE 1                      TO WS-CW-ITEMS (CW-IDX)
               MOVE WS-PT-ARR-OD-FEE (PT-IDX)
                                           TO WS-CW-UNIT (CW-IDX)
               MOVE 'ARRANGED OVERDRAFT FEE'
                                           TO WS-CW-DESC (CW-IDX)
               PERFORM 2295-EXTEND-CHARGE
           END-IF
           .
      *================================================================*
       2240-UNARRANGED-OD-FEE.
      *================================================================*
           IF ACM-UNARR-OD-DAYS NUMERIC
               MOVE ACM-UNARR-OD-DAYS      TO WS-UNARR-DAYS
           ELSE
               MOVE ZERO                   TO WS-UNARR-DAYS
           END-IF

           IF WS-PT-UNARR-MAX-DAYS (PT-IDX) > ZERO
           AND WS-UNARR-DAYS > WS-PT-UNARR-MAX-DAYS (PT-IDX)
               MOVE WS-PT-UNARR-MAX-DAYS (PT-IDX) TO WS-UNARR-DAYS
           END-IF

           IF WS-UNARR-DAYS > ZERO
           AND WS-PT-UNARR-DAY-FEE (PT-IDX) > ZERO
               PERFORM 2290-NEW-CHARGE
               MOVE 'UODF'                 TO WS-CW-TYPE (CW-IDX)
               MOVE WS-UNARR-DAYS          TO WS-CW-ITEMS (CW-IDX)
               MOVE WS-PT-UNARR-DAY-FEE (PT-IDX)
                                           TO WS-CW-UNIT (CW-IDX)
               MOVE WS-UNARR-DAYS          TO WS-COUNT-EDIT
               STRING 'UNARRANGED OVERDRAFT ' WS-COUNT-EDIT ' DAYS'
                      DELIMITED BY SIZE
                      INTO WS-CW-DESC (CW-IDX)
               END-STRING
               PERFORM 2295-EXTEND-CHARGE
           END-IF
           .
      *================================================================*
       2250-ITEM-FEES.
      *================================================================*
           IF ACM-UNPAID-ITEMS NUMERIC
           AND ACM-UNPAID-ITEMS > ZERO
           AND WS-PT-UNPAID-FEE (PT-IDX) > ZERO
               PERFORM 2290-NEW-CHARGE
               MOVE 'UNPD'                 TO WS-CW-TYPE (CW-IDX)
               MOVE ACM-UNPAID-ITEMS       TO WS-CW-ITEMS (CW-IDX)
               MOVE WS-PT-UNPAID-FEE (PT-IDX)
                                           TO WS-CW-UNIT (CW-IDX)
               MOVE 'UNPAID ITEM FEES'     TO WS-CW-DESC (CW-IDX)
               PERFORM 2295-EXTEND-CHARGE
           END-IF

           IF ACM-PAID-ITEMS NUMERIC
           AND ACM-PAID-ITEMS > ZERO
           AND WS-PT-PAID-FEE (PT-IDX) > ZERO
               PERFORM 2290-NEW-CHARGE
               MOVE 'PDIT'                 TO WS-CW-TYPE (CW-IDX)
               MOVE ACM-PAID-ITEMS         TO WS-CW-ITEMS (CW-IDX)
               MOVE WS-PT-PAID-FEE (PT-IDX)
                                           TO WS-CW-UNIT (CW-IDX)
               MOVE 'PAID ITEM FEES (UNARRANGED)'
                                           TO WS-CW-DESC (CW-IDX)
               PERFORM 2295-EXTEND-CHARGE
           END-IF
           .
      *================================================================*
       2260-TRANSACTION-FEES.
      *================================================================*
      *    BUSINESS TARIFFS CHARGE PER ITEM ABOVE A FREE ALLOWANCE.
      *    ONE TXNF CHARGE RECORD PER TXN CODE IN THE TABLE.
           PERFORM VARYING WS-TFEE-SUB FROM 1 BY 1
                   UNTIL WS-TFEE-SUB > 6
               MOVE WS-PT-TFEE-CODE (PT-IDX, WS-TFEE-SUB)
                                           TO WS-TFEE-CODE
               IF WS-TFEE-CODE NOT = SPACES
               AND WS-PT-TFEE-AMT (PT-IDX, WS-TFEE-SUB) > ZERO
                   MOVE WS-TFEE-CODE       TO WS-HV-TXN-CODE
                   EXEC SQL
                       SELECT COUNT(*)
                         INTO :WS-TXN-COUNT
                         FROM MBK.TXNHIST
                        WHERE SORT_CODE = :WS-HV-SORT-CODE
                          AND ACCT_NO   = :WS-HV-ACCT-NO
                          AND POST_DATE BETWEEN :WS-FROM-DB2
                                            AND :WS-TO-DB2
                          AND TXN_CODE  = :WS-HV-TXN-CODE
                   END-EXEC
                   IF SQLCODE NOT = 0
                       MOVE '2260-TRANSACTION-FEES' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
                   END-IF

                   COMPUTE WS-CHARGEABLE = WS-TXN-COUNT
                         - WS-PT-TFEE-FREE (PT-IDX, WS-TFEE-SUB)
                   IF WS-CHARGEABLE > ZERO
                       PERFORM 2290-NEW-CHARGE
                       MOVE 'TXNF'         TO WS-CW-TYPE (CW-IDX)
                       MOVE WS-CHARGEABLE  TO WS-CW-ITEMS (CW-IDX)
                       MOVE WS-PT-TFEE-AMT (PT-IDX, WS-TFEE-SUB)
                                           TO WS-CW-UNIT (CW-IDX)
                       STRING 'TRANSACTION FEES - ' WS-TFEE-CODE
                              DELIMITED BY SIZE
                              INTO WS-CW-DESC (CW-IDX)
                       END-STRING
                       PERFORM 2295-EXTEND-CHARGE
                   END-IF
               END-IF
           END-PERFORM
           .
      *================================================================*
       2270-PAYMENT-FEES.
      *================================================================*
           IF WS-PT-SWIFT-FEE (PT-IDX) > ZERO
               MOVE 'SWFT'                 TO WS-HV-SCHEME
               PERFORM 2275-COUNT-PAYMENTS
               IF WS-PAY-COUNT > ZERO
                   PERFORM 2290-NEW-CHARGE
                   MOVE 'SWFT'             TO WS-CW-TYPE (CW-IDX)
                   MOVE WS-PAY-COUNT       TO WS-CW-ITEMS (CW-IDX)
                   MOVE WS-PT-SWIFT-FEE (PT-IDX)
                                           TO WS-CW-UNIT (CW-IDX)
                   MOVE 'INTERNATIONAL PAYMENT FEES'
                                           TO WS-CW-DESC (CW-IDX)
                   PERFORM 2295-EXTEND-CHARGE
               END-IF
           END-IF

           IF WS-PT-CHAPS-FEE (PT-IDX) > ZERO
               MOVE 'CHAP'                 TO WS-HV-SCHEME
               PERFORM 2275-COUNT-PAYMENTS
               IF WS-PAY-COUNT > ZERO
                   PERFORM 2290-NEW-CHARGE
                   MOVE 'CHAP'             TO WS-CW-TYPE (CW-IDX)
                   MOVE WS-PAY-COUNT       TO WS-CW-ITEMS (CW-IDX)
                   MOVE WS-PT-CHAPS-FEE (PT-IDX)
                                           TO WS-CW-UNIT (CW-IDX)
                   MOVE 'CHAPS PAYMENT FEES'
                                           TO WS-CW-DESC (CW-IDX)
                   PERFORM 2295-EXTEND-CHARGE
               END-IF
           END-IF
           .
      *================================================================*
       2275-COUNT-PAYMENTS.
      *================================================================*
      *    PAYMENTS CAPTURED IN THE PERIOD THAT WERE NOT REJECTED.
      *    SWIFT PAYMENTS WHERE THE PAYER CHOSE 'OUR' ALREADY HAD THE
      *    FEE ADDED BY PYINIT1 (CHARGE_AMT > 0) AND ARE EXCLUDED.
           EXEC SQL
               SELECT COUNT(*)
                 INTO :WS-PAY-COUNT
                 FROM MBK.PAYINSTR
                WHERE DR_SORT_CODE = :WS-HV-SORT-CODE
                  AND DR_ACCT_NO   = :WS-HV-ACCT-NO
                  AND PAY_SCHEME   = :WS-HV-SCHEME
                  AND PAY_STATUS  <> 'REJ'
                  AND CHARGE_AMT   = 0
                  AND DATE(CAPTURE_TS) BETWEEN :WS-FROM-DB2
                                           AND :WS-TO-DB2
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2275-COUNT-PAYMENTS'  TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *================================================================*
       2290-NEW-CHARGE.
      *================================================================*
           IF WS-CHG-COUNT >= 20
               MOVE '2290-NEW-CHARGE'      TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'MORE THAN 20 CHARGE ITEMS FOR ONE ACCOUNT'
                                           TO ABN-MESSAGE
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-CHG-COUNT
           SET CW-IDX                      TO WS-CHG-COUNT
           MOVE SPACES                     TO WS-CW-TYPE (CW-IDX)
                                              WS-CW-DESC (CW-IDX)
           MOVE ZERO                       TO WS-CW-ITEMS (CW-IDX)
                                              WS-CW-UNIT (CW-IDX)
                                              WS-CW-GROSS (CW-IDX)
                                              WS-CW-CAP-RED (CW-IDX)
                                              WS-CW-NET (CW-IDX)
           .
      *================================================================*
       2295-EXTEND-CHARGE.
      *================================================================*
           COMPUTE WS-CW-GROSS (CW-IDX) =
                   WS-CW-ITEMS (CW-IDX) * WS-CW-UNIT (CW-IDX)
           MOVE WS-CW-GROSS (CW-IDX)       TO WS-CW-NET (CW-IDX)
           .
      *================================================================*
       2500-APPLY-MONTHLY-CAP.
      *================================================================*
           IF WS-PT-MONTHLY-CAP (PT-IDX) NOT > ZERO
               GO TO 2500-EXIT
           END-IF

           MOVE ZERO                       TO WS-CAPPED-TOTAL
           PERFORM VARYING CW-IDX FROM 1 BY 1
                   UNTIL CW-IDX > WS-CHG-COUNT
               IF WS-CW-TYPE (CW-IDX) = 'UODF' OR 'UNPD' OR 'PDIT'
                   ADD WS-CW-GROSS (CW-IDX) TO WS-CAPPED-TOTAL
               END-IF
           END-PERFORM

           IF WS-CAPPED-TOTAL NOT > WS-PT-MONTHLY-CAP (PT-IDX)
               GO TO 2500-EXIT
           END-IF

           ADD 1                           TO WS-CAPPED-COUNT
           COMPUTE WS-EXCESS =
                   WS-CAPPED-TOTAL - WS-PT-MONTHLY-CAP (PT-IDX)

      *    REDUCE IN THE ORDER UODF, PDIT, UNPD.
           PERFORM VARYING WS-CAP-SUB FROM 1 BY 4
                   UNTIL WS-CAP-SUB > 12 OR WS-EXCESS NOT > ZERO
               MOVE WS-CAP-ORDER (WS-CAP-SUB:4) TO WS-CAP-TYPE
               PERFORM VARYING CW-IDX FROM 1 BY 1
                       UNTIL CW-IDX > WS-CHG-COUNT
                          OR WS-EXCESS NOT > ZERO
                   IF WS-CW-TYPE (CW-IDX) = WS-CAP-TYPE
                       IF WS-CW-NET (CW-IDX) > WS-EXCESS
                           MOVE WS-EXCESS  TO WS-REDUCE
                       ELSE
                           MOVE WS-CW-NET (CW-IDX) TO WS-REDUCE
                       END-IF
                       ADD WS-REDUCE       TO WS-CW-CAP-RED (CW-IDX)
                       SUBTRACT WS-REDUCE  FROM WS-CW-NET (CW-IDX)
                       SUBTRACT WS-REDUCE  FROM WS-EXCESS
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       2600-WRITE-PENDING.
      *================================================================*
           IF WS-CHG-COUNT = ZERO
               ADD 1                       TO WS-NIL-COUNT
               GO TO 2600-EXIT
           END-IF

           PERFORM 2650-CALC-APPLY-DATE
           MOVE ZERO                       TO WS-ITEM-SEQ
                                              WS-ACCOUNT-TOTAL

           PERFORM VARYING CW-IDX FROM 1 BY 1
                   UNTIL CW-IDX > WS-CHG-COUNT
                      OR WS-ALREADY-CHARGED
               PERFORM 2610-BUILD-PENDING
               WRITE CGP-CHARGE-RECORD
               EVALUATE TRUE
                   WHEN WS-CHGPEND-OK
                       ADD 1               TO WS-ITEMS-WRITTEN
                       PERFORM 2620-ACCUMULATE
                       PERFORM 7100-REPORT-DETAIL
                   WHEN WS-CHGPEND-DUPKEY
      *                FIRST ITEM ALREADY ON FILE = EARLIER RUN DID IT.
                       IF CW-IDX = 1
                           SET WS-ALREADY-CHARGED TO TRUE
                           ADD 1           TO WS-RERUN-SKIP-COUNT
                       ELSE
                           MOVE '2600-WRITE-PENDING' TO ABN-PARAGRAPH
                           MOVE 1002       TO ABN-ABEND-CODE
                           MOVE 'CHGPEND PARTIAL DUPLICATE'
                                           TO ABN-MESSAGE
                           MOVE CGP-KEY    TO ABN-KEY-DATA
                           PERFORM Z999-ABEND
                       END-IF
                   WHEN OTHER
                       MOVE '2600-WRITE-PENDING' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'CHGPEND WRITE FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-CHGPEND-STATUS TO ABN-FILE-STATUS
                       MOVE CGP-KEY        TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
               END-EVALUATE
           END-PERFORM

           IF NOT WS-ALREADY-CHARGED
               IF ACM-CHARGES-WAIVED
                   ADD 1                   TO WS-SUPPRESSED-COUNT
               ELSE
                   ADD 1                   TO WS-CHARGED-COUNT
                   ADD 1               TO WS-PT-ACCTS-CHARGED (PT-IDX)
               END-IF
           END-IF
           .
       2600-EXIT.
           EXIT.
      *================================================================*
       2610-BUILD-PENDING.
      *================================================================*
           MOVE SPACES                     TO CGP-CHARGE-RECORD
           MOVE ACM-SORT-CODE              TO CGP-SORT-CODE
           MOVE ACM-ACCT-NO                TO CGP-ACCT-NO
           MOVE WS-PERIOD-END              TO CGP-PERIOD-END
           MOVE WS-CW-TYPE (CW-IDX)        TO CGP-CHARGE-TYPE
           ADD 1                           TO WS-ITEM-SEQ
           MOVE WS-ITEM-SEQ                TO CGP-ITEM-SEQ
           MOVE ACM-CIF-NO                 TO CGP-CIF-NO
           MOVE ACM-PROD-CD                TO CGP-PROD-CD
           MOVE WS-PERIOD-START            TO CGP-PERIOD-START
           MOVE WS-BUS-DATE                TO CGP-CALC-DATE
           MOVE ZERO                       TO CGP-NOTIFY-DATE
           MOVE WS-APPLY-DATE              TO CGP-APPLY-DATE
           MOVE WS-CW-ITEMS (CW-IDX)       TO CGP-ITEM-COUNT
           MOVE WS-CW-UNIT (CW-IDX)        TO CGP-UNIT-AMT
           MOVE WS-CW-GROSS (CW-IDX)       TO CGP-GROSS-AMT
           MOVE WS-CW-CAP-RED (CW-IDX)     TO CGP-CAP-REDUCTION
           MOVE ZERO                       TO CGP-WAIVED-AMT
           MOVE WS-CW-NET (CW-IDX)         TO CGP-NET-AMT
           MOVE WS-CW-DESC (CW-IDX)        TO CGP-DESCRIPTION
           IF ACM-CHARGES-WAIVED
               SET CGP-SUPPRESSED          TO TRUE
               MOVE 'WAIV'                 TO CGP-SUPPRESS-RSN
           ELSE
               IF WS-CW-NET (CW-IDX) = ZERO
                   SET CGP-SUPPRESSED      TO TRUE
                   MOVE 'CAPD'             TO CGP-SUPPRESS-RSN
               ELSE
                   SET CGP-CALCULATED      TO TRUE
               END-IF
           END-IF
           .
      *================================================================*
       2620-ACCUMULATE.
      *================================================================*
           ADD CGP-GROSS-AMT               TO WS-TOTAL-GROSS
           ADD CGP-CAP-REDUCTION           TO WS-TOTAL-CAP-RED
           IF CGP-SUPPRESSED
               ADD CGP-NET-AMT             TO WS-TOTAL-SUPPRESSED
               ADD CGP-NET-AMT          TO WS-PT-TOTAL-SUPPRESSED
                                                              (PT-IDX)
           ELSE
               ADD CGP-NET-AMT             TO WS-TOTAL-NET
               ADD CGP-NET-AMT             TO WS-PT-TOTAL-CHARGED
                                                              (PT-IDX)
           END-IF
           .
      *================================================================*
       2650-CALC-APPLY-DATE.
      *================================================================*
      *    APPLY DATE = TODAY + NOTIFICATION DAYS, ROLLED FORWARD TO A
      *    BUSINESS DAY.
           MOVE 'D'                        TO DTE-FUNCTION
           MOVE WS-BUS-DATE                TO DTE-DATE-IN
           MOVE WS-PT-NOTIFY-DAYS (PT-IDX) TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           IF NOT DTE-OK
               MOVE '2650-CALC-APPLY-DATE' TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'DTEUTIL FAILED ADDING NOTIFICATION DAYS'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           MOVE DTE-DATE-OUT               TO WS-APPLY-DATE

           MOVE ZERO                       TO WS-LOOP-GUARD
           MOVE 'B'                        TO DTE-FUNCTION
           MOVE WS-APPLY-DATE              TO DTE-DATE-IN
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           PERFORM UNTIL DTE-BUS-DAY-IND = 'Y'
                      OR WS-LOOP-GUARD > 10
               MOVE 'D'                    TO DTE-FUNCTION
               MOVE WS-APPLY-DATE          TO DTE-DATE-IN
               MOVE 1                      TO DTE-DAYS
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               MOVE DTE-DATE-OUT           TO WS-APPLY-DATE
               MOVE 'B'                    TO DTE-FUNCTION
               MOVE WS-APPLY-DATE          TO DTE-DATE-IN
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               ADD 1                       TO WS-LOOP-GUARD
           END-PERFORM
           .
      *================================================================*
       2800-RESET-PERIOD.
      *================================================================*
      *    START A NEW CHARGING PERIOD. DONE EVEN WHEN NO CHARGE WAS
      *    DUE SO THE NEXT PERIOD STARTS FROM A CLEAN BASE.
           MOVE ZERO                       TO ACM-UNARR-OD-DAYS
                                              ACM-UNPAID-ITEMS
                                              ACM-PAID-ITEMS
           MOVE WS-NEXT-PERIOD-START       TO ACM-CHG-PERIOD-START
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER

           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '2800-RESET-PERIOD'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 3100-PRODUCT-TOTALS
           PERFORM 3200-RUN-TOTALS

           CLOSE ACCTMAST-FILE
                 CHGPEND-FILE
                 REPORT-FILE

           DISPLAY 'CGCAL010 - ACCOUNTS READ         ' WS-READ-COUNT
           DISPLAY 'CGCAL010 - PERIOD ENDING TODAY   ' WS-DUE-COUNT
           DISPLAY 'CGCAL010 - CHARGED               ' WS-CHARGED-COUNT
           DISPLAY 'CGCAL010 - SUPPRESSED (WAIVED)   '
                   WS-SUPPRESSED-COUNT
           DISPLAY 'CGCAL010 - NOTHING DUE           ' WS-NIL-COUNT
           DISPLAY 'CGCAL010 - CAP APPLIED           ' WS-CAPPED-COUNT
           DISPLAY 'CGCAL010 - SKIPPED (RERUN)       '
                   WS-RERUN-SKIP-COUNT
           DISPLAY 'CGCAL010 - CHARGE ITEMS WRITTEN  ' WS-ITEMS-WRITTEN

           IF WS-NO-PRODUCT-COUNT > ZERO
               DISPLAY 'CGCAL010 W001 ' WS-NO-PRODUCT-COUNT
                       ' ACCOUNTS WITH NO PRODUCT RECORD'
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       3100-PRODUCT-TOTALS.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           PERFORM VARYING PT-IDX FROM 1 BY 1
                   UNTIL PT-IDX > WS-PROD-COUNT
               IF WS-PT-ACCTS-CHARGED (PT-IDX) > ZERO
               OR WS-PT-TOTAL-SUPPRESSED (PT-IDX) > ZERO
                   IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
                       PERFORM 7000-REPORT-HEADINGS
                   END-IF
                   MOVE SPACES             TO TL-LABEL
                   STRING 'PRODUCT ' WS-PT-PROD-CD (PT-IDX)
                          ' CHARGED'
                          DELIMITED BY SIZE
                          INTO TL-LABEL
                   END-STRING
                   MOVE WS-PT-ACCTS-CHARGED (PT-IDX) TO TL-COUNT
                   MOVE WS-PT-TOTAL-CHARGED (PT-IDX) TO TL-AMOUNT
                   WRITE REPORT-RECORD     FROM WS-TOTAL-LINE
                   ADD 2                   TO WS-LINE-COUNT
                   IF WS-PT-TOTAL-SUPPRESSED (PT-IDX) > ZERO
                       MOVE SPACES         TO TL-LABEL
                       STRING 'PRODUCT ' WS-PT-PROD-CD (PT-IDX)
                              ' SUPPRESSED'
                              DELIMITED BY SIZE
                              INTO TL-LABEL
                       END-STRING
                       MOVE ZERO           TO TL-COUNT
                       MOVE WS-PT-TOTAL-SUPPRESSED (PT-IDX)
                                           TO TL-AMOUNT
                       WRITE REPORT-RECORD FROM WS-TOTAL-LINE
                       ADD 2               TO WS-LINE-COUNT
                   END-IF
               END-IF
           END-PERFORM
           .
      *================================================================*
       3200-RUN-TOTALS.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS

           MOVE 'ACCOUNTS WITH PERIOD ENDING'  TO TL-LABEL
           MOVE WS-DUE-COUNT                   TO TL-COUNT
           MOVE ZERO                           TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE

           MOVE 'GROSS CHARGES CALCULATED'     TO TL-LABEL
           MOVE WS-ITEMS-WRITTEN               TO TL-COUNT
           MOVE WS-TOTAL-GROSS                 TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE

           MOVE 'REDUCED BY MONTHLY CAP'       TO TL-LABEL
           MOVE WS-CAPPED-COUNT                TO TL-COUNT
           MOVE WS-TOTAL-CAP-RED               TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE

           MOVE 'SUPPRESSED (ACCOUNT WAIVED)'  TO TL-LABEL
           MOVE WS-SUPPRESSED-COUNT            TO TL-COUNT
           MOVE WS-TOTAL-SUPPRESSED            TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE

           MOVE 'NET CHARGES PENDING'          TO TL-LABEL
           MOVE WS-CHARGED-COUNT               TO TL-COUNT
           MOVE WS-TOTAL-NET                   TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE

           MOVE 'SKIPPED - ALREADY CALCULATED' TO TL-LABEL
           MOVE WS-RERUN-SKIP-COUNT            TO TL-COUNT
           MOVE ZERO                           TO TL-AMOUNT
           WRITE REPORT-RECORD                 FROM WS-TOTAL-LINE
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
           MOVE CGP-SORT-CODE              TO DL-SORT-CODE
           MOVE CGP-ACCT-NO                TO DL-ACCT-NO
           MOVE CGP-PROD-CD                TO DL-PROD-CD
           MOVE CGP-CHARGE-TYPE            TO DL-TYPE
           MOVE CGP-ITEM-COUNT             TO DL-ITEMS
           MOVE CGP-UNIT-AMT               TO DL-UNIT
           MOVE CGP-GROSS-AMT              TO DL-GROSS
           MOVE CGP-CAP-REDUCTION          TO DL-CAP-RED
           MOVE CGP-NET-AMT                TO DL-NET
           MOVE CGP-APPLY-DATE             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO DL-APPLY-DATE
           MOVE CGP-STATUS                 TO DL-STATUS
           EVALUATE TRUE
               WHEN CGP-SUPPRESSED AND CGP-SUPPRESS-RSN = 'WAIV'
                   MOVE 'ACCOUNT WAIVED'   TO DL-NOTE
               WHEN CGP-SUPPRESSED
                   MOVE 'NIL AFTER CAP'    TO DL-NOTE
               WHEN CGP-CAP-REDUCTION > ZERO
                   MOVE 'CAPPED'           TO DL-NOTE
               WHEN OTHER
                   MOVE SPACES             TO DL-NOTE
           END-EVALUATE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
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
           MOVE ACM-KEY                    TO ABN-KEY-DATA
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
