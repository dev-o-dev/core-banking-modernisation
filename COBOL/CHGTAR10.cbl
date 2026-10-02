       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CHGTAR10.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-10-03.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CHGTAR10                                            *
      * TITLE    : PERSONAL CURRENT ACCOUNTS - ANNUAL SUMMARY OF       *
      *            CHARGES (TARIFF SUMMARY STATEMENT)                  *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : CHGTAR10                                            *
      * JCL      : CHGTAR01 (MONTHLY, FIRST BUSINESS DAY)              *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ)      *
      *            CIFMAST  - CUSTOMER MASTER (VSAM KSDS, RANDOM)      *
      *            PRDPARM  - PRODUCT PARAMETERS (PRODUCT NAME)        *
      *            MBK.CHGHIST - CHARGES APPLIED (CGAPL010) AND        *
      *                       REFUNDED (CGREF010)                      *
      *            MBK.INTPAID - INTEREST CAPITALISED (INCAP010)       *
      *            PARMIN   - OPTIONAL CONTROL CARDS (SEE BELOW)       *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : LETTERS  - SUMMARY LETTER EXTRACT (CHGTARRC FB 600) *
      *            REPORT   - CONTROL REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * UNDER THE 2011 PERSONAL CURRENT ACCOUNT UNDERTAKINGS (NOW FCA  *
      * BCOBS 4.3 / PSR 2017 STATEMENT OF FEES) EVERY PERSONAL CURRENT *
      * ACCOUNT CUSTOMER GETS, ONCE A YEAR, A SUMMARY OF ALL CHARGES   *
      * AND INTEREST FOR THE PREVIOUS 12 MONTHS - EVEN IF NIL.         *
      *                                                                *
      * SELECTION: OPEN OR DORMANT CURRENT ACCOUNTS ('CURR') HELD BY A *
      * PERSONAL CUSTOMER (CIF-CUST-TYPE 'P') WHOSE OPENING ANNIVERSARY*
      * MONTH (MONTH OF ACM-OPEN-DATE) IS THE BUSINESS MONTH. THE      *
      * PERIOD IS THE 12 CALENDAR MONTHS ENDING ON THE LAST DAY OF THE *
      * PREVIOUS MONTH, OR FROM THE OPENING DATE IF LATER (PART YEAR). *
      *                                                                *
      * FOR EACH ACCOUNT:                                              *
      *   CHGHIST  SUM OF ITEM_COUNT, NET_AMT, REFUND_AMT BY           *
      *            CHARGE_TYPE FOR APPLY_DATE IN THE PERIOD            *
      *   INTPAID  DEBIT INTEREST (GROSS, OF WHICH UNARRANGED) AND     *
      *            CREDIT INTEREST (GROSS AND TAX) CAPITALISED IN THE  *
      *            PERIOD                                              *
      *                                                                *
      * PARMIN CARDS (BOTH OPTIONAL):                                  *
      *   MONTH=MM        ANNIVERSARY MONTH OVERRIDE (RERUN OF A MISSED*
      *                   MONTH - PERIOD IS STILL TO LAST MONTH END)   *
      *   MODE=ALL        EVERY ELIGIBLE ACCOUNT REGARDLESS OF         *
      *                   ANNIVERSARY (USED ONCE, OCTOBER 2011 LAUNCH) *
      *----------------------------------------------------------------*
      * KNOWN PROBLEMS                                                 *
      * PIR 2012/140 OPEN - CHAPS FEES ('CHAP') ARE NOT A SEPARATE     *
      *   LINE ON THE TEMPLATE AND PRINT AS 'OTHER CHARGES'.           *
      * PIR 2013/052 OPEN - REFUNDS ARE TAKEN FROM THE ORIGINAL CHARGE *
      *   ROW (APPLY_DATE), NOT THE REFUND DATE. A REFUND GIVEN AFTER  *
      *   THE PERIOD FOR A CHARGE IN THE PERIOD IS SHOWN; A REFUND IN  *
      *   THE PERIOD FOR AN OLDER CHARGE IS NOT.                       *
      * JOINT ACCOUNTS GET ONE LETTER, TO THE FIRST NAMED HOLDER ONLY. *
      * E-STATEMENT TEMPLATE ASOCE01 NEVER WENT LIVE - ALL BY POST.    *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-10-03 AOS  ORIGINAL VERSION (PCA UNDERTAKINGS, OCT 2011)  *
      * 2012-05-14 AOS  REFUNDS SHOWN SEPARATELY                       *
      * 2013-11-18 SAT  SUPPRESS FOR DECEASED CUSTOMERS                *
      * 2015-02-09 PDQ  UNARRANGED DEBIT INTEREST FROM INTPAID         *
      * 2016-03-07 KLM  HIGH CHARGE LIST FOR CUSTOMER TREATMENT TEAM   *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
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
           SELECT LETTERS-FILE     ASSIGN TO LETTERS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LETTERS-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD.
           05  PARM-KEYWORD                PIC X(06).
               88  PARM-MONTH                  VALUE 'MONTH='.
           05  PARM-VALUE                  PIC X(04).
           05  FILLER                      PIC X(70).
       01  PARMIN-RECORD-MODE.
           05  PARM-MODE-KEYWORD           PIC X(05).
               88  PARM-MODE                   VALUE 'MODE='.
           05  PARM-MODE-VALUE             PIC X(03).
           05  FILLER                      PIC X(72).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(600).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CHGTAR10'.
      *
       01  WS-CONSTANTS.
           05  WS-LETTER-CD                PIC X(04) VALUE 'ASOC'.
           05  WS-TEMPLATE-ID              PIC X(08) VALUE 'ASOCP01'.
      *    ACCOUNTS CHARGED MORE THAN THIS IN THE YEAR ARE LISTED FOR
      *    THE CUSTOMER TREATMENT TEAM (FCA HIGH COST CREDIT REVIEW).
           05  WS-HIGH-CHARGE-LIMIT        PIC S9(07)V99 COMP-3
                                           VALUE +500.00.
      *
       01  WS-FILE-STATUSES.
           05  WS-PARMIN-STATUS            PIC X(02).
               88  WS-PARMIN-OK                VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-EOC-IND                  PIC X(01).
               88  WS-END-OF-CURSOR            VALUE 'Y'.
           05  WS-ALL-IND                  PIC X(01) VALUE 'N'.
               88  WS-MODE-ALL                 VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-ANNIV-MM                 PIC 9(02).
           05  WS-PERIOD-TO                PIC 9(08).
           05  WS-PERIOD-TO-R REDEFINES WS-PERIOD-TO.
               10  WS-PT-CCYY              PIC 9(04).
               10  WS-PT-MM                PIC 9(02).
               10  WS-PT-DD                PIC 9(02).
           05  WS-PERIOD-FROM              PIC 9(08).
           05  WS-PERIOD-FROM-R REDEFINES WS-PERIOD-FROM.
               10  WS-PF-CCYY              PIC 9(04).
               10  WS-PF-MM                PIC 9(02).
               10  WS-PF-DD                PIC 9(02).
           05  WS-ACCT-FROM                PIC 9(08).
           05  WS-PERIOD-TO-INT            PIC S9(09) COMP.
           05  WS-CACHED-PROD-CD           PIC X(10) VALUE LOW-VALUES.
           05  WS-PROD-NAME                PIC X(18).
           05  WS-NAME-WORK                PIC X(40).
           05  WS-LINE-IDX                 PIC S9(04) COMP.
           05  WS-I                        PIC S9(04) COMP.
           05  WS-TOTAL-CHARGES            PIC S9(13)V99 COMP-3.
           05  WS-TOTAL-REFUNDS            PIC S9(13)V99 COMP-3.
           05  WS-DR-INT-GROSS             PIC S9(13)V99 COMP-3.
           05  WS-DR-INT-UNARR             PIC S9(13)V99 COMP-3.
           05  WS-DR-INT-ARR               PIC S9(13)V99 COMP-3.
           05  WS-CR-INT-GROSS             PIC S9(13)V99 COMP-3.
           05  WS-CR-INT-TAX               PIC S9(13)V99 COMP-3.
           05  WS-EXC-TEXT                 PIC X(45).
      *
      *---- CHARGE TYPES - LINE ORDER ON THE LETTER -------------------*
       01  WS-CHG-TYPE-VALUES.
           05  FILLER    PIC X(04) VALUE 'MFEE'.
           05  FILLER    PIC X(04) VALUE 'AODF'.
           05  FILLER    PIC X(04) VALUE 'UODF'.
           05  FILLER    PIC X(04) VALUE 'UNPD'.
           05  FILLER    PIC X(04) VALUE 'PDIT'.
           05  FILLER    PIC X(04) VALUE 'TXNF'.
           05  FILLER    PIC X(04) VALUE 'SWFT'.
           05  FILLER    PIC X(04) VALUE 'OTHR'.
       01  WS-CHG-TYPES REDEFINES WS-CHG-TYPE-VALUES.
           05  WS-CHG-TYPE                 PIC X(04) OCCURS 8 TIMES.
      *
      *---- PER ACCOUNT CHARGE LINES ----------------------------------*
       01  WS-ACCT-LINES.
           05  WS-AL-ENTRY                 OCCURS 8 TIMES.
               10  WS-AL-COUNT             PIC S9(09) COMP-3.
               10  WS-AL-AMOUNT            PIC S9(13)V99 COMP-3.
               10  WS-AL-REFUND            PIC S9(13)V99 COMP-3.
      *
      *---- RUN TOTALS BY CHARGE TYPE ---------------------------------*
       01  WS-RUN-LINES.
           05  WS-RL-ENTRY                 OCCURS 8 TIMES.
               10  WS-RL-ACCOUNTS          PIC S9(09) COMP-3.
               10  WS-RL-COUNT             PIC S9(11) COMP-3.
               10  WS-RL-AMOUNT            PIC S9(15)V99 COMP-3.
               10  WS-RL-REFUND            PIC S9(15)V99 COMP-3.
      *
      *---- HOST VARIABLES --------------------------------------------*
       01  HV-FROM-DATE                    PIC X(10).
       01  HV-TO-DATE                      PIC X(10).
       01  HV-ITEM-COUNT                   PIC S9(09) COMP.
       01  HV-NET-AMT                      PIC S9(13)V99 COMP-3.
       01  HV-REFUND-AMT                   PIC S9(13)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-ACCTS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOT-ANNIVERSARY          PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOT-ELIGIBLE             PIC S9(09) COMP-3 VALUE 0.
           05  WS-BUSINESS-SKIPPED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-DECEASED-SKIPPED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOO-NEW                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-CIF-NOT-FOUND            PIC S9(09) COMP-3 VALUE 0.
           05  WS-LETTERS-WRITTEN          PIC S9(09) COMP-3 VALUE 0.
           05  WS-NIL-LETTERS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-PART-YEAR                PIC S9(09) COMP-3 VALUE 0.
           05  WS-HIGH-CHARGE              PIC S9(09) COMP-3 VALUE 0.
           05  WS-REFUND-EXCEPTIONS        PIC S9(09) COMP-3 VALUE 0.
           05  WS-OTHER-TYPES              PIC S9(09) COMP-3 VALUE 0.
           05  WS-GRAND-CHARGES            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GRAND-REFUNDS            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GRAND-DR-INT             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GRAND-DR-UNARR           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GRAND-CR-INT             PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CHGTAR10'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - ANNUAL SUMMARY OF CHARGES'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(18) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(14) VALUE 'CUSTOMER'.
           05  FILLER            PIC X(20) VALUE 'PERIOD'.
           05  FILLER            PIC X(16) VALUE '       CHARGES'.
           05  FILLER            PIC X(16) VALUE '       REFUNDS'.
           05  FILLER            PIC X(48) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-FROM                     PIC 9(08).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-TO                       PIC 9(08).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-CHARGES                  PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-REFUNDS                  PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-NOTE                     PIC X(45).
           05  FILLER                      PIC X(03) VALUE SPACES.
       01  WS-TYPE-HEADING.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(10) VALUE 'TYPE'.
           05  FILLER            PIC X(14) VALUE '      ACCOUNTS'.
           05  FILLER            PIC X(14) VALUE '         ITEMS'.
           05  FILLER            PIC X(22)
                                 VALUE '           NET CHARGED'.
           05  FILLER            PIC X(22)
                                 VALUE '              REFUNDED'.
           05  FILLER            PIC X(50) VALUE SPACES.
       01  WS-TYPE-LINE.
           05  TY-CC                       PIC X(01) VALUE ' '.
           05  TY-TYPE                     PIC X(04).
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  TY-ACCOUNTS                 PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  TY-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  TY-AMOUNT                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  TY-REFUND                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(54) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY CHGTARRC.
      *
           COPY DATECARD.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLCHGH END-EXEC.
           EXEC SQL INCLUDE DCLINTP END-EXEC.
      *
      *    CHARGES IN THE PERIOD, SUMMED BY TYPE. XCHGH01 LEADS ON
      *    SORT_CODE, ACCT_NO SO THIS IS A MATCHING INDEX SCAN.
           EXEC SQL DECLARE CHGCSR CURSOR FOR
               SELECT CHARGE_TYPE,
                      SUM(ITEM_COUNT),
                      SUM(NET_AMT),
                      SUM(REFUND_AMT)
                 FROM MBK.CHGHIST
                WHERE SORT_CODE  = :CGH-SORT-CODE
                  AND ACCT_NO    = :CGH-ACCT-NO
                  AND APPLY_DATE BETWEEN :HV-FROM-DATE
                                     AND :HV-TO-DATE
                GROUP BY CHARGE_TYPE
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
           PERFORM 8000-READ-NEXT
           PERFORM 2000-PROCESS-ACCOUNT THRU 2000-EXIT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PARMIN-FILE
                       ACCTMAST-FILE
                       CIFMAST-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                OUTPUT LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-PARMIN-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-LETTERS-STATUS NOT = '00'
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
           MOVE WS-BUS-DATE (5:2)          TO WS-ANNIV-MM

           READ PARMIN-FILE
           PERFORM UNTIL NOT WS-PARMIN-OK
               EVALUATE TRUE
                   WHEN PARM-MONTH
                       IF PARM-VALUE (1:2) NUMERIC
                       AND PARM-VALUE (1:2) >= '01'
                       AND PARM-VALUE (1:2) <= '12'
                           MOVE PARM-VALUE (1:2) TO WS-ANNIV-MM
                       ELSE
                           MOVE '1000-INITIALISE' TO ABN-PARAGRAPH
                           MOVE 1005       TO ABN-ABEND-CODE
                           MOVE 'INVALID MONTH= CARD (MM)'
                                           TO ABN-MESSAGE
                           PERFORM Z999-ABEND
                       END-IF
                   WHEN PARM-MODE
                       IF PARM-MODE-VALUE = 'ALL'
                           SET WS-MODE-ALL TO TRUE
                       END-IF
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               READ PARMIN-FILE
           END-PERFORM
           CLOSE PARMIN-FILE

      *    PERIOD END = LAST DAY OF THE PREVIOUS CALENDAR MONTH.
           MOVE WS-BUS-DATE                TO WS-PERIOD-TO
           MOVE 01                         TO WS-PT-DD
           COMPUTE WS-PERIOD-TO-INT =
                   FUNCTION INTEGER-OF-DATE (WS-PERIOD-TO) - 1
           COMPUTE WS-PERIOD-TO =
                   FUNCTION DATE-OF-INTEGER (WS-PERIOD-TO-INT)
      *    PERIOD START = FIRST DAY OF THE MONTH 11 MONTHS EARLIER.
           MOVE WS-PT-CCYY                 TO WS-PF-CCYY
           SUBTRACT 1                      FROM WS-PF-CCYY
           COMPUTE WS-PF-MM = WS-PT-MM + 1
           IF WS-PF-MM > 12
               MOVE 01                     TO WS-PF-MM
               ADD 1                       TO WS-PF-CCYY
           END-IF
           MOVE 01                         TO WS-PF-DD
           MOVE SPACES                     TO HV-TO-DATE
           STRING WS-PERIOD-TO (1:4) '-' WS-PERIOD-TO (5:2) '-'
                  WS-PERIOD-TO (7:2)
                  DELIMITED BY SIZE
                  INTO HV-TO-DATE
           END-STRING
           DISPLAY 'CHGTAR10 - PERIOD ' WS-PERIOD-FROM ' TO '
                   WS-PERIOD-TO ' ANNIVERSARY MONTH ' WS-ANNIV-MM
           IF WS-MODE-ALL
               DISPLAY 'CHGTAR10 - MODE=ALL - EVERY ELIGIBLE ACCOUNT'
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE ZERO                   TO WS-RL-ACCOUNTS (WS-I)
                                              WS-RL-COUNT (WS-I)
                                              WS-RL-AMOUNT (WS-I)
                                              WS-RL-REFUND (WS-I)
           END-PERFORM
           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           ADD 1                           TO WS-ACCTS-READ
           IF NOT ACM-CURRENT
           OR NOT (ACM-OPEN OR ACM-DORMANT)
           OR ACM-OPEN-DATE NOT NUMERIC
               ADD 1                       TO WS-NOT-ELIGIBLE
               GO TO 2000-NEXT
           END-IF
           IF NOT WS-MODE-ALL
           AND ACM-OPEN-DATE (5:2) NOT = WS-ANNIV-MM
               ADD 1                       TO WS-NOT-ANNIVERSARY
               GO TO 2000-NEXT
           END-IF
      *    OPENED AFTER THE PERIOD - FIRST SUMMARY NEXT YEAR.
           IF ACM-OPEN-DATE > WS-PERIOD-TO
               ADD 1                       TO WS-TOO-NEW
               GO TO 2000-NEXT
           END-IF

           MOVE ACM-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   CONTINUE
               WHEN WS-CIFMAST-NOTFND
                   ADD 1                   TO WS-CIF-NOT-FOUND
                   MOVE 'CUSTOMER NOT ON CIF - NO SUMMARY SENT'
                                           TO WS-EXC-TEXT
                   PERFORM 7100-REPORT-ACCOUNT
                   GO TO 2000-NEXT
               WHEN OTHER
                   MOVE '2000-PROCESS-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE ACM-CIF-NO         TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           IF NOT CIF-PERSONAL
               ADD 1                       TO WS-BUSINESS-SKIPPED
               GO TO 2000-NEXT
           END-IF
           IF CIF-DECEASED
           OR (CIF-DATE-OF-DEATH NUMERIC
               AND CIF-DATE-OF-DEATH > ZERO)
               ADD 1                       TO WS-DECEASED-SKIPPED
               GO TO 2000-NEXT
           END-IF

      *    PART YEAR - FROM THE OPENING DATE.
           IF ACM-OPEN-DATE > WS-PERIOD-FROM
               MOVE ACM-OPEN-DATE          TO WS-ACCT-FROM
           ELSE
               MOVE WS-PERIOD-FROM         TO WS-ACCT-FROM
           END-IF
           MOVE SPACES                     TO HV-FROM-DATE
           STRING WS-ACCT-FROM (1:4) '-' WS-ACCT-FROM (5:2) '-'
                  WS-ACCT-FROM (7:2)
                  DELIMITED BY SIZE
                  INTO HV-FROM-DATE
           END-STRING

           PERFORM 3000-GET-CHARGES
           PERFORM 3500-GET-INTEREST
           PERFORM 4000-BUILD-LETTER
           PERFORM 4500-WRITE-LETTER
           PERFORM 5000-ACCUMULATE
           .
       2000-NEXT.
           PERFORM 8000-READ-NEXT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-GET-CHARGES.
      *================================================================*
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE ZERO                   TO WS-AL-COUNT (WS-I)
                                              WS-AL-AMOUNT (WS-I)
                                              WS-AL-REFUND (WS-I)
           END-PERFORM
           MOVE ZERO                       TO WS-TOTAL-CHARGES
                                              WS-TOTAL-REFUNDS
           MOVE ACM-SORT-CODE              TO CGH-SORT-CODE
           MOVE ACM-ACCT-NO                TO CGH-ACCT-NO
           EXEC SQL OPEN CHGCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '3000-GET-CHARGES'     TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE 'N'                        TO WS-EOC-IND
           PERFORM UNTIL WS-END-OF-CURSOR
               EXEC SQL
                   FETCH CHGCSR
                    INTO :CGH-CHARGE-TYPE, :HV-ITEM-COUNT,
                         :HV-NET-AMT, :HV-REFUND-AMT
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 3100-ADD-CHARGE-TYPE
                   WHEN +100
                       SET WS-END-OF-CURSOR TO TRUE
                   WHEN OTHER
                       MOVE '3000-GET-CHARGES' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CHGCSR END-EXEC
           .
      *----------------------------------------------------------------*
       3100-ADD-CHARGE-TYPE.
      *----------------------------------------------------------------*
      *    UNKNOWN TYPES (AND CHAP - PIR 2012/140) GO TO LINE 8.
           MOVE 8                          TO WS-LINE-IDX
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 7
               IF WS-CHG-TYPE (WS-I) = CGH-CHARGE-TYPE
                   MOVE WS-I               TO WS-LINE-IDX
               END-IF
           END-PERFORM
           IF WS-LINE-IDX = 8
               ADD 1                       TO WS-OTHER-TYPES
           END-IF
           ADD HV-ITEM-COUNT           TO WS-AL-COUNT (WS-LINE-IDX)
           ADD HV-NET-AMT              TO WS-AL-AMOUNT (WS-LINE-IDX)
           ADD HV-REFUND-AMT           TO WS-AL-REFUND (WS-LINE-IDX)
           ADD HV-NET-AMT                  TO WS-TOTAL-CHARGES
           ADD HV-REFUND-AMT               TO WS-TOTAL-REFUNDS
      *    A REFUND GREATER THAN THE CHARGE MEANS CGREF010 WAS RUN
      *    TWICE OR THE ROW WAS UPDATED BY SPUFI.
           IF HV-REFUND-AMT > HV-NET-AMT
               ADD 1                       TO WS-REFUND-EXCEPTIONS
               MOVE SPACES                 TO WS-EXC-TEXT
               STRING 'REFUND EXCEEDS CHARGE - TYPE ' CGH-CHARGE-TYPE
                      DELIMITED BY SIZE
                      INTO WS-EXC-TEXT
               END-STRING
               PERFORM 7100-REPORT-ACCOUNT
           END-IF
           .
      *================================================================*
       3500-GET-INTEREST.
      *================================================================*
           MOVE ACM-SORT-CODE              TO INP-SORT-CODE
           MOVE ACM-ACCT-NO                TO INP-ACCT-NO
      *    DEBIT INTEREST - GROSS AND THE UNARRANGED PART.
           EXEC SQL
               SELECT COALESCE(SUM(GROSS_AMT), 0),
                      COALESCE(SUM(UNARR_AMT), 0)
                 INTO :INP-GROSS-AMT, :INP-UNARR-AMT
                 FROM MBK.INTPAID
                WHERE SORT_CODE = :INP-SORT-CODE
                  AND ACCT_NO   = :INP-ACCT-NO
                  AND CR_DR_IND = 'D'
                  AND CAP_DATE BETWEEN :HV-FROM-DATE AND :HV-TO-DATE
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '3500-GET-INTEREST'    TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE INP-GROSS-AMT              TO WS-DR-INT-GROSS
           MOVE INP-UNARR-AMT              TO WS-DR-INT-UNARR
           COMPUTE WS-DR-INT-ARR = WS-DR-INT-GROSS - WS-DR-INT-UNARR
           IF WS-DR-INT-ARR < ZERO
               MOVE ZERO                   TO WS-DR-INT-ARR
           END-IF
      *    CREDIT INTEREST - GROSS AND TAX DEDUCTED (NIL FROM APRIL
      *    2016 - PERSONAL SAVINGS ALLOWANCE).
           EXEC SQL
               SELECT COALESCE(SUM(GROSS_AMT), 0),
                      COALESCE(SUM(TAX_AMT), 0)
                 INTO :INP-GROSS-AMT, :INP-TAX-AMT
                 FROM MBK.INTPAID
                WHERE SORT_CODE = :INP-SORT-CODE
                  AND ACCT_NO   = :INP-ACCT-NO
                  AND CR_DR_IND = 'C'
                  AND CAP_DATE BETWEEN :HV-FROM-DATE AND :HV-TO-DATE
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '3500-GET-INTEREST'    TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE INP-GROSS-AMT              TO WS-CR-INT-GROSS
           MOVE INP-TAX-AMT                TO WS-CR-INT-TAX
           .
      *================================================================*
       4000-BUILD-LETTER.
      *================================================================*
           PERFORM 4100-GET-PRODUCT-NAME
           MOVE SPACES                     TO CTL-LETTER-RECORD
           MOVE WS-LETTER-CD               TO CTL-LETTER-CD
           MOVE WS-TEMPLATE-ID             TO CTL-TEMPLATE-ID
           MOVE WS-BUS-DATE                TO CTL-ISSUE-DATE
           MOVE ACM-SORT-CODE              TO CTL-SORT-CODE
           MOVE ACM-ACCT-NO                TO CTL-ACCT-NO
           MOVE ACM-CIF-NO                 TO CTL-CIF-NO
           MOVE ACM-PROD-CD                TO CTL-PROD-CD
           MOVE WS-PROD-NAME               TO CTL-PROD-NAME
           MOVE SPACES                     TO WS-NAME-WORK
           STRING CIF-TITLE      DELIMITED BY SPACE
                  ' '            DELIMITED BY SIZE
                  CIF-FIRST-NM   DELIMITED BY '  '
                  ' '            DELIMITED BY SIZE
                  CIF-LAST-NM    DELIMITED BY '  '
                  INTO WS-NAME-WORK
           END-STRING
           MOVE WS-NAME-WORK               TO CTL-CUST-NAME
           MOVE CIF-ADDR-LINE1             TO CTL-ADDR-LINE1
           MOVE CIF-ADDR-LINE2             TO CTL-ADDR-LINE2
           MOVE CIF-TOWN                   TO CTL-TOWN
           MOVE CIF-POST-CD                TO CTL-POST-CD
           MOVE WS-ACCT-FROM               TO CTL-PERIOD-FROM
           MOVE WS-PERIOD-TO               TO CTL-PERIOD-TO
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE WS-CHG-TYPE (WS-I)     TO CTL-CHG-TYPE (WS-I)
               MOVE WS-AL-COUNT (WS-I)     TO CTL-CHG-COUNT (WS-I)
               IF WS-AL-AMOUNT (WS-I) > ZERO
                   MOVE WS-AL-AMOUNT (WS-I) TO CTL-CHG-AMOUNT (WS-I)
               ELSE
                   MOVE ZERO               TO CTL-CHG-AMOUNT (WS-I)
               END-IF
           END-PERFORM
           MOVE WS-TOTAL-CHARGES           TO CTL-TOTAL-CHARGES
           MOVE WS-TOTAL-REFUNDS           TO CTL-TOTAL-REFUNDS
           MOVE WS-DR-INT-ARR              TO CTL-DR-INT-ARRANGED
           MOVE WS-DR-INT-UNARR            TO CTL-DR-INT-UNARRANGED
           MOVE WS-CR-INT-GROSS            TO CTL-CR-INT-GROSS
           MOVE WS-CR-INT-TAX              TO CTL-CR-INT-TAX
           IF ACM-INT-RATE-DR NUMERIC
               MOVE ACM-INT-RATE-DR        TO CTL-CURRENT-DR-RATE
           ELSE
               MOVE ZERO                   TO CTL-CURRENT-DR-RATE
           END-IF
           IF WS-ACCT-FROM > WS-PERIOD-FROM
               SET CTL-PART-YEAR           TO TRUE
               ADD 1                       TO WS-PART-YEAR
           ELSE
               MOVE 'N'                    TO CTL-PART-YEAR-IND
           END-IF
           .
      *----------------------------------------------------------------*
       4100-GET-PRODUCT-NAME.
      *----------------------------------------------------------------*
           IF ACM-PROD-CD NOT = WS-CACHED-PROD-CD
               MOVE ACM-PROD-CD            TO WS-CACHED-PROD-CD
                                              PRD-PROD-CD
               READ PRDPARM-FILE
               IF WS-PRDPARM-OK
                   MOVE PRD-SHORT-NAME     TO WS-PROD-NAME
               ELSE
                   MOVE 'CURRENT ACCOUNT'  TO WS-PROD-NAME
               END-IF
           END-IF
           .
      *================================================================*
       4500-WRITE-LETTER.
      *================================================================*
           WRITE LETTERS-RECORD            FROM CTL-LETTER-RECORD
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '4500-WRITE-LETTER'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-LETTERS-WRITTEN
           .
      *================================================================*
       5000-ACCUMULATE.
      *================================================================*
           IF WS-TOTAL-CHARGES = ZERO
           AND WS-DR-INT-GROSS = ZERO
               ADD 1                       TO WS-NIL-LETTERS
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               IF WS-AL-COUNT (WS-I) NOT = ZERO
               OR WS-AL-AMOUNT (WS-I) NOT = ZERO
                   ADD 1                   TO WS-RL-ACCOUNTS (WS-I)
                   ADD WS-AL-COUNT (WS-I)  TO WS-RL-COUNT (WS-I)
                   ADD WS-AL-AMOUNT (WS-I) TO WS-RL-AMOUNT (WS-I)
                   ADD WS-AL-REFUND (WS-I) TO WS-RL-REFUND (WS-I)
               END-IF
           END-PERFORM
           ADD WS-TOTAL-CHARGES            TO WS-GRAND-CHARGES
           ADD WS-TOTAL-REFUNDS            TO WS-GRAND-REFUNDS
           ADD WS-DR-INT-GROSS             TO WS-GRAND-DR-INT
           ADD WS-DR-INT-UNARR             TO WS-GRAND-DR-UNARR
           ADD WS-CR-INT-GROSS             TO WS-GRAND-CR-INT
           IF WS-TOTAL-CHARGES - WS-TOTAL-REFUNDS
                   > WS-HIGH-CHARGE-LIMIT
               ADD 1                       TO WS-HIGH-CHARGE
               MOVE 'HIGH CHARGES - CUSTOMER TREATMENT REVIEW'
                                           TO WS-EXC-TEXT
               PERFORM 7100-REPORT-ACCOUNT
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
       7100-REPORT-ACCOUNT.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE ACM-SORT-CODE              TO DL-SORT-CODE
           MOVE ACM-ACCT-NO                TO DL-ACCT-NO
           MOVE ACM-CIF-NO                 TO DL-CIF-NO
           MOVE WS-PERIOD-FROM             TO DL-FROM
           MOVE WS-PERIOD-TO               TO DL-TO
           IF WS-EXC-TEXT (1:8) = 'CUSTOMER'
               MOVE ZERO                   TO DL-CHARGES DL-REFUNDS
           ELSE
               MOVE WS-ACCT-FROM           TO DL-FROM
               MOVE WS-TOTAL-CHARGES       TO DL-CHARGES
               MOVE WS-TOTAL-REFUNDS       TO DL-REFUNDS
           END-IF
           MOVE WS-EXC-TEXT                TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
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
      *    CHARGE TYPE ANALYSIS PAGE
           PERFORM 7000-REPORT-HEADINGS
           WRITE REPORT-RECORD             FROM WS-TYPE-HEADING
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE WS-CHG-TYPE (WS-I)     TO TY-TYPE
               MOVE WS-RL-ACCOUNTS (WS-I)  TO TY-ACCOUNTS
               MOVE WS-RL-COUNT (WS-I)     TO TY-COUNT
               MOVE WS-RL-AMOUNT (WS-I)    TO TY-AMOUNT
               MOVE WS-RL-REFUND (WS-I)    TO TY-REFUND
               WRITE REPORT-RECORD         FROM WS-TYPE-LINE
           END-PERFORM

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-ACCTS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NOT ELIGIBLE (TYPE / STATUS)' TO TL-LABEL
           MOVE WS-NOT-ELIGIBLE            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NOT ANNIVERSARY MONTH'    TO TL-LABEL
           MOVE WS-NOT-ANNIVERSARY         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'OPENED AFTER PERIOD END'  TO TL-LABEL
           MOVE WS-TOO-NEW                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'BUSINESS CUSTOMERS (NOT IN SCOPE)' TO TL-LABEL
           MOVE WS-BUSINESS-SKIPPED        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'DECEASED - SUPPRESSED'    TO TL-LABEL
           MOVE WS-DECEASED-SKIPPED        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SUMMARIES WRITTEN'        TO TL-LABEL
           MOVE WS-LETTERS-WRITTEN         TO TL-COUNT
           MOVE WS-GRAND-CHARGES           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH NIL CHARGES AND INTEREST' TO TL-LABEL
           MOVE WS-NIL-LETTERS             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH PART YEAR'     TO TL-LABEL
           MOVE WS-PART-YEAR               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REFUNDS SHOWN'            TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-GRAND-REFUNDS           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'DEBIT INTEREST CHARGED'   TO TL-LABEL
           MOVE WS-GRAND-DR-INT            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH UNARRANGED'    TO TL-LABEL
           MOVE WS-GRAND-DR-UNARR          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CREDIT INTEREST PAID (GROSS)' TO TL-LABEL
           MOVE WS-GRAND-CR-INT            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'HIGH CHARGE ACCOUNTS LISTED' TO TL-LABEL
           MOVE WS-HIGH-CHARGE             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CHARGE TYPES MAPPED TO OTHR' TO TL-LABEL
           MOVE WS-OTHER-TYPES             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCEPTION - CUSTOMER NOT ON CIF' TO TL-LABEL
           MOVE WS-CIF-NOT-FOUND           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCEPTION - REFUND EXCEEDS CHARGE' TO TL-LABEL
           MOVE WS-REFUND-EXCEPTIONS       TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE ACCTMAST-FILE CIFMAST-FILE PRDPARM-FILE LETTERS-FILE
                 REPORT-FILE
           DISPLAY 'CHGTAR10 - SUMMARIES WRITTEN ' WS-LETTERS-WRITTEN
           IF WS-CIF-NOT-FOUND > ZERO
           OR WS-REFUND-EXCEPTIONS > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
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
