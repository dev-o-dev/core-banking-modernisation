       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDDRM010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2008-09-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDDRM010                                            *
      * TITLE    : DORMANCY PROCESSING AND UNCLAIMED ASSETS            *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : DDDRM01 STEP DR020 (MONTHLY - FIRST BUSINESS DAY)   *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (VSAM KSDS, SEQ LOAD) *
      *            CIFMAST  - CIF CUSTOMER MASTER (VSAM KSDS, RANDOM)  *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ I-O)  *
      * OUTPUT   : LETTERS  - DORMANCY LETTER EXTRACT (FB 300)         *
      *            RECLAIM  - RECLAIM FUND CANDIDATES (FB 200)         *
      *            REPORT   - DORMANCY REPORT (FBA 133)                *
      *----------------------------------------------------------------*
      * AN OPEN ACCOUNT WITH NO CUSTOMER-INITIATED TRANSACTION FOR     *
      * PRD-DORMANT-MONTHS MONTHS BECOMES DORMANT:                     *
      *   - ACM-ACCT-STATUS 'D', ACM-DORMANT-DATE = TODAY,             *
      *     ACM-DORMANCY-IND 'D'                                       *
      *   - DEBITS ARE REFUSED ONLINE (PSMEMO1 REASON ADRM); A CREDIT  *
      *     REACTIVATES THE ACCOUNT (PSMEMO1 SETS STATUS 'O')          *
      * ONE MONTH BEFORE DORMANCY A WARNING LETTER IS SENT AND         *
      * ACM-DORMANCY-IND IS SET TO 'W'.                                *
      *                                                                *
      * "CUSTOMER-INITIATED" = ACM-LAST-CUST-TXN-DATE, SET BY PSEOD010 *
      * FOR TXN CODES IN PST-CUSTOMER-INITIATED (NOT INTEREST, TAX OR  *
      * CHARGES). ACCOUNTS OPENED BEFORE MARCH 2010 HAVE NO VALUE -    *
      * ACM-LAST-TXN-DATE IS USED INSTEAD (WHICH INCLUDES INTEREST, SO *
      * SAVINGS ACCOUNTS NEVER WENT DORMANT BEFORE 2010 - SEE THE      *
      * DORMANCY REVIEW PAPER DDA-2009-12).                            *
      *                                                                *
      * REACTIVATION: AN OPEN ACCOUNT WITH ACM-DORMANCY-IND 'D' WAS    *
      * REACTIVATED ONLINE BY A CREDIT - THE INDICATOR IS CLEARED AND  *
      * A REACTIVATION IS REPORTED (AS PROMISED IN PSMEMO1).           *
      *                                                                *
      * RECLAIM FUND: A DORMANT ACCOUNT WITH A CREDIT BALANCE THAT HAS *
      * HAD NO CUSTOMER ACTIVITY FOR PRD-RECLAIM-YEARS (15) YEARS IS   *
      * A CANDIDATE FOR TRANSFER UNDER THE DORMANT BANK AND BUILDING   *
      * SOCIETY ACCOUNTS ACT 2008. MEGABANK HAS NOT YET SIGNED UP TO   *
      * THE SCHEME - CANDIDATES ARE ONLY WRITTEN TO THE RECLAIM FILE   *
      * FOR THE UNCLAIMED ASSETS WORKING GROUP. NO MONEY MOVES.        *
      *                                                                *
      * EXCLUSIONS: CLOSED, PENDING CLOSE, FROZEN ACCOUNTS; LOANS AND  *
      * MORTGAGES; FIXED TERM DEPOSITS BEFORE MATURITY; CUSTOMERS      *
      * MARKED DECEASED (BEREAVEMENT TEAM HANDLES THEM).               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2008-09-15 JNB  ORIGINAL VERSION (DORMANT ACCOUNTS ACT 2008)   *
      * 2009-02-02 JNB  PRE-DORMANCY WARNING LETTER                    *
      * 2010-03-08 AOS  USES ACM-LAST-CUST-TXN-DATE (NEW FIELD)        *
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
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT LETTERS-FILE     ASSIGN TO LETTERS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LETTERS-STATUS.
           SELECT RECLAIM-FILE     ASSIGN TO RECLAIM
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RECLAIM-STATUS.
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
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(300).
      *
       FD  RECLAIM-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RECLAIM-RECORD                  PIC X(200).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDDRM010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-EOF              VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-RECLAIM-STATUS           PIC X(02).
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
           05  WS-CHANGED-IND              PIC X(01) VALUE 'N'.
               88  WS-ACCT-CHANGED             VALUE 'Y'.
           05  WS-CUST-READ-IND            PIC X(01) VALUE 'N'.
               88  WS-CUST-READ                VALUE 'Y'.
      *
       01  WS-DATE-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-R REDEFINES WS-BUS-DATE.
               10  WS-BD-CCYY              PIC 9(04).
               10  WS-BD-MM                PIC 9(02).
               10  WS-BD-DD                PIC 9(02).
           05  WS-ACTIVITY-DATE            PIC 9(08).
           05  WS-ACTIVITY-R REDEFINES WS-ACTIVITY-DATE.
               10  WS-AD-CCYY              PIC 9(04).
               10  WS-AD-MM                PIC 9(02).
               10  WS-AD-DD                PIC 9(02).
           05  WS-MONTHS-IDLE              PIC S9(05) COMP.
           05  WS-YEARS-IDLE               PIC S9(04) COMP.
           05  WS-DORMANT-MONTHS           PIC S9(05) COMP.
           05  WS-RECLAIM-YEARS            PIC S9(04) COMP.
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
      *
       01  WS-ACTION                       PIC X(10).
           88  WS-ACT-NONE                     VALUE SPACES.
           88  WS-ACT-WARN                     VALUE 'WARNING'.
           88  WS-ACT-DORMANT                  VALUE 'DORMANT'.
           88  WS-ACT-REACTIVATED              VALUE 'REACTIVATE'.
           88  WS-ACT-RECLAIM                  VALUE 'RECLAIM'.
           88  WS-ACT-EXCLUDED                 VALUE 'EXCLUDED'.
      *
      *---- PRODUCT TABLE (DORMANCY FIELDS ONLY) ----------------------*
       01  WS-PRODUCT-TABLE.
           05  WS-PROD-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-PROD-ENTRY               OCCURS 1 TO 300 TIMES
                                           DEPENDING ON WS-PROD-COUNT
                                           ASCENDING KEY WS-PT-PROD-CD
                                           INDEXED BY PT-IDX.
               10  WS-PT-PROD-CD           PIC X(10).
               10  WS-PT-SHORT-NAME        PIC X(18).
               10  WS-PT-DORMANT-MONTHS    PIC 9(03).
               10  WS-PT-DORMANT-MIN-BAL   PIC S9(13)V99 COMP-3.
               10  WS-PT-RECLAIM-YEARS     PIC 9(02).
               10  WS-PT-LETTER-IND        PIC X(01).
               10  WS-PT-DORMANT-COUNT     PIC S9(07) COMP-3.
               10  WS-PT-DORMANT-BAL       PIC S9(15)V99 COMP-3.
      *
      *---- LETTER EXTRACT (DOCUMENT SERVICES TEMPLATES DRMWRN01 AND   *
      *---- DRMNTF01)                                                  *
       01  WS-LETTER.
           05  LTR-TEMPLATE                PIC X(08).
           05  LTR-SORT-CODE               PIC X(06).
           05  LTR-ACCT-NO                 PIC X(08).
           05  LTR-CIF-NO                  PIC X(12).
           05  LTR-NAME                    PIC X(60).
           05  LTR-ADDR-LINE1              PIC X(40).
           05  LTR-ADDR-LINE2              PIC X(40).
           05  LTR-TOWN                    PIC X(40).
           05  LTR-POST-CD                 PIC X(10).
           05  LTR-PROD-NAME               PIC X(18).
           05  LTR-LAST-ACTIVITY           PIC 9(08).
           05  LTR-EFFECTIVE-DATE          PIC 9(08).
           05  LTR-BALANCE                 PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  FILLER                      PIC X(26).
      *
      *---- RECLAIM CANDIDATE RECORD ----------------------------------*
       01  WS-RECLAIM.
           05  RCL-SORT-CODE               PIC X(06).
           05  RCL-ACCT-NO                 PIC X(08).
           05  RCL-CIF-NO                  PIC X(12).
           05  RCL-PROD-CD                 PIC X(10).
           05  RCL-OPEN-DATE               PIC 9(08).
           05  RCL-LAST-ACTIVITY           PIC 9(08).
           05  RCL-DORMANT-DATE            PIC 9(08).
           05  RCL-YEARS-IDLE              PIC 9(03).
           05  RCL-LEDGER-BAL              PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  RCL-ACCRUED-INT             PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  RCL-CUST-NAME               PIC X(60).
           05  RCL-POST-CD                 PIC X(10).
           05  FILLER                      PIC X(27).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ELIGIBLE-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-WARN-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-DORMANT-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-REACTIVATE-COUNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-RECLAIM-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXCLUDE-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-LETTER-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ALREADY-DORMANT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-DORMANT-TOTAL-BAL        PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-RECLAIM-TOTAL-BAL        PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'DDDRM010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - DORMANCY AND UNCLAIMED ASSETS'.
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
           05  FILLER            PIC X(14) VALUE 'CIF NUMBER'.
           05  FILLER            PIC X(12) VALUE 'LAST ACTIVE'.
           05  FILLER            PIC X(07) VALUE 'MONTHS'.
           05  FILLER            PIC X(18) VALUE '    LEDGER BALANCE'.
           05  FILLER            PIC X(02) VALUE SPACES.
           05  FILLER            PIC X(11) VALUE 'ACTION'.
           05  FILLER            PIC X(38) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-LAST-ACTIVE              PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-MONTHS                   PIC ZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-BALANCE                  PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTE                     PIC X(38).
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
           PERFORM 2000-PROCESS-ACCOUNT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       CIFMAST-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                OUTPUT LETTERS-FILE
                       RECLAIM-FILE
                       REPORT-FILE
           IF NOT WS-PRDPARM-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-LETTERS-STATUS NOT = '00'
           OR WS-RECLAIM-STATUS NOT = '00'
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
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO HD1-BUS-DATE

           PERFORM 1100-LOAD-PRODUCTS
           CLOSE PRDPARM-FILE

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       1100-LOAD-PRODUCTS.
      *================================================================*
           MOVE ZERO                       TO WS-PROD-COUNT
           PERFORM 1110-READ-PRODUCT
           PERFORM UNTIL WS-END-OF-PRODUCTS
               IF WS-PROD-COUNT >= 300
                   MOVE '1100-LOAD-PRODUCTS' TO ABN-PARAGRAPH
                   MOVE 1004               TO ABN-ABEND-CODE
                   MOVE 'PRODUCT TABLE FULL'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               END-IF
               ADD 1                       TO WS-PROD-COUNT
               SET PT-IDX                  TO WS-PROD-COUNT
               MOVE PRD-PROD-CD        TO WS-PT-PROD-CD (PT-IDX)
               MOVE PRD-SHORT-NAME     TO WS-PT-SHORT-NAME (PT-IDX)
               MOVE PRD-DORMANT-MONTHS TO WS-PT-DORMANT-MONTHS (PT-IDX)
               MOVE PRD-DORMANT-MIN-BAL
                                     TO WS-PT-DORMANT-MIN-BAL (PT-IDX)
               MOVE PRD-RECLAIM-YEARS  TO WS-PT-RECLAIM-YEARS (PT-IDX)
               MOVE PRD-DORMANT-LETTER-IND
                                       TO WS-PT-LETTER-IND (PT-IDX)
               MOVE ZERO               TO WS-PT-DORMANT-COUNT (PT-IDX)
                                          WS-PT-DORMANT-BAL (PT-IDX)
      *        DEFAULTS IF PRODUCT CONTROL HAS NOT SET THE FIELDS.
               IF WS-PT-DORMANT-MONTHS (PT-IDX) = ZERO
                   MOVE 36             TO WS-PT-DORMANT-MONTHS (PT-IDX)
               END-IF
               IF WS-PT-RECLAIM-YEARS (PT-IDX) = ZERO
                   MOVE 15             TO WS-PT-RECLAIM-YEARS (PT-IDX)
               END-IF
               PERFORM 1110-READ-PRODUCT
           END-PERFORM
           .
      *================================================================*
       1110-READ-PRODUCT.
      *================================================================*
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN WS-PRDPARM-OK
                   CONTINUE
               WHEN WS-PRDPARM-EOF
                   SET WS-END-OF-PRODUCTS  TO TRUE
               WHEN OTHER
                   MOVE '1110-READ-PRODUCT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PRDPARM READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-PRDPARM-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           MOVE SPACES                     TO WS-ACTION
                                              DL-NOTE
           MOVE 'N'                        TO WS-CHANGED-IND
                                              WS-CUST-READ-IND

           EVALUATE TRUE
               WHEN ACM-CLOSED
               WHEN ACM-PENDING-CLOSE
               WHEN ACM-LOAN
               WHEN ACM-MORTGAGE
                   CONTINUE
               WHEN ACM-FROZEN
                   ADD 1                   TO WS-EXCLUDE-COUNT
               WHEN ACM-OPEN AND ACM-DORMANCY-IND = 'D'
                   PERFORM 2500-REACTIVATED
               WHEN ACM-DORMANT
                   ADD 1                   TO WS-ALREADY-DORMANT
                   PERFORM 2050-FIND-PRODUCT
                   PERFORM 2100-CALC-IDLE
                   PERFORM 2600-CHECK-RECLAIM THRU 2600-EXIT
               WHEN ACM-OPEN
                   PERFORM 2050-FIND-PRODUCT
                   IF WS-PROD-FOUND
                       PERFORM 2100-CALC-IDLE
                       PERFORM 2200-ASSESS-OPEN THRU 2200-EXIT
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-ACCT-CHANGED
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '2000-PROCESS-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
           END-IF

           IF NOT WS-ACT-NONE
               PERFORM 7100-REPORT-DETAIL
           END-IF

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       2050-FIND-PRODUCT.
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
       2100-CALC-IDLE.
      *================================================================*
      *    LAST ACTIVITY: CUSTOMER-INITIATED DATE IF HELD, ELSE THE LAST
      *    TRANSACTION DATE, ELSE THE OPEN DATE.
           EVALUATE TRUE
               WHEN ACM-LAST-CUST-TXN-DATE NUMERIC
               AND  ACM-LAST-CUST-TXN-DATE > ZERO
                   MOVE ACM-LAST-CUST-TXN-DATE TO WS-ACTIVITY-DATE
               WHEN ACM-LAST-TXN-DATE NUMERIC
               AND  ACM-LAST-TXN-DATE > ZERO
                   MOVE ACM-LAST-TXN-DATE  TO WS-ACTIVITY-DATE
               WHEN OTHER
                   MOVE ACM-OPEN-DATE      TO WS-ACTIVITY-DATE
           END-EVALUATE

      *    WHOLE MONTHS BETWEEN THE ACTIVITY DATE AND TODAY.
           COMPUTE WS-MONTHS-IDLE =
                   (WS-BD-CCYY - WS-AD-CCYY) * 12
                 + (WS-BD-MM - WS-AD-MM)
           IF WS-BD-DD < WS-AD-DD
               SUBTRACT 1                  FROM WS-MONTHS-IDLE
           END-IF
           IF WS-MONTHS-IDLE < ZERO
               MOVE ZERO                   TO WS-MONTHS-IDLE
           END-IF
           DIVIDE WS-MONTHS-IDLE BY 12 GIVING WS-YEARS-IDLE
           .
      *================================================================*
       2200-ASSESS-OPEN.
      *================================================================*
           MOVE WS-PT-DORMANT-MONTHS (PT-IDX) TO WS-DORMANT-MONTHS

      *    FIXED TERM DEPOSITS ARE NOT DORMANT BEFORE MATURITY.
           IF ACM-FIXED-TERM
           AND ACM-MATURITY-DATE > WS-BUS-DATE
               GO TO 2200-EXIT
           END-IF

      *    ZERO-BALANCE ACCOUNTS ARE NOT MADE DORMANT - THEY GO TO THE
      *    NIL BALANCE CLOSURE REVIEW INSTEAD (NOT AUTOMATED).
           IF ACM-LEDGER-BAL < WS-PT-DORMANT-MIN-BAL (PT-IDX)
           OR ACM-LEDGER-BAL = ZERO
               GO TO 2200-EXIT
           END-IF

           ADD 1                           TO WS-ELIGIBLE-COUNT

           EVALUATE TRUE
               WHEN WS-MONTHS-IDLE >= WS-DORMANT-MONTHS
                   PERFORM 2300-MAKE-DORMANT THRU 2300-EXIT
               WHEN WS-MONTHS-IDLE = WS-DORMANT-MONTHS - 1
               AND  ACM-DORMANCY-IND NOT = 'W'
                   PERFORM 2400-SEND-WARNING THRU 2400-EXIT
               WHEN WS-MONTHS-IDLE < WS-DORMANT-MONTHS - 1
               AND  ACM-DORMANCY-IND = 'W'
      *            ACTIVITY SINCE THE WARNING - CLEAR IT.
                   MOVE SPACE              TO ACM-DORMANCY-IND
                   SET WS-ACCT-CHANGED     TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-MAKE-DORMANT.
      *================================================================*
           PERFORM 2800-READ-CUSTOMER THRU 2800-EXIT
           IF WS-CUST-READ AND CIF-DECEASED
               SET WS-ACT-EXCLUDED         TO TRUE
               MOVE 'CUSTOMER DECEASED - BEREAVEMENT TEAM'
                                           TO DL-NOTE
               ADD 1                       TO WS-EXCLUDE-COUNT
               GO TO 2300-EXIT
           END-IF

           SET ACM-DORMANT                 TO TRUE
           MOVE 'D'                        TO ACM-DORMANCY-IND
           MOVE WS-BUS-DATE                TO ACM-DORMANT-DATE
           SET WS-ACCT-CHANGED             TO TRUE
           SET WS-ACT-DORMANT              TO TRUE

           ADD 1                           TO WS-DORMANT-COUNT
           ADD ACM-LEDGER-BAL              TO WS-DORMANT-TOTAL-BAL
           ADD 1                         TO WS-PT-DORMANT-COUNT (PT-IDX)
           ADD ACM-LEDGER-BAL            TO WS-PT-DORMANT-BAL (PT-IDX)

           IF WS-PT-LETTER-IND (PT-IDX) = 'Y'
               MOVE 'DRMNTF01'             TO LTR-TEMPLATE
               PERFORM 2900-WRITE-LETTER THRU 2900-EXIT
           END-IF
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2400-SEND-WARNING.
      *================================================================*
           PERFORM 2800-READ-CUSTOMER THRU 2800-EXIT
           IF WS-CUST-READ AND CIF-DECEASED
               GO TO 2400-EXIT
           END-IF
           MOVE 'W'                        TO ACM-DORMANCY-IND
           SET WS-ACCT-CHANGED             TO TRUE
           SET WS-ACT-WARN                 TO TRUE
           ADD 1                           TO WS-WARN-COUNT
           MOVE 'WARNING LETTER - DORMANT NEXT MONTH'
                                           TO DL-NOTE
           IF WS-PT-LETTER-IND (PT-IDX) = 'Y'
               MOVE 'DRMWRN01'             TO LTR-TEMPLATE
               PERFORM 2900-WRITE-LETTER THRU 2900-EXIT
           END-IF
           .
       2400-EXIT.
           EXIT.
      *================================================================*
       2500-REACTIVATED.
      *================================================================*
           MOVE SPACE                      TO ACM-DORMANCY-IND
           MOVE ZERO                       TO ACM-DORMANT-DATE
           SET WS-ACCT-CHANGED             TO TRUE
           SET WS-ACT-REACTIVATED          TO TRUE
           ADD 1                           TO WS-REACTIVATE-COUNT
           MOVE 'REACTIVATED BY CREDIT (PSMEMO1)' TO DL-NOTE
           MOVE ZERO                       TO WS-MONTHS-IDLE
           .
      *================================================================*
       2600-CHECK-RECLAIM.
      *================================================================*
           IF NOT WS-PROD-FOUND
               GO TO 2600-EXIT
           END-IF
           MOVE WS-PT-RECLAIM-YEARS (PT-IDX) TO WS-RECLAIM-YEARS
           IF WS-YEARS-IDLE < WS-RECLAIM-YEARS
           OR ACM-LEDGER-BAL NOT > ZERO
               GO TO 2600-EXIT
           END-IF

           PERFORM 2800-READ-CUSTOMER THRU 2800-EXIT
           IF WS-CUST-READ AND CIF-DECEASED
               GO TO 2600-EXIT
           END-IF

           MOVE SPACES                     TO WS-RECLAIM
           MOVE ACM-SORT-CODE              TO RCL-SORT-CODE
           MOVE ACM-ACCT-NO                TO RCL-ACCT-NO
           MOVE ACM-CIF-NO                 TO RCL-CIF-NO
           MOVE ACM-PROD-CD                TO RCL-PROD-CD
           MOVE ACM-OPEN-DATE              TO RCL-OPEN-DATE
           MOVE WS-ACTIVITY-DATE           TO RCL-LAST-ACTIVITY
           MOVE ACM-DORMANT-DATE           TO RCL-DORMANT-DATE
           MOVE WS-YEARS-IDLE              TO RCL-YEARS-IDLE
           MOVE ACM-LEDGER-BAL             TO RCL-LEDGER-BAL
           MOVE ACM-ACCRUED-INT-CR         TO RCL-ACCRUED-INT
           IF WS-CUST-READ
               STRING CIF-TITLE     DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-FIRST-NM  DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-LAST-NM   DELIMITED BY '  '
                      INTO RCL-CUST-NAME
               END-STRING
               MOVE CIF-POST-CD            TO RCL-POST-CD
           END-IF
           WRITE RECLAIM-RECORD            FROM WS-RECLAIM
           IF WS-RECLAIM-STATUS NOT = '00'
               MOVE '2600-CHECK-RECLAIM'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'RECLAIM WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-RECLAIM-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

           SET WS-ACT-RECLAIM              TO TRUE
           MOVE 'RECLAIM FUND CANDIDATE - NO ACTION TAKEN'
                                           TO DL-NOTE
           ADD 1                           TO WS-RECLAIM-COUNT
           ADD ACM-LEDGER-BAL              TO WS-RECLAIM-TOTAL-BAL
           .
       2600-EXIT.
           EXIT.
      *================================================================*
       2800-READ-CUSTOMER.
      *================================================================*
           IF WS-CUST-READ
               GO TO 2800-EXIT
           END-IF
           MOVE ACM-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   SET WS-CUST-READ        TO TRUE
               WHEN WS-CIFMAST-NOTFND
                   MOVE 'N'                TO WS-CUST-READ-IND
                   MOVE 'CIF NOT FOUND'    TO DL-NOTE
               WHEN OTHER
                   MOVE '2800-READ-CUSTOMER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE ACM-CIF-NO         TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       2800-EXIT.
           EXIT.
      *================================================================*
       2900-WRITE-LETTER.
      *================================================================*
      *    ON ENTRY LTR-TEMPLATE IS SET AND THE CUSTOMER HAS BEEN READ.
           IF NOT WS-CUST-READ
               GO TO 2900-EXIT
           END-IF
           MOVE ACM-SORT-CODE              TO LTR-SORT-CODE
           MOVE ACM-ACCT-NO                TO LTR-ACCT-NO
           MOVE ACM-CIF-NO                 TO LTR-CIF-NO
           MOVE SPACES                     TO LTR-NAME
           STRING CIF-TITLE     DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-FIRST-NM  DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-LAST-NM   DELIMITED BY '  '
                  INTO LTR-NAME
           END-STRING
           MOVE CIF-ADDR-LINE1             TO LTR-ADDR-LINE1
           MOVE CIF-ADDR-LINE2             TO LTR-ADDR-LINE2
           MOVE CIF-TOWN                   TO LTR-TOWN
           MOVE CIF-POST-CD                TO LTR-POST-CD
           MOVE WS-PT-SHORT-NAME (PT-IDX)  TO LTR-PROD-NAME
           MOVE WS-ACTIVITY-DATE           TO LTR-LAST-ACTIVITY
           MOVE WS-BUS-DATE                TO LTR-EFFECTIVE-DATE
           MOVE ACM-LEDGER-BAL             TO LTR-BALANCE
           WRITE LETTERS-RECORD            FROM WS-LETTER
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '2900-WRITE-LETTER'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-LETTER-COUNT
           .
       2900-EXIT.
           EXIT.
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           PERFORM VARYING PT-IDX FROM 1 BY 1
                   UNTIL PT-IDX > WS-PROD-COUNT
               IF WS-PT-DORMANT-COUNT (PT-IDX) > ZERO
                   MOVE SPACES             TO TL-LABEL
                   STRING 'NEW DORMANT - ' WS-PT-PROD-CD (PT-IDX)
                          DELIMITED BY SIZE
                          INTO TL-LABEL
                   END-STRING
                   MOVE WS-PT-DORMANT-COUNT (PT-IDX) TO TL-COUNT
                   MOVE WS-PT-DORMANT-BAL (PT-IDX)   TO TL-AMOUNT
                   WRITE REPORT-RECORD     FROM WS-TOTAL-LINE
               END-IF
           END-PERFORM

           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ELIGIBLE FOR DORMANCY TEST' TO TL-LABEL
           MOVE WS-ELIGIBLE-COUNT          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'WARNING LETTERS'          TO TL-LABEL
           MOVE WS-WARN-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MADE DORMANT'             TO TL-LABEL
           MOVE WS-DORMANT-COUNT           TO TL-COUNT
           MOVE WS-DORMANT-TOTAL-BAL       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ALREADY DORMANT'          TO TL-LABEL
           MOVE WS-ALREADY-DORMANT         TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REACTIVATED SINCE LAST RUN' TO TL-LABEL
           MOVE WS-REACTIVATE-COUNT        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RECLAIM FUND CANDIDATES'  TO TL-LABEL
           MOVE WS-RECLAIM-COUNT           TO TL-COUNT
           MOVE WS-RECLAIM-TOTAL-BAL       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCLUDED (FROZEN / DECEASED)' TO TL-LABEL
           MOVE WS-EXCLUDE-COUNT           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LETTERS WRITTEN'          TO TL-LABEL
           MOVE WS-LETTER-COUNT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE CIFMAST-FILE
                 ACCTMAST-FILE
                 LETTERS-FILE
                 RECLAIM-FILE
                 REPORT-FILE

           DISPLAY 'DDDRM010 - ACCOUNTS READ   ' WS-READ-COUNT
           DISPLAY 'DDDRM010 - MADE DORMANT    ' WS-DORMANT-COUNT
           DISPLAY 'DDDRM010 - WARNINGS        ' WS-WARN-COUNT
           DISPLAY 'DDDRM010 - REACTIVATED     ' WS-REACTIVATE-COUNT
           DISPLAY 'DDDRM010 - RECLAIM CANDS   ' WS-RECLAIM-COUNT
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
           MOVE ACM-PROD-CD                TO DL-PROD-CD
           MOVE ACM-CIF-NO                 TO DL-CIF-NO
           MOVE WS-ACTIVITY-DATE           TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO DL-LAST-ACTIVE
           MOVE WS-MONTHS-IDLE             TO DL-MONTHS
           MOVE ACM-LEDGER-BAL             TO DL-BALANCE
           MOVE WS-ACTION                  TO DL-ACTION
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
       8900-FORMAT-DATE.
      *================================================================*
           MOVE SPACES                     TO WS-DATE-DISPLAY
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-DATE-DISPLAY
           END-STRING
           .
      *
           COPY ABNDPROC.
