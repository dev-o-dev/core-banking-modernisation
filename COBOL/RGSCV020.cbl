       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RGSCV020.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-01-04.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : RGSCV020                                            *
      * TITLE    : FSCS SINGLE CUSTOMER VIEW - FULL EXTRACT            *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : RGSCV01 STEP RG020                                  *
      *----------------------------------------------------------------*
      * INPUT    : CIFMAST  - CIF CUSTOMER MASTER (SEQUENTIAL)         *
      *            ACCTCIF  - ACCOUNT MASTER VIA CIF ALTERNATE INDEX   *
      *                       PATH (DYNAMIC, START/READ NEXT)          *
      *            PRDPARM  - PRODUCT PARAMETERS (RANDOM)              *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : SCVFILE  - SCV FILE FOR FSCS - RGSCVREC             *
      *            DQREPORT - DATA QUALITY EXCEPTIONS (FBA 133)        *
      *            REPORT   - SCV SUMMARY (FBA 133)                    *
      *----------------------------------------------------------------*
      * REPLACES THE STUB RGSCV010 (CUSTOMERS WITHOUT BALANCES), WHICH *
      * REMAINS IN THE LIBRARY FOR THE MONTHLY CUSTOMER COUNT RETURN.  *
      *                                                                *
      * FOR EACH ACTIVE / BLOCKED / DECEASED CUSTOMER:                 *
      *   - EVERY DEPOSIT ACCOUNT (CURR, SAVE, ISA, FTD) NOT CLOSED IS *
      *     LISTED; LOANS AND MORTGAGES ARE NOT DEPOSITS               *
      *   - ELIGIBLE BALANCE = LEDGER BALANCE + ACCRUED CREDIT         *
      *     INTEREST (NEGATIVE BALANCES COUNT AS ZERO - FSCS DOES NOT  *
      *     NET OFF OVERDRAFTS ON THE SCV FILE; SET-OFF IS APPLIED BY  *
      *     FSCS AT PAYOUT)                                            *
      *   - AGGREGATE PER CUSTOMER; COMPENSATABLE = MIN(AGGREGATE,     *
      *     WS-COMP-LIMIT)                                             *
      *                                                                *
      * EXCLUSIONS (CUSTOMER STILL WRITTEN, FLAGGED):                  *
      *   LEGL  ACCOUNT FROZEN BY LEGAL ORDER (ACM-FROZEN)             *
      *   BENE  BENEFICIARY ACCOUNT (CLIENT MONEY - SEGMENT 'TRUST')   *
      *   BUSL  LARGE BUSINESS (SEGMENT 'CORPORATE' - NOT ELIGIBLE)    *
      *                                                                *
      * DATA QUALITY FLAGS (SCVC-DQ-FLAGS, ONE BYTE EACH 'Y'/'N'):     *
      *   1 NO DATE OF BIRTH        2 NO NATIONAL INSURANCE NUMBER     *
      *   3 NO POST CODE            4 NO ADDRESS LINE 1                *
      *   5 KYC NOT VERIFIED        6 NO CONTACT TELEPHONE             *
      * JOINT ACCOUNTS: THE HOST HOLDS ONE CIF PER ACCOUNT (PIR        *
      * 1999/007) SO JOINT HOLDERS CANNOT BE SPLIT 50/50 AS FSCS       *
      * REQUIRES. SCVA-HOLDER-COUNT IS ALWAYS 1. KNOWN GAP REPORTED TO *
      * THE FSA IN THE SCV IMPLEMENTATION PLAN.                        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-01-04 JNB  ORIGINAL VERSION (SCV PROJECT)                 *
      * 2010-11-29 JNB  COMPENSATION LIMIT 85,000 FROM 31/12/2010 -    *
      *                 READ FROM SYSIN PARM, DEFAULT 50,000           *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT ACCTCIF-FILE     ASSIGN TO ACCTCIF
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS ACM-KEY
                                   ALTERNATE RECORD KEY IS ACM-CIF-NO
                                       WITH DUPLICATES
                                   FILE STATUS IS WS-ACCTCIF-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT SCVFILE-FILE     ASSIGN TO SCVFILE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-SCVFILE-STATUS.
           SELECT DQREPORT-FILE    ASSIGN TO DQREPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DQREPORT-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTCIF-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  SCVFILE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SCVFILE-RECORD                  PIC X(400).
       FD  DQREPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DQREPORT-RECORD                 PIC X(133).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD.
           05  PARM-KEYWORD                PIC X(11).
               88  PARM-COMP-LIMIT             VALUE 'COMPLIMIT= '.
           05  PARM-VALUE                  PIC 9(07)V99.
           05  FILLER                      PIC X(60).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'RGSCV020'.
      *
       01  WS-CONSTANTS.
           05  WS-FIRM-FRN                 PIC X(06) VALUE '123456'.
           05  WS-FIRM-NAME                PIC X(40)
               VALUE 'MEGABANK PLC'.
           05  WS-SPEC-VERSION             PIC X(05) VALUE '1.0  '.
      *
       01  WS-FILE-STATUSES.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-EOF              VALUE '10'.
           05  WS-ACCTCIF-STATUS           PIC X(02).
               88  WS-ACCTCIF-OK               VALUE '00' '02'.
               88  WS-ACCTCIF-EOF              VALUE '10'.
               88  WS-ACCTCIF-NOTFND           VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-SCVFILE-STATUS           PIC X(02).
           05  WS-DQREPORT-STATUS          PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
           05  WS-PARMIN-STATUS            PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CUSTOMERS         VALUE 'Y'.
           05  WS-END-ACCTS-IND            PIC X(01).
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-COMP-LIMIT               PIC S9(07)V99 COMP-3
                                           VALUE +50000.00.
           05  WS-SCV-ID                   PIC 9(12) VALUE ZERO.
           05  WS-CUST-AGG                 PIC S9(13)V99 COMP-3.
           05  WS-CUST-COMP                PIC S9(13)V99 COMP-3.
           05  WS-CUST-ACCTS               PIC 9(03).
           05  WS-ELIGIBLE                 PIC S9(13)V99 COMP-3.
           05  WS-EXCL-REASON              PIC X(04).
           05  WS-DQ-FLAGS.
               10  WS-DQ-FLAG              PIC X(01) OCCURS 10 TIMES.
           05  WS-DQ-ANY-IND               PIC X(01).
               88  WS-DQ-ANY                   VALUE 'Y'.
           05  WS-DQ-SUB                   PIC S9(04) COMP.
           05  WS-LAST-PROD                PIC X(10) VALUE LOW-VALUES.
      *
      *---- ACCOUNT RECORDS FOR ONE CUSTOMER --------------------------*
      *    THE CUSTOMER RECORD (WITH TOTALS) MUST PRECEDE ITS ACCOUNT
      *    RECORDS IN THE FILE, SO ACCOUNTS ARE BUFFERED.
       01  WS-ACCT-BUFFER.
           05  WS-AB-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-AB-ENTRY                 OCCURS 99 TIMES
                                           INDEXED BY AB-IDX.
               10  WS-AB-RECORD            PIC X(400).
      *
       01  WS-DQ-NAMES.
           05  FILLER  PIC X(20) VALUE 'NO DATE OF BIRTH'.
           05  FILLER  PIC X(20) VALUE 'NO NI NUMBER'.
           05  FILLER  PIC X(20) VALUE 'NO POST CODE'.
           05  FILLER  PIC X(20) VALUE 'NO ADDRESS LINE 1'.
           05  FILLER  PIC X(20) VALUE 'KYC NOT VERIFIED'.
           05  FILLER  PIC X(20) VALUE 'NO TELEPHONE'.
       01  WS-DQ-NAME-TABLE REDEFINES WS-DQ-NAMES.
           05  WS-DQ-NAME                  PIC X(20) OCCURS 6 TIMES.
      *
       01  WS-COUNTERS.
           05  WS-CUST-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CUST-WRITTEN             PIC S9(09) COMP-3 VALUE 0.
           05  WS-CUST-NO-DEPOSITS         PIC S9(09) COMP-3 VALUE 0.
           05  WS-CUST-EXCLUDED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-CUST-OVER-LIMIT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCT-WRITTEN             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RECORDS-WRITTEN          PIC S9(09) COMP-3 VALUE 0.
           05  WS-DQ-CUSTOMERS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DQ-COUNT                 PIC S9(09) COMP-3
                                           OCCURS 6 TIMES.
           05  WS-TOTAL-BALANCE            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-COMP               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-BUFFER-OVERFLOW          PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-DQ-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'RGSCV020'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - SCV DATA QUALITY EXCEPTIONS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  DH1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  DH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-DQ-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(14) VALUE 'CIF NUMBER'.
           05  FILLER            PIC X(42) VALUE 'NAME'.
           05  FILLER            PIC X(18) VALUE '   ELIGIBLE BAL'.
           05  FILLER            PIC X(58) VALUE 'EXCEPTIONS'.
       01  WS-DQ-LINE.
           05  DQ-CC                       PIC X(01) VALUE ' '.
           05  DQ-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DQ-NAME                     PIC X(40).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DQ-BALANCE                  PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DQ-TEXT                     PIC X(57).
       01  WS-SUM-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'RGSCV020'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - FSCS SINGLE CUSTOMER VIEW SUMMARY'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  SH1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(17) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY RGSCVREC.
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
           PERFORM 2000-PROCESS-CUSTOMER
               UNTIL WS-END-OF-CUSTOMERS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CIFMAST-FILE
                       ACCTCIF-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                       PARMIN-FILE
                OUTPUT SCVFILE-FILE
                       DQREPORT-FILE
                       REPORT-FILE
           IF NOT WS-CIFMAST-OK
           OR NOT WS-ACCTCIF-OK
           OR NOT WS-PRDPARM-OK
           OR WS-SCVFILE-STATUS NOT = '00'
           OR WS-DQREPORT-STATUS NOT = '00'
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
                  INTO DH1-BUS-DATE
           END-STRING
           MOVE DH1-BUS-DATE               TO SH1-BUS-DATE

      *    OPTIONAL PARM CARD: COMPLIMIT= NNNNNNNNN (PENCE).
           IF WS-PARMIN-STATUS = '00'
               READ PARMIN-FILE
               IF WS-PARMIN-STATUS = '00'
               AND PARM-COMP-LIMIT
               AND PARM-VALUE NUMERIC
                   MOVE PARM-VALUE         TO WS-COMP-LIMIT
               END-IF
               CLOSE PARMIN-FILE
           END-IF
           DISPLAY 'RGSCV020 - COMPENSATION LIMIT ' WS-COMP-LIMIT

           PERFORM VARYING WS-DQ-SUB FROM 1 BY 1 UNTIL WS-DQ-SUB > 6
               MOVE ZERO                   TO WS-DQ-COUNT (WS-DQ-SUB)
           END-PERFORM

      *---- FILE HEADER -----------------------------------------------*
           MOVE SPACES                     TO SCV-RECORD
           SET SCV-HEADER                  TO TRUE
           MOVE ZERO                       TO SCV-SCV-ID
           MOVE WS-FIRM-FRN                TO SCVH-FIRM-FRN
           MOVE WS-FIRM-NAME               TO SCVH-FIRM-NAME
           MOVE FUNCTION CURRENT-DATE (1:8) TO SCVH-EXTRACT-DATE
           MOVE FUNCTION CURRENT-DATE (9:6) TO SCVH-EXTRACT-TIME
           MOVE WS-BUS-DATE                TO SCVH-BALANCE-DATE
           MOVE WS-SPEC-VERSION            TO SCVH-SPEC-VERSION
           MOVE WS-COMP-LIMIT              TO SCVH-COMP-LIMIT
           PERFORM 2900-WRITE-SCV

           PERFORM 8000-READ-CUSTOMER
           .
      *================================================================*
       2000-PROCESS-CUSTOMER.
      *================================================================*
           IF CIF-ACTIVE OR CIF-BLOCKED OR CIF-DECEASED
               PERFORM 2100-COLLECT-ACCOUNTS THRU 2100-EXIT
               IF WS-AB-COUNT = ZERO
                   ADD 1                   TO WS-CUST-NO-DEPOSITS
               ELSE
                   PERFORM 2200-ASSESS-CUSTOMER
                   PERFORM 2300-WRITE-CUSTOMER
               END-IF
           END-IF
           PERFORM 8000-READ-CUSTOMER
           .
      *================================================================*
       2100-COLLECT-ACCOUNTS.
      *================================================================*
           MOVE ZERO                       TO WS-AB-COUNT
                                              WS-CUST-AGG
                                              WS-CUST-ACCTS
           MOVE SPACES                     TO WS-EXCL-REASON
           MOVE 'N'                        TO WS-END-ACCTS-IND

           MOVE CIF-NO                     TO ACM-CIF-NO
           START ACCTCIF-FILE KEY = ACM-CIF-NO
           IF WS-ACCTCIF-NOTFND
               GO TO 2100-EXIT
           END-IF
           IF NOT WS-ACCTCIF-OK
               MOVE '2100-COLLECT-ACCOUNTS' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTCIF START FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTCIF-STATUS      TO ABN-FILE-STATUS
               MOVE CIF-NO                 TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           PERFORM UNTIL WS-END-OF-ACCOUNTS
               READ ACCTCIF-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN WS-ACCTCIF-EOF
                       SET WS-END-OF-ACCOUNTS TO TRUE
                   WHEN NOT WS-ACCTCIF-OK
                       MOVE '2100-COLLECT-ACCOUNTS' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTCIF READ FAILED' TO ABN-MESSAGE
                       MOVE WS-ACCTCIF-STATUS TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
                   WHEN ACM-CIF-NO NOT = CIF-NO
                       SET WS-END-OF-ACCOUNTS TO TRUE
                   WHEN ACM-CLOSED
                   WHEN ACM-LOAN
                   WHEN ACM-MORTGAGE
                       CONTINUE
                   WHEN OTHER
                       PERFORM 2110-BUFFER-ACCOUNT THRU 2110-EXIT
               END-EVALUATE
           END-PERFORM
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2110-BUFFER-ACCOUNT.
      *================================================================*
           IF WS-AB-COUNT >= 99
               ADD 1                       TO WS-BUFFER-OVERFLOW
               GO TO 2110-EXIT
           END-IF

           COMPUTE WS-ELIGIBLE = ACM-LEDGER-BAL + ACM-ACCRUED-INT-CR
           IF WS-ELIGIBLE < ZERO
               MOVE ZERO                   TO WS-ELIGIBLE
           END-IF

           IF ACM-PROD-CD NOT = WS-LAST-PROD
               MOVE ACM-PROD-CD            TO PRD-PROD-CD
                                              WS-LAST-PROD
               READ PRDPARM-FILE
               IF NOT WS-PRDPARM-OK
                   MOVE ACM-PROD-CD        TO PRD-DESCRIPTION
               END-IF
           END-IF

           MOVE SPACES                     TO SCV-RECORD
           SET SCV-ACCOUNT                 TO TRUE
           MOVE ACM-SORT-CODE              TO SCVA-SORT-CODE
           MOVE ACM-ACCT-NO                TO SCVA-ACCT-NO
           MOVE ACM-IBAN                   TO SCVA-IBAN
           MOVE ACM-ACCT-TYPE-CD           TO SCVA-PRODUCT-TYPE
           MOVE PRD-DESCRIPTION            TO SCVA-PRODUCT-NAME
           MOVE ACM-ACCT-STATUS            TO SCVA-ACCT-STATUS
           MOVE 1                          TO SCVA-HOLDER-COUNT
           MOVE ACM-CCY-CD                 TO SCVA-CCY-CD
           MOVE ACM-LEDGER-BAL             TO SCVA-LEDGER-BAL
           MOVE ACM-ACCRUED-INT-CR         TO SCVA-ACCRUED-INT
           MOVE WS-ELIGIBLE                TO SCVA-ELIGIBLE-BAL
           MOVE 'N'                        TO SCVA-BENEFICIARY-IND
                                              SCVA-LEGAL-HOLD-IND
                                              SCVA-DORMANT-IND
           IF ACM-FROZEN
               MOVE 'Y'                    TO SCVA-LEGAL-HOLD-IND
               MOVE 'LEGL'                 TO WS-EXCL-REASON
           END-IF
           IF ACM-DORMANT
               MOVE 'Y'                    TO SCVA-DORMANT-IND
           END-IF
           IF CIF-SEGMENT-CD = 'TRUST'
               MOVE 'Y'                    TO SCVA-BENEFICIARY-IND
           END-IF

           ADD 1                           TO WS-AB-COUNT
           SET AB-IDX                      TO WS-AB-COUNT
           MOVE SCV-RECORD                 TO WS-AB-RECORD (AB-IDX)
           ADD WS-ELIGIBLE                 TO WS-CUST-AGG
           ADD 1                           TO WS-CUST-ACCTS
           .
       2110-EXIT.
           EXIT.
      *================================================================*
       2200-ASSESS-CUSTOMER.
      *================================================================*
           EVALUATE TRUE
               WHEN CIF-SEGMENT-CD = 'CORPORATE'
                   MOVE 'BUSL'             TO WS-EXCL-REASON
               WHEN CIF-SEGMENT-CD = 'TRUST'
                   MOVE 'BENE'             TO WS-EXCL-REASON
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-EXCL-REASON NOT = SPACES
               MOVE ZERO                   TO WS-CUST-COMP
               ADD 1                       TO WS-CUST-EXCLUDED
           ELSE
               IF WS-CUST-AGG > WS-COMP-LIMIT
                   MOVE WS-COMP-LIMIT      TO WS-CUST-COMP
                   ADD 1                   TO WS-CUST-OVER-LIMIT
               ELSE
                   MOVE WS-CUST-AGG        TO WS-CUST-COMP
               END-IF
           END-IF

      *---- DATA QUALITY ----------------------------------------------*
           MOVE ALL 'N'                    TO WS-DQ-FLAGS
           MOVE 'N'                        TO WS-DQ-ANY-IND
           IF CIF-PERSONAL
               IF CIF-DOB NOT NUMERIC OR CIF-DOB = ZERO
                   MOVE 'Y'                TO WS-DQ-FLAG (1)
               END-IF
               IF CIF-NAT-INS-NO = SPACES
                   MOVE 'Y'                TO WS-DQ-FLAG (2)
               END-IF
           END-IF
           IF CIF-POST-CD = SPACES
               MOVE 'Y'                    TO WS-DQ-FLAG (3)
           END-IF
           IF CIF-ADDR-LINE1 = SPACES
               MOVE 'Y'                    TO WS-DQ-FLAG (4)
           END-IF
           IF NOT CIF-KYC-VERIFIED
               MOVE 'Y'                    TO WS-DQ-FLAG (5)
           END-IF
           IF CIF-HOME-TEL = SPACES AND CIF-MOBILE-TEL = SPACES
               MOVE 'Y'                    TO WS-DQ-FLAG (6)
           END-IF
           PERFORM VARYING WS-DQ-SUB FROM 1 BY 1 UNTIL WS-DQ-SUB > 6
               IF WS-DQ-FLAG (WS-DQ-SUB) = 'Y'
                   SET WS-DQ-ANY           TO TRUE
                   ADD 1                   TO WS-DQ-COUNT (WS-DQ-SUB)
               END-IF
           END-PERFORM
           IF WS-DQ-ANY
               ADD 1                       TO WS-DQ-CUSTOMERS
               PERFORM 7100-DQ-DETAIL
           END-IF
           .
      *================================================================*
       2300-WRITE-CUSTOMER.
      *================================================================*
           ADD 1                           TO WS-SCV-ID

           MOVE SPACES                     TO SCV-RECORD
           SET SCV-CUSTOMER                TO TRUE
           MOVE WS-SCV-ID                  TO SCV-SCV-ID
           IF CIF-BUSINESS
               STRING CIF-FIRST-NM DELIMITED BY '  '
                      ' '          DELIMITED BY SIZE
                      CIF-LAST-NM  DELIMITED BY '  '
                      INTO SCVC-COMPANY-NAME
               END-STRING
           ELSE
               MOVE CIF-TITLE              TO SCVC-TITLE
               MOVE CIF-FIRST-NM           TO SCVC-FIRST-NAME
               MOVE CIF-MIDDLE-NM          TO SCVC-MIDDLE-NAME
               MOVE CIF-LAST-NM            TO SCVC-SURNAME
               MOVE CIF-DOB                TO SCVC-DOB
               MOVE CIF-NAT-INS-NO         TO SCVC-NINO
           END-IF
           MOVE CIF-CUST-TYPE              TO SCVC-CUST-TYPE
           MOVE WS-CUST-ACCTS              TO SCVC-ACCOUNT-COUNT
           MOVE WS-CUST-AGG                TO SCVC-AGG-BALANCE
           MOVE WS-CUST-COMP               TO SCVC-COMPENSATABLE
           IF WS-EXCL-REASON = SPACES
               MOVE 'N'                    TO SCVC-EXCL-IND
           ELSE
               MOVE 'Y'                    TO SCVC-EXCL-IND
           END-IF
           MOVE WS-EXCL-REASON             TO SCVC-EXCL-REASON
           MOVE WS-DQ-FLAGS                TO SCVC-DQ-FLAGS
           MOVE CIF-KYC-STATUS             TO SCVC-KYC-STATUS
           PERFORM 2900-WRITE-SCV
           ADD 1                           TO WS-CUST-WRITTEN
           ADD WS-CUST-AGG                 TO WS-TOTAL-BALANCE
           ADD WS-CUST-COMP                TO WS-TOTAL-COMP

      *---- CONTACT ---------------------------------------------------*
           MOVE SPACES                     TO SCV-RECORD
           SET SCV-CONTACT                 TO TRUE
           MOVE WS-SCV-ID                  TO SCV-SCV-ID
           MOVE CIF-ADDR-LINE1             TO SCVK-ADDR-LINE1
           MOVE CIF-ADDR-LINE2             TO SCVK-ADDR-LINE2
           MOVE CIF-TOWN                   TO SCVK-TOWN
           MOVE CIF-COUNTY                 TO SCVK-COUNTY
           MOVE CIF-POST-CD                TO SCVK-POST-CD
           MOVE CIF-CTRY-CD                TO SCVK-CTRY-CD
           MOVE CIF-HOME-TEL               TO SCVK-HOME-TEL
           MOVE CIF-MOBILE-TEL             TO SCVK-MOBILE-TEL
           MOVE CIF-EMAIL-ADDR             TO SCVK-EMAIL
           PERFORM 2900-WRITE-SCV

      *---- ACCOUNTS --------------------------------------------------*
           PERFORM VARYING AB-IDX FROM 1 BY 1
                   UNTIL AB-IDX > WS-AB-COUNT
               MOVE WS-AB-RECORD (AB-IDX)  TO SCV-RECORD
               MOVE WS-SCV-ID              TO SCV-SCV-ID
               PERFORM 2900-WRITE-SCV
               ADD 1                       TO WS-ACCT-WRITTEN
           END-PERFORM
           .
      *================================================================*
       2900-WRITE-SCV.
      *================================================================*
           WRITE SCVFILE-RECORD            FROM SCV-RECORD
           IF WS-SCVFILE-STATUS NOT = '00'
               MOVE '2900-WRITE-SCV'       TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SCVFILE WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-SCVFILE-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-RECORDS-WRITTEN
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           MOVE SPACES                     TO SCV-RECORD
           SET SCV-TRAILER                 TO TRUE
           MOVE ALL '9'                    TO SCV-SCV-ID
           MOVE WS-CUST-WRITTEN            TO SCVT-CUSTOMER-COUNT
           MOVE WS-ACCT-WRITTEN            TO SCVT-ACCOUNT-COUNT
           COMPUTE SCVT-RECORD-COUNT = WS-RECORDS-WRITTEN + 1
           MOVE WS-TOTAL-BALANCE           TO SCVT-TOTAL-BALANCE
           MOVE WS-TOTAL-COMP              TO SCVT-TOTAL-COMPENSATION
           PERFORM 2900-WRITE-SCV

           WRITE REPORT-RECORD             FROM WS-SUM-HEADING-1
           MOVE 'CUSTOMERS READ'           TO TL-LABEL
           MOVE WS-CUST-READ               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS WITH DEPOSITS (ON SCV)' TO TL-LABEL
           MOVE WS-CUST-WRITTEN            TO TL-COUNT
           MOVE WS-TOTAL-BALANCE           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TOTAL COMPENSATABLE'      TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-COMP              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS OVER THE LIMIT' TO TL-LABEL
           MOVE WS-CUST-OVER-LIMIT         TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS EXCLUDED / FLAGGED' TO TL-LABEL
           MOVE WS-CUST-EXCLUDED           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS WITHOUT DEPOSITS' TO TL-LABEL
           MOVE WS-CUST-NO-DEPOSITS        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS ON SCV'          TO TL-LABEL
           MOVE WS-ACCT-WRITTEN            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS WITH DATA QUALITY ISSUES' TO TL-LABEL
           MOVE WS-DQ-CUSTOMERS            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           PERFORM VARYING WS-DQ-SUB FROM 1 BY 1 UNTIL WS-DQ-SUB > 6
               MOVE SPACES                 TO TL-LABEL
               STRING '  ' WS-DQ-NAME (WS-DQ-SUB)
                      DELIMITED BY SIZE
                      INTO TL-LABEL
               END-STRING
               MOVE WS-DQ-COUNT (WS-DQ-SUB) TO TL-COUNT
               WRITE REPORT-RECORD         FROM WS-TOTAL-LINE
           END-PERFORM
           MOVE 'CUSTOMERS WITH OVER 99 ACCOUNTS' TO TL-LABEL
           MOVE WS-BUFFER-OVERFLOW         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE CIFMAST-FILE
                 ACCTCIF-FILE
                 PRDPARM-FILE
                 SCVFILE-FILE
                 DQREPORT-FILE
                 REPORT-FILE

           DISPLAY 'RGSCV020 - CUSTOMERS ON SCV  ' WS-CUST-WRITTEN
           DISPLAY 'RGSCV020 - TOTAL COMPENSATION ' WS-TOTAL-COMP
           IF WS-BUFFER-OVERFLOW > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       7100-DQ-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               ADD 1                       TO WS-PAGE-COUNT
               MOVE WS-PAGE-COUNT          TO DH1-PAGE
               WRITE DQREPORT-RECORD       FROM WS-DQ-HEADING-1
               WRITE DQREPORT-RECORD       FROM WS-DQ-HEADING-2
               MOVE 3                      TO WS-LINE-COUNT
           END-IF
           MOVE CIF-NO                     TO DQ-CIF-NO
           MOVE SPACES                     TO DQ-NAME DQ-TEXT
           STRING CIF-FIRST-NM DELIMITED BY SPACE
                  ' '          DELIMITED BY SIZE
                  CIF-LAST-NM  DELIMITED BY '  '
                  INTO DQ-NAME
           END-STRING
           MOVE WS-CUST-AGG                TO DQ-BALANCE
           PERFORM VARYING WS-DQ-SUB FROM 1 BY 1 UNTIL WS-DQ-SUB > 6
               IF WS-DQ-FLAG (WS-DQ-SUB) = 'Y'
                   STRING DQ-TEXT DELIMITED BY '  '
                          WS-DQ-NAME (WS-DQ-SUB) DELIMITED BY '  '
                          '; ' DELIMITED BY SIZE
                          INTO DQ-TEXT
                   END-STRING
               END-IF
           END-PERFORM
           WRITE DQREPORT-RECORD           FROM WS-DQ-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-CUSTOMER.
      *================================================================*
           READ CIFMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   ADD 1                   TO WS-CUST-READ
               WHEN WS-CIFMAST-EOF
                   SET WS-END-OF-CUSTOMERS TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-CUSTOMER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
