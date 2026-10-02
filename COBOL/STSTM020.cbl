       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STSTM020.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-03-22.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : STSTM020                                            *
      * TITLE    : STATEMENT COMPOSITION                               *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : STSTM020                                            *
      * JCL      : STMT01 STEP ST020                                   *
      *----------------------------------------------------------------*
      * INPUT    : STMTREQ  - STATEMENT REQUESTS (STSTM010 + DDCLS010) *
      *                       STREQREC                                 *
      *            CIFMAST  - CIF CUSTOMER MASTER (RANDOM)             *
      *            ACCTMAST - DDA ACCOUNT MASTER (RANDOM, READ ONLY)   *
      *            PRDPARM  - PRODUCT PARAMETERS (RANDOM)              *
      *            MBK.TXNHIST  - TRANSACTIONS FOR THE PERIOD          *
      *            MBK.INTPAID  - INTEREST PAID / CHARGED IN PERIOD    *
      *            MBK.CHGHIST  - CHARGES APPLIED IN PERIOD            *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : PRINT    - STATEMENT PRINT STREAM (FBA 133) FOR     *
      *                       THE PRINT VENDOR (LINE DATA, ASA CC)     *
      *            INDEX    - ONE INDEX RECORD PER STATEMENT -         *
      *                       STIDXREC                                 *
      *            REPORT   - PRODUCTION CONTROL REPORT (FBA 133)      *
      *----------------------------------------------------------------*
      * REPLACES THE DOCUMENT SERVICES DOC1 COMPOSITION (STMT02) FOR   *
      * DDA STATEMENTS. THE OUTPUT IS PLAIN LINE DATA - THE PRINT      *
      * VENDOR APPLIES THE OVERLAY (LOGO, BOXES) BY FORM ID.           *
      *                                                                *
      * PAGE LAYOUT (60 PRINT LINES PER PAGE)                          *
      *   PAGE 1   ADDRESS BLOCK, ACCOUNT DETAILS, SUMMARY BOX, THEN   *
      *            UP TO WS-P1-TXN-LINES TRANSACTION LINES             *
      *   PAGE 2+  CONTINUATION HEADER, UP TO WS-PN-TXN-LINES LINES    *
      *   LAST     INTEREST AND CHARGES SUMMARY ("WHAT YOU PAID AND    *
      *            EARNED"), FSCS PROTECTION TEXT                      *
      *                                                                *
      * OPENING BALANCE = CLOSING - CREDITS + DEBITS IN THE PERIOD,    *
      * WHERE CLOSING IS THE RUNNING BALANCE OF THE LAST TRANSACTION   *
      * (OR THE REQUEST'S CLOSING BALANCE IF THERE WERE NONE). A       *
      * MISMATCH WITH THE PREVIOUS STATEMENT'S CLOSING BALANCE IS NOT  *
      * CHECKED - THE PREVIOUS STATEMENT IS NOT STORED ON THE HOST.    *
      *                                                                *
      * STATEMENTS ARE SUPPRESSED (INDEX ONLY, NOTHING PRINTED) FOR    *
      * CUSTOMERS MARKED DECEASED OR WITH A "GONE AWAY" ADDRESS        *
      * (POST CODE 'RTS').                                             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-03-22 SAT  ORIGINAL VERSION (CR 10/0141)                  *
      * 2010-06-14 SAT  INTEREST / CHARGES SUMMARY BOX (OFT ANNUAL     *
      *                 SUMMARY REQUIREMENT - PRINTED EVERY STATEMENT) *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STMTREQ-FILE     ASSIGN TO STMTREQ
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-STMTREQ-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT PRINT-FILE       ASSIGN TO PRINT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PRINT-STATUS.
           SELECT INDEX-FILE       ASSIGN TO INDEX
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-INDEX-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  STMTREQ-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STMTREQ-RECORD                  PIC X(100).
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  PRINT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PRINT-RECORD                    PIC X(133).
       FD  INDEX-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  INDEX-RECORD                    PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'STSTM020'.
      *
       01  WS-CONSTANTS.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +60.
           05  WS-P1-TXN-LINES             PIC S9(04) COMP VALUE +28.
           05  WS-PN-TXN-LINES             PIC S9(04) COMP VALUE +46.
           05  WS-BIC                      PIC X(11) VALUE 'MBKPGB2L'.
           05  WS-FSCS-LIMIT               PIC X(08) VALUE '50,000'.
      *
       01  WS-FILE-STATUSES.
           05  WS-STMTREQ-STATUS           PIC X(02).
               88  WS-STMTREQ-OK               VALUE '00'.
               88  WS-STMTREQ-EOF              VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-PRINT-STATUS             PIC X(02).
           05  WS-INDEX-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-REQUESTS          VALUE 'Y'.
           05  WS-EOC-IND                  PIC X(01).
               88  WS-END-OF-CURSOR            VALUE 'Y'.
           05  WS-SUPPRESS-IND             PIC X(01).
               88  WS-SUPPRESS                 VALUE 'Y'.
           05  WS-FAIL-IND                 PIC X(01).
               88  WS-FAILED                   VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-FROM-DB2                 PIC X(10).
           05  WS-TO-DB2                   PIC X(10).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(11).
           05  WS-MONTH-NAMES              PIC X(36) VALUE
               'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'.
           05  WS-MONTH-NO                 PIC 9(02).
           05  WS-DB2-DATE-IN              PIC X(10).
           05  WS-FAIL-REASON              PIC X(04).
           05  WS-OUTPUT-LINE-NO           PIC 9(09) VALUE ZERO.
           05  WS-FIRST-LINE-NO            PIC 9(09).
      *
       01  WS-STATEMENT-TOTALS.
           05  WS-OPENING-BAL              PIC S9(13)V99 COMP-3.
           05  WS-CLOSING-BAL              PIC S9(13)V99 COMP-3.
           05  WS-TOTAL-IN                 PIC S9(13)V99 COMP-3.
           05  WS-TOTAL-OUT                PIC S9(13)V99 COMP-3.
           05  WS-TXN-COUNT                PIC S9(05) COMP-3.
           05  WS-PAGE-NO                  PIC S9(04) COMP.
           05  WS-PAGE-TXN-LINES           PIC S9(04) COMP.
           05  WS-PAGE-LIMIT               PIC S9(04) COMP.
           05  WS-INT-EARNED               PIC S9(13)V99 COMP-3.
           05  WS-TAX-DEDUCTED             PIC S9(13)V99 COMP-3.
           05  WS-INT-PAID                 PIC S9(13)V99 COMP-3.
           05  WS-CHARGES-PAID             PIC S9(13)V99 COMP-3.
           05  WS-CHARGES-REFUNDED         PIC S9(13)V99 COMP-3.
      *
      *---- TRANSACTION BUFFER - ONE STATEMENT ------------------------*
      *    TRANSACTIONS ARE BUFFERED SO THE OPENING BALANCE AND SUMMARY
      *    (PRINTED ON PAGE 1) ARE KNOWN BEFORE THE LINES ARE PRINTED.
      *    LONGER STATEMENTS ARE TRUNCATED AT WS-TXN-MAX AND FLAGGED -
      *    BUSINESS ACCOUNTS WITH MORE ACTIVITY USE THE BUSINESS
      *    STATEMENT SERVICE (NOT IN THIS LIBRARY).
       01  WS-TXN-BUFFER.
           05  WS-TXN-MAX                  PIC S9(05) COMP VALUE +3000.
           05  WS-TXN-USED                 PIC S9(05) COMP VALUE ZERO.
           05  WS-TXN-TRUNCATED-IND        PIC X(01).
               88  WS-TXN-TRUNCATED            VALUE 'Y'.
           05  WS-TXN-ENTRY                OCCURS 3000 TIMES
                                           INDEXED BY TX-IDX.
               10  WS-TX-POST-DATE         PIC X(10).
               10  WS-TX-TXN-CODE          PIC X(04).
               10  WS-TX-DR-CR             PIC X(01).
               10  WS-TX-AMOUNT            PIC S9(13)V99 COMP-3.
               10  WS-TX-RUNNING-BAL       PIC S9(13)V99 COMP-3.
               10  WS-TX-NARR-1            PIC X(35).
               10  WS-TX-NARR-2            PIC X(35).
      *
      *---- ADDRESS BLOCK ---------------------------------------------*
       01  WS-ADDRESS-BLOCK.
           05  WS-ADDR-LINE                PIC X(60) OCCURS 7 TIMES.
       01  WS-ADDR-SUB                     PIC S9(04) COMP.
       01  WS-CUST-NAME                    PIC X(60).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-PRODUCED-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUPPRESSED-COUNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-FAILED-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-TRUNC-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-PAGES              PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-SHEETS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ELECTRONIC-COUNT         PIC S9(09) COMP-3 VALUE 0.
      *
      *---- PRINT LINES -----------------------------------------------*
       01  PL-LINE.
           05  PL-CC                       PIC X(01).
           05  PL-TEXT                     PIC X(132).
      *
       01  PL-FORM-ID-LINE.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(08) VALUE '%%FORM='.
           05  PLF-FORM-ID                 PIC X(08).
           05  FILLER                      PIC X(10) VALUE ' %%DOCKEY='.
           05  PLF-DOC-KEY                 PIC X(30).
           05  FILLER                      PIC X(10) VALUE ' %%SHEETS='.
           05  PLF-DELIVERY                PIC X(01).
           05  FILLER                      PIC X(65) VALUE SPACES.
      *
       01  PL-ACCOUNT-LINE-1.
           05  FILLER                      PIC X(01) VALUE '-'.
           05  FILLER                      PIC X(70) VALUE SPACES.
           05  FILLER                      PIC X(16)
               VALUE 'ACCOUNT NAME    '.
           05  PLA-PROD-NAME               PIC X(40).
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  PL-ACCOUNT-LINE-2.
           05  FILLER                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(70) VALUE SPACES.
           05  FILLER                      PIC X(16)
               VALUE 'SORT CODE       '.
           05  PLA-SORT-CODE               PIC X(08).
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  FILLER                      PIC X(16)
               VALUE 'ACCOUNT NUMBER  '.
           05  PLA-ACCT-NO                 PIC X(08).
           05  FILLER                      PIC X(10) VALUE SPACES.
       01  PL-ACCOUNT-LINE-3.
           05  FILLER                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(70) VALUE SPACES.
           05  FILLER                      PIC X(16)
               VALUE 'IBAN            '.
           05  PLA-IBAN                    PIC X(42).
           05  FILLER                      PIC X(04) VALUE SPACES.
       01  PL-ACCOUNT-LINE-4.
           05  FILLER                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(70) VALUE SPACES.
           05  FILLER                      PIC X(16)
               VALUE 'BIC             '.
           05  PLA-BIC                     PIC X(11).
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(16)
               VALUE 'STATEMENT NO    '.
           05  PLA-STMT-NO                 PIC ZZZZ9.
           05  FILLER                      PIC X(09) VALUE SPACES.
       01  PL-PERIOD-LINE.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  FILLER                      PIC X(70) VALUE SPACES.
           05  FILLER                      PIC X(16)
               VALUE 'PERIOD          '.
           05  PLP-FROM                    PIC X(11).
           05  FILLER                      PIC X(04) VALUE ' TO '.
           05  PLP-TO                      PIC X(11).
           05  FILLER                      PIC X(20) VALUE SPACES.
      *
       01  PL-SUMMARY-HEAD.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(40)
               VALUE 'YOUR ACCOUNT SUMMARY'.
           05  FILLER                      PIC X(87) VALUE SPACES.
       01  PL-SUMMARY-LINE.
           05  FILLER                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  PLS-LABEL                   PIC X(30).
           05  PLS-AMOUNT                  PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  PLS-DRCR                    PIC X(03).
           05  FILLER                      PIC X(78) VALUE SPACES.
      *
       01  PL-CONT-HEAD.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(30)
               VALUE 'MEGABANK PLC - STATEMENT'.
           05  PLC-SORT-CODE               PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PLC-ACCT-NO                 PIC X(08).
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  FILLER                      PIC X(13)
                                           VALUE 'STATEMENT NO '.
           05  PLC-STMT-NO                 PIC ZZZZ9.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  FILLER                      PIC X(05) VALUE 'PAGE '.
           05  PLC-PAGE                    PIC ZZ9.
           05  FILLER                      PIC X(46) VALUE SPACES.
       01  PL-TXN-HEAD.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(13) VALUE 'DATE'.
           05  FILLER                      PIC X(38) VALUE 'DETAILS'.
           05  FILLER                      PIC X(18)
               VALUE '      PAID OUT'.
           05  FILLER                      PIC X(18)
               VALUE '       PAID IN'.
           05  FILLER                      PIC X(20)
               VALUE '           BALANCE'.
           05  FILLER                      PIC X(20) VALUE SPACES.
       01  PL-TXN-LINE.
           05  PLT-CC                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  PLT-DATE                    PIC X(11).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PLT-DETAILS                 PIC X(35).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PLT-OUT                     PIC Z,ZZZ,ZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PLT-IN                      PIC Z,ZZZ,ZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PLT-BALANCE                 PIC Z,ZZZ,ZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
           05  PLT-BAL-DRCR                PIC X(03).
           05  FILLER                      PIC X(22) VALUE SPACES.
      *
       01  PL-TEXT-LINE.
           05  PLX-CC                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  PLX-TEXT                    PIC X(100).
           05  FILLER                      PIC X(27) VALUE SPACES.
      *
       01  WS-REPORT-CONTROL.
           05  WS-RPT-LINES                PIC S9(04) COMP VALUE +99.
           05  WS-RPT-PAGE                 PIC S9(04) COMP VALUE ZERO.
       01  WS-RPT-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'STSTM020'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - STATEMENT PRODUCTION CONTROL'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  RH1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  RH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-RPT-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(07) VALUE 'STMT'.
           05  FILLER            PIC X(12) VALUE 'PERIOD TO'.
           05  FILLER            PIC X(07) VALUE '  TXNS'.
           05  FILLER            PIC X(07) VALUE ' PAGES'.
           05  FILLER            PIC X(18) VALUE '   CLOSING BAL'.
           05  FILLER            PIC X(04) VALUE 'DLV'.
           05  FILLER            PIC X(58) VALUE 'RESULT'.
       01  WS-RPT-DETAIL.
           05  RD-CC                       PIC X(01) VALUE ' '.
           05  RD-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  RD-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-STMT-NO                  PIC ZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-PERIOD-TO                PIC X(11).
           05  RD-TXNS                     PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  RD-PAGES                    PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  RD-CLOSING                  PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-DELIVERY                 PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  RD-RESULT                   PIC X(40).
           05  FILLER                      PIC X(17) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
       01  HV-SORT-CODE                    PIC X(06).
       01  HV-ACCT-NO                      PIC X(08).
       01  HV-FROM-DATE                    PIC X(10).
       01  HV-TO-DATE                      PIC X(10).
       01  HV-SUM-1                        PIC S9(13)V99 COMP-3.
       01  HV-SUM-2                        PIC S9(13)V99 COMP-3.
       01  HV-IND-1                        PIC S9(04) COMP.
       01  HV-IND-2                        PIC S9(04) COMP.
      *
      *    STATEMENT PERIOD IS FROM (EXCLUSIVE - THE LAST STATEMENT
      *    DATE) TO (INCLUSIVE).
           EXEC SQL DECLARE STMTCSR CURSOR FOR
               SELECT POST_DATE, POST_SEQ, TXN_CODE, DR_CR_IND,
                      TXN_AMT, RUNNING_BAL, NARRATIVE_1, NARRATIVE_2
                 FROM MBK.TXNHIST
                WHERE SORT_CODE = :HV-SORT-CODE
                  AND ACCT_NO   = :HV-ACCT-NO
                  AND POST_DATE >  :HV-FROM-DATE
                  AND POST_DATE <= :HV-TO-DATE
                ORDER BY POST_DATE, POST_SEQ
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
           COPY STREQREC.
      *
           COPY STIDXREC.
      *
           COPY IBNPARM.
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
           PERFORM 2000-PROCESS-REQUEST
               UNTIL WS-END-OF-REQUESTS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  STMTREQ-FILE
                       CIFMAST-FILE
                       ACCTMAST-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                OUTPUT PRINT-FILE
                       INDEX-FILE
                       REPORT-FILE
           IF NOT WS-STMTREQ-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-PRINT-STATUS NOT = '00'
           OR WS-INDEX-STATUS NOT = '00'
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
           PERFORM 8900-FORMAT-DISPLAY-DATE
           MOVE WS-DATE-DISPLAY            TO RH1-BUS-DATE
           PERFORM 8000-READ-REQUEST
           .
      *================================================================*
       2000-PROCESS-REQUEST.
      *================================================================*
           MOVE 'N'                        TO WS-SUPPRESS-IND
                                              WS-FAIL-IND
                                              WS-TXN-TRUNCATED-IND
           MOVE SPACES                     TO WS-FAIL-REASON
           MOVE ZERO                       TO WS-TXN-USED
                                              WS-TXN-COUNT
                                              WS-TOTAL-IN
                                              WS-TOTAL-OUT
                                              WS-PAGE-NO

           PERFORM 2100-READ-MASTERS THRU 2100-EXIT
           IF NOT WS-FAILED
               PERFORM 2200-LOAD-TRANSACTIONS
           END-IF
           IF NOT WS-FAILED
               PERFORM 2300-CALCULATE-BALANCES
               PERFORM 2400-INTEREST-AND-CHARGES
               PERFORM 2500-BUILD-DOC-KEY
               IF WS-SUPPRESS
                   ADD 1                   TO WS-SUPPRESSED-COUNT
                   SET SIX-SUPPRESSED      TO TRUE
               ELSE
                   PERFORM 4000-PRINT-STATEMENT
                   ADD 1                   TO WS-PRODUCED-COUNT
               END-IF
               SET SIX-PRODUCED            TO TRUE
           ELSE
               ADD 1                       TO WS-FAILED-COUNT
               MOVE SPACES                 TO SIX-INDEX-RECORD
               SET SIX-FAILED              TO TRUE
               MOVE WS-FAIL-REASON         TO SIX-FAIL-REASON
           END-IF

           PERFORM 2900-WRITE-INDEX
           PERFORM 7100-REPORT-DETAIL
           PERFORM 8000-READ-REQUEST
           .
      *================================================================*
       2100-READ-MASTERS.
      *================================================================*
           MOVE SRQ-SORT-CODE              TO ACM-SORT-CODE
           MOVE SRQ-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF NOT WS-ACCTMAST-OK
               SET WS-FAILED               TO TRUE
               MOVE 'ANFD'                 TO WS-FAIL-REASON
               GO TO 2100-EXIT
           END-IF

           MOVE SRQ-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           IF NOT WS-CIFMAST-OK
               SET WS-FAILED               TO TRUE
               MOVE 'CNFD'                 TO WS-FAIL-REASON
               GO TO 2100-EXIT
           END-IF
           IF CIF-DECEASED
           OR CIF-POST-CD (1:3) = 'RTS'
               SET WS-SUPPRESS             TO TRUE
           END-IF

           MOVE ACM-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF NOT WS-PRDPARM-OK
               MOVE ACM-PROD-CD            TO PRD-DESCRIPTION
               MOVE 'STMTSTD1'             TO PRD-STMT-TEMPLATE-ID
           END-IF
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-LOAD-TRANSACTIONS.
      *================================================================*
           MOVE SRQ-SORT-CODE              TO HV-SORT-CODE
           MOVE SRQ-ACCT-NO                TO HV-ACCT-NO
           MOVE SRQ-PERIOD-FROM            TO WS-DATE-WORK
           PERFORM 8950-FORMAT-DB2-DATE
           MOVE WS-FROM-DB2                TO HV-FROM-DATE
           MOVE SRQ-PERIOD-TO              TO WS-DATE-WORK
           PERFORM 8950-FORMAT-DB2-DATE
           MOVE WS-FROM-DB2                TO HV-TO-DATE

           EXEC SQL OPEN STMTCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2200-LOAD-TRANSACTIONS' TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE 'N'                        TO WS-EOC-IND
           PERFORM UNTIL WS-END-OF-CURSOR
               EXEC SQL
                   FETCH STMTCSR
                    INTO :TXH-POST-DATE, :TXH-POST-SEQ,
                         :TXH-TXN-CODE, :TXH-DR-CR-IND,
                         :TXH-TXN-AMT, :TXH-RUNNING-BAL,
                         :TXH-NARRATIVE-1, :TXH-NARRATIVE-2
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 2210-BUFFER-TRANSACTION
                   WHEN +100
                       SET WS-END-OF-CURSOR TO TRUE
                   WHEN OTHER
                       MOVE '2200-LOAD-TRANSACTIONS' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE STMTCSR END-EXEC
           .
      *================================================================*
       2210-BUFFER-TRANSACTION.
      *================================================================*
           ADD 1                           TO WS-TXN-COUNT
           IF TXH-DR-CR-IND = 'D'
               ADD TXH-TXN-AMT             TO WS-TOTAL-OUT
           ELSE
               ADD TXH-TXN-AMT             TO WS-TOTAL-IN
           END-IF
      *    THE CLOSING BALANCE IS ALWAYS THE LAST RUNNING BALANCE, EVEN
      *    IF THE BUFFER IS FULL.
           MOVE TXH-RUNNING-BAL            TO WS-CLOSING-BAL

           IF WS-TXN-USED >= WS-TXN-MAX
               SET WS-TXN-TRUNCATED        TO TRUE
           ELSE
               ADD 1                       TO WS-TXN-USED
               SET TX-IDX                  TO WS-TXN-USED
               MOVE TXH-POST-DATE          TO WS-TX-POST-DATE (TX-IDX)
               MOVE TXH-TXN-CODE           TO WS-TX-TXN-CODE (TX-IDX)
               MOVE TXH-DR-CR-IND          TO WS-TX-DR-CR (TX-IDX)
               MOVE TXH-TXN-AMT            TO WS-TX-AMOUNT (TX-IDX)
               MOVE TXH-RUNNING-BAL        TO WS-TX-RUNNING-BAL (TX-IDX)
               MOVE SPACES                 TO WS-TX-NARR-1 (TX-IDX)
                                              WS-TX-NARR-2 (TX-IDX)
               IF TXH-NARRATIVE-1-LEN > ZERO
                   MOVE TXH-NARRATIVE-1-TEXT (1:TXH-NARRATIVE-1-LEN)
                                           TO WS-TX-NARR-1 (TX-IDX)
               END-IF
               IF TXH-NARRATIVE-2-LEN > ZERO
                   MOVE TXH-NARRATIVE-2-TEXT (1:TXH-NARRATIVE-2-LEN)
                                           TO WS-TX-NARR-2 (TX-IDX)
               END-IF
           END-IF
           .
      *================================================================*
       2300-CALCULATE-BALANCES.
      *================================================================*
           IF WS-TXN-COUNT = ZERO
               MOVE SRQ-CLOSING-BAL        TO WS-CLOSING-BAL
           END-IF
           COMPUTE WS-OPENING-BAL = WS-CLOSING-BAL
                                  - WS-TOTAL-IN + WS-TOTAL-OUT
           IF WS-TXN-TRUNCATED
               ADD 1                       TO WS-TRUNC-COUNT
           END-IF
           .
      *================================================================*
       2400-INTEREST-AND-CHARGES.
      *================================================================*
           MOVE ZERO                       TO WS-INT-EARNED
                                              WS-TAX-DEDUCTED
                                              WS-INT-PAID
                                              WS-CHARGES-PAID
                                              WS-CHARGES-REFUNDED
           EXEC SQL
               SELECT SUM(GROSS_AMT), SUM(TAX_AMT)
                 INTO :HV-SUM-1 :HV-IND-1, :HV-SUM-2 :HV-IND-2
                 FROM MBK.INTPAID
                WHERE SORT_CODE = :HV-SORT-CODE
                  AND ACCT_NO   = :HV-ACCT-NO
                  AND CR_DR_IND = 'C'
                  AND CAP_DATE  >  :HV-FROM-DATE
                  AND CAP_DATE  <= :HV-TO-DATE
           END-EXEC
           IF SQLCODE = 0
               IF HV-IND-1 >= 0
                   MOVE HV-SUM-1           TO WS-INT-EARNED
               END-IF
               IF HV-IND-2 >= 0
                   MOVE HV-SUM-2           TO WS-TAX-DEDUCTED
               END-IF
           END-IF

           EXEC SQL
               SELECT SUM(NET_AMT)
                 INTO :HV-SUM-1 :HV-IND-1
                 FROM MBK.INTPAID
                WHERE SORT_CODE = :HV-SORT-CODE
                  AND ACCT_NO   = :HV-ACCT-NO
                  AND CR_DR_IND = 'D'
                  AND CAP_DATE  >  :HV-FROM-DATE
                  AND CAP_DATE  <= :HV-TO-DATE
           END-EXEC
           IF SQLCODE = 0 AND HV-IND-1 >= 0
               MOVE HV-SUM-1               TO WS-INT-PAID
           END-IF

           EXEC SQL
               SELECT SUM(NET_AMT), SUM(REFUND_AMT)
                 INTO :HV-SUM-1 :HV-IND-1, :HV-SUM-2 :HV-IND-2
                 FROM MBK.CHGHIST
                WHERE SORT_CODE  = :HV-SORT-CODE
                  AND ACCT_NO    = :HV-ACCT-NO
                  AND APPLY_DATE >  :HV-FROM-DATE
                  AND APPLY_DATE <= :HV-TO-DATE
           END-EXEC
           IF SQLCODE = 0
               IF HV-IND-1 >= 0
                   MOVE HV-SUM-1           TO WS-CHARGES-PAID
               END-IF
               IF HV-IND-2 >= 0
                   MOVE HV-SUM-2           TO WS-CHARGES-REFUNDED
               END-IF
           END-IF
           .
      *================================================================*
       2500-BUILD-DOC-KEY.
      *================================================================*
           MOVE SPACES                     TO SIX-INDEX-RECORD
           STRING 'MBKST' SRQ-SORT-CODE SRQ-ACCT-NO SRQ-STMT-NO
                  SRQ-PERIOD-TO
                  DELIMITED BY SIZE
                  INTO SIX-DOC-KEY
           END-STRING
           MOVE SIX-DOC-KEY                TO SRQ-DOC-KEY
           .
      *================================================================*
       2900-WRITE-INDEX.
      *================================================================*
           MOVE SRQ-SORT-CODE              TO SIX-SORT-CODE
           MOVE SRQ-ACCT-NO                TO SIX-ACCT-NO
           MOVE SRQ-CIF-NO                 TO SIX-CIF-NO
           MOVE SRQ-STMT-NO                TO SIX-STMT-NO
           MOVE SRQ-PERIOD-FROM            TO SIX-PERIOD-FROM
           MOVE SRQ-PERIOD-TO              TO SIX-PERIOD-TO
           MOVE SRQ-REQUEST-TYPE           TO SIX-REQUEST-TYPE
           IF NOT WS-FAILED
               MOVE WS-OPENING-BAL         TO SIX-OPENING-BAL
               MOVE WS-CLOSING-BAL         TO SIX-CLOSING-BAL
               MOVE WS-TXN-COUNT           TO SIX-TXN-COUNT
               MOVE WS-PAGE-NO             TO SIX-PAGE-COUNT
               COMPUTE SIX-SHEET-COUNT = (WS-PAGE-NO + 1) / 2
               MOVE WS-FIRST-LINE-NO       TO SIX-FIRST-LINE-NO
               MOVE WS-CUST-NAME (1:40)    TO SIX-IDX-CUST-NAME
               MOVE CIF-POST-CD            TO SIX-IDX-POST-CD
               IF NOT WS-SUPPRESS
                   IF CIF-EMAIL-ADDR NOT = SPACES
                   AND PRD-ESTMT-ALLOWED = 'Y'
                   AND CIF-MKTG-EMAIL-IND = 'Y'
                       SET SIX-ELECTRONIC  TO TRUE
                   ELSE
                       SET SIX-PAPER       TO TRUE
                   END-IF
               END-IF
               ADD WS-PAGE-NO              TO WS-TOTAL-PAGES
               ADD SIX-SHEET-COUNT         TO WS-TOTAL-SHEETS
               IF SIX-ELECTRONIC
                   ADD 1                   TO WS-ELECTRONIC-COUNT
               END-IF
           END-IF
           WRITE INDEX-RECORD              FROM SIX-INDEX-RECORD
           IF WS-INDEX-STATUS NOT = '00'
               MOVE '2900-WRITE-INDEX'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'INDEX WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-INDEX-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       4000-PRINT-STATEMENT.
      *================================================================*
           COMPUTE WS-FIRST-LINE-NO = WS-OUTPUT-LINE-NO + 1
           MOVE 1                          TO WS-PAGE-NO
           PERFORM 4100-PRINT-FIRST-PAGE-HEAD
           PERFORM 4200-PRINT-SUMMARY
           PERFORM 4300-PRINT-TXN-HEADING
           MOVE WS-P1-TXN-LINES            TO WS-PAGE-LIMIT
           MOVE ZERO                       TO WS-PAGE-TXN-LINES

      *---- BALANCE BROUGHT FORWARD -----------------------------------*
           MOVE SPACES                     TO PL-TXN-LINE
           MOVE ' '                        TO PLT-CC
           MOVE SRQ-PERIOD-FROM            TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DISPLAY-DATE
           MOVE WS-DATE-DISPLAY            TO PLT-DATE
           MOVE 'BALANCE BROUGHT FORWARD'  TO PLT-DETAILS
           MOVE ZERO                       TO PLT-OUT PLT-IN
           PERFORM 4600-SET-BALANCE-FIELDS-OPEN
           PERFORM 4900-WRITE-TXN-LINE

           PERFORM VARYING TX-IDX FROM 1 BY 1
                   UNTIL TX-IDX > WS-TXN-USED
               PERFORM 4400-PRINT-TRANSACTION
           END-PERFORM

           IF WS-TXN-TRUNCATED
               MOVE SPACES                 TO PL-TEXT-LINE
               MOVE '0'                    TO PLX-CC
               STRING 'FURTHER TRANSACTIONS ARE NOT SHOWN - '
                      'PLEASE ASK FOR A FULL STATEMENT'
                      DELIMITED BY SIZE
                      INTO PLX-TEXT
               END-STRING
               PERFORM 4950-WRITE-TEXT-LINE
           END-IF

      *---- BALANCE CARRIED FORWARD -----------------------------------*
           MOVE SPACES                     TO PL-TXN-LINE
           MOVE '0'                        TO PLT-CC
           MOVE SRQ-PERIOD-TO              TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DISPLAY-DATE
           MOVE WS-DATE-DISPLAY            TO PLT-DATE
           MOVE 'BALANCE CARRIED FORWARD'  TO PLT-DETAILS
           MOVE ZERO                       TO PLT-OUT PLT-IN
           PERFORM 4610-SET-BALANCE-FIELDS-CLOSE
           PERFORM 4900-WRITE-TXN-LINE

           PERFORM 4700-PRINT-INTEREST-BOX THRU 4700-EXIT
           PERFORM 4800-PRINT-FSCS-TEXT
           .
      *================================================================*
       4100-PRINT-FIRST-PAGE-HEAD.
      *================================================================*
           MOVE PRD-STMT-TEMPLATE-ID       TO PLF-FORM-ID
           IF PLF-FORM-ID = SPACES
               MOVE 'STMTSTD1'             TO PLF-FORM-ID
           END-IF
           MOVE SIX-DOC-KEY                TO PLF-DOC-KEY
           MOVE 'P'                        TO PLF-DELIVERY
           WRITE PRINT-RECORD              FROM PL-FORM-ID-LINE
           ADD 1                           TO WS-OUTPUT-LINE-NO

      *    ADDRESS BLOCK - FIRST 7 LINES, WINDOW ENVELOPE POSITION.
           PERFORM 4110-BUILD-ADDRESS
           PERFORM VARYING WS-ADDR-SUB FROM 1 BY 1
                   UNTIL WS-ADDR-SUB > 7
               MOVE SPACES                 TO PL-TEXT-LINE
               MOVE ' '                    TO PLX-CC
               MOVE WS-ADDR-LINE (WS-ADDR-SUB) TO PLX-TEXT
               PERFORM 4950-WRITE-TEXT-LINE
           END-PERFORM

           MOVE PRD-DESCRIPTION            TO PLA-PROD-NAME
           WRITE PRINT-RECORD              FROM PL-ACCOUNT-LINE-1
           STRING SRQ-SORT-CODE (1:2) '-' SRQ-SORT-CODE (3:2) '-'
                  SRQ-SORT-CODE (5:2)
                  DELIMITED BY SIZE
                  INTO PLA-SORT-CODE
           END-STRING
           MOVE SRQ-ACCT-NO                TO PLA-ACCT-NO
           WRITE PRINT-RECORD              FROM PL-ACCOUNT-LINE-2

           MOVE 'V'                        TO IBN-FUNCTION
           MOVE ACM-IBAN                   TO IBN-IBAN
           CALL 'DDIBAN1' USING IBN-PARM-BLOCK
           IF IBN-OK
               MOVE IBN-IBAN-PRINT         TO PLA-IBAN
           ELSE
               MOVE ACM-IBAN               TO PLA-IBAN
           END-IF
           WRITE PRINT-RECORD              FROM PL-ACCOUNT-LINE-3
           MOVE WS-BIC                     TO PLA-BIC
           MOVE SRQ-STMT-NO                TO PLA-STMT-NO
           WRITE PRINT-RECORD              FROM PL-ACCOUNT-LINE-4

           MOVE SRQ-PERIOD-FROM            TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DISPLAY-DATE
           MOVE WS-DATE-DISPLAY            TO PLP-FROM
           MOVE SRQ-PERIOD-TO              TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DISPLAY-DATE
           MOVE WS-DATE-DISPLAY            TO PLP-TO
           WRITE PRINT-RECORD              FROM PL-PERIOD-LINE
           ADD 5                           TO WS-OUTPUT-LINE-NO
           .
      *================================================================*
       4110-BUILD-ADDRESS.
      *================================================================*
           MOVE SPACES                     TO WS-ADDRESS-BLOCK
                                              WS-CUST-NAME
           STRING CIF-TITLE     DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-FIRST-NM  DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-LAST-NM   DELIMITED BY '  '
                  INTO WS-CUST-NAME
           END-STRING
           MOVE 1                          TO WS-ADDR-SUB
           MOVE WS-CUST-NAME               TO WS-ADDR-LINE (1)
           IF CIF-ADDR-LINE1 NOT = SPACES
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-ADDR-LINE1         TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           IF CIF-ADDR-LINE2 NOT = SPACES
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-ADDR-LINE2         TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           IF CIF-TOWN NOT = SPACES
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-TOWN               TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           IF CIF-COUNTY NOT = SPACES
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-COUNTY             TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           ADD 1                           TO WS-ADDR-SUB
           MOVE CIF-POST-CD                TO WS-ADDR-LINE (WS-ADDR-SUB)
           IF CIF-CTRY-CD NOT = 'GB' AND CIF-CTRY-CD NOT = SPACES
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-CTRY-CD            TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           .
      *================================================================*
       4200-PRINT-SUMMARY.
      *================================================================*
           WRITE PRINT-RECORD              FROM PL-SUMMARY-HEAD
           MOVE 'BALANCE AT START OF PERIOD' TO PLS-LABEL
           PERFORM 4210-SIGNED-OPENING
           WRITE PRINT-RECORD              FROM PL-SUMMARY-LINE
           MOVE 'TOTAL PAID IN'            TO PLS-LABEL
           MOVE WS-TOTAL-IN                TO PLS-AMOUNT
           MOVE SPACES                     TO PLS-DRCR
           WRITE PRINT-RECORD              FROM PL-SUMMARY-LINE
           MOVE 'TOTAL PAID OUT'           TO PLS-LABEL
           MOVE WS-TOTAL-OUT               TO PLS-AMOUNT
           MOVE SPACES                     TO PLS-DRCR
           WRITE PRINT-RECORD              FROM PL-SUMMARY-LINE
           MOVE 'BALANCE AT END OF PERIOD' TO PLS-LABEL
           PERFORM 4220-SIGNED-CLOSING
           WRITE PRINT-RECORD              FROM PL-SUMMARY-LINE
           IF ACM-OD-LIMIT > ZERO
               MOVE 'ARRANGED OVERDRAFT LIMIT' TO PLS-LABEL
               MOVE ACM-OD-LIMIT           TO PLS-AMOUNT
               MOVE SPACES                 TO PLS-DRCR
               WRITE PRINT-RECORD          FROM PL-SUMMARY-LINE
               ADD 1                       TO WS-OUTPUT-LINE-NO
           END-IF
           ADD 5                           TO WS-OUTPUT-LINE-NO
           .
      *================================================================*
       4210-SIGNED-OPENING.
      *================================================================*
           IF WS-OPENING-BAL < ZERO
               COMPUTE PLS-AMOUNT = 0 - WS-OPENING-BAL
               MOVE ' OD'                  TO PLS-DRCR
           ELSE
               MOVE WS-OPENING-BAL         TO PLS-AMOUNT
               MOVE SPACES                 TO PLS-DRCR
           END-IF
           .
      *================================================================*
       4220-SIGNED-CLOSING.
      *================================================================*
           IF WS-CLOSING-BAL < ZERO
               COMPUTE PLS-AMOUNT = 0 - WS-CLOSING-BAL
               MOVE ' OD'                  TO PLS-DRCR
           ELSE
               MOVE WS-CLOSING-BAL         TO PLS-AMOUNT
               MOVE SPACES                 TO PLS-DRCR
           END-IF
           .
      *================================================================*
       4300-PRINT-TXN-HEADING.
      *================================================================*
           WRITE PRINT-RECORD              FROM PL-TXN-HEAD
           ADD 1                           TO WS-OUTPUT-LINE-NO
           .
      *================================================================*
       4400-PRINT-TRANSACTION.
      *================================================================*
           MOVE SPACES                     TO PL-TXN-LINE
           MOVE ' '                        TO PLT-CC
           MOVE WS-TX-POST-DATE (TX-IDX)   TO WS-DB2-DATE-IN
           MOVE WS-DB2-DATE-IN (1:4)       TO WS-DW-CCYY
           MOVE WS-DB2-DATE-IN (6:2)       TO WS-DW-MM
           MOVE WS-DB2-DATE-IN (9:2)       TO WS-DW-DD
           PERFORM 8900-FORMAT-DISPLAY-DATE
           MOVE WS-DATE-DISPLAY            TO PLT-DATE
           MOVE WS-TX-NARR-1 (TX-IDX)      TO PLT-DETAILS
           IF WS-TX-DR-CR (TX-IDX) = 'D'
               MOVE WS-TX-AMOUNT (TX-IDX)  TO PLT-OUT
               MOVE ZERO                   TO PLT-IN
           ELSE
               MOVE ZERO                   TO PLT-OUT
               MOVE WS-TX-AMOUNT (TX-IDX)  TO PLT-IN
           END-IF
      *    THE RUNNING BALANCE IS SHOWN ONLY ON THE LAST ITEM OF EACH
      *    DAY (UK STATEMENT CONVENTION).
           IF TX-IDX = WS-TXN-USED
           OR WS-TX-POST-DATE (TX-IDX + 1)
              NOT = WS-TX-POST-DATE (TX-IDX)
               IF WS-TX-RUNNING-BAL (TX-IDX) < ZERO
                   COMPUTE PLT-BALANCE =
                           0 - WS-TX-RUNNING-BAL (TX-IDX)
                   MOVE ' OD'              TO PLT-BAL-DRCR
               ELSE
                   MOVE WS-TX-RUNNING-BAL (TX-IDX) TO PLT-BALANCE
                   MOVE SPACES             TO PLT-BAL-DRCR
               END-IF
           ELSE
               MOVE ZERO                   TO PLT-BALANCE
               MOVE SPACES                 TO PLT-BAL-DRCR
           END-IF
           PERFORM 4900-WRITE-TXN-LINE

      *    SECOND NARRATIVE LINE (REFERENCE) IF PRESENT.
           IF WS-TX-NARR-2 (TX-IDX) NOT = SPACES
               MOVE SPACES                 TO PL-TXN-LINE
               MOVE ' '                    TO PLT-CC
               MOVE WS-TX-NARR-2 (TX-IDX)  TO PLT-DETAILS
               MOVE ZERO                   TO PLT-OUT PLT-IN PLT-BALANCE
               PERFORM 4900-WRITE-TXN-LINE
           END-IF
           .
      *================================================================*
       4600-SET-BALANCE-FIELDS-OPEN.
      *================================================================*
           IF WS-OPENING-BAL < ZERO
               COMPUTE PLT-BALANCE = 0 - WS-OPENING-BAL
               MOVE ' OD'                  TO PLT-BAL-DRCR
           ELSE
               MOVE WS-OPENING-BAL         TO PLT-BALANCE
               MOVE SPACES                 TO PLT-BAL-DRCR
           END-IF
           .
      *================================================================*
       4610-SET-BALANCE-FIELDS-CLOSE.
      *================================================================*
           IF WS-CLOSING-BAL < ZERO
               COMPUTE PLT-BALANCE = 0 - WS-CLOSING-BAL
               MOVE ' OD'                  TO PLT-BAL-DRCR
           ELSE
               MOVE WS-CLOSING-BAL         TO PLT-BALANCE
               MOVE SPACES                 TO PLT-BAL-DRCR
           END-IF
           .
      *================================================================*
       4700-PRINT-INTEREST-BOX.
      *================================================================*
           IF WS-INT-EARNED = ZERO AND WS-INT-PAID = ZERO
           AND WS-CHARGES-PAID = ZERO AND WS-CHARGES-REFUNDED = ZERO
               GO TO 4700-EXIT
           END-IF
           PERFORM 4710-ENSURE-ROOM
           MOVE SPACES                     TO PL-TEXT-LINE
           MOVE '0'                        TO PLX-CC
           MOVE 'INTEREST AND CHARGES IN THIS PERIOD' TO PLX-TEXT
           PERFORM 4950-WRITE-TEXT-LINE

           IF WS-INT-EARNED > ZERO
               MOVE 'CREDIT INTEREST EARNED (GROSS)' TO PLS-LABEL
               MOVE WS-INT-EARNED          TO PLS-AMOUNT
               MOVE SPACES                 TO PLS-DRCR
               WRITE PRINT-RECORD          FROM PL-SUMMARY-LINE
               MOVE 'INCOME TAX DEDUCTED'  TO PLS-LABEL
               MOVE WS-TAX-DEDUCTED        TO PLS-AMOUNT
               WRITE PRINT-RECORD          FROM PL-SUMMARY-LINE
               ADD 2                       TO WS-OUTPUT-LINE-NO
           END-IF
           IF WS-INT-PAID > ZERO
               MOVE 'OVERDRAFT INTEREST PAID'  TO PLS-LABEL
               MOVE WS-INT-PAID            TO PLS-AMOUNT
               MOVE SPACES                 TO PLS-DRCR
               WRITE PRINT-RECORD          FROM PL-SUMMARY-LINE
               ADD 1                       TO WS-OUTPUT-LINE-NO
           END-IF
           IF WS-CHARGES-PAID > ZERO
               MOVE 'ACCOUNT CHARGES PAID' TO PLS-LABEL
               MOVE WS-CHARGES-PAID        TO PLS-AMOUNT
               MOVE SPACES                 TO PLS-DRCR
               WRITE PRINT-RECORD          FROM PL-SUMMARY-LINE
               ADD 1                       TO WS-OUTPUT-LINE-NO
           END-IF
           IF WS-CHARGES-REFUNDED > ZERO
               MOVE 'CHARGES REFUNDED'     TO PLS-LABEL
               MOVE WS-CHARGES-REFUNDED    TO PLS-AMOUNT
               MOVE SPACES                 TO PLS-DRCR
               WRITE PRINT-RECORD          FROM PL-SUMMARY-LINE
               ADD 1                       TO WS-OUTPUT-LINE-NO
           END-IF
           .
       4700-EXIT.
           EXIT.
      *================================================================*
       4710-ENSURE-ROOM.
      *================================================================*
      *    THE BOX NEEDS UP TO 8 LINES - START A NEW PAGE IF THE
      *    CURRENT ONE IS NEARLY FULL.
           IF WS-PAGE-TXN-LINES > WS-PAGE-LIMIT - 8
               PERFORM 4920-NEW-PAGE
           END-IF
           .
      *================================================================*
       4800-PRINT-FSCS-TEXT.
      *================================================================*
           MOVE SPACES                     TO PL-TEXT-LINE
           MOVE '0'                        TO PLX-CC
           STRING 'YOUR ELIGIBLE DEPOSITS WITH MEGABANK PLC ARE '
                  'PROTECTED UP TO A TOTAL OF ' WS-FSCS-LIMIT
                  ' POUNDS BY THE'
                  DELIMITED BY SIZE
                  INTO PLX-TEXT
           END-STRING
           PERFORM 4950-WRITE-TEXT-LINE
           MOVE SPACES                     TO PL-TEXT-LINE
           MOVE ' '                        TO PLX-CC
           STRING 'FINANCIAL SERVICES COMPENSATION SCHEME. '
                  'SEE WWW.FSCS.ORG.UK FOR DETAILS.'
                  DELIMITED BY SIZE
                  INTO PLX-TEXT
           END-STRING
           PERFORM 4950-WRITE-TEXT-LINE
           IF SRQ-CLOSURE
               MOVE SPACES                 TO PL-TEXT-LINE
               MOVE '0'                    TO PLX-CC
               STRING 'THIS IS THE FINAL STATEMENT FOR THIS '
                      'ACCOUNT, WHICH IS NOW CLOSED.'
                      DELIMITED BY SIZE
                      INTO PLX-TEXT
               END-STRING
               PERFORM 4950-WRITE-TEXT-LINE
           END-IF
           .
      *================================================================*
       4900-WRITE-TXN-LINE.
      *================================================================*
           IF WS-PAGE-TXN-LINES >= WS-PAGE-LIMIT
               PERFORM 4920-NEW-PAGE
           END-IF
           WRITE PRINT-RECORD              FROM PL-TXN-LINE
           IF WS-PRINT-STATUS NOT = '00'
               MOVE '4900-WRITE-TXN-LINE'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'PRINT WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-PRINT-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-PAGE-TXN-LINES
                                              WS-OUTPUT-LINE-NO
           .
      *================================================================*
       4920-NEW-PAGE.
      *================================================================*
           ADD 1                           TO WS-PAGE-NO
           STRING SRQ-SORT-CODE (1:2) '-' SRQ-SORT-CODE (3:2) '-'
                  SRQ-SORT-CODE (5:2)
                  DELIMITED BY SIZE
                  INTO PLC-SORT-CODE
           END-STRING
           MOVE SRQ-ACCT-NO                TO PLC-ACCT-NO
           MOVE SRQ-STMT-NO                TO PLC-STMT-NO
           MOVE WS-PAGE-NO                 TO PLC-PAGE
           WRITE PRINT-RECORD              FROM PL-CONT-HEAD
           WRITE PRINT-RECORD              FROM PL-TXN-HEAD
           ADD 2                           TO WS-OUTPUT-LINE-NO
           MOVE ZERO                       TO WS-PAGE-TXN-LINES
           MOVE WS-PN-TXN-LINES            TO WS-PAGE-LIMIT
           .
      *================================================================*
       4950-WRITE-TEXT-LINE.
      *================================================================*
           IF WS-PAGE-TXN-LINES >= WS-PAGE-LIMIT
               PERFORM 4920-NEW-PAGE
           END-IF
           WRITE PRINT-RECORD              FROM PL-TEXT-LINE
           ADD 1                           TO WS-PAGE-TXN-LINES
                                              WS-OUTPUT-LINE-NO
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'REQUESTS READ'            TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'STATEMENTS PRINTED'       TO TL-LABEL
           MOVE WS-PRODUCED-COUNT          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH ELECTRONIC'    TO TL-LABEL
           MOVE WS-ELECTRONIC-COUNT        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SUPPRESSED (DECEASED / GONE AWAY)' TO TL-LABEL
           MOVE WS-SUPPRESSED-COUNT        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'FAILED'                   TO TL-LABEL
           MOVE WS-FAILED-COUNT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TRUNCATED (OVER 3000 ITEMS)' TO TL-LABEL
           MOVE WS-TRUNC-COUNT             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PAGES'                    TO TL-LABEL
           MOVE WS-TOTAL-PAGES             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SHEETS (DUPLEX)'          TO TL-LABEL
           MOVE WS-TOTAL-SHEETS            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE STMTREQ-FILE
                 CIFMAST-FILE
                 ACCTMAST-FILE
                 PRDPARM-FILE
                 PRINT-FILE
                 INDEX-FILE
                 REPORT-FILE

           DISPLAY 'STSTM020 - REQUESTS   ' WS-READ-COUNT
           DISPLAY 'STSTM020 - PRINTED    ' WS-PRODUCED-COUNT
           DISPLAY 'STSTM020 - FAILED     ' WS-FAILED-COUNT
           IF WS-FAILED-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       7000-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-RPT-PAGE
           MOVE WS-RPT-PAGE                TO RH1-PAGE
           WRITE REPORT-RECORD             FROM WS-RPT-HEADING-1
           WRITE REPORT-RECORD             FROM WS-RPT-HEADING-2
           MOVE 3                          TO WS-RPT-LINES
           .
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-RPT-LINES >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SRQ-SORT-CODE              TO RD-SORT-CODE
           MOVE SRQ-ACCT-NO                TO RD-ACCT-NO
           MOVE SRQ-STMT-NO                TO RD-STMT-NO
           MOVE SRQ-PERIOD-TO              TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DISPLAY-DATE
           MOVE WS-DATE-DISPLAY            TO RD-PERIOD-TO
           MOVE WS-TXN-COUNT               TO RD-TXNS
           MOVE WS-PAGE-NO                 TO RD-PAGES
           MOVE WS-CLOSING-BAL             TO RD-CLOSING
           MOVE SIX-DELIVERY               TO RD-DELIVERY
           EVALUATE TRUE
               WHEN WS-FAILED
                   STRING 'FAILED - ' WS-FAIL-REASON
                          DELIMITED BY SIZE
                          INTO RD-RESULT
                   END-STRING
               WHEN WS-SUPPRESS
                   MOVE 'SUPPRESSED - INDEX ONLY' TO RD-RESULT
               WHEN WS-TXN-TRUNCATED
                   MOVE 'PRINTED - TRUNCATED'     TO RD-RESULT
               WHEN OTHER
                   MOVE 'PRINTED'                 TO RD-RESULT
           END-EVALUATE
           WRITE REPORT-RECORD             FROM WS-RPT-DETAIL
           ADD 1                           TO WS-RPT-LINES
           MOVE SPACES                     TO RD-RESULT
           .
      *================================================================*
       8000-READ-REQUEST.
      *================================================================*
           READ STMTREQ-FILE INTO SRQ-STMT-REQUEST
           EVALUATE TRUE
               WHEN WS-STMTREQ-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-STMTREQ-EOF
                   SET WS-END-OF-REQUESTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-REQUEST' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'STMTREQ READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-STMTREQ-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8900-FORMAT-DISPLAY-DATE.
      *================================================================*
      *    WS-DATE-WORK (CCYYMMDD) -> '14 JUL 2010'
           MOVE SPACES                     TO WS-DATE-DISPLAY
           MOVE WS-DW-MM                   TO WS-MONTH-NO
           IF WS-MONTH-NO < 1 OR WS-MONTH-NO > 12
               MOVE WS-DATE-WORK-X         TO WS-DATE-DISPLAY
           ELSE
               STRING WS-DW-DD ' '
                      WS-MONTH-NAMES ((WS-MONTH-NO - 1) * 3 + 1:3)
                      ' ' WS-DW-CCYY
                      DELIMITED BY SIZE
                      INTO WS-DATE-DISPLAY
               END-STRING
           END-IF
           .
      *================================================================*
       8950-FORMAT-DB2-DATE.
      *================================================================*
           MOVE SPACES                     TO WS-FROM-DB2
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-FROM-DB2
           END-STRING
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           MOVE SRQ-STMT-REQUEST (1:40)    TO ABN-KEY-DATA
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
