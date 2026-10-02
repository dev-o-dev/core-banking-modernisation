       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CGNTF010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2009-10-01.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CGNTF010                                            *
      * TITLE    : ACCOUNT CHARGES - PRE-NOTIFICATION EXTRACT          *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CGCHG01 STEP CG030                                  *
      *----------------------------------------------------------------*
      * INPUT    : CIFMAST  - CIF CUSTOMER MASTER (VSAM KSDS, RANDOM)  *
      *            ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM)   *
      *            PRDPARM  - PRODUCT PARAMETERS (VSAM KSDS, RANDOM)   *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : CHGPEND  - PENDING CHARGES (VSAM KSDS, SEQ I-O)     *
      * OUTPUT   : NOTIFY   - LETTER EXTRACT FOR DOCUMENT SERVICES     *
      *                       CGNTFREC (FB 300)                        *
      *            REPORT   - NOTIFICATION CONTROL REPORT (FBA 133)    *
      *----------------------------------------------------------------*
      * EVERY PENDING CHARGE AT STATUS 'C' (CALCULATED) IS NOTIFIED.   *
      * CHARGES ARE GROUPED BY ACCOUNT AND CHARGING PERIOD (CONTROL    *
      * BREAK ON THE FIRST 22 BYTES OF CGP-KEY) - ONE NOTICE PER       *
      * ACCOUNT PER PERIOD. SUPPRESSED CHARGES ARE NOT NOTIFIED.       *
      *                                                                *
      * THE NOTICE ALSO SHOWS, FOR INFORMATION, ANY DEBIT INTEREST     *
      * ACCRUED SO FAR (ACM-ACCRUED-INT-DR) - THE FCA/OFT "WHAT YOU    *
      * WILL PAY" REQUIREMENT. THIS IS NOT A CHARGE RECORD.            *
      *                                                                *
      * CUSTOMERS WITH AN EMAIL ADDRESS AND CIF-MKTG-EMAIL-IND = 'Y'   *
      * ARE FLAGGED ELECTRONIC. THIS IS A KNOWN SHORTCUT: THE EMAIL    *
      * MARKETING FLAG IS USED AS A PROXY FOR "PAPERLESS" BECAUSE THE  *
      * CIF HAS NO PAPERLESS INDICATOR (PIR 2009/233 - OPEN).          *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2009-10-01 AOS  ORIGINAL VERSION (PSR 2009)                    *
      * 2010-01-11 AOS  DEBIT INTEREST INFORMATION LINE                *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
           SELECT CHGPEND-FILE     ASSIGN TO CHGPEND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS CGP-KEY
                                   FILE STATUS IS WS-CHGPEND-STATUS.
           SELECT NOTIFY-FILE      ASSIGN TO NOTIFY
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-NOTIFY-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
      *
       FD  CHGPEND-FILE.
           COPY CGCHGREC.
      *
       FD  NOTIFY-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  NOTIFY-RECORD                   PIC X(300).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CGNTF010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-NOTFND           VALUE '23'.
           05  WS-CHGPEND-STATUS           PIC X(02).
               88  WS-CHGPEND-OK               VALUE '00'.
               88  WS-CHGPEND-EOF              VALUE '10'.
           05  WS-NOTIFY-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CHARGES           VALUE 'Y'.
           05  WS-GROUP-OPEN-IND           PIC X(01) VALUE 'N'.
               88  WS-GROUP-OPEN               VALUE 'Y'.
           05  WS-CUST-FOUND-IND           PIC X(01) VALUE 'N'.
               88  WS-CUST-FOUND               VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-GROUP-KEY                PIC X(22) VALUE LOW-VALUES.
           05  WS-THIS-KEY                 PIC X(22).
           05  WS-GROUP-ITEMS              PIC 9(03).
           05  WS-GROUP-TOTAL              PIC S9(13)V99 COMP-3.
           05  WS-DR-INT-INFO              PIC S9(13)V99 COMP-3.
           05  WS-ADDR-SUB                 PIC S9(04) COMP.
           05  WS-ADDR-LINES.
               10  WS-ADDR-LINE            PIC X(40) OCCURS 4 TIMES.
           05  WS-SALUTATION               PIC X(50).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOTIFIED-ITEMS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOTICE-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAPER-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-ELECTRONIC-COUNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-CUSTOMER-COUNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RECORDS-WRITTEN          PIC S9(09) COMP-3 VALUE 0.
           05  WS-HASH-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
      *
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CGNTF010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - CHARGES PRE-NOTIFICATION CONTROL'.
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
           05  FILLER            PIC X(14) VALUE 'CIF NUMBER'.
           05  FILLER            PIC X(06) VALUE 'ITEMS'.
           05  FILLER            PIC X(16) VALUE '       TOTAL NET'.
           05  FILLER            PIC X(16) VALUE '    DR INT INFO'.
           05  FILLER            PIC X(12) VALUE 'APPLY DATE'.
           05  FILLER            PIC X(06) VALUE 'DLVY'.
           05  FILLER            PIC X(43) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ITEMS                    PIC ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TOTAL                    PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DR-INT                   PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-APPLY-DATE               PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DELIVERY                 PIC X(01).
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY CGNTFREC.
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
           PERFORM 2000-PROCESS-CHARGE
               UNTIL WS-END-OF-CHARGES
           IF WS-GROUP-OPEN
               PERFORM 2500-CLOSE-GROUP
           END-IF
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CIFMAST-FILE
                       ACCTMAST-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                I-O    CHGPEND-FILE
                OUTPUT NOTIFY-FILE
                       REPORT-FILE

           IF NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-CHGPEND-OK
           OR WS-NOTIFY-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-CIFMAST-STATUS ' ' WS-ACCTMAST-STATUS ' '
                      WS-PRDPARM-STATUS ' ' WS-CHGPEND-STATUS ' '
                      WS-NOTIFY-STATUS ' ' WS-REPORT-STATUS ' '
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
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               MOVE DATECARD-RECORD (1:40) TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO HD1-BUS-DATE

           PERFORM 8000-READ-CHARGE
           .
      *================================================================*
       2000-PROCESS-CHARGE.
      *================================================================*
           IF CGP-CALCULATED
               MOVE CGP-KEY (1:22)         TO WS-THIS-KEY
               IF WS-THIS-KEY NOT = WS-GROUP-KEY
                   IF WS-GROUP-OPEN
                       PERFORM 2500-CLOSE-GROUP
                   END-IF
                   PERFORM 2100-OPEN-GROUP
               END-IF
               PERFORM 2200-NOTIFY-CHARGE
           END-IF

           PERFORM 8000-READ-CHARGE
           .
      *================================================================*
       2100-OPEN-GROUP.
      *================================================================*
           MOVE WS-THIS-KEY                TO WS-GROUP-KEY
           SET WS-GROUP-OPEN               TO TRUE
           MOVE ZERO                       TO WS-GROUP-ITEMS
                                              WS-GROUP-TOTAL
                                              WS-DR-INT-INFO

      *---- ACCOUNT (FOR DEBIT INTEREST INFORMATION) ------------------*
           MOVE CGP-SORT-CODE              TO ACM-SORT-CODE
           MOVE CGP-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF WS-ACCTMAST-OK
               MOVE ACM-ACCRUED-INT-DR     TO WS-DR-INT-INFO
           END-IF

      *---- PRODUCT NAME ----------------------------------------------*
           MOVE CGP-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF NOT WS-PRDPARM-OK
               MOVE CGP-PROD-CD            TO PRD-SHORT-NAME
           END-IF

      *---- CUSTOMER NAME AND ADDRESS ---------------------------------*
           MOVE CGP-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           IF WS-CIFMAST-OK
               SET WS-CUST-FOUND           TO TRUE
           ELSE
               MOVE 'N'                    TO WS-CUST-FOUND-IND
               ADD 1                       TO WS-NO-CUSTOMER-COUNT
           END-IF

           PERFORM 2110-WRITE-ADDRESS
           .
      *================================================================*
       2110-WRITE-ADDRESS.
      *================================================================*
           MOVE SPACES                     TO CNT-NOTIFY-RECORD
           SET CNT-ADDRESS-REC             TO TRUE
           MOVE CGP-SORT-CODE              TO CNT-SORT-CODE
           MOVE CGP-ACCT-NO                TO CNT-ACCT-NO
           MOVE CGP-CIF-NO                 TO CNTA-CIF-NO

           IF WS-CUST-FOUND
               PERFORM 2120-BUILD-SALUTATION
               PERFORM 2130-BUILD-ADDRESS
               MOVE WS-SALUTATION          TO CNTA-SALUTATION
               MOVE WS-ADDR-LINE (1)       TO CNTA-ADDR-LINE1
               MOVE WS-ADDR-LINE (2)       TO CNTA-ADDR-LINE2
               MOVE WS-ADDR-LINE (3)       TO CNTA-ADDR-LINE3
               MOVE WS-ADDR-LINE (4)       TO CNTA-ADDR-LINE4
               MOVE CIF-POST-CD            TO CNTA-POST-CD
               IF CIF-EMAIL-ADDR NOT = SPACES
               AND CIF-MKTG-EMAIL-IND = 'Y'
                   SET CNTA-ELECTRONIC     TO TRUE
                   ADD 1                   TO WS-ELECTRONIC-COUNT
               ELSE
                   SET CNTA-PAPER          TO TRUE
                   ADD 1                   TO WS-PAPER-COUNT
               END-IF
           ELSE
      *        NO CIF - DOCUMENT SERVICES ROUTE TO THE BRANCH (RETURNED
      *        MAIL QUEUE) USING THE SORT CODE.
               MOVE 'DEAR CUSTOMER'        TO CNTA-SALUTATION
               MOVE 'C/O BRANCH - ADDRESS NOT HELD'
                                           TO CNTA-ADDR-LINE1
               SET CNTA-PAPER              TO TRUE
               ADD 1                       TO WS-PAPER-COUNT
           END-IF

           MOVE PRD-SHORT-NAME             TO CNTA-PROD-NAME
           MOVE CGP-PERIOD-START           TO CNTA-PERIOD-START
           MOVE CGP-PERIOD-END             TO CNTA-PERIOD-END
           MOVE CGP-APPLY-DATE             TO CNTA-APPLY-DATE
           PERFORM 2900-WRITE-NOTIFY
           ADD 1                           TO WS-NOTICE-COUNT
           .
      *================================================================*
       2120-BUILD-SALUTATION.
      *================================================================*
           MOVE SPACES                     TO WS-SALUTATION
           IF CIF-BUSINESS
               STRING 'DEAR SIR OR MADAM' DELIMITED BY SIZE
                      INTO WS-SALUTATION
               END-STRING
           ELSE
               IF CIF-TITLE NOT = SPACES
                   STRING 'DEAR '        DELIMITED BY SIZE
                          CIF-TITLE      DELIMITED BY SPACE
                          ' '            DELIMITED BY SIZE
                          CIF-LAST-NM    DELIMITED BY '  '
                          INTO WS-SALUTATION
                   END-STRING
               ELSE
                   STRING 'DEAR '        DELIMITED BY SIZE
                          CIF-FIRST-NM   DELIMITED BY SPACE
                          ' '            DELIMITED BY SIZE
                          CIF-LAST-NM    DELIMITED BY '  '
                          INTO WS-SALUTATION
                   END-STRING
               END-IF
           END-IF
           .
      *================================================================*
       2130-BUILD-ADDRESS.
      *================================================================*
      *    UP TO 4 LINES - NAME, ADDRESS LINES, TOWN, COUNTY. BLANK
      *    LINES ARE CLOSED UP. THE ADDRESS FIELDS ARE 60 WIDE ON THE
      *    CIF BUT DOCUMENT SERVICES ONLY PRINT 40 (ROYAL MAIL MAILSORT
      *    LIMIT) - LONGER LINES ARE TRUNCATED. PIR 2009/240.
           MOVE SPACES                     TO WS-ADDR-LINES
           MOVE ZERO                       TO WS-ADDR-SUB

           ADD 1                           TO WS-ADDR-SUB
           STRING CIF-TITLE     DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-FIRST-NM  DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-LAST-NM   DELIMITED BY '  '
                  INTO WS-ADDR-LINE (WS-ADDR-SUB)
           END-STRING

           IF CIF-ADDR-LINE1 NOT = SPACES
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-ADDR-LINE1         TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           IF CIF-ADDR-LINE2 NOT = SPACES
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-ADDR-LINE2         TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           IF CIF-TOWN NOT = SPACES AND WS-ADDR-SUB < 4
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-TOWN               TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           IF CIF-COUNTY NOT = SPACES AND WS-ADDR-SUB < 4
               ADD 1                       TO WS-ADDR-SUB
               MOVE CIF-COUNTY             TO WS-ADDR-LINE (WS-ADDR-SUB)
           END-IF
           .
      *================================================================*
       2200-NOTIFY-CHARGE.
      *================================================================*
           MOVE SPACES                     TO CNT-NOTIFY-RECORD
           SET CNT-CHARGE-REC              TO TRUE
           MOVE CGP-SORT-CODE              TO CNT-SORT-CODE
           MOVE CGP-ACCT-NO                TO CNT-ACCT-NO
           MOVE CGP-CHARGE-TYPE            TO CNTC-CHARGE-TYPE
           MOVE CGP-DESCRIPTION            TO CNTC-DESCRIPTION
           MOVE CGP-ITEM-COUNT             TO CNTC-ITEM-COUNT
           MOVE CGP-UNIT-AMT               TO CNTC-UNIT-AMT
           MOVE CGP-GROSS-AMT              TO CNTC-GROSS-AMT
           MOVE CGP-CAP-REDUCTION          TO CNTC-CAP-REDUCTION
           MOVE CGP-NET-AMT                TO CNTC-NET-AMT
           PERFORM 2900-WRITE-NOTIFY

           ADD 1                           TO WS-GROUP-ITEMS
           ADD CGP-NET-AMT                 TO WS-GROUP-TOTAL
           ADD CGP-NET-AMT                 TO WS-HASH-TOTAL
           ADD 1                           TO WS-NOTIFIED-ITEMS

           SET CGP-NOTIFIED                TO TRUE
           MOVE WS-BUS-DATE                TO CGP-NOTIFY-DATE
           REWRITE CGP-CHARGE-RECORD
           IF NOT WS-CHGPEND-OK
               MOVE '2200-NOTIFY-CHARGE'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHGPEND REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-CHGPEND-STATUS      TO ABN-FILE-STATUS
               MOVE CGP-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2500-CLOSE-GROUP.
      *================================================================*
           MOVE SPACES                     TO CNT-NOTIFY-RECORD
           SET CNT-TOTAL-REC               TO TRUE
           MOVE WS-GROUP-KEY (1:6)         TO CNT-SORT-CODE
           MOVE WS-GROUP-KEY (7:8)         TO CNT-ACCT-NO
           MOVE WS-GROUP-ITEMS             TO CNTT-ITEM-COUNT
           MOVE WS-GROUP-TOTAL             TO CNTT-TOTAL-NET
           MOVE WS-DR-INT-INFO             TO CNTT-DEBIT-INT-INFO
           PERFORM 2900-WRITE-NOTIFY

           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE WS-GROUP-KEY (1:6)         TO DL-SORT-CODE
           MOVE WS-GROUP-KEY (7:8)         TO DL-ACCT-NO
           MOVE CNTA-CIF-NO                TO DL-CIF-NO
           MOVE WS-GROUP-ITEMS             TO DL-ITEMS
           MOVE WS-GROUP-TOTAL             TO DL-TOTAL
           MOVE WS-DR-INT-INFO             TO DL-DR-INT
           MOVE CGP-APPLY-DATE             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO DL-APPLY-DATE
           IF WS-CUST-FOUND
               MOVE SPACES                 TO DL-NOTE
           ELSE
               MOVE 'CIF NOT FOUND - SENT C/O BRANCH' TO DL-NOTE
           END-IF
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT

           MOVE 'N'                        TO WS-GROUP-OPEN-IND
           .
      *================================================================*
       2900-WRITE-NOTIFY.
      *================================================================*
           WRITE NOTIFY-RECORD             FROM CNT-NOTIFY-RECORD
           IF WS-NOTIFY-STATUS NOT = '00'
               MOVE '2900-WRITE-NOTIFY'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'NOTIFY WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-NOTIFY-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-RECORDS-WRITTEN
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           MOVE SPACES                     TO CNT-NOTIFY-RECORD
           SET CNT-FILE-TRAILER            TO TRUE
           MOVE ALL '9'                    TO CNT-SORT-CODE
                                              CNT-ACCT-NO
           MOVE WS-NOTICE-COUNT            TO CNTZ-ACCOUNT-COUNT
           COMPUTE CNTZ-RECORD-COUNT = WS-RECORDS-WRITTEN + 1
           MOVE WS-HASH-TOTAL              TO CNTZ-HASH-TOTAL
           PERFORM 2900-WRITE-NOTIFY

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'NOTICES PRODUCED'         TO TL-LABEL
           MOVE WS-NOTICE-COUNT            TO TL-COUNT
           MOVE WS-HASH-TOTAL              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH PAPER'         TO TL-LABEL
           MOVE WS-PAPER-COUNT             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH ELECTRONIC'    TO TL-LABEL
           MOVE WS-ELECTRONIC-COUNT        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CHARGE ITEMS NOTIFIED'    TO TL-LABEL
           MOVE WS-NOTIFIED-ITEMS          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMER NOT FOUND'       TO TL-LABEL
           MOVE WS-NO-CUSTOMER-COUNT       TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE CIFMAST-FILE
                 ACCTMAST-FILE
                 PRDPARM-FILE
                 CHGPEND-FILE
                 NOTIFY-FILE
                 REPORT-FILE

           DISPLAY 'CGNTF010 - CHARGE RECORDS READ  ' WS-READ-COUNT
           DISPLAY 'CGNTF010 - NOTICES              ' WS-NOTICE-COUNT
           DISPLAY 'CGNTF010 - ITEMS NOTIFIED       ' WS-NOTIFIED-ITEMS

           IF WS-NO-CUSTOMER-COUNT > ZERO
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
           WRITE REPORT-RECORD             FROM WS-HEADING-3
           MOVE 4                          TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-CHARGE.
      *================================================================*
           READ CHGPEND-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-CHGPEND-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-CHGPEND-EOF
                   SET WS-END-OF-CHARGES   TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-CHARGE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CHGPEND READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-CHGPEND-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8900-FORMAT-DATE.
      *================================================================*
           MOVE SPACES                     TO WS-DATE-DISPLAY
           STRING WS-DW-DD '/' WS-DW-MM '/' WS-DW-CCYY
                  DELIMITED BY SIZE
                  INTO WS-DATE-DISPLAY
           END-STRING
           .
      *
           COPY ABNDPROC.
