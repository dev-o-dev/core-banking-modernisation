       IDENTIFICATION DIVISION.
       PROGRAM-ID.    INTAX010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2007-04-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : INTAX010                                            *
      * TITLE    : ANNUAL INTEREST CERTIFICATES AND HMRC RETURN OF     *
      *            INTEREST (BBSI)                                     *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : INTAX010                                            *
      * JCL      : INTAX01 (ANNUAL - RUN ON REQUEST AFTER 5 APRIL)     *
      *----------------------------------------------------------------*
      * INPUT    : PARMIN   - TAX YEAR CARD 'TAXYEAR=CCYY/CCYY'        *
      *            MBK.INTPAID - INTEREST PAID IN THE TAX YEAR         *
      *            CIFMAST  - CIF CUSTOMER MASTER (RANDOM)             *
      *            ACCTMAST - DDA ACCOUNT MASTER (RANDOM)              *
      * OUTPUT   : CERTS    - CERTIFICATE EXTRACT FOR DOCUMENT         *
      *                       SERVICES (TEMPLATE TAXCERT1, FB 250)     *
      *            BBSI     - HMRC RETURN OF INTEREST FILE (FB 300)    *
      *            REPORT   - CONTROL REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * ONE CERTIFICATE PER CUSTOMER LISTING EACH ACCOUNT THAT PAID    *
      * CREDIT INTEREST IN THE TAX YEAR (6 APRIL - 5 APRIL): GROSS,    *
      * TAX DEDUCTED, NET. CUSTOMERS WHOSE ONLY INTEREST IS BELOW      *
      * WS-MIN-CERT-AMT DO NOT GET A CERTIFICATE (STILL REPORTED TO    *
      * HMRC).                                                         *
      *                                                                *
      * HMRC BBSI RETURN (TAXES MANAGEMENT ACT 1970 S17): EVERY        *
      * ACCOUNT WHERE GROSS INTEREST >= WS-HMRC-THRESHOLD IS REPORTED  *
      * WITH THE HOLDER'S NAME, ADDRESS AND NI NUMBER. NON-RESIDENT    *
      * (ACM-TAX-NON-RESIDENT) AND R85 GROSS ACCOUNTS ARE FLAGGED.     *
      * THE FILE IS LOADED INTO HMRC'S SUBMISSION TOOL BY FINANCE      *
      * (NOT TRANSMITTED FROM THE HOST).                               *
      *                                                                *
      * DEBIT INTEREST ROWS (CR_DR_IND = 'D') ARE IGNORED.             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2007-04-02 AOS  ORIGINAL VERSION (READS INTPAID - REPLACES THE *
      *                 VSAM TAXFILE CERTIFICATE RUN)                  *
      * 2009-04-14 AOS  BBSI MINIMUM REPORTING THRESHOLD AS PARM       *
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
           SELECT CERTS-FILE       ASSIGN TO CERTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CERTS-STATUS.
           SELECT BBSI-FILE        ASSIGN TO BBSI
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-BBSI-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD.
           05  PARM-KEYWORD                PIC X(08).
               88  PARM-TAX-YEAR               VALUE 'TAXYEAR='.
               88  PARM-THRESHOLD              VALUE 'MINBBSI='.
           05  PARM-VALUE                  PIC X(09).
           05  FILLER                      PIC X(63).
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CERTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CERTS-RECORD                    PIC X(250).
       FD  BBSI-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BBSI-RECORD                     PIC X(300).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'INTAX010'.
      *
       01  WS-CONSTANTS.
           05  WS-MIN-CERT-AMT             PIC S9(13)V99 COMP-3
                                           VALUE +0.01.
           05  WS-HMRC-THRESHOLD           PIC S9(13)V99 COMP-3
                                           VALUE +0.01.
           05  WS-BANK-REF                 PIC X(10) VALUE 'MBKP-BBSI'.
      *
       01  WS-FILE-STATUSES.
           05  WS-PARMIN-STATUS            PIC X(02).
               88  WS-PARMIN-OK                VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
           05  WS-CERTS-STATUS             PIC X(02).
           05  WS-BBSI-STATUS              PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOC-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CURSOR            VALUE 'Y'.
           05  WS-CUST-OK-IND              PIC X(01).
               88  WS-CUST-OK                  VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-TAX-YEAR                 PIC X(09) VALUE SPACES.
           05  WS-CURR-CIF                 PIC X(12) VALUE LOW-VALUES.
           05  WS-CURR-ACCT                PIC X(14) VALUE LOW-VALUES.
           05  WS-CUST-NAME                PIC X(60).
           05  WS-THRESHOLD-NUM            PIC 9(07)V99.
      *
      *---- ACCOUNTS FOR ONE CUSTOMER --------------------------------*
       01  WS-CUST-ACCOUNTS.
           05  WS-CA-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-CA-ENTRY                 OCCURS 50 TIMES
                                           INDEXED BY CA-IDX.
               10  WS-CA-SORT-CODE         PIC X(06).
               10  WS-CA-ACCT-NO           PIC X(08).
               10  WS-CA-PROD-CD           PIC X(10).
               10  WS-CA-GROSS             PIC S9(13)V99 COMP-3.
               10  WS-CA-TAX               PIC S9(13)V99 COMP-3.
               10  WS-CA-NET               PIC S9(13)V99 COMP-3.
               10  WS-CA-TAX-STATUS        PIC X(01).
       01  WS-CUST-GROSS                   PIC S9(13)V99 COMP-3.
       01  WS-CUST-TAX                     PIC S9(13)V99 COMP-3.
       01  WS-CUST-NET                     PIC S9(13)V99 COMP-3.
      *
      *---- CERTIFICATE EXTRACT RECORDS ------------------------------*
       01  WS-CERT-HEADER.
           05  CTH-REC-TYPE                PIC X(01) VALUE 'H'.
           05  CTH-CIF-NO                  PIC X(12).
           05  CTH-TAX-YEAR                PIC X(09).
           05  CTH-NAME                    PIC X(60).
           05  CTH-ADDR-LINE1              PIC X(40).
           05  CTH-ADDR-LINE2              PIC X(40).
           05  CTH-TOWN                    PIC X(30).
           05  CTH-POST-CD                 PIC X(10).
           05  CTH-ACCOUNT-COUNT           PIC 9(03).
           05  FILLER                      PIC X(45).
       01  WS-CERT-DETAIL.
           05  CTD-REC-TYPE                PIC X(01) VALUE 'D'.
           05  CTD-CIF-NO                  PIC X(12).
           05  CTD-SORT-CODE               PIC X(06).
           05  CTD-ACCT-NO                 PIC X(08).
           05  CTD-PROD-CD                 PIC X(10).
           05  CTD-GROSS                   PIC 9(11)V99.
           05  CTD-TAX                     PIC 9(11)V99.
           05  CTD-NET                     PIC 9(11)V99.
           05  CTD-TAX-STATUS              PIC X(01).
           05  FILLER                      PIC X(173).
       01  WS-CERT-TOTAL.
           05  CTT-REC-TYPE                PIC X(01) VALUE 'T'.
           05  CTT-CIF-NO                  PIC X(12).
           05  CTT-GROSS                   PIC 9(11)V99.
           05  CTT-TAX                     PIC 9(11)V99.
           05  CTT-NET                     PIC 9(11)V99.
           05  FILLER                      PIC X(198).
      *
      *---- HMRC BBSI RECORD -----------------------------------------*
       01  WS-BBSI-RECORD.
           05  BB-REC-TYPE                 PIC X(01).
           05  BB-BANK-REF                 PIC X(10).
           05  BB-TAX-YEAR                 PIC X(09).
           05  BB-SORT-CODE                PIC X(06).
           05  BB-ACCT-NO                  PIC X(08).
           05  BB-TITLE                    PIC X(06).
           05  BB-FORENAME                 PIC X(40).
           05  BB-SURNAME                  PIC X(40).
           05  BB-NINO                     PIC X(13).
           05  BB-DOB                      PIC 9(08).
           05  BB-ADDR-LINE1               PIC X(40).
           05  BB-ADDR-LINE2               PIC X(40).
           05  BB-POST-CD                  PIC X(10).
           05  BB-GROSS                    PIC 9(11)V99.
           05  BB-TAX                      PIC 9(11)V99.
           05  BB-STATUS-FLAG              PIC X(01).
           05  BB-JOINT-IND                PIC X(01).
           05  FILLER                      PIC X(41).
      *
       01  WS-COUNTERS.
           05  WS-ROWS-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CUSTOMERS                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CERTIFICATES             PIC S9(09) COMP-3 VALUE 0.
           05  WS-BBSI-RECORDS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-CIF                   PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-NINO                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCT-OVERFLOW            PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-GROSS              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-TAX                PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'INTAX010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(45)
               VALUE 'MEGABANK PLC - INTEREST CERTIFICATES / BBSI'.
           05  FILLER                      PIC X(10) VALUE 'TAX YEAR '.
           05  HD1-TAX-YEAR                PIC X(09).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(22) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
       01  WS-EXCEPTION-LINE.
           05  EL-CC                       PIC X(01) VALUE ' '.
           05  EL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-ACCOUNT                  PIC X(15).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-TEXT                     PIC X(60).
           05  FILLER                      PIC X(41) VALUE SPACES.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLINTP END-EXEC.
      *
       01  HV-TAX-YEAR                     PIC X(09).
       01  HV-GROSS                        PIC S9(13)V99 COMP-3.
       01  HV-TAX                          PIC S9(13)V99 COMP-3.
       01  HV-NET                          PIC S9(13)V99 COMP-3.
      *
           EXEC SQL DECLARE TAXCSR CURSOR FOR
               SELECT CIF_NO, SORT_CODE, ACCT_NO, PROD_CD,
                      MAX(TAX_STATUS),
                      SUM(GROSS_AMT), SUM(TAX_AMT), SUM(NET_AMT)
                 FROM MBK.INTPAID
                WHERE TAX_YEAR  = :HV-TAX-YEAR
                  AND CR_DR_IND = 'C'
                GROUP BY CIF_NO, SORT_CODE, ACCT_NO, PROD_CD
                ORDER BY CIF_NO, SORT_CODE, ACCT_NO
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
           PERFORM 2000-PROCESS-ROWS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PARMIN-FILE
                       CIFMAST-FILE
                       ACCTMAST-FILE
                OUTPUT CERTS-FILE
                       BBSI-FILE
                       REPORT-FILE
           IF NOT WS-PARMIN-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-CERTS-STATUS NOT = '00'
           OR WS-BBSI-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           READ PARMIN-FILE
           PERFORM UNTIL NOT WS-PARMIN-OK
               EVALUATE TRUE
                   WHEN PARM-TAX-YEAR
                       MOVE PARM-VALUE     TO WS-TAX-YEAR
                   WHEN PARM-THRESHOLD
                       IF PARM-VALUE NUMERIC
                           MOVE PARM-VALUE TO WS-THRESHOLD-NUM
                           MOVE WS-THRESHOLD-NUM TO WS-HMRC-THRESHOLD
                       END-IF
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               READ PARMIN-FILE
           END-PERFORM
           CLOSE PARMIN-FILE

           IF WS-TAX-YEAR (5:1) NOT = '/'
           OR WS-TAX-YEAR (1:4) NOT NUMERIC
           OR WS-TAX-YEAR (6:4) NOT NUMERIC
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'TAXYEAR=CCYY/CCYY CARD MISSING OR INVALID'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           MOVE WS-TAX-YEAR                TO HV-TAX-YEAR
                                              HD1-TAX-YEAR
           DISPLAY 'INTAX010 - TAX YEAR ' WS-TAX-YEAR
                   ' BBSI THRESHOLD ' WS-HMRC-THRESHOLD

      *---- BBSI HEADER -----------------------------------------------*
           MOVE SPACES                     TO WS-BBSI-RECORD
           MOVE 'H'                        TO BB-REC-TYPE
           MOVE WS-BANK-REF                TO BB-BANK-REF
           MOVE WS-TAX-YEAR                TO BB-TAX-YEAR
           WRITE BBSI-RECORD               FROM WS-BBSI-RECORD

           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       2000-PROCESS-ROWS.
      *================================================================*
           EXEC SQL OPEN TAXCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2000-PROCESS-ROWS'    TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           PERFORM UNTIL WS-END-OF-CURSOR
               EXEC SQL
                   FETCH TAXCSR
                    INTO :INP-CIF-NO, :INP-SORT-CODE, :INP-ACCT-NO,
                         :INP-PROD-CD, :INP-TAX-STATUS,
                         :HV-GROSS, :HV-TAX, :HV-NET
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-ROWS-READ
                       IF INP-CIF-NO NOT = WS-CURR-CIF
                           PERFORM 2900-END-CUSTOMER THRU 2900-EXIT
                           PERFORM 2050-START-CUSTOMER
                       END-IF
                       PERFORM 2100-ADD-ACCOUNT
                   WHEN +100
                       SET WS-END-OF-CURSOR TO TRUE
                   WHEN OTHER
                       MOVE '2000-PROCESS-ROWS' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
           PERFORM 2900-END-CUSTOMER THRU 2900-EXIT
           EXEC SQL CLOSE TAXCSR END-EXEC
           .
      *================================================================*
       2050-START-CUSTOMER.
      *================================================================*
           MOVE INP-CIF-NO                 TO WS-CURR-CIF
           MOVE ZERO                       TO WS-CA-COUNT
                                              WS-CUST-GROSS
                                              WS-CUST-TAX
                                              WS-CUST-NET
           ADD 1                           TO WS-CUSTOMERS
           MOVE 'N'                        TO WS-CUST-OK-IND
           MOVE INP-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           IF WS-CIFMAST-OK
               SET WS-CUST-OK              TO TRUE
               MOVE SPACES                 TO WS-CUST-NAME
               STRING CIF-TITLE     DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-FIRST-NM  DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-LAST-NM   DELIMITED BY '  '
                      INTO WS-CUST-NAME
               END-STRING
           ELSE
               ADD 1                       TO WS-NO-CIF
               MOVE 'CUSTOMER NOT ON CIF - BBSI WITHOUT NAME/ADDRESS'
                                           TO EL-TEXT
               MOVE INP-CIF-NO             TO EL-CIF-NO
               MOVE SPACES                 TO EL-ACCOUNT
               PERFORM 7100-EXCEPTION
           END-IF
           .
      *================================================================*
       2100-ADD-ACCOUNT.
      *================================================================*
           IF WS-CA-COUNT >= 50
               ADD 1                       TO WS-ACCT-OVERFLOW
           ELSE
               ADD 1                       TO WS-CA-COUNT
               SET CA-IDX                  TO WS-CA-COUNT
               MOVE INP-SORT-CODE          TO WS-CA-SORT-CODE (CA-IDX)
               MOVE INP-ACCT-NO            TO WS-CA-ACCT-NO (CA-IDX)
               MOVE INP-PROD-CD            TO WS-CA-PROD-CD (CA-IDX)
               MOVE HV-GROSS               TO WS-CA-GROSS (CA-IDX)
               MOVE HV-TAX                 TO WS-CA-TAX (CA-IDX)
               MOVE HV-NET                 TO WS-CA-NET (CA-IDX)
               MOVE INP-TAX-STATUS         TO WS-CA-TAX-STATUS (CA-IDX)
           END-IF
           ADD HV-GROSS                    TO WS-CUST-GROSS
                                              WS-TOTAL-GROSS
           ADD HV-TAX                      TO WS-CUST-TAX
                                              WS-TOTAL-TAX
           ADD HV-NET                      TO WS-CUST-NET

           IF HV-GROSS >= WS-HMRC-THRESHOLD
               PERFORM 2200-WRITE-BBSI
           END-IF
           .
      *================================================================*
       2200-WRITE-BBSI.
      *================================================================*
           MOVE SPACES                     TO WS-BBSI-RECORD
           MOVE 'D'                        TO BB-REC-TYPE
           MOVE WS-BANK-REF                TO BB-BANK-REF
           MOVE WS-TAX-YEAR                TO BB-TAX-YEAR
           MOVE INP-SORT-CODE              TO BB-SORT-CODE
           MOVE INP-ACCT-NO                TO BB-ACCT-NO
           IF WS-CUST-OK
               MOVE CIF-TITLE              TO BB-TITLE
               MOVE CIF-FIRST-NM           TO BB-FORENAME
               MOVE CIF-LAST-NM            TO BB-SURNAME
               MOVE CIF-NAT-INS-NO         TO BB-NINO
               MOVE CIF-DOB                TO BB-DOB
               MOVE CIF-ADDR-LINE1         TO BB-ADDR-LINE1
               MOVE CIF-ADDR-LINE2         TO BB-ADDR-LINE2
               MOVE CIF-POST-CD            TO BB-POST-CD
               IF CIF-NAT-INS-NO = SPACES AND CIF-PERSONAL
                   ADD 1                   TO WS-NO-NINO
               END-IF
           ELSE
               MOVE ZERO                   TO BB-DOB
           END-IF
           MOVE HV-GROSS                   TO BB-GROSS
           MOVE HV-TAX                     TO BB-TAX
      *    STATUS FLAG: N NET, G GROSS (R85), X NON-RESIDENT.
           IF INP-TAX-STATUS = SPACE
               MOVE 'N'                    TO BB-STATUS-FLAG
           ELSE
               MOVE INP-TAX-STATUS         TO BB-STATUS-FLAG
           END-IF
      *    JOINT ACCOUNTS CANNOT BE IDENTIFIED (ONE CIF PER ACCOUNT).
           MOVE 'N'                        TO BB-JOINT-IND
           WRITE BBSI-RECORD               FROM WS-BBSI-RECORD
           IF WS-BBSI-STATUS NOT = '00'
               MOVE '2200-WRITE-BBSI'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'BBSI WRITE FAILED'    TO ABN-MESSAGE
               MOVE WS-BBSI-STATUS         TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-BBSI-RECORDS
           .
      *================================================================*
       2900-END-CUSTOMER.
      *================================================================*
           IF WS-CURR-CIF = LOW-VALUES
               GO TO 2900-EXIT
           END-IF
           IF NOT WS-CUST-OK
           OR WS-CUST-GROSS < WS-MIN-CERT-AMT
           OR CIF-DECEASED
               GO TO 2900-EXIT
           END-IF

           MOVE WS-CURR-CIF                TO CTH-CIF-NO
           MOVE WS-TAX-YEAR                TO CTH-TAX-YEAR
           MOVE WS-CUST-NAME               TO CTH-NAME
           MOVE CIF-ADDR-LINE1             TO CTH-ADDR-LINE1
           MOVE CIF-ADDR-LINE2             TO CTH-ADDR-LINE2
           MOVE CIF-TOWN                   TO CTH-TOWN
           MOVE CIF-POST-CD                TO CTH-POST-CD
           MOVE WS-CA-COUNT                TO CTH-ACCOUNT-COUNT
           WRITE CERTS-RECORD              FROM WS-CERT-HEADER

           PERFORM VARYING CA-IDX FROM 1 BY 1
                   UNTIL CA-IDX > WS-CA-COUNT
               MOVE WS-CURR-CIF            TO CTD-CIF-NO
               MOVE WS-CA-SORT-CODE (CA-IDX) TO CTD-SORT-CODE
               MOVE WS-CA-ACCT-NO (CA-IDX) TO CTD-ACCT-NO
               MOVE WS-CA-PROD-CD (CA-IDX) TO CTD-PROD-CD
               MOVE WS-CA-GROSS (CA-IDX)   TO CTD-GROSS
               MOVE WS-CA-TAX (CA-IDX)     TO CTD-TAX
               MOVE WS-CA-NET (CA-IDX)     TO CTD-NET
               MOVE WS-CA-TAX-STATUS (CA-IDX) TO CTD-TAX-STATUS
               WRITE CERTS-RECORD          FROM WS-CERT-DETAIL
           END-PERFORM

           MOVE WS-CURR-CIF                TO CTT-CIF-NO
           MOVE WS-CUST-GROSS              TO CTT-GROSS
           MOVE WS-CUST-TAX                TO CTT-TAX
           MOVE WS-CUST-NET                TO CTT-NET
           WRITE CERTS-RECORD              FROM WS-CERT-TOTAL
           IF WS-CERTS-STATUS NOT = '00'
               MOVE '2900-END-CUSTOMER'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CERTS WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-CERTS-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-CERTIFICATES
           .
       2900-EXIT.
           EXIT.
      *================================================================*
       3000-FINALISE.
      *================================================================*
           MOVE SPACES                     TO WS-BBSI-RECORD
           MOVE 'T'                        TO BB-REC-TYPE
           MOVE WS-BANK-REF                TO BB-BANK-REF
           MOVE WS-TAX-YEAR                TO BB-TAX-YEAR
           MOVE WS-TOTAL-GROSS             TO BB-GROSS
           MOVE WS-TOTAL-TAX               TO BB-TAX
           MOVE WS-BBSI-RECORDS            TO BB-ACCT-NO
           WRITE BBSI-RECORD               FROM WS-BBSI-RECORD

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'ACCOUNT ROWS READ'        TO TL-LABEL
           MOVE WS-ROWS-READ               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS'                TO TL-LABEL
           MOVE WS-CUSTOMERS               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CERTIFICATES'             TO TL-LABEL
           MOVE WS-CERTIFICATES            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'BBSI RECORDS / GROSS INTEREST' TO TL-LABEL
           MOVE WS-BBSI-RECORDS            TO TL-COUNT
           MOVE WS-TOTAL-GROSS             TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TAX DEDUCTED'             TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-TAX               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS NOT ON CIF'     TO TL-LABEL
           MOVE WS-NO-CIF                  TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PERSONAL CUSTOMERS WITH NO NINO' TO TL-LABEL
           MOVE WS-NO-NINO                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS OVER 50 ACCOUNTS' TO TL-LABEL
           MOVE WS-ACCT-OVERFLOW           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE CIFMAST-FILE
                 ACCTMAST-FILE
                 CERTS-FILE
                 BBSI-FILE
                 REPORT-FILE

           DISPLAY 'INTAX010 - CERTIFICATES ' WS-CERTIFICATES
           DISPLAY 'INTAX010 - BBSI RECORDS ' WS-BBSI-RECORDS
           IF WS-NO-CIF > ZERO OR WS-ACCT-OVERFLOW > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       7000-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           MOVE 2                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-EXCEPTION.
      *================================================================*
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-EXCEPTION-LINE
           ADD 1                           TO WS-LINE-COUNT
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
      *
           COPY ABNDPROC.
