       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TAXCR010.
       AUTHOR.        H B KHAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2016-11-14.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : TAXCR010                                            *
      * TITLE    : FATCA / CRS - ANNUAL RETURN TO HMRC AND SELF-       *
      *            CERTIFICATION CHASE LIST                            *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : TAXCR010                                            *
      * JCL      : TAXCR01 (ANNUAL - JANUARY, RETURN DUE 31 MAY)       *
      *----------------------------------------------------------------*
      * INPUT    : PARMIN   - 'RPTYEAR=CCYY' AND 'USDRATE=N.NNNN'      *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      *            CIFMAST  - CIF MASTER (SEQUENTIAL - EVERY CUSTOMER) *
      *            ACCTMAST - DDA MASTER BY CIF (ALTERNATE KEY, DD     *
      *                       ACCTMAS1 = CIF PATH)                     *
      *            MBK.INTPAID - INTEREST PAID IN THE YEAR (CURSOR)    *
      *            MBK.TXNHIST - BALANCE AT 31 DECEMBER (SELECT)       *
      * UPDATE   : TAXCERT  - SELF-CERTIFICATIONS (TXRESREC, RANDOM)   *
      * OUTPUT   : CRSOUT   - HMRC CRS / FATCA RETURN FILE (FB 300)    *
      *            CHASE    - MISSING / DEFECTIVE SELF-CERT LIST FOR   *
      *                       CUSTOMER LETTERS (FB 200)                *
      *            REPORT   - CONTROL REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * INTERNATIONAL TAX COMPLIANCE REGULATIONS 2015: EVERY ACCOUNT   *
      * HELD AT 31 DECEMBER (OR CLOSED DURING THE YEAR) BY A REPORTABLE*
      * PERSON IS RETURNED WITH THE YEAR-END BALANCE (NIL IF CLOSED OR *
      * OVERDRAWN) AND THE GROSS INTEREST PAID IN THE CALENDAR YEAR.   *
      *                                                                *
      * A CUSTOMER IS REPORTABLE TO A COUNTRY WHEN:                    *
      *   1. SELF-CERTIFIED (TXR-CERT-VALID) AS TAX RESIDENT THERE     *
      *      (ANY COUNTRY EXCEPT GB), OR AS A US PERSON (FATCA).       *
      *   2. NOT SELF-CERTIFIED, BUT THE INDICIA SEARCH FINDS A        *
      *      FOREIGN ADDRESS (CIF-CTRY-CD) OR TELEPHONE NUMBER - THE   *
      *      ACCOUNT IS REPORTED AS UNDOCUMENTED TO THAT COUNTRY.      *
      *   3. SELF-CERTIFIED BUT CONTRADICTED BY AN INDICIUM FOR MORE   *
      *      THAN 90 DAYS (CHANGE OF CIRCUMSTANCES NOT CURED) - ALSO   *
      *      REPORTED UNDOCUMENTED TO THE INDICATED COUNTRY.           *
      * FATCA ONLY: PRE-EXISTING ACCOUNTS (OPENED BEFORE 1 JULY 2014)  *
      * WHOSE AGGREGATE BALANCE IS NOT OVER USD 50,000 ARE EXEMPT      *
      * (US-UK IGA ANNEX I) - USDRATE CONVERTS THE STERLING TOTAL.     *
      * LOANS AND MORTGAGES ARE NOT DEPOSITORY ACCOUNTS - NOT REPORTED.*
      *                                                                *
      * CHASE LIST (ONE ROW PER CUSTOMER AND REASON, LIVE CUSTOMERS):  *
      *   NOSC  NO SELF-CERT, FOREIGN INDICIA - REPORTED UNDOCUMENTED  *
      *   NEWA  ACCOUNT OPENED SINCE 1 JAN 2016 WITH NO SELF-CERT      *
      *         (MANDATORY AT OPENING - BRANCH PROCESS FAILURE)        *
      *   CONT  SELF-CERT CONTRADICTED BY INDICIA                      *
      *   NTIN  FOREIGN RESIDENCE WITH NO TIN AND NO REASON            *
      *   ENTY  BUSINESS CUSTOMER - ENTITY CLASSIFICATION MANUAL       *
      * A CUSTOMER WITH INDICIA OR A CHASE BUT NO SELF-CERT RECORD GETS*
      * A NEW TAXCERT RECORD (STATUS 'P') HOLDING INDICIA AND CHASES.  *
      *                                                                *
      * KNOWN ISSUES:                                                  *
      * PIR 2017/031 OPEN: ENTITIES ARE LISTED FOR MANUAL REVIEW ONLY. *
      * PIR 2017/058 OPEN: WHERE TXNHIST HAS NO ROW ON OR BEFORE 31 DEC*
      *   (HISTORY ARCHIVED BY PSARC010 FOR ACCOUNTS WITH NO MOVEMENT  *
      *   FOR SEVEN YEARS) THE CURRENT LEDGER BALANCE IS USED. IT IS   *
      *   RIGHT UNLESS THE ACCOUNT HAS MOVED SINCE 1 JANUARY.          *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2016-11-14 HBK  ORIGINAL VERSION (FIRST CRS RETURN, YEAR 2016) *
      * 2017-03-13 PDQ  CHASE COUNT ON TAXCERT, NEW TAXCERT RECORDS FOR*
      *                 UNDOCUMENTED CUSTOMERS (HMRC REVIEW FEB 2017)  *
      * 2017-11-20 HBK  FATCA USD 50,000 PRE-EXISTING THRESHOLD - 2016 *
      *                 RETURN OVER-REPORTED 1,140 US ACCOUNTS         *
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
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT TAXCERT-FILE     ASSIGN TO TAXCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS TXR-KEY
                                   FILE STATUS IS WS-TAXCERT-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS ACM-KEY
                                   ALTERNATE RECORD KEY IS ACM-CIF-NO
                                       WITH DUPLICATES
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CRSOUT-FILE      ASSIGN TO CRSOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CRSOUT-STATUS.
           SELECT CHASE-FILE       ASSIGN TO CHASE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CHASE-STATUS.
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
           05  PARM-KEYWORD                PIC X(08).
               88  PARM-RPT-YEAR               VALUE 'RPTYEAR='.
               88  PARM-USD-RATE               VALUE 'USDRATE='.
           05  PARM-VALUE                  PIC X(20).
           05  FILLER                      PIC X(52).
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  TAXCERT-FILE.
           COPY TXRESREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CRSOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CRSOUT-RECORD                   PIC X(300).
       FD  CHASE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHASE-RECORD                    PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'TAXCR010'.
      *
       01  WS-CONSTANTS.
           05  WS-GIIN                     PIC X(19)
                                           VALUE 'MB7K2Q.99999.SL.826'.
           05  WS-FATCA-THRESHOLD-USD      PIC S9(09)V99 COMP-3
                                           VALUE +50000.00.
           05  WS-FATCA-PREEXIST-DATE      PIC 9(08) VALUE 20140701.
           05  WS-CRS-NEW-ACCT-DATE        PIC 9(08) VALUE 20160101.
           05  WS-CURE-DAYS                PIC S9(05) COMP VALUE +90.
           05  WS-MAX-ACCOUNTS             PIC S9(04) COMP VALUE +50.
           05  WS-MAX-JUR                  PIC S9(04) COMP VALUE +6.
      *
      *---- TELEPHONE INDICIA - INTERNATIONAL DIALLING PREFIXES -------*
       01  WS-PHONE-VALUES.
           05  FILLER                      PIC X(07) VALUE '+1  2US'.
           05  FILLER                      PIC X(07) VALUE '+3534IE'.
           05  FILLER                      PIC X(07) VALUE '+33 3FR'.
           05  FILLER                      PIC X(07) VALUE '+49 3DE'.
           05  FILLER                      PIC X(07) VALUE '+34 3ES'.
           05  FILLER                      PIC X(07) VALUE '+61 3AU'.
       01  WS-PHONE-TABLE REDEFINES WS-PHONE-VALUES.
           05  WS-PT-ENTRY                 OCCURS 6 TIMES.
               10  WS-PT-PREFIX            PIC X(04).
               10  WS-PT-LEN               PIC 9(01).
               10  WS-PT-CTRY              PIC X(02).
      *
       01  WS-FILE-STATUSES.
           05  WS-PARMIN-STATUS            PIC X(02).
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-EOF              VALUE '10'.
           05  WS-TAXCERT-STATUS           PIC X(02).
               88  WS-TAXCERT-OK               VALUE '00'.
               88  WS-TAXCERT-NOTFND           VALUE '23'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00' '02'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-CRSOUT-STATUS            PIC X(02).
           05  WS-CHASE-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CIF               VALUE 'Y'.
           05  WS-CERT-IND                 PIC X(01).
               88  WS-CERT-FOUND               VALUE 'Y'.
               88  WS-CERT-NOT-FOUND           VALUE 'N'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-CERT-CHANGED-IND         PIC X(01).
               88  WS-CERT-CHANGED             VALUE 'Y'.
           05  WS-FOUND-IND                PIC X(01).
               88  WS-FOUND                    VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-RPT-YEAR                 PIC 9(04) VALUE ZERO.
           05  WS-YEAR-START               PIC 9(08).
           05  WS-YEAR-END                 PIC 9(08).
           05  WS-USD-RATE                 PIC S9(03)V9(04) COMP-3
                                           VALUE ZERO.
           05  WS-TOTAL-GBP                PIC S9(13)V99 COMP-3.
           05  WS-TOTAL-USD                PIC S9(13)V99 COMP-3.
           05  WS-INDICIA-CTRY             PIC X(02).
           05  WS-PHONE-CTRY               PIC X(02).
           05  WS-PHONE-NO                 PIC X(20).
           05  WS-NEW-ACCT-IND             PIC X(01).
               88  WS-HAS-NEW-ACCOUNT          VALUE 'Y'.
           05  WS-PREEXIST-ONLY-IND        PIC X(01).
               88  WS-PREEXIST-ONLY            VALUE 'Y'.
           05  WS-ADD-CTRY                 PIC X(02).
           05  WS-ADD-TIN                  PIC X(20).
           05  WS-ADD-NO-TIN-RSN           PIC X(01).
           05  WS-ADD-UNDOC                PIC X(01).
           05  WS-CHASE-CD                 PIC X(04).
           05  WS-CHASE-TEXT               PIC X(40).
           05  WS-I                        PIC S9(04) COMP.
           05  WS-J                        PIC S9(04) COMP.
           05  WS-K                        PIC S9(04) COMP.
           05  WS-DISP-AMOUNT              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
      *---- REPORTABLE JURISDICTIONS FOR THE CURRENT CUSTOMER ---------*
       01  WS-JUR-TABLE.
           05  WS-JUR-COUNT                PIC S9(04) COMP.
           05  WS-JUR-ENTRY                OCCURS 6 TIMES.
               10  WS-JUR-CTRY             PIC X(02).
               10  WS-JUR-REGIME           PIC X(04).
               10  WS-JUR-TIN              PIC X(20).
               10  WS-JUR-NO-TIN-RSN       PIC X(01).
               10  WS-JUR-UNDOC            PIC X(01).
      *
      *---- ACCOUNTS OF THE CURRENT CUSTOMER --------------------------*
       01  WS-ACCT-TABLE.
           05  WS-ACCT-COUNT               PIC S9(04) COMP.
           05  WS-ACCT-ENTRY               OCCURS 50 TIMES.
               10  WS-AT-SORT-CODE         PIC X(06).
               10  WS-AT-ACCT-NO           PIC X(08).
               10  WS-AT-OPEN-DATE         PIC 9(08).
               10  WS-AT-CLOSED-IND        PIC X(01).
               10  WS-AT-CCY-CD            PIC X(03).
               10  WS-AT-LEDGER-BAL        PIC S9(13)V99 COMP-3.
               10  WS-AT-YE-BAL            PIC S9(13)V99 COMP-3.
               10  WS-AT-INTEREST          PIC S9(13)V99 COMP-3.
      *
      *---- HMRC RETURN RECORD (FB 300) -------------------------------*
       01  WS-CRS-RECORD.
           05  CRS-REC-TYPE                PIC X(01).
           05  CRS-BODY                    PIC X(299).
           05  CRS-HDR REDEFINES CRS-BODY.
               10  CRSH-SENDER             PIC X(30).
               10  CRSH-GIIN               PIC X(19).
               10  CRSH-RPT-YEAR           PIC 9(04).
               10  CRSH-CREATE-DATE        PIC 9(08).
               10  CRSH-CREATING-PGM       PIC X(08).
               10  FILLER                  PIC X(230).
           05  CRS-ACCT REDEFINES CRS-BODY.
               10  CRSA-REGIME             PIC X(04).
               10  CRSA-JURISDICTION       PIC X(02).
               10  CRSA-CIF-NO             PIC X(12).
               10  CRSA-HOLDER-TYPE        PIC X(01).
               10  CRSA-TITLE              PIC X(06).
               10  CRSA-FIRST-NM           PIC X(40).
               10  CRSA-LAST-NM            PIC X(40).
               10  CRSA-DOB                PIC 9(08).
               10  CRSA-BIRTH-TOWN         PIC X(30).
               10  CRSA-BIRTH-CTRY         PIC X(02).
               10  CRSA-ADDR-LINE1         PIC X(40).
               10  CRSA-TOWN               PIC X(30).
               10  CRSA-POST-CD            PIC X(10).
               10  CRSA-ADDR-CTRY          PIC X(02).
               10  CRSA-TIN                PIC X(20).
               10  CRSA-NO-TIN-RSN         PIC X(01).
               10  CRSA-SORT-CODE          PIC X(06).
               10  CRSA-ACCT-NO            PIC X(08).
               10  CRSA-CLOSED-IND         PIC X(01).
               10  CRSA-UNDOC-IND          PIC X(01).
               10  CRSA-BALANCE            PIC 9(13)V99.
               10  CRSA-INTEREST           PIC 9(11)V99.
               10  CRSA-CCY-CD             PIC X(03).
               10  FILLER                  PIC X(04).
           05  CRS-TRL REDEFINES CRS-BODY.
               10  CRST-ACCOUNT-COUNT      PIC 9(09).
               10  CRST-CRS-COUNT          PIC 9(09).
               10  CRST-FATCA-COUNT        PIC 9(09).
               10  CRST-TOTAL-BAL          PIC 9(15)V99.
               10  CRST-TOTAL-INT          PIC 9(13)V99.
               10  FILLER                  PIC X(240).
      *
      *---- CHASE LIST RECORD (FB 200) --------------------------------*
       01  WS-CHASE-RECORD.
           05  CHS-CIF-NO                  PIC X(12).
           05  CHS-REASON-CD               PIC X(04).
           05  CHS-REASON-TEXT             PIC X(40).
           05  CHS-NAME                    PIC X(40).
           05  CHS-ADDR-LINE1              PIC X(40).
           05  CHS-TOWN                    PIC X(30).
           05  CHS-POST-CD                 PIC X(10).
           05  CHS-ADDR-CTRY               PIC X(02).
           05  CHS-INDICIA-CTRY            PIC X(02).
           05  CHS-CHASE-COUNT             PIC 9(02).
           05  CHS-HOME-BRANCH             PIC X(06).
           05  FILLER                      PIC X(12).
      *
       01  WS-COUNTERS.
           05  WS-CIF-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-CERTIFIED                PIC S9(09) COMP-3 VALUE 0.
           05  WS-REPORTABLE-CUST          PIC S9(09) COMP-3 VALUE 0.
           05  WS-UNDOC-CUST               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCOUNTS-RETURNED        PIC S9(09) COMP-3 VALUE 0.
           05  WS-CRS-RECORDS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-FATCA-RECORDS            PIC S9(09) COMP-3 VALUE 0.
           05  WS-FATCA-EXEMPT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-CHASE-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CERTS-CREATED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-ARCHIVED-BAL             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCT-OVERFLOW            PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-BAL                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-INT                PIC S9(13)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'TAXCR010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - FATCA / CRS RETURN TO HMRC'.
           05  FILLER                      PIC X(15)
               VALUE 'REPORTING YEAR'.
           05  HD1-RPT-YEAR                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(14) VALUE 'CIF NUMBER'.
           05  FILLER            PIC X(06) VALUE 'CODE'.
           05  FILLER            PIC X(42) VALUE 'REASON'.
           05  FILLER            PIC X(04) VALUE 'IND'.
           05  FILLER            PIC X(66) VALUE 'CUSTOMER'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CODE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-REASON                   PIC X(40).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-INDICIA                  PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NAME                     PIC X(40).
           05  FILLER                      PIC X(26) VALUE SPACES.
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
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLINTP END-EXEC.
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
       01  HV-CIF-NO                       PIC X(12).
       01  HV-YEAR-START                   PIC X(10).
       01  HV-YEAR-END                     PIC X(10).
       01  HV-GROSS                        PIC S9(13)V99 COMP-3.
       01  HV-BALANCE                      PIC S9(13)V99 COMP-3.
      *
      *    GROSS CREDIT INTEREST CAPITALISED IN THE CALENDAR YEAR, BY
      *    ACCOUNT, FOR ONE CUSTOMER.
           EXEC SQL DECLARE INTCSR CURSOR FOR
               SELECT SORT_CODE, ACCT_NO, SUM(GROSS_AMT)
                 FROM MBK.INTPAID
                WHERE CIF_NO    = :HV-CIF-NO
                  AND CR_DR_IND = 'C'
                  AND CAP_DATE BETWEEN :HV-YEAR-START
                                   AND :HV-YEAR-END
                GROUP BY SORT_CODE, ACCT_NO
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
           PERFORM 2000-PROCESS-CUSTOMER THRU 2000-EXIT
               UNTIL WS-END-OF-CIF
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PARMIN-FILE
                       DATECARD-FILE
                       CIFMAST-FILE
                       ACCTMAST-FILE
                I-O    TAXCERT-FILE
                OUTPUT CRSOUT-FILE
                       CHASE-FILE
                       REPORT-FILE
           IF WS-PARMIN-STATUS NOT = '00'
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-TAXCERT-OK
           OR WS-CRSOUT-STATUS NOT = '00'
           OR WS-CHASE-STATUS NOT = '00'
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
           PERFORM 1100-READ-PARMS

      *    THE YEAR MUST HAVE ENDED - BALANCES ARE AS AT 31 DECEMBER.
           IF WS-RPT-YEAR < 2016
           OR WS-RPT-YEAR >= WS-BUS-DATE (1:4)
           OR WS-USD-RATE NOT > ZERO
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'RPTYEAR (NOT ENDED / PRE-2016) OR USDRATE INVALID'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           COMPUTE WS-YEAR-START = WS-RPT-YEAR * 10000 + 0101
           COMPUTE WS-YEAR-END   = WS-RPT-YEAR * 10000 + 1231
           MOVE WS-RPT-YEAR                TO HV-YEAR-START (1:4)
                                              HV-YEAR-END (1:4)
           MOVE '-01-01'                   TO HV-YEAR-START (5:6)
           MOVE '-12-31'                   TO HV-YEAR-END (5:6)
           MOVE WS-RPT-YEAR                TO HD1-RPT-YEAR

      *---- RETURN HEADER ---------------------------------------------*
           MOVE SPACES                     TO WS-CRS-RECORD
           MOVE 'H'                        TO CRS-REC-TYPE
           MOVE 'MEGABANK PLC'             TO CRSH-SENDER
           MOVE WS-GIIN                    TO CRSH-GIIN
           MOVE WS-RPT-YEAR                TO CRSH-RPT-YEAR
           MOVE WS-BUS-DATE                TO CRSH-CREATE-DATE
           MOVE WS-PROGRAM-ID              TO CRSH-CREATING-PGM
           PERFORM 6000-WRITE-RETURN
           PERFORM 7000-REPORT-HEADINGS
           PERFORM 8000-READ-CIF
           .
      *================================================================*
       1100-READ-PARMS.
      *================================================================*
           READ PARMIN-FILE
           PERFORM UNTIL WS-PARMIN-STATUS NOT = '00'
               EVALUATE TRUE
                   WHEN PARM-RPT-YEAR
                       IF PARM-VALUE (1:4) NUMERIC
                           MOVE PARM-VALUE (1:4) TO WS-RPT-YEAR
                       END-IF
                   WHEN PARM-USD-RATE
                       COMPUTE WS-USD-RATE =
                               FUNCTION NUMVAL (PARM-VALUE)
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               READ PARMIN-FILE
           END-PERFORM
           CLOSE PARMIN-FILE
           DISPLAY 'TAXCR010 - REPORTING YEAR ' WS-RPT-YEAR
           .
      *================================================================*
       2000-PROCESS-CUSTOMER.
      *================================================================*
      *    MERGED CIFS ARE EMPTY SHELLS - THE SURVIVOR CARRIES THE
      *    ACCOUNTS.
           IF CIF-MERGED-INTO NOT = SPACES
           AND CIF-MERGED-INTO NOT = LOW-VALUES
               PERFORM 8000-READ-CIF
               GO TO 2000-EXIT
           END-IF
           INITIALIZE WS-JUR-TABLE WS-ACCT-TABLE
           MOVE SPACES                     TO WS-INDICIA-CTRY
           MOVE 'N'                        TO WS-CERT-CHANGED-IND
                                              WS-NEW-ACCT-IND

      *---- ENTITIES - MANUAL CLASSIFICATION (PIR 2017/031) -----------*
           IF CIF-BUSINESS
               IF NOT CIF-CLOSED
                   MOVE 'ENTY'             TO WS-CHASE-CD
                   MOVE 'ENTITY - CLASSIFY AND ADD CONTROLLING PERS'
                                           TO WS-CHASE-TEXT
                   PERFORM 5000-CHASE
               END-IF
               PERFORM 8000-READ-CIF
               GO TO 2000-EXIT
           END-IF

           PERFORM 2100-READ-SELF-CERT
           PERFORM 2200-INDICIA-SEARCH
           PERFORM 2500-LOAD-ACCOUNTS THRU 2500-EXIT

           IF WS-CERT-FOUND AND TXR-CERT-VALID
               ADD 1                       TO WS-CERTIFIED
               PERFORM 2300-CERTIFIED-CUSTOMER THRU 2300-EXIT
           ELSE
               PERFORM 2400-UNCERTIFIED-CUSTOMER
           END-IF

      *    NOTHING TO RETURN WITHOUT AN ACCOUNT HELD IN THE YEAR.
           IF WS-JUR-COUNT > ZERO AND WS-ACCT-COUNT > ZERO
               ADD 1                       TO WS-REPORTABLE-CUST
               PERFORM 2600-YEAR-END-FIGURES
               PERFORM 2700-RETURN-CUSTOMER
               MOVE WS-RPT-YEAR            TO TXR-LAST-REPORTED-YEAR
               SET WS-CERT-CHANGED         TO TRUE
           END-IF
           IF WS-CERT-CHANGED
               PERFORM 2800-SAVE-SELF-CERT
           END-IF
           PERFORM 8000-READ-CIF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-READ-SELF-CERT.
      *================================================================*
           MOVE CIF-NO                     TO TXR-CIF-NO
           READ TAXCERT-FILE
           EVALUATE TRUE
               WHEN WS-TAXCERT-OK
                   SET WS-CERT-FOUND       TO TRUE
               WHEN WS-TAXCERT-NOTFND
                   SET WS-CERT-NOT-FOUND   TO TRUE
                   INITIALIZE TXR-SELF-CERT-RECORD
                   MOVE CIF-NO             TO TXR-CIF-NO
               WHEN OTHER
                   MOVE '2100-READ-SELF-CERT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'TAXCERT READ FAILED' TO ABN-MESSAGE
                   MOVE WS-TAXCERT-STATUS  TO ABN-FILE-STATUS
                   MOVE CIF-NO             TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2200-INDICIA-SEARCH.
      *================================================================*
      *    ELECTRONIC RECORD SEARCH (CRS SECTION III B 2): RESIDENCE
      *    ADDRESS AND TELEPHONE NUMBER. THE ADDRESS WINS IF BOTH.
           MOVE SPACES                     TO WS-PHONE-CTRY
           IF CIF-CTRY-CD NOT = 'GB'
           AND CIF-CTRY-CD NOT = SPACES
               MOVE CIF-CTRY-CD            TO WS-INDICIA-CTRY
               MOVE 'Y'                    TO TXR-IND-ADDRESS
           END-IF
           MOVE CIF-MOBILE-TEL             TO WS-PHONE-NO
           PERFORM 2210-PHONE-PREFIX
           IF WS-PHONE-CTRY = SPACES
               MOVE CIF-HOME-TEL           TO WS-PHONE-NO
               PERFORM 2210-PHONE-PREFIX
           END-IF
           IF WS-PHONE-CTRY NOT = SPACES
               MOVE 'Y'                    TO TXR-IND-TELEPHONE
               IF WS-INDICIA-CTRY = SPACES
                   MOVE WS-PHONE-CTRY      TO WS-INDICIA-CTRY
               END-IF
           END-IF
      *    FATCA: A US PLACE OF BIRTH IS AN INDICIUM IN ITS OWN RIGHT
           IF TXR-BIRTH-CTRY = 'US'
               MOVE 'Y'                    TO TXR-IND-US-BIRTHPLACE
               IF WS-INDICIA-CTRY = SPACES
                   MOVE 'US'               TO WS-INDICIA-CTRY
               END-IF
           END-IF
           MOVE WS-BUS-DATE                TO TXR-INDICIA-CHECK-DATE
           .
      *================================================================*
       2210-PHONE-PREFIX.
      *================================================================*
      *    UK NUMBERS ('+44' OR NATIONAL FORMAT) ARE NOT INDICIA.
           IF WS-PHONE-NO (1:1) = '+'
           AND WS-PHONE-NO (1:3) NOT = '+44'
               PERFORM VARYING WS-K FROM 1 BY 1
                       UNTIL WS-K > 6 OR WS-PHONE-CTRY NOT = SPACES
                   IF WS-PHONE-NO (1:WS-PT-LEN (WS-K))
                      = WS-PT-PREFIX (WS-K) (1:WS-PT-LEN (WS-K))
                       MOVE WS-PT-CTRY (WS-K) TO WS-PHONE-CTRY
                   END-IF
               END-PERFORM
           END-IF
           .
      *================================================================*
       2300-CERTIFIED-CUSTOMER.
      *================================================================*
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > TXR-RES-COUNT OR WS-I > 5
               IF TXR-RES-CTRY (WS-I) NOT = 'GB'
               AND TXR-RES-CTRY (WS-I) NOT = SPACES
                   MOVE TXR-RES-CTRY (WS-I)   TO WS-ADD-CTRY
                   MOVE TXR-RES-TIN (WS-I)    TO WS-ADD-TIN
                   MOVE TXR-NO-TIN-RSN (WS-I) TO WS-ADD-NO-TIN-RSN
                   MOVE 'N'                   TO WS-ADD-UNDOC
                   PERFORM 2450-ADD-JURISDICTION
                   IF TXR-RES-TIN (WS-I) = SPACES
                   AND TXR-TIN-SUPPLIED (WS-I)
                       MOVE 'NTIN'         TO WS-CHASE-CD
                       MOVE 'FOREIGN TAX RESIDENCE WITHOUT A TIN'
                                           TO WS-CHASE-TEXT
                       PERFORM 5000-CHASE
                   END-IF
               END-IF
           END-PERFORM
           IF TXR-US-PERSON
               MOVE 'US'                   TO WS-ADD-CTRY
               MOVE TXR-US-TIN             TO WS-ADD-TIN
               MOVE SPACE                  TO WS-ADD-NO-TIN-RSN
               MOVE 'N'                    TO WS-ADD-UNDOC
               PERFORM 2450-ADD-JURISDICTION
           END-IF

      *---- REASONABLENESS - DOES AN INDICIUM CONTRADICT THE CERT? ----*
           IF WS-INDICIA-CTRY = SPACES
               GO TO 2300-EXIT
           END-IF
           MOVE 'N'                        TO WS-FOUND-IND
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > TXR-RES-COUNT OR WS-I > 5
               IF TXR-RES-CTRY (WS-I) = WS-INDICIA-CTRY
                   SET WS-FOUND            TO TRUE
               END-IF
           END-PERFORM
           IF WS-INDICIA-CTRY = 'US' AND TXR-US-PERSON
               SET WS-FOUND                TO TRUE
           END-IF
           IF WS-FOUND
               GO TO 2300-EXIT
           END-IF
           IF NOT TXR-CHANGE-IN-CIRC
               SET TXR-CHANGE-IN-CIRC      TO TRUE
               MOVE WS-BUS-DATE            TO TXR-CHANGE-CIRC-DATE
               SET WS-CERT-CHANGED         TO TRUE
           END-IF
           IF NOT CIF-CLOSED AND NOT CIF-DECEASED
               MOVE 'CONT'                 TO WS-CHASE-CD
               MOVE 'SELF-CERT CONTRADICTED BY INDICIA'
                                           TO WS-CHASE-TEXT
               PERFORM 5000-CHASE
           END-IF
      *    NOT CURED WITHIN 90 DAYS - REPORT UNDOCUMENTED AS WELL.
           IF WS-BUS-DATE-INT - FUNCTION INTEGER-OF-DATE
                                (TXR-CHANGE-CIRC-DATE) > WS-CURE-DAYS
               MOVE WS-INDICIA-CTRY        TO WS-ADD-CTRY
               MOVE SPACES                 TO WS-ADD-TIN
               MOVE 'B'                    TO WS-ADD-NO-TIN-RSN
               MOVE 'Y'                    TO WS-ADD-UNDOC
               PERFORM 2450-ADD-JURISDICTION
           END-IF
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2400-UNCERTIFIED-CUSTOMER.
      *================================================================*
           IF WS-INDICIA-CTRY NOT = SPACES
               MOVE WS-INDICIA-CTRY        TO WS-ADD-CTRY
               MOVE SPACES                 TO WS-ADD-TIN
               MOVE 'B'                    TO WS-ADD-NO-TIN-RSN
               MOVE 'Y'                    TO WS-ADD-UNDOC
               PERFORM 2450-ADD-JURISDICTION
               ADD 1                       TO WS-UNDOC-CUST
               SET WS-CERT-CHANGED         TO TRUE
               IF NOT CIF-CLOSED AND NOT CIF-DECEASED
                   MOVE 'NOSC'             TO WS-CHASE-CD
                   MOVE 'NO SELF-CERT, FOREIGN INDICIA - UNDOCUMENTED'
                                           TO WS-CHASE-TEXT
                   PERFORM 5000-CHASE
               END-IF
           END-IF
           IF WS-HAS-NEW-ACCOUNT
           AND NOT CIF-CLOSED AND NOT CIF-DECEASED
               MOVE 'NEWA'                 TO WS-CHASE-CD
               MOVE 'ACCOUNT OPENED SINCE 2016 WITHOUT SELF-CERT'
                                           TO WS-CHASE-TEXT
               PERFORM 5000-CHASE
           END-IF
           .
      *================================================================*
       2450-ADD-JURISDICTION.
      *================================================================*
           MOVE 'N'                        TO WS-FOUND-IND
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-JUR-COUNT
               IF WS-JUR-CTRY (WS-J) = WS-ADD-CTRY
                   SET WS-FOUND            TO TRUE
               END-IF
           END-PERFORM
           IF NOT WS-FOUND AND WS-JUR-COUNT < WS-MAX-JUR
               ADD 1                       TO WS-JUR-COUNT
               MOVE WS-JUR-COUNT           TO WS-J
               MOVE WS-ADD-CTRY            TO WS-JUR-CTRY (WS-J)
               IF WS-ADD-CTRY = 'US'
                   MOVE 'FATC'             TO WS-JUR-REGIME (WS-J)
               ELSE
                   MOVE 'CRS '             TO WS-JUR-REGIME (WS-J)
               END-IF
               MOVE WS-ADD-TIN             TO WS-JUR-TIN (WS-J)
               MOVE WS-ADD-NO-TIN-RSN      TO WS-JUR-NO-TIN-RSN (WS-J)
               MOVE WS-ADD-UNDOC           TO WS-JUR-UNDOC (WS-J)
           END-IF
           .
      *================================================================*
       2500-LOAD-ACCOUNTS.
      *================================================================*
      *    DEPOSIT ACCOUNTS HELD AT ANY TIME IN THE REPORTING YEAR, IN
      *    CIF ORDER VIA THE ALTERNATE KEY.
           MOVE CIF-NO                     TO ACM-CIF-NO
           START ACCTMAST-FILE KEY = ACM-CIF-NO
           IF NOT WS-ACCTMAST-OK
               GO TO 2500-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ ACCTMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN WS-ACCTMAST-EOF
                       SET WS-END-BROWSE   TO TRUE
                   WHEN NOT WS-ACCTMAST-OK
                       MOVE '2500-LOAD-ACCOUNTS' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTMAST BROWSE FAILED' TO ABN-MESSAGE
                       MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                       MOVE CIF-NO         TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   WHEN ACM-CIF-NO NOT = CIF-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN OTHER
                       PERFORM 2510-KEEP-ACCOUNT THRU 2510-EXIT
               END-EVALUATE
           END-PERFORM
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       2510-KEEP-ACCOUNT.
      *================================================================*
           IF ACM-LOAN OR ACM-MORTGAGE
               GO TO 2510-EXIT
           END-IF
           IF ACM-OPEN-DATE >= WS-CRS-NEW-ACCT-DATE
               SET WS-HAS-NEW-ACCOUNT      TO TRUE
           END-IF
           IF ACM-OPEN-DATE > WS-YEAR-END
               GO TO 2510-EXIT
           END-IF
           IF ACM-CLOSED AND ACM-CLOSE-DATE < WS-YEAR-START
               GO TO 2510-EXIT
           END-IF
           IF WS-ACCT-COUNT >= WS-MAX-ACCOUNTS
               ADD 1                       TO WS-ACCT-OVERFLOW
               GO TO 2510-EXIT
           END-IF
           ADD 1                           TO WS-ACCT-COUNT
           MOVE WS-ACCT-COUNT              TO WS-I
           MOVE ACM-SORT-CODE              TO WS-AT-SORT-CODE (WS-I)
           MOVE ACM-ACCT-NO                TO WS-AT-ACCT-NO (WS-I)
           MOVE ACM-OPEN-DATE              TO WS-AT-OPEN-DATE (WS-I)
           MOVE ACM-CCY-CD                 TO WS-AT-CCY-CD (WS-I)
           MOVE ACM-LEDGER-BAL             TO WS-AT-LEDGER-BAL (WS-I)
           MOVE ZERO                       TO WS-AT-YE-BAL (WS-I)
                                              WS-AT-INTEREST (WS-I)
      *    CLOSED IN THE YEAR, OR SINCE 31 DEC (STILL HELD AT YEAR END)
           IF ACM-CLOSED AND ACM-CLOSE-DATE <= WS-YEAR-END
               MOVE 'Y'                    TO WS-AT-CLOSED-IND (WS-I)
           ELSE
               MOVE 'N'                    TO WS-AT-CLOSED-IND (WS-I)
           END-IF
           .
       2510-EXIT.
           EXIT.
      *================================================================*
       2600-YEAR-END-FIGURES.
      *================================================================*
           MOVE ZERO                       TO WS-TOTAL-GBP
           MOVE 'Y'                        TO WS-PREEXIST-ONLY-IND
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
               IF WS-AT-CLOSED-IND (WS-I) = 'N'
                   PERFORM 2610-BALANCE-AT-YEAR-END
               END-IF
      *        CRS: A NEGATIVE BALANCE IS REPORTED AS NIL
               IF WS-AT-YE-BAL (WS-I) < ZERO
                   MOVE ZERO               TO WS-AT-YE-BAL (WS-I)
               END-IF
               ADD WS-AT-YE-BAL (WS-I)     TO WS-TOTAL-GBP
               IF WS-AT-OPEN-DATE (WS-I) >= WS-FATCA-PREEXIST-DATE
                   MOVE 'N'                TO WS-PREEXIST-ONLY-IND
               END-IF
           END-PERFORM
           PERFORM 2620-INTEREST-PAID
           .
      *================================================================*
       2610-BALANCE-AT-YEAR-END.
      *================================================================*
           MOVE WS-AT-SORT-CODE (WS-I)     TO TXH-SORT-CODE
           MOVE WS-AT-ACCT-NO (WS-I)       TO TXH-ACCT-NO
           EXEC SQL
               SELECT RUNNING_BAL
                 INTO :HV-BALANCE
                 FROM MBK.TXNHIST
                WHERE SORT_CODE = :TXH-SORT-CODE
                  AND ACCT_NO   = :TXH-ACCT-NO
                  AND POST_DATE <= :HV-YEAR-END
                ORDER BY POST_DATE DESC, POST_SEQ DESC
                FETCH FIRST 1 ROW ONLY
                WITH UR
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   MOVE HV-BALANCE         TO WS-AT-YE-BAL (WS-I)
               WHEN +100
      *            PIR 2017/058 - HISTORY ARCHIVED: CURRENT LEDGER
                   MOVE WS-AT-LEDGER-BAL (WS-I) TO WS-AT-YE-BAL (WS-I)
                   ADD 1                   TO WS-ARCHIVED-BAL
               WHEN OTHER
                   MOVE '2610-BALANCE-AT-YEAR-END' TO ABN-PARAGRAPH
                   MOVE TXH-ACCT-NO        TO ABN-KEY-DATA
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       2620-INTEREST-PAID.
      *================================================================*
           MOVE CIF-NO                     TO HV-CIF-NO
           EXEC SQL OPEN INTCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2620-INTEREST-PAID'   TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH INTCSR
                    INTO :INP-SORT-CODE, :INP-ACCT-NO, :HV-GROSS
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM VARYING WS-J FROM 1 BY 1
                               UNTIL WS-J > WS-ACCT-COUNT
                           IF WS-AT-SORT-CODE (WS-J) = INP-SORT-CODE
                           AND WS-AT-ACCT-NO (WS-J) = INP-ACCT-NO
                               ADD HV-GROSS TO WS-AT-INTEREST (WS-J)
                           END-IF
                       END-PERFORM
                   WHEN +100
                       CONTINUE
                   WHEN OTHER
                       MOVE '2620-INTEREST-PAID' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE INTCSR END-EXEC
           .
      *================================================================*
       2700-RETURN-CUSTOMER.
      *================================================================*
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-JUR-COUNT
      *        FATCA DE MINIMIS FOR PRE-EXISTING ACCOUNTS ONLY
               IF WS-JUR-REGIME (WS-J) = 'FATC'
               AND WS-PREEXIST-ONLY
               AND WS-JUR-UNDOC (WS-J) = 'N'
                   COMPUTE WS-TOTAL-USD ROUNDED =
                           WS-TOTAL-GBP * WS-USD-RATE
                   IF WS-TOTAL-USD NOT > WS-FATCA-THRESHOLD-USD
                       ADD 1               TO WS-FATCA-EXEMPT
                   ELSE
                       PERFORM 2710-RETURN-ACCOUNTS
                   END-IF
               ELSE
                   PERFORM 2710-RETURN-ACCOUNTS
               END-IF
           END-PERFORM
           .
      *================================================================*
       2710-RETURN-ACCOUNTS.
      *================================================================*
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
               MOVE SPACES                 TO WS-CRS-RECORD
               MOVE 'A'                    TO CRS-REC-TYPE
               MOVE WS-JUR-REGIME (WS-J)   TO CRSA-REGIME
               MOVE WS-JUR-CTRY (WS-J)     TO CRSA-JURISDICTION
               MOVE CIF-NO                 TO CRSA-CIF-NO
               MOVE 'I'                    TO CRSA-HOLDER-TYPE
               MOVE CIF-TITLE              TO CRSA-TITLE
               MOVE CIF-FIRST-NM           TO CRSA-FIRST-NM
               MOVE CIF-LAST-NM            TO CRSA-LAST-NM
               MOVE CIF-DOB                TO CRSA-DOB
               MOVE TXR-BIRTH-TOWN         TO CRSA-BIRTH-TOWN
               MOVE TXR-BIRTH-CTRY         TO CRSA-BIRTH-CTRY
               MOVE CIF-ADDR-LINE1         TO CRSA-ADDR-LINE1
               MOVE CIF-TOWN               TO CRSA-TOWN
               MOVE CIF-POST-CD            TO CRSA-POST-CD
               MOVE CIF-CTRY-CD            TO CRSA-ADDR-CTRY
               MOVE WS-JUR-TIN (WS-J)      TO CRSA-TIN
               MOVE WS-JUR-NO-TIN-RSN (WS-J) TO CRSA-NO-TIN-RSN
               MOVE WS-AT-SORT-CODE (WS-I) TO CRSA-SORT-CODE
               MOVE WS-AT-ACCT-NO (WS-I)   TO CRSA-ACCT-NO
               MOVE WS-AT-CLOSED-IND (WS-I) TO CRSA-CLOSED-IND
               MOVE WS-JUR-UNDOC (WS-J)    TO CRSA-UNDOC-IND
               MOVE WS-AT-YE-BAL (WS-I)    TO CRSA-BALANCE
               MOVE WS-AT-INTEREST (WS-I)  TO CRSA-INTEREST
               MOVE WS-AT-CCY-CD (WS-I)    TO CRSA-CCY-CD
               PERFORM 6000-WRITE-RETURN
               ADD 1                       TO WS-ACCOUNTS-RETURNED
               IF WS-JUR-REGIME (WS-J) = 'FATC'
                   ADD 1                   TO WS-FATCA-RECORDS
               ELSE
                   ADD 1                   TO WS-CRS-RECORDS
               END-IF
               ADD WS-AT-YE-BAL (WS-I)     TO WS-TOTAL-BAL
               ADD WS-AT-INTEREST (WS-I)   TO WS-TOTAL-INT
           END-PERFORM
           .
      *================================================================*
       2800-SAVE-SELF-CERT.
      *================================================================*
           MOVE WS-BUS-DATE                TO TXR-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO TXR-LAST-MAINT-USER
           IF WS-CERT-FOUND
               REWRITE TXR-SELF-CERT-RECORD
           ELSE
      *        NEW 'P' RECORD - INDICIA ON FILE, AWAITING THE CUSTOMER
               SET TXR-CERT-PENDING        TO TRUE
               MOVE ZERO                   TO TXR-RES-COUNT
               WRITE TXR-SELF-CERT-RECORD
               ADD 1                       TO WS-CERTS-CREATED
           END-IF
           IF NOT WS-TAXCERT-OK
               MOVE '2800-SAVE-SELF-CERT'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'TAXCERT WRITE / REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-TAXCERT-STATUS      TO ABN-FILE-STATUS
               MOVE TXR-CIF-NO             TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       5000-CHASE.
      *================================================================*
           MOVE SPACES                     TO WS-CHASE-RECORD
           MOVE CIF-NO                     TO CHS-CIF-NO
           MOVE WS-CHASE-CD                TO CHS-REASON-CD
           MOVE WS-CHASE-TEXT              TO CHS-REASON-TEXT
           STRING CIF-TITLE DELIMITED BY SPACE
                  ' '       DELIMITED BY SIZE
                  CIF-FIRST-NM DELIMITED BY SPACE
                  ' '       DELIMITED BY SIZE
                  CIF-LAST-NM DELIMITED BY SIZE
                  INTO CHS-NAME
           END-STRING
           MOVE CIF-ADDR-LINE1             TO CHS-ADDR-LINE1
           MOVE CIF-TOWN                   TO CHS-TOWN
           MOVE CIF-POST-CD                TO CHS-POST-CD
           MOVE CIF-CTRY-CD                TO CHS-ADDR-CTRY
           MOVE WS-INDICIA-CTRY            TO CHS-INDICIA-CTRY
           MOVE CIF-HOME-BRANCH-SC         TO CHS-HOME-BRANCH
      *    ONE CHASE PER CUSTOMER PER RUN IS COUNTED ON TAXCERT
           IF TXR-LAST-CHASE-DATE NOT = WS-BUS-DATE
           AND WS-CHASE-CD NOT = 'ENTY'
               IF TXR-CHASE-COUNT < 99
                   ADD 1                   TO TXR-CHASE-COUNT
               END-IF
               MOVE WS-BUS-DATE            TO TXR-LAST-CHASE-DATE
               SET WS-CERT-CHANGED         TO TRUE
           END-IF
           IF WS-CHASE-CD = 'ENTY'
               MOVE ZERO                   TO CHS-CHASE-COUNT
           ELSE
               MOVE TXR-CHASE-COUNT        TO CHS-CHASE-COUNT
           END-IF
           WRITE CHASE-RECORD              FROM WS-CHASE-RECORD
           IF WS-CHASE-STATUS NOT = '00'
               MOVE '5000-CHASE'           TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHASE WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-CHASE-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-CHASE-COUNT
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE CIF-NO                     TO DL-CIF-NO
           MOVE WS-CHASE-CD                TO DL-CODE
           MOVE WS-CHASE-TEXT              TO DL-REASON
           MOVE WS-INDICIA-CTRY            TO DL-INDICIA
           MOVE CHS-NAME                   TO DL-NAME
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       6000-WRITE-RETURN.
      *================================================================*
           WRITE CRSOUT-RECORD             FROM WS-CRS-RECORD
           IF WS-CRSOUT-STATUS NOT = '00'
               MOVE '6000-WRITE-RETURN'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CRSOUT WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-CRSOUT-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC
      *---- RETURN TRAILER --------------------------------------------*
           MOVE SPACES                     TO WS-CRS-RECORD
           MOVE 'T'                        TO CRS-REC-TYPE
           MOVE WS-ACCOUNTS-RETURNED       TO CRST-ACCOUNT-COUNT
           MOVE WS-CRS-RECORDS             TO CRST-CRS-COUNT
           MOVE WS-FATCA-RECORDS           TO CRST-FATCA-COUNT
           MOVE WS-TOTAL-BAL               TO CRST-TOTAL-BAL
           MOVE WS-TOTAL-INT               TO CRST-TOTAL-INT
           PERFORM 6000-WRITE-RETURN

      *---- CONTROL TOTALS --------------------------------------------*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'CUSTOMERS READ'           TO TL-LABEL
           MOVE WS-CIF-READ                TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  WITH VALID SELF-CERT'   TO TL-LABEL
           MOVE WS-CERTIFIED               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  REPORTABLE'             TO TL-LABEL
           MOVE WS-REPORTABLE-CUST         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  UNDOCUMENTED (INDICIA ONLY)' TO TL-LABEL
           MOVE WS-UNDOC-CUST              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNT RECORDS RETURNED - BALANCE' TO TL-LABEL
           MOVE WS-ACCOUNTS-RETURNED       TO TL-COUNT
           MOVE WS-TOTAL-BAL               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  INTEREST PAID IN YEAR'  TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-INT               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH FATCA'         TO TL-LABEL
           MOVE WS-FATCA-RECORDS           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'FATCA PRE-EXISTING UNDER USD 50,000' TO TL-LABEL
           MOVE WS-FATCA-EXEMPT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'YEAR-END BALANCE FROM LEDGER (ARCHIVED)' TO TL-LABEL
           MOVE WS-ARCHIVED-BAL            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CHASE LIST ENTRIES'       TO TL-LABEL
           MOVE WS-CHASE-COUNT             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE CIFMAST-FILE TAXCERT-FILE ACCTMAST-FILE CRSOUT-FILE
                 CHASE-FILE REPORT-FILE
           DISPLAY 'TAXCR010 - ACCOUNTS RETURNED ' WS-ACCOUNTS-RETURNED
           DISPLAY 'TAXCR010 - CHASE ENTRIES     ' WS-CHASE-COUNT
           DISPLAY 'TAXCR010 - NEW TAXCERT (P)   ' WS-CERTS-CREATED
           DISPLAY 'TAXCR010 - OVER 50 ACCOUNTS  ' WS-ACCT-OVERFLOW
           IF WS-CHASE-COUNT > ZERO
           OR WS-ACCT-OVERFLOW > ZERO
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
       8000-READ-CIF.
      *================================================================*
           READ CIFMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   ADD 1                   TO WS-CIF-READ
               WHEN WS-CIFMAST-EOF
                   SET WS-END-OF-CIF       TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-CIF'    TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
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
