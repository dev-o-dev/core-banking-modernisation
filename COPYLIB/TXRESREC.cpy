      *================================================================*
      * COPYBOOK : TXRESREC                                            *
      * TITLE    : TAX RESIDENCY SELF-CERTIFICATION (FATCA / CRS)      *
      * DATASET  : MBK.PRD.TAX.SELFCERT  (VSAM KSDS)                   *
      *            RECORDSIZE(300 300)  KEYS(12 0)  - KEY = CIF NUMBER *
      * CICS FILE: TAXCERT (BRANCH SCREEN CFTAX1 / IBANK UPDATE)       *
      * IDCAMS   : CNTL(TXDEFRES)                                      *
      *----------------------------------------------------------------*
      * ONE RECORD PER CUSTOMER WHO HAS GIVEN A SELF-CERTIFICATION OF  *
      * TAX RESIDENCE UNDER THE INTERNATIONAL TAX COMPLIANCE           *
      * REGULATIONS 2015 (SI 2015/878): US FATCA (IGA 2013) AND THE    *
      * OECD COMMON REPORTING STANDARD (FIRST UK RETURN 31 MAY 2017    *
      * FOR CALENDAR YEAR 2016).                                       *
      *                                                                *
      * UP TO 5 COUNTRIES OF TAX RESIDENCE, EACH WITH ITS TIN OR A     *
      * REASON FOR NO TIN (OECD CODES A/B/C). THE UK ('GB') IS HELD    *
      * LIKE ANY OTHER COUNTRY BUT IS NEVER REPORTABLE.                *
      *                                                                *
      * A US CITIZEN IS FATCA REPORTABLE WHATEVER THEIR RESIDENCE -    *
      * TXR-US-PERSON-IND = 'Y' (FORM W-9 HELD) WITH THE SSN IN        *
      * TXR-US-TIN.                                                    *
      *                                                                *
      * INDICIA FLAGS ARE SET BY THE ANNUAL INDICIA SEARCH (TAXCR010)  *
      * AND BY BRANCH STAFF. AN INDICIUM THAT CONTRADICTS THE SELF-    *
      * CERTIFICATION SETS TXR-CHANGE-CIRC-IND - A NEW SELF-CERT MUST  *
      * BE OBTAINED WITHIN 90 DAYS OR THE ACCOUNT IS REPORTED AS       *
      * UNDOCUMENTED TO EVERY INDICATED COUNTRY.                       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2015-11-23 HBK  ORIGINAL VERSION (CRS PROJECT - SELF-CERT AT   *
      *                 ACCOUNT OPENING FROM 1 JANUARY 2016)           *
      * 2016-06-06 HBK  US TIN / W-9 FIELDS (FATCA MOVED FROM THE      *
      *                 SPREADSHEET PROCESS)                           *
      * 2017-03-13 PDQ  CHASE COUNT AND LAST REPORTED YEAR             *
      *================================================================*
       01  TXR-SELF-CERT-RECORD.
           05  TXR-KEY.
               10  TXR-CIF-NO              PIC X(12).
           05  TXR-CERT-STATUS             PIC X(01).
               88  TXR-CERT-VALID              VALUE 'V'.
               88  TXR-CERT-PENDING            VALUE 'P'.
               88  TXR-CERT-WITHDRAWN          VALUE 'W'.
               88  TXR-CERT-REFUSED            VALUE 'R'.
           05  TXR-CERT-DATE               PIC 9(08).
           05  TXR-CERT-CHANNEL            PIC X(04).
               88  TXR-CHNL-BRANCH             VALUE 'BRCH'.
               88  TXR-CHNL-IBANK              VALUE 'IBNK'.
               88  TXR-CHNL-POST               VALUE 'POST'.
           05  TXR-CERT-FORM               PIC X(04).
               88  TXR-FORM-INDIVIDUAL         VALUE 'CRSI'.
               88  TXR-FORM-ENTITY             VALUE 'CRSE'.
               88  TXR-FORM-W9                 VALUE 'W9  '.
               88  TXR-FORM-W8BEN              VALUE 'W8BN'.
           05  TXR-US-PERSON-IND           PIC X(01).
               88  TXR-US-PERSON               VALUE 'Y'.
           05  TXR-US-TIN                  PIC X(09).
           05  TXR-BIRTH-CTRY              PIC X(02).
           05  TXR-BIRTH-TOWN              PIC X(30).
           05  TXR-RES-COUNT               PIC 9(01).
           05  TXR-RESIDENCY               OCCURS 5 TIMES.
               10  TXR-RES-CTRY            PIC X(02).
               10  TXR-RES-TIN             PIC X(20).
               10  TXR-NO-TIN-RSN          PIC X(01).
                   88  TXR-TIN-SUPPLIED        VALUE ' '.
                   88  TXR-TIN-NOT-ISSUED      VALUE 'A'.
                   88  TXR-TIN-UNOBTAINABLE    VALUE 'B'.
                   88  TXR-TIN-NOT-REQUIRED    VALUE 'C'.
               10  TXR-RES-FROM-DATE       PIC 9(08).
           05  TXR-INDICIA.
               10  TXR-IND-ADDRESS         PIC X(01).
               10  TXR-IND-TELEPHONE       PIC X(01).
               10  TXR-IND-STANDING-ORDER  PIC X(01).
               10  TXR-IND-POWER-ATTORNEY  PIC X(01).
               10  TXR-IND-US-BIRTHPLACE   PIC X(01).
               10  TXR-IND-HOLD-MAIL       PIC X(01).
           05  TXR-INDICIA-CHECK-DATE      PIC 9(08).
           05  TXR-CHANGE-CIRC-IND         PIC X(01).
               88  TXR-CHANGE-IN-CIRC          VALUE 'Y'.
           05  TXR-CHANGE-CIRC-DATE        PIC 9(08).
           05  TXR-CHASE-COUNT             PIC 9(02).
           05  TXR-LAST-CHASE-DATE         PIC 9(08).
           05  TXR-LAST-REPORTED-YEAR      PIC 9(04).
           05  TXR-LAST-MAINT-DATE         PIC 9(08).
           05  TXR-LAST-MAINT-USER         PIC X(08).
           05  FILLER                      PIC X(20).
