      *================================================================*
      * COPYBOOK : CFCUSREC                                            *
      * TITLE    : CUSTOMER INFORMATION FILE (CIF) MASTER RECORD       *
      * DATASET  : MBK.PRD.CIF.MASTER  (VSAM KSDS)                     *
      *            RECORDSIZE(700 700)  KEYS(12 0)                     *
      * CICS FILE: CIFMAST                                             *
      *----------------------------------------------------------------*
      * NOTE: THE INTERNET BANKING CHANNEL TABLE CUST_MASTER IS A      *
      *       SHADOW OF THIS RECORD. DO NOT CHANGE FIELD WIDTHS        *
      *       WITHOUT RAISING A CHANGE AGAINST IBANK (CUSTMNT2 AND     *
      *       IBFEED02 BOTH DEPEND ON THEM).                           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1989-03-14 RJH  ORIGINAL VERSION (REPLACES CUSTREC IMS SEGMENT)*
      * 1996-08-02 PDK  ADDED EMAIL + MOBILE (CR 96/0412)              *
      * 1998-11-20 MLW  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-05-09 SAT  KYC STATUS + RISK RATING (MLR 2003)            *
      * 2007-02-26 JNB  MARKETING INDICATORS FOR IBANK (CR 07/0088)    *
      * 2011-02-14 AOS  CIF-EXTENSION-2011 FROM FILLER - KYC REVIEW,   *
      *                 BEREAVEMENT, DUPLICATES. LENGTH UNCHANGED.     *
      *================================================================*
       01  CIF-CUSTOMER-RECORD.
           05  CIF-KEY.
               10  CIF-NO                  PIC X(12).
           05  CIF-REC-STATUS              PIC X(01).
               88  CIF-ACTIVE                  VALUE 'A'.
               88  CIF-BLOCKED                 VALUE 'B'.
               88  CIF-CLOSED                  VALUE 'C'.
               88  CIF-DECEASED                VALUE 'D'.
           05  CIF-CUST-TYPE               PIC X(01).
               88  CIF-PERSONAL                VALUE 'P'.
               88  CIF-BUSINESS                VALUE 'B'.
           05  CIF-NAME.
               10  CIF-TITLE               PIC X(06).
               10  CIF-FIRST-NM            PIC X(40).
               10  CIF-MIDDLE-NM           PIC X(40).
               10  CIF-LAST-NM             PIC X(40).
           05  CIF-DOB                     PIC 9(08).
           05  CIF-NAT-INS-NO              PIC X(13).
           05  CIF-ADDRESS.
               10  CIF-ADDR-LINE1          PIC X(60).
               10  CIF-ADDR-LINE2          PIC X(60).
               10  CIF-TOWN                PIC X(40).
               10  CIF-COUNTY              PIC X(40).
               10  CIF-POST-CD             PIC X(10).
               10  CIF-CTRY-CD             PIC X(02).
           05  CIF-CONTACT.
               10  CIF-HOME-TEL            PIC X(20).
               10  CIF-MOBILE-TEL          PIC X(20).
               10  CIF-EMAIL-ADDR          PIC X(80).
           05  CIF-EMPLOYMENT.
               10  CIF-EMPL-STATUS         PIC X(20).
               10  CIF-OCCUPATION          PIC X(40).
               10  CIF-ANN-INCOME          PIC S9(13)V99 COMP-3.
           05  CIF-MKTG-EMAIL-IND          PIC X(01).
           05  CIF-MKTG-POST-IND           PIC X(01).
           05  CIF-SEGMENT-CD              PIC X(10).
           05  CIF-KYC-STATUS              PIC X(01).
               88  CIF-KYC-VERIFIED            VALUE 'V'.
               88  CIF-KYC-PENDING             VALUE 'P'.
               88  CIF-KYC-FAILED              VALUE 'F'.
           05  CIF-RISK-RATING             PIC X(01).
               88  CIF-RISK-LOW                VALUE 'L'.
               88  CIF-RISK-MEDIUM             VALUE 'M'.
               88  CIF-RISK-HIGH               VALUE 'H'.
           05  CIF-HOME-BRANCH-SC          PIC X(06).
           05  CIF-CREATE-DATE             PIC 9(08).
           05  CIF-LAST-MAINT-DATE         PIC 9(08).
           05  CIF-LAST-MAINT-TIME         PIC 9(06).
           05  CIF-LAST-MAINT-USER         PIC X(08).
           05  CIF-LAST-MAINT-SRCE         PIC X(04).
               88  CIF-MAINT-BRANCH            VALUE 'BRCH'.
               88  CIF-MAINT-IBANK             VALUE 'IBNK'.
               88  CIF-MAINT-BATCH             VALUE 'BTCH'.
               88  CIF-MAINT-TELE              VALUE 'TELE'.
      *---- 2011 EXTENSION (WAS FILLER) -------------------------------*
           05  CIF-EXTENSION-2011.
      *        PERIODIC KYC REVIEW - CFKYC010
               10  CIF-KYC-LAST-REVIEW     PIC 9(08).
               10  CIF-KYC-NEXT-REVIEW     PIC 9(08).
               10  CIF-PEP-IND             PIC X(01).
                   88  CIF-IS-PEP              VALUE 'Y'.
      *        BEREAVEMENT - CFDEC010
               10  CIF-DATE-OF-DEATH       PIC 9(08).
               10  CIF-BEREAVE-REF         PIC X(12).
               10  CIF-BEREAVE-STAGE       PIC X(01).
                   88  CIF-BRV-NOTIFIED        VALUE 'N'.
                   88  CIF-BRV-ACCOUNTS-STOPPED VALUE 'S'.
                   88  CIF-BRV-PROBATE-RECEIVED VALUE 'P'.
                   88  CIF-BRV-SETTLED         VALUE 'X'.
      *        DUPLICATE DETECTION / MERGE - CFDUP010
               10  CIF-DUP-CHECK-DATE      PIC 9(08).
               10  CIF-MERGED-INTO         PIC X(12).
           05  FILLER                      PIC X(27).
