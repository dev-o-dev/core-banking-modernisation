      *================================================================*
      * COPYBOOK : CUSTMNRC                                            *
      * TITLE    : CUSTMNT2 - CHANNEL CUSTOMER MAINTENANCE RECORD      *
      * DATASET  : MBK.PRD.IBANK.CUSTMNT2.INPUT(0)                     *
      *            RECFM=FB LRECL=400                                  *
      * SOURCE   : IBANK CHANNEL (NIGHTLY EXTRACT OF CUST_MASTER       *
      *            CHANGES) - RECEIVED VIA CONNECT:DIRECT 22:30        *
      *----------------------------------------------------------------*
      * MAINTENANCE TYPES (CMND-MAINT-TYPE)                            *
      *   A  ADDRESS        C  CONTACT DETAILS                         *
      *   E  EMPLOYMENT     M  MARKETING PREFERENCES                   *
      * ONE RECORD PER CHANGE. A CUSTOMER MAY HAVE SEVERAL RECORDS -   *
      * THE FILE IS SORTED BY CIF + CHANNEL TIMESTAMP BEFORE USE SO    *
      * THE LAST CHANGE WINS.                                          *
      *----------------------------------------------------------------*
      * 2005-01-10 AOS  ORIGINAL (CUSTMNT1 - ADDRESS ONLY)             *
      * 2007-02-26 JNB  CUSTMNT2 - CONTACT/EMPLOYMENT/MARKETING        *
      *================================================================*
       01  CMN-MAINT-RECORD.
           05  CMN-REC-TYPE                PIC X(01).
               88  CMN-HEADER-REC              VALUE 'H'.
               88  CMN-DETAIL-REC              VALUE 'D'.
               88  CMN-TRAILER-REC             VALUE 'T'.
           05  CMN-REC-DATA                PIC X(399).
      *
           05  CMN-HEADER REDEFINES CMN-REC-DATA.
               10  CMNH-FILE-ID            PIC X(08).
               10  CMNH-EXTRACT-DATE       PIC 9(08).
               10  CMNH-EXTRACT-TIME       PIC 9(06).
               10  CMNH-SOURCE-SYS         PIC X(08).
               10  FILLER                  PIC X(369).
      *
           05  CMN-DETAIL REDEFINES CMN-REC-DATA.
               10  CMND-CIF-NO             PIC X(12).
               10  CMND-MAINT-TYPE         PIC X(01).
                   88  CMND-ADDRESS-CHG        VALUE 'A'.
                   88  CMND-CONTACT-CHG        VALUE 'C'.
                   88  CMND-EMPLOYMENT-CHG     VALUE 'E'.
                   88  CMND-MARKETING-CHG      VALUE 'M'.
               10  CMND-CHANNEL-TS         PIC X(26).
               10  CMND-LOGON-ID           PIC X(20).
               10  CMND-MAINT-DATA         PIC X(260).
               10  CMND-ADDRESS-DATA REDEFINES CMND-MAINT-DATA.
                   15  CMND-ADDR-LINE1     PIC X(60).
                   15  CMND-ADDR-LINE2     PIC X(60).
                   15  CMND-TOWN           PIC X(40).
                   15  CMND-COUNTY         PIC X(40).
                   15  CMND-POST-CD        PIC X(10).
                   15  CMND-CTRY-CD        PIC X(02).
                   15  FILLER              PIC X(48).
               10  CMND-CONTACT-DATA REDEFINES CMND-MAINT-DATA.
                   15  CMND-HOME-TEL       PIC X(20).
                   15  CMND-MOBILE-TEL     PIC X(20).
                   15  CMND-EMAIL-ADDR     PIC X(80).
                   15  FILLER              PIC X(140).
               10  CMND-EMPLOYMENT-DATA REDEFINES CMND-MAINT-DATA.
                   15  CMND-EMPL-STATUS    PIC X(20).
                   15  CMND-OCCUPATION     PIC X(40).
                   15  CMND-ANN-INCOME     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  FILLER              PIC X(184).
               10  CMND-MARKETING-DATA REDEFINES CMND-MAINT-DATA.
                   15  CMND-MKTG-EMAIL-IND PIC X(01).
                   15  CMND-MKTG-POST-IND  PIC X(01).
                   15  FILLER              PIC X(258).
               10  FILLER                  PIC X(80).
      *
           05  CMN-TRAILER REDEFINES CMN-REC-DATA.
               10  CMNT-REC-COUNT          PIC 9(09).
               10  FILLER                  PIC X(390).
