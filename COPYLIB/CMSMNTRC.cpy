      *================================================================*
      * COPYBOOK : CMSMNTRC                                            *
      * TITLE    : CARD MANAGEMENT SYSTEM (CMS) MAINTENANCE INTERFACE  *
      *            - ISSUER INPUT FILE, INTERFACE SPEC CMS-IF-07 V2.3  *
      * DATASETS : MBK.PRD.CMS.MAINT.OUTPUT(+1)   (NORMAL PRIORITY)    *
      *            MBK.PRD.CMS.HOTCARD.OUTPUT(+1) (LOST/STOLEN/FRAUD - *
      *            ALSO UPDATES BASE24 NEGATIVE CARD FILE)             *
      *            RECFM=FB LRECL=150                                  *
      *----------------------------------------------------------------*
      * CMS STATUS CODES (CMS-NEW-STATUS)                              *
      *   00 ACTIVE   05 TEMPORARY BLOCK   41 LOST                     *
      *   43 STOLEN   59 SUSPECTED FRAUD   (ISO 8583 RESPONSE CODES)   *
      *================================================================*
       01  CMS-MAINT-RECORD.
           05  CMS-TXN-TYPE                PIC X(02).
               88  CMS-STATUS-CHANGE           VALUE '01'.
               88  CMS-LIMIT-CHANGE            VALUE '02'.
               88  CMS-REISSUE                 VALUE '03'.
               88  CMS-PIN-REMINDER            VALUE '04'.
               88  CMS-ACTIVATE                VALUE '05'.
               88  CMS-USAGE-FLAGS             VALUE '06'.
           05  CMS-ISSUER-ID               PIC X(06).
           05  CMS-ACCT-SORT-CODE          PIC X(06).
           05  CMS-ACCT-NO                 PIC X(08).
           05  CMS-PAN-FIRST6              PIC X(06).
           05  CMS-PAN-LAST4               PIC X(04).
           05  CMS-EXPIRY-YYMM             PIC X(04).
           05  CMS-NEW-STATUS              PIC X(02).
           05  CMS-ATM-LIMIT               PIC 9(07).
           05  CMS-POS-LIMIT               PIC 9(07).
           05  CMS-OVERSEAS-FLAG           PIC X(01).
           05  CMS-CONTACTLESS-FLAG        PIC X(01).
           05  CMS-REISSUE-REASON          PIC X(02).
           05  CMS-SOURCE-REF              PIC X(20).
           05  CMS-REQUEST-DATE            PIC 9(08).
           05  CMS-REQUEST-TIME            PIC 9(06).
           05  FILLER                      PIC X(60).
