      *================================================================*
      * COPYBOOK : CRDMNTRC                                            *
      * TITLE    : CRDMNT1 - CHANNEL CARD MAINTENANCE RECORD           *
      * DATASET  : MBK.PRD.IBANK.CRDMNT1.INPUT(0)                      *
      *            RECFM=FB LRECL=200                                  *
      *----------------------------------------------------------------*
      * ACTION CODES (CRMD-ACTION-CD)                                  *
      *   ACT ACTIVATE          BLK FREEZE (TEMP BLOCK)                *
      *   UBK UNFREEZE          LST REPORT LOST                        *
      *   STL REPORT STOLEN     FRD REPORT SUSPECTED FRAUD             *
      *   RPL REPLACEMENT       LIM DAILY LIMIT CHANGE                 *
      *   PIN PIN REMINDER      OVS OVERSEAS USE SWITCH                *
      *   CTL CONTACTLESS SWITCH                                       *
      *----------------------------------------------------------------*
      * THE CHANNEL HOLDS THE MASKED PAN ONLY (PCI DSS). THE CARD IS   *
      * IDENTIFIED TO THE CARD MANAGEMENT SYSTEM BY ACCOUNT + FIRST 6  *
      * + LAST 4 + EXPIRY.                                             *
      *----------------------------------------------------------------*
      * 2006-09-18 AOS  ORIGINAL (CRDMNT1)                             *
      * 2008-11-03 JNB  CONTACTLESS SWITCH (ACTION CTL)                *
      *================================================================*
       01  CRM-MAINT-RECORD.
           05  CRM-REC-TYPE                PIC X(01).
               88  CRM-HEADER-REC              VALUE 'H'.
               88  CRM-DETAIL-REC              VALUE 'D'.
               88  CRM-TRAILER-REC             VALUE 'T'.
           05  CRM-REC-DATA                PIC X(199).
      *
           05  CRM-HEADER REDEFINES CRM-REC-DATA.
               10  CRMH-FILE-ID            PIC X(08).
               10  CRMH-EXTRACT-DATE       PIC 9(08).
               10  CRMH-EXTRACT-TIME       PIC 9(06).
               10  FILLER                  PIC X(177).
      *
           05  CRM-DETAIL REDEFINES CRM-REC-DATA.
               10  CRMD-CHNL-CARD-ID       PIC 9(12).
               10  CRMD-SORT-CODE          PIC X(06).
               10  CRMD-ACCT-NO            PIC X(08).
               10  CRMD-CARD-NO-MASKED     PIC X(19).
               10  CRMD-CARD-NO-PARTS REDEFINES CRMD-CARD-NO-MASKED.
                   15  CRMD-PAN-FIRST6     PIC X(06).
                   15  CRMD-PAN-MASK       PIC X(09).
                   15  CRMD-PAN-LAST4      PIC X(04).
               10  CRMD-EXPIRY-MMYY.
                   15  CRMD-EXPIRY-MM      PIC X(02).
                   15  CRMD-EXPIRY-YY      PIC X(02).
               10  CRMD-ACTION-CD          PIC X(03).
                   88  CRMD-ACTIVATE           VALUE 'ACT'.
                   88  CRMD-FREEZE             VALUE 'BLK'.
                   88  CRMD-UNFREEZE           VALUE 'UBK'.
                   88  CRMD-LOST               VALUE 'LST'.
                   88  CRMD-STOLEN             VALUE 'STL'.
                   88  CRMD-FRAUD              VALUE 'FRD'.
                   88  CRMD-REPLACE            VALUE 'RPL'.
                   88  CRMD-LIMITS             VALUE 'LIM'.
                   88  CRMD-PIN-REMINDER       VALUE 'PIN'.
                   88  CRMD-OVERSEAS           VALUE 'OVS'.
                   88  CRMD-CONTACTLESS        VALUE 'CTL'.
                   88  CRMD-HOT-CARD           VALUE 'LST' 'STL' 'FRD'.
               10  CRMD-DAILY-ATM-LIMIT    PIC 9(09)V99.
               10  CRMD-DAILY-POS-LIMIT    PIC 9(09)V99.
               10  CRMD-OVERSEAS-IND       PIC X(01).
               10  CRMD-CONTACTLESS-IND    PIC X(01).
               10  CRMD-CHANNEL-TS         PIC X(26).
               10  CRMD-LOGON-ID           PIC X(20).
               10  FILLER                  PIC X(77).
      *
           05  CRM-TRAILER REDEFINES CRM-REC-DATA.
               10  CRMT-REC-COUNT          PIC 9(09).
               10  FILLER                  PIC X(190).
