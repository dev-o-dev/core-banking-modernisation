      *================================================================*
      * COPYBOOK : CGNTFREC                                            *
      * TITLE    : CHARGES PRE-NOTIFICATION LETTER EXTRACT             *
      * DATASET  : MBK.PRD.CHG.NOTIFY(+1)  RECFM=FB LRECL=300          *
      *            SENT TO DOCUMENT SERVICES (DOC1 TEMPLATE CHGNTF01)  *
      *            BY CONNECT:DIRECT PROCESS MBKCOPY                   *
      *----------------------------------------------------------------*
      * ONE 'A' (ADDRESS) RECORD PER ACCOUNT, FOLLOWED BY ONE 'C'      *
      * (CHARGE) RECORD PER CHARGE ITEM AND ONE 'T' (TOTAL) RECORD.    *
      * A FILE 'Z' TRAILER CARRIES THE ACCOUNT COUNT AND HASH TOTAL.   *
      *                                                                *
      * CNT-DELIVERY = 'E' MEANS THE CUSTOMER HAS OPTED FOR PAPERLESS  *
      * AND IBANK WILL SHOW THE NOTICE IN THE SECURE MESSAGE INBOX;    *
      * DOCUMENT SERVICES ONLY ARCHIVES (DOES NOT PRINT) THOSE.        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2009-10-01 AOS  ORIGINAL VERSION (PSR 2009)                    *
      *================================================================*
       01  CNT-NOTIFY-RECORD.
           05  CNT-REC-TYPE                PIC X(01).
               88  CNT-ADDRESS-REC             VALUE 'A'.
               88  CNT-CHARGE-REC              VALUE 'C'.
               88  CNT-TOTAL-REC               VALUE 'T'.
               88  CNT-FILE-TRAILER            VALUE 'Z'.
           05  CNT-SORT-CODE               PIC X(06).
           05  CNT-ACCT-NO                 PIC X(08).
           05  CNT-BODY                    PIC X(285).
      *
           05  CNT-ADDRESS REDEFINES CNT-BODY.
               10  CNTA-CIF-NO             PIC X(12).
               10  CNTA-DELIVERY           PIC X(01).
                   88  CNTA-PAPER              VALUE 'P'.
                   88  CNTA-ELECTRONIC         VALUE 'E'.
               10  CNTA-SALUTATION         PIC X(50).
               10  CNTA-ADDR-LINE1         PIC X(40).
               10  CNTA-ADDR-LINE2         PIC X(40).
               10  CNTA-ADDR-LINE3         PIC X(40).
               10  CNTA-ADDR-LINE4         PIC X(40).
               10  CNTA-POST-CD            PIC X(10).
               10  CNTA-PROD-NAME          PIC X(18).
               10  CNTA-PERIOD-START       PIC 9(08).
               10  CNTA-PERIOD-END         PIC 9(08).
               10  CNTA-APPLY-DATE         PIC 9(08).
               10  FILLER                  PIC X(10).
      *
           05  CNT-CHARGE REDEFINES CNT-BODY.
               10  CNTC-CHARGE-TYPE        PIC X(04).
               10  CNTC-DESCRIPTION        PIC X(35).
               10  CNTC-ITEM-COUNT         PIC 9(05).
               10  CNTC-UNIT-AMT           PIC 9(07)V99.
               10  CNTC-GROSS-AMT          PIC 9(11)V99.
               10  CNTC-CAP-REDUCTION      PIC 9(11)V99.
               10  CNTC-NET-AMT            PIC 9(11)V99.
               10  FILLER                  PIC X(193).
      *
           05  CNT-TOTAL REDEFINES CNT-BODY.
               10  CNTT-ITEM-COUNT         PIC 9(03).
               10  CNTT-TOTAL-NET          PIC 9(11)V99.
               10  CNTT-DEBIT-INT-INFO     PIC 9(11)V99.
               10  FILLER                  PIC X(256).
      *
           05  CNT-TRAILER REDEFINES CNT-BODY.
               10  CNTZ-ACCOUNT-COUNT      PIC 9(09).
               10  CNTZ-RECORD-COUNT       PIC 9(09).
               10  CNTZ-HASH-TOTAL         PIC 9(15)V99.
               10  FILLER                  PIC X(250).
