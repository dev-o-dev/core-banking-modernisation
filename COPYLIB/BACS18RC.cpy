      *================================================================*
      * COPYBOOK : BACS18RC                                            *
      * TITLE    : BACS STANDARD 18 SUBMISSION FILE RECORDS            *
      * DATASET  : MBK.PRD.PAY.BACS.OUTBOUND(+1)  RECFM=FB LRECL=100   *
      *            COLLECTED BY BACSTEL-IP BUREAU SOFTWARE (PTX)       *
      *----------------------------------------------------------------*
      * LABEL RECORDS (VOL1/HDR1/HDR2/UHL1/EOF1/EOF2/UTL1) ARE 80      *
      * BYTES AND ARE PADDED TO 100 WITH SPACES - PTX STRIPS THE       *
      * PADDING ON SUBMISSION (PTX PROFILE MBKSTD18).                  *
      *----------------------------------------------------------------*
      * SERVICE USER NUMBER (SUN) 412345 - MEGABANK CUSTOMER PAYMENTS  *
      *================================================================*
       01  B18-LABEL-RECORD.
           05  B18-LABEL-ID                PIC X(04).
           05  B18-LABEL-BODY              PIC X(76).
      *    VOL1 - VOLUME HEADER
           05  B18-VOL1 REDEFINES B18-LABEL-BODY.
               10  B18V-SERIAL-NO          PIC X(06).
               10  B18V-ACCESSIBILITY      PIC X(01).
               10  FILLER                  PIC X(26).
               10  B18V-OWNER-ID           PIC X(14).
               10  FILLER                  PIC X(28).
               10  B18V-LABEL-STD          PIC X(01).
      *    HDR1 / EOF1
           05  B18-HDR1 REDEFINES B18-LABEL-BODY.
               10  B18H1-FILE-ID           PIC X(17).
               10  B18H1-SET-ID            PIC X(06).
               10  B18H1-FILE-SECTION      PIC X(04).
               10  B18H1-FILE-SEQ          PIC X(04).
               10  B18H1-GENERATION        PIC X(04).
               10  B18H1-GEN-VERSION       PIC X(02).
               10  B18H1-CREATE-DATE       PIC X(06).
               10  B18H1-EXPIRY-DATE       PIC X(06).
               10  B18H1-ACCESSIBILITY     PIC X(01).
               10  B18H1-BLOCK-COUNT       PIC X(06).
               10  B18H1-SYSTEM-CODE       PIC X(13).
               10  FILLER                  PIC X(07).
      *    HDR2 / EOF2
           05  B18-HDR2 REDEFINES B18-LABEL-BODY.
               10  B18H2-RECORD-FORMAT     PIC X(01).
               10  B18H2-BLOCK-LENGTH      PIC X(05).
               10  B18H2-RECORD-LENGTH     PIC X(05).
               10  FILLER                  PIC X(35).
               10  B18H2-BUFFER-OFFSET     PIC X(02).
               10  FILLER                  PIC X(28).
      *    UHL1 - USER HEADER
           05  B18-UHL1 REDEFINES B18-LABEL-BODY.
               10  B18U-PROCESS-DATE       PIC X(06).
               10  B18U-RECEIVING-ID       PIC X(06).
               10  FILLER                  PIC X(04).
               10  B18U-CURRENCY-CD        PIC X(02).
               10  B18U-COUNTRY-CD         PIC X(06).
               10  B18U-WORK-CODE          PIC X(09).
               10  B18U-FILE-NO            PIC X(03).
               10  FILLER                  PIC X(07).
               10  B18U-AUDIT-PRINT        PIC X(07).
               10  FILLER                  PIC X(26).
      *    UTL1 - USER TRAILER
           05  B18-UTL1 REDEFINES B18-LABEL-BODY.
               10  B18T-DEBIT-TOTAL        PIC 9(13).
               10  B18T-CREDIT-TOTAL       PIC 9(13).
               10  B18T-DEBIT-COUNT        PIC 9(07).
               10  B18T-CREDIT-COUNT       PIC 9(07).
               10  FILLER                  PIC X(08).
               10  FILLER                  PIC X(28).
           05  FILLER                      PIC X(20).
      *
       01  B18-DATA-RECORD.
           05  B18D-DEST-SORT-CODE         PIC X(06).
           05  B18D-DEST-ACCT-NO           PIC X(08).
           05  B18D-DEST-ACCT-TYPE         PIC X(01).
           05  B18D-TXN-CODE               PIC X(02).
               88  B18D-CREDIT                 VALUE '99'.
               88  B18D-DEBIT-CONTRA           VALUE '17'.
           05  B18D-ORIG-SORT-CODE         PIC X(06).
           05  B18D-ORIG-ACCT-NO           PIC X(08).
           05  B18D-FREE-FORMAT            PIC X(04).
           05  B18D-AMOUNT-PENCE           PIC 9(11).
           05  B18D-ORIG-NAME              PIC X(18).
           05  B18D-REFERENCE              PIC X(18).
           05  B18D-DEST-ACCT-NAME         PIC X(18).
