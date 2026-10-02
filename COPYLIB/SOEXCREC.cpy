      *================================================================*
      * COPYBOOK : SOEXCREC                                            *
      * TITLE    : STANDING ORDER EXCEPTION / LETTER EXTRACT           *
      * DATASET  : MBK.PRD.SO.EXCEPTS(+1)  RECFM=FB LRECL=200          *
      *----------------------------------------------------------------*
      * WRITTEN BY SOEXE010 FOR EVERY STANDING ORDER THAT COULD NOT BE *
      * PAID, WAS SUSPENDED OR WAS CANCELLED BY THE SYSTEM. READ BY    *
      * SORPT010 (OPERATIONS REPORT) AND SENT TO DOCUMENT SERVICES FOR *
      * CUSTOMER LETTERS (TEMPLATES SOUNP01 / SOSUS01 / SOCAN01).      *
      *----------------------------------------------------------------*
      * EXCEPTION CODES (SOX-EXC-CD)                                   *
      *   NSFR  INSUFFICIENT FUNDS - WILL RETRY NEXT BUSINESS DAY      *
      *   NSFF  INSUFFICIENT FUNDS - FINAL ATTEMPT, PAYMENT MISSED     *
      *   SUSP  SUSPENDED AFTER 3 CONSECUTIVE MISSED PAYMENTS          *
      *   ACLS  PAYER ACCOUNT CLOSED - STANDING ORDER CANCELLED        *
      *   ARST  PAYER ACCOUNT RESTRICTED / FROZEN - PAYMENT MISSED     *
      *   BENF  INTERNAL BENEFICIARY ACCOUNT NOT FOUND OR CLOSED       *
      *   BADD  INVALID DATA ON STANDING ORDER - SUSPENDED             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1988-07-11 RJH  ORIGINAL VERSION                               *
      * 2008-05-27 AOS  RETRY / SUSPEND CODES                          *
      *================================================================*
       01  SOX-EXCEPTION-RECORD.
           05  SOX-KEY.
               10  SOX-PAYER-SORT-CODE     PIC X(06).
               10  SOX-PAYER-ACCT-NO       PIC X(08).
               10  SOX-SO-SEQ              PIC 9(04).
           05  SOX-BUS-DATE                PIC 9(08).
           05  SOX-EXC-CD                  PIC X(04).
               88  SOX-NSF-RETRY               VALUE 'NSFR'.
               88  SOX-NSF-FINAL               VALUE 'NSFF'.
               88  SOX-SUSPENDED               VALUE 'SUSP'.
               88  SOX-ACCT-CLOSED             VALUE 'ACLS'.
               88  SOX-ACCT-RESTRICTED         VALUE 'ARST'.
               88  SOX-BENE-NOT-FOUND          VALUE 'BENF'.
               88  SOX-BAD-DATA                VALUE 'BADD'.
           05  SOX-LETTER-IND              PIC X(01).
           05  SOX-CIF-NO                  PIC X(12).
           05  SOX-BENE-NAME               PIC X(35).
           05  SOX-BENE-SORT-CODE          PIC X(06).
           05  SOX-BENE-ACCT-NO            PIC X(08).
           05  SOX-BENE-REF                PIC X(18).
           05  SOX-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  SOX-DUE-DATE                PIC 9(08).
           05  SOX-AVAILABLE               PIC S9(13)V99 COMP-3.
           05  SOX-NEXT-DUE-DATE           PIC 9(08).
           05  SOX-CONSEC-FAILS            PIC 9(02).
           05  SOX-FEE-AMT                 PIC S9(07)V99 COMP-3.
           05  FILLER                      PIC X(51).
