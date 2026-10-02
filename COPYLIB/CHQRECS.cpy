      *================================================================*
      * COPYBOOK : CHQRECS                                             *
      * TITLE    : CHEQUE CLEARING RECORDS                             *
      *----------------------------------------------------------------*
      * CQI-  INWARD CLEARING ITEM - CHEQUES DRAWN ON MEGABANK         *
      *       ACCOUNTS PRESENTED BY OTHER BANKS THROUGH THE CHEQUE AND *
      *       CREDIT CLEARING COMPANY (C&CCC). FILE FROM THE CLEARING  *
      *       AGENT (IPSL) VIA CONNECT:DIRECT:                         *
      *         MBK.PRD.CHQ.INWARD(0)   FB 150                         *
      * CQR-  RETURNED (UNPAID) CHEQUE - SENT BACK TO THE PRESENTING   *
      *       BANK:  MBK.PRD.CHQ.RETURNS(+1)  FB 150                   *
      * CQS-  CHEQUE STOP - VSAM KSDS MBK.PRD.CHQ.STOPS                *
      *       RECORDSIZE(100 100) KEYS(20 0) - CICS FILE CHQSTOP       *
      * CQD-  OUTWARD (DEPOSITED) CHEQUE AWAITING CLEARANCE - VSAM     *
      *       KSDS MBK.PRD.CHQ.UNCLEARED RECORDSIZE(120 120)           *
      *       KEYS(28 0) - WRITTEN BY THE TELLER SYSTEM, RELEASED BY   *
      *       CQUNC010                                                 *
      *----------------------------------------------------------------*
      * RETURN REASONS (CQR-RETURN-RSN)                                *
      *   RD   REFER TO DRAWER (INSUFFICIENT FUNDS)                    *
      *   PS   PAYMENT STOPPED BY DRAWER                               *
      *   AC   ACCOUNT CLOSED                                          *
      *   NA   NO ACCOUNT                                              *
      *   DE   DRAWER DECEASED                                         *
      *   FR   ACCOUNT FROZEN / RESTRICTED - REFER TO DRAWER           *
      *   OD   OUT OF DATE (OVER 6 MONTHS)                             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1990-06-11 RJH  ORIGINAL VERSION (INWARD CLEARING)             *
      * 2007-11-19 JNB  2-4-6 CLEARING TIMESCALES (OFT) - UNCLEARED    *
      *                 FUNDS RECORD                                   *
      *================================================================*
       01  CQI-INWARD-ITEM.
           05  CQI-REC-TYPE                PIC X(01).
               88  CQI-HEADER                  VALUE 'H'.
               88  CQI-ITEM                    VALUE 'I'.
               88  CQI-TRAILER                 VALUE 'T'.
           05  CQI-BODY                    PIC X(149).
           05  CQI-ITEM-DATA REDEFINES CQI-BODY.
               10  CQI-SORT-CODE           PIC X(06).
               10  CQI-ACCT-NO             PIC X(08).
               10  CQI-SERIAL-NO           PIC X(06).
               10  CQI-AMOUNT              PIC 9(11)V99.
               10  CQI-CHEQUE-DATE         PIC 9(08).
               10  CQI-PRESENTING-BANK     PIC X(06).
               10  CQI-ITEM-REF            PIC X(20).
               10  CQI-CLEARING-DATE       PIC 9(08).
               10  CQI-PAYEE-NAME          PIC X(35).
               10  FILLER                  PIC X(39).
           05  CQI-HDR-DATA REDEFINES CQI-BODY.
               10  CQIH-FILE-ID            PIC X(12).
               10  CQIH-CLEARING-DATE      PIC 9(08).
               10  FILLER                  PIC X(129).
           05  CQI-TRL-DATA REDEFINES CQI-BODY.
               10  CQIT-ITEM-COUNT         PIC 9(09).
               10  CQIT-TOTAL-AMT          PIC 9(13)V99.
               10  FILLER                  PIC X(125).
      *
       01  CQR-RETURN-ITEM.
           05  CQR-SORT-CODE               PIC X(06).
           05  CQR-ACCT-NO                 PIC X(08).
           05  CQR-SERIAL-NO               PIC X(06).
           05  CQR-AMOUNT                  PIC 9(11)V99.
           05  CQR-PRESENTING-BANK         PIC X(06).
           05  CQR-ITEM-REF                PIC X(20).
           05  CQR-RETURN-RSN              PIC X(02).
               88  CQR-REFER-TO-DRAWER         VALUE 'RD'.
               88  CQR-PAYMENT-STOPPED         VALUE 'PS'.
               88  CQR-ACCOUNT-CLOSED          VALUE 'AC'.
               88  CQR-NO-ACCOUNT              VALUE 'NA'.
               88  CQR-DRAWER-DECEASED         VALUE 'DE'.
               88  CQR-FROZEN                  VALUE 'FR'.
               88  CQR-OUT-OF-DATE             VALUE 'OD'.
           05  CQR-RETURN-DATE             PIC 9(08).
           05  FILLER                      PIC X(81).
      *
       01  CQS-STOP-RECORD.
           05  CQS-KEY.
               10  CQS-SORT-CODE           PIC X(06).
               10  CQS-ACCT-NO             PIC X(08).
               10  CQS-SERIAL-FROM         PIC X(06).
           05  CQS-SERIAL-TO               PIC X(06).
           05  CQS-AMOUNT                  PIC S9(11)V99 COMP-3.
           05  CQS-PAYEE                   PIC X(30).
           05  CQS-PLACED-DATE             PIC 9(08).
           05  CQS-PLACED-USER             PIC X(08).
           05  CQS-STATUS                  PIC X(01).
               88  CQS-ACTIVE                  VALUE 'A'.
               88  CQS-REMOVED                 VALUE 'R'.
               88  CQS-HIT                     VALUE 'H'.
           05  CQS-HIT-DATE                PIC 9(08).
           05  FILLER                      PIC X(12).
      *
       01  CQD-DEPOSIT-RECORD.
           05  CQD-KEY.
               10  CQD-SORT-CODE           PIC X(06).
               10  CQD-ACCT-NO             PIC X(08).
               10  CQD-DEPOSIT-DATE        PIC 9(08).
               10  CQD-SEQ                 PIC 9(06).
           05  CQD-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  CQD-DRAWN-ON-SORT           PIC X(06).
           05  CQD-INTEREST-DATE           PIC 9(08).
           05  CQD-WITHDRAW-DATE           PIC 9(08).
           05  CQD-CERTAINTY-DATE          PIC 9(08).
           05  CQD-STATUS                  PIC X(01).
               88  CQD-UNCLEARED               VALUE 'U'.
               88  CQD-AVAILABLE               VALUE 'W'.
               88  CQD-CLEARED                 VALUE 'C'.
               88  CQD-RETURNED                VALUE 'R'.
           05  CQD-BRANCH-SC               PIC X(06).
           05  CQD-TELLER-ID               PIC X(08).
           05  FILLER                      PIC X(39).
