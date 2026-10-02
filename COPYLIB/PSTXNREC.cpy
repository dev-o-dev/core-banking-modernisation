      *================================================================*
      * COPYBOOK : PSTXNREC                                            *
      * TITLE    : POSTING TRANSACTION RECORD                          *
      * USED BY  : MEMO LOG      MBK.PRD.PST.MEMOLOG (VSAM ESDS)       *
      *            CICS FILE MEMOLOG - WRITTEN BY PSMEMO1              *
      *            BATCH INPUT   MBK.PRD.PST.POSTINGS.SORTED(+1)       *
      *            FB LRECL=200 - READ BY PSEOD010                     *
      *----------------------------------------------------------------*
      * SORT KEY FOR HARD POSTING IS POSITIONS 1-36 (PST-KEY).         *
      *----------------------------------------------------------------*
      * TRANSACTION CODES (PST-TXN-CODE)                               *
      *   TFR  INTERNAL TRANSFER      FPO  FASTER PAYMENT OUT          *
      *   FPI  FASTER PAYMENT IN      BGC  BACS/BANK GIRO CREDIT       *
      *   BAC  BACS PAYMENT OUT       CHP  CHAPS PAYMENT OUT           *
      *   SWO  SWIFT PAYMENT OUT      SWI  SWIFT PAYMENT IN            *
      *   CHG  CHARGE                 INT  INTEREST                    *
      *   DD   DIRECT DEBIT           SO   STANDING ORDER              *
      *   CSH  CASH (BRANCH/ATM)      REV  REVERSAL                    *
      *   TAX  INCOME TAX DEDUCTED    OPN  ACCOUNT OPENING DEPOSIT     *
      *   CLS  CLOSURE TRANSFER       UNP  UNPAID / RETURNED ITEM      *
      *   CHQ  CHEQUE PAID (INWARD)   CHD  CHEQUE DEPOSIT (OUTWARD)    *
      *   CRD  DEBIT CARD SETTLEMENT  LNR  LOAN REPAYMENT              *
      *   LND  LOAN DRAWDOWN (ADVANCE PAID TO CURRENT ACCOUNT)         *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1994-01-17 GHT  ORIGINAL VERSION                               *
      * 1998-11-23 MLW  Y2K                                            *
      * 2001-11-05 DKP  EXTERNAL REFERENCE WIDENED TO 24 (IBANK)       *
      * 2008-05-27 AOS  FASTER PAYMENTS TXN CODES (FPS GO-LIVE)        *
      * 2010-03-08 AOS  TAX, OPN, CLS, UNP FOR THE 2010 BATCH SUITE    *
      * 2010-08-02 JNB  CHQ, CHD, CRD, LNR (CHEQUES, CARDS, LOANS)     *
      * 2010-09-06 JNB  LND LOAN DRAWDOWN                              *
      *================================================================*
       01  PST-POSTING-RECORD.
           05  PST-KEY.
               10  PST-SORT-CODE           PIC X(06).
               10  PST-ACCT-NO             PIC X(08).
               10  PST-POST-DATE           PIC 9(08).
               10  PST-POST-TIME           PIC 9(08).
               10  PST-SEQ-NO              PIC 9(06).
           05  PST-TXN-CODE                PIC X(04).
               88  PST-TXN-VALID               VALUE 'TFR ' 'FPO '
                                                     'FPI ' 'BGC '
                                                     'BAC ' 'CHP '
                                                     'SWO ' 'SWI '
                                                     'CHG ' 'INT '
                                                     'DD  ' 'SO  '
                                                     'CSH ' 'REV '
                                                     'TAX ' 'OPN '
                                                     'CLS ' 'UNP '
                                                     'CHQ ' 'CHD '
                                                     'CRD ' 'LNR '
                                                     'LND '.
               88  PST-CUSTOMER-INITIATED      VALUE 'TFR ' 'FPO '
                                                     'FPI ' 'BGC '
                                                     'BAC ' 'CHP '
                                                     'SWO ' 'SWI '
                                                     'DD  ' 'SO  '
                                                     'CSH ' 'OPN '
                                                     'CHQ ' 'CHD '
                                                     'CRD ' 'LNR '.
           05  PST-DR-CR-IND               PIC X(01).
               88  PST-DEBIT                   VALUE 'D'.
               88  PST-CREDIT                  VALUE 'C'.
           05  PST-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  PST-CCY-CD                  PIC X(03).
           05  PST-VALUE-DATE              PIC 9(08).
           05  PST-NARRATIVE-1             PIC X(35).
           05  PST-NARRATIVE-2             PIC X(35).
           05  PST-CHANNEL-CD              PIC X(06).
               88  PST-CHNL-IBANK              VALUE 'IBANK '.
               88  PST-CHNL-BRANCH             VALUE 'BRANCH'.
               88  PST-CHNL-ATM                VALUE 'ATM   '.
               88  PST-CHNL-TELE               VALUE 'TELE  '.
               88  PST-CHNL-BATCH              VALUE 'BATCH '.
           05  PST-EXT-REF                 PIC X(24).
           05  PST-POST-TYPE               PIC X(01).
               88  PST-MEMO-POST               VALUE 'M'.
               88  PST-HARD-POST               VALUE 'H'.
           05  PST-STATUS                  PIC X(01).
               88  PST-PENDING                 VALUE 'P'.
               88  PST-APPLIED                 VALUE 'A'.
               88  PST-REJECTED                VALUE 'R'.
           05  PST-REJECT-CD               PIC X(04).
           05  PST-USER-ID                 PIC X(08).
           05  FILLER                      PIC X(26).
