      *================================================================*
      * COPYBOOK : LNLTRREC                                            *
      * TITLE    : LOAN CUSTOMER LETTER EXTRACT                        *
      * DATASET  : MBK.PRD.LOAN.LETTERS(+1)  RECFM=FB LRECL=300        *
      *            WRITTEN BY LNBKG010 / LNREP010 / LNARR010 /         *
      *            LNSTM010; PRINTED BY DOCUMENT SERVICES (TEMPLATE    *
      *            NAMED IN LNX-TEMPLATE-ID).                          *
      *----------------------------------------------------------------*
      * LETTER CODES (LNX-LETTER-CD)                                   *
      *   WELC  WELCOME / COPY OF AGREEMENT CONFIRMATION     LNWEL01   *
      *   MISS  MISSED PAYMENT - INFORMAL REMINDER           LNMIS01   *
      *   NSIA  NOTICE OF SUMS IN ARREARS (CCA S86B)         LNNSA01   *
      *   DFLT  DEFAULT NOTICE (CCA S87 / S88)               LNDFT01   *
      *   TERM  TERMINATION - PASSED TO COLLECTIONS          LNTRM01   *
      *   FBRN  FORBEARANCE ARRANGEMENT CONFIRMED            LNFBR01   *
      *   ANST  ANNUAL STATEMENT (CCA S77A)                  LNANS01   *
      *   SETL  SETTLEMENT CONFIRMATION / CLOSURE            LNSET01   *
      *   ARRC  ARREARS CLEARED                              LNARC01   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-09-06 JNB  ORIGINAL VERSION                               *
      *================================================================*
       01  LNX-LETTER-RECORD.
           05  LNX-LETTER-CD               PIC X(04).
           05  LNX-TEMPLATE-ID             PIC X(08).
           05  LNX-ISSUE-DATE              PIC 9(08).
           05  LNX-LOAN-NO                 PIC X(10).
           05  LNX-CIF-NO                  PIC X(12).
           05  LNX-AGREEMENT-REF           PIC X(16).
           05  LNX-AMOUNTS.
               10  LNX-ADVANCE-AMT         PIC 9(11)V99.
               10  LNX-PRINCIPAL-BAL       PIC 9(11)V99.
               10  LNX-ACCRUED-INT         PIC 9(11)V99.
               10  LNX-ARREARS-AMT         PIC 9(11)V99.
               10  LNX-FEES-AMT            PIC 9(11)V99.
               10  LNX-MONTHLY-PAYMENT     PIC 9(11)V99.
               10  LNX-SETTLEMENT-AMT      PIC 9(11)V99.
           05  LNX-NOMINAL-RATE            PIC 9(03)V9(04).
           05  LNX-APR                     PIC 9(03)V9(01).
           05  LNX-TERM-MONTHS             PIC 9(03).
           05  LNX-PAYMENTS-REMAINING      PIC 9(03).
           05  LNX-MISSED-COUNT            PIC 9(03).
           05  LNX-NEXT-DUE-DATE           PIC 9(08).
           05  LNX-REMEDY-DATE             PIC 9(08).
           05  LNX-PERIOD-FROM             PIC 9(08).
           05  LNX-PERIOD-TO               PIC 9(08).
           05  LNX-PERIOD-PAID             PIC 9(11)V99.
           05  LNX-PERIOD-INT              PIC 9(11)V99.
           05  LNX-PERIOD-FEES             PIC 9(11)V99.
           05  LNX-REPAY-SORT-CODE         PIC X(06).
           05  LNX-REPAY-ACCT-NO           PIC X(08).
           05  LNX-DOC-REF                 PIC X(20).
           05  FILLER                      PIC X(26).
