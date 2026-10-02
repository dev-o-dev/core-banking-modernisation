      *================================================================*
      * COPYBOOK : LNORGREC                                            *
      * TITLE    : LOAN ORIGINATION - APPROVED LOAN FOR BOOKING        *
      * DATASET  : MBK.PRD.LOAN.APPROVED(0)  RECFM=FB LRECL=300        *
      *            WRITTEN BY THE LOAN ORIGINATION SYSTEM (LOS - JAVA  *
      *            ON WEBSPHERE) AT 18:00 AND SENT VIA CONNECT:DIRECT. *
      *            READ BY LNBKG010.                                   *
      *----------------------------------------------------------------*
      * ONE RECORD PER SIGNED AGREEMENT. THE LOS HAS ALREADY DONE      *
      * AFFORDABILITY, CREDIT SCORING AND THE CCA PRE-CONTRACT         *
      * INFORMATION (SECCI). THE HOST CALCULATES THE INSTALMENT ITSELF *
      * AND REJECTS THE LOAN IF IT DIFFERS FROM THE LOS FIGURE BY MORE *
      * THAN 1P - THE AGREEMENT THE CUSTOMER SIGNED MUST BE WHAT WE    *
      * COLLECT.                                                       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-09-06 JNB  ORIGINAL VERSION                               *
      *================================================================*
       01  LNO-APPROVED-LOAN.
           05  LNO-REC-TYPE                PIC X(01).
               88  LNO-HEADER                  VALUE 'H'.
               88  LNO-DETAIL                  VALUE 'D'.
               88  LNO-TRAILER                 VALUE 'T'.
           05  LNO-BODY                    PIC X(299).
           05  LNO-DETAIL-DATA REDEFINES LNO-BODY.
               10  LNO-APPLICATION-ID      PIC X(20).
               10  LNO-AGREEMENT-REF       PIC X(16).
               10  LNO-CIF-NO              PIC X(12).
               10  LNO-PROD-CD             PIC X(10).
               10  LNO-ADVANCE-AMT         PIC 9(11)V99.
               10  LNO-TERM-MONTHS         PIC 9(03).
               10  LNO-NOMINAL-RATE        PIC 9(03)V9(04).
               10  LNO-APR                 PIC 9(03)V9(01).
               10  LNO-MONTHLY-PAYMENT     PIC 9(11)V99.
               10  LNO-PAYMENT-DAY         PIC 9(02).
               10  LNO-PURPOSE-CD          PIC X(04).
               10  LNO-PAYOUT-SORT-CODE    PIC X(06).
               10  LNO-PAYOUT-ACCT-NO      PIC X(08).
               10  LNO-REPAY-SORT-CODE     PIC X(06).
               10  LNO-REPAY-ACCT-NO       PIC X(08).
               10  LNO-SIGNED-DATE         PIC 9(08).
               10  LNO-DRAWDOWN-DATE       PIC 9(08).
               10  LNO-BRANCH-SORT-CODE    PIC X(06).
               10  LNO-UNDERWRITER-ID      PIC X(08).
               10  LNO-CREDIT-SCORE        PIC 9(04).
               10  LNO-SETTLE-LOAN-NO      PIC X(10).
               10  FILLER                  PIC X(123).
           05  LNO-HDR-DATA REDEFINES LNO-BODY.
               10  LNOH-FILE-DATE          PIC 9(08).
               10  LNOH-FILE-SEQ           PIC 9(06).
               10  FILLER                  PIC X(285).
           05  LNO-TRL-DATA REDEFINES LNO-BODY.
               10  LNOT-RECORD-COUNT       PIC 9(09).
               10  LNOT-ADVANCE-TOTAL      PIC 9(13)V99.
               10  FILLER                  PIC X(275).
