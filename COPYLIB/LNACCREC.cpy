      *================================================================*
      * COPYBOOK : LNACCREC                                            *
      * TITLE    : PERSONAL LOAN ACCOUNT MASTER RECORD                 *
      * DATASET  : MBK.PRD.LOAN.MASTER - VSAM KSDS                     *
      *            RECORDSIZE(500 500) KEYS(10 0)                      *
      *            AIX MBK.PRD.LOAN.MASTER.CIFAIX - KEYS(12 10)        *
      *            NONUNIQUEKEY (LOANS BY CUSTOMER)                    *
      *            CICS FILE LOANMST (READ ONLY), LOANCIF (AIX PATH)   *
      *----------------------------------------------------------------*
      * UNSECURED PERSONAL LOANS, FIXED RATE, FIXED TERM, REPAID BY    *
      * EQUAL MONTHLY INSTALMENTS COLLECTED FROM A MEGABANK CURRENT    *
      * ACCOUNT (LN-REPAY-SORT-CODE / LN-REPAY-ACCT-NO). LOANS         *
      * REGULATED BY THE CONSUMER CREDIT ACT 1974.                     *
      *                                                                *
      * INTEREST IS CHARGED DAILY ON THE OUTSTANDING PRINCIPAL         *
      * (ACTUAL/365) BY LNINT010 AND HELD IN LN-ACCRUED-INT UNTIL THE  *
      * NEXT INSTALMENT. EACH INSTALMENT PAYS ACCRUED INTEREST FIRST,  *
      * THEN DEFAULT FEES, THEN PRINCIPAL (LNREP010).                  *
      *                                                                *
      * ARREARS (LNARR010):                                            *
      *   LN-ARREARS-AMT    INSTALMENTS DUE BUT NOT PAID               *
      *   LN-MISSED-COUNT   NUMBER OF CONSECUTIVE MISSED INSTALMENTS   *
      *   LN-ARREARS-BUCKET 0 UP TO DATE  1 1-29 DAYS  2 30-59         *
      *                     3 60-89  4 90-179  5 180+                  *
      *   LN-NOSIA-DATE     NOTICE OF SUMS IN ARREARS SENT (CCA S86B - *
      *                     AFTER 2 MISSED PAYMENTS, THEN EVERY 6      *
      *                     MONTHS WHILE IN ARREARS)                   *
      *   LN-DEFAULT-DATE   DEFAULT NOTICE SERVED (CCA S87 - 14 DAYS   *
      *                     TO REMEDY)                                 *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-09-06 JNB  ORIGINAL VERSION (LOANS MOVED FROM THE         *
      *                 PACKAGE SYSTEM 'LENDPRO' TO THE HOST)          *
      *================================================================*
       01  LN-LOAN-RECORD.
           05  LN-KEY.
               10  LN-LOAN-NO              PIC X(10).
           05  LN-CIF-NO                   PIC X(12).
           05  LN-PROD-CD                  PIC X(10).
           05  LN-STATUS                   PIC X(01).
               88  LN-ACTIVE                   VALUE 'A'.
               88  LN-SETTLED                  VALUE 'S'.
               88  LN-DEFAULTED                VALUE 'D'.
               88  LN-COLLECTIONS              VALUE 'C'.
               88  LN-WRITTEN-OFF              VALUE 'W'.
               88  LN-PENDING-DRAWDOWN         VALUE 'P'.
               88  LN-OPEN-STATUS              VALUE 'A' 'D' 'C'.
           05  LN-REPAY-ACCOUNT.
               10  LN-REPAY-SORT-CODE      PIC X(06).
               10  LN-REPAY-ACCT-NO        PIC X(08).
      *
      *---- ADVANCE AND TERMS -----------------------------------------*
           05  LN-TERMS.
               10  LN-ADVANCE-AMT          PIC S9(13)V99 COMP-3.
               10  LN-ADVANCE-DATE         PIC 9(08).
               10  LN-TERM-MONTHS          PIC 9(03).
               10  LN-NOMINAL-RATE         PIC S9(03)V9(04) COMP-3.
               10  LN-APR                  PIC S9(03)V9(01) COMP-3.
               10  LN-MONTHLY-PAYMENT      PIC S9(13)V99 COMP-3.
               10  LN-FINAL-PAYMENT        PIC S9(13)V99 COMP-3.
               10  LN-PAYMENT-DAY          PIC 9(02).
               10  LN-FIRST-DUE-DATE       PIC 9(08).
               10  LN-FINAL-DUE-DATE       PIC 9(08).
               10  LN-TOTAL-PAYABLE        PIC S9(13)V99 COMP-3.
               10  LN-PURPOSE-CD           PIC X(04).
                   88  LN-PURP-CAR             VALUE 'CAR '.
                   88  LN-PURP-HOME-IMP        VALUE 'HOME'.
                   88  LN-PURP-CONSOL          VALUE 'CONS'.
                   88  LN-PURP-OTHER           VALUE 'OTHR'.
               10  LN-AGREEMENT-REF        PIC X(16).
               10  LN-ORIG-SYSTEM-REF      PIC X(20).
               10  LN-BRANCH-SORT-CODE     PIC X(06).
      *
      *---- BALANCES --------------------------------------------------*
           05  LN-BALANCES.
               10  LN-PRINCIPAL-BAL        PIC S9(13)V99 COMP-3.
               10  LN-ACCRUED-INT          PIC S9(13)V99 COMP-3.
               10  LN-INT-RESIDUE          PIC S9(03)V9(06) COMP-3.
               10  LN-LAST-ACCRUAL-DATE    PIC 9(08).
               10  LN-FEES-OUTSTANDING     PIC S9(13)V99 COMP-3.
               10  LN-TOTAL-INT-CHARGED    PIC S9(13)V99 COMP-3.
               10  LN-TOTAL-INT-PAID       PIC S9(13)V99 COMP-3.
               10  LN-TOTAL-PRIN-PAID      PIC S9(13)V99 COMP-3.
               10  LN-TOTAL-FEES-PAID      PIC S9(13)V99 COMP-3.
               10  LN-INT-PAID-TAX-YEAR    PIC S9(13)V99 COMP-3.
      *
      *---- PAYMENT HISTORY -------------------------------------------*
           05  LN-REPAYMENT.
               10  LN-NEXT-DUE-DATE        PIC 9(08).
               10  LN-PAYMENTS-MADE        PIC 9(03).
               10  LN-PAYMENTS-REMAINING   PIC 9(03).
               10  LN-LAST-PAY-DATE        PIC 9(08).
               10  LN-LAST-PAY-AMT         PIC S9(13)V99 COMP-3.
               10  LN-RETRY-DATE           PIC 9(08).
               10  LN-RETRY-COUNT          PIC 9(01).
      *
      *---- ARREARS ---------------------------------------------------*
           05  LN-ARREARS.
               10  LN-ARREARS-AMT          PIC S9(13)V99 COMP-3.
               10  LN-MISSED-COUNT         PIC 9(03).
               10  LN-ARREARS-SINCE        PIC 9(08).
               10  LN-ARREARS-BUCKET       PIC 9(01).
                   88  LN-UP-TO-DATE           VALUE 0.
                   88  LN-BUCKET-1             VALUE 1.
                   88  LN-BUCKET-2             VALUE 2.
                   88  LN-BUCKET-3             VALUE 3.
                   88  LN-BUCKET-4             VALUE 4.
                   88  LN-BUCKET-5             VALUE 5.
               10  LN-WORST-BUCKET         PIC 9(01).
               10  LN-NOSIA-DATE           PIC 9(08).
               10  LN-NOSIA-COUNT          PIC 9(02).
               10  LN-DEFAULT-DATE         PIC 9(08).
               10  LN-DEFAULT-REMEDY-DATE  PIC 9(08).
               10  LN-TERMINATION-DATE     PIC 9(08).
               10  LN-COLLECTIONS-DATE     PIC 9(08).
               10  LN-COLLECTIONS-AGENCY   PIC X(04).
               10  LN-WRITE-OFF-DATE       PIC 9(08).
               10  LN-WRITE-OFF-AMT        PIC S9(13)V99 COMP-3.
               10  LN-FORBEARANCE-IND      PIC X(01).
                   88  LN-IN-FORBEARANCE       VALUE 'Y'.
               10  LN-FORBEARANCE-END      PIC 9(08).
               10  LN-FORBEARANCE-PAYMENT  PIC S9(13)V99 COMP-3.
               10  LN-INT-FROZEN-IND       PIC X(01).
                   88  LN-INT-FROZEN           VALUE 'Y'.
               10  LN-LAST-LETTER-CD       PIC X(04).
               10  LN-LAST-LETTER-DATE     PIC 9(08).
               10  LN-FEE-CHARGED-MISSED   PIC 9(03).
      *
      *---- SETTLEMENT / STATEMENTS -----------------------------------*
           05  LN-SETTLEMENT.
               10  LN-SETTLE-QUOTE-DATE    PIC 9(08).
               10  LN-SETTLE-QUOTE-AMT     PIC S9(13)V99 COMP-3.
               10  LN-SETTLE-VALID-TO      PIC 9(08).
               10  LN-SETTLED-DATE         PIC 9(08).
               10  LN-SETTLED-AMT          PIC S9(13)V99 COMP-3.
               10  LN-ANNUAL-STMT-DATE     PIC 9(08).
               10  LN-WITHDRAWAL-END-DATE  PIC 9(08).
      *
      *---- AUDIT -----------------------------------------------------*
           05  LN-OPENED-DATE              PIC 9(08).
           05  LN-CLOSED-DATE              PIC 9(08).
           05  LN-LAST-MAINT-DATE          PIC 9(08).
           05  LN-LAST-MAINT-USER          PIC X(08).
           05  FILLER                      PIC X(019).
