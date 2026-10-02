      *================================================================*
      * COPYBOOK : GLJNLREC                                            *
      * TITLE    : GENERAL LEDGER JOURNAL LINE                         *
      * DATASETS : MBK.PRD.GL.JOURNAL.<SOURCE>(+1)  RECFM=FB LRECL=250 *
      *            ONE GDG PER SOURCE SYSTEM - INACR010 (ACCRUALS),    *
      *            INCAP010 (CAPITALISATION), CGAPL010 (CHARGES),      *
      *            GLEXT010 (CUSTOMER TRANSACTION MOVEMENTS)           *
      *            CONCATENATED BY JOB GLFEED01 INTO                   *
      *            MBK.PRD.GL.FEED(+1) AND SENT TO THE GROUP LEDGER    *
      *            (ORACLE GL - FINANCE SYSTEMS) VIA CONNECT:DIRECT.   *
      *----------------------------------------------------------------*
      * EVERY JOURNAL (GLJ-JOURNAL-ID) MUST BALANCE: SUM OF DEBITS =   *
      * SUM OF CREDITS. GLREC010 REJECTS THE WHOLE FEED IF ANY         *
      * JOURNAL DOES NOT BALANCE.                                      *
      *                                                                *
      * GL ACCOUNT CODE FORMAT (CHART OF ACCOUNTS 2002):               *
      *   NNNNN-NNNN    E.G. 21010-0100 CUSTOMER CURRENT ACCOUNTS      *
      *   1XXXX ASSETS  2XXXX LIABILITIES  4XXXX INCOME  5XXXX EXPENSE *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2002-06-03 KLM  ORIGINAL VERSION (ORACLE GL REPLACES MSA GL)   *
      * 2006-01-16 KLM  COST CENTRE AND PRODUCT ADDED FOR MI           *
      * 2010-09-06 JNB  SOURCE 'LOAN' - LNINT010 / LNBKG010 / LNWOF010 *
      * 2017-03-06 PDQ  SOURCES 'CMPL' (CMPRD010), 'ATMS' (ATMSTL10)   *
      *================================================================*
       01  GLJ-JOURNAL-LINE.
           05  GLJ-REC-TYPE                PIC X(01).
               88  GLJ-HEADER                  VALUE 'H'.
               88  GLJ-DETAIL                  VALUE 'D'.
               88  GLJ-TRAILER                 VALUE 'T'.
           05  GLJ-SOURCE-SYSTEM           PIC X(04).
               88  GLJ-SRC-ACCRUAL             VALUE 'ACCR'.
               88  GLJ-SRC-CAPITALISE          VALUE 'ICAP'.
               88  GLJ-SRC-CHARGES             VALUE 'CHRG'.
               88  GLJ-SRC-MOVEMENTS           VALUE 'MOVT'.
               88  GLJ-SRC-SUSPENSE            VALUE 'SUSP'.
               88  GLJ-SRC-LOANS               VALUE 'LOAN'.
               88  GLJ-SRC-COMPLAINTS          VALUE 'CMPL'.
               88  GLJ-SRC-ATM-SETTLE          VALUE 'ATMS'.
               88  GLJ-SRC-BRANCH-CREDITS      VALUE 'BGCS'.
           05  GLJ-JOURNAL-ID              PIC X(16).
           05  GLJ-BUSINESS-DATE           PIC 9(08).
           05  GLJ-BODY                    PIC X(221).
      *
           05  GLJ-HDR REDEFINES GLJ-BODY.
               10  GLJH-RUN-ID             PIC X(05).
               10  GLJH-CREATE-DATE        PIC 9(08).
               10  GLJH-CREATE-TIME        PIC 9(06).
               10  GLJH-CREATING-PGM       PIC X(08).
               10  FILLER                  PIC X(194).
      *
           05  GLJ-DTL REDEFINES GLJ-BODY.
               10  GLJD-LINE-NO            PIC 9(07).
               10  GLJD-GL-ACCOUNT         PIC X(10).
               10  GLJD-COST-CENTRE        PIC X(06).
               10  GLJD-PROD-CD            PIC X(10).
               10  GLJD-CCY-CD             PIC X(03).
               10  GLJD-DR-CR-IND          PIC X(01).
                   88  GLJD-DEBIT              VALUE 'D'.
                   88  GLJD-CREDIT             VALUE 'C'.
               10  GLJD-AMOUNT             PIC 9(13)V99.
               10  GLJD-VALUE-DATE         PIC 9(08).
               10  GLJD-DESCRIPTION        PIC X(40).
               10  GLJD-SORT-CODE          PIC X(06).
               10  GLJD-ACCT-NO            PIC X(08).
               10  GLJD-TXN-CODE           PIC X(04).
               10  GLJD-SOURCE-REF         PIC X(24).
               10  FILLER                  PIC X(79).
      *
           05  GLJ-TRL REDEFINES GLJ-BODY.
               10  GLJT-LINE-COUNT         PIC 9(09).
               10  GLJT-TOTAL-DR           PIC 9(15)V99.
               10  GLJT-TOTAL-CR           PIC 9(15)V99.
               10  GLJT-JOURNAL-COUNT      PIC 9(07).
               10  FILLER                  PIC X(171).
