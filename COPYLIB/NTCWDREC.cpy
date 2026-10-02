      *================================================================*
      * COPYBOOK : NTCWDREC                                            *
      * TITLE    : NOTICE SAVINGS - WITHDRAWAL NOTICE RECORD           *
      * DATASET  : MBK.PRD.SAV.NOTICES  (VSAM KSDS)                    *
      *            RECORDSIZE(150 150)  KEYS(22 0)                     *
      * CICS FILE: SAVNTC (WRITTEN BY THE BRANCH TELLER SYSTEM AND     *
      *            IBANK 'GIVE NOTICE' VIA THE MEMOPOST GATEWAY)       *
      *----------------------------------------------------------------*
      * A NOTICE ACCOUNT (PRD-NOTICE-DAYS > 0) PAYS A HIGHER RATE IN   *
      * RETURN FOR N DAYS' NOTICE OF A WITHDRAWAL. THE CUSTOMER GIVES  *
      * NOTICE (NTC-GIVEN-DATE), THE FUNDS ARE PAID ON NTC-DUE-DATE TO *
      * THE NOMINATED ACCOUNT BY INNTC010.                             *
      * AN IMMEDIATE (EARLY) WITHDRAWAL IS ALLOWED FOR NTC-TYPE 'E' -  *
      * IT LOSES PRD-CR-PENALTY-DAYS DAYS' INTEREST ON THE AMOUNT.     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-07-04 AOS  ORIGINAL VERSION (95-DAY NOTICE ACCOUNT LAUNCH)*
      *================================================================*
       01  NTC-NOTICE-RECORD.
           05  NTC-KEY.
               10  NTC-SORT-CODE           PIC X(06).
               10  NTC-ACCT-NO             PIC X(08).
               10  NTC-GIVEN-DATE          PIC 9(08).
           05  NTC-TYPE                    PIC X(01).
               88  NTC-NOTICE              VALUE 'N'.
               88  NTC-EARLY               VALUE 'E'.
               88  NTC-CLOSURE             VALUE 'C'.
           05  NTC-STATUS                  PIC X(01).
               88  NTC-PENDING             VALUE 'P'.
               88  NTC-PAID                VALUE 'D'.
               88  NTC-CANCELLED           VALUE 'X'.
               88  NTC-FAILED              VALUE 'F'.
           05  NTC-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  NTC-DUE-DATE                PIC 9(08).
           05  NTC-TO-SORT-CODE            PIC X(06).
           05  NTC-TO-ACCT-NO              PIC X(08).
           05  NTC-CHANNEL                 PIC X(06).
           05  NTC-USER-ID                 PIC X(08).
           05  NTC-PAID-DATE               PIC 9(08).
           05  NTC-PAID-AMOUNT             PIC S9(13)V99 COMP-3.
           05  NTC-PENALTY-AMT             PIC S9(13)V99 COMP-3.
           05  NTC-FAIL-REASON             PIC X(04).
           05  NTC-REFERENCE               PIC X(18).
           05  FILLER                      PIC X(36).
