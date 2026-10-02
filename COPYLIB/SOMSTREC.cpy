      *================================================================*
      * COPYBOOK : SOMSTREC                                            *
      * TITLE    : STANDING ORDER MASTER RECORD                        *
      * DATASET  : MBK.PRD.SO.MASTER  (VSAM KSDS)                      *
      *            RECORDSIZE(400 400)  KEYS(18 0)                     *
      *            AIX MBK.PRD.SO.MASTER.DUEAIX  KEYS(8 18)            *
      *            NONUNIQUEKEY - BY NEXT DUE DATE (USED BY SOEXE010)  *
      * CICS FILE: SOMAST (BASE)                                       *
      *----------------------------------------------------------------*
      * KEY = PAYER SORT CODE + ACCOUNT + 4-DIGIT SO SEQUENCE. THE     *
      * SEQUENCE IS ALLOCATED FROM SOM-LAST-SO-SEQ ON THE ACCOUNT'S    *
      * CONTROL RECORD (SEQUENCE 0000) - SEE SOMNT01 3100-ALLOCATE.    *
      *                                                                *
      * FREQUENCIES (SOM-FREQ-CD)                                      *
      *   W1 WEEKLY      W2 FORTNIGHTLY   W4 FOUR-WEEKLY               *
      *   M1 MONTHLY     M3 QUARTERLY     M6 HALF-YEARLY  MA ANNUALLY  *
      *                                                                *
      * BENEFICIARY ROUTING (SOM-ROUTE-CD)                             *
      *   I  INTERNAL TRANSFER TO A MEGABANK ACCOUNT (TFR POSTINGS)    *
      *   F  FASTER PAYMENTS (FPS STANDING ORDER - FROM 2008)          *
      *   B  BACS (LEGACY - NEW SOS ARE ROUTED VIA FPS)                *
      *                                                                *
      * WEEKEND / HOLIDAY RULE: AN SO DUE ON A NON-BUSINESS DAY IS     *
      * PAID ON THE NEXT BUSINESS DAY. THE NEXT DUE DATE IS ALWAYS     *
      * CALCULATED FROM THE NOMINAL (UNADJUSTED) DATE SO MONTHLY SOS   *
      * DO NOT DRIFT. SOM-NOMINAL-DAY HOLDS THE ORIGINAL DAY OF MONTH. *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1988-07-11 RJH  ORIGINAL VERSION (DD0500 STANDING ORDERS)      *
      * 1998-11-23 MLW  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-10-14 DKP  IBANK SO MAINTENANCE - CREATED-BY CHANNEL      *
      * 2008-05-27 AOS  FPS ROUTING, RETRY COUNT                       *
      *================================================================*
       01  SOM-SO-RECORD.
           05  SOM-KEY.
               10  SOM-PAYER-SORT-CODE     PIC X(06).
               10  SOM-PAYER-ACCT-NO       PIC X(08).
               10  SOM-SO-SEQ              PIC 9(04).
                   88  SOM-CONTROL-REC         VALUE 0.
           05  SOM-NEXT-DUE-DATE           PIC 9(08).
           05  SOM-DATA                    PIC X(374).
      *
      *---- SEQUENCE 0000 - ACCOUNT CONTROL RECORD --------------------*
           05  SOM-CONTROL-DATA REDEFINES SOM-DATA.
               10  SOM-LAST-SO-SEQ         PIC 9(04).
               10  SOM-ACTIVE-COUNT        PIC 9(04).
               10  SOM-CTL-CREATE-DATE     PIC 9(08).
               10  FILLER                  PIC X(358).
      *
      *---- SEQUENCE 0001-9999 - STANDING ORDER -----------------------*
           05  SOM-SO-DATA REDEFINES SOM-DATA.
               10  SOM-STATUS              PIC X(01).
                   88  SOM-ACTIVE              VALUE 'A'.
                   88  SOM-SUSPENDED           VALUE 'S'.
                   88  SOM-CANCELLED           VALUE 'C'.
                   88  SOM-COMPLETED           VALUE 'X'.
               10  SOM-ROUTE-CD            PIC X(01).
                   88  SOM-ROUTE-INTERNAL      VALUE 'I'.
                   88  SOM-ROUTE-FPS           VALUE 'F'.
                   88  SOM-ROUTE-BACS          VALUE 'B'.
               10  SOM-BENE-NAME           PIC X(35).
               10  SOM-BENE-SORT-CODE      PIC X(06).
               10  SOM-BENE-ACCT-NO        PIC X(08).
               10  SOM-BENE-REF            PIC X(18).
               10  SOM-FREQ-CD             PIC X(02).
                   88  SOM-WEEKLY              VALUE 'W1'.
                   88  SOM-FORTNIGHTLY         VALUE 'W2'.
                   88  SOM-FOUR-WEEKLY         VALUE 'W4'.
                   88  SOM-MONTHLY             VALUE 'M1'.
                   88  SOM-QUARTERLY           VALUE 'M3'.
                   88  SOM-HALF-YEARLY         VALUE 'M6'.
                   88  SOM-ANNUALLY            VALUE 'MA'.
                   88  SOM-FREQ-VALID          VALUE 'W1' 'W2' 'W4'
                                                     'M1' 'M3' 'M6'
                                                     'MA'.
               10  SOM-NOMINAL-DAY         PIC 9(02).
               10  SOM-FIRST-PAY-DATE      PIC 9(08).
               10  SOM-FIRST-AMT           PIC S9(13)V99 COMP-3.
               10  SOM-REGULAR-AMT         PIC S9(13)V99 COMP-3.
               10  SOM-FINAL-AMT           PIC S9(13)V99 COMP-3.
               10  SOM-FINAL-PAY-DATE      PIC 9(08).
               10  SOM-PAYMENTS-REMAINING  PIC 9(04).
                   88  SOM-UNTIL-FURTHER-NOTICE VALUE 9999.
               10  SOM-PAYMENTS-MADE       PIC 9(05).
               10  SOM-NOMINAL-DUE-DATE    PIC 9(08).
               10  SOM-LAST-PAID-DATE      PIC 9(08).
               10  SOM-LAST-PAID-AMT       PIC S9(13)V99 COMP-3.
               10  SOM-LAST-ATTEMPT-DATE   PIC 9(08).
               10  SOM-RETRY-COUNT         PIC 9(01).
               10  SOM-LAST-FAIL-RSN       PIC X(04).
               10  SOM-CONSEC-FAILS        PIC 9(02).
               10  SOM-TOTAL-PAID          PIC S9(13)V99 COMP-3.
               10  SOM-CREATE-DATE         PIC 9(08).
               10  SOM-CREATE-CHANNEL      PIC X(06).
               10  SOM-CREATE-USER         PIC X(08).
               10  SOM-AMEND-DATE          PIC 9(08).
               10  SOM-AMEND-CHANNEL       PIC X(06).
               10  SOM-AMEND-USER          PIC X(08).
               10  SOM-CANCEL-DATE         PIC 9(08).
               10  SOM-CANCEL-RSN          PIC X(04).
               10  SOM-LAST-PAY-ID         PIC X(16).
               10  FILLER                  PIC X(125).
