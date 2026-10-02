      *================================================================*
      * COPYBOOK : ODLTRREC                                            *
      * TITLE    : ARRANGED OVERDRAFT REVIEW / EXPIRY LETTER EXTRACT   *
      * DATASET  : MBK.PRD.OD.LETTERS(+1)  RECFM=FB LRECL=200          *
      *            WRITTEN BY ODREV010; PRINTED BY DOCUMENT SERVICES   *
      *            (TEMPLATE NAMED IN ODL-TEMPLATE-ID). THE POSTAL     *
      *            ADDRESS IS MERGED BY DOCUMENT SERVICES FROM THE     *
      *            NIGHTLY CIF EXTRACT USING ODL-CIF-NO.               *
      *----------------------------------------------------------------*
      * LETTER CODES (ODL-LETTER-CD)                                   *
      *   ODRV  LIMIT DUE FOR REVIEW - 30 DAY NOTICE    ODRVP01/ODRVB01*
      *   ODR7  LIMIT DUE FOR REVIEW - FINAL REMINDER   ODR7P01/ODR7B01*
      *   ODEX  LIMIT EXPIRED AND REMOVED (IN CREDIT)   ODEXP01/ODEXB01*
      *   ODXO  LIMIT EXPIRED AND REMOVED - ACCOUNT NOW                *
      *         IN UNARRANGED OVERDRAFT                 ODXOP01/ODXOB01*
      * TEMPLATE SUFFIX P = PERSONAL, B = BUSINESS (CIF-CUST-TYPE).    *
      *----------------------------------------------------------------*
      * AMOUNTS ARE UNSIGNED UNLESS SHOWN SIGN LEADING SEPARATE.       *
      * ODL-EXCESS-AMT IS THE AMOUNT BY WHICH THE LEDGER BALANCE IS    *
      * BELOW ZERO AFTER THE LIMIT WAS REMOVED (ODXO ONLY).            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2004-11-08 SAT  ORIGINAL VERSION (OFT OVERDRAFT REVIEW)        *
      * 2009-10-05 AOS  ODR7 FINAL REMINDER, BUSINESS TEMPLATES        *
      *================================================================*
       01  ODL-LETTER-RECORD.
           05  ODL-LETTER-CD               PIC X(04).
               88  ODL-REVIEW-30               VALUE 'ODRV'.
               88  ODL-REVIEW-7                VALUE 'ODR7'.
               88  ODL-EXPIRED                 VALUE 'ODEX'.
               88  ODL-EXPIRED-OVERDRAWN       VALUE 'ODXO'.
           05  ODL-TEMPLATE-ID             PIC X(08).
           05  ODL-ISSUE-DATE              PIC 9(08).
           05  ODL-SORT-CODE               PIC X(06).
           05  ODL-ACCT-NO                 PIC X(08).
           05  ODL-CIF-NO                  PIC X(12).
           05  ODL-CUST-TYPE               PIC X(01).
           05  ODL-CUST-NAME               PIC X(40).
           05  ODL-POST-CD                 PIC X(10).
           05  ODL-PROD-CD                 PIC X(10).
           05  ODL-OD-LIMIT                PIC 9(11)V99.
           05  ODL-EXPIRY-DATE             PIC 9(08).
           05  ODL-LEDGER-BAL              PIC S9(11)V99
                                           SIGN LEADING SEPARATE.
           05  ODL-EXCESS-AMT              PIC 9(11)V99.
           05  ODL-DR-RATE                 PIC 9(03)V9(04).
           05  ODL-MONTHLY-FEE             PIC 9(05)V99.
           05  ODL-DAYS-TO-EXPIRY          PIC 9(03).
           05  ODL-HOME-BRANCH-SC          PIC X(06).
           05  FILLER                      PIC X(22).
