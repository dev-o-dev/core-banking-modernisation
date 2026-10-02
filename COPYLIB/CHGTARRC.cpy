      *================================================================*
      * COPYBOOK : CHGTARRC                                            *
      * TITLE    : ANNUAL SUMMARY OF CHARGES - LETTER EXTRACT          *
      * DATASET  : MBK.PRD.CHG.ANNSUMM(+1)  RECFM=FB LRECL=600         *
      *            WRITTEN BY CHGTAR10 (JOB CHGTAR01, MONTHLY).        *
      *            PRINTED BY DOCUMENT SERVICES - TEMPLATE ASOCP01     *
      *            (ASOCE01 FOR E-STATEMENT CUSTOMERS IS NOT YET LIVE).*
      *----------------------------------------------------------------*
      * ONE RECORD PER PERSONAL CURRENT ACCOUNT ON ITS ANNIVERSARY.    *
      * 8 CHARGE LINES IN FIXED ORDER (CTL-CHG-TYPE USES THE CGCAL010  *
      * CODES MFEE AODF UODF UNPD PDIT TXNF SWFT; LINE 8 IS 'OTHR' AND *
      * HOLDS CHAP AND ANY OTHER CODE). THE TEMPLATE SUPPLIES THE LINE *
      * DESCRIPTION FROM THE CODE. LINES WITH ZERO COUNT AND AMOUNT    *
      * ARE NOT PRINTED. AMOUNTS ARE NET OF CAP REDUCTIONS; REFUNDS    *
      * ARE SHOWN SEPARATELY AND NOT DEDUCTED FROM THE LINES.          *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-10-03 AOS  ORIGINAL VERSION (OFT/BBA PCA AGREEMENT 2011)  *
      * 2015-02-09 PDQ  UNARRANGED DEBIT INTEREST SHOWN SEPARATELY     *
      *================================================================*
       01  CTL-LETTER-RECORD.
           05  CTL-LETTER-CD               PIC X(04).
           05  CTL-TEMPLATE-ID             PIC X(08).
           05  CTL-ISSUE-DATE              PIC 9(08).
           05  CTL-SORT-CODE               PIC X(06).
           05  CTL-ACCT-NO                 PIC X(08).
           05  CTL-CIF-NO                  PIC X(12).
           05  CTL-PROD-CD                 PIC X(10).
           05  CTL-PROD-NAME               PIC X(18).
           05  CTL-CUST-NAME               PIC X(40).
           05  CTL-ADDRESS.
               10  CTL-ADDR-LINE1          PIC X(60).
               10  CTL-ADDR-LINE2          PIC X(60).
               10  CTL-TOWN                PIC X(40).
               10  CTL-POST-CD             PIC X(10).
           05  CTL-PERIOD-FROM             PIC 9(08).
           05  CTL-PERIOD-TO               PIC 9(08).
           05  CTL-CHG-LINE                OCCURS 8 TIMES.
               10  CTL-CHG-TYPE            PIC X(04).
               10  CTL-CHG-COUNT           PIC 9(05).
               10  CTL-CHG-AMOUNT          PIC 9(07)V99.
           05  CTL-TOTAL-CHARGES           PIC 9(09)V99.
           05  CTL-TOTAL-REFUNDS           PIC 9(09)V99.
           05  CTL-DR-INT-ARRANGED         PIC 9(09)V99.
           05  CTL-DR-INT-UNARRANGED       PIC 9(09)V99.
           05  CTL-CR-INT-GROSS            PIC 9(09)V99.
           05  CTL-CR-INT-TAX              PIC 9(09)V99.
           05  CTL-CURRENT-DR-RATE         PIC 9(03)V9(04).
           05  CTL-PART-YEAR-IND           PIC X(01).
               88  CTL-PART-YEAR               VALUE 'Y'.
           05  FILLER                      PIC X(82).
