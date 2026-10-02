      *================================================================*
      * COPYBOOK : CSSRECS                                             *
      * TITLE    : CURRENT ACCOUNT SWITCH SERVICE (CASS) RECORDS       *
      *----------------------------------------------------------------*
      * CASS (BACS, SEPTEMBER 2013) MOVES A CUSTOMER'S CURRENT ACCOUNT *
      * FROM AN OLD BANK TO A NEW BANK IN 7 WORKING DAYS, WITH ALL     *
      * STANDING ORDERS AND DIRECT DEBITS, AND REDIRECTS PAYMENTS TO   *
      * THE OLD ACCOUNT FOR 36 MONTHS.                                 *
      *                                                                *
      * FILES EXCHANGED WITH THE CASS CENTRAL SYSTEM VIA BACSTEL-IP    *
      * (FB 300), ONE FILE PER DAY IN EACH DIRECTION:                  *
      *   CSI-  INBOUND  - MESSAGES FROM CASS TO MEGABANK              *
      *   CSO-  OUTBOUND - MESSAGES FROM MEGABANK TO CASS              *
      * MESSAGE TYPES (CSI-MSG-TYPE / CSO-MSG-TYPE):                   *
      *   SWRQ  SWITCH REQUEST - TO OLD BANK (MEGABANK IS OLD BANK)    *
      *   SWAK  SWITCH ACCEPTED BY OLD BANK                            *
      *   SWRJ  SWITCH REJECTED BY OLD BANK (REASON IN CSx-REASON)     *
      *   SOTR  STANDING ORDER TRANSFER - ONE PER SO                   *
      *   DDTR  DIRECT DEBIT MANDATE TRANSFER - ONE PER MANDATE        *
      *   BALT  CLOSING BALANCE TRANSFER (SWITCH DATE)                 *
      *   SWCP  SWITCH COMPLETE                                        *
      * MEGABANK AS NEW BANK RECEIVES SWAK/SWRJ/SOTR/DDTR/BALT/SWCP    *
      * (CSSIN010). AS OLD BANK IT RECEIVES SWRQ AND SENDS SWAK, SOTR, *
      * DDTR, BALT AND SWCP (CSSOU010).                                *
      *----------------------------------------------------------------*
      * CSW-  SWITCH CONTROL RECORD - VSAM KSDS MBK.PRD.CASS.SWITCHES  *
      *       RECORDSIZE(250 250) KEYS(18 0), ONE PER SWITCH           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2013-06-17 PDQ  ORIGINAL VERSION (CASS GO-LIVE 16/09/2013)     *
      *================================================================*
       01  CSI-CASS-MESSAGE.
           05  CSI-MSG-TYPE                PIC X(04).
               88  CSI-SWITCH-REQUEST          VALUE 'SWRQ'.
               88  CSI-SWITCH-ACCEPTED         VALUE 'SWAK'.
               88  CSI-SWITCH-REJECTED         VALUE 'SWRJ'.
               88  CSI-SO-TRANSFER             VALUE 'SOTR'.
               88  CSI-DD-TRANSFER             VALUE 'DDTR'.
               88  CSI-BALANCE-TRANSFER        VALUE 'BALT'.
               88  CSI-SWITCH-COMPLETE         VALUE 'SWCP'.
               88  CSI-HEADER                  VALUE 'HDR '.
               88  CSI-TRAILER                 VALUE 'TRL '.
           05  CSI-SWITCH-ID               PIC X(18).
           05  CSI-SWITCH-DATE             PIC 9(08).
           05  CSI-OLD-SORT-CODE           PIC X(06).
           05  CSI-OLD-ACCT-NO             PIC X(08).
           05  CSI-NEW-SORT-CODE           PIC X(06).
           05  CSI-NEW-ACCT-NO             PIC X(08).
           05  CSI-REASON                  PIC X(04).
           05  CSI-BODY                    PIC X(238).
      *---- SWRQ ------------------------------------------------------*
           05  CSI-SWRQ-DATA REDEFINES CSI-BODY.
               10  CSI-CUST-SURNAME        PIC X(35).
               10  CSI-CUST-INITIALS       PIC X(05).
               10  CSI-CUST-DOB            PIC 9(08).
               10  CSI-CUST-POST-CD        PIC X(10).
               10  CSI-FULL-SWITCH         PIC X(01).
               10  FILLER                  PIC X(179).
      *---- SOTR ------------------------------------------------------*
           05  CSI-SOTR-DATA REDEFINES CSI-BODY.
               10  CSI-SO-BENE-NAME        PIC X(35).
               10  CSI-SO-BENE-SORT-CODE   PIC X(06).
               10  CSI-SO-BENE-ACCT-NO     PIC X(08).
               10  CSI-SO-BENE-REF         PIC X(18).
               10  CSI-SO-FREQ-CD          PIC X(02).
               10  CSI-SO-NEXT-DATE        PIC 9(08).
               10  CSI-SO-AMOUNT           PIC 9(11)V99.
               10  CSI-SO-FINAL-DATE       PIC 9(08).
               10  FILLER                  PIC X(140).
      *---- DDTR ------------------------------------------------------*
           05  CSI-DDTR-DATA REDEFINES CSI-BODY.
               10  CSI-DD-ORIG-SUN         PIC X(06).
               10  CSI-DD-ORIG-NAME        PIC X(18).
               10  CSI-DD-PAYER-REF        PIC X(18).
               10  CSI-DD-LAST-COLL-DATE   PIC 9(08).
               10  CSI-DD-LAST-COLL-AMT    PIC 9(11)V99.
               10  FILLER                  PIC X(175).
      *---- BALT ------------------------------------------------------*
           05  CSI-BALT-DATA REDEFINES CSI-BODY.
               10  CSI-BAL-AMOUNT          PIC 9(11)V99.
               10  CSI-BAL-SIGN            PIC X(01).
               10  FILLER                  PIC X(224).
      *---- HDR / TRL -------------------------------------------------*
           05  CSI-CTL-DATA REDEFINES CSI-BODY.
               10  CSI-FILE-DATE           PIC 9(08).
               10  CSI-FILE-SEQ            PIC 9(06).
               10  CSI-RECORD-COUNT        PIC 9(09).
               10  FILLER                  PIC X(215).
      *
      *---- OUTBOUND MESSAGE - SAME LAYOUT ----------------------------*
       01  CSO-CASS-MESSAGE                PIC X(300).
      *
       01  CSW-SWITCH-RECORD.
           05  CSW-KEY.
               10  CSW-SWITCH-ID           PIC X(18).
           05  CSW-DIRECTION               PIC X(01).
               88  CSW-INCOMING                VALUE 'I'.
               88  CSW-OUTGOING                VALUE 'O'.
           05  CSW-STATUS                  PIC X(02).
               88  CSW-REQUESTED               VALUE 'RQ'.
               88  CSW-ACCEPTED                VALUE 'AK'.
               88  CSW-REJECTED                VALUE 'RJ'.
               88  CSW-TRANSFERRING            VALUE 'TR'.
               88  CSW-COMPLETE                VALUE 'CP'.
               88  CSW-REDIRECTING             VALUE 'RD'.
               88  CSW-ENDED                   VALUE 'EN'.
           05  CSW-SWITCH-DATE             PIC 9(08).
           05  CSW-OLD-SORT-CODE           PIC X(06).
           05  CSW-OLD-ACCT-NO             PIC X(08).
           05  CSW-NEW-SORT-CODE           PIC X(06).
           05  CSW-NEW-ACCT-NO             PIC X(08).
           05  CSW-CIF-NO                  PIC X(12).
           05  CSW-SO-COUNT                PIC 9(03).
           05  CSW-DD-COUNT                PIC 9(03).
           05  CSW-BALANCE-AMT             PIC S9(13)V99 COMP-3.
           05  CSW-REQUEST-DATE            PIC 9(08).
           05  CSW-ACCEPT-DATE             PIC 9(08).
           05  CSW-COMPLETE-DATE           PIC 9(08).
           05  CSW-REDIRECT-END            PIC 9(08).
           05  CSW-REJECT-REASON           PIC X(04).
           05  CSW-LAST-MAINT-DATE         PIC 9(08).
           05  CSW-LAST-MAINT-PGM          PIC X(08).
           05  FILLER                      PIC X(115).
