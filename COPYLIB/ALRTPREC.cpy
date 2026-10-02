      *================================================================*
      * COPYBOOK : ALRTPREC                                            *
      * TITLE    : CUSTOMER ACCOUNT ALERT PREFERENCES                  *
      * DATASET  : MBK.PRD.DDA.ALERTPRF  (VSAM KSDS)                   *
      *            RECORDSIZE(150 150)  KEYS(14 0)                     *
      * CICS FILE: ALRTPRF (IBANK 'MANAGE ALERTS' AND BRANCH SCREEN)   *
      *----------------------------------------------------------------*
      * ONE RECORD PER CURRENT ACCOUNT. HOLDS:                         *
      *   - THE CUSTOMER'S ALERT CHOICES (CHANNEL, LOW-BALANCE OPT-IN  *
      *     AND THRESHOLD, OPT-OUT OF THE MANDATORY UNARRANGED         *
      *     OVERDRAFT ALERTS)                                          *
      *   - THE STATE AND LAST-ALERT DATES USED BY DDALR010 TO         *
      *     SUPPRESS REPEAT ALERTS                                     *
      * UNARRANGED OVERDRAFT ALERTS ARE ON BY DEFAULT (AUTO-ENROLMENT  *
      * UNDER THE 2017 RULES). DDALR010 CREATES A DEFAULT RECORD THE   *
      * FIRST TIME AN ACCOUNT WITHOUT ONE NEEDS AN ALERT.              *
      *                                                                *
      * STATE FLAGS ('Y' = CONDITION TRUE AT THE LAST DDALR010 RUN):   *
      *   ALP-UNARR-STATE   IN UNARRANGED OVERDRAFT                    *
      *   ALP-LOWBAL-STATE  BELOW THE LOW-BALANCE THRESHOLD            *
      * AN ALERT IS SENT ONLY WHEN A CONDITION BECOMES TRUE. IT IS     *
      * RE-ARMED WHEN THE CONDITION CLEARS.                            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2016-09-05 HLT  ORIGINAL VERSION (UNARRANGED OD ALERTS)        *
      * 2017-01-23 HLT  LOW-BALANCE ALERTS MOVED FROM IBANK TO HOST    *
      *================================================================*
       01  ALP-PREF-RECORD.
           05  ALP-KEY.
               10  ALP-SORT-CODE           PIC X(06).
               10  ALP-ACCT-NO             PIC X(08).
           05  ALP-CIF-NO                  PIC X(12).
           05  ALP-STATUS                  PIC X(01).
               88  ALP-ACTIVE                  VALUE 'A'.
               88  ALP-ALL-SUSPENDED           VALUE 'S'.
           05  ALP-CHANNEL                 PIC X(01).
               88  ALP-BY-SMS                  VALUE 'S'.
               88  ALP-BY-EMAIL                VALUE 'E'.
               88  ALP-BY-BOTH                 VALUE 'B'.
           05  ALP-UNARR-OPT-OUT           PIC X(01).
               88  ALP-UNARR-OPTED-OUT         VALUE 'Y'.
           05  ALP-LOWBAL-IND              PIC X(01).
               88  ALP-LOWBAL-OPTED-IN         VALUE 'Y'.
           05  ALP-LOWBAL-THRESHOLD        PIC S9(13)V99 COMP-3.
           05  ALP-STATE.
               10  ALP-UNARR-STATE         PIC X(01).
                   88  ALP-WAS-UNARRANGED      VALUE 'Y'.
               10  ALP-LOWBAL-STATE        PIC X(01).
                   88  ALP-WAS-BELOW-THRESHOLD VALUE 'Y'.
           05  ALP-LAST-ALERTS.
               10  ALP-LAST-UNARR-DATE     PIC 9(08).
               10  ALP-LAST-CHGWARN-DATE   PIC 9(08).
               10  ALP-LAST-LOWBAL-DATE    PIC 9(08).
               10  ALP-LAST-LOWBAL-BAL     PIC S9(13)V99 COMP-3.
           05  ALP-ALERT-COUNT-MTD         PIC 9(05).
           05  ALP-ALERT-MONTH             PIC 9(06).
           05  ALP-CREATE-DATE             PIC 9(08).
           05  ALP-CREATE-SOURCE           PIC X(04).
               88  ALP-CREATED-BY-BATCH        VALUE 'BTCH'.
               88  ALP-CREATED-BY-IBANK        VALUE 'IBNK'.
               88  ALP-CREATED-BY-BRANCH       VALUE 'BRCH'.
           05  ALP-LAST-MAINT-DATE         PIC 9(08).
           05  ALP-LAST-MAINT-USER         PIC X(08).
           05  FILLER                      PIC X(39).
