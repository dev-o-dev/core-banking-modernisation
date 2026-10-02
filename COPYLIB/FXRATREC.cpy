      *================================================================*
      * COPYBOOK : FXRATREC                                            *
      * TITLE    : FOREIGN EXCHANGE RATE TABLE RECORD                  *
      * DATASET  : MBK.PRD.FX.RATES  (VSAM KSDS)                       *
      *            RECORDSIZE(150 150)  KEYS(3 0)                      *
      * CICS FILE: FXRATES (READ ONLY ONLINE - LOADED BY FXRAT010)     *
      *----------------------------------------------------------------*
      * ONE RECORD PER CURRENCY: STERLING AGAINST THE CURRENCY, I.E.   *
      * UNITS OF CURRENCY PER 1 GBP (USD 1.2534 MEANS 1 GBP = 1.2534   *
      * USD). RATES COME FROM GROUP TREASURY'S 07:30 AND 14:00 FIXING  *
      * FILES. THE CUSTOMER RATE IS THE MID RATE LESS (BUY) OR PLUS    *
      * (SELL) A MARGIN IN BASIS POINTS THAT FALLS WITH THE STERLING   *
      * AMOUNT (FXR-MARGIN-TIER).                                      *
      *   BANK SELLS CURRENCY (CUSTOMER PAYS ABROAD):                  *
      *       CUSTOMER RATE = MID * (1 - MARGIN/10000)                 *
      *   BANK BUYS CURRENCY (PAYMENT RECEIVED FROM ABROAD):           *
      *       CUSTOMER RATE = MID * (1 + MARGIN/10000)                 *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-05-09 JNB  ORIGINAL VERSION (PIR 2009/201 - SWIFT RATES   *
      *                 NOT VALIDATED)                                 *
      *================================================================*
       01  FXR-RATE-RECORD.
           05  FXR-KEY.
               10  FXR-CCY-CD              PIC X(03).
           05  FXR-CCY-NAME                PIC X(20).
           05  FXR-DECIMALS                PIC 9(01).
           05  FXR-STATUS                  PIC X(01).
               88  FXR-ACTIVE                  VALUE 'A'.
               88  FXR-HELD                    VALUE 'H'.
               88  FXR-SUSPENDED               VALUE 'S'.
           05  FXR-MID-RATE                PIC S9(05)V9(06) COMP-3.
           05  FXR-PREV-MID-RATE           PIC S9(05)V9(06) COMP-3.
           05  FXR-RATE-DATE               PIC 9(08).
           05  FXR-RATE-TIME               PIC 9(06).
           05  FXR-FIXING-ID               PIC X(08).
           05  FXR-TOLERANCE-PCT           PIC S9(02)V99 COMP-3.
           05  FXR-MARGIN-TIER             OCCURS 4 TIMES.
               10  FXR-TIER-GBP-UPTO       PIC S9(11)V99 COMP-3.
               10  FXR-TIER-MARGIN-BP      PIC S9(04) COMP-3.
           05  FXR-MAX-DEAL-GBP            PIC S9(11)V99 COMP-3.
           05  FXR-SWIFT-ALLOWED           PIC X(01).
           05  FXR-CARD-ALLOWED            PIC X(01).
           05  FXR-LAST-MAINT-DATE         PIC 9(08).
           05  FXR-LAST-MAINT-USER         PIC X(08).
           05  FILLER                      PIC X(23).
