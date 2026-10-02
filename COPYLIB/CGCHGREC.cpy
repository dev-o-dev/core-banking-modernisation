      *================================================================*
      * COPYBOOK : CGCHGREC                                            *
      * TITLE    : PENDING CHARGE RECORD                               *
      * DATASET  : MBK.PRD.CHG.PENDING  (VSAM KSDS)                    *
      *            RECORDSIZE(250 250)  KEYS(32 0)                     *
      *----------------------------------------------------------------*
      * WRITTEN BY CGCAL010 ON THE CHARGING CYCLE DAY. EACH RECORD IS  *
      * ONE CHARGE ITEM FOR ONE ACCOUNT FOR ONE CHARGING PERIOD.       *
      * CGNTF010 PRODUCES THE PRE-NOTIFICATION LETTER EXTRACT.         *
      * CGAPL010 APPLIES THE CHARGE ON THE APPLICATION DATE (CYCLE     *
      * DATE + PRD-CHG-NOTIFY-DAYS, ROLLED TO A BUSINESS DAY).         *
      * CGREF010 MAY WAIVE A CHARGE BEFORE IT IS APPLIED (STATUS W) OR *
      * REFUND IT AFTERWARDS (MBK.CHGHIST ROW UPDATED).                *
      *----------------------------------------------------------------*
      * CHARGE TYPES (CGP-CHARGE-TYPE)                                 *
      *   MFEE MONTHLY ACCOUNT FEE     AODF ARRANGED OD MONTHLY FEE    *
      *   UODF UNARRANGED OD DAILY FEE UNPD UNPAID ITEM FEE            *
      *   PDIT PAID ITEM FEE           TXNF TRANSACTION FEE            *
      *   DINT DEBIT INTEREST (INFO - POSTED BY INCAP010, NOT HERE)    *
      *   SWFT SWIFT PAYMENT FEE       CHAP CHAPS PAYMENT FEE          *
      *   RTDD RETURNED DIRECT DEBIT   STSO STOPPED STANDING ORDER     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1995-04-03 GHT  ORIGINAL VERSION (REPLACES DD0700 CHARGES)     *
      * 1998-11-23 MLW  Y2K                                            *
      * 2004-08-09 SAT  CAP / BUFFER (OFT REVIEW)                      *
      * 2009-10-01 AOS  NOTIFICATION DATE / APPLY DATE SPLIT (PSR 2009)*
      *================================================================*
       01  CGP-CHARGE-RECORD.
           05  CGP-KEY.
               10  CGP-SORT-CODE           PIC X(06).
               10  CGP-ACCT-NO             PIC X(08).
               10  CGP-PERIOD-END          PIC 9(08).
               10  CGP-CHARGE-TYPE         PIC X(04).
                   88  CGP-MONTHLY-FEE         VALUE 'MFEE'.
                   88  CGP-ARR-OD-FEE          VALUE 'AODF'.
                   88  CGP-UNARR-OD-FEE        VALUE 'UODF'.
                   88  CGP-UNPAID-ITEM         VALUE 'UNPD'.
                   88  CGP-PAID-ITEM           VALUE 'PDIT'.
                   88  CGP-TXN-FEE             VALUE 'TXNF'.
                   88  CGP-DEBIT-INT-INFO      VALUE 'DINT'.
                   88  CGP-SWIFT-FEE           VALUE 'SWFT'.
                   88  CGP-CHAPS-FEE           VALUE 'CHAP'.
                   88  CGP-RETURNED-DD         VALUE 'RTDD'.
                   88  CGP-STOPPED-SO          VALUE 'STSO'.
               10  CGP-ITEM-SEQ            PIC 9(06).
           05  CGP-CIF-NO                  PIC X(12).
           05  CGP-PROD-CD                 PIC X(10).
           05  CGP-PERIOD-START            PIC 9(08).
           05  CGP-CALC-DATE               PIC 9(08).
           05  CGP-NOTIFY-DATE             PIC 9(08).
           05  CGP-APPLY-DATE              PIC 9(08).
           05  CGP-ITEM-COUNT              PIC 9(05).
           05  CGP-UNIT-AMT                PIC S9(07)V99 COMP-3.
           05  CGP-GROSS-AMT               PIC S9(13)V99 COMP-3.
           05  CGP-CAP-REDUCTION           PIC S9(13)V99 COMP-3.
           05  CGP-WAIVED-AMT              PIC S9(13)V99 COMP-3.
           05  CGP-NET-AMT                 PIC S9(13)V99 COMP-3.
           05  CGP-STATUS                  PIC X(01).
               88  CGP-CALCULATED              VALUE 'C'.
               88  CGP-NOTIFIED                VALUE 'N'.
               88  CGP-APPLIED                 VALUE 'A'.
               88  CGP-WAIVED                  VALUE 'W'.
               88  CGP-SUPPRESSED              VALUE 'S'.
               88  CGP-FAILED                  VALUE 'F'.
           05  CGP-SUPPRESS-RSN            PIC X(04).
           05  CGP-WAIVE-USER              PIC X(08).
           05  CGP-WAIVE-REASON            PIC X(30).
           05  CGP-DESCRIPTION             PIC X(35).
           05  CGP-POST-EXT-REF            PIC X(24).
           05  FILLER                      PIC X(20).
