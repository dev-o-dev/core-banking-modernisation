      *================================================================*
      * COPYBOOK : INACCREC                                            *
      * TITLE    : DAILY INTEREST ACCRUAL AUDIT RECORD                 *
      * DATASET  : MBK.PRD.INT.ACCRUALS(+1)  RECFM=FB LRECL=150        *
      *            WRITTEN BY INACR010, ONE RECORD PER ACCOUNT PER     *
      *            DAY ON WHICH AN ACCRUAL WAS MADE. RETAINED 400 GENS *
      *            (GDG LIMIT 255 + MONTHLY ARCHIVE TO TAPE)           *
      *----------------------------------------------------------------*
      * USED FOR: CUSTOMER COMPLAINTS (RECALCULATION EVIDENCE), THE    *
      * MONTHLY ACCRUAL RECONCILIATION (GLREC010) AND THE ANNUAL       *
      * INTEREST CERTIFICATE CROSS-CHECK (INTAX010).                   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1992-02-10 RJH  ORIGINAL VERSION                               *
      * 1998-11-23 MLW  Y2K                                            *
      * 2004-08-09 SAT  ARRANGED / UNARRANGED DEBIT SPLIT              *
      *================================================================*
       01  IAC-ACCRUAL-RECORD.
           05  IAC-KEY.
               10  IAC-SORT-CODE           PIC X(06).
               10  IAC-ACCT-NO             PIC X(08).
               10  IAC-ACCRUAL-DATE        PIC 9(08).
           05  IAC-PROD-CD                 PIC X(10).
           05  IAC-DAYS                    PIC 9(03).
           05  IAC-BALANCE-USED            PIC S9(13)V99 COMP-3.
           05  IAC-CR-RATE                 PIC S9(03)V9(04) COMP-3.
           05  IAC-CR-ACCRUAL              PIC S9(09)V9(05) COMP-3.
           05  IAC-DR-ARR-BALANCE          PIC S9(13)V99 COMP-3.
           05  IAC-DR-ARR-RATE             PIC S9(03)V9(04) COMP-3.
           05  IAC-DR-ARR-ACCRUAL          PIC S9(09)V9(05) COMP-3.
           05  IAC-DR-UNARR-BALANCE        PIC S9(13)V99 COMP-3.
           05  IAC-DR-UNARR-RATE           PIC S9(03)V9(04) COMP-3.
           05  IAC-DR-UNARR-ACCRUAL        PIC S9(09)V9(05) COMP-3.
           05  IAC-CR-ACCRUED-TO-DATE      PIC S9(13)V99 COMP-3.
           05  IAC-DR-ACCRUED-TO-DATE      PIC S9(13)V99 COMP-3.
           05  IAC-CR-RESIDUE              PIC S9(01)V9(05) COMP-3.
           05  IAC-DR-RESIDUE              PIC S9(01)V9(05) COMP-3.
           05  IAC-TIER-USED               PIC 9(01).
           05  IAC-BONUS-IND               PIC X(01).
           05  IAC-RUN-ID                  PIC X(05).
           05  FILLER                      PIC X(24).
