      *================================================================*
      * COPYBOOK : SVSWPREC                                            *
      * TITLE    : SAVINGS SWEEP INSTRUCTION RECORD                    *
      * DATASET  : MBK.PRD.SAV.SWEEP  (VSAM KSDS)                      *
      *            RECORDSIZE(150 150)  KEYS(14 0)                     *
      * CICS FILE: SAVSWP (SET UP BY THE BRANCH TELLER SYSTEM AND      *
      *            IBANK 'SAVINGS SWEEP' VIA THE MEMOPOST GATEWAY)     *
      *----------------------------------------------------------------*
      * ONE RECORD PER CURRENT ACCOUNT THAT HAS OPTED IN TO THE        *
      * NIGHTLY SAVINGS SWEEP. THE KEY IS THE CURRENT ACCOUNT.         *
      * SVSWP010 (JOB SVSWP01) SWEEPS:                                 *
      *   OUT - CLEARED FUNDS ABOVE SWP-THRESHOLD TO THE NOMINATED     *
      *         MEGABANK SAVINGS ACCOUNT (IN MULTIPLES OF              *
      *         SWP-SWEEP-UNIT, AT LEAST SWP-MIN-SWEEP-AMT)            *
      *   BACK- WHEN THE CURRENT ACCOUNT WOULD FALL BELOW              *
      *         SWP-FLOOR-BAL, ENOUGH TO RESTORE SWP-RETURN-TARGET     *
      *         (LIMITED BY THE SAVINGS AVAILABLE BALANCE)             *
      * AMOUNTS ARE STERLING, 2DP. THE SAVINGS ACCOUNT MUST BELONG TO  *
      * THE SAME CIF (SWP-CIF-NO) AND MAY NOT BE AN ISA, FIXED TERM    *
      * OR NOTICE ACCOUNT (THE TELLER SCREEN ENFORCES THIS; SVSWP010   *
      * CHECKS AGAIN).                                                 *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2015-03-02 RKM  ORIGINAL VERSION (SWEEP PROPOSITION CR 15/0022)*
      * 2015-11-16 RKM  SWP-FAIL-COUNT / AUTO SUSPEND AFTER 5 FAILURES *
      *================================================================*
       01  SWP-SWEEP-RECORD.
           05  SWP-KEY.
               10  SWP-SORT-CODE           PIC X(06).
               10  SWP-ACCT-NO             PIC X(08).
           05  SWP-CIF-NO                  PIC X(12).
           05  SWP-SAVINGS-ACCOUNT.
               10  SWP-SAV-SORT-CODE       PIC X(06).
               10  SWP-SAV-ACCT-NO         PIC X(08).
           05  SWP-STATUS                  PIC X(01).
               88  SWP-ACTIVE                  VALUE 'A'.
               88  SWP-SUSPENDED               VALUE 'S'.
               88  SWP-CANCELLED               VALUE 'C'.
           05  SWP-DIRECTION               PIC X(01).
               88  SWP-BOTH-WAYS               VALUE 'B'.
               88  SWP-OUT-ONLY                VALUE 'O'.
               88  SWP-BACK-ONLY               VALUE 'I'.
               88  SWP-SWEEPS-OUT              VALUE 'B' 'O'.
               88  SWP-SWEEPS-BACK             VALUE 'B' 'I'.
           05  SWP-THRESHOLD               PIC S9(13)V99 COMP-3.
           05  SWP-MIN-SWEEP-AMT           PIC S9(13)V99 COMP-3.
           05  SWP-SWEEP-UNIT              PIC S9(07)V99 COMP-3.
           05  SWP-FLOOR-BAL               PIC S9(13)V99 COMP-3.
           05  SWP-RETURN-TARGET           PIC S9(13)V99 COMP-3.
           05  SWP-START-DATE              PIC 9(08).
           05  SWP-END-DATE                PIC 9(08).
           05  SWP-CREATE-CHANNEL          PIC X(06).
           05  SWP-CREATE-USER             PIC X(08).
           05  SWP-LAST-SWEEP.
               10  SWP-LAST-SWEEP-DATE     PIC 9(08).
               10  SWP-LAST-SWEEP-DIR      PIC X(01).
                   88  SWP-LAST-WAS-OUT        VALUE 'O'.
                   88  SWP-LAST-WAS-BACK       VALUE 'I'.
               10  SWP-LAST-SWEEP-AMT      PIC S9(13)V99 COMP-3.
           05  SWP-FAIL-COUNT              PIC 9(03).
           05  SWP-LAST-FAIL-RSN           PIC X(04).
           05  SWP-LAST-MAINT-DATE         PIC 9(08).
           05  SWP-LAST-MAINT-USER         PIC X(08).
           05  FILLER                      PIC X(01).
