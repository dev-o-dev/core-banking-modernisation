      *================================================================*
      * COPYBOOK : DDACCREC                                            *
      * TITLE    : DEPOSIT ACCOUNT (DDA) MASTER RECORD                 *
      * DATASET  : MBK.PRD.DDA.ACCTMAST  (VSAM KSDS)                   *
      *            RECORDSIZE(400 400)  KEYS(14 0)                     *
      *            AIX MBK.PRD.DDA.ACCTMAST.CIFAIX  KEYS(12 14)        *
      *            NONUNIQUEKEY  PATH MBK.PRD.DDA.ACCTMAST.CIFPATH     *
      * CICS FILE: ACCTMST (BASE)  ACCTCIF (PATH VIA CIF NUMBER)       *
      *----------------------------------------------------------------*
      * BALANCE DEFINITIONS (SEE DDA OPERATIONS MANUAL SECTION 4)      *
      *   LEDGER-BAL   BOOK BALANCE AS AT LAST HARD POST (EOD)         *
      *   AVAIL-BAL    CLEARED FUNDS = LEDGER - UNCLEARED - HOLDS      *
      *                + MEMO CREDITS - MEMO DEBITS.                   *
      *                *** EXCLUDES OVERDRAFT LIMIT ***                *
      *   SPENDING POWER (NOT STORED) = AVAIL-BAL + OD-LIMIT           *
      *   NB. THE IBANK CHANNEL SHOWS SPENDING POWER AS "AVAILABLE";   *
      *       IBFEED02 ADDS THE OD LIMIT WHEN BUILDING THE FEED.       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1987-10-05 RJH  ORIGINAL VERSION                               *
      * 1994-01-17 GHT  MEMO POSTING TOTALS ADDED (ONLINE MEMO POST)   *
      * 1998-11-23 MLW  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-04-30 DKP  IBAN ADDED (EU REG 2560/2001)                  *
      * 2005-09-12 AOS  CIF NUMBER MOVED TO OFFSET 14 FOR AIX          *
      * 2010-03-08 AOS  ACM-EXTENSION-2010 CARVED FROM FILLER FOR THE  *
      *                 INTEREST, CHARGES, DORMANCY AND CLOSURE BATCH  *
      *                 (CR 10/0141). RECORD LENGTH UNCHANGED.         *
      * 2010-11-01 JNB  MATURITY INSTRUCTION / NOTICE DATE (INFTD010)  *
      *================================================================*
       01  ACM-ACCOUNT-RECORD.
           05  ACM-KEY.
               10  ACM-SORT-CODE           PIC X(06).
               10  ACM-ACCT-NO             PIC X(08).
           05  ACM-CIF-NO                  PIC X(12).
           05  ACM-IBAN                    PIC X(34).
           05  ACM-ACCT-TYPE-CD            PIC X(04).
               88  ACM-CURRENT                 VALUE 'CURR'.
               88  ACM-SAVINGS                 VALUE 'SAVE'.
               88  ACM-ISA                     VALUE 'ISA '.
               88  ACM-FIXED-TERM              VALUE 'FTD '.
               88  ACM-LOAN                    VALUE 'LOAN'.
               88  ACM-MORTGAGE                VALUE 'MORT'.
           05  ACM-PROD-CD                 PIC X(10).
           05  ACM-CCY-CD                  PIC X(03).
           05  ACM-ACCT-STATUS             PIC X(01).
               88  ACM-OPEN                    VALUE 'O'.
               88  ACM-CLOSED                  VALUE 'C'.
               88  ACM-DORMANT                 VALUE 'D'.
               88  ACM-FROZEN                  VALUE 'F'.
               88  ACM-PENDING-CLOSE           VALUE 'P'.
           05  ACM-BALANCES.
               10  ACM-LEDGER-BAL          PIC S9(13)V99 COMP-3.
               10  ACM-AVAIL-BAL           PIC S9(13)V99 COMP-3.
               10  ACM-UNCLEARED-AMT       PIC S9(13)V99 COMP-3.
               10  ACM-MEMO-DR-TOT         PIC S9(13)V99 COMP-3.
               10  ACM-MEMO-CR-TOT         PIC S9(13)V99 COMP-3.
               10  ACM-HOLD-AMT            PIC S9(13)V99 COMP-3.
           05  ACM-LIMITS.
               10  ACM-OD-LIMIT            PIC S9(13)V99 COMP-3.
               10  ACM-OD-EXPIRY-DATE      PIC 9(08).
           05  ACM-INT-RATE-CR             PIC S9(03)V9(04) COMP-3.
           05  ACM-INT-RATE-DR             PIC S9(03)V9(04) COMP-3.
           05  ACM-ACCRUED-INT-CR          PIC S9(13)V99 COMP-3.
           05  ACM-ACCRUED-INT-DR          PIC S9(13)V99 COMP-3.
           05  ACM-DATES.
               10  ACM-OPEN-DATE           PIC 9(08).
               10  ACM-CLOSE-DATE          PIC 9(08).
               10  ACM-MATURITY-DATE       PIC 9(08).
               10  ACM-LAST-TXN-DATE       PIC 9(08).
               10  ACM-LAST-STMT-DATE      PIC 9(08).
           05  ACM-STMT-DAY                PIC 9(02).
           05  ACM-STMT-CYCLE              PIC X(01).
               88  ACM-STMT-MONTHLY            VALUE 'M'.
               88  ACM-STMT-QUARTERLY          VALUE 'Q'.
               88  ACM-STMT-ANNUAL             VALUE 'A'.
           05  ACM-LAST-STMT-NO            PIC 9(05).
           05  ACM-POST-RESTRICT           PIC X(01).
               88  ACM-NO-RESTRICT             VALUE ' '.
               88  ACM-NO-DEBITS               VALUE 'D'.
               88  ACM-NO-CREDITS              VALUE 'C'.
               88  ACM-NO-POSTINGS             VALUE 'A'.
           05  ACM-DORMANCY-IND            PIC X(01).
           05  ACM-TXN-SEQ-NO              PIC 9(09) COMP.
           05  ACM-LAST-MAINT-DATE         PIC 9(08).
           05  ACM-LAST-MAINT-TIME         PIC 9(06).
           05  ACM-LAST-MAINT-USER         PIC X(08).
      *---- 2010 EXTENSION (WAS FILLER) -------------------------------*
           05  ACM-EXTENSION-2010.
      *        SUB-PENNY ACCRUAL CARRIED FORWARD BY INACR010
               10  ACM-ACCR-CR-RESIDUE     PIC S9(01)V9(05) COMP-3.
               10  ACM-ACCR-DR-RESIDUE     PIC S9(01)V9(05) COMP-3.
               10  ACM-LAST-ACCRUAL-DATE   PIC 9(08).
               10  ACM-LAST-CR-CAP-DATE    PIC 9(08).
               10  ACM-LAST-DR-CAP-DATE    PIC 9(08).
               10  ACM-TAX-STATUS          PIC X(01).
                   88  ACM-TAX-NET             VALUE ' ' 'N'.
                   88  ACM-TAX-GROSS-R85       VALUE 'G'.
                   88  ACM-TAX-NON-RESIDENT    VALUE 'X'.
               10  ACM-BONUS-END-DATE      PIC 9(08).
               10  ACM-ACCR-DR-UNARR       PIC S9(13)V99 COMP-3.
      *        CHARGING PERIOD COUNTERS - RESET BY CGCAL010
               10  ACM-CHG-PERIOD.
                   15  ACM-UNARR-OD-DAYS   PIC 9(03).
                   15  ACM-UNPAID-ITEMS    PIC 9(03).
                   15  ACM-PAID-ITEMS      PIC 9(03).
                   15  ACM-CHG-PERIOD-START
                                           PIC 9(08).
                   15  ACM-CHG-WAIVE-IND   PIC X(01).
                       88  ACM-CHARGES-WAIVED  VALUE 'Y'.
      *        LAST CUSTOMER-INITIATED TRANSACTION (NOT INT/CHG) -
      *        SET BY PSEOD010, USED BY DDDRM010 FOR DORMANCY
               10  ACM-LAST-CUST-TXN-DATE  PIC 9(08).
               10  ACM-DORMANT-DATE        PIC 9(08).
      *        CLOSURE REQUEST - SET BY DDCLS01, ACTIONED BY DDCLS010
               10  ACM-CLOSE-REQUEST.
                   15  ACM-CLOSE-REQ-DATE  PIC 9(08).
                   15  ACM-CLOSE-XFER-SORT-CODE
                                           PIC X(06).
                   15  ACM-CLOSE-XFER-ACCT-NO
                                           PIC X(08).
                   15  ACM-CLOSE-RSN       PIC X(04).
               10  ACM-ISA-SUBS-YTD        PIC S9(13)V99 COMP-3.
      *        FIXED TERM MATURITY - SET BY DDOPN01 / BRANCH, ACTIONED
      *        BY INFTD010. PAY-AWAY USES THE ACM-CLOSE-XFER FIELDS.
               10  ACM-MATURITY-INSTR      PIC X(01).
                   88  ACM-MAT-RENEW           VALUE 'R'.
                   88  ACM-MAT-PAY-AWAY        VALUE 'P'.
                   88  ACM-MAT-NONE            VALUE ' ' 'N'.
               10  ACM-MATURITY-NOTICE-DATE
                                           PIC 9(08).
           05  FILLER                      PIC X(32).
