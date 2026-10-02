      *================================================================*
      * COPYBOOK : PRDPARRC                                            *
      * TITLE    : PRODUCT PARAMETER RECORD                            *
      * DATASET  : MBK.PRD.PRD.PARMS  (VSAM KSDS)                      *
      *            RECORDSIZE(1000 1000)  KEYS(10 0)                   *
      * CICS FILE: PRDPARM (READ ONLY ONLINE)                          *
      *----------------------------------------------------------------*
      * ONE RECORD PER PRODUCT CODE (ACM-PROD-CD). HOLDS THE INTEREST, *
      * CHARGING, DORMANCY AND STATEMENT RULES FOR THE PRODUCT.        *
      * MAINTAINED ONLY BY BATCH PROGRAM PRMNT010 FROM CHANGE CARDS    *
      * RAISED BY PRODUCT CONTROL (FORM PC/17). THERE IS NO ONLINE     *
      * MAINTENANCE - SEE PIR 2007/066.                                *
      *                                                                *
      * RATES ARE HELD AS PERCENTAGES PER ANNUM, 4 DECIMAL PLACES      *
      * (E.G. +001.2500 = 1.25% P.A.).                                 *
      *                                                                *
      * INTEREST TIERS: UP TO 5 BALANCE TIERS. A TIER APPLIES TO THE   *
      * WHOLE BALANCE IF PRD-TIER-METHOD = 'W' (WHOLE BALANCE), OR TO  *
      * THE SLICE OF BALANCE WITHIN THE TIER IF 'B' (BANDED). UNUSED   *
      * TIERS HAVE A THRESHOLD OF ZERO. TIERS MUST BE IN ASCENDING     *
      * THRESHOLD ORDER - PRMNT010 CHECKS THIS.                        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1991-05-20 RJH  ORIGINAL VERSION (REPLACES DD0100 RATE TABLE)  *
      * 1998-11-23 MLW  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-05 DKP  CHANNEL OPEN INDICATORS FOR IBANK              *
      * 2004-08-09 SAT  UNARRANGED OVERDRAFT BUFFER AND MONTHLY CAP    *
      *                 (OFT UNARRANGED OD CHARGES REVIEW)             *
      * 2008-09-15 JNB  DORMANCY / RECLAIM FUND FIELDS (DORMANT BANK   *
      *                 AND BUILDING SOCIETY ACCOUNTS ACT 2008)        *
      * 2009-10-01 AOS  CHARGE PRE-NOTIFICATION DAYS (PSR 2009)        *
      * 2010-08-02 JNB  DEBIT CARD FEES (NON-STERLING / CASH)          *
      *================================================================*
       01  PRD-PARAM-RECORD.
           05  PRD-KEY.
               10  PRD-PROD-CD             PIC X(10).
           05  PRD-DESCRIPTION             PIC X(40).
           05  PRD-SHORT-NAME              PIC X(18).
           05  PRD-ACCT-TYPE-CD            PIC X(04).
               88  PRD-CURRENT                 VALUE 'CURR'.
               88  PRD-SAVINGS                 VALUE 'SAVE'.
               88  PRD-ISA                     VALUE 'ISA '.
               88  PRD-FIXED-TERM              VALUE 'FTD '.
               88  PRD-LOAN                    VALUE 'LOAN'.
               88  PRD-MORTGAGE                VALUE 'MORT'.
           05  PRD-CCY-CD                  PIC X(03).
           05  PRD-STATUS                  PIC X(01).
               88  PRD-ACTIVE                  VALUE 'A'.
               88  PRD-CLOSED-TO-NEW           VALUE 'N'.
               88  PRD-WITHDRAWN               VALUE 'W'.
           05  PRD-CUST-TYPE               PIC X(01).
               88  PRD-PERSONAL-ONLY           VALUE 'P'.
               88  PRD-BUSINESS-ONLY           VALUE 'B'.
               88  PRD-ANY-CUST-TYPE           VALUE 'A'.
           05  PRD-LAUNCH-DATE             PIC 9(08).
           05  PRD-WITHDRAW-DATE           PIC 9(08).
      *
      *---- ACCOUNT OPENING RULES -------------------------------------*
           05  PRD-OPEN-RULES.
               10  PRD-MIN-AGE             PIC 9(03).
               10  PRD-MAX-AGE             PIC 9(03).
               10  PRD-MAX-PER-CUST        PIC 9(03).
               10  PRD-MIN-OPEN-DEPOSIT    PIC S9(13)V99 COMP-3.
               10  PRD-MAX-BALANCE         PIC S9(13)V99 COMP-3.
               10  PRD-KYC-REQUIRED        PIC X(01).
               10  PRD-OPEN-BRANCH-IND     PIC X(01).
               10  PRD-OPEN-IBANK-IND      PIC X(01).
               10  PRD-OPEN-TELE-IND       PIC X(01).
               10  PRD-ACCT-NO-RANGE.
                   15  PRD-ACCT-NO-LOW     PIC 9(08).
                   15  PRD-ACCT-NO-HIGH    PIC 9(08).
               10  PRD-DEFAULT-SORT-CODE   PIC X(06).
               10  PRD-TERM-MONTHS         PIC 9(03).
               10  PRD-NOTICE-DAYS         PIC 9(03).
               10  PRD-ISA-SUBS-LIMIT      PIC S9(13)V99 COMP-3.
      *
      *---- CREDIT INTEREST -------------------------------------------*
           05  PRD-CR-INTEREST.
               10  PRD-CR-INT-IND          PIC X(01).
                   88  PRD-PAYS-CR-INT         VALUE 'Y'.
               10  PRD-TIER-METHOD         PIC X(01).
                   88  PRD-TIER-WHOLE          VALUE 'W'.
                   88  PRD-TIER-BANDED         VALUE 'B'.
               10  PRD-CR-TIER             OCCURS 5 TIMES
                                           INDEXED BY PRD-TIER-IDX.
                   15  PRD-CR-TIER-THRESHOLD
                                           PIC S9(13)V99 COMP-3.
                   15  PRD-CR-TIER-RATE    PIC S9(03)V9(04) COMP-3.
               10  PRD-CR-BONUS-RATE       PIC S9(03)V9(04) COMP-3.
               10  PRD-CR-BONUS-MONTHS     PIC 9(03).
               10  PRD-CR-MIN-BAL          PIC S9(13)V99 COMP-3.
               10  PRD-CR-DAY-BASIS        PIC 9(03).
               10  PRD-CR-CAP-FREQ         PIC X(01).
                   88  PRD-CR-CAP-MONTHLY      VALUE 'M'.
                   88  PRD-CR-CAP-QUARTERLY    VALUE 'Q'.
                   88  PRD-CR-CAP-HALF-YEARLY  VALUE 'H'.
                   88  PRD-CR-CAP-ANNUAL       VALUE 'A'.
                   88  PRD-CR-CAP-MATURITY     VALUE 'T'.
               10  PRD-CR-CAP-DAY          PIC 9(02).
               10  PRD-CR-CAP-MONTH        PIC 9(02).
               10  PRD-TAX-DEDUCT-IND      PIC X(01).
                   88  PRD-DEDUCT-TAX          VALUE 'Y'.
                   88  PRD-TAX-FREE            VALUE 'N'.
               10  PRD-CR-PENALTY-DAYS     PIC 9(03).
      *
      *---- DEBIT INTEREST --------------------------------------------*
           05  PRD-DR-INTEREST.
               10  PRD-DR-INT-IND          PIC X(01).
                   88  PRD-CHARGES-DR-INT      VALUE 'Y'.
               10  PRD-DR-RATE-ARRANGED    PIC S9(03)V9(04) COMP-3.
               10  PRD-DR-RATE-UNARRANGED  PIC S9(03)V9(04) COMP-3.
               10  PRD-DR-DAY-BASIS        PIC 9(03).
               10  PRD-DR-CAP-FREQ         PIC X(01).
                   88  PRD-DR-CAP-MONTHLY      VALUE 'M'.
                   88  PRD-DR-CAP-QUARTERLY    VALUE 'Q'.
               10  PRD-DR-CAP-DAY          PIC 9(02).
               10  PRD-DR-INT-FREE-AMT     PIC S9(13)V99 COMP-3.
               10  PRD-DR-MIN-CHARGE       PIC S9(13)V99 COMP-3.
               10  PRD-OD-MAX-LIMIT        PIC S9(13)V99 COMP-3.
      *
      *---- CHARGES ---------------------------------------------------*
           05  PRD-CHARGES.
               10  PRD-CHG-IND             PIC X(01).
                   88  PRD-LEVIES-CHARGES      VALUE 'Y'.
               10  PRD-CHG-CYCLE-DAY       PIC 9(02).
               10  PRD-CHG-NOTIFY-DAYS     PIC 9(03).
               10  PRD-MONTHLY-FEE         PIC S9(13)V99 COMP-3.
               10  PRD-FREE-BANKING-BAL    PIC S9(13)V99 COMP-3.
               10  PRD-ARR-OD-MONTHLY-FEE  PIC S9(13)V99 COMP-3.
               10  PRD-UNARR-OD-DAILY-FEE  PIC S9(13)V99 COMP-3.
               10  PRD-UNARR-OD-MAX-DAYS   PIC 9(03).
               10  PRD-UNARR-OD-BUFFER     PIC S9(13)V99 COMP-3.
               10  PRD-UNPAID-ITEM-FEE     PIC S9(13)V99 COMP-3.
               10  PRD-PAID-ITEM-FEE       PIC S9(13)V99 COMP-3.
               10  PRD-MONTHLY-CHG-CAP     PIC S9(13)V99 COMP-3.
               10  PRD-TXN-FEE-TABLE       OCCURS 6 TIMES
                                           INDEXED BY PRD-TFEE-IDX.
                   15  PRD-TFEE-TXN-CODE   PIC X(04).
                   15  PRD-TFEE-FREE-COUNT PIC 9(03).
                   15  PRD-TFEE-AMOUNT     PIC S9(07)V99 COMP-3.
               10  PRD-SWIFT-FEE           PIC S9(07)V99 COMP-3.
               10  PRD-CHAPS-FEE           PIC S9(07)V99 COMP-3.
               10  PRD-STOPPED-SO-FEE      PIC S9(07)V99 COMP-3.
               10  PRD-RETURNED-DD-FEE     PIC S9(07)V99 COMP-3.
      *
      *---- DORMANCY / UNCLAIMED ASSETS -------------------------------*
           05  PRD-DORMANCY.
               10  PRD-DORMANT-MONTHS      PIC 9(03).
               10  PRD-DORMANT-MIN-BAL     PIC S9(13)V99 COMP-3.
               10  PRD-RECLAIM-YEARS       PIC 9(02).
               10  PRD-DORMANT-LETTER-IND  PIC X(01).
      *
      *---- STATEMENTS ------------------------------------------------*
           05  PRD-STATEMENTS.
               10  PRD-DEFAULT-STMT-CYCLE  PIC X(01).
               10  PRD-DEFAULT-STMT-DAY    PIC 9(02).
               10  PRD-STMT-TEMPLATE-ID    PIC X(08).
               10  PRD-ESTMT-ALLOWED       PIC X(01).
      *
      *---- GENERAL LEDGER --------------------------------------------*
           05  PRD-GL-CODES.
               10  PRD-GL-BALANCE-CR       PIC X(10).
               10  PRD-GL-BALANCE-DR       PIC X(10).
               10  PRD-GL-INT-EXPENSE      PIC X(10).
               10  PRD-GL-INT-INCOME       PIC X(10).
               10  PRD-GL-INT-ACCR-PAY     PIC X(10).
               10  PRD-GL-INT-ACCR-REC     PIC X(10).
               10  PRD-GL-FEE-INCOME       PIC X(10).
               10  PRD-GL-TAX-PAYABLE      PIC X(10).
               10  PRD-COST-CENTRE         PIC X(06).
      *
      *---- AUDIT -----------------------------------------------------*
           05  PRD-VERSION-NO              PIC 9(05).
           05  PRD-EFFECTIVE-DATE          PIC 9(08).
           05  PRD-LAST-MAINT-DATE         PIC 9(08).
           05  PRD-LAST-MAINT-USER         PIC X(08).
           05  PRD-LAST-CHANGE-REF         PIC X(12).
      *
      *---- DEBIT CARD (2010) -----------------------------------------*
           05  PRD-CARD-FEES.
               10  PRD-NSTF-PCT            PIC S9(01)V9(04) COMP-3.
               10  PRD-NSTF-MIN            PIC S9(05)V99 COMP-3.
               10  PRD-CASH-FEE-PCT        PIC S9(01)V9(04) COMP-3.
               10  PRD-CASH-FEE-MIN        PIC S9(05)V99 COMP-3.
               10  PRD-DAILY-CASH-LIMIT    PIC S9(07)V99 COMP-3.
           05  FILLER                      PIC X(377).
