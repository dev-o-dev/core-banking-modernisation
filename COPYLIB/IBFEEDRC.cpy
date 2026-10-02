      *================================================================*
      * COPYBOOK : IBFEEDRC                                            *
      * TITLE    : IBFEED02 - HOST TO INTERNET BANKING DAILY FEED      *
      * DATASET  : MBK.PRD.IBANK.IBFEED02.OUTPUT(+1)                   *
      *            RECFM=FB LRECL=250                                  *
      * DEST     : IBANK ORACLE LOADER (SERVER IBKDB01) VIA            *
      *            CONNECT:DIRECT NODE MBK.UNIX.IBANK                  *
      *----------------------------------------------------------------*
      * RECORD TYPES                                                   *
      *   H  HEADER       - ONE PER FILE                               *
      *   B  BALANCE      - ONE PER ACCOUNT (ALL NON-CLOSED ACCOUNTS   *
      *                     PLUS ACCOUNTS CLOSED ON THE BUSINESS DATE) *
      *   T  TRANSACTION  - ONE PER POSTING HARD-POSTED TODAY,         *
      *                     IMMEDIATELY FOLLOWING ITS B RECORD         *
      *   Z  TRAILER      - COUNTS AND HASH TOTALS                     *
      *----------------------------------------------------------------*
      * NOTES FOR THE CHANNEL                                          *
      *  - SORT CODE IS 6 DIGITS WITHOUT HYPHENS. THE CHANNEL STORES   *
      *    '99-99-99' AND REFORMATS ON LOAD.                           *
      *  - IBFB-AVAIL-BAL IS SPENDING POWER (CLEARED + OD LIMIT) TO    *
      *    MATCH THE CHANNEL DEFINITION OF AVAILABLE BALANCE.          *
      *  - AMOUNTS ARE ZONED DECIMAL, LEADING SEPARATE SIGN, 2DP.      *
      *----------------------------------------------------------------*
      * 2000-06-19 DKP  ORIGINAL (IBFEED01 - BALANCES ONLY)            *
      * 2001-11-05 DKP  IBFEED02 - TRANSACTION RECORDS ADDED           *
      * 2006-03-14 AOS  NARRATIVE-2 AND CHANNEL CODE ADDED             *
      *================================================================*
       01  IBF-FEED-RECORD.
           05  IBF-REC-TYPE                PIC X(01).
               88  IBF-HEADER-REC              VALUE 'H'.
               88  IBF-BALANCE-REC             VALUE 'B'.
               88  IBF-TXN-REC                 VALUE 'T'.
               88  IBF-TRAILER-REC             VALUE 'Z'.
           05  IBF-REC-DATA                PIC X(249).
      *
           05  IBF-HEADER REDEFINES IBF-REC-DATA.
               10  IBFH-FILE-ID            PIC X(08).
               10  IBFH-BUSINESS-DATE      PIC 9(08).
               10  IBFH-CREATE-DATE        PIC 9(08).
               10  IBFH-CREATE-TIME        PIC 9(06).
               10  IBFH-FILE-SEQ-NO        PIC 9(06).
               10  IBFH-SOURCE-SYS         PIC X(08).
               10  FILLER                  PIC X(205).
      *
           05  IBF-BALANCE REDEFINES IBF-REC-DATA.
               10  IBFB-SORT-CODE          PIC X(06).
               10  IBFB-ACCT-NO            PIC X(08).
               10  IBFB-CIF-NO             PIC X(12).
               10  IBFB-ACCT-TYPE-CD       PIC X(04).
               10  IBFB-PROD-CD            PIC X(10).
               10  IBFB-CCY-CD             PIC X(03).
               10  IBFB-ACCT-STATUS        PIC X(01).
               10  IBFB-LEDGER-BAL         PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IBFB-AVAIL-BAL          PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IBFB-OD-LIMIT           PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IBFB-INT-RATE           PIC S9(03)V9(04)
                                           SIGN LEADING SEPARATE.
               10  IBFB-OPEN-DATE          PIC 9(08).
               10  IBFB-CLOSE-DATE         PIC 9(08).
               10  IBFB-MATURITY-DATE      PIC 9(08).
               10  IBFB-STMT-DAY           PIC 9(02).
               10  IBFB-IBAN               PIC X(34).
               10  FILLER                  PIC X(89).
      *
           05  IBF-TRANSACTION REDEFINES IBF-REC-DATA.
               10  IBFT-SORT-CODE          PIC X(06).
               10  IBFT-ACCT-NO            PIC X(08).
               10  IBFT-POST-DATE          PIC 9(08).
               10  IBFT-POST-SEQ           PIC 9(09).
               10  IBFT-TXN-CODE           PIC X(04).
               10  IBFT-DR-CR-IND          PIC X(01).
               10  IBFT-TXN-AMT            PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IBFT-RUNNING-BAL        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IBFT-CCY-CD             PIC X(03).
               10  IBFT-VALUE-DATE         PIC 9(08).
               10  IBFT-NARRATIVE-1        PIC X(35).
               10  IBFT-NARRATIVE-2        PIC X(35).
               10  IBFT-CHANNEL-CD         PIC X(06).
               10  IBFT-EXT-REF            PIC X(24).
               10  FILLER                  PIC X(70).
      *
           05  IBF-TRAILER REDEFINES IBF-REC-DATA.
               10  IBFZ-BAL-COUNT          PIC 9(09).
               10  IBFZ-TXN-COUNT          PIC 9(09).
               10  IBFZ-HASH-LEDGER        PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
               10  IBFZ-HASH-TXN-AMT       PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(195).
