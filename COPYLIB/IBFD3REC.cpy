      *================================================================*
      * COPYBOOK : IBFD3REC                                            *
      * TITLE    : IBFEED03 - HOST TO INTERNET BANKING TRANSACTION     *
      *            SHADOW TABLE LOAD FILE                              *
      * DATASET  : MBK.PRD.IBANK.IBFEED03.OUTPUT(+1)                   *
      *            RECFM=FB LRECL=300                                  *
      * DEST     : IBANK ORACLE LOADER (SERVER IBKDB01) VIA            *
      *            CONNECT:DIRECT NODE MBK.UNIX.IBANK - LOADS TABLE    *
      *            IBK_TXN_SHADOW (TRANSACTION SEARCH / CATEGORIES)    *
      *----------------------------------------------------------------*
      * COMPANION TO IBFEED02 (IBFEEDRC). IBFEED02 FEEDS THE BALANCE   *
      * AND STATEMENT VIEW; IBFEED03 FEEDS THE SEARCHABLE TRANSACTION  *
      * HISTORY WITH ACCOUNT ATTRIBUTES DENORMALISED ON TO EVERY ROW,  *
      * A CUSTOMER-FRIENDLY DESCRIPTION AND A SPENDING CATEGORY.       *
      *----------------------------------------------------------------*
      * RECORD TYPES                                                   *
      *   H  HEADER       - ONE PER FILE                               *
      *   T  TRANSACTION  - ONE PER MBK.TXNHIST ROW FOR THE DATE       *
      *   S  ACCOUNT SUM  - ONE AFTER EACH ACCOUNT'S T RECORDS         *
      *   Z  TRAILER      - COUNTS AND CONTROL TOTALS                  *
      *----------------------------------------------------------------*
      * NOTES FOR THE CHANNEL                                          *
      *  - SORT CODE IS 6 DIGITS WITHOUT HYPHENS (AS IBFEED02).        *
      *  - AMOUNTS ARE ZONED DECIMAL, LEADING SEPARATE SIGN, 2DP.      *
      *    IB3T-TXN-AMT IS UNSIGNED IN MEANING - USE IB3T-DR-CR-IND.   *
      *  - THE LOADER REJECTS THE FILE IF IB3Z TOTALS DO NOT AGREE     *
      *    WITH THE T RECORDS, OR IF IB3H-FILE-SEQ-NO IS NOT HIGHER    *
      *    THAN THE LAST FILE LOADED.                                  *
      *  - IB3S-BAL-CHECK 'N' = HOST RUNNING BALANCE CHAIN DID NOT     *
      *    AGREE WITH THE LEDGER. THE CHANNEL SHOWS THE TRANSACTIONS   *
      *    BUT HIDES THE RUNNING BALANCE COLUMN FOR THAT ACCOUNT/DAY.  *
      *----------------------------------------------------------------*
      * 2014-05-12 SJW  ORIGINAL VERSION (IBANK TRANSACTION SEARCH)    *
      * 2015-10-05 SJW  SPENDING CATEGORY ADDED (IB3T-CATEGORY)        *
      *================================================================*
       01  IB3-FEED-RECORD.
           05  IB3-REC-TYPE                PIC X(01).
               88  IB3-HEADER-REC              VALUE 'H'.
               88  IB3-TXN-REC                 VALUE 'T'.
               88  IB3-SUMMARY-REC             VALUE 'S'.
               88  IB3-TRAILER-REC             VALUE 'Z'.
           05  IB3-REC-DATA                PIC X(299).
      *
           05  IB3-HEADER REDEFINES IB3-REC-DATA.
               10  IB3H-FILE-ID            PIC X(08).
               10  IB3H-BUSINESS-DATE      PIC 9(08).
               10  IB3H-CREATE-DATE        PIC 9(08).
               10  IB3H-CREATE-TIME        PIC 9(06).
               10  IB3H-FILE-SEQ-NO        PIC 9(06).
               10  IB3H-SOURCE-SYS         PIC X(08).
               10  IB3H-TARGET-TABLE       PIC X(18).
               10  FILLER                  PIC X(237).
      *
           05  IB3-TRANSACTION REDEFINES IB3-REC-DATA.
               10  IB3T-SORT-CODE          PIC X(06).
               10  IB3T-ACCT-NO            PIC X(08).
               10  IB3T-CIF-NO             PIC X(12).
               10  IB3T-ACCT-TYPE-CD       PIC X(04).
               10  IB3T-PROD-CD            PIC X(10).
               10  IB3T-POST-DATE          PIC 9(08).
               10  IB3T-POST-SEQ           PIC 9(09).
               10  IB3T-TXN-CODE           PIC X(04).
               10  IB3T-TXN-DESC           PIC X(20).
               10  IB3T-CATEGORY           PIC X(04).
               10  IB3T-DR-CR-IND          PIC X(01).
               10  IB3T-TXN-AMT            PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IB3T-RUNNING-BAL        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IB3T-CCY-CD             PIC X(03).
               10  IB3T-VALUE-DATE         PIC 9(08).
               10  IB3T-NARRATIVE-1        PIC X(35).
               10  IB3T-NARRATIVE-2        PIC X(35).
               10  IB3T-CHANNEL-CD         PIC X(06).
               10  IB3T-EXT-REF            PIC X(24).
               10  IB3T-POST-TYPE          PIC X(01).
               10  IB3T-BATCH-RUN-ID       PIC X(08).
               10  IB3T-INSERT-TS          PIC X(26).
               10  FILLER                  PIC X(35).
      *
           05  IB3-SUMMARY REDEFINES IB3-REC-DATA.
               10  IB3S-SORT-CODE          PIC X(06).
               10  IB3S-ACCT-NO            PIC X(08).
               10  IB3S-TXN-COUNT          PIC 9(07).
               10  IB3S-TOTAL-DR           PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IB3S-TOTAL-CR           PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IB3S-OPENING-BAL        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IB3S-CLOSING-BAL        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IB3S-LEDGER-BAL         PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  IB3S-BAL-CHECK          PIC X(01).
                   88  IB3S-BAL-AGREED         VALUE 'Y'.
                   88  IB3S-BAL-DIFFERENT      VALUE 'N'.
               10  FILLER                  PIC X(197).
      *
           05  IB3-TRAILER REDEFINES IB3-REC-DATA.
               10  IB3Z-ACCT-COUNT         PIC 9(09).
               10  IB3Z-TXN-COUNT          PIC 9(09).
               10  IB3Z-TOTAL-DR           PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
               10  IB3Z-TOTAL-CR           PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
               10  IB3Z-HASH-ACCT-NO       PIC 9(18).
               10  IB3Z-RECORD-COUNT       PIC 9(09).
               10  FILLER                  PIC X(218).
