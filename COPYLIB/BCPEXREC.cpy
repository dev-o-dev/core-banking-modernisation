      *================================================================*
      * COPYBOOK : BCPEXREC                                            *
      * TITLE    : BUSINESS CONTINUITY ACCOUNT EXTRACT                 *
      * DATASET  : MBK.PRD.BCP.ACCTEXT(+1)  RECFM=FB LRECL=300         *
      *            WRITTEN BY BCPEXT10 (JOB BCPEXT01) EVERY NIGHT.     *
      *            TRANSMITTED BY CONNECT:DIRECT TO THE DR SITE        *
      *            (THATCHAM) AND SPLIT BY REGION FOR THE BRANCH       *
      *            CONTINGENCY PCS (BCPVIEW).                          *
      *----------------------------------------------------------------*
      * ALL AMOUNTS ARE ZONED DECIMAL WITH SIGN LEADING SEPARATE SO    *
      * THAT THE PC SOFTWARE CAN READ THE FILE AFTER ASCII CONVERSION. *
      * NO PACKED FIELDS MAY BE ADDED TO THIS RECORD.                  *
      *                                                                *
      * RECORD TYPES: H HEADER (ONE)  D DETAIL  T TRAILER (ONE)        *
      * THE TRAILER CARRIES HASH TOTALS WHICH BCPVIEW RECOMPUTES ON    *
      * LOAD. A MISMATCH MAKES BCPVIEW REFUSE THE FILE AND KEEP THE    *
      * PREVIOUS NIGHT'S DATA.                                         *
      *----------------------------------------------------------------*
      * SERVE INDICATOR (BCX-SERVE-IND / BCX-SERVE-RSN):               *
      *   Y  BRANCH MAY PAY OUT UP TO BCX-CONTINGENCY-LIMIT            *
      *   N  REFER - REASON IN BCX-SERVE-RSN (FRZN, NODR, DCSD, BLKD,  *
      *      KYCF, CLSP, DORM, NOFN)                                   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1998-06-15 MLW  ORIGINAL VERSION (Y2K CONTINGENCY PROGRAMME)   *
      * 2001-04-30 DKP  IBAN ADDED                                     *
      * 2009-03-02 PDQ  CONTINGENCY LIMIT AND SERVE INDICATOR          *
      * 2013-05-20 KLM  DATE OF BIRTH FOR ID CHECKS (BCP TEST 2012)    *
      *================================================================*
       01  BCX-DETAIL-RECORD.
           05  BCX-REC-TYPE                PIC X(01).
               88  BCX-HEADER                  VALUE 'H'.
               88  BCX-DETAIL                  VALUE 'D'.
               88  BCX-TRAILER                 VALUE 'T'.
           05  BCX-SORT-CODE               PIC X(06).
           05  BCX-ACCT-NO                 PIC X(08).
           05  BCX-CIF-NO                  PIC X(12).
           05  BCX-ACCT-TYPE-CD            PIC X(04).
           05  BCX-PROD-CD                 PIC X(10).
           05  BCX-CCY-CD                  PIC X(03).
           05  BCX-ACCT-STATUS             PIC X(01).
           05  BCX-POST-RESTRICT           PIC X(01).
           05  BCX-CUST-NAME               PIC X(40).
           05  BCX-POST-CD                 PIC X(10).
           05  BCX-DOB                     PIC 9(08).
           05  BCX-CUST-TYPE               PIC X(01).
           05  BCX-CIF-STATUS              PIC X(01).
           05  BCX-LEDGER-BAL              PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  BCX-AVAIL-BAL               PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  BCX-UNCLEARED-AMT           PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  BCX-HOLD-AMT                PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  BCX-OD-LIMIT                PIC 9(13)V99.
           05  BCX-OD-EXPIRY-DATE          PIC 9(08).
           05  BCX-SPENDING-POWER          PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  BCX-CONTINGENCY-LIMIT       PIC 9(07)V99.
           05  BCX-SERVE-IND               PIC X(01).
               88  BCX-MAY-SERVE               VALUE 'Y'.
               88  BCX-REFER                   VALUE 'N'.
           05  BCX-SERVE-RSN               PIC X(04).
           05  BCX-LAST-TXN-DATE           PIC 9(08).
           05  BCX-MATURITY-DATE           PIC 9(08).
           05  BCX-IBAN                    PIC X(34).
           05  FILLER                      PIC X(27).
      *
       01  BCX-HEADER-RECORD REDEFINES BCX-DETAIL-RECORD.
           05  BCH-REC-TYPE                PIC X(01).
           05  BCH-FILE-ID                 PIC X(08).
           05  BCH-BUS-DATE                PIC 9(08).
           05  BCH-CREATE-DATE             PIC 9(08).
           05  BCH-CREATE-TIME             PIC 9(06).
           05  BCH-RUN-ID                  PIC X(05).
           05  BCH-SOURCE-SYS              PIC X(08).
           05  BCH-LAYOUT-VERSION          PIC X(02).
           05  FILLER                      PIC X(254).
      *
       01  BCX-TRAILER-RECORD REDEFINES BCX-DETAIL-RECORD.
           05  BCT-REC-TYPE                PIC X(01).
           05  BCT-FILE-ID                 PIC X(08).
           05  BCT-BUS-DATE                PIC 9(08).
           05  BCT-RECORD-COUNT            PIC 9(09).
           05  BCT-HASH-ACCT-NO            PIC 9(15).
           05  BCT-HASH-LEDGER             PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
           05  BCT-HASH-AVAIL              PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
           05  BCT-HASH-SPENDING           PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
           05  BCT-SERVE-N-COUNT           PIC 9(09).
           05  FILLER                      PIC X(196).
