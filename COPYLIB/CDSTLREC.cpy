      *================================================================*
      * COPYBOOK : CDSTLREC                                            *
      * TITLE    : DEBIT CARD SETTLEMENT (CLEARING) RECORD             *
      *----------------------------------------------------------------*
      * DAILY CLEARING FILE FROM THE CARD SCHEME PROCESSOR VIA         *
      * CONNECT:DIRECT:  MBK.PRD.CARD.CLEARING(0)   FB 200             *
      * ONE RECORD PER PRESENTMENT, REFUND OR CHARGEBACK. SORTED BY    *
      * ACCOUNT IN STEP CD010 OF JOB CDSTL01.                          *
      *----------------------------------------------------------------*
      * CDS-TXN-TYPE                                                   *
      *   P  PRESENTMENT (PURCHASE / CASH)     - DEBIT                 *
      *   F  REFUND FROM MERCHANT               - CREDIT               *
      *   C  CHARGEBACK WON FOR THE CARDHOLDER  - CREDIT               *
      *   R  REPRESENTMENT AFTER CHARGEBACK     - DEBIT                *
      * CDS-FX FIELDS ARE SET FOR NON-STERLING TRANSACTIONS. THE       *
      * SCHEME HAS ALREADY CONVERTED TO GBP (CDS-AMOUNT). MEGABANK     *
      * ADDS A NON-STERLING TRANSACTION FEE (PRD-NSTF-PCT) AS A        *
      * SEPARATE 'CHG ' POSTING.                                       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-08-02 JNB  ORIGINAL VERSION                               *
      *================================================================*
       01  CDS-SETTLEMENT-RECORD.
           05  CDS-REC-TYPE                PIC X(01).
               88  CDS-HEADER                  VALUE 'H'.
               88  CDS-DETAIL                  VALUE 'D'.
               88  CDS-TRAILER                 VALUE 'T'.
           05  CDS-BODY                    PIC X(199).
           05  CDS-DETAIL-DATA REDEFINES CDS-BODY.
               10  CDS-SORT-CODE           PIC X(06).
               10  CDS-ACCT-NO             PIC X(08).
               10  CDS-CARD-TOKEN          PIC X(19).
               10  CDS-TXN-TYPE            PIC X(01).
                   88  CDS-PRESENTMENT         VALUE 'P'.
                   88  CDS-REFUND              VALUE 'F'.
                   88  CDS-CHARGEBACK          VALUE 'C'.
                   88  CDS-REPRESENTMENT       VALUE 'R'.
                   88  CDS-IS-DEBIT            VALUE 'P' 'R'.
               10  CDS-AMOUNT              PIC 9(11)V99.
               10  CDS-AUTH-REF            PIC X(12).
               10  CDS-TXN-DATE            PIC 9(08).
               10  CDS-MERCHANT-NAME       PIC X(25).
               10  CDS-MERCHANT-TOWN       PIC X(13).
               10  CDS-MERCHANT-COUNTRY    PIC X(03).
               10  CDS-MCC                 PIC X(04).
               10  CDS-CASH-IND            PIC X(01).
               10  CDS-FX-CCY              PIC X(03).
               10  CDS-FX-AMOUNT           PIC 9(11)V99.
               10  CDS-FX-RATE             PIC 9(05)V9(06).
               10  CDS-SCHEME-REF          PIC X(23).
               10  FILLER                  PIC X(36).
           05  CDS-HDR-DATA REDEFINES CDS-BODY.
               10  CDSH-FILE-ID            PIC X(12).
               10  CDSH-SETTLE-DATE        PIC 9(08).
               10  FILLER                  PIC X(179).
           05  CDS-TRL-DATA REDEFINES CDS-BODY.
               10  CDST-RECORD-COUNT       PIC 9(09).
               10  CDST-DEBIT-TOTAL        PIC 9(13)V99.
               10  CDST-CREDIT-TOTAL       PIC 9(13)V99.
               10  FILLER                  PIC X(160).
