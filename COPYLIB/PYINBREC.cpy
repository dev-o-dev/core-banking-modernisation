      *================================================================*
      * COPYBOOK : PYINBREC                                            *
      * TITLE    : INBOUND PAYMENTS FILE (CREDITS RECEIVED)            *
      * DATASETS : MBK.PRD.PAY.INBOUND.FPS(0)    FPS GATEWAY SETTLED   *
      *                                          CREDITS (4 CYCLES/DAY *
      *                                          CONCATENATED AT EOD)  *
      *            MBK.PRD.PAY.INBOUND.BACS(0)   BACS INPUT (ARUCS     *
      *                                          REFORMATTED BY PTX)   *
      *            MBK.PRD.PAY.RETURNS(+1)       UNAPPLIED CREDITS FOR *
      *                                          RETURN TO ORIGINATOR  *
      *            RECFM=FB LRECL=200                                  *
      *----------------------------------------------------------------*
      * RETURN REASONS (PIBD-RETURN-RSN)                               *
      *   ACNF ACCOUNT NOT FOUND     ACCL ACCOUNT CLOSED               *
      *   ACBL ACCOUNT BLOCKED       CCYX CURRENCY NOT SUPPORTED       *
      *================================================================*
       01  PIB-INBOUND-RECORD.
           05  PIB-REC-TYPE                PIC X(01).
               88  PIB-HEADER-REC              VALUE 'H'.
               88  PIB-DETAIL-REC              VALUE 'D'.
               88  PIB-TRAILER-REC             VALUE 'T'.
           05  PIB-REC-DATA                PIC X(199).
      *
           05  PIB-HEADER REDEFINES PIB-REC-DATA.
               10  PIBH-FILE-ID            PIC X(08).
               10  PIBH-SCHEME             PIC X(04).
               10  PIBH-BUSINESS-DATE      PIC 9(08).
               10  PIBH-FILE-SEQ-NO        PIC 9(06).
               10  FILLER                  PIC X(173).
      *
           05  PIB-DETAIL REDEFINES PIB-REC-DATA.
               10  PIBD-SCHEME             PIC X(04).
               10  PIBD-SCHEME-REF         PIC X(35).
               10  PIBD-BENE-SORT-CODE     PIC X(06).
               10  PIBD-BENE-ACCT-NO       PIC X(08).
               10  PIBD-AMOUNT             PIC 9(11)V99.
               10  PIBD-CCY-CD             PIC X(03).
               10  PIBD-VALUE-DATE         PIC 9(08).
               10  PIBD-ORIG-NAME          PIC X(18).
               10  PIBD-ORIG-SORT-CODE     PIC X(06).
               10  PIBD-ORIG-ACCT-NO       PIC X(08).
               10  PIBD-REFERENCE          PIC X(18).
               10  PIBD-BACS-TXN-CD        PIC X(02).
               10  PIBD-RETURN-RSN         PIC X(04).
               10  FILLER                  PIC X(66).
      *
           05  PIB-TRAILER REDEFINES PIB-REC-DATA.
               10  PIBT-REC-COUNT          PIC 9(09).
               10  PIBT-TOTAL-AMT          PIC 9(13)V99.
               10  FILLER                  PIC X(175).
