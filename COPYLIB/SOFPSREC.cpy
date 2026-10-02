      *================================================================*
      * COPYBOOK : SOFPSREC                                            *
      * TITLE    : FASTER PAYMENTS STANDING ORDER SUBMISSION FILE      *
      * DATASET  : MBK.PRD.SO.FPSOUT(+1)  RECFM=FB LRECL=250           *
      *            COLLECTED BY THE PAYMENTS GATEWAY (FILE ADAPTER     *
      *            PGW-FA-02) AND SUBMITTED TO FPS AS FORWARD-DATED    *
      *            STANDING ORDER PAYMENTS IN THE FIRST CYCLE OF THE   *
      *            VALUE DATE.                                         *
      *----------------------------------------------------------------*
      * GATEWAY RETURNS AN ACK/NAK FILE (MBK.PRD.SO.FPSACK) WHICH IS   *
      * NOT YET PROCESSED BY THE HOST - REJECTED SO PAYMENTS ARE       *
      * RETURNED AS INBOUND CREDITS (PYINB010) WITH A RETURN REFERENCE *
      * (PIR 2008/201 - OPEN).                                         *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2008-05-27 AOS  ORIGINAL VERSION (FPS STANDING ORDERS)         *
      *================================================================*
       01  SOF-FPS-RECORD.
           05  SOF-REC-TYPE                PIC X(01).
               88  SOF-HEADER                  VALUE 'H'.
               88  SOF-PAYMENT                 VALUE 'P'.
               88  SOF-TRAILER                 VALUE 'T'.
           05  SOF-BODY                    PIC X(249).
           05  SOF-HDR REDEFINES SOF-BODY.
               10  SOFH-FILE-ID            PIC X(12).
               10  SOFH-CREATE-DATE        PIC 9(08).
               10  SOFH-VALUE-DATE         PIC 9(08).
               10  SOFH-SUBMITTER-ID       PIC X(08).
               10  FILLER                  PIC X(213).
           05  SOF-PAY REDEFINES SOF-BODY.
               10  SOFP-HOST-PAY-ID        PIC X(16).
               10  SOFP-DR-SORT-CODE       PIC X(06).
               10  SOFP-DR-ACCT-NO         PIC X(08).
               10  SOFP-DR-NAME            PIC X(18).
               10  SOFP-CR-SORT-CODE       PIC X(06).
               10  SOFP-CR-ACCT-NO         PIC X(08).
               10  SOFP-CR-NAME            PIC X(35).
               10  SOFP-AMOUNT             PIC 9(11)V99.
               10  SOFP-REFERENCE          PIC X(18).
               10  SOFP-VALUE-DATE         PIC 9(08).
               10  SOFP-SO-KEY             PIC X(18).
               10  FILLER                  PIC X(95).
           05  SOF-TRL REDEFINES SOF-BODY.
               10  SOFT-PAY-COUNT          PIC 9(09).
               10  SOFT-PAY-TOTAL          PIC 9(13)V99.
               10  FILLER                  PIC X(225).
