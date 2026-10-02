      *================================================================*
      * COPYBOOK : PYFPSMSG                                            *
      * TITLE    : PAYMENTS GATEWAY MESSAGES (MQ)                      *
      *            GATEWAY INTERFACE SPEC PGW-IF-02 V1.4               *
      *----------------------------------------------------------------*
      * QUEUES                                                         *
      *   MBK.FPS.OUTBOUND     HOST -> FPS GATEWAY    (FPM-REQUEST)    *
      *   MBK.CHAPS.OUTBOUND   HOST -> CHAPS GATEWAY  (FPM-REQUEST)    *
      *   MBK.FPS.RESPONSE     FPS GATEWAY -> HOST    (FPR-RESPONSE)   *
      *                        TRIGGERED -> TRANSACTION PYFR           *
      * MESSAGES ARE MQFMT_STRING, CCSID 500. PERSISTENT.              *
      *----------------------------------------------------------------*
      * 2008-05-27 AOS  ORIGINAL (FASTER PAYMENTS GO-LIVE)             *
      * 2009-04-06 AOS  CHAPS ROUTED THROUGH THE SAME GATEWAY          *
      *================================================================*
       01  FPM-REQUEST.
           05  FPM-HEADER.
               10  FPM-MSG-TYPE            PIC X(04).
                   88  FPM-SINGLE-IMMEDIATE    VALUE 'SIP '.
                   88  FPM-CHAPS-PAYMENT       VALUE 'CHP '.
               10  FPM-VERSION             PIC X(02).
               10  FPM-MSG-ID              PIC X(16).
               10  FPM-CREATE-TS           PIC X(26).
           05  FPM-PAYMENT.
               10  FPM-ORIG-SORT-CODE      PIC X(06).
               10  FPM-ORIG-ACCT-NO        PIC X(08).
               10  FPM-ORIG-NAME           PIC X(35).
               10  FPM-BENE-SORT-CODE      PIC X(06).
               10  FPM-BENE-ACCT-NO        PIC X(08).
               10  FPM-BENE-NAME           PIC X(35).
               10  FPM-AMOUNT              PIC 9(11)V99.
               10  FPM-CCY-CD              PIC X(03).
               10  FPM-REFERENCE           PIC X(18).
               10  FPM-SETTLEMENT-DATE     PIC 9(08).
           05  FILLER                      PIC X(112).
      *
       01  FPR-RESPONSE.
           05  FPR-HEADER.
               10  FPR-MSG-TYPE            PIC X(04).
                   88  FPR-SIP-RESPONSE        VALUE 'SIPR'.
               10  FPR-VERSION             PIC X(02).
               10  FPR-MSG-ID              PIC X(16).
               10  FPR-RESPONSE-TS         PIC X(26).
           05  FPR-RESULT.
               10  FPR-STATUS              PIC X(04).
                   88  FPR-ACCEPTED            VALUE 'ACSC'.
                   88  FPR-REJECTED            VALUE 'RJCT'.
               10  FPR-REJECT-RSN          PIC X(04).
               10  FPR-FPS-TXN-ID          PIC X(35).
           05  FILLER                      PIC X(109).
