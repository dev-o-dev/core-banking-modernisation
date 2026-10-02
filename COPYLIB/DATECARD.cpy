      *================================================================*
      * COPYBOOK : DATECARD                                            *
      * TITLE    : BUSINESS DATE CONTROL CARD                          *
      * DATASET  : MBK.PRD.CNTL(BUSDATE)   RECFM=FB LRECL=80           *
      *----------------------------------------------------------------*
      * MAINTAINED BY JOB MBKDTROL (DATE ROLL) AT THE END OF THE EOD   *
      * NETWORK. EVERY BATCH PROGRAM READS ITS BUSINESS DATE FROM THIS *
      * CARD (DD DATECARD) - NEVER FROM THE SYSTEM CLOCK. OPERATIONS   *
      * MAY OVERRIDE IT FOR RERUNS BY SUPPLYING A SYSIN DATE CARD.     *
      *                                                                *
      * EXAMPLE:                                                       *
      * BUSDATE=20100714 PREVDATE=20100713 NEXTDATE=20100715 RUN=0714A *
      *================================================================*
       01  DTC-DATE-CARD.
           05  FILLER                      PIC X(08).
               88  DTC-VALID-CARD              VALUE 'BUSDATE='.
           05  DTC-BUSINESS-DATE           PIC 9(08).
           05  FILLER                      PIC X(10).
           05  DTC-PREV-BUS-DATE           PIC 9(08).
           05  FILLER                      PIC X(10).
           05  DTC-NEXT-BUS-DATE           PIC 9(08).
           05  FILLER                      PIC X(05).
           05  DTC-RUN-ID                  PIC X(05).
           05  FILLER                      PIC X(18).
