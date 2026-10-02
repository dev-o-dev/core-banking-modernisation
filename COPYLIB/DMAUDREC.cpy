      *================================================================*
      * COPYBOOK : DMAUDREC                                            *
      * TITLE    : AUDDIS / ADDACS / ARUDD INTERFACE RECORDS           *
      * DATASETS : MBK.PRD.DDM.AUDDIS.IN(0)     FROM PTX - AUDDIS      *
      *                                         INSTRUCTIONS RECEIVED  *
      *            MBK.PRD.DDM.ADDACS.OUT(+1)   TO PTX - ADDACS ADVICES*
      *            MBK.PRD.DDM.ARUDD.OUT(+1)    TO PTX - UNPAID DDS    *
      *            RECFM=FB LRECL=150                                  *
      *----------------------------------------------------------------*
      * THE BACS BUREAU SOFTWARE (PTX) CONVERTS BETWEEN THESE FLAT     *
      * RECORDS AND THE BACS XML/STD18 REPORTS. LAYOUT AGREED WITH     *
      * PAYMENTS OPERATIONS - SPEC MBK-PTX-007 V3.                     *
      *----------------------------------------------------------------*
      * AUDDIS TRANSACTION CODES (DMA-TXN-CODE)                        *
      *   0N NEW INSTRUCTION     0C CANCEL INSTRUCTION                 *
      *   0S CONVERSION OF PAPER INSTRUCTION                           *
      * ADDACS REASON CODES (DMA-ADDACS-RSN)                           *
      *   0 INSTRUCTION CANCELLED - REFER TO PAYER                     *
      *   1 INSTRUCTION CANCELLED BY PAYER                             *
      *   2 PAYER DECEASED                                             *
      *   3 ACCOUNT TRANSFERRED TO NEW BANK                            *
      *   B ACCOUNT CLOSED                                             *
      *   C ACCOUNT TRANSFERRED TO A NEW BRANCH                        *
      *   D ADVANCE NOTICE DISPUTED                                    *
      *   E INSTRUCTION AMENDED                                        *
      *   R INSTRUCTION RE-INSTATED                                    *
      * ARUDD REASON CODES (DMA-ARUDD-RSN)                             *
      *   0 REFER TO PAYER       1 INSTRUCTION CANCELLED               *
      *   2 PAYER DECEASED       3 ACCOUNT TRANSFERRED                 *
      *   5 NO ACCOUNT           6 NO INSTRUCTION                      *
      *   7 AMOUNT DIFFERS       8 AMOUNT NOT YET DUE                  *
      *   9 PRESENTATION OVERDUE A ORIG SUN NOT AUTHORISED             *
      *   B ACCOUNT CLOSED                                             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2001-04-02 DKP  ORIGINAL VERSION (AUDDIS GO-LIVE)              *
      * 2003-09-08 SAT  ARUDD RECORD                                   *
      *================================================================*
       01  DMA-INTERFACE-RECORD.
           05  DMA-REC-TYPE                PIC X(01).
               88  DMA-HEADER                  VALUE 'H'.
               88  DMA-DETAIL                  VALUE 'D'.
               88  DMA-TRAILER                 VALUE 'T'.
           05  DMA-BODY                    PIC X(149).
      *
           05  DMA-HDR REDEFINES DMA-BODY.
               10  DMAH-FILE-TYPE          PIC X(06).
                   88  DMAH-AUDDIS             VALUE 'AUDDIS'.
                   88  DMAH-ADDACS             VALUE 'ADDACS'.
                   88  DMAH-ARUDD              VALUE 'ARUDD '.
               10  DMAH-PROCESS-DATE       PIC 9(08).
               10  DMAH-FILE-SEQ           PIC 9(06).
               10  DMAH-SUBMITTER          PIC X(06).
               10  FILLER                  PIC X(123).
      *
           05  DMA-DTL REDEFINES DMA-BODY.
               10  DMA-TXN-CODE            PIC X(02).
                   88  DMA-NEW-INSTRUCTION     VALUE '0N'.
                   88  DMA-CANCEL-INSTRUCTION  VALUE '0C'.
                   88  DMA-CONVERSION          VALUE '0S'.
                   88  DMA-COLLECTION          VALUE '17'.
                   88  DMA-FIRST-COLLECTION    VALUE '01'.
                   88  DMA-REPRESENTED         VALUE '18'.
                   88  DMA-FINAL-COLLECTION    VALUE '19'.
               10  DMA-PAYER-SORT-CODE     PIC X(06).
               10  DMA-PAYER-ACCT-NO       PIC X(08).
               10  DMA-PAYER-NAME          PIC X(18).
               10  DMA-ORIG-SUN            PIC X(06).
               10  DMA-ORIG-NAME           PIC X(18).
               10  DMA-ORIG-SORT-CODE      PIC X(06).
               10  DMA-ORIG-ACCT-NO        PIC X(08).
               10  DMA-PAYER-REF           PIC X(18).
               10  DMA-AMOUNT              PIC 9(11)V99.
               10  DMA-EFFECTIVE-DATE      PIC 9(08).
               10  DMA-ADDACS-RSN          PIC X(01).
               10  DMA-ARUDD-RSN           PIC X(01).
               10  DMA-NEW-SORT-CODE       PIC X(06).
               10  DMA-NEW-ACCT-NO         PIC X(08).
               10  DMA-RESULT-CD           PIC X(04).
               10  FILLER                  PIC X(20).
      *
           05  DMA-TRL REDEFINES DMA-BODY.
               10  DMAT-REC-COUNT          PIC 9(09).
               10  DMAT-TOTAL-AMT          PIC 9(13)V99.
               10  FILLER                  PIC X(125).
