       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DMADC010.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2001-04-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DMADC010                                            *
      * TITLE    : DIRECT DEBITS - ADDACS ADVICES AND MANDATE          *
      *            HOUSEKEEPING                                        *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : DMDDI01 STEP DM030                                  *
      *----------------------------------------------------------------*
      * INPUT    : DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : DDMAND   - DD MANDATE FILE (VSAM KSDS, SEQ I-O)     *
      * OUTPUT   : ADDACS   - ADDACS ADVICES FOR PTX - DMAUDREC        *
      *            REPORT   - ADDACS / HOUSEKEEPING REPORT (FBA 133)   *
      *----------------------------------------------------------------*
      * 1. ADDACS: EVERY MANDATE CANCELLED BY THE PAYER (STATUS 'C' -  *
      *    DMSVC01 / BRANCH), CLOSED WITH THE ACCOUNT (STATUS 'X' -    *
      *    DDCLS010) OR TRANSFERRED (STATUS 'T') THAT HAS NO ADDACS    *
      *    SENT DATE IS ADVISED TO THE ORIGINATOR:                     *
      *       'C' -> REASON 1  INSTRUCTION CANCELLED BY PAYER          *
      *       'X' -> REASON B  ACCOUNT CLOSED                          *
      *       'T' -> REASON 3  ACCOUNT TRANSFERRED (NEW DETAILS SENT)  *
      *    A MANDATE WHOSE ADDACS REASON WAS SET EXPLICITLY (E.G. '2'  *
      *    PAYER DECEASED BY THE BEREAVEMENT TEAM) KEEPS THAT REASON.  *
      *                                                                *
      * 2. DORMANCY: A LIVE MANDATE WITH NO COLLECTION FOR 13 MONTHS   *
      *    (BACS RULE) BECOMES DORMANT. NO ADVICE IS SENT; A FURTHER   *
      *    COLLECTION IS RETURNED WITH ARUDD REASON 1 BY DMCOL010.     *
      *    MANDATES NEVER COLLECTED ARE MEASURED FROM THE SET-UP DATE. *
      *                                                                *
      * 3. PURGE: CANCELLED / CLOSED MANDATES ADVISED MORE THAN 6      *
      *    YEARS AGO ARE DELETED (INDEMNITY CLAIMS ARE UNLIMITED IN    *
      *    TIME UNDER THE DD GUARANTEE BUT THE BANK'S RECORD RETENTION *
      *    POLICY RRP-07 IS 6 YEARS - KNOWN CONFLICT, SEE PIR 2008/210)*
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2001-04-02 DKP  ORIGINAL VERSION (AUDDIS / ADDACS GO-LIVE)     *
      * 2004-02-16 SAT  13-MONTH DORMANCY                              *
      * 2008-11-17 JNB  6-YEAR PURGE                                   *
      * 2010-05-17 JNB  REASON B FOR MANDATES CLOSED BY DDCLS010       *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DDMAND-FILE      ASSIGN TO DDMAND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS DMM-KEY
                                   FILE STATUS IS WS-DDMAND-STATUS.
           SELECT ADDACS-FILE      ASSIGN TO ADDACS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ADDACS-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  DDMAND-FILE.
           COPY DMMNDREC.
       FD  ADDACS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ADDACS-RECORD                   PIC X(150).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DMADC010'.
      *
       01  WS-CONSTANTS.
           05  WS-DORMANT-MONTHS           PIC S9(04) COMP VALUE +13.
           05  WS-PURGE-YEARS              PIC S9(04) COMP VALUE +6.
           05  WS-OUR-SUBMITTER            PIC X(06) VALUE '412345'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DDMAND-STATUS            PIC X(02).
               88  WS-DDMAND-OK                VALUE '00'.
               88  WS-DDMAND-EOF               VALUE '10'.
           05  WS-ADDACS-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-MANDATES          VALUE 'Y'.
           05  WS-CHANGED-IND              PIC X(01).
               88  WS-CHANGED                  VALUE 'Y'.
      *
       01  WS-DATE-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-R REDEFINES WS-BUS-DATE.
               10  WS-BD-CCYY              PIC 9(04).
               10  WS-BD-MM                PIC 9(02).
               10  WS-BD-DD                PIC 9(02).
           05  WS-DORMANT-BEFORE           PIC 9(08).
           05  WS-DORMANT-BEFORE-R REDEFINES WS-DORMANT-BEFORE.
               10  WS-DB-CCYY              PIC 9(04).
               10  WS-DB-MM                PIC 9(02).
               10  WS-DB-DD                PIC 9(02).
           05  WS-PURGE-BEFORE             PIC 9(08).
           05  WS-PURGE-BEFORE-R REDEFINES WS-PURGE-BEFORE.
               10  WS-PB-CCYY              PIC 9(04).
               10  WS-PB-MM                PIC 9(02).
               10  WS-PB-DD                PIC 9(02).
           05  WS-MONTH-TOTAL              PIC S9(06) COMP.
           05  WS-LAST-ACTIVITY            PIC 9(08).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
           05  WS-ACTION                   PIC X(10).
           05  WS-ADDACS-RSN               PIC X(01).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ADDACS-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DORMANT-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PURGE-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-LIVE-COUNT               PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'DMADC010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - ADDACS AND MANDATE HOUSEKEEPING'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(08) VALUE 'SUN'.
           05  FILLER            PIC X(20) VALUE 'ORIGINATOR'.
           05  FILLER            PIC X(20) VALUE 'PAYER REFERENCE'.
           05  FILLER            PIC X(04) VALUE 'ST'.
           05  FILLER            PIC X(12) VALUE 'LAST COLL'.
           05  FILLER            PIC X(11) VALUE 'ACTION'.
           05  FILLER            PIC X(38) VALUE 'ADDACS RSN'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SUN                      PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ORIG-NAME                PIC X(18).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PAYER-REF                PIC X(18).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-STATUS                   PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-LAST-COLL                PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-ADDACS-RSN               PIC X(01).
           05  FILLER                      PIC X(37) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
           COPY DMAUDREC.
      *
       01  WS-OUT-RECORD                   PIC X(150).
      *
           COPY DATECARD.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-MANDATE
               UNTIL WS-END-OF-MANDATES
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  DATECARD-FILE
                I-O    DDMAND-FILE
                OUTPUT ADDACS-FILE
                       REPORT-FILE
           IF NOT WS-DDMAND-OK
           OR WS-ADDACS-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
           OR DTC-BUSINESS-DATE NOT NUMERIC
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO HD1-BUS-DATE

      *    DORMANCY CUT-OFF = TODAY LESS 13 MONTHS (DAY CAPPED AT 28).
           MOVE WS-BUS-DATE                TO WS-DORMANT-BEFORE
           COMPUTE WS-MONTH-TOTAL = WS-DB-CCYY * 12 + WS-DB-MM - 1
                                  - WS-DORMANT-MONTHS
           DIVIDE WS-MONTH-TOTAL BY 12 GIVING WS-DB-CCYY
                                       REMAINDER WS-DB-MM
           ADD 1                           TO WS-DB-MM
           IF WS-DB-DD > 28
               MOVE 28                     TO WS-DB-DD
           END-IF

      *    PURGE CUT-OFF = TODAY LESS 6 YEARS.
           MOVE WS-BUS-DATE                TO WS-PURGE-BEFORE
           SUBTRACT WS-PURGE-YEARS         FROM WS-PB-CCYY
           IF WS-PB-MM = 2 AND WS-PB-DD = 29
               MOVE 28                     TO WS-PB-DD
           END-IF

           DISPLAY 'DMADC010 - DORMANT IF NO ACTIVITY SINCE '
                   WS-DORMANT-BEFORE
           DISPLAY 'DMADC010 - PURGE IF ADVISED BEFORE      '
                   WS-PURGE-BEFORE

      *---- ADDACS HEADER ---------------------------------------------*
           MOVE SPACES                     TO DMA-INTERFACE-RECORD
           SET DMA-HEADER                  TO TRUE
           SET DMAH-ADDACS                 TO TRUE
           MOVE WS-BUS-DATE                TO DMAH-PROCESS-DATE
           MOVE 1                          TO DMAH-FILE-SEQ
           MOVE WS-OUR-SUBMITTER           TO DMAH-SUBMITTER
           MOVE DMA-INTERFACE-RECORD       TO WS-OUT-RECORD
           WRITE ADDACS-RECORD             FROM WS-OUT-RECORD

           PERFORM 8000-READ-MANDATE
           .
      *================================================================*
       2000-PROCESS-MANDATE.
      *================================================================*
           MOVE 'N'                        TO WS-CHANGED-IND
           MOVE SPACES                     TO WS-ACTION
                                              WS-ADDACS-RSN

           EVALUATE TRUE
      *        1. ADDACS NOT YET SENT
               WHEN (DMM-CANC-BY-PAYER OR DMM-CLOSED
                     OR DMM-TRANSFERRED)
               AND  (DMM-ADDACS-SENT-DATE = ZERO
                     OR DMM-ADDACS-SENT-DATE NOT NUMERIC)
                   PERFORM 2100-SEND-ADDACS
      *        3. PURGE OLD FINISHED MANDATES
               WHEN (DMM-CANC-BY-PAYER OR DMM-CANC-BY-ORIG
                     OR DMM-CLOSED OR DMM-TRANSFERRED)
               AND  DMM-ADDACS-SENT-DATE NUMERIC
               AND  DMM-ADDACS-SENT-DATE > ZERO
               AND  DMM-ADDACS-SENT-DATE < WS-PURGE-BEFORE
                   PERFORM 2300-PURGE-MANDATE
      *        2. DORMANCY
               WHEN DMM-LIVE
                   ADD 1                   TO WS-LIVE-COUNT
                   PERFORM 2200-CHECK-DORMANT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-CHANGED
               MOVE WS-BUS-DATE            TO DMM-LAST-MAINT-DATE
               MOVE WS-PROGRAM-ID          TO DMM-LAST-MAINT-PGM
               REWRITE DMM-MANDATE-RECORD
               IF NOT WS-DDMAND-OK
                   MOVE '2000-PROCESS-MANDATE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'DDMAND REWRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-DDMAND-STATUS   TO ABN-FILE-STATUS
                   MOVE DMM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
           END-IF

           IF WS-ACTION NOT = SPACES
               PERFORM 7100-REPORT-DETAIL
           END-IF

           PERFORM 8000-READ-MANDATE
           .
      *================================================================*
       2100-SEND-ADDACS.
      *================================================================*
           EVALUATE TRUE
               WHEN DMM-ADDACS-REASON NOT = SPACE
                   MOVE DMM-ADDACS-REASON  TO WS-ADDACS-RSN
               WHEN DMM-CANC-BY-PAYER
                   MOVE '1'                TO WS-ADDACS-RSN
               WHEN DMM-CLOSED
                   MOVE 'B'                TO WS-ADDACS-RSN
               WHEN DMM-TRANSFERRED
                   MOVE '3'                TO WS-ADDACS-RSN
           END-EVALUATE

           MOVE SPACES                     TO DMA-INTERFACE-RECORD
           SET DMA-DETAIL                  TO TRUE
           MOVE SPACES                     TO DMA-TXN-CODE
           MOVE DMM-PAYER-SORT-CODE        TO DMA-PAYER-SORT-CODE
           MOVE DMM-PAYER-ACCT-NO          TO DMA-PAYER-ACCT-NO
           MOVE DMM-PAYER-NAME             TO DMA-PAYER-NAME
           MOVE DMM-ORIG-SUN               TO DMA-ORIG-SUN
           MOVE DMM-ORIG-NAME              TO DMA-ORIG-NAME
           MOVE DMM-ORIG-SORT-CODE         TO DMA-ORIG-SORT-CODE
           MOVE DMM-ORIG-ACCT-NO           TO DMA-ORIG-ACCT-NO
           MOVE DMM-PAYER-REF              TO DMA-PAYER-REF
           MOVE ZERO                       TO DMA-AMOUNT
           MOVE WS-BUS-DATE                TO DMA-EFFECTIVE-DATE
           MOVE WS-ADDACS-RSN              TO DMA-ADDACS-RSN
           MOVE SPACE                      TO DMA-ARUDD-RSN
      *    NEW ACCOUNT DETAILS FOR TRANSFERS ARE NOT HELD ON THE
      *    MANDATE - THE SWITCHING TEAM KEY THEM INTO BACSTEL-IP
      *    DIRECTLY. PIR 2009/155.
           MOVE SPACES                     TO DMA-NEW-SORT-CODE
                                              DMA-NEW-ACCT-NO
           MOVE 'ADVS'                     TO DMA-RESULT-CD
           MOVE DMA-INTERFACE-RECORD       TO WS-OUT-RECORD
           WRITE ADDACS-RECORD             FROM WS-OUT-RECORD
           IF WS-ADDACS-STATUS NOT = '00'
               MOVE '2100-SEND-ADDACS'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ADDACS WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-ADDACS-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

           MOVE WS-BUS-DATE                TO DMM-ADDACS-SENT-DATE
           MOVE WS-ADDACS-RSN              TO DMM-ADDACS-REASON
           SET WS-CHANGED                  TO TRUE
           MOVE 'ADDACS'                   TO WS-ACTION
           ADD 1                           TO WS-ADDACS-COUNT
           .
      *================================================================*
       2200-CHECK-DORMANT.
      *================================================================*
           IF DMM-LAST-COLL-DATE NUMERIC
           AND DMM-LAST-COLL-DATE > ZERO
               MOVE DMM-LAST-COLL-DATE     TO WS-LAST-ACTIVITY
           ELSE
               MOVE DMM-SETUP-DATE         TO WS-LAST-ACTIVITY
           END-IF

           IF WS-LAST-ACTIVITY < WS-DORMANT-BEFORE
               SET DMM-DORMANT             TO TRUE
               SET WS-CHANGED              TO TRUE
               MOVE 'DORMANT'              TO WS-ACTION
               ADD 1                       TO WS-DORMANT-COUNT
           END-IF
           .
      *================================================================*
       2300-PURGE-MANDATE.
      *================================================================*
           DELETE DDMAND-FILE RECORD
           IF NOT WS-DDMAND-OK
               MOVE '2300-PURGE-MANDATE'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'DDMAND DELETE FAILED' TO ABN-MESSAGE
               MOVE WS-DDMAND-STATUS       TO ABN-FILE-STATUS
               MOVE DMM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           MOVE 'PURGED'                   TO WS-ACTION
           ADD 1                           TO WS-PURGE-COUNT
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           MOVE SPACES                     TO DMA-INTERFACE-RECORD
           SET DMA-TRAILER                 TO TRUE
           MOVE WS-ADDACS-COUNT            TO DMAT-REC-COUNT
           MOVE ZERO                       TO DMAT-TOTAL-AMT
           MOVE DMA-INTERFACE-RECORD       TO WS-OUT-RECORD
           WRITE ADDACS-RECORD             FROM WS-OUT-RECORD

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'MANDATES READ'            TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LIVE MANDATES'            TO TL-LABEL
           MOVE WS-LIVE-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ADDACS ADVICES SENT'      TO TL-LABEL
           MOVE WS-ADDACS-COUNT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MADE DORMANT (13 MONTHS)' TO TL-LABEL
           MOVE WS-DORMANT-COUNT           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PURGED (6 YEARS)'         TO TL-LABEL
           MOVE WS-PURGE-COUNT             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE DDMAND-FILE
                 ADDACS-FILE
                 REPORT-FILE

           DISPLAY 'DMADC010 - MANDATES READ ' WS-READ-COUNT
           DISPLAY 'DMADC010 - ADDACS SENT   ' WS-ADDACS-COUNT
           DISPLAY 'DMADC010 - DORMANT       ' WS-DORMANT-COUNT
           DISPLAY 'DMADC010 - PURGED        ' WS-PURGE-COUNT
           .
      *================================================================*
       7000-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           WRITE REPORT-RECORD             FROM WS-HEADING-2
           WRITE REPORT-RECORD             FROM WS-HEADING-3
           MOVE 4                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE DMM-PAYER-SORT-CODE        TO DL-SORT-CODE
           MOVE DMM-PAYER-ACCT-NO          TO DL-ACCT-NO
           MOVE DMM-ORIG-SUN               TO DL-SUN
           MOVE DMM-ORIG-NAME              TO DL-ORIG-NAME
           MOVE DMM-PAYER-REF              TO DL-PAYER-REF
           MOVE DMM-STATUS                 TO DL-STATUS
           IF DMM-LAST-COLL-DATE NUMERIC AND DMM-LAST-COLL-DATE > ZERO
               MOVE DMM-LAST-COLL-DATE     TO WS-DATE-WORK
               PERFORM 8900-FORMAT-DATE
               MOVE WS-DATE-DISPLAY        TO DL-LAST-COLL
           ELSE
               MOVE 'NEVER'                TO DL-LAST-COLL
           END-IF
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE WS-ADDACS-RSN              TO DL-ADDACS-RSN
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-MANDATE.
      *================================================================*
           READ DDMAND-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-DDMAND-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-DDMAND-EOF
                   SET WS-END-OF-MANDATES  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-MANDATE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'DDMAND READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-DDMAND-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8900-FORMAT-DATE.
      *================================================================*
           MOVE SPACES                     TO WS-DATE-DISPLAY
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-DATE-DISPLAY
           END-STRING
           .
      *
           COPY ABNDPROC.
