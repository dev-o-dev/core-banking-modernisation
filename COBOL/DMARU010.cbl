       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DMARU010.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2003-09-08.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DMARU010                                            *
      * TITLE    : DIRECT DEBITS - BUILD ARUDD (UNPAID) SUBMISSION     *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : DMCOL01 STEP DC040                                  *
      *----------------------------------------------------------------*
      * INPUT    : RETURNS  - UNPAID ITEMS FROM DMCOL010, SORTED BY    *
      *                       ORIGINATOR SUN (STEP DC030) - DMAUDREC   *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : ARUDD    - ARUDD FILE FOR PTX - DMAUDREC WITH       *
      *                       HEADER AND TRAILER                       *
      *            REPORT   - UNPAIDS BY ORIGINATOR (FBA 133)          *
      *----------------------------------------------------------------*
      * BACS RULES REQUIRE UNPAID DIRECT DEBITS TO BE RETURNED TO THE  *
      * ORIGINATOR'S BANK NO LATER THAN THE DAY AFTER THE DEBIT DATE.  *
      * THIS PROGRAM WRAPS THE RETURNS WRITTEN BY DMCOL010 IN AN ARUDD *
      * HEADER AND TRAILER AND PRODUCES A SUMMARY BY ORIGINATOR FOR    *
      * PAYMENTS OPERATIONS (HIGH UNPAID RATES ARE REPORTED TO BACS    *
      * AS POSSIBLE ORIGINATOR MISCONDUCT - DDIG REPORTING).           *
      *                                                                *
      * AN ORIGINATOR WITH MORE THAN WS-ALERT-PCT PERCENT OF ITS       *
      * COLLECTIONS RETURNED TODAY IS FLAGGED. THE COLLECTION COUNT    *
      * PER ORIGINATOR IS NOT AVAILABLE HERE (ONLY THE UNPAIDS ARE),   *
      * SO THE FLAG USES THE UNPAID COUNT AGAINST A FIXED THRESHOLD    *
      * (WS-ALERT-COUNT). PIR 2004/033 TO PASS THE TOTALS THROUGH.     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2003-09-08 SAT  ORIGINAL VERSION                               *
      * 2006-05-02 AOS  ORIGINATOR ALERT THRESHOLD                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RETURNS-FILE     ASSIGN TO RETURNS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RETURNS-STATUS.
           SELECT ARUDD-FILE       ASSIGN TO ARUDD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ARUDD-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  RETURNS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RETURNS-RECORD                  PIC X(150).
       FD  ARUDD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ARUDD-RECORD                    PIC X(150).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DMARU010'.
      *
       01  WS-CONSTANTS.
           05  WS-ALERT-COUNT              PIC 9(05) VALUE 50.
           05  WS-OUR-SUBMITTER            PIC X(06) VALUE '412345'.
      *
       01  WS-FILE-STATUSES.
           05  WS-RETURNS-STATUS           PIC X(02).
               88  WS-RETURNS-OK               VALUE '00'.
               88  WS-RETURNS-EOF              VALUE '10'.
           05  WS-ARUDD-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-RETURNS           VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CURR-SUN                 PIC X(06) VALUE LOW-VALUES.
           05  WS-CURR-ORIG-NAME           PIC X(18).
           05  WS-SUN-COUNT                PIC S9(07) COMP-3.
           05  WS-SUN-TOTAL                PIC S9(13)V99 COMP-3.
           05  WS-RSN-SUB                  PIC S9(04) COMP.
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
      *
       01  WS-RSN-CODES                    PIC X(12)
                                           VALUE '0123456789AB'.
       01  WS-SUN-RSN-TABLE.
           05  WS-SUN-RSN-COUNT            PIC S9(05) COMP-3
                                           OCCURS 12 TIMES.
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-WRITTEN-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUN-GROUPS               PIC S9(07) COMP-3 VALUE 0.
           05  WS-ALERT-GROUPS             PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOTAL-AMT                PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'DMARU010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - ARUDD UNPAID DIRECT DEBITS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(08) VALUE 'SUN'.
           05  FILLER            PIC X(20) VALUE 'ORIGINATOR'.
           05  FILLER            PIC X(08) VALUE ' ITEMS'.
           05  FILLER            PIC X(17) VALUE '          AMOUNT'.
           05  FILLER            PIC X(03) VALUE SPACES.
           05  FILLER            PIC X(48)
               VALUE ' R0  R1  R2  R3  R5  R6  R7  R8  R9  RA  RB'.
           05  FILLER            PIC X(28) VALUE 'ALERT'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-SUN-LINE.
           05  SL-CC                       PIC X(01) VALUE ' '.
           05  SL-SUN                      PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  SL-ORIG-NAME                PIC X(18).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  SL-COUNT                    PIC ZZ,ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  SL-AMOUNT                   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  SL-RSN                      PIC ZZ9 OCCURS 11 TIMES.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  SL-ALERT                    PIC X(30).
           05  FILLER                      PIC X(07) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
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
           PERFORM 2000-PROCESS-RETURN
               UNTIL WS-END-OF-RETURNS
           PERFORM 2900-END-SUN THRU 2900-EXIT
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  RETURNS-FILE
                       DATECARD-FILE
                OUTPUT ARUDD-FILE
                       REPORT-FILE
           IF NOT WS-RETURNS-OK
           OR WS-ARUDD-STATUS NOT = '00'
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

      *---- ARUDD HEADER ----------------------------------------------*
           MOVE SPACES                     TO DMA-INTERFACE-RECORD
           SET DMA-HEADER                  TO TRUE
           SET DMAH-ARUDD                  TO TRUE
           MOVE WS-BUS-DATE                TO DMAH-PROCESS-DATE
           MOVE 1                          TO DMAH-FILE-SEQ
           MOVE WS-OUR-SUBMITTER           TO DMAH-SUBMITTER
           PERFORM 2800-WRITE-ARUDD

           PERFORM 7000-REPORT-HEADINGS
           PERFORM 8000-READ-RETURN
           .
      *================================================================*
       2000-PROCESS-RETURN.
      *================================================================*
           IF DMA-ORIG-SUN NOT = WS-CURR-SUN
               PERFORM 2900-END-SUN THRU 2900-EXIT
               PERFORM 2100-START-SUN
           END-IF

           SET DMA-DETAIL                  TO TRUE
           PERFORM 2800-WRITE-ARUDD
           ADD 1                           TO WS-SUN-COUNT
           ADD DMA-AMOUNT                  TO WS-SUN-TOTAL
                                              WS-TOTAL-AMT
           PERFORM VARYING WS-RSN-SUB FROM 1 BY 1 UNTIL WS-RSN-SUB > 12
               IF WS-RSN-CODES (WS-RSN-SUB:1) = DMA-ARUDD-RSN
                   ADD 1               TO WS-SUN-RSN-COUNT (WS-RSN-SUB)
               END-IF
           END-PERFORM

           PERFORM 8000-READ-RETURN
           .
      *================================================================*
       2100-START-SUN.
      *================================================================*
           MOVE DMA-ORIG-SUN               TO WS-CURR-SUN
           MOVE DMA-ORIG-NAME              TO WS-CURR-ORIG-NAME
           MOVE ZERO                       TO WS-SUN-COUNT
                                              WS-SUN-TOTAL
           PERFORM VARYING WS-RSN-SUB FROM 1 BY 1 UNTIL WS-RSN-SUB > 12
               MOVE ZERO               TO WS-SUN-RSN-COUNT (WS-RSN-SUB)
           END-PERFORM
           ADD 1                           TO WS-SUN-GROUPS
           .
      *================================================================*
       2800-WRITE-ARUDD.
      *================================================================*
           MOVE DMA-INTERFACE-RECORD       TO WS-OUT-RECORD
           WRITE ARUDD-RECORD              FROM WS-OUT-RECORD
           IF WS-ARUDD-STATUS NOT = '00'
               MOVE '2800-WRITE-ARUDD'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ARUDD WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-ARUDD-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           IF DMA-DETAIL
               ADD 1                       TO WS-WRITTEN-COUNT
           END-IF
           .
      *================================================================*
       2900-END-SUN.
      *================================================================*
           IF WS-CURR-SUN = LOW-VALUES
               GO TO 2900-EXIT
           END-IF
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE WS-CURR-SUN                TO SL-SUN
           MOVE WS-CURR-ORIG-NAME          TO SL-ORIG-NAME
           MOVE WS-SUN-COUNT               TO SL-COUNT
           MOVE WS-SUN-TOTAL               TO SL-AMOUNT
      *    REASON COLUMNS 0,1,2,3,5,6,7,8,9,A,B (REASON 4 UNUSED).
           MOVE WS-SUN-RSN-COUNT (1)       TO SL-RSN (1)
           MOVE WS-SUN-RSN-COUNT (2)       TO SL-RSN (2)
           MOVE WS-SUN-RSN-COUNT (3)       TO SL-RSN (3)
           MOVE WS-SUN-RSN-COUNT (4)       TO SL-RSN (4)
           MOVE WS-SUN-RSN-COUNT (6)       TO SL-RSN (5)
           MOVE WS-SUN-RSN-COUNT (7)       TO SL-RSN (6)
           MOVE WS-SUN-RSN-COUNT (8)       TO SL-RSN (7)
           MOVE WS-SUN-RSN-COUNT (9)       TO SL-RSN (8)
           MOVE WS-SUN-RSN-COUNT (10)      TO SL-RSN (9)
           MOVE WS-SUN-RSN-COUNT (11)      TO SL-RSN (10)
           MOVE WS-SUN-RSN-COUNT (12)      TO SL-RSN (11)
           IF WS-SUN-COUNT > WS-ALERT-COUNT
               MOVE '*** HIGH UNPAIDS - REVIEW ***' TO SL-ALERT
               ADD 1                       TO WS-ALERT-GROUPS
           ELSE
               MOVE SPACES                 TO SL-ALERT
           END-IF
           WRITE REPORT-RECORD             FROM WS-SUN-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
       2900-EXIT.
           EXIT.
      *================================================================*
       3000-FINALISE.
      *================================================================*
           MOVE SPACES                     TO DMA-INTERFACE-RECORD
           SET DMA-TRAILER                 TO TRUE
           MOVE WS-WRITTEN-COUNT           TO DMAT-REC-COUNT
           MOVE WS-TOTAL-AMT               TO DMAT-TOTAL-AMT
           PERFORM 2800-WRITE-ARUDD

           MOVE 'UNPAID ITEMS RETURNED'    TO TL-LABEL
           MOVE WS-WRITTEN-COUNT           TO TL-COUNT
           MOVE WS-TOTAL-AMT               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ORIGINATORS'              TO TL-LABEL
           MOVE WS-SUN-GROUPS              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ORIGINATORS FLAGGED'      TO TL-LABEL
           MOVE WS-ALERT-GROUPS            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE RETURNS-FILE
                 ARUDD-FILE
                 REPORT-FILE

           DISPLAY 'DMARU010 - UNPAIDS RETURNED ' WS-WRITTEN-COUNT
           DISPLAY 'DMARU010 - ORIGINATORS      ' WS-SUN-GROUPS
           IF WS-ALERT-GROUPS > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
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
       8000-READ-RETURN.
      *================================================================*
           READ RETURNS-FILE INTO DMA-INTERFACE-RECORD
           EVALUATE TRUE
               WHEN WS-RETURNS-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-RETURNS-EOF
                   SET WS-END-OF-RETURNS   TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-RETURN' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'RETURNS READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-RETURNS-STATUS  TO ABN-FILE-STATUS
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
