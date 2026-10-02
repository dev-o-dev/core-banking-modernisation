       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SORPT010.
       AUTHOR.        R J HALLIDAY.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1988-07-11.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : SORPT010                                            *
      * TITLE    : STANDING ORDERS - EXCEPTIONS REPORT FOR OPERATIONS  *
      * TYPE     : BATCH COBOL (NO DB2) - INTERNAL SORT                *
      * JCL      : SOEXE01 STEP SO030                                  *
      *----------------------------------------------------------------*
      * INPUT    : EXCEPTS  - TODAY'S EXCEPTIONS FROM SOEXE010         *
      *                       (SOEXCREC)                               *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * WORK     : SORTWK01-03                                         *
      * OUTPUT   : REPORT   - EXCEPTIONS BY TYPE AND PAYER BRANCH      *
      *                       (FBA 133), ONE SECTION PER TYPE, WITH    *
      *                       A BRANCH SUMMARY MATRIX AT THE END       *
      *----------------------------------------------------------------*
      * THE REPORT IS PRINTED IN PAYMENTS OPERATIONS (LEEDS) EACH      *
      * MORNING. 'NSFR' ITEMS ARE INFORMATION ONLY (RETRY TONIGHT);    *
      * 'SUSP' AND 'BADD' NEED A CALL TO THE CUSTOMER; 'BENF' MEANS AN *
      * INTERNAL BENEFICIARY ACCOUNT HAS CLOSED AND THE ORDER MUST BE  *
      * AMENDED.                                                       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1988-07-11 RJH  ORIGINAL VERSION (SO0700)                      *
      * 1998-12-07 MLW  Y2K                                            *
      * 2008-05-27 AOS  NEW EXCEPTION CODES, BRANCH MATRIX             *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXCEPTS-FILE     ASSIGN TO EXCEPTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-EXCEPTS-STATUS.
           SELECT SORT-FILE        ASSIGN TO SORTWK01.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  EXCEPTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  EXCEPTS-RECORD                  PIC X(200).
       SD  SORT-FILE.
       01  SR-RECORD.
           05  SR-PAYER-SORT-CODE          PIC X(06).
           05  SR-PAYER-ACCT-NO            PIC X(08).
           05  SR-SO-SEQ                   PIC 9(04).
           05  SR-BUS-DATE                 PIC 9(08).
           05  SR-EXC-CD                   PIC X(04).
           05  FILLER                      PIC X(170).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'SORPT010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-EXCEPTS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-SORT-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-SORT              VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CURR-EXC                 PIC X(04) VALUE LOW-VALUES.
           05  WS-CURR-BRANCH              PIC X(06) VALUE LOW-VALUES.
           05  WS-I                        PIC S9(04) COMP.
           05  WS-J                        PIC S9(04) COMP.
           05  WS-EXC-IDX                  PIC S9(04) COMP.
      *
      *---- EXCEPTION TYPES -------------------------------------------*
       01  WS-EXC-TYPE-VALUES.
           05  FILLER  PIC X(44)
               VALUE 'NSFRINSUFFICIENT FUNDS - RETRY TOMORROW     '.
           05  FILLER  PIC X(44)
               VALUE 'NSFFINSUFFICIENT FUNDS - PAYMENT MISSED     '.
           05  FILLER  PIC X(44)
               VALUE 'SUSPSUSPENDED - 3 CONSECUTIVE MISSES        '.
           05  FILLER  PIC X(44)
               VALUE 'ACLSPAYER ACCOUNT CLOSED - ORDER CANCELLED  '.
           05  FILLER  PIC X(44)
               VALUE 'ARSTPAYER ACCOUNT RESTRICTED - MISSED       '.
           05  FILLER  PIC X(44)
               VALUE 'BENFINTERNAL BENEFICIARY CLOSED / NOT FOUND '.
           05  FILLER  PIC X(44)
               VALUE 'BADDINVALID STANDING ORDER DATA - SUSPENDED '.
           05  FILLER  PIC X(44)
               VALUE '????UNKNOWN EXCEPTION CODE                  '.
       01  WS-EXC-TYPES REDEFINES WS-EXC-TYPE-VALUES.
           05  WS-EXC-ENTRY                OCCURS 8 TIMES.
               10  WS-EXC-CODE             PIC X(04).
               10  WS-EXC-TEXT             PIC X(40).
      *
      *---- TOTALS BY TYPE --------------------------------------------*
       01  WS-TYPE-TOTALS.
           05  WS-TT-ENTRY                 OCCURS 8 TIMES.
               10  WS-TT-COUNT             PIC S9(07) COMP-3.
               10  WS-TT-AMOUNT            PIC S9(13)V99 COMP-3.
      *
      *---- BRANCH MATRIX (UP TO 300 PAYER SORT CODES) ----------------*
       01  WS-BRANCH-TABLE.
           05  WS-BR-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-BR-OVERFLOW              PIC S9(07) COMP-3 VALUE 0.
           05  WS-BR-ENTRY                 OCCURS 300 TIMES
                                           INDEXED BY BR-IDX.
               10  WS-BR-SORT-CODE         PIC X(06).
               10  WS-BR-CELL              PIC S9(05) COMP-3
                                           OCCURS 8 TIMES.
      *
       01  WS-GROUP-COUNT                  PIC S9(07) COMP-3.
       01  WS-GROUP-AMOUNT                 PIC S9(13)V99 COMP-3.
       01  WS-GRAND-COUNT                  PIC S9(07) COMP-3 VALUE 0.
       01  WS-GRAND-AMOUNT                 PIC S9(13)V99 COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'SORPT010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - STANDING ORDER EXCEPTIONS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-TYPE-HEADING.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  FILLER                      PIC X(16)
                                           VALUE 'EXCEPTION TYPE: '.
           05  TH-CODE                     PIC X(04).
           05  FILLER                      PIC X(03) VALUE ' - '.
           05  TH-TEXT                     PIC X(40).
           05  FILLER                      PIC X(69) VALUE SPACES.
       01  WS-COLUMN-HEADING.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(16) VALUE 'PAYER ACCOUNT'.
           05  FILLER            PIC X(06) VALUE 'SEQ'.
           05  FILLER            PIC X(14) VALUE 'CUSTOMER'.
           05  FILLER            PIC X(30) VALUE 'BENEFICIARY'.
           05  FILLER            PIC X(16) VALUE '          AMOUNT'.
           05  FILLER            PIC X(16) VALUE '       AVAILABLE'.
           05  FILLER            PIC X(10) VALUE 'DUE'.
           05  FILLER            PIC X(10) VALUE 'NEXT DUE'.
           05  FILLER            PIC X(14) VALUE 'FAILS LTR'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SEQ                      PIC 9(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-BENE-NAME                PIC X(28).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AVAILABLE                PIC -Z,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DUE                      PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NEXT-DUE                 PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-FAILS                    PIC Z9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-LETTER                   PIC X(01).
           05  FILLER                      PIC X(07) VALUE SPACES.
       01  WS-SUBTOTAL-LINE.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  FILLER                      PIC X(20)
                                           VALUE '  TOTAL FOR TYPE   '.
           05  ST-CODE                     PIC X(04).
           05  FILLER                      PIC X(10) VALUE '  ITEMS: '.
           05  ST-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(10) VALUE '  AMOUNT: '.
           05  ST-AMOUNT                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(63) VALUE SPACES.
       01  WS-MATRIX-HEADING.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  FILLER                      PIC X(12) VALUE 'BRANCH SC'.
           05  MH-CODE                     PIC X(08) OCCURS 8 TIMES.
           05  FILLER                      PIC X(08) VALUE '   TOTAL'.
           05  FILLER                      PIC X(48) VALUE SPACES.
       01  WS-MATRIX-LINE.
           05  ML-CC                       PIC X(01) VALUE ' '.
           05  ML-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  ML-CELL                     PIC ZZZZZZ9 OCCURS 8 TIMES.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  ML-TOTAL                    PIC ZZZZZZ9.
           05  FILLER                      PIC X(56) VALUE SPACES.
       01  WS-ROW-TOTAL                    PIC S9(07) COMP-3.
      *
           COPY SOEXCREC.
      *
           COPY DATECARD.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN SECTION.
      *================================================================*
       0000-START.
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           SORT SORT-FILE
               ON ASCENDING KEY SR-EXC-CD
                                SR-PAYER-SORT-CODE
                                SR-PAYER-ACCT-NO
                                SR-SO-SEQ
               USING EXCEPTS-FILE
               OUTPUT PROCEDURE IS 2000-PRODUCE-REPORT
           IF SORT-RETURN NOT = ZERO
               MOVE '0000-MAIN'            TO ABN-PARAGRAPH
               MOVE 1006                   TO ABN-ABEND-CODE
               MOVE 'SORT FAILED'          TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           PERFORM 3000-BRANCH-MATRIX
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE SECTION.
      *================================================================*
       1000-START.
           OPEN INPUT  DATECARD-FILE
                OUTPUT REPORT-FILE
           IF WS-REPORT-STATUS NOT = '00'
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
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE ZERO                   TO WS-TT-COUNT (WS-I)
                                              WS-TT-AMOUNT (WS-I)
           END-PERFORM
           .
      *================================================================*
       2000-PRODUCE-REPORT SECTION.
      *================================================================*
       2000-START.
           RETURN SORT-FILE INTO SOX-EXCEPTION-RECORD
               AT END SET WS-END-OF-SORT   TO TRUE
           END-RETURN
           IF WS-END-OF-SORT
               PERFORM 7000-PAGE-HEADING
               MOVE SPACES                 TO WS-DETAIL-LINE
               MOVE 'NO STANDING ORDER EXCEPTIONS TODAY'
                                           TO DL-BENE-NAME
               WRITE REPORT-RECORD         FROM WS-DETAIL-LINE
               GO TO 2000-EXIT
           END-IF
           PERFORM UNTIL WS-END-OF-SORT
               IF SOX-EXC-CD NOT = WS-CURR-EXC
                   IF WS-CURR-EXC NOT = LOW-VALUES
                       PERFORM 2200-TYPE-TOTAL
                   END-IF
                   PERFORM 2100-NEW-TYPE
               END-IF
               PERFORM 2300-DETAIL
               RETURN SORT-FILE INTO SOX-EXCEPTION-RECORD
                   AT END SET WS-END-OF-SORT TO TRUE
               END-RETURN
           END-PERFORM
           PERFORM 2200-TYPE-TOTAL
           GO TO 2000-EXIT
           .
      *----------------------------------------------------------------*
       2100-NEW-TYPE.
      *----------------------------------------------------------------*
           MOVE SOX-EXC-CD                 TO WS-CURR-EXC
           MOVE 8                          TO WS-EXC-IDX
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 7
               IF WS-EXC-CODE (WS-I) = SOX-EXC-CD
                   MOVE WS-I               TO WS-EXC-IDX
               END-IF
           END-PERFORM
           MOVE ZERO                       TO WS-GROUP-COUNT
                                              WS-GROUP-AMOUNT
           PERFORM 7000-PAGE-HEADING
           MOVE SOX-EXC-CD                 TO TH-CODE
           MOVE WS-EXC-TEXT (WS-EXC-IDX)   TO TH-TEXT
           WRITE REPORT-RECORD             FROM WS-TYPE-HEADING
           WRITE REPORT-RECORD             FROM WS-COLUMN-HEADING
           ADD 4                           TO WS-LINE-COUNT
           .
      *----------------------------------------------------------------*
       2200-TYPE-TOTAL.
      *----------------------------------------------------------------*
           MOVE WS-CURR-EXC                TO ST-CODE
           MOVE WS-GROUP-COUNT             TO ST-COUNT
           MOVE WS-GROUP-AMOUNT            TO ST-AMOUNT
           WRITE REPORT-RECORD             FROM WS-SUBTOTAL-LINE
           ADD 2                           TO WS-LINE-COUNT
           ADD WS-GROUP-COUNT              TO WS-TT-COUNT (WS-EXC-IDX)
                                              WS-GRAND-COUNT
           ADD WS-GROUP-AMOUNT             TO WS-TT-AMOUNT (WS-EXC-IDX)
                                              WS-GRAND-AMOUNT
           .
      *----------------------------------------------------------------*
       2300-DETAIL.
      *----------------------------------------------------------------*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-PAGE-HEADING
               WRITE REPORT-RECORD         FROM WS-COLUMN-HEADING
               ADD 2                       TO WS-LINE-COUNT
           END-IF
           MOVE SOX-PAYER-SORT-CODE        TO DL-SORT-CODE
           MOVE SOX-PAYER-ACCT-NO          TO DL-ACCT-NO
           MOVE SOX-SO-SEQ                 TO DL-SEQ
           MOVE SOX-CIF-NO                 TO DL-CIF-NO
           MOVE SOX-BENE-NAME              TO DL-BENE-NAME
           MOVE SOX-AMOUNT                 TO DL-AMOUNT
           MOVE SOX-AVAILABLE              TO DL-AVAILABLE
           MOVE SOX-DUE-DATE               TO DL-DUE
           MOVE SOX-NEXT-DUE-DATE          TO DL-NEXT-DUE
           MOVE SOX-CONSEC-FAILS           TO DL-FAILS
           MOVE SOX-LETTER-IND             TO DL-LETTER
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           ADD 1                           TO WS-GROUP-COUNT
           ADD SOX-AMOUNT                  TO WS-GROUP-AMOUNT

      *    BRANCH MATRIX - PAYER SORT CODE IDENTIFIES THE BRANCH.
           SET BR-IDX                      TO 1
           SEARCH WS-BR-ENTRY
               AT END
                   ADD 1                   TO WS-BR-OVERFLOW
               WHEN BR-IDX > WS-BR-COUNT
                   ADD 1                   TO WS-BR-COUNT
                   MOVE SOX-PAYER-SORT-CODE
                                   TO WS-BR-SORT-CODE (BR-IDX)
                   PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
                       MOVE ZERO   TO WS-BR-CELL (BR-IDX WS-J)
                   END-PERFORM
                   ADD 1   TO WS-BR-CELL (BR-IDX WS-EXC-IDX)
               WHEN WS-BR-SORT-CODE (BR-IDX) = SOX-PAYER-SORT-CODE
                   ADD 1   TO WS-BR-CELL (BR-IDX WS-EXC-IDX)
           END-SEARCH
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-BRANCH-MATRIX SECTION.
      *================================================================*
       3000-START.
           IF WS-BR-COUNT = ZERO
               GO TO 3000-EXIT
           END-IF
           PERFORM 7000-PAGE-HEADING
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE SPACES                 TO MH-CODE (WS-I)
               MOVE WS-EXC-CODE (WS-I)     TO MH-CODE (WS-I) (5:4)
           END-PERFORM
           WRITE REPORT-RECORD             FROM WS-MATRIX-HEADING
           ADD 2                           TO WS-LINE-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-BR-COUNT
               IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
                   PERFORM 7000-PAGE-HEADING
                   WRITE REPORT-RECORD     FROM WS-MATRIX-HEADING
                   ADD 2                   TO WS-LINE-COUNT
               END-IF
               MOVE WS-BR-SORT-CODE (WS-I) TO ML-SORT-CODE
               MOVE ZERO                   TO WS-ROW-TOTAL
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
                   MOVE WS-BR-CELL (WS-I WS-J) TO ML-CELL (WS-J)
                   ADD WS-BR-CELL (WS-I WS-J) TO WS-ROW-TOTAL
               END-PERFORM
               MOVE WS-ROW-TOTAL           TO ML-TOTAL
               WRITE REPORT-RECORD         FROM WS-MATRIX-LINE
               ADD 1                       TO WS-LINE-COUNT
           END-PERFORM
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       7000-PAGE-HEADING SECTION.
      *================================================================*
       7000-START.
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           MOVE 2                          TO WS-LINE-COUNT
           .
      *================================================================*
       9000-FINALISE SECTION.
      *================================================================*
       9000-START.
           PERFORM 7000-PAGE-HEADING
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               IF WS-TT-COUNT (WS-I) > ZERO
                   MOVE WS-EXC-CODE (WS-I) TO ST-CODE
                   MOVE WS-TT-COUNT (WS-I) TO ST-COUNT
                   MOVE WS-TT-AMOUNT (WS-I) TO ST-AMOUNT
                   WRITE REPORT-RECORD     FROM WS-SUBTOTAL-LINE
               END-IF
           END-PERFORM
           MOVE 'ALL '                     TO ST-CODE
           MOVE WS-GRAND-COUNT             TO ST-COUNT
           MOVE WS-GRAND-AMOUNT            TO ST-AMOUNT
           WRITE REPORT-RECORD             FROM WS-SUBTOTAL-LINE
           CLOSE REPORT-FILE
           DISPLAY 'SORPT010 - EXCEPTIONS ' WS-GRAND-COUNT
           IF WS-BR-OVERFLOW > ZERO
               DISPLAY 'SORPT010 - BRANCH TABLE FULL, '
                       WS-BR-OVERFLOW ' ITEMS NOT IN MATRIX'
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
       Z999-ABEND-SECTION SECTION.
           COPY ABNDPROC.
