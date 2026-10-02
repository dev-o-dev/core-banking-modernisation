       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CHLGRPT1.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2001-01-22.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CHLGRPT1                                            *
      * TITLE    : CHANNEL SERVICE USAGE AND RESPONSE TIME REPORT      *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CHLGSWP1 STEP CL020 (DAILY 00:30, AFTER THE SWEEP)  *
      *----------------------------------------------------------------*
      * INPUT    : CHLG     - YESTERDAY'S CHROUTER AUDIT LOG (SWEPT    *
      *                       FROM TDQ CHLG BY STEP CL010), FB 160     *
      *            PARMIN   - OPTIONAL 'SLA=NNNNN' RESPONSE TARGET MS  *
      * OUTPUT   : SUMMARY  - ONE RECORD PER SERVICE/CHANNEL FOR THE   *
      *                       CAPACITY DATABASE (FB 120)               *
      *            REPORT   - SERVICE REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * FOR EACH SERVICE AND CHANNEL: CALLS, RETURN CODE BREAKDOWN     *
      * (0000/0004/0008/0012/0016), ABENDS, AVERAGE AND MAXIMUM        *
      * ELAPSED TIME, AND THE NUMBER OF CALLS IN EACH RESPONSE BAND    *
      * (<50MS, <200MS, <1S, <5S, 5S+). CALLS OVER THE SLA ARE COUNTED *
      * SEPARATELY - THE IBANK SLA IS 2,000 MS (SLA SCHEDULE 3 OF THE  *
      * IBANK HOSTING AGREEMENT).                                      *
      * AN HOURLY PROFILE OF ALL CALLS IS PRINTED AT THE END.          *
      *                                                                *
      * THE LOG RECORD LAYOUT IS DEFINED IN CHROUTER (WS-LOG-RECORD);  *
      * IT IS REPEATED HERE - KEEP THEM IN STEP.                       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2001-01-22 DKP  ORIGINAL VERSION (IBANK GO-LIVE MONITORING)    *
      * 2006-06-30 AOS  CHANNEL BREAKDOWN (IA-06-14)                   *
      * 2010-08-02 JNB  TABLE RAISED TO 40 SERVICE/CHANNEL PAIRS -     *
      *                 CARDAUTH VOLUMES; SLA PARM                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHLG-FILE        ASSIGN TO CHLG
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CHLG-STATUS.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
           SELECT SUMMARY-FILE     ASSIGN TO SUMMARY
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-SUMMARY-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CHLG-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHLG-RECORD                     PIC X(160).
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD                   PIC X(80).
       FD  SUMMARY-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SUMMARY-RECORD                  PIC X(120).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CHLGRPT1'.
      *
      *---- CHROUTER AUDIT LOG RECORD (SEE CHROUTER WS-LOG-RECORD) ----*
       01  LG-LOG-RECORD.
           05  LG-DATE                     PIC X(10).
           05  FILLER                      PIC X(01).
           05  LG-TIME                     PIC X(08).
           05  FILLER                      PIC X(01).
           05  LG-TRANID                   PIC X(04).
           05  FILLER                      PIC X(01).
           05  LG-TASKNO                   PIC 9(07).
           05  FILLER                      PIC X(01).
           05  LG-SERVICE-CD               PIC X(08).
           05  FILLER                      PIC X(01).
           05  LG-CHANNEL-CD               PIC X(06).
           05  FILLER                      PIC X(01).
           05  LG-USER-ID                  PIC X(08).
           05  FILLER                      PIC X(01).
           05  LG-REQUEST-ID               PIC X(24).
           05  FILLER                      PIC X(01).
           05  LG-PROGRAM                  PIC X(08).
           05  FILLER                      PIC X(01).
           05  LG-RETURN-CD                PIC X(04).
           05  FILLER                      PIC X(01).
           05  LG-REASON-CD                PIC X(04).
           05  FILLER                      PIC X(01).
           05  LG-ELAPSED-MS               PIC 9(07).
           05  FILLER                      PIC X(01).
           05  LG-ABCODE                   PIC X(04).
           05  FILLER                      PIC X(46).
      *
       01  WS-FILE-STATUSES.
           05  WS-CHLG-STATUS              PIC X(02).
               88  WS-CHLG-OK                  VALUE '00'.
               88  WS-CHLG-EOF                 VALUE '10'.
           05  WS-PARMIN-STATUS            PIC X(02).
           05  WS-SUMMARY-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
      *
       01  WS-WORK-FIELDS.
           05  WS-SLA-MS                   PIC 9(07) VALUE 2000.
           05  WS-LOG-DATE                 PIC X(10) VALUE SPACES.
           05  WS-HOUR                     PIC 9(02).
           05  WS-BAND                     PIC S9(04) COMP.
           05  WS-RC-IDX                   PIC S9(04) COMP.
           05  WS-AVG-MS                   PIC 9(07).
           05  WS-PCT                      PIC 9(03)V9.
           05  WS-I                        PIC S9(04) COMP.
           05  WS-BAR-LEN                  PIC S9(04) COMP.
           05  WS-MAX-HOUR-CALLS           PIC S9(09) COMP-3 VALUE 0.
      *
      *---- SERVICE / CHANNEL TABLE -----------------------------------*
       01  WS-STATS-TABLE.
           05  WS-ST-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-ST-OVERFLOW              PIC S9(09) COMP-3 VALUE 0.
           05  WS-ST-ENTRY                 OCCURS 40 TIMES
                                           INDEXED BY ST-IDX.
               10  WS-ST-SERVICE           PIC X(08).
               10  WS-ST-CHANNEL           PIC X(06).
               10  WS-ST-CALLS             PIC S9(09) COMP-3.
               10  WS-ST-RC                PIC S9(09) COMP-3
                                           OCCURS 5 TIMES.
               10  WS-ST-ABENDS            PIC S9(09) COMP-3.
               10  WS-ST-TOTAL-MS          PIC S9(13) COMP-3.
               10  WS-ST-MAX-MS            PIC S9(07) COMP-3.
               10  WS-ST-OVER-SLA          PIC S9(09) COMP-3.
               10  WS-ST-BAND              PIC S9(09) COMP-3
                                           OCCURS 5 TIMES.
      *
      *---- HOURLY PROFILE --------------------------------------------*
       01  WS-HOURLY.
           05  WS-HR-CALLS                 PIC S9(09) COMP-3
                                           OCCURS 24 TIMES.
           05  WS-HR-ERRORS                PIC S9(09) COMP-3
                                           OCCURS 24 TIMES.
      *
       01  WS-TOTALS.
           05  WS-TOT-CALLS                PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOT-ERRORS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOT-ABENDS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOT-OVER-SLA             PIC S9(09) COMP-3 VALUE 0.
           05  WS-BAD-RECORDS              PIC S9(09) COMP-3 VALUE 0.
      *
      *---- SUMMARY RECORD FOR THE CAPACITY DATABASE -----------------*
       01  WS-SUMMARY.
           05  SM-LOG-DATE                 PIC X(10).
           05  SM-SERVICE                  PIC X(08).
           05  SM-CHANNEL                  PIC X(06).
           05  SM-CALLS                    PIC 9(09).
           05  SM-RC-0000                  PIC 9(09).
           05  SM-RC-0004                  PIC 9(09).
           05  SM-RC-0008                  PIC 9(09).
           05  SM-RC-0012                  PIC 9(09).
           05  SM-RC-0016                  PIC 9(09).
           05  SM-ABENDS                   PIC 9(07).
           05  SM-AVG-MS                   PIC 9(07).
           05  SM-MAX-MS                   PIC 9(07).
           05  SM-OVER-SLA                 PIC 9(07).
           05  FILLER                      PIC X(14).
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CHLGRPT1'.
           05  FILLER                      PIC X(20) VALUE SPACES.
           05  FILLER                      PIC X(50)
               VALUE 'MEGABANK PLC - CHANNEL SERVICE USAGE'.
           05  FILLER                      PIC X(10) VALUE 'LOG DATE '.
           05  HD1-LOG-DATE                PIC X(10).
           05  FILLER                      PIC X(08) VALUE '  SLA: '.
           05  HD1-SLA                     PIC ZZZZZZ9.
           05  FILLER                      PIC X(03) VALUE 'MS'.
           05  FILLER                      PIC X(06) VALUE ' PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(04) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER  PIC X(01) VALUE '0'.
           05  FILLER  PIC X(10) VALUE 'SERVICE'.
           05  FILLER  PIC X(08) VALUE 'CHANNEL'.
           05  FILLER  PIC X(11) VALUE '     CALLS'.
           05  FILLER  PIC X(09) VALUE '    0000'.
           05  FILLER  PIC X(09) VALUE '    0004'.
           05  FILLER  PIC X(09) VALUE '    0008'.
           05  FILLER  PIC X(09) VALUE '    0012'.
           05  FILLER  PIC X(09) VALUE '    0016'.
           05  FILLER  PIC X(07) VALUE ' ABEND'.
           05  FILLER  PIC X(08) VALUE ' AVG MS'.
           05  FILLER  PIC X(09) VALUE '  MAX MS'.
           05  FILLER  PIC X(09) VALUE ' >SLA'.
           05  FILLER  PIC X(26) VALUE '  ERR %'.
       01  WS-HEADING-3.
           05  FILLER  PIC X(01) VALUE ' '.
           05  FILLER  PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SERVICE                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CHANNEL                  PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CALLS                    PIC ZZZZZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RC                       PIC ZZZZZZZ9 OCCURS 5 TIMES.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-ABENDS                   PIC ZZZZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-AVG                      PIC ZZZZZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-MAX                      PIC ZZZZZZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-OVER                     PIC ZZZZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ERR-PCT                  PIC ZZ9.9.
           05  FILLER                      PIC X(22) VALUE SPACES.
       01  WS-BAND-LINE.
           05  BL-CC                       PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(20)
                                           VALUE '      BANDS <50MS:'.
           05  BL-B1                       PIC ZZZZZZZ9.
           05  FILLER                      PIC X(08) VALUE '  <200:'.
           05  BL-B2                       PIC ZZZZZZZ9.
           05  FILLER                      PIC X(07) VALUE '  <1S:'.
           05  BL-B3                       PIC ZZZZZZZ9.
           05  FILLER                      PIC X(07) VALUE '  <5S:'.
           05  BL-B4                       PIC ZZZZZZZ9.
           05  FILLER                      PIC X(07) VALUE '  5S+:'.
           05  BL-B5                       PIC ZZZZZZZ9.
           05  FILLER                      PIC X(43) VALUE SPACES.
       01  WS-HOUR-LINE.
           05  HL-CC                       PIC X(01) VALUE ' '.
           05  HL-HOUR                     PIC 99.
           05  FILLER                      PIC X(05) VALUE ':00  '.
           05  HL-CALLS                    PIC ZZZZZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  HL-ERRORS                   PIC ZZZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  HL-BAR                      PIC X(80).
           05  FILLER                      PIC X(25) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           READ CHLG-FILE INTO LG-LOG-RECORD
           PERFORM UNTIL NOT WS-CHLG-OK
               PERFORM 2000-ACCUMULATE THRU 2000-EXIT
               READ CHLG-FILE INTO LG-LOG-RECORD
           END-PERFORM
           IF NOT WS-CHLG-EOF
               MOVE '0000-MAIN'            TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHLG READ FAILED'     TO ABN-MESSAGE
               MOVE WS-CHLG-STATUS         TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           PERFORM 3000-REPORT-SERVICES
           PERFORM 4000-REPORT-HOURS
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CHLG-FILE
                       PARMIN-FILE
                OUTPUT SUMMARY-FILE
                       REPORT-FILE
           IF NOT WS-CHLG-OK
           OR WS-SUMMARY-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF WS-PARMIN-STATUS = '00'
               READ PARMIN-FILE
               IF WS-PARMIN-STATUS = '00'
               AND PARMIN-RECORD (1:4) = 'SLA='
               AND PARMIN-RECORD (5:5) NUMERIC
                   MOVE PARMIN-RECORD (5:5) TO WS-SLA-MS
               END-IF
               CLOSE PARMIN-FILE
           END-IF
           MOVE WS-SLA-MS                  TO HD1-SLA
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               MOVE ZERO                   TO WS-HR-CALLS (WS-I)
                                              WS-HR-ERRORS (WS-I)
           END-PERFORM
           .
      *================================================================*
       2000-ACCUMULATE.
      *================================================================*
           IF LG-ELAPSED-MS NOT NUMERIC
           OR LG-TIME (1:2) NOT NUMERIC
               ADD 1                       TO WS-BAD-RECORDS
               GO TO 2000-EXIT
           END-IF
           IF WS-LOG-DATE = SPACES
               MOVE LG-DATE                TO WS-LOG-DATE HD1-LOG-DATE
           END-IF
           ADD 1                           TO WS-TOT-CALLS

           SET ST-IDX                      TO 1
           SEARCH WS-ST-ENTRY
               AT END
                   ADD 1                   TO WS-ST-OVERFLOW
                   GO TO 2000-HOURLY
               WHEN ST-IDX > WS-ST-COUNT
                   ADD 1                   TO WS-ST-COUNT
                   PERFORM 2100-NEW-ENTRY
               WHEN WS-ST-SERVICE (ST-IDX) = LG-SERVICE-CD
               AND  WS-ST-CHANNEL (ST-IDX) = LG-CHANNEL-CD
                   CONTINUE
           END-SEARCH

           ADD 1                           TO WS-ST-CALLS (ST-IDX)
           EVALUATE LG-RETURN-CD
               WHEN '0000'  MOVE 1         TO WS-RC-IDX
               WHEN '0004'  MOVE 2         TO WS-RC-IDX
               WHEN '0008'  MOVE 3         TO WS-RC-IDX
               WHEN '0012'  MOVE 4         TO WS-RC-IDX
               WHEN OTHER   MOVE 5         TO WS-RC-IDX
           END-EVALUATE
           ADD 1                   TO WS-ST-RC (ST-IDX WS-RC-IDX)
           IF LG-ABCODE NOT = SPACES
               ADD 1                       TO WS-ST-ABENDS (ST-IDX)
                                              WS-TOT-ABENDS
           END-IF
           ADD LG-ELAPSED-MS               TO WS-ST-TOTAL-MS (ST-IDX)
           IF LG-ELAPSED-MS > WS-ST-MAX-MS (ST-IDX)
               MOVE LG-ELAPSED-MS          TO WS-ST-MAX-MS (ST-IDX)
           END-IF
           IF LG-ELAPSED-MS > WS-SLA-MS
               ADD 1                       TO WS-ST-OVER-SLA (ST-IDX)
                                              WS-TOT-OVER-SLA
           END-IF
           EVALUATE TRUE
               WHEN LG-ELAPSED-MS < 50     MOVE 1 TO WS-BAND
               WHEN LG-ELAPSED-MS < 200    MOVE 2 TO WS-BAND
               WHEN LG-ELAPSED-MS < 1000   MOVE 3 TO WS-BAND
               WHEN LG-ELAPSED-MS < 5000   MOVE 4 TO WS-BAND
               WHEN OTHER                  MOVE 5 TO WS-BAND
           END-EVALUATE
           ADD 1                   TO WS-ST-BAND (ST-IDX WS-BAND)
           .
       2000-HOURLY.
           MOVE LG-TIME (1:2)              TO WS-HOUR
           IF WS-HOUR < 24
               ADD 1                       TO WS-HR-CALLS (WS-HOUR + 1)
               IF LG-RETURN-CD = '0012' OR LG-ABCODE NOT = SPACES
                   ADD 1          TO WS-HR-ERRORS (WS-HOUR + 1)
               END-IF
           END-IF
           IF LG-RETURN-CD = '0012' OR '0016'
               ADD 1                       TO WS-TOT-ERRORS
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-NEW-ENTRY.
      *================================================================*
           MOVE LG-SERVICE-CD              TO WS-ST-SERVICE (ST-IDX)
           MOVE LG-CHANNEL-CD              TO WS-ST-CHANNEL (ST-IDX)
           MOVE ZERO                       TO WS-ST-CALLS (ST-IDX)
                                              WS-ST-ABENDS (ST-IDX)
                                              WS-ST-TOTAL-MS (ST-IDX)
                                              WS-ST-MAX-MS (ST-IDX)
                                              WS-ST-OVER-SLA (ST-IDX)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE ZERO                   TO WS-ST-RC (ST-IDX WS-I)
                                              WS-ST-BAND (ST-IDX WS-I)
           END-PERFORM
           .
      *================================================================*
       3000-REPORT-SERVICES.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           PERFORM VARYING ST-IDX FROM 1 BY 1
                   UNTIL ST-IDX > WS-ST-COUNT
               IF WS-LINE-COUNT >= 53
                   PERFORM 7000-REPORT-HEADINGS
               END-IF
               MOVE WS-ST-SERVICE (ST-IDX) TO DL-SERVICE
               MOVE WS-ST-CHANNEL (ST-IDX) TO DL-CHANNEL
               MOVE WS-ST-CALLS (ST-IDX)   TO DL-CALLS
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
                   MOVE WS-ST-RC (ST-IDX WS-I) TO DL-RC (WS-I)
               END-PERFORM
               MOVE WS-ST-ABENDS (ST-IDX)  TO DL-ABENDS
               IF WS-ST-CALLS (ST-IDX) > ZERO
                   COMPUTE WS-AVG-MS ROUNDED = WS-ST-TOTAL-MS (ST-IDX)
                                         / WS-ST-CALLS (ST-IDX)
                   COMPUTE WS-PCT ROUNDED =
                       (WS-ST-RC (ST-IDX 4) + WS-ST-RC (ST-IDX 5))
                       * 100 / WS-ST-CALLS (ST-IDX)
               ELSE
                   MOVE ZERO               TO WS-AVG-MS WS-PCT
               END-IF
               MOVE WS-AVG-MS              TO DL-AVG
               MOVE WS-ST-MAX-MS (ST-IDX)  TO DL-MAX
               MOVE WS-ST-OVER-SLA (ST-IDX) TO DL-OVER
               MOVE WS-PCT                 TO DL-ERR-PCT
               WRITE REPORT-RECORD         FROM WS-DETAIL-LINE
               MOVE WS-ST-BAND (ST-IDX 1)  TO BL-B1
               MOVE WS-ST-BAND (ST-IDX 2)  TO BL-B2
               MOVE WS-ST-BAND (ST-IDX 3)  TO BL-B3
               MOVE WS-ST-BAND (ST-IDX 4)  TO BL-B4
               MOVE WS-ST-BAND (ST-IDX 5)  TO BL-B5
               WRITE REPORT-RECORD         FROM WS-BAND-LINE
               ADD 2                       TO WS-LINE-COUNT
               PERFORM 3100-WRITE-SUMMARY
           END-PERFORM
           .
      *================================================================*
       3100-WRITE-SUMMARY.
      *================================================================*
           MOVE SPACES                     TO WS-SUMMARY
           MOVE WS-LOG-DATE                TO SM-LOG-DATE
           MOVE WS-ST-SERVICE (ST-IDX)     TO SM-SERVICE
           MOVE WS-ST-CHANNEL (ST-IDX)     TO SM-CHANNEL
           MOVE WS-ST-CALLS (ST-IDX)       TO SM-CALLS
           MOVE WS-ST-RC (ST-IDX 1)        TO SM-RC-0000
           MOVE WS-ST-RC (ST-IDX 2)        TO SM-RC-0004
           MOVE WS-ST-RC (ST-IDX 3)        TO SM-RC-0008
           MOVE WS-ST-RC (ST-IDX 4)        TO SM-RC-0012
           MOVE WS-ST-RC (ST-IDX 5)        TO SM-RC-0016
           MOVE WS-ST-ABENDS (ST-IDX)      TO SM-ABENDS
           MOVE WS-AVG-MS                  TO SM-AVG-MS
           MOVE WS-ST-MAX-MS (ST-IDX)      TO SM-MAX-MS
           MOVE WS-ST-OVER-SLA (ST-IDX)    TO SM-OVER-SLA
           WRITE SUMMARY-RECORD            FROM WS-SUMMARY
           IF WS-SUMMARY-STATUS NOT = '00'
               MOVE '3100-WRITE-SUMMARY'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SUMMARY WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-SUMMARY-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       4000-REPORT-HOURS.
      *================================================================*
      *    HISTOGRAM - THE BUSIEST HOUR GETS A BAR OF 80 ASTERISKS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               IF WS-HR-CALLS (WS-I) > WS-MAX-HOUR-CALLS
                   MOVE WS-HR-CALLS (WS-I) TO WS-MAX-HOUR-CALLS
               END-IF
           END-PERFORM
           PERFORM 7000-REPORT-HEADINGS
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               COMPUTE HL-HOUR = WS-I - 1
               MOVE WS-HR-CALLS (WS-I)     TO HL-CALLS
               MOVE WS-HR-ERRORS (WS-I)    TO HL-ERRORS
               MOVE SPACES                 TO HL-BAR
               IF WS-MAX-HOUR-CALLS > ZERO
                   COMPUTE WS-BAR-LEN = WS-HR-CALLS (WS-I) * 80
                                      / WS-MAX-HOUR-CALLS
                   IF WS-BAR-LEN > ZERO
                       MOVE ALL '*'        TO HL-BAR (1:WS-BAR-LEN)
                   END-IF
               END-IF
               WRITE REPORT-RECORD         FROM WS-HOUR-LINE
           END-PERFORM
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
       9000-FINALISE.
      *================================================================*
           MOVE 'TOTAL CHANNEL CALLS'      TO TL-LABEL
           MOVE WS-TOT-CALLS               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SYSTEM / INVALID REQUEST ERRORS' TO TL-LABEL
           MOVE WS-TOT-ERRORS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ABENDS IN SERVICE PROGRAMS' TO TL-LABEL
           MOVE WS-TOT-ABENDS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CALLS OVER SLA'           TO TL-LABEL
           MOVE WS-TOT-OVER-SLA            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'UNREADABLE LOG RECORDS'   TO TL-LABEL
           MOVE WS-BAD-RECORDS             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CALLS NOT TABLED (TABLE FULL)' TO TL-LABEL
           MOVE WS-ST-OVERFLOW             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE CHLG-FILE SUMMARY-FILE REPORT-FILE
           DISPLAY 'CHLGRPT1 - CALLS ' WS-TOT-CALLS
                   ' ABENDS ' WS-TOT-ABENDS
           IF WS-TOT-ABENDS > ZERO OR WS-ST-OVERFLOW > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
