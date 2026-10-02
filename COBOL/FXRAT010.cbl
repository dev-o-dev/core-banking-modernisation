       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FXRAT010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-05-09.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : FXRAT010                                            *
      * TITLE    : FX RATES - LOAD TREASURY FIXING INTO THE RATE TABLE *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : FXRAT010                                            *
      * JCL      : FXRAT01 (07:45 AND 14:15 - TRIGGERED BY ARRIVAL OF  *
      *            THE TREASURY FILE)                                  *
      *----------------------------------------------------------------*
      * INPUT    : FIXING   - GROUP TREASURY RATE FILE (FB 80)         *
      *                       H<FIXING ID><DATE><TIME>                 *
      *                       D<CCY><MID RATE 9(5)V9(6)>               *
      *                       T<COUNT>                                 *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : FXRATES  - FX RATE TABLE (VSAM KSDS, RANDOM)        *
      *            MBK.FXRATEH - RATE HISTORY (ONE ROW PER CCY/FIX)    *
      * OUTPUT   : REPORT   - RATE LOAD REPORT (FBA 133)               *
      *----------------------------------------------------------------*
      * CICS READS FXRATES WHILE THIS JOB UPDATES IT - THE FILE IS     *
      * SHARED (VSAM SHAREOPTIONS(2 3)) AND EACH RECORD IS REWRITTEN   *
      * WHOLE, SO A CICS READ SEES EITHER THE OLD OR THE NEW RATE.     *
      *                                                                *
      * VALIDATION, PER CURRENCY:                                      *
      *   - THE CURRENCY MUST ALREADY BE ON THE TABLE (NEW CURRENCIES  *
      *     ARE SET UP BY TREASURY OPERATIONS WITH FXMNT - ISPF)       *
      *   - MID RATE > ZERO                                            *
      *   - MOVE FROM THE PREVIOUS MID WITHIN FXR-TOLERANCE-PCT (5%    *
      *     DEFAULT). A BIGGER MOVE PUTS THE CURRENCY ON HOLD - THE    *
      *     OLD RATE STAYS IN FORCE AND FXSVC01 QUOTES WITH A WARNING  *
      *     UNTIL TREASURY CONFIRMS WITH A FORCE CARD ('F' IN COL 2 OF *
      *     THE DETAIL RECORD - RESENT BY TREASURY)                    *
      * THE WHOLE FILE IS REJECTED (RC 8, NOTHING UPDATED) IF THE      *
      * TRAILER COUNT IS WRONG OR THE FIXING DATE IS NOT TODAY.        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-05-09 JNB  ORIGINAL VERSION                               *
      * 2016-06-27 PDQ  TOLERANCE PER CURRENCY (GBP MOVES 24/06/2016   *
      *                 HELD EVERY CURRENCY AT THE 07:30 FIXING)       *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIXING-FILE      ASSIGN TO FIXING
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-FIXING-STATUS.
           SELECT FXRATES-FILE     ASSIGN TO FXRATES
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS FXR-KEY
                                   FILE STATUS IS WS-FXRATES-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  FIXING-FILE
           RECORDING MODE IS F.
       01  FIXING-RECORD                   PIC X(80).
       FD  FXRATES-FILE.
           COPY FXRATREC.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'FXRAT010'.
      *
       01  WS-CONSTANTS.
           05  WS-DEFAULT-TOLERANCE        PIC S9(02)V99 COMP-3
                                           VALUE +5.00.
           05  WS-MAX-CCY                  PIC S9(04) COMP VALUE +100.
      *
      *---- FIXING FILE RECORD ----------------------------------------*
       01  FIX-RECORD.
           05  FIX-REC-TYPE                PIC X(01).
               88  FIX-HEADER                  VALUE 'H'.
               88  FIX-DETAIL                  VALUE 'D' 'F'.
               88  FIX-FORCE                   VALUE 'F'.
               88  FIX-TRAILER                 VALUE 'T'.
           05  FIX-BODY                    PIC X(79).
           05  FIX-HDR REDEFINES FIX-BODY.
               10  FIXH-FIXING-ID          PIC X(08).
               10  FIXH-DATE               PIC 9(08).
               10  FIXH-TIME               PIC 9(06).
               10  FILLER                  PIC X(57).
           05  FIX-DTL REDEFINES FIX-BODY.
               10  FIXD-CCY-CD             PIC X(03).
               10  FIXD-MID-RATE           PIC 9(05)V9(06).
               10  FILLER                  PIC X(65).
           05  FIX-TRL REDEFINES FIX-BODY.
               10  FIXT-COUNT              PIC 9(05).
               10  FILLER                  PIC X(74).
      *
       01  WS-FILE-STATUSES.
           05  WS-FIXING-STATUS            PIC X(02).
               88  WS-FIXING-OK                VALUE '00'.
               88  WS-FIXING-EOF               VALUE '10'.
           05  WS-FXRATES-STATUS           PIC X(02).
               88  WS-FXRATES-OK               VALUE '00'.
               88  WS-FXRATES-NOTFND           VALUE '23'.
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-FILE-VALID-IND           PIC X(01) VALUE 'Y'.
               88  WS-FILE-VALID               VALUE 'Y'.
           05  WS-TRAILER-IND              PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-DATE-ISO                 PIC X(10).
           05  WS-FIXING-ID                PIC X(08).
           05  WS-FIXING-TIME              PIC 9(06).
           05  WS-MOVE-PCT                 PIC S9(03)V9(04) COMP-3.
           05  WS-TOLERANCE                PIC S9(02)V99 COMP-3.
           05  WS-RESULT                   PIC X(08).
           05  WS-NOTE                     PIC X(40).
           05  WS-REJECT-REASON            PIC X(50).
      *
      *---- DETAIL RECORDS ARE TABLED AND APPLIED ONLY WHEN THE WHOLE -*
      *---- FILE HAS BEEN VALIDATED ----------------------------------*
       01  WS-RATE-TABLE.
           05  WS-RT-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-RT-ENTRY                 OCCURS 100 TIMES.
               10  WS-RT-CCY               PIC X(03).
               10  WS-RT-MID               PIC S9(05)V9(06) COMP-3.
               10  WS-RT-FORCE             PIC X(01).
       01  WS-SUB                          PIC S9(04) COMP.
      *
       01  WS-COUNTERS.
           05  WS-DETAILS-READ             PIC S9(05) COMP-3 VALUE 0.
           05  WS-APPLIED                  PIC S9(05) COMP-3 VALUE 0.
           05  WS-HELD                     PIC S9(05) COMP-3 VALUE 0.
           05  WS-FORCED                   PIC S9(05) COMP-3 VALUE 0.
           05  WS-UNKNOWN                  PIC S9(05) COMP-3 VALUE 0.
           05  WS-INVALID                  PIC S9(05) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'FXRAT010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(40)
               VALUE 'MEGABANK PLC - FX RATE LOAD'.
           05  FILLER                      PIC X(08) VALUE 'FIXING '.
           05  HD1-FIXING                  PIC X(08).
           05  FILLER                      PIC X(07) VALUE SPACES.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(05) VALUE ' PG '.
           05  HD1-PAGE                    PIC ZZZ9.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(06) VALUE 'CCY'.
           05  FILLER            PIC X(22) VALUE 'NAME'.
           05  FILLER            PIC X(16) VALUE '   PREVIOUS MID'.
           05  FILLER            PIC X(16) VALUE '        NEW MID'.
           05  FILLER            PIC X(11) VALUE '   MOVE %'.
           05  FILLER            PIC X(10) VALUE 'RESULT'.
           05  FILLER            PIC X(51) VALUE 'NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-CCY                      PIC X(03).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-NAME                     PIC X(20).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PREV                     PIC ZZZZ9.999999.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  DL-NEW                      PIC ZZZZ9.999999.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-MOVE                     PIC -ZZ9.9999.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RESULT                   PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
           05  FILLER                      PIC X(12) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(85) VALUE SPACES.
      *
           COPY DATECARD.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLFXRH END-EXEC.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           PERFORM 2000-READ-AND-VALIDATE THRU 2000-EXIT
           IF WS-FILE-VALID
               PERFORM 3000-APPLY-RATES
           ELSE
               PERFORM 3900-REJECT-FILE
           END-IF
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  FIXING-FILE
                       DATECARD-FILE
                I-O    FXRATES-FILE
                OUTPUT REPORT-FILE
           IF NOT WS-FIXING-OK
           OR NOT WS-FXRATES-OK
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
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           MOVE HD1-BUS-DATE               TO WS-DATE-ISO
           .
      *================================================================*
       2000-READ-AND-VALIDATE.
      *================================================================*
           READ FIXING-FILE INTO FIX-RECORD
           IF NOT WS-FIXING-OK OR NOT FIX-HEADER
               MOVE 'N'                    TO WS-FILE-VALID-IND
               MOVE 'NO HEADER RECORD'     TO WS-REJECT-REASON
               GO TO 2000-EXIT
           END-IF
           MOVE FIXH-FIXING-ID             TO WS-FIXING-ID HD1-FIXING
           MOVE FIXH-TIME                  TO WS-FIXING-TIME
           IF FIXH-DATE NOT = WS-BUS-DATE
               MOVE 'N'                    TO WS-FILE-VALID-IND
               MOVE 'FIXING DATE IS NOT THE BUSINESS DATE'
                                           TO WS-REJECT-REASON
               GO TO 2000-EXIT
           END-IF

           READ FIXING-FILE INTO FIX-RECORD
           PERFORM UNTIL NOT WS-FIXING-OK OR WS-TRAILER-SEEN
               EVALUATE TRUE
                   WHEN FIX-DETAIL
                       ADD 1               TO WS-DETAILS-READ
                       IF WS-RT-COUNT >= WS-MAX-CCY
                           MOVE 'N'        TO WS-FILE-VALID-IND
                           MOVE 'MORE THAN 100 CURRENCIES'
                                           TO WS-REJECT-REASON
                       ELSE
                           ADD 1           TO WS-RT-COUNT
                           MOVE FIXD-CCY-CD
                                   TO WS-RT-CCY (WS-RT-COUNT)
                           IF FIXD-MID-RATE NUMERIC
                               MOVE FIXD-MID-RATE
                                   TO WS-RT-MID (WS-RT-COUNT)
                           ELSE
                               MOVE ZERO
                                   TO WS-RT-MID (WS-RT-COUNT)
                           END-IF
                           IF FIX-FORCE
                               MOVE 'Y' TO WS-RT-FORCE (WS-RT-COUNT)
                           ELSE
                               MOVE 'N' TO WS-RT-FORCE (WS-RT-COUNT)
                           END-IF
                       END-IF
                   WHEN FIX-TRAILER
                       SET WS-TRAILER-SEEN TO TRUE
                       IF FIXT-COUNT NOT NUMERIC
                       OR FIXT-COUNT NOT = WS-DETAILS-READ
                           MOVE 'N'        TO WS-FILE-VALID-IND
                           MOVE 'TRAILER COUNT DOES NOT MATCH'
                                           TO WS-REJECT-REASON
                       END-IF
                   WHEN OTHER
                       MOVE 'N'            TO WS-FILE-VALID-IND
                       MOVE 'UNKNOWN RECORD TYPE'
                                           TO WS-REJECT-REASON
               END-EVALUATE
               READ FIXING-FILE INTO FIX-RECORD
           END-PERFORM
           IF NOT WS-TRAILER-SEEN
               MOVE 'N'                    TO WS-FILE-VALID-IND
               MOVE 'NO TRAILER - FILE TRUNCATED'
                                           TO WS-REJECT-REASON
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-APPLY-RATES.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-RT-COUNT
               PERFORM 3100-APPLY-ONE THRU 3100-EXIT
           END-PERFORM
           EXEC SQL COMMIT END-EXEC
           .
      *================================================================*
       3100-APPLY-ONE.
      *================================================================*
           MOVE SPACES                     TO WS-NOTE
           MOVE ZERO                       TO WS-MOVE-PCT
           MOVE WS-RT-CCY (WS-SUB)         TO FXR-CCY-CD
           READ FXRATES-FILE
           IF WS-FXRATES-NOTFND
               ADD 1                       TO WS-UNKNOWN
               MOVE 'UNKNOWN'              TO WS-RESULT
               MOVE 'CURRENCY NOT SET UP - USE FXMNT'
                                           TO WS-NOTE
               MOVE SPACES                 TO FXR-CCY-NAME
               MOVE ZERO                   TO FXR-MID-RATE
               PERFORM 7100-REPORT-DETAIL
               GO TO 3100-EXIT
           END-IF
           IF NOT WS-FXRATES-OK
               MOVE '3100-APPLY-ONE'       TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'FXRATES READ FAILED'  TO ABN-MESSAGE
               MOVE WS-FXRATES-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           IF WS-RT-MID (WS-SUB) NOT > ZERO
               ADD 1                       TO WS-INVALID
               MOVE 'INVALID'              TO WS-RESULT
               MOVE 'ZERO OR NON-NUMERIC RATE - OLD RATE KEPT'
                                           TO WS-NOTE
               PERFORM 7100-REPORT-DETAIL
               GO TO 3100-EXIT
           END-IF

           IF FXR-TOLERANCE-PCT NUMERIC AND FXR-TOLERANCE-PCT > ZERO
               MOVE FXR-TOLERANCE-PCT      TO WS-TOLERANCE
           ELSE
               MOVE WS-DEFAULT-TOLERANCE   TO WS-TOLERANCE
           END-IF
           IF FXR-MID-RATE > ZERO
               COMPUTE WS-MOVE-PCT ROUNDED =
                   (WS-RT-MID (WS-SUB) - FXR-MID-RATE)
                   * 100 / FXR-MID-RATE
           END-IF

           EVALUATE TRUE
               WHEN FXR-SUSPENDED
                   MOVE 'SUSPEND'          TO WS-RESULT
                   MOVE 'CURRENCY SUSPENDED - NOT UPDATED'
                                           TO WS-NOTE
                   PERFORM 7100-REPORT-DETAIL
                   GO TO 3100-EXIT
               WHEN FUNCTION ABS (WS-MOVE-PCT) > WS-TOLERANCE
               AND  WS-RT-FORCE (WS-SUB) NOT = 'Y'
               AND  FXR-MID-RATE > ZERO
                   SET FXR-HELD            TO TRUE
                   ADD 1                   TO WS-HELD
                   MOVE 'HELD'             TO WS-RESULT
                   MOVE 'MOVE OVER TOLERANCE - TREASURY TO CONFIRM'
                                           TO WS-NOTE
                   MOVE 'H'                TO FXH-STATUS
               WHEN OTHER
                   MOVE FXR-MID-RATE       TO FXR-PREV-MID-RATE
                   MOVE WS-RT-MID (WS-SUB) TO FXR-MID-RATE
                   MOVE WS-BUS-DATE        TO FXR-RATE-DATE
                   MOVE WS-FIXING-TIME     TO FXR-RATE-TIME
                   MOVE WS-FIXING-ID       TO FXR-FIXING-ID
                   SET FXR-ACTIVE          TO TRUE
                   ADD 1                   TO WS-APPLIED
                   MOVE 'APPLIED'          TO WS-RESULT
                   MOVE 'A'                TO FXH-STATUS
                   IF WS-RT-FORCE (WS-SUB) = 'Y'
                       ADD 1               TO WS-FORCED
                       MOVE 'FORCED BY TREASURY' TO WS-NOTE
                       MOVE 'F'            TO FXH-STATUS
                   END-IF
           END-EVALUATE
           MOVE WS-BUS-DATE                TO FXR-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO FXR-LAST-MAINT-USER
           REWRITE FXR-RATE-RECORD
           IF NOT WS-FXRATES-OK
               MOVE '3100-APPLY-ONE'       TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'FXRATES REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-FXRATES-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

           MOVE WS-RT-CCY (WS-SUB)         TO FXH-CCY-CD
           MOVE WS-DATE-ISO                TO FXH-RATE-DATE
           MOVE WS-FIXING-ID               TO FXH-FIXING-ID
           MOVE WS-RT-MID (WS-SUB)         TO FXH-MID-RATE
           IF FXR-HELD
               MOVE FXR-MID-RATE           TO FXH-PREV-MID-RATE
           ELSE
               MOVE FXR-PREV-MID-RATE      TO FXH-PREV-MID-RATE
           END-IF
           MOVE WS-MOVE-PCT                TO FXH-MOVE-PCT
           MOVE WS-PROGRAM-ID              TO FXH-LOADED-BY
           EXEC SQL
               INSERT INTO MBK.FXRATEH
                     ( CCY_CD, RATE_DATE, FIXING_ID, MID_RATE,
                       PREV_MID_RATE, MOVE_PCT, STATUS, LOADED_BY,
                       INSERT_TS )
               VALUES ( :FXH-CCY-CD, :FXH-RATE-DATE, :FXH-FIXING-ID,
                        :FXH-MID-RATE, :FXH-PREV-MID-RATE,
                        :FXH-MOVE-PCT, :FXH-STATUS, :FXH-LOADED-BY,
                        CURRENT TIMESTAMP )
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
      *            FIXING RELOADED (FORCE CARD) - KEEP THE LATEST.
                   EXEC SQL
                       UPDATE MBK.FXRATEH
                          SET MID_RATE  = :FXH-MID-RATE,
                              MOVE_PCT  = :FXH-MOVE-PCT,
                              STATUS    = :FXH-STATUS,
                              INSERT_TS = CURRENT TIMESTAMP
                        WHERE CCY_CD    = :FXH-CCY-CD
                          AND RATE_DATE = :FXH-RATE-DATE
                          AND FIXING_ID = :FXH-FIXING-ID
                   END-EXEC
                   IF SQLCODE NOT = 0
                       MOVE '3100-APPLY-ONE' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
                   END-IF
               WHEN OTHER
                   MOVE '3100-APPLY-ONE'   TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           PERFORM 7100-REPORT-DETAIL
           .
       3100-EXIT.
           EXIT.
      *================================================================*
       3900-REJECT-FILE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE SPACES                     TO WS-DETAIL-LINE
           MOVE '0'                        TO DL-CC
           MOVE 'FILE REJECTED - NO RATES UPDATED' TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           MOVE WS-REJECT-REASON           TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           DISPLAY 'FXRAT010 - FILE REJECTED: ' WS-REJECT-REASON
           .
      *================================================================*
       7000-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           WRITE REPORT-RECORD             FROM WS-HEADING-2
           MOVE 3                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE WS-RT-CCY (WS-SUB)         TO DL-CCY
           MOVE FXR-CCY-NAME               TO DL-NAME
           MOVE FXR-PREV-MID-RATE          TO DL-PREV
           MOVE WS-RT-MID (WS-SUB)         TO DL-NEW
           MOVE WS-MOVE-PCT                TO DL-MOVE
           MOVE WS-RESULT                  TO DL-RESULT
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           MOVE 'RATES ON FILE'            TO TL-LABEL
           MOVE WS-DETAILS-READ            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'APPLIED'                  TO TL-LABEL
           MOVE WS-APPLIED                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH FORCED'        TO TL-LABEL
           MOVE WS-FORCED                  TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'HELD - OVER TOLERANCE'    TO TL-LABEL
           MOVE WS-HELD                    TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'UNKNOWN CURRENCIES'       TO TL-LABEL
           MOVE WS-UNKNOWN                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INVALID RATES'            TO TL-LABEL
           MOVE WS-INVALID                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE FIXING-FILE FXRATES-FILE REPORT-FILE
           DISPLAY 'FXRAT010 - APPLIED ' WS-APPLIED
                   ' HELD ' WS-HELD
           EVALUATE TRUE
               WHEN NOT WS-FILE-VALID
                   MOVE 8                  TO RETURN-CODE
               WHEN WS-HELD > ZERO
               OR   WS-UNKNOWN > ZERO
               OR   WS-INVALID > ZERO
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
