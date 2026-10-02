       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GLSUS010.
       AUTHOR.        K L MORGAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2004-01-12.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : GLSUS010                                            *
      * TITLE    : GENERAL LEDGER - SUSPENSE CLEARING AND AGEING       *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : GLSUS010                                            *
      * JCL      : GLSUS01 STEP GS010 (DAILY, AFTER GLFEED01)          *
      *----------------------------------------------------------------*
      * INPUT    : CLEARIN  - CLEARING INSTRUCTIONS KEYED BY FINANCE   *
      *                       OPERATIONS (ISPF PANEL MBKGLS1, FB 120)  *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : MBK.GLSUSP - SUSPENSE ITEMS                         *
      * OUTPUT   : REPORT   - CLEARING RESULTS + AGEING (FBA 133)      *
      *            ESCALATE - ITEMS OVER 30 DAYS FOR THE FINANCE       *
      *                       CONTROLLER (EMAILED BY JOB STEP GS020)   *
      *----------------------------------------------------------------*
      * 1. CLEARING: EACH INSTRUCTION NAMES A SUSPENSE ID AND THE      *
      *    MANUAL JOURNAL REFERENCE THAT CLEARED IT IN ORACLE GL. THE  *
      *    ITEM IS MARKED CLEARED ('C'). WRITE-OFFS ('W') NEED A       *
      *    SECOND USER (FOUR EYES) AND ARE LIMITED TO WS-WRITEOFF-MAX. *
      * 2. AGEING: ALL OPEN ITEMS ARE BUCKETED BY AGE IN CALENDAR DAYS *
      *    0-5, 6-30, 31-90, OVER 90. ITEMS OVER 30 DAYS ARE WRITTEN   *
      *    TO THE ESCALATION FILE. THE STEP ENDS RC 4 IF ANY ITEM IS   *
      *    OVER 90 DAYS (AUDIT REQUIREMENT IA-08-017).                 *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2004-01-12 KLM  ORIGINAL VERSION                               *
      * 2008-06-23 KLM  WRITE-OFF WITH FOUR EYES, 90-DAY RC 4          *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLEARIN-FILE     ASSIGN TO CLEARIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CLEARIN-STATUS.
           SELECT ESCALATE-FILE    ASSIGN TO ESCALATE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ESCALATE-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CLEARIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CLEARIN-RECORD                  PIC X(120).
       FD  ESCALATE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ESCALATE-RECORD                 PIC X(133).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'GLSUS010'.
      *
       01  WS-CONSTANTS.
           05  WS-WRITEOFF-MAX             PIC S9(13)V99 COMP-3
                                           VALUE +500.00.
      *
       01  WS-CLEAR-INSTRUCTION.
           05  CLI-SUSP-ID                 PIC 9(09).
           05  CLI-ACTION                  PIC X(01).
               88  CLI-CLEAR                   VALUE 'C'.
               88  CLI-WRITE-OFF               VALUE 'W'.
           05  CLI-CLEARING-REF            PIC X(16).
           05  CLI-USER                    PIC X(08).
           05  CLI-APPROVER                PIC X(08).
           05  CLI-NOTES                   PIC X(60).
           05  FILLER                      PIC X(18).
      *
       01  WS-FILE-STATUSES.
           05  WS-CLEARIN-STATUS           PIC X(02).
               88  WS-CLEARIN-OK               VALUE '00'.
               88  WS-CLEARIN-EOF              VALUE '10'.
           05  WS-ESCALATE-STATUS          PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-CLR-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-END-OF-CLEARING          VALUE 'Y'.
           05  WS-CSR-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-END-OF-CURSOR            VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-DB2             PIC X(10).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-RAISED-INT               PIC S9(09) COMP.
           05  WS-AGE-DAYS                 PIC S9(05) COMP.
           05  WS-RAISED-NUM               PIC 9(08).
           05  WS-RESULT                   PIC X(10).
           05  WS-RESULT-TEXT              PIC X(50).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-BUCKET                   PIC S9(04) COMP.
      *
       01  WS-AGEING.
           05  WS-AGE-BUCKET               OCCURS 4 TIMES.
               10  WS-AB-COUNT             PIC S9(07) COMP-3.
               10  WS-AB-DR                PIC S9(15)V99 COMP-3.
               10  WS-AB-CR                PIC S9(15)V99 COMP-3.
       01  WS-BUCKET-NAMES.
           05  FILLER                      PIC X(12) VALUE '0-5 DAYS'.
           05  FILLER                      PIC X(12) VALUE '6-30 DAYS'.
           05  FILLER                      PIC X(12) VALUE '31-90 DAYS'.
           05  FILLER                      PIC X(12)
                                           VALUE 'OVER 90 DAYS'.
       01  WS-BUCKET-NAME-TABLE REDEFINES WS-BUCKET-NAMES.
           05  WS-BUCKET-NAME              PIC X(12) OCCURS 4 TIMES.
      *
       01  WS-COUNTERS.
           05  WS-CLEAR-READ               PIC S9(07) COMP-3 VALUE 0.
           05  WS-CLEARED                  PIC S9(07) COMP-3 VALUE 0.
           05  WS-WRITTEN-OFF              PIC S9(07) COMP-3 VALUE 0.
           05  WS-CLEAR-REJECTED           PIC S9(07) COMP-3 VALUE 0.
           05  WS-OPEN-COUNT               PIC S9(07) COMP-3 VALUE 0.
           05  WS-ESCALATED                PIC S9(07) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'GLSUS010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - GL SUSPENSE CLEARING AND AGEING'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(11) VALUE '  SUSP ID'.
           05  FILLER            PIC X(12) VALUE 'RAISED'.
           05  FILLER            PIC X(10) VALUE 'SOURCE'.
           05  FILLER            PIC X(12) VALUE 'PRODUCT'.
           05  FILLER            PIC X(06) VALUE 'TXN'.
           05  FILLER            PIC X(04) VALUE 'D/C'.
           05  FILLER            PIC X(17) VALUE '          AMOUNT'.
           05  FILLER            PIC X(06) VALUE '  AGE'.
           05  FILLER            PIC X(11) VALUE 'RESULT'.
           05  FILLER            PIC X(43) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SUSP-ID                  PIC ZZZZZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RAISED                   PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SOURCE                   PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TXN-CODE                 PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DR-CR                    PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-AGE                      PIC ZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RESULT                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTE                     PIC X(41).
       01  WS-BUCKET-LINE.
           05  BL-CC                       PIC X(01) VALUE '0'.
           05  BL-NAME                     PIC X(14).
           05  BL-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  FILLER                      PIC X(08) VALUE 'DEBITS '.
           05  BL-DR                       PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  FILLER                      PIC X(08) VALUE 'CREDITS '.
           05  BL-CR                       PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(50) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLGLSP END-EXEC.
      *
           EXEC SQL DECLARE OPENCSR CURSOR FOR
               SELECT SUSP_ID, RAISED_DATE, SOURCE_PGM, PROD_CD,
                      TXN_CODE, DR_CR_IND, AMOUNT, REASON_CD, NOTES
                 FROM MBK.GLSUSP
                WHERE SUSP_STATUS = 'O'
                ORDER BY RAISED_DATE, SUSP_ID
                FOR FETCH ONLY
           END-EXEC.
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
           PERFORM 2000-APPLY-CLEARING
           PERFORM 3000-AGE-OPEN-ITEMS
           PERFORM 4000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CLEARIN-FILE
                       DATECARD-FILE
                OUTPUT ESCALATE-FILE
                       REPORT-FILE
           IF NOT WS-CLEARIN-OK
           OR WS-ESCALATE-STATUS NOT = '00'
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
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-BUS-DATE-DB2
           END-STRING
           MOVE WS-BUS-DATE-DB2            TO HD1-BUS-DATE
           COMPUTE WS-BUS-DATE-INT =
                   FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
           PERFORM VARYING WS-BUCKET FROM 1 BY 1 UNTIL WS-BUCKET > 4
               MOVE ZERO TO WS-AB-COUNT (WS-BUCKET)
                            WS-AB-DR (WS-BUCKET)
                            WS-AB-CR (WS-BUCKET)
           END-PERFORM
           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       2000-APPLY-CLEARING.
      *================================================================*
           PERFORM 8000-READ-CLEARING
           PERFORM UNTIL WS-END-OF-CLEARING
               PERFORM 2100-APPLY-ONE THRU 2100-EXIT
               PERFORM 8000-READ-CLEARING
           END-PERFORM
           EXEC SQL COMMIT END-EXEC
           .
      *================================================================*
       2100-APPLY-ONE.
      *================================================================*
           MOVE SPACES                     TO WS-RESULT-TEXT
           MOVE 'CLEARED'                  TO WS-RESULT

           IF CLI-SUSP-ID NOT NUMERIC
           OR NOT (CLI-CLEAR OR CLI-WRITE-OFF)
           OR CLI-CLEARING-REF = SPACES
           OR CLI-USER = SPACES
               MOVE 'REJECTED'             TO WS-RESULT
               MOVE 'INVALID INSTRUCTION'  TO WS-RESULT-TEXT
               PERFORM 2900-REPORT-CLEARING
               GO TO 2100-EXIT
           END-IF

           MOVE CLI-SUSP-ID                TO GSP-SUSP-ID
           EXEC SQL
               SELECT RAISED_DATE, SOURCE_PGM, PROD_CD, TXN_CODE,
                      DR_CR_IND, AMOUNT, SUSP_STATUS
                 INTO :GSP-RAISED-DATE, :GSP-SOURCE-PGM,
                      :GSP-PROD-CD, :GSP-TXN-CODE, :GSP-DR-CR-IND,
                      :GSP-AMOUNT, :GSP-SUSP-STATUS
                 FROM MBK.GLSUSP
                WHERE SUSP_ID = :GSP-SUSP-ID
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'REJECTED'         TO WS-RESULT
                   MOVE 'SUSPENSE ID NOT FOUND' TO WS-RESULT-TEXT
                   PERFORM 2900-REPORT-CLEARING
                   GO TO 2100-EXIT
               WHEN OTHER
                   MOVE '2100-APPLY-ONE'   TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE

           EVALUATE TRUE
               WHEN GSP-SUSP-STATUS NOT = 'O'
                   MOVE 'REJECTED'         TO WS-RESULT
                   MOVE 'ITEM IS NOT OPEN' TO WS-RESULT-TEXT
               WHEN CLI-WRITE-OFF
               AND (CLI-APPROVER = SPACES OR CLI-APPROVER = CLI-USER)
                   MOVE 'REJECTED'         TO WS-RESULT
                   MOVE 'WRITE-OFF NEEDS A SECOND APPROVER'
                                           TO WS-RESULT-TEXT
               WHEN CLI-WRITE-OFF
               AND  GSP-AMOUNT > WS-WRITEOFF-MAX
                   MOVE 'REJECTED'         TO WS-RESULT
                   MOVE 'WRITE-OFF OVER LIMIT - FINANCE CONTROLLER'
                                           TO WS-RESULT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-RESULT = 'REJECTED'
               PERFORM 2900-REPORT-CLEARING
               GO TO 2100-EXIT
           END-IF

           IF CLI-WRITE-OFF
               MOVE 'W'                    TO GSP-SUSP-STATUS
               MOVE 'WRITTEN OFF'          TO WS-RESULT
               ADD 1                       TO WS-WRITTEN-OFF
           ELSE
               MOVE 'C'                    TO GSP-SUSP-STATUS
               ADD 1                       TO WS-CLEARED
           END-IF
           MOVE WS-BUS-DATE-DB2            TO GSP-CLEARED-DATE
           MOVE CLI-USER                   TO GSP-CLEARED-BY
           MOVE CLI-CLEARING-REF           TO GSP-CLEARING-REF
           MOVE CLI-NOTES                  TO GSP-NOTES-TEXT
           MOVE 60                         TO GSP-NOTES-LEN

           EXEC SQL
               UPDATE MBK.GLSUSP
                  SET SUSP_STATUS  = :GSP-SUSP-STATUS,
                      CLEARED_DATE = :GSP-CLEARED-DATE,
                      CLEARED_BY   = :GSP-CLEARED-BY,
                      CLEARING_REF = :GSP-CLEARING-REF,
                      NOTES        = :GSP-NOTES
                WHERE SUSP_ID = :GSP-SUSP-ID
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2100-APPLY-ONE'       TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE CLI-CLEARING-REF           TO WS-RESULT-TEXT
           PERFORM 2900-REPORT-CLEARING
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2900-REPORT-CLEARING.
      *================================================================*
           IF WS-RESULT = 'REJECTED'
               ADD 1                       TO WS-CLEAR-REJECTED
           END-IF
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SPACES                     TO WS-DETAIL-LINE
           IF CLI-SUSP-ID NUMERIC
               MOVE CLI-SUSP-ID            TO DL-SUSP-ID
           END-IF
           MOVE GSP-RAISED-DATE            TO DL-RAISED
           MOVE GSP-SOURCE-PGM             TO DL-SOURCE
           MOVE GSP-PROD-CD                TO DL-PROD-CD
           MOVE GSP-TXN-CODE               TO DL-TXN-CODE
           MOVE GSP-DR-CR-IND              TO DL-DR-CR
           MOVE GSP-AMOUNT                 TO DL-AMOUNT
           MOVE ZERO                       TO DL-AGE
           MOVE WS-RESULT                  TO DL-RESULT
           MOVE WS-RESULT-TEXT             TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       3000-AGE-OPEN-ITEMS.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           EXEC SQL OPEN OPENCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '3000-AGE-OPEN-ITEMS'  TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           PERFORM UNTIL WS-END-OF-CURSOR
               EXEC SQL
                   FETCH OPENCSR
                    INTO :GSP-SUSP-ID, :GSP-RAISED-DATE,
                         :GSP-SOURCE-PGM, :GSP-PROD-CD,
                         :GSP-TXN-CODE, :GSP-DR-CR-IND, :GSP-AMOUNT,
                         :GSP-REASON-CD, :GSP-NOTES
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 3100-AGE-ONE
                   WHEN +100
                       SET WS-END-OF-CURSOR TO TRUE
                   WHEN OTHER
                       MOVE '3000-AGE-OPEN-ITEMS' TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE OPENCSR END-EXEC
           .
      *================================================================*
       3100-AGE-ONE.
      *================================================================*
           ADD 1                           TO WS-OPEN-COUNT
           MOVE GSP-RAISED-DATE (1:4)      TO WS-RAISED-NUM (1:4)
           MOVE GSP-RAISED-DATE (6:2)      TO WS-RAISED-NUM (5:2)
           MOVE GSP-RAISED-DATE (9:2)      TO WS-RAISED-NUM (7:2)
           COMPUTE WS-RAISED-INT =
                   FUNCTION INTEGER-OF-DATE (WS-RAISED-NUM)
           COMPUTE WS-AGE-DAYS = WS-BUS-DATE-INT - WS-RAISED-INT

           EVALUATE TRUE
               WHEN WS-AGE-DAYS <= 5    MOVE 1 TO WS-BUCKET
               WHEN WS-AGE-DAYS <= 30   MOVE 2 TO WS-BUCKET
               WHEN WS-AGE-DAYS <= 90   MOVE 3 TO WS-BUCKET
               WHEN OTHER               MOVE 4 TO WS-BUCKET
           END-EVALUATE
           ADD 1                           TO WS-AB-COUNT (WS-BUCKET)
           IF GSP-DR-CR-IND = 'D'
               ADD GSP-AMOUNT              TO WS-AB-DR (WS-BUCKET)
           ELSE
               ADD GSP-AMOUNT              TO WS-AB-CR (WS-BUCKET)
           END-IF

           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SPACES                     TO WS-DETAIL-LINE
           MOVE GSP-SUSP-ID                TO DL-SUSP-ID
           MOVE GSP-RAISED-DATE            TO DL-RAISED
           MOVE GSP-SOURCE-PGM             TO DL-SOURCE
           MOVE GSP-PROD-CD                TO DL-PROD-CD
           MOVE GSP-TXN-CODE               TO DL-TXN-CODE
           MOVE GSP-DR-CR-IND              TO DL-DR-CR
           MOVE GSP-AMOUNT                 TO DL-AMOUNT
           MOVE WS-AGE-DAYS                TO DL-AGE
           MOVE 'OPEN'                     TO DL-RESULT
           MOVE GSP-NOTES-TEXT (1:41)      TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT

           IF WS-BUCKET >= 3
               MOVE 'ESCALATE'             TO DL-RESULT
               WRITE ESCALATE-RECORD       FROM WS-DETAIL-LINE
               ADD 1                       TO WS-ESCALATED
           END-IF
           .
      *================================================================*
       4000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           PERFORM VARYING WS-BUCKET FROM 1 BY 1 UNTIL WS-BUCKET > 4
               MOVE WS-BUCKET-NAME (WS-BUCKET) TO BL-NAME
               MOVE WS-AB-COUNT (WS-BUCKET)    TO BL-COUNT
               MOVE WS-AB-DR (WS-BUCKET)       TO BL-DR
               MOVE WS-AB-CR (WS-BUCKET)       TO BL-CR
               WRITE REPORT-RECORD             FROM WS-BUCKET-LINE
           END-PERFORM

           MOVE 'CLEARING INSTRUCTIONS READ' TO TL-LABEL
           MOVE WS-CLEAR-READ              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ITEMS CLEARED'            TO TL-LABEL
           MOVE WS-CLEARED                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ITEMS WRITTEN OFF'        TO TL-LABEL
           MOVE WS-WRITTEN-OFF             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INSTRUCTIONS REJECTED'    TO TL-LABEL
           MOVE WS-CLEAR-REJECTED          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'OPEN ITEMS'               TO TL-LABEL
           MOVE WS-OPEN-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ESCALATED (OVER 30 DAYS)' TO TL-LABEL
           MOVE WS-ESCALATED               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE CLEARIN-FILE
                 ESCALATE-FILE
                 REPORT-FILE

           DISPLAY 'GLSUS010 - CLEARED      ' WS-CLEARED
           DISPLAY 'GLSUS010 - OPEN ITEMS   ' WS-OPEN-COUNT
           DISPLAY 'GLSUS010 - OVER 90 DAYS ' WS-AB-COUNT (4)
           IF WS-AB-COUNT (4) > ZERO
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
       8000-READ-CLEARING.
      *================================================================*
           READ CLEARIN-FILE INTO WS-CLEAR-INSTRUCTION
           EVALUATE TRUE
               WHEN WS-CLEARIN-OK
                   ADD 1                   TO WS-CLEAR-READ
               WHEN WS-CLEARIN-EOF
                   SET WS-END-OF-CLEARING  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-CLEARING' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CLEARIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-CLEARIN-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
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
