       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STSTM030.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-03-22.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : STSTM030                                            *
      * TITLE    : STATEMENT CYCLE - ACCOUNT UPDATE AND E-STATEMENT    *
      *            NOTIFICATION FEED                                   *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : STMT01 STEP ST030                                   *
      *----------------------------------------------------------------*
      * INPUT    : INDEX    - STATEMENT INDEX FROM STSTM020 - STIDXREC *
      *                       SORTED BY SORT CODE + ACCOUNT (ST025)    *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM)   *
      * OUTPUT   : ESTMTFD  - E-STATEMENT FEED FOR IBANK (FB 150)      *
      *                       -> CHANNEL TABLE STMT_HDR                *
      *            REPORT   - UPDATE CONTROL REPORT (FBA 133)          *
      *----------------------------------------------------------------*
      * FOR EVERY STATEMENT PRODUCED (STATUS 'P'):                     *
      *   - ACM-LAST-STMT-DATE = PERIOD TO, ACM-LAST-STMT-NO = STMT NO *
      *     (ONLY IF THE STATEMENT IS NEWER THAN THE ONE ON FILE - A   *
      *     DUPLICATE OR AD-HOC REQUEST DOES NOT MOVE THE CYCLE)       *
      *   - ONE FEED RECORD FOR IBANK SO THE STATEMENT APPEARS IN      *
      *     "MY DOCUMENTS" (ALL STATEMENTS, NOT ONLY ELECTRONIC ONES - *
      *     IBANK SHOWS PAPER STATEMENTS AS "COPY AVAILABLE")          *
      * FAILED STATEMENTS ARE REPORTED AND THE ACCOUNT IS NOT UPDATED  *
      * SO STSTM010 WILL SELECT IT AGAIN NEXT CYCLE - UNLESS THE       *
      * STATEMENT DAY HAS PASSED, IN WHICH CASE IT IS MISSED UNTIL THE *
      * FOLLOWING PERIOD (PIR 2010/112 - OPEN).                        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-03-22 SAT  ORIGINAL VERSION (REPLACES DOC1 STMT03)        *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INDEX-FILE       ASSIGN TO INDEX
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-INDEX-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT ESTMTFD-FILE     ASSIGN TO ESTMTFD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ESTMTFD-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  INDEX-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  INDEX-RECORD                    PIC X(200).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  ESTMTFD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ESTMTFD-RECORD                  PIC X(150).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'STSTM030'.
      *
       01  WS-FILE-STATUSES.
           05  WS-INDEX-STATUS             PIC X(02).
               88  WS-INDEX-OK                 VALUE '00'.
               88  WS-INDEX-EOF                VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-ESTMTFD-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INDEX             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-RESULT                   PIC X(40).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
      *
      *---- IBANK E-STATEMENT FEED RECORD (CHANNEL TABLE STMT_HDR) ----*
       01  WS-ESTMT-RECORD.
           05  EST-REC-TYPE                PIC X(01).
               88  EST-HEADER                  VALUE 'H'.
               88  EST-DETAIL                  VALUE 'D'.
               88  EST-TRAILER                 VALUE 'T'.
           05  EST-DOC-KEY                 PIC X(30).
           05  EST-CIF-NO                  PIC X(12).
           05  EST-SORT-CODE               PIC X(06).
           05  EST-ACCT-NO                 PIC X(08).
           05  EST-STMT-NO                 PIC 9(05).
           05  EST-PERIOD-FROM             PIC 9(08).
           05  EST-PERIOD-TO               PIC 9(08).
           05  EST-CLOSING-BAL             PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  EST-PAGE-COUNT              PIC 9(04).
           05  EST-DELIVERY                PIC X(01).
           05  EST-NOTIFY-IND              PIC X(01).
           05  EST-CREATE-DATE             PIC 9(08).
           05  FILLER                      PIC X(43).
       01  WS-ESTMT-TRAILER REDEFINES WS-ESTMT-RECORD.
           05  FILLER                      PIC X(01).
           05  ESTT-RECORD-COUNT           PIC 9(09).
           05  ESTT-CREATE-DATE            PIC 9(08).
           05  FILLER                      PIC X(132).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-UPDATED-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOT-ADVANCED-COUNT       PIC S9(09) COMP-3 VALUE 0.
           05  WS-FAILED-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-FEED-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-MISSING-ACCT-COUNT       PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'STSTM030'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - STATEMENT CYCLE ACCOUNT UPDATE'.
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
           05  FILLER            PIC X(07) VALUE 'STMT'.
           05  FILLER            PIC X(12) VALUE 'PERIOD TO'.
           05  FILLER            PIC X(12) VALUE 'PREV STMT'.
           05  FILLER            PIC X(82) VALUE 'RESULT'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-STMT-NO                  PIC ZZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PERIOD-TO                PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PREV-STMT                PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RESULT                   PIC X(40).
           05  FILLER                      PIC X(42) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
           COPY STIDXREC.
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
           PERFORM 2000-PROCESS-INDEX
               UNTIL WS-END-OF-INDEX
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  INDEX-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                OUTPUT ESTMTFD-FILE
                       REPORT-FILE
           IF NOT WS-INDEX-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-ESTMTFD-STATUS NOT = '00'
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

           MOVE SPACES                     TO WS-ESTMT-RECORD
           SET EST-HEADER                  TO TRUE
           MOVE WS-BUS-DATE                TO EST-CREATE-DATE
           MOVE 'MBKESTMT'                 TO EST-DOC-KEY
           WRITE ESTMTFD-RECORD            FROM WS-ESTMT-RECORD

           PERFORM 8000-READ-INDEX
           .
      *================================================================*
       2000-PROCESS-INDEX.
      *================================================================*
           MOVE SPACES                     TO WS-RESULT
                                              DL-PREV-STMT
           IF SIX-FAILED
               ADD 1                       TO WS-FAILED-COUNT
               STRING 'STATEMENT FAILED (' SIX-FAIL-REASON
                      ') - ACCOUNT NOT UPDATED'
                      DELIMITED BY SIZE
                      INTO WS-RESULT
               END-STRING
               PERFORM 7100-REPORT-DETAIL
           ELSE
               PERFORM 2100-UPDATE-ACCOUNT THRU 2100-EXIT
               PERFORM 2200-WRITE-FEED
           END-IF
           PERFORM 8000-READ-INDEX
           .
      *================================================================*
       2100-UPDATE-ACCOUNT.
      *================================================================*
           MOVE SIX-SORT-CODE              TO ACM-SORT-CODE
           MOVE SIX-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   CONTINUE
               WHEN WS-ACCTMAST-NOTFND
                   ADD 1                   TO WS-MISSING-ACCT-COUNT
                   MOVE 'ACCOUNT NOT ON MASTER' TO WS-RESULT
                   PERFORM 7100-REPORT-DETAIL
                   GO TO 2100-EXIT
               WHEN OTHER
                   MOVE '2100-UPDATE-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE

           IF ACM-LAST-STMT-DATE NUMERIC AND ACM-LAST-STMT-DATE > ZERO
               MOVE ACM-LAST-STMT-DATE     TO WS-DATE-WORK
               PERFORM 8900-FORMAT-DATE
               MOVE WS-DATE-DISPLAY        TO DL-PREV-STMT
           END-IF

           IF SIX-PERIOD-TO > ACM-LAST-STMT-DATE
           OR ACM-LAST-STMT-DATE NOT NUMERIC
               MOVE SIX-PERIOD-TO          TO ACM-LAST-STMT-DATE
               MOVE SIX-STMT-NO            TO ACM-LAST-STMT-NO
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '2100-UPDATE-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
               ADD 1                       TO WS-UPDATED-COUNT
               MOVE 'UPDATED'              TO WS-RESULT
           ELSE
               ADD 1                       TO WS-NOT-ADVANCED-COUNT
               MOVE 'NOT ADVANCED - NOT NEWER THAN LAST STATEMENT'
                                           TO WS-RESULT
           END-IF
           PERFORM 7100-REPORT-DETAIL
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-WRITE-FEED.
      *================================================================*
           MOVE SPACES                     TO WS-ESTMT-RECORD
           SET EST-DETAIL                  TO TRUE
           MOVE SIX-DOC-KEY                TO EST-DOC-KEY
           MOVE SIX-CIF-NO                 TO EST-CIF-NO
           MOVE SIX-SORT-CODE              TO EST-SORT-CODE
           MOVE SIX-ACCT-NO                TO EST-ACCT-NO
           MOVE SIX-STMT-NO                TO EST-STMT-NO
           MOVE SIX-PERIOD-FROM            TO EST-PERIOD-FROM
           MOVE SIX-PERIOD-TO              TO EST-PERIOD-TO
           MOVE SIX-CLOSING-BAL            TO EST-CLOSING-BAL
           MOVE SIX-PAGE-COUNT             TO EST-PAGE-COUNT
           MOVE SIX-DELIVERY               TO EST-DELIVERY
      *    ONLY ELECTRONIC CUSTOMERS GET AN EMAIL NOTIFICATION.
           IF SIX-ELECTRONIC
               MOVE 'Y'                    TO EST-NOTIFY-IND
           ELSE
               MOVE 'N'                    TO EST-NOTIFY-IND
           END-IF
           MOVE WS-BUS-DATE                TO EST-CREATE-DATE
           WRITE ESTMTFD-RECORD            FROM WS-ESTMT-RECORD
           IF WS-ESTMTFD-STATUS NOT = '00'
               MOVE '2200-WRITE-FEED'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ESTMTFD WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ESTMTFD-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-FEED-COUNT
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           MOVE SPACES                     TO WS-ESTMT-RECORD
           SET EST-TRAILER                 TO TRUE
           MOVE WS-FEED-COUNT              TO ESTT-RECORD-COUNT
           MOVE WS-BUS-DATE                TO ESTT-CREATE-DATE
           WRITE ESTMTFD-RECORD            FROM WS-ESTMT-RECORD

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'INDEX RECORDS READ'       TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS UPDATED'         TO TL-LABEL
           MOVE WS-UPDATED-COUNT           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NOT ADVANCED (AD-HOC/DUPLICATE)' TO TL-LABEL
           MOVE WS-NOT-ADVANCED-COUNT      TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'FAILED STATEMENTS'        TO TL-LABEL
           MOVE WS-FAILED-COUNT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'IBANK FEED RECORDS'       TO TL-LABEL
           MOVE WS-FEED-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE INDEX-FILE
                 ACCTMAST-FILE
                 ESTMTFD-FILE
                 REPORT-FILE

           DISPLAY 'STSTM030 - INDEX READ  ' WS-READ-COUNT
           DISPLAY 'STSTM030 - UPDATED     ' WS-UPDATED-COUNT
           DISPLAY 'STSTM030 - FAILED      ' WS-FAILED-COUNT
           IF WS-FAILED-COUNT > ZERO OR WS-MISSING-ACCT-COUNT > ZERO
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
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SIX-SORT-CODE              TO DL-SORT-CODE
           MOVE SIX-ACCT-NO                TO DL-ACCT-NO
           MOVE SIX-STMT-NO                TO DL-STMT-NO
           MOVE SIX-PERIOD-TO              TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO DL-PERIOD-TO
           MOVE WS-RESULT                  TO DL-RESULT
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-INDEX.
      *================================================================*
           READ INDEX-FILE INTO SIX-INDEX-RECORD
           EVALUATE TRUE
               WHEN WS-INDEX-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-INDEX-EOF
                   SET WS-END-OF-INDEX     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-INDEX'  TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'INDEX READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-INDEX-STATUS    TO ABN-FILE-STATUS
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
