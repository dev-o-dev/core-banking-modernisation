       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PSRCN010.
       AUTHOR.        S A TURNER.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2004-11-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PSRCN010                                            *
      * TITLE    : MEMO LOG / ACCOUNT MASTER MEMO TOTAL RECONCILIATION *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : PSEOD01 STEP PS005 (AFTER ONLINE CLOSE, BEFORE THE  *
      *            MEMO LOG IS SORTED FOR POSTING)                     *
      *----------------------------------------------------------------*
      * INPUT    : MEMOSRT  - TODAY'S MEMO LOG, SORTED BY ACCOUNT      *
      *                       (STEP PS004 SORT OF THE ESDS UNLOAD)     *
      *            ACCTMAST - DDA ACCOUNT MASTER (SEQUENTIAL)          *
      *            PARMIN   - 'FIX=Y' TO CORRECT THE MASTER (DEFAULT N)*
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : ACCTMAST - REWRITTEN ONLY WHEN FIX=Y                *
      *            BREAKS   - ACCOUNTS OUT OF BALANCE (FB 120)         *
      *            REPORT   - RECONCILIATION REPORT (FBA 133)          *
      *----------------------------------------------------------------*
      * PSMEMO1 UPDATES ACM-MEMO-DR-TOT / ACM-MEMO-CR-TOT AND WRITES   *
      * THE MEMO LOG IN ONE CICS UNIT OF WORK, SO THEY SHOULD ALWAYS   *
      * AGREE. THEY HAVE NOT ALWAYS AGREED:                            *
      *   - PIR 2004/077: TELLER TERMINAL TIMEOUT DURING SYNCPOINT     *
      *     LEFT THE MASTER UPDATED WITHOUT THE LOG RECORD (CICS TS    *
      *     2.3 APAR PQ88123 - FIXED, BUT THIS CHECK STAYED)           *
      *   - PSREV01 REVERSALS WRITE A CONTRA LOG RECORD AND REDUCE THE *
      *     TOTALS - A FAILED REVERSAL SHOWS HERE                      *
      * PSEOD010 HARD POSTS FROM THE LOG AND THEN RESETS THE MEMO      *
      * TOTALS TO ZERO, SO A BREAK NOT CAUGHT HERE IS LOST AND THE     *
      * AVAILABLE BALANCE IS WRONG FROM TOMORROW.                      *
      *                                                                *
      * MATCH-MERGE OF THE SORTED LOG AGAINST THE MASTER IN KEY ORDER. *
      * WITH FIX=Y THE MASTER MEMO TOTALS ARE SET TO THE LOG TOTALS    *
      * AND THE AVAILABLE BALANCE ADJUSTED BY THE SAME DIFFERENCE.     *
      * DDA OPERATIONS DECIDE - THE JOB NORMALLY RUNS FIX=N AND STOPS  *
      * THE SCHEDULE (RC 8) IF THERE ARE MORE THAN WS-MAX-BREAKS.      *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2004-11-15 SAT  ORIGINAL VERSION (PIR 2004/077)                *
      * 2010-06-28 JNB  REVERSAL CONTRA RECORDS ('REV ') NET OFF       *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MEMOSRT-FILE     ASSIGN TO MEMOSRT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-MEMOSRT-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
           SELECT BREAKS-FILE      ASSIGN TO BREAKS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-BREAKS-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  MEMOSRT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  MEMOSRT-RECORD                  PIC X(200).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD                   PIC X(80).
       FD  BREAKS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BREAKS-RECORD                   PIC X(120).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PSRCN010'.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-BREAKS               PIC S9(05) COMP-3 VALUE +25.
      *
       01  WS-FILE-STATUSES.
           05  WS-MEMOSRT-STATUS           PIC X(02).
               88  WS-MEMOSRT-OK               VALUE '00'.
               88  WS-MEMOSRT-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-PARMIN-STATUS            PIC X(02).
           05  WS-BREAKS-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-FIX-IND                  PIC X(01) VALUE 'N'.
               88  WS-FIX-MODE                 VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-LOG-KEY                  PIC X(14).
           05  WS-MST-KEY                  PIC X(14).
           05  WS-PREV-LOG-KEY             PIC X(14) VALUE LOW-VALUES.
           05  WS-LOG-DR                   PIC S9(13)V99 COMP-3.
           05  WS-LOG-CR                   PIC S9(13)V99 COMP-3.
           05  WS-DIFF-DR                  PIC S9(13)V99 COMP-3.
           05  WS-DIFF-CR                  PIC S9(13)V99 COMP-3.
           05  WS-AVAIL-ADJ                PIC S9(13)V99 COMP-3.
           05  WS-BREAK-TYPE               PIC X(04).
      *
       01  WS-BREAK.
           05  BK-BUS-DATE                 PIC 9(08).
           05  BK-SORT-CODE                PIC X(06).
           05  BK-ACCT-NO                  PIC X(08).
           05  BK-TYPE                     PIC X(04).
           05  BK-MASTER-DR                PIC -9(13).99.
           05  BK-LOG-DR                   PIC -9(13).99.
           05  BK-MASTER-CR                PIC -9(13).99.
           05  BK-LOG-CR                   PIC -9(13).99.
           05  BK-FIXED-IND                PIC X(01).
           05  FILLER                      PIC X(25).
      *
       01  WS-COUNTERS.
           05  WS-LOG-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOG-HARD-IGNORED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-MASTER-READ              PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-WITH-MEMO          PIC S9(09) COMP-3 VALUE 0.
           05  WS-BREAKS                   PIC S9(09) COMP-3 VALUE 0.
           05  WS-ORPHAN-LOG               PIC S9(09) COMP-3 VALUE 0.
           05  WS-FIXED                    PIC S9(09) COMP-3 VALUE 0.
           05  WS-SEQ-ERRORS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-LOG-DR             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-LOG-CR             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-MST-DR             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-MST-CR             PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'PSRCN010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - MEMO TOTAL RECONCILIATION'.
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
           05  FILLER            PIC X(06) VALUE 'TYPE'.
           05  FILLER            PIC X(18) VALUE '  MASTER MEMO DR'.
           05  FILLER            PIC X(18) VALUE '     LOG MEMO DR'.
           05  FILLER            PIC X(18) VALUE '  MASTER MEMO CR'.
           05  FILLER            PIC X(18) VALUE '     LOG MEMO CR'.
           05  FILLER            PIC X(35) VALUE 'FIXED'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TYPE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-MST-DR                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-LOG-DR                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-MST-CR                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-LOG-CR                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-FIXED                    PIC X(03).
           05  FILLER                      PIC X(30) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY PSTXNREC.
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
           PERFORM 2000-MATCH
               UNTIL WS-LOG-KEY = HIGH-VALUES
                 AND WS-MST-KEY = HIGH-VALUES
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  MEMOSRT-FILE
                       PARMIN-FILE
                       DATECARD-FILE
                OUTPUT BREAKS-FILE
                       REPORT-FILE
           READ PARMIN-FILE
           IF WS-PARMIN-STATUS = '00'
           AND PARMIN-RECORD (1:5) = 'FIX=Y'
               SET WS-FIX-MODE             TO TRUE
           END-IF
           CLOSE PARMIN-FILE
           IF WS-FIX-MODE
               OPEN I-O ACCTMAST-FILE
           ELSE
               OPEN INPUT ACCTMAST-FILE
           END-IF
           IF NOT WS-MEMOSRT-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-BREAKS-STATUS NOT = '00'
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
           DISPLAY 'PSRCN010 - FIX MODE ' WS-FIX-IND
           PERFORM 8000-READ-LOG
           PERFORM 8100-READ-MASTER
           .
      *================================================================*
       2000-MATCH.
      *================================================================*
           EVALUATE TRUE
               WHEN WS-LOG-KEY < WS-MST-KEY
      *            LOG RECORDS FOR AN ACCOUNT NOT ON THE MASTER.
                   PERFORM 2100-SUM-LOG
                   ADD 1                   TO WS-ORPHAN-LOG
                   MOVE 'ORPH'             TO WS-BREAK-TYPE
                   MOVE WS-PREV-LOG-KEY (1:6) TO BK-SORT-CODE
                                                 DL-SORT-CODE
                   MOVE WS-PREV-LOG-KEY (7:8) TO BK-ACCT-NO
                                                 DL-ACCT-NO
                   MOVE ZERO               TO WS-DIFF-DR WS-DIFF-CR
                   PERFORM 2500-WRITE-BREAK-ORPHAN
               WHEN WS-LOG-KEY = WS-MST-KEY
                   PERFORM 2100-SUM-LOG
                   PERFORM 2200-COMPARE THRU 2200-EXIT
                   PERFORM 8100-READ-MASTER
               WHEN OTHER
      *            MASTER WITH NO LOG - TOTALS MUST BE ZERO.
                   MOVE ZERO               TO WS-LOG-DR WS-LOG-CR
                   PERFORM 2200-COMPARE THRU 2200-EXIT
                   PERFORM 8100-READ-MASTER
           END-EVALUATE
           .
      *================================================================*
       2100-SUM-LOG.
      *================================================================*
           MOVE ZERO                       TO WS-LOG-DR WS-LOG-CR
           MOVE WS-LOG-KEY                 TO WS-PREV-LOG-KEY
           PERFORM UNTIL WS-LOG-KEY NOT = WS-PREV-LOG-KEY
               IF PST-MEMO-POST AND NOT PST-REJECTED
                   IF PST-DEBIT
                       ADD PST-AMOUNT      TO WS-LOG-DR
                                              WS-TOTAL-LOG-DR
                   ELSE
                       ADD PST-AMOUNT      TO WS-LOG-CR
                                              WS-TOTAL-LOG-CR
                   END-IF
               ELSE
                   ADD 1                   TO WS-LOG-HARD-IGNORED
               END-IF
               PERFORM 8000-READ-LOG
           END-PERFORM
           .
      *================================================================*
       2200-COMPARE.
      *================================================================*
           ADD ACM-MEMO-DR-TOT             TO WS-TOTAL-MST-DR
           ADD ACM-MEMO-CR-TOT             TO WS-TOTAL-MST-CR
           IF WS-LOG-DR NOT = ZERO OR WS-LOG-CR NOT = ZERO
               ADD 1                       TO WS-ACCTS-WITH-MEMO
           END-IF
           IF ACM-MEMO-DR-TOT = WS-LOG-DR
           AND ACM-MEMO-CR-TOT = WS-LOG-CR
               GO TO 2200-EXIT
           END-IF

           COMPUTE WS-DIFF-DR = WS-LOG-DR - ACM-MEMO-DR-TOT
           COMPUTE WS-DIFF-CR = WS-LOG-CR - ACM-MEMO-CR-TOT
           EVALUATE TRUE
               WHEN WS-LOG-DR = ZERO AND WS-LOG-CR = ZERO
                   MOVE 'NLOG'             TO WS-BREAK-TYPE
               WHEN WS-DIFF-DR NOT = ZERO AND WS-DIFF-CR NOT = ZERO
                   MOVE 'BOTH'             TO WS-BREAK-TYPE
               WHEN WS-DIFF-DR NOT = ZERO
                   MOVE 'DR  '             TO WS-BREAK-TYPE
               WHEN OTHER
                   MOVE 'CR  '             TO WS-BREAK-TYPE
           END-EVALUATE
           ADD 1                           TO WS-BREAKS
           MOVE ACM-SORT-CODE              TO BK-SORT-CODE DL-SORT-CODE
           MOVE ACM-ACCT-NO                TO BK-ACCT-NO DL-ACCT-NO
           MOVE ACM-MEMO-DR-TOT            TO BK-MASTER-DR DL-MST-DR
           MOVE ACM-MEMO-CR-TOT            TO BK-MASTER-CR DL-MST-CR
           MOVE WS-LOG-DR                  TO BK-LOG-DR DL-LOG-DR
           MOVE WS-LOG-CR                  TO BK-LOG-CR DL-LOG-CR

           IF WS-FIX-MODE
      *        AVAILABLE = LEDGER - MEMO DR + MEMO CR (- HOLDS ETC),
      *        SO MOVE IT BY THE SAME CORRECTION.
               COMPUTE WS-AVAIL-ADJ = WS-DIFF-CR - WS-DIFF-DR
               ADD WS-AVAIL-ADJ            TO ACM-AVAIL-BAL
               MOVE WS-LOG-DR              TO ACM-MEMO-DR-TOT
               MOVE WS-LOG-CR              TO ACM-MEMO-CR-TOT
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '2200-COMPARE'     TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
               ADD 1                       TO WS-FIXED
               MOVE 'Y'                    TO BK-FIXED-IND
               MOVE 'YES'                  TO DL-FIXED
           ELSE
               MOVE 'N'                    TO BK-FIXED-IND
               MOVE 'NO '                  TO DL-FIXED
           END-IF
           PERFORM 2400-WRITE-BREAK
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2400-WRITE-BREAK.
      *================================================================*
           MOVE WS-BUS-DATE                TO BK-BUS-DATE
           MOVE WS-BREAK-TYPE              TO BK-TYPE DL-TYPE
           WRITE BREAKS-RECORD             FROM WS-BREAK
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       2500-WRITE-BREAK-ORPHAN.
      *================================================================*
           MOVE ZERO                       TO BK-MASTER-DR BK-MASTER-CR
                                              DL-MST-DR DL-MST-CR
           MOVE WS-LOG-DR                  TO BK-LOG-DR DL-LOG-DR
           MOVE WS-LOG-CR                  TO BK-LOG-CR DL-LOG-CR
           MOVE 'N'                        TO BK-FIXED-IND
           MOVE 'NO '                      TO DL-FIXED
           PERFORM 2400-WRITE-BREAK
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'MEMO LOG RECORDS READ'    TO TL-LABEL
           MOVE WS-LOG-READ                TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  NOT MEMO / REJECTED (IGNORED)' TO TL-LABEL
           MOVE WS-LOG-HARD-IGNORED        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNT MASTER RECORDS READ' TO TL-LABEL
           MOVE WS-MASTER-READ             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS WITH MEMO ACTIVITY' TO TL-LABEL
           MOVE WS-ACCTS-WITH-MEMO         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MEMO DEBITS - LOG'        TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-LOG-DR            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MEMO DEBITS - MASTER'     TO TL-LABEL
           MOVE WS-TOTAL-MST-DR            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MEMO CREDITS - LOG'       TO TL-LABEL
           MOVE WS-TOTAL-LOG-CR            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MEMO CREDITS - MASTER'    TO TL-LABEL
           MOVE WS-TOTAL-MST-CR            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS OUT OF BALANCE'  TO TL-LABEL
           MOVE WS-BREAKS                  TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LOG ENTRIES FOR UNKNOWN ACCOUNTS' TO TL-LABEL
           MOVE WS-ORPHAN-LOG              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CORRECTED (FIX=Y)'        TO TL-LABEL
           MOVE WS-FIXED                   TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LOG SEQUENCE ERRORS'      TO TL-LABEL
           MOVE WS-SEQ-ERRORS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE MEMOSRT-FILE ACCTMAST-FILE BREAKS-FILE REPORT-FILE
           DISPLAY 'PSRCN010 - BREAKS ' WS-BREAKS
                   ' ORPHANS ' WS-ORPHAN-LOG ' FIXED ' WS-FIXED
           EVALUATE TRUE
               WHEN WS-BREAKS + WS-ORPHAN-LOG > WS-MAX-BREAKS
               AND NOT WS-FIX-MODE
                   MOVE 8                  TO RETURN-CODE
               WHEN WS-BREAKS > ZERO OR WS-ORPHAN-LOG > ZERO
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
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
       8000-READ-LOG.
      *================================================================*
           READ MEMOSRT-FILE INTO PST-POSTING-RECORD
           EVALUATE TRUE
               WHEN WS-MEMOSRT-OK
                   ADD 1                   TO WS-LOG-READ
                   MOVE PST-SORT-CODE      TO WS-LOG-KEY (1:6)
                   MOVE PST-ACCT-NO        TO WS-LOG-KEY (7:8)
                   IF WS-LOG-KEY < WS-PREV-LOG-KEY
                       ADD 1               TO WS-SEQ-ERRORS
                       MOVE '8000-READ-LOG' TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'MEMO LOG NOT IN ACCOUNT ORDER'
                                           TO ABN-MESSAGE
                       MOVE WS-LOG-KEY     TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
               WHEN WS-MEMOSRT-EOF
                   MOVE HIGH-VALUES        TO WS-LOG-KEY
               WHEN OTHER
                   MOVE '8000-READ-LOG'    TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'MEMOSRT READ FAILED' TO ABN-MESSAGE
                   MOVE WS-MEMOSRT-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8100-READ-MASTER.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-MASTER-READ
                   MOVE ACM-KEY            TO WS-MST-KEY
               WHEN WS-ACCTMAST-EOF
                   MOVE HIGH-VALUES        TO WS-MST-KEY
               WHEN OTHER
                   MOVE '8100-READ-MASTER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
