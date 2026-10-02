       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDHLX010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-05-17.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDHLX010                                            *
      * TITLE    : FUNDS HOLDS - EXPIRY AND RECONCILIATION             *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : DDCLS01 STEP DC005                                  *
      *----------------------------------------------------------------*
      * INPUT    : DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : HOLDS    - FUNDS HOLD FILE (VSAM KSDS, SEQ I-O)     *
      *            ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM)   *
      * OUTPUT   : REPORT   - HOLDS EXPIRED / MISMATCHES (FBA 133)     *
      *----------------------------------------------------------------*
      * 1. EVERY ACTIVE HOLD WITH AN EXPIRY DATE ON OR BEFORE TODAY IS *
      *    MARKED EXPIRED AND ITS AMOUNT IS RETURNED TO THE AVAILABLE  *
      *    BALANCE (ACM-AVAIL-BAL) AND TAKEN OFF ACM-HOLD-AMT.         *
      * 2. FOR EVERY ACCOUNT WITH HOLD RECORDS, THE SUM OF THE ACTIVE  *
      *    HOLDS IS COMPARED WITH ACM-HOLD-AMT. A DIFFERENCE IS        *
      *    REPORTED BUT NOT CORRECTED - DDA OPERATIONS INVESTIGATE.    *
      *    (THE TELLER SYSTEM STILL UPDATES ACM-HOLD-AMT DIRECTLY FOR  *
      *    CHEQUE HOLDS - PIR 2010/091 - SO SOME MISMATCHES ARE        *
      *    EXPECTED UNTIL THE TELLER IS CHANGED TO CALL DDHLD01.)      *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-05-17 JNB  ORIGINAL VERSION                               *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HOLDS-FILE       ASSIGN TO HOLDS
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS HLD-KEY
                                   FILE STATUS IS WS-HOLDS-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  HOLDS-FILE.
           COPY DDHLDREC.
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDHLX010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-HOLDS-STATUS             PIC X(02).
               88  WS-HOLDS-OK                 VALUE '00'.
               88  WS-HOLDS-EOF                VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-HOLDS             VALUE 'Y'.
           05  WS-ACCT-FOUND-IND           PIC X(01) VALUE 'N'.
               88  WS-ACCT-FOUND               VALUE 'Y'.
           05  WS-ACCT-CHANGED-IND         PIC X(01) VALUE 'N'.
               88  WS-ACCT-CHANGED             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CURR-ACCT                PIC X(14) VALUE LOW-VALUES.
           05  WS-ACTIVE-TOTAL             PIC S9(13)V99 COMP-3.
           05  WS-EXPIRED-TOTAL            PIC S9(13)V99 COMP-3.
           05  WS-DIFFERENCE               PIC S9(13)V99 COMP-3.
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCOUNT-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXPIRED-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-MISMATCH-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-ORPHAN-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-EXPIRED            PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'DDHLX010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - FUNDS HOLDS EXPIRY / RECONCILE'.
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
           05  FILLER            PIC X(06) VALUE 'SEQ'.
           05  FILLER            PIC X(10) VALUE 'ACTION'.
           05  FILLER            PIC X(06) VALUE 'RSN'.
           05  FILLER            PIC X(18) VALUE '       HOLD AMOUNT'.
           05  FILLER            PIC X(18) VALUE '    ACM-HOLD-AMT'.
           05  FILLER            PIC X(18) VALUE '   SUM OF ACTIVE'.
           05  FILLER            PIC X(38) VALUE '  NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SEQ                      PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(09).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-REASON                   PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-HOLD-AMT                 PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  DL-ACM-HOLD                 PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  DL-SUM-ACTIVE               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(36).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
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
           PERFORM 2000-PROCESS-HOLD
               UNTIL WS-END-OF-HOLDS
           PERFORM 2900-END-ACCOUNT THRU 2900-EXIT
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  DATECARD-FILE
                I-O    HOLDS-FILE
                       ACCTMAST-FILE
                OUTPUT REPORT-FILE
           IF NOT WS-HOLDS-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-HOLDS-STATUS ' ' WS-ACCTMAST-STATUS ' '
                      WS-REPORT-STATUS ' ' WS-DATECARD-STATUS
                      DELIMITED BY SIZE
                      INTO ABN-KEY-DATA
               END-STRING
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

           PERFORM 8000-READ-HOLD
           .
      *================================================================*
       2000-PROCESS-HOLD.
      *================================================================*
           IF HLD-KEY (1:14) NOT = WS-CURR-ACCT
               PERFORM 2900-END-ACCOUNT THRU 2900-EXIT
               PERFORM 2100-START-ACCOUNT
           END-IF

           IF NOT HLD-CONTROL-REC AND HLD-ACTIVE
               IF HLD-EXPIRY-DATE NUMERIC
               AND HLD-EXPIRY-DATE NOT = ZERO
               AND HLD-EXPIRY-DATE <= WS-BUS-DATE
                   PERFORM 2200-EXPIRE-HOLD
               ELSE
                   ADD HLD-AMOUNT          TO WS-ACTIVE-TOTAL
               END-IF
           END-IF

           PERFORM 8000-READ-HOLD
           .
      *================================================================*
       2100-START-ACCOUNT.
      *================================================================*
           MOVE HLD-KEY (1:14)             TO WS-CURR-ACCT
           MOVE ZERO                       TO WS-ACTIVE-TOTAL
                                              WS-EXPIRED-TOTAL
           MOVE 'N'                        TO WS-ACCT-CHANGED-IND
           ADD 1                           TO WS-ACCOUNT-COUNT

           MOVE HLD-SORT-CODE              TO ACM-SORT-CODE
           MOVE HLD-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   SET WS-ACCT-FOUND       TO TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'N'                TO WS-ACCT-FOUND-IND
                   ADD 1                   TO WS-ORPHAN-COUNT
               WHEN OTHER
                   MOVE '2100-START-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE WS-CURR-ACCT       TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2200-EXPIRE-HOLD.
      *================================================================*
           SET HLD-EXPIRED                 TO TRUE
           MOVE WS-BUS-DATE                TO HLD-RELEASED-DATE
           MOVE WS-PROGRAM-ID              TO HLD-RELEASED-USER
           REWRITE HLD-HOLD-RECORD
           IF NOT WS-HOLDS-OK
               MOVE '2200-EXPIRE-HOLD'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'HOLDS REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-HOLDS-STATUS        TO ABN-FILE-STATUS
               MOVE HLD-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           ADD HLD-AMOUNT                  TO WS-EXPIRED-TOTAL
           ADD 1                           TO WS-EXPIRED-COUNT
           ADD HLD-AMOUNT                  TO WS-TOTAL-EXPIRED

           IF WS-ACCT-FOUND
               SUBTRACT HLD-AMOUNT         FROM ACM-HOLD-AMT
               ADD HLD-AMOUNT              TO ACM-AVAIL-BAL
               IF ACM-HOLD-AMT < ZERO
                   ADD ACM-HOLD-AMT        TO ACM-AVAIL-BAL
                   MOVE ZERO               TO ACM-HOLD-AMT
               END-IF
               SET WS-ACCT-CHANGED         TO TRUE
           END-IF

           MOVE HLD-SORT-CODE              TO DL-SORT-CODE
           MOVE HLD-ACCT-NO                TO DL-ACCT-NO
           MOVE HLD-SEQ                    TO DL-SEQ
           MOVE 'EXPIRED'                  TO DL-ACTION
           MOVE HLD-REASON-CD              TO DL-REASON
           MOVE HLD-AMOUNT                 TO DL-HOLD-AMT
           MOVE ZERO                       TO DL-ACM-HOLD
                                              DL-SUM-ACTIVE
           MOVE HLD-NARRATIVE              TO DL-NOTE
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2900-END-ACCOUNT.
      *================================================================*
           IF WS-CURR-ACCT = LOW-VALUES
               GO TO 2900-EXIT
           END-IF

           IF WS-ACCT-FOUND
      *        DECREMENT THE ACTIVE COUNT ON THE CONTROL RECORD IS NOT
      *        DONE HERE - DDHLD01 CORRECTS IT WHEN IT NEXT PLACES A
      *        HOLD (THE COUNT IS ONLY A GUARD).
               IF WS-ACCT-CHANGED
                   MOVE WS-BUS-DATE        TO ACM-LAST-MAINT-DATE
                   MOVE ZERO               TO ACM-LAST-MAINT-TIME
                   MOVE WS-PROGRAM-ID      TO ACM-LAST-MAINT-USER
                   REWRITE ACM-ACCOUNT-RECORD
                   IF NOT WS-ACCTMAST-OK
                       MOVE '2900-END-ACCOUNT' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                       MOVE WS-CURR-ACCT   TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
               END-IF

               IF ACM-HOLD-AMT NOT = WS-ACTIVE-TOTAL
                   ADD 1                   TO WS-MISMATCH-COUNT
                   COMPUTE WS-DIFFERENCE =
                           ACM-HOLD-AMT - WS-ACTIVE-TOTAL
                   MOVE WS-CURR-ACCT (1:6) TO DL-SORT-CODE
                   MOVE WS-CURR-ACCT (7:8) TO DL-ACCT-NO
                   MOVE SPACES             TO DL-SEQ DL-REASON
                   MOVE 'MISMATCH'         TO DL-ACTION
                   MOVE WS-DIFFERENCE      TO DL-HOLD-AMT
                   MOVE ACM-HOLD-AMT       TO DL-ACM-HOLD
                   MOVE WS-ACTIVE-TOTAL    TO DL-SUM-ACTIVE
                   MOVE 'HOLD TOTAL DIFFERS - REFER DDA OPS'
                                           TO DL-NOTE
                   PERFORM 7100-REPORT-DETAIL
               END-IF
           ELSE
               MOVE WS-CURR-ACCT (1:6)     TO DL-SORT-CODE
               MOVE WS-CURR-ACCT (7:8)     TO DL-ACCT-NO
               MOVE SPACES                 TO DL-SEQ DL-REASON
               MOVE 'ORPHAN'               TO DL-ACTION
               MOVE WS-ACTIVE-TOTAL        TO DL-HOLD-AMT
               MOVE ZERO                   TO DL-ACM-HOLD
                                              DL-SUM-ACTIVE
               MOVE 'HOLDS EXIST FOR AN UNKNOWN ACCOUNT'
                                           TO DL-NOTE
               PERFORM 7100-REPORT-DETAIL
           END-IF
           .
       2900-EXIT.
           EXIT.
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'HOLD RECORDS READ'        TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS WITH HOLDS'      TO TL-LABEL
           MOVE WS-ACCOUNT-COUNT           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'HOLDS EXPIRED'            TO TL-LABEL
           MOVE WS-EXPIRED-COUNT           TO TL-COUNT
           MOVE WS-TOTAL-EXPIRED           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'HOLD TOTAL MISMATCHES'    TO TL-LABEL
           MOVE WS-MISMATCH-COUNT          TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ORPHAN HOLD ACCOUNTS'     TO TL-LABEL
           MOVE WS-ORPHAN-COUNT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE HOLDS-FILE
                 ACCTMAST-FILE
                 REPORT-FILE

           DISPLAY 'DDHLX010 - HOLDS READ    ' WS-READ-COUNT
           DISPLAY 'DDHLX010 - EXPIRED       ' WS-EXPIRED-COUNT
           DISPLAY 'DDHLX010 - MISMATCHES    ' WS-MISMATCH-COUNT
           IF WS-MISMATCH-COUNT > ZERO OR WS-ORPHAN-COUNT > ZERO
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
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-HOLD.
      *================================================================*
           READ HOLDS-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-HOLDS-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-HOLDS-EOF
                   SET WS-END-OF-HOLDS     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-HOLD'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'HOLDS READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-HOLDS-STATUS    TO ABN-FILE-STATUS
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
