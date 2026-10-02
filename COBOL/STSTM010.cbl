       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STSTM010.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2004-03-01.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : STSTM010                                            *
      * TITLE    : STATEMENT CYCLE - SELECT ACCOUNTS DUE A STATEMENT   *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : STMT01 STEP ST010                                   *
      *----------------------------------------------------------------*
      * *** STUB IN THIS LIBRARY ***                                   *
      * ONLY THE SELECTION STEP OF THE STATEMENT CYCLE IS HELD HERE.   *
      * THE REST OF THE CYCLE LIVES IN THE DOCUMENT SERVICES LIBRARY   *
      * MBK.PRD.DOC1.* AND IS OWNED BY THE DOCUMENT SERVICES TEAM:     *
      *   STMT02  EXTRACT TXNHIST FOR EACH REQUEST, COMPOSE WITH DOC1, *
      *           PRODUCE AFP PRINT STREAM + CMOD ARCHIVE INDEX        *
      *   STMT03  RETURN FILE - UPDATES ACM-LAST-STMT-DATE/NO AND      *
      *           SENDS DOC_KEY TO IBANK (STMT_HDR TABLE)              *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (SEQUENTIAL)          *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : STMTREQ  - STATEMENT REQUESTS FOR STMT02 (FB 100)   *
      *----------------------------------------------------------------*
      * AN ACCOUNT IS DUE WHEN ITS STATEMENT DAY = TODAY'S DAY OF      *
      * MONTH AND ITS CYCLE (M/Q/A) FALLS DUE THIS MONTH. STATEMENT    *
      * DAYS 29-31 ARE PRODUCED ON THE LAST BUSINESS DAY OF SHORTER    *
      * MONTHS - NOT YET IMPLEMENTED (PIR 2005/019): THEY ARE SKIPPED. *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2004-03-01 SAT  ORIGINAL VERSION (SPLIT FROM DD0900)           *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT STMTREQ-FILE     ASSIGN TO STMTREQ
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-STMTREQ-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  STMTREQ-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STMTREQ-RECORD                  PIC X(100).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'STSTM010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-STMTREQ-STATUS           PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-R REDEFINES WS-BUS-DATE.
               10  WS-BD-CCYY              PIC 9(04).
               10  WS-BD-MM                PIC 9(02).
               10  WS-BD-DD                PIC 9(02).
           05  WS-QUOTIENT                 PIC 9(02).
           05  WS-REMAINDER                PIC 9(02).
           05  WS-DUE-IND                  PIC X(01).
               88  WS-DUE                      VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-REQUEST-COUNT            PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-STMT-REQUEST.
           05  SRQ-SORT-CODE               PIC X(06).
           05  SRQ-ACCT-NO                 PIC X(08).
           05  SRQ-CIF-NO                  PIC X(12).
           05  SRQ-STMT-NO                 PIC 9(05).
           05  SRQ-PERIOD-FROM             PIC 9(08).
           05  SRQ-PERIOD-TO               PIC 9(08).
           05  SRQ-CLOSING-BAL             PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  SRQ-DOC-KEY                 PIC X(30).
           05  FILLER                      PIC X(07).
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

           OPEN INPUT  ACCTMAST-FILE
                       DATECARD-FILE
                OUTPUT STMTREQ-FILE
           IF NOT WS-ACCTMAST-OK
           OR WS-STMTREQ-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '0000-MAIN'            TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
               MOVE '0000-MAIN'            TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE

           PERFORM 1000-READ-ACCOUNT
           PERFORM 2000-CHECK-ACCOUNT
               UNTIL WS-ACCTMAST-EOF

           CLOSE ACCTMAST-FILE
                 STMTREQ-FILE

           DISPLAY 'STSTM010 - ACCOUNTS READ       ' WS-READ-COUNT
           DISPLAY 'STSTM010 - STATEMENTS REQUESTED ' WS-REQUEST-COUNT
           GOBACK
           .
      *================================================================*
       1000-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           IF WS-ACCTMAST-OK
               ADD 1                       TO WS-READ-COUNT
           ELSE
               IF NOT WS-ACCTMAST-EOF
                   MOVE '1000-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               END-IF
           END-IF
           .
      *================================================================*
       2000-CHECK-ACCOUNT.
      *================================================================*
           MOVE 'N'                        TO WS-DUE-IND

           IF (ACM-OPEN OR ACM-DORMANT OR ACM-FROZEN)
           AND ACM-STMT-DAY = WS-BD-DD
               EVALUATE TRUE
                   WHEN ACM-STMT-MONTHLY
                       SET WS-DUE          TO TRUE
                   WHEN ACM-STMT-QUARTERLY
                       DIVIDE WS-BD-MM BY 3 GIVING WS-QUOTIENT
                                          REMAINDER WS-REMAINDER
                       IF WS-REMAINDER = 0
                           SET WS-DUE      TO TRUE
                       END-IF
                   WHEN ACM-STMT-ANNUAL
                       IF WS-BD-MM = ACM-OPEN-DATE (5:2)
                           SET WS-DUE      TO TRUE
                       END-IF
               END-EVALUATE
           END-IF

           IF WS-DUE
               MOVE SPACES                 TO WS-STMT-REQUEST
               MOVE ACM-SORT-CODE          TO SRQ-SORT-CODE
               MOVE ACM-ACCT-NO            TO SRQ-ACCT-NO
               MOVE ACM-CIF-NO             TO SRQ-CIF-NO
               COMPUTE SRQ-STMT-NO = ACM-LAST-STMT-NO + 1
               IF ACM-LAST-STMT-DATE = ZERO
                   MOVE ACM-OPEN-DATE      TO SRQ-PERIOD-FROM
               ELSE
                   COMPUTE SRQ-PERIOD-FROM =
                       FUNCTION DATE-OF-INTEGER (
                       FUNCTION INTEGER-OF-DATE (ACM-LAST-STMT-DATE)
                       + 1)
               END-IF
               MOVE WS-BUS-DATE            TO SRQ-PERIOD-TO
               MOVE ACM-LEDGER-BAL         TO SRQ-CLOSING-BAL
               STRING 'STMT/' ACM-SORT-CODE ACM-ACCT-NO '/'
                      WS-BUS-DATE
                      DELIMITED BY SIZE
                      INTO SRQ-DOC-KEY
               END-STRING
               WRITE STMTREQ-RECORD        FROM WS-STMT-REQUEST
               ADD 1                       TO WS-REQUEST-COUNT
           END-IF

           PERFORM 1000-READ-ACCOUNT
           .
      *
           COPY ABNDPROC.
