       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IBFD0201.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2001-11-05.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : IBFD0201                                            *
      * TITLE    : IBFEED02 - HOST TO INTERNET BANKING DAILY FEED      *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : IBFD0201                                            *
      * JCL      : IBFEED02 STEP IB010                                 *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (VSAM, SEQUENTIAL)    *
      *            MBK.TXNHIST - TODAY'S HARD POSTINGS (CURSOR)        *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : IBFEED   - FEED FILE (IBFEEDRC)                     *
      *----------------------------------------------------------------*
      * MATCH-MERGE OF THE ACCOUNT MASTER (KEY ORDER) WITH A CURSOR    *
      * OVER TXNHIST FOR THE BUSINESS DATE IN THE SAME KEY ORDER.      *
      * FOR EACH ACCOUNT: ONE 'B' RECORD, THEN ITS 'T' RECORDS.        *
      *                                                                *
      * MUST RUN AFTER PSEOD010 (HARD POSTING) - SEE CA-7 NETWORK      *
      * MBKEOD. THE CHANNEL REPLACES ITS BALANCES AND APPENDS THE      *
      * TRANSACTIONS; IT REJECTS THE WHOLE FILE IF THE TRAILER TOTALS  *
      * DO NOT MATCH.                                                  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2000-06-19 DKP  ORIGINAL VERSION (IBFD0101 - BALANCES ONLY)    *
      * 2001-11-05 DKP  IBFEED02 - TRANSACTION RECORDS FROM VSAM       *
      *                 TXNHIST                                        *
      * 2002-03-18 DKP  TRANSACTIONS READ FROM DB2 TXNHIST             *
      * 2006-03-14 AOS  NARRATIVE-2 AND CHANNEL CODE                   *
      * 2007-01-22 JNB  ACCOUNTS CLOSED TODAY INCLUDED                 *
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
           SELECT IBFEED-FILE      ASSIGN TO IBFEED
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-IBFEED-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  IBFEED-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  IBFEED-RECORD                   PIC X(250).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'IBFD0201'.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-IBFEED-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-ACCT-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-ACCT-EOF                 VALUE 'Y'.
           05  WS-TXN-EOF-IND              PIC X(01) VALUE 'N'.
               88  WS-TXN-EOF                  VALUE 'Y'.
      *
       01  WS-KEYS.
           05  WS-ACCT-KEY.
               10  WS-AK-SORT-CODE         PIC X(06).
               10  WS-AK-ACCT-NO           PIC X(08).
           05  WS-TXN-KEY.
               10  WS-TK-SORT-CODE         PIC X(06).
               10  WS-TK-ACCT-NO           PIC X(08).
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE-DB2             PIC X(10).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURR-DATE            PIC 9(08).
               10  WS-CURR-TIME            PIC 9(06).
               10  FILLER                  PIC X(07).
      *
       01  WS-COUNTERS.
           05  WS-ACCT-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BAL-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-TXN-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ORPHAN-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-HASH-LEDGER              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-HASH-TXN-AMT             PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
           EXEC SQL DECLARE TXNCSR CURSOR FOR
               SELECT SORT_CODE, ACCT_NO, POST_SEQ, TXN_CODE,
                      DR_CR_IND, TXN_AMT, RUNNING_BAL, CCY_CD,
                      VALUE_DATE, NARRATIVE_1, NARRATIVE_2,
                      CHANNEL_CD, EXT_REF
                 FROM MBK.TXNHIST
                WHERE POST_DATE = :WS-BUS-DATE-DB2
                ORDER BY SORT_CODE, ACCT_NO, POST_SEQ
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
           COPY IBFEEDRC.
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
           PERFORM 2000-PROCESS-ACCOUNT
               UNTIL WS-ACCT-EOF
           PERFORM 2900-SKIP-ORPHANS
               UNTIL WS-TXN-EOF
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       DATECARD-FILE
                OUTPUT IBFEED-FILE

           IF NOT WS-ACCTMAST-OK
           OR WS-IBFEED-STATUS NOT = '00'
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

           MOVE DTC-BUSINESS-DATE          TO WS-DATE-WORK
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-BUS-DATE-DB2
           END-STRING
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA

      *---- HEADER ----------------------------------------------------*
           MOVE SPACES                     TO IBF-FEED-RECORD
           SET IBF-HEADER-REC              TO TRUE
           MOVE 'IBFEED02'                 TO IBFH-FILE-ID
           MOVE DTC-BUSINESS-DATE          TO IBFH-BUSINESS-DATE
           MOVE WS-CURR-DATE               TO IBFH-CREATE-DATE
           MOVE WS-CURR-TIME               TO IBFH-CREATE-TIME
      *    FILE SEQUENCE = DAYS SINCE 01/01/2000. THE CHANNEL LOADER
      *    REJECTS A FILE WHOSE SEQUENCE IS NOT GREATER THAN THE LAST.
           COMPUTE IBFH-FILE-SEQ-NO =
                   FUNCTION INTEGER-OF-DATE (DTC-BUSINESS-DATE)
                 - FUNCTION INTEGER-OF-DATE (20000101)
           MOVE 'MBKHOST '                 TO IBFH-SOURCE-SYS
           PERFORM 8200-WRITE-FEED

           EXEC SQL OPEN TXNCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           PERFORM 8000-READ-ACCOUNT
           PERFORM 8100-FETCH-TXN
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
      *    TRANSACTIONS FOR ACCOUNTS NOT ON THE MASTER - SKIP THEM.
           PERFORM 2900-SKIP-ORPHANS
               UNTIL WS-TXN-EOF
                  OR WS-TXN-KEY NOT < WS-ACCT-KEY

      *    CLOSED ACCOUNTS ARE ONLY SENT ON THE DAY THEY CLOSE.
           IF ACM-CLOSED
           AND ACM-CLOSE-DATE NOT = DTC-BUSINESS-DATE
               CONTINUE
           ELSE
               PERFORM 2100-WRITE-BALANCE
               PERFORM 2200-WRITE-TRANSACTION
                   UNTIL WS-TXN-EOF
                      OR WS-TXN-KEY NOT = WS-ACCT-KEY
           END-IF

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       2100-WRITE-BALANCE.
      *================================================================*
           MOVE ACM-OD-LIMIT               TO WS-OD-LIMIT
           IF ACM-OD-EXPIRY-DATE NOT = ZERO
           AND ACM-OD-EXPIRY-DATE < DTC-BUSINESS-DATE
               MOVE ZERO                   TO WS-OD-LIMIT
           END-IF

           MOVE SPACES                     TO IBF-FEED-RECORD
           SET IBF-BALANCE-REC             TO TRUE
           MOVE ACM-SORT-CODE              TO IBFB-SORT-CODE
           MOVE ACM-ACCT-NO                TO IBFB-ACCT-NO
           MOVE ACM-CIF-NO                 TO IBFB-CIF-NO
           MOVE ACM-ACCT-TYPE-CD           TO IBFB-ACCT-TYPE-CD
           MOVE ACM-PROD-CD                TO IBFB-PROD-CD
           MOVE ACM-CCY-CD                 TO IBFB-CCY-CD
           MOVE ACM-ACCT-STATUS            TO IBFB-ACCT-STATUS
           MOVE ACM-LEDGER-BAL             TO IBFB-LEDGER-BAL
      *    CHANNEL "AVAILABLE" = CLEARED FUNDS + ARRANGED OVERDRAFT
           COMPUTE IBFB-AVAIL-BAL = ACM-AVAIL-BAL + WS-OD-LIMIT
           MOVE WS-OD-LIMIT                TO IBFB-OD-LIMIT
           MOVE ACM-INT-RATE-CR            TO IBFB-INT-RATE
           MOVE ACM-OPEN-DATE              TO IBFB-OPEN-DATE
           MOVE ACM-CLOSE-DATE             TO IBFB-CLOSE-DATE
           MOVE ACM-MATURITY-DATE          TO IBFB-MATURITY-DATE
           MOVE ACM-STMT-DAY               TO IBFB-STMT-DAY
           MOVE ACM-IBAN                   TO IBFB-IBAN
           PERFORM 8200-WRITE-FEED

           ADD 1                           TO WS-BAL-COUNT
           ADD ACM-LEDGER-BAL              TO WS-HASH-LEDGER
           .
      *================================================================*
       2200-WRITE-TRANSACTION.
      *================================================================*
           MOVE SPACES                     TO IBF-FEED-RECORD
           SET IBF-TXN-REC                 TO TRUE
           MOVE TXH-SORT-CODE              TO IBFT-SORT-CODE
           MOVE TXH-ACCT-NO                TO IBFT-ACCT-NO
           MOVE DTC-BUSINESS-DATE          TO IBFT-POST-DATE
           MOVE TXH-POST-SEQ               TO IBFT-POST-SEQ
           MOVE TXH-TXN-CODE               TO IBFT-TXN-CODE
           MOVE TXH-DR-CR-IND              TO IBFT-DR-CR-IND
           MOVE TXH-TXN-AMT                TO IBFT-TXN-AMT
           MOVE TXH-RUNNING-BAL            TO IBFT-RUNNING-BAL
           MOVE TXH-CCY-CD                 TO IBFT-CCY-CD
      *    DB2 DATE 'YYYY-MM-DD' TO CCYYMMDD
           STRING TXH-VALUE-DATE (1:4)
                  TXH-VALUE-DATE (6:2)
                  TXH-VALUE-DATE (9:2)
                  DELIMITED BY SIZE
                  INTO IBFT-VALUE-DATE
           END-STRING
           IF TXH-NARRATIVE-1-LEN > ZERO
               MOVE TXH-NARRATIVE-1-TEXT (1:TXH-NARRATIVE-1-LEN)
                                           TO IBFT-NARRATIVE-1
           END-IF
           IF TXH-NARRATIVE-2-LEN > ZERO
               MOVE TXH-NARRATIVE-2-TEXT (1:TXH-NARRATIVE-2-LEN)
                                           TO IBFT-NARRATIVE-2
           END-IF
           MOVE TXH-CHANNEL-CD             TO IBFT-CHANNEL-CD
           MOVE TXH-EXT-REF                TO IBFT-EXT-REF
           PERFORM 8200-WRITE-FEED

           ADD 1                           TO WS-TXN-COUNT
           ADD TXH-TXN-AMT                 TO WS-HASH-TXN-AMT

           PERFORM 8100-FETCH-TXN
           .
      *================================================================*
       2900-SKIP-ORPHANS.
      *================================================================*
           DISPLAY 'IBFD0201 W001 TXNHIST ROW WITH NO ACCOUNT: '
                   WS-TXN-KEY ' SEQ ' TXH-POST-SEQ
           ADD 1                           TO WS-ORPHAN-COUNT
           PERFORM 8100-FETCH-TXN
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL CLOSE TXNCSR END-EXEC

           MOVE SPACES                     TO IBF-FEED-RECORD
           SET IBF-TRAILER-REC             TO TRUE
           MOVE WS-BAL-COUNT               TO IBFZ-BAL-COUNT
           MOVE WS-TXN-COUNT               TO IBFZ-TXN-COUNT
           MOVE WS-HASH-LEDGER             TO IBFZ-HASH-LEDGER
           MOVE WS-HASH-TXN-AMT            TO IBFZ-HASH-TXN-AMT
           PERFORM 8200-WRITE-FEED

           CLOSE ACCTMAST-FILE
                 IBFEED-FILE

           DISPLAY 'IBFD0201 - ACCOUNTS READ       ' WS-ACCT-READ
           DISPLAY 'IBFD0201 - BALANCE RECORDS     ' WS-BAL-COUNT
           DISPLAY 'IBFD0201 - TRANSACTION RECORDS ' WS-TXN-COUNT
           DISPLAY 'IBFD0201 - ORPHAN TRANSACTIONS ' WS-ORPHAN-COUNT

           IF WS-ORPHAN-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       8000-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-ACCT-READ
                   MOVE ACM-KEY            TO WS-ACCT-KEY
               WHEN WS-ACCTMAST-EOF
                   SET WS-ACCT-EOF         TO TRUE
                   MOVE HIGH-VALUES        TO WS-ACCT-KEY
               WHEN OTHER
                   MOVE '8000-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8100-FETCH-TXN.
      *================================================================*
           EXEC SQL
               FETCH TXNCSR
                INTO :TXH-SORT-CODE, :TXH-ACCT-NO, :TXH-POST-SEQ,
                     :TXH-TXN-CODE, :TXH-DR-CR-IND, :TXH-TXN-AMT,
                     :TXH-RUNNING-BAL, :TXH-CCY-CD, :TXH-VALUE-DATE,
                     :TXH-NARRATIVE-1, :TXH-NARRATIVE-2,
                     :TXH-CHANNEL-CD, :TXH-EXT-REF
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   MOVE TXH-SORT-CODE      TO WS-TK-SORT-CODE
                   MOVE TXH-ACCT-NO        TO WS-TK-ACCT-NO
               WHEN 100
                   SET WS-TXN-EOF          TO TRUE
                   MOVE HIGH-VALUES        TO WS-TXN-KEY
               WHEN OTHER
                   MOVE '8100-FETCH-TXN'   TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       8200-WRITE-FEED.
      *================================================================*
           WRITE IBFEED-RECORD             FROM IBF-FEED-RECORD
           IF WS-IBFEED-STATUS NOT = '00'
               MOVE '8200-WRITE-FEED'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'IBFEED WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-IBFEED-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
