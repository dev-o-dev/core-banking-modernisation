       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PSEOD010.
       AUTHOR.        G H THORNTON.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1994-02-28.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PSEOD010                                            *
      * TITLE    : END OF DAY HARD POSTING                             *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : PSEOD010                                            *
      * JCL      : PSEOD01 STEP PS020                                  *
      *----------------------------------------------------------------*
      * INPUT    : POSTIN   - DAY'S POSTINGS SORTED BY PST-KEY         *
      *                       (MEMO LOG + INBOUND CREDITS + BATCH      *
      *                       CHARGES/INTEREST) - PSTXNREC             *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM)   *
      *            MBK.TXNHIST (INSERT)                                *
      * OUTPUT   : REJECTS  - POSTINGS NOT APPLIED  - PSTXNREC         *
      *            JOURNAL  - POSTING JOURNAL REPORT (FBA 133)         *
      *----------------------------------------------------------------*
      * FOR EACH ACCOUNT (CONTROL BREAK ON SORT CODE + ACCOUNT):       *
      *   - READ THE ACCOUNT MASTER                                    *
      *   - APPLY EACH POSTING TO THE LEDGER BALANCE                   *
      *       MEMO POSTINGS ('M') - AVAILABLE BALANCE WAS ALREADY      *
      *         MOVED ONLINE BY PSMEMO1; REVERSE THE MEMO TOTAL        *
      *       HARD POSTINGS ('H') - ALSO MOVE THE AVAILABLE BALANCE    *
      *   - INSERT ONE TXNHIST ROW PER POSTING WITH RUNNING BALANCE    *
      *   - REWRITE THE ACCOUNT MASTER                                 *
      *                                                                *
      * MEMO POSTINGS ARE ALWAYS APPLIED - THE CUSTOMER HAS ALREADY    *
      * BEEN GIVEN (OR HAD TAKEN) THE FUNDS. ONLY HARD POSTINGS CAN    *
      * BE REJECTED.                                                   *
      *----------------------------------------------------------------*
      * RESTART: THE VSAM UPDATES ARE NOT COORDINATED WITH DB2 COMMIT. *
      * ON AN ABEND:                                                   *
      *   1. RESTORE ACCTMAST FROM PSEOD01 STEP PS005 BACKUP (+0)      *
      *   2. DELETE FROM MBK.TXNHIST WHERE BATCH_RUN_ID = <RUN ID>     *
      *      (DBA - SPUFI MEMBER MBK.PRD.SQL(PSDELRUN))                *
      *   3. RESTART PSEOD01 FROM STEP PS020                           *
      * SEE OPERATIONS RUNBOOK SECTION 12.4.                           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1994-02-28 GHT  ORIGINAL VERSION (REPLACES DDA POSTING PGM     *
      *                 DD0400 - ASSEMBLER)                            *
      * 1998-11-30 MLW  Y2K                                            *
      * 1999-08-16 KLM  TRANSACTION HISTORY WRITTEN TO DB2 INSTEAD OF  *
      *                 VSAM TXNHIST (PROJECT HORIZON)                 *
      * 2002-02-11 DKP  MEMO/HARD POSTING DISTINCTION                  *
      * 2007-10-01 AOS  POST SEQUENCE CONTINUES FROM EXISTING ROWS SO  *
      *                 A PARTIAL RERUN DOES NOT GET SQLCODE -803      *
      * 2010-03-08 AOS  ACM-LAST-CUST-TXN-DATE FOR DORMANCY (CR 10/141)*
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POSTIN-FILE      ASSIGN TO POSTIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-POSTIN-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT REJECTS-FILE     ASSIGN TO REJECTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REJECTS-STATUS.
           SELECT JOURNAL-FILE     ASSIGN TO JOURNAL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-JOURNAL-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  POSTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  POSTIN-RECORD                   PIC X(200).
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  REJECTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REJECTS-RECORD                  PIC X(200).
      *
       FD  JOURNAL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  JOURNAL-RECORD                  PIC X(133).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PSEOD010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-POSTIN-STATUS            PIC X(02).
               88  WS-POSTIN-OK                VALUE '00'.
               88  WS-POSTIN-EOF               VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-REJECTS-STATUS           PIC X(02).
           05  WS-JOURNAL-STATUS           PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-POSTINGS          VALUE 'Y'.
           05  WS-ACCT-FOUND-IND           PIC X(01) VALUE 'N'.
               88  WS-ACCT-FOUND               VALUE 'Y'.
           05  WS-ACCT-UPDATED-IND         PIC X(01) VALUE 'N'.
               88  WS-ACCT-UPDATED             VALUE 'Y'.
      *
       01  WS-CONTROL-FIELDS.
           05  WS-CURRENT-KEY.
               10  WS-CURR-SORT-CODE       PIC X(06).
               10  WS-CURR-ACCT-NO         PIC X(08).
           05  WS-PREV-POST-KEY            PIC X(36) VALUE LOW-VALUES.
           05  WS-POST-SEQ                 PIC S9(09) COMP.
           05  WS-COMMIT-FREQ              PIC S9(04) COMP VALUE +500.
           05  WS-SINCE-COMMIT             PIC S9(04) COMP VALUE ZERO.
           05  WS-BUS-DATE-DB2             PIC X(10).
           05  WS-VALUE-DATE-DB2           PIC X(10).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-REVERSED                 PIC X(35).
           05  WS-TRAILING                 PIC S9(04) COMP.
           05  WS-REJECT-CD                PIC X(04).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-POSTED-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-MEMO-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-HARD-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-REJECT-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCOUNT-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-DR                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-CR                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-REJECTED           PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
      *---- REPORT LINES ----------------------------------------------*
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
      *
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'PSEOD010'.
           05  FILLER                      PIC X(30) VALUE SPACES.
           05  FILLER                      PIC X(50)
               VALUE 'MEGABANK PLC - DDA HARD POSTING JOURNAL'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(07) VALUE 'SORT CD'.
           05  FILLER            PIC X(02) VALUE SPACES.
           05  FILLER            PIC X(09) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(05) VALUE '  SEQ'.
           05  FILLER            PIC X(01) VALUE SPACES.
           05  FILLER            PIC X(04) VALUE 'TXN'.
           05  FILLER            PIC X(01) VALUE SPACES.
           05  FILLER            PIC X(03) VALUE 'D/C'.
           05  FILLER            PIC X(02) VALUE SPACES.
           05  FILLER            PIC X(04) VALUE 'TYPE'.
           05  FILLER            PIC X(01) VALUE SPACES.
           05  FILLER            PIC X(16) VALUE '          AMOUNT'.
           05  FILLER            PIC X(01) VALUE SPACES.
           05  FILLER            PIC X(18) VALUE '       RUNNING BAL'.
           05  FILLER            PIC X(02) VALUE SPACES.
           05  FILLER            PIC X(06) VALUE 'CHNL'.
           05  FILLER            PIC X(03) VALUE SPACES.
           05  FILLER            PIC X(24) VALUE 'EXTERNAL REFERENCE'.
           05  FILLER            PIC X(03) VALUE SPACES.
           05  FILLER            PIC X(13) VALUE 'STATUS'.
           05  FILLER            PIC X(07) VALUE SPACES.
       01  WS-HEADING-3.
           05  FILLER                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(132) VALUE ALL '-'.
      *
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SEQ                      PIC ZZZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TXN-CODE                 PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DR-CR                    PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-POST-TYPE                PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-AMOUNT                   PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-RUNNING-BAL              PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CHANNEL                  PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-EXT-REF                  PIC X(24).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-STATUS                   PIC X(13).
           05  FILLER                      PIC X(07) VALUE SPACES.
      *
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
       01  WS-MAX-SEQ                      PIC S9(09) COMP.
       01  WS-MAX-SEQ-IND                  PIC S9(04) COMP.
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
           PERFORM 2000-PROCESS-ACCOUNT
               UNTIL WS-END-OF-POSTINGS
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  POSTIN-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                OUTPUT REJECTS-FILE
                       JOURNAL-FILE

           IF NOT WS-POSTIN-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-REJECTS-STATUS NOT = '00'
           OR WS-JOURNAL-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-POSTIN-STATUS ' ' WS-ACCTMAST-STATUS ' '
                      WS-REJECTS-STATUS ' ' WS-JOURNAL-STATUS ' '
                      WS-DATECARD-STATUS
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
               MOVE DATECARD-RECORD (1:40) TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE

           MOVE DTC-BUSINESS-DATE          TO WS-DATE-WORK
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-BUS-DATE-DB2
           END-STRING
           MOVE WS-BUS-DATE-DB2            TO HD1-BUS-DATE

           DISPLAY 'PSEOD010 - BUSINESS DATE ' WS-BUS-DATE-DB2
                   ' RUN ID ' DTC-RUN-ID

           PERFORM 8000-READ-POSTING
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           MOVE PST-SORT-CODE              TO WS-CURR-SORT-CODE
           MOVE PST-ACCT-NO                TO WS-CURR-ACCT-NO
           ADD 1                           TO WS-ACCOUNT-COUNT

           PERFORM 2100-START-ACCOUNT

           PERFORM 2200-APPLY-POSTING
               UNTIL WS-END-OF-POSTINGS
                  OR PST-SORT-CODE NOT = WS-CURR-SORT-CODE
                  OR PST-ACCT-NO   NOT = WS-CURR-ACCT-NO

           PERFORM 2300-END-ACCOUNT
           .
      *================================================================*
       2100-START-ACCOUNT.
      *================================================================*
           MOVE 'N'                        TO WS-ACCT-UPDATED-IND
           MOVE WS-CURRENT-KEY             TO ACM-KEY
           READ ACCTMAST-FILE

           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   SET WS-ACCT-FOUND       TO TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'N'                TO WS-ACCT-FOUND-IND
               WHEN OTHER
                   MOVE '2100-START-ACCOUNT'
                                           TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE WS-CURRENT-KEY     TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE

      *    CONTINUE THE SEQUENCE FROM ANY ROWS ALREADY POSTED TODAY.
           MOVE WS-CURR-SORT-CODE          TO TXH-SORT-CODE
           MOVE WS-CURR-ACCT-NO            TO TXH-ACCT-NO
           EXEC SQL
               SELECT MAX(POST_SEQ)
                 INTO :WS-MAX-SEQ :WS-MAX-SEQ-IND
                 FROM MBK.TXNHIST
                WHERE SORT_CODE = :TXH-SORT-CODE
                  AND ACCT_NO   = :TXH-ACCT-NO
                  AND POST_DATE = :WS-BUS-DATE-DB2
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE '2100-START-ACCOUNT'   TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           IF WS-MAX-SEQ-IND < 0
               MOVE ZERO                   TO WS-POST-SEQ
           ELSE
               MOVE WS-MAX-SEQ             TO WS-POST-SEQ
           END-IF
           .
      *================================================================*
       2200-APPLY-POSTING.
      *================================================================*
           MOVE SPACES                     TO WS-REJECT-CD

           EVALUATE TRUE
               WHEN NOT WS-ACCT-FOUND
                   MOVE 'ANFD'             TO WS-REJECT-CD
               WHEN PST-MEMO-POST
                   CONTINUE
               WHEN ACM-CLOSED
                   MOVE 'ACLS'             TO WS-REJECT-CD
               WHEN PST-DEBIT AND (ACM-NO-DEBITS OR ACM-NO-POSTINGS)
                   MOVE 'RSTD'             TO WS-REJECT-CD
               WHEN PST-CREDIT AND (ACM-NO-CREDITS OR ACM-NO-POSTINGS)
                   MOVE 'RSTC'             TO WS-REJECT-CD
               WHEN NOT PST-TXN-VALID
                   MOVE 'INVT'             TO WS-REJECT-CD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-REJECT-CD NOT = SPACES
               PERFORM 2250-REJECT-POSTING
           ELSE
               PERFORM 2210-UPDATE-BALANCES
               PERFORM 2220-INSERT-TXNHIST
               PERFORM 7100-JOURNAL-DETAIL
           END-IF

           PERFORM 8000-READ-POSTING
           .
      *================================================================*
       2210-UPDATE-BALANCES.
      *================================================================*
           IF PST-DEBIT
               SUBTRACT PST-AMOUNT         FROM ACM-LEDGER-BAL
               ADD PST-AMOUNT              TO WS-TOTAL-DR
           ELSE
               ADD PST-AMOUNT              TO ACM-LEDGER-BAL
               ADD PST-AMOUNT              TO WS-TOTAL-CR
           END-IF

      *    DORMANCY CLOCK (DDDRM010) RUNS FROM THE LAST TRANSACTION THE
      *    CUSTOMER MADE - BANK-GENERATED INTEREST AND CHARGES DO NOT
      *    COUNT (DORMANT BANK AND BUILDING SOCIETY ACCOUNTS ACT 2008).
           IF PST-CUSTOMER-INITIATED
               MOVE DTC-BUSINESS-DATE      TO ACM-LAST-CUST-TXN-DATE
           END-IF

           IF PST-MEMO-POST
               ADD 1                       TO WS-MEMO-COUNT
               IF PST-DEBIT
                   SUBTRACT PST-AMOUNT     FROM ACM-MEMO-DR-TOT
               ELSE
                   SUBTRACT PST-AMOUNT     FROM ACM-MEMO-CR-TOT
               END-IF
           ELSE
               ADD 1                       TO WS-HARD-COUNT
               IF PST-DEBIT
                   SUBTRACT PST-AMOUNT     FROM ACM-AVAIL-BAL
               ELSE
                   ADD PST-AMOUNT          TO ACM-AVAIL-BAL
               END-IF
           END-IF

           ADD 1                           TO WS-POST-SEQ
           ADD 1                           TO WS-POSTED-COUNT
           SET WS-ACCT-UPDATED             TO TRUE
           .
      *================================================================*
       2220-INSERT-TXNHIST.
      *================================================================*
           MOVE PST-SORT-CODE              TO TXH-SORT-CODE
           MOVE PST-ACCT-NO                TO TXH-ACCT-NO
           MOVE WS-BUS-DATE-DB2            TO TXH-POST-DATE
           MOVE WS-POST-SEQ                TO TXH-POST-SEQ
           MOVE PST-TXN-CODE               TO TXH-TXN-CODE
           MOVE PST-DR-CR-IND              TO TXH-DR-CR-IND
           MOVE PST-AMOUNT                 TO TXH-TXN-AMT
           MOVE ACM-LEDGER-BAL             TO TXH-RUNNING-BAL
           MOVE PST-CCY-CD                 TO TXH-CCY-CD

           IF PST-VALUE-DATE NUMERIC AND PST-VALUE-DATE NOT = ZERO
               MOVE PST-VALUE-DATE         TO WS-DATE-WORK
               STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                      DELIMITED BY SIZE
                      INTO TXH-VALUE-DATE
               END-STRING
           ELSE
               MOVE WS-BUS-DATE-DB2        TO TXH-VALUE-DATE
           END-IF

           MOVE PST-NARRATIVE-1            TO TXH-NARRATIVE-1-TEXT
           MOVE FUNCTION REVERSE (PST-NARRATIVE-1)
                                           TO WS-REVERSED
           MOVE ZERO                       TO WS-TRAILING
           INSPECT WS-REVERSED TALLYING WS-TRAILING
                   FOR LEADING SPACES
           COMPUTE TXH-NARRATIVE-1-LEN = 35 - WS-TRAILING

           MOVE PST-NARRATIVE-2            TO TXH-NARRATIVE-2-TEXT
           MOVE FUNCTION REVERSE (PST-NARRATIVE-2)
                                           TO WS-REVERSED
           MOVE ZERO                       TO WS-TRAILING
           INSPECT WS-REVERSED TALLYING WS-TRAILING
                   FOR LEADING SPACES
           COMPUTE TXH-NARRATIVE-2-LEN = 35 - WS-TRAILING

           MOVE PST-CHANNEL-CD             TO TXH-CHANNEL-CD
           MOVE PST-EXT-REF                TO TXH-EXT-REF
           MOVE PST-POST-TYPE              TO TXH-POST-TYPE
           MOVE PST-USER-ID                TO TXH-USER-ID
           MOVE SPACES                     TO TXH-BATCH-RUN-ID
           MOVE 'PS'                       TO TXH-BATCH-RUN-ID (1:2)
           MOVE DTC-RUN-ID                 TO TXH-BATCH-RUN-ID (3:5)

           EXEC SQL
               INSERT INTO MBK.TXNHIST
                     (SORT_CODE, ACCT_NO, POST_DATE, POST_SEQ,
                      TXN_CODE, DR_CR_IND, TXN_AMT, RUNNING_BAL,
                      CCY_CD, VALUE_DATE, NARRATIVE_1, NARRATIVE_2,
                      CHANNEL_CD, EXT_REF, POST_TYPE, USER_ID,
                      BATCH_RUN_ID)
               VALUES
                     (:TXH-SORT-CODE, :TXH-ACCT-NO, :TXH-POST-DATE,
                      :TXH-POST-SEQ, :TXH-TXN-CODE, :TXH-DR-CR-IND,
                      :TXH-TXN-AMT, :TXH-RUNNING-BAL, :TXH-CCY-CD,
                      :TXH-VALUE-DATE, :TXH-NARRATIVE-1,
                      :TXH-NARRATIVE-2, :TXH-CHANNEL-CD,
                      :TXH-EXT-REF, :TXH-POST-TYPE, :TXH-USER-ID,
                      :TXH-BATCH-RUN-ID)
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE '2220-INSERT-TXNHIST'  TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *================================================================*
       2250-REJECT-POSTING.
      *================================================================*
           SET PST-REJECTED                TO TRUE
           MOVE WS-REJECT-CD               TO PST-REJECT-CD
           WRITE REJECTS-RECORD            FROM PST-POSTING-RECORD
           IF WS-REJECTS-STATUS NOT = '00'
               MOVE '2250-REJECT-POSTING'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'REJECTS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-REJECTS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-REJECT-COUNT
           ADD PST-AMOUNT                  TO WS-TOTAL-REJECTED
           PERFORM 7100-JOURNAL-DETAIL
           .
      *================================================================*
       2300-END-ACCOUNT.
      *================================================================*
           IF WS-ACCT-FOUND AND WS-ACCT-UPDATED
               MOVE DTC-BUSINESS-DATE      TO ACM-LAST-TXN-DATE
               MOVE DTC-BUSINESS-DATE      TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE 'PSEOD010'             TO ACM-LAST-MAINT-USER

      *        A NON-ZERO MEMO TOTAL AFTER POSTING MEANS THE MEMO LOG
      *        AND THE MASTER DISAGREE. REPORT IT - DO NOT FAIL.
               IF ACM-MEMO-DR-TOT NOT = ZERO
               OR ACM-MEMO-CR-TOT NOT = ZERO
                   DISPLAY 'PSEOD010 W001 MEMO RESIDUE ON '
                           ACM-SORT-CODE ' ' ACM-ACCT-NO
               END-IF

               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '2300-END-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE WS-CURRENT-KEY     TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
           END-IF

           ADD 1                           TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
               EXEC SQL COMMIT END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC

      *    CONTROL CHECK - EVERY RECORD READ IS POSTED OR REJECTED.
           IF WS-READ-COUNT NOT = WS-POSTED-COUNT + WS-REJECT-COUNT
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'CONTROL COUNT MISMATCH - READ NOT = POSTED+REJ'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           PERFORM 7200-JOURNAL-TOTALS

           CLOSE POSTIN-FILE
                 ACCTMAST-FILE
                 REJECTS-FILE
                 JOURNAL-FILE

           DISPLAY 'PSEOD010 - POSTINGS READ     ' WS-READ-COUNT
           DISPLAY 'PSEOD010 - POSTED (MEMO)     ' WS-MEMO-COUNT
           DISPLAY 'PSEOD010 - POSTED (HARD)     ' WS-HARD-COUNT
           DISPLAY 'PSEOD010 - REJECTED          ' WS-REJECT-COUNT
           DISPLAY 'PSEOD010 - ACCOUNTS          ' WS-ACCOUNT-COUNT

      *    RC 4 IF ANYTHING WAS REJECTED - SCHEDULER RAISES A TICKET
      *    FOR DDA OPERATIONS BUT THE NETWORK CONTINUES.
           IF WS-REJECT-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       7000-JOURNAL-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE JOURNAL-RECORD            FROM WS-HEADING-1
           WRITE JOURNAL-RECORD            FROM WS-HEADING-2
           WRITE JOURNAL-RECORD            FROM WS-HEADING-3
           MOVE 4                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-JOURNAL-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-JOURNAL-HEADINGS
           END-IF

           MOVE PST-SORT-CODE              TO DL-SORT-CODE
           MOVE PST-ACCT-NO                TO DL-ACCT-NO
           MOVE PST-TXN-CODE               TO DL-TXN-CODE
           MOVE PST-DR-CR-IND              TO DL-DR-CR
           MOVE PST-AMOUNT                 TO DL-AMOUNT
           MOVE PST-CHANNEL-CD             TO DL-CHANNEL
           MOVE PST-EXT-REF                TO DL-EXT-REF
           IF PST-MEMO-POST
               MOVE 'MEMO'                 TO DL-POST-TYPE
           ELSE
               MOVE 'HARD'                 TO DL-POST-TYPE
           END-IF

           IF PST-REJECTED
               MOVE ZERO                   TO DL-SEQ
               MOVE ZERO                   TO DL-RUNNING-BAL
               MOVE SPACES                 TO DL-STATUS
               STRING 'REJECTED ' PST-REJECT-CD
                      DELIMITED BY SIZE
                      INTO DL-STATUS
               END-STRING
           ELSE
               MOVE WS-POST-SEQ            TO DL-SEQ
               MOVE ACM-LEDGER-BAL         TO DL-RUNNING-BAL
               MOVE 'POSTED'               TO DL-STATUS
           END-IF

           WRITE JOURNAL-RECORD            FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-JOURNAL-TOTALS.
      *================================================================*
           PERFORM 7000-JOURNAL-HEADINGS

           MOVE 'POSTINGS READ'            TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE JOURNAL-RECORD            FROM WS-TOTAL-LINE

           MOVE 'POSTINGS APPLIED'         TO TL-LABEL
           MOVE WS-POSTED-COUNT            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE JOURNAL-RECORD            FROM WS-TOTAL-LINE

           MOVE 'TOTAL DEBITS APPLIED'     TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-DR                TO TL-AMOUNT
           WRITE JOURNAL-RECORD            FROM WS-TOTAL-LINE

           MOVE 'TOTAL CREDITS APPLIED'    TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-CR                TO TL-AMOUNT
           WRITE JOURNAL-RECORD            FROM WS-TOTAL-LINE

           MOVE 'REJECTED'                 TO TL-LABEL
           MOVE WS-REJECT-COUNT            TO TL-COUNT
           MOVE WS-TOTAL-REJECTED          TO TL-AMOUNT
           WRITE JOURNAL-RECORD            FROM WS-TOTAL-LINE

           MOVE 'ACCOUNTS PROCESSED'       TO TL-LABEL
           MOVE WS-ACCOUNT-COUNT           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE JOURNAL-RECORD            FROM WS-TOTAL-LINE
           .
      *================================================================*
       8000-READ-POSTING.
      *================================================================*
           READ POSTIN-FILE INTO PST-POSTING-RECORD

           EVALUATE TRUE
               WHEN WS-POSTIN-OK
                   ADD 1                   TO WS-READ-COUNT
      *            INPUT MUST BE IN KEY SEQUENCE (SORT STEP PS010).
                   IF PST-KEY < WS-PREV-POST-KEY
                       MOVE '8000-READ-POSTING'
                                           TO ABN-PARAGRAPH
                       MOVE 1006           TO ABN-ABEND-CODE
                       MOVE 'POSTINGS OUT OF SEQUENCE'
                                           TO ABN-MESSAGE
                       MOVE PST-KEY        TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
                   MOVE PST-KEY            TO WS-PREV-POST-KEY
               WHEN WS-POSTIN-EOF
                   SET WS-END-OF-POSTINGS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-POSTING' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'POSTIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-POSTIN-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           MOVE WS-CURRENT-KEY             TO ABN-KEY-DATA
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
