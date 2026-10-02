       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PSARC010.
       AUTHOR.        P D QUINN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2012-02-13.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PSARC010                                            *
      * TITLE    : TRANSACTION HISTORY - ARCHIVE AND PURGE             *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : PSARC010                                            *
      * JCL      : PSARC01 (MONTHLY, SUNDAY AFTER MONTH END - OUTSIDE  *
      *            THE ONLINE DAY. MUST NOT OVERLAP PSEOD01)           *
      *----------------------------------------------------------------*
      * INPUT    : PARMIN   - CONTROL CARDS (SEE BELOW)                *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : MBK.TXNHIST - ROWS OLDER THAN THE CUT-OFF DELETED   *
      *            CHKPT    - CHECKPOINT (VSAM KSDS, ONE RECORD KEYED  *
      *                       'PSARC010', LRECL 200)                   *
      * OUTPUT   : ARCHIVE  - UNLOADED ROWS (PSARCREC FB 250, GDG)     *
      *            REPORT   - ARCHIVE REPORT BY POSTING MONTH (FBA 133)*
      *----------------------------------------------------------------*
      * PARMIN CARDS (KEYWORD IN COLS 1-8, '=' IN COL 9, VALUE 10-18): *
      *   RETMNTHS=NNN       MONTHS OF HISTORY TO KEEP (MIN 084)       *
      *   COMMITFQ=NNNNN     ROWS DELETED PER COMMIT (DEFAULT 01000)   *
      *   MAXROWS =NNNNNNNNN STOP CLEANLY AFTER THIS MANY DELETES      *
      *                      (BATCH WINDOW - DEFAULT 005000000)        *
      *   MODE    =PURGE     UNLOAD AND DELETE (DEFAULT)               *
      *   MODE    =TRIAL     UNLOAD ONLY - NOTHING DELETED, NO         *
      *                      CHECKPOINT. USED BEFORE A RETENTION CHANGE*
      *   RESTART =N         IGNORE AN INCOMPLETE CHECKPOINT AND START *
      *                      AGAIN FROM THE BEGINNING (DBA ONLY)       *
      *                                                                *
      * CUT-OFF = FIRST DAY OF THE BUSINESS MONTH LESS RETMNTHS        *
      * MONTHS. EVERY ROW WITH POST_DATE BEFORE THE CUT-OFF IS WRITTEN *
      * TO THE ARCHIVE, THEN DELETED. THE ARCHIVE RECORD IS ALWAYS     *
      * WRITTEN BEFORE THE DELETE, SO A ROW CAN BE ARCHIVED TWICE BUT  *
      * NEVER LOST.                                                    *
      *                                                                *
      * COMMIT AND RESTART:                                            *
      *   EVERY COMMITFQ DELETES THE PROGRAM COMMITS (THE CURSOR IS    *
      *   WITH HOLD) AND THEN REWRITES THE CHECKPOINT WITH THE LAST    *
      *   KEY AND THE CUMULATIVE COUNTS. CHECKPOINT STATUS:            *
      *     R  RUNNING / ABENDED  - NEXT RUN RESTARTS AFTER THE KEY    *
      *        WITH THE SAME CUT-OFF AND CARRIES THE COUNTS FORWARD    *
      *     S  STOPPED AT MAXROWS - NEXT RUN STARTS A NEW PASS         *
      *     C  COMPLETE                                                *
      *   THE VSAM CHECKPOINT IS NOT IN THE DB2 UNIT OF WORK. IF THE   *
      *   STEP FAILS BETWEEN THE COMMIT AND THE REWRITE THE RESTART    *
      *   KEY IS ONE INTERVAL BEHIND - HARMLESS, BECAUSE THE DELETED   *
      *   ROWS ARE NO LONGER THERE TO BE FETCHED.                      *
      *   RESTART JCL: RESUBMIT PSARC01 UNCHANGED. THE ARCHIVE OF THE  *
      *   FAILED RUN IS KEPT (DISP=(NEW,CATLG,CATLG)) AND THE RESTART  *
      *   WRITES THE NEXT GENERATION, HEADER RESTART IND = 'Y'.        *
      *----------------------------------------------------------------*
      * KNOWN PROBLEMS                                                 *
      * PIR 2012/031 OPEN - ROWS UNLOADED AFTER THE LAST COMMIT OF AN  *
      *   ABENDED RUN APPEAR IN BOTH ARCHIVE GENERATIONS. THE TAPE     *
      *   VAULT LOAD (ARCLOAD) WAS TO DE-DUPLICATE ON KEY BUT NEVER    *
      *   DID; OPERATIONS RUN SORT WITH SUM FIELDS=NONE BY HAND.       *
      * PIR 2014/077 OPEN - DELETES DO NOT FREE SPACE IN TSTXNH. THE   *
      *   DBA MUST REORG THE PURGED PARTITIONS AFTERWARDS. ROTATING    *
      *   THE QUARTERLY PARTITIONS (SEE DDL MBKCORE, PIR 2008/112)     *
      *   WOULD REPLACE THIS PROGRAM - CR 14/0220 RAISED, NOT FUNDED.  *
      * THE MINIMUM RETENTION (WS-MIN-RETAIN) IS 7 YEARS FOR HMRC AND  *
      * FOS EVIDENCE. LEGAL HOLD ACCOUNTS ARE NOT EXCLUDED - LEGAL     *
      * MUST ASK FOR A RESTORE FROM THE ARCHIVE (RUNBOOK SECTION 30).  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2012-02-13 PDQ  ORIGINAL VERSION (REPLACES DBA SPUFI DELETES)  *
      * 2012-06-04 PDQ  COMMIT FREQUENCY FROM PARMIN, WAS 500 FIXED    *
      * 2014-09-01 KLM  CHECKPOINT/RESTART AFTER 2014-08 ABEND -904    *
      *                 (RESOURCE UNAVAILABLE) LOST 6 HOURS            *
      * 2015-03-09 KLM  MAXROWS STOP, TRIAL MODE                       *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
           SELECT CHKPT-FILE       ASSIGN TO CHKPT
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CKP-KEY
                                   FILE STATUS IS WS-CHKPT-STATUS.
           SELECT ARCHIVE-FILE     ASSIGN TO ARCHIVE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ARCHIVE-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD.
           05  PARM-KEYWORD                PIC X(08).
               88  PARM-RETAIN                 VALUE 'RETMNTHS'.
               88  PARM-COMMIT                 VALUE 'COMMITFQ'.
               88  PARM-MAXROWS                VALUE 'MAXROWS '.
               88  PARM-MODE                   VALUE 'MODE    '.
               88  PARM-RESTART                VALUE 'RESTART '.
           05  PARM-EQUALS                 PIC X(01).
           05  PARM-VALUE                  PIC X(09).
           05  FILLER                      PIC X(62).
       FD  CHKPT-FILE.
       01  CKP-RECORD.
           05  CKP-KEY                     PIC X(08).
           05  CKP-STATUS                  PIC X(01).
               88  CKP-RUNNING                 VALUE 'R'.
               88  CKP-STOPPED                 VALUE 'S'.
               88  CKP-COMPLETE                VALUE 'C'.
           05  CKP-BUS-DATE                PIC 9(08).
           05  CKP-CUTOFF-DATE             PIC X(10).
           05  CKP-MODE                    PIC X(05).
           05  CKP-LAST-KEY.
               10  CKP-LAST-POST-DATE      PIC X(10).
               10  CKP-LAST-SORT-CODE      PIC X(06).
               10  CKP-LAST-ACCT-NO        PIC X(08).
               10  CKP-LAST-POST-SEQ       PIC S9(09) COMP.
           05  CKP-ROWS-UNLOADED           PIC S9(11) COMP-3.
           05  CKP-ROWS-DELETED            PIC S9(11) COMP-3.
           05  CKP-DEBIT-TOTAL             PIC S9(15)V99 COMP-3.
           05  CKP-CREDIT-TOTAL            PIC S9(15)V99 COMP-3.
           05  CKP-COMMITS                 PIC S9(07) COMP-3.
           05  CKP-RESTART-COUNT           PIC 9(03).
           05  CKP-UPDATE-DATE             PIC 9(08).
           05  CKP-UPDATE-TIME             PIC 9(06).
           05  FILLER                      PIC X(89).
       FD  ARCHIVE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ARCHIVE-RECORD                  PIC X(250).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PSARC010'.
      *
       01  WS-CONSTANTS.
           05  WS-CKP-KEY-VALUE            PIC X(08) VALUE 'PSARC010'.
           05  WS-ARCHIVE-ID               PIC X(08) VALUE 'MBKTXNAR'.
           05  WS-MIN-RETAIN               PIC 9(03) VALUE 084.
      *
       01  WS-PARAMETERS.
           05  WS-RETAIN-MONTHS            PIC 9(03) VALUE ZERO.
           05  WS-COMMIT-FREQ              PIC 9(05) VALUE 01000.
           05  WS-MAX-ROWS                 PIC 9(09) VALUE 005000000.
           05  WS-MODE                     PIC X(05) VALUE 'PURGE'.
               88  WS-MODE-PURGE               VALUE 'PURGE'.
               88  WS-MODE-TRIAL               VALUE 'TRIAL'.
           05  WS-RESTART-ALLOWED          PIC X(01) VALUE 'Y'.
               88  WS-RESTART-OK               VALUE 'Y'.
      *
       01  WS-FILE-STATUSES.
           05  WS-PARMIN-STATUS            PIC X(02).
               88  WS-PARMIN-OK                VALUE '00'.
           05  WS-CHKPT-STATUS             PIC X(02).
               88  WS-CHKPT-OK                 VALUE '00'.
               88  WS-CHKPT-NOTFND             VALUE '23'.
           05  WS-ARCHIVE-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOC-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CURSOR            VALUE 'Y'.
           05  WS-RESTART-IND              PIC X(01) VALUE 'N'.
               88  WS-IS-RESTART               VALUE 'Y'.
           05  WS-STOP-IND                 PIC X(01) VALUE 'N'.
               88  WS-STOPPED-AT-MAX           VALUE 'Y'.
           05  WS-CKP-EXISTS-IND           PIC X(01) VALUE 'N'.
               88  WS-CKP-EXISTS               VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CUTOFF                   PIC 9(08).
           05  WS-CUTOFF-R REDEFINES WS-CUTOFF.
               10  WS-CO-CCYY              PIC 9(04).
               10  WS-CO-MM                PIC 9(02).
               10  WS-CO-DD                PIC 9(02).
           05  WS-TOTAL-MONTHS             PIC S9(07) COMP-3.
           05  WS-SINCE-COMMIT             PIC S9(07) COMP-3 VALUE 0.
           05  WS-NARR-LEN                 PIC S9(04) COMP.
           05  WS-CURR-MONTH               PIC X(07) VALUE LOW-VALUES.
           05  WS-CURRENT-DATE-TIME.
               10  WS-CDT-DATE             PIC 9(08).
               10  WS-CDT-TIME             PIC 9(06).
               10  FILLER                  PIC X(07).
      *
      *---- HOST VARIABLES --------------------------------------------*
       01  HV-CUTOFF-DATE                  PIC X(10).
       01  HV-RESTART-KEY.
           05  HV-RS-POST-DATE             PIC X(10).
           05  HV-RS-SORT-CODE             PIC X(06).
           05  HV-RS-ACCT-NO               PIC X(08).
           05  HV-RS-POST-SEQ              PIC S9(09) COMP.
      *
      *---- THIS RUN (ARCHIVE GENERATION) / CUMULATIVE (CHECKPOINT) ---*
       01  WS-COUNTERS.
           05  WS-RUN-FETCHED              PIC S9(11) COMP-3 VALUE 0.
           05  WS-RUN-UNLOADED             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RUN-DELETED              PIC S9(11) COMP-3 VALUE 0.
           05  WS-RUN-VANISHED             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RUN-DEBITS               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-RUN-CREDITS              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-RUN-COMMITS              PIC S9(07) COMP-3 VALUE 0.
           05  WS-MONTH-ROWS               PIC S9(11) COMP-3 VALUE 0.
           05  WS-MONTH-DELETED            PIC S9(11) COMP-3 VALUE 0.
           05  WS-MONTH-DEBITS             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-MONTH-CREDITS            PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'PSARC010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - TRANSACTION HISTORY ARCHIVE'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(10) VALUE 'POST MONTH'.
           05  FILLER            PIC X(14) VALUE '          ROWS'.
           05  FILLER            PIC X(21)
                                 VALUE '               DEBITS'.
           05  FILLER            PIC X(21)
                                 VALUE '              CREDITS'.
           05  FILLER            PIC X(14) VALUE '       DELETED'.
           05  FILLER            PIC X(52) VALUE SPACES.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-MONTH                    PIC X(07).
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  DL-ROWS                     PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-DEBITS                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-CREDITS                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-DELETED                  PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(52) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
       01  WS-TEXT-LINE.
           05  TX-CC                       PIC X(01) VALUE '0'.
           05  TX-TEXT                     PIC X(100).
           05  FILLER                      PIC X(32) VALUE SPACES.
      *
           COPY PSARCREC.
      *
           COPY DATECARD.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
      *    FRESH PASS. XTXNH02 (POST_DATE, SORT_CODE, ACCT_NO,
      *    POST_SEQ) GIVES THE ORDER WITHOUT A SORT.
           EXEC SQL DECLARE ARCCSR1 CURSOR WITH HOLD FOR
               SELECT SORT_CODE, ACCT_NO, POST_DATE, POST_SEQ,
                      TXN_CODE, DR_CR_IND, TXN_AMT, RUNNING_BAL,
                      CCY_CD, VALUE_DATE, NARRATIVE_1, NARRATIVE_2,
                      CHANNEL_CD, EXT_REF, POST_TYPE, USER_ID,
                      BATCH_RUN_ID, CHAR(INSERT_TS)
                 FROM MBK.TXNHIST
                WHERE POST_DATE < :HV-CUTOFF-DATE
                ORDER BY POST_DATE, SORT_CODE, ACCT_NO, POST_SEQ
                FOR FETCH ONLY
           END-EXEC.
      *
      *    RESTART PASS - STRICTLY AFTER THE LAST CHECKPOINTED KEY.
           EXEC SQL DECLARE ARCCSR2 CURSOR WITH HOLD FOR
               SELECT SORT_CODE, ACCT_NO, POST_DATE, POST_SEQ,
                      TXN_CODE, DR_CR_IND, TXN_AMT, RUNNING_BAL,
                      CCY_CD, VALUE_DATE, NARRATIVE_1, NARRATIVE_2,
                      CHANNEL_CD, EXT_REF, POST_TYPE, USER_ID,
                      BATCH_RUN_ID, CHAR(INSERT_TS)
                 FROM MBK.TXNHIST
                WHERE POST_DATE < :HV-CUTOFF-DATE
                  AND (POST_DATE > :HV-RS-POST-DATE
                   OR (POST_DATE = :HV-RS-POST-DATE
                  AND (SORT_CODE > :HV-RS-SORT-CODE
                   OR (SORT_CODE = :HV-RS-SORT-CODE
                  AND (ACCT_NO > :HV-RS-ACCT-NO
                   OR (ACCT_NO = :HV-RS-ACCT-NO
                  AND POST_SEQ > :HV-RS-POST-SEQ))))))
                ORDER BY POST_DATE, SORT_CODE, ACCT_NO, POST_SEQ
                FOR FETCH ONLY
           END-EXEC.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           PERFORM 1500-CHECKPOINT-START THRU 1500-EXIT
           PERFORM 2000-ARCHIVE
           PERFORM 8000-FINAL-CHECKPOINT THRU 8000-EXIT
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
      *    A NEWLY DEFINED (NEVER LOADED) CHECKPOINT KSDS CANNOT BE
      *    OPENED I-O (STATUS 35) - OPEN IT OUTPUT ONCE TO PUT IT IN
      *    LOADED STATE, THEN REOPEN I-O.
           OPEN I-O    CHKPT-FILE
           IF WS-CHKPT-STATUS = '35'
               OPEN OUTPUT CHKPT-FILE
               CLOSE CHKPT-FILE
               OPEN I-O CHKPT-FILE
               DISPLAY 'PSARC010 - EMPTY CHECKPOINT FILE INITIALISED'
           END-IF
           OPEN INPUT  PARMIN-FILE
                       DATECARD-FILE
                OUTPUT ARCHIVE-FILE
                       REPORT-FILE
           IF NOT WS-PARMIN-OK
           OR NOT WS-CHKPT-OK
           OR WS-ARCHIVE-STATUS NOT = '00'
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
           PERFORM 1100-READ-PARAMETERS
           PERFORM 1200-COMPUTE-CUTOFF
           .
      *================================================================*
       1100-READ-PARAMETERS.
      *================================================================*
           READ PARMIN-FILE
           PERFORM UNTIL NOT WS-PARMIN-OK
               DISPLAY 'PSARC010 - PARM ' PARMIN-RECORD (1:18)
               EVALUATE TRUE
                   WHEN PARM-RETAIN
                       IF PARM-VALUE (1:3) NUMERIC
                           MOVE PARM-VALUE (1:3) TO WS-RETAIN-MONTHS
                       ELSE
                           PERFORM 1190-BAD-PARM
                       END-IF
                   WHEN PARM-COMMIT
                       IF PARM-VALUE (1:5) NUMERIC
                       AND PARM-VALUE (1:5) > '00000'
                           MOVE PARM-VALUE (1:5) TO WS-COMMIT-FREQ
                       ELSE
                           PERFORM 1190-BAD-PARM
                       END-IF
                   WHEN PARM-MAXROWS
                       IF PARM-VALUE NUMERIC
                           MOVE PARM-VALUE TO WS-MAX-ROWS
                       ELSE
                           PERFORM 1190-BAD-PARM
                       END-IF
                   WHEN PARM-MODE
                       MOVE PARM-VALUE (1:5) TO WS-MODE
                       IF NOT (WS-MODE-PURGE OR WS-MODE-TRIAL)
                           PERFORM 1190-BAD-PARM
                       END-IF
                   WHEN PARM-RESTART
                       IF PARM-VALUE (1:1) = 'N'
                           MOVE 'N'        TO WS-RESTART-ALLOWED
                       END-IF
                   WHEN PARMIN-RECORD (1:1) = '*'
                       CONTINUE
                   WHEN OTHER
                       PERFORM 1190-BAD-PARM
               END-EVALUATE
               READ PARMIN-FILE
           END-PERFORM
           CLOSE PARMIN-FILE
           IF WS-RETAIN-MONTHS < WS-MIN-RETAIN
               MOVE '1100-READ-PARAMETERS' TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'RETMNTHS MISSING OR BELOW MINIMUM 084'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           .
      *----------------------------------------------------------------*
       1190-BAD-PARM.
      *----------------------------------------------------------------*
           MOVE '1100-READ-PARAMETERS'     TO ABN-PARAGRAPH
           MOVE 1005                       TO ABN-ABEND-CODE
           MOVE 'INVALID PARMIN CARD'      TO ABN-MESSAGE
           MOVE PARMIN-RECORD (1:40)       TO ABN-KEY-DATA
           PERFORM Z999-ABEND
           .
      *================================================================*
       1200-COMPUTE-CUTOFF.
      *================================================================*
      *    FIRST OF THE BUSINESS MONTH, LESS RETMNTHS MONTHS.
           MOVE WS-BUS-DATE                TO WS-CUTOFF
           COMPUTE WS-TOTAL-MONTHS =
                   WS-CO-CCYY * 12 + WS-CO-MM - 1 - WS-RETAIN-MONTHS
           COMPUTE WS-CO-CCYY = WS-TOTAL-MONTHS / 12
           COMPUTE WS-CO-MM =
                   WS-TOTAL-MONTHS - (WS-CO-CCYY * 12) + 1
           MOVE 01                         TO WS-CO-DD
           MOVE SPACES                     TO HV-CUTOFF-DATE
           STRING WS-CUTOFF (1:4) '-' WS-CUTOFF (5:2) '-'
                  WS-CUTOFF (7:2)
                  DELIMITED BY SIZE
                  INTO HV-CUTOFF-DATE
           END-STRING
           DISPLAY 'PSARC010 - RETAIN ' WS-RETAIN-MONTHS
                   ' MONTHS, CUT-OFF ' HV-CUTOFF-DATE
                   ' MODE ' WS-MODE
           .
      *================================================================*
       1500-CHECKPOINT-START.
      *================================================================*
           MOVE LOW-VALUES                 TO HV-RS-POST-DATE
                                              HV-RS-SORT-CODE
                                              HV-RS-ACCT-NO
           MOVE ZERO                       TO HV-RS-POST-SEQ
           IF WS-MODE-TRIAL
               DISPLAY 'PSARC010 - TRIAL MODE, CHECKPOINT NOT USED'
               GO TO 1500-WRITE-HEADER
           END-IF
           MOVE WS-CKP-KEY-VALUE           TO CKP-KEY
           READ CHKPT-FILE
           EVALUATE TRUE
               WHEN WS-CHKPT-OK
                   SET WS-CKP-EXISTS       TO TRUE
               WHEN WS-CHKPT-NOTFND
                   MOVE 'N'                TO WS-CKP-EXISTS-IND
               WHEN OTHER
                   MOVE '1500-CHECKPOINT-START' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CHKPT READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CHKPT-STATUS    TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE

           IF WS-CKP-EXISTS
           AND CKP-RUNNING
           AND CKP-MODE = 'PURGE'
               IF WS-RESTART-OK
                   SET WS-IS-RESTART       TO TRUE
               ELSE
                   DISPLAY 'PSARC010 - RESTART=N - INCOMPLETE RUN OF '
                           CKP-BUS-DATE ' IGNORED'
               END-IF
           END-IF

           IF WS-IS-RESTART
      *        KEEP THE ORIGINAL CUT-OFF SO BOTH HALVES OF THE RUN
      *        PURGE THE SAME RANGE, EVEN IF THE DATE HAS ROLLED.
               IF CKP-CUTOFF-DATE NOT = HV-CUTOFF-DATE
                   DISPLAY 'PSARC010 - CUT-OFF FROM CHECKPOINT '
                           CKP-CUTOFF-DATE ' REPLACES '
                           HV-CUTOFF-DATE
                   MOVE CKP-CUTOFF-DATE    TO HV-CUTOFF-DATE
               END-IF
      *        ABENDED BEFORE THE FIRST COMMIT - NO KEY TO RESTART
      *        FROM, SO RE-READ FROM THE START WITH THE SAME CUT-OFF.
               IF CKP-LAST-POST-DATE = SPACES
                   DISPLAY 'PSARC010 - NO COMMIT IN FAILED RUN, '
                           'STARTING FROM THE BEGINNING'
                   MOVE 'N'                TO WS-RESTART-IND
                   ADD 1                   TO CKP-RESTART-COUNT
                   GO TO 1500-SAVE
               END-IF
               MOVE CKP-LAST-POST-DATE     TO HV-RS-POST-DATE
               MOVE CKP-LAST-SORT-CODE     TO HV-RS-SORT-CODE
               MOVE CKP-LAST-ACCT-NO       TO HV-RS-ACCT-NO
               MOVE CKP-LAST-POST-SEQ      TO HV-RS-POST-SEQ
               ADD 1                       TO CKP-RESTART-COUNT
               DISPLAY 'PSARC010 - RESTARTING AFTER '
                       HV-RS-POST-DATE ' ' HV-RS-SORT-CODE ' '
                       HV-RS-ACCT-NO ' ' HV-RS-POST-SEQ
               DISPLAY 'PSARC010 - ROWS ALREADY DELETED '
                       CKP-ROWS-DELETED
           ELSE
               MOVE WS-CKP-KEY-VALUE       TO CKP-KEY
               MOVE WS-BUS-DATE            TO CKP-BUS-DATE
               MOVE HV-CUTOFF-DATE         TO CKP-CUTOFF-DATE
               MOVE WS-MODE                TO CKP-MODE
               MOVE SPACES                 TO CKP-LAST-POST-DATE
                                              CKP-LAST-SORT-CODE
                                              CKP-LAST-ACCT-NO
               MOVE ZERO                   TO CKP-LAST-POST-SEQ
                                              CKP-ROWS-UNLOADED
                                              CKP-ROWS-DELETED
                                              CKP-DEBIT-TOTAL
                                              CKP-CREDIT-TOTAL
                                              CKP-COMMITS
                                              CKP-RESTART-COUNT
           END-IF
           .
       1500-SAVE.
           SET CKP-RUNNING                 TO TRUE
           PERFORM 6000-SAVE-CHECKPOINT
           .
       1500-WRITE-HEADER.
           MOVE SPACES                     TO ARC-HEADER-RECORD
           MOVE 'H'                        TO ARH-REC-TYPE
           MOVE WS-ARCHIVE-ID              TO ARH-ARCHIVE-ID
           MOVE WS-BUS-DATE                TO ARH-BUS-DATE
           MOVE HV-CUTOFF-DATE             TO ARH-CUTOFF-DATE
           MOVE WS-RETAIN-MONTHS           TO ARH-RETAIN-MONTHS
           MOVE WS-MODE                    TO ARH-MODE
           IF WS-IS-RESTART
               MOVE 'Y'                    TO ARH-RESTART-IND
               MOVE HV-RS-POST-DATE        TO ARH-RESTART-POST-DATE
               MOVE HV-RS-SORT-CODE        TO ARH-RESTART-SORT-CODE
               MOVE HV-RS-ACCT-NO          TO ARH-RESTART-ACCT-NO
               MOVE HV-RS-POST-SEQ         TO ARH-RESTART-POST-SEQ
           ELSE
               MOVE 'N'                    TO ARH-RESTART-IND
               MOVE ZERO                   TO ARH-RESTART-POST-SEQ
           END-IF
           PERFORM 5000-WRITE-ARCHIVE
           .
       1500-EXIT.
           EXIT.
      *================================================================*
       2000-ARCHIVE.
      *================================================================*
           IF WS-IS-RESTART
               EXEC SQL OPEN ARCCSR2 END-EXEC
           ELSE
               EXEC SQL OPEN ARCCSR1 END-EXEC
           END-IF
           IF SQLCODE NOT = 0
               MOVE '2000-ARCHIVE'         TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           PERFORM 2100-FETCH
           PERFORM UNTIL WS-END-OF-CURSOR
                      OR WS-STOPPED-AT-MAX
               PERFORM 2200-PROCESS-ROW
               IF NOT WS-STOPPED-AT-MAX
                   PERFORM 2100-FETCH
               END-IF
           END-PERFORM
           IF WS-CURR-MONTH NOT = LOW-VALUES
               PERFORM 7100-MONTH-LINE
           END-IF
           IF WS-IS-RESTART
               EXEC SQL CLOSE ARCCSR2 END-EXEC
           ELSE
               EXEC SQL CLOSE ARCCSR1 END-EXEC
           END-IF
           .
      *================================================================*
       2100-FETCH.
      *================================================================*
           IF WS-IS-RESTART
               EXEC SQL
                   FETCH ARCCSR2
                    INTO :TXH-SORT-CODE, :TXH-ACCT-NO, :TXH-POST-DATE,
                         :TXH-POST-SEQ, :TXH-TXN-CODE, :TXH-DR-CR-IND,
                         :TXH-TXN-AMT, :TXH-RUNNING-BAL, :TXH-CCY-CD,
                         :TXH-VALUE-DATE, :TXH-NARRATIVE-1,
                         :TXH-NARRATIVE-2, :TXH-CHANNEL-CD,
                         :TXH-EXT-REF, :TXH-POST-TYPE, :TXH-USER-ID,
                         :TXH-BATCH-RUN-ID, :TXH-INSERT-TS
               END-EXEC
           ELSE
               EXEC SQL
                   FETCH ARCCSR1
                    INTO :TXH-SORT-CODE, :TXH-ACCT-NO, :TXH-POST-DATE,
                         :TXH-POST-SEQ, :TXH-TXN-CODE, :TXH-DR-CR-IND,
                         :TXH-TXN-AMT, :TXH-RUNNING-BAL, :TXH-CCY-CD,
                         :TXH-VALUE-DATE, :TXH-NARRATIVE-1,
                         :TXH-NARRATIVE-2, :TXH-CHANNEL-CD,
                         :TXH-EXT-REF, :TXH-POST-TYPE, :TXH-USER-ID,
                         :TXH-BATCH-RUN-ID, :TXH-INSERT-TS
               END-EXEC
           END-IF
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-RUN-FETCHED
               WHEN +100
                   SET WS-END-OF-CURSOR    TO TRUE
               WHEN OTHER
                   MOVE '2100-FETCH'       TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       2200-PROCESS-ROW.
      *================================================================*
      *    CONTROL BREAK ON POSTING MONTH FOR THE REPORT.
           IF TXH-POST-DATE (1:7) NOT = WS-CURR-MONTH
               IF WS-CURR-MONTH NOT = LOW-VALUES
                   PERFORM 7100-MONTH-LINE
               END-IF
               MOVE TXH-POST-DATE (1:7)    TO WS-CURR-MONTH
           END-IF
      *    BELT AND BRACES - THE CURSOR PREDICATE ALREADY EXCLUDES
      *    ANYTHING ON OR AFTER THE CUT-OFF.
           IF TXH-POST-DATE NOT < HV-CUTOFF-DATE
               MOVE '2200-PROCESS-ROW'     TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'ROW AFTER CUT-OFF RETURNED BY CURSOR'
                                           TO ABN-MESSAGE
               MOVE TXH-POST-DATE          TO ABN-KEY-DATA
               EXEC SQL ROLLBACK END-EXEC
               PERFORM Z999-ABEND
           END-IF
           PERFORM 2300-BUILD-ARCHIVE
           PERFORM 5000-WRITE-ARCHIVE
           ADD 1                           TO WS-RUN-UNLOADED
                                              WS-MONTH-ROWS
           IF TXH-DR-CR-IND = 'D'
               ADD TXH-TXN-AMT             TO WS-RUN-DEBITS
                                              WS-MONTH-DEBITS
           ELSE
               ADD TXH-TXN-AMT             TO WS-RUN-CREDITS
                                              WS-MONTH-CREDITS
           END-IF
           IF WS-MODE-PURGE
               PERFORM 2400-DELETE-ROW
           END-IF
           .
      *================================================================*
       2300-BUILD-ARCHIVE.
      *================================================================*
           MOVE SPACES                     TO ARC-DETAIL-RECORD
           MOVE 'D'                        TO ARC-REC-TYPE
           MOVE TXH-SORT-CODE              TO ARC-SORT-CODE
           MOVE TXH-ACCT-NO                TO ARC-ACCT-NO
           MOVE TXH-POST-DATE              TO ARC-POST-DATE
           MOVE TXH-POST-SEQ               TO ARC-POST-SEQ
           MOVE TXH-TXN-CODE               TO ARC-TXN-CODE
           MOVE TXH-DR-CR-IND              TO ARC-DR-CR-IND
           MOVE TXH-TXN-AMT                TO ARC-TXN-AMT
           MOVE TXH-RUNNING-BAL            TO ARC-RUNNING-BAL
           MOVE TXH-CCY-CD                 TO ARC-CCY-CD
           MOVE TXH-VALUE-DATE             TO ARC-VALUE-DATE
           MOVE TXH-NARRATIVE-1-LEN        TO WS-NARR-LEN
           IF WS-NARR-LEN > 0 AND WS-NARR-LEN <= 35
               MOVE TXH-NARRATIVE-1-TEXT (1:WS-NARR-LEN)
                                           TO ARC-NARRATIVE-1
           END-IF
           MOVE TXH-NARRATIVE-2-LEN        TO WS-NARR-LEN
           IF WS-NARR-LEN > 0 AND WS-NARR-LEN <= 35
               MOVE TXH-NARRATIVE-2-TEXT (1:WS-NARR-LEN)
                                           TO ARC-NARRATIVE-2
           END-IF
           MOVE TXH-CHANNEL-CD             TO ARC-CHANNEL-CD
           MOVE TXH-EXT-REF                TO ARC-EXT-REF
           MOVE TXH-POST-TYPE              TO ARC-POST-TYPE
           MOVE TXH-USER-ID                TO ARC-USER-ID
           MOVE TXH-BATCH-RUN-ID           TO ARC-BATCH-RUN-ID
           MOVE TXH-INSERT-TS              TO ARC-INSERT-TS
           .
      *================================================================*
       2400-DELETE-ROW.
      *================================================================*
           EXEC SQL
               DELETE FROM MBK.TXNHIST
                WHERE SORT_CODE = :TXH-SORT-CODE
                  AND ACCT_NO   = :TXH-ACCT-NO
                  AND POST_DATE = :TXH-POST-DATE
                  AND POST_SEQ  = :TXH-POST-SEQ
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-RUN-DELETED
                                              WS-MONTH-DELETED
                                              CKP-ROWS-DELETED
                   ADD 1                   TO WS-SINCE-COMMIT
               WHEN +100
      *            ALREADY GONE (A DBA DELETE OR A DOUBLE RESTART).
                   ADD 1                   TO WS-RUN-VANISHED
               WHEN OTHER
                   MOVE '2400-DELETE-ROW'  TO ABN-PARAGRAPH
                   MOVE TXH-SORT-CODE      TO ABN-KEY-DATA (1:6)
                   MOVE TXH-ACCT-NO        TO ABN-KEY-DATA (8:8)
                   MOVE TXH-POST-DATE      TO ABN-KEY-DATA (17:10)
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           ADD 1                           TO CKP-ROWS-UNLOADED
           IF TXH-DR-CR-IND = 'D'
               ADD TXH-TXN-AMT             TO CKP-DEBIT-TOTAL
           ELSE
               ADD TXH-TXN-AMT             TO CKP-CREDIT-TOTAL
           END-IF
           MOVE TXH-POST-DATE              TO CKP-LAST-POST-DATE
           MOVE TXH-SORT-CODE              TO CKP-LAST-SORT-CODE
           MOVE TXH-ACCT-NO                TO CKP-LAST-ACCT-NO
           MOVE TXH-POST-SEQ               TO CKP-LAST-POST-SEQ
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
               PERFORM 2500-COMMIT
               IF WS-RUN-DELETED >= WS-MAX-ROWS
                   SET WS-STOPPED-AT-MAX   TO TRUE
                   DISPLAY 'PSARC010 - MAXROWS REACHED AFTER '
                           WS-RUN-DELETED ' DELETES'
               END-IF
           END-IF
           .
      *================================================================*
       2500-COMMIT.
      *================================================================*
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2500-COMMIT'          TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           ADD 1                           TO WS-RUN-COMMITS
                                              CKP-COMMITS
           MOVE ZERO                       TO WS-SINCE-COMMIT
      *    CHECKPOINT AFTER THE COMMIT - SEE HEADER.
           PERFORM 6000-SAVE-CHECKPOINT
           .
      *================================================================*
       5000-WRITE-ARCHIVE.
      *================================================================*
           WRITE ARCHIVE-RECORD            FROM ARC-DETAIL-RECORD
           IF WS-ARCHIVE-STATUS NOT = '00'
               MOVE '5000-WRITE-ARCHIVE'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ARCHIVE WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ARCHIVE-STATUS      TO ABN-FILE-STATUS
               EXEC SQL ROLLBACK END-EXEC
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       6000-SAVE-CHECKPOINT.
      *================================================================*
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-TIME
           MOVE WS-CDT-DATE                TO CKP-UPDATE-DATE
           MOVE WS-CDT-TIME                TO CKP-UPDATE-TIME
           IF WS-CKP-EXISTS
               REWRITE CKP-RECORD
           ELSE
               WRITE CKP-RECORD
               SET WS-CKP-EXISTS           TO TRUE
           END-IF
           IF NOT WS-CHKPT-OK
               MOVE '6000-SAVE-CHECKPOINT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHKPT WRITE/REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CHKPT-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       7000-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           WRITE REPORT-RECORD             FROM WS-HEADING-2
           MOVE 4                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-MONTH-LINE.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE WS-CURR-MONTH              TO DL-MONTH
           MOVE WS-MONTH-ROWS              TO DL-ROWS
           MOVE WS-MONTH-DEBITS            TO DL-DEBITS
           MOVE WS-MONTH-CREDITS           TO DL-CREDITS
           MOVE WS-MONTH-DELETED           TO DL-DELETED
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           MOVE ZERO                       TO WS-MONTH-ROWS
                                              WS-MONTH-DELETED
                                              WS-MONTH-DEBITS
                                              WS-MONTH-CREDITS
           .
      *================================================================*
       7200-TOTAL-LINE.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           ADD 2                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-FINAL-CHECKPOINT.
      *================================================================*
      *    TRAILER FOR THIS GENERATION, THEN THE LAST COMMIT AND THE
      *    CHECKPOINT STATUS (C COMPLETE / S STOPPED AT MAXROWS).
           MOVE SPACES                     TO ARC-TRAILER-RECORD
           MOVE 'T'                        TO ART-REC-TYPE
           MOVE WS-ARCHIVE-ID              TO ART-ARCHIVE-ID
           MOVE WS-BUS-DATE                TO ART-BUS-DATE
           MOVE WS-RUN-UNLOADED            TO ART-ROW-COUNT
           MOVE WS-RUN-DEBITS              TO ART-DEBIT-TOTAL
           MOVE WS-RUN-CREDITS             TO ART-CREDIT-TOTAL
           MOVE WS-RUN-DELETED             TO ART-ROWS-DELETED
           PERFORM 5000-WRITE-ARCHIVE
           CLOSE ARCHIVE-FILE
           IF WS-MODE-TRIAL
               EXEC SQL ROLLBACK END-EXEC
               GO TO 8000-EXIT
           END-IF
           IF WS-STOPPED-AT-MAX
               SET CKP-STOPPED             TO TRUE
           ELSE
               SET CKP-COMPLETE            TO TRUE
           END-IF
           PERFORM 2500-COMMIT
           .
       8000-EXIT.
           EXIT.
      *================================================================*
       9000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE SPACES                     TO TX-TEXT
           STRING 'MODE ' WS-MODE '   RETENTION ' WS-RETAIN-MONTHS
                  ' MONTHS   CUT-OFF ' HV-CUTOFF-DATE
                  '   COMMIT EVERY ' WS-COMMIT-FREQ ' ROWS'
                  DELIMITED BY SIZE
                  INTO TX-TEXT
           END-STRING
           WRITE REPORT-RECORD             FROM WS-TEXT-LINE
           ADD 2                           TO WS-LINE-COUNT
           IF WS-IS-RESTART
               MOVE SPACES                 TO TX-TEXT
               STRING 'RESTARTED AFTER KEY ' HV-RS-POST-DATE ' '
                      HV-RS-SORT-CODE ' ' HV-RS-ACCT-NO
                      ' - SEE PIR 2012/031 FOR DUPLICATES'
                      DELIMITED BY SIZE
                      INTO TX-TEXT
               END-STRING
               WRITE REPORT-RECORD         FROM WS-TEXT-LINE
               ADD 2                       TO WS-LINE-COUNT
           END-IF
           MOVE 'ROWS FETCHED THIS RUN'    TO TL-LABEL
           MOVE WS-RUN-FETCHED             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'ROWS ARCHIVED THIS RUN - DEBITS' TO TL-LABEL
           MOVE WS-RUN-UNLOADED            TO TL-COUNT
           MOVE WS-RUN-DEBITS              TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'ROWS ARCHIVED THIS RUN - CREDITS' TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-RUN-CREDITS             TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'ROWS DELETED THIS RUN'    TO TL-LABEL
           MOVE WS-RUN-DELETED             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'ROWS ALREADY GONE (SQLCODE +100)' TO TL-LABEL
           MOVE WS-RUN-VANISHED            TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           MOVE 'COMMITS THIS RUN'         TO TL-LABEL
           MOVE WS-RUN-COMMITS             TO TL-COUNT
           PERFORM 7200-TOTAL-LINE
           IF WS-MODE-PURGE
               MOVE 'CUMULATIVE ROWS DELETED (CHECKPOINT)'
                                           TO TL-LABEL
               MOVE CKP-ROWS-DELETED       TO TL-COUNT
               PERFORM 7200-TOTAL-LINE
               MOVE 'CUMULATIVE DEBITS ARCHIVED' TO TL-LABEL
               MOVE ZERO                   TO TL-COUNT
               MOVE CKP-DEBIT-TOTAL        TO TL-AMOUNT
               PERFORM 7200-TOTAL-LINE
               MOVE 'CUMULATIVE CREDITS ARCHIVED' TO TL-LABEL
               MOVE CKP-CREDIT-TOTAL       TO TL-AMOUNT
               PERFORM 7200-TOTAL-LINE
               MOVE 'RESTARTS FOR THIS PASS' TO TL-LABEL
               MOVE CKP-RESTART-COUNT      TO TL-COUNT
               MOVE ZERO                   TO TL-AMOUNT
               PERFORM 7200-TOTAL-LINE
           END-IF
           IF WS-STOPPED-AT-MAX
               MOVE SPACES                 TO TX-TEXT
               MOVE '*** MAXROWS REACHED - PURGE INCOMPLETE, NEXT RUN'
                                           TO TX-TEXT (1:49)
               MOVE ' CONTINUES ***'       TO TX-TEXT (50:14)
               WRITE REPORT-RECORD         FROM WS-TEXT-LINE
           END-IF
           CLOSE CHKPT-FILE REPORT-FILE
           DISPLAY 'PSARC010 - ARCHIVED ' WS-RUN-UNLOADED
                   ' DELETED ' WS-RUN-DELETED
                   ' COMMITS ' WS-RUN-COMMITS
           IF WS-STOPPED-AT-MAX
           OR WS-RUN-VANISHED > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
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
