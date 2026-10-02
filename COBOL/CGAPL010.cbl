       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CGAPL010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2009-10-01.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CGAPL010                                            *
      * TITLE    : ACCOUNT CHARGES - APPLY NOTIFIED CHARGES            *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : CGAPL010                                            *
      * JCL      : CGCHG01 STEP CG040                                  *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (GL CODES)            *
      *            ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM    *
      *                       READ ONLY - STATUS CHECK)                *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : CHGPEND  - PENDING CHARGES (VSAM KSDS, SEQ I-O)     *
      *            MBK.CHGHIST (INSERT) - CHARGES APPLIED              *
      * OUTPUT   : CHGPOST  - CHARGE POSTINGS FOR PSEOD01 - PSTXNREC   *
      *            GLJNL    - GL FEE INCOME JOURNAL - GLJNLREC         *
      *            REPORT   - CHARGES APPLIED / DEFERRED (FBA 133)     *
      *----------------------------------------------------------------*
      * A PENDING CHARGE IS APPLIED WHEN:                              *
      *   - ITS STATUS IS 'N' (CUSTOMER NOTIFIED BY CGNTF010), AND     *
      *   - ITS APPLY DATE IS TODAY OR EARLIER                         *
      * ONE 'CHG ' HARD POSTING IS WRITTEN PER CHARGE ITEM. THE        *
      * POSTINGS ARE MERGED INTO TONIGHT'S PSEOD01 RUN (CGCHG01 RUNS   *
      * BEFORE PSEOD01 IN THE NETWORK - SEE SCHED MBKEOD).             *
      *                                                                *
      * A CHARGE STILL AT STATUS 'C' ON ITS APPLY DATE WAS NEVER       *
      * NOTIFIED (E.G. CGNTF010 FAILED). IT IS NOT APPLIED; ITS APPLY  *
      * DATE IS MOVED ON BY THE NOTIFICATION PERIOD AND IT IS REPORTED *
      * AS DEFERRED. COMPLIANCE REQUIREMENT CMP-2009-14.               *
      *                                                                *
      * IF THE ACCOUNT HAS CLOSED SINCE THE CHARGE WAS CALCULATED THE  *
      * CHARGE IS MARKED 'F' (FAILED) AND NOT POSTED - DDCLS010 TAKES  *
      * ANY OUTSTANDING CHARGES INTO THE CLOSING BALANCE ITSELF.       *
      *                                                                *
      * APPLIED AND FAILED RECORDS OLDER THAN 90 DAYS ARE DELETED FROM *
      * CHGPEND (THE PERMANENT RECORD IS MBK.CHGHIST).                 *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2009-10-01 AOS  ORIGINAL VERSION (PSR 2009 CHARGES PROJECT)    *
      * 2010-02-15 AOS  HOUSEKEEPING OF OLD CHGPEND RECORDS            *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CHGPEND-FILE     ASSIGN TO CHGPEND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS CGP-KEY
                                   FILE STATUS IS WS-CHGPEND-STATUS.
           SELECT CHGPOST-FILE     ASSIGN TO CHGPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CHGPOST-STATUS.
           SELECT GLJNL-FILE       ASSIGN TO GLJNL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLJNL-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  CHGPEND-FILE.
           COPY CGCHGREC.
      *
       FD  CHGPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHGPOST-RECORD                  PIC X(200).
      *
       FD  GLJNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJNL-RECORD                    PIC X(250).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CGAPL010'.
      *
       01  WS-CONSTANTS.
           05  WS-RETAIN-DAYS              PIC S9(05) COMP-3 VALUE -90.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 23550000.
      *
       01  WS-FILE-STATUSES.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-NOTFND           VALUE '23'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-CHGPEND-STATUS           PIC X(02).
               88  WS-CHGPEND-OK               VALUE '00'.
               88  WS-CHGPEND-EOF              VALUE '10'.
           05  WS-CHGPOST-STATUS           PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CHARGES           VALUE 'Y'.
           05  WS-ACCT-OK-IND              PIC X(01) VALUE 'N'.
               88  WS-ACCT-CHARGEABLE          VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-PURGE-BEFORE             PIC 9(08).
           05  WS-LAST-ACCT-KEY            PIC X(14) VALUE LOW-VALUES.
           05  WS-LAST-PROD-CD             PIC X(10) VALUE LOW-VALUES.
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DB2                 PIC X(10).
           05  WS-ACTION                   PIC X(10).
           05  WS-REVERSED                 PIC X(35).
           05  WS-TRAILING                 PIC S9(04) COMP.
      *
      *---- GL ACCUMULATION BY PRODUCT (SMALL - TYPICALLY < 40) -------*
       01  WS-GL-TABLE.
           05  WS-GL-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-GL-ENTRY                 OCCURS 300 TIMES
                                           INDEXED BY GL-IDX.
               10  WS-GL-PROD-CD           PIC X(10).
               10  WS-GL-BALANCE-ACCT      PIC X(10).
               10  WS-GL-FEE-INCOME        PIC X(10).
               10  WS-GL-COST-CENTRE       PIC X(06).
               10  WS-GL-CCY-CD            PIC X(03).
               10  WS-GL-AMOUNT            PIC S9(13)V99 COMP-3.
       01  WS-GL-CONTROL.
           05  WS-GL-LINE-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-GL-TOTAL-DR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-TOTAL-CR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-APPLIED-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-DEFERRED-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-FAILED-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOT-DUE-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PURGED-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-OTHER-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-APPLIED-TOTAL            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-DEFERRED-TOTAL           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-FAILED-TOTAL             PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
      *
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CGAPL010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - ACCOUNT CHARGES APPLIED'.
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
           05  FILLER            PIC X(12) VALUE 'PERIOD END'.
           05  FILLER            PIC X(06) VALUE 'TYPE'.
           05  FILLER            PIC X(14) VALUE '    NET AMOUNT'.
           05  FILLER            PIC X(03) VALUE SPACES.
           05  FILLER            PIC X(12) VALUE 'APPLY DATE'.
           05  FILLER            PIC X(11) VALUE 'ACTION'.
           05  FILLER            PIC X(35) VALUE 'DESCRIPTION'.
           05  FILLER            PIC X(20) VALUE SPACES.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PERIOD-END               PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TYPE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NET                      PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  DL-APPLY-DATE               PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DESC                     PIC X(35).
           05  FILLER                      PIC X(20) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLCHGH END-EXEC.
      *
           COPY PSTXNREC.
      *
           COPY GLJNLREC.
      *
           COPY DTEPARM.
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
           PERFORM 2000-PROCESS-CHARGE
               UNTIL WS-END-OF-CHARGES
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       ACCTMAST-FILE
                       DATECARD-FILE
                I-O    CHGPEND-FILE
                OUTPUT CHGPOST-FILE
                       GLJNL-FILE
                       REPORT-FILE

           IF NOT WS-PRDPARM-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CHGPEND-OK
           OR WS-CHGPOST-STATUS NOT = '00'
           OR WS-GLJNL-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-PRDPARM-STATUS ' ' WS-ACCTMAST-STATUS ' '
                      WS-CHGPEND-STATUS ' ' WS-CHGPOST-STATUS ' '
                      WS-GLJNL-STATUS ' ' WS-REPORT-STATUS ' '
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
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE

           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO HD1-BUS-DATE

      *    HOUSEKEEPING CUT-OFF.
           MOVE 'D'                        TO DTE-FUNCTION
           MOVE WS-BUS-DATE                TO DTE-DATE-IN
           MOVE WS-RETAIN-DAYS             TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO WS-PURGE-BEFORE

           DISPLAY 'CGAPL010 - BUSINESS DATE ' WS-DATE-DB2
                   ' PURGE APPLIED BEFORE ' WS-PURGE-BEFORE

           PERFORM 8000-READ-CHARGE
           .
      *================================================================*
       2000-PROCESS-CHARGE.
      *================================================================*
           EVALUATE TRUE
      *        HOUSEKEEPING - FINISHED RECORDS PAST RETENTION
               WHEN (CGP-APPLIED OR CGP-FAILED OR CGP-WAIVED
                     OR CGP-SUPPRESSED)
               AND  CGP-APPLY-DATE < WS-PURGE-BEFORE
                   PERFORM 2900-PURGE-RECORD
               WHEN CGP-APPLY-DATE > WS-BUS-DATE
                   ADD 1                   TO WS-NOT-DUE-COUNT
               WHEN CGP-NOTIFIED
                   PERFORM 2100-CHECK-ACCOUNT
                   IF WS-ACCT-CHARGEABLE
                       PERFORM 2200-APPLY-CHARGE
                   ELSE
                       PERFORM 2300-FAIL-CHARGE
                   END-IF
               WHEN CGP-CALCULATED
                   PERFORM 2400-DEFER-CHARGE
               WHEN OTHER
                   ADD 1                   TO WS-OTHER-COUNT
           END-EVALUATE

           PERFORM 8000-READ-CHARGE
           .
      *================================================================*
       2100-CHECK-ACCOUNT.
      *================================================================*
      *    ONE READ PER ACCOUNT - CHARGES FOR AN ACCOUNT ARE ADJACENT.
           IF CGP-SORT-CODE NOT = WS-LAST-ACCT-KEY (1:6)
           OR CGP-ACCT-NO   NOT = WS-LAST-ACCT-KEY (7:8)
               MOVE CGP-SORT-CODE          TO ACM-SORT-CODE
               MOVE CGP-ACCT-NO            TO ACM-ACCT-NO
               READ ACCTMAST-FILE
               EVALUATE TRUE
                   WHEN WS-ACCTMAST-OK
                       IF ACM-CLOSED OR ACM-PENDING-CLOSE
                           MOVE 'N'        TO WS-ACCT-OK-IND
                       ELSE
                           SET WS-ACCT-CHARGEABLE TO TRUE
                       END-IF
                   WHEN WS-ACCTMAST-NOTFND
                       MOVE 'N'            TO WS-ACCT-OK-IND
                   WHEN OTHER
                       MOVE '2100-CHECK-ACCOUNT' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                       MOVE CGP-KEY        TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
               END-EVALUATE
               MOVE CGP-SORT-CODE          TO WS-LAST-ACCT-KEY (1:6)
               MOVE CGP-ACCT-NO            TO WS-LAST-ACCT-KEY (7:8)
           END-IF
           .
      *================================================================*
       2200-APPLY-CHARGE.
      *================================================================*
           PERFORM 2210-WRITE-POSTING
           PERFORM 2220-INSERT-CHGHIST
           PERFORM 2230-ACCUMULATE-GL

           SET CGP-APPLIED                 TO TRUE
           MOVE PST-EXT-REF                TO CGP-POST-EXT-REF
           PERFORM 2800-REWRITE-CHARGE

           ADD 1                           TO WS-APPLIED-COUNT
           ADD CGP-NET-AMT                 TO WS-APPLIED-TOTAL
           MOVE 'APPLIED'                  TO WS-ACTION
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2210-WRITE-POSTING.
      *================================================================*
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE CGP-SORT-CODE              TO PST-SORT-CODE
           MOVE CGP-ACCT-NO                TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'CHG '                     TO PST-TXN-CODE
           SET PST-DEBIT                   TO TRUE
           MOVE CGP-NET-AMT                TO PST-AMOUNT
           MOVE ACM-CCY-CD                 TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           MOVE CGP-DESCRIPTION            TO PST-NARRATIVE-1
           MOVE CGP-PERIOD-START           TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           STRING 'PERIOD ' WS-DATE-DB2 ' TO '
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE CGP-PERIOD-END             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO PST-NARRATIVE-2 (22:10)
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           STRING 'CG' CGP-PERIOD-END CGP-CHARGE-TYPE CGP-ITEM-SEQ
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID

           WRITE CHGPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-CHGPOST-STATUS NOT = '00'
               MOVE '2210-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHGPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CHGPOST-STATUS      TO ABN-FILE-STATUS
               MOVE CGP-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2220-INSERT-CHGHIST.
      *================================================================*
           MOVE CGP-SORT-CODE              TO CGH-SORT-CODE
           MOVE CGP-ACCT-NO                TO CGH-ACCT-NO
           MOVE CGP-PERIOD-END             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO CGH-PERIOD-END
           MOVE CGP-CHARGE-TYPE            TO CGH-CHARGE-TYPE
           MOVE CGP-ITEM-SEQ               TO CGH-ITEM-SEQ
           MOVE CGP-CIF-NO                 TO CGH-CIF-NO
           MOVE CGP-PROD-CD                TO CGH-PROD-CD
           MOVE CGP-PERIOD-START           TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO CGH-PERIOD-START
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO CGH-APPLY-DATE
           MOVE CGP-ITEM-COUNT             TO CGH-ITEM-COUNT
           MOVE CGP-GROSS-AMT              TO CGH-GROSS-AMT
           MOVE CGP-CAP-REDUCTION          TO CGH-CAP-REDUCTION
           MOVE CGP-NET-AMT                TO CGH-NET-AMT
           MOVE ZERO                       TO CGH-REFUND-AMT
           MOVE SPACES                     TO CGH-REFUND-REASON
                                              CGH-REFUND-USER
           MOVE CGP-DESCRIPTION            TO CGH-DESCRIPTION-TEXT
           MOVE FUNCTION REVERSE (CGP-DESCRIPTION) TO WS-REVERSED
           MOVE ZERO                       TO WS-TRAILING
           INSPECT WS-REVERSED TALLYING WS-TRAILING
                   FOR LEADING SPACES
           COMPUTE CGH-DESCRIPTION-LEN = 35 - WS-TRAILING
           MOVE PST-EXT-REF                TO CGH-EXT-REF
           MOVE SPACES                     TO CGH-BATCH-RUN-ID
           MOVE 'CG'                       TO CGH-BATCH-RUN-ID (1:2)
           MOVE DTC-RUN-ID                 TO CGH-BATCH-RUN-ID (3:5)

           EXEC SQL
               INSERT INTO MBK.CHGHIST
                     (SORT_CODE, ACCT_NO, PERIOD_END, CHARGE_TYPE,
                      ITEM_SEQ, CIF_NO, PROD_CD, PERIOD_START,
                      APPLY_DATE, ITEM_COUNT, GROSS_AMT,
                      CAP_REDUCTION, NET_AMT, REFUND_AMT,
                      REFUND_DATE, REFUND_REASON, REFUND_USER,
                      DESCRIPTION, EXT_REF, BATCH_RUN_ID, INSERT_TS)
               VALUES
                     (:CGH-SORT-CODE, :CGH-ACCT-NO, :CGH-PERIOD-END,
                      :CGH-CHARGE-TYPE, :CGH-ITEM-SEQ, :CGH-CIF-NO,
                      :CGH-PROD-CD, :CGH-PERIOD-START,
                      :CGH-APPLY-DATE, :CGH-ITEM-COUNT,
                      :CGH-GROSS-AMT, :CGH-CAP-REDUCTION,
                      :CGH-NET-AMT, :CGH-REFUND-AMT, NULL,
                      :CGH-REFUND-REASON, :CGH-REFUND-USER,
                      :CGH-DESCRIPTION, :CGH-EXT-REF,
                      :CGH-BATCH-RUN-ID, CURRENT TIMESTAMP)
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
                   DISPLAY 'CGAPL010 W001 CHGHIST ROW EXISTS - '
                           'RERUN? ' CGP-KEY
               WHEN OTHER
                   MOVE '2220-INSERT-CHGHIST' TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       2230-ACCUMULATE-GL.
      *================================================================*
      *    FIND (OR ADD) THE PRODUCT IN THE GL TABLE. THE PRODUCT
      *    RECORD IS READ ONCE PER PRODUCT.
           SET GL-IDX                      TO 1
           SEARCH WS-GL-ENTRY
               AT END
                   PERFORM 2235-ADD-GL-PRODUCT
               WHEN GL-IDX > WS-GL-COUNT
                   PERFORM 2235-ADD-GL-PRODUCT
               WHEN WS-GL-PROD-CD (GL-IDX) = CGP-PROD-CD
                   CONTINUE
           END-SEARCH
           ADD CGP-NET-AMT                 TO WS-GL-AMOUNT (GL-IDX)
           .
      *================================================================*
       2235-ADD-GL-PRODUCT.
      *================================================================*
           IF WS-GL-COUNT >= 300
               MOVE '2235-ADD-GL-PRODUCT'  TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL PRODUCT TABLE FULL'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-GL-COUNT
           SET GL-IDX                      TO WS-GL-COUNT
           MOVE CGP-PROD-CD                TO WS-GL-PROD-CD (GL-IDX)
                                              PRD-PROD-CD
           MOVE ZERO                       TO WS-GL-AMOUNT (GL-IDX)
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               MOVE PRD-GL-BALANCE-DR      TO WS-GL-BALANCE-ACCT
                                                         (GL-IDX)
               MOVE PRD-GL-FEE-INCOME      TO WS-GL-FEE-INCOME
                                                         (GL-IDX)
               MOVE PRD-COST-CENTRE        TO WS-GL-COST-CENTRE
                                                         (GL-IDX)
               MOVE PRD-CCY-CD             TO WS-GL-CCY-CD (GL-IDX)
           ELSE
      *        NO PRODUCT - POST TO THE CHARGES SUSPENSE ACCOUNT SO
      *        FINANCE CAN CLEAR IT MANUALLY.
               MOVE '29900-0010'           TO WS-GL-BALANCE-ACCT
                                                         (GL-IDX)
               MOVE '49900-0010'           TO WS-GL-FEE-INCOME
                                                         (GL-IDX)
               MOVE '000000'               TO WS-GL-COST-CENTRE
                                                         (GL-IDX)
               MOVE 'GBP'                  TO WS-GL-CCY-CD (GL-IDX)
               DISPLAY 'CGAPL010 W002 PRODUCT NOT FOUND FOR GL - '
                       CGP-PROD-CD ' - POSTED TO SUSPENSE'
           END-IF
           .
      *================================================================*
       2300-FAIL-CHARGE.
      *================================================================*
           SET CGP-FAILED                  TO TRUE
           MOVE 'ACLS'                     TO CGP-SUPPRESS-RSN
           PERFORM 2800-REWRITE-CHARGE
           ADD 1                           TO WS-FAILED-COUNT
           ADD CGP-NET-AMT                 TO WS-FAILED-TOTAL
           MOVE 'ACCT CLOSD'               TO WS-ACTION
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2400-DEFER-CHARGE.
      *================================================================*
      *    NEVER NOTIFIED - MOVE THE APPLY DATE ON 14 DAYS FROM TODAY.
      *    CGNTF010 WILL PICK IT UP TONIGHT AND NOTIFY IT.
           MOVE 'D'                        TO DTE-FUNCTION
           MOVE WS-BUS-DATE                TO DTE-DATE-IN
           MOVE 14                         TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO CGP-APPLY-DATE
           PERFORM 2800-REWRITE-CHARGE
           ADD 1                           TO WS-DEFERRED-COUNT
           ADD CGP-NET-AMT                 TO WS-DEFERRED-TOTAL
           MOVE 'DEFERRED'                 TO WS-ACTION
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2800-REWRITE-CHARGE.
      *================================================================*
           REWRITE CGP-CHARGE-RECORD
           IF NOT WS-CHGPEND-OK
               MOVE '2800-REWRITE-CHARGE'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHGPEND REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-CHGPEND-STATUS      TO ABN-FILE-STATUS
               MOVE CGP-KEY                TO ABN-KEY-DATA
               EXEC SQL ROLLBACK END-EXEC
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2900-PURGE-RECORD.
      *================================================================*
           DELETE CHGPEND-FILE RECORD
           IF NOT WS-CHGPEND-OK
               MOVE '2900-PURGE-RECORD'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHGPEND DELETE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-CHGPEND-STATUS      TO ABN-FILE-STATUS
               MOVE CGP-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-PURGED-COUNT
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC

           PERFORM 3100-WRITE-GL-JOURNAL
           PERFORM 3200-RUN-TOTALS

           CLOSE PRDPARM-FILE
                 ACCTMAST-FILE
                 CHGPEND-FILE
                 CHGPOST-FILE
                 GLJNL-FILE
                 REPORT-FILE

           DISPLAY 'CGAPL010 - CHARGES READ       ' WS-READ-COUNT
           DISPLAY 'CGAPL010 - APPLIED            ' WS-APPLIED-COUNT
           DISPLAY 'CGAPL010 - DEFERRED           ' WS-DEFERRED-COUNT
           DISPLAY 'CGAPL010 - FAILED (CLOSED)    ' WS-FAILED-COUNT
           DISPLAY 'CGAPL010 - NOT YET DUE        ' WS-NOT-DUE-COUNT
           DISPLAY 'CGAPL010 - PURGED             ' WS-PURGED-COUNT

      *    DEFERRALS MEAN CUSTOMERS WERE NOT NOTIFIED ON TIME -
      *    COMPLIANCE MUST BE TOLD (RC 4 RAISES A TICKET).
           IF WS-DEFERRED-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       3100-WRITE-GL-JOURNAL.
      *================================================================*
      *    DR CUSTOMER BALANCES (DEBIT GL)   CR FEE INCOME
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-CHARGES             TO TRUE
           STRING 'CHRG' WS-BUS-DATE DTC-RUN-ID (1:4)
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE DTC-RUN-ID                 TO GLJH-RUN-ID
           MOVE FUNCTION CURRENT-DATE (1:8) TO GLJH-CREATE-DATE
           MOVE FUNCTION CURRENT-DATE (9:6) TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3190-WRITE-GL-RECORD

           PERFORM VARYING GL-IDX FROM 1 BY 1
                   UNTIL GL-IDX > WS-GL-COUNT
               IF WS-GL-AMOUNT (GL-IDX) > ZERO
                   MOVE WS-GL-BALANCE-ACCT (GL-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT          TO TRUE
                   MOVE 'ACCOUNT CHARGES APPLIED'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-GL-FEE-INCOME (GL-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT         TO TRUE
                   MOVE 'ACCOUNT CHARGES APPLIED'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
               END-IF
           END-PERFORM

           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-GL-LINE-COUNT           TO GLJT-LINE-COUNT
           MOVE WS-GL-TOTAL-DR             TO GLJT-TOTAL-DR
           MOVE WS-GL-TOTAL-CR             TO GLJT-TOTAL-CR
           MOVE 1                          TO GLJT-JOURNAL-COUNT
           PERFORM 3190-WRITE-GL-RECORD

           IF WS-GL-TOTAL-DR NOT = WS-APPLIED-TOTAL
               MOVE '3100-WRITE-GL-JOURNAL' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL CHARGES DO NOT EQUAL CHARGES APPLIED'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3180-GL-DETAIL.
      *================================================================*
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-GL-LINE-COUNT
           MOVE WS-GL-LINE-COUNT           TO GLJD-LINE-NO
           MOVE WS-GL-COST-CENTRE (GL-IDX) TO GLJD-COST-CENTRE
           MOVE WS-GL-PROD-CD (GL-IDX)     TO GLJD-PROD-CD
           MOVE WS-GL-CCY-CD (GL-IDX)      TO GLJD-CCY-CD
           MOVE WS-GL-AMOUNT (GL-IDX)      TO GLJD-AMOUNT
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE SPACES                     TO GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE 'CHG '                     TO GLJD-TXN-CODE
           MOVE GLJ-JOURNAL-ID             TO GLJD-SOURCE-REF
           IF GLJD-DEBIT
               ADD GLJD-AMOUNT             TO WS-GL-TOTAL-DR
           ELSE
               ADD GLJD-AMOUNT             TO WS-GL-TOTAL-CR
           END-IF
           PERFORM 3190-WRITE-GL-RECORD
           .
      *================================================================*
       3190-WRITE-GL-RECORD.
      *================================================================*
           WRITE GLJNL-RECORD              FROM GLJ-JOURNAL-LINE
           IF WS-GLJNL-STATUS NOT = '00'
               MOVE '3190-WRITE-GL-RECORD' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'GLJNL WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-GLJNL-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3200-RUN-TOTALS.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS

           MOVE 'CHARGE RECORDS READ'      TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           MOVE 'CHARGES APPLIED'          TO TL-LABEL
           MOVE WS-APPLIED-COUNT           TO TL-COUNT
           MOVE WS-APPLIED-TOTAL           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           MOVE 'DEFERRED - NOT NOTIFIED'  TO TL-LABEL
           MOVE WS-DEFERRED-COUNT          TO TL-COUNT
           MOVE WS-DEFERRED-TOTAL          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           MOVE 'FAILED - ACCOUNT CLOSED'  TO TL-LABEL
           MOVE WS-FAILED-COUNT            TO TL-COUNT
           MOVE WS-FAILED-TOTAL            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           MOVE 'NOT YET DUE'              TO TL-LABEL
           MOVE WS-NOT-DUE-COUNT           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           MOVE 'PURGED (OVER 90 DAYS)'    TO TL-LABEL
           MOVE WS-PURGED-COUNT            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
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
           MOVE CGP-SORT-CODE              TO DL-SORT-CODE
           MOVE CGP-ACCT-NO                TO DL-ACCT-NO
           MOVE CGP-PERIOD-END             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO DL-PERIOD-END
           MOVE CGP-CHARGE-TYPE            TO DL-TYPE
           MOVE CGP-NET-AMT                TO DL-NET
           MOVE CGP-APPLY-DATE             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO DL-APPLY-DATE
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE CGP-DESCRIPTION            TO DL-DESC
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-CHARGE.
      *================================================================*
           READ CHGPEND-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-CHGPEND-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-CHGPEND-EOF
                   SET WS-END-OF-CHARGES   TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-CHARGE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CHGPEND READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-CHGPEND-STATUS  TO ABN-FILE-STATUS
                   EXEC SQL ROLLBACK END-EXEC
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8900-FORMAT-DB2-DATE.
      *================================================================*
           MOVE SPACES                     TO WS-DATE-DB2
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-DATE-DB2
           END-STRING
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           MOVE CGP-KEY                    TO ABN-KEY-DATA
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
