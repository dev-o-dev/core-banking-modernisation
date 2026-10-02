       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CGREF010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2007-08-20.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CGREF010                                            *
      * TITLE    : ACCOUNT CHARGES - REFUNDS                           *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : CGREF010                                            *
      * JCL      : CGREF01 STEP CR010                                  *
      *----------------------------------------------------------------*
      * INPUT    : REFUNDIN - REFUND REQUESTS KEYED BY CUSTOMER        *
      *                       RELATIONS ON ISPF PANEL MBKCGR1 (FB 120) *
      *            PRDPARM  - PRODUCT PARAMETERS (GL CODES)            *
      *            ACCTMAST - DDA ACCOUNT MASTER (STATUS CHECK)        *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : MBK.CHGHIST - REFUND AMOUNT / DATE / REASON / USER  *
      * OUTPUT   : REFPOST  - REFUND POSTINGS FOR PSEOD01 - PSTXNREC   *
      *            GLJNL    - GL REFUND JOURNAL - GLJNLREC             *
      *            REPORT   - REFUNDS ACCEPTED / REJECTED (FBA 133)    *
      *----------------------------------------------------------------*
      * CONTROLS (CUSTOMER RELATIONS PROCEDURE CR-PROC-11):            *
      *   - THE CHARGE MUST HAVE BEEN APPLIED (ROW IN MBK.CHGHIST)     *
      *   - THE REFUND MAY NOT TAKE TOTAL REFUNDS ABOVE THE NET CHARGE *
      *   - THE REQUESTER AND THE APPROVER MUST BE DIFFERENT USERS     *
      *   - REFUNDS OVER WS-L2-LIMIT NEED A LEVEL 2 APPROVER (USER ID  *
      *     IN THE WS-L2-APPROVERS TABLE BELOW - YES, HARD CODED;      *
      *     CHANGED BY CR WHEN THE TEAM CHANGES. PIR 2008/150.)        *
      *   - REASON CODE MUST BE VALID                                  *
      *   - A REFUND TO A CLOSED ACCOUNT IS REJECTED - CUSTOMER        *
      *     RELATIONS PAY BY CHEQUE FROM THE COMPLAINTS LEDGER         *
      * REQUEST AMOUNT ZERO MEANS "REFUND THE REMAINING BALANCE OF THE *
      * CHARGE".                                                       *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   GDWL GOODWILL       CMPL COMPLAINT UPHELD                    *
      *   BERR BANK ERROR     FOSD FINANCIAL OMBUDSMAN DECISION        *
      *   HARD FINANCIAL HARDSHIP (COLLECTIONS REFERRAL)               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2007-08-20 JNB  ORIGINAL VERSION (OFT TEST CASE - BANK CHARGES *
      *                 RECLAIMS; REPLACES MANUAL BRANCH CREDITS)      *
      * 2008-04-07 JNB  LEVEL 2 APPROVAL ABOVE 250.00                  *
      * 2009-10-01 AOS  READS CHGHIST (WAS CHGFILE VSAM)               *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REFUNDIN-FILE    ASSIGN TO REFUNDIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REFUNDIN-STATUS.
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
           SELECT REFPOST-FILE     ASSIGN TO REFPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REFPOST-STATUS.
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
       FD  REFUNDIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REFUNDIN-RECORD                 PIC X(120).
      *
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  REFPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REFPOST-RECORD                  PIC X(200).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CGREF010'.
      *
       01  WS-CONSTANTS.
           05  WS-L2-LIMIT                 PIC S9(13)V99 COMP-3
                                           VALUE +250.00.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 23560000.
      *
      *---- LEVEL 2 APPROVERS (CUSTOMER RELATIONS TEAM LEADERS) -------*
       01  WS-L2-APPROVER-VALUES.
           05  FILLER                      PIC X(08) VALUE 'CRTL0001'.
           05  FILLER                      PIC X(08) VALUE 'CRTL0002'.
           05  FILLER                      PIC X(08) VALUE 'CRTL0004'.
           05  FILLER                      PIC X(08) VALUE 'CRTL0007'.
           05  FILLER                      PIC X(08) VALUE 'CRMGR001'.
           05  FILLER                      PIC X(08) VALUE 'CRMGR002'.
       01  WS-L2-APPROVER-TABLE REDEFINES WS-L2-APPROVER-VALUES.
           05  WS-L2-APPROVER              PIC X(08)
                                           OCCURS 6 TIMES
                                           INDEXED BY L2-IDX.
      *
      *---- REFUND REQUEST LAYOUT (ISPF PANEL MBKCGR1 OUTPUT) ---------*
       01  WS-REFUND-REQUEST.
           05  RFQ-SORT-CODE               PIC X(06).
           05  RFQ-ACCT-NO                 PIC X(08).
           05  RFQ-PERIOD-END              PIC 9(08).
           05  RFQ-CHARGE-TYPE             PIC X(04).
           05  RFQ-ITEM-SEQ                PIC 9(06).
           05  RFQ-AMOUNT                  PIC 9(09)V99.
           05  RFQ-REASON-CD               PIC X(04).
               88  RFQ-REASON-VALID            VALUE 'GDWL' 'CMPL'
                                                     'BERR' 'FOSD'
                                                     'HARD'.
           05  RFQ-REQUEST-USER            PIC X(08).
           05  RFQ-APPROVE-USER            PIC X(08).
           05  RFQ-CASE-REF                PIC X(12).
           05  RFQ-NARRATIVE               PIC X(35).
           05  FILLER                      PIC X(10).
      *
       01  WS-FILE-STATUSES.
           05  WS-REFUNDIN-STATUS          PIC X(02).
               88  WS-REFUNDIN-OK              VALUE '00'.
               88  WS-REFUNDIN-EOF             VALUE '10'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-REFPOST-STATUS           PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-REQUESTS          VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-REJECT-CD                PIC X(04).
           05  WS-REJECT-TEXT              PIC X(40).
           05  WS-REFUND-AMT               PIC S9(13)V99 COMP-3.
           05  WS-REMAINING                PIC S9(13)V99 COMP-3.
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DB2                 PIC X(10).
      *
       01  WS-GL-TABLE.
           05  WS-GL-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-GL-ENTRY                 OCCURS 100 TIMES
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
           05  WS-ACCEPT-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-REJECT-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCEPT-TOTAL             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-REJECT-TOTAL             PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CGREF010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - CHARGE REFUNDS'.
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
           05  FILLER            PIC X(11) VALUE 'PERIOD END'.
           05  FILLER            PIC X(06) VALUE 'TYPE'.
           05  FILLER            PIC X(14) VALUE '        AMOUNT'.
           05  FILLER            PIC X(02) VALUE SPACES.
           05  FILLER            PIC X(05) VALUE 'RSN'.
           05  FILLER            PIC X(09) VALUE 'REQUEST'.
           05  FILLER            PIC X(09) VALUE 'APPROVE'.
           05  FILLER            PIC X(13) VALUE 'CASE REF'.
           05  FILLER            PIC X(09) VALUE 'RESULT'.
           05  FILLER            PIC X(35) VALUE 'REASON'.
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
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TYPE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-REASON                   PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-REQUEST-USER             PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-APPROVE-USER             PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CASE-REF                 PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-RESULT                   PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TEXT                     PIC X(34).
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
       01  WS-REFUND-IND                   PIC S9(04) COMP.
      *
           COPY PSTXNREC.
      *
           COPY GLJNLREC.
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
           PERFORM 2000-PROCESS-REQUEST
               UNTIL WS-END-OF-REQUESTS
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  REFUNDIN-FILE
                       PRDPARM-FILE
                       ACCTMAST-FILE
                       DATECARD-FILE
                OUTPUT REFPOST-FILE
                       GLJNL-FILE
                       REPORT-FILE

           IF NOT WS-REFUNDIN-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-REFPOST-STATUS NOT = '00'
           OR WS-GLJNL-STATUS NOT = '00'
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
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO HD1-BUS-DATE

           PERFORM 8000-READ-REQUEST
           .
      *================================================================*
       2000-PROCESS-REQUEST.
      *================================================================*
           MOVE SPACES                     TO WS-REJECT-CD
                                              WS-REJECT-TEXT

           PERFORM 2100-VALIDATE-REQUEST
           IF WS-REJECT-CD = SPACES
               PERFORM 2200-CHECK-CHARGE
           END-IF
           IF WS-REJECT-CD = SPACES
               PERFORM 2300-CHECK-ACCOUNT
           END-IF

           IF WS-REJECT-CD = SPACES
               PERFORM 2400-APPLY-REFUND
           ELSE
               ADD 1                       TO WS-REJECT-COUNT
               ADD RFQ-AMOUNT              TO WS-REJECT-TOTAL
               MOVE RFQ-AMOUNT             TO WS-REFUND-AMT
               MOVE 'REJECTED'             TO DL-RESULT
               MOVE WS-REJECT-TEXT         TO DL-TEXT
               PERFORM 7100-REPORT-DETAIL
           END-IF

           PERFORM 8000-READ-REQUEST
           .
      *================================================================*
       2100-VALIDATE-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN RFQ-SORT-CODE NOT NUMERIC
               WHEN RFQ-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO WS-REJECT-CD
                   MOVE 'SORT CODE / ACCOUNT INVALID'
                                           TO WS-REJECT-TEXT
               WHEN RFQ-PERIOD-END NOT NUMERIC
               WHEN RFQ-ITEM-SEQ NOT NUMERIC
                   MOVE 'INVK'             TO WS-REJECT-CD
                   MOVE 'CHARGE KEY INVALID'
                                           TO WS-REJECT-TEXT
               WHEN RFQ-AMOUNT NOT NUMERIC
                   MOVE 'INVM'             TO WS-REJECT-CD
                   MOVE 'AMOUNT NOT NUMERIC'
                                           TO WS-REJECT-TEXT
               WHEN NOT RFQ-REASON-VALID
                   MOVE 'INVR'             TO WS-REJECT-CD
                   MOVE 'REASON CODE NOT VALID'
                                           TO WS-REJECT-TEXT
               WHEN RFQ-REQUEST-USER = SPACES
               WHEN RFQ-APPROVE-USER = SPACES
                   MOVE 'NOAP'             TO WS-REJECT-CD
                   MOVE 'REQUESTER / APPROVER MISSING'
                                           TO WS-REJECT-TEXT
               WHEN RFQ-REQUEST-USER = RFQ-APPROVE-USER
                   MOVE 'SAME'             TO WS-REJECT-CD
                   MOVE 'REQUESTER MAY NOT APPROVE OWN REFUND'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       2200-CHECK-CHARGE.
      *================================================================*
           MOVE RFQ-SORT-CODE              TO CGH-SORT-CODE
           MOVE RFQ-ACCT-NO                TO CGH-ACCT-NO
           MOVE RFQ-PERIOD-END             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO CGH-PERIOD-END
           MOVE RFQ-CHARGE-TYPE            TO CGH-CHARGE-TYPE
           MOVE RFQ-ITEM-SEQ               TO CGH-ITEM-SEQ

           EXEC SQL
               SELECT NET_AMT, REFUND_AMT, PROD_CD, DESCRIPTION
                 INTO :CGH-NET-AMT, :CGH-REFUND-AMT, :CGH-PROD-CD,
                      :CGH-DESCRIPTION
                 FROM MBK.CHGHIST
                WHERE SORT_CODE   = :CGH-SORT-CODE
                  AND ACCT_NO     = :CGH-ACCT-NO
                  AND PERIOD_END  = :CGH-PERIOD-END
                  AND CHARGE_TYPE = :CGH-CHARGE-TYPE
                  AND ITEM_SEQ    = :CGH-ITEM-SEQ
                  FOR UPDATE OF REFUND_AMT, REFUND_DATE,
                                REFUND_REASON, REFUND_USER
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   COMPUTE WS-REMAINING = CGH-NET-AMT - CGH-REFUND-AMT
                   IF RFQ-AMOUNT = ZERO
                       MOVE WS-REMAINING   TO WS-REFUND-AMT
                   ELSE
                       MOVE RFQ-AMOUNT     TO WS-REFUND-AMT
                   END-IF
                   EVALUATE TRUE
                       WHEN WS-REMAINING NOT > ZERO
                           MOVE 'FREF'     TO WS-REJECT-CD
                           MOVE 'CHARGE ALREADY FULLY REFUNDED'
                                           TO WS-REJECT-TEXT
                       WHEN WS-REFUND-AMT > WS-REMAINING
                           MOVE 'XREF'     TO WS-REJECT-CD
                           MOVE 'AMOUNT EXCEEDS UNREFUNDED CHARGE'
                                           TO WS-REJECT-TEXT
                       WHEN WS-REFUND-AMT > WS-L2-LIMIT
                           PERFORM 2210-CHECK-LEVEL-2
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               WHEN +100
                   MOVE 'NCHG'             TO WS-REJECT-CD
                   MOVE 'CHARGE NOT FOUND IN CHGHIST'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   MOVE '2200-CHECK-CHARGE' TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       2210-CHECK-LEVEL-2.
      *================================================================*
           SET L2-IDX                      TO 1
           SEARCH WS-L2-APPROVER
               AT END
                   MOVE 'L2AP'             TO WS-REJECT-CD
                   MOVE 'OVER 250.00 - LEVEL 2 APPROVER NEEDED'
                                           TO WS-REJECT-TEXT
               WHEN WS-L2-APPROVER (L2-IDX) = RFQ-APPROVE-USER
                   CONTINUE
           END-SEARCH
           .
      *================================================================*
       2300-CHECK-ACCOUNT.
      *================================================================*
           MOVE RFQ-SORT-CODE              TO ACM-SORT-CODE
           MOVE RFQ-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   IF ACM-CLOSED
                       MOVE 'ACLS'         TO WS-REJECT-CD
                       MOVE 'ACCOUNT CLOSED - PAY BY CHEQUE'
                                           TO WS-REJECT-TEXT
                   END-IF
                   IF ACM-NO-CREDITS OR ACM-NO-POSTINGS
                       MOVE 'RSTC'         TO WS-REJECT-CD
                       MOVE 'CREDITS RESTRICTED ON ACCOUNT'
                                           TO WS-REJECT-TEXT
                   END-IF
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'ANFD'             TO WS-REJECT-CD
                   MOVE 'ACCOUNT NOT FOUND'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   MOVE '2300-CHECK-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2400-APPLY-REFUND.
      *================================================================*
           ADD WS-REFUND-AMT               TO CGH-REFUND-AMT
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO CGH-REFUND-DATE
           MOVE RFQ-REASON-CD              TO CGH-REFUND-REASON
           MOVE RFQ-APPROVE-USER           TO CGH-REFUND-USER

           EXEC SQL
               UPDATE MBK.CHGHIST
                  SET REFUND_AMT    = :CGH-REFUND-AMT,
                      REFUND_DATE   = :CGH-REFUND-DATE,
                      REFUND_REASON = :CGH-REFUND-REASON,
                      REFUND_USER   = :CGH-REFUND-USER
                WHERE SORT_CODE   = :CGH-SORT-CODE
                  AND ACCT_NO     = :CGH-ACCT-NO
                  AND PERIOD_END  = :CGH-PERIOD-END
                  AND CHARGE_TYPE = :CGH-CHARGE-TYPE
                  AND ITEM_SEQ    = :CGH-ITEM-SEQ
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2400-APPLY-REFUND'    TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           PERFORM 2410-WRITE-POSTING
           PERFORM 2420-ACCUMULATE-GL

      *    ONE REQUEST = ONE UNIT OF WORK.
           EXEC SQL COMMIT END-EXEC

           ADD 1                           TO WS-ACCEPT-COUNT
           ADD WS-REFUND-AMT               TO WS-ACCEPT-TOTAL
           MOVE 'ACCEPTED'                 TO DL-RESULT
           MOVE SPACES                     TO DL-TEXT
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2410-WRITE-POSTING.
      *================================================================*
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE RFQ-SORT-CODE              TO PST-SORT-CODE
           MOVE RFQ-ACCT-NO                TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'CHG '                     TO PST-TXN-CODE
           SET PST-CREDIT                  TO TRUE
           MOVE WS-REFUND-AMT              TO PST-AMOUNT
           MOVE ACM-CCY-CD                 TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           IF RFQ-NARRATIVE NOT = SPACES
               MOVE RFQ-NARRATIVE          TO PST-NARRATIVE-1
           ELSE
               MOVE 'REFUND OF CHARGES'    TO PST-NARRATIVE-1
           END-IF
           STRING CGH-DESCRIPTION-TEXT (1:23) ' ' CGH-PERIOD-END
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           STRING 'RF' RFQ-CASE-REF WS-POST-SEQ
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE RFQ-APPROVE-USER           TO PST-USER-ID

           WRITE REFPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-REFPOST-STATUS NOT = '00'
               MOVE '2410-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'REFPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-REFPOST-STATUS      TO ABN-FILE-STATUS
               EXEC SQL ROLLBACK END-EXEC
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2420-ACCUMULATE-GL.
      *================================================================*
           SET GL-IDX                      TO 1
           SEARCH WS-GL-ENTRY
               AT END
                   PERFORM 2425-ADD-GL-PRODUCT
               WHEN GL-IDX > WS-GL-COUNT
                   PERFORM 2425-ADD-GL-PRODUCT
               WHEN WS-GL-PROD-CD (GL-IDX) = CGH-PROD-CD
                   CONTINUE
           END-SEARCH
           ADD WS-REFUND-AMT               TO WS-GL-AMOUNT (GL-IDX)
           .
      *================================================================*
       2425-ADD-GL-PRODUCT.
      *================================================================*
           IF WS-GL-COUNT >= 100
               MOVE '2425-ADD-GL-PRODUCT'  TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL PRODUCT TABLE FULL' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-GL-COUNT
           SET GL-IDX                      TO WS-GL-COUNT
           MOVE CGH-PROD-CD                TO WS-GL-PROD-CD (GL-IDX)
                                              PRD-PROD-CD
           MOVE ZERO                       TO WS-GL-AMOUNT (GL-IDX)
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               MOVE PRD-GL-BALANCE-CR  TO WS-GL-BALANCE-ACCT (GL-IDX)
               MOVE PRD-GL-FEE-INCOME  TO WS-GL-FEE-INCOME (GL-IDX)
               MOVE PRD-COST-CENTRE    TO WS-GL-COST-CENTRE (GL-IDX)
               MOVE PRD-CCY-CD         TO WS-GL-CCY-CD (GL-IDX)
           ELSE
               MOVE '29900-0010'       TO WS-GL-BALANCE-ACCT (GL-IDX)
               MOVE '49900-0010'       TO WS-GL-FEE-INCOME (GL-IDX)
               MOVE '000000'           TO WS-GL-COST-CENTRE (GL-IDX)
               MOVE 'GBP'              TO WS-GL-CCY-CD (GL-IDX)
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 3100-WRITE-GL-JOURNAL

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'REQUESTS READ'            TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REFUNDS ACCEPTED'         TO TL-LABEL
           MOVE WS-ACCEPT-COUNT            TO TL-COUNT
           MOVE WS-ACCEPT-TOTAL            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REFUNDS REJECTED'         TO TL-LABEL
           MOVE WS-REJECT-COUNT            TO TL-COUNT
           MOVE WS-REJECT-TOTAL            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE REFUNDIN-FILE
                 PRDPARM-FILE
                 ACCTMAST-FILE
                 REFPOST-FILE
                 GLJNL-FILE
                 REPORT-FILE

           DISPLAY 'CGREF010 - REQUESTS  ' WS-READ-COUNT
           DISPLAY 'CGREF010 - ACCEPTED  ' WS-ACCEPT-COUNT
           DISPLAY 'CGREF010 - REJECTED  ' WS-REJECT-COUNT

           IF WS-REJECT-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       3100-WRITE-GL-JOURNAL.
      *================================================================*
      *    DR FEE INCOME   CR CUSTOMER BALANCES
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-CHARGES             TO TRUE
           STRING 'CREF' WS-BUS-DATE DTC-RUN-ID (1:4)
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
               MOVE WS-GL-FEE-INCOME (GL-IDX) TO GLJD-GL-ACCOUNT
               SET GLJD-DEBIT              TO TRUE
               PERFORM 3180-GL-DETAIL
               MOVE WS-GL-BALANCE-ACCT (GL-IDX) TO GLJD-GL-ACCOUNT
               SET GLJD-CREDIT             TO TRUE
               PERFORM 3180-GL-DETAIL
           END-PERFORM

           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-GL-LINE-COUNT           TO GLJT-LINE-COUNT
           MOVE WS-GL-TOTAL-DR             TO GLJT-TOTAL-DR
           MOVE WS-GL-TOTAL-CR             TO GLJT-TOTAL-CR
           MOVE 1                          TO GLJT-JOURNAL-COUNT
           PERFORM 3190-WRITE-GL-RECORD
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
           MOVE 'REFUND OF ACCOUNT CHARGES' TO GLJD-DESCRIPTION
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
           MOVE RFQ-SORT-CODE              TO DL-SORT-CODE
           MOVE RFQ-ACCT-NO                TO DL-ACCT-NO
           IF RFQ-PERIOD-END NUMERIC
               MOVE RFQ-PERIOD-END         TO WS-DATE-WORK
               PERFORM 8900-FORMAT-DB2-DATE
               MOVE WS-DATE-DB2            TO DL-PERIOD-END
           ELSE
               MOVE RFQ-PERIOD-END         TO DL-PERIOD-END
           END-IF
           MOVE RFQ-CHARGE-TYPE            TO DL-TYPE
           IF WS-REFUND-AMT NUMERIC
               MOVE WS-REFUND-AMT          TO DL-AMOUNT
           ELSE
               MOVE ZERO                   TO DL-AMOUNT
           END-IF
           MOVE RFQ-REASON-CD              TO DL-REASON
           MOVE RFQ-REQUEST-USER           TO DL-REQUEST-USER
           MOVE RFQ-APPROVE-USER           TO DL-APPROVE-USER
           MOVE RFQ-CASE-REF               TO DL-CASE-REF
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-REQUEST.
      *================================================================*
           READ REFUNDIN-FILE INTO WS-REFUND-REQUEST
           EVALUATE TRUE
               WHEN WS-REFUNDIN-OK
                   ADD 1                   TO WS-READ-COUNT
                   MOVE ZERO               TO WS-REFUND-AMT
               WHEN WS-REFUNDIN-EOF
                   SET WS-END-OF-REQUESTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-REQUEST' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'REFUNDIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-REFUNDIN-STATUS TO ABN-FILE-STATUS
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
           MOVE WS-REFUND-REQUEST (1:40)   TO ABN-KEY-DATA
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
