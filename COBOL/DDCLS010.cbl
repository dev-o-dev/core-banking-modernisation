       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDCLS010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-05-17.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDCLS010                                            *
      * TITLE    : ACCOUNT CLOSURE - END OF DAY COMPLETION             *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : DDCLS01 STEP DC020                                  *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (VSAM KSDS, RANDOM)   *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ I-O)  *
      *            CHGPEND  - PENDING CHARGES (VSAM KSDS, DYNAMIC)     *
      *            SOMAST   - STANDING ORDERS (VSAM KSDS, DYNAMIC)     *
      *            DDMAND   - DD MANDATES (VSAM KSDS, DYNAMIC)         *
      * OUTPUT   : CLSPOST  - CLOSING POSTINGS FOR PSEOD01 - PSTXNREC  *
      *            STMTREQ  - FINAL STATEMENT REQUESTS - STREQREC      *
      *            GLJNL    - GL JOURNAL (INTEREST/TAX/CHARGES)        *
      *            REPORT   - CLOSURE REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * CLOSURE TAKES TWO NIGHTS BECAUSE ONLY PSEOD010 MAY CHANGE THE  *
      * LEDGER BALANCE AND IT REJECTS HARD POSTINGS TO A CLOSED        *
      * ACCOUNT:                                                       *
      *                                                                *
      * NIGHT 1 (ACM-PENDING-CLOSE, ACM-DORMANCY-IND NOT 'X')          *
      *   - FINAL CREDIT INTEREST (GROSS) AND TAX, FINAL DEBIT         *
      *     INTEREST, ALL CALCULATED/NOTIFIED PENDING CHARGES          *
      *   - A 'CLS ' DEBIT FOR THE CLOSING BALANCE AND A 'CLS ' CREDIT *
      *     TO THE TRANSFER ACCOUNT (OR NO TRANSFER IF NIL)            *
      *   - ALL POSTINGS GO TO CLSPOST AND ARE APPLIED BY TOMORROW'S   *
      *     PSEOD010 (TIME 23:59 SO THEY SORT LAST FOR THE DAY)        *
      *   - STANDING ORDERS CANCELLED, MANDATES MARKED CLOSED (ADDACS  *
      *     REASON 'B' IS SENT BY DMADC010), CHARGES MARKED APPLIED    *
      *   - ACM-DORMANCY-IND SET TO 'X' = "CLOSING POSTINGS RAISED"    *
      *     (THE FIELD IS OVERLOADED - THERE WAS NO SPARE FIELD WHEN   *
      *     THIS WAS WRITTEN AND THE EXTENSION AREA WAS FULL).         *
      *                                                                *
      * NIGHT 2 (ACM-PENDING-CLOSE, ACM-DORMANCY-IND = 'X')            *
      *   - IF LEDGER = 0 AND NO MEMO ITEMS: STATUS 'C', CLOSE DATE,   *
      *     FINAL STATEMENT REQUEST (TYPE 'X')                         *
      *   - OTHERWISE (A CREDIT ARRIVED, E.G. A RETURNED PAYMENT) THE  *
      *     ACCOUNT STAYS PENDING AND IS REPORTED FOR MANUAL ACTION.   *
      *                                                                *
      * THE CLOSING BALANCE IS CALCULATED FROM THE LEDGER AFTER        *
      * TONIGHT'S HARD POSTING, SO DDCLS01 MUST RUN AFTER PSEOD01 (SEE *
      * SCHED MBKEOD).                                                 *
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
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CHGPEND-FILE     ASSIGN TO CHGPEND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS CGP-KEY
                                   FILE STATUS IS WS-CHGPEND-STATUS.
           SELECT SOMAST-FILE      ASSIGN TO SOMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS SOM-KEY
                                   FILE STATUS IS WS-SOMAST-STATUS.
           SELECT DDMAND-FILE      ASSIGN TO DDMAND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS DMM-KEY
                                   FILE STATUS IS WS-DDMAND-STATUS.
           SELECT CLSPOST-FILE     ASSIGN TO CLSPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CLSPOST-STATUS.
           SELECT STMTREQ-FILE     ASSIGN TO STMTREQ
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-STMTREQ-STATUS.
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
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CHGPEND-FILE.
           COPY CGCHGREC.
       FD  SOMAST-FILE.
           COPY SOMSTREC.
       FD  DDMAND-FILE.
           COPY DMMNDREC.
       FD  CLSPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CLSPOST-RECORD                  PIC X(200).
       FD  STMTREQ-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STMTREQ-RECORD                  PIC X(100).
       FD  GLJNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJNL-RECORD                    PIC X(250).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDCLS010'.
      *
       01  WS-CONSTANTS.
           05  WS-BASIC-RATE               PIC S9(03)V99 COMP-3
                                           VALUE +20.00.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 23590000.
      *
       01  WS-FILE-STATUSES.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-CHGPEND-STATUS           PIC X(02).
               88  WS-CHGPEND-OK               VALUE '00'.
               88  WS-CHGPEND-EOF              VALUE '10'.
               88  WS-CHGPEND-NOTFND           VALUE '23'.
           05  WS-SOMAST-STATUS            PIC X(02).
               88  WS-SOMAST-OK                VALUE '00'.
               88  WS-SOMAST-EOF               VALUE '10'.
               88  WS-SOMAST-NOTFND            VALUE '23'.
           05  WS-DDMAND-STATUS            PIC X(02).
               88  WS-DDMAND-OK                VALUE '00'.
               88  WS-DDMAND-EOF               VALUE '10'.
               88  WS-DDMAND-NOTFND            VALUE '23'.
           05  WS-CLSPOST-STATUS           PIC X(02).
           05  WS-STMTREQ-STATUS           PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-PROD-OK-IND              PIC X(01).
               88  WS-PROD-OK                  VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-GROSS                    PIC S9(13)V99 COMP-3.
           05  WS-TAX                      PIC S9(13)V99 COMP-3.
           05  WS-DR-INT                   PIC S9(13)V99 COMP-3.
           05  WS-CHARGES                  PIC S9(13)V99 COMP-3.
           05  WS-CLOSING                  PIC S9(13)V99 COMP-3.
           05  WS-SO-CANCELLED             PIC 9(03).
           05  WS-DD-CLOSED                PIC 9(03).
           05  WS-NOTE                     PIC X(40).
           05  WS-STAGE                    PIC X(06).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
      *
      *---- GL TOTALS BY PRODUCT --------------------------------------*
       01  WS-GL-TABLE.
           05  WS-GL-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-GL-ENTRY                 OCCURS 200 TIMES
                                           INDEXED BY GL-IDX.
               10  WS-GL-PROD-CD           PIC X(10).
               10  WS-GL-BAL-CR            PIC X(10).
               10  WS-GL-BAL-DR            PIC X(10).
               10  WS-GL-ACCR-PAY          PIC X(10).
               10  WS-GL-ACCR-REC          PIC X(10).
               10  WS-GL-TAX-PAY           PIC X(10).
               10  WS-GL-FEE-INC           PIC X(10).
               10  WS-GL-COST-CENTRE       PIC X(06).
               10  WS-GL-CCY               PIC X(03).
               10  WS-GL-GROSS             PIC S9(13)V99 COMP-3.
               10  WS-GL-TAX               PIC S9(13)V99 COMP-3.
               10  WS-GL-DR-INT            PIC S9(13)V99 COMP-3.
               10  WS-GL-CHARGES           PIC S9(13)V99 COMP-3.
       01  WS-GL-CONTROL.
           05  WS-GL-LINE-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-GL-TOTAL-DR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-TOTAL-CR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-AMOUNT                PIC S9(13)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-STAGE1-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-STAGE2-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-STUCK-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-POSTING-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-PAID-OUT           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-SO-CANC            PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-DD-CLSD            PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'DDCLS010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - ACCOUNT CLOSURES'.
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
           05  FILLER            PIC X(07) VALUE 'STAGE'.
           05  FILLER            PIC X(13) VALUE '     GROSS'.
           05  FILLER            PIC X(11) VALUE '      TAX'.
           05  FILLER            PIC X(12) VALUE '    DR INT'.
           05  FILLER            PIC X(12) VALUE '   CHARGES'.
           05  FILLER            PIC X(15) VALUE '   PAID OUT'.
           05  FILLER            PIC X(04) VALUE 'SO'.
           05  FILLER            PIC X(04) VALUE 'DD'.
           05  FILLER            PIC X(35) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-STAGE                    PIC X(06).
           05  DL-GROSS                    PIC ZZZ,ZZZ,ZZ9.99.
           05  DL-TAX                      PIC ZZ,ZZZ,ZZ9.99.
           05  DL-DR-INT                   PIC ZZZ,ZZZ,ZZ9.99.
           05  DL-CHARGES                  PIC ZZZ,ZZZ,ZZ9.99.
           05  DL-PAID-OUT                 PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SO                       PIC ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DD                       PIC ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(33).
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
           COPY STREQREC.
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
           PERFORM 2000-PROCESS-ACCOUNT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                       CHGPEND-FILE
                       SOMAST-FILE
                       DDMAND-FILE
                OUTPUT CLSPOST-FILE
                       STMTREQ-FILE
                       GLJNL-FILE
                       REPORT-FILE
           IF NOT WS-PRDPARM-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CHGPEND-OK
           OR NOT WS-SOMAST-OK
           OR NOT WS-DDMAND-OK
           OR WS-CLSPOST-STATUS NOT = '00'
           OR WS-STMTREQ-STATUS NOT = '00'
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
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO HD1-BUS-DATE

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           IF ACM-PENDING-CLOSE
               IF ACM-DORMANCY-IND = 'X'
                   PERFORM 2500-STAGE-2
               ELSE
                   PERFORM 2100-STAGE-1
               END-IF
           END-IF
           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       2100-STAGE-1.
      *================================================================*
           MOVE 'RAISE '                   TO WS-STAGE
           MOVE SPACES                     TO WS-NOTE
           MOVE ZERO                       TO WS-GROSS WS-TAX WS-DR-INT
                                              WS-CHARGES WS-CLOSING
                                              WS-SO-CANCELLED
                                              WS-DD-CLOSED
           PERFORM 2110-GET-PRODUCT

      *---- INTEREST --------------------------------------------------*
           MOVE ACM-ACCRUED-INT-CR         TO WS-GROSS
           MOVE ACM-ACCRUED-INT-DR         TO WS-DR-INT
           IF WS-GROSS > ZERO
           AND ACM-TAX-NET
           AND WS-PROD-OK
           AND PRD-DEDUCT-TAX
               COMPUTE WS-TAX = FUNCTION INTEGER-PART
                       (WS-GROSS * WS-BASIC-RATE) / 100
           END-IF

           IF WS-GROSS > ZERO
               PERFORM 2800-INIT-POSTING
               MOVE 'INT '                 TO PST-TXN-CODE
               SET PST-CREDIT              TO TRUE
               MOVE WS-GROSS               TO PST-AMOUNT
               MOVE 'FINAL INTEREST ON CLOSURE' TO PST-NARRATIVE-1
               PERFORM 2850-WRITE-POSTING
           END-IF
           IF WS-TAX > ZERO
               PERFORM 2800-INIT-POSTING
               MOVE 'TAX '                 TO PST-TXN-CODE
               SET PST-DEBIT               TO TRUE
               MOVE WS-TAX                 TO PST-AMOUNT
               MOVE 'INCOME TAX DEDUCTED AT 20.00%' TO PST-NARRATIVE-1
               PERFORM 2850-WRITE-POSTING
           END-IF
           IF WS-DR-INT > ZERO
               PERFORM 2800-INIT-POSTING
               MOVE 'INT '                 TO PST-TXN-CODE
               SET PST-DEBIT               TO TRUE
               MOVE WS-DR-INT              TO PST-AMOUNT
               MOVE 'FINAL OVERDRAFT INTEREST' TO PST-NARRATIVE-1
               PERFORM 2850-WRITE-POSTING
           END-IF

           PERFORM 2200-TAKE-PENDING-CHARGES THRU 2200-EXIT

      *---- CLOSING BALANCE TRANSFER ----------------------------------*
      *    THE LEDGER ALREADY INCLUDES TODAY'S POSTINGS (PSEOD01 RAN
      *    BEFORE THIS JOB). MEMO TOTALS SHOULD BE ZERO AT THIS POINT.
           COMPUTE WS-CLOSING = ACM-LEDGER-BAL + WS-GROSS - WS-TAX
                              - WS-DR-INT - WS-CHARGES

           EVALUATE TRUE
               WHEN WS-CLOSING > ZERO
               AND  ACM-CLOSE-XFER-ACCT-NO NOT = SPACES
                   PERFORM 2800-INIT-POSTING
                   MOVE 'CLS '             TO PST-TXN-CODE
                   SET PST-DEBIT           TO TRUE
                   MOVE WS-CLOSING         TO PST-AMOUNT
                   MOVE 'CLOSING BALANCE TRANSFERRED' TO PST-NARRATIVE-1
                   STRING 'TO ' ACM-CLOSE-XFER-SORT-CODE ' '
                          ACM-CLOSE-XFER-ACCT-NO
                          DELIMITED BY SIZE
                          INTO PST-NARRATIVE-2
                   END-STRING
                   PERFORM 2850-WRITE-POSTING

                   PERFORM 2800-INIT-POSTING
                   MOVE ACM-CLOSE-XFER-SORT-CODE TO PST-SORT-CODE
                   MOVE ACM-CLOSE-XFER-ACCT-NO   TO PST-ACCT-NO
                   MOVE 'CLS '             TO PST-TXN-CODE
                   SET PST-CREDIT          TO TRUE
                   MOVE WS-CLOSING         TO PST-AMOUNT
                   MOVE 'BALANCE FROM CLOSED ACCOUNT'
                                           TO PST-NARRATIVE-1
                   STRING 'FROM ' ACM-SORT-CODE ' ' ACM-ACCT-NO
                          DELIMITED BY SIZE
                          INTO PST-NARRATIVE-2
                   END-STRING
                   PERFORM 2850-WRITE-POSTING
                   ADD WS-CLOSING          TO WS-TOTAL-PAID-OUT
               WHEN WS-CLOSING > ZERO
                   MOVE 'NO TRANSFER ACCOUNT - REFER TO BRANCH'
                                           TO WS-NOTE
               WHEN WS-CLOSING < ZERO
                   MOVE 'BALANCE NOW NEGATIVE - REFER TO COLLECTIONS'
                                           TO WS-NOTE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           PERFORM 2300-CANCEL-STANDING-ORDERS THRU 2300-EXIT
           PERFORM 2400-CLOSE-MANDATES THRU 2400-EXIT
           PERFORM 2700-ACCUMULATE-GL THRU 2700-EXIT

      *---- MARK STAGE 1 COMPLETE -------------------------------------*
           MOVE ZERO                       TO ACM-ACCRUED-INT-CR
                                              ACM-ACCRUED-INT-DR
                                              ACM-ACCR-CR-RESIDUE
                                              ACM-ACCR-DR-RESIDUE
                                              ACM-ACCR-DR-UNARR
           MOVE WS-BUS-DATE                TO ACM-LAST-CR-CAP-DATE
                                              ACM-LAST-DR-CAP-DATE
           MOVE 'X'                        TO ACM-DORMANCY-IND
           SET ACM-NO-DEBITS               TO TRUE
           PERFORM 2900-REWRITE-ACCOUNT
           ADD 1                           TO WS-STAGE1-COUNT
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2110-GET-PRODUCT.
      *================================================================*
           MOVE ACM-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               SET WS-PROD-OK              TO TRUE
           ELSE
               MOVE 'N'                    TO WS-PROD-OK-IND
           END-IF
           .
      *================================================================*
       2200-TAKE-PENDING-CHARGES.
      *================================================================*
           MOVE ACM-SORT-CODE              TO CGP-SORT-CODE
           MOVE ACM-ACCT-NO                TO CGP-ACCT-NO
           MOVE ZERO                       TO CGP-PERIOD-END
           MOVE LOW-VALUES                 TO CGP-CHARGE-TYPE
           MOVE ZERO                       TO CGP-ITEM-SEQ
           START CHGPEND-FILE KEY NOT < CGP-KEY
           IF NOT WS-CHGPEND-OK
               GO TO 2200-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ CHGPEND-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-CHGPEND-OK
                   WHEN CGP-SORT-CODE NOT = ACM-SORT-CODE
                   WHEN CGP-ACCT-NO NOT = ACM-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN CGP-CALCULATED OR CGP-NOTIFIED
                       PERFORM 2800-INIT-POSTING
                       MOVE 'CHG '         TO PST-TXN-CODE
                       SET PST-DEBIT       TO TRUE
                       MOVE CGP-NET-AMT    TO PST-AMOUNT
                       MOVE CGP-DESCRIPTION TO PST-NARRATIVE-1
                       MOVE 'CHARGES TAKEN ON CLOSURE'
                                           TO PST-NARRATIVE-2
                       PERFORM 2850-WRITE-POSTING
                       ADD CGP-NET-AMT     TO WS-CHARGES
                       SET CGP-APPLIED     TO TRUE
                       MOVE WS-BUS-DATE    TO CGP-APPLY-DATE
                       MOVE PST-EXT-REF    TO CGP-POST-EXT-REF
                       REWRITE CGP-CHARGE-RECORD
                       IF NOT WS-CHGPEND-OK
                           MOVE '2200-TAKE-PENDING-CHARGES'
                                           TO ABN-PARAGRAPH
                           MOVE 1002       TO ABN-ABEND-CODE
                           MOVE 'CHGPEND REWRITE FAILED'
                                           TO ABN-MESSAGE
                           MOVE WS-CHGPEND-STATUS TO ABN-FILE-STATUS
                           PERFORM Z999-ABEND
                       END-IF
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-CANCEL-STANDING-ORDERS.
      *================================================================*
           MOVE ACM-SORT-CODE              TO SOM-PAYER-SORT-CODE
           MOVE ACM-ACCT-NO                TO SOM-PAYER-ACCT-NO
           MOVE 1                          TO SOM-SO-SEQ
           START SOMAST-FILE KEY NOT < SOM-KEY
           IF NOT WS-SOMAST-OK
               GO TO 2300-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ SOMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-SOMAST-OK
                   WHEN SOM-PAYER-SORT-CODE NOT = ACM-SORT-CODE
                   WHEN SOM-PAYER-ACCT-NO NOT = ACM-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN SOM-ACTIVE OR SOM-SUSPENDED
                       SET SOM-CANCELLED   TO TRUE
                       MOVE WS-BUS-DATE    TO SOM-CANCEL-DATE
                                              SOM-AMEND-DATE
                       MOVE 'ACLS'         TO SOM-CANCEL-RSN
                       MOVE 'BATCH '       TO SOM-AMEND-CHANNEL
                       MOVE WS-PROGRAM-ID  TO SOM-AMEND-USER
                       REWRITE SOM-SO-RECORD
                       IF NOT WS-SOMAST-OK
                           MOVE '2300-CANCEL-STANDING-ORDERS'
                                           TO ABN-PARAGRAPH
                           MOVE 1002       TO ABN-ABEND-CODE
                           MOVE 'SOMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                           MOVE WS-SOMAST-STATUS TO ABN-FILE-STATUS
                           PERFORM Z999-ABEND
                       END-IF
                       ADD 1               TO WS-SO-CANCELLED
                                              WS-TOTAL-SO-CANC
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2400-CLOSE-MANDATES.
      *================================================================*
      *    DMADC010 SENDS ADDACS REASON 'B' FOR EVERY MANDATE WITH
      *    STATUS 'X' AND NO ADDACS SENT DATE.
           MOVE ACM-SORT-CODE              TO DMM-PAYER-SORT-CODE
           MOVE ACM-ACCT-NO                TO DMM-PAYER-ACCT-NO
           MOVE LOW-VALUES                 TO DMM-ORIG-SUN
                                              DMM-PAYER-REF
           START DDMAND-FILE KEY NOT < DMM-KEY
           IF NOT WS-DDMAND-OK
               GO TO 2400-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ DDMAND-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-DDMAND-OK
                   WHEN DMM-PAYER-SORT-CODE NOT = ACM-SORT-CODE
                   WHEN DMM-PAYER-ACCT-NO NOT = ACM-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN DMM-LIVE OR DMM-DORMANT
                       SET DMM-CLOSED      TO TRUE
                       MOVE WS-BUS-DATE    TO DMM-CANCEL-DATE
                                              DMM-LAST-MAINT-DATE
                       MOVE 'BATCH '       TO DMM-CANCEL-CHANNEL
                       MOVE WS-PROGRAM-ID  TO DMM-CANCEL-USER
                                              DMM-LAST-MAINT-PGM
                       MOVE ZERO           TO DMM-ADDACS-SENT-DATE
                       MOVE 'B'            TO DMM-ADDACS-REASON
                       REWRITE DMM-MANDATE-RECORD
                       IF NOT WS-DDMAND-OK
                           MOVE '2400-CLOSE-MANDATES'
                                           TO ABN-PARAGRAPH
                           MOVE 1002       TO ABN-ABEND-CODE
                           MOVE 'DDMAND REWRITE FAILED'
                                           TO ABN-MESSAGE
                           MOVE WS-DDMAND-STATUS TO ABN-FILE-STATUS
                           PERFORM Z999-ABEND
                       END-IF
                       ADD 1               TO WS-DD-CLOSED
                                              WS-TOTAL-DD-CLSD
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
       2400-EXIT.
           EXIT.
      *================================================================*
       2500-STAGE-2.
      *================================================================*
           MOVE 'CLOSE '                   TO WS-STAGE
           MOVE ZERO                       TO WS-GROSS WS-TAX WS-DR-INT
                                              WS-CHARGES WS-SO-CANCELLED
                                              WS-DD-CLOSED
           MOVE ACM-LEDGER-BAL             TO WS-CLOSING

           IF ACM-LEDGER-BAL = ZERO
           AND ACM-MEMO-CR-TOT = ZERO
           AND ACM-MEMO-DR-TOT = ZERO
           AND ACM-HOLD-AMT = ZERO
               SET ACM-CLOSED              TO TRUE
               MOVE WS-BUS-DATE            TO ACM-CLOSE-DATE
               SET ACM-NO-POSTINGS         TO TRUE
               MOVE SPACE                  TO ACM-DORMANCY-IND
               MOVE ZERO                   TO ACM-AVAIL-BAL
               PERFORM 2900-REWRITE-ACCOUNT
               PERFORM 2600-FINAL-STATEMENT
               ADD 1                       TO WS-STAGE2-COUNT
               MOVE 'ACCOUNT CLOSED'       TO WS-NOTE
           ELSE
      *        SOMETHING ARRIVED AFTER THE CLOSING POSTINGS - LEAVE IT
      *        PENDING FOR DDA OPERATIONS.
               ADD 1                       TO WS-STUCK-COUNT
               MOVE 'BALANCE NOT NIL - MANUAL ACTION REQUIRED'
                                           TO WS-NOTE
               MOVE 4                      TO RETURN-CODE
           END-IF
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2600-FINAL-STATEMENT.
      *================================================================*
           MOVE SPACES                     TO SRQ-STMT-REQUEST
           MOVE ACM-SORT-CODE              TO SRQ-SORT-CODE
           MOVE ACM-ACCT-NO                TO SRQ-ACCT-NO
           MOVE ACM-CIF-NO                 TO SRQ-CIF-NO
           COMPUTE SRQ-STMT-NO = ACM-LAST-STMT-NO + 1
           IF ACM-LAST-STMT-DATE NUMERIC AND ACM-LAST-STMT-DATE > ZERO
               MOVE ACM-LAST-STMT-DATE     TO SRQ-PERIOD-FROM
           ELSE
               MOVE ACM-OPEN-DATE          TO SRQ-PERIOD-FROM
           END-IF
           MOVE WS-BUS-DATE                TO SRQ-PERIOD-TO
           MOVE ZERO                       TO SRQ-CLOSING-BAL
           SET SRQ-CLOSURE                 TO TRUE
           WRITE STMTREQ-RECORD            FROM SRQ-STMT-REQUEST
           IF WS-STMTREQ-STATUS NOT = '00'
               MOVE '2600-FINAL-STATEMENT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'STMTREQ WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-STMTREQ-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2700-ACCUMULATE-GL.
      *================================================================*
           IF WS-GROSS = ZERO AND WS-TAX = ZERO
           AND WS-DR-INT = ZERO AND WS-CHARGES = ZERO
               GO TO 2700-EXIT
           END-IF
           SET GL-IDX                      TO 1
           SEARCH WS-GL-ENTRY
               AT END
                   PERFORM 2710-ADD-GL-PRODUCT
               WHEN GL-IDX > WS-GL-COUNT
                   PERFORM 2710-ADD-GL-PRODUCT
               WHEN WS-GL-PROD-CD (GL-IDX) = ACM-PROD-CD
                   CONTINUE
           END-SEARCH
           ADD WS-GROSS                    TO WS-GL-GROSS (GL-IDX)
           ADD WS-TAX                      TO WS-GL-TAX (GL-IDX)
           ADD WS-DR-INT                   TO WS-GL-DR-INT (GL-IDX)
           ADD WS-CHARGES                  TO WS-GL-CHARGES (GL-IDX)
           .
       2700-EXIT.
           EXIT.
      *================================================================*
       2710-ADD-GL-PRODUCT.
      *================================================================*
           IF WS-GL-COUNT >= 200
               MOVE '2710-ADD-GL-PRODUCT'  TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL PRODUCT TABLE FULL' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-GL-COUNT
           SET GL-IDX                      TO WS-GL-COUNT
           MOVE ACM-PROD-CD                TO WS-GL-PROD-CD (GL-IDX)
           MOVE ZERO TO WS-GL-GROSS (GL-IDX) WS-GL-TAX (GL-IDX)
                        WS-GL-DR-INT (GL-IDX) WS-GL-CHARGES (GL-IDX)
           IF WS-PROD-OK
               MOVE PRD-GL-BALANCE-CR      TO WS-GL-BAL-CR (GL-IDX)
               MOVE PRD-GL-BALANCE-DR      TO WS-GL-BAL-DR (GL-IDX)
               MOVE PRD-GL-INT-ACCR-PAY    TO WS-GL-ACCR-PAY (GL-IDX)
               MOVE PRD-GL-INT-ACCR-REC    TO WS-GL-ACCR-REC (GL-IDX)
               MOVE PRD-GL-TAX-PAYABLE     TO WS-GL-TAX-PAY (GL-IDX)
               MOVE PRD-GL-FEE-INCOME      TO WS-GL-FEE-INC (GL-IDX)
               MOVE PRD-COST-CENTRE        TO WS-GL-COST-CENTRE (GL-IDX)
               MOVE PRD-CCY-CD             TO WS-GL-CCY (GL-IDX)
           ELSE
               MOVE '29900-0010'           TO WS-GL-BAL-CR (GL-IDX)
                                              WS-GL-BAL-DR (GL-IDX)
                                              WS-GL-ACCR-PAY (GL-IDX)
                                              WS-GL-ACCR-REC (GL-IDX)
                                              WS-GL-TAX-PAY (GL-IDX)
               MOVE '49900-0010'           TO WS-GL-FEE-INC (GL-IDX)
               MOVE '000000'               TO WS-GL-COST-CENTRE (GL-IDX)
               MOVE 'GBP'                  TO WS-GL-CCY (GL-IDX)
           END-IF
           .
      *================================================================*
       2800-INIT-POSTING.
      *================================================================*
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE ACM-SORT-CODE              TO PST-SORT-CODE
           MOVE ACM-ACCT-NO                TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE ACM-CCY-CD                 TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           STRING 'CLS' ACM-SORT-CODE ACM-ACCT-NO WS-POST-SEQ
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           .
      *================================================================*
       2850-WRITE-POSTING.
      *================================================================*
           WRITE CLSPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-CLSPOST-STATUS NOT = '00'
               MOVE '2850-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CLSPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CLSPOST-STATUS      TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-POSTING-COUNT
           .
      *================================================================*
       2900-REWRITE-ACCOUNT.
      *================================================================*
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER
           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '2900-REWRITE-ACCOUNT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 3100-WRITE-GL-JOURNAL

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'PENDING CLOSURES - POSTINGS RAISED' TO TL-LABEL
           MOVE WS-STAGE1-COUNT            TO TL-COUNT
           MOVE WS-TOTAL-PAID-OUT          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS CLOSED'          TO TL-LABEL
           MOVE WS-STAGE2-COUNT            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'STUCK - MANUAL ACTION'    TO TL-LABEL
           MOVE WS-STUCK-COUNT             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'STANDING ORDERS CANCELLED' TO TL-LABEL
           MOVE WS-TOTAL-SO-CANC           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MANDATES CLOSED (ADDACS B)' TO TL-LABEL
           MOVE WS-TOTAL-DD-CLSD           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'POSTINGS WRITTEN'         TO TL-LABEL
           MOVE WS-POSTING-COUNT           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE PRDPARM-FILE
                 ACCTMAST-FILE
                 CHGPEND-FILE
                 SOMAST-FILE
                 DDMAND-FILE
                 CLSPOST-FILE
                 STMTREQ-FILE
                 GLJNL-FILE
                 REPORT-FILE

           DISPLAY 'DDCLS010 - ACCOUNTS READ     ' WS-READ-COUNT
           DISPLAY 'DDCLS010 - STAGE 1 (RAISED)  ' WS-STAGE1-COUNT
           DISPLAY 'DDCLS010 - STAGE 2 (CLOSED)  ' WS-STAGE2-COUNT
           DISPLAY 'DDCLS010 - STUCK             ' WS-STUCK-COUNT
           .
      *================================================================*
       3100-WRITE-GL-JOURNAL.
      *================================================================*
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-CAPITALISE          TO TRUE
           STRING 'CLOS' WS-BUS-DATE DTC-RUN-ID (1:4)
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
               IF WS-GL-GROSS (GL-IDX) > ZERO
                   MOVE WS-GL-GROSS (GL-IDX)   TO WS-GL-AMOUNT
                   MOVE WS-GL-ACCR-PAY (GL-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT              TO TRUE
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-GL-BAL-CR (GL-IDX)  TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT             TO TRUE
                   PERFORM 3180-GL-DETAIL
               END-IF
               IF WS-GL-TAX (GL-IDX) > ZERO
                   MOVE WS-GL-TAX (GL-IDX)     TO WS-GL-AMOUNT
                   MOVE WS-GL-BAL-CR (GL-IDX)  TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT              TO TRUE
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-GL-TAX-PAY (GL-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT             TO TRUE
                   PERFORM 3180-GL-DETAIL
               END-IF
               IF WS-GL-DR-INT (GL-IDX) > ZERO
                   MOVE WS-GL-DR-INT (GL-IDX)  TO WS-GL-AMOUNT
                   MOVE WS-GL-BAL-DR (GL-IDX)  TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT              TO TRUE
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-GL-ACCR-REC (GL-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT             TO TRUE
                   PERFORM 3180-GL-DETAIL
               END-IF
               IF WS-GL-CHARGES (GL-IDX) > ZERO
                   MOVE WS-GL-CHARGES (GL-IDX) TO WS-GL-AMOUNT
                   MOVE WS-GL-BAL-DR (GL-IDX)  TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT              TO TRUE
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-GL-FEE-INC (GL-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT             TO TRUE
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
           .
      *================================================================*
       3180-GL-DETAIL.
      *================================================================*
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-GL-LINE-COUNT
           MOVE WS-GL-LINE-COUNT           TO GLJD-LINE-NO
           MOVE WS-GL-COST-CENTRE (GL-IDX) TO GLJD-COST-CENTRE
           MOVE WS-GL-PROD-CD (GL-IDX)     TO GLJD-PROD-CD
           MOVE WS-GL-CCY (GL-IDX)         TO GLJD-CCY-CD
           MOVE WS-GL-AMOUNT               TO GLJD-AMOUNT
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE 'ACCOUNT CLOSURE - FINAL ITEMS' TO GLJD-DESCRIPTION
           MOVE SPACES                     TO GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE 'CLS '                     TO GLJD-TXN-CODE
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
           MOVE ACM-SORT-CODE              TO DL-SORT-CODE
           MOVE ACM-ACCT-NO                TO DL-ACCT-NO
           MOVE WS-STAGE                   TO DL-STAGE
           MOVE WS-GROSS                   TO DL-GROSS
           MOVE WS-TAX                     TO DL-TAX
           MOVE WS-DR-INT                  TO DL-DR-INT
           MOVE WS-CHARGES                 TO DL-CHARGES
           MOVE WS-CLOSING                 TO DL-PAID-OUT
           MOVE WS-SO-CANCELLED            TO DL-SO
           MOVE WS-DD-CLOSED               TO DL-DD
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
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
