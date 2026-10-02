       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GLEXT010.
       AUTHOR.        K L MORGAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2002-06-03.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : GLEXT010                                            *
      * TITLE    : GENERAL LEDGER - CUSTOMER MOVEMENTS EXTRACT         *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : GLEXT010                                            *
      * JCL      : GLFEED01 STEP GL010                                 *
      *----------------------------------------------------------------*
      * INPUT    : MBK.TXNHIST  - TODAY'S HARD POSTINGS (CURSOR)       *
      *            MBK.GLMAP    - TXN CODE TO CONTRA GL MAPPING        *
      *            ACCTMAST     - DDA ACCOUNT MASTER (RANDOM) - PRODUCT*
      *            PRDPARM      - PRODUCT PARAMETERS (RANDOM) - GL CODE*
      *            DATECARD     - BUSINESS DATE CONTROL CARD           *
      * OUTPUT   : GLJNL    - GL MOVEMENTS JOURNAL - GLJNLREC          *
      *            MBK.GLSUSP (INSERT) - UNMAPPED MOVEMENTS            *
      *            REPORT   - GL EXTRACT SUMMARY (FBA 133)             *
      *----------------------------------------------------------------*
      * THE GROUP LEDGER HOLDS ONE BALANCE PER PRODUCT GL ACCOUNT. THE *
      * DAY'S CUSTOMER MOVEMENTS ARE SUMMARISED BY PRODUCT, TXN CODE   *
      * AND DIRECTION AND POSTED AGAINST A CONTRA ACCOUNT FROM GLMAP:  *
      *                                                                *
      *   CUSTOMER CREDIT  DR CONTRA (E.G. FPS SETTLEMENT)             *
      *                    CR PRODUCT BALANCE GL                       *
      *   CUSTOMER DEBIT   DR PRODUCT BALANCE GL                       *
      *                    CR CONTRA                                   *
      *                                                                *
      * GLMAP-ACTION:                                                  *
      *   'P' POST TO THE CONTRA                                       *
      *   'S' SKIP - ALREADY JOURNALED BY ITS OWN PROGRAM (INT, TAX,   *
      *       CHG FROM INCAP010 / CGAPL010 / DDCLS010)                 *
      *   'I' INTERNAL - BOTH SIDES ARE CUSTOMER ACCOUNTS (TFR, CLS,   *
      *       OPN, REV). POSTED PRODUCT-TO-PRODUCT THROUGH THE         *
      *       INTERNAL TRANSFER CLEARING ACCOUNT 19000-0001, WHICH     *
      *       MUST NET TO ZERO AT THE END OF THE RUN.                  *
      * A PRODUCT-SPECIFIC GLMAP ROW TAKES PRECEDENCE OVER THE         *
      * '*DEFAULT*' ROW FOR THE TXN CODE.                              *
      *                                                                *
      * MOVEMENTS WITH NO MAPPING GO TO SUSPENSE 29900-0001 AND A ROW  *
      * IS WRITTEN TO MBK.GLSUSP FOR FINANCE TO CLEAR (GLSUS010).      *
      *                                                                *
      * ALL CUSTOMER BALANCES GO TO THE PRODUCT'S CREDIT-BALANCE GL    *
      * ACCOUNT. OVERDRAWN BALANCES ARE RECLASSIFIED TO THE DEBIT GL   *
      * MONTHLY BY A MANUAL FINANCE JOURNAL, NOT HERE (PIR 2003/019).  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2002-06-03 KLM  ORIGINAL VERSION (ORACLE GL REPLACES MSA GL)   *
      * 2004-01-12 KLM  INTERNAL TRANSFER CLEARING ACCOUNT             *
      * 2006-01-16 KLM  PRODUCT-SPECIFIC MAPPINGS                      *
      * 2010-03-08 AOS  SKIP FOR INT/TAX/CHG NOW JOURNALED AT SOURCE   *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
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
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'GLEXT010'.
      *
       01  WS-CONSTANTS.
           05  WS-SUSPENSE-GL              PIC X(10) VALUE '29900-0001'.
           05  WS-INTERNAL-GL              PIC X(10) VALUE '19000-0001'.
           05  WS-DEFAULT-PROD             PIC X(10) VALUE '*DEFAULT*'.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOC-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CURSOR            VALUE 'Y'.
           05  WS-MAP-FOUND-IND            PIC X(01).
               88  WS-MAP-FOUND                VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-DB2             PIC X(10).
           05  WS-LAST-ACCT                PIC X(14) VALUE LOW-VALUES.
           05  WS-ACCT-PROD-CD             PIC X(10).
           05  WS-ACCT-CCY-CD              PIC X(03).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-INTERNAL-NET             PIC S9(15)V99 COMP-3
                                           VALUE ZERO.
           05  WS-SUSP-SEQ                 PIC S9(09) COMP.
      *
      *---- SUMMARY TABLE: PRODUCT x TXN CODE x DIRECTION -------------*
       01  WS-SUMMARY-TABLE.
           05  WS-SUM-COUNT                PIC S9(04) COMP VALUE ZERO.
           05  WS-SUM-MAX                  PIC S9(04) COMP VALUE +2000.
           05  WS-SUM-ENTRY                OCCURS 2000 TIMES
                                           INDEXED BY SM-IDX.
               10  WS-SM-KEY.
                   15  WS-SM-PROD-CD       PIC X(10).
                   15  WS-SM-TXN-CODE      PIC X(04).
                   15  WS-SM-DR-CR         PIC X(01).
               10  WS-SM-CCY-CD            PIC X(03).
               10  WS-SM-COUNT             PIC S9(09) COMP-3.
               10  WS-SM-AMOUNT            PIC S9(15)V99 COMP-3.
      *
      *---- PRODUCT GL CACHE ------------------------------------------*
       01  WS-PROD-GL-TABLE.
           05  WS-PG-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-PG-ENTRY                 OCCURS 300 TIMES
                                           INDEXED BY PG-IDX.
               10  WS-PG-PROD-CD           PIC X(10).
               10  WS-PG-BALANCE-GL        PIC X(10).
               10  WS-PG-COST-CENTRE       PIC X(06).
      *
       01  WS-COUNTERS.
           05  WS-ROW-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-TXN-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-ACCT-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-POSTED-GROUPS            PIC S9(07) COMP-3 VALUE 0.
           05  WS-SKIPPED-GROUPS           PIC S9(07) COMP-3 VALUE 0.
           05  WS-INTERNAL-GROUPS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-SUSPENSE-GROUPS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOTAL-DR-MOVES           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-CR-MOVES           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-SKIPPED-AMOUNT           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-SUSPENSE-AMOUNT          PIC S9(15)V99 COMP-3
                                           VALUE 0.
       01  WS-GL-CONTROL.
           05  WS-GL-LINE-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-GL-TOTAL-DR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-TOTAL-CR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'GLEXT010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - GL CUSTOMER MOVEMENTS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(12) VALUE 'PRODUCT'.
           05  FILLER            PIC X(06) VALUE 'TXN'.
           05  FILLER            PIC X(04) VALUE 'D/C'.
           05  FILLER            PIC X(11) VALUE '      ITEMS'.
           05  FILLER            PIC X(20) VALUE '              AMOUNT'.
           05  FILLER            PIC X(03) VALUE SPACES.
           05  FILLER            PIC X(12) VALUE 'BALANCE GL'.
           05  FILLER            PIC X(12) VALUE 'CONTRA GL'.
           05  FILLER            PIC X(08) VALUE 'ACTION'.
           05  FILLER            PIC X(44) VALUE 'DESCRIPTION'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TXN-CODE                 PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-DR-CR                    PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-COUNT                    PIC ZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  DL-BALANCE-GL               PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CONTRA-GL                PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(07).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DESC                     PIC X(40).
           05  FILLER                      PIC X(04) VALUE SPACES.
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
           EXEC SQL INCLUDE DCLGLMP END-EXEC.
           EXEC SQL INCLUDE DCLGLSP END-EXEC.
      *
       01  HV-POST-DATE                    PIC X(10).
       01  HV-SORT-CODE                    PIC X(06).
       01  HV-ACCT-NO                      PIC X(08).
       01  HV-TXN-CODE                     PIC X(04).
       01  HV-DR-CR-IND                    PIC X(01).
       01  HV-SUM-AMT                      PIC S9(15)V99 COMP-3.
       01  HV-ROW-COUNT                    PIC S9(09) COMP.
       01  HV-PROD-CD                      PIC X(10).
      *
      *    ONE ROW PER ACCOUNT / TXN CODE / DIRECTION FOR THE DAY,
      *    IN ACCOUNT ORDER SO THE ACCOUNT MASTER IS READ ONCE EACH.
           EXEC SQL DECLARE MOVECSR CURSOR FOR
               SELECT SORT_CODE, ACCT_NO, TXN_CODE, DR_CR_IND,
                      SUM(TXN_AMT), COUNT(*)
                 FROM MBK.TXNHIST
                WHERE POST_DATE = :HV-POST-DATE
                GROUP BY SORT_CODE, ACCT_NO, TXN_CODE, DR_CR_IND
                ORDER BY SORT_CODE, ACCT_NO
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
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
           PERFORM 2000-SUMMARISE-MOVEMENTS
           PERFORM 3000-WRITE-JOURNAL
           PERFORM 4000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                OUTPUT GLJNL-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
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
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-BUS-DATE-DB2
           END-STRING
           MOVE WS-BUS-DATE-DB2            TO HD1-BUS-DATE
                                              HV-POST-DATE
           DISPLAY 'GLEXT010 - BUSINESS DATE ' WS-BUS-DATE-DB2
           .
      *================================================================*
       2000-SUMMARISE-MOVEMENTS.
      *================================================================*
           EXEC SQL OPEN MOVECSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2000-SUMMARISE-MOVEMENTS' TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF

           PERFORM UNTIL WS-END-OF-CURSOR
               EXEC SQL
                   FETCH MOVECSR
                    INTO :HV-SORT-CODE, :HV-ACCT-NO, :HV-TXN-CODE,
                         :HV-DR-CR-IND, :HV-SUM-AMT, :HV-ROW-COUNT
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-ROW-COUNT
                       ADD HV-ROW-COUNT    TO WS-TXN-COUNT
                       PERFORM 2100-GET-ACCOUNT-PRODUCT THRU 2100-EXIT
                       PERFORM 2200-ADD-TO-SUMMARY
                   WHEN +100
                       SET WS-END-OF-CURSOR TO TRUE
                   WHEN OTHER
                       MOVE '2000-SUMMARISE-MOVEMENTS'
                                           TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE MOVECSR END-EXEC
           DISPLAY 'GLEXT010 - ACCOUNT/TXN GROUPS ' WS-ROW-COUNT
                   ' TRANSACTIONS ' WS-TXN-COUNT
                   ' SUMMARY LINES ' WS-SUM-COUNT
           .
      *================================================================*
       2100-GET-ACCOUNT-PRODUCT.
      *================================================================*
           IF HV-SORT-CODE = WS-LAST-ACCT (1:6)
           AND HV-ACCT-NO = WS-LAST-ACCT (7:8)
               GO TO 2100-EXIT
           END-IF
           MOVE HV-SORT-CODE               TO WS-LAST-ACCT (1:6)
                                              ACM-SORT-CODE
           MOVE HV-ACCT-NO                 TO WS-LAST-ACCT (7:8)
                                              ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   MOVE ACM-PROD-CD        TO WS-ACCT-PROD-CD
                   MOVE ACM-CCY-CD         TO WS-ACCT-CCY-CD
               WHEN WS-ACCTMAST-NOTFND
      *            SHOULD NOT HAPPEN - PSEOD010 REJECTS POSTINGS FOR
      *            UNKNOWN ACCOUNTS. MAP TO A DUMMY PRODUCT SO THE
      *            MOVEMENT GOES TO SUSPENSE.
                   MOVE '*UNKNOWN*'        TO WS-ACCT-PROD-CD
                   MOVE 'GBP'              TO WS-ACCT-CCY-CD
                   ADD 1                   TO WS-NO-ACCT-COUNT
               WHEN OTHER
                   MOVE '2100-GET-ACCOUNT-PRODUCT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE WS-LAST-ACCT       TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-ADD-TO-SUMMARY.
      *================================================================*
           SET SM-IDX                      TO 1
           SEARCH WS-SUM-ENTRY
               AT END
                   PERFORM 2210-NEW-SUMMARY-ENTRY
               WHEN SM-IDX > WS-SUM-COUNT
                   PERFORM 2210-NEW-SUMMARY-ENTRY
               WHEN WS-SM-PROD-CD (SM-IDX) = WS-ACCT-PROD-CD
               AND  WS-SM-TXN-CODE (SM-IDX) = HV-TXN-CODE
               AND  WS-SM-DR-CR (SM-IDX) = HV-DR-CR-IND
                   CONTINUE
           END-SEARCH
           ADD HV-ROW-COUNT                TO WS-SM-COUNT (SM-IDX)
           ADD HV-SUM-AMT                  TO WS-SM-AMOUNT (SM-IDX)
           IF HV-DR-CR-IND = 'D'
               ADD HV-SUM-AMT              TO WS-TOTAL-DR-MOVES
           ELSE
               ADD HV-SUM-AMT              TO WS-TOTAL-CR-MOVES
           END-IF
           .
      *================================================================*
       2210-NEW-SUMMARY-ENTRY.
      *================================================================*
           IF WS-SUM-COUNT >= WS-SUM-MAX
               MOVE '2210-NEW-SUMMARY-ENTRY' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL SUMMARY TABLE FULL - INCREASE WS-SUM-MAX'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-SUM-COUNT
           SET SM-IDX                      TO WS-SUM-COUNT
           MOVE WS-ACCT-PROD-CD            TO WS-SM-PROD-CD (SM-IDX)
           MOVE HV-TXN-CODE                TO WS-SM-TXN-CODE (SM-IDX)
           MOVE HV-DR-CR-IND               TO WS-SM-DR-CR (SM-IDX)
           MOVE WS-ACCT-CCY-CD             TO WS-SM-CCY-CD (SM-IDX)
           MOVE ZERO                       TO WS-SM-COUNT (SM-IDX)
                                              WS-SM-AMOUNT (SM-IDX)
           .
      *================================================================*
       3000-WRITE-JOURNAL.
      *================================================================*
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-MOVEMENTS           TO TRUE
           STRING 'MOVT' WS-BUS-DATE DTC-RUN-ID (1:4)
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE DTC-RUN-ID                 TO GLJH-RUN-ID
           MOVE FUNCTION CURRENT-DATE (1:8) TO GLJH-CREATE-DATE
           MOVE FUNCTION CURRENT-DATE (9:6) TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3900-WRITE-GL-RECORD

           PERFORM 7000-REPORT-HEADINGS

           PERFORM VARYING SM-IDX FROM 1 BY 1
                   UNTIL SM-IDX > WS-SUM-COUNT
               PERFORM 3100-PROCESS-SUMMARY-LINE
           END-PERFORM

      *    THE INTERNAL CLEARING ACCOUNT MUST NET TO ZERO - EVERY
      *    INTERNAL DEBIT HAS A MATCHING INTERNAL CREDIT THE SAME DAY.
      *    A NON-ZERO NET IS POSTED TO SUSPENSE AND REPORTED.
           IF WS-INTERNAL-NET NOT = ZERO
               DISPLAY 'GLEXT010 W001 INTERNAL CLEARING NOT ZERO: '
                       WS-INTERNAL-NET
               PERFORM 3500-CLEAR-INTERNAL-TO-SUSPENSE
           END-IF

           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-GL-LINE-COUNT           TO GLJT-LINE-COUNT
           MOVE WS-GL-TOTAL-DR             TO GLJT-TOTAL-DR
           MOVE WS-GL-TOTAL-CR             TO GLJT-TOTAL-CR
           MOVE 1                          TO GLJT-JOURNAL-COUNT
           PERFORM 3900-WRITE-GL-RECORD

           IF WS-GL-TOTAL-DR NOT = WS-GL-TOTAL-CR
               MOVE '3000-WRITE-JOURNAL'   TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL MOVEMENTS JOURNAL DOES NOT BALANCE'
                                           TO ABN-MESSAGE
               PERFORM 9200-ROLLBACK
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3100-PROCESS-SUMMARY-LINE.
      *================================================================*
           PERFORM 3200-GET-PRODUCT-GL
           PERFORM 3300-FIND-MAPPING

           MOVE WS-SM-PROD-CD (SM-IDX)     TO DL-PROD-CD
           MOVE WS-SM-TXN-CODE (SM-IDX)    TO DL-TXN-CODE
           MOVE WS-SM-DR-CR (SM-IDX)       TO DL-DR-CR
           MOVE WS-SM-COUNT (SM-IDX)       TO DL-COUNT
           MOVE WS-SM-AMOUNT (SM-IDX)      TO DL-AMOUNT
           MOVE WS-PG-BALANCE-GL (PG-IDX)  TO DL-BALANCE-GL

           EVALUATE TRUE
               WHEN NOT WS-MAP-FOUND
                   MOVE WS-SUSPENSE-GL     TO GLM-CONTRA-GL
                   MOVE 'SUSPENS'          TO DL-ACTION
                   MOVE 'NO GLMAP ENTRY - POSTED TO SUSPENSE'
                                           TO DL-DESC
                   PERFORM 3400-POST-PAIR
                   PERFORM 3600-RAISE-SUSPENSE
                   ADD 1                   TO WS-SUSPENSE-GROUPS
                   ADD WS-SM-AMOUNT (SM-IDX) TO WS-SUSPENSE-AMOUNT
               WHEN GLM-MAP-ACTION = 'S'
                   MOVE 'SKIP'             TO DL-ACTION
                   MOVE GLM-DESCRIPTION-TEXT TO DL-DESC
                   ADD 1                   TO WS-SKIPPED-GROUPS
                   ADD WS-SM-AMOUNT (SM-IDX) TO WS-SKIPPED-AMOUNT
               WHEN GLM-MAP-ACTION = 'I'
                   MOVE WS-INTERNAL-GL     TO GLM-CONTRA-GL
                   MOVE 'INTERNL'          TO DL-ACTION
                   MOVE GLM-DESCRIPTION-TEXT TO DL-DESC
                   PERFORM 3400-POST-PAIR
                   ADD 1                   TO WS-INTERNAL-GROUPS
      *            CUSTOMER DEBIT = CR CLEARING, CREDIT = DR CLEARING
                   IF WS-SM-DR-CR (SM-IDX) = 'D'
                       SUBTRACT WS-SM-AMOUNT (SM-IDX)
                                           FROM WS-INTERNAL-NET
                   ELSE
                       ADD WS-SM-AMOUNT (SM-IDX) TO WS-INTERNAL-NET
                   END-IF
               WHEN OTHER
                   MOVE 'POST'             TO DL-ACTION
                   MOVE GLM-DESCRIPTION-TEXT TO DL-DESC
                   PERFORM 3400-POST-PAIR
                   ADD 1                   TO WS-POSTED-GROUPS
           END-EVALUATE

           MOVE GLM-CONTRA-GL              TO DL-CONTRA-GL
           IF GLM-MAP-ACTION = 'S' AND WS-MAP-FOUND
               MOVE SPACES                 TO DL-CONTRA-GL
           END-IF
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       3200-GET-PRODUCT-GL.
      *================================================================*
           SET PG-IDX                      TO 1
           SEARCH WS-PG-ENTRY
               AT END
                   PERFORM 3210-LOAD-PRODUCT-GL
               WHEN PG-IDX > WS-PG-COUNT
                   PERFORM 3210-LOAD-PRODUCT-GL
               WHEN WS-PG-PROD-CD (PG-IDX) = WS-SM-PROD-CD (SM-IDX)
                   CONTINUE
           END-SEARCH
           .
      *================================================================*
       3210-LOAD-PRODUCT-GL.
      *================================================================*
           IF WS-PG-COUNT >= 300
               MOVE '3210-LOAD-PRODUCT-GL' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'PRODUCT GL CACHE FULL' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-PG-COUNT
           SET PG-IDX                      TO WS-PG-COUNT
           MOVE WS-SM-PROD-CD (SM-IDX)     TO WS-PG-PROD-CD (PG-IDX)
                                              PRD-PROD-CD
           READ PRDPARM-FILE
           IF WS-PRDPARM-OK
               MOVE PRD-GL-BALANCE-CR      TO WS-PG-BALANCE-GL (PG-IDX)
               MOVE PRD-COST-CENTRE        TO WS-PG-COST-CENTRE (PG-IDX)
           ELSE
               MOVE WS-SUSPENSE-GL         TO WS-PG-BALANCE-GL (PG-IDX)
               MOVE '000000'               TO WS-PG-COST-CENTRE (PG-IDX)
           END-IF
           .
      *================================================================*
       3300-FIND-MAPPING.
      *================================================================*
      *    PRODUCT-SPECIFIC ROW FIRST, THEN THE DEFAULT ROW. ONLY ROWS
      *    EFFECTIVE ON THE BUSINESS DATE ARE CONSIDERED.
           MOVE 'N'                        TO WS-MAP-FOUND-IND
           MOVE WS-SM-TXN-CODE (SM-IDX)    TO HV-TXN-CODE
           MOVE WS-SM-DR-CR (SM-IDX)       TO HV-DR-CR-IND
           MOVE WS-SM-PROD-CD (SM-IDX)     TO HV-PROD-CD
           PERFORM 3310-SELECT-MAPPING
           IF NOT WS-MAP-FOUND
               MOVE WS-DEFAULT-PROD        TO HV-PROD-CD
               PERFORM 3310-SELECT-MAPPING
           END-IF
           .
      *================================================================*
       3310-SELECT-MAPPING.
      *================================================================*
           EXEC SQL
               SELECT CONTRA_GL, CONTRA_CC, MAP_ACTION, DESCRIPTION
                 INTO :GLM-CONTRA-GL, :GLM-CONTRA-CC,
                      :GLM-MAP-ACTION, :GLM-DESCRIPTION
                 FROM MBK.GLMAP
                WHERE TXN_CODE  = :HV-TXN-CODE
                  AND DR_CR_IND = :HV-DR-CR-IND
                  AND PROD_CD   = :HV-PROD-CD
                  AND :HV-POST-DATE BETWEEN EFFECTIVE_FROM
                                        AND EFFECTIVE_TO
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   SET WS-MAP-FOUND        TO TRUE
                   IF GLM-DESCRIPTION-LEN < 40
                       MOVE SPACES TO GLM-DESCRIPTION-TEXT
                                     (GLM-DESCRIPTION-LEN + 1:)
                   END-IF
               WHEN +100
                   CONTINUE
               WHEN -811
      *            OVERLAPPING EFFECTIVE DATES - DATA ERROR IN GLMAP.
                   MOVE '3310-SELECT-MAPPING' TO ABN-PARAGRAPH
                   MOVE 1004               TO ABN-ABEND-CODE
                   MOVE 'GLMAP HAS OVERLAPPING ROWS - FIX THE TABLE'
                                           TO ABN-MESSAGE
                   MOVE HV-TXN-CODE        TO ABN-KEY-DATA
                   PERFORM 9200-ROLLBACK
                   PERFORM Z999-ABEND
               WHEN OTHER
                   MOVE '3310-SELECT-MAPPING' TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       3400-POST-PAIR.
      *================================================================*
      *    CUSTOMER CREDIT: DR CONTRA, CR BALANCE GL
      *    CUSTOMER DEBIT : DR BALANCE GL, CR CONTRA
           IF WS-SM-DR-CR (SM-IDX) = 'C'
               MOVE GLM-CONTRA-GL          TO GLJD-GL-ACCOUNT
               SET GLJD-DEBIT              TO TRUE
               PERFORM 3800-GL-DETAIL
               MOVE WS-PG-BALANCE-GL (PG-IDX) TO GLJD-GL-ACCOUNT
               SET GLJD-CREDIT             TO TRUE
               PERFORM 3800-GL-DETAIL
           ELSE
               MOVE WS-PG-BALANCE-GL (PG-IDX) TO GLJD-GL-ACCOUNT
               SET GLJD-DEBIT              TO TRUE
               PERFORM 3800-GL-DETAIL
               MOVE GLM-CONTRA-GL          TO GLJD-GL-ACCOUNT
               SET GLJD-CREDIT             TO TRUE
               PERFORM 3800-GL-DETAIL
           END-IF
           .
      *================================================================*
       3500-CLEAR-INTERNAL-TO-SUSPENSE.
      *================================================================*
           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-DETAIL                  TO TRUE
           MOVE 'GBP'                      TO GLJD-CCY-CD
           MOVE '000000'                   TO GLJD-COST-CENTRE
           MOVE '*INTERNAL*'               TO GLJD-PROD-CD
           MOVE 'TFR '                     TO GLJD-TXN-CODE
           MOVE 'INTERNAL CLEARING IMBALANCE TO SUSPENSE'
                                           TO GLJD-DESCRIPTION
           IF WS-INTERNAL-NET > ZERO
      *        NET DEBIT ON CLEARING - CREDIT IT, DEBIT SUSPENSE
               MOVE WS-INTERNAL-NET        TO GLJD-AMOUNT
               MOVE WS-SUSPENSE-GL         TO GLJD-GL-ACCOUNT
               SET GLJD-DEBIT              TO TRUE
               PERFORM 3810-WRITE-DETAIL
               MOVE WS-INTERNAL-GL         TO GLJD-GL-ACCOUNT
               SET GLJD-CREDIT             TO TRUE
               PERFORM 3810-WRITE-DETAIL
           ELSE
               COMPUTE GLJD-AMOUNT = 0 - WS-INTERNAL-NET
               MOVE WS-INTERNAL-GL         TO GLJD-GL-ACCOUNT
               SET GLJD-DEBIT              TO TRUE
               PERFORM 3810-WRITE-DETAIL
               MOVE WS-SUSPENSE-GL         TO GLJD-GL-ACCOUNT
               SET GLJD-CREDIT             TO TRUE
               PERFORM 3810-WRITE-DETAIL
           END-IF

           MOVE '*INTERNAL*'               TO GSP-PROD-CD
           MOVE 'TFR '                     TO GSP-TXN-CODE
           IF WS-INTERNAL-NET > ZERO
               MOVE 'D'                    TO GSP-DR-CR-IND
               MOVE WS-INTERNAL-NET        TO GSP-AMOUNT
           ELSE
               MOVE 'C'                    TO GSP-DR-CR-IND
               COMPUTE GSP-AMOUNT = 0 - WS-INTERNAL-NET
           END-IF
           MOVE 1                          TO GSP-ITEM-COUNT
           MOVE 'INTX'                     TO GSP-REASON-CD
           MOVE 'INTERNAL TRANSFER CLEARING DID NOT NET TO ZERO'
                                           TO GSP-NOTES-TEXT
           PERFORM 3650-INSERT-SUSPENSE
           .
      *================================================================*
       3600-RAISE-SUSPENSE.
      *================================================================*
           MOVE WS-SM-PROD-CD (SM-IDX)     TO GSP-PROD-CD
           MOVE WS-SM-TXN-CODE (SM-IDX)    TO GSP-TXN-CODE
           MOVE WS-SM-DR-CR (SM-IDX)       TO GSP-DR-CR-IND
           MOVE WS-SM-AMOUNT (SM-IDX)      TO GSP-AMOUNT
           MOVE WS-SM-COUNT (SM-IDX)       TO GSP-ITEM-COUNT
           MOVE 'NMAP'                     TO GSP-REASON-CD
           MOVE SPACES                     TO GSP-NOTES-TEXT
           STRING 'NO GLMAP ROW FOR ' WS-SM-TXN-CODE (SM-IDX)
                  ' / ' WS-SM-DR-CR (SM-IDX)
                  ' / ' WS-SM-PROD-CD (SM-IDX)
                  DELIMITED BY SIZE
                  INTO GSP-NOTES-TEXT
           END-STRING
           PERFORM 3650-INSERT-SUSPENSE
           .
      *================================================================*
       3650-INSERT-SUSPENSE.
      *================================================================*
           EXEC SQL
               SELECT COALESCE(MAX(SUSP_ID), 0) + 1
                 INTO :WS-SUSP-SEQ
                 FROM MBK.GLSUSP
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '3650-INSERT-SUSPENSE' TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           MOVE WS-SUSP-SEQ                TO GSP-SUSP-ID
           MOVE WS-BUS-DATE-DB2            TO GSP-RAISED-DATE
           MOVE WS-PROGRAM-ID              TO GSP-SOURCE-PGM
           MOVE WS-SUSPENSE-GL             TO GSP-SUSP-GL
           MOVE 'O'                        TO GSP-SUSP-STATUS
           MOVE 80                         TO GSP-NOTES-LEN

           EXEC SQL
               INSERT INTO MBK.GLSUSP
                     (SUSP_ID, RAISED_DATE, SOURCE_PGM, SUSP_GL,
                      PROD_CD, TXN_CODE, DR_CR_IND, AMOUNT,
                      ITEM_COUNT, REASON_CD, SUSP_STATUS,
                      CLEARED_DATE, CLEARED_BY, CLEARING_REF, NOTES)
               VALUES
                     (:GSP-SUSP-ID, :GSP-RAISED-DATE,
                      :GSP-SOURCE-PGM, :GSP-SUSP-GL, :GSP-PROD-CD,
                      :GSP-TXN-CODE, :GSP-DR-CR-IND, :GSP-AMOUNT,
                      :GSP-ITEM-COUNT, :GSP-REASON-CD,
                      :GSP-SUSP-STATUS, NULL, NULL, NULL, :GSP-NOTES)
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE '3650-INSERT-SUSPENSE' TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *================================================================*
       3800-GL-DETAIL.
      *================================================================*
           MOVE WS-SM-AMOUNT (SM-IDX)      TO GLJD-AMOUNT
           MOVE WS-PG-COST-CENTRE (PG-IDX) TO GLJD-COST-CENTRE
           IF GLJD-GL-ACCOUNT = GLM-CONTRA-GL
           AND GLM-CONTRA-CC NOT = SPACES
           AND WS-MAP-FOUND
               MOVE GLM-CONTRA-CC          TO GLJD-COST-CENTRE
           END-IF
           MOVE WS-SM-PROD-CD (SM-IDX)     TO GLJD-PROD-CD
           MOVE WS-SM-CCY-CD (SM-IDX)      TO GLJD-CCY-CD
           MOVE WS-SM-TXN-CODE (SM-IDX)    TO GLJD-TXN-CODE
           IF WS-MAP-FOUND
               MOVE GLM-DESCRIPTION-TEXT   TO GLJD-DESCRIPTION
           ELSE
               MOVE 'UNMAPPED CUSTOMER MOVEMENT' TO GLJD-DESCRIPTION
           END-IF
           PERFORM 3810-WRITE-DETAIL
           .
      *================================================================*
       3810-WRITE-DETAIL.
      *================================================================*
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-GL-LINE-COUNT
           MOVE WS-GL-LINE-COUNT           TO GLJD-LINE-NO
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE SPACES                     TO GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE GLJ-JOURNAL-ID             TO GLJD-SOURCE-REF
           IF GLJD-DEBIT
               ADD GLJD-AMOUNT             TO WS-GL-TOTAL-DR
           ELSE
               ADD GLJD-AMOUNT             TO WS-GL-TOTAL-CR
           END-IF
           PERFORM 3900-WRITE-GL-RECORD
           .
      *================================================================*
       3900-WRITE-GL-RECORD.
      *================================================================*
           WRITE GLJNL-RECORD              FROM GLJ-JOURNAL-LINE
           IF WS-GLJNL-STATUS NOT = '00'
               MOVE '3900-WRITE-GL-RECORD' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'GLJNL WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-GLJNL-STATUS        TO ABN-FILE-STATUS
               PERFORM 9200-ROLLBACK
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       4000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC

           IF WS-LINE-COUNT > WS-LINES-PER-PAGE - 14
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE 'TRANSACTIONS SUMMARISED'  TO TL-LABEL
           MOVE WS-TXN-COUNT               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMER DEBITS'          TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-DR-MOVES          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMER CREDITS'         TO TL-LABEL
           MOVE WS-TOTAL-CR-MOVES          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'GROUPS POSTED TO CONTRA'  TO TL-LABEL
           MOVE WS-POSTED-GROUPS           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'GROUPS SKIPPED (JOURNALED AT SOURCE)'
                                           TO TL-LABEL
           MOVE WS-SKIPPED-GROUPS          TO TL-COUNT
           MOVE WS-SKIPPED-AMOUNT          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'GROUPS INTERNAL'          TO TL-LABEL
           MOVE WS-INTERNAL-GROUPS         TO TL-COUNT
           MOVE WS-INTERNAL-NET            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'GROUPS TO SUSPENSE'       TO TL-LABEL
           MOVE WS-SUSPENSE-GROUPS         TO TL-COUNT
           MOVE WS-SUSPENSE-AMOUNT         TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'GL LINES / TOTAL DEBITS'  TO TL-LABEL
           MOVE WS-GL-LINE-COUNT           TO TL-COUNT
           MOVE WS-GL-TOTAL-DR             TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE ACCTMAST-FILE
                 PRDPARM-FILE
                 GLJNL-FILE
                 REPORT-FILE

           DISPLAY 'GLEXT010 - GL LINES        ' WS-GL-LINE-COUNT
           DISPLAY 'GLEXT010 - SUSPENSE GROUPS ' WS-SUSPENSE-GROUPS
           IF WS-SUSPENSE-GROUPS > ZERO
           OR WS-NO-ACCT-COUNT > ZERO
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
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           PERFORM 9200-ROLLBACK
           PERFORM Z999-ABEND
           .
      *================================================================*
       9200-ROLLBACK.
      *================================================================*
           EXEC SQL ROLLBACK END-EXEC
           .
      *
           COPY ABNDPROC.
