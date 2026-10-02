       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CDSTL010.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-08-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CDSTL010                                            *
      * TITLE    : DEBIT CARD SETTLEMENT - POST CLEARED CARD ITEMS     *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CDSTL01 STEP CD020                                  *
      *----------------------------------------------------------------*
      * INPUT    : CLEARING - SCHEME CLEARING FILE SORTED BY ACCOUNT   *
      *                       (CDSTLREC)                               *
      *            PRDPARM  - PRODUCT PARAMETERS (CARD FEES)           *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : DDHOLD   - ACCOUNT HOLDS (VSAM KSDS, DYNAMIC)       *
      *            ACCTMAST - DDA ACCOUNT MASTER (RANDOM)              *
      * OUTPUT   : CRDPOST  - CARD POSTINGS FOR PSEOD01 (PSTXNREC)     *
      *            EXCEPT   - UNMATCHED / REJECTED ITEMS (FB 200)      *
      *            REPORT   - SETTLEMENT REPORT (FBA 133)              *
      *----------------------------------------------------------------*
      * FOR EACH PRESENTMENT:                                          *
      *   1. FIND THE ACTIVE 'CARD' HOLD WHOSE NARRATIVE CARRIES THE   *
      *      AUTH REF (PLACED BY CRDAUT01) AND RELEASE IT - THE WHOLE  *
      *      HOLD AMOUNT IS RELEASED EVEN IF THE SETTLED AMOUNT        *
      *      DIFFERS (TIPS, FUEL PRE-AUTHS, HOTEL FINAL BILLS).        *
      *   2. WRITE A 'CRD ' HARD DEBIT FOR THE SETTLED AMOUNT.         *
      *   3. FOR A NON-STERLING ITEM WRITE A 'CHG ' DEBIT FOR THE      *
      *      NON-STERLING TRANSACTION FEE (PRD-NSTF-PCT, MINIMUM       *
      *      PRD-NSTF-MIN). CASH WITHDRAWALS ABROAD ALSO PAY THE CASH  *
      *      FEE (PRD-CASH-FEE-PCT, MINIMUM PRD-CASH-FEE-MIN).         *
      * A PRESENTMENT WITH NO HOLD (STAND-IN AUTHORISATION, OR THE     *
      * HOLD EXPIRED BEFORE THE MERCHANT PRESENTED) IS STILL POSTED -  *
      * THE SCHEME GUARANTEES PAYMENT TO THE MERCHANT. IT IS COUNTED   *
      * AS 'FORCED' FOR THE CARD FRAUD TEAM.                           *
      *                                                                *
      * REFUNDS AND CHARGEBACKS ARE CREDITS ('CRD ' CREDIT). A REFUND  *
      * DOES NOT REVERSE THE NON-STERLING FEE (SCHEME RULE IN THE      *
      * CARDHOLDER TERMS, SECTION 14.3).                               *
      *                                                                *
      * ITEMS FOR CLOSED OR UNKNOWN ACCOUNTS GO TO EXCEPT FOR THE      *
      * CARD OPERATIONS TEAM (CHARGEBACK TO THE MERCHANT ACQUIRER IS A *
      * MANUAL PROCESS).                                               *
      *                                                                *
      * GL: 'CRD ' MOVEMENTS ARE EXTRACTED BY GLEXT010; MBK.GLMAP MAPS *
      * TXN 'CRD ' TO THE SCHEME SETTLEMENT ACCOUNT 18200-0010.        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-08-02 JNB  ORIGINAL VERSION (REPLACES THE CARD PROCESSOR'S*
      *                 'DEBIT FILE' WHICH CAME IN AS BACS-STYLE 'DD ' *
      *                 ITEMS WITH NO MERCHANT DETAIL)                 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLEARING-FILE    ASSIGN TO CLEARING
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CLEARING-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT DDHOLD-FILE      ASSIGN TO DDHOLD
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS HLD-KEY
                                   FILE STATUS IS WS-DDHOLD-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CRDPOST-FILE     ASSIGN TO CRDPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CRDPOST-STATUS.
           SELECT EXCEPT-FILE      ASSIGN TO EXCEPT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-EXCEPT-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CLEARING-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CLEARING-RECORD                 PIC X(200).
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  DDHOLD-FILE.
           COPY DDHLDREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CRDPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CRDPOST-RECORD                  PIC X(200).
       FD  EXCEPT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  EXCEPT-RECORD                   PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CDSTL010'.
      *
       01  WS-CONSTANTS.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00050000.
           05  WS-DEFAULT-NSTF-PCT         PIC S9(01)V9(04) COMP-3
                                           VALUE +0.0275.
      *
       01  WS-FILE-STATUSES.
           05  WS-CLEARING-STATUS          PIC X(02).
               88  WS-CLEARING-OK              VALUE '00'.
               88  WS-CLEARING-EOF             VALUE '10'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-DDHOLD-STATUS            PIC X(02).
               88  WS-DDHOLD-OK                VALUE '00'.
               88  WS-DDHOLD-EOF               VALUE '10'.
               88  WS-DDHOLD-NOTFND            VALUE '23'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-CRDPOST-STATUS           PIC X(02).
           05  WS-EXCEPT-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-ACCT-FOUND-IND           PIC X(01).
               88  WS-ACCT-FOUND               VALUE 'Y'.
           05  WS-ACCT-CHANGED-IND         PIC X(01).
               88  WS-ACCT-CHANGED             VALUE 'Y'.
           05  WS-HOLD-FOUND-IND           PIC X(01).
               88  WS-HOLD-FOUND               VALUE 'Y'.
           05  WS-END-HOLDS-IND            PIC X(01).
               88  WS-END-HOLDS                VALUE 'Y'.
           05  WS-TRAILER-IND              PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CURR-ACCT                PIC X(14) VALUE LOW-VALUES.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-FEE                      PIC S9(13)V99 COMP-3.
           05  WS-CASH-FEE                 PIC S9(13)V99 COMP-3.
           05  WS-NSTF-PCT                 PIC S9(01)V9(04) COMP-3.
           05  WS-NSTF-MIN                 PIC S9(05)V99 COMP-3.
           05  WS-CASH-PCT                 PIC S9(01)V9(04) COMP-3.
           05  WS-CASH-MIN                 PIC S9(05)V99 COMP-3.
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-RESULT                   PIC X(08).
           05  WS-NOTE                     PIC X(40).
           05  WS-REJECT-RSN               PIC X(04).
           05  WS-SAVED-ACCOUNT            PIC X(400).
           05  WS-HOLD-RELEASED-AMT        PIC S9(13)V99 COMP-3.
      *
       01  WS-EXCEPTION-RECORD.
           05  EX-REJECT-RSN               PIC X(04).
           05  EX-REJECT-TEXT              PIC X(40).
           05  EX-DETAIL                   PIC X(156).
      *
       01  WS-COUNTERS.
           05  WS-RECS-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-PRESENTMENTS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-MATCHED                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-FORCED                   PIC S9(09) COMP-3 VALUE 0.
           05  WS-CREDITS                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-REJECTED                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-FEES                     PIC S9(09) COMP-3 VALUE 0.
           05  WS-DEBIT-TOTAL              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-CREDIT-TOTAL             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-POSTED-DR                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-POSTED-CR                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-REJECTED-AMT             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-FEE-TOTAL                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-HOLDS-RELEASED           PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CDSTL010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - DEBIT CARD SETTLEMENT'.
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
           05  FILLER            PIC X(03) VALUE 'T'.
           05  FILLER            PIC X(14) VALUE 'AUTH REF'.
           05  FILLER            PIC X(17) VALUE '          AMOUNT'.
           05  FILLER            PIC X(27) VALUE 'MERCHANT'.
           05  FILLER            PIC X(04) VALUE 'CCY'.
           05  FILLER            PIC X(10) VALUE 'RESULT'.
           05  FILLER            PIC X(38) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TYPE                     PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AUTH-REF                 PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-MERCHANT                 PIC X(25).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CCY                      PIC X(03).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-RESULT                   PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(35).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY CDSTLREC.
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
           PERFORM 2000-PROCESS-RECORD
               UNTIL WS-END-OF-INPUT
           PERFORM 2900-END-ACCOUNT
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CLEARING-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                I-O    DDHOLD-FILE
                       ACCTMAST-FILE
                OUTPUT CRDPOST-FILE
                       EXCEPT-FILE
                       REPORT-FILE
           IF NOT WS-CLEARING-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-DDHOLD-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-CRDPOST-STATUS NOT = '00'
           OR WS-EXCEPT-STATUS NOT = '00'
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

           PERFORM 8000-READ-CLEARING
           IF WS-END-OF-INPUT OR NOT CDS-HEADER
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'CARD CLEARING FILE HAS NO HEADER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           DISPLAY 'CDSTL010 - FILE ' CDSH-FILE-ID
                   ' SETTLEMENT DATE ' CDSH-SETTLE-DATE
           PERFORM 8000-READ-CLEARING
           .
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           EVALUATE TRUE
               WHEN CDS-DETAIL
                   IF CDS-SORT-CODE NOT = WS-CURR-ACCT (1:6)
                   OR CDS-ACCT-NO NOT = WS-CURR-ACCT (7:8)
                       PERFORM 2900-END-ACCOUNT
                       PERFORM 2050-START-ACCOUNT
                   END-IF
                   PERFORM 2100-PROCESS-ITEM THRU 2100-EXIT
               WHEN CDS-TRAILER
                   SET WS-TRAILER-SEEN     TO TRUE
                   IF CDST-RECORD-COUNT NOT = WS-RECS-READ
                   OR CDST-DEBIT-TOTAL NOT = WS-DEBIT-TOTAL
                   OR CDST-CREDIT-TOTAL NOT = WS-CREDIT-TOTAL
                       MOVE '2000-PROCESS-RECORD' TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'CLEARING TRAILER COUNT / TOTAL MISMATCH'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           PERFORM 8000-READ-CLEARING
           .
      *================================================================*
       2050-START-ACCOUNT.
      *================================================================*
           MOVE CDS-SORT-CODE              TO WS-CURR-ACCT (1:6)
           MOVE CDS-ACCT-NO                TO WS-CURR-ACCT (7:8)
           MOVE 'N'                        TO WS-ACCT-FOUND-IND
                                              WS-ACCT-CHANGED-IND
           MOVE WS-DEFAULT-NSTF-PCT        TO WS-NSTF-PCT
           MOVE ZERO                       TO WS-NSTF-MIN
                                              WS-CASH-PCT
                                              WS-CASH-MIN
           MOVE CDS-SORT-CODE              TO ACM-SORT-CODE
           MOVE CDS-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   SET WS-ACCT-FOUND       TO TRUE
                   MOVE ACM-ACCOUNT-RECORD TO WS-SAVED-ACCOUNT
                   MOVE ACM-PROD-CD        TO PRD-PROD-CD
                   READ PRDPARM-FILE
                   IF WS-PRDPARM-OK
                       IF PRD-NSTF-PCT NUMERIC
                           MOVE PRD-NSTF-PCT     TO WS-NSTF-PCT
                           MOVE PRD-NSTF-MIN     TO WS-NSTF-MIN
                           MOVE PRD-CASH-FEE-PCT TO WS-CASH-PCT
                           MOVE PRD-CASH-FEE-MIN TO WS-CASH-MIN
                       END-IF
                   END-IF
               WHEN WS-ACCTMAST-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE '2050-START-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2100-PROCESS-ITEM.
      *================================================================*
           MOVE CDS-AMOUNT                 TO WS-AMOUNT
           MOVE SPACES                     TO WS-NOTE WS-REJECT-RSN
           IF WS-ACCT-FOUND
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
           END-IF

           EVALUATE TRUE
               WHEN NOT WS-ACCT-FOUND
                   MOVE 'NOAC'             TO WS-REJECT-RSN
                   MOVE 'ACCOUNT NOT FOUND' TO WS-NOTE
               WHEN ACM-CLOSED
                   MOVE 'CLSD'             TO WS-REJECT-RSN
                   MOVE 'ACCOUNT CLOSED'   TO WS-NOTE
               WHEN CDS-AMOUNT NOT NUMERIC
               WHEN CDS-AMOUNT = ZERO
                   MOVE 'AMNT'             TO WS-REJECT-RSN
                   MOVE 'INVALID AMOUNT'   TO WS-NOTE
               WHEN NOT (CDS-PRESENTMENT OR CDS-REFUND
                         OR CDS-CHARGEBACK OR CDS-REPRESENTMENT)
                   MOVE 'TYPE'             TO WS-REJECT-RSN
                   MOVE 'UNKNOWN TRANSACTION TYPE' TO WS-NOTE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-REJECT-RSN NOT = SPACES
               PERFORM 2800-REJECT
               GO TO 2100-EXIT
           END-IF

           EVALUATE TRUE
               WHEN CDS-PRESENTMENT
                   ADD 1                   TO WS-PRESENTMENTS
                   PERFORM 2200-RELEASE-HOLD THRU 2200-EXIT
                   PERFORM 2300-POST-DEBIT
                   PERFORM 2400-CHARGE-FEES THRU 2400-EXIT
               WHEN CDS-REPRESENTMENT
                   PERFORM 2300-POST-DEBIT
                   MOVE 'REPRESENTMENT'    TO WS-NOTE
               WHEN OTHER
                   PERFORM 2350-POST-CREDIT
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-RELEASE-HOLD.
      *================================================================*
           MOVE 'N'                        TO WS-HOLD-FOUND-IND
                                              WS-END-HOLDS-IND
           MOVE ZERO                       TO WS-HOLD-RELEASED-AMT
           MOVE CDS-SORT-CODE              TO HLD-SORT-CODE
           MOVE CDS-ACCT-NO                TO HLD-ACCT-NO
           MOVE 1                          TO HLD-SEQ
           START DDHOLD-FILE KEY NOT < HLD-KEY
           IF NOT WS-DDHOLD-OK
               SET WS-END-HOLDS            TO TRUE
           END-IF
           PERFORM UNTIL WS-END-HOLDS OR WS-HOLD-FOUND
               READ DDHOLD-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN NOT WS-DDHOLD-OK
                   WHEN HLD-SORT-CODE NOT = CDS-SORT-CODE
                   WHEN HLD-ACCT-NO NOT = CDS-ACCT-NO
                       SET WS-END-HOLDS    TO TRUE
                   WHEN HLD-ACTIVE
                   AND  HLD-RSN-CARD
                   AND  HLD-NARRATIVE (6:12) = CDS-AUTH-REF
                       SET WS-HOLD-FOUND   TO TRUE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM

           IF NOT WS-HOLD-FOUND
               ADD 1                       TO WS-FORCED
               MOVE 'FORCED'               TO WS-RESULT
               MOVE 'NO MATCHING HOLD - FORCE POSTED' TO WS-NOTE
               GO TO 2200-EXIT
           END-IF

           MOVE HLD-AMOUNT                 TO WS-HOLD-RELEASED-AMT
           SET HLD-RELEASED                TO TRUE
           MOVE WS-BUS-DATE                TO HLD-RELEASED-DATE
           MOVE WS-PROGRAM-ID              TO HLD-RELEASED-USER
           REWRITE HLD-HOLD-RECORD
           IF NOT WS-DDHOLD-OK
               MOVE '2200-RELEASE-HOLD'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'DDHOLD REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-DDHOLD-STATUS       TO ABN-FILE-STATUS
               MOVE HLD-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

      *    CONTROL RECORD: ONE FEWER ACTIVE HOLD.
           MOVE CDS-SORT-CODE              TO HLD-SORT-CODE
           MOVE CDS-ACCT-NO                TO HLD-ACCT-NO
           MOVE ZERO                       TO HLD-SEQ
           READ DDHOLD-FILE
           IF WS-DDHOLD-OK AND HLD-ACTIVE-COUNT > ZERO
               SUBTRACT 1                  FROM HLD-ACTIVE-COUNT
               REWRITE HLD-HOLD-RECORD
           END-IF

      *    THE HOLD COMES OFF ACM-HOLD-AMT AND BACK ONTO AVAILABLE;
      *    THE HARD DEBIT THEN TAKES THE SETTLED AMOUNT OFF AVAILABLE
      *    IN PSEOD010.
           SUBTRACT WS-HOLD-RELEASED-AMT   FROM ACM-HOLD-AMT
           ADD WS-HOLD-RELEASED-AMT        TO ACM-AVAIL-BAL
           IF ACM-HOLD-AMT < ZERO
               ADD ACM-HOLD-AMT            TO ACM-AVAIL-BAL
               MOVE ZERO                   TO ACM-HOLD-AMT
           END-IF
           MOVE ACM-ACCOUNT-RECORD         TO WS-SAVED-ACCOUNT
           SET WS-ACCT-CHANGED             TO TRUE
           ADD 1                           TO WS-MATCHED
           ADD WS-HOLD-RELEASED-AMT        TO WS-HOLDS-RELEASED
           MOVE 'SETTLED'                  TO WS-RESULT
           IF WS-HOLD-RELEASED-AMT NOT = WS-AMOUNT
               MOVE 'SETTLED AMOUNT DIFFERS FROM AUTH' TO WS-NOTE
           END-IF
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-POST-DEBIT.
      *================================================================*
           PERFORM 2500-BUILD-POSTING
           MOVE 'CRD '                     TO PST-TXN-CODE
           SET PST-DEBIT                   TO TRUE
           MOVE WS-AMOUNT                  TO PST-AMOUNT
           PERFORM 2600-WRITE-POSTING
           ADD WS-AMOUNT                   TO WS-POSTED-DR
           IF CDS-REPRESENTMENT
               MOVE 'REPRESNT'             TO WS-RESULT
           END-IF
           PERFORM 7100-REPORT-DETAIL THRU 7100-EXIT
           .
      *================================================================*
       2350-POST-CREDIT.
      *================================================================*
           PERFORM 2500-BUILD-POSTING
           MOVE 'CRD '                     TO PST-TXN-CODE
           SET PST-CREDIT                  TO TRUE
           MOVE WS-AMOUNT                  TO PST-AMOUNT
           IF CDS-CHARGEBACK
               MOVE 'CHARGEBACK CREDIT'    TO PST-NARRATIVE-2
               MOVE 'CHGBACK'              TO WS-RESULT
           ELSE
               MOVE 'REFUND'               TO WS-RESULT
           END-IF
           PERFORM 2600-WRITE-POSTING
           ADD 1                           TO WS-CREDITS
           ADD WS-AMOUNT                   TO WS-POSTED-CR
           PERFORM 7100-REPORT-DETAIL THRU 7100-EXIT
           .
      *================================================================*
       2400-CHARGE-FEES.
      *================================================================*
           IF CDS-FX-CCY = SPACES OR CDS-FX-CCY = 'GBP'
               GO TO 2400-EXIT
           END-IF
           IF ACM-CHARGES-WAIVED
               GO TO 2400-EXIT
           END-IF

           COMPUTE WS-FEE ROUNDED = WS-AMOUNT * WS-NSTF-PCT
           IF WS-FEE < WS-NSTF-MIN
               MOVE WS-NSTF-MIN            TO WS-FEE
           END-IF
           MOVE ZERO                       TO WS-CASH-FEE
           IF CDS-CASH-IND = 'Y'
               COMPUTE WS-CASH-FEE ROUNDED = WS-AMOUNT * WS-CASH-PCT
               IF WS-CASH-FEE < WS-CASH-MIN
                   MOVE WS-CASH-MIN        TO WS-CASH-FEE
               END-IF
           END-IF

           IF WS-FEE > ZERO
               PERFORM 2500-BUILD-POSTING
               MOVE 'CHG '                 TO PST-TXN-CODE
               SET PST-DEBIT               TO TRUE
               MOVE WS-FEE                 TO PST-AMOUNT
               MOVE 'NON-STERLING TXN FEE' TO PST-NARRATIVE-1
               PERFORM 2600-WRITE-POSTING
               ADD 1                       TO WS-FEES
               ADD WS-FEE                  TO WS-FEE-TOTAL
           END-IF
           IF WS-CASH-FEE > ZERO
               PERFORM 2500-BUILD-POSTING
               MOVE 'CHG '                 TO PST-TXN-CODE
               SET PST-DEBIT               TO TRUE
               MOVE WS-CASH-FEE            TO PST-AMOUNT
               MOVE 'NON-STERLING CASH FEE' TO PST-NARRATIVE-1
               PERFORM 2600-WRITE-POSTING
               ADD 1                       TO WS-FEES
               ADD WS-CASH-FEE             TO WS-FEE-TOTAL
           END-IF
           .
       2400-EXIT.
           EXIT.
      *================================================================*
       2500-BUILD-POSTING.
      *================================================================*
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE CDS-SORT-CODE              TO PST-SORT-CODE
           MOVE CDS-ACCT-NO                TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE CDS-TXN-DATE               TO PST-VALUE-DATE
           MOVE SPACES                     TO PST-NARRATIVE-1
           STRING CDS-MERCHANT-NAME DELIMITED BY '  '
                  ' '               DELIMITED BY SIZE
                  CDS-MERCHANT-TOWN DELIMITED BY '  '
                  INTO PST-NARRATIVE-1
           END-STRING
           IF CDS-FX-CCY NOT = SPACES AND CDS-FX-CCY NOT = 'GBP'
               STRING CDS-FX-CCY ' ' CDS-FX-AMOUNT ' @ ' CDS-FX-RATE
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
           ELSE
               STRING 'CARD ' CDS-TXN-DATE (7:2) '/'
                      CDS-TXN-DATE (5:2)
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
           END-IF
           MOVE 'CARDS '                   TO PST-CHANNEL-CD
           MOVE CDS-SCHEME-REF             TO PST-EXT-REF
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           .
      *================================================================*
       2600-WRITE-POSTING.
      *================================================================*
           WRITE CRDPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-CRDPOST-STATUS NOT = '00'
               MOVE '2600-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CRDPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CRDPOST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2800-REJECT.
      *================================================================*
           MOVE WS-REJECT-RSN              TO EX-REJECT-RSN
           MOVE WS-NOTE                    TO EX-REJECT-TEXT
           MOVE CDS-SETTLEMENT-RECORD (1:156) TO EX-DETAIL
           WRITE EXCEPT-RECORD             FROM WS-EXCEPTION-RECORD
           IF WS-EXCEPT-STATUS NOT = '00'
               MOVE '2800-REJECT'          TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'EXCEPT WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-EXCEPT-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-REJECTED
           ADD WS-AMOUNT                   TO WS-REJECTED-AMT
           MOVE 'REJECTED'                 TO WS-RESULT
           PERFORM 7100-REPORT-DETAIL THRU 7100-EXIT
           .
      *================================================================*
       2900-END-ACCOUNT.
      *================================================================*
           IF WS-ACCT-CHANGED
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '2900-END-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               END-IF
               MOVE 'N'                    TO WS-ACCT-CHANGED-IND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF NOT WS-TRAILER-SEEN
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'CARD CLEARING FILE HAS NO TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'CLEARING RECORDS READ'    TO TL-LABEL
           MOVE WS-RECS-READ               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PRESENTMENTS'             TO TL-LABEL
           MOVE WS-PRESENTMENTS            TO TL-COUNT
           MOVE WS-POSTED-DR               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  MATCHED TO A HOLD / HOLDS RELEASED' TO TL-LABEL
           MOVE WS-MATCHED                 TO TL-COUNT
           MOVE WS-HOLDS-RELEASED          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  FORCE POSTED (NO HOLD)' TO TL-LABEL
           MOVE WS-FORCED                  TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REFUNDS / CHARGEBACKS'    TO TL-LABEL
           MOVE WS-CREDITS                 TO TL-COUNT
           MOVE WS-POSTED-CR               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'FEES CHARGED'             TO TL-LABEL
           MOVE WS-FEES                    TO TL-COUNT
           MOVE WS-FEE-TOTAL               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REJECTED TO CARD OPERATIONS' TO TL-LABEL
           MOVE WS-REJECTED                TO TL-COUNT
           MOVE WS-REJECTED-AMT            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE CLEARING-FILE PRDPARM-FILE DDHOLD-FILE ACCTMAST-FILE
                 CRDPOST-FILE EXCEPT-FILE REPORT-FILE

           DISPLAY 'CDSTL010 - PRESENTMENTS ' WS-PRESENTMENTS
           DISPLAY 'CDSTL010 - FORCED       ' WS-FORCED
           DISPLAY 'CDSTL010 - REJECTED     ' WS-REJECTED
           IF WS-REJECTED > ZERO
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
      *    ONLY EXCEPTIONS ARE LISTED - A NORMAL DAY HAS 400,000 ITEMS.
           IF WS-NOTE = SPACES
               GO TO 7100-EXIT
           END-IF
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE CDS-SORT-CODE              TO DL-SORT-CODE
           MOVE CDS-ACCT-NO                TO DL-ACCT-NO
           MOVE CDS-TXN-TYPE               TO DL-TYPE
           MOVE CDS-AUTH-REF               TO DL-AUTH-REF
           MOVE WS-AMOUNT                  TO DL-AMOUNT
           MOVE CDS-MERCHANT-NAME          TO DL-MERCHANT
           MOVE CDS-FX-CCY                 TO DL-CCY
           MOVE WS-RESULT                  TO DL-RESULT
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
       7100-EXIT.
           EXIT.
      *================================================================*
       8000-READ-CLEARING.
      *================================================================*
           READ CLEARING-FILE INTO CDS-SETTLEMENT-RECORD
           EVALUATE TRUE
               WHEN WS-CLEARING-OK
                   IF CDS-DETAIL
                       ADD 1               TO WS-RECS-READ
                       IF CDS-AMOUNT NUMERIC
                           IF CDS-IS-DEBIT
                               ADD CDS-AMOUNT TO WS-DEBIT-TOTAL
                           ELSE
                               ADD CDS-AMOUNT TO WS-CREDIT-TOTAL
                           END-IF
                       END-IF
                   END-IF
               WHEN WS-CLEARING-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-CLEARING' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CLEARING READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CLEARING-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
