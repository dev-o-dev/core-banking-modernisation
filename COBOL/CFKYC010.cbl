       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFKYC010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-03-07.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFKYC010                                            *
      * TITLE    : PERIODIC KYC REVIEW SCHEDULING (MLR 2007 REG 8)     *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CFKYC01 (WEEKLY - SUNDAY)                           *
      *----------------------------------------------------------------*
      * INPUT    : KYCDONE  - COMPLETED REVIEWS FROM THE KYC TEAM'S    *
      *                       CASE TOOL (FB 100)                       *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : CIFMAST  - CIF MASTER (DYNAMIC)                     *
      * OUTPUT   : KYCCASE  - NEW REVIEW CASES FOR THE CASE TOOL       *
      *                       (FB 200)                                 *
      *            LETTERS  - DOCUMENT REQUEST / REMINDER LETTERS      *
      *                       (FB 200)                                 *
      *            REPORT   - KYC REVIEW REPORT (FBA 133)              *
      *----------------------------------------------------------------*
      * ONGOING MONITORING (MONEY LAUNDERING REGULATIONS 2007 REG 8,   *
      * JMLSG GUIDANCE PART I 5.7): CUSTOMER INFORMATION IS REFRESHED  *
      * ON A CYCLE DRIVEN BY RISK RATING -                             *
      *     HIGH (AND ALL PEPS)   EVERY 12 MONTHS                      *
      *     MEDIUM                EVERY 36 MONTHS                      *
      *     LOW                   EVERY 60 MONTHS                      *
      *                                                                *
      * PASS 1 - COMPLETED REVIEWS (KYCDONE): KYC STATUS, RISK RATING  *
      *          AND PEP FLAG UPDATED, LAST REVIEW = REVIEW DATE AND   *
      *          THE NEXT REVIEW SCHEDULED.                            *
      * PASS 2 - SWEEP OF THE CIF:                                     *
      *   - NO NEXT REVIEW DATE (CUSTOMERS BEFORE 2011): SCHEDULED     *
      *     FROM THE CREATE DATE, SPREAD BY CIF NUMBER SO THAT THE     *
      *     BACK BOOK DOES NOT ALL FALL DUE IN ONE WEEK                *
      *   - DUE: CASE RAISED, LETTER 'KYC1' REQUESTING DOCUMENTS,      *
      *     KYC STATUS SET TO PENDING                                  *
      *   - PENDING FOR 60 DAYS: REMINDER 'KYC2'                       *
      *   - PENDING FOR 120 DAYS: ESCALATION CASE ('EXIT') FOR THE     *
      *     FINANCIAL CRIME TEAM TO CONSIDER RESTRICTION OR EXIT       *
      * DECEASED, CLOSED AND MERGED CUSTOMERS ARE SKIPPED.             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-03-07 AOS  ORIGINAL VERSION (FSA THEMATIC REVIEW ON PEPS  *
      *                 AND HIGH RISK CUSTOMERS, JUNE 2011)            *
      * 2016-03-14 PDQ  BACK BOOK SPREAD CHANGED FROM 52 TO 104 WEEKS  *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KYCDONE-FILE     ASSIGN TO KYCDONE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-KYCDONE-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT KYCCASE-FILE     ASSIGN TO KYCCASE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-KYCCASE-STATUS.
           SELECT LETTERS-FILE     ASSIGN TO LETTERS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LETTERS-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KYCDONE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  KYCDONE-RECORD                  PIC X(100).
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  KYCCASE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  KYCCASE-RECORD                  PIC X(200).
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFKYC010'.
      *
       01  WS-CONSTANTS.
           05  WS-HIGH-MONTHS              PIC S9(03) COMP-3 VALUE +12.
           05  WS-MEDIUM-MONTHS            PIC S9(03) COMP-3 VALUE +36.
           05  WS-LOW-MONTHS               PIC S9(03) COMP-3 VALUE +60.
           05  WS-REMIND-DAYS              PIC S9(05) COMP-3 VALUE +60.
           05  WS-ESCALATE-DAYS            PIC S9(05) COMP-3 VALUE +120.
           05  WS-BACKBOOK-SPREAD          PIC S9(05) COMP-3 VALUE +728.
      *
      *---- COMPLETED REVIEW ------------------------------------------*
       01  KYD-COMPLETED-REVIEW.
           05  KYD-CIF-NO                  PIC X(12).
           05  KYD-CASE-REF                PIC X(12).
           05  KYD-REVIEW-DATE             PIC 9(08).
           05  KYD-OUTCOME                 PIC X(01).
               88  KYD-VERIFIED                VALUE 'V'.
               88  KYD-FAILED                  VALUE 'F'.
           05  KYD-RISK-RATING             PIC X(01).
               88  KYD-RISK-VALID              VALUE 'L' 'M' 'H'.
           05  KYD-PEP-IND                 PIC X(01).
           05  KYD-REVIEWER                PIC X(08).
           05  FILLER                      PIC X(57).
      *
       01  WS-FILE-STATUSES.
           05  WS-KYCDONE-STATUS           PIC X(02).
               88  WS-KYCDONE-OK               VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-EOF              VALUE '10'.
           05  WS-KYCCASE-STATUS           PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CIF               VALUE 'Y'.
           05  WS-CHANGED-IND              PIC X(01).
               88  WS-CIF-CHANGED              VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-BASE-DATE                PIC 9(08).
           05  WS-CYCLE-MONTHS             PIC S9(03) COMP-3.
           05  WS-DAYS-PENDING             PIC S9(07) COMP-3.
           05  WS-SPREAD-DAYS              PIC S9(07) COMP-3.
           05  WS-CIF-NUMERIC              PIC 9(12).
           05  WS-ACTION                   PIC X(10).
           05  WS-NOTE                     PIC X(40).
           05  WS-CASE-SEQ                 PIC 9(06) VALUE ZERO.
      *
      *---- MONTH ARITHMETIC ------------------------------------------*
       01  WS-MONTH-ARITH.
           05  WS-MA-DATE                  PIC 9(08).
           05  WS-MA-DATE-R REDEFINES WS-MA-DATE.
               10  WS-MA-CCYY              PIC 9(04).
               10  WS-MA-MM                PIC 9(02).
               10  WS-MA-DD                PIC 9(02).
           05  WS-MA-MONTHS                PIC S9(05) COMP-3.
           05  WS-MA-TOTAL-MONTHS          PIC S9(07) COMP-3.
      *
       01  WS-CASE.
           05  KC-CASE-TYPE                PIC X(04).
               88  KC-PERIODIC                 VALUE 'PREV'.
               88  KC-ESCALATION               VALUE 'EXIT'.
           05  KC-CASE-REF                 PIC X(12).
           05  KC-RAISED-DATE              PIC 9(08).
           05  KC-CIF-NO                   PIC X(12).
           05  KC-CUST-TYPE                PIC X(01).
           05  KC-RISK-RATING              PIC X(01).
           05  KC-PEP-IND                  PIC X(01).
           05  KC-LAST-REVIEW              PIC 9(08).
           05  KC-DUE-DATE                 PIC 9(08).
           05  KC-HOME-BRANCH              PIC X(06).
           05  KC-NAME                     PIC X(60).
           05  KC-POST-CD                  PIC X(10).
           05  FILLER                      PIC X(69).
      *
       01  WS-LETTER.
           05  KL-LETTER-CD                PIC X(04).
           05  KL-ISSUE-DATE               PIC 9(08).
           05  KL-CIF-NO                   PIC X(12).
           05  KL-TITLE                    PIC X(06).
           05  KL-FIRST-NM                 PIC X(40).
           05  KL-LAST-NM                  PIC X(40).
           05  KL-ADDR-LINE1               PIC X(30).
           05  KL-POST-CD                  PIC X(10).
           05  KL-RESPOND-BY               PIC 9(08).
           05  FILLER                      PIC X(42).
      *
       01  WS-COUNTERS.
           05  WS-DONE-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DONE-APPLIED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DONE-REJECTED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-DONE-FAILED              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CIF-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-SCHEDULED                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CASES-RAISED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-REMINDERS                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ESCALATIONS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-PEPS                     PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CFKYC010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - PERIODIC KYC REVIEW'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(14) VALUE 'CUSTOMER'.
           05  FILLER            PIC X(04) VALUE 'RSK'.
           05  FILLER            PIC X(04) VALUE 'PEP'.
           05  FILLER            PIC X(04) VALUE 'KYC'.
           05  FILLER            PIC X(10) VALUE 'LAST REV'.
           05  FILLER            PIC X(10) VALUE 'NEXT REV'.
           05  FILLER            PIC X(12) VALUE 'ACTION'.
           05  FILLER            PIC X(74) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RISK                     PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-PEP                      PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-KYC                      PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-LAST-REVIEW              PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NEXT-REVIEW              PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
           05  FILLER                      PIC X(34) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
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
           PERFORM 2000-APPLY-COMPLETED
           PERFORM 3000-SWEEP-CIF
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  KYCDONE-FILE
                       DATECARD-FILE
                I-O    CIFMAST-FILE
                OUTPUT KYCCASE-FILE
                       LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-KYCDONE-OK
           OR NOT WS-CIFMAST-OK
           OR WS-KYCCASE-STATUS NOT = '00'
           OR WS-LETTERS-STATUS NOT = '00'
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
           COMPUTE WS-BUS-DATE-INT =
                   FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           .
      *================================================================*
       2000-APPLY-COMPLETED.
      *================================================================*
           READ KYCDONE-FILE INTO KYD-COMPLETED-REVIEW
           PERFORM UNTIL NOT WS-KYCDONE-OK
               ADD 1                       TO WS-DONE-READ
               PERFORM 2100-APPLY-ONE THRU 2100-EXIT
               READ KYCDONE-FILE INTO KYD-COMPLETED-REVIEW
           END-PERFORM
           CLOSE KYCDONE-FILE
           .
      *================================================================*
       2100-APPLY-ONE.
      *================================================================*
           MOVE SPACES                     TO WS-NOTE
           MOVE KYD-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-CIFMAST-OK
                   MOVE 'CUSTOMER NOT ON CIF' TO WS-NOTE
               WHEN NOT (KYD-VERIFIED OR KYD-FAILED)
                   MOVE 'INVALID OUTCOME'  TO WS-NOTE
               WHEN NOT KYD-RISK-VALID
                   MOVE 'INVALID RISK RATING' TO WS-NOTE
               WHEN KYD-REVIEW-DATE NOT NUMERIC
               WHEN KYD-REVIEW-DATE > WS-BUS-DATE
                   MOVE 'INVALID REVIEW DATE' TO WS-NOTE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-NOTE NOT = SPACES
               ADD 1                       TO WS-DONE-REJECTED
               MOVE 'REJECTED'             TO WS-ACTION
               PERFORM 7100-REPORT-DETAIL
               GO TO 2100-EXIT
           END-IF

           MOVE KYD-OUTCOME                TO CIF-KYC-STATUS
           MOVE KYD-RISK-RATING            TO CIF-RISK-RATING
           IF KYD-PEP-IND = 'Y'
               MOVE 'Y'                    TO CIF-PEP-IND
           ELSE
               MOVE 'N'                    TO CIF-PEP-IND
           END-IF
           MOVE KYD-REVIEW-DATE            TO CIF-KYC-LAST-REVIEW
           MOVE KYD-REVIEW-DATE            TO WS-BASE-DATE
           PERFORM 6000-SCHEDULE-NEXT
           MOVE WS-BUS-DATE                TO CIF-LAST-MAINT-DATE
           MOVE ZERO                       TO CIF-LAST-MAINT-TIME
           MOVE KYD-REVIEWER               TO CIF-LAST-MAINT-USER
           SET CIF-MAINT-BATCH             TO TRUE
           REWRITE CIF-CUSTOMER-RECORD
           IF NOT WS-CIFMAST-OK
               MOVE '2100-APPLY-ONE'       TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CIFMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CIFMAST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-DONE-APPLIED
           IF KYD-FAILED
      *        FAILED REVIEW - THE FINANCIAL CRIME TEAM DECIDES ON
      *        RESTRICTION OR EXIT; THE CASE TOOL RAISES THAT CASE.
               ADD 1                       TO WS-DONE-FAILED
               MOVE 'FAILED'               TO WS-ACTION
               MOVE 'REVIEW FAILED - FINCRIME CASE IN TOOL'
                                           TO WS-NOTE
               PERFORM 7100-REPORT-DETAIL
           END-IF
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       3000-SWEEP-CIF.
      *================================================================*
           MOVE LOW-VALUES                 TO CIF-KEY
           START CIFMAST-FILE KEY NOT < CIF-KEY
           IF NOT WS-CIFMAST-OK
               SET WS-END-OF-CIF           TO TRUE
           END-IF
           PERFORM UNTIL WS-END-OF-CIF
               READ CIFMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN WS-CIFMAST-EOF
                       SET WS-END-OF-CIF   TO TRUE
                   WHEN NOT WS-CIFMAST-OK
                       MOVE '3000-SWEEP-CIF' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                       MOVE WS-CIFMAST-STATUS TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
                   WHEN CIF-DECEASED OR CIF-CLOSED
                   WHEN CIF-MERGED-INTO NOT = SPACES
                       CONTINUE
                   WHEN OTHER
                       ADD 1               TO WS-CIF-READ
                       PERFORM 3100-CHECK-CUSTOMER
               END-EVALUATE
           END-PERFORM
           .
      *================================================================*
       3100-CHECK-CUSTOMER.
      *================================================================*
           MOVE 'N'                        TO WS-CHANGED-IND
           MOVE SPACES                     TO WS-ACTION WS-NOTE
           IF CIF-IS-PEP
               ADD 1                       TO WS-PEPS
           END-IF

      *---- BACK BOOK: NO SCHEDULE YET --------------------------------*
           IF CIF-KYC-NEXT-REVIEW NOT NUMERIC
           OR CIF-KYC-NEXT-REVIEW = ZERO
               IF CIF-KYC-LAST-REVIEW NOT NUMERIC
                   MOVE ZERO               TO CIF-KYC-LAST-REVIEW
               END-IF
               IF CIF-PEP-IND NOT = 'Y'
                   MOVE 'N'                TO CIF-PEP-IND
               END-IF
      *        SPREAD: FIRST REVIEW BETWEEN NOW AND 104 WEEKS AHEAD,
      *        POSITION FROM THE CIF NUMBER.
               IF CIF-NO NUMERIC
                   MOVE CIF-NO             TO WS-CIF-NUMERIC
               ELSE
                   MOVE ZERO               TO WS-CIF-NUMERIC
               END-IF
               COMPUTE WS-SPREAD-DAYS =
                   FUNCTION MOD (WS-CIF-NUMERIC, WS-BACKBOOK-SPREAD)
               IF CIF-RISK-HIGH OR CIF-IS-PEP
                   COMPUTE WS-SPREAD-DAYS = WS-SPREAD-DAYS / 8
               END-IF
               COMPUTE CIF-KYC-NEXT-REVIEW = FUNCTION DATE-OF-INTEGER
                       (WS-BUS-DATE-INT + 7 + WS-SPREAD-DAYS)
               SET WS-CIF-CHANGED          TO TRUE
               ADD 1                       TO WS-SCHEDULED
               GO TO 3100-REWRITE
           END-IF

           EVALUATE TRUE
      *---- PENDING - REMIND / ESCALATE -------------------------------*
               WHEN CIF-KYC-PENDING
                   COMPUTE WS-DAYS-PENDING = WS-BUS-DATE-INT
                       - FUNCTION INTEGER-OF-DATE (CIF-KYC-NEXT-REVIEW)
                   EVALUATE TRUE
                       WHEN WS-DAYS-PENDING >= WS-ESCALATE-DAYS
                       AND  WS-DAYS-PENDING < WS-ESCALATE-DAYS + 7
                           SET KC-ESCALATION TO TRUE
                           PERFORM 5000-RAISE-CASE
                           ADD 1           TO WS-ESCALATIONS
                           MOVE 'ESCALATED' TO WS-ACTION
                           MOVE 'NO RESPONSE IN 120 DAYS - FINCRIME'
                                           TO WS-NOTE
                       WHEN WS-DAYS-PENDING >= WS-REMIND-DAYS
                       AND  WS-DAYS-PENDING < WS-REMIND-DAYS + 7
                           MOVE 'KYC2'     TO KL-LETTER-CD
                           PERFORM 5100-WRITE-LETTER
                           ADD 1           TO WS-REMINDERS
                           MOVE 'REMINDER' TO WS-ACTION
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
      *---- DUE -------------------------------------------------------*
               WHEN CIF-KYC-NEXT-REVIEW <= WS-BUS-DATE
                   SET KC-PERIODIC         TO TRUE
                   PERFORM 5000-RAISE-CASE
                   MOVE 'KYC1'             TO KL-LETTER-CD
                   PERFORM 5100-WRITE-LETTER
                   SET CIF-KYC-PENDING     TO TRUE
                   SET WS-CIF-CHANGED      TO TRUE
                   ADD 1                   TO WS-CASES-RAISED
                   MOVE 'CASE RAISED'      TO WS-ACTION
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       3100-REWRITE.
           IF WS-CIF-CHANGED
               MOVE WS-BUS-DATE            TO CIF-LAST-MAINT-DATE
               MOVE ZERO                   TO CIF-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO CIF-LAST-MAINT-USER
               SET CIF-MAINT-BATCH         TO TRUE
               REWRITE CIF-CUSTOMER-RECORD
               IF NOT WS-CIFMAST-OK
                   MOVE '3100-CHECK-CUSTOMER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST REWRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               END-IF
           END-IF
           IF WS-ACTION NOT = SPACES
               PERFORM 7100-REPORT-DETAIL
           END-IF
           .
      *================================================================*
       5000-RAISE-CASE.
      *================================================================*
           ADD 1                           TO WS-CASE-SEQ
           STRING 'K' WS-BUS-DATE (3:6) WS-CASE-SEQ (2:5)
                  DELIMITED BY SIZE
                  INTO KC-CASE-REF
           END-STRING
           MOVE WS-BUS-DATE                TO KC-RAISED-DATE
           MOVE CIF-NO                     TO KC-CIF-NO
           MOVE CIF-CUST-TYPE              TO KC-CUST-TYPE
           MOVE CIF-RISK-RATING            TO KC-RISK-RATING
           MOVE CIF-PEP-IND                TO KC-PEP-IND
           MOVE CIF-KYC-LAST-REVIEW        TO KC-LAST-REVIEW
           MOVE CIF-KYC-NEXT-REVIEW        TO KC-DUE-DATE
           MOVE CIF-HOME-BRANCH-SC         TO KC-HOME-BRANCH
           MOVE SPACES                     TO KC-NAME
           STRING CIF-FIRST-NM  DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-LAST-NM   DELIMITED BY '  '
                  INTO KC-NAME
           END-STRING
           MOVE CIF-POST-CD                TO KC-POST-CD
           WRITE KYCCASE-RECORD            FROM WS-CASE
           IF WS-KYCCASE-STATUS NOT = '00'
               MOVE '5000-RAISE-CASE'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'KYCCASE WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-KYCCASE-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       5100-WRITE-LETTER.
      *================================================================*
           MOVE WS-BUS-DATE                TO KL-ISSUE-DATE
           MOVE CIF-NO                     TO KL-CIF-NO
           MOVE CIF-TITLE                  TO KL-TITLE
           MOVE CIF-FIRST-NM               TO KL-FIRST-NM
           MOVE CIF-LAST-NM                TO KL-LAST-NM
           MOVE CIF-ADDR-LINE1             TO KL-ADDR-LINE1
           MOVE CIF-POST-CD                TO KL-POST-CD
           COMPUTE KL-RESPOND-BY = FUNCTION DATE-OF-INTEGER
                   (WS-BUS-DATE-INT + 30)
           WRITE LETTERS-RECORD            FROM WS-LETTER
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '5100-WRITE-LETTER'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       6000-SCHEDULE-NEXT.
      *================================================================*
           EVALUATE TRUE
               WHEN CIF-IS-PEP OR CIF-RISK-HIGH
                   MOVE WS-HIGH-MONTHS     TO WS-CYCLE-MONTHS
               WHEN CIF-RISK-MEDIUM
                   MOVE WS-MEDIUM-MONTHS   TO WS-CYCLE-MONTHS
               WHEN OTHER
                   MOVE WS-LOW-MONTHS      TO WS-CYCLE-MONTHS
           END-EVALUATE
           MOVE WS-BASE-DATE               TO WS-MA-DATE
           MOVE WS-CYCLE-MONTHS            TO WS-MA-MONTHS
           COMPUTE WS-MA-TOTAL-MONTHS =
                   WS-MA-CCYY * 12 + WS-MA-MM - 1 + WS-MA-MONTHS
           COMPUTE WS-MA-CCYY = WS-MA-TOTAL-MONTHS / 12
           COMPUTE WS-MA-MM =
                   WS-MA-TOTAL-MONTHS - (WS-MA-CCYY * 12) + 1
      *    ALWAYS THE 1ST OF THE MONTH - AVOIDS 29-31 PROBLEMS AND
      *    GROUPS CASES FOR THE TEAM'S MONTHLY PLANNING.
           MOVE 01                         TO WS-MA-DD
           MOVE WS-MA-DATE                 TO CIF-KYC-NEXT-REVIEW
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
           MOVE CIF-NO                     TO DL-CIF-NO
           MOVE CIF-RISK-RATING            TO DL-RISK
           MOVE CIF-PEP-IND                TO DL-PEP
           MOVE CIF-KYC-STATUS             TO DL-KYC
           IF CIF-KYC-LAST-REVIEW NUMERIC
               MOVE CIF-KYC-LAST-REVIEW    TO DL-LAST-REVIEW
           ELSE
               MOVE ZERO                   TO DL-LAST-REVIEW
           END-IF
           IF CIF-KYC-NEXT-REVIEW NUMERIC
               MOVE CIF-KYC-NEXT-REVIEW    TO DL-NEXT-REVIEW
           ELSE
               MOVE ZERO                   TO DL-NEXT-REVIEW
           END-IF
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'COMPLETED REVIEWS RECEIVED' TO TL-LABEL
           MOVE WS-DONE-READ               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  APPLIED'                TO TL-LABEL
           MOVE WS-DONE-APPLIED            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH FAILED'        TO TL-LABEL
           MOVE WS-DONE-FAILED             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  REJECTED'               TO TL-LABEL
           MOVE WS-DONE-REJECTED           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACTIVE CUSTOMERS SWEPT'   TO TL-LABEL
           MOVE WS-CIF-READ                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH PEPS'          TO TL-LABEL
           MOVE WS-PEPS                    TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'BACK BOOK SCHEDULED'      TO TL-LABEL
           MOVE WS-SCHEDULED               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REVIEW CASES RAISED'      TO TL-LABEL
           MOVE WS-CASES-RAISED            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REMINDERS SENT'           TO TL-LABEL
           MOVE WS-REMINDERS               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ESCALATED TO FINANCIAL CRIME' TO TL-LABEL
           MOVE WS-ESCALATIONS             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE CIFMAST-FILE KYCCASE-FILE LETTERS-FILE REPORT-FILE
           DISPLAY 'CFKYC010 - CASES RAISED ' WS-CASES-RAISED
           DISPLAY 'CFKYC010 - ESCALATIONS  ' WS-ESCALATIONS
           IF WS-DONE-REJECTED > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
