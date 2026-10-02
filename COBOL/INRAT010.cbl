       IDENTIFICATION DIVISION.
       PROGRAM-ID.    INRAT010.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2004-03-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : INRAT010                                            *
      * TITLE    : INTEREST RATE CHANGES - ANNOUNCE AND APPLY          *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : INRAT01 STEP RT020 (DAILY, BEFORE INACR01)          *
      *----------------------------------------------------------------*
      * INPUT    : RATEIN   - NEW RATE CHANGES FROM PRODUCT CONTROL    *
      *                       RTCHGREC (MAY BE EMPTY)                  *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : RATEPEND - PENDING RATE CHANGES (VSAM KSDS)         *
      *            PRDPARM  - PRODUCT PARAMETERS (VSAM KSDS, RANDOM)   *
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (SEQUENTIAL) - ONLY   *
      *                       READ WHEN NOTICES ARE NEEDED             *
      *            CIFMAST  - CIF (RANDOM) - NOTICE ADDRESSES          *
      * OUTPUT   : NOTICES  - CUSTOMER RATE CHANGE NOTICES (FB 250)    *
      *            REPORT   - RATE CHANGE REPORT (FBA 133)             *
      *----------------------------------------------------------------*
      * PHASE 1 - ANNOUNCE                                             *
      *   EACH NEW CHANGE IS VALIDATED:                                *
      *     - PRODUCT EXISTS, RATE TYPE / TIER VALID, NEW RATE 0-99%   *
      *     - FOUR EYES (REQUESTER NOT = APPROVER)                     *
      *     - NOTICE PERIOD: A DECREASE TO A CREDIT RATE OR AN         *
      *       INCREASE TO A DEBIT RATE (I.E. "TO THE CUSTOMER'S        *
      *       DISADVANTAGE") NEEDS WS-NOTICE-DAYS (14) DAYS BETWEEN    *
      *       TODAY AND THE EFFECTIVE DATE, UNLESS IT IS A BANK OF     *
      *       ENGLAND BASE RATE CHANGE ON A TRACKER PRODUCT            *
      *       (RTC-BASE-RATE-IND = 'Y') WHICH APPLIES IMMEDIATELY      *
      *   VALID CHANGES ARE WRITTEN TO RATEPEND. CHANGES TO THE        *
      *   CUSTOMER'S DISADVANTAGE GENERATE A NOTICE FOR EVERY OPEN     *
      *   ACCOUNT ON THE PRODUCT WITH A BALANCE IN THE AFFECTED TIER   *
      *   (CREDIT) OR AN OVERDRAFT LIMIT (DEBIT).                      *
      *                                                                *
      * PHASE 2 - APPLY                                                *
      *   EVERY ANNOUNCED CHANGE WITH AN EFFECTIVE DATE ON OR BEFORE   *
      *   TODAY IS APPLIED TO THE PRODUCT RECORD AND MARKED APPLIED.   *
      *   INACR010 USES THE NEW RATE FROM TONIGHT'S ACCRUAL. ACCOUNTS  *
      *   WITH A NEGOTIATED RATE (ACM-INT-RATE-CR/DR NOT ZERO) ARE NOT *
      *   AFFECTED - THEY ARE LISTED FOR RELATIONSHIP MANAGERS.        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2004-03-15 SAT  ORIGINAL VERSION                               *
      * 2008-10-08 AOS  BASE RATE TRACKER IMMEDIATE CHANGES (BANK RATE *
      *                 CUTS OCT 2008 - MARCH 2009)                    *
      * 2009-06-01 JNB  NEGOTIATED RATE ACCOUNTS LISTED                *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATEIN-FILE      ASSIGN TO RATEIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RATEIN-STATUS.
           SELECT RATEPEND-FILE    ASSIGN TO RATEPEND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS RTC-KEY
                                   FILE STATUS IS WS-RATEPEND-STATUS.
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
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT NOTICES-FILE     ASSIGN TO NOTICES
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-NOTICES-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  RATEIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RATEIN-RECORD                   PIC X(100).
       FD  RATEPEND-FILE.
           COPY RTCHGREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  NOTICES-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  NOTICES-RECORD                  PIC X(250).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'INRAT010'.
      *
       01  WS-CONSTANTS.
           05  WS-NOTICE-DAYS              PIC S9(04) COMP VALUE +14.
      *
       01  WS-FILE-STATUSES.
           05  WS-RATEIN-STATUS            PIC X(02).
               88  WS-RATEIN-OK                VALUE '00'.
               88  WS-RATEIN-EOF               VALUE '10'.
           05  WS-RATEPEND-STATUS          PIC X(02).
               88  WS-RATEPEND-OK              VALUE '00'.
               88  WS-RATEPEND-EOF             VALUE '10'.
               88  WS-RATEPEND-DUPKEY          VALUE '22'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-NOTICES-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-IN-EOF-IND               PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-PEND-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-PENDING           VALUE 'Y'.
           05  WS-ACCT-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-EFF-DATE-INT             PIC S9(09) COMP.
           05  WS-DAYS-NOTICE              PIC S9(05) COMP.
           05  WS-REJECT-TEXT              PIC X(50).
           05  WS-DISADVANTAGE-IND         PIC X(01).
               88  WS-DISADVANTAGE             VALUE 'Y'.
           05  WS-TIER-LOW                 PIC S9(13)V99 COMP-3.
           05  WS-TIER-HIGH                PIC S9(13)V99 COMP-3.
           05  WS-NOTICE-COUNT             PIC S9(07) COMP-3.
           05  WS-NEGOTIATED-COUNT         PIC S9(07) COMP-3.
           05  WS-RATE-EDIT-OLD            PIC ZZ9.9999.
           05  WS-RATE-EDIT-NEW            PIC ZZ9.9999.
      *
      *---- TABLE OF CHANGES NEEDING NOTICES (SCANNED IN ONE PASS) ----*
       01  WS-NOTICE-TABLE.
           05  WS-NT-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-NT-ENTRY                 OCCURS 50 TIMES
                                           INDEXED BY NT-IDX.
               10  WS-NT-CHANGE            PIC X(100).
               10  WS-NT-TIER-LOW          PIC S9(13)V99 COMP-3.
               10  WS-NT-TIER-HIGH         PIC S9(13)V99 COMP-3.
               10  WS-NT-NOTICES           PIC S9(07) COMP-3.
      *
      *---- NOTICE RECORD (DOCUMENT SERVICES TEMPLATE RATECHG1) -------*
       01  WS-NOTICE.
           05  NTC-SORT-CODE               PIC X(06).
           05  NTC-ACCT-NO                 PIC X(08).
           05  NTC-CIF-NO                  PIC X(12).
           05  NTC-NAME                    PIC X(60).
           05  NTC-ADDR-LINE1              PIC X(40).
           05  NTC-TOWN                    PIC X(30).
           05  NTC-POST-CD                 PIC X(10).
           05  NTC-PROD-NAME               PIC X(18).
           05  NTC-RATE-TYPE               PIC X(01).
           05  NTC-OLD-RATE                PIC 9(03)V9(04).
           05  NTC-NEW-RATE                PIC 9(03)V9(04).
           05  NTC-EFFECTIVE-DATE          PIC 9(08).
           05  NTC-BALANCE                 PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  NTC-SWITCH-RIGHT-IND        PIC X(01).
           05  FILLER                      PIC X(19).
      *
       01  WS-COUNTERS.
           05  WS-INPUT-READ               PIC S9(07) COMP-3 VALUE 0.
           05  WS-ANNOUNCED                PIC S9(07) COMP-3 VALUE 0.
           05  WS-REJECTED                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-APPLIED                  PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOTAL-NOTICES            PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-SCANNED            PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'INRAT010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - INTEREST RATE CHANGES'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(12) VALUE 'PRODUCT'.
           05  FILLER            PIC X(06) VALUE 'TYPE'.
           05  FILLER            PIC X(11) VALUE 'EFFECTIVE'.
           05  FILLER            PIC X(11) VALUE '  OLD RATE'.
           05  FILLER            PIC X(11) VALUE '  NEW RATE'.
           05  FILLER            PIC X(04) VALUE 'DIR'.
           05  FILLER            PIC X(10) VALUE 'ACTION'.
           05  FILLER            PIC X(10) VALUE ' NOTICES'.
           05  FILLER            PIC X(57) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TYPE                     PIC X(01).
           05  DL-TIER                     PIC X(01).
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  DL-EFFECTIVE                PIC 9(08).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-OLD-RATE                 PIC ZZ9.9999.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-NEW-RATE                 PIC ZZ9.9999.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-DIRECTION                PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACTION                   PIC X(09).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTICES                  PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-NOTE                     PIC X(50).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
       01  WS-INPUT-CHANGE                 PIC X(100).
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
           PERFORM 2000-ANNOUNCE-CHANGES
           IF WS-NT-COUNT > ZERO
               PERFORM 3000-PRODUCE-NOTICES
           END-IF
           PERFORM 4000-APPLY-DUE-CHANGES THRU 4000-EXIT
           PERFORM 5000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  RATEIN-FILE
                       ACCTMAST-FILE
                       CIFMAST-FILE
                       DATECARD-FILE
                I-O    RATEPEND-FILE
                       PRDPARM-FILE
                OUTPUT NOTICES-FILE
                       REPORT-FILE
           IF NOT WS-RATEIN-OK
           OR NOT WS-RATEPEND-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR WS-NOTICES-STATUS NOT = '00'
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
           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       2000-ANNOUNCE-CHANGES.
      *================================================================*
           PERFORM 8000-READ-INPUT
           PERFORM UNTIL WS-END-OF-INPUT
               MOVE WS-INPUT-CHANGE        TO RTC-RATE-CHANGE
               PERFORM 2100-VALIDATE-CHANGE THRU 2100-EXIT
               IF WS-REJECT-TEXT = SPACES
                   PERFORM 2200-REGISTER-CHANGE
               ELSE
                   ADD 1                   TO WS-REJECTED
                   MOVE 'REJECTED'         TO DL-ACTION
                   MOVE ZERO               TO DL-NOTICES
                   MOVE WS-REJECT-TEXT     TO DL-NOTE
                   PERFORM 7100-REPORT-DETAIL
               END-IF
               PERFORM 8000-READ-INPUT
           END-PERFORM
           .
      *================================================================*
       2100-VALIDATE-CHANGE.
      *================================================================*
           MOVE SPACES                     TO WS-REJECT-TEXT
           MOVE 'N'                        TO WS-DISADVANTAGE-IND
           EVALUATE TRUE
               WHEN NOT RTC-TYPE-VALID
                   MOVE 'RATE TYPE MUST BE C, B, A OR U'
                                           TO WS-REJECT-TEXT
               WHEN RTC-CREDIT-TIER
               AND (RTC-TIER NOT NUMERIC OR RTC-TIER < 1
                    OR RTC-TIER > 5)
                   MOVE 'CREDIT TIER MUST BE 1-5' TO WS-REJECT-TEXT
               WHEN RTC-NEW-RATE NOT NUMERIC
               WHEN RTC-NEW-RATE < ZERO
               WHEN RTC-NEW-RATE > 99.9999
                   MOVE 'NEW RATE OUT OF RANGE' TO WS-REJECT-TEXT
               WHEN RTC-EFFECTIVE-DATE NOT NUMERIC
                   MOVE 'EFFECTIVE DATE INVALID' TO WS-REJECT-TEXT
               WHEN RTC-REQUESTED-BY = SPACES
               WHEN RTC-APPROVED-BY = SPACES
               WHEN RTC-REQUESTED-BY = RTC-APPROVED-BY
                   MOVE 'FOUR EYES - REQUESTER AND APPROVER NEEDED'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-TEXT NOT = SPACES
               GO TO 2100-EXIT
           END-IF

           MOVE RTC-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF NOT WS-PRDPARM-OK
               MOVE 'PRODUCT NOT FOUND'    TO WS-REJECT-TEXT
               GO TO 2100-EXIT
           END-IF

      *    CURRENT RATE FROM THE PRODUCT.
           EVALUATE TRUE
               WHEN RTC-CREDIT-TIER
                   MOVE PRD-CR-TIER-RATE (RTC-TIER) TO RTC-OLD-RATE
               WHEN RTC-BONUS
                   MOVE PRD-CR-BONUS-RATE  TO RTC-OLD-RATE
               WHEN RTC-ARRANGED
                   MOVE PRD-DR-RATE-ARRANGED TO RTC-OLD-RATE
               WHEN RTC-UNARRANGED
                   MOVE PRD-DR-RATE-UNARRANGED TO RTC-OLD-RATE
           END-EVALUATE

           EVALUATE TRUE
               WHEN RTC-NEW-RATE = RTC-OLD-RATE
                   MOVE 'NEW RATE EQUALS THE CURRENT RATE'
                                           TO WS-REJECT-TEXT
                   GO TO 2100-EXIT
               WHEN RTC-NEW-RATE > RTC-OLD-RATE
                   SET RTC-INCREASE        TO TRUE
               WHEN OTHER
                   SET RTC-DECREASE        TO TRUE
           END-EVALUATE

      *    CUSTOMER'S DISADVANTAGE: LOWER SAVINGS RATE, HIGHER OD RATE.
           IF ((RTC-CREDIT-TIER OR RTC-BONUS) AND RTC-DECREASE)
           OR ((RTC-ARRANGED OR RTC-UNARRANGED) AND RTC-INCREASE)
               SET WS-DISADVANTAGE         TO TRUE
           END-IF

           COMPUTE WS-EFF-DATE-INT =
                   FUNCTION INTEGER-OF-DATE (RTC-EFFECTIVE-DATE)
           COMPUTE WS-DAYS-NOTICE = WS-EFF-DATE-INT - WS-BUS-DATE-INT
           IF WS-DAYS-NOTICE < ZERO
               MOVE 'EFFECTIVE DATE IS IN THE PAST' TO WS-REJECT-TEXT
           ELSE
               IF WS-DISADVANTAGE
               AND RTC-BASE-RATE-IND NOT = 'Y'
               AND WS-DAYS-NOTICE < WS-NOTICE-DAYS
                   MOVE 'LESS THAN 14 DAYS NOTICE FOR AN ADVERSE CHANGE'
                                           TO WS-REJECT-TEXT
               END-IF
           END-IF
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-REGISTER-CHANGE.
      *================================================================*
           MOVE WS-BUS-DATE                TO RTC-ANNOUNCE-DATE
           SET RTC-ANNOUNCED               TO TRUE
           MOVE ZERO                       TO RTC-NOTICE-COUNT
                                              RTC-APPLIED-DATE
           IF NOT RTC-CREDIT-TIER
               MOVE ZERO                   TO RTC-TIER
           END-IF
           MOVE SPACES                     TO RTC-KEY (21:2)
           WRITE RTC-RATE-CHANGE
           EVALUATE TRUE
               WHEN WS-RATEPEND-OK
                   ADD 1                   TO WS-ANNOUNCED
                   MOVE 'ANNOUNCED'        TO DL-ACTION
                   MOVE ZERO               TO DL-NOTICES
                   IF WS-DISADVANTAGE
                       PERFORM 2300-QUEUE-NOTICES THRU 2300-EXIT
                       MOVE 'ADVERSE - CUSTOMER NOTICES QUEUED'
                                           TO DL-NOTE
                   ELSE
                       MOVE 'FAVOURABLE - NO NOTICE NEEDED'
                                           TO DL-NOTE
                   END-IF
               WHEN WS-RATEPEND-DUPKEY
                   ADD 1                   TO WS-REJECTED
                   MOVE 'REJECTED'         TO DL-ACTION
                   MOVE 'SAME CHANGE ALREADY PENDING' TO DL-NOTE
               WHEN OTHER
                   MOVE '2200-REGISTER-CHANGE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'RATEPEND WRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-RATEPEND-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2300-QUEUE-NOTICES.
      *================================================================*
           IF WS-NT-COUNT >= 50
               DISPLAY 'INRAT010 W001 MORE THAN 50 ADVERSE CHANGES - '
                       'NOTICES FOR ' RTC-PROD-CD ' NOT PRODUCED'
               MOVE 4                      TO RETURN-CODE
               GO TO 2300-EXIT
           END-IF
           ADD 1                           TO WS-NT-COUNT
           SET NT-IDX                      TO WS-NT-COUNT
           MOVE RTC-RATE-CHANGE            TO WS-NT-CHANGE (NT-IDX)
           MOVE ZERO                       TO WS-NT-NOTICES (NT-IDX)
      *    BALANCE RANGE AFFECTED BY A TIER CHANGE.
           IF RTC-CREDIT-TIER
               MOVE PRD-CR-TIER-THRESHOLD (RTC-TIER)
                                       TO WS-NT-TIER-LOW (NT-IDX)
               MOVE 9999999999999.99   TO WS-NT-TIER-HIGH (NT-IDX)
               IF RTC-TIER < 5
               AND PRD-CR-TIER-THRESHOLD (RTC-TIER + 1) > ZERO
               AND PRD-TIER-WHOLE
                   COMPUTE WS-NT-TIER-HIGH (NT-IDX) =
                           PRD-CR-TIER-THRESHOLD (RTC-TIER + 1) - 0.01
               END-IF
           ELSE
               MOVE ZERO               TO WS-NT-TIER-LOW (NT-IDX)
               MOVE 9999999999999.99   TO WS-NT-TIER-HIGH (NT-IDX)
           END-IF
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       3000-PRODUCE-NOTICES.
      *================================================================*
      *    ONE PASS OF THE ACCOUNT MASTER FOR ALL QUEUED CHANGES.
           PERFORM 8100-READ-ACCOUNT
           PERFORM UNTIL WS-END-OF-ACCOUNTS
               IF ACM-OPEN OR ACM-DORMANT
                   PERFORM VARYING NT-IDX FROM 1 BY 1
                           UNTIL NT-IDX > WS-NT-COUNT
                       MOVE WS-NT-CHANGE (NT-IDX) TO RTC-RATE-CHANGE
                       IF ACM-PROD-CD = RTC-PROD-CD
                           PERFORM 3100-CHECK-ACCOUNT THRU 3100-EXIT
                       END-IF
                   END-PERFORM
               END-IF
               PERFORM 8100-READ-ACCOUNT
           END-PERFORM

      *    STORE NOTICE COUNTS ON THE PENDING RECORDS.
           PERFORM VARYING NT-IDX FROM 1 BY 1
                   UNTIL NT-IDX > WS-NT-COUNT
               MOVE WS-NT-CHANGE (NT-IDX)  TO RTC-RATE-CHANGE
               READ RATEPEND-FILE
               IF WS-RATEPEND-OK
                   MOVE WS-NT-NOTICES (NT-IDX) TO RTC-NOTICE-COUNT
                   REWRITE RTC-RATE-CHANGE
               END-IF
               MOVE RTC-PROD-CD            TO DL-PROD-CD
               MOVE RTC-RATE-TYPE          TO DL-TYPE
               MOVE RTC-TIER               TO DL-TIER
               MOVE RTC-EFFECTIVE-DATE     TO DL-EFFECTIVE
               MOVE RTC-OLD-RATE           TO DL-OLD-RATE
               MOVE RTC-NEW-RATE           TO DL-NEW-RATE
               MOVE RTC-DIRECTION          TO DL-DIRECTION
               MOVE 'NOTICES'              TO DL-ACTION
               MOVE WS-NT-NOTICES (NT-IDX) TO DL-NOTICES
               MOVE SPACES                 TO DL-NOTE
               PERFORM 7100-REPORT-DETAIL-RAW
           END-PERFORM
           .
      *================================================================*
       3100-CHECK-ACCOUNT.
      *================================================================*
      *    NEGOTIATED RATES ARE UNAFFECTED - NO NOTICE.
           IF (RTC-CREDIT-TIER OR RTC-BONUS)
               IF ACM-INT-RATE-CR NOT = ZERO
                   GO TO 3100-EXIT
               END-IF
               IF ACM-LEDGER-BAL < WS-NT-TIER-LOW (NT-IDX)
               OR ACM-LEDGER-BAL > WS-NT-TIER-HIGH (NT-IDX)
                   GO TO 3100-EXIT
               END-IF
               IF RTC-BONUS
               AND (ACM-BONUS-END-DATE NOT NUMERIC
                    OR ACM-BONUS-END-DATE <= WS-BUS-DATE)
                   GO TO 3100-EXIT
               END-IF
           ELSE
               IF ACM-INT-RATE-DR NOT = ZERO
                   GO TO 3100-EXIT
               END-IF
      *        ARRANGED RATE AFFECTS ACCOUNTS WITH A LIMIT; UNARRANGED
      *        RATE AFFECTS EVERY CURRENT ACCOUNT.
               IF RTC-ARRANGED AND ACM-OD-LIMIT NOT > ZERO
                   GO TO 3100-EXIT
               END-IF
           END-IF

           MOVE SPACES                     TO WS-NOTICE
           MOVE ACM-SORT-CODE              TO NTC-SORT-CODE
           MOVE ACM-ACCT-NO                TO NTC-ACCT-NO
           MOVE ACM-CIF-NO                 TO NTC-CIF-NO
           MOVE ACM-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           IF WS-CIFMAST-OK
               IF CIF-DECEASED
                   GO TO 3100-EXIT
               END-IF
               STRING CIF-TITLE     DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-FIRST-NM  DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-LAST-NM   DELIMITED BY '  '
                      INTO NTC-NAME
               END-STRING
               MOVE CIF-ADDR-LINE1         TO NTC-ADDR-LINE1
               MOVE CIF-TOWN               TO NTC-TOWN
               MOVE CIF-POST-CD            TO NTC-POST-CD
           END-IF
           MOVE ACM-PROD-CD                TO NTC-PROD-NAME
           MOVE RTC-RATE-TYPE              TO NTC-RATE-TYPE
           MOVE RTC-OLD-RATE               TO NTC-OLD-RATE
           MOVE RTC-NEW-RATE               TO NTC-NEW-RATE
           MOVE RTC-EFFECTIVE-DATE         TO NTC-EFFECTIVE-DATE
           MOVE ACM-LEDGER-BAL             TO NTC-BALANCE
      *    THE CUSTOMER MAY SWITCH OR CLOSE WITHOUT PENALTY (BCOBS 4).
           MOVE 'Y'                        TO NTC-SWITCH-RIGHT-IND
           WRITE NOTICES-RECORD            FROM WS-NOTICE
           IF WS-NOTICES-STATUS NOT = '00'
               MOVE '3100-CHECK-ACCOUNT'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'NOTICES WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-NOTICES-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-NT-NOTICES (NT-IDX)
                                              WS-TOTAL-NOTICES
           .
       3100-EXIT.
           EXIT.
      *================================================================*
       4000-APPLY-DUE-CHANGES.
      *================================================================*
           MOVE LOW-VALUES                 TO RTC-KEY
           START RATEPEND-FILE KEY NOT < RTC-KEY
           IF NOT WS-RATEPEND-OK
               GO TO 4000-EXIT
           END-IF
           PERFORM UNTIL WS-END-OF-PENDING
               READ RATEPEND-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN WS-RATEPEND-EOF
                       SET WS-END-OF-PENDING TO TRUE
                   WHEN NOT WS-RATEPEND-OK
                       MOVE '4000-APPLY-DUE-CHANGES' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'RATEPEND READ FAILED' TO ABN-MESSAGE
                       MOVE WS-RATEPEND-STATUS TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
                   WHEN RTC-ANNOUNCED
                   AND  RTC-EFFECTIVE-DATE <= WS-BUS-DATE
                       PERFORM 4100-APPLY-ONE THRU 4100-EXIT
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-APPLY-ONE.
      *================================================================*
           MOVE RTC-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           IF NOT WS-PRDPARM-OK
               SET RTC-CANCELLED           TO TRUE
               MOVE 'NPRD'                 TO RTC-REASON-CD
               REWRITE RTC-RATE-CHANGE
               MOVE 'CANCELLED'            TO DL-ACTION
               MOVE 'PRODUCT NO LONGER EXISTS' TO DL-NOTE
               PERFORM 7100-REPORT-DETAIL
               GO TO 4100-EXIT
           END-IF

           EVALUATE TRUE
               WHEN RTC-CREDIT-TIER
                   MOVE RTC-NEW-RATE  TO PRD-CR-TIER-RATE (RTC-TIER)
               WHEN RTC-BONUS
                   MOVE RTC-NEW-RATE       TO PRD-CR-BONUS-RATE
               WHEN RTC-ARRANGED
                   MOVE RTC-NEW-RATE       TO PRD-DR-RATE-ARRANGED
               WHEN RTC-UNARRANGED
                   MOVE RTC-NEW-RATE       TO PRD-DR-RATE-UNARRANGED
           END-EVALUATE
           ADD 1                           TO PRD-VERSION-NO
           MOVE WS-BUS-DATE                TO PRD-EFFECTIVE-DATE
                                              PRD-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO PRD-LAST-MAINT-USER
           STRING 'RATE' RTC-EFFECTIVE-DATE
                  DELIMITED BY SIZE
                  INTO PRD-LAST-CHANGE-REF
           END-STRING
           REWRITE PRD-PARAM-RECORD
           IF NOT WS-PRDPARM-OK
               MOVE '4100-APPLY-ONE'       TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'PRDPARM REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-PRDPARM-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

           SET RTC-APPLIED                 TO TRUE
           MOVE WS-BUS-DATE                TO RTC-APPLIED-DATE
           REWRITE RTC-RATE-CHANGE
           ADD 1                           TO WS-APPLIED
           MOVE 'APPLIED'                  TO DL-ACTION
           MOVE RTC-NOTICE-COUNT           TO DL-NOTICES
           MOVE 'PRODUCT RATE UPDATED - EFFECTIVE TONIGHT'
                                           TO DL-NOTE
           PERFORM 7100-REPORT-DETAIL
           .
       4100-EXIT.
           EXIT.
      *================================================================*
       5000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'RATE CHANGES RECEIVED'    TO TL-LABEL
           MOVE WS-INPUT-READ              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ANNOUNCED'                TO TL-LABEL
           MOVE WS-ANNOUNCED               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REJECTED'                 TO TL-LABEL
           MOVE WS-REJECTED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'APPLIED TODAY'            TO TL-LABEL
           MOVE WS-APPLIED                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMER NOTICES'         TO TL-LABEL
           MOVE WS-TOTAL-NOTICES           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS SCANNED'         TO TL-LABEL
           MOVE WS-ACCTS-SCANNED           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE RATEIN-FILE
                 RATEPEND-FILE
                 PRDPARM-FILE
                 ACCTMAST-FILE
                 CIFMAST-FILE
                 NOTICES-FILE
                 REPORT-FILE

           DISPLAY 'INRAT010 - ANNOUNCED ' WS-ANNOUNCED
           DISPLAY 'INRAT010 - APPLIED   ' WS-APPLIED
           DISPLAY 'INRAT010 - NOTICES   ' WS-TOTAL-NOTICES
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
           MOVE RTC-PROD-CD                TO DL-PROD-CD
           MOVE RTC-RATE-TYPE              TO DL-TYPE
           IF RTC-CREDIT-TIER
               MOVE RTC-TIER               TO DL-TIER
           ELSE
               MOVE SPACE                  TO DL-TIER
           END-IF
           IF RTC-EFFECTIVE-DATE NUMERIC
               MOVE RTC-EFFECTIVE-DATE     TO DL-EFFECTIVE
           ELSE
               MOVE ZERO                   TO DL-EFFECTIVE
           END-IF
           IF RTC-OLD-RATE NUMERIC
               MOVE RTC-OLD-RATE           TO DL-OLD-RATE
           ELSE
               MOVE ZERO                   TO DL-OLD-RATE
           END-IF
           IF RTC-NEW-RATE NUMERIC
               MOVE RTC-NEW-RATE           TO DL-NEW-RATE
           ELSE
               MOVE ZERO                   TO DL-NEW-RATE
           END-IF
           MOVE RTC-DIRECTION              TO DL-DIRECTION
           PERFORM 7100-REPORT-DETAIL-RAW
           .
      *================================================================*
       7100-REPORT-DETAIL-RAW.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-INPUT.
      *================================================================*
           READ RATEIN-FILE INTO WS-INPUT-CHANGE
           EVALUATE TRUE
               WHEN WS-RATEIN-OK
                   ADD 1                   TO WS-INPUT-READ
               WHEN WS-RATEIN-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-INPUT'  TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'RATEIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-RATEIN-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8100-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-ACCTS-SCANNED
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8100-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
