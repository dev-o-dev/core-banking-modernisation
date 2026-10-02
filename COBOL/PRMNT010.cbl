       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRMNT010.
       AUTHOR.        R J HALLIDAY.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1991-05-20.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PRMNT010                                            *
      * TITLE    : PRODUCT PARAMETER MAINTENANCE                       *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : PRMNT01 STEP PR020 (ON REQUEST, AND IN MBKEOD)      *
      *----------------------------------------------------------------*
      * INPUT    : CHANGES  - CHANGE CARDS SORTED BY CHANGE REF + SEQ  *
      *                       PRCHGREC                                 *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : PRDPARM  - PRODUCT PARAMETERS (VSAM KSDS, RANDOM)   *
      * OUTPUT   : CARRYFWD - FUTURE-DATED CARDS FOR THE NEXT RUN      *
      *            AUDIT    - BEFORE / AFTER IMAGES (FB 1010)          *
      *            REPORT   - CHANGE REPORT FOR PRODUCT CONTROL        *
      *----------------------------------------------------------------*
      * THE ONLY WAY TO CHANGE A PRODUCT (PIR 2007/066 ASKED FOR AN    *
      * ONLINE SCREEN - NOT FUNDED). EACH CHANGE SET (CARDS SHARING A  *
      * CHANGE REF) IS VALIDATED IN FULL BEFORE ANY CARD IS APPLIED:   *
      *   - REQUESTER AND APPROVER PRESENT AND DIFFERENT (FOUR EYES)   *
      *   - ALL CARDS IN THE SET FOR THE SAME PRODUCT                  *
      *   - FIELD NAME KNOWN, OCCURRENCE IN RANGE, VALUE VALID FOR     *
      *     THE FIELD TYPE (RATE / AMOUNT / NUMBER / CODE / TEXT)      *
      *   - AFTER APPLYING, THE PRODUCT IS CROSS-CHECKED:              *
      *       CREDIT TIERS IN ASCENDING THRESHOLD ORDER                *
      *       TIER 1 THRESHOLD ZERO                                    *
      *       UNARRANGED RATE >= ARRANGED RATE                         *
      *       NOTIFICATION DAYS >= 14                                  *
      *       GL CODES PRESENT IF THE PRODUCT CHARGES / PAYS INTEREST  *
      * A SET THAT FAILS ANY CHECK IS REJECTED WHOLE AND REPORTED.     *
      *                                                                *
      * RATE CHANGES THAT AFFECT EXISTING CUSTOMERS NEED NOTICE (BCOBS *
      * 4) - THIS PROGRAM DOES NOT CHECK THAT. PRODUCT CONTROL MUST    *
      * USE INRAT010 (WHICH PRODUCES THE CUSTOMER NOTICES) FOR         *
      * REDUCTIONS TO SAVINGS RATES. PIR 2009/301.                     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1991-05-20 RJH  ORIGINAL VERSION                               *
      * 1998-11-30 MLW  Y2K                                            *
      * 2004-08-09 SAT  CHARGES FIELDS (OFT)                           *
      * 2007-02-12 JNB  CHANGE SETS, FOUR EYES, CROSS-CHECKS           *
      * 2008-09-15 JNB  DORMANCY FIELDS                                *
      * 2009-10-01 AOS  NOTIFICATION DAYS CROSS-CHECK (PSR 2009)       *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHANGES-FILE     ASSIGN TO CHANGES
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CHANGES-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT CARRYFWD-FILE    ASSIGN TO CARRYFWD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CARRYFWD-STATUS.
           SELECT AUDIT-FILE       ASSIGN TO AUDIT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-AUDIT-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CHANGES-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHANGES-RECORD                  PIC X(120).
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  CARRYFWD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CARRYFWD-RECORD                 PIC X(120).
       FD  AUDIT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  AUDIT-RECORD.
           05  AUD-CHANGE-REF              PIC X(12).
           05  AUD-IMAGE-TYPE              PIC X(01).
           05  AUD-BUS-DATE                PIC 9(08).
           05  AUD-PRODUCT-IMAGE           PIC X(1000).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PRMNT010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-CHANGES-STATUS           PIC X(02).
               88  WS-CHANGES-OK               VALUE '00'.
               88  WS-CHANGES-EOF              VALUE '10'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-NOTFND           VALUE '23'.
           05  WS-CARRYFWD-STATUS          PIC X(02).
           05  WS-AUDIT-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CHANGES           VALUE 'Y'.
           05  WS-SET-OK-IND               PIC X(01).
               88  WS-SET-OK                   VALUE 'Y'.
           05  WS-NEW-PRODUCT-IND          PIC X(01).
               88  WS-NEW-PRODUCT              VALUE 'Y'.
           05  WS-FUTURE-IND               PIC X(01).
               88  WS-SET-IS-FUTURE            VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CURR-REF                 PIC X(12).
           05  WS-CURR-PROD                PIC X(10).
           05  WS-REJECT-TEXT              PIC X(60).
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-OCC                      PIC S9(04) COMP.
           05  WS-VALUE                    PIC X(30).
           05  WS-NUM-VALUE                PIC S9(11)V9(04).
           05  WS-NUM-OK-IND               PIC X(01).
               88  WS-NUM-OK                   VALUE 'Y'.
           05  WS-PREV-THRESHOLD           PIC S9(13)V99 COMP-3.
           05  WS-DATE-DISPLAY             PIC X(10).
           05  WS-OLD-DISPLAY              PIC X(30).
      *
      *---- THE CHANGE SET BEING VALIDATED (MAX 60 CARDS) -------------*
       01  WS-SET-TABLE.
           05  WS-SET-COUNT                PIC S9(04) COMP VALUE ZERO.
           05  WS-SET-CARD                 PIC X(120)
                                           OCCURS 60 TIMES.
      *
      *---- WORKING COPIES OF THE PRODUCT -----------------------------*
       01  WS-LOOKAHEAD-CARD               PIC X(120).
       01  WS-PRODUCT-BEFORE               PIC X(1000).
       01  WS-PRODUCT-AFTER                PIC X(1000).
      *
      *---- FIELD TYPES -----------------------------------------------*
      *    R RATE (S9(3)V9(4))  A AMOUNT  N NUMBER  C CODE (1 CHAR,
      *    VALID LIST IN WS-FT-CODES)  T TEXT  D DATE (CCYYMMDD)
       01  WS-FIELD-VALUES.
           05  FILLER PIC X(40) VALUE
               'DESCRIPTION             T0              '.
           05  FILLER PIC X(40) VALUE
               'SHORT-NAME              T0              '.
           05  FILLER PIC X(40) VALUE
               'STATUS                  C0ANW           '.
           05  FILLER PIC X(40) VALUE
               'CUST-TYPE               C0PBA           '.
           05  FILLER PIC X(40) VALUE
               'MIN-AGE                 N0              '.
           05  FILLER PIC X(40) VALUE
               'MAX-AGE                 N0              '.
           05  FILLER PIC X(40) VALUE
               'MAX-PER-CUST            N0              '.
           05  FILLER PIC X(40) VALUE
               'MIN-OPEN-DEPOSIT        A0              '.
           05  FILLER PIC X(40) VALUE
               'MAX-BALANCE             A0              '.
           05  FILLER PIC X(40) VALUE
               'KYC-REQUIRED            C0YN            '.
           05  FILLER PIC X(40) VALUE
               'OPEN-BRANCH-IND         C0YN            '.
           05  FILLER PIC X(40) VALUE
               'OPEN-IBANK-IND          C0YN            '.
           05  FILLER PIC X(40) VALUE
               'OPEN-TELE-IND           C0YN            '.
           05  FILLER PIC X(40) VALUE
               'CR-INT-IND              C0YN            '.
           05  FILLER PIC X(40) VALUE
               'TIER-METHOD             C0WB            '.
           05  FILLER PIC X(40) VALUE
               'CR-TIER-THRESHOLD       A5              '.
           05  FILLER PIC X(40) VALUE
               'CR-TIER-RATE            R5              '.
           05  FILLER PIC X(40) VALUE
               'CR-BONUS-RATE           R0              '.
           05  FILLER PIC X(40) VALUE
               'CR-BONUS-MONTHS         N0              '.
           05  FILLER PIC X(40) VALUE
               'CR-MIN-BAL              A0              '.
           05  FILLER PIC X(40) VALUE
               'CR-DAY-BASIS            N0              '.
           05  FILLER PIC X(40) VALUE
               'CR-CAP-FREQ             C0MQHAT         '.
           05  FILLER PIC X(40) VALUE
               'CR-CAP-DAY              N0              '.
           05  FILLER PIC X(40) VALUE
               'CR-CAP-MONTH            N0              '.
           05  FILLER PIC X(40) VALUE
               'TAX-DEDUCT-IND          C0YN            '.
           05  FILLER PIC X(40) VALUE
               'DR-INT-IND              C0YN            '.
           05  FILLER PIC X(40) VALUE
               'DR-RATE-ARRANGED        R0              '.
           05  FILLER PIC X(40) VALUE
               'DR-RATE-UNARRANGED      R0              '.
           05  FILLER PIC X(40) VALUE
               'DR-CAP-FREQ             C0MQ            '.
           05  FILLER PIC X(40) VALUE
               'DR-CAP-DAY              N0              '.
           05  FILLER PIC X(40) VALUE
               'DR-INT-FREE-AMT         A0              '.
           05  FILLER PIC X(40) VALUE
               'DR-MIN-CHARGE           A0              '.
           05  FILLER PIC X(40) VALUE
               'OD-MAX-LIMIT            A0              '.
           05  FILLER PIC X(40) VALUE
               'CHG-IND                 C0YN            '.
           05  FILLER PIC X(40) VALUE
               'CHG-CYCLE-DAY           N0              '.
           05  FILLER PIC X(40) VALUE
               'CHG-NOTIFY-DAYS         N0              '.
           05  FILLER PIC X(40) VALUE
               'MONTHLY-FEE             A0              '.
           05  FILLER PIC X(40) VALUE
               'FREE-BANKING-BAL        A0              '.
           05  FILLER PIC X(40) VALUE
               'UNARR-OD-DAILY-FEE      A0              '.
           05  FILLER PIC X(40) VALUE
               'UNARR-OD-MAX-DAYS       N0              '.
           05  FILLER PIC X(40) VALUE
               'UNARR-OD-BUFFER         A0              '.
           05  FILLER PIC X(40) VALUE
               'UNPAID-ITEM-FEE         A0              '.
           05  FILLER PIC X(40) VALUE
               'PAID-ITEM-FEE           A0              '.
           05  FILLER PIC X(40) VALUE
               'MONTHLY-CHG-CAP         A0              '.
           05  FILLER PIC X(40) VALUE
               'DORMANT-MONTHS          N0              '.
           05  FILLER PIC X(40) VALUE
               'RECLAIM-YEARS           N0              '.
           05  FILLER PIC X(40) VALUE
               'DEFAULT-STMT-CYCLE      C0MQA           '.
           05  FILLER PIC X(40) VALUE
               'DEFAULT-STMT-DAY        N0              '.
           05  FILLER PIC X(40) VALUE
               'GL-BALANCE-CR           T0              '.
           05  FILLER PIC X(40) VALUE
               'GL-BALANCE-DR           T0              '.
           05  FILLER PIC X(40) VALUE
               'GL-INT-EXPENSE          T0              '.
           05  FILLER PIC X(40) VALUE
               'GL-INT-INCOME           T0              '.
           05  FILLER PIC X(40) VALUE
               'GL-FEE-INCOME           T0              '.
           05  FILLER PIC X(40) VALUE
               'COST-CENTRE             T0              '.
       01  WS-FIELD-TABLE REDEFINES WS-FIELD-VALUES.
           05  WS-FT-ENTRY                 OCCURS 54 TIMES
                                           INDEXED BY FT-IDX.
               10  WS-FT-NAME              PIC X(24).
               10  WS-FT-TYPE              PIC X(01).
               10  WS-FT-MAX-OCC           PIC 9(01).
               10  WS-FT-CODES             PIC X(14).
       01  WS-FIELD-NO                     PIC S9(04) COMP.
      *
       01  WS-COUNTERS.
           05  WS-CARDS-READ               PIC S9(07) COMP-3 VALUE 0.
           05  WS-SETS-READ                PIC S9(07) COMP-3 VALUE 0.
           05  WS-SETS-APPLIED             PIC S9(07) COMP-3 VALUE 0.
           05  WS-SETS-REJECTED            PIC S9(07) COMP-3 VALUE 0.
           05  WS-SETS-CARRIED             PIC S9(07) COMP-3 VALUE 0.
           05  WS-PRODUCTS-ADDED           PIC S9(07) COMP-3 VALUE 0.
           05  WS-FIELDS-CHANGED           PIC S9(07) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'PRMNT010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - PRODUCT PARAMETER CHANGES'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(14) VALUE 'CHANGE REF'.
           05  FILLER            PIC X(12) VALUE 'PRODUCT'.
           05  FILLER            PIC X(03) VALUE 'A'.
           05  FILLER            PIC X(27) VALUE 'FIELD'.
           05  FILLER            PIC X(32) VALUE 'OLD VALUE'.
           05  FILLER            PIC X(32) VALUE 'NEW VALUE'.
           05  FILLER            PIC X(12) VALUE 'RESULT'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-CHANGE-REF               PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-FIELD                    PIC X(24).
           05  DL-OCC                      PIC X(03).
           05  DL-OLD                      PIC X(30).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NEW                      PIC X(30).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RESULT                   PIC X(12).
       01  WS-REJECT-LINE.
           05  RL-CC                       PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(14) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE '** REJECT:'.
           05  RL-TEXT                     PIC X(60).
           05  FILLER                      PIC X(46) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
       01  WS-RATE-EDIT                    PIC -ZZ9.9999.
       01  WS-AMOUNT-EDIT                  PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-NUMBER-EDIT                  PIC ZZZZ9.
      *
           COPY PRCHGREC.
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
           PERFORM 2000-PROCESS-SET THRU 2000-EXIT
               UNTIL WS-END-OF-CHANGES
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CHANGES-FILE
                       DATECARD-FILE
                I-O    PRDPARM-FILE
                OUTPUT CARRYFWD-FILE
                       AUDIT-FILE
                       REPORT-FILE
           IF NOT WS-CHANGES-OK
           OR NOT WS-PRDPARM-OK
           OR WS-CARRYFWD-STATUS NOT = '00'
           OR WS-AUDIT-STATUS NOT = '00'
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
           PERFORM 7000-REPORT-HEADINGS
           PERFORM 8000-READ-CARD
           .
      *================================================================*
       2000-PROCESS-SET.
      *================================================================*
      *    COLLECT ALL CARDS FOR THE CHANGE REF.
           MOVE PCH-CHANGE-REF             TO WS-CURR-REF
           MOVE PCH-PROD-CD                TO WS-CURR-PROD
           MOVE ZERO                       TO WS-SET-COUNT
           MOVE 'Y'                        TO WS-SET-OK-IND
           MOVE 'N'                        TO WS-NEW-PRODUCT-IND
                                              WS-FUTURE-IND
           MOVE SPACES                     TO WS-REJECT-TEXT
           ADD 1                           TO WS-SETS-READ

           PERFORM UNTIL WS-END-OF-CHANGES
                      OR PCH-CHANGE-REF NOT = WS-CURR-REF
               IF WS-SET-COUNT >= 60
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'MORE THAN 60 CARDS IN ONE CHANGE SET'
                                           TO WS-REJECT-TEXT
               ELSE
                   ADD 1                   TO WS-SET-COUNT
                   MOVE PCH-CHANGE-CARD
                                    TO WS-SET-CARD (WS-SET-COUNT)
               END-IF
               PERFORM 8000-READ-CARD
           END-PERFORM
      *    THE CARD JUST READ BELONGS TO THE NEXT SET - KEEP IT SAFE
      *    WHILE THIS SET IS PROCESSED THROUGH PCH-CHANGE-CARD.
           MOVE PCH-CHANGE-CARD            TO WS-LOOKAHEAD-CARD

           IF WS-SET-OK
               PERFORM 2100-CHECK-SET-HEADER
           END-IF
           IF WS-SET-OK AND WS-SET-IS-FUTURE
               PERFORM 2200-CARRY-FORWARD
               MOVE WS-LOOKAHEAD-CARD      TO PCH-CHANGE-CARD
               GO TO 2000-EXIT
           END-IF
           IF WS-SET-OK
               PERFORM 2300-LOAD-PRODUCT
           END-IF
           IF WS-SET-OK
               PERFORM 2400-APPLY-CARDS
           END-IF
           IF WS-SET-OK
               PERFORM 2600-CROSS-CHECK
           END-IF

           IF WS-SET-OK
               PERFORM 2700-WRITE-PRODUCT
               ADD 1                       TO WS-SETS-APPLIED
           ELSE
               ADD 1                       TO WS-SETS-REJECTED
               PERFORM 7200-REPORT-REJECT
           END-IF
           MOVE WS-LOOKAHEAD-CARD          TO PCH-CHANGE-CARD
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-CHECK-SET-HEADER.
      *================================================================*
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-SET-COUNT OR NOT WS-SET-OK
               MOVE WS-SET-CARD (WS-SUB)   TO PCH-CHANGE-CARD
               EVALUATE TRUE
                   WHEN PCH-PROD-CD NOT = WS-CURR-PROD
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'CARDS IN THE SET ARE FOR SEVERAL PRODUCTS'
                                           TO WS-REJECT-TEXT
                   WHEN PCH-REQUESTED-BY = SPACES
                   WHEN PCH-APPROVED-BY = SPACES
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'REQUESTER AND APPROVER ARE BOTH REQUIRED'
                                           TO WS-REJECT-TEXT
                   WHEN PCH-REQUESTED-BY = PCH-APPROVED-BY
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'APPROVER MUST NOT BE THE REQUESTER'
                                           TO WS-REJECT-TEXT
                   WHEN PCH-EFFECTIVE-DATE NOT NUMERIC
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'EFFECTIVE DATE NOT NUMERIC'
                                           TO WS-REJECT-TEXT
                   WHEN PCH-EFFECTIVE-DATE > WS-BUS-DATE
                       SET WS-SET-IS-FUTURE TO TRUE
                   WHEN PCH-ADD
                       SET WS-NEW-PRODUCT  TO TRUE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
      *================================================================*
       2200-CARRY-FORWARD.
      *================================================================*
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-SET-COUNT
               WRITE CARRYFWD-RECORD       FROM WS-SET-CARD (WS-SUB)
           END-PERFORM
           ADD 1                           TO WS-SETS-CARRIED
           MOVE WS-SET-CARD (1)            TO PCH-CHANGE-CARD
           MOVE PCH-CHANGE-REF             TO DL-CHANGE-REF
           MOVE PCH-PROD-CD                TO DL-PROD-CD
           MOVE SPACES                     TO DL-ACTION DL-FIELD DL-OCC
                                              DL-OLD
           STRING 'EFFECTIVE ' PCH-EFFECTIVE-DATE
                  DELIMITED BY SIZE
                  INTO DL-NEW
           END-STRING
           MOVE 'CARRIED FWD'              TO DL-RESULT
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2300-LOAD-PRODUCT.
      *================================================================*
           MOVE WS-CURR-PROD               TO PRD-PROD-CD
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN WS-PRDPARM-OK AND WS-NEW-PRODUCT
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'ADD REQUESTED BUT PRODUCT ALREADY EXISTS'
                                           TO WS-REJECT-TEXT
               WHEN WS-PRDPARM-OK
                   MOVE PRD-PARAM-RECORD   TO WS-PRODUCT-BEFORE
               WHEN WS-PRDPARM-NOTFND AND WS-NEW-PRODUCT
                   PERFORM 2310-INITIALISE-PRODUCT
                   MOVE SPACES             TO WS-PRODUCT-BEFORE
               WHEN WS-PRDPARM-NOTFND
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'PRODUCT NOT FOUND (USE ACTION A TO ADD)'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   MOVE '2300-LOAD-PRODUCT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PRDPARM READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-PRDPARM-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2310-INITIALISE-PRODUCT.
      *================================================================*
      *    A NEW PRODUCT STARTS WITH SAFE DEFAULTS - NOTHING IS PAID OR
      *    CHARGED AND NOBODY CAN OPEN IT UNTIL CARDS SAY OTHERWISE.
           MOVE SPACES                     TO PRD-PARAM-RECORD
           MOVE WS-CURR-PROD               TO PRD-PROD-CD
           MOVE 'CURR'                     TO PRD-ACCT-TYPE-CD
           MOVE 'GBP'                      TO PRD-CCY-CD
           SET PRD-ACTIVE                  TO TRUE
           SET PRD-ANY-CUST-TYPE           TO TRUE
           MOVE WS-BUS-DATE                TO PRD-LAUNCH-DATE
           MOVE ZERO                       TO PRD-WITHDRAW-DATE
                                              PRD-MIN-AGE PRD-MAX-AGE
                                              PRD-MAX-PER-CUST
                                              PRD-MIN-OPEN-DEPOSIT
                                              PRD-MAX-BALANCE
                                              PRD-ACCT-NO-LOW
                                              PRD-ACCT-NO-HIGH
                                              PRD-TERM-MONTHS
                                              PRD-NOTICE-DAYS
                                              PRD-ISA-SUBS-LIMIT
           MOVE 'Y'                        TO PRD-KYC-REQUIRED
           MOVE 'N'                        TO PRD-OPEN-BRANCH-IND
                                              PRD-OPEN-IBANK-IND
                                              PRD-OPEN-TELE-IND
                                              PRD-CR-INT-IND
                                              PRD-DR-INT-IND
                                              PRD-CHG-IND
                                              PRD-TAX-DEDUCT-IND
                                              PRD-DORMANT-LETTER-IND
                                              PRD-ESTMT-ALLOWED
           MOVE 'W'                        TO PRD-TIER-METHOD
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
               MOVE ZERO TO PRD-CR-TIER-THRESHOLD (WS-SUB)
                            PRD-CR-TIER-RATE (WS-SUB)
           END-PERFORM
           MOVE ZERO                       TO PRD-CR-BONUS-RATE
                                              PRD-CR-BONUS-MONTHS
                                              PRD-CR-MIN-BAL
                                              PRD-CR-CAP-DAY
                                              PRD-CR-CAP-MONTH
                                              PRD-CR-PENALTY-DAYS
                                              PRD-DR-RATE-ARRANGED
                                              PRD-DR-RATE-UNARRANGED
                                              PRD-DR-CAP-DAY
                                              PRD-DR-INT-FREE-AMT
                                              PRD-DR-MIN-CHARGE
                                              PRD-OD-MAX-LIMIT
                                              PRD-CHG-CYCLE-DAY
                                              PRD-MONTHLY-FEE
                                              PRD-FREE-BANKING-BAL
                                              PRD-ARR-OD-MONTHLY-FEE
                                              PRD-UNARR-OD-DAILY-FEE
                                              PRD-UNARR-OD-MAX-DAYS
                                              PRD-UNARR-OD-BUFFER
                                              PRD-UNPAID-ITEM-FEE
                                              PRD-PAID-ITEM-FEE
                                              PRD-MONTHLY-CHG-CAP
                                              PRD-SWIFT-FEE
                                              PRD-CHAPS-FEE
                                              PRD-STOPPED-SO-FEE
                                              PRD-RETURNED-DD-FEE
                                              PRD-DORMANT-MIN-BAL
                                              PRD-VERSION-NO
           MOVE 365                        TO PRD-CR-DAY-BASIS
                                              PRD-DR-DAY-BASIS
           MOVE 'A'                        TO PRD-CR-CAP-FREQ
           MOVE 'Q'                        TO PRD-DR-CAP-FREQ
           MOVE 14                         TO PRD-CHG-NOTIFY-DAYS
           MOVE 36                         TO PRD-DORMANT-MONTHS
           MOVE 15                         TO PRD-RECLAIM-YEARS
           MOVE 'M'                        TO PRD-DEFAULT-STMT-CYCLE
           MOVE ZERO                       TO PRD-DEFAULT-STMT-DAY
           MOVE 'STMTSTD1'                 TO PRD-STMT-TEMPLATE-ID
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 6
               MOVE SPACES TO PRD-TFEE-TXN-CODE (WS-SUB)
               MOVE ZERO   TO PRD-TFEE-FREE-COUNT (WS-SUB)
                              PRD-TFEE-AMOUNT (WS-SUB)
           END-PERFORM
           ADD 1                           TO WS-PRODUCTS-ADDED
           .
      *================================================================*
       2400-APPLY-CARDS.
      *================================================================*
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-SET-COUNT OR NOT WS-SET-OK
               MOVE WS-SET-CARD (WS-SUB)   TO PCH-CHANGE-CARD
               MOVE SPACES                 TO WS-OLD-DISPLAY
               EVALUATE TRUE
                   WHEN PCH-ADD
                       MOVE PCH-NEW-VALUE  TO PRD-SHORT-NAME
                                              PRD-DESCRIPTION
                       MOVE 'NEW PRODUCT'  TO DL-RESULT
                   WHEN PCH-WITHDRAW
                       MOVE PRD-STATUS     TO WS-OLD-DISPLAY
                       SET PRD-WITHDRAWN   TO TRUE
                       MOVE WS-BUS-DATE    TO PRD-WITHDRAW-DATE
                       MOVE 'WITHDRAWN'    TO DL-RESULT
                   WHEN PCH-CLOSE-TO-NEW
                       MOVE PRD-STATUS     TO WS-OLD-DISPLAY
                       SET PRD-CLOSED-TO-NEW TO TRUE
                       MOVE 'CLOSED TO NEW' TO DL-RESULT
                   WHEN PCH-UPDATE
                       PERFORM 2500-APPLY-FIELD THRU 2500-EXIT
                       MOVE 'APPLIED'      TO DL-RESULT
                   WHEN OTHER
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'ACTION MUST BE A, U, W OR C'
                                           TO WS-REJECT-TEXT
               END-EVALUATE
               IF WS-SET-OK
                   MOVE PCH-CHANGE-REF     TO DL-CHANGE-REF
                   MOVE PCH-PROD-CD        TO DL-PROD-CD
                   MOVE PCH-ACTION         TO DL-ACTION
                   MOVE PCH-FIELD-NAME     TO DL-FIELD
                   IF PCH-OCCURRENCE NUMERIC AND PCH-OCCURRENCE > ZERO
                       STRING '(' PCH-OCCURRENCE ')'
                              DELIMITED BY SIZE
                              INTO DL-OCC
                       END-STRING
                   ELSE
                       MOVE SPACES         TO DL-OCC
                   END-IF
                   MOVE WS-OLD-DISPLAY     TO DL-OLD
                   MOVE PCH-NEW-VALUE      TO DL-NEW
                   PERFORM 7100-REPORT-DETAIL
                   ADD 1                   TO WS-FIELDS-CHANGED
               END-IF
           END-PERFORM
           .
      *================================================================*
       2500-APPLY-FIELD.
      *================================================================*
           SET FT-IDX                      TO 1
           SEARCH WS-FT-ENTRY
               AT END
                   MOVE 'N'                TO WS-SET-OK-IND
                   STRING 'UNKNOWN FIELD NAME ' PCH-FIELD-NAME
                          DELIMITED BY SIZE
                          INTO WS-REJECT-TEXT
                   END-STRING
               WHEN WS-FT-NAME (FT-IDX) = PCH-FIELD-NAME
                   SET WS-FIELD-NO         TO FT-IDX
           END-SEARCH
           IF NOT WS-SET-OK
               GO TO 2500-EXIT
           END-IF

      *---- OCCURRENCE ------------------------------------------------*
           IF WS-FT-MAX-OCC (FT-IDX) > ZERO
               IF PCH-OCCURRENCE NOT NUMERIC
               OR PCH-OCCURRENCE = ZERO
               OR PCH-OCCURRENCE > WS-FT-MAX-OCC (FT-IDX)
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'OCCURRENCE NUMBER OUT OF RANGE'
                                           TO WS-REJECT-TEXT
                   GO TO 2500-EXIT
               END-IF
               MOVE PCH-OCCURRENCE         TO WS-OCC
           ELSE
               MOVE 1                      TO WS-OCC
           END-IF

      *---- VALUE BY TYPE ---------------------------------------------*
           MOVE PCH-NEW-VALUE              TO WS-VALUE
           EVALUATE WS-FT-TYPE (FT-IDX)
               WHEN 'R'
               WHEN 'A'
               WHEN 'N'
                   PERFORM 2550-PARSE-NUMBER
                   IF NOT WS-NUM-OK
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'VALUE IS NOT A VALID NUMBER'
                                           TO WS-REJECT-TEXT
                       GO TO 2500-EXIT
                   END-IF
                   IF WS-FT-TYPE (FT-IDX) = 'R'
                   AND (WS-NUM-VALUE < ZERO OR WS-NUM-VALUE > 99.9999)
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'RATE MUST BE 0.0000 TO 99.9999'
                                           TO WS-REJECT-TEXT
                       GO TO 2500-EXIT
                   END-IF
                   IF WS-NUM-VALUE < ZERO
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'NEGATIVE VALUES ARE NOT ALLOWED'
                                           TO WS-REJECT-TEXT
                       GO TO 2500-EXIT
                   END-IF
               WHEN 'C'
                   IF WS-VALUE (2:29) NOT = SPACES
                   OR WS-VALUE (1:1) = SPACE
                       MOVE 'N'            TO WS-SET-OK-IND
                       MOVE 'CODE VALUE MUST BE ONE CHARACTER'
                                           TO WS-REJECT-TEXT
                       GO TO 2500-EXIT
                   END-IF
                   MOVE ZERO               TO WS-OCC
                   INSPECT WS-FT-CODES (FT-IDX) TALLYING WS-OCC
                           FOR ALL WS-VALUE (1:1)
                   IF WS-OCC = ZERO
                       MOVE 'N'            TO WS-SET-OK-IND
                       STRING 'CODE MUST BE ONE OF '
                              WS-FT-CODES (FT-IDX)
                              DELIMITED BY SIZE
                              INTO WS-REJECT-TEXT
                       END-STRING
                       GO TO 2500-EXIT
                   END-IF
                   MOVE 1                  TO WS-OCC
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           PERFORM 2560-STORE-FIELD
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       2550-PARSE-NUMBER.
      *================================================================*
           MOVE 'Y'                        TO WS-NUM-OK-IND
           IF FUNCTION TRIM (WS-VALUE) = SPACES
               MOVE 'N'                    TO WS-NUM-OK-IND
           ELSE
               IF FUNCTION TEST-NUMVAL (WS-VALUE) NOT = ZERO
                   MOVE 'N'                TO WS-NUM-OK-IND
               ELSE
                   COMPUTE WS-NUM-VALUE = FUNCTION NUMVAL (WS-VALUE)
               END-IF
           END-IF
           .
      *================================================================*
       2560-STORE-FIELD.
      *================================================================*
      *    ONE WHEN PER MAINTAINABLE FIELD. THE OLD VALUE IS SHOWN ON
      *    THE REPORT.
           EVALUATE WS-FT-NAME (FT-IDX)
               WHEN 'DESCRIPTION'
                   MOVE PRD-DESCRIPTION    TO WS-OLD-DISPLAY
                   MOVE WS-VALUE           TO PRD-DESCRIPTION
               WHEN 'SHORT-NAME'
                   MOVE PRD-SHORT-NAME     TO WS-OLD-DISPLAY
                   MOVE WS-VALUE           TO PRD-SHORT-NAME
               WHEN 'STATUS'
                   MOVE PRD-STATUS         TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-STATUS
               WHEN 'CUST-TYPE'
                   MOVE PRD-CUST-TYPE      TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-CUST-TYPE
               WHEN 'MIN-AGE'
                   MOVE PRD-MIN-AGE        TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-MIN-AGE
               WHEN 'MAX-AGE'
                   MOVE PRD-MAX-AGE        TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-MAX-AGE
               WHEN 'MAX-PER-CUST'
                   MOVE PRD-MAX-PER-CUST   TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-MAX-PER-CUST
               WHEN 'MIN-OPEN-DEPOSIT'
                   MOVE PRD-MIN-OPEN-DEPOSIT TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-MIN-OPEN-DEPOSIT
               WHEN 'MAX-BALANCE'
                   MOVE PRD-MAX-BALANCE    TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-MAX-BALANCE
               WHEN 'KYC-REQUIRED'
                   MOVE PRD-KYC-REQUIRED   TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-KYC-REQUIRED
               WHEN 'OPEN-BRANCH-IND'
                   MOVE PRD-OPEN-BRANCH-IND TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-OPEN-BRANCH-IND
               WHEN 'OPEN-IBANK-IND'
                   MOVE PRD-OPEN-IBANK-IND TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-OPEN-IBANK-IND
               WHEN 'OPEN-TELE-IND'
                   MOVE PRD-OPEN-TELE-IND  TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-OPEN-TELE-IND
               WHEN 'CR-INT-IND'
                   MOVE PRD-CR-INT-IND     TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-CR-INT-IND
               WHEN 'TIER-METHOD'
                   MOVE PRD-TIER-METHOD    TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-TIER-METHOD
               WHEN 'CR-TIER-THRESHOLD'
                   MOVE PRD-CR-TIER-THRESHOLD (WS-OCC) TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE  TO PRD-CR-TIER-THRESHOLD (WS-OCC)
               WHEN 'CR-TIER-RATE'
                   MOVE PRD-CR-TIER-RATE (WS-OCC) TO WS-RATE-EDIT
                   MOVE WS-RATE-EDIT       TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CR-TIER-RATE (WS-OCC)
               WHEN 'CR-BONUS-RATE'
                   MOVE PRD-CR-BONUS-RATE  TO WS-RATE-EDIT
                   MOVE WS-RATE-EDIT       TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CR-BONUS-RATE
               WHEN 'CR-BONUS-MONTHS'
                   MOVE PRD-CR-BONUS-MONTHS TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CR-BONUS-MONTHS
               WHEN 'CR-MIN-BAL'
                   MOVE PRD-CR-MIN-BAL     TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CR-MIN-BAL
               WHEN 'CR-DAY-BASIS'
                   MOVE PRD-CR-DAY-BASIS   TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CR-DAY-BASIS
               WHEN 'CR-CAP-FREQ'
                   MOVE PRD-CR-CAP-FREQ    TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-CR-CAP-FREQ
               WHEN 'CR-CAP-DAY'
                   MOVE PRD-CR-CAP-DAY     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CR-CAP-DAY
               WHEN 'CR-CAP-MONTH'
                   MOVE PRD-CR-CAP-MONTH   TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CR-CAP-MONTH
               WHEN 'TAX-DEDUCT-IND'
                   MOVE PRD-TAX-DEDUCT-IND TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-TAX-DEDUCT-IND
               WHEN 'DR-INT-IND'
                   MOVE PRD-DR-INT-IND     TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-DR-INT-IND
               WHEN 'DR-RATE-ARRANGED'
                   MOVE PRD-DR-RATE-ARRANGED TO WS-RATE-EDIT
                   MOVE WS-RATE-EDIT       TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-DR-RATE-ARRANGED
               WHEN 'DR-RATE-UNARRANGED'
                   MOVE PRD-DR-RATE-UNARRANGED TO WS-RATE-EDIT
                   MOVE WS-RATE-EDIT       TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-DR-RATE-UNARRANGED
               WHEN 'DR-CAP-FREQ'
                   MOVE PRD-DR-CAP-FREQ    TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-DR-CAP-FREQ
               WHEN 'DR-CAP-DAY'
                   MOVE PRD-DR-CAP-DAY     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-DR-CAP-DAY
               WHEN 'DR-INT-FREE-AMT'
                   MOVE PRD-DR-INT-FREE-AMT TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-DR-INT-FREE-AMT
               WHEN 'DR-MIN-CHARGE'
                   MOVE PRD-DR-MIN-CHARGE  TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-DR-MIN-CHARGE
               WHEN 'OD-MAX-LIMIT'
                   MOVE PRD-OD-MAX-LIMIT   TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-OD-MAX-LIMIT
               WHEN 'CHG-IND'
                   MOVE PRD-CHG-IND        TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-CHG-IND
               WHEN 'CHG-CYCLE-DAY'
                   MOVE PRD-CHG-CYCLE-DAY  TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CHG-CYCLE-DAY
               WHEN 'CHG-NOTIFY-DAYS'
                   MOVE PRD-CHG-NOTIFY-DAYS TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-CHG-NOTIFY-DAYS
               WHEN 'MONTHLY-FEE'
                   MOVE PRD-MONTHLY-FEE    TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-MONTHLY-FEE
               WHEN 'FREE-BANKING-BAL'
                   MOVE PRD-FREE-BANKING-BAL TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-FREE-BANKING-BAL
               WHEN 'UNARR-OD-DAILY-FEE'
                   MOVE PRD-UNARR-OD-DAILY-FEE TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-UNARR-OD-DAILY-FEE
               WHEN 'UNARR-OD-MAX-DAYS'
                   MOVE PRD-UNARR-OD-MAX-DAYS TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-UNARR-OD-MAX-DAYS
               WHEN 'UNARR-OD-BUFFER'
                   MOVE PRD-UNARR-OD-BUFFER TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-UNARR-OD-BUFFER
               WHEN 'UNPAID-ITEM-FEE'
                   MOVE PRD-UNPAID-ITEM-FEE TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-UNPAID-ITEM-FEE
               WHEN 'PAID-ITEM-FEE'
                   MOVE PRD-PAID-ITEM-FEE  TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-PAID-ITEM-FEE
               WHEN 'MONTHLY-CHG-CAP'
                   MOVE PRD-MONTHLY-CHG-CAP TO WS-AMOUNT-EDIT
                   MOVE WS-AMOUNT-EDIT     TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-MONTHLY-CHG-CAP
               WHEN 'DORMANT-MONTHS'
                   MOVE PRD-DORMANT-MONTHS TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-DORMANT-MONTHS
               WHEN 'RECLAIM-YEARS'
                   MOVE PRD-RECLAIM-YEARS  TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-RECLAIM-YEARS
               WHEN 'DEFAULT-STMT-CYCLE'
                   MOVE PRD-DEFAULT-STMT-CYCLE TO WS-OLD-DISPLAY
                   MOVE WS-VALUE (1:1)     TO PRD-DEFAULT-STMT-CYCLE
               WHEN 'DEFAULT-STMT-DAY'
                   MOVE PRD-DEFAULT-STMT-DAY TO WS-OLD-DISPLAY
                   MOVE WS-NUM-VALUE       TO PRD-DEFAULT-STMT-DAY
               WHEN 'GL-BALANCE-CR'
                   MOVE PRD-GL-BALANCE-CR  TO WS-OLD-DISPLAY
                   MOVE WS-VALUE           TO PRD-GL-BALANCE-CR
               WHEN 'GL-BALANCE-DR'
                   MOVE PRD-GL-BALANCE-DR  TO WS-OLD-DISPLAY
                   MOVE WS-VALUE           TO PRD-GL-BALANCE-DR
               WHEN 'GL-INT-EXPENSE'
                   MOVE PRD-GL-INT-EXPENSE TO WS-OLD-DISPLAY
                   MOVE WS-VALUE           TO PRD-GL-INT-EXPENSE
               WHEN 'GL-INT-INCOME'
                   MOVE PRD-GL-INT-INCOME  TO WS-OLD-DISPLAY
                   MOVE WS-VALUE           TO PRD-GL-INT-INCOME
               WHEN 'GL-FEE-INCOME'
                   MOVE PRD-GL-FEE-INCOME  TO WS-OLD-DISPLAY
                   MOVE WS-VALUE           TO PRD-GL-FEE-INCOME
               WHEN 'COST-CENTRE'
                   MOVE PRD-COST-CENTRE    TO WS-OLD-DISPLAY
                   MOVE WS-VALUE           TO PRD-COST-CENTRE
           END-EVALUATE
           .
      *================================================================*
       2600-CROSS-CHECK.
      *================================================================*
           IF PRD-PAYS-CR-INT
               IF PRD-CR-TIER-THRESHOLD (1) NOT = ZERO
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'CREDIT TIER 1 THRESHOLD MUST BE ZERO'
                                           TO WS-REJECT-TEXT
               END-IF
               MOVE ZERO                   TO WS-PREV-THRESHOLD
               PERFORM VARYING WS-SUB FROM 2 BY 1 UNTIL WS-SUB > 5
                   IF PRD-CR-TIER-THRESHOLD (WS-SUB) > ZERO
                       IF PRD-CR-TIER-THRESHOLD (WS-SUB)
                          NOT > WS-PREV-THRESHOLD
                           MOVE 'N'        TO WS-SET-OK-IND
                           MOVE 'CREDIT TIER THRESHOLDS NOT ASCENDING'
                                           TO WS-REJECT-TEXT
                       END-IF
                       MOVE PRD-CR-TIER-THRESHOLD (WS-SUB)
                                           TO WS-PREV-THRESHOLD
                   END-IF
               END-PERFORM
               IF PRD-CR-DAY-BASIS NOT = ZERO AND 360 AND 365
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'CREDIT DAY BASIS MUST BE 0, 360 OR 365'
                                           TO WS-REJECT-TEXT
               END-IF
               IF PRD-GL-INT-EXPENSE = SPACES
               OR PRD-GL-INT-ACCR-PAY = SPACES
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'INTEREST-PAYING PRODUCT NEEDS EXPENSE GL CODES'
                                           TO WS-REJECT-TEXT
               END-IF
           END-IF

           IF PRD-CHARGES-DR-INT
               IF PRD-DR-RATE-UNARRANGED < PRD-DR-RATE-ARRANGED
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'UNARRANGED RATE BELOW ARRANGED RATE'
                                           TO WS-REJECT-TEXT
               END-IF
               IF PRD-GL-INT-INCOME = SPACES
               OR PRD-GL-INT-ACCR-REC = SPACES
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'DEBIT INTEREST PRODUCT NEEDS INCOME GL CODES'
                                           TO WS-REJECT-TEXT
               END-IF
           END-IF

           IF PRD-LEVIES-CHARGES
               IF PRD-CHG-NOTIFY-DAYS < 14
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'CHARGE NOTIFICATION MUST BE 14 DAYS OR MORE'
                                           TO WS-REJECT-TEXT
               END-IF
               IF PRD-GL-FEE-INCOME = SPACES
                   MOVE 'N'                TO WS-SET-OK-IND
                   MOVE 'CHARGING PRODUCT NEEDS A FEE INCOME GL CODE'
                                           TO WS-REJECT-TEXT
               END-IF
           END-IF

           IF PRD-MIN-AGE > ZERO AND PRD-MAX-AGE > ZERO
           AND PRD-MIN-AGE > PRD-MAX-AGE
               MOVE 'N'                    TO WS-SET-OK-IND
               MOVE 'MINIMUM AGE ABOVE MAXIMUM AGE'
                                           TO WS-REJECT-TEXT
           END-IF
           .
      *================================================================*
       2700-WRITE-PRODUCT.
      *================================================================*
           ADD 1                           TO PRD-VERSION-NO
           MOVE WS-BUS-DATE                TO PRD-EFFECTIVE-DATE
                                              PRD-LAST-MAINT-DATE
           MOVE WS-SET-CARD (1) (98:8)     TO PRD-LAST-MAINT-USER
           MOVE WS-CURR-REF                TO PRD-LAST-CHANGE-REF
           MOVE PRD-PARAM-RECORD           TO WS-PRODUCT-AFTER

           IF WS-NEW-PRODUCT
               WRITE PRD-PARAM-RECORD
           ELSE
               REWRITE PRD-PARAM-RECORD
           END-IF
           IF NOT WS-PRDPARM-OK
               MOVE '2700-WRITE-PRODUCT'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'PRDPARM WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-PRDPARM-STATUS      TO ABN-FILE-STATUS
               MOVE WS-CURR-PROD           TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           MOVE WS-CURR-REF                TO AUD-CHANGE-REF
           MOVE WS-BUS-DATE                TO AUD-BUS-DATE
           IF NOT WS-NEW-PRODUCT
               MOVE 'B'                    TO AUD-IMAGE-TYPE
               MOVE WS-PRODUCT-BEFORE      TO AUD-PRODUCT-IMAGE
               WRITE AUDIT-RECORD
           END-IF
           MOVE 'A'                        TO AUD-IMAGE-TYPE
           MOVE WS-PRODUCT-AFTER           TO AUD-PRODUCT-IMAGE
           WRITE AUDIT-RECORD
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'CHANGE CARDS READ'        TO TL-LABEL
           MOVE WS-CARDS-READ              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CHANGE SETS READ'         TO TL-LABEL
           MOVE WS-SETS-READ               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CHANGE SETS APPLIED'      TO TL-LABEL
           MOVE WS-SETS-APPLIED            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CHANGE SETS REJECTED'     TO TL-LABEL
           MOVE WS-SETS-REJECTED           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CHANGE SETS CARRIED FORWARD' TO TL-LABEL
           MOVE WS-SETS-CARRIED            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NEW PRODUCTS'             TO TL-LABEL
           MOVE WS-PRODUCTS-ADDED          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'FIELDS CHANGED'           TO TL-LABEL
           MOVE WS-FIELDS-CHANGED          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE CHANGES-FILE
                 PRDPARM-FILE
                 CARRYFWD-FILE
                 AUDIT-FILE
                 REPORT-FILE

           DISPLAY 'PRMNT010 - SETS APPLIED  ' WS-SETS-APPLIED
           DISPLAY 'PRMNT010 - SETS REJECTED ' WS-SETS-REJECTED
           IF WS-SETS-REJECTED > ZERO
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
       7200-REPORT-REJECT.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE - 1
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE WS-CURR-REF                TO DL-CHANGE-REF
           MOVE WS-CURR-PROD               TO DL-PROD-CD
           MOVE SPACES                     TO DL-ACTION DL-FIELD DL-OCC
                                              DL-OLD DL-NEW
           MOVE 'REJECTED'                 TO DL-RESULT
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           MOVE WS-REJECT-TEXT             TO RL-TEXT
           WRITE REPORT-RECORD             FROM WS-REJECT-LINE
           ADD 2                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-CARD.
      *================================================================*
           READ CHANGES-FILE INTO PCH-CHANGE-CARD
           EVALUATE TRUE
               WHEN WS-CHANGES-OK
                   ADD 1                   TO WS-CARDS-READ
               WHEN WS-CHANGES-EOF
                   SET WS-END-OF-CHANGES   TO TRUE
                   MOVE HIGH-VALUES        TO PCH-CHANGE-REF
               WHEN OTHER
                   MOVE '8000-READ-CARD'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CHANGES READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-CHANGES-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
