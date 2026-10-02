       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFMRG010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-06-06.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFMRG010                                            *
      * TITLE    : CIF - MERGE CONFIRMED DUPLICATE CUSTOMERS           *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CFMRG01 (WEEKLY - SATURDAY, AFTER CFDUP01 REVIEW)   *
      *----------------------------------------------------------------*
      * INPUT    : MERGEIN  - CONFIRMED MERGES FROM THE CIF DATA       *
      *                       QUALITY TEAM (FB 80): KEEP CIF, RETIRE   *
      *                       CIF, REQUESTER, APPROVER                 *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : CIFMAST  - CIF MASTER (RANDOM)                      *
      *            ACCTMAST - DDA MASTER (DYNAMIC, ALTERNATE KEY CIF)  *
      *            LOANMAST - LOANS (DYNAMIC, ALTERNATE KEY CIF)       *
      * OUTPUT   : AUDIT    - MERGE AUDIT TRAIL (FB 200)               *
      *            REPORT   - MERGE REPORT (FBA 133)                   *
      *----------------------------------------------------------------*
      * FOR EACH CONFIRMED PAIR (FOUND BY CFDUP010, CHECKED BY A       *
      * PERSON, APPROVED BY A SECOND PERSON):                          *
      *   1. VALIDATE - BOTH CIFS EXIST, DIFFERENT, NOT DECEASED,      *
      *      NOT ALREADY MERGED, SAME CUSTOMER TYPE, REQUESTER NOT     *
      *      THE APPROVER (FOUR EYES)                                  *
      *   2. EVERY ACCOUNT AND LOAN ON THE RETIRED CIF IS MOVED TO THE *
      *      KEPT CIF (ACM-CIF-NO / LN-CIF-NO REWRITTEN - THE AIX IS   *
      *      MAINTAINED BY VSAM AS AN UPGRADE SET)                     *
      *   3. KYC: THE KEPT CIF TAKES THE HIGHER RISK RATING, PEP IF    *
      *      EITHER IS A PEP, AND THE EARLIER NEXT REVIEW DATE         *
      *   4. CONTACT DETAILS ON THE KEPT CIF ARE FILLED FROM THE       *
      *      RETIRED CIF WHERE THE KEPT CIF HAS NONE                   *
      *   5. THE RETIRED CIF IS CLOSED WITH CIF-MERGED-INTO SET SO     *
      *      THAT OLD REFERENCES (LETTERS, IBANK SHADOW) CAN BE        *
      *      FOLLOWED                                                  *
      * THE IBANK SHADOW TABLE CUST_MASTER IS NOT UPDATED BY THIS JOB  *
      * - IBFEED02 PICKS UP THE CHANGE TONIGHT. PIR 2011/066: A        *
      * CUSTOMER LOGGED ON TO IBANK DURING THE MERGE SEES THE OLD      *
      * ACCOUNT LIST UNTIL THE NEXT DAY.                               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-06-06 AOS  ORIGINAL VERSION (FSCS SCV DATA QUALITY)       *
      * 2013-09-02 PDQ  LOANS MOVED AS WELL AS ACCOUNTS                *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MERGEIN-FILE     ASSIGN TO MERGEIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-MERGEIN-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS ACM-KEY
                                   ALTERNATE RECORD KEY IS ACM-CIF-NO
                                       WITH DUPLICATES
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT LOANMAST-FILE    ASSIGN TO LOANMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS LN-KEY
                                   ALTERNATE RECORD KEY IS LN-CIF-NO
                                       WITH DUPLICATES
                                   FILE STATUS IS WS-LOANMAST-STATUS.
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
       FD  MERGEIN-FILE
           RECORDING MODE IS F.
       01  MERGEIN-RECORD                  PIC X(80).
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  LOANMAST-FILE.
           COPY LNACCREC.
       FD  AUDIT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  AUDIT-RECORD                    PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFMRG010'.
      *
       01  MRG-REQUEST.
           05  MRG-KEEP-CIF                PIC X(12).
           05  MRG-RETIRE-CIF              PIC X(12).
           05  MRG-CASE-REF                PIC X(12).
           05  MRG-REQUESTER               PIC X(08).
           05  MRG-APPROVER                PIC X(08).
           05  MRG-SCORE                   PIC 9(03).
           05  FILLER                      PIC X(25).
      *
       01  WS-FILE-STATUSES.
           05  WS-MERGEIN-STATUS           PIC X(02).
               88  WS-MERGEIN-OK               VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00' '02'.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00' '02'.
           05  WS-AUDIT-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-REJECT-TEXT              PIC X(50).
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-KEY-COUNT                PIC S9(04) COMP.
           05  WS-ACCTS-MOVED              PIC S9(04) COMP.
           05  WS-LOANS-MOVED              PIC S9(04) COMP.
           05  WS-ACCT-KEYS.
               10  WS-ACCT-KEY-ENTRY       PIC X(14) OCCURS 50 TIMES.
           05  WS-LOAN-KEYS REDEFINES WS-ACCT-KEYS.
               10  WS-LOAN-KEY-ENTRY       PIC X(10) OCCURS 70 TIMES.
      *
      *---- SAVED COPIES OF THE TWO CIF RECORDS -----------------------*
       01  WS-KEEP-CIF                     PIC X(700).
       01  WS-RETIRE-CIF                   PIC X(700).
       01  WS-RISK-RANK-KEEP               PIC 9(01).
       01  WS-RISK-RANK-RETIRE             PIC 9(01).
       01  WS-RETIRE-DATA.
           05  WS-RT-RISK                  PIC X(01).
           05  WS-RT-PEP                   PIC X(01).
           05  WS-RT-NEXT-REVIEW           PIC 9(08).
           05  WS-RT-MOBILE                PIC X(20).
           05  WS-RT-HOME-TEL              PIC X(20).
           05  WS-RT-EMAIL                 PIC X(80).
           05  WS-RT-NINO                  PIC X(13).
           05  WS-RT-CUST-TYPE             PIC X(01).
      *
       01  WS-AUDIT.
           05  AU-DATE                     PIC 9(08).
           05  AU-CASE-REF                 PIC X(12).
           05  AU-ACTION                   PIC X(04).
           05  AU-KEEP-CIF                 PIC X(12).
           05  AU-RETIRE-CIF               PIC X(12).
           05  AU-OBJECT-TYPE              PIC X(04).
           05  AU-OBJECT-KEY               PIC X(14).
           05  AU-REQUESTER                PIC X(08).
           05  AU-APPROVER                 PIC X(08).
           05  AU-NOTE                     PIC X(50).
           05  FILLER                      PIC X(68).
      *
       01  WS-COUNTERS.
           05  WS-REQUESTS                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-MERGED                   PIC S9(07) COMP-3 VALUE 0.
           05  WS-REJECTED                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOT-ACCTS                PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOT-LOANS                PIC S9(07) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CFMRG010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - CUSTOMER MERGES'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(14) VALUE 'CASE'.
           05  FILLER            PIC X(14) VALUE 'KEEP CIF'.
           05  FILLER            PIC X(14) VALUE 'RETIRE CIF'.
           05  FILLER            PIC X(07) VALUE 'ACCTS'.
           05  FILLER            PIC X(07) VALUE 'LOANS'.
           05  FILLER            PIC X(10) VALUE 'RESULT'.
           05  FILLER            PIC X(66) VALUE 'NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-CASE                     PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-KEEP                     PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RETIRE                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACCTS                    PIC ZZ9.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  DL-LOANS                    PIC ZZ9.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  DL-RESULT                   PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(50).
           05  FILLER                      PIC X(16) VALUE SPACES.
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
           READ MERGEIN-FILE INTO MRG-REQUEST
           PERFORM UNTIL NOT WS-MERGEIN-OK
               ADD 1                       TO WS-REQUESTS
               PERFORM 2000-PROCESS-MERGE THRU 2000-EXIT
               READ MERGEIN-FILE INTO MRG-REQUEST
           END-PERFORM
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  MERGEIN-FILE
                       DATECARD-FILE
                I-O    CIFMAST-FILE
                       ACCTMAST-FILE
                       LOANMAST-FILE
                OUTPUT AUDIT-FILE
                       REPORT-FILE
           IF NOT WS-MERGEIN-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-LOANMAST-OK
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
           .
      *================================================================*
       2000-PROCESS-MERGE.
      *================================================================*
           MOVE SPACES                     TO WS-REJECT-TEXT
           MOVE ZERO                       TO WS-ACCTS-MOVED
                                              WS-LOANS-MOVED
           PERFORM 2100-VALIDATE THRU 2100-EXIT
           IF WS-REJECT-TEXT NOT = SPACES
               ADD 1                       TO WS-REJECTED
               MOVE 'REJECTED'             TO DL-RESULT
               MOVE 'REJ '                 TO AU-ACTION
               MOVE 'CIF '                 TO AU-OBJECT-TYPE
               MOVE MRG-RETIRE-CIF         TO AU-OBJECT-KEY
               MOVE WS-REJECT-TEXT         TO AU-NOTE
               PERFORM 6000-WRITE-AUDIT
               PERFORM 7100-REPORT-DETAIL
               GO TO 2000-EXIT
           END-IF

           PERFORM 2200-MOVE-ACCOUNTS THRU 2200-EXIT
           PERFORM 2300-MOVE-LOANS THRU 2300-EXIT
           PERFORM 2400-UPDATE-KEPT-CIF
           PERFORM 2500-RETIRE-CIF

           ADD 1                           TO WS-MERGED
           ADD WS-ACCTS-MOVED              TO WS-TOT-ACCTS
           ADD WS-LOANS-MOVED              TO WS-TOT-LOANS
           MOVE 'MERGED'                   TO DL-RESULT
           MOVE 'MERGE COMPLETE - IBANK SHADOW UPDATED TONIGHT'
                                           TO WS-REJECT-TEXT
           PERFORM 7100-REPORT-DETAIL
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-VALIDATE.
      *================================================================*
           EVALUATE TRUE
               WHEN MRG-KEEP-CIF = MRG-RETIRE-CIF
                   MOVE 'KEEP AND RETIRE CIF ARE THE SAME'
                                           TO WS-REJECT-TEXT
               WHEN MRG-REQUESTER = SPACES OR MRG-APPROVER = SPACES
                   MOVE 'REQUESTER AND APPROVER REQUIRED'
                                           TO WS-REJECT-TEXT
               WHEN MRG-REQUESTER = MRG-APPROVER
                   MOVE 'APPROVER MUST NOT BE THE REQUESTER'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-TEXT NOT = SPACES
               GO TO 2100-EXIT
           END-IF

           MOVE MRG-RETIRE-CIF             TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-CIFMAST-OK
                   MOVE 'RETIRE CIF NOT FOUND' TO WS-REJECT-TEXT
               WHEN CIF-MERGED-INTO NOT = SPACES
                   MOVE 'RETIRE CIF ALREADY MERGED'
                                           TO WS-REJECT-TEXT
               WHEN CIF-DECEASED
                   MOVE 'RETIRE CIF IS DECEASED - USE BEREAVEMENT'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   MOVE CIF-CUSTOMER-RECORD TO WS-RETIRE-CIF
                   MOVE CIF-RISK-RATING    TO WS-RT-RISK
                   MOVE CIF-PEP-IND        TO WS-RT-PEP
                   MOVE CIF-KYC-NEXT-REVIEW TO WS-RT-NEXT-REVIEW
                   MOVE CIF-MOBILE-TEL     TO WS-RT-MOBILE
                   MOVE CIF-HOME-TEL       TO WS-RT-HOME-TEL
                   MOVE CIF-EMAIL-ADDR     TO WS-RT-EMAIL
                   MOVE CIF-NAT-INS-NO     TO WS-RT-NINO
                   MOVE CIF-CUST-TYPE      TO WS-RT-CUST-TYPE
           END-EVALUATE
           IF WS-REJECT-TEXT NOT = SPACES
               GO TO 2100-EXIT
           END-IF

           MOVE MRG-KEEP-CIF               TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-CIFMAST-OK
                   MOVE 'KEEP CIF NOT FOUND' TO WS-REJECT-TEXT
               WHEN CIF-MERGED-INTO NOT = SPACES
                   MOVE 'KEEP CIF HAS ITSELF BEEN MERGED'
                                           TO WS-REJECT-TEXT
               WHEN CIF-DECEASED OR CIF-CLOSED
                   MOVE 'KEEP CIF IS CLOSED OR DECEASED'
                                           TO WS-REJECT-TEXT
               WHEN CIF-CUST-TYPE NOT = WS-RT-CUST-TYPE
                   MOVE 'CUSTOMER TYPES DIFFER (PERSONAL/BUSINESS)'
                                           TO WS-REJECT-TEXT
               WHEN CIF-NAT-INS-NO NOT = SPACES
               AND  WS-RT-NINO NOT = SPACES
               AND  CIF-NAT-INS-NO NOT = WS-RT-NINO
                   MOVE 'DIFFERENT NI NUMBERS - NOT THE SAME PERSON'
                                           TO WS-REJECT-TEXT
               WHEN OTHER
                   MOVE CIF-CUSTOMER-RECORD TO WS-KEEP-CIF
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-MOVE-ACCOUNTS.
      *================================================================*
      *    TABLE THE KEYS FIRST - REWRITING THE ALTERNATE KEY WHILE
      *    BROWSING ON IT WOULD LOSE THE POSITION.
           MOVE ZERO                       TO WS-KEY-COUNT
           MOVE MRG-RETIRE-CIF             TO ACM-CIF-NO
           START ACCTMAST-FILE KEY NOT < ACM-CIF-NO
           IF NOT WS-ACCTMAST-OK
               GO TO 2200-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ ACCTMAST-FILE NEXT RECORD
               IF NOT WS-ACCTMAST-OK
               OR ACM-CIF-NO NOT = MRG-RETIRE-CIF
               OR WS-KEY-COUNT >= 50
                   SET WS-END-BROWSE       TO TRUE
               ELSE
                   ADD 1                   TO WS-KEY-COUNT
                   MOVE ACM-KEY TO WS-ACCT-KEY-ENTRY (WS-KEY-COUNT)
               END-IF
           END-PERFORM

           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-KEY-COUNT
               MOVE WS-ACCT-KEY-ENTRY (WS-SUB) TO ACM-KEY
               READ ACCTMAST-FILE KEY IS ACM-KEY
               IF WS-ACCTMAST-OK
                   MOVE MRG-KEEP-CIF       TO ACM-CIF-NO
                   MOVE WS-BUS-DATE        TO ACM-LAST-MAINT-DATE
                   MOVE ZERO               TO ACM-LAST-MAINT-TIME
                   MOVE WS-PROGRAM-ID      TO ACM-LAST-MAINT-USER
                   REWRITE ACM-ACCOUNT-RECORD
                   IF NOT WS-ACCTMAST-OK
                       MOVE '2200-MOVE-ACCOUNTS' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                       MOVE ACM-KEY        TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
                   ADD 1                   TO WS-ACCTS-MOVED
                   MOVE 'MOVE'             TO AU-ACTION
                   MOVE 'ACCT'             TO AU-OBJECT-TYPE
                   MOVE ACM-KEY            TO AU-OBJECT-KEY
                   MOVE SPACES             TO AU-NOTE
                   PERFORM 6000-WRITE-AUDIT
               END-IF
           END-PERFORM
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-MOVE-LOANS.
      *================================================================*
           MOVE ZERO                       TO WS-KEY-COUNT
           MOVE MRG-RETIRE-CIF             TO LN-CIF-NO
           START LOANMAST-FILE KEY NOT < LN-CIF-NO
           IF NOT WS-LOANMAST-OK
               GO TO 2300-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ LOANMAST-FILE NEXT RECORD
               IF NOT WS-LOANMAST-OK
               OR LN-CIF-NO NOT = MRG-RETIRE-CIF
               OR WS-KEY-COUNT >= 70
                   SET WS-END-BROWSE       TO TRUE
               ELSE
                   ADD 1                   TO WS-KEY-COUNT
                   MOVE LN-KEY TO WS-LOAN-KEY-ENTRY (WS-KEY-COUNT)
               END-IF
           END-PERFORM

           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-KEY-COUNT
               MOVE WS-LOAN-KEY-ENTRY (WS-SUB) TO LN-KEY
               READ LOANMAST-FILE KEY IS LN-KEY
               IF WS-LOANMAST-OK
                   MOVE MRG-KEEP-CIF       TO LN-CIF-NO
                   MOVE WS-BUS-DATE        TO LN-LAST-MAINT-DATE
                   MOVE WS-PROGRAM-ID      TO LN-LAST-MAINT-USER
                   REWRITE LN-LOAN-RECORD
                   IF NOT WS-LOANMAST-OK
                       MOVE '2300-MOVE-LOANS' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'LOANMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-LOANMAST-STATUS TO ABN-FILE-STATUS
                       MOVE LN-KEY         TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
                   ADD 1                   TO WS-LOANS-MOVED
                   MOVE 'MOVE'             TO AU-ACTION
                   MOVE 'LOAN'             TO AU-OBJECT-TYPE
                   MOVE LN-KEY             TO AU-OBJECT-KEY
                   MOVE SPACES             TO AU-NOTE
                   PERFORM 6000-WRITE-AUDIT
               END-IF
           END-PERFORM
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2400-UPDATE-KEPT-CIF.
      *================================================================*
           MOVE WS-KEEP-CIF                TO CIF-CUSTOMER-RECORD
      *    HIGHER RISK RATING WINS (H > M > L).
           EVALUATE CIF-RISK-RATING
               WHEN 'H'     MOVE 3         TO WS-RISK-RANK-KEEP
               WHEN 'M'     MOVE 2         TO WS-RISK-RANK-KEEP
               WHEN OTHER   MOVE 1         TO WS-RISK-RANK-KEEP
           END-EVALUATE
           EVALUATE WS-RT-RISK
               WHEN 'H'     MOVE 3         TO WS-RISK-RANK-RETIRE
               WHEN 'M'     MOVE 2         TO WS-RISK-RANK-RETIRE
               WHEN OTHER   MOVE 1         TO WS-RISK-RANK-RETIRE
           END-EVALUATE
           IF WS-RISK-RANK-RETIRE > WS-RISK-RANK-KEEP
               MOVE WS-RT-RISK             TO CIF-RISK-RATING
           END-IF
           IF WS-RT-PEP = 'Y'
               MOVE 'Y'                    TO CIF-PEP-IND
           END-IF
           IF WS-RT-NEXT-REVIEW NUMERIC
           AND WS-RT-NEXT-REVIEW > ZERO
               IF CIF-KYC-NEXT-REVIEW NOT NUMERIC
               OR CIF-KYC-NEXT-REVIEW = ZERO
               OR WS-RT-NEXT-REVIEW < CIF-KYC-NEXT-REVIEW
                   MOVE WS-RT-NEXT-REVIEW  TO CIF-KYC-NEXT-REVIEW
               END-IF
           END-IF
           IF CIF-MOBILE-TEL = SPACES
               MOVE WS-RT-MOBILE           TO CIF-MOBILE-TEL
           END-IF
           IF CIF-HOME-TEL = SPACES
               MOVE WS-RT-HOME-TEL         TO CIF-HOME-TEL
           END-IF
           IF CIF-EMAIL-ADDR = SPACES
               MOVE WS-RT-EMAIL            TO CIF-EMAIL-ADDR
           END-IF
           IF CIF-NAT-INS-NO = SPACES
               MOVE WS-RT-NINO             TO CIF-NAT-INS-NO
           END-IF
           MOVE WS-BUS-DATE                TO CIF-DUP-CHECK-DATE
                                              CIF-LAST-MAINT-DATE
           MOVE ZERO                       TO CIF-LAST-MAINT-TIME
           MOVE MRG-APPROVER               TO CIF-LAST-MAINT-USER
           SET CIF-MAINT-BATCH             TO TRUE
           REWRITE CIF-CUSTOMER-RECORD
           IF NOT WS-CIFMAST-OK
               MOVE '2400-UPDATE-KEPT-CIF' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CIFMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CIFMAST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           MOVE 'UPDT'                     TO AU-ACTION
           MOVE 'CIF '                     TO AU-OBJECT-TYPE
           MOVE MRG-KEEP-CIF               TO AU-OBJECT-KEY
           MOVE 'KYC / CONTACT DETAILS CONSOLIDATED' TO AU-NOTE
           PERFORM 6000-WRITE-AUDIT
           .
      *================================================================*
       2500-RETIRE-CIF.
      *================================================================*
           MOVE MRG-RETIRE-CIF             TO CIF-NO
           READ CIFMAST-FILE
           IF NOT WS-CIFMAST-OK
               MOVE '2500-RETIRE-CIF'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CIFMAST RE-READ FAILED' TO ABN-MESSAGE
               MOVE WS-CIFMAST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           SET CIF-CLOSED                  TO TRUE
           MOVE MRG-KEEP-CIF               TO CIF-MERGED-INTO
           MOVE WS-BUS-DATE                TO CIF-DUP-CHECK-DATE
                                              CIF-LAST-MAINT-DATE
           MOVE ZERO                       TO CIF-LAST-MAINT-TIME
           MOVE MRG-APPROVER               TO CIF-LAST-MAINT-USER
           SET CIF-MAINT-BATCH             TO TRUE
           MOVE 'N'                        TO CIF-MKTG-EMAIL-IND
                                              CIF-MKTG-POST-IND
           REWRITE CIF-CUSTOMER-RECORD
           IF NOT WS-CIFMAST-OK
               MOVE '2500-RETIRE-CIF'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CIFMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CIFMAST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           MOVE 'RETR'                     TO AU-ACTION
           MOVE 'CIF '                     TO AU-OBJECT-TYPE
           MOVE MRG-RETIRE-CIF             TO AU-OBJECT-KEY
           MOVE 'CLOSED - MERGED INTO KEPT CIF' TO AU-NOTE
           PERFORM 6000-WRITE-AUDIT
           .
      *================================================================*
       6000-WRITE-AUDIT.
      *================================================================*
           MOVE WS-BUS-DATE                TO AU-DATE
           MOVE MRG-CASE-REF               TO AU-CASE-REF
           MOVE MRG-KEEP-CIF               TO AU-KEEP-CIF
           MOVE MRG-RETIRE-CIF             TO AU-RETIRE-CIF
           MOVE MRG-REQUESTER              TO AU-REQUESTER
           MOVE MRG-APPROVER               TO AU-APPROVER
           WRITE AUDIT-RECORD              FROM WS-AUDIT
           IF WS-AUDIT-STATUS NOT = '00'
               MOVE '6000-WRITE-AUDIT'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'AUDIT WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-AUDIT-STATUS        TO ABN-FILE-STATUS
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
           MOVE 3                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE MRG-CASE-REF               TO DL-CASE
           MOVE MRG-KEEP-CIF               TO DL-KEEP
           MOVE MRG-RETIRE-CIF             TO DL-RETIRE
           MOVE WS-ACCTS-MOVED             TO DL-ACCTS
           MOVE WS-LOANS-MOVED             TO DL-LOANS
           MOVE WS-REJECT-TEXT             TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'MERGE REQUESTS'           TO TL-LABEL
           MOVE WS-REQUESTS                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MERGED'                   TO TL-LABEL
           MOVE WS-MERGED                  TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REJECTED'                 TO TL-LABEL
           MOVE WS-REJECTED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS MOVED'           TO TL-LABEL
           MOVE WS-TOT-ACCTS               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LOANS MOVED'              TO TL-LABEL
           MOVE WS-TOT-LOANS               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE MERGEIN-FILE CIFMAST-FILE ACCTMAST-FILE LOANMAST-FILE
                 AUDIT-FILE REPORT-FILE
           DISPLAY 'CFMRG010 - MERGED ' WS-MERGED
                   ' REJECTED ' WS-REJECTED
           IF WS-REJECTED > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
