       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BCPEXT10.
       AUTHOR.        M L WRIGHT.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1998-06-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : BCPEXT10                                            *
      * TITLE    : BUSINESS CONTINUITY - NIGHTLY ACCOUNT EXTRACT       *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : BCPEXT01 STEP BC010 (NIGHTLY, END OF MBKEOD)        *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ)      *
      *            CIFMAST  - CUSTOMER MASTER (VSAM KSDS, RANDOM)      *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : BCPFILE  - CONTINGENCY EXTRACT (BCPEXREC FB 300)    *
      *            REPORT   - CONTROL REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * IF THE HOST IS UNAVAILABLE (DR INVOCATION OR A LONG OUTAGE)    *
      * BRANCHES RUN THE BCPVIEW CONTINGENCY SYSTEM ON A STANDALONE PC *
      * LOADED FROM THIS FILE, SO THAT CUSTOMERS CAN BE IDENTIFIED AND *
      * PAID LIMITED SUMS AGAINST LAST NIGHT'S BALANCE. THE SAME FILE  *
      * IS SENT TO THE DR SITE AS A CROSS-CHECK FOR THE RESTORED       *
      * ACCOUNT MASTER (OPERATIONS RUNBOOK SECTION 21).                *
      *                                                                *
      * EVERY ACCOUNT EXCEPT CLOSED ONES (AND LOAN / MORTGAGE SHADOW   *
      * ACCOUNTS) IS EXTRACTED WITH ITS BALANCES, LIMIT, HOLDS AND     *
      * STATUS. SPENDING POWER = AVAILABLE + ARRANGED OD LIMIT (THE    *
      * LIMIT IS IGNORED IF IT HAS EXPIRED). THE CONTINGENCY LIMIT IS  *
      * THE LOWER OF SPENDING POWER AND WS-CONTINGENCY-CAP PER DAY.    *
      *                                                                *
      * SERVE INDICATOR 'N' (REFER TO HEAD OFFICE BY PHONE) WHEN:      *
      *   FRZN  ACCOUNT FROZEN          NODR  NO DEBITS / NO POSTINGS  *
      *   DCSD  CUSTOMER DECEASED       BLKD  CIF BLOCKED              *
      *   KYCF  KYC FAILED              CLSP  CLOSURE PENDING          *
      *   DORM  DORMANT (RECLAIM CHECK) NOFN  NO SPENDING POWER        *
      *                                                                *
      * TRAILER HASH TOTALS: RECORD COUNT, SUM OF ACCOUNT NUMBERS      *
      * (NUMERIC), LEDGER, AVAILABLE AND SPENDING POWER. ANY ACCOUNT   *
      * WITH A NON-NUMERIC ACCOUNT NUMBER IS STILL EXTRACTED BUT ADDS  *
      * ZERO TO THE ACCOUNT HASH AND IS REPORTED (RC 4).               *
      *----------------------------------------------------------------*
      * KNOWN PROBLEMS                                                 *
      * PIR 2010/066 OPEN - HOLDS ARE THE TOTAL ACM-HOLD-AMT ONLY, NOT *
      *   THE INDIVIDUAL HOLDS ON DDHLDREC, SO A BRANCH CANNOT TELL A  *
      *   CARD PRE-AUTH FROM A LEGAL HOLD. LEGAL HOLDS ALSO SET        *
      *   ACM-NO-DEBITS SO THE SERVE INDICATOR IS STILL CORRECT.       *
      * PIR 2013/120 OPEN - BCPVIEW STILL HAS A 32,000 ACCOUNT LIMIT   *
      *   PER REGION; LONDON REGION (SORT CODES 40-NN-NN) EXCEEDS IT   *
      *   AND IS SPLIT INTO TWO FILES BY STEP BC020 (SEE JCL).         *
      * THE CONTINGENCY CAP IS COMPILED IN (WAS 250.00 IN THE 1998 BCP *
      * MANUAL, RAISED TO 500.00 IN 2009). A CHANGE NEEDS A RECOMPILE. *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1998-06-15 MLW  ORIGINAL VERSION (Y2K CONTINGENCY PROGRAMME)   *
      * 2001-04-30 DKP  IBAN ON EXTRACT                                *
      * 2005-09-12 AOS  CIF NUMBER FROM NEW OFFSET (DDACCREC CHANGE)   *
      * 2009-03-02 PDQ  CONTINGENCY LIMIT 500.00, SERVE INDICATOR      *
      * 2011-02-21 JNB  DECEASED FROM CIF BEREAVEMENT FIELDS           *
      * 2013-05-20 KLM  DATE OF BIRTH (BCP TEST NOV 2012 FINDING 4)    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
           SELECT BCPFILE-FILE     ASSIGN TO BCPFILE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-BCPFILE-STATUS.
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
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  BCPFILE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BCPFILE-RECORD                  PIC X(300).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'BCPEXT10'.
      *
       01  WS-CONSTANTS.
           05  WS-FILE-ID                  PIC X(08) VALUE 'MBKBCP01'.
           05  WS-SOURCE-SYS               PIC X(08) VALUE 'MBKPROD'.
           05  WS-LAYOUT-VERSION           PIC X(02) VALUE '04'.
           05  WS-CONTINGENCY-CAP          PIC S9(07)V99 COMP-3
                                           VALUE +500.00.
      *    FEWER ACCOUNTS THAN THIS STOPS THE RUN BEFORE THE TRAILER
      *    IS WRITTEN (PROTECTS BRANCHES FROM A HALF-EMPTY FILE AFTER
      *    A BAD VSAM RESTORE - INC 2007/0311). NEVER REVISED SINCE
      *    2007 - THE FILE NOW HOLDS OVER 400,000 ACCOUNTS.
           05  WS-MIN-EXPECTED-ACCTS       PIC S9(09) COMP-3
                                           VALUE +1000.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-BCPFILE-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-CIF-IND                  PIC X(01).
               88  WS-CIF-FOUND                VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CURRENT-DATE-TIME.
               10  WS-CDT-DATE             PIC 9(08).
               10  WS-CDT-TIME             PIC 9(06).
               10  FILLER                  PIC X(07).
           05  WS-LAST-CIF-NO              PIC X(12) VALUE LOW-VALUES.
           05  WS-LIMIT-USED               PIC S9(13)V99 COMP-3.
           05  WS-SPENDING                 PIC S9(13)V99 COMP-3.
           05  WS-CONT-LIMIT               PIC S9(07)V99 COMP-3.
           05  WS-ACCT-NO-NUM              PIC 9(08).
           05  WS-SERVE-RSN                PIC X(04).
           05  WS-NAME-WORK                PIC X(40).
           05  WS-TYPE-IDX                 PIC S9(04) COMP.
           05  WS-I                        PIC S9(04) COMP.
      *
      *---- HASH TOTALS -----------------------------------------------*
       01  WS-HASH-TOTALS.
           05  WS-HASH-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-HASH-ACCT-NO             PIC S9(15) COMP-3 VALUE 0.
           05  WS-HASH-LEDGER              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-HASH-AVAIL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-HASH-SPENDING            PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
      *---- COUNTS BY ACCOUNT TYPE ------------------------------------*
       01  WS-TYPE-VALUES.
           05  FILLER                      PIC X(04) VALUE 'CURR'.
           05  FILLER                      PIC X(04) VALUE 'SAVE'.
           05  FILLER                      PIC X(04) VALUE 'ISA '.
           05  FILLER                      PIC X(04) VALUE 'FTD '.
           05  FILLER                      PIC X(04) VALUE 'OTHR'.
       01  WS-TYPE-CODES REDEFINES WS-TYPE-VALUES.
           05  WS-TYPE-CODE                PIC X(04) OCCURS 5 TIMES.
       01  WS-TYPE-TOTALS.
           05  WS-TT-ENTRY                 OCCURS 5 TIMES.
               10  WS-TT-COUNT             PIC S9(09) COMP-3.
               10  WS-TT-LEDGER            PIC S9(15)V99 COMP-3.
      *
      *---- COUNTS BY SERVE REASON ------------------------------------*
       01  WS-RSN-VALUES.
           05  FILLER                      PIC X(34)
               VALUE 'FRZNACCOUNT FROZEN                '.
           05  FILLER                      PIC X(34)
               VALUE 'NODRNO DEBITS / NO POSTINGS       '.
           05  FILLER                      PIC X(34)
               VALUE 'DCSDCUSTOMER DECEASED             '.
           05  FILLER                      PIC X(34)
               VALUE 'BLKDCUSTOMER BLOCKED              '.
           05  FILLER                      PIC X(34)
               VALUE 'KYCFKYC FAILED                    '.
           05  FILLER                      PIC X(34)
               VALUE 'CLSPCLOSURE PENDING               '.
           05  FILLER                      PIC X(34)
               VALUE 'DORMDORMANT                       '.
           05  FILLER                      PIC X(34)
               VALUE 'NOFNNO SPENDING POWER             '.
       01  WS-RSN-TABLE REDEFINES WS-RSN-VALUES.
           05  WS-RSN-ENTRY                OCCURS 8 TIMES.
               10  WS-RSN-CODE             PIC X(04).
               10  WS-RSN-TEXT             PIC X(30).
       01  WS-RSN-COUNTS.
           05  WS-RSN-COUNT                PIC S9(09) COMP-3
                                           OCCURS 8 TIMES.
      *
       01  WS-COUNTERS.
           05  WS-ACCTS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-CLOSED-SKIPPED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOAN-SKIPPED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-SERVE-Y                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-SERVE-N                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXPIRED-LIMITS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-CIF-NOT-FOUND            PIC S9(09) COMP-3 VALUE 0.
           05  WS-BAD-ACCT-NO              PIC S9(09) COMP-3 VALUE 0.
           05  WS-NON-STERLING             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RECORDS-WRITTEN          PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-HOLDS              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-UNCLEARED          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-OD-LIMITS          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-CONT-LIMIT         PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'BCPEXT10'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - BUSINESS CONTINUITY EXTRACT'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(08) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(14) VALUE 'CUSTOMER'.
           05  FILLER            PIC X(16) VALUE '        LEDGER'.
           05  FILLER            PIC X(84) VALUE 'EXCEPTION'.
       01  WS-EXCEPTION-LINE.
           05  EL-CC                       PIC X(01) VALUE ' '.
           05  EL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-LEDGER                   PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-TEXT                     PIC X(50).
           05  FILLER                      PIC X(34) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
       01  WS-HASH-LINE.
           05  HL-CC                       PIC X(01) VALUE ' '.
           05  HL-LABEL                    PIC X(40).
           05  HL-VALUE                    PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(69) VALUE SPACES.
      *
           COPY BCPEXREC.
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
           PERFORM 2000-PROCESS-ACCOUNT THRU 2000-EXIT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 3000-WRITE-TRAILER
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       CIFMAST-FILE
                       DATECARD-FILE
                OUTPUT BCPFILE-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR WS-BCPFILE-STATUS NOT = '00'
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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE ZERO                   TO WS-TT-COUNT (WS-I)
                                              WS-TT-LEDGER (WS-I)
           END-PERFORM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE ZERO                   TO WS-RSN-COUNT (WS-I)
           END-PERFORM

      *    HEADER RECORD. THE CREATION STAMP IS THE SYSTEM CLOCK -
      *    BCPVIEW SHOWS IT SO BRANCHES KNOW HOW OLD THE DATA IS.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-TIME
           MOVE SPACES                     TO BCX-HEADER-RECORD
           MOVE 'H'                        TO BCH-REC-TYPE
           MOVE WS-FILE-ID                 TO BCH-FILE-ID
           MOVE WS-BUS-DATE                TO BCH-BUS-DATE
           MOVE WS-CDT-DATE                TO BCH-CREATE-DATE
           MOVE WS-CDT-TIME                TO BCH-CREATE-TIME
           MOVE DTC-RUN-ID                 TO BCH-RUN-ID
           MOVE WS-SOURCE-SYS              TO BCH-SOURCE-SYS
           MOVE WS-LAYOUT-VERSION          TO BCH-LAYOUT-VERSION
           PERFORM 5000-WRITE-BCP

           PERFORM 7000-REPORT-HEADINGS
           PERFORM 8000-READ-NEXT
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           ADD 1                           TO WS-ACCTS-READ
           IF ACM-CLOSED
               ADD 1                       TO WS-CLOSED-SKIPPED
               PERFORM 8000-READ-NEXT
               GO TO 2000-EXIT
           END-IF
           IF ACM-LOAN OR ACM-MORTGAGE
               ADD 1                       TO WS-LOAN-SKIPPED
               PERFORM 8000-READ-NEXT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-GET-CUSTOMER THRU 2100-EXIT
           PERFORM 2200-SPENDING-POWER
           PERFORM 2300-SERVE-INDICATOR
           PERFORM 2400-BUILD-DETAIL
           PERFORM 5000-WRITE-BCP
           PERFORM 2500-ACCUMULATE
           PERFORM 8000-READ-NEXT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-GET-CUSTOMER.
      *================================================================*
      *    JOINT ACCOUNTS CARRY THE FIRST NAMED HOLDER ONLY (THE
      *    SECOND HOLDER IS ON THE JOINT MANDATE FILE, NOT EXTRACTED).
           IF ACM-CIF-NO = WS-LAST-CIF-NO
               GO TO 2100-EXIT
           END-IF
           MOVE ACM-CIF-NO                 TO WS-LAST-CIF-NO
                                              CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   SET WS-CIF-FOUND        TO TRUE
               WHEN WS-CIFMAST-NOTFND
                   MOVE 'N'                TO WS-CIF-IND
               WHEN OTHER
                   MOVE '2100-GET-CUSTOMER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE ACM-CIF-NO         TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           MOVE SPACES                     TO WS-NAME-WORK
           IF WS-CIF-FOUND
               IF CIF-BUSINESS
                   STRING CIF-FIRST-NM DELIMITED BY '  '
                          ' '          DELIMITED BY SIZE
                          CIF-LAST-NM  DELIMITED BY '  '
                          INTO WS-NAME-WORK
                   END-STRING
               ELSE
                   STRING CIF-LAST-NM  DELIMITED BY '  '
                          ', '         DELIMITED BY SIZE
                          CIF-FIRST-NM DELIMITED BY '  '
                          ' '          DELIMITED BY SIZE
                          CIF-TITLE    DELIMITED BY SPACE
                          INTO WS-NAME-WORK
                   END-STRING
               END-IF
           ELSE
               MOVE '*** CUSTOMER NOT ON CIF ***' TO WS-NAME-WORK
           END-IF
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-SPENDING-POWER.
      *================================================================*
           MOVE ZERO                       TO WS-LIMIT-USED
           IF ACM-OD-LIMIT NUMERIC
           AND ACM-OD-LIMIT > ZERO
      *        AN EXPIRED LIMIT NOT YET REMOVED BY ODREV010 (IT RUNS
      *        LATER IN THE NIGHT) IS NOT GIVEN TO THE BRANCHES.
               IF ACM-OD-EXPIRY-DATE NUMERIC
               AND ACM-OD-EXPIRY-DATE > ZERO
               AND ACM-OD-EXPIRY-DATE < WS-BUS-DATE
                   ADD 1                   TO WS-EXPIRED-LIMITS
               ELSE
                   MOVE ACM-OD-LIMIT       TO WS-LIMIT-USED
               END-IF
           END-IF
           COMPUTE WS-SPENDING = ACM-AVAIL-BAL + WS-LIMIT-USED
           IF WS-SPENDING > WS-CONTINGENCY-CAP
               MOVE WS-CONTINGENCY-CAP     TO WS-CONT-LIMIT
           ELSE
               IF WS-SPENDING > ZERO
                   MOVE WS-SPENDING        TO WS-CONT-LIMIT
               ELSE
                   MOVE ZERO               TO WS-CONT-LIMIT
               END-IF
           END-IF
           .
      *================================================================*
       2300-SERVE-INDICATOR.
      *================================================================*
      *    FIRST MATCHING REASON WINS - ORDER IS SEVERITY ORDER.
           MOVE SPACES                     TO WS-SERVE-RSN
           EVALUATE TRUE
               WHEN ACM-FROZEN
                   MOVE 'FRZN'             TO WS-SERVE-RSN
               WHEN ACM-NO-DEBITS OR ACM-NO-POSTINGS
                   MOVE 'NODR'             TO WS-SERVE-RSN
               WHEN WS-CIF-FOUND
                AND (CIF-DECEASED
                     OR (CIF-DATE-OF-DEATH NUMERIC
                         AND CIF-DATE-OF-DEATH > ZERO))
                   MOVE 'DCSD'             TO WS-SERVE-RSN
               WHEN WS-CIF-FOUND AND CIF-BLOCKED
                   MOVE 'BLKD'             TO WS-SERVE-RSN
               WHEN WS-CIF-FOUND AND CIF-KYC-FAILED
                   MOVE 'KYCF'             TO WS-SERVE-RSN
               WHEN ACM-PENDING-CLOSE
                   MOVE 'CLSP'             TO WS-SERVE-RSN
               WHEN ACM-DORMANT
                   MOVE 'DORM'             TO WS-SERVE-RSN
               WHEN WS-CONT-LIMIT = ZERO
                   MOVE 'NOFN'             TO WS-SERVE-RSN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *    CUSTOMER NOT ON CIF - BRANCH CANNOT IDENTIFY THEM.
           IF NOT WS-CIF-FOUND
           AND WS-SERVE-RSN = SPACES
               MOVE 'BLKD'                 TO WS-SERVE-RSN
           END-IF
           IF WS-SERVE-RSN NOT = SPACES
               MOVE ZERO                   TO WS-CONT-LIMIT
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
                   IF WS-RSN-CODE (WS-I) = WS-SERVE-RSN
                       ADD 1               TO WS-RSN-COUNT (WS-I)
                   END-IF
               END-PERFORM
           END-IF
           .
      *================================================================*
       2400-BUILD-DETAIL.
      *================================================================*
           MOVE SPACES                     TO BCX-DETAIL-RECORD
           MOVE 'D'                        TO BCX-REC-TYPE
           MOVE ACM-SORT-CODE              TO BCX-SORT-CODE
           MOVE ACM-ACCT-NO                TO BCX-ACCT-NO
           MOVE ACM-CIF-NO                 TO BCX-CIF-NO
           MOVE ACM-ACCT-TYPE-CD           TO BCX-ACCT-TYPE-CD
           MOVE ACM-PROD-CD                TO BCX-PROD-CD
           MOVE ACM-CCY-CD                 TO BCX-CCY-CD
           MOVE ACM-ACCT-STATUS            TO BCX-ACCT-STATUS
           MOVE ACM-POST-RESTRICT          TO BCX-POST-RESTRICT
           MOVE WS-NAME-WORK               TO BCX-CUST-NAME
           IF WS-CIF-FOUND
               MOVE CIF-POST-CD            TO BCX-POST-CD
               MOVE CIF-CUST-TYPE          TO BCX-CUST-TYPE
               MOVE CIF-REC-STATUS         TO BCX-CIF-STATUS
               IF CIF-DOB NUMERIC
                   MOVE CIF-DOB            TO BCX-DOB
               ELSE
                   MOVE ZERO               TO BCX-DOB
               END-IF
           ELSE
               MOVE SPACES                 TO BCX-POST-CD
               MOVE '?'                    TO BCX-CUST-TYPE
                                              BCX-CIF-STATUS
               MOVE ZERO                   TO BCX-DOB
           END-IF
           MOVE ACM-LEDGER-BAL             TO BCX-LEDGER-BAL
           MOVE ACM-AVAIL-BAL              TO BCX-AVAIL-BAL
           MOVE ACM-UNCLEARED-AMT          TO BCX-UNCLEARED-AMT
           MOVE ACM-HOLD-AMT               TO BCX-HOLD-AMT
           MOVE WS-LIMIT-USED              TO BCX-OD-LIMIT
           IF ACM-OD-EXPIRY-DATE NUMERIC
               MOVE ACM-OD-EXPIRY-DATE     TO BCX-OD-EXPIRY-DATE
           ELSE
               MOVE ZERO                   TO BCX-OD-EXPIRY-DATE
           END-IF
           MOVE WS-SPENDING                TO BCX-SPENDING-POWER
           MOVE WS-CONT-LIMIT              TO BCX-CONTINGENCY-LIMIT
           IF WS-SERVE-RSN = SPACES
               SET BCX-MAY-SERVE           TO TRUE
               ADD 1                       TO WS-SERVE-Y
           ELSE
               SET BCX-REFER               TO TRUE
               MOVE WS-SERVE-RSN           TO BCX-SERVE-RSN
               ADD 1                       TO WS-SERVE-N
           END-IF
           IF ACM-LAST-TXN-DATE NUMERIC
               MOVE ACM-LAST-TXN-DATE      TO BCX-LAST-TXN-DATE
           ELSE
               MOVE ZERO                   TO BCX-LAST-TXN-DATE
           END-IF
           IF ACM-MATURITY-DATE NUMERIC
               MOVE ACM-MATURITY-DATE      TO BCX-MATURITY-DATE
           ELSE
               MOVE ZERO                   TO BCX-MATURITY-DATE
           END-IF
           MOVE ACM-IBAN                   TO BCX-IBAN
           .
      *================================================================*
       2500-ACCUMULATE.
      *================================================================*
           ADD 1                           TO WS-HASH-COUNT
           IF ACM-ACCT-NO NUMERIC
               MOVE ACM-ACCT-NO            TO WS-ACCT-NO-NUM
               ADD WS-ACCT-NO-NUM          TO WS-HASH-ACCT-NO
           ELSE
               ADD 1                       TO WS-BAD-ACCT-NO
               MOVE 'NON-NUMERIC ACCOUNT NUMBER - NOT IN HASH'
                                           TO EL-TEXT
               PERFORM 7100-EXCEPTION
           END-IF
           ADD ACM-LEDGER-BAL              TO WS-HASH-LEDGER
           ADD ACM-AVAIL-BAL               TO WS-HASH-AVAIL
           ADD WS-SPENDING                 TO WS-HASH-SPENDING
           ADD ACM-HOLD-AMT                TO WS-TOTAL-HOLDS
           ADD ACM-UNCLEARED-AMT           TO WS-TOTAL-UNCLEARED
           ADD WS-LIMIT-USED               TO WS-TOTAL-OD-LIMITS
           ADD WS-CONT-LIMIT               TO WS-TOTAL-CONT-LIMIT
           IF ACM-CCY-CD NOT = 'GBP'
               ADD 1                       TO WS-NON-STERLING
           END-IF
           IF NOT WS-CIF-FOUND
               ADD 1                       TO WS-CIF-NOT-FOUND
               MOVE 'CUSTOMER NOT ON CIF - SERVE IND N'
                                           TO EL-TEXT
               PERFORM 7100-EXCEPTION
           END-IF
           MOVE 5                          TO WS-TYPE-IDX
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               IF WS-TYPE-CODE (WS-I) = ACM-ACCT-TYPE-CD
                   MOVE WS-I               TO WS-TYPE-IDX
               END-IF
           END-PERFORM
           ADD 1                       TO WS-TT-COUNT (WS-TYPE-IDX)
           ADD ACM-LEDGER-BAL          TO WS-TT-LEDGER (WS-TYPE-IDX)
           .
      *================================================================*
       3000-WRITE-TRAILER.
      *================================================================*
           IF WS-HASH-COUNT < WS-MIN-EXPECTED-ACCTS
               MOVE '3000-WRITE-TRAILER'   TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'TOO FEW ACCOUNTS - CHECK ACCTMAST RESTORE'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           MOVE SPACES                     TO BCX-TRAILER-RECORD
           MOVE 'T'                        TO BCT-REC-TYPE
           MOVE WS-FILE-ID                 TO BCT-FILE-ID
           MOVE WS-BUS-DATE                TO BCT-BUS-DATE
           MOVE WS-HASH-COUNT              TO BCT-RECORD-COUNT
           MOVE WS-HASH-ACCT-NO            TO BCT-HASH-ACCT-NO
           MOVE WS-HASH-LEDGER             TO BCT-HASH-LEDGER
           MOVE WS-HASH-AVAIL              TO BCT-HASH-AVAIL
           MOVE WS-HASH-SPENDING           TO BCT-HASH-SPENDING
           MOVE WS-SERVE-N                 TO BCT-SERVE-N-COUNT
           PERFORM 5000-WRITE-BCP
           .
      *================================================================*
       5000-WRITE-BCP.
      *================================================================*
           WRITE BCPFILE-RECORD            FROM BCX-DETAIL-RECORD
           IF WS-BCPFILE-STATUS NOT = '00'
               MOVE '5000-WRITE-BCP'       TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'BCPFILE WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-BCPFILE-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-RECORDS-WRITTEN
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
       7100-EXCEPTION.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE ACM-SORT-CODE              TO EL-SORT-CODE
           MOVE ACM-ACCT-NO                TO EL-ACCT-NO
           MOVE ACM-CIF-NO                 TO EL-CIF-NO
           MOVE ACM-LEDGER-BAL             TO EL-LEDGER
           WRITE REPORT-RECORD             FROM WS-EXCEPTION-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-HASH-LINE.
      *================================================================*
           WRITE REPORT-RECORD             FROM WS-HASH-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-NEXT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   CONTINUE
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-NEXT'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-ACCTS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SKIPPED - CLOSED'         TO TL-LABEL
           MOVE WS-CLOSED-SKIPPED          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SKIPPED - LOAN / MORTGAGE SHADOW' TO TL-LABEL
           MOVE WS-LOAN-SKIPPED            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE SPACES                 TO TL-LABEL
               STRING 'EXTRACTED - TYPE ' WS-TYPE-CODE (WS-I)
                      DELIMITED BY SIZE
                      INTO TL-LABEL
               END-STRING
               MOVE WS-TT-COUNT (WS-I)     TO TL-COUNT
               MOVE WS-TT-LEDGER (WS-I)    TO TL-AMOUNT
               WRITE REPORT-RECORD         FROM WS-TOTAL-LINE
           END-PERFORM
           MOVE 'SERVE INDICATOR Y'        TO TL-LABEL
           MOVE WS-SERVE-Y                 TO TL-COUNT
           MOVE WS-TOTAL-CONT-LIMIT        TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SERVE INDICATOR N (REFER)' TO TL-LABEL
           MOVE WS-SERVE-N                 TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE SPACES                 TO TL-LABEL
               STRING '  ' WS-RSN-CODE (WS-I) ' ' WS-RSN-TEXT (WS-I)
                      DELIMITED BY SIZE
                      INTO TL-LABEL
               END-STRING
               MOVE WS-RSN-COUNT (WS-I)    TO TL-COUNT
               WRITE REPORT-RECORD         FROM WS-TOTAL-LINE
           END-PERFORM
           MOVE 'EXPIRED LIMITS EXCLUDED'  TO TL-LABEL
           MOVE WS-EXPIRED-LIMITS          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NON-STERLING ACCOUNTS'    TO TL-LABEL
           MOVE WS-NON-STERLING            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCEPTION - CUSTOMER NOT ON CIF' TO TL-LABEL
           MOVE WS-CIF-NOT-FOUND           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCEPTION - NON-NUMERIC ACCOUNT NO' TO TL-LABEL
           MOVE WS-BAD-ACCT-NO             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RECORDS WRITTEN (INCL HEADER/TRAILER)'
                                           TO TL-LABEL
           MOVE WS-RECORDS-WRITTEN         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

      *    HASH TOTALS - MUST MATCH THE BCPVIEW LOAD LOG AT THE DR
      *    SITE. OPERATIONS TICK THESE ON THE BCP DAILY CHECKLIST.
           MOVE SPACES                     TO WS-HASH-LINE
           WRITE REPORT-RECORD             FROM WS-HASH-LINE
           MOVE 'TRAILER HASH - DETAIL RECORDS' TO HL-LABEL
           MOVE WS-HASH-COUNT              TO HL-VALUE
           PERFORM 7200-HASH-LINE
           MOVE 'TRAILER HASH - ACCOUNT NUMBERS' TO HL-LABEL
           MOVE WS-HASH-ACCT-NO            TO HL-VALUE
           PERFORM 7200-HASH-LINE
           MOVE 'TRAILER HASH - LEDGER BALANCES' TO HL-LABEL
           MOVE WS-HASH-LEDGER             TO HL-VALUE
           PERFORM 7200-HASH-LINE
           MOVE 'TRAILER HASH - AVAILABLE BALANCES' TO HL-LABEL
           MOVE WS-HASH-AVAIL              TO HL-VALUE
           PERFORM 7200-HASH-LINE
           MOVE 'TRAILER HASH - SPENDING POWER' TO HL-LABEL
           MOVE WS-HASH-SPENDING           TO HL-VALUE
           PERFORM 7200-HASH-LINE
           MOVE 'MEMO - HOLDS'             TO HL-LABEL
           MOVE WS-TOTAL-HOLDS             TO HL-VALUE
           PERFORM 7200-HASH-LINE
           MOVE 'MEMO - UNCLEARED'         TO HL-LABEL
           MOVE WS-TOTAL-UNCLEARED         TO HL-VALUE
           PERFORM 7200-HASH-LINE
           MOVE 'MEMO - ARRANGED OD LIMITS' TO HL-LABEL
           MOVE WS-TOTAL-OD-LIMITS         TO HL-VALUE
           PERFORM 7200-HASH-LINE

           CLOSE ACCTMAST-FILE CIFMAST-FILE BCPFILE-FILE REPORT-FILE
           DISPLAY 'BCPEXT10 - DETAIL RECORDS ' WS-HASH-COUNT
           DISPLAY 'BCPEXT10 - HASH ACCT NO   ' WS-HASH-ACCT-NO
           DISPLAY 'BCPEXT10 - HASH LEDGER    ' WS-HASH-LEDGER
           IF WS-CIF-NOT-FOUND > ZERO
           OR WS-BAD-ACCT-NO > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
