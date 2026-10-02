       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFSEG010.
       AUTHOR.        S A TURNER.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2009-02-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFSEG010                                            *
      * TITLE    : CIF - MONTHLY CUSTOMER SEGMENTATION                 *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CFSEG01 (MONTHLY - FIRST SATURDAY, BEFORE MIEXT01)  *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA MASTER, READ IN CIF ORDER VIA THE    *
      *                       ALTERNATE KEY (DD ACCTMAS1 = CIF PATH)   *
      *            LOANMAST - LOAN MASTER, RANDOM BY CIF (LOANMAS1)    *
      *            PARMIN   - OPTIONAL THRESHOLD OVERRIDES             *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : CIFMAST  - CIF MASTER (RANDOM) - CIF-SEGMENT-CD     *
      * OUTPUT   : CHANGES  - SEGMENT CHANGES FOR CRM (FB 80)          *
      *            REPORT   - SEGMENT MIGRATION MATRIX (FBA 133)       *
      *----------------------------------------------------------------*
      * SEGMENT CODES (CIF-SEGMENT-CD, FIRST 8 BYTES USED):            *
      *   BUSINESS  BUSINESS CUSTOMER (CIF-BUSINESS)                   *
      *   PREMIER   TOTAL DEPOSITS >= 75,000 OR INCOME >= 75,000       *
      *   PLUS      TOTAL DEPOSITS >= 10,000 OR A LOAN >= 10,000       *
      *   STUDENT   AGE 18-25 HOLDING A STUDENT PRODUCT ('STU' PREFIX) *
      *   BASIC     ONLY BASIC BANK ACCOUNTS ('BBA' PREFIX)            *
      *   DORMANT   EVERY OPEN ACCOUNT DORMANT                         *
      *   STANDARD  EVERYONE ELSE                                      *
      *   INACTIVE  NO OPEN ACCOUNTS OR LOANS                          *
      * THE FIRST MATCH IN THAT ORDER WINS. PREMIER CUSTOMERS ARE NOT  *
      * DOWNGRADED UNTIL THEY HAVE BEEN BELOW THE THRESHOLD FOR THREE  *
      * RUNS (COUNTER IN CIF-SEGMENT-CD BYTES 9-10) - THEY KEEP THEIR  *
      * RELATIONSHIP MANAGER THROUGH A TEMPORARY DIP.                  *
      *                                                                *
      * KNOWN ISSUE PIR 2010/041 OPEN: JOINT ACCOUNTS COUNT ONLY FOR   *
      * THE PRIMARY CIF (THE HOST HAS ONE CIF PER ACCOUNT).            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2009-02-02 SAT  ORIGINAL VERSION (PREMIER BANKING LAUNCH)      *
      * 2010-09-06 JNB  LOANS INCLUDED (PERSONAL LOANS ON THE HOST)    *
      * 2014-11-03 PDQ  BASIC BANK ACCOUNT SEGMENT (PAYMENT ACCOUNTS   *
      *                 DIRECTIVE - BASIC ACCOUNTS NOT MARKETED TO)    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
           SELECT CHANGES-FILE     ASSIGN TO CHANGES
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CHANGES-STATUS.
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
       FD  LOANMAST-FILE.
           COPY LNACCREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD.
           05  PARM-KEYWORD                PIC X(08).
               88  PARM-PREMIER                VALUE 'PREMIER='.
               88  PARM-PLUS                   VALUE 'PLUS=   '.
           05  PARM-VALUE                  PIC 9(09).
           05  FILLER                      PIC X(63).
       FD  CHANGES-FILE
           RECORDING MODE IS F.
       01  CHANGES-RECORD                  PIC X(80).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFSEG010'.
      *
       01  WS-THRESHOLDS.
           05  WS-PREMIER-THRESHOLD        PIC S9(11)V99 COMP-3
                                           VALUE +75000.00.
           05  WS-PLUS-THRESHOLD           PIC S9(11)V99 COMP-3
                                           VALUE +10000.00.
           05  WS-PREMIER-GRACE-RUNS       PIC 9(02) VALUE 03.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00' '02'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00' '02'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-PARMIN-STATUS            PIC X(02).
           05  WS-CHANGES-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CURR-CIF                 PIC X(12) VALUE LOW-VALUES.
           05  WS-AGE                      PIC 9(03).
           05  WS-NEW-SEGMENT              PIC X(08).
           05  WS-OLD-SEGMENT              PIC X(08).
           05  WS-GRACE-COUNT              PIC 9(02).
           05  WS-OLD-IDX                  PIC S9(04) COMP.
           05  WS-NEW-IDX                  PIC S9(04) COMP.
           05  WS-I                        PIC S9(04) COMP.
           05  WS-J                        PIC S9(04) COMP.
      *
      *---- PER-CUSTOMER ACCUMULATORS ---------------------------------*
       01  WS-CUSTOMER.
           05  WC-OPEN-ACCOUNTS            PIC S9(04) COMP.
           05  WC-DORMANT-ACCOUNTS         PIC S9(04) COMP.
           05  WC-BASIC-ACCOUNTS           PIC S9(04) COMP.
           05  WC-STUDENT-ACCOUNTS         PIC S9(04) COMP.
           05  WC-DEPOSITS                 PIC S9(13)V99 COMP-3.
           05  WC-BORROWING                PIC S9(13)V99 COMP-3.
           05  WC-LARGEST-LOAN             PIC S9(13)V99 COMP-3.
           05  WC-OPEN-LOANS               PIC S9(04) COMP.
      *
      *---- SEGMENTS AND MIGRATION MATRIX -----------------------------*
       01  WS-SEGMENT-VALUES.
           05  FILLER                      PIC X(08) VALUE 'BUSINESS'.
           05  FILLER                      PIC X(08) VALUE 'PREMIER '.
           05  FILLER                      PIC X(08) VALUE 'PLUS    '.
           05  FILLER                      PIC X(08) VALUE 'STUDENT '.
           05  FILLER                      PIC X(08) VALUE 'BASIC   '.
           05  FILLER                      PIC X(08) VALUE 'DORMANT '.
           05  FILLER                      PIC X(08) VALUE 'STANDARD'.
           05  FILLER                      PIC X(08) VALUE 'INACTIVE'.
           05  FILLER                      PIC X(08) VALUE 'UNSET   '.
       01  WS-SEGMENT-TABLE REDEFINES WS-SEGMENT-VALUES.
           05  WS-SEGMENT                  PIC X(08) OCCURS 9 TIMES.
       01  WS-MATRIX.
           05  WS-MX-ROW                   OCCURS 9 TIMES.
               10  WS-MX-CELL              PIC S9(07) COMP-3
                                           OCCURS 9 TIMES.
      *
       01  WS-CHANGE.
           05  CH-CIF-NO                   PIC X(12).
           05  CH-OLD-SEGMENT              PIC X(08).
           05  CH-NEW-SEGMENT              PIC X(08).
           05  CH-EFFECTIVE-DATE           PIC 9(08).
           05  CH-DEPOSITS                 PIC 9(11)V99.
           05  CH-BORROWING                PIC 9(11)V99.
           05  CH-HOME-BRANCH              PIC X(06).
           05  FILLER                      PIC X(12).
      *
       01  WS-COUNTERS.
           05  WS-CUSTOMERS                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CHANGED                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-GRACE-HELD               PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-CIF                   PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCOUNTS-READ            PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CFSEG010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - CUSTOMER SEGMENT MIGRATION'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-MATRIX-HEADING.
           05  FILLER                      PIC X(01) VALUE '0'.
           05  FILLER                      PIC X(12) VALUE 'FROM \ TO'.
           05  MH-SEGMENT                  PIC X(11) OCCURS 9 TIMES.
           05  FILLER                      PIC X(21) VALUE SPACES.
       01  WS-MATRIX-LINE.
           05  ML-CC                       PIC X(01) VALUE ' '.
           05  ML-SEGMENT                  PIC X(12).
           05  ML-CELL                     PIC ZZZ,ZZZ,ZZ9
                                           OCCURS 9 TIMES.
           05  FILLER                      PIC X(21) VALUE SPACES.
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
           PERFORM 2000-PROCESS-ACCOUNT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 3000-END-CUSTOMER THRU 3000-EXIT
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       LOANMAST-FILE
                       PARMIN-FILE
                       DATECARD-FILE
                I-O    CIFMAST-FILE
                OUTPUT CHANGES-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-LOANMAST-OK
           OR NOT WS-CIFMAST-OK
           OR WS-CHANGES-STATUS NOT = '00'
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

           IF WS-PARMIN-STATUS = '00'
               READ PARMIN-FILE
               PERFORM UNTIL WS-PARMIN-STATUS NOT = '00'
                   EVALUATE TRUE
                       WHEN PARM-PREMIER AND PARM-VALUE NUMERIC
                           MOVE PARM-VALUE TO WS-PREMIER-THRESHOLD
                       WHEN PARM-PLUS AND PARM-VALUE NUMERIC
                           MOVE PARM-VALUE TO WS-PLUS-THRESHOLD
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
                   READ PARMIN-FILE
               END-PERFORM
               CLOSE PARMIN-FILE
           END-IF
           DISPLAY 'CFSEG010 - PREMIER ' WS-PREMIER-THRESHOLD
                   ' PLUS ' WS-PLUS-THRESHOLD

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
                   MOVE ZERO               TO WS-MX-CELL (WS-I WS-J)
               END-PERFORM
           END-PERFORM

      *    READ THE ACCOUNT MASTER IN CIF ORDER (ALTERNATE KEY).
           MOVE LOW-VALUES                 TO ACM-CIF-NO
           START ACCTMAST-FILE KEY NOT < ACM-CIF-NO
           IF NOT WS-ACCTMAST-OK
               SET WS-END-OF-ACCOUNTS      TO TRUE
           ELSE
               PERFORM 8000-READ-ACCOUNT
           END-IF
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           IF ACM-CIF-NO NOT = WS-CURR-CIF
               PERFORM 3000-END-CUSTOMER THRU 3000-EXIT
               MOVE ACM-CIF-NO             TO WS-CURR-CIF
               INITIALIZE WS-CUSTOMER
           END-IF
           IF NOT ACM-CLOSED
               ADD 1                       TO WC-OPEN-ACCOUNTS
               IF ACM-DORMANT
                   ADD 1                   TO WC-DORMANT-ACCOUNTS
               END-IF
               IF ACM-PROD-CD (1:3) = 'BBA'
                   ADD 1                   TO WC-BASIC-ACCOUNTS
               END-IF
               IF ACM-PROD-CD (1:3) = 'STU'
                   ADD 1                   TO WC-STUDENT-ACCOUNTS
               END-IF
               IF ACM-LEDGER-BAL > ZERO
                   ADD ACM-LEDGER-BAL      TO WC-DEPOSITS
               ELSE
                   SUBTRACT ACM-LEDGER-BAL FROM WC-BORROWING
               END-IF
           END-IF
           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       3000-END-CUSTOMER.
      *================================================================*
           IF WS-CURR-CIF = LOW-VALUES
               GO TO 3000-EXIT
           END-IF
           ADD 1                           TO WS-CUSTOMERS
           PERFORM 3100-ADD-LOANS
           MOVE WS-CURR-CIF                TO CIF-NO
           READ CIFMAST-FILE
           IF NOT WS-CIFMAST-OK
               ADD 1                       TO WS-NO-CIF
               GO TO 3000-EXIT
           END-IF
           IF CIF-DECEASED OR CIF-MERGED-INTO NOT = SPACES
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-DECIDE-SEGMENT

           MOVE CIF-SEGMENT-CD (1:8)       TO WS-OLD-SEGMENT
           IF CIF-SEGMENT-CD (9:2) NUMERIC
               MOVE CIF-SEGMENT-CD (9:2)   TO WS-GRACE-COUNT
           ELSE
               MOVE ZERO                   TO WS-GRACE-COUNT
           END-IF

      *    PREMIER GRACE PERIOD.
           IF WS-OLD-SEGMENT = 'PREMIER '
           AND WS-NEW-SEGMENT NOT = 'PREMIER '
           AND WS-NEW-SEGMENT NOT = 'INACTIVE'
               ADD 1                       TO WS-GRACE-COUNT
               IF WS-GRACE-COUNT < WS-PREMIER-GRACE-RUNS
                   MOVE 'PREMIER '         TO WS-NEW-SEGMENT
                   ADD 1                   TO WS-GRACE-HELD
               ELSE
                   MOVE ZERO               TO WS-GRACE-COUNT
               END-IF
           ELSE
               MOVE ZERO                   TO WS-GRACE-COUNT
           END-IF

           PERFORM 3300-MATRIX
           IF WS-NEW-SEGMENT NOT = WS-OLD-SEGMENT
               PERFORM 3400-WRITE-CHANGE
           END-IF
           IF WS-NEW-SEGMENT NOT = WS-OLD-SEGMENT
           OR CIF-SEGMENT-CD (9:2) NOT = WS-GRACE-COUNT
               MOVE WS-NEW-SEGMENT         TO CIF-SEGMENT-CD (1:8)
               MOVE WS-GRACE-COUNT         TO CIF-SEGMENT-CD (9:2)
               MOVE WS-BUS-DATE            TO CIF-LAST-MAINT-DATE
               MOVE ZERO                   TO CIF-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO CIF-LAST-MAINT-USER
               SET CIF-MAINT-BATCH         TO TRUE
               REWRITE CIF-CUSTOMER-RECORD
               IF NOT WS-CIFMAST-OK
                   MOVE '3000-END-CUSTOMER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST REWRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE CIF-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-ADD-LOANS.
      *================================================================*
           MOVE WS-CURR-CIF                TO LN-CIF-NO
           START LOANMAST-FILE KEY NOT < LN-CIF-NO
           IF NOT WS-LOANMAST-OK
               GO TO 3100-EXIT
           END-IF
           MOVE 'N'                        TO WS-END-BROWSE-IND
           PERFORM UNTIL WS-END-BROWSE
               READ LOANMAST-FILE NEXT RECORD
               IF NOT WS-LOANMAST-OK OR LN-CIF-NO NOT = WS-CURR-CIF
                   SET WS-END-BROWSE       TO TRUE
               ELSE
                   IF LN-OPEN-STATUS
                       ADD 1               TO WC-OPEN-LOANS
                       ADD LN-PRINCIPAL-BAL TO WC-BORROWING
                       IF LN-PRINCIPAL-BAL > WC-LARGEST-LOAN
                           MOVE LN-PRINCIPAL-BAL TO WC-LARGEST-LOAN
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       3100-EXIT.
           EXIT.
      *================================================================*
       3200-DECIDE-SEGMENT.
      *================================================================*
           MOVE ZERO                       TO WS-AGE
           IF CIF-DOB NUMERIC AND CIF-DOB > ZERO
               COMPUTE WS-AGE = (WS-BUS-DATE - CIF-DOB) / 10000
           END-IF
           EVALUATE TRUE
               WHEN WC-OPEN-ACCOUNTS = ZERO AND WC-OPEN-LOANS = ZERO
                   MOVE 'INACTIVE'         TO WS-NEW-SEGMENT
               WHEN CIF-BUSINESS
                   MOVE 'BUSINESS'         TO WS-NEW-SEGMENT
               WHEN WC-DEPOSITS >= WS-PREMIER-THRESHOLD
               WHEN CIF-ANN-INCOME >= WS-PREMIER-THRESHOLD
                   MOVE 'PREMIER '         TO WS-NEW-SEGMENT
               WHEN WC-DEPOSITS >= WS-PLUS-THRESHOLD
               WHEN WC-LARGEST-LOAN >= WS-PLUS-THRESHOLD
                   MOVE 'PLUS    '         TO WS-NEW-SEGMENT
               WHEN WC-STUDENT-ACCOUNTS > ZERO
               AND  WS-AGE >= 18 AND WS-AGE <= 25
                   MOVE 'STUDENT '         TO WS-NEW-SEGMENT
               WHEN WC-BASIC-ACCOUNTS = WC-OPEN-ACCOUNTS
               AND  WC-OPEN-ACCOUNTS > ZERO
                   MOVE 'BASIC   '         TO WS-NEW-SEGMENT
               WHEN WC-DORMANT-ACCOUNTS = WC-OPEN-ACCOUNTS
               AND  WC-OPEN-ACCOUNTS > ZERO
               AND  WC-OPEN-LOANS = ZERO
                   MOVE 'DORMANT '         TO WS-NEW-SEGMENT
               WHEN OTHER
                   MOVE 'STANDARD'         TO WS-NEW-SEGMENT
           END-EVALUATE
           .
      *================================================================*
       3300-MATRIX.
      *================================================================*
           MOVE 9                          TO WS-OLD-IDX WS-NEW-IDX
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               IF WS-SEGMENT (WS-I) = WS-OLD-SEGMENT
                   MOVE WS-I               TO WS-OLD-IDX
               END-IF
               IF WS-SEGMENT (WS-I) = WS-NEW-SEGMENT
                   MOVE WS-I               TO WS-NEW-IDX
               END-IF
           END-PERFORM
           ADD 1                   TO WS-MX-CELL (WS-OLD-IDX WS-NEW-IDX)
           .
      *================================================================*
       3400-WRITE-CHANGE.
      *================================================================*
           MOVE SPACES                     TO WS-CHANGE
           MOVE WS-CURR-CIF                TO CH-CIF-NO
           MOVE WS-OLD-SEGMENT             TO CH-OLD-SEGMENT
           MOVE WS-NEW-SEGMENT             TO CH-NEW-SEGMENT
           MOVE WS-BUS-DATE                TO CH-EFFECTIVE-DATE
           MOVE WC-DEPOSITS                TO CH-DEPOSITS
           MOVE WC-BORROWING               TO CH-BORROWING
           MOVE CIF-HOME-BRANCH-SC         TO CH-HOME-BRANCH
           WRITE CHANGES-RECORD            FROM WS-CHANGE
           IF WS-CHANGES-STATUS NOT = '00'
               MOVE '3400-WRITE-CHANGE'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CHANGES WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CHANGES-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-CHANGED
           .
      *================================================================*
       8000-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-ACCOUNTS-READ
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE SPACES                 TO MH-SEGMENT (WS-I)
               MOVE WS-SEGMENT (WS-I)      TO MH-SEGMENT (WS-I) (4:8)
           END-PERFORM
           WRITE REPORT-RECORD             FROM WS-MATRIX-HEADING
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE WS-SEGMENT (WS-I)      TO ML-SEGMENT
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
                   MOVE WS-MX-CELL (WS-I WS-J) TO ML-CELL (WS-J)
               END-PERFORM
               WRITE REPORT-RECORD         FROM WS-MATRIX-LINE
           END-PERFORM
           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-ACCOUNTS-READ           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CUSTOMERS SEGMENTED'      TO TL-LABEL
           MOVE WS-CUSTOMERS               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SEGMENT CHANGES (SENT TO CRM)' TO TL-LABEL
           MOVE WS-CHANGED                 TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PREMIER HELD IN GRACE PERIOD' TO TL-LABEL
           MOVE WS-GRACE-HELD              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS WITH NO CIF RECORD' TO TL-LABEL
           MOVE WS-NO-CIF                  TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE ACCTMAST-FILE LOANMAST-FILE CIFMAST-FILE
                 CHANGES-FILE REPORT-FILE
           DISPLAY 'CFSEG010 - CUSTOMERS ' WS-CUSTOMERS
                   ' CHANGED ' WS-CHANGED
           IF WS-NO-CIF > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
