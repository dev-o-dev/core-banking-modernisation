       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CSSIN010.
       AUTHOR.        P D QUINN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2013-06-17.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CSSIN010                                            *
      * TITLE    : CURRENT ACCOUNT SWITCH SERVICE - INBOUND MESSAGES   *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CSSDLY01 STEP CS010                                 *
      *----------------------------------------------------------------*
      * INPUT    : CASSIN   - TODAY'S MESSAGES FROM CASS (CSSRECS)     *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : SWITCHES - CASS SWITCH CONTROL (VSAM KSDS, RANDOM)  *
      *            ACCTMAST - DDA MASTER (RANDOM)                      *
      *            SOMAST   - STANDING ORDERS (RANDOM)                 *
      *            DDMAND   - DIRECT DEBIT MANDATES (RANDOM)           *
      * OUTPUT   : CSSPOST  - BALANCE TRANSFER CREDITS FOR PSEOD01     *
      *                       (PSTXNREC, FEED - DISP=MOD)              *
      *            NOTIFY   - CUSTOMER NOTIFICATIONS (FB 150)          *
      *            REPORT   - CASS INBOUND REPORT (FBA 133)            *
      *----------------------------------------------------------------*
      * MEGABANK AS NEW BANK (SWITCH RECORD DIRECTION 'I'):            *
      *   SWAK  OLD BANK ACCEPTED - STATUS AK                          *
      *   SWRJ  OLD BANK REJECTED - STATUS RJ, CUSTOMER NOTIFIED       *
      *   SOTR  A STANDING ORDER IS CREATED ON THE NEW ACCOUNT. ROUTE  *
      *         'I' IF THE BENEFICIARY IS A MEGABANK ACCOUNT, ELSE     *
      *         'F' (FASTER PAYMENTS). FIRST PAYMENT = CSI-SO-NEXT-DATE*
      *   DDTR  A MANDATE IS CREATED ON THE NEW ACCOUNT, SOURCE 'S'    *
      *         (SWITCH). NO AUDDIS 0C IS NEEDED - CASS TELLS THE      *
      *         ORIGINATOR (ACCOUNT REDIRECTION, BACS RULES 2013).     *
      *   BALT  CLOSING BALANCE CREDITED TO THE NEW ACCOUNT ('TFR ')   *
      *         ON THE SWITCH DATE. A NEGATIVE BALANCE (OVERDRAWN OLD  *
      *         ACCOUNT) IS NOT TRANSFERRED - IT STAYS WITH THE OLD    *
      *         BANK UNDER CASS RULES; IT IS REPORTED ONLY.            *
      *   SWCP  SWITCH COMPLETE - STATUS CP                            *
      * MEGABANK AS OLD BANK (DIRECTION 'O'):                          *
      *   SWRQ  A NEW SWITCH RECORD IS CREATED WITH STATUS RQ AFTER    *
      *         MATCHING THE CUSTOMER (SURNAME, DOB, POSTCODE) TO THE  *
      *         OLD ACCOUNT'S CIF. CSSOU010 SENDS SWAK / SWRJ TONIGHT. *
      *         MISMATCHES ARE REJECTED HERE WITH REASON 'NMAT'.       *
      * DUPLICATE SOTR / DDTR (A REPEATED CASS FILE) ARE IGNORED - THE *
      * SO / MANDATE KEY ALREADY EXISTS (STATUS 22).                   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2013-06-17 PDQ  ORIGINAL VERSION (CASS GO-LIVE 16/09/2013)     *
      * 2014-02-03 PDQ  SURNAME MATCH IGNORES PUNCTUATION (CASS        *
      *                 REJECT RATE 4% IN FIRST QUARTER)               *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CASSIN-FILE      ASSIGN TO CASSIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CASSIN-STATUS.
           SELECT SWITCHES-FILE    ASSIGN TO SWITCHES
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS SWF-KEY
                                   FILE STATUS IS WS-SWITCHES-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT SOMAST-FILE      ASSIGN TO SOMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS SOM-KEY
                                   FILE STATUS IS WS-SOMAST-STATUS.
           SELECT DDMAND-FILE      ASSIGN TO DDMAND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS DMM-KEY
                                   FILE STATUS IS WS-DDMAND-STATUS.
           SELECT CSSPOST-FILE     ASSIGN TO CSSPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CSSPOST-STATUS.
           SELECT NOTIFY-FILE      ASSIGN TO NOTIFY
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-NOTIFY-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CASSIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CASSIN-RECORD                   PIC X(300).
       FD  SWITCHES-FILE.
       01  SWITCHES-RECORD.
           05  SWF-KEY                     PIC X(18).
           05  FILLER                      PIC X(232).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  SOMAST-FILE.
           COPY SOMSTREC.
       FD  DDMAND-FILE.
           COPY DMMNDREC.
       FD  CSSPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CSSPOST-RECORD                  PIC X(200).
       FD  NOTIFY-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  NOTIFY-RECORD                   PIC X(150).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CSSIN010'.
      *
       01  WS-CONSTANTS.
           05  WS-REDIRECT-MONTHS          PIC 9(02) VALUE 36.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00012000.
           05  WS-ALPHA                    PIC X(26)
               VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05  WS-LOWER                    PIC X(26)
               VALUE 'abcdefghijklmnopqrstuvwxyz'.
      *
       01  WS-FILE-STATUSES.
           05  WS-CASSIN-STATUS            PIC X(02).
               88  WS-CASSIN-OK                VALUE '00'.
               88  WS-CASSIN-EOF               VALUE '10'.
           05  WS-SWITCHES-STATUS          PIC X(02).
               88  WS-SWITCHES-OK              VALUE '00'.
               88  WS-SWITCHES-NOTFND          VALUE '23'.
               88  WS-SWITCHES-DUPKEY          VALUE '22'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-SOMAST-STATUS            PIC X(02).
               88  WS-SOMAST-OK                VALUE '00'.
               88  WS-SOMAST-NOTFND            VALUE '23'.
               88  WS-SOMAST-DUPKEY            VALUE '22'.
           05  WS-DDMAND-STATUS            PIC X(02).
               88  WS-DDMAND-OK                VALUE '00'.
               88  WS-DDMAND-DUPKEY            VALUE '22'.
           05  WS-CSSPOST-STATUS           PIC X(02).
           05  WS-NOTIFY-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-TRAILER-IND              PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-RESULT                   PIC X(08).
           05  WS-NOTE                     PIC X(40).
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-NEW-SEQ                  PIC 9(04).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-NORM-IN                  PIC X(40).
           05  WS-NORM-OUT                 PIC X(40).
           05  WS-NAME-A                   PIC X(40).
           05  WS-NAME-B                   PIC X(40).
           05  WS-I                        PIC S9(04) COMP.
           05  WS-J                        PIC S9(04) COMP.
           05  WS-CHAR                     PIC X(01).
           05  WS-MSG-COUNT                PIC 9(09) VALUE ZERO.
           05  WS-SAVED-SO-KEY             PIC X(18).
           05  WS-MA-DATE                  PIC 9(08).
           05  WS-MA-DATE-R REDEFINES WS-MA-DATE.
               10  WS-MA-CCYY              PIC 9(04).
               10  WS-MA-MMDD              PIC 9(04).
      *
           COPY CSSRECS.
      *
       01  WS-NOTIFICATION.
           05  NF-NOTICE-CD                PIC X(04).
           05  NF-ISSUE-DATE               PIC 9(08).
           05  NF-SWITCH-ID                PIC X(18).
           05  NF-CIF-NO                   PIC X(12).
           05  NF-SORT-CODE                PIC X(06).
           05  NF-ACCT-NO                  PIC X(08).
           05  NF-SWITCH-DATE              PIC 9(08).
           05  NF-REASON                   PIC X(04).
           05  NF-AMOUNT                   PIC 9(11)V99.
           05  NF-COUNT                    PIC 9(03).
           05  FILLER                      PIC X(66).
      *
       01  WS-COUNTERS.
           05  WS-MSGS-READ                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCEPTED                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-REJECTED                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-SOS-CREATED              PIC S9(07) COMP-3 VALUE 0.
           05  WS-DDS-CREATED              PIC S9(07) COMP-3 VALUE 0.
           05  WS-DUPLICATES               PIC S9(07) COMP-3 VALUE 0.
           05  WS-BALANCES                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-BALANCE-TOTAL            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-COMPLETED                PIC S9(07) COMP-3 VALUE 0.
           05  WS-REQUESTS-IN              PIC S9(07) COMP-3 VALUE 0.
           05  WS-REQUESTS-NOMATCH         PIC S9(07) COMP-3 VALUE 0.
           05  WS-ERRORS                   PIC S9(07) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CSSIN010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - CASS INBOUND MESSAGES'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(06) VALUE 'MSG'.
           05  FILLER            PIC X(20) VALUE 'SWITCH ID'.
           05  FILLER            PIC X(17) VALUE 'OLD ACCOUNT'.
           05  FILLER            PIC X(17) VALUE 'NEW ACCOUNT'.
           05  FILLER            PIC X(10) VALUE 'SW DATE'.
           05  FILLER            PIC X(10) VALUE 'RESULT'.
           05  FILLER            PIC X(52) VALUE 'NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-MSG                      PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SWITCH-ID                PIC X(18).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-OLD-SC                   PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-OLD-ACCT                 PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NEW-SC                   PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-NEW-ACCT                 PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SW-DATE                  PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RESULT                   PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(40).
           05  FILLER                      PIC X(12) VALUE SPACES.
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
           PERFORM 2000-PROCESS-MESSAGE THRU 2000-EXIT
               UNTIL WS-END-OF-INPUT
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  CASSIN-FILE
                       CIFMAST-FILE
                       DATECARD-FILE
                I-O    SWITCHES-FILE
                       ACCTMAST-FILE
                       SOMAST-FILE
                       DDMAND-FILE
                EXTEND CSSPOST-FILE
                OUTPUT NOTIFY-FILE
                       REPORT-FILE
           IF NOT WS-CASSIN-OK
           OR NOT WS-SWITCHES-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-SOMAST-OK
           OR NOT WS-DDMAND-OK
           OR WS-CSSPOST-STATUS NOT = '00'
           OR WS-NOTIFY-STATUS NOT = '00'
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
           PERFORM 8000-READ-MESSAGE
           IF WS-END-OF-INPUT OR NOT CSI-HEADER
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'CASS FILE HAS NO HEADER' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           DISPLAY 'CSSIN010 - CASS FILE ' CSI-FILE-DATE
                   ' SEQ ' CSI-FILE-SEQ
           PERFORM 8000-READ-MESSAGE
           .
      *================================================================*
       2000-PROCESS-MESSAGE.
      *================================================================*
           MOVE SPACES                     TO WS-RESULT WS-NOTE
           IF CSI-TRAILER
               SET WS-TRAILER-SEEN         TO TRUE
               IF CSI-RECORD-COUNT NOT = WS-MSG-COUNT
                   MOVE '2000-PROCESS-MESSAGE' TO ABN-PARAGRAPH
                   MOVE 1004               TO ABN-ABEND-CODE
                   MOVE 'CASS TRAILER COUNT MISMATCH'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               END-IF
               PERFORM 8000-READ-MESSAGE
               GO TO 2000-EXIT
           END-IF

           IF CSI-SWITCH-REQUEST
               PERFORM 3000-SWITCH-REQUEST THRU 3000-EXIT
               PERFORM 7100-REPORT-DETAIL
               PERFORM 8000-READ-MESSAGE
               GO TO 2000-EXIT
           END-IF

           MOVE CSI-SWITCH-ID              TO SWF-KEY
           READ SWITCHES-FILE INTO CSW-SWITCH-RECORD
           IF NOT WS-SWITCHES-OK
               ADD 1                       TO WS-ERRORS
               MOVE 'ERROR'                TO WS-RESULT
               MOVE 'UNKNOWN SWITCH ID - REFER TO CASS DESK'
                                           TO WS-NOTE
               PERFORM 7100-REPORT-DETAIL
               PERFORM 8000-READ-MESSAGE
               GO TO 2000-EXIT
           END-IF

           EVALUATE TRUE
               WHEN CSI-SWITCH-ACCEPTED
                   SET CSW-ACCEPTED        TO TRUE
                   MOVE WS-BUS-DATE        TO CSW-ACCEPT-DATE
                   MOVE CSI-SWITCH-DATE    TO CSW-SWITCH-DATE
                   ADD 1                   TO WS-ACCEPTED
                   MOVE 'ACCEPTED'         TO WS-RESULT
               WHEN CSI-SWITCH-REJECTED
                   SET CSW-REJECTED        TO TRUE
                   MOVE CSI-REASON         TO CSW-REJECT-REASON
                   ADD 1                   TO WS-REJECTED
                   MOVE 'REJECTED'         TO WS-RESULT
                   MOVE 'SWRJ'             TO NF-NOTICE-CD
                   PERFORM 6000-NOTIFY
               WHEN CSI-SO-TRANSFER
                   PERFORM 4000-CREATE-SO THRU 4000-EXIT
               WHEN CSI-DD-TRANSFER
                   PERFORM 4500-CREATE-MANDATE THRU 4500-EXIT
               WHEN CSI-BALANCE-TRANSFER
                   PERFORM 5000-BALANCE THRU 5000-EXIT
               WHEN CSI-SWITCH-COMPLETE
                   SET CSW-COMPLETE        TO TRUE
                   MOVE WS-BUS-DATE        TO CSW-COMPLETE-DATE
                   ADD 1                   TO WS-COMPLETED
                   MOVE 'COMPLETE'         TO WS-RESULT
                   MOVE 'SWCP'             TO NF-NOTICE-CD
                   PERFORM 6000-NOTIFY
               WHEN OTHER
                   ADD 1                   TO WS-ERRORS
                   MOVE 'ERROR'            TO WS-RESULT
                   MOVE 'UNKNOWN MESSAGE TYPE' TO WS-NOTE
           END-EVALUATE

           MOVE WS-BUS-DATE                TO CSW-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO CSW-LAST-MAINT-PGM
           REWRITE SWITCHES-RECORD         FROM CSW-SWITCH-RECORD
           IF NOT WS-SWITCHES-OK
               MOVE '2000-PROCESS-MESSAGE' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'SWITCHES REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-SWITCHES-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           PERFORM 7100-REPORT-DETAIL
           PERFORM 8000-READ-MESSAGE
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-SWITCH-REQUEST.
      *================================================================*
      *    WE ARE THE OLD BANK. FIND THE ACCOUNT AND CHECK THE
      *    CUSTOMER DETAILS CASS SENT AGAINST THE CIF.
           ADD 1                           TO WS-REQUESTS-IN
           MOVE SPACES                     TO CSW-SWITCH-RECORD
           MOVE CSI-SWITCH-ID              TO CSW-SWITCH-ID
           SET CSW-OUTGOING                TO TRUE
           MOVE CSI-SWITCH-DATE            TO CSW-SWITCH-DATE
           MOVE CSI-OLD-SORT-CODE          TO CSW-OLD-SORT-CODE
           MOVE CSI-OLD-ACCT-NO            TO CSW-OLD-ACCT-NO
           MOVE CSI-NEW-SORT-CODE          TO CSW-NEW-SORT-CODE
           MOVE CSI-NEW-ACCT-NO            TO CSW-NEW-ACCT-NO
           MOVE ZERO                       TO CSW-SO-COUNT CSW-DD-COUNT
                                              CSW-BALANCE-AMT
                                              CSW-ACCEPT-DATE
                                              CSW-COMPLETE-DATE
                                              CSW-REDIRECT-END
           MOVE WS-BUS-DATE                TO CSW-REQUEST-DATE
                                              CSW-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO CSW-LAST-MAINT-PGM
           SET CSW-REQUESTED               TO TRUE

           MOVE CSI-OLD-SORT-CODE          TO ACM-SORT-CODE
           MOVE CSI-OLD-ACCT-NO            TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE 'NACC'             TO CSW-REJECT-REASON
                   MOVE 'NO SUCH ACCOUNT'  TO WS-NOTE
               WHEN NOT ACM-CURRENT
                   MOVE 'NCUR'             TO CSW-REJECT-REASON
                   MOVE 'NOT A CURRENT ACCOUNT' TO WS-NOTE
               WHEN ACM-CLOSED OR ACM-PENDING-CLOSE
                   MOVE 'ACLS'             TO CSW-REJECT-REASON
                   MOVE 'ACCOUNT CLOSED / CLOSING' TO WS-NOTE
               WHEN ACM-FROZEN
                   MOVE 'AFRZ'             TO CSW-REJECT-REASON
                   MOVE 'ACCOUNT FROZEN' TO WS-NOTE
               WHEN OTHER
                   MOVE ACM-CIF-NO         TO CSW-CIF-NO CIF-NO
                   READ CIFMAST-FILE
                   IF NOT WS-CIFMAST-OK
                       MOVE 'NMAT'         TO CSW-REJECT-REASON
                       MOVE 'NO CUSTOMER RECORD' TO WS-NOTE
                   ELSE
                       PERFORM 3100-MATCH-CUSTOMER
                   END-IF
           END-EVALUATE

           IF CSW-REJECT-REASON NOT = SPACES
      *        STAYS 'RQ' WITH A REASON - CSSOU010 SENDS SWRJ.
               ADD 1                       TO WS-REQUESTS-NOMATCH
               MOVE 'TO REJECT'            TO WS-RESULT
           ELSE
               MOVE 'TO ACCEPT'            TO WS-RESULT
               MOVE 'SWITCH ACCEPTED - SWAK SENT TONIGHT' TO WS-NOTE
           END-IF
           WRITE SWITCHES-RECORD           FROM CSW-SWITCH-RECORD
           EVALUATE TRUE
               WHEN WS-SWITCHES-OK
                   CONTINUE
               WHEN WS-SWITCHES-DUPKEY
                   ADD 1                   TO WS-DUPLICATES
                   MOVE 'DUP'              TO WS-RESULT
                   MOVE 'REQUEST ALREADY HELD - IGNORED' TO WS-NOTE
               WHEN OTHER
                   MOVE '3000-SWITCH-REQUEST' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SWITCHES WRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-SWITCHES-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-MATCH-CUSTOMER.
      *================================================================*
           MOVE CIF-LAST-NM                TO WS-NORM-IN
           PERFORM 3200-NORMALISE
           MOVE WS-NORM-OUT                TO WS-NAME-A
           MOVE CSI-CUST-SURNAME           TO WS-NORM-IN
           PERFORM 3200-NORMALISE
           MOVE WS-NORM-OUT                TO WS-NAME-B
           EVALUATE TRUE
               WHEN WS-NAME-A NOT = WS-NAME-B
                   MOVE 'NMAT'             TO CSW-REJECT-REASON
                   MOVE 'SURNAME DOES NOT MATCH' TO WS-NOTE
               WHEN CIF-DOB NUMERIC
               AND  CSI-CUST-DOB NUMERIC
               AND  CIF-DOB NOT = CSI-CUST-DOB
                   MOVE 'NMAT'             TO CSW-REJECT-REASON
                   MOVE 'DATE OF BIRTH DOES NOT MATCH' TO WS-NOTE
               WHEN OTHER
                   MOVE CIF-POST-CD        TO WS-NORM-IN
                   PERFORM 3200-NORMALISE
                   MOVE WS-NORM-OUT        TO WS-NAME-A
                   MOVE CSI-CUST-POST-CD   TO WS-NORM-IN
                   PERFORM 3200-NORMALISE
                   IF WS-NAME-A NOT = WS-NORM-OUT
                       MOVE 'NMAT'         TO CSW-REJECT-REASON
                       MOVE 'POSTCODE DOES NOT MATCH' TO WS-NOTE
                   END-IF
           END-EVALUATE
           .
      *================================================================*
       3200-NORMALISE.
      *================================================================*
           INSPECT WS-NORM-IN CONVERTING WS-LOWER TO WS-ALPHA
           MOVE SPACES                     TO WS-NORM-OUT
           MOVE ZERO                       TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 40
               MOVE WS-NORM-IN (WS-I:1)    TO WS-CHAR
               IF (WS-CHAR >= 'A' AND WS-CHAR <= 'Z')
               OR (WS-CHAR >= '0' AND WS-CHAR <= '9')
                   ADD 1                   TO WS-J
                   MOVE WS-CHAR            TO WS-NORM-OUT (WS-J:1)
               END-IF
           END-PERFORM
           .
      *================================================================*
       4000-CREATE-SO.
      *================================================================*
      *---- NEXT SEQUENCE FROM THE ACCOUNT'S CONTROL RECORD -----------*
           MOVE CSW-NEW-SORT-CODE          TO SOM-PAYER-SORT-CODE
           MOVE CSW-NEW-ACCT-NO            TO SOM-PAYER-ACCT-NO
           MOVE ZERO                       TO SOM-SO-SEQ
           READ SOMAST-FILE
           EVALUATE TRUE
               WHEN WS-SOMAST-OK
                   ADD 1                   TO SOM-LAST-SO-SEQ
                                              SOM-ACTIVE-COUNT
                   MOVE SOM-LAST-SO-SEQ    TO WS-NEW-SEQ
                   REWRITE SOM-SO-RECORD
               WHEN WS-SOMAST-NOTFND
                   MOVE SPACES             TO SOM-DATA
                   MOVE 1                  TO SOM-LAST-SO-SEQ
                                              SOM-ACTIVE-COUNT
                   MOVE WS-BUS-DATE        TO SOM-CTL-CREATE-DATE
                   MOVE ZERO               TO SOM-NEXT-DUE-DATE
                   MOVE 1                  TO WS-NEW-SEQ
                   WRITE SOM-SO-RECORD
               WHEN OTHER
                   MOVE '4000-CREATE-SO'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SOMAST CONTROL READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-SOMAST-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE

           MOVE SPACES                     TO SOM-SO-RECORD
           MOVE CSW-NEW-SORT-CODE          TO SOM-PAYER-SORT-CODE
           MOVE CSW-NEW-ACCT-NO            TO SOM-PAYER-ACCT-NO
           MOVE WS-NEW-SEQ                 TO SOM-SO-SEQ
           MOVE CSI-SO-NEXT-DATE           TO SOM-NEXT-DUE-DATE
                                              SOM-FIRST-PAY-DATE
                                              SOM-NOMINAL-DUE-DATE
           SET SOM-ACTIVE                  TO TRUE
           MOVE CSI-SO-BENE-NAME           TO SOM-BENE-NAME
           MOVE CSI-SO-BENE-SORT-CODE      TO SOM-BENE-SORT-CODE
           MOVE CSI-SO-BENE-ACCT-NO        TO SOM-BENE-ACCT-NO
           MOVE CSI-SO-BENE-REF            TO SOM-BENE-REF
           MOVE CSI-SO-FREQ-CD             TO SOM-FREQ-CD
           IF NOT SOM-FREQ-VALID
               SET SOM-SUSPENDED           TO TRUE
               MOVE 'BADF'                 TO SOM-LAST-FAIL-RSN
               MOVE 'UNKNOWN FREQUENCY - SO SUSPENDED' TO WS-NOTE
           END-IF
           MOVE CSI-SO-NEXT-DATE (7:2)     TO SOM-NOMINAL-DAY
           MOVE CSI-SO-AMOUNT              TO SOM-FIRST-AMT
                                              SOM-REGULAR-AMT
                                              SOM-FINAL-AMT
           IF CSI-SO-FINAL-DATE NUMERIC
           AND CSI-SO-FINAL-DATE > ZERO
               MOVE CSI-SO-FINAL-DATE      TO SOM-FINAL-PAY-DATE
               MOVE ZERO                   TO SOM-PAYMENTS-REMAINING
           ELSE
               MOVE ZERO                   TO SOM-FINAL-PAY-DATE
               SET SOM-UNTIL-FURTHER-NOTICE TO TRUE
           END-IF
           MOVE ZERO                       TO SOM-PAYMENTS-MADE
                                              SOM-LAST-PAID-DATE
                                              SOM-LAST-PAID-AMT
                                              SOM-LAST-ATTEMPT-DATE
                                              SOM-RETRY-COUNT
                                              SOM-CONSEC-FAILS
                                              SOM-TOTAL-PAID
                                              SOM-AMEND-DATE
                                              SOM-CANCEL-DATE
           MOVE WS-BUS-DATE                TO SOM-CREATE-DATE
           MOVE 'CASS  '                   TO SOM-CREATE-CHANNEL
           MOVE WS-PROGRAM-ID              TO SOM-CREATE-USER

      *    ROUTE: INTERNAL IF THE BENEFICIARY IS ON OUR MASTER.
           MOVE SOM-KEY                    TO WS-SAVED-SO-KEY
           MOVE CSI-SO-BENE-SORT-CODE      TO ACM-SORT-CODE
           MOVE CSI-SO-BENE-ACCT-NO        TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF WS-ACCTMAST-OK
               SET SOM-ROUTE-INTERNAL      TO TRUE
           ELSE
               SET SOM-ROUTE-FPS           TO TRUE
           END-IF

           WRITE SOM-SO-RECORD
           EVALUATE TRUE
               WHEN WS-SOMAST-OK
                   ADD 1                   TO WS-SOS-CREATED
                                              CSW-SO-COUNT
                   MOVE 'SO ADDED'         TO WS-RESULT
                   SET CSW-TRANSFERRING    TO TRUE
               WHEN WS-SOMAST-DUPKEY
                   ADD 1                   TO WS-DUPLICATES
                   MOVE 'DUP'              TO WS-RESULT
                   MOVE 'SO KEY EXISTS - CHECK CONTROL RECORD'
                                           TO WS-NOTE
               WHEN OTHER
                   MOVE '4000-CREATE-SO'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'SOMAST WRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-SOMAST-STATUS   TO ABN-FILE-STATUS
                   MOVE WS-SAVED-SO-KEY    TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4500-CREATE-MANDATE.
      *================================================================*
           MOVE SPACES                     TO DMM-MANDATE-RECORD
           MOVE CSW-NEW-SORT-CODE          TO DMM-PAYER-SORT-CODE
           MOVE CSW-NEW-ACCT-NO            TO DMM-PAYER-ACCT-NO
           MOVE CSI-DD-ORIG-SUN            TO DMM-ORIG-SUN
           MOVE CSI-DD-PAYER-REF           TO DMM-PAYER-REF
           SET DMM-LIVE                    TO TRUE
           MOVE CSI-DD-ORIG-NAME           TO DMM-ORIG-NAME
           SET DMM-SRC-SWITCH              TO TRUE
           MOVE WS-BUS-DATE                TO DMM-SETUP-DATE
                                              DMM-LAST-MAINT-DATE
           MOVE ZERO                       TO DMM-FIRST-COLL-DATE
                                              DMM-COLL-COUNT
                                              DMM-COLL-TOTAL
                                              DMM-UNPAID-COUNT
                                              DMM-LAST-UNPAID-DATE
                                              DMM-INDEMNITY-COUNT
                                              DMM-MAX-AMT
                                              DMM-CANCEL-DATE
                                              DMM-ADDACS-SENT-DATE
      *    CARRY THE LAST COLLECTION SO THE FIRST-COLLECTION RULES
      *    (ADVANCE NOTICE) DO NOT TRIGGER AT THE NEW BANK.
           IF CSI-DD-LAST-COLL-DATE NUMERIC
               MOVE CSI-DD-LAST-COLL-DATE  TO DMM-LAST-COLL-DATE
               MOVE CSI-DD-LAST-COLL-AMT   TO DMM-LAST-COLL-AMT
           ELSE
               MOVE ZERO                   TO DMM-LAST-COLL-DATE
                                              DMM-LAST-COLL-AMT
           END-IF
           MOVE WS-PROGRAM-ID              TO DMM-LAST-MAINT-PGM
           WRITE DMM-MANDATE-RECORD
           EVALUATE TRUE
               WHEN WS-DDMAND-OK
                   ADD 1                   TO WS-DDS-CREATED
                                              CSW-DD-COUNT
                   MOVE 'DD ADDED'         TO WS-RESULT
                   SET CSW-TRANSFERRING    TO TRUE
               WHEN WS-DDMAND-DUPKEY
                   ADD 1                   TO WS-DUPLICATES
                   MOVE 'DUP'              TO WS-RESULT
                   MOVE 'MANDATE ALREADY HELD' TO WS-NOTE
               WHEN OTHER
                   MOVE '4500-CREATE-MANDATE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'DDMAND WRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-DDMAND-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       4500-EXIT.
           EXIT.
      *================================================================*
       5000-BALANCE.
      *================================================================*
           MOVE CSI-BAL-AMOUNT             TO WS-AMOUNT
           IF CSI-BAL-SIGN = '-'
               MOVE 'NONE'                 TO WS-RESULT
               MOVE 'OLD ACCOUNT OVERDRAWN - DEBT STAYS WITH OLD BANK'
                                           TO WS-NOTE
               GO TO 5000-EXIT
           END-IF
           MOVE WS-AMOUNT                  TO CSW-BALANCE-AMT
           IF WS-AMOUNT = ZERO
               MOVE 'NIL'                  TO WS-RESULT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE CSW-NEW-SORT-CODE          TO PST-SORT-CODE
           MOVE CSW-NEW-ACCT-NO            TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
                                              PST-VALUE-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'TFR '                     TO PST-TXN-CODE
           SET PST-CREDIT                  TO TRUE
           MOVE WS-AMOUNT                  TO PST-AMOUNT
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE 'CURRENT ACCOUNT SWITCH'   TO PST-NARRATIVE-1
           STRING 'FROM ' CSW-OLD-SORT-CODE ' ' CSW-OLD-ACCT-NO
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           MOVE CSW-SWITCH-ID              TO PST-EXT-REF
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE CSSPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-CSSPOST-STATUS NOT = '00'
               MOVE '5000-BALANCE'         TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CSSPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-CSSPOST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-BALANCES
           ADD WS-AMOUNT                   TO WS-BALANCE-TOTAL
           MOVE 'CREDITED'                 TO WS-RESULT
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       6000-NOTIFY.
      *================================================================*
           MOVE WS-BUS-DATE                TO NF-ISSUE-DATE
           MOVE CSW-SWITCH-ID              TO NF-SWITCH-ID
           MOVE CSW-CIF-NO                 TO NF-CIF-NO
           MOVE CSW-NEW-SORT-CODE          TO NF-SORT-CODE
           MOVE CSW-NEW-ACCT-NO            TO NF-ACCT-NO
           MOVE CSW-SWITCH-DATE            TO NF-SWITCH-DATE
           MOVE CSW-REJECT-REASON          TO NF-REASON
           MOVE CSW-BALANCE-AMT            TO NF-AMOUNT
           COMPUTE NF-COUNT = CSW-SO-COUNT + CSW-DD-COUNT
           WRITE NOTIFY-RECORD             FROM WS-NOTIFICATION
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
           MOVE CSI-MSG-TYPE               TO DL-MSG
           MOVE CSI-SWITCH-ID              TO DL-SWITCH-ID
           MOVE CSI-OLD-SORT-CODE          TO DL-OLD-SC
           MOVE CSI-OLD-ACCT-NO            TO DL-OLD-ACCT
           MOVE CSI-NEW-SORT-CODE          TO DL-NEW-SC
           MOVE CSI-NEW-ACCT-NO            TO DL-NEW-ACCT
           MOVE CSI-SWITCH-DATE            TO DL-SW-DATE
           MOVE WS-RESULT                  TO DL-RESULT
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-MESSAGE.
      *================================================================*
           READ CASSIN-FILE INTO CSI-CASS-MESSAGE
           EVALUATE TRUE
               WHEN WS-CASSIN-OK
                   ADD 1                   TO WS-MSGS-READ
                   IF NOT CSI-HEADER AND NOT CSI-TRAILER
                       ADD 1               TO WS-MSG-COUNT
                   END-IF
               WHEN WS-CASSIN-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-MESSAGE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CASSIN READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CASSIN-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           IF NOT WS-TRAILER-SEEN
               MOVE '9000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'CASS FILE HAS NO TRAILER' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'MESSAGES READ'            TO TL-LABEL
           MOVE WS-MSGS-READ               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INCOMING: ACCEPTED BY OLD BANK' TO TL-LABEL
           MOVE WS-ACCEPTED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INCOMING: REJECTED BY OLD BANK' TO TL-LABEL
           MOVE WS-REJECTED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INCOMING: STANDING ORDERS SET UP' TO TL-LABEL
           MOVE WS-SOS-CREATED             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INCOMING: MANDATES SET UP' TO TL-LABEL
           MOVE WS-DDS-CREATED             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INCOMING: BALANCES CREDITED' TO TL-LABEL
           MOVE WS-BALANCES                TO TL-COUNT
           MOVE WS-BALANCE-TOTAL           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'INCOMING: COMPLETED'      TO TL-LABEL
           MOVE WS-COMPLETED               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'OUTGOING: SWITCH REQUESTS RECEIVED' TO TL-LABEL
           MOVE WS-REQUESTS-IN             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'OUTGOING:   TO BE REJECTED' TO TL-LABEL
           MOVE WS-REQUESTS-NOMATCH        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'DUPLICATES IGNORED'       TO TL-LABEL
           MOVE WS-DUPLICATES              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ERRORS'                   TO TL-LABEL
           MOVE WS-ERRORS                  TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE CASSIN-FILE SWITCHES-FILE ACCTMAST-FILE CIFMAST-FILE
                 SOMAST-FILE DDMAND-FILE CSSPOST-FILE NOTIFY-FILE
                 REPORT-FILE
           DISPLAY 'CSSIN010 - MESSAGES ' WS-MSGS-READ
                   ' ERRORS ' WS-ERRORS
           IF WS-ERRORS > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
