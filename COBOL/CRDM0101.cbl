       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRDM0101.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2006-09-18.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CRDM0101                                            *
      * TITLE    : CRDMNT1 - CHANNEL CARD MAINTENANCE TO CARD          *
      *            MANAGEMENT SYSTEM (CMS) INTERFACE                   *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CRDMNT1 STEP CR010                                  *
      *----------------------------------------------------------------*
      * INPUT    : MAINTIN  - CRDMNT1 FILE FROM IBANK (CRDMNTRC)       *
      *            ACCTMAST - DDA ACCOUNT MASTER (READ ONLY, RANDOM)   *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : CMSOUT   - CMS ISSUER INPUT FILE (CMSMNTRC)         *
      *            HOTCARD  - LOST/STOLEN/FRAUD - SENT TO CMS AT HIGH  *
      *                       PRIORITY, ALSO LOADS BASE24 NEGATIVE     *
      *                       CARD FILE (CMSMNTRC)                     *
      *            REJECTS  - REJECTED REQUESTS (REASON + CRDMNTRC)    *
      *----------------------------------------------------------------*
      * THE HOST DOES NOT HOLD CARD RECORDS - CARDS ARE OWNED BY THE   *
      * CMS (THIRD PARTY PROCESSOR). THIS PROGRAM ONLY VALIDATES THE   *
      * LINKED ACCOUNT AND REFORMATS THE CHANNEL REQUEST.              *
      *                                                                *
      * !!! A CARD REPORTED LOST/STOLEN ONLINE IS NOT BLOCKED AT       *
      * !!! BASE24 UNTIL THIS JOB RUNS (22:45). THE IBANK SCREEN TELLS *
      * !!! THE CUSTOMER TO RING THE 24-HOUR LINE AS WELL. RISK        *
      * !!! ACCEPTED BY RETAIL FRAUD 2006 - RA-2006-031.               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2006-09-18 AOS  ORIGINAL VERSION                               *
      * 2007-05-21 AOS  AUTOMATIC REISSUE FOR LOST/STOLEN/FRAUD        *
      * 2008-11-03 JNB  CONTACTLESS SWITCH                             *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAINTIN-FILE     ASSIGN TO MAINTIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-MAINTIN-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT CMSOUT-FILE      ASSIGN TO CMSOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CMSOUT-STATUS.
           SELECT HOTCARD-FILE     ASSIGN TO HOTCARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-HOTCARD-STATUS.
           SELECT REJECTS-FILE     ASSIGN TO REJECTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REJECTS-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  MAINTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  MAINTIN-RECORD                  PIC X(200).
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  CMSOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CMSOUT-RECORD                   PIC X(150).
      *
       FD  HOTCARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  HOTCARD-RECORD                  PIC X(150).
      *
       FD  REJECTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REJECTS-RECORD                  PIC X(204).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CRDM0101'.
      *
       01  WS-CONSTANTS.
           05  WS-ISSUER-ID                PIC X(06) VALUE 'MBK001'.
           05  WS-MAX-ATM-LIMIT            PIC 9(09)V99 VALUE 1000.00.
           05  WS-MAX-POS-LIMIT            PIC 9(09)V99 VALUE 10000.00.
      *
       01  WS-FILE-STATUSES.
           05  WS-MAINTIN-STATUS           PIC X(02).
               88  WS-MAINTIN-OK               VALUE '00'.
               88  WS-MAINTIN-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-CMSOUT-STATUS            PIC X(02).
           05  WS-HOTCARD-STATUS           PIC X(02).
           05  WS-REJECTS-STATUS           PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-HEADER-SEEN-IND          PIC X(01) VALUE 'N'.
               88  WS-HEADER-SEEN              VALUE 'Y'.
           05  WS-TRAILER-SEEN-IND         PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-REJECT-RSN               PIC X(04).
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURR-DATE            PIC 9(08).
               10  WS-CURR-TIME            PIC 9(06).
               10  FILLER                  PIC X(07).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-CMS-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-HOTCARD-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-REJECT-COUNT             PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REJECT-RECORD.
           05  REJ-REASON                  PIC X(04).
           05  REJ-MAINT-RECORD            PIC X(200).
      *
           COPY CRDMNTRC.
      *
           COPY CMSMNTRC.
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
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  MAINTIN-FILE
                       ACCTMAST-FILE
                       DATECARD-FILE
                OUTPUT CMSOUT-FILE
                       HOTCARD-FILE
                       REJECTS-FILE

           IF NOT WS-MAINTIN-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-CMSOUT-STATUS NOT = '00'
           OR WS-HOTCARD-STATUS NOT = '00'
           OR WS-REJECTS-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE

           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA

           PERFORM 8000-READ-MAINTIN
           .
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           EVALUATE TRUE
               WHEN CRM-HEADER-REC
                   SET WS-HEADER-SEEN      TO TRUE
               WHEN CRM-DETAIL-REC
                   ADD 1                   TO WS-READ-COUNT
                   PERFORM 2100-PROCESS-DETAIL
               WHEN CRM-TRAILER-REC
                   SET WS-TRAILER-SEEN     TO TRUE
                   IF CRMT-REC-COUNT NOT = WS-READ-COUNT
                       MOVE '2000-PROCESS-RECORD'
                                           TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'TRAILER COUNT NOT = DETAIL RECORDS READ'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
               WHEN OTHER
                   MOVE '2000-PROCESS-RECORD'
                                           TO ABN-PARAGRAPH
                   MOVE 1007               TO ABN-ABEND-CODE
                   MOVE 'UNKNOWN RECORD TYPE'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
           END-EVALUATE

           PERFORM 8000-READ-MAINTIN
           .
      *================================================================*
       2100-PROCESS-DETAIL.
      *================================================================*
           MOVE SPACES                     TO WS-REJECT-RSN
           PERFORM 2200-VALIDATE-REQUEST

           IF WS-REJECT-RSN = SPACES
               PERFORM 2300-BUILD-CMS-BASE
               EVALUATE TRUE
                   WHEN CRMD-ACTIVATE
                       SET CMS-ACTIVATE    TO TRUE
                       MOVE '00'           TO CMS-NEW-STATUS
                       PERFORM 2800-WRITE-CMS
                   WHEN CRMD-FREEZE
                       SET CMS-STATUS-CHANGE TO TRUE
                       MOVE '05'           TO CMS-NEW-STATUS
                       PERFORM 2800-WRITE-CMS
                   WHEN CRMD-UNFREEZE
                       SET CMS-STATUS-CHANGE TO TRUE
                       MOVE '00'           TO CMS-NEW-STATUS
                       PERFORM 2800-WRITE-CMS
                   WHEN CRMD-HOT-CARD
                       PERFORM 2400-HOT-CARD
                   WHEN CRMD-REPLACE
                       SET CMS-REISSUE     TO TRUE
                       MOVE '01'           TO CMS-REISSUE-REASON
                       PERFORM 2800-WRITE-CMS
                   WHEN CRMD-LIMITS
                       SET CMS-LIMIT-CHANGE TO TRUE
      *                CMS HOLDS LIMITS IN WHOLE POUNDS - PENCE DROPPED
                       MOVE CRMD-DAILY-ATM-LIMIT TO CMS-ATM-LIMIT
                       MOVE CRMD-DAILY-POS-LIMIT TO CMS-POS-LIMIT
                       PERFORM 2800-WRITE-CMS
                   WHEN CRMD-PIN-REMINDER
                       SET CMS-PIN-REMINDER TO TRUE
                       PERFORM 2800-WRITE-CMS
                   WHEN CRMD-OVERSEAS
                   WHEN CRMD-CONTACTLESS
                       SET CMS-USAGE-FLAGS TO TRUE
                       MOVE CRMD-OVERSEAS-IND
                                           TO CMS-OVERSEAS-FLAG
                       MOVE CRMD-CONTACTLESS-IND
                                           TO CMS-CONTACTLESS-FLAG
                       PERFORM 2800-WRITE-CMS
               END-EVALUATE
           ELSE
               PERFORM 2900-WRITE-REJECT
           END-IF
           .
      *================================================================*
       2200-VALIDATE-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN CRMD-PAN-FIRST6 NOT NUMERIC
               WHEN CRMD-PAN-LAST4 NOT NUMERIC
                   MOVE 'PANX'             TO WS-REJECT-RSN
               WHEN CRMD-EXPIRY-MMYY NOT NUMERIC
                   MOVE 'EXPX'             TO WS-REJECT-RSN
               WHEN NOT (CRMD-ACTIVATE OR CRMD-FREEZE OR CRMD-UNFREEZE
                         OR CRMD-HOT-CARD OR CRMD-REPLACE
                         OR CRMD-LIMITS OR CRMD-PIN-REMINDER
                         OR CRMD-OVERSEAS OR CRMD-CONTACTLESS)
                   MOVE 'ACTX'             TO WS-REJECT-RSN
               WHEN CRMD-LIMITS
               AND (CRMD-DAILY-ATM-LIMIT NOT NUMERIC
                    OR CRMD-DAILY-POS-LIMIT NOT NUMERIC
                    OR CRMD-DAILY-ATM-LIMIT > WS-MAX-ATM-LIMIT
                    OR CRMD-DAILY-POS-LIMIT > WS-MAX-POS-LIMIT)
                   MOVE 'LIMX'             TO WS-REJECT-RSN
               WHEN (CRMD-OVERSEAS OR CRMD-CONTACTLESS)
               AND (CRMD-OVERSEAS-IND NOT = 'Y' AND 'N'
                    OR CRMD-CONTACTLESS-IND NOT = 'Y' AND 'N')
                   MOVE 'FLGX'             TO WS-REJECT-RSN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-REJECT-RSN = SPACES
               MOVE CRMD-SORT-CODE         TO ACM-SORT-CODE
               MOVE CRMD-ACCT-NO           TO ACM-ACCT-NO
               READ ACCTMAST-FILE
               EVALUATE TRUE
                   WHEN WS-ACCTMAST-NOTFND
                       MOVE 'ACNF'         TO WS-REJECT-RSN
                   WHEN NOT WS-ACCTMAST-OK
                       MOVE '2200-VALIDATE-REQUEST'
                                           TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-ACCTMAST-STATUS
                                           TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
      *            HOT CARD REQUESTS ARE ALWAYS PASSED ON, EVEN FOR A
      *            CLOSED ACCOUNT - A LIVE CARD MAY STILL BE OUT THERE.
                   WHEN ACM-CLOSED AND NOT CRMD-HOT-CARD
                       MOVE 'ACCL'         TO WS-REJECT-RSN
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF
           .
      *================================================================*
       2300-BUILD-CMS-BASE.
      *================================================================*
           MOVE SPACES                     TO CMS-MAINT-RECORD
           MOVE WS-ISSUER-ID               TO CMS-ISSUER-ID
           MOVE CRMD-SORT-CODE             TO CMS-ACCT-SORT-CODE
           MOVE CRMD-ACCT-NO               TO CMS-ACCT-NO
           MOVE CRMD-PAN-FIRST6            TO CMS-PAN-FIRST6
           MOVE CRMD-PAN-LAST4             TO CMS-PAN-LAST4
           STRING CRMD-EXPIRY-YY CRMD-EXPIRY-MM
                  DELIMITED BY SIZE
                  INTO CMS-EXPIRY-YYMM
           END-STRING
           MOVE ZERO                       TO CMS-ATM-LIMIT
                                              CMS-POS-LIMIT
           MOVE 'IBK'                      TO CMS-SOURCE-REF
           MOVE CRMD-CHNL-CARD-ID          TO CMS-SOURCE-REF (4:12)
           MOVE WS-CURR-DATE               TO CMS-REQUEST-DATE
           MOVE WS-CURR-TIME               TO CMS-REQUEST-TIME
           .
      *================================================================*
       2400-HOT-CARD.
      *================================================================*
      *    1. STATUS CHANGE TO THE HOT CARD FILE (BLOCKS AT BASE24)
      *    2. AUTOMATIC REISSUE TO THE NORMAL FILE
           SET CMS-STATUS-CHANGE           TO TRUE
           EVALUATE TRUE
               WHEN CRMD-LOST    MOVE '41' TO CMS-NEW-STATUS
               WHEN CRMD-STOLEN  MOVE '43' TO CMS-NEW-STATUS
               WHEN CRMD-FRAUD   MOVE '59' TO CMS-NEW-STATUS
           END-EVALUATE

           WRITE HOTCARD-RECORD            FROM CMS-MAINT-RECORD
           IF WS-HOTCARD-STATUS NOT = '00'
               MOVE '2400-HOT-CARD'        TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'HOTCARD WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-HOTCARD-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-HOTCARD-COUNT

           SET CMS-REISSUE                 TO TRUE
           MOVE CMS-NEW-STATUS             TO CMS-REISSUE-REASON
           MOVE SPACES                     TO CMS-NEW-STATUS
           PERFORM 2800-WRITE-CMS
           .
      *================================================================*
       2800-WRITE-CMS.
      *================================================================*
           WRITE CMSOUT-RECORD             FROM CMS-MAINT-RECORD
           IF WS-CMSOUT-STATUS NOT = '00'
               MOVE '2800-WRITE-CMS'       TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CMSOUT WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-CMSOUT-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-CMS-COUNT
           .
      *================================================================*
       2900-WRITE-REJECT.
      *================================================================*
           MOVE WS-REJECT-RSN              TO REJ-REASON
           MOVE CRM-MAINT-RECORD           TO REJ-MAINT-RECORD
           WRITE REJECTS-RECORD            FROM WS-REJECT-RECORD
           IF WS-REJECTS-STATUS NOT = '00'
               MOVE '2900-WRITE-REJECT'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'REJECTS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-REJECTS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-REJECT-COUNT
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF NOT WS-HEADER-SEEN OR NOT WS-TRAILER-SEEN
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'CRDMNT1 FILE HEADER OR TRAILER MISSING'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           CLOSE MAINTIN-FILE
                 ACCTMAST-FILE
                 CMSOUT-FILE
                 HOTCARD-FILE
                 REJECTS-FILE

           DISPLAY 'CRDM0101 - REQUESTS READ       ' WS-READ-COUNT
           DISPLAY 'CRDM0101 - CMS RECORDS WRITTEN ' WS-CMS-COUNT
           DISPLAY 'CRDM0101 - HOT CARD RECORDS    ' WS-HOTCARD-COUNT
           DISPLAY 'CRDM0101 - REJECTED            ' WS-REJECT-COUNT

           IF WS-REJECT-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       8000-READ-MAINTIN.
      *================================================================*
           READ MAINTIN-FILE INTO CRM-MAINT-RECORD
           EVALUATE TRUE
               WHEN WS-MAINTIN-OK
                   CONTINUE
               WHEN WS-MAINTIN-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-MAINTIN' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'MAINTIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-MAINTIN-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
