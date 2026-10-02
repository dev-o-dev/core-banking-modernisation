       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PYINB010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2008-04-14.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PYINB010                                            *
      * TITLE    : INBOUND PAYMENTS - CREDITS RECEIVED                 *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : PYINB01 STEP PI010                                  *
      *----------------------------------------------------------------*
      * INPUT    : INBOUND  - FPS SETTLED CREDITS AND BACS INPUT,      *
      *                       CONCATENATED. EACH FILE HAS ITS OWN      *
      *                       HEADER AND TRAILER (PYINBREC)            *
      *            ACCTMAST - DDA ACCOUNT MASTER (READ ONLY, RANDOM)   *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : POSTOUT  - HARD POSTINGS FOR PSEOD010 (PSTXNREC)    *
      *            RETURNS  - CREDITS TO BE RETURNED TO THE SENDING    *
      *                       BANK (PYINBREC WITH RETURN REASON)       *
      *----------------------------------------------------------------*
      * INBOUND FPS CREDITS ARE NOT MEMO POSTED WHEN THE GATEWAY       *
      * RECEIVES THEM - THEY ARE APPLIED HERE AT END OF DAY ONLY, SO   *
      * THE CUSTOMER DOES NOT SEE THE FUNDS UNTIL THE NEXT MORNING.    *
      * (KNOWN LIMITATION - "FASTER" IN NAME ONLY FOR INBOUND. SEE     *
      * BRD FPS-IN-2, PHASE 2 NOT FUNDED.)                             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2008-04-14 AOS  ORIGINAL VERSION                               *
      * 2009-02-02 AOS  BACS INPUT ADDED TO THE SAME PROGRAM           *
      * 2009-04-06 AOS  CHAPS INBOUND (TXN CODE CHP)                   *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INBOUND-FILE     ASSIGN TO INBOUND
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-INBOUND-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT POSTOUT-FILE     ASSIGN TO POSTOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-POSTOUT-STATUS.
           SELECT RETURNS-FILE     ASSIGN TO RETURNS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RETURNS-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  INBOUND-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  INBOUND-RECORD                  PIC X(200).
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  POSTOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  POSTOUT-RECORD                  PIC X(200).
      *
       FD  RETURNS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RETURNS-RECORD                  PIC X(200).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PYINB010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-INBOUND-STATUS           PIC X(02).
               88  WS-INBOUND-OK               VALUE '00'.
               88  WS-INBOUND-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-POSTOUT-STATUS           PIC X(02).
           05  WS-RETURNS-STATUS           PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-IN-FILE-IND              PIC X(01) VALUE 'N'.
               88  WS-IN-FILE                  VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-RETURN-RSN               PIC X(04).
           05  WS-TXN-CODE                 PIC X(04).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-FILE-SCHEME              PIC X(04).
      *
       01  WS-FILE-TOTALS.
           05  WS-FILE-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-FILE-AMOUNT              PIC S9(13)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-RUN-TOTALS.
           05  WS-FILES-READ               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DETAIL-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-APPLIED-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-APPLIED-AMOUNT           PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-RETURNED-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETURNED-AMOUNT          PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-DISP-AMOUNT              PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
           COPY PYINBREC.
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
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  INBOUND-FILE
                       ACCTMAST-FILE
                       DATECARD-FILE
                OUTPUT POSTOUT-FILE
                       RETURNS-FILE

           IF NOT WS-INBOUND-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-POSTOUT-STATUS NOT = '00'
           OR WS-RETURNS-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-INBOUND-STATUS ' ' WS-ACCTMAST-STATUS ' '
                      WS-POSTOUT-STATUS ' ' WS-RETURNS-STATUS ' '
                      WS-DATECARD-STATUS
                      DELIMITED BY SIZE
                      INTO ABN-KEY-DATA
               END-STRING
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

           PERFORM 8000-READ-INBOUND
           .
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           EVALUATE TRUE
               WHEN PIB-HEADER-REC
                   PERFORM 2100-START-FILE
               WHEN PIB-DETAIL-REC
                   IF NOT WS-IN-FILE
                       MOVE '2000-PROCESS-RECORD'
                                           TO ABN-PARAGRAPH
                       MOVE 1007           TO ABN-ABEND-CODE
                       MOVE 'DETAIL RECORD BEFORE HEADER'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
                   PERFORM 2200-PROCESS-CREDIT
               WHEN PIB-TRAILER-REC
                   PERFORM 2300-END-FILE
               WHEN OTHER
                   MOVE '2000-PROCESS-RECORD'
                                           TO ABN-PARAGRAPH
                   MOVE 1007               TO ABN-ABEND-CODE
                   MOVE 'UNKNOWN RECORD TYPE ON INBOUND FILE'
                                           TO ABN-MESSAGE
                   MOVE INBOUND-RECORD (1:40)
                                           TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE

           PERFORM 8000-READ-INBOUND
           .
      *================================================================*
       2100-START-FILE.
      *================================================================*
           IF WS-IN-FILE
               MOVE '2100-START-FILE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'HEADER WITHOUT PRECEDING TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

      *    A FILE FOR ANOTHER BUSINESS DATE MEANS THE WRONG GENERATION
      *    HAS BEEN CONCATENATED - STOP BEFORE ANYONE IS CREDITED TWICE.
           IF PIBH-BUSINESS-DATE NOT = DTC-BUSINESS-DATE
               MOVE '2100-START-FILE'      TO ABN-PARAGRAPH
               MOVE 1006                   TO ABN-ABEND-CODE
               MOVE 'INBOUND FILE BUSINESS DATE NOT = DATE CARD'
                                           TO ABN-MESSAGE
               MOVE PIB-INBOUND-RECORD (1:40)
                                           TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           SET WS-IN-FILE                  TO TRUE
           MOVE PIBH-SCHEME                TO WS-FILE-SCHEME
           MOVE ZERO                       TO WS-FILE-COUNT
                                              WS-FILE-AMOUNT
           ADD 1                           TO WS-FILES-READ
           DISPLAY 'PYINB010 - PROCESSING ' PIBH-FILE-ID
                   ' SCHEME ' PIBH-SCHEME
                   ' SEQ ' PIBH-FILE-SEQ-NO
           .
      *================================================================*
       2200-PROCESS-CREDIT.
      *================================================================*
           ADD 1                           TO WS-FILE-COUNT
                                              WS-DETAIL-COUNT
           ADD PIBD-AMOUNT                 TO WS-FILE-AMOUNT
           MOVE SPACES                     TO WS-RETURN-RSN

           EVALUATE PIBD-SCHEME
               WHEN 'FPS '  MOVE 'FPI '    TO WS-TXN-CODE
               WHEN 'BACS'  MOVE 'BGC '    TO WS-TXN-CODE
               WHEN 'CHAP'  MOVE 'CHP '    TO WS-TXN-CODE
               WHEN 'SWFT'  MOVE 'SWI '    TO WS-TXN-CODE
               WHEN OTHER   MOVE 'BGC '    TO WS-TXN-CODE
           END-EVALUATE

           IF PIBD-CCY-CD NOT = 'GBP'
               MOVE 'CCYX'                 TO WS-RETURN-RSN
           ELSE
               MOVE PIBD-BENE-SORT-CODE    TO ACM-SORT-CODE
               MOVE PIBD-BENE-ACCT-NO      TO ACM-ACCT-NO
               READ ACCTMAST-FILE
               EVALUATE TRUE
                   WHEN WS-ACCTMAST-NOTFND
                       MOVE 'ACNF'         TO WS-RETURN-RSN
                   WHEN NOT WS-ACCTMAST-OK
                       MOVE '2200-PROCESS-CREDIT'
                                           TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-ACCTMAST-STATUS
                                           TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
                   WHEN ACM-CLOSED
                       MOVE 'ACCL'         TO WS-RETURN-RSN
                   WHEN ACM-FROZEN
                   WHEN ACM-NO-CREDITS
                   WHEN ACM-NO-POSTINGS
                       MOVE 'ACBL'         TO WS-RETURN-RSN
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF

           IF WS-RETURN-RSN = SPACES
               PERFORM 2210-WRITE-POSTING
           ELSE
               PERFORM 2220-WRITE-RETURN
           END-IF
           .
      *================================================================*
       2210-WRITE-POSTING.
      *================================================================*
           ADD 1                           TO WS-POST-SEQ
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE PIBD-BENE-SORT-CODE        TO PST-SORT-CODE
           MOVE PIBD-BENE-ACCT-NO          TO PST-ACCT-NO
           MOVE DTC-BUSINESS-DATE          TO PST-POST-DATE
      *    TIME 23:59:00.00 - INBOUND CREDITS SORT AFTER THE DAY'S
      *    MEMO POSTINGS FOR THE SAME ACCOUNT.
           MOVE 23590000                   TO PST-POST-TIME
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE WS-TXN-CODE                TO PST-TXN-CODE
           SET PST-CREDIT                  TO TRUE
           MOVE PIBD-AMOUNT                TO PST-AMOUNT
           MOVE PIBD-CCY-CD                TO PST-CCY-CD
           MOVE PIBD-VALUE-DATE            TO PST-VALUE-DATE
           MOVE PIBD-ORIG-NAME             TO PST-NARRATIVE-1
           MOVE PIBD-REFERENCE             TO PST-NARRATIVE-2
           SET PST-CHNL-BATCH              TO TRUE
           MOVE PIBD-SCHEME-REF            TO PST-EXT-REF
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE 'PYINB010'                 TO PST-USER-ID

           WRITE POSTOUT-RECORD            FROM PST-POSTING-RECORD
           IF WS-POSTOUT-STATUS NOT = '00'
               MOVE '2210-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'POSTOUT WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-POSTOUT-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

           ADD 1                           TO WS-APPLIED-COUNT
           ADD PIBD-AMOUNT                 TO WS-APPLIED-AMOUNT
           .
      *================================================================*
       2220-WRITE-RETURN.
      *================================================================*
           MOVE WS-RETURN-RSN              TO PIBD-RETURN-RSN
           WRITE RETURNS-RECORD            FROM PIB-INBOUND-RECORD
           IF WS-RETURNS-STATUS NOT = '00'
               MOVE '2220-WRITE-RETURN'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'RETURNS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-RETURNS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

           ADD 1                           TO WS-RETURNED-COUNT
           ADD PIBD-AMOUNT                 TO WS-RETURNED-AMOUNT
           .
      *================================================================*
       2300-END-FILE.
      *================================================================*
           IF NOT WS-IN-FILE
               MOVE '2300-END-FILE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'TRAILER WITHOUT HEADER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           IF PIBT-REC-COUNT NOT = WS-FILE-COUNT
           OR PIBT-TOTAL-AMT NOT = WS-FILE-AMOUNT
               MOVE '2300-END-FILE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'INBOUND TRAILER COUNT/AMOUNT MISMATCH'
                                           TO ABN-MESSAGE
               MOVE WS-FILE-SCHEME         TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           MOVE 'N'                        TO WS-IN-FILE-IND
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF WS-IN-FILE
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'END OF INPUT WITHOUT TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           CLOSE INBOUND-FILE
                 ACCTMAST-FILE
                 POSTOUT-FILE
                 RETURNS-FILE

           DISPLAY 'PYINB010 - FILES READ         ' WS-FILES-READ
           DISPLAY 'PYINB010 - CREDITS READ       ' WS-DETAIL-COUNT
           MOVE WS-APPLIED-AMOUNT          TO WS-DISP-AMOUNT
           DISPLAY 'PYINB010 - CREDITS APPLIED    ' WS-APPLIED-COUNT
                   ' ' WS-DISP-AMOUNT
           MOVE WS-RETURNED-AMOUNT         TO WS-DISP-AMOUNT
           DISPLAY 'PYINB010 - CREDITS RETURNED   ' WS-RETURNED-COUNT
                   ' ' WS-DISP-AMOUNT

           IF WS-RETURNED-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       8000-READ-INBOUND.
      *================================================================*
           READ INBOUND-FILE INTO PIB-INBOUND-RECORD
           EVALUATE TRUE
               WHEN WS-INBOUND-OK
                   CONTINUE
               WHEN WS-INBOUND-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-INBOUND' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'INBOUND READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-INBOUND-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
