       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DMAUD010.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2001-04-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DMAUD010                                            *
      * TITLE    : DIRECT DEBITS - AUDDIS INSTRUCTION PROCESSING       *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : DMDDI01 STEP DM010                                  *
      *----------------------------------------------------------------*
      * INPUT    : AUDDISIN - AUDDIS INSTRUCTIONS FROM PTX - DMAUDREC  *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      *            ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM)   *
      *            CIFMAST  - CIF CUSTOMER MASTER (VSAM KSDS, RANDOM)  *
      * UPDATE   : DDMAND   - DD MANDATE FILE (VSAM KSDS, RANDOM)      *
      * OUTPUT   : AUDDISRS - RESULTS BACK TO PTX - DMAUDREC           *
      *            REPORT   - AUDDIS PROCESSING REPORT (FBA 133)       *
      *----------------------------------------------------------------*
      * THE AUTOMATED DIRECT DEBIT INSTRUCTION SERVICE: ORIGINATORS    *
      * (SERVICE USERS) LODGE NEW INSTRUCTIONS ELECTRONICALLY. FOR     *
      * EACH INSTRUCTION THE PAYER'S BANK MUST ACCEPT OR REJECT IT     *
      * WITHIN THE BACS CYCLE.                                         *
      *                                                                *
      *   0N NEW     - CREATE A LIVE MANDATE. A CANCELLED MANDATE WITH *
      *                THE SAME KEY IS REINSTATED. A LIVE ONE IS A     *
      *                DUPLICATE AND IS ACCEPTED WITHOUT CHANGE.       *
      *   0S CONVERT - PAPER MANDATE BEING CONVERTED; AS 0N BUT THE    *
      *                MANDATE SOURCE IS 'V'                           *
      *   0C CANCEL  - ORIGINATOR CANCELS; STATUS 'O'                  *
      *                                                                *
      * REJECTION RESULT CODES (DMA-RESULT-CD)                         *
      *   ACCP  ACCEPTED           DUPL  ALREADY LIVE (ACCEPTED)       *
      *   NOAC  NO ACCOUNT         ACLS  ACCOUNT CLOSED                *
      *   NODD  ACCOUNT DOES NOT ACCEPT DIRECT DEBITS (NOT A CURRENT   *
      *         ACCOUNT, OR DEBITS RESTRICTED)                         *
      *   DECD  PAYER DECEASED     NMAN  NO MANDATE TO CANCEL          *
      *   INVD  INVALID DATA                                           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2001-04-02 DKP  ORIGINAL VERSION (AUDDIS GO-LIVE)              *
      * 2003-09-08 SAT  0S CONVERSIONS                                 *
      * 2008-11-17 JNB  REINSTATE CANCELLED MANDATE ON NEW 0N          *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUDDISIN-FILE    ASSIGN TO AUDDISIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-AUDDISIN-STATUS.
           SELECT AUDDISRS-FILE    ASSIGN TO AUDDISRS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-AUDDISRS-STATUS.
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
           SELECT DDMAND-FILE      ASSIGN TO DDMAND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS DMM-KEY
                                   FILE STATUS IS WS-DDMAND-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  AUDDISIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  AUDDISIN-RECORD                 PIC X(150).
       FD  AUDDISRS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  AUDDISRS-RECORD                 PIC X(150).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  DDMAND-FILE.
           COPY DMMNDREC.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DMAUD010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-AUDDISIN-STATUS          PIC X(02).
               88  WS-AUDDISIN-OK              VALUE '00'.
               88  WS-AUDDISIN-EOF             VALUE '10'.
           05  WS-AUDDISRS-STATUS          PIC X(02).
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-DDMAND-STATUS            PIC X(02).
               88  WS-DDMAND-OK                VALUE '00'.
               88  WS-DDMAND-NOTFND            VALUE '23'.
               88  WS-DDMAND-DUPKEY            VALUE '22'.
           05  WS-REPORT-STATUS            PIC X(02).
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
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-RESULT                   PIC X(04).
           05  WS-RESULT-TEXT              PIC X(40).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
           05  WS-FILE-SEQ                 PIC 9(06).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-DETAIL-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-NEW-COUNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-REINSTATE-COUNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-DUPL-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-CANCEL-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-REJECT-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RESULT-COUNT             PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'DMAUD010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - AUDDIS INSTRUCTIONS PROCESSED'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(04) VALUE 'TC'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(08) VALUE 'SUN'.
           05  FILLER            PIC X(20) VALUE 'ORIGINATOR'.
           05  FILLER            PIC X(20) VALUE 'PAYER REFERENCE'.
           05  FILLER            PIC X(06) VALUE 'RSLT'.
           05  FILLER            PIC X(55) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-TXN-CODE                 PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SUN                      PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ORIG-NAME                PIC X(18).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PAYER-REF                PIC X(18).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-RESULT                   PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-TEXT                     PIC X(40).
           05  FILLER                      PIC X(15) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
           COPY DMAUDREC.
      *
       01  WS-RESULT-RECORD                PIC X(150).
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
           OPEN INPUT  AUDDISIN-FILE
                       ACCTMAST-FILE
                       CIFMAST-FILE
                       DATECARD-FILE
                I-O    DDMAND-FILE
                OUTPUT AUDDISRS-FILE
                       REPORT-FILE
           IF NOT WS-AUDDISIN-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-DDMAND-OK
           OR WS-AUDDISRS-STATUS NOT = '00'
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
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO HD1-BUS-DATE

           PERFORM 8000-READ-INPUT
           IF WS-END-OF-INPUT OR NOT DMA-HEADER
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'AUDDIS FILE HAS NO HEADER RECORD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF NOT DMAH-AUDDIS
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'INPUT IS NOT AN AUDDIS FILE'
                                           TO ABN-MESSAGE
               MOVE DMA-INTERFACE-RECORD (1:40) TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           SET WS-HEADER-SEEN              TO TRUE
           MOVE DMAH-FILE-SEQ              TO WS-FILE-SEQ
           DISPLAY 'DMAUD010 - AUDDIS FILE ' DMAH-FILE-SEQ
                   ' PROCESS DATE ' DMAH-PROCESS-DATE

      *    THE RESULTS FILE HAS THE SAME HEADER.
           MOVE DMA-INTERFACE-RECORD       TO WS-RESULT-RECORD
           WRITE AUDDISRS-RECORD           FROM WS-RESULT-RECORD

           PERFORM 8000-READ-INPUT
           .
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           EVALUATE TRUE
               WHEN DMA-DETAIL
                   ADD 1                   TO WS-DETAIL-COUNT
                   PERFORM 2100-PROCESS-INSTRUCTION
               WHEN DMA-TRAILER
                   SET WS-TRAILER-SEEN     TO TRUE
                   IF DMAT-REC-COUNT NOT = WS-DETAIL-COUNT
                       MOVE '2000-PROCESS-RECORD' TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'AUDDIS TRAILER COUNT MISMATCH'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
               WHEN OTHER
                   MOVE '2000-PROCESS-RECORD' TO ABN-PARAGRAPH
                   MOVE 1006               TO ABN-ABEND-CODE
                   MOVE 'UNEXPECTED RECORD TYPE IN AUDDIS FILE'
                                           TO ABN-MESSAGE
                   MOVE DMA-INTERFACE-RECORD (1:40) TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           PERFORM 8000-READ-INPUT
           .
      *================================================================*
       2100-PROCESS-INSTRUCTION.
      *================================================================*
           MOVE 'ACCP'                     TO WS-RESULT
           MOVE SPACES                     TO WS-RESULT-TEXT

           EVALUATE TRUE
               WHEN DMA-PAYER-SORT-CODE NOT NUMERIC
               WHEN DMA-PAYER-ACCT-NO NOT NUMERIC
               WHEN DMA-ORIG-SUN NOT NUMERIC
               WHEN DMA-PAYER-REF = SPACES
                   MOVE 'INVD'             TO WS-RESULT
                   MOVE 'INVALID ACCOUNT, SUN OR REFERENCE'
                                           TO WS-RESULT-TEXT
               WHEN DMA-NEW-INSTRUCTION
               WHEN DMA-CONVERSION
                   PERFORM 2200-CHECK-PAYER-ACCOUNT
                   IF WS-RESULT = 'ACCP'
                       PERFORM 2300-CREATE-MANDATE
                   END-IF
               WHEN DMA-CANCEL-INSTRUCTION
                   PERFORM 2400-CANCEL-MANDATE
               WHEN OTHER
                   MOVE 'INVD'             TO WS-RESULT
                   MOVE 'TRANSACTION CODE NOT 0N, 0S OR 0C'
                                           TO WS-RESULT-TEXT
           END-EVALUATE

           IF WS-RESULT NOT = 'ACCP' AND WS-RESULT NOT = 'DUPL'
               ADD 1                       TO WS-REJECT-COUNT
           END-IF

      *---- RESULT RECORD ---------------------------------------------*
           MOVE WS-RESULT                  TO DMA-RESULT-CD
           MOVE DMA-INTERFACE-RECORD       TO WS-RESULT-RECORD
           WRITE AUDDISRS-RECORD           FROM WS-RESULT-RECORD
           IF WS-AUDDISRS-STATUS NOT = '00'
               MOVE '2100-PROCESS-INSTRUCTION' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'AUDDISRS WRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-AUDDISRS-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-RESULT-COUNT

           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2200-CHECK-PAYER-ACCOUNT.
      *================================================================*
           MOVE DMA-PAYER-SORT-CODE        TO ACM-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'NOAC'             TO WS-RESULT
                   MOVE 'NO SUCH ACCOUNT'  TO WS-RESULT-TEXT
               WHEN NOT WS-ACCTMAST-OK
                   MOVE '2200-CHECK-PAYER-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               WHEN ACM-CLOSED OR ACM-PENDING-CLOSE
                   MOVE 'ACLS'             TO WS-RESULT
                   MOVE 'ACCOUNT CLOSED'   TO WS-RESULT-TEXT
               WHEN NOT ACM-CURRENT
               WHEN ACM-NO-DEBITS
               WHEN ACM-NO-POSTINGS
                   MOVE 'NODD'             TO WS-RESULT
                   MOVE 'ACCOUNT DOES NOT ACCEPT DIRECT DEBITS'
                                           TO WS-RESULT-TEXT
               WHEN OTHER
                   PERFORM 2210-CHECK-CUSTOMER
           END-EVALUATE
           .
      *================================================================*
       2210-CHECK-CUSTOMER.
      *================================================================*
           MOVE ACM-CIF-NO                 TO CIF-NO
           READ CIFMAST-FILE
           IF WS-CIFMAST-OK
               IF CIF-DECEASED
                   MOVE 'DECD'             TO WS-RESULT
                   MOVE 'PAYER DECEASED'   TO WS-RESULT-TEXT
               END-IF
           END-IF
           .
      *================================================================*
       2300-CREATE-MANDATE.
      *================================================================*
           MOVE DMA-PAYER-SORT-CODE        TO DMM-PAYER-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO DMM-PAYER-ACCT-NO
           MOVE DMA-ORIG-SUN               TO DMM-ORIG-SUN
           MOVE DMA-PAYER-REF              TO DMM-PAYER-REF
           READ DDMAND-FILE
           EVALUATE TRUE
               WHEN WS-DDMAND-NOTFND
                   PERFORM 2310-BUILD-NEW
                   WRITE DMM-MANDATE-RECORD
                   IF NOT WS-DDMAND-OK
                       MOVE '2300-CREATE-MANDATE' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'DDMAND WRITE FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-DDMAND-STATUS TO ABN-FILE-STATUS
                       MOVE DMM-KEY        TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
                   ADD 1                   TO WS-NEW-COUNT
                   MOVE 'NEW MANDATE CREATED' TO WS-RESULT-TEXT
               WHEN NOT WS-DDMAND-OK
                   MOVE '2300-CREATE-MANDATE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'DDMAND READ FAILED' TO ABN-MESSAGE
                   MOVE WS-DDMAND-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               WHEN DMM-LIVE
                   MOVE 'DUPL'             TO WS-RESULT
                   MOVE 'MANDATE ALREADY LIVE - NO CHANGE'
                                           TO WS-RESULT-TEXT
                   ADD 1                   TO WS-DUPL-COUNT
               WHEN OTHER
      *            CANCELLED / DORMANT / CLOSED - REINSTATE. A MANDATE
      *            CLOSED BECAUSE THE ACCOUNT CLOSED CANNOT GET HERE
      *            (2200 REJECTS CLOSED ACCOUNTS).
                   SET DMM-LIVE            TO TRUE
                   MOVE DMA-ORIG-NAME      TO DMM-ORIG-NAME
                   MOVE DMA-PAYER-NAME     TO DMM-PAYER-NAME
                   MOVE WS-BUS-DATE        TO DMM-SETUP-DATE
                                              DMM-LAST-MAINT-DATE
                   MOVE ZERO               TO DMM-CANCEL-DATE
                                              DMM-ADDACS-SENT-DATE
                   MOVE SPACES             TO DMM-CANCEL-CHANNEL
                                              DMM-CANCEL-USER
                                              DMM-ADDACS-REASON
                   MOVE WS-PROGRAM-ID      TO DMM-LAST-MAINT-PGM
                   REWRITE DMM-MANDATE-RECORD
                   IF NOT WS-DDMAND-OK
                       MOVE '2300-CREATE-MANDATE' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'DDMAND REWRITE FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-DDMAND-STATUS TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
                   END-IF
                   ADD 1                   TO WS-REINSTATE-COUNT
                   MOVE 'CANCELLED MANDATE REINSTATED'
                                           TO WS-RESULT-TEXT
           END-EVALUATE
           .
      *================================================================*
       2310-BUILD-NEW.
      *================================================================*
           MOVE SPACES                     TO DMM-MANDATE-RECORD
           MOVE DMA-PAYER-SORT-CODE        TO DMM-PAYER-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO DMM-PAYER-ACCT-NO
           MOVE DMA-ORIG-SUN               TO DMM-ORIG-SUN
           MOVE DMA-PAYER-REF              TO DMM-PAYER-REF
           SET DMM-LIVE                    TO TRUE
           MOVE DMA-ORIG-NAME              TO DMM-ORIG-NAME
           MOVE DMA-ORIG-SORT-CODE         TO DMM-ORIG-SORT-CODE
           MOVE DMA-ORIG-ACCT-NO           TO DMM-ORIG-ACCT-NO
           MOVE DMA-PAYER-NAME             TO DMM-PAYER-NAME
           IF DMA-CONVERSION
               SET DMM-SRC-CONVERSION      TO TRUE
           ELSE
               SET DMM-SRC-AUDDIS          TO TRUE
           END-IF
           MOVE WS-BUS-DATE                TO DMM-SETUP-DATE
           MOVE ZERO                       TO DMM-FIRST-COLL-DATE
                                              DMM-LAST-COLL-DATE
                                              DMM-LAST-COLL-AMT
                                              DMM-COLL-COUNT
                                              DMM-COLL-TOTAL
                                              DMM-UNPAID-COUNT
                                              DMM-LAST-UNPAID-DATE
                                              DMM-INDEMNITY-COUNT
                                              DMM-MAX-AMT
                                              DMM-CANCEL-DATE
                                              DMM-ADDACS-SENT-DATE
           MOVE WS-BUS-DATE                TO DMM-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO DMM-LAST-MAINT-PGM
           .
      *================================================================*
       2400-CANCEL-MANDATE.
      *================================================================*
           MOVE DMA-PAYER-SORT-CODE        TO DMM-PAYER-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO DMM-PAYER-ACCT-NO
           MOVE DMA-ORIG-SUN               TO DMM-ORIG-SUN
           MOVE DMA-PAYER-REF              TO DMM-PAYER-REF
           READ DDMAND-FILE
           EVALUATE TRUE
               WHEN WS-DDMAND-NOTFND
                   MOVE 'NMAN'             TO WS-RESULT
                   MOVE 'NO MANDATE TO CANCEL'
                                           TO WS-RESULT-TEXT
               WHEN NOT WS-DDMAND-OK
                   MOVE '2400-CANCEL-MANDATE' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'DDMAND READ FAILED' TO ABN-MESSAGE
                   MOVE WS-DDMAND-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               WHEN OTHER
                   SET DMM-CANC-BY-ORIG    TO TRUE
                   MOVE WS-BUS-DATE        TO DMM-CANCEL-DATE
                                              DMM-LAST-MAINT-DATE
                   MOVE 'BACS  '           TO DMM-CANCEL-CHANNEL
                   MOVE DMA-ORIG-SUN       TO DMM-CANCEL-USER
                   MOVE WS-PROGRAM-ID      TO DMM-LAST-MAINT-PGM
      *            ORIGINATOR CANCELLED - NO ADDACS IS NEEDED.
                   MOVE WS-BUS-DATE        TO DMM-ADDACS-SENT-DATE
                   REWRITE DMM-MANDATE-RECORD
                   IF NOT WS-DDMAND-OK
                       MOVE '2400-CANCEL-MANDATE' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'DDMAND REWRITE FAILED'
                                           TO ABN-MESSAGE
                       MOVE WS-DDMAND-STATUS TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
                   END-IF
                   ADD 1                   TO WS-CANCEL-COUNT
                   MOVE 'CANCELLED BY ORIGINATOR'
                                           TO WS-RESULT-TEXT
           END-EVALUATE
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF NOT WS-TRAILER-SEEN
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'AUDDIS FILE HAS NO TRAILER - FILE TRUNCATED?'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           MOVE SPACES                     TO DMA-INTERFACE-RECORD
           SET DMA-TRAILER                 TO TRUE
           MOVE WS-RESULT-COUNT            TO DMAT-REC-COUNT
           MOVE ZERO                       TO DMAT-TOTAL-AMT
           MOVE DMA-INTERFACE-RECORD       TO WS-RESULT-RECORD
           WRITE AUDDISRS-RECORD           FROM WS-RESULT-RECORD

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'INSTRUCTIONS RECEIVED'    TO TL-LABEL
           MOVE WS-DETAIL-COUNT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'NEW MANDATES'             TO TL-LABEL
           MOVE WS-NEW-COUNT               TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REINSTATED'               TO TL-LABEL
           MOVE WS-REINSTATE-COUNT         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'DUPLICATES (ALREADY LIVE)' TO TL-LABEL
           MOVE WS-DUPL-COUNT              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CANCELLED BY ORIGINATOR'  TO TL-LABEL
           MOVE WS-CANCEL-COUNT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'REJECTED'                 TO TL-LABEL
           MOVE WS-REJECT-COUNT            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE AUDDISIN-FILE
                 AUDDISRS-FILE
                 ACCTMAST-FILE
                 CIFMAST-FILE
                 DDMAND-FILE
                 REPORT-FILE

           DISPLAY 'DMAUD010 - INSTRUCTIONS ' WS-DETAIL-COUNT
           DISPLAY 'DMAUD010 - NEW          ' WS-NEW-COUNT
           DISPLAY 'DMAUD010 - REJECTED     ' WS-REJECT-COUNT
           IF WS-REJECT-COUNT > ZERO
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
           MOVE DMA-TXN-CODE               TO DL-TXN-CODE
           MOVE DMA-PAYER-SORT-CODE        TO DL-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO DL-ACCT-NO
           MOVE DMA-ORIG-SUN               TO DL-SUN
           MOVE DMA-ORIG-NAME              TO DL-ORIG-NAME
           MOVE DMA-PAYER-REF              TO DL-PAYER-REF
           MOVE WS-RESULT                  TO DL-RESULT
           MOVE WS-RESULT-TEXT             TO DL-TEXT
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-INPUT.
      *================================================================*
           READ AUDDISIN-FILE INTO DMA-INTERFACE-RECORD
           EVALUATE TRUE
               WHEN WS-AUDDISIN-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-AUDDISIN-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-INPUT'  TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'AUDDISIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-AUDDISIN-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8900-FORMAT-DATE.
      *================================================================*
           MOVE SPACES                     TO WS-DATE-DISPLAY
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-DATE-DISPLAY
           END-STRING
           .
      *
           COPY ABNDPROC.
