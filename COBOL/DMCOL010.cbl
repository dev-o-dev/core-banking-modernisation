       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DMCOL010.
       AUTHOR.        R J HALLIDAY.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1989-09-04.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DMCOL010                                            *
      * TITLE    : DIRECT DEBITS - INBOUND COLLECTIONS (PAYING BANK)   *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : DMCOL01 STEP DC020                                  *
      *----------------------------------------------------------------*
      * INPUT    : COLLIN   - DIRECT DEBITS DRAWN ON MEGABANK PAYERS   *
      *                       (BACS INPUT REPORT, REFORMATTED BY PTX,  *
      *                       SORTED BY PAYER ACCOUNT IN STEP DC010)   *
      *                       DMAUDREC LAYOUT, TXN CODES 01/17/18/19   *
      *            PRDPARM  - PRODUCT PARAMETERS                       *
      *            CIFMAST  - CIF (DECEASED CHECK)                     *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, RANDOM)   *
      *                       - UNPAID / PAID ITEM COUNTERS            *
      *            DDMAND   - DD MANDATE FILE (VSAM KSDS, RANDOM)      *
      * OUTPUT   : DDPOST   - DD DEBIT POSTINGS FOR PSEOD01 - PSTXNREC *
      *            RETURNS  - UNPAID ITEMS FOR DMARU010 - DMAUDREC     *
      *            LETTERS  - UNPAID DD CUSTOMER LETTER EXTRACT        *
      *            REPORT   - COLLECTIONS REPORT (FBA 133)             *
      *----------------------------------------------------------------*
      * EACH COLLECTION IS EITHER PAID (A 'DD  ' HARD POSTING) OR      *
      * RETURNED UNPAID WITH AN ARUDD REASON CODE:                     *
      *   5  NO ACCOUNT            B  ACCOUNT CLOSED                   *
      *   6  NO INSTRUCTION        1  INSTRUCTION CANCELLED            *
      *   2  PAYER DECEASED        0  REFER TO PAYER (INSUFFICIENT     *
      *                               FUNDS OR ACCOUNT RESTRICTED)     *
      *                                                                *
      * FUNDS CHECK AS SOEXE010: AVAILABLE + EFFECTIVE OD LIMIT, LESS  *
      * WHAT HAS ALREADY BEEN PAID FROM THE ACCOUNT IN THIS RUN.       *
      * STANDING ORDERS (SOEXE010) RUN FIRST AND ARE NOT VISIBLE HERE  *
      * - THEIR DEBITS ARE NOT YET ON THE MASTER. A CUSTOMER CAN       *
      * THEREFORE HAVE BOTH AN SO AND A DD PAID INTO AN UNARRANGED     *
      * OVERDRAFT. KNOWN (PIR 2003/144).                               *
      *                                                                *
      * A COLLECTION PAID THAT TAKES THE ACCOUNT BEYOND ITS ARRANGED   *
      * LIMIT IS NOT POSSIBLE UNDER THE RULE ABOVE, EXCEPT WITHIN THE  *
      * PRODUCT BUFFER (PRD-UNARR-OD-BUFFER) WHICH IS ALLOWED FOR DDS  *
      * ONLY - SUCH ITEMS COUNT AS "PAID ITEMS" FOR CHARGING.          *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1989-09-04 RJH  ORIGINAL VERSION (DD0510 - PAPER DDI REGISTER) *
      * 1998-11-30 MLW  Y2K                                            *
      * 2001-04-02 DKP  MANDATE FILE KEY INCLUDES PAYER REFERENCE      *
      * 2003-09-08 SAT  ARUDD REASON CODES; RETURNS FILE FOR DMARU010  *
      * 2004-08-09 SAT  BUFFER FOR PAID ITEMS (OFT)                    *
      * 2009-10-01 AOS  UNPAID / PAID COUNTERS FOR CGCAL010            *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COLLIN-FILE      ASSIGN TO COLLIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-COLLIN-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT DDMAND-FILE      ASSIGN TO DDMAND
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS DMM-KEY
                                   FILE STATUS IS WS-DDMAND-STATUS.
           SELECT DDPOST-FILE      ASSIGN TO DDPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DDPOST-STATUS.
           SELECT RETURNS-FILE     ASSIGN TO RETURNS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RETURNS-STATUS.
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
       FD  COLLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  COLLIN-RECORD                   PIC X(150).
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  DDMAND-FILE.
           COPY DMMNDREC.
       FD  DDPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DDPOST-RECORD                   PIC X(200).
       FD  RETURNS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RETURNS-RECORD                  PIC X(150).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DMCOL010'.
      *
       01  WS-CONSTANTS.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00020000.
      *
       01  WS-FILE-STATUSES.
           05  WS-COLLIN-STATUS            PIC X(02).
               88  WS-COLLIN-OK                VALUE '00'.
               88  WS-COLLIN-EOF               VALUE '10'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-DDMAND-STATUS            PIC X(02).
               88  WS-DDMAND-OK                VALUE '00'.
               88  WS-DDMAND-NOTFND            VALUE '23'.
           05  WS-DDPOST-STATUS            PIC X(02).
           05  WS-RETURNS-STATUS           PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-PAYER-FOUND-IND          PIC X(01) VALUE 'N'.
               88  WS-PAYER-FOUND              VALUE 'Y'.
           05  WS-PAYER-CHANGED-IND        PIC X(01) VALUE 'N'.
               88  WS-PAYER-CHANGED            VALUE 'Y'.
           05  WS-DECEASED-IND             PIC X(01) VALUE 'N'.
               88  WS-PAYER-DECEASED           VALUE 'Y'.
           05  WS-TRAILER-SEEN-IND         PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-CURR-PAYER               PIC X(14) VALUE LOW-VALUES.
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-BUFFER                   PIC S9(13)V99 COMP-3.
           05  WS-SPENDING-POWER           PIC S9(13)V99 COMP-3.
           05  WS-RESERVED                 PIC S9(13)V99 COMP-3.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-ARUDD-RSN                PIC X(01).
           05  WS-RESULT-TEXT              PIC X(30).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-PAID-ITEM-IND            PIC X(01).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
      *
       01  WS-SAVED-ACCOUNT                PIC X(400).
      *
      *---- UNPAID DD LETTER (DOCUMENT SERVICES TEMPLATE DDUNP01) -----*
       01  WS-LETTER.
           05  LTR-SORT-CODE               PIC X(06).
           05  LTR-ACCT-NO                 PIC X(08).
           05  LTR-CIF-NO                  PIC X(12).
           05  LTR-ORIG-NAME               PIC X(18).
           05  LTR-PAYER-REF               PIC X(18).
           05  LTR-AMOUNT                  PIC 9(11)V99.
           05  LTR-DUE-DATE                PIC 9(08).
           05  LTR-ARUDD-RSN               PIC X(01).
           05  LTR-REASON-TEXT             PIC X(30).
           05  LTR-FEE-AMT                 PIC 9(07)V99.
           05  FILLER                      PIC X(77).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-DETAIL-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETURNED-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAID-ITEM-COUNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-DETAIL-TOTAL             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-PAID-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-RETURNED-TOTAL           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-RSN-COUNT                PIC S9(07) COMP-3
                                           OCCURS 12 TIMES.
      *
       01  WS-RSN-CODES                    PIC X(12)
                                           VALUE '0123456789AB'.
       01  WS-RSN-SUB                      PIC S9(04) COMP.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'DMCOL010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - DIRECT DEBIT COLLECTIONS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(09) VALUE 'SORT CD'.
           05  FILLER            PIC X(10) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(08) VALUE 'SUN'.
           05  FILLER            PIC X(20) VALUE 'ORIGINATOR'.
           05  FILLER            PIC X(20) VALUE 'REFERENCE'.
           05  FILLER            PIC X(04) VALUE 'TC'.
           05  FILLER            PIC X(15) VALUE '         AMOUNT'.
           05  FILLER            PIC X(03) VALUE SPACES.
           05  FILLER            PIC X(09) VALUE 'RESULT'.
           05  FILLER            PIC X(35) VALUE 'REASON'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
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
           05  DL-TXN-CODE                 PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-RESULT                   PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-REASON                   PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TEXT                     PIC X(30).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY DMAUDREC.
      *
       01  WS-RETURN-RECORD                PIC X(150).
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
           PERFORM 2900-END-PAYER
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  COLLIN-FILE
                       PRDPARM-FILE
                       CIFMAST-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                       DDMAND-FILE
                OUTPUT DDPOST-FILE
                       RETURNS-FILE
                       LETTERS-FILE
                       REPORT-FILE
           IF NOT WS-COLLIN-OK
           OR NOT WS-PRDPARM-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-DDMAND-OK
           OR WS-DDPOST-STATUS NOT = '00'
           OR WS-RETURNS-STATUS NOT = '00'
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
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DATE
           MOVE WS-DATE-DISPLAY            TO HD1-BUS-DATE

           PERFORM VARYING WS-RSN-SUB FROM 1 BY 1 UNTIL WS-RSN-SUB > 12
               MOVE ZERO                   TO WS-RSN-COUNT (WS-RSN-SUB)
           END-PERFORM

      *    THE SORT STEP PUTS THE HEADER FIRST (KEY LOW-VALUES) AND
      *    THE TRAILER LAST (KEY HIGH-VALUES) - SEE CNTL(DMCOLSRT).
           PERFORM 8000-READ-INPUT
           IF WS-END-OF-INPUT OR NOT DMA-HEADER
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'COLLECTIONS FILE HAS NO HEADER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           PERFORM 8000-READ-INPUT
           .
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           EVALUATE TRUE
               WHEN DMA-DETAIL
                   ADD 1                   TO WS-DETAIL-COUNT
                   ADD DMA-AMOUNT          TO WS-DETAIL-TOTAL
                   IF DMA-PAYER-SORT-CODE NOT = WS-CURR-PAYER (1:6)
                   OR DMA-PAYER-ACCT-NO NOT = WS-CURR-PAYER (7:8)
                       PERFORM 2900-END-PAYER
                       PERFORM 2050-START-PAYER
                   END-IF
                   PERFORM 2100-PROCESS-COLLECTION THRU 2100-EXIT
               WHEN DMA-TRAILER
                   SET WS-TRAILER-SEEN     TO TRUE
                   IF DMAT-REC-COUNT NOT = WS-DETAIL-COUNT
                   OR DMAT-TOTAL-AMT NOT = WS-DETAIL-TOTAL
                       MOVE '2000-PROCESS-RECORD' TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'COLLECTIONS TRAILER COUNT/AMOUNT MISMATCH'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           PERFORM 8000-READ-INPUT
           .
      *================================================================*
       2050-START-PAYER.
      *================================================================*
           MOVE DMA-PAYER-SORT-CODE        TO WS-CURR-PAYER (1:6)
           MOVE DMA-PAYER-ACCT-NO          TO WS-CURR-PAYER (7:8)
           MOVE ZERO                       TO WS-RESERVED
                                              WS-SPENDING-POWER
                                              WS-BUFFER
           MOVE 'N'                        TO WS-PAYER-FOUND-IND
                                              WS-PAYER-CHANGED-IND
                                              WS-DECEASED-IND

           MOVE DMA-PAYER-SORT-CODE        TO ACM-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   SET WS-PAYER-FOUND      TO TRUE
                   MOVE ACM-OD-LIMIT       TO WS-OD-LIMIT
                   IF ACM-OD-EXPIRY-DATE NOT = ZERO
                   AND ACM-OD-EXPIRY-DATE < WS-BUS-DATE
                       MOVE ZERO           TO WS-OD-LIMIT
                   END-IF
                   COMPUTE WS-SPENDING-POWER =
                           ACM-AVAIL-BAL + WS-OD-LIMIT
                   MOVE ACM-PROD-CD        TO PRD-PROD-CD
                   READ PRDPARM-FILE
                   IF WS-PRDPARM-OK
                       MOVE PRD-UNARR-OD-BUFFER TO WS-BUFFER
                   END-IF
                   MOVE ACM-CIF-NO         TO CIF-NO
                   READ CIFMAST-FILE
                   IF WS-CIFMAST-OK AND CIF-DECEASED
                       SET WS-PAYER-DECEASED TO TRUE
                   END-IF
                   MOVE ACM-ACCOUNT-RECORD TO WS-SAVED-ACCOUNT
               WHEN WS-ACCTMAST-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE '2050-START-PAYER' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE WS-CURR-PAYER      TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2100-PROCESS-COLLECTION.
      *================================================================*
           MOVE DMA-AMOUNT                 TO WS-AMOUNT
           MOVE SPACE                      TO WS-ARUDD-RSN
           MOVE SPACES                     TO WS-RESULT-TEXT
           MOVE 'N'                        TO WS-PAID-ITEM-IND

      *---- ACCOUNT CHECKS --------------------------------------------*
           IF NOT WS-PAYER-FOUND
               MOVE '5'                    TO WS-ARUDD-RSN
               MOVE 'NO ACCOUNT'           TO WS-RESULT-TEXT
               GO TO 2100-RETURN
           END-IF
           MOVE WS-SAVED-ACCOUNT           TO ACM-ACCOUNT-RECORD
           EVALUATE TRUE
               WHEN ACM-CLOSED
               WHEN ACM-PENDING-CLOSE
                   MOVE 'B'                TO WS-ARUDD-RSN
                   MOVE 'ACCOUNT CLOSED'   TO WS-RESULT-TEXT
               WHEN WS-PAYER-DECEASED
                   MOVE '2'                TO WS-ARUDD-RSN
                   MOVE 'PAYER DECEASED'   TO WS-RESULT-TEXT
               WHEN ACM-FROZEN
               WHEN ACM-DORMANT
               WHEN ACM-NO-DEBITS
               WHEN ACM-NO-POSTINGS
                   MOVE '0'                TO WS-ARUDD-RSN
                   MOVE 'ACCOUNT RESTRICTED - REFER'
                                           TO WS-RESULT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-ARUDD-RSN NOT = SPACE
               GO TO 2100-RETURN
           END-IF

      *---- MANDATE CHECKS --------------------------------------------*
           MOVE DMA-PAYER-SORT-CODE        TO DMM-PAYER-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO DMM-PAYER-ACCT-NO
           MOVE DMA-ORIG-SUN               TO DMM-ORIG-SUN
           MOVE DMA-PAYER-REF              TO DMM-PAYER-REF
           READ DDMAND-FILE
           EVALUATE TRUE
               WHEN WS-DDMAND-NOTFND
                   MOVE '6'                TO WS-ARUDD-RSN
                   MOVE 'NO INSTRUCTION'   TO WS-RESULT-TEXT
               WHEN NOT WS-DDMAND-OK
                   MOVE '2100-PROCESS-COLLECTION' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'DDMAND READ FAILED' TO ABN-MESSAGE
                   MOVE WS-DDMAND-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               WHEN DMM-CANC-BY-PAYER
               WHEN DMM-CANC-BY-ORIG
               WHEN DMM-DORMANT
                   MOVE '1'                TO WS-ARUDD-RSN
                   MOVE 'INSTRUCTION CANCELLED' TO WS-RESULT-TEXT
               WHEN DMM-CLOSED
                   MOVE 'B'                TO WS-ARUDD-RSN
                   MOVE 'ACCOUNT CLOSED'   TO WS-RESULT-TEXT
               WHEN DMM-TRANSFERRED
                   MOVE '3'                TO WS-ARUDD-RSN
                   MOVE 'ACCOUNT TRANSFERRED' TO WS-RESULT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-ARUDD-RSN NOT = SPACE
               GO TO 2100-RETURN
           END-IF

      *---- FUNDS -----------------------------------------------------*
           EVALUATE TRUE
               WHEN WS-AMOUNT <= WS-SPENDING-POWER - WS-RESERVED
                   CONTINUE
               WHEN WS-AMOUNT <= WS-SPENDING-POWER - WS-RESERVED
                               + WS-BUFFER
      *            PAID INTO THE BUFFER - A "PAID ITEM" FOR CHARGING.
                   MOVE 'Y'                TO WS-PAID-ITEM-IND
               WHEN OTHER
                   MOVE '0'                TO WS-ARUDD-RSN
                   MOVE 'INSUFFICIENT FUNDS - REFER'
                                           TO WS-RESULT-TEXT
           END-EVALUATE
           IF WS-ARUDD-RSN NOT = SPACE
               GO TO 2100-RETURN
           END-IF

           PERFORM 2200-PAY-COLLECTION
           GO TO 2100-EXIT
           .
       2100-RETURN.
           PERFORM 2300-RETURN-COLLECTION
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-PAY-COLLECTION.
      *================================================================*
           ADD WS-AMOUNT                   TO WS-RESERVED

           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE DMA-PAYER-SORT-CODE        TO PST-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'DD  '                     TO PST-TXN-CODE
           SET PST-DEBIT                   TO TRUE
           MOVE WS-AMOUNT                  TO PST-AMOUNT
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE DMA-EFFECTIVE-DATE         TO PST-VALUE-DATE
           MOVE DMA-ORIG-NAME              TO PST-NARRATIVE-1
           MOVE DMA-PAYER-REF              TO PST-NARRATIVE-2
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           STRING 'DD' DMA-ORIG-SUN DMA-PAYER-REF (1:16)
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE DDPOST-RECORD             FROM PST-POSTING-RECORD
           IF WS-DDPOST-STATUS NOT = '00'
               MOVE '2200-PAY-COLLECTION'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'DDPOST WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-DDPOST-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

      *---- MANDATE HISTORY -------------------------------------------*
           IF DMM-FIRST-COLL-DATE = ZERO
               MOVE WS-BUS-DATE            TO DMM-FIRST-COLL-DATE
           END-IF
           MOVE WS-BUS-DATE                TO DMM-LAST-COLL-DATE
                                              DMM-LAST-MAINT-DATE
           MOVE WS-AMOUNT                  TO DMM-LAST-COLL-AMT
           ADD 1                           TO DMM-COLL-COUNT
           ADD WS-AMOUNT                   TO DMM-COLL-TOTAL
           IF WS-AMOUNT > DMM-MAX-AMT
               MOVE WS-AMOUNT              TO DMM-MAX-AMT
           END-IF
           MOVE WS-PROGRAM-ID              TO DMM-LAST-MAINT-PGM
           PERFORM 2800-REWRITE-MANDATE

           IF WS-PAID-ITEM-IND = 'Y'
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               IF ACM-PAID-ITEMS NOT NUMERIC
                   MOVE ZERO               TO ACM-PAID-ITEMS
               END-IF
               IF ACM-PAID-ITEMS < 999
                   ADD 1                   TO ACM-PAID-ITEMS
               END-IF
               MOVE ACM-ACCOUNT-RECORD     TO WS-SAVED-ACCOUNT
               SET WS-PAYER-CHANGED        TO TRUE
               ADD 1                       TO WS-PAID-ITEM-COUNT
               MOVE 'PAID INTO BUFFER (PAID ITEM)' TO WS-RESULT-TEXT
           END-IF

           ADD 1                           TO WS-PAID-COUNT
           ADD WS-AMOUNT                   TO WS-PAID-TOTAL
           MOVE 'PAID'                     TO DL-RESULT
           MOVE SPACE                      TO DL-REASON
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2300-RETURN-COLLECTION.
      *================================================================*
           MOVE WS-ARUDD-RSN               TO DMA-ARUDD-RSN
           MOVE 'RETN'                     TO DMA-RESULT-CD
           MOVE DMA-INTERFACE-RECORD       TO WS-RETURN-RECORD
           WRITE RETURNS-RECORD            FROM WS-RETURN-RECORD
           IF WS-RETURNS-STATUS NOT = '00'
               MOVE '2300-RETURN-COLLECTION' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'RETURNS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-RETURNS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

      *    REASON 0 (REFER TO PAYER) - UNPAID ITEM FEE AND LETTER.
           IF WS-ARUDD-RSN = '0' AND WS-PAYER-FOUND
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               IF ACM-UNPAID-ITEMS NOT NUMERIC
                   MOVE ZERO               TO ACM-UNPAID-ITEMS
               END-IF
               IF ACM-UNPAID-ITEMS < 999
                   ADD 1                   TO ACM-UNPAID-ITEMS
               END-IF
               MOVE ACM-ACCOUNT-RECORD     TO WS-SAVED-ACCOUNT
               SET WS-PAYER-CHANGED        TO TRUE
               PERFORM 2310-WRITE-LETTER
           END-IF

      *    MANDATE UNPAID HISTORY (IF THE MANDATE EXISTS).
           IF WS-DDMAND-OK AND WS-ARUDD-RSN NOT = '6'
               ADD 1                       TO DMM-UNPAID-COUNT
               MOVE WS-BUS-DATE            TO DMM-LAST-UNPAID-DATE
                                              DMM-LAST-MAINT-DATE
               MOVE WS-ARUDD-RSN           TO DMM-LAST-UNPAID-RSN
               MOVE WS-PROGRAM-ID          TO DMM-LAST-MAINT-PGM
               PERFORM 2800-REWRITE-MANDATE
           END-IF

           PERFORM VARYING WS-RSN-SUB FROM 1 BY 1 UNTIL WS-RSN-SUB > 12
               IF WS-RSN-CODES (WS-RSN-SUB:1) = WS-ARUDD-RSN
                   ADD 1                   TO WS-RSN-COUNT (WS-RSN-SUB)
               END-IF
           END-PERFORM

           ADD 1                           TO WS-RETURNED-COUNT
           ADD WS-AMOUNT                   TO WS-RETURNED-TOTAL
           MOVE 'UNPAID'                   TO DL-RESULT
           MOVE WS-ARUDD-RSN               TO DL-REASON
           PERFORM 7100-REPORT-DETAIL
           .
      *================================================================*
       2310-WRITE-LETTER.
      *================================================================*
           MOVE SPACES                     TO WS-LETTER
           MOVE DMA-PAYER-SORT-CODE        TO LTR-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO LTR-ACCT-NO
           MOVE ACM-CIF-NO                 TO LTR-CIF-NO
           MOVE DMA-ORIG-NAME              TO LTR-ORIG-NAME
           MOVE DMA-PAYER-REF              TO LTR-PAYER-REF
           MOVE WS-AMOUNT                  TO LTR-AMOUNT
           MOVE DMA-EFFECTIVE-DATE         TO LTR-DUE-DATE
           MOVE WS-ARUDD-RSN               TO LTR-ARUDD-RSN
           MOVE WS-RESULT-TEXT             TO LTR-REASON-TEXT
           IF WS-PRDPARM-OK
               MOVE PRD-UNPAID-ITEM-FEE    TO LTR-FEE-AMT
           ELSE
               MOVE ZERO                   TO LTR-FEE-AMT
           END-IF
           WRITE LETTERS-RECORD            FROM WS-LETTER
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '2310-WRITE-LETTER'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2800-REWRITE-MANDATE.
      *================================================================*
           REWRITE DMM-MANDATE-RECORD
           IF NOT WS-DDMAND-OK
               MOVE '2800-REWRITE-MANDATE' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'DDMAND REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-DDMAND-STATUS       TO ABN-FILE-STATUS
               MOVE DMM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2900-END-PAYER.
      *================================================================*
           IF WS-PAYER-CHANGED
               MOVE WS-SAVED-ACCOUNT       TO ACM-ACCOUNT-RECORD
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '2900-END-PAYER'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE WS-CURR-PAYER      TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
               MOVE 'N'                    TO WS-PAYER-CHANGED-IND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF NOT WS-TRAILER-SEEN
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'COLLECTIONS FILE HAS NO TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           IF WS-PAID-COUNT + WS-RETURNED-COUNT NOT = WS-DETAIL-COUNT
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'PAID + RETURNED NOT = COLLECTIONS READ'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           PERFORM 7000-REPORT-HEADINGS
           MOVE 'COLLECTIONS RECEIVED'     TO TL-LABEL
           MOVE WS-DETAIL-COUNT            TO TL-COUNT
           MOVE WS-DETAIL-TOTAL            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PAID'                     TO TL-LABEL
           MOVE WS-PAID-COUNT              TO TL-COUNT
           MOVE WS-PAID-TOTAL              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OF WHICH INTO BUFFER (PAID ITEMS)' TO TL-LABEL
           MOVE WS-PAID-ITEM-COUNT         TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RETURNED UNPAID'          TO TL-LABEL
           MOVE WS-RETURNED-COUNT          TO TL-COUNT
           MOVE WS-RETURNED-TOTAL          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           PERFORM VARYING WS-RSN-SUB FROM 1 BY 1 UNTIL WS-RSN-SUB > 12
               IF WS-RSN-COUNT (WS-RSN-SUB) > ZERO
                   MOVE SPACES             TO TL-LABEL
                   STRING '  ARUDD REASON '
                          WS-RSN-CODES (WS-RSN-SUB:1)
                          DELIMITED BY SIZE
                          INTO TL-LABEL
                   END-STRING
                   MOVE WS-RSN-COUNT (WS-RSN-SUB) TO TL-COUNT
                   MOVE ZERO               TO TL-AMOUNT
                   WRITE REPORT-RECORD     FROM WS-TOTAL-LINE
               END-IF
           END-PERFORM

           CLOSE COLLIN-FILE
                 PRDPARM-FILE
                 CIFMAST-FILE
                 ACCTMAST-FILE
                 DDMAND-FILE
                 DDPOST-FILE
                 RETURNS-FILE
                 LETTERS-FILE
                 REPORT-FILE

           DISPLAY 'DMCOL010 - COLLECTIONS ' WS-DETAIL-COUNT
           DISPLAY 'DMCOL010 - PAID        ' WS-PAID-COUNT
           DISPLAY 'DMCOL010 - RETURNED    ' WS-RETURNED-COUNT
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
           MOVE DMA-PAYER-SORT-CODE        TO DL-SORT-CODE
           MOVE DMA-PAYER-ACCT-NO          TO DL-ACCT-NO
           MOVE DMA-ORIG-SUN               TO DL-SUN
           MOVE DMA-ORIG-NAME              TO DL-ORIG-NAME
           MOVE DMA-PAYER-REF              TO DL-PAYER-REF
           MOVE DMA-TXN-CODE               TO DL-TXN-CODE
           MOVE WS-AMOUNT                  TO DL-AMOUNT
           MOVE WS-RESULT-TEXT             TO DL-TEXT
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-INPUT.
      *================================================================*
           READ COLLIN-FILE INTO DMA-INTERFACE-RECORD
           EVALUATE TRUE
               WHEN WS-COLLIN-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-COLLIN-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-INPUT'  TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'COLLIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-COLLIN-STATUS   TO ABN-FILE-STATUS
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
