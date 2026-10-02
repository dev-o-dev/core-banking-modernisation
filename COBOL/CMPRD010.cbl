       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMPRD010.
       AUTHOR.        P D QUINN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2016-02-08.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CMPRD010                                            *
      * TITLE    : COMPLAINTS - PAY APPROVED REDRESS TO CUSTOMER       *
      *            ACCOUNTS (REFUND, CONSEQUENTIAL LOSS, 8% INTEREST)  *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CMPRD01 STEP RD010 (DAILY, BEFORE PSEOD01)          *
      *----------------------------------------------------------------*
      * INPUT    : REDRESS  - APPROVED REDRESS FROM CMS (CMRDRREC)     *
      *            ACCTMAST - DDA ACCOUNT MASTER (RANDOM, READ ONLY)   *
      *            CIFMAST  - CUSTOMER MASTER (RANDOM - NAME/ADDRESS)  *
      *            PRDPARM  - PRODUCT PARAMETERS (TAX PAYABLE GL)      *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : RDRPOST  - 'TFR ' CREDITS FOR TONIGHT'S PSEOD01     *
      *                       (PSTXNREC, FEED - DISP=MOD, EXTEND)      *
      *            GLJNL    - REDRESS EXPENSE JOURNAL (GLJNLREC)       *
      *            LETTERS  - REDRESS PAID LETTER EXTRACT (FB 350)     *
      *            RESPONSE - PAID / REJECTED STATUS BACK TO CMS       *
      *            REPORT   - REDRESS PAYMENTS REPORT (FBA 133)        *
      *----------------------------------------------------------------*
      * EACH REDRESS ITEM APPROVED IN THE COMPLAINTS SYSTEM (CMS) IS   *
      * PAID TO THE CUSTOMER'S MEGABANK ACCOUNT AS UP TO THREE 'TFR '  *
      * CREDITS - REFUND, CONSEQUENTIAL LOSS, 8% INTEREST (NET OF TAX) *
      * - SO THE STATEMENT SHOWS EACH ELEMENT SEPARATELY.              *
      *                                                                *
      * 8% SIMPLE INTEREST (FINANCIAL OMBUDSMAN SERVICE APPROACH):     *
      *   DAYS     = PAYMENT DATE (BUSINESS DATE) - RDID-LOSS-DATE     *
      *   BASE     = REFUND (IF RDID-INT-IND = 'Y')                    *
      *            + CONSEQUENTIAL LOSS (IF RDID-INT-ON-CONSEQ = 'Y')  *
      *   GROSS    = BASE X 8 / 100 X DAYS / 365, ROUNDED TO 1P        *
      *   TAX      = GROSS X 20% BASIC RATE FOR INDIVIDUALS (HMRC -    *
      *              LETTER OFFERS A TAX CERTIFICATE). CIF-BUSINESS    *
      *              CUSTOMERS ARE PAID GROSS.                         *
      *   NET      = GROSS - TAX                                       *
      *                                                                *
      * VALIDATION - AN ITEM THAT FAILS ANY CHECK IS NOT PAID AND IS *
      * REJECTED BACK TO CMS (RESPONSE STATUS 'R' + REASON CODE -    *
      * THE CODES ARE LISTED IN COPYBOOK CMRDRREC).                  *
      *                                                                *
      * GENERAL LEDGER - CHOICE OF CONTRA ACCOUNT                      *
      *   THE CUSTOMER SIDE OF EACH 'TFR ' CREDIT IS JOURNALED BY      *
      *   GLEXT010 TONIGHT: MBK.GLMAP MAPS 'TFR ' AS AN INTERNAL       *
      *   TRANSFER ('I'), SO GLEXT010 POSTS DR 19000-0001 INTERNAL     *
      *   TRANSFER CLEARING / CR PRODUCT BALANCE GL. THIS PROGRAM MUST *
      *   NOT CREDIT THE CUSTOMER BALANCE GL AGAIN, SO IT JOURNALS:    *
      *     DR 53000-0010  COMPLAINTS REDRESS EXPENSE  (GROSS)         *
      *     CR 19000-0001  INTERNAL TRANSFER CLEARING  (NET PAID)      *
      *     CR PRD-GL-TAX-PAYABLE  INCOME TAX ON 8% INTEREST (TAX)     *
      *   SO THAT 19000-0001 NETS TO ZERO ACROSS THE MOVT AND CMPL     *
      *   JOURNALS. A POSTING REJECTED BY PSEOD010 LEAVES A BALANCE    *
      *   ON 19000-0001 FOR FINANCE TO CLEAR.                          *
      *   THE JOURNAL IS MBK.PRD.GL.JOURNAL.CMPL(+1), SOURCE 'CMPL'.   *
      *   GLFEED01 STEP GL020 MUST CONCATENATE THE (0) GENERATION.     *
      *                                                                *
      * PASS 1 PROVES THE FILE (HEADER, TRAILER TOTALS, ORDER) BEFORE  *
      * ANYTHING IS WRITTEN TO THE MOD FEED.                           *
      * RERUN: PSEOD010 REJECTS DUPLICATE EXTERNAL REFERENCES, WHICH   *
      * CARRY THE COMPLAINT REF, SEQ AND ELEMENT. RC 4 IF ANY REJECTED.*
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2016-02-08 PDQ  ORIGINAL VERSION (CMS REPLATFORM - REDRESS WAS *
      *                 PAID BY BRANCH TELLERS FROM SUNDRY ACCOUNTS)   *
      * 2016-06-20 PDQ  INTEREST ON CONSEQUENTIAL LOSS (FOS DECISIONS) *
      * 2016-11-07 HLT  TAX DEDUCTED FROM 8% INTEREST - INDIVIDUALS    *
      *                 ONLY (TAX CERTIFICATE LETTER)                  *
      * 2017-03-27 PDQ  SECOND APPROVER ABOVE 50,000 (INTERNAL AUDIT   *
      *                 FINDING IA-17-04)                              *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REDRESS-FILE     ASSIGN TO REDRESS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REDRESS-STATUS.
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
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT RDRPOST-FILE     ASSIGN TO RDRPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RDRPOST-STATUS.
           SELECT GLJNL-FILE       ASSIGN TO GLJNL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLJNL-STATUS.
           SELECT LETTERS-FILE     ASSIGN TO LETTERS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LETTERS-STATUS.
           SELECT RESPONSE-FILE    ASSIGN TO RESPONSE
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-RESPONSE-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  REDRESS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REDRESS-RECORD                  PIC X(300).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  RDRPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RDRPOST-RECORD                  PIC X(200).
       FD  GLJNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJNL-RECORD                    PIC X(250).
       FD  LETTERS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LETTERS-RECORD                  PIC X(350).
       FD  RESPONSE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RESPONSE-RECORD                 PIC X(100).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CMPRD010'.
       01  WS-CONSTANTS.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00016000.
           05  WS-FOS-RATE-PCT             PIC 9(03)V99 VALUE 008.00.
           05  WS-DAY-BASIS                PIC 9(03) VALUE 365.
           05  WS-BASIC-RATE-PCT           PIC 9(03)V99 VALUE 020.00.
           05  WS-MAX-LOSS-DAYS            PIC 9(05) VALUE 05479.
           05  WS-SECOND-APPROVAL-LIMIT    PIC S9(13)V99 COMP-3
                                           VALUE +50000.00.
           05  WS-MAX-REDRESS              PIC S9(13)V99 COMP-3
                                           VALUE +250000.00.
           05  WS-GL-REDRESS-EXPENSE       PIC X(10) VALUE '53000-0010'.
           05  WS-GL-TRANSFER-CLEARING     PIC X(10) VALUE '19000-0001'.
           05  WS-REDRESS-COST-CENTRE      PIC X(06) VALUE 'CC4710'.
           05  WS-LETTER-TEMPLATE          PIC X(08) VALUE 'RDRPAID1'.
           05  WS-LETTER-TEMPLATE-TAX      PIC X(08) VALUE 'RDRPAIT1'.
       01  WS-FILE-STATUSES.
           05  WS-REDRESS-STATUS           PIC X(02).
               88  WS-REDRESS-OK               VALUE '00'.
               88  WS-REDRESS-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-NOTFND           VALUE '23'.
           05  WS-RDRPOST-STATUS           PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-LETTERS-STATUS           PIC X(02).
           05  WS-RESPONSE-STATUS          PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-REDRESS           VALUE 'Y'.
           05  WS-TRAILER-IND              PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
           COPY CMRDRREC.
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-PREV-ITEM-KEY            PIC X(14) VALUE LOW-VALUES.
           05  WS-REJECT-RSN               PIC X(04).
           05  WS-NOTE                     PIC X(40).
           05  WS-REFUND                   PIC S9(13)V99 COMP-3.
           05  WS-CONSEQ                   PIC S9(13)V99 COMP-3.
           05  WS-PRINCIPAL-TOTAL          PIC S9(13)V99 COMP-3.
           05  WS-INT-BASE                 PIC S9(13)V99 COMP-3.
           05  WS-INT-DAYS                 PIC S9(07) COMP-3.
           05  WS-INT-GROSS                PIC S9(13)V99 COMP-3.
           05  WS-INT-TAX                  PIC S9(13)V99 COMP-3.
           05  WS-INT-NET                  PIC S9(13)V99 COMP-3.
           05  WS-GROSS-COST               PIC S9(13)V99 COMP-3.
           05  WS-NET-PAID                 PIC S9(13)V99 COMP-3.
           05  WS-GL-TAX-PAYABLE           PIC X(10).
           05  WS-SAVE-AMOUNT              PIC S9(13)V99 COMP-3.
           05  WS-SAVE-NARRATIVE           PIC X(35).
           05  WS-PROD-COST-CENTRE         PIC X(06).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-GL-LINE-NO               PIC 9(07) VALUE ZERO.
           05  WS-TIME-NOW                 PIC 9(08).
           05  WS-EXT-REF.
               10  WS-XR-PREFIX            PIC X(03) VALUE 'RDR'.
               10  WS-XR-COMPLAINT-REF     PIC X(12).
               10  WS-XR-SEQ               PIC 9(02).
               10  WS-XR-ELEMENT           PIC X(01).
                   88  WS-XR-REFUND            VALUE 'R'.
                   88  WS-XR-CONSEQ            VALUE 'C'.
                   88  WS-XR-INTEREST          VALUE 'I'.
      *
      *---- FIRST PASS (FILE VALIDATION) ------------------------------*
       01  WS-PASS1.
           05  WS-P1-DETAILS               PIC S9(09) COMP-3.
           05  WS-P1-TRAILERS              PIC S9(09) COMP-3.
           05  WS-P1-REFUND                PIC S9(15)V99 COMP-3.
           05  WS-P1-CONSEQ                PIC S9(15)V99 COMP-3.
           05  WS-P1-PREV-KEY              PIC X(14).
       01  WS-COUNTERS.
           05  WS-ITEMS-READ               PIC S9(09) COMP-3.
           05  WS-ITEMS-PAID               PIC S9(09) COMP-3.
           05  WS-ITEMS-REJECTED           PIC S9(09) COMP-3.
           05  WS-ITEMS-WITH-INT           PIC S9(09) COMP-3.
           05  WS-ITEMS-GROSS-INT          PIC S9(09) COMP-3.
           05  WS-POSTINGS-WRITTEN         PIC S9(09) COMP-3.
           05  WS-TOT-REFUND               PIC S9(15)V99 COMP-3.
           05  WS-TOT-CONSEQ               PIC S9(15)V99 COMP-3.
           05  WS-TOT-INT-GROSS            PIC S9(15)V99 COMP-3.
           05  WS-TOT-INT-TAX              PIC S9(15)V99 COMP-3.
           05  WS-TOT-NET-PAID             PIC S9(15)V99 COMP-3.
           05  WS-TOT-POSTED               PIC S9(15)V99 COMP-3.
           05  WS-TOT-REJECTED             PIC S9(15)V99 COMP-3.
           05  WS-GL-LINES                 PIC S9(09) COMP-3.
           05  WS-GL-DR                    PIC S9(15)V99 COMP-3.
           05  WS-GL-CR                    PIC S9(15)V99 COMP-3.
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CMPRD010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - COMPLAINTS REDRESS PAYMENTS'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(16) VALUE 'COMPLAINT / SEQ'.
           05  FILLER            PIC X(16) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(15) VALUE '        REFUND'.
           05  FILLER            PIC X(15) VALUE '   CONSEQ LOSS'.
           05  FILLER            PIC X(06) VALUE ' DAYS'.
           05  FILLER            PIC X(14) VALUE ' 8PC INT GROSS'.
           05  FILLER            PIC X(11) VALUE '        TAX'.
           05  FILLER            PIC X(15) VALUE '      NET PAID'.
           05  FILLER            PIC X(05) VALUE ' RSN'.
           05  FILLER            PIC X(19) VALUE 'NOTE'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-COMPLAINT-REF            PIC X(12).
           05  FILLER                      PIC X(01) VALUE '/'.
           05  DL-SEQ                      PIC 9(02).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-REFUND                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CONSEQ                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DAYS                     PIC ZZZZ9.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-INT-GROSS                PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-INT-TAX                  PIC ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NET-PAID                 PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-RSN                      PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTE                     PIC X(19).
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
           COPY GLJNLREC.
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
           PERFORM 1100-VALIDATE-FILE
           PERFORM 1200-OPEN-OUTPUTS
           PERFORM 2000-PROCESS-ITEM THRU 2000-EXIT
               UNTIL WS-END-OF-REDRESS
           PERFORM 3000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  DATECARD-FILE
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'DATECARD OPEN FAILED' TO ABN-MESSAGE
               MOVE WS-DATECARD-STATUS     TO ABN-FILE-STATUS
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
           INITIALIZE WS-PASS1 WS-COUNTERS
           MOVE LOW-VALUES                 TO WS-P1-PREV-KEY
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           .
      *================================================================*
       1100-VALIDATE-FILE.
      *================================================================*
      *    FIRST PASS - NOTHING IS WRITTEN UNTIL THE FILE IS PROVEN
      *    COMPLETE (HEADER FIRST, ONE TRAILER LAST, COUNTS AND TOTALS
      *    AGREE, ITEMS IN ASCENDING ORDER).
           OPEN INPUT REDRESS-FILE
           IF NOT WS-REDRESS-OK
               MOVE '1100-VALIDATE-FILE'   TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'REDRESS OPEN FAILED'  TO ABN-MESSAGE
               MOVE WS-REDRESS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           PERFORM 8000-READ-REDRESS
           IF WS-END-OF-REDRESS
           OR NOT RDI-HEADER
           OR NOT RDIH-VALID-FILE-ID
               MOVE '1100-VALIDATE-FILE'   TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'REDRESS FILE HAS NO VALID HEADER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           PERFORM UNTIL WS-END-OF-REDRESS
               PERFORM 8000-READ-REDRESS
               IF NOT WS-END-OF-REDRESS
                   PERFORM 1150-CHECK-RECORD
               END-IF
           END-PERFORM
           IF WS-P1-TRAILERS NOT = 1
               MOVE '1100-VALIDATE-FILE'   TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'REDRESS FILE HAS NO TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF RDIT-RECORD-COUNT NOT = WS-P1-DETAILS
           OR RDIT-TOTAL-REFUND NOT = WS-P1-REFUND
           OR RDIT-TOTAL-CONSEQ NOT = WS-P1-CONSEQ
               MOVE '1100-VALIDATE-FILE'   TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'REDRESS TRAILER DOES NOT AGREE WITH DETAILS'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE REDRESS-FILE
      *    SECOND PASS STARTS HERE - REOPEN AND SKIP THE HEADER
           MOVE 'N'                        TO WS-EOF-IND
                                              WS-TRAILER-IND
           OPEN INPUT REDRESS-FILE
           IF NOT WS-REDRESS-OK
               MOVE '1100-VALIDATE-FILE'   TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'REDRESS REOPEN FAILED' TO ABN-MESSAGE
               MOVE WS-REDRESS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           PERFORM 8000-READ-REDRESS
           PERFORM 8000-READ-REDRESS
           .
      *================================================================*
       1150-CHECK-RECORD.
      *================================================================*
           IF WS-TRAILER-SEEN
               MOVE '1150-CHECK-RECORD'    TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'RECORDS FOUND AFTER THE TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           EVALUATE TRUE
               WHEN RDI-DETAIL
                   ADD 1                   TO WS-P1-DETAILS
                   IF RDID-REFUND-AMT NUMERIC
                       ADD RDID-REFUND-AMT TO WS-P1-REFUND
                   END-IF
                   IF RDID-CONSEQ-LOSS-AMT NUMERIC
                       ADD RDID-CONSEQ-LOSS-AMT TO WS-P1-CONSEQ
                   END-IF
                   IF RDID-ITEM-KEY < WS-P1-PREV-KEY
                       MOVE '1150-CHECK-RECORD' TO ABN-PARAGRAPH
                       MOVE 1006           TO ABN-ABEND-CODE
                       MOVE 'REDRESS FILE NOT IN COMPLAINT REF ORDER'
                                           TO ABN-MESSAGE
                       MOVE RDID-ITEM-KEY  TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
                   MOVE RDID-ITEM-KEY      TO WS-P1-PREV-KEY
               WHEN RDI-TRAILER
                   ADD 1                   TO WS-P1-TRAILERS
                   SET WS-TRAILER-SEEN     TO TRUE
               WHEN OTHER
                   MOVE '1150-CHECK-RECORD' TO ABN-PARAGRAPH
                   MOVE 1007               TO ABN-ABEND-CODE
                   MOVE 'UNEXPECTED RECORD TYPE IN REDRESS FILE'
                                           TO ABN-MESSAGE
                   MOVE RDI-REC-TYPE       TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       1200-OPEN-OUTPUTS.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       CIFMAST-FILE
                       PRDPARM-FILE
                EXTEND RDRPOST-FILE
                OUTPUT GLJNL-FILE
                       LETTERS-FILE
                       RESPONSE-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-RDRPOST-STATUS NOT = '00'
           OR WS-GLJNL-STATUS NOT = '00'
           OR WS-LETTERS-STATUS NOT = '00'
           OR WS-RESPONSE-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
               MOVE '1200-OPEN-OUTPUTS'    TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
      *---- GL JOURNAL HEADER -----------------------------------------*
           ACCEPT WS-TIME-NOW              FROM TIME
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
      *    NO 88 FOR COMPLAINTS IN GLJNLREC - SOURCE MOVED DIRECTLY
           MOVE 'CMPL'                     TO GLJ-SOURCE-SYSTEM
           STRING 'CMPRD' WS-BUS-DATE
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE 'CMPRD'                    TO GLJH-RUN-ID
           MOVE WS-BUS-DATE                TO GLJH-CREATE-DATE
           MOVE WS-TIME-NOW (1:6)          TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 6900-WRITE-GL
           .
      *================================================================*
       2000-PROCESS-ITEM.
      *================================================================*
           IF RDI-TRAILER
               SET WS-END-OF-REDRESS       TO TRUE
               GO TO 2000-EXIT
           END-IF
           ADD 1                           TO WS-ITEMS-READ
           MOVE SPACES                     TO WS-REJECT-RSN WS-NOTE
           MOVE ZERO                       TO WS-REFUND WS-CONSEQ
                                              WS-PRINCIPAL-TOTAL
                                              WS-INT-BASE WS-INT-DAYS
                                              WS-INT-GROSS WS-INT-TAX
                                              WS-INT-NET WS-GROSS-COST
                                              WS-NET-PAID
           PERFORM 2100-VALIDATE-ITEM THRU 2100-EXIT
           IF WS-REJECT-RSN = SPACES
               PERFORM 2200-CHECK-ACCOUNT THRU 2200-EXIT
           END-IF
           IF WS-REJECT-RSN = SPACES
               PERFORM 2300-CALC-INTEREST
               PERFORM 2400-PAY
               PERFORM 2450-JOURNAL
               PERFORM 2500-LETTER
           ELSE
               PERFORM 2600-REJECT
           END-IF
           PERFORM 2700-RESPONSE
           PERFORM 7100-REPORT-DETAIL
           MOVE RDID-ITEM-KEY              TO WS-PREV-ITEM-KEY
           PERFORM 8000-READ-REDRESS
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-VALIDATE-ITEM.
      *================================================================*
           IF RDID-ITEM-KEY = WS-PREV-ITEM-KEY
               MOVE 'DUPL'                 TO WS-REJECT-RSN
               MOVE 'DUPLICATE COMPLAINT REF / SEQ IN FILE'
                                           TO WS-NOTE
               GO TO 2100-EXIT
           END-IF
      *---- AMOUNTS ---------------------------------------------------*
           IF RDID-REFUND-AMT NOT NUMERIC
           OR RDID-CONSEQ-LOSS-AMT NOT NUMERIC
               MOVE 'AMNT'                 TO WS-REJECT-RSN
               MOVE 'AMOUNT NOT NUMERIC'   TO WS-NOTE
               GO TO 2100-EXIT
           END-IF
           MOVE RDID-REFUND-AMT            TO WS-REFUND
           MOVE RDID-CONSEQ-LOSS-AMT       TO WS-CONSEQ
           COMPUTE WS-PRINCIPAL-TOTAL = WS-REFUND + WS-CONSEQ
           IF WS-PRINCIPAL-TOTAL NOT > ZERO
               MOVE 'AMNT'                 TO WS-REJECT-RSN
               MOVE 'NOTHING TO PAY'       TO WS-NOTE
               GO TO 2100-EXIT
           END-IF
           IF WS-PRINCIPAL-TOTAL > WS-MAX-REDRESS
               MOVE 'LIMT'                 TO WS-REJECT-RSN
               MOVE 'ABOVE BATCH LIMIT - PAY BY CHAPS'
                                           TO WS-NOTE
               GO TO 2100-EXIT
           END-IF
      *---- LOSS DATE (ONLY NEEDED IF INTEREST IS DUE) ----------------*
           IF RDID-INT-ON-REFUND OR RDID-INT-ON-CONSEQ-LOSS
               IF RDID-LOSS-DATE NOT NUMERIC
               OR RDID-LOSS-CCYY < 1990
               OR RDID-LOSS-MM < 01 OR RDID-LOSS-MM > 12
               OR RDID-LOSS-DD < 01 OR RDID-LOSS-DD > 31
                   MOVE 'LDTE'             TO WS-REJECT-RSN
                   MOVE 'LOSS DATE INVALID' TO WS-NOTE
                   GO TO 2100-EXIT
               END-IF
               IF RDID-LOSS-DATE > WS-BUS-DATE
                   MOVE 'LDTE'             TO WS-REJECT-RSN
                   MOVE 'LOSS DATE IN THE FUTURE' TO WS-NOTE
                   GO TO 2100-EXIT
               END-IF
               COMPUTE WS-INT-DAYS =
                       FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
                     - FUNCTION INTEGER-OF-DATE (RDID-LOSS-DATE)
               IF WS-INT-DAYS > WS-MAX-LOSS-DAYS
                   MOVE 'LDTE'             TO WS-REJECT-RSN
                   MOVE 'LOSS DATE OVER 15 YEARS AGO - CHECK'
                                           TO WS-NOTE
                   GO TO 2100-EXIT
               END-IF
           END-IF
      *---- APPROVAL (FOUR EYES) --------------------------------------*
           IF RDID-APPROVER-ID = SPACES
           OR RDID-APPROVER-ID = RDID-HANDLER-ID
               MOVE 'APPR'                 TO WS-REJECT-RSN
               MOVE 'NO INDEPENDENT APPROVER' TO WS-NOTE
               GO TO 2100-EXIT
           END-IF
           IF RDID-APPROVED-DATE NOT NUMERIC
           OR RDID-APPROVED-DATE > WS-BUS-DATE
               MOVE 'APPR'                 TO WS-REJECT-RSN
               MOVE 'APPROVAL DATE INVALID' TO WS-NOTE
               GO TO 2100-EXIT
           END-IF
           IF WS-PRINCIPAL-TOTAL > WS-SECOND-APPROVAL-LIMIT
               IF RDID-APPROVER-2-ID = SPACES
               OR RDID-APPROVER-2-ID = RDID-APPROVER-ID
               OR RDID-APPROVER-2-ID = RDID-HANDLER-ID
                   MOVE 'AUTH'             TO WS-REJECT-RSN
                   MOVE 'SECOND APPROVER REQUIRED OVER 50,000'
                                           TO WS-NOTE
                   GO TO 2100-EXIT
               END-IF
           END-IF
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-CHECK-ACCOUNT.
      *================================================================*
           MOVE RDID-SORT-CODE             TO ACM-SORT-CODE
           MOVE RDID-ACCT-NO               TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'NACC'             TO WS-REJECT-RSN
                   MOVE 'ACCOUNT NOT FOUND' TO WS-NOTE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE '2200-CHECK-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               WHEN ACM-CLOSED OR ACM-PENDING-CLOSE
                   MOVE 'ACLS'             TO WS-REJECT-RSN
                   MOVE 'ACCOUNT CLOSED - PAY BY CHEQUE' TO WS-NOTE
               WHEN ACM-FROZEN OR ACM-NO-CREDITS OR ACM-NO-POSTINGS
                   MOVE 'ARST'             TO WS-REJECT-RSN
                   MOVE 'ACCOUNT FROZEN / CREDITS RESTRICTED'
                                           TO WS-NOTE
               WHEN ACM-CCY-CD NOT = 'GBP'
                   MOVE 'CCYX'             TO WS-REJECT-RSN
                   MOVE 'ACCOUNT NOT STERLING' TO WS-NOTE
               WHEN ACM-CIF-NO NOT = RDID-CIF-NO
                   MOVE 'CIFM'             TO WS-REJECT-RSN
                   MOVE 'CIF IS NOT THE ACCOUNT HOLDER' TO WS-NOTE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-RSN NOT = SPACES
               GO TO 2200-EXIT
           END-IF
      *---- CUSTOMER (STATUS, NAME AND ADDRESS FOR THE LETTER) --------*
           MOVE RDID-CIF-NO                TO CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-NOTFND
                   MOVE 'NCIF'             TO WS-REJECT-RSN
                   MOVE 'CUSTOMER NOT ON CIF' TO WS-NOTE
               WHEN NOT WS-CIFMAST-OK
                   MOVE '2200-CHECK-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE CIF-NO             TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               WHEN CIF-DECEASED
                   MOVE 'DCSD'             TO WS-REJECT-RSN
                   MOVE 'CUSTOMER DECEASED - BEREAVEMENT TEAM'
                                           TO WS-NOTE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-RSN NOT = SPACES
               GO TO 2200-EXIT
           END-IF
      *---- PRODUCT (TAX PAYABLE GL FOR THE INTEREST TAX) -------------*
           MOVE ACM-PROD-CD                TO PRD-PROD-CD
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN WS-PRDPARM-OK
                   MOVE PRD-GL-TAX-PAYABLE TO WS-GL-TAX-PAYABLE
                   MOVE PRD-COST-CENTRE    TO WS-PROD-COST-CENTRE
                   IF WS-GL-TAX-PAYABLE = SPACES
                       MOVE 'NPRD'         TO WS-REJECT-RSN
                       MOVE 'PRODUCT HAS NO TAX PAYABLE GL'
                                           TO WS-NOTE
                   END-IF
               WHEN WS-PRDPARM-NOTFND
                   MOVE 'NPRD'             TO WS-REJECT-RSN
                   MOVE 'NO PRODUCT PARAMETERS' TO WS-NOTE
               WHEN OTHER
                   MOVE '2200-CHECK-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PRDPARM READ FAILED' TO ABN-MESSAGE
                   MOVE WS-PRDPARM-STATUS  TO ABN-FILE-STATUS
                   MOVE PRD-PROD-CD        TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-CALC-INTEREST.
      *================================================================*
      *    8% SIMPLE INTEREST FROM LOSS DATE TO PAYMENT DATE - SEE
      *    HEADER. WS-INT-DAYS WAS SET IN 2100-VALIDATE-ITEM.
           IF RDID-INT-ON-REFUND
               ADD WS-REFUND               TO WS-INT-BASE
           END-IF
           IF RDID-INT-ON-CONSEQ-LOSS
               ADD WS-CONSEQ               TO WS-INT-BASE
           END-IF
           IF WS-INT-BASE > ZERO
           AND WS-INT-DAYS > ZERO
               COMPUTE WS-INT-GROSS ROUNDED =
                       WS-INT-BASE * WS-FOS-RATE-PCT / 100
                     * WS-INT-DAYS / WS-DAY-BASIS
               ADD 1                       TO WS-ITEMS-WITH-INT
           ELSE
               MOVE ZERO                   TO WS-INT-GROSS
           END-IF
      *    BASIC RATE TAX FOR INDIVIDUALS; BUSINESSES PAID GROSS
           IF WS-INT-GROSS > ZERO
               IF CIF-BUSINESS
                   MOVE ZERO               TO WS-INT-TAX
                   ADD 1                   TO WS-ITEMS-GROSS-INT
               ELSE
                   COMPUTE WS-INT-TAX ROUNDED =
                           WS-INT-GROSS * WS-BASIC-RATE-PCT / 100
               END-IF
           END-IF
           COMPUTE WS-INT-NET   = WS-INT-GROSS - WS-INT-TAX
           COMPUTE WS-GROSS-COST = WS-REFUND + WS-CONSEQ + WS-INT-GROSS
           COMPUTE WS-NET-PAID  = WS-REFUND + WS-CONSEQ + WS-INT-NET
           .
      *================================================================*
       2400-PAY.
      *================================================================*
           MOVE RDID-COMPLAINT-REF         TO WS-XR-COMPLAINT-REF
           MOVE RDID-REDRESS-SEQ           TO WS-XR-SEQ
           IF WS-REFUND > ZERO
               SET WS-XR-REFUND            TO TRUE
               MOVE WS-REFUND              TO PST-AMOUNT
               MOVE 'COMPLAINT REDRESS - REFUND'
                                           TO PST-NARRATIVE-1
               PERFORM 5000-WRITE-CREDIT
           END-IF
           IF WS-CONSEQ > ZERO
               SET WS-XR-CONSEQ            TO TRUE
               MOVE WS-CONSEQ              TO PST-AMOUNT
               MOVE 'COMPLAINT REDRESS - COMPENSATION'
                                           TO PST-NARRATIVE-1
               PERFORM 5000-WRITE-CREDIT
           END-IF
           IF WS-INT-NET > ZERO
               SET WS-XR-INTEREST          TO TRUE
               MOVE WS-INT-NET             TO PST-AMOUNT
               IF WS-INT-TAX > ZERO
                   MOVE 'REDRESS 8PC INTEREST NET OF TAX'
                                           TO PST-NARRATIVE-1
               ELSE
                   MOVE 'REDRESS 8PC INTEREST'
                                           TO PST-NARRATIVE-1
               END-IF
               PERFORM 5000-WRITE-CREDIT
           END-IF
           ADD 1                           TO WS-ITEMS-PAID
           ADD WS-REFUND                   TO WS-TOT-REFUND
           ADD WS-CONSEQ                   TO WS-TOT-CONSEQ
           ADD WS-INT-GROSS                TO WS-TOT-INT-GROSS
           ADD WS-INT-TAX                  TO WS-TOT-INT-TAX
           ADD WS-NET-PAID                 TO WS-TOT-NET-PAID
           MOVE 'PAID'                     TO WS-NOTE
           .
      *================================================================*
       2450-JOURNAL.
      *================================================================*
      *    DR EXPENSE (GROSS) / CR CLEARING (NET) / CR TAX (TAX)
           MOVE SPACES                     TO GLJ-BODY
           MOVE WS-GL-REDRESS-EXPENSE      TO GLJD-GL-ACCOUNT
           MOVE WS-REDRESS-COST-CENTRE     TO GLJD-COST-CENTRE
           SET GLJD-DEBIT                  TO TRUE
           MOVE WS-GROSS-COST              TO GLJD-AMOUNT
           MOVE 'COMPLAINTS REDRESS EXPENSE' TO GLJD-DESCRIPTION
           PERFORM 6000-GL-DETAIL
           MOVE SPACES                     TO GLJ-BODY
           MOVE WS-GL-TRANSFER-CLEARING    TO GLJD-GL-ACCOUNT
           MOVE WS-PROD-COST-CENTRE        TO GLJD-COST-CENTRE
           SET GLJD-CREDIT                 TO TRUE
           MOVE WS-NET-PAID                TO GLJD-AMOUNT
           MOVE 'REDRESS PAID TO CUSTOMER (TFR CLEARING)'
                                           TO GLJD-DESCRIPTION
           PERFORM 6000-GL-DETAIL
           IF WS-INT-TAX > ZERO
               MOVE SPACES                 TO GLJ-BODY
               MOVE WS-GL-TAX-PAYABLE      TO GLJD-GL-ACCOUNT
               MOVE WS-PROD-COST-CENTRE    TO GLJD-COST-CENTRE
               SET GLJD-CREDIT             TO TRUE
               MOVE WS-INT-TAX             TO GLJD-AMOUNT
               MOVE 'TAX ON REDRESS 8PC INTEREST'
                                           TO GLJD-DESCRIPTION
               PERFORM 6000-GL-DETAIL
           END-IF
           .
      *================================================================*
       2500-LETTER.
      *================================================================*
           MOVE SPACES                     TO RDR-LETTER
           MOVE 'RDRP'                     TO LT-LETTER-CD
           IF WS-INT-TAX > ZERO
      *        TEMPLATE WITH THE R185-STYLE TAX DEDUCTION STATEMENT
               MOVE WS-LETTER-TEMPLATE-TAX TO LT-TEMPLATE-ID
           ELSE
               MOVE WS-LETTER-TEMPLATE     TO LT-TEMPLATE-ID
           END-IF
           MOVE WS-BUS-DATE                TO LT-ISSUE-DATE
           MOVE CIF-NO                     TO LT-CIF-NO
           MOVE CIF-TITLE                  TO LT-TITLE
           MOVE CIF-FIRST-NM               TO LT-FIRST-NM
           MOVE CIF-LAST-NM                TO LT-LAST-NM
           MOVE CIF-ADDR-LINE1             TO LT-ADDR-LINE1
           MOVE CIF-ADDR-LINE2             TO LT-ADDR-LINE2
           MOVE CIF-TOWN                   TO LT-TOWN
           MOVE CIF-POST-CD                TO LT-POST-CD
           MOVE RDID-COMPLAINT-REF         TO LT-COMPLAINT-REF
           MOVE RDID-FOS-REF               TO LT-FOS-REF
           MOVE RDID-SORT-CODE             TO LT-SORT-CODE
           MOVE RDID-ACCT-NO (5:4)         TO LT-ACCT-LAST4
           MOVE WS-REFUND                  TO LT-REFUND
           MOVE WS-CONSEQ                  TO LT-CONSEQ
           MOVE WS-INT-GROSS               TO LT-INT-GROSS
           MOVE WS-INT-TAX                 TO LT-INT-TAX
           MOVE WS-NET-PAID                TO LT-NET-TOTAL
           IF WS-INT-GROSS > ZERO
               MOVE RDID-LOSS-DATE         TO LT-LOSS-DATE
               MOVE WS-INT-DAYS            TO LT-INT-DAYS
           ELSE
               MOVE ZERO                   TO LT-LOSS-DATE LT-INT-DAYS
           END-IF
           MOVE WS-BUS-DATE                TO LT-PAY-DATE
           WRITE LETTERS-RECORD            FROM RDR-LETTER
           IF WS-LETTERS-STATUS NOT = '00'
               MOVE '2500-LETTER'          TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'LETTERS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-LETTERS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       2600-REJECT.
      *================================================================*
           ADD 1                           TO WS-ITEMS-REJECTED
           IF WS-PRINCIPAL-TOTAL > ZERO
               ADD WS-PRINCIPAL-TOTAL      TO WS-TOT-REJECTED
           END-IF
           .
      *================================================================*
       2700-RESPONSE.
      *================================================================*
           MOVE SPACES                     TO RDR-RESPONSE
           MOVE RDID-COMPLAINT-REF         TO RS-COMPLAINT-REF
           MOVE RDID-REDRESS-SEQ           TO RS-REDRESS-SEQ
           IF WS-REJECT-RSN = SPACES
               SET RS-PAID                 TO TRUE
               MOVE WS-BUS-DATE            TO RS-PAY-DATE
               MOVE WS-NET-PAID            TO RS-NET-PAID
               MOVE WS-INT-GROSS           TO RS-INT-GROSS
               MOVE WS-INT-TAX             TO RS-INT-TAX
           ELSE
               SET RS-REJECTED             TO TRUE
               MOVE WS-REJECT-RSN          TO RS-REASON
               MOVE ZERO                   TO RS-PAY-DATE RS-NET-PAID
                                              RS-INT-GROSS RS-INT-TAX
           END-IF
           MOVE WS-NOTE                    TO RS-TEXT
           WRITE RESPONSE-RECORD           FROM RDR-RESPONSE
           IF WS-RESPONSE-STATUS NOT = '00'
               MOVE '2700-RESPONSE'        TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'RESPONSE WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-RESPONSE-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       5000-WRITE-CREDIT.
      *================================================================*
      *    CALLER SETS PST-AMOUNT, PST-NARRATIVE-1 AND WS-XR-ELEMENT.
      *    THE OTHER FIELDS ARE SET HERE, SO SAVE THE CALLER'S FIRST.
           MOVE PST-AMOUNT                 TO WS-SAVE-AMOUNT
           MOVE PST-NARRATIVE-1            TO WS-SAVE-NARRATIVE
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE WS-SAVE-AMOUNT             TO PST-AMOUNT
           MOVE WS-SAVE-NARRATIVE          TO PST-NARRATIVE-1
           MOVE RDID-SORT-CODE             TO PST-SORT-CODE
           MOVE RDID-ACCT-NO               TO PST-ACCT-NO
           SET PST-CREDIT                  TO TRUE
           MOVE 'TFR '                     TO PST-TXN-CODE
           STRING 'COMPLAINT REF ' RDID-COMPLAINT-REF
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE WS-BUS-DATE                TO PST-POST-DATE
                                              PST-VALUE-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           MOVE WS-EXT-REF                 TO PST-EXT-REF
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE RDRPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-RDRPOST-STATUS NOT = '00'
               MOVE '5000-WRITE-CREDIT'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'RDRPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-RDRPOST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-POSTINGS-WRITTEN
           ADD PST-AMOUNT                  TO WS-TOT-POSTED
           .
      *================================================================*
       6000-GL-DETAIL.
      *================================================================*
      *    CALLER CLEARS GLJ-BODY AND SETS ACCOUNT, COST CENTRE,
      *    DIRECTION, AMOUNT AND DESCRIPTION.
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-GL-LINE-NO
           MOVE WS-GL-LINE-NO              TO GLJD-LINE-NO
           MOVE ACM-PROD-CD                TO GLJD-PROD-CD
           MOVE 'GBP'                      TO GLJD-CCY-CD
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE RDID-SORT-CODE             TO GLJD-SORT-CODE
           MOVE RDID-ACCT-NO               TO GLJD-ACCT-NO
           MOVE 'TFR '                     TO GLJD-TXN-CODE
           STRING 'RDR' RDID-COMPLAINT-REF RDID-REDRESS-SEQ
                  DELIMITED BY SIZE
                  INTO GLJD-SOURCE-REF
           END-STRING
           PERFORM 6900-WRITE-GL
           ADD 1                           TO WS-GL-LINES
           IF GLJD-DEBIT
               ADD GLJD-AMOUNT             TO WS-GL-DR
           ELSE
               ADD GLJD-AMOUNT             TO WS-GL-CR
           END-IF
           .
      *================================================================*
       6900-WRITE-GL.
      *================================================================*
           WRITE GLJNL-RECORD              FROM GLJ-JOURNAL-LINE
           IF WS-GLJNL-STATUS NOT = '00'
               MOVE '6900-WRITE-GL'        TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'GLJNL WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-GLJNL-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
      *---- JOURNAL MUST BALANCE (GLREC010 WOULD REJECT THE FEED) -----*
           IF WS-GL-DR NOT = WS-GL-CR
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'REDRESS JOURNAL DOES NOT BALANCE'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
      *---- POSTINGS MUST EQUAL THE NET PAID --------------------------*
           IF WS-TOT-POSTED NOT = WS-TOT-NET-PAID
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'RDRPOST TOTAL NOT EQUAL TO NET REDRESS PAID'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-GL-LINES                TO GLJT-LINE-COUNT
           MOVE WS-GL-DR                   TO GLJT-TOTAL-DR
           MOVE WS-GL-CR                   TO GLJT-TOTAL-CR
           MOVE 1                          TO GLJT-JOURNAL-COUNT
           PERFORM 6900-WRITE-GL
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'REDRESS ITEMS READ'       TO TL-LABEL
           MOVE WS-ITEMS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'ITEMS PAID'               TO TL-LABEL
           MOVE WS-ITEMS-PAID              TO TL-COUNT
           MOVE WS-TOT-NET-PAID            TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  REFUNDS'                TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOT-REFUND              TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  CONSEQUENTIAL LOSS'     TO TL-LABEL
           MOVE WS-TOT-CONSEQ              TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  8PC INTEREST GROSS (ITEMS WITH INT)' TO TL-LABEL
           MOVE WS-ITEMS-WITH-INT          TO TL-COUNT
           MOVE WS-TOT-INT-GROSS           TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE '  TAX DEDUCTED / BUSINESSES PAID GROSS'
                                           TO TL-LABEL
           MOVE WS-ITEMS-GROSS-INT         TO TL-COUNT
           MOVE WS-TOT-INT-TAX             TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'ITEMS REJECTED (PRINCIPAL)' TO TL-LABEL
           MOVE WS-ITEMS-REJECTED          TO TL-COUNT
           MOVE WS-TOT-REJECTED            TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'RDRPOST CREDITS WRITTEN'  TO TL-LABEL
           MOVE WS-POSTINGS-WRITTEN        TO TL-COUNT
           MOVE WS-TOT-POSTED              TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           MOVE 'GL LINES (DEBITS)'        TO TL-LABEL
           MOVE WS-GL-LINES                TO TL-COUNT
           MOVE WS-GL-DR                   TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL
           CLOSE REDRESS-FILE ACCTMAST-FILE CIFMAST-FILE PRDPARM-FILE
                 RDRPOST-FILE GLJNL-FILE LETTERS-FILE RESPONSE-FILE
                 REPORT-FILE
           DISPLAY 'CMPRD010 - PAID ' WS-ITEMS-PAID
                   ' REJECTED ' WS-ITEMS-REJECTED
           IF WS-ITEMS-REJECTED > ZERO
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
           MOVE 3                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-REPORT-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE RDID-COMPLAINT-REF         TO DL-COMPLAINT-REF
           MOVE RDID-REDRESS-SEQ           TO DL-SEQ
           MOVE RDID-SORT-CODE             TO DL-SORT-CODE
           MOVE RDID-ACCT-NO               TO DL-ACCT-NO
           MOVE WS-REFUND                  TO DL-REFUND
           MOVE WS-CONSEQ                  TO DL-CONSEQ
           MOVE WS-INT-DAYS                TO DL-DAYS
           MOVE WS-INT-GROSS               TO DL-INT-GROSS
           MOVE WS-INT-TAX                 TO DL-INT-TAX
           MOVE WS-NET-PAID                TO DL-NET-PAID
           MOVE WS-REJECT-RSN              TO DL-RSN
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '7100-REPORT-DETAIL'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'REPORT WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-REPORT-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       7200-WRITE-TOTAL.
      *================================================================*
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           .
      *================================================================*
       8000-READ-REDRESS.
      *================================================================*
           READ REDRESS-FILE INTO RDR-REDRESS-RECORD
           EVALUATE TRUE
               WHEN WS-REDRESS-OK
                   CONTINUE
               WHEN WS-REDRESS-EOF
                   SET WS-END-OF-REDRESS   TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-REDRESS' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'REDRESS READ FAILED' TO ABN-MESSAGE
                   MOVE WS-REDRESS-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
