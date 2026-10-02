       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PYBGC010.
       AUTHOR.        G T WALSH.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2015-02-23.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PYBGC010                                            *
      * TITLE    : BRANCH PAYING-IN SLIPS AND BANK GIRO CREDITS        *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : PYBGC01 STEP BG010 (DAILY, BEFORE PSEOD01)          *
      *----------------------------------------------------------------*
      * INPUT    : IMGCAP   - IMAGE CAPTURE FILE FROM THE BRANCH       *
      *                       SCANNERS (FB 250 - LAYOUT BELOW)         *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA MASTER (RANDOM) - UNCLEARED TOTAL    *
      *            UNCLEAR  - UNCLEARED DEPOSITS (CHQRECS CQD-, KSDS)  *
      * OUTPUT   : BGCPOST  - 'BGC ' CREDITS FOR TONIGHT'S PSEOD01     *
      *                       (PSTXNREC, FEED - DISP=MOD)              *
      *            CLROUT   - OUTWARD CLEARING: CREDITS FOR OTHER      *
      *                       BANKS AND ALL CHEQUES PAID IN (FB 150)   *
      *            GLJNL    - BRANCH SUSPENSE JOURNAL (GLJNLREC)       *
      *            REPORT   - SLIP RECONCILIATION REPORT (FBA 133)     *
      *----------------------------------------------------------------*
      * EVERY CREDIT PAID IN OVER A BRANCH COUNTER (PAYING-IN BOOK     *
      * SLIP OR BANK GIRO CREDIT) IS SCANNED: ONE 'S' SLIP RECORD WITH *
      * THE TELLER-COUNTED CASH, FOLLOWED BY ONE 'Q' RECORD PER CHEQUE.*
      *                                                                *
      * RECONCILIATION: SLIP TOTAL (WRITTEN BY THE CUSTOMER) MUST EQUAL*
      * CASH + THE CHEQUES CAPTURED. THE CUSTOMER IS CREDITED WITH WHAT*
      * WAS ACTUALLY RECEIVED (CASH + CHEQUES); ANY DIFFERENCE GOES TO *
      * BRANCH SUSPENSE 29920-0100 (COST CENTRE = BRANCH SORT CODE)    *
      * AGAINST TILL DIFFERENCES 18300-0100 FOR THE BRANCH TO CLEAR.   *
      *                                                                *
      * MEGABANK BENEFICIARY (SLP-BENE-IND 'M'):                       *
      *   'BGC ' CREDIT FOR THE CASH - AVAILABLE AT ONCE.              *
      *   'BGC ' CREDIT FOR THE CHEQUES, HELD AS UNCLEARED: ONE CQD-   *
      *   RECORD PER CHEQUE ON UNCLEAR (RELEASED BY CQUNC010) AND      *
      *   ACM-UNCLEARED-AMT RAISED. PSEOD010 ADDS EVERY HARD CREDIT TO *
      *   THE AVAILABLE BALANCE, SO THE CHEQUE AMOUNT IS TAKEN OFF     *
      *   ACM-AVAIL-BAL HERE IN ADVANCE (NET EFFECT: CASH ONLY).       *
      *   CLEARING: IMAGE CLEARING (ICS) CHEQUES - FUNDS AND CERTAINTY *
      *   NEXT BUSINESS DAY; PAPER CHEQUES - 2-4-6 WORKING DAYS.       *
      *   AN ACCOUNT THAT IS MISSING, CLOSED OR BLOCKED FOR CREDITS    *
      *   CANNOT BE CREDITED - THE SLIP GOES TO BRANCH SUSPENSE.       *
      * OTHER BANK BENEFICIARY ('O'): A CREDIT ITEM ON CLROUT FOR THE  *
      *   CREDIT CLEARING, AGAINST CREDITS IN TRANSIT 18200-0100 /     *
      *   CREDIT CLEARING SETTLEMENT 28600-0100.                       *
      * EVERY CHEQUE, WHOEVER IT IS FOR, IS A DEBIT ITEM ON CLROUT FOR *
      * PRESENTATION TO THE DRAWEE BANK.                               *
      *                                                                *
      * RERUN: CQD- KEYS USE SEQUENCES FROM WS-CQD-SEQ-BASE (THE TELLER*
      * SYSTEM USES 000001-499999). A DUPLICATE KEY MEANS THE SLIP WAS *
      * PROCESSED BY AN EARLIER RUN - IT IS SKIPPED AND REPORTED.      *
      *                                                                *
      * KNOWN ISSUE PIR 2016/087 OPEN: 2-4-6 WORKING DAYS SKIP WEEKENDS*
      * BUT NOT BANK HOLIDAYS - FUNDS CAN BE RELEASED A DAY EARLY OVER *
      * A BANK HOLIDAY (BANKHOL IS NOT READ HERE). ICS ITEMS USE       *
      * DTC-NEXT-BUS-DATE AND ARE NOT AFFECTED.                        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2015-02-23 GTW  ORIGINAL VERSION (BRANCH IMAGE CAPTURE ROLL-OUT*
      *                 - REPLACES THE BRANCH BGC BATCH SHEETS)        *
      * 2016-05-09 GTW  BRANCH SUSPENSE JOURNAL (AUDIT IA-16-011)      *
      * 2017-10-30 PDQ  IMAGE CLEARING SYSTEM (ICS) - NEXT-DAY FUNDS FOR
      *                 ITEMS FLAGGED IMGQ-ICS-IND                     *
      * 2018-03-19 PDQ  RERUN PROTECTION ON DUPLICATE CQD- KEY         *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IMGCAP-FILE      ASSIGN TO IMGCAP
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-IMGCAP-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT UNCLEAR-FILE     ASSIGN TO UNCLEAR
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS UNC-KEY
                                   FILE STATUS IS WS-UNCLEAR-STATUS.
           SELECT BGCPOST-FILE     ASSIGN TO BGCPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-BGCPOST-STATUS.
           SELECT CLROUT-FILE      ASSIGN TO CLROUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-CLROUT-STATUS.
           SELECT GLJNL-FILE       ASSIGN TO GLJNL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLJNL-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  IMGCAP-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  IMGCAP-RECORD                   PIC X(250).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  UNCLEAR-FILE.
       01  UNCLEAR-RECORD.
           05  UNC-KEY                     PIC X(28).
           05  FILLER                      PIC X(92).
       FD  BGCPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BGCPOST-RECORD                  PIC X(200).
       FD  CLROUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CLROUT-RECORD                   PIC X(150).
       FD  GLJNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJNL-RECORD                    PIC X(250).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PYBGC010'.
      *
       01  WS-CONSTANTS.
           05  WS-CQD-SEQ-BASE             PIC 9(06) VALUE 500000.
           05  WS-ICS-GO-LIVE              PIC 9(08) VALUE 20171030.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00020000.
           05  WS-MAX-CHEQUES              PIC S9(04) COMP VALUE +60.
           05  WS-MAX-BRANCHES             PIC S9(04) COMP VALUE +900.
           05  WS-GL-BRANCH-SUSP           PIC X(10) VALUE '29920-0100'.
           05  WS-GL-TILL-DIFF             PIC X(10) VALUE '18300-0100'.
           05  WS-GL-IN-TRANSIT            PIC X(10) VALUE '18200-0100'.
           05  WS-GL-CREDIT-CLRG           PIC X(10) VALUE '28600-0100'.
      *
       01  WS-FILE-STATUSES.
           05  WS-IMGCAP-STATUS            PIC X(02).
               88  WS-IMGCAP-OK                VALUE '00'.
               88  WS-IMGCAP-EOF               VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-UNCLEAR-STATUS           PIC X(02).
               88  WS-UNCLEAR-OK               VALUE '00'.
               88  WS-UNCLEAR-DUPKEY           VALUE '22'.
           05  WS-BGCPOST-STATUS           PIC X(02).
           05  WS-CLROUT-STATUS            PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-FILE              VALUE 'Y'.
           05  WS-TRAILER-IND              PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
           05  WS-CREDIT-IND               PIC X(01).
               88  WS-CAN-CREDIT               VALUE 'Y'.
           05  WS-RERUN-IND                PIC X(01).
               88  WS-SLIP-ALREADY-DONE        VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-TIME-NOW                 PIC 9(08).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-CQD-SEQ                  PIC 9(06) VALUE ZERO.
           05  WS-SLIP-SEQ                 PIC 9(04) VALUE ZERO.
           05  WS-CHQ-CAPTURED             PIC S9(11)V99 COMP-3.
           05  WS-VERIFIED                 PIC S9(11)V99 COMP-3.
           05  WS-DIFFERENCE               PIC S9(11)V99 COMP-3.
           05  WS-SUSPENSE                 PIC S9(11)V99 COMP-3.
           05  WS-ACTION                   PIC X(08).
           05  WS-NOTE                     PIC X(40).
           05  WS-REF-SUFFIX               PIC X(01).
           05  WS-I                        PIC S9(04) COMP.
           05  WS-B                        PIC S9(04) COMP.
      *---- WORKING-DAY ARITHMETIC (9500) -----------------------------*
           05  WS-WD-DATE                  PIC 9(08).
           05  WS-WD-INT                   PIC S9(09) COMP.
           05  WS-WD-DAYS                  PIC S9(04) COMP.
           05  WS-WD-DOW                   PIC S9(04) COMP.
           05  WS-WITHDRAW-2               PIC 9(08).
           05  WS-WITHDRAW-4               PIC 9(08).
           05  WS-CERTAIN-6                PIC 9(08).
      *
      *---- IMAGE CAPTURE RECORD (FB 250) -----------------------------*
       01  WS-IMG-RECORD.
           05  IMG-REC-TYPE                PIC X(01).
               88  IMG-HEADER                  VALUE 'H'.
               88  IMG-SLIP                    VALUE 'S'.
               88  IMG-CHEQUE                  VALUE 'Q'.
               88  IMG-TRAILER                 VALUE 'T'.
           05  IMG-BODY                    PIC X(249).
           05  IMG-HDR REDEFINES IMG-BODY.
               10  IMGH-CAPTURE-DATE       PIC 9(08).
               10  IMGH-CAPTURE-SITE       PIC X(08).
               10  IMGH-FILE-SEQ           PIC 9(06).
               10  FILLER                  PIC X(227).
      *    'S' SLIP LAYOUT - SEE WS-SLIP-R (SLP- FIELDS) BELOW
           05  IMG-CHQ REDEFINES IMG-BODY.
               10  IMGQ-SLIP-ID            PIC X(16).
               10  IMGQ-ITEM-SEQ           PIC 9(03).
               10  IMGQ-DRAWN-SORT         PIC X(06).
               10  IMGQ-DRAWN-ACCT         PIC X(08).
               10  IMGQ-SERIAL             PIC X(06).
               10  IMGQ-AMOUNT             PIC 9(09)V99.
               10  IMGQ-CHQ-DATE           PIC 9(08).
               10  IMGQ-IMAGE-REF          PIC X(20).
               10  IMGQ-ICS-IND            PIC X(01).
                   88  IMGQ-IMAGE-CLEARED      VALUE 'Y'.
               10  FILLER                  PIC X(170).
           05  IMG-TRL REDEFINES IMG-BODY.
               10  IMGT-SLIP-COUNT         PIC 9(07).
               10  IMGT-ITEM-COUNT         PIC 9(07).
               10  IMGT-TOTAL-AMT          PIC 9(13)V99.
               10  FILLER                  PIC X(220).
      *
      *---- CURRENT SLIP AND ITS CHEQUES ------------------------------*
      *    THE 'S' RECORD IS COPIED HERE SO THE CHEQUE READ-AHEAD
      *    CAN USE WS-IMG-RECORD
       01  WS-SLIP                         PIC X(250).
       01  WS-SLIP-R REDEFINES WS-SLIP.
           05  FILLER                      PIC X(01).
           05  SLP-SLIP-ID                 PIC X(16).
           05  SLP-BRANCH-SC               PIC X(06).
           05  SLP-TELLER-ID               PIC X(08).
           05  SLP-BENE-IND                PIC X(01).
               88  SLP-BENE-MEGABANK           VALUE 'M'.
               88  SLP-BENE-OTHER-BANK         VALUE 'O'.
           05  SLP-BENE-SORT-CODE          PIC X(06).
           05  SLP-BENE-ACCT-NO            PIC X(08).
           05  SLP-BENE-NAME               PIC X(35).
           05  SLP-PAYER-REF               PIC X(18).
           05  SLP-PAID-IN-BY              PIC X(35).
           05  SLP-CREDIT-TYPE             PIC X(01).
               88  SLP-BANK-GIRO               VALUE 'G'.
           05  SLP-SLIP-TOTAL              PIC 9(09)V99.
           05  SLP-CASH-AMT                PIC 9(09)V99.
           05  SLP-CHQ-COUNT               PIC 9(03).
           05  SLP-CHQ-TOTAL               PIC 9(09)V99.
           05  FILLER                      PIC X(79).
       01  WS-CHQ-TABLE.
           05  WS-CHQ-COUNT                PIC S9(04) COMP.
           05  WS-CHQ-ENTRY                OCCURS 60 TIMES.
               10  WS-CT-DRAWN-SORT        PIC X(06).
               10  WS-CT-DRAWN-ACCT        PIC X(08).
               10  WS-CT-SERIAL            PIC X(06).
               10  WS-CT-AMOUNT            PIC 9(09)V99.
               10  WS-CT-IMAGE-REF         PIC X(20).
               10  WS-CT-ICS-IND           PIC X(01).
      *
      *---- OUTWARD CLEARING RECORD (FB 150) --------------------------*
       01  WS-CLR-RECORD.
           05  CLO-REC-TYPE                PIC X(01).
           05  CLO-BODY                    PIC X(149).
           05  CLO-ITEM REDEFINES CLO-BODY.
               10  CLO-SORT-CODE           PIC X(06).
               10  CLO-ACCT-NO             PIC X(08).
               10  CLO-AMOUNT              PIC 9(11)V99.
               10  CLO-SERIAL              PIC X(06).
               10  CLO-REFERENCE           PIC X(18).
               10  CLO-NAME                PIC X(35).
               10  CLO-BRANCH-SC           PIC X(06).
               10  CLO-ITEM-REF            PIC X(20).
               10  CLO-ICS-IND             PIC X(01).
               10  CLO-CAPTURE-DATE        PIC 9(08).
               10  FILLER                  PIC X(28).
           05  CLO-TRL REDEFINES CLO-BODY.
               10  CLOT-CREDIT-COUNT       PIC 9(07).
               10  CLOT-CREDIT-TOTAL       PIC 9(13)V99.
               10  CLOT-DEBIT-COUNT        PIC 9(07).
               10  CLOT-DEBIT-TOTAL        PIC 9(13)V99.
               10  FILLER                  PIC X(105).
      *
      *---- PER-BRANCH TOTALS -----------------------------------------*
       01  WS-BRANCH-TABLE.
           05  WS-BR-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-BR-ENTRY                 OCCURS 900 TIMES.
               10  WS-BR-SORT-CODE         PIC X(06).
               10  WS-BR-SLIPS             PIC S9(07) COMP-3.
               10  WS-BR-CASH              PIC S9(13)V99 COMP-3.
               10  WS-BR-CHEQUES           PIC S9(13)V99 COMP-3.
               10  WS-BR-DIFFERENCE        PIC S9(13)V99 COMP-3.
               10  WS-BR-SUSPENSE          PIC S9(13)V99 COMP-3.
               10  WS-BR-OTHER-BANK        PIC S9(13)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-RECORDS-READ             PIC S9(09) COMP-3 VALUE 0.
           05  WS-SLIPS                    PIC S9(09) COMP-3 VALUE 0.
           05  WS-ITEMS                    PIC S9(09) COMP-3 VALUE 0.
           05  WS-MBK-SLIPS                PIC S9(09) COMP-3 VALUE 0.
           05  WS-OTHER-SLIPS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-DIFF-SLIPS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUSP-SLIPS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-RERUN-SLIPS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CQD-WRITTEN              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CLR-CREDITS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CLR-DEBITS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-SLIP-TOTAL-SUM           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-CASH-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-CHQ-TOTAL                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-DIFF-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-SUSP-TOTAL               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-CLR-CREDIT-AMT           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-CLR-DEBIT-AMT            PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
      *---- GL JOURNAL WORK -------------------------------------------*
       01  WS-GL-WORK.
           05  WS-LINE-NO                  PIC 9(07) VALUE ZERO.
           05  WS-GL-LINES                 PIC 9(09) VALUE ZERO.
           05  WS-GL-DR                    PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-CR                    PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-PAIR-DR-GL               PIC X(10).
           05  WS-PAIR-CR-GL               PIC X(10).
           05  WS-PAIR-SWAP-GL             PIC X(10).
           05  WS-PAIR-AMOUNT              PIC S9(13)V99 COMP-3.
           05  WS-PAIR-DESC                PIC X(40).
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'PYBGC010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - BRANCH CREDITS / BANK GIRO'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(18) VALUE 'SLIP'.
           05  FILLER            PIC X(16) VALUE 'CREDIT TO'.
           05  FILLER            PIC X(15) VALUE '    SLIP TOTAL'.
           05  FILLER            PIC X(15) VALUE '          CASH'.
           05  FILLER            PIC X(15) VALUE '       CHEQUES'.
           05  FILLER            PIC X(15) VALUE '    DIFFERENCE'.
           05  FILLER            PIC X(10) VALUE ' ACTION'.
           05  FILLER            PIC X(28) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SLIP-ID                  PIC X(16).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SLIP-TOTAL               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CASH                     PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-CHEQUES                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-DIFFERENCE               PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-ACTION                   PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-NOTE                     PIC X(28).
       01  WS-BRANCH-LINE.
           05  BL-CC                       PIC X(01) VALUE ' '.
           05  BL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  BL-SLIPS                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  BL-CASH                     PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  BL-CHEQUES                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  BL-OTHER-BANK               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  BL-DIFFERENCE               PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  BL-SUSPENSE                 PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(26) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY CHQRECS.
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
           PERFORM 2000-PROCESS-SLIP THRU 2000-EXIT
               UNTIL WS-END-OF-FILE
           PERFORM 3000-CLEARING-TRAILER
           PERFORM 3500-WRITE-JOURNAL
           PERFORM 4000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  IMGCAP-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                       UNCLEAR-FILE
                EXTEND BGCPOST-FILE
                OUTPUT CLROUT-FILE
                       GLJNL-FILE
                       REPORT-FILE
           IF NOT WS-IMGCAP-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-UNCLEAR-OK
           OR WS-BGCPOST-STATUS NOT = '00'
           OR WS-CLROUT-STATUS NOT = '00'
           OR WS-GLJNL-STATUS NOT = '00'
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
      *    2-4-6 DATES ARE THE SAME FOR EVERY PAPER CHEQUE TODAY
           MOVE 2                          TO WS-WD-DAYS
           PERFORM 9500-ADD-WORKING-DAYS
           MOVE WS-WD-DATE                 TO WS-WITHDRAW-2
           MOVE 4                          TO WS-WD-DAYS
           PERFORM 9500-ADD-WORKING-DAYS
           MOVE WS-WD-DATE                 TO WS-WITHDRAW-4
           MOVE 6                          TO WS-WD-DAYS
           PERFORM 9500-ADD-WORKING-DAYS
           MOVE WS-WD-DATE                 TO WS-CERTAIN-6

           PERFORM 8000-READ-IMAGE
           IF WS-END-OF-FILE OR NOT IMG-HEADER
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'IMAGE CAPTURE FILE HAS NO HEADER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF IMGH-CAPTURE-DATE NOT = WS-BUS-DATE
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'CAPTURE DATE IS NOT THE BUSINESS DATE'
                                           TO ABN-MESSAGE
               MOVE IMGH-CAPTURE-DATE      TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           DISPLAY 'PYBGC010 - CAPTURE SITE ' IMGH-CAPTURE-SITE
                   ' FILE ' IMGH-FILE-SEQ
      *---- CLEARING FILE HEADER --------------------------------------*
           MOVE SPACES                     TO WS-CLR-RECORD
           MOVE 'H'                        TO CLO-REC-TYPE
           MOVE 'MEGABANK BRANCH ITEMS'    TO CLO-NAME
           MOVE WS-BUS-DATE                TO CLO-CAPTURE-DATE
           PERFORM 5200-WRITE-CLEARING
           PERFORM 7000-REPORT-HEADINGS
           PERFORM 8000-READ-IMAGE
           .
      *================================================================*
       2000-PROCESS-SLIP.
      *================================================================*
           IF IMG-TRAILER
               PERFORM 2900-RECONCILE-TRAILER
               PERFORM 8000-READ-IMAGE
               GO TO 2000-EXIT
           END-IF
           IF NOT IMG-SLIP
               MOVE '2000-PROCESS-SLIP'    TO ABN-PARAGRAPH
               MOVE 1006                   TO ABN-ABEND-CODE
               MOVE 'CHEQUE OR UNKNOWN RECORD WITHOUT A SLIP'
                                           TO ABN-MESSAGE
               MOVE IMG-BODY (1:40)        TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           ADD 1                           TO WS-SLIPS
           MOVE WS-IMG-RECORD              TO WS-SLIP
           MOVE SPACES                     TO WS-ACTION WS-NOTE
           MOVE 'N'                        TO WS-RERUN-IND
           MOVE ZERO                       TO WS-CHQ-COUNT
                                              WS-CHQ-CAPTURED
                                              WS-SUSPENSE
           PERFORM 2100-COLLECT-CHEQUES

      *---- RECONCILE THE SLIP ----------------------------------------*
           COMPUTE WS-VERIFIED   = SLP-CASH-AMT + WS-CHQ-CAPTURED
           COMPUTE WS-DIFFERENCE = SLP-SLIP-TOTAL - WS-VERIFIED
           IF WS-DIFFERENCE NOT = ZERO
               ADD 1                       TO WS-DIFF-SLIPS
               ADD WS-DIFFERENCE           TO WS-DIFF-TOTAL
               MOVE 'SLIP DIFFERS FROM CASH + CHEQUES'
                                           TO WS-NOTE
           END-IF
           IF WS-CHQ-COUNT NOT = SLP-CHQ-COUNT
               MOVE 'CHEQUE COUNT DIFFERS FROM SLIP' TO WS-NOTE
           END-IF

           EVALUATE SLP-BENE-IND
               WHEN 'M'
                   ADD 1                   TO WS-MBK-SLIPS
                   PERFORM 2200-MEGABANK-CREDIT THRU 2200-EXIT
               WHEN 'O'
                   ADD 1                   TO WS-OTHER-SLIPS
                   PERFORM 2300-OTHER-BANK-CREDIT
               WHEN OTHER
                   MOVE WS-VERIFIED        TO WS-SUSPENSE
                   MOVE 'SUSPENSE'         TO WS-ACTION
                   MOVE 'BENEFICIARY BANK NOT KNOWN' TO WS-NOTE
           END-EVALUATE

      *    CHEQUES GO TO THE DRAWEE BANKS UNLESS ALREADY SENT
           IF NOT WS-SLIP-ALREADY-DONE
               PERFORM 2400-PRESENT-CHEQUES
               PERFORM 2500-BRANCH-TOTALS THRU 2500-EXIT
               ADD SLP-SLIP-TOTAL          TO WS-SLIP-TOTAL-SUM
               ADD SLP-CASH-AMT            TO WS-CASH-TOTAL
               ADD WS-CHQ-CAPTURED         TO WS-CHQ-TOTAL
               IF WS-SUSPENSE NOT = ZERO
                   ADD 1                   TO WS-SUSP-SLIPS
                   ADD WS-SUSPENSE         TO WS-SUSP-TOTAL
               END-IF
           END-IF
           IF WS-NOTE NOT = SPACES OR WS-ACTION = 'SUSPENSE'
               PERFORM 7100-REPORT-DETAIL
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-COLLECT-CHEQUES.
      *================================================================*
      *    THE CHEQUES FOLLOW THEIR SLIP. READING ON LEAVES THE NEXT
      *    SLIP (OR THE TRAILER) IN WS-IMG-RECORD.
           PERFORM 8000-READ-IMAGE
           PERFORM UNTIL WS-END-OF-FILE OR NOT IMG-CHEQUE
               IF IMGQ-SLIP-ID NOT = SLP-SLIP-ID
                   MOVE '2100-COLLECT-CHEQUES' TO ABN-PARAGRAPH
                   MOVE 1006               TO ABN-ABEND-CODE
                   MOVE 'CHEQUE DOES NOT BELONG TO THE CURRENT SLIP'
                                           TO ABN-MESSAGE
                   MOVE IMGQ-SLIP-ID       TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
               ADD 1                       TO WS-ITEMS
               IF WS-CHQ-COUNT < WS-MAX-CHEQUES
                   ADD 1                   TO WS-CHQ-COUNT
                   MOVE WS-CHQ-COUNT       TO WS-I
                   MOVE IMGQ-DRAWN-SORT    TO WS-CT-DRAWN-SORT (WS-I)
                   MOVE IMGQ-DRAWN-ACCT    TO WS-CT-DRAWN-ACCT (WS-I)
                   MOVE IMGQ-SERIAL        TO WS-CT-SERIAL (WS-I)
                   MOVE IMGQ-AMOUNT        TO WS-CT-AMOUNT (WS-I)
                   MOVE IMGQ-IMAGE-REF     TO WS-CT-IMAGE-REF (WS-I)
                   MOVE IMGQ-ICS-IND       TO WS-CT-ICS-IND (WS-I)
                   ADD IMGQ-AMOUNT         TO WS-CHQ-CAPTURED
               ELSE
                   MOVE 'OVER 60 CHEQUES - EXCESS NOT CREDITED'
                                           TO WS-NOTE
               END-IF
               PERFORM 8000-READ-IMAGE
           END-PERFORM
           .
      *================================================================*
       2200-MEGABANK-CREDIT.
      *================================================================*
           MOVE SLP-BENE-SORT-CODE         TO ACM-SORT-CODE
           MOVE SLP-BENE-ACCT-NO           TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           MOVE 'Y'                        TO WS-CREDIT-IND
           EVALUATE TRUE
               WHEN WS-ACCTMAST-NOTFND
                   MOVE 'N'                TO WS-CREDIT-IND
                   MOVE 'ACCOUNT NOT FOUND - TO SUSPENSE' TO WS-NOTE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE '2200-MEGABANK-CREDIT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               WHEN ACM-CLOSED
                   MOVE 'N'                TO WS-CREDIT-IND
                   MOVE 'ACCOUNT CLOSED - TO SUSPENSE' TO WS-NOTE
               WHEN ACM-NO-CREDITS OR ACM-NO-POSTINGS
                   MOVE 'N'                TO WS-CREDIT-IND
                   MOVE 'CREDITS BLOCKED - TO SUSPENSE' TO WS-NOTE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT WS-CAN-CREDIT
               MOVE WS-VERIFIED            TO WS-SUSPENSE
               MOVE 'SUSPENSE'             TO WS-ACTION
               GO TO 2200-EXIT
           END-IF

      *---- UNCLEARED RECORDS FIRST - A DUPLICATE MEANS A RERUN -------*
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-CHQ-COUNT OR WS-SLIP-ALREADY-DONE
               PERFORM 2210-WRITE-UNCLEARED
           END-PERFORM
           IF WS-SLIP-ALREADY-DONE
               ADD 1                       TO WS-RERUN-SLIPS
               MOVE 'RERUN'                TO WS-ACTION
               MOVE 'SLIP ALREADY PROCESSED - SKIPPED' TO WS-NOTE
               GO TO 2200-EXIT
           END-IF

      *---- CASH - AVAILABLE NOW --------------------------------------*
           IF SLP-CASH-AMT > ZERO
               MOVE SPACES                 TO PST-POSTING-RECORD
               MOVE SLP-CASH-AMT           TO PST-AMOUNT
               MOVE 'C'                    TO WS-REF-SUFFIX
               IF SLP-BANK-GIRO
                   MOVE 'BANK GIRO CREDIT - CASH' TO PST-NARRATIVE-1
               ELSE
                   MOVE 'CASH PAID IN AT BRANCH' TO PST-NARRATIVE-1
               END-IF
               PERFORM 5000-WRITE-POSTING
           END-IF
      *---- CHEQUES - UNCLEARED ---------------------------------------*
           IF WS-CHQ-CAPTURED > ZERO
               MOVE SPACES                 TO PST-POSTING-RECORD
               MOVE WS-CHQ-CAPTURED        TO PST-AMOUNT
               MOVE 'Q'                    TO WS-REF-SUFFIX
               MOVE 'CHEQUES PAID IN - UNCLEARED' TO PST-NARRATIVE-1
               PERFORM 5000-WRITE-POSTING
      *        PSEOD010 WILL ADD THE WHOLE CREDIT TO AVAILABLE
               ADD WS-CHQ-CAPTURED         TO ACM-UNCLEARED-AMT
               SUBTRACT WS-CHQ-CAPTURED    FROM ACM-AVAIL-BAL
               MOVE WS-BUS-DATE            TO ACM-LAST-MAINT-DATE
               MOVE ZERO                   TO ACM-LAST-MAINT-TIME
               MOVE WS-PROGRAM-ID          TO ACM-LAST-MAINT-USER
               REWRITE ACM-ACCOUNT-RECORD
               IF NOT WS-ACCTMAST-OK
                   MOVE '2200-MEGABANK-CREDIT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE ACM-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               END-IF
           END-IF
           MOVE 'CREDITED'                 TO WS-ACTION
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2210-WRITE-UNCLEARED.
      *================================================================*
           MOVE SPACES                     TO CQD-DEPOSIT-RECORD
           MOVE SLP-BENE-SORT-CODE         TO CQD-SORT-CODE
           MOVE SLP-BENE-ACCT-NO           TO CQD-ACCT-NO
           MOVE WS-BUS-DATE                TO CQD-DEPOSIT-DATE
           PERFORM 2205-CQD-SEQUENCE
           MOVE WS-CQD-SEQ                 TO CQD-SEQ
           MOVE WS-CT-AMOUNT (WS-I)        TO CQD-AMOUNT
           MOVE WS-CT-DRAWN-SORT (WS-I)    TO CQD-DRAWN-ON-SORT
           IF WS-CT-ICS-IND (WS-I) = 'Y'
           AND WS-BUS-DATE >= WS-ICS-GO-LIVE
               MOVE DTC-NEXT-BUS-DATE      TO CQD-INTEREST-DATE
                                              CQD-WITHDRAW-DATE
                                              CQD-CERTAINTY-DATE
           ELSE
               MOVE WS-WITHDRAW-2          TO CQD-INTEREST-DATE
               MOVE WS-WITHDRAW-4          TO CQD-WITHDRAW-DATE
               MOVE WS-CERTAIN-6           TO CQD-CERTAINTY-DATE
           END-IF
           SET CQD-UNCLEARED               TO TRUE
           MOVE SLP-BRANCH-SC              TO CQD-BRANCH-SC
           MOVE SLP-TELLER-ID              TO CQD-TELLER-ID
           WRITE UNCLEAR-RECORD            FROM CQD-DEPOSIT-RECORD
           EVALUATE TRUE
               WHEN WS-UNCLEAR-OK
                   ADD 1                   TO WS-CQD-WRITTEN
               WHEN WS-UNCLEAR-DUPKEY
                   SET WS-SLIP-ALREADY-DONE TO TRUE
               WHEN OTHER
                   MOVE '2210-WRITE-UNCLEARED' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'UNCLEAR WRITE FAILED' TO ABN-MESSAGE
                   MOVE WS-UNCLEAR-STATUS  TO ABN-FILE-STATUS
                   MOVE CQD-KEY            TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2205-CQD-SEQUENCE.
      *================================================================*
      *    BASE + (SLIP SEQUENCE - LAST 4 OF THE SLIP ID - MOD 5000)
      *    * 100 + ITEM: A RERUN OF THE SAME FILE GIVES THE SAME KEYS.
           COMPUTE WS-SLIP-SEQ = FUNCTION MOD
                   (FUNCTION NUMVAL (SLP-SLIP-ID (13:4)), 5000)
           COMPUTE WS-CQD-SEQ = WS-CQD-SEQ-BASE
                              + WS-SLIP-SEQ * 100 + WS-I
           .
      *================================================================*
       2300-OTHER-BANK-CREDIT.
      *================================================================*
           IF WS-VERIFIED NOT > ZERO
               MOVE 'NOTHING RECEIVED - NO CREDIT SENT' TO WS-NOTE
               MOVE 'NONE'                 TO WS-ACTION
           ELSE
               MOVE SPACES                 TO WS-CLR-RECORD
               MOVE 'C'                    TO CLO-REC-TYPE
               MOVE SLP-BENE-SORT-CODE     TO CLO-SORT-CODE
               MOVE SLP-BENE-ACCT-NO       TO CLO-ACCT-NO
               MOVE WS-VERIFIED            TO CLO-AMOUNT
               MOVE SLP-PAYER-REF          TO CLO-REFERENCE
               MOVE SLP-BENE-NAME          TO CLO-NAME
               MOVE SLP-BRANCH-SC          TO CLO-BRANCH-SC
               MOVE SLP-SLIP-ID            TO CLO-ITEM-REF
               MOVE WS-BUS-DATE            TO CLO-CAPTURE-DATE
               PERFORM 5200-WRITE-CLEARING
               ADD 1                       TO WS-CLR-CREDITS
               ADD WS-VERIFIED             TO WS-CLR-CREDIT-AMT
               MOVE 'CLEARING'             TO WS-ACTION
           END-IF
           .
      *================================================================*
       2400-PRESENT-CHEQUES.
      *================================================================*
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CHQ-COUNT
               MOVE SPACES                 TO WS-CLR-RECORD
               MOVE 'D'                    TO CLO-REC-TYPE
               MOVE WS-CT-DRAWN-SORT (WS-I) TO CLO-SORT-CODE
               MOVE WS-CT-DRAWN-ACCT (WS-I) TO CLO-ACCT-NO
               MOVE WS-CT-AMOUNT (WS-I)    TO CLO-AMOUNT
               MOVE WS-CT-SERIAL (WS-I)    TO CLO-SERIAL
      *        THE DEPOSIT REFERENCE COMES BACK ON A RETURN (CQUNC010
      *        MATCHES CQR-ITEM-REF = DEPOSIT DATE + SEQUENCE)
               PERFORM 2205-CQD-SEQUENCE
               STRING WS-BUS-DATE WS-CQD-SEQ
                      DELIMITED BY SIZE
                      INTO CLO-REFERENCE
               END-STRING
               MOVE SLP-BENE-NAME          TO CLO-NAME
               MOVE SLP-BRANCH-SC          TO CLO-BRANCH-SC
               MOVE WS-CT-IMAGE-REF (WS-I) TO CLO-ITEM-REF
               MOVE WS-CT-ICS-IND (WS-I)   TO CLO-ICS-IND
               MOVE WS-BUS-DATE            TO CLO-CAPTURE-DATE
               PERFORM 5200-WRITE-CLEARING
               ADD 1                       TO WS-CLR-DEBITS
               ADD WS-CT-AMOUNT (WS-I)     TO WS-CLR-DEBIT-AMT
           END-PERFORM
           .
      *================================================================*
       2500-BRANCH-TOTALS.
      *================================================================*
           PERFORM VARYING WS-B FROM 1 BY 1
                   UNTIL WS-B > WS-BR-COUNT
                      OR WS-BR-SORT-CODE (WS-B) = SLP-BRANCH-SC
               CONTINUE
           END-PERFORM
           IF WS-B > WS-BR-COUNT
               IF WS-BR-COUNT >= WS-MAX-BRANCHES
                   DISPLAY 'PYBGC010 - BRANCH TABLE FULL ' SLP-BRANCH-SC
                   GO TO 2500-EXIT
               END-IF
               ADD 1                       TO WS-BR-COUNT
               MOVE WS-BR-COUNT            TO WS-B
               MOVE SLP-BRANCH-SC          TO WS-BR-SORT-CODE (WS-B)
               MOVE ZERO                   TO WS-BR-SLIPS (WS-B)
                                              WS-BR-CASH (WS-B)
                                              WS-BR-CHEQUES (WS-B)
                                              WS-BR-DIFFERENCE (WS-B)
                                              WS-BR-SUSPENSE (WS-B)
                                              WS-BR-OTHER-BANK (WS-B)
           END-IF
           ADD 1                           TO WS-BR-SLIPS (WS-B)
           ADD SLP-CASH-AMT                TO WS-BR-CASH (WS-B)
           ADD WS-CHQ-CAPTURED             TO WS-BR-CHEQUES (WS-B)
           ADD WS-DIFFERENCE               TO WS-BR-DIFFERENCE (WS-B)
           ADD WS-SUSPENSE                 TO WS-BR-SUSPENSE (WS-B)
           IF WS-ACTION = 'CLEARING'
               ADD WS-VERIFIED             TO WS-BR-OTHER-BANK (WS-B)
           END-IF
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       2900-RECONCILE-TRAILER.
      *================================================================*
           SET WS-TRAILER-SEEN             TO TRUE
           IF IMGT-SLIP-COUNT NOT = WS-SLIPS
           OR IMGT-ITEM-COUNT NOT = WS-ITEMS
               MOVE '2900-RECONCILE-TRAILER' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'IMAGE TRAILER SLIP / ITEM COUNT MISMATCH'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
      *    SLIP TOTALS OF SKIPPED (RERUN) SLIPS ARE NOT IN THE SUM
           IF IMGT-TOTAL-AMT NOT = WS-SLIP-TOTAL-SUM
           AND WS-RERUN-SLIPS = ZERO
               MOVE '2900-RECONCILE-TRAILER' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'IMAGE TRAILER SLIP TOTAL MISMATCH'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-CLEARING-TRAILER.
      *================================================================*
           IF NOT WS-TRAILER-SEEN
               MOVE '3000-CLEARING-TRAILER' TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'IMAGE CAPTURE FILE HAS NO TRAILER'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           MOVE SPACES                     TO WS-CLR-RECORD
           MOVE 'T'                        TO CLO-REC-TYPE
           MOVE WS-CLR-CREDITS             TO CLOT-CREDIT-COUNT
           MOVE WS-CLR-CREDIT-AMT          TO CLOT-CREDIT-TOTAL
           MOVE WS-CLR-DEBITS              TO CLOT-DEBIT-COUNT
           MOVE WS-CLR-DEBIT-AMT           TO CLOT-DEBIT-TOTAL
           PERFORM 5200-WRITE-CLEARING
           .
      *================================================================*
       3500-WRITE-JOURNAL.
      *================================================================*
           ACCEPT WS-TIME-NOW              FROM TIME
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-BRANCH-CREDITS      TO TRUE
           STRING 'BGCSUS' WS-BUS-DATE
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE 'BGCSU'                    TO GLJH-RUN-ID
           MOVE WS-BUS-DATE                TO GLJH-CREATE-DATE
           MOVE WS-TIME-NOW (1:6)          TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3900-WRITE-GL
      *    ONE SET OF LINES PER BRANCH, COST CENTRE = BRANCH SORT CODE
           PERFORM VARYING WS-B FROM 1 BY 1 UNTIL WS-B > WS-BR-COUNT
               MOVE WS-GL-TILL-DIFF        TO WS-PAIR-DR-GL
               MOVE WS-GL-BRANCH-SUSP      TO WS-PAIR-CR-GL
               MOVE WS-BR-DIFFERENCE (WS-B) TO WS-PAIR-AMOUNT
               MOVE 'PAYING-IN SLIP DIFFERENCES' TO WS-PAIR-DESC
               PERFORM 3200-JOURNAL-PAIR THRU 3200-EXIT
               MOVE WS-GL-TILL-DIFF        TO WS-PAIR-DR-GL
               MOVE WS-GL-BRANCH-SUSP      TO WS-PAIR-CR-GL
               MOVE WS-BR-SUSPENSE (WS-B)  TO WS-PAIR-AMOUNT
               MOVE 'CREDITS NOT APPLIED - SUSPENSE' TO WS-PAIR-DESC
               PERFORM 3200-JOURNAL-PAIR THRU 3200-EXIT
               MOVE WS-GL-IN-TRANSIT       TO WS-PAIR-DR-GL
               MOVE WS-GL-CREDIT-CLRG      TO WS-PAIR-CR-GL
               MOVE WS-BR-OTHER-BANK (WS-B) TO WS-PAIR-AMOUNT
               MOVE 'BGC TO OTHER BANKS - CREDIT CLEARING'
                                           TO WS-PAIR-DESC
               PERFORM 3200-JOURNAL-PAIR THRU 3200-EXIT
           END-PERFORM
           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-GL-LINES                TO GLJT-LINE-COUNT
           MOVE WS-GL-DR                   TO GLJT-TOTAL-DR
           MOVE WS-GL-CR                   TO GLJT-TOTAL-CR
           MOVE 1                          TO GLJT-JOURNAL-COUNT
           PERFORM 3900-WRITE-GL
           .
      *================================================================*
       3200-JOURNAL-PAIR.
      *================================================================*
      *    A NEGATIVE AMOUNT (E.G. SLIP UNDER-STATED) SWAPS THE SIDES.
           IF WS-PAIR-AMOUNT = ZERO
               GO TO 3200-EXIT
           END-IF
           IF WS-PAIR-AMOUNT < ZERO
               MOVE WS-PAIR-DR-GL          TO WS-PAIR-SWAP-GL
               MOVE WS-PAIR-CR-GL          TO WS-PAIR-DR-GL
               MOVE WS-PAIR-SWAP-GL        TO WS-PAIR-CR-GL
               COMPUTE WS-PAIR-AMOUNT = ZERO - WS-PAIR-AMOUNT
           END-IF
           MOVE WS-PAIR-DR-GL              TO GLJD-GL-ACCOUNT
           MOVE 'D'                        TO GLJD-DR-CR-IND
           PERFORM 3100-DETAIL
           MOVE WS-PAIR-CR-GL              TO GLJD-GL-ACCOUNT
           MOVE 'C'                        TO GLJD-DR-CR-IND
           PERFORM 3100-DETAIL
           .
       3200-EXIT.
           EXIT.
      *================================================================*
       3100-DETAIL.
      *================================================================*
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-LINE-NO
           MOVE WS-LINE-NO                 TO GLJD-LINE-NO
           MOVE WS-BR-SORT-CODE (WS-B)     TO GLJD-COST-CENTRE
           MOVE SPACES                     TO GLJD-PROD-CD
                                              GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE 'GBP'                      TO GLJD-CCY-CD
           MOVE WS-PAIR-AMOUNT             TO GLJD-AMOUNT
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE WS-PAIR-DESC               TO GLJD-DESCRIPTION
           MOVE 'BGC '                     TO GLJD-TXN-CODE
           MOVE GLJ-JOURNAL-ID             TO GLJD-SOURCE-REF
           PERFORM 3900-WRITE-GL
           ADD 1                           TO WS-GL-LINES
           IF GLJD-DEBIT
               ADD GLJD-AMOUNT             TO WS-GL-DR
           ELSE
               ADD GLJD-AMOUNT             TO WS-GL-CR
           END-IF
           .
      *================================================================*
       3900-WRITE-GL.
      *================================================================*
           WRITE GLJNL-RECORD              FROM GLJ-JOURNAL-LINE
           IF WS-GLJNL-STATUS NOT = '00'
               MOVE '3900-WRITE-GL'        TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'GLJNL WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-GLJNL-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       5000-WRITE-POSTING.
      *================================================================*
           MOVE SLP-BENE-SORT-CODE         TO PST-SORT-CODE
           MOVE SLP-BENE-ACCT-NO           TO PST-ACCT-NO
           SET PST-CREDIT                  TO TRUE
           MOVE 'BGC '                     TO PST-TXN-CODE
           MOVE WS-BUS-DATE                TO PST-POST-DATE
                                              PST-VALUE-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'GBP'                      TO PST-CCY-CD
           STRING SLP-PAYER-REF ' ' SLP-PAID-IN-BY
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE 'BRANCH'                   TO PST-CHANNEL-CD
      *    EXT REF = BGC + SLIP ID + C (CASH) / Q (CHEQUES)
           STRING 'BGC' SLP-SLIP-ID WS-REF-SUFFIX
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE SLP-TELLER-ID              TO PST-USER-ID
           WRITE BGCPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-BGCPOST-STATUS NOT = '00'
               MOVE '5000-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'BGCPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-BGCPOST-STATUS      TO ABN-FILE-STATUS
               MOVE PST-EXT-REF            TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       5200-WRITE-CLEARING.
      *================================================================*
           WRITE CLROUT-RECORD             FROM WS-CLR-RECORD
           IF WS-CLROUT-STATUS NOT = '00'
               MOVE '5200-WRITE-CLEARING'  TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CLROUT WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-CLROUT-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       4000-FINALISE.
      *================================================================*
      *---- BRANCH SUMMARY --------------------------------------------*
           PERFORM 7000-REPORT-HEADINGS
           MOVE '0'                        TO BL-CC
           MOVE 'BRANCH'                   TO BL-SORT-CODE
           MOVE ZERO                       TO BL-SLIPS BL-CASH
                                              BL-CHEQUES BL-OTHER-BANK
                                              BL-DIFFERENCE BL-SUSPENSE
           WRITE REPORT-RECORD             FROM WS-BRANCH-LINE
           MOVE ' '                        TO BL-CC
           PERFORM VARYING WS-B FROM 1 BY 1 UNTIL WS-B > WS-BR-COUNT
               IF WS-LINE-COUNT >= 55
                   PERFORM 7000-REPORT-HEADINGS
               END-IF
               MOVE WS-BR-SORT-CODE (WS-B) TO BL-SORT-CODE
               MOVE WS-BR-SLIPS (WS-B)     TO BL-SLIPS
               MOVE WS-BR-CASH (WS-B)      TO BL-CASH
               MOVE WS-BR-CHEQUES (WS-B)   TO BL-CHEQUES
               MOVE WS-BR-OTHER-BANK (WS-B) TO BL-OTHER-BANK
               MOVE WS-BR-DIFFERENCE (WS-B) TO BL-DIFFERENCE
               MOVE WS-BR-SUSPENSE (WS-B)  TO BL-SUSPENSE
               WRITE REPORT-RECORD         FROM WS-BRANCH-LINE
               ADD 1                       TO WS-LINE-COUNT
           END-PERFORM
      *---- CONTROL TOTALS --------------------------------------------*
           MOVE 'SLIPS READ - SLIP TOTALS' TO TL-LABEL
           MOVE WS-SLIPS                   TO TL-COUNT
           MOVE WS-SLIP-TOTAL-SUM          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  CASH COUNTED'           TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-CASH-TOTAL              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  CHEQUES CAPTURED'       TO TL-LABEL
           MOVE WS-ITEMS                   TO TL-COUNT
           MOVE WS-CHQ-TOTAL               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  SLIPS WITH DIFFERENCES' TO TL-LABEL
           MOVE WS-DIFF-SLIPS              TO TL-COUNT
           MOVE WS-DIFF-TOTAL              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MEGABANK SLIPS'           TO TL-LABEL
           MOVE WS-MBK-SLIPS               TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  UNCLEARED CHEQUE RECORDS WRITTEN' TO TL-LABEL
           MOVE WS-CQD-WRITTEN             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'OTHER BANK CREDITS TO CLEARING' TO TL-LABEL
           MOVE WS-CLR-CREDITS             TO TL-COUNT
           MOVE WS-CLR-CREDIT-AMT          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CHEQUES PRESENTED TO CLEARING' TO TL-LABEL
           MOVE WS-CLR-DEBITS              TO TL-COUNT
           MOVE WS-CLR-DEBIT-AMT           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SLIPS TO BRANCH SUSPENSE'  TO TL-LABEL
           MOVE WS-SUSP-SLIPS              TO TL-COUNT
           MOVE WS-SUSP-TOTAL              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SLIPS SKIPPED - ALREADY PROCESSED' TO TL-LABEL
           MOVE WS-RERUN-SLIPS             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

           CLOSE IMGCAP-FILE ACCTMAST-FILE UNCLEAR-FILE BGCPOST-FILE
                 CLROUT-FILE GLJNL-FILE REPORT-FILE
           DISPLAY 'PYBGC010 - SLIPS ' WS-SLIPS ' DIFFERENCES '
                   WS-DIFF-SLIPS ' SUSPENSE ' WS-SUSP-SLIPS
           IF WS-GL-DR NOT = WS-GL-CR
               MOVE '4000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL JOURNAL OUT OF BALANCE' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF WS-DIFF-SLIPS > ZERO
           OR WS-SUSP-SLIPS > ZERO
           OR WS-RERUN-SLIPS > ZERO
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
           IF WS-LINE-COUNT >= 55
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SLP-SLIP-ID                TO DL-SLIP-ID
           MOVE SLP-BENE-SORT-CODE         TO DL-SORT-CODE
           MOVE SLP-BENE-ACCT-NO           TO DL-ACCT-NO
           MOVE SLP-SLIP-TOTAL             TO DL-SLIP-TOTAL
           MOVE SLP-CASH-AMT               TO DL-CASH
           MOVE WS-CHQ-CAPTURED            TO DL-CHEQUES
           MOVE WS-DIFFERENCE              TO DL-DIFFERENCE
           MOVE WS-ACTION                  TO DL-ACTION
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-IMAGE.
      *================================================================*
           READ IMGCAP-FILE
           EVALUATE TRUE
               WHEN WS-IMGCAP-OK
                   MOVE IMGCAP-RECORD      TO WS-IMG-RECORD
                   ADD 1                   TO WS-RECORDS-READ
               WHEN WS-IMGCAP-EOF
                   SET WS-END-OF-FILE      TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-IMAGE'  TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'IMGCAP READ FAILED' TO ABN-MESSAGE
                   MOVE WS-IMGCAP-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9500-ADD-WORKING-DAYS.
      *================================================================*
      *    WS-WD-DATE = BUSINESS DATE + WS-WD-DAYS WORKING DAYS.
      *    WEEKENDS ONLY - BANK HOLIDAYS ARE NOT SKIPPED (PIR 2016/087).
      *    INTEGER-OF-DATE 1 = MONDAY 1 JANUARY 1601.
           COMPUTE WS-WD-INT = FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
           PERFORM UNTIL WS-WD-DAYS = ZERO
               ADD 1                       TO WS-WD-INT
               COMPUTE WS-WD-DOW = FUNCTION MOD (WS-WD-INT - 1, 7)
      *        0 = MONDAY ... 5 = SATURDAY, 6 = SUNDAY
               IF WS-WD-DOW < 5
                   SUBTRACT 1              FROM WS-WD-DAYS
               END-IF
           END-PERFORM
           COMPUTE WS-WD-DATE = FUNCTION DATE-OF-INTEGER (WS-WD-INT)
           .
      *
           COPY ABNDPROC.
