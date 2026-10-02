       IDENTIFICATION DIVISION.
       PROGRAM-ID.    INCAP010.
       AUTHOR.        R J HALLIDAY.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1992-03-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : INCAP010                                            *
      * TITLE    : INTEREST CAPITALISATION (CREDIT AND DEBIT)          *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : INCAP010                                            *
      * JCL      : INACR01 STEP IA030                                  *
      *----------------------------------------------------------------*
      * INPUT    : PRDPARM  - PRODUCT PARAMETERS (VSAM KSDS, SEQ LOAD) *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ I-O)  *
      *            MBK.INTPAID (INSERT) - INTEREST PAID / CHARGED      *
      * OUTPUT   : INTPOST  - INTEREST POSTINGS FOR THE NEXT HARD      *
      *                       POSTING RUN - PSTXNREC (FB 200)          *
      *            GLJNL    - GL CAPITALISATION JOURNAL - GLJNLREC     *
      *            REGISTER - CAPITALISATION REGISTER (FBA 133)        *
      *----------------------------------------------------------------*
      * RUNS IN THE EOD NETWORK AFTER INACR010. FOR EVERY ACCOUNT      *
      * WHOSE CREDIT OR DEBIT CAPITALISATION DATE FALLS IN THE PERIOD  *
      * TODAY TO (NOT INCLUDING) THE NEXT BUSINESS DATE:               *
      *   - THE ACCRUED INTEREST IS MOVED OUT OF ACM-ACCRUED-INT-xx    *
      *     AND WRITTEN AS HARD POSTINGS ('H') TO INTPOST. THE         *
      *     POSTINGS ARE APPLIED TO THE LEDGER BY TOMORROW NIGHT'S     *
      *     PSEOD010 RUN, VALUE DATED TODAY, SO THE CUSTOMER SEES THE  *
      *     INTEREST ON THE NEXT BUSINESS DAY'S STATEMENT LINE.        *
      *   - CREDIT INTEREST ON A NET-PAYING ACCOUNT HAS INCOME TAX     *
      *     DEDUCTED AT THE BASIC RATE (WS-BASIC-RATE) - TWO POSTINGS: *
      *     GROSS INTEREST CREDIT AND TAX DEBIT (UK ACCOUNTS THAT ARE  *
      *     NOT REGISTERED FOR GROSS PAYMENT ON FORM R85).             *
      *   - DEBIT INTEREST BELOW THE PRODUCT MINIMUM CHARGE IS WAIVED  *
      *     (DE MINIMIS) - THE ACCRUAL IS WRITTEN OFF TO THE GL.       *
      *   - ONE MBK.INTPAID ROW PER CAPITALISATION FOR THE ANNUAL TAX  *
      *     CERTIFICATE (INTAX010) AND HMRC RETURN OF INTEREST.        *
      *                                                                *
      * THE CAPITALISATION DATE IS THE PRODUCT CAPITALISATION DAY IN   *
      * A DUE MONTH. A DAY BEYOND THE END OF A SHORT MONTH MEANS THE   *
      * LAST DAY OF THAT MONTH. FIXED TERM PRODUCTS (FREQUENCY 'T')    *
      * CAPITALISE ON THE ACCOUNT MATURITY DATE.                       *
      *                                                                *
      * ACCOUNTS PENDING CLOSURE ARE NOT TOUCHED - DDCLS010 PAYS THE   *
      * FINAL INTEREST WHEN IT CLOSES THE ACCOUNT.                     *
      *----------------------------------------------------------------*
      * RESTART: ACM-LAST-CR-CAP-DATE / ACM-LAST-DR-CAP-DATE ARE SET   *
      * WHEN AN ACCOUNT IS CAPITALISED, SO A RERUN SKIPS IT. HOWEVER   *
      * THE INTPOST FILE AND INTPAID ROWS FROM THE FAILED RUN MUST BE  *
      * KEPT (INTPOST IS DISP=MOD ON RESTART - SEE JCL INACR01) AND    *
      * THE DB2 COMMITS ARE TAKEN ONLY AFTER EACH VSAM REWRITE.        *
      * IF IN DOUBT RESTORE ACCTMAST FROM INACR01 STEP IA005 AND ASK   *
      * THE DBA TO DELETE INTPAID ROWS FOR THE RUN ID.                 *
      *----------------------------------------------------------------*
      * RETURN CODES  0 OK   4 EXCEPTIONS (SEE REGISTER)               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1992-03-02 RJH  ORIGINAL VERSION                               *
      * 1996-04-22 PDK  QUARTERLY / HALF-YEARLY FREQUENCIES            *
      * 1998-11-30 MLW  Y2K                                            *
      * 2000-04-06 MLW  TAX RATE 20% (FROM 23%) - WS-BASIC-RATE        *
      * 2002-06-03 KLM  GL CAPITALISATION JOURNAL                      *
      * 2004-08-09 SAT  UNARRANGED SHARE SHOWN IN DEBIT NARRATIVE      *
      * 2006-03-13 AOS  FIXED TERM DEPOSITS CAPITALISE AT MATURITY     *
      * 2007-04-02 AOS  INTPAID DB2 TABLE REPLACES VSAM TAXFILE        *
      * 2010-03-08 AOS  OUTPUT POSTINGS NOW MERGED BY PSEOD01 PS010    *
      *                 (WAS A SEPARATE POSTING PROGRAM DD0310)        *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT INTPOST-FILE     ASSIGN TO INTPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-INTPOST-STATUS.
           SELECT GLJNL-FILE       ASSIGN TO GLJNL
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-GLJNL-STATUS.
           SELECT REGISTER-FILE    ASSIGN TO REGISTER
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REGISTER-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
      *
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
      *
       FD  INTPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  INTPOST-RECORD                  PIC X(200).
      *
       FD  GLJNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJNL-RECORD                    PIC X(250).
      *
       FD  REGISTER-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REGISTER-RECORD                 PIC X(133).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'INCAP010'.
      *
       01  WS-CONSTANTS.
      *    BASIC RATE OF INCOME TAX ON SAVINGS INTEREST (FROM 6/4/2000)
           05  WS-BASIC-RATE               PIC S9(03)V99 COMP-3
                                           VALUE +20.00.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 23580000.
      *
       01  WS-FILE-STATUSES.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
               88  WS-PRDPARM-EOF              VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-INTPOST-STATUS           PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REGISTER-STATUS          PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-PROD-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-PRODUCTS          VALUE 'Y'.
           05  WS-PROD-FOUND-IND           PIC X(01) VALUE 'N'.
               88  WS-PROD-FOUND               VALUE 'Y'.
           05  WS-ACCT-CHANGED-IND         PIC X(01) VALUE 'N'.
               88  WS-ACCT-CHANGED             VALUE 'Y'.
           05  WS-CR-DUE-IND               PIC X(01) VALUE 'N'.
               88  WS-CR-CAP-DUE               VALUE 'Y'.
           05  WS-DR-DUE-IND               PIC X(01) VALUE 'N'.
               88  WS-DR-CAP-DUE               VALUE 'Y'.
           05  WS-WAIVED-IND               PIC X(01) VALUE 'N'.
               88  WS-DR-WAIVED                VALUE 'Y'.
      *
      *---- DATES -----------------------------------------------------*
       01  WS-DATE-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-R REDEFINES WS-BUS-DATE.
               10  WS-BD-CCYY              PIC 9(04).
               10  WS-BD-MM                PIC 9(02).
               10  WS-BD-DD                PIC 9(02).
           05  WS-NEXT-BUS-DATE            PIC 9(08).
           05  WS-NOMINAL-DATE             PIC 9(08).
           05  WS-NOMINAL-DATE-R REDEFINES WS-NOMINAL-DATE.
               10  WS-ND-CCYY              PIC 9(04).
               10  WS-ND-MM                PIC 9(02).
               10  WS-ND-DD                PIC 9(02).
           05  WS-CAP-DAY                  PIC 9(02).
           05  WS-CAP-MONTH                PIC 9(02).
           05  WS-MONTH-DIFF               PIC S9(04) COMP.
           05  WS-MONTH-REM                PIC S9(04) COMP.
           05  WS-MONTH-QUOT               PIC S9(04) COMP.
           05  WS-DAYS-IN-MONTH            PIC 9(02).
           05  WS-LEAP-REM-4               PIC S9(04) COMP.
           05  WS-LEAP-REM-100             PIC S9(04) COMP.
           05  WS-LEAP-REM-400             PIC S9(04) COMP.
           05  WS-LEAP-QUOT                PIC S9(06) COMP.
           05  WS-DATE-DB2                 PIC X(10).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DISPLAY             PIC X(10).
           05  WS-PERIOD-FROM              PIC 9(08).
           05  WS-TAX-YEAR                 PIC X(09).
           05  WS-TAX-YEAR-START           PIC 9(04).
           05  WS-TAX-YEAR-END             PIC 9(04).
      *
       01  WS-MONTH-DAYS-VALUES.
           05  FILLER                      PIC X(24)
               VALUE '312831303130313130313031'.
       01  WS-MONTH-DAYS-TABLE REDEFINES WS-MONTH-DAYS-VALUES.
           05  WS-MONTH-DAYS               PIC 9(02) OCCURS 12 TIMES.
      *
      *---- AMOUNTS ---------------------------------------------------*
       01  WS-AMOUNTS.
           05  WS-GROSS-INT                PIC S9(13)V99 COMP-3.
           05  WS-TAX-AMT                  PIC S9(13)V99 COMP-3.
           05  WS-NET-INT                  PIC S9(13)V99 COMP-3.
           05  WS-DR-INT                   PIC S9(13)V99 COMP-3.
           05  WS-UNARR-INT                PIC S9(13)V99 COMP-3.
           05  WS-TAX-RATE-USED            PIC S9(03)V99 COMP-3.
           05  WS-AMOUNT-EDIT              PIC Z,ZZZ,ZZ9.99.
           05  WS-RATE-EDIT                PIC Z9.99.
      *
       01  WS-POSTING-CONTROL.
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-EXT-REF-SEQ              PIC 9(08) VALUE ZERO.
           05  WS-COMMIT-FREQ              PIC S9(04) COMP VALUE +200.
           05  WS-SINCE-COMMIT             PIC S9(04) COMP VALUE ZERO.
      *
      *---- PRODUCT TABLE ---------------------------------------------*
       01  WS-PRODUCT-TABLE.
           05  WS-PROD-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-PROD-MAX                 PIC S9(04) COMP VALUE +300.
           05  WS-PROD-ENTRY               OCCURS 1 TO 300 TIMES
                                           DEPENDING ON WS-PROD-COUNT
                                           ASCENDING KEY WS-PT-PROD-CD
                                           INDEXED BY PT-IDX.
               10  WS-PT-PROD-CD           PIC X(10).
               10  WS-PT-ACCT-TYPE         PIC X(04).
               10  WS-PT-CCY-CD            PIC X(03).
               10  WS-PT-CR-INT-IND        PIC X(01).
               10  WS-PT-CR-CAP-FREQ       PIC X(01).
               10  WS-PT-CR-CAP-DAY        PIC 9(02).
               10  WS-PT-CR-CAP-MONTH      PIC 9(02).
               10  WS-PT-TAX-DEDUCT-IND    PIC X(01).
               10  WS-PT-DR-INT-IND        PIC X(01).
               10  WS-PT-DR-CAP-FREQ       PIC X(01).
               10  WS-PT-DR-CAP-DAY        PIC 9(02).
               10  WS-PT-DR-MIN-CHARGE     PIC S9(13)V99 COMP-3.
               10  WS-PT-GL-BALANCE-CR     PIC X(10).
               10  WS-PT-GL-BALANCE-DR     PIC X(10).
               10  WS-PT-GL-INT-EXPENSE    PIC X(10).
               10  WS-PT-GL-INT-INCOME     PIC X(10).
               10  WS-PT-GL-ACCR-PAY       PIC X(10).
               10  WS-PT-GL-ACCR-REC       PIC X(10).
               10  WS-PT-GL-TAX-PAYABLE    PIC X(10).
               10  WS-PT-COST-CENTRE       PIC X(06).
      *        ACCUMULATORS FOR THE GL JOURNAL
               10  WS-PT-CR-GROSS          PIC S9(13)V99 COMP-3.
               10  WS-PT-CR-TAX            PIC S9(13)V99 COMP-3.
               10  WS-PT-DR-CHARGED        PIC S9(13)V99 COMP-3.
               10  WS-PT-DR-WAIVED         PIC S9(13)V99 COMP-3.
               10  WS-PT-CR-COUNT          PIC S9(07) COMP-3.
               10  WS-PT-DR-COUNT          PIC S9(07) COMP-3.
      *
      *---- COUNTERS --------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-CR-CAP-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DR-CAP-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DR-WAIVE-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-TAXED-COUNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-ZERO-CAP-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-POSTING-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-INTPAID-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXCEPTION-COUNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-SKIP-PENDING-CLOSE       PIC S9(09) COMP-3 VALUE 0.
           05  WS-GL-LINE-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-GROSS              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-TAX                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-DR-INT             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-WAIVED             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-POSTED-CR-TOTAL          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-POSTED-DR-TOTAL          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-TOTAL-DR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-GL-TOTAL-CR              PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
      *---- REPORT ----------------------------------------------------*
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
      *
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'INCAP010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - INTEREST CAPITALISATION REGISTER'.
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
           05  FILLER            PIC X(11) VALUE 'PRODUCT'.
           05  FILLER            PIC X(04) VALUE 'C/D'.
           05  FILLER            PIC X(11) VALUE 'PERIOD FROM'.
           05  FILLER            PIC X(16) VALUE '           GROSS'.
           05  FILLER            PIC X(16) VALUE '             TAX'.
           05  FILLER            PIC X(16) VALUE '             NET'.
           05  FILLER            PIC X(16) VALUE '      UNARRANGED'.
           05  FILLER            PIC X(02) VALUE SPACES.
           05  FILLER            PIC X(21) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
      *
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-CR-DR                    PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-PERIOD-FROM              PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-GROSS                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  DL-TAX                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  DL-NET                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  DL-UNARR                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(20).
           05  FILLER                      PIC X(06) VALUE SPACES.
      *
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
      *---- DB2 -------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLINTP END-EXEC.
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
           PERFORM 2000-PROCESS-ACCOUNT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PRDPARM-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                OUTPUT INTPOST-FILE
                       GLJNL-FILE
                       REGISTER-FILE

           IF NOT WS-PRDPARM-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-INTPOST-STATUS NOT = '00'
           OR WS-GLJNL-STATUS NOT = '00'
           OR WS-REGISTER-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-PRDPARM-STATUS ' ' WS-ACCTMAST-STATUS ' '
                      WS-INTPOST-STATUS ' ' WS-GLJNL-STATUS ' '
                      WS-REGISTER-STATUS ' ' WS-DATECARD-STATUS
                      DELIMITED BY SIZE
                      INTO ABN-KEY-DATA
               END-STRING
               PERFORM Z999-ABEND
           END-IF

           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
           OR DTC-BUSINESS-DATE NOT NUMERIC
           OR DTC-NEXT-BUS-DATE NOT NUMERIC
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               MOVE DATECARD-RECORD (1:40) TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE

           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           MOVE DTC-NEXT-BUS-DATE          TO WS-NEXT-BUS-DATE
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO HD1-BUS-DATE

      *    UK TAX YEAR RUNS 6 APRIL TO 5 APRIL.
           IF WS-BD-MM > 4
           OR (WS-BD-MM = 4 AND WS-BD-DD >= 6)
               MOVE WS-BD-CCYY             TO WS-TAX-YEAR-START
           ELSE
               COMPUTE WS-TAX-YEAR-START = WS-BD-CCYY - 1
           END-IF
           COMPUTE WS-TAX-YEAR-END = WS-TAX-YEAR-START + 1
           STRING WS-TAX-YEAR-START '/' WS-TAX-YEAR-END
                  DELIMITED BY SIZE
                  INTO WS-TAX-YEAR
           END-STRING

           DISPLAY 'INCAP010 - BUSINESS DATE ' WS-DATE-DB2
                   ' NEXT ' WS-NEXT-BUS-DATE
                   ' TAX YEAR ' WS-TAX-YEAR
                   ' RUN ID ' DTC-RUN-ID

           PERFORM 1300-LOAD-PRODUCTS
           CLOSE PRDPARM-FILE

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       1300-LOAD-PRODUCTS.
      *================================================================*
           MOVE ZERO                       TO WS-PROD-COUNT
           PERFORM 1310-READ-PRODUCT
           PERFORM UNTIL WS-END-OF-PRODUCTS
               IF WS-PROD-COUNT >= WS-PROD-MAX
                   MOVE '1300-LOAD-PRODUCTS' TO ABN-PARAGRAPH
                   MOVE 1004               TO ABN-ABEND-CODE
                   MOVE 'PRODUCT TABLE FULL - INCREASE WS-PROD-MAX'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               END-IF
               ADD 1                       TO WS-PROD-COUNT
               SET PT-IDX                  TO WS-PROD-COUNT
               MOVE PRD-PROD-CD            TO WS-PT-PROD-CD (PT-IDX)
               MOVE PRD-ACCT-TYPE-CD       TO WS-PT-ACCT-TYPE (PT-IDX)
               MOVE PRD-CCY-CD             TO WS-PT-CCY-CD (PT-IDX)
               MOVE PRD-CR-INT-IND         TO WS-PT-CR-INT-IND (PT-IDX)
               MOVE PRD-CR-CAP-FREQ        TO WS-PT-CR-CAP-FREQ (PT-IDX)
               MOVE PRD-CR-CAP-DAY         TO WS-PT-CR-CAP-DAY (PT-IDX)
               MOVE PRD-CR-CAP-MONTH
                                     TO WS-PT-CR-CAP-MONTH (PT-IDX)
               MOVE PRD-TAX-DEDUCT-IND
                                     TO WS-PT-TAX-DEDUCT-IND (PT-IDX)
               MOVE PRD-DR-INT-IND         TO WS-PT-DR-INT-IND (PT-IDX)
               MOVE PRD-DR-CAP-FREQ        TO WS-PT-DR-CAP-FREQ (PT-IDX)
               MOVE PRD-DR-CAP-DAY         TO WS-PT-DR-CAP-DAY (PT-IDX)
               MOVE PRD-DR-MIN-CHARGE
                                     TO WS-PT-DR-MIN-CHARGE (PT-IDX)
               MOVE PRD-GL-BALANCE-CR
                                     TO WS-PT-GL-BALANCE-CR (PT-IDX)
               MOVE PRD-GL-BALANCE-DR
                                     TO WS-PT-GL-BALANCE-DR (PT-IDX)
               MOVE PRD-GL-INT-EXPENSE
                                     TO WS-PT-GL-INT-EXPENSE (PT-IDX)
               MOVE PRD-GL-INT-INCOME
                                     TO WS-PT-GL-INT-INCOME (PT-IDX)
               MOVE PRD-GL-INT-ACCR-PAY
                                     TO WS-PT-GL-ACCR-PAY (PT-IDX)
               MOVE PRD-GL-INT-ACCR-REC
                                     TO WS-PT-GL-ACCR-REC (PT-IDX)
               MOVE PRD-GL-TAX-PAYABLE
                                     TO WS-PT-GL-TAX-PAYABLE (PT-IDX)
               MOVE PRD-COST-CENTRE  TO WS-PT-COST-CENTRE (PT-IDX)
               MOVE ZERO TO WS-PT-CR-GROSS (PT-IDX)
                            WS-PT-CR-TAX (PT-IDX)
                            WS-PT-DR-CHARGED (PT-IDX)
                            WS-PT-DR-WAIVED (PT-IDX)
                            WS-PT-CR-COUNT (PT-IDX)
                            WS-PT-DR-COUNT (PT-IDX)
               PERFORM 1310-READ-PRODUCT
           END-PERFORM
           DISPLAY 'INCAP010 - PRODUCTS LOADED ' WS-PROD-COUNT
           .
      *================================================================*
       1310-READ-PRODUCT.
      *================================================================*
           READ PRDPARM-FILE
           EVALUATE TRUE
               WHEN WS-PRDPARM-OK
                   CONTINUE
               WHEN WS-PRDPARM-EOF
                   SET WS-END-OF-PRODUCTS  TO TRUE
               WHEN OTHER
                   MOVE '1310-READ-PRODUCT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'PRDPARM READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-PRDPARM-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           MOVE 'N'                        TO WS-ACCT-CHANGED-IND
                                              WS-CR-DUE-IND
                                              WS-DR-DUE-IND

           EVALUATE TRUE
               WHEN ACM-CLOSED
                   CONTINUE
               WHEN ACM-PENDING-CLOSE
                   ADD 1                   TO WS-SKIP-PENDING-CLOSE
               WHEN OTHER
                   PERFORM 2050-FIND-PRODUCT
                   IF WS-PROD-FOUND
                       PERFORM 2100-CHECK-CR-DUE THRU 2100-EXIT
                       PERFORM 2200-CHECK-DR-DUE THRU 2200-EXIT
                       IF WS-CR-CAP-DUE
                           PERFORM 2300-CAPITALISE-CREDIT
                       END-IF
                       IF WS-DR-CAP-DUE
                           PERFORM 2400-CAPITALISE-DEBIT
                       END-IF
                   END-IF
           END-EVALUATE

           IF WS-ACCT-CHANGED
               PERFORM 2900-REWRITE-ACCOUNT
           END-IF

           PERFORM 8000-READ-ACCOUNT
           .
      *================================================================*
       2050-FIND-PRODUCT.
      *================================================================*
           MOVE 'N'                        TO WS-PROD-FOUND-IND
           SEARCH ALL WS-PROD-ENTRY
               AT END
                   CONTINUE
               WHEN WS-PT-PROD-CD (PT-IDX) = ACM-PROD-CD
                   SET WS-PROD-FOUND       TO TRUE
           END-SEARCH
      *    A MISSING PRODUCT IS REPORTED BY INACR010 - NOT REPEATED.
           .
      *================================================================*
       2100-CHECK-CR-DUE.
      *================================================================*
           IF WS-PT-CR-INT-IND (PT-IDX) NOT = 'Y'
               GO TO 2100-EXIT
           END-IF

      *    ALREADY CAPITALISED TODAY (RERUN).
           IF ACM-LAST-CR-CAP-DATE NUMERIC
           AND ACM-LAST-CR-CAP-DATE >= WS-BUS-DATE
               GO TO 2100-EXIT
           END-IF

           MOVE WS-PT-CR-CAP-DAY (PT-IDX)  TO WS-CAP-DAY
           MOVE WS-PT-CR-CAP-MONTH (PT-IDX) TO WS-CAP-MONTH

           EVALUATE WS-PT-CR-CAP-FREQ (PT-IDX)
               WHEN 'T'
      *            FIXED TERM - ON OR AFTER MATURITY
                   IF ACM-MATURITY-DATE NUMERIC
                   AND ACM-MATURITY-DATE NOT = ZERO
                   AND ACM-MATURITY-DATE < WS-NEXT-BUS-DATE
                       SET WS-CR-CAP-DUE   TO TRUE
                   END-IF
               WHEN 'M'
                   PERFORM 2500-NOMINAL-IN-WINDOW THRU 2500-EXIT
                   IF WS-NOMINAL-DATE NOT = ZERO
                       SET WS-CR-CAP-DUE   TO TRUE
                   END-IF
               WHEN 'Q'
                   MOVE 3                  TO WS-MONTH-QUOT
                   PERFORM 2600-MONTH-DUE
                   IF WS-MONTH-REM = ZERO
                       PERFORM 2500-NOMINAL-IN-WINDOW THRU 2500-EXIT
                       IF WS-NOMINAL-DATE NOT = ZERO
                           SET WS-CR-CAP-DUE TO TRUE
                       END-IF
                   END-IF
               WHEN 'H'
                   MOVE 6                  TO WS-MONTH-QUOT
                   PERFORM 2600-MONTH-DUE
                   IF WS-MONTH-REM = ZERO
                       PERFORM 2500-NOMINAL-IN-WINDOW THRU 2500-EXIT
                       IF WS-NOMINAL-DATE NOT = ZERO
                           SET WS-CR-CAP-DUE TO TRUE
                       END-IF
                   END-IF
               WHEN 'A'
                   MOVE 12                 TO WS-MONTH-QUOT
                   PERFORM 2600-MONTH-DUE
                   IF WS-MONTH-REM = ZERO
                       PERFORM 2500-NOMINAL-IN-WINDOW THRU 2500-EXIT
                       IF WS-NOMINAL-DATE NOT = ZERO
                           SET WS-CR-CAP-DUE TO TRUE
                       END-IF
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-CHECK-DR-DUE.
      *================================================================*
           IF WS-PT-DR-INT-IND (PT-IDX) NOT = 'Y'
               GO TO 2200-EXIT
           END-IF
           IF ACM-LAST-DR-CAP-DATE NUMERIC
           AND ACM-LAST-DR-CAP-DATE >= WS-BUS-DATE
               GO TO 2200-EXIT
           END-IF

           MOVE WS-PT-DR-CAP-DAY (PT-IDX)  TO WS-CAP-DAY
      *    DEBIT INTEREST QUARTERS ARE ALWAYS MAR/JUN/SEP/DEC.
           MOVE 3                          TO WS-CAP-MONTH

           EVALUATE WS-PT-DR-CAP-FREQ (PT-IDX)
               WHEN 'M'
                   PERFORM 2500-NOMINAL-IN-WINDOW THRU 2500-EXIT
                   IF WS-NOMINAL-DATE NOT = ZERO
                       SET WS-DR-CAP-DUE   TO TRUE
                   END-IF
               WHEN 'Q'
                   MOVE 3                  TO WS-MONTH-QUOT
                   PERFORM 2600-MONTH-DUE
                   IF WS-MONTH-REM = ZERO
                       PERFORM 2500-NOMINAL-IN-WINDOW THRU 2500-EXIT
                       IF WS-NOMINAL-DATE NOT = ZERO
                           SET WS-DR-CAP-DUE TO TRUE
                       END-IF
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2500-NOMINAL-IN-WINDOW.
      *================================================================*
      *    BUILD THE NOMINAL CAPITALISATION DATE FOR THE CURRENT MONTH
      *    FROM WS-CAP-DAY (CAPPED AT THE MONTH END). IF IT FALLS IN
      *    [TODAY, NEXT BUSINESS DATE) LEAVE IT IN WS-NOMINAL-DATE,
      *    OTHERWISE RETURN ZERO.
      *    ALSO TRIES NEXT MONTH, BECAUSE THE WINDOW CAN SPAN A MONTH
      *    END (E.G. FRIDAY 29TH TO MONDAY 1ST WITH CAP DAY 31).
           MOVE WS-BD-CCYY                 TO WS-ND-CCYY
           MOVE WS-BD-MM                   TO WS-ND-MM
           PERFORM 2510-SET-NOMINAL-DAY
           IF WS-NOMINAL-DATE >= WS-BUS-DATE
           AND WS-NOMINAL-DATE < WS-NEXT-BUS-DATE
               GO TO 2500-EXIT
           END-IF

           IF WS-BD-MM = 12
               COMPUTE WS-ND-CCYY = WS-BD-CCYY + 1
               MOVE 1                      TO WS-ND-MM
           ELSE
               MOVE WS-BD-CCYY             TO WS-ND-CCYY
               COMPUTE WS-ND-MM = WS-BD-MM + 1
           END-IF
           PERFORM 2510-SET-NOMINAL-DAY
           IF WS-NOMINAL-DATE >= WS-BUS-DATE
           AND WS-NOMINAL-DATE < WS-NEXT-BUS-DATE
               GO TO 2500-EXIT
           END-IF

           MOVE ZERO                       TO WS-NOMINAL-DATE
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       2510-SET-NOMINAL-DAY.
      *================================================================*
           MOVE WS-MONTH-DAYS (WS-ND-MM)   TO WS-DAYS-IN-MONTH
           IF WS-ND-MM = 2
               DIVIDE WS-ND-CCYY BY 4   GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-4
               DIVIDE WS-ND-CCYY BY 100 GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-100
               DIVIDE WS-ND-CCYY BY 400 GIVING WS-LEAP-QUOT
                                        REMAINDER WS-LEAP-REM-400
               IF (WS-LEAP-REM-4 = 0 AND WS-LEAP-REM-100 NOT = 0)
               OR WS-LEAP-REM-400 = 0
                   MOVE 29                 TO WS-DAYS-IN-MONTH
               END-IF
           END-IF

           IF WS-CAP-DAY = ZERO
           OR WS-CAP-DAY > WS-DAYS-IN-MONTH
               MOVE WS-DAYS-IN-MONTH       TO WS-ND-DD
           ELSE
               MOVE WS-CAP-DAY             TO WS-ND-DD
           END-IF
           .
      *================================================================*
       2600-MONTH-DUE.
      *================================================================*
      *    ON ENTRY WS-MONTH-QUOT = FREQUENCY IN MONTHS (3, 6, 12) AND
      *    WS-CAP-MONTH = THE FIRST CAPITALISATION MONTH OF THE YEAR.
      *    WS-MONTH-REM = 0 IF THE CURRENT MONTH (OR NEXT MONTH WHEN
      *    THE WINDOW CROSSES A MONTH END) IS A DUE MONTH.
           IF WS-CAP-MONTH = ZERO
               MOVE 12                     TO WS-CAP-MONTH
           END-IF
           COMPUTE WS-MONTH-DIFF = WS-BD-MM - WS-CAP-MONTH + 12
           DIVIDE WS-MONTH-DIFF BY WS-MONTH-QUOT
                  GIVING WS-LEAP-QUOT REMAINDER WS-MONTH-REM

           IF WS-MONTH-REM NOT = ZERO
           AND WS-NEXT-BUS-DATE (5:2) NOT = WS-BUS-DATE (5:2)
               COMPUTE WS-MONTH-DIFF = WS-BD-MM + 1 - WS-CAP-MONTH + 12
               DIVIDE WS-MONTH-DIFF BY WS-MONTH-QUOT
                      GIVING WS-LEAP-QUOT REMAINDER WS-MONTH-REM
           END-IF
           .
      *================================================================*
       2300-CAPITALISE-CREDIT.
      *================================================================*
           MOVE ACM-ACCRUED-INT-CR         TO WS-GROSS-INT
           MOVE ZERO                       TO WS-TAX-AMT
                                              WS-TAX-RATE-USED

           IF ACM-LAST-CR-CAP-DATE NUMERIC
           AND ACM-LAST-CR-CAP-DATE NOT = ZERO
               MOVE ACM-LAST-CR-CAP-DATE   TO WS-PERIOD-FROM
           ELSE
               MOVE ACM-OPEN-DATE          TO WS-PERIOD-FROM
           END-IF

           IF WS-GROSS-INT NOT > ZERO
               ADD 1                       TO WS-ZERO-CAP-COUNT
           ELSE
      *        TAX - NET PAYING UK ACCOUNTS ON TAXABLE PRODUCTS ONLY.
      *        TAX IS ROUNDED DOWN TO THE PENNY (HMRC PRACTICE).
               IF WS-PT-TAX-DEDUCT-IND (PT-IDX) = 'Y'
               AND ACM-TAX-NET
                   MOVE WS-BASIC-RATE      TO WS-TAX-RATE-USED
                   COMPUTE WS-TAX-AMT =
                           FUNCTION INTEGER-PART
                               (WS-GROSS-INT * WS-BASIC-RATE) / 100
                   ADD 1                   TO WS-TAXED-COUNT
               END-IF
               COMPUTE WS-NET-INT = WS-GROSS-INT - WS-TAX-AMT

               PERFORM 2310-POST-GROSS-CREDIT
               IF WS-TAX-AMT > ZERO
                   PERFORM 2320-POST-TAX-DEBIT
               END-IF
               PERFORM 2330-INSERT-INTPAID-CR

               ADD 1                       TO WS-CR-CAP-COUNT
               ADD 1                       TO WS-PT-CR-COUNT (PT-IDX)
               ADD WS-GROSS-INT            TO WS-PT-CR-GROSS (PT-IDX)
               ADD WS-TAX-AMT              TO WS-PT-CR-TAX (PT-IDX)
               ADD WS-GROSS-INT            TO WS-TOTAL-GROSS
               ADD WS-TAX-AMT              TO WS-TOTAL-TAX

               MOVE 'C'                    TO DL-CR-DR
               MOVE WS-GROSS-INT           TO DL-GROSS
               MOVE WS-TAX-AMT             TO DL-TAX
               MOVE WS-NET-INT             TO DL-NET
               MOVE ZERO                   TO DL-UNARR
               IF WS-TAX-AMT > ZERO
                   MOVE 'NET - TAX DEDUCTED' TO DL-NOTE
               ELSE
                   MOVE 'PAID GROSS'       TO DL-NOTE
               END-IF
               PERFORM 7100-REGISTER-DETAIL
           END-IF

      *    THE ACCRUAL AND THE SUB-PENNY RESIDUE ARE CLEARED.
           MOVE ZERO                       TO ACM-ACCRUED-INT-CR
                                              ACM-ACCR-CR-RESIDUE
           MOVE WS-BUS-DATE                TO ACM-LAST-CR-CAP-DATE
           SET WS-ACCT-CHANGED             TO TRUE
           .
      *================================================================*
       2310-POST-GROSS-CREDIT.
      *================================================================*
           PERFORM 2800-INIT-POSTING
           MOVE 'INT '                     TO PST-TXN-CODE
           SET PST-CREDIT                  TO TRUE
           MOVE WS-GROSS-INT               TO PST-AMOUNT
           IF WS-TAX-AMT > ZERO
               MOVE 'GROSS INTEREST'       TO PST-NARRATIVE-1
           ELSE
               MOVE 'INTEREST PAID'        TO PST-NARRATIVE-1
           END-IF
           MOVE WS-PERIOD-FROM             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           STRING 'PERIOD FROM ' WS-DATE-DB2
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           PERFORM 2850-WRITE-POSTING
           ADD WS-GROSS-INT                TO WS-POSTED-CR-TOTAL
           .
      *================================================================*
       2320-POST-TAX-DEBIT.
      *================================================================*
           PERFORM 2800-INIT-POSTING
           MOVE 'TAX '                     TO PST-TXN-CODE
           SET PST-DEBIT                   TO TRUE
           MOVE WS-TAX-AMT                 TO PST-AMOUNT
           MOVE WS-TAX-RATE-USED           TO WS-RATE-EDIT
           STRING 'INCOME TAX DEDUCTED AT ' WS-RATE-EDIT '%'
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-1
           END-STRING
           MOVE WS-GROSS-INT               TO WS-AMOUNT-EDIT
           STRING 'ON GROSS INTEREST ' WS-AMOUNT-EDIT
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           PERFORM 2850-WRITE-POSTING
           ADD WS-TAX-AMT                  TO WS-POSTED-DR-TOTAL
           .
      *================================================================*
       2330-INSERT-INTPAID-CR.
      *================================================================*
           MOVE ACM-SORT-CODE              TO INP-SORT-CODE
           MOVE ACM-ACCT-NO                TO INP-ACCT-NO
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO INP-CAP-DATE
                                              INP-PERIOD-TO
           MOVE 'C'                        TO INP-CR-DR-IND
           MOVE ACM-CIF-NO                 TO INP-CIF-NO
           MOVE ACM-PROD-CD                TO INP-PROD-CD
           MOVE WS-PERIOD-FROM             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO INP-PERIOD-FROM
           MOVE WS-GROSS-INT               TO INP-GROSS-AMT
           MOVE WS-TAX-AMT                 TO INP-TAX-AMT
           MOVE WS-NET-INT                 TO INP-NET-AMT
           MOVE ZERO                       TO INP-UNARR-AMT
           MOVE WS-TAX-RATE-USED           TO INP-TAX-RATE
           IF ACM-TAX-STATUS = SPACE
               MOVE 'N'                    TO INP-TAX-STATUS
           ELSE
               MOVE ACM-TAX-STATUS         TO INP-TAX-STATUS
           END-IF
           MOVE WS-TAX-YEAR                TO INP-TAX-YEAR
           MOVE 'N'                        TO INP-WAIVED-IND
           IF WS-PT-CR-CAP-FREQ (PT-IDX) = 'T'
               MOVE 'MATR'                 TO INP-CAP-REASON
           ELSE
               MOVE 'CYCL'                 TO INP-CAP-REASON
           END-IF
           PERFORM 2890-INSERT-INTPAID
           .
      *================================================================*
       2400-CAPITALISE-DEBIT.
      *================================================================*
           MOVE ACM-ACCRUED-INT-DR         TO WS-DR-INT
           MOVE ACM-ACCR-DR-UNARR          TO WS-UNARR-INT
           IF WS-UNARR-INT NOT NUMERIC
           OR WS-UNARR-INT > WS-DR-INT
               MOVE ZERO                   TO WS-UNARR-INT
           END-IF
           MOVE 'N'                        TO WS-WAIVED-IND

           IF ACM-LAST-DR-CAP-DATE NUMERIC
           AND ACM-LAST-DR-CAP-DATE NOT = ZERO
               MOVE ACM-LAST-DR-CAP-DATE   TO WS-PERIOD-FROM
           ELSE
               MOVE ACM-OPEN-DATE          TO WS-PERIOD-FROM
           END-IF

           IF WS-DR-INT NOT > ZERO
               ADD 1                       TO WS-ZERO-CAP-COUNT
           ELSE
               IF WS-DR-INT < WS-PT-DR-MIN-CHARGE (PT-IDX)
               OR ACM-CHARGES-WAIVED
                   SET WS-DR-WAIVED        TO TRUE
                   ADD 1                   TO WS-DR-WAIVE-COUNT
                   ADD WS-DR-INT           TO WS-PT-DR-WAIVED (PT-IDX)
                   ADD WS-DR-INT           TO WS-TOTAL-WAIVED
                   MOVE 'WAIVED - DE MINIMIS' TO DL-NOTE
                   IF ACM-CHARGES-WAIVED
                       MOVE 'WAIVED - ACCOUNT FLAG' TO DL-NOTE
                   END-IF
               ELSE
                   PERFORM 2410-POST-DEBIT-INTEREST
                   ADD WS-DR-INT           TO WS-PT-DR-CHARGED (PT-IDX)
                   ADD WS-DR-INT           TO WS-TOTAL-DR-INT
                   MOVE 'CHARGED'          TO DL-NOTE
               END-IF

               PERFORM 2420-INSERT-INTPAID-DR

               ADD 1                       TO WS-DR-CAP-COUNT
               ADD 1                       TO WS-PT-DR-COUNT (PT-IDX)

               MOVE 'D'                    TO DL-CR-DR
               MOVE WS-DR-INT              TO DL-GROSS
               MOVE ZERO                   TO DL-TAX
               IF WS-DR-WAIVED
                   MOVE ZERO               TO DL-NET
               ELSE
                   MOVE WS-DR-INT          TO DL-NET
               END-IF
               MOVE WS-UNARR-INT           TO DL-UNARR
               PERFORM 7100-REGISTER-DETAIL
           END-IF

           MOVE ZERO                       TO ACM-ACCRUED-INT-DR
                                              ACM-ACCR-DR-RESIDUE
                                              ACM-ACCR-DR-UNARR
           MOVE WS-BUS-DATE                TO ACM-LAST-DR-CAP-DATE
           SET WS-ACCT-CHANGED             TO TRUE
           .
      *================================================================*
       2410-POST-DEBIT-INTEREST.
      *================================================================*
           PERFORM 2800-INIT-POSTING
           MOVE 'INT '                     TO PST-TXN-CODE
           SET PST-DEBIT                   TO TRUE
           MOVE WS-DR-INT                  TO PST-AMOUNT
           MOVE 'OVERDRAFT INTEREST'       TO PST-NARRATIVE-1
           IF WS-UNARR-INT > ZERO
               MOVE WS-UNARR-INT           TO WS-AMOUNT-EDIT
               STRING 'INCL UNARRANGED ' WS-AMOUNT-EDIT
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
           ELSE
               MOVE WS-PERIOD-FROM         TO WS-DATE-WORK
               PERFORM 8900-FORMAT-DB2-DATE
               STRING 'PERIOD FROM ' WS-DATE-DB2
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
           END-IF
           PERFORM 2850-WRITE-POSTING
           ADD WS-DR-INT                   TO WS-POSTED-DR-TOTAL
           .
      *================================================================*
       2420-INSERT-INTPAID-DR.
      *================================================================*
           MOVE ACM-SORT-CODE              TO INP-SORT-CODE
           MOVE ACM-ACCT-NO                TO INP-ACCT-NO
           MOVE WS-BUS-DATE                TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO INP-CAP-DATE
                                              INP-PERIOD-TO
           MOVE 'D'                        TO INP-CR-DR-IND
           MOVE ACM-CIF-NO                 TO INP-CIF-NO
           MOVE ACM-PROD-CD                TO INP-PROD-CD
           MOVE WS-PERIOD-FROM             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO INP-PERIOD-FROM
           MOVE WS-DR-INT                  TO INP-GROSS-AMT
           MOVE ZERO                       TO INP-TAX-AMT
                                              INP-TAX-RATE
           IF WS-DR-WAIVED
               MOVE ZERO                   TO INP-NET-AMT
               MOVE 'Y'                    TO INP-WAIVED-IND
           ELSE
               MOVE WS-DR-INT              TO INP-NET-AMT
               MOVE 'N'                    TO INP-WAIVED-IND
           END-IF
           MOVE WS-UNARR-INT               TO INP-UNARR-AMT
           MOVE 'N'                        TO INP-TAX-STATUS
           MOVE WS-TAX-YEAR                TO INP-TAX-YEAR
           MOVE 'CYCL'                     TO INP-CAP-REASON
           PERFORM 2890-INSERT-INTPAID
           .
      *================================================================*
       2800-INIT-POSTING.
      *================================================================*
      *    COMMON FIELDS FOR A BATCH HARD POSTING. THE POST TIME IS SET
      *    LATE IN THE DAY SO THAT, WHEN MERGED WITH TOMORROW'S MEMO LOG
      *    IN PSEOD01, INTEREST SORTS AFTER THE CUSTOMER'S OWN ITEMS
      *    FOR THE SAME ACCOUNT AND DATE.
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE ACM-SORT-CODE              TO PST-SORT-CODE
           MOVE ACM-ACCT-NO                TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE ACM-CCY-CD                 TO PST-CCY-CD
           MOVE WS-BUS-DATE                TO PST-VALUE-DATE
           MOVE 'BATCH '                   TO PST-CHANNEL-CD
           ADD 1                           TO WS-EXT-REF-SEQ
           STRING 'INCAP' WS-BUS-DATE WS-EXT-REF-SEQ
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           .
      *================================================================*
       2850-WRITE-POSTING.
      *================================================================*
           WRITE INTPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-INTPOST-STATUS NOT = '00'
               MOVE '2850-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'INTPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-INTPOST-STATUS      TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-POSTING-COUNT
           .
      *================================================================*
       2890-INSERT-INTPAID.
      *================================================================*
           MOVE SPACES                     TO INP-BATCH-RUN-ID
           MOVE 'IC'                       TO INP-BATCH-RUN-ID (1:2)
           MOVE DTC-RUN-ID                 TO INP-BATCH-RUN-ID (3:5)

           EXEC SQL
               INSERT INTO MBK.INTPAID
                     (SORT_CODE, ACCT_NO, CAP_DATE, CR_DR_IND,
                      CIF_NO, PROD_CD, PERIOD_FROM, PERIOD_TO,
                      GROSS_AMT, TAX_AMT, NET_AMT, UNARR_AMT,
                      TAX_RATE, TAX_STATUS, TAX_YEAR, WAIVED_IND,
                      CAP_REASON, BATCH_RUN_ID, INSERT_TS)
               VALUES
                     (:INP-SORT-CODE, :INP-ACCT-NO, :INP-CAP-DATE,
                      :INP-CR-DR-IND, :INP-CIF-NO, :INP-PROD-CD,
                      :INP-PERIOD-FROM, :INP-PERIOD-TO,
                      :INP-GROSS-AMT, :INP-TAX-AMT, :INP-NET-AMT,
                      :INP-UNARR-AMT, :INP-TAX-RATE, :INP-TAX-STATUS,
                      :INP-TAX-YEAR, :INP-WAIVED-IND, :INP-CAP-REASON,
                      :INP-BATCH-RUN-ID, CURRENT TIMESTAMP)
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-INTPAID-COUNT
               WHEN -803
      *            ALREADY INSERTED BY A PREVIOUS (FAILED) RUN THAT
      *            COMMITTED BEFORE THE VSAM REWRITE. THE ROW STANDS.
                   DISPLAY 'INCAP010 W002 INTPAID ROW EXISTS '
                           INP-SORT-CODE ' ' INP-ACCT-NO ' '
                           INP-CAP-DATE ' ' INP-CR-DR-IND
                   ADD 1                   TO WS-EXCEPTION-COUNT
               WHEN OTHER
                   MOVE '2890-INSERT-INTPAID' TO ABN-PARAGRAPH
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *================================================================*
       2900-REWRITE-ACCOUNT.
      *================================================================*
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER

           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '2900-REWRITE-ACCOUNT' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               EXEC SQL ROLLBACK END-EXEC
               PERFORM Z999-ABEND
           END-IF

      *    COMMIT AFTER THE REWRITE SO THAT A COMMITTED INTPAID ROW
      *    ALWAYS HAS A MATCHING CLEARED ACCRUAL ON THE MASTER (BUT SEE
      *    THE RESTART NOTE IN THE HEADER - VSAM IS NOT IN THE UOW).
           ADD 1                           TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
               EXEC SQL COMMIT END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           EXEC SQL COMMIT END-EXEC

      *    CONTROL CHECK - POSTINGS WRITTEN MUST EQUAL THE AMOUNTS
      *    CAPITALISED (CREDIT = GROSS, DEBIT = TAX + DEBIT INTEREST).
           IF WS-POSTED-CR-TOTAL NOT = WS-TOTAL-GROSS
           OR WS-POSTED-DR-TOTAL NOT = WS-TOTAL-TAX + WS-TOTAL-DR-INT
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'POSTED TOTALS DO NOT MATCH CAPITALISED TOTALS'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           PERFORM 3100-WRITE-GL-JOURNAL
           PERFORM 3200-REGISTER-TOTALS

           CLOSE ACCTMAST-FILE
                 INTPOST-FILE
                 GLJNL-FILE
                 REGISTER-FILE

           DISPLAY 'INCAP010 - ACCOUNTS READ        ' WS-READ-COUNT
           DISPLAY 'INCAP010 - CREDIT CAPITALISED   ' WS-CR-CAP-COUNT
           DISPLAY 'INCAP010 -   OF WHICH TAXED     ' WS-TAXED-COUNT
           DISPLAY 'INCAP010 - DEBIT CAPITALISED    ' WS-DR-CAP-COUNT
           DISPLAY 'INCAP010 -   OF WHICH WAIVED    ' WS-DR-WAIVE-COUNT
           DISPLAY 'INCAP010 - DUE BUT NIL ACCRUED  ' WS-ZERO-CAP-COUNT
           DISPLAY 'INCAP010 - POSTINGS WRITTEN     ' WS-POSTING-COUNT
           DISPLAY 'INCAP010 - INTPAID ROWS         ' WS-INTPAID-COUNT
           DISPLAY 'INCAP010 - SKIPPED PEND CLOSE   '
                   WS-SKIP-PENDING-CLOSE

           IF WS-EXCEPTION-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       3100-WRITE-GL-JOURNAL.
      *================================================================*
      *    PER PRODUCT:
      *      CREDIT: DR ACCRUED PAYABLE      CR CUSTOMER BALS (GROSS)
      *              DR CUSTOMER BALANCES    CR TAX PAYABLE (TAX)
      *      DEBIT : DR CUSTOMER BALANCES    CR ACCRUED RECEIVABLE
      *      WAIVED: DR INTEREST INCOME      CR ACCRUED RECEIVABLE
      *              (REVERSES THE INCOME RECOGNISED BY INACR010)
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           SET GLJ-SRC-CAPITALISE          TO TRUE
           STRING 'ICAP' WS-BUS-DATE DTC-RUN-ID (1:4)
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE DTC-RUN-ID                 TO GLJH-RUN-ID
           MOVE FUNCTION CURRENT-DATE (1:8) TO GLJH-CREATE-DATE
           MOVE FUNCTION CURRENT-DATE (9:6) TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3190-WRITE-GL-RECORD

           PERFORM VARYING PT-IDX FROM 1 BY 1
                   UNTIL PT-IDX > WS-PROD-COUNT
               IF WS-PT-CR-GROSS (PT-IDX) > ZERO
                   MOVE WS-PT-GL-ACCR-PAY (PT-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT          TO TRUE
                   MOVE WS-PT-CR-GROSS (PT-IDX) TO GLJD-AMOUNT
                   MOVE 'CREDIT INTEREST CAPITALISED'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-PT-GL-BALANCE-CR (PT-IDX)
                                           TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT         TO TRUE
                   MOVE WS-PT-CR-GROSS (PT-IDX) TO GLJD-AMOUNT
                   MOVE 'CREDIT INTEREST CAPITALISED'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
               END-IF
               IF WS-PT-CR-TAX (PT-IDX) > ZERO
                   MOVE WS-PT-GL-BALANCE-CR (PT-IDX)
                                           TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT          TO TRUE
                   MOVE WS-PT-CR-TAX (PT-IDX) TO GLJD-AMOUNT
                   MOVE 'INCOME TAX DEDUCTED FROM INTEREST'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-PT-GL-TAX-PAYABLE (PT-IDX)
                                           TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT         TO TRUE
                   MOVE WS-PT-CR-TAX (PT-IDX) TO GLJD-AMOUNT
                   MOVE 'INCOME TAX DEDUCTED FROM INTEREST'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
               END-IF
               IF WS-PT-DR-CHARGED (PT-IDX) > ZERO
                   MOVE WS-PT-GL-BALANCE-DR (PT-IDX)
                                           TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT          TO TRUE
                   MOVE WS-PT-DR-CHARGED (PT-IDX) TO GLJD-AMOUNT
                   MOVE 'DEBIT INTEREST CAPITALISED'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-PT-GL-ACCR-REC (PT-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT         TO TRUE
                   MOVE WS-PT-DR-CHARGED (PT-IDX) TO GLJD-AMOUNT
                   MOVE 'DEBIT INTEREST CAPITALISED'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
               END-IF
               IF WS-PT-DR-WAIVED (PT-IDX) > ZERO
                   MOVE WS-PT-GL-INT-INCOME (PT-IDX)
                                           TO GLJD-GL-ACCOUNT
                   SET GLJD-DEBIT          TO TRUE
                   MOVE WS-PT-DR-WAIVED (PT-IDX) TO GLJD-AMOUNT
                   MOVE 'DEBIT INTEREST WAIVED - DE MINIMIS'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
                   MOVE WS-PT-GL-ACCR-REC (PT-IDX) TO GLJD-GL-ACCOUNT
                   SET GLJD-CREDIT         TO TRUE
                   MOVE WS-PT-DR-WAIVED (PT-IDX) TO GLJD-AMOUNT
                   MOVE 'DEBIT INTEREST WAIVED - DE MINIMIS'
                                           TO GLJD-DESCRIPTION
                   PERFORM 3180-GL-DETAIL
               END-IF
           END-PERFORM

           MOVE SPACES                     TO GLJ-BODY
           SET GLJ-TRAILER                 TO TRUE
           MOVE WS-GL-LINE-COUNT           TO GLJT-LINE-COUNT
           MOVE WS-GL-TOTAL-DR             TO GLJT-TOTAL-DR
           MOVE WS-GL-TOTAL-CR             TO GLJT-TOTAL-CR
           MOVE 1                          TO GLJT-JOURNAL-COUNT
           PERFORM 3190-WRITE-GL-RECORD

           IF WS-GL-TOTAL-DR NOT = WS-GL-TOTAL-CR
               MOVE '3100-WRITE-GL-JOURNAL' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL CAPITALISATION JOURNAL DOES NOT BALANCE'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3180-GL-DETAIL.
      *================================================================*
           SET GLJ-DETAIL                  TO TRUE
           ADD 1                           TO WS-GL-LINE-COUNT
           MOVE WS-GL-LINE-COUNT           TO GLJD-LINE-NO
           MOVE WS-PT-COST-CENTRE (PT-IDX) TO GLJD-COST-CENTRE
           MOVE WS-PT-PROD-CD (PT-IDX)     TO GLJD-PROD-CD
           MOVE WS-PT-CCY-CD (PT-IDX)      TO GLJD-CCY-CD
           MOVE WS-BUS-DATE                TO GLJD-VALUE-DATE
           MOVE SPACES                     TO GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE 'INT '                     TO GLJD-TXN-CODE
           MOVE GLJ-JOURNAL-ID             TO GLJD-SOURCE-REF
           IF GLJD-DEBIT
               ADD GLJD-AMOUNT             TO WS-GL-TOTAL-DR
           ELSE
               ADD GLJD-AMOUNT             TO WS-GL-TOTAL-CR
           END-IF
           PERFORM 3190-WRITE-GL-RECORD
           .
      *================================================================*
       3190-WRITE-GL-RECORD.
      *================================================================*
           WRITE GLJNL-RECORD              FROM GLJ-JOURNAL-LINE
           IF WS-GLJNL-STATUS NOT = '00'
               MOVE '3190-WRITE-GL-RECORD' TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'GLJNL WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-GLJNL-STATUS        TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3200-REGISTER-TOTALS.
      *================================================================*
           PERFORM 7000-REGISTER-HEADINGS

           MOVE 'ACCOUNTS READ'            TO TL-LABEL
           MOVE WS-READ-COUNT              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE

           MOVE 'CREDIT INTEREST CAPITALISED (GROSS)'
                                           TO TL-LABEL
           MOVE WS-CR-CAP-COUNT            TO TL-COUNT
           MOVE WS-TOTAL-GROSS             TO TL-AMOUNT
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE

           MOVE 'INCOME TAX DEDUCTED'      TO TL-LABEL
           MOVE WS-TAXED-COUNT             TO TL-COUNT
           MOVE WS-TOTAL-TAX               TO TL-AMOUNT
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE

           MOVE 'DEBIT INTEREST CHARGED'   TO TL-LABEL
           COMPUTE TL-COUNT = WS-DR-CAP-COUNT - WS-DR-WAIVE-COUNT
           MOVE WS-TOTAL-DR-INT            TO TL-AMOUNT
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE

           MOVE 'DEBIT INTEREST WAIVED'    TO TL-LABEL
           MOVE WS-DR-WAIVE-COUNT          TO TL-COUNT
           MOVE WS-TOTAL-WAIVED            TO TL-AMOUNT
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE

           MOVE 'DUE BUT NOTHING ACCRUED'  TO TL-LABEL
           MOVE WS-ZERO-CAP-COUNT          TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE

           MOVE 'POSTINGS WRITTEN FOR PSEOD01'
                                           TO TL-LABEL
           MOVE WS-POSTING-COUNT           TO TL-COUNT
           COMPUTE TL-AMOUNT = WS-POSTED-CR-TOTAL - WS-POSTED-DR-TOTAL
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE

           MOVE 'SKIPPED - PENDING CLOSURE'
                                           TO TL-LABEL
           MOVE WS-SKIP-PENDING-CLOSE      TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE

           MOVE 'GL JOURNAL LINES / TOTAL DEBITS'
                                           TO TL-LABEL
           MOVE WS-GL-LINE-COUNT           TO TL-COUNT
           MOVE WS-GL-TOTAL-DR             TO TL-AMOUNT
           WRITE REGISTER-RECORD           FROM WS-TOTAL-LINE
           .
      *================================================================*
       7000-REGISTER-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REGISTER-RECORD           FROM WS-HEADING-1
           WRITE REGISTER-RECORD           FROM WS-HEADING-2
           WRITE REGISTER-RECORD           FROM WS-HEADING-3
           MOVE 4                          TO WS-LINE-COUNT
           .
      *================================================================*
       7100-REGISTER-DETAIL.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REGISTER-HEADINGS
           END-IF
           MOVE ACM-SORT-CODE              TO DL-SORT-CODE
           MOVE ACM-ACCT-NO                TO DL-ACCT-NO
           MOVE ACM-PROD-CD                TO DL-PROD-CD
           MOVE WS-PERIOD-FROM             TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO DL-PERIOD-FROM
           WRITE REGISTER-RECORD           FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   ADD 1                   TO WS-READ-COUNT
               WHEN WS-ACCTMAST-EOF
                   SET WS-END-OF-ACCOUNTS  TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   EXEC SQL ROLLBACK END-EXEC
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       8900-FORMAT-DB2-DATE.
      *================================================================*
      *    WS-DATE-WORK (CCYYMMDD) -> WS-DATE-DB2 (CCYY-MM-DD)
           MOVE SPACES                     TO WS-DATE-DB2
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-DATE-DB2
           END-STRING
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           MOVE ACM-KEY                    TO ABN-KEY-DATA
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
