       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RGBOE010.
       AUTHOR.        G H TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1993-04-05.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : RGBOE010                                            *
      * TITLE    : BANK OF ENGLAND QUARTERLY STATISTICAL RETURN        *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : RGBOE01 (QUARTERLY - FIRST BUSINESS DAY AFTER THE   *
      *            QUARTER END, BEFORE PSEOD01 POSTS THE NEW QUARTER)  *
      *----------------------------------------------------------------*
      * INPUT    : ACCTMAST - DDA ACCOUNT MASTER (VSAM KSDS, SEQ)      *
      *            LOANMAST - PERSONAL LOAN MASTER (VSAM KSDS, SEQ)    *
      *            CIFMAST  - CUSTOMER MASTER (VSAM KSDS, RANDOM) FOR  *
      *                       THE SECTOR (CIF-CUST-TYPE)               *
      *            PARMIN   - OPTIONAL 'QTREND=CCYYMMDD' OVERRIDE      *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : BOERTN   - FORM LINE FILE (RGBOEREC FB 100)         *
      *            REPORT   - RETURN WORKING REPORT (FBA 133)          *
      *----------------------------------------------------------------*
      * BUILDS THE STERLING DEPOSIT AND LENDING LINES OF THE QUARTERLY *
      * BALANCE SHEET RETURN (INTERNAL FORM CODE 'BTQ1'). EACH LINE IS *
      * A SECTOR (P PERSONAL / B BUSINESS FROM CIF-CUST-TYPE) AND A    *
      * PRODUCT CLASS:                                                 *
      *   D1nn/D2nn  DEPOSITS (CREDIT BALANCES + ACCRUED CR INTEREST)  *
      *              CURRENT, INSTANT ACCESS SAVINGS, CASH ISA, AND    *
      *              FIXED TERM BY RESIDUAL MATURITY FROM THE QUARTER  *
      *              END (ACM-MATURITY-DATE): UP TO 3M, 3M-1Y, 1-2Y,   *
      *              2-5Y, OVER 5Y. A MATURED DEPOSIT WITH NO          *
      *              INSTRUCTION (MATURITY DATE ZERO - SEE INFTD010)   *
      *              IS INSTANT ACCESS AND REPORTED AS SAVINGS.        *
      *   A3nn       OVERDRAFTS (DEBIT BALANCES + ACCRUED DR INTEREST) *
      *   A4nn       PERSONAL LOANS FROM LOANMAST (PRINCIPAL + ACCRUED *
      *              INTEREST + FEES). 90+ DAYS (BUCKET 4-5), DEFAULTED*
      *              OR IN COLLECTIONS ARE SHOWN SEPARATELY.           *
      *   M9nn       MEMORANDUM - UNDRAWN ARRANGED OD LIMITS AND LOANS *
      *              WRITTEN OFF IN THE QUARTER.                       *
      * AMOUNTS ARE REPORTED IN GBP THOUSANDS, ROUNDED PER LINE.       *
      * EXCLUDED: CLOSED ACCOUNTS, NON-STERLING ACCOUNTS (FORM BT-FX   *
      * IS PREPARED BY TREASURY BY HAND), LOAN/MORTGAGE SHADOW         *
      * ACCOUNTS ON ACCTMAST (MORTGAGES ARE REPORTED BY THE MORTGAGE   *
      * SERVICER UNDER ITS OWN REPORTER CODE).                         *
      *----------------------------------------------------------------*
      * KNOWN PROBLEMS                                                 *
      * PIR 2009/054 OPEN - BALANCES ARE AS AT THE RUN, NOT AS AT THE  *
      *   QUARTER END. IF THE JOB SLIPS PAST THE FIRST BUSINESS DAY    *
      *   THE RETURN INCLUDES NEW-QUARTER POSTINGS. THE PROGRAM SETS   *
      *   RC 4 WHEN RUN MORE THAN WS-MAX-LATE-DAYS AFTER QUARTER END.  *
      * PIR 2011/006 OPEN - A CUSTOMER NOT ON CIF IS REPORTED AS       *
      *   PERSONAL. REGULATORY REPORTING ADJUST THE SUBMISSION.        *
      * BUSINESS CASH ISA BALANCES (SHOULD NOT EXIST) ARE PUT IN       *
      * BUSINESS INSTANT ACCESS AND LISTED AS EXCEPTIONS.              *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1993-04-05 GHT  ORIGINAL VERSION (PRINT ONLY, KEYED BY HAND)   *
      * 1998-12-07 MLW  Y2K                                            *
      * 1999-04-12 GHT  CASH ISA LINE (ISA LAUNCH APRIL 1999)          *
      * 2006-07-03 SAT  RETURN FILE FOR ELECTRONIC SUBMISSION          *
      * 2009-01-26 AOS  RESIDUAL MATURITY BANDS FROM ACM-MATURITY-DATE *
      *                 (WAS ORIGINAL TERM FROM PRDPARM)               *
      * 2011-01-17 JNB  LOAN LINES FROM LOANMAST (LENDPRO RETIRED)     *
      * 2014-04-07 PDQ  QTREND= OVERRIDE, LATE RUN WARNING             *
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
           SELECT LOANMAST-FILE    ASSIGN TO LOANMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS LN-KEY
                                   FILE STATUS IS WS-LOANMAST-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
           SELECT BOERTN-FILE      ASSIGN TO BOERTN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-BOERTN-STATUS.
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
           05  PARM-KEYWORD                PIC X(07).
               88  PARM-QTR-END                VALUE 'QTREND='.
           05  PARM-VALUE                  PIC X(08).
           05  FILLER                      PIC X(65).
       FD  BOERTN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BOERTN-RECORD                   PIC X(100).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'RGBOE010'.
      *
       01  WS-CONSTANTS.
           05  WS-FORM-ID                  PIC X(04) VALUE 'BTQ1'.
           05  WS-REPORTER-CD              PIC X(07) VALUE 'MBK0471'.
           05  WS-HOME-CCY                 PIC X(03) VALUE 'GBP'.
           05  WS-INCLUDE-ACCRUED          PIC X(01) VALUE 'Y'.
           05  WS-MAX-LATE-DAYS            PIC S9(03) COMP-3 VALUE +4.
      *    RESIDUAL MATURITY BAND LIMITS IN DAYS
           05  WS-BAND-1-DAYS              PIC S9(05) COMP-3 VALUE +92.
           05  WS-BAND-2-DAYS              PIC S9(05) COMP-3 VALUE +365.
           05  WS-BAND-3-DAYS              PIC S9(05) COMP-3 VALUE +730.
           05  WS-BAND-4-DAYS              PIC S9(05) COMP-3
                                           VALUE +1826.
      *
       01  WS-FILE-STATUSES.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
           05  WS-LOANMAST-STATUS          PIC X(02).
               88  WS-LOANMAST-OK              VALUE '00'.
               88  WS-LOANMAST-EOF             VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-PARMIN-STATUS            PIC X(02).
               88  WS-PARMIN-OK                VALUE '00'.
           05  WS-BOERTN-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-ACCT-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-LOAN-EOF-IND             PIC X(01) VALUE 'N'.
               88  WS-END-OF-LOANS             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-DATE-INT             PIC S9(09) COMP.
           05  WS-QTR-END                  PIC 9(08).
           05  WS-QTR-END-R REDEFINES WS-QTR-END.
               10  WS-QE-CCYY              PIC 9(04).
               10  WS-QE-MMDD              PIC 9(04).
           05  WS-QTR-START                PIC 9(08).
           05  WS-QTR-END-INT              PIC S9(09) COMP.
           05  WS-MAT-INT                  PIC S9(09) COMP.
           05  WS-RESIDUAL-DAYS            PIC S9(07) COMP-3.
           05  WS-LATE-DAYS                PIC S9(07) COMP-3.
           05  WS-BUS-MM                   PIC 9(02).
           05  WS-SECTOR                   PIC X(01).
           05  WS-LAST-CIF-NO              PIC X(12) VALUE LOW-VALUES.
           05  WS-LAST-SECTOR              PIC X(01).
           05  WS-LINE-CD                  PIC X(04).
           05  WS-AMOUNT                   PIC S9(15)V99 COMP-3.
           05  WS-UNDRAWN                  PIC S9(15)V99 COMP-3.
           05  WS-AMOUNT-000               PIC S9(13) COMP-3.
           05  WS-EXC-TEXT                 PIC X(50).
           05  WS-I                        PIC S9(04) COMP.
           05  WS-LINE-IDX                 PIC S9(04) COMP.
      *
      *---- FORM LINE TABLE -------------------------------------------*
      *    CODE(4) SECTOR(1) DESCRIPTION(40)
       01  WS-FORM-LINE-VALUES.
           05  FILLER  PIC X(45) VALUE
               'D110PPERSONAL SIGHT - CURRENT ACCOUNTS'.
           05  FILLER  PIC X(45) VALUE
               'D120PPERSONAL SIGHT - INSTANT ACCESS SAVINGS'.
           05  FILLER  PIC X(45) VALUE
               'D130PPERSONAL CASH ISA'.
           05  FILLER  PIC X(45) VALUE
               'D141PPERSONAL TIME - RESIDUAL UP TO 3 MONTHS'.
           05  FILLER  PIC X(45) VALUE
               'D142PPERSONAL TIME - RESIDUAL 3M TO 1 YEAR'.
           05  FILLER  PIC X(45) VALUE
               'D143PPERSONAL TIME - RESIDUAL 1 TO 2 YEARS'.
           05  FILLER  PIC X(45) VALUE
               'D144PPERSONAL TIME - RESIDUAL 2 TO 5 YEARS'.
           05  FILLER  PIC X(45) VALUE
               'D145PPERSONAL TIME - RESIDUAL OVER 5 YEARS'.
           05  FILLER  PIC X(45) VALUE
               'D210BBUSINESS SIGHT - CURRENT ACCOUNTS'.
           05  FILLER  PIC X(45) VALUE
               'D220BBUSINESS SIGHT - INSTANT ACCESS SAVINGS'.
           05  FILLER  PIC X(45) VALUE
               'D241BBUSINESS TIME - RESIDUAL UP TO 3 MONTHS'.
           05  FILLER  PIC X(45) VALUE
               'D242BBUSINESS TIME - RESIDUAL 3M TO 1 YEAR'.
           05  FILLER  PIC X(45) VALUE
               'D243BBUSINESS TIME - RESIDUAL 1 TO 2 YEARS'.
           05  FILLER  PIC X(45) VALUE
               'D244BBUSINESS TIME - RESIDUAL 2 TO 5 YEARS'.
           05  FILLER  PIC X(45) VALUE
               'D245BBUSINESS TIME - RESIDUAL OVER 5 YEARS'.
           05  FILLER  PIC X(45) VALUE
               'A310PPERSONAL OVERDRAFTS'.
           05  FILLER  PIC X(45) VALUE
               'A320BBUSINESS OVERDRAFTS'.
           05  FILLER  PIC X(45) VALUE
               'A410PPERSONAL LOANS - PERFORMING'.
           05  FILLER  PIC X(45) VALUE
               'A420PPERSONAL LOANS - 90+ DAYS / DEFAULT'.
           05  FILLER  PIC X(45) VALUE
               'A430BBUSINESS LOANS'.
           05  FILLER  PIC X(45) VALUE
               'M910AMEMO - UNDRAWN ARRANGED OD LIMITS'.
           05  FILLER  PIC X(45) VALUE
               'M920AMEMO - LOANS WRITTEN OFF IN QUARTER'.
       01  WS-FORM-LINES REDEFINES WS-FORM-LINE-VALUES.
           05  WS-FL-ENTRY                 OCCURS 22 TIMES
                                           INDEXED BY FL-IDX.
               10  WS-FL-CODE              PIC X(04).
               10  WS-FL-SECTOR            PIC X(01).
               10  WS-FL-DESC              PIC X(40).
       01  WS-FORM-LINE-COUNT              PIC S9(04) COMP VALUE +22.
       01  WS-FORM-TOTALS.
           05  WS-FT-ENTRY                 OCCURS 22 TIMES.
               10  WS-FT-COUNT             PIC S9(09) COMP-3.
               10  WS-FT-AMOUNT            PIC S9(15)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-ACCTS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-CLOSED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-NON-GBP            PIC S9(09) COMP-3 VALUE 0.
           05  WS-NON-GBP-AMT              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-ACCTS-SHADOW             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACCTS-REPORTED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOANS-READ               PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOANS-REPORTED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOANS-SKIPPED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-CIF-NOT-FOUND            PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXCEPTIONS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-DEPOSITS           PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-LENDING            PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-EXACT              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-000                PIC S9(13) COMP-3 VALUE 0.
           05  WS-RETURN-RECORDS           PIC S9(05) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'RGBOE010'.
           05  FILLER                      PIC X(20) VALUE SPACES.
           05  FILLER                      PIC X(40)
               VALUE 'MEGABANK PLC - BOE QUARTERLY RETURN'.
           05  FILLER                      PIC X(05) VALUE 'QE '.
           05  HD1-QTR-END                 PIC X(10).
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(06) VALUE 'LINE'.
           05  FILLER            PIC X(04) VALUE 'SEC'.
           05  FILLER            PIC X(42) VALUE 'DESCRIPTION'.
           05  FILLER            PIC X(13) VALUE '     ACCOUNTS'.
           05  FILLER            PIC X(18) VALUE '      GBP 000S'.
           05  FILLER            PIC X(25)
                                 VALUE '          EXACT (GBP)'.
           05  FILLER            PIC X(24) VALUE SPACES.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-LINE-CD                  PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-SECTOR                   PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  DL-DESC                     PIC X(40).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-AMOUNT-000               PIC -ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  DL-AMOUNT-EXACT             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(28) VALUE SPACES.
       01  WS-EXCEPTION-LINE.
           05  EL-CC                       PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(05) VALUE 'EXC  '.
           05  EL-KEY                      PIC X(16).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-AMOUNT                   PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  EL-TEXT                     PIC X(50).
           05  FILLER                      PIC X(29) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY RGBOEREC.
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
           PERFORM 8000-READ-ACCOUNT
           PERFORM 2000-PROCESS-ACCOUNT THRU 2000-EXIT
               UNTIL WS-END-OF-ACCOUNTS
           PERFORM 8100-READ-LOAN
           PERFORM 3000-PROCESS-LOAN THRU 3000-EXIT
               UNTIL WS-END-OF-LOANS
           PERFORM 4000-WRITE-RETURN
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  ACCTMAST-FILE
                       LOANMAST-FILE
                       CIFMAST-FILE
                       PARMIN-FILE
                       DATECARD-FILE
                OUTPUT BOERTN-FILE
                       REPORT-FILE
           IF NOT WS-ACCTMAST-OK
           OR NOT WS-LOANMAST-OK
           OR NOT WS-CIFMAST-OK
           OR NOT WS-PARMIN-OK
           OR WS-BOERTN-STATUS NOT = '00'
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
           COMPUTE WS-BUS-DATE-INT =
                   FUNCTION INTEGER-OF-DATE (WS-BUS-DATE)
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING

      *    DEFAULT QUARTER END = THE LAST ONE BEFORE THE BUSINESS DATE.
           MOVE WS-BUS-DATE (5:2)          TO WS-BUS-MM
           MOVE WS-BUS-DATE (1:4)          TO WS-QE-CCYY
           EVALUATE TRUE
               WHEN WS-BUS-MM <= 03
                   SUBTRACT 1              FROM WS-QE-CCYY
                   MOVE 1231               TO WS-QE-MMDD
               WHEN WS-BUS-MM <= 06
                   MOVE 0331               TO WS-QE-MMDD
               WHEN WS-BUS-MM <= 09
                   MOVE 0630               TO WS-QE-MMDD
               WHEN OTHER
                   MOVE 0930               TO WS-QE-MMDD
           END-EVALUATE
           READ PARMIN-FILE
           PERFORM UNTIL NOT WS-PARMIN-OK
               IF PARM-QTR-END
                   IF PARM-VALUE NUMERIC
                   AND (PARM-VALUE (5:4) = '0331' OR '0630'
                                        OR '0930' OR '1231')
                       MOVE PARM-VALUE     TO WS-QTR-END
                       DISPLAY 'RGBOE010 - QUARTER END OVERRIDE '
                               WS-QTR-END
                   ELSE
                       MOVE '1000-INITIALISE' TO ABN-PARAGRAPH
                       MOVE 1005           TO ABN-ABEND-CODE
                       MOVE 'QTREND= MUST BE A QUARTER END CCYYMMDD'
                                           TO ABN-MESSAGE
                       MOVE PARM-VALUE     TO ABN-KEY-DATA
                       PERFORM Z999-ABEND
                   END-IF
               END-IF
               READ PARMIN-FILE
           END-PERFORM
           CLOSE PARMIN-FILE
           IF WS-QTR-END NOT < WS-BUS-DATE
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'QUARTER END IS NOT BEFORE THE BUSINESS DATE'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           COMPUTE WS-QTR-END-INT =
                   FUNCTION INTEGER-OF-DATE (WS-QTR-END)
      *    QUARTER START = DAY AFTER THE PREVIOUS QUARTER END, WHICH
      *    IS (QE - 3 MONTHS) + 1. DONE ON THE MMDD TO AVOID MONTH
      *    ARITHMETIC.
           EVALUATE WS-QE-MMDD
               WHEN 0331
                   COMPUTE WS-QTR-START = WS-QE-CCYY * 10000 + 0101
               WHEN 0630
                   COMPUTE WS-QTR-START = WS-QE-CCYY * 10000 + 0401
               WHEN 0930
                   COMPUTE WS-QTR-START = WS-QE-CCYY * 10000 + 0701
               WHEN OTHER
                   COMPUTE WS-QTR-START = WS-QE-CCYY * 10000 + 1001
           END-EVALUATE
           COMPUTE WS-LATE-DAYS = WS-BUS-DATE-INT - WS-QTR-END-INT
           STRING WS-QTR-END (1:4) '-' WS-QTR-END (5:2) '-'
                  WS-QTR-END (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-QTR-END
           END-STRING
           DISPLAY 'RGBOE010 - QUARTER ' WS-QTR-START ' TO '
                   WS-QTR-END ' RUN ' WS-LATE-DAYS ' DAYS AFTER'
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-FORM-LINE-COUNT
               MOVE ZERO                   TO WS-FT-COUNT (WS-I)
                                              WS-FT-AMOUNT (WS-I)
           END-PERFORM
           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       2000-PROCESS-ACCOUNT.
      *================================================================*
           ADD 1                           TO WS-ACCTS-READ
           EVALUATE TRUE
               WHEN ACM-CLOSED
                   ADD 1                   TO WS-ACCTS-CLOSED
                   GO TO 2000-NEXT
               WHEN ACM-LOAN OR ACM-MORTGAGE
                   ADD 1                   TO WS-ACCTS-SHADOW
                   GO TO 2000-NEXT
               WHEN ACM-CCY-CD NOT = WS-HOME-CCY
                   ADD 1                   TO WS-ACCTS-NON-GBP
                   ADD ACM-LEDGER-BAL      TO WS-NON-GBP-AMT
                   GO TO 2000-NEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           ADD 1                           TO WS-ACCTS-REPORTED
           MOVE ACM-CIF-NO                 TO CIF-NO
           PERFORM 5100-GET-SECTOR THRU 5100-EXIT

           IF ACM-LEDGER-BAL < ZERO
               PERFORM 2300-OVERDRAWN
           ELSE
               PERFORM 2100-DEPOSIT THRU 2100-EXIT
           END-IF

      *    MEMO - UNDRAWN ARRANGED LIMIT
           IF ACM-OD-LIMIT NUMERIC
           AND ACM-OD-LIMIT > ZERO
               IF ACM-LEDGER-BAL < ZERO
                   COMPUTE WS-UNDRAWN = ACM-OD-LIMIT + ACM-LEDGER-BAL
               ELSE
                   MOVE ACM-OD-LIMIT       TO WS-UNDRAWN
               END-IF
               IF WS-UNDRAWN > ZERO
                   MOVE 'M910'             TO WS-LINE-CD
                   MOVE WS-UNDRAWN         TO WS-AMOUNT
                   PERFORM 5000-ADD-TO-LINE
               END-IF
           END-IF
           .
       2000-NEXT.
           PERFORM 8000-READ-ACCOUNT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-DEPOSIT.
      *================================================================*
           MOVE ACM-LEDGER-BAL             TO WS-AMOUNT
           IF WS-INCLUDE-ACCRUED = 'Y'
           AND ACM-ACCRUED-INT-CR NUMERIC
               ADD ACM-ACCRUED-INT-CR      TO WS-AMOUNT
           END-IF
           EVALUATE TRUE
               WHEN ACM-CURRENT
                   IF WS-SECTOR = 'B'
                       MOVE 'D210'         TO WS-LINE-CD
                   ELSE
                       MOVE 'D110'         TO WS-LINE-CD
                   END-IF
               WHEN ACM-SAVINGS
                   PERFORM 2150-INSTANT-ACCESS
               WHEN ACM-ISA
                   IF WS-SECTOR = 'B'
                       MOVE 'D220'         TO WS-LINE-CD
                       MOVE 'BUSINESS CUSTOMER HOLDS A CASH ISA'
                                           TO WS-EXC-TEXT
                       PERFORM 7100-ACCOUNT-EXCEPTION
                   ELSE
                       MOVE 'D130'         TO WS-LINE-CD
                   END-IF
               WHEN ACM-FIXED-TERM
                   PERFORM 2200-FIXED-TERM
               WHEN OTHER
                   MOVE 'UNKNOWN ACCOUNT TYPE - REPORTED AS SAVINGS'
                                           TO WS-EXC-TEXT
                   PERFORM 7100-ACCOUNT-EXCEPTION
                   PERFORM 2150-INSTANT-ACCESS
           END-EVALUATE
           PERFORM 5000-ADD-TO-LINE
           ADD WS-AMOUNT                   TO WS-TOTAL-DEPOSITS
           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2150-INSTANT-ACCESS.
      *----------------------------------------------------------------*
           IF WS-SECTOR = 'B'
               MOVE 'D220'                 TO WS-LINE-CD
           ELSE
               MOVE 'D120'                 TO WS-LINE-CD
           END-IF
           .
      *================================================================*
       2200-FIXED-TERM.
      *================================================================*
      *    MATURED WITH NO INSTRUCTION - INSTANT ACCESS (INFTD010).
           IF ACM-MATURITY-DATE NOT NUMERIC
           OR ACM-MATURITY-DATE = ZERO
               PERFORM 2150-INSTANT-ACCESS
           ELSE
               COMPUTE WS-MAT-INT =
                       FUNCTION INTEGER-OF-DATE (ACM-MATURITY-DATE)
               COMPUTE WS-RESIDUAL-DAYS = WS-MAT-INT - WS-QTR-END-INT
               EVALUATE TRUE
                   WHEN WS-RESIDUAL-DAYS <= WS-BAND-1-DAYS
                       MOVE 'D141'         TO WS-LINE-CD
                   WHEN WS-RESIDUAL-DAYS <= WS-BAND-2-DAYS
                       MOVE 'D142'         TO WS-LINE-CD
                   WHEN WS-RESIDUAL-DAYS <= WS-BAND-3-DAYS
                       MOVE 'D143'         TO WS-LINE-CD
                   WHEN WS-RESIDUAL-DAYS <= WS-BAND-4-DAYS
                       MOVE 'D144'         TO WS-LINE-CD
                   WHEN OTHER
                       MOVE 'D145'         TO WS-LINE-CD
               END-EVALUATE
               IF WS-SECTOR = 'B'
                   MOVE '2'                TO WS-LINE-CD (2:1)
               END-IF
           END-IF
           .
      *================================================================*
       2300-OVERDRAWN.
      *================================================================*
           COMPUTE WS-AMOUNT = ZERO - ACM-LEDGER-BAL
           IF WS-INCLUDE-ACCRUED = 'Y'
           AND ACM-ACCRUED-INT-DR NUMERIC
               ADD ACM-ACCRUED-INT-DR      TO WS-AMOUNT
           END-IF
           IF NOT ACM-CURRENT
               MOVE 'DEBIT BALANCE ON NON-CURRENT ACCOUNT'
                                           TO WS-EXC-TEXT
               PERFORM 7100-ACCOUNT-EXCEPTION
           END-IF
           IF WS-SECTOR = 'B'
               MOVE 'A320'                 TO WS-LINE-CD
           ELSE
               MOVE 'A310'                 TO WS-LINE-CD
           END-IF
           PERFORM 5000-ADD-TO-LINE
           ADD WS-AMOUNT                   TO WS-TOTAL-LENDING
           .
      *================================================================*
       3000-PROCESS-LOAN.
      *================================================================*
           ADD 1                           TO WS-LOANS-READ
      *    WRITTEN OFF IN THE QUARTER - MEMORANDUM ONLY.
           IF LN-WRITTEN-OFF
               IF LN-WRITE-OFF-DATE NUMERIC
               AND LN-WRITE-OFF-DATE NOT < WS-QTR-START
               AND LN-WRITE-OFF-DATE NOT > WS-QTR-END
                   MOVE 'M920'             TO WS-LINE-CD
                   MOVE LN-WRITE-OFF-AMT   TO WS-AMOUNT
                   PERFORM 5000-ADD-TO-LINE
               ELSE
                   ADD 1                   TO WS-LOANS-SKIPPED
               END-IF
               GO TO 3000-NEXT
           END-IF
           IF NOT LN-OPEN-STATUS
               ADD 1                       TO WS-LOANS-SKIPPED
               GO TO 3000-NEXT
           END-IF
      *    DRAWN AFTER THE QUARTER END - NOT ON THE BALANCE SHEET YET.
           IF LN-ADVANCE-DATE NUMERIC
           AND LN-ADVANCE-DATE > WS-QTR-END
               ADD 1                       TO WS-LOANS-SKIPPED
               GO TO 3000-NEXT
           END-IF
           ADD 1                           TO WS-LOANS-REPORTED
           MOVE LN-CIF-NO                  TO CIF-NO
           PERFORM 5100-GET-SECTOR THRU 5100-EXIT
           MOVE LN-PRINCIPAL-BAL           TO WS-AMOUNT
           IF LN-ACCRUED-INT NUMERIC
               ADD LN-ACCRUED-INT          TO WS-AMOUNT
           END-IF
           IF LN-FEES-OUTSTANDING NUMERIC
               ADD LN-FEES-OUTSTANDING     TO WS-AMOUNT
           END-IF
           EVALUATE TRUE
               WHEN WS-SECTOR = 'B'
                   MOVE 'A430'             TO WS-LINE-CD
                   MOVE 'BUSINESS CUSTOMER ON PERSONAL LOAN BOOK'
                                           TO WS-EXC-TEXT
                   PERFORM 7200-LOAN-EXCEPTION
               WHEN LN-DEFAULTED OR LN-COLLECTIONS
                   MOVE 'A420'             TO WS-LINE-CD
               WHEN LN-ARREARS-BUCKET NUMERIC
               AND  LN-ARREARS-BUCKET >= 4
                   MOVE 'A420'             TO WS-LINE-CD
               WHEN OTHER
                   MOVE 'A410'             TO WS-LINE-CD
           END-EVALUATE
           PERFORM 5000-ADD-TO-LINE
           ADD WS-AMOUNT                   TO WS-TOTAL-LENDING
           .
       3000-NEXT.
           PERFORM 8100-READ-LOAN
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-WRITE-RETURN.
      *================================================================*
           MOVE SPACES                     TO RBD-HEADER-RECORD
           MOVE 'H'                        TO RBH-REC-TYPE
           MOVE WS-FORM-ID                 TO RBH-FORM-ID
           MOVE WS-REPORTER-CD             TO RBH-REPORTER-CD
           MOVE WS-QTR-END                 TO RBH-PERIOD-END
           MOVE WS-BUS-DATE                TO RBH-RUN-DATE
           MOVE WS-HOME-CCY                TO RBH-CCY-CD
           MOVE '000S'                     TO RBH-UNITS
           PERFORM 4900-WRITE-BOERTN
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-FORM-LINE-COUNT
      *        NIL LINES ARE STILL SUBMITTED (BOE RULE - A MISSING
      *        LINE IS QUERIED, A ZERO LINE IS NOT).
               COMPUTE WS-AMOUNT-000 ROUNDED =
                       WS-FT-AMOUNT (WS-I) / 1000
               MOVE SPACES                 TO RBD-DETAIL-RECORD
               MOVE 'D'                    TO RBD-REC-TYPE
               MOVE WS-FORM-ID             TO RBD-FORM-ID
               MOVE WS-FL-CODE (WS-I)      TO RBD-LINE-CD
               MOVE WS-FL-SECTOR (WS-I)    TO RBD-SECTOR-CD
               MOVE WS-FL-DESC (WS-I)      TO RBD-LINE-DESC
               MOVE WS-FT-COUNT (WS-I)     TO RBD-ACCOUNT-COUNT
               MOVE WS-AMOUNT-000          TO RBD-AMOUNT-000
               MOVE WS-FT-AMOUNT (WS-I)    TO RBD-AMOUNT-EXACT
               PERFORM 4900-WRITE-BOERTN
               ADD WS-AMOUNT-000           TO WS-TOTAL-000
               ADD WS-FT-AMOUNT (WS-I)     TO WS-TOTAL-EXACT
               PERFORM 7300-REPORT-LINE
           END-PERFORM
           MOVE SPACES                     TO RBD-TRAILER-RECORD
           MOVE 'T'                        TO RBT-REC-TYPE
           MOVE WS-FORM-ID                 TO RBT-FORM-ID
           MOVE WS-FORM-LINE-COUNT         TO RBT-LINE-COUNT
           MOVE WS-TOTAL-000               TO RBT-TOTAL-000
           MOVE WS-TOTAL-EXACT             TO RBT-TOTAL-EXACT
           PERFORM 4900-WRITE-BOERTN
           .
      *----------------------------------------------------------------*
       4900-WRITE-BOERTN.
      *----------------------------------------------------------------*
           WRITE BOERTN-RECORD             FROM RBD-DETAIL-RECORD
           IF WS-BOERTN-STATUS NOT = '00'
               MOVE '4900-WRITE-BOERTN'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'BOERTN WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-BOERTN-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-RETURN-RECORDS
           .
      *================================================================*
       5000-ADD-TO-LINE.
      *================================================================*
           SET FL-IDX                      TO 1
           SEARCH WS-FL-ENTRY
               AT END
                   MOVE '5000-ADD-TO-LINE' TO ABN-PARAGRAPH
                   MOVE 1004               TO ABN-ABEND-CODE
                   MOVE 'FORM LINE NOT IN TABLE' TO ABN-MESSAGE
                   MOVE WS-LINE-CD         TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               WHEN WS-FL-CODE (FL-IDX) = WS-LINE-CD
                   SET WS-LINE-IDX         TO FL-IDX
           END-SEARCH
           ADD 1                       TO WS-FT-COUNT (WS-LINE-IDX)
           ADD WS-AMOUNT               TO WS-FT-AMOUNT (WS-LINE-IDX)
           .
      *================================================================*
       5100-GET-SECTOR.
      *================================================================*
      *    CALLER HAS MOVED THE CUSTOMER NUMBER TO CIF-NO.
           IF CIF-NO = WS-LAST-CIF-NO
               MOVE WS-LAST-SECTOR         TO WS-SECTOR
               GO TO 5100-EXIT
           END-IF
           MOVE CIF-NO                     TO WS-LAST-CIF-NO
           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   IF CIF-BUSINESS
                       MOVE 'B'            TO WS-SECTOR
                   ELSE
                       MOVE 'P'            TO WS-SECTOR
                   END-IF
               WHEN WS-CIFMAST-NOTFND
      *            PIR 2011/006 - DEFAULT TO PERSONAL.
                   MOVE 'P'                TO WS-SECTOR
                   ADD 1                   TO WS-CIF-NOT-FOUND
                   DISPLAY 'RGBOE010 - CIF NOT FOUND ' WS-LAST-CIF-NO
               WHEN OTHER
                   MOVE '5100-GET-SECTOR'  TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE WS-LAST-CIF-NO     TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE
           MOVE WS-SECTOR                  TO WS-LAST-SECTOR
           .
       5100-EXIT.
           EXIT.
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
       7100-ACCOUNT-EXCEPTION.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SPACES                     TO EL-KEY
           STRING ACM-SORT-CODE '-' ACM-ACCT-NO
                  DELIMITED BY SIZE
                  INTO EL-KEY
           END-STRING
           MOVE ACM-CIF-NO                 TO EL-CIF-NO
           MOVE ACM-LEDGER-BAL             TO EL-AMOUNT
           MOVE WS-EXC-TEXT                TO EL-TEXT
           WRITE REPORT-RECORD             FROM WS-EXCEPTION-LINE
           ADD 1                           TO WS-LINE-COUNT
                                              WS-EXCEPTIONS
           .
      *================================================================*
       7200-LOAN-EXCEPTION.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE SPACES                     TO EL-KEY
           STRING 'LOAN ' LN-LOAN-NO
                  DELIMITED BY SIZE
                  INTO EL-KEY
           END-STRING
           MOVE LN-CIF-NO                  TO EL-CIF-NO
           MOVE WS-AMOUNT                  TO EL-AMOUNT
           MOVE WS-EXC-TEXT                TO EL-TEXT
           WRITE REPORT-RECORD             FROM WS-EXCEPTION-LINE
           ADD 1                           TO WS-LINE-COUNT
                                              WS-EXCEPTIONS
           .
      *================================================================*
       7300-REPORT-LINE.
      *================================================================*
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE WS-FL-CODE (WS-I)          TO DL-LINE-CD
           MOVE WS-FL-SECTOR (WS-I)        TO DL-SECTOR
           MOVE WS-FL-DESC (WS-I)          TO DL-DESC
           MOVE WS-FT-COUNT (WS-I)         TO DL-COUNT
           MOVE WS-AMOUNT-000              TO DL-AMOUNT-000
           MOVE WS-FT-AMOUNT (WS-I)        TO DL-AMOUNT-EXACT
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       8000-READ-ACCOUNT.
      *================================================================*
           READ ACCTMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-ACCTMAST-OK
                   CONTINUE
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
       8100-READ-LOAN.
      *================================================================*
           READ LOANMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-LOANMAST-OK
                   CONTINUE
               WHEN WS-LOANMAST-EOF
                   SET WS-END-OF-LOANS     TO TRUE
               WHEN OTHER
                   MOVE '8100-READ-LOAN'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'LOANMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-LOANMAST-STATUS TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'DDA ACCOUNTS READ'        TO TL-LABEL
           MOVE WS-ACCTS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  REPORTED'               TO TL-LABEL
           MOVE WS-ACCTS-REPORTED          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  EXCLUDED - CLOSED'      TO TL-LABEL
           MOVE WS-ACCTS-CLOSED            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  EXCLUDED - LOAN/MORTGAGE SHADOW' TO TL-LABEL
           MOVE WS-ACCTS-SHADOW            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  EXCLUDED - NON-STERLING (FOR BT-FX)' TO TL-LABEL
           MOVE WS-ACCTS-NON-GBP           TO TL-COUNT
           MOVE WS-NON-GBP-AMT             TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'LOANS READ'               TO TL-LABEL
           MOVE WS-LOANS-READ              TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  REPORTED'               TO TL-LABEL
           MOVE WS-LOANS-REPORTED          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  SKIPPED (SETTLED/OLD WRITE-OFF/NEW)' TO TL-LABEL
           MOVE WS-LOANS-SKIPPED           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TOTAL STERLING DEPOSITS'  TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-TOTAL-DEPOSITS          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'TOTAL STERLING LENDING'   TO TL-LABEL
           MOVE WS-TOTAL-LENDING           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CONTROL - SUM OF LINES (EXACT)' TO TL-LABEL
           MOVE WS-TOTAL-EXACT             TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CONTROL - SUM OF LINES (GBP 000)' TO TL-LABEL
           MOVE WS-TOTAL-000               TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCEPTION - CUSTOMER NOT ON CIF' TO TL-LABEL
           MOVE WS-CIF-NOT-FOUND           TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'EXCEPTIONS LISTED'        TO TL-LABEL
           MOVE WS-EXCEPTIONS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RETURN RECORDS WRITTEN'   TO TL-LABEL
           MOVE WS-RETURN-RECORDS          TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           IF WS-LATE-DAYS > WS-MAX-LATE-DAYS
               MOVE '*** RUN LATE - NOT QUARTER END BALANCES'
                                           TO TL-LABEL
               MOVE WS-LATE-DAYS           TO TL-COUNT
               WRITE REPORT-RECORD         FROM WS-TOTAL-LINE
           END-IF
           CLOSE ACCTMAST-FILE LOANMAST-FILE CIFMAST-FILE BOERTN-FILE
                 REPORT-FILE
           DISPLAY 'RGBOE010 - DEPOSITS ' WS-TOTAL-DEPOSITS
           DISPLAY 'RGBOE010 - LENDING  ' WS-TOTAL-LENDING
           IF WS-CIF-NOT-FOUND > ZERO
           OR WS-EXCEPTIONS > ZERO
           OR WS-LATE-DAYS > WS-MAX-LATE-DAYS
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *
           COPY ABNDPROC.
