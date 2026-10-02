       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ISASUB10.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-04-06.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : ISASUB10                                            *
      * TITLE    : CASH ISA - SUBSCRIPTION MONITORING AND HMRC RETURN  *
      * TYPE     : BATCH COBOL / DB2 - RUN UNDER IKJEFT01 (DSN RUN)    *
      * PLAN     : ISASUB10                                            *
      * JCL      : ISASUB01 (DAILY AFTER PSEOD01)                      *
      *            ISAYRE01 (ONCE, ON THE FIRST BUSINESS DAY AFTER     *
      *                      5 APRIL - MODE=YEAREND)                   *
      *----------------------------------------------------------------*
      * INPUT    : PARMIN   - 'MODE=DAILY' OR 'MODE=YEAREND'           *
      *                       'TAXYEAR=CCYY/CCYY' (YEAREND ONLY)       *
      *            MBK.TXNHIST - TODAY'S POSTED CREDITS                *
      *            PRDPARM  - PRODUCT PARAMETERS (SUBSCRIPTION LIMIT)  *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : ACCTMAST - DDA ACCOUNT MASTER (DYNAMIC)             *
      * OUTPUT   : BREACH   - SUBSCRIPTION LIMIT BREACHES (FB 150)     *
      *            ISARTN   - HMRC ANNUAL ISA RETURN (YEAREND, FB 200) *
      *            REPORT   - CONTROL REPORT (FBA 133)                 *
      *----------------------------------------------------------------*
      * DAILY: EVERY CUSTOMER CREDIT POSTED TODAY TO AN ISA ACCOUNT    *
      * COUNTS AS A SUBSCRIPTION EXCEPT:                               *
      *   - INTEREST ('INT ') AND REVERSALS ('REV ')                   *
      *   - TRANSFERS IN OF PREVIOUS YEARS' SUBSCRIPTIONS FROM ANOTHER *
      *     ISA MANAGER (EXT_REF STARTS 'ISAT')                        *
      * ACM-ISA-SUBS-YTD IS INCREASED. IF IT GOES OVER THE PRODUCT     *
      * LIMIT (PRD-ISA-SUBS-LIMIT - SET EACH YEAR FROM THE HMRC        *
      * ANNOUNCEMENT) THE ACCOUNT IS RESTRICTED TO NO CREDITS AND THE  *
      * BREACH IS PASSED TO SAVINGS OPERATIONS, WHO RETURN THE EXCESS  *
      * AND REPORT IT TO HMRC (ISA REGULATIONS 1998 REG 4).            *
      * WITHDRAWALS DO NOT REDUCE THE SUBSCRIPTION TOTAL. FLEXIBLE ISA *
      * (APRIL 2016) IS NOT SUPPORTED - PIR 2016/044 OPEN.             *
      *                                                                *
      * YEAREND: EVERY ISA ACCOUNT (OPEN OR CLOSED IN THE YEAR) GETS   *
      * AN ANNUAL RETURN RECORD - SUBSCRIPTIONS IN THE YEAR AND THE    *
      * VALUE AT 5 APRIL (LEDGER BALANCE - THE RUN IS ON THE FIRST     *
      * BUSINESS DAY AFTER, BEFORE ANY POSTINGS OF THE NEW YEAR ARE    *
      * APPLIED). ACM-ISA-SUBS-YTD IS THEN RESET AND NO-CREDIT         *
      * RESTRICTIONS PLACED FOR A BREACH ARE LIFTED.                   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-04-06 AOS  ORIGINAL VERSION (2010/11 TAX YEAR)            *
      * 2011-04-04 AOS  TRANSFERS IN EXCLUDED (EXT_REF 'ISAT')         *
      * 2014-07-01 PDQ  NISA - SINGLE 15,000 LIMIT FROM PRDPARM        *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMIN-FILE      ASSIGN TO PARMIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-PARMIN-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS DYNAMIC
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT PRDPARM-FILE     ASSIGN TO PRDPARM
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS PRD-KEY
                                   FILE STATUS IS WS-PRDPARM-STATUS.
           SELECT BREACH-FILE      ASSIGN TO BREACH
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-BREACH-STATUS.
           SELECT ISARTN-FILE      ASSIGN TO ISARTN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ISARTN-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PARMIN-FILE
           RECORDING MODE IS F.
       01  PARMIN-RECORD.
           05  PARM-KEYWORD                PIC X(08).
               88  PARM-MODE                   VALUE 'MODE=   '.
               88  PARM-TAX-YEAR               VALUE 'TAXYEAR='.
           05  PARM-VALUE                  PIC X(09).
           05  FILLER                      PIC X(63).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  PRDPARM-FILE.
           COPY PRDPARRC.
       FD  BREACH-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BREACH-RECORD                   PIC X(150).
       FD  ISARTN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ISARTN-RECORD                   PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'ISASUB10'.
      *
       01  WS-CONSTANTS.
           05  WS-DEFAULT-LIMIT            PIC S9(13)V99 COMP-3
                                           VALUE +20000.00.
           05  WS-MANAGER-REF              PIC X(08) VALUE 'Z1234MBK'.
      *
       01  WS-FILE-STATUSES.
           05  WS-PARMIN-STATUS            PIC X(02).
               88  WS-PARMIN-OK                VALUE '00'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-EOF             VALUE '10'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-PRDPARM-STATUS           PIC X(02).
               88  WS-PRDPARM-OK               VALUE '00'.
           05  WS-BREACH-STATUS            PIC X(02).
           05  WS-ISARTN-STATUS            PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-MODE                     PIC X(07) VALUE SPACES.
               88  WS-MODE-DAILY               VALUE 'DAILY  '.
               88  WS-MODE-YEAREND             VALUE 'YEAREND'.
           05  WS-EOC-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-CURSOR            VALUE 'Y'.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-ACCOUNTS          VALUE 'Y'.
           05  WS-ACCT-OK-IND              PIC X(01).
               88  WS-ACCT-IS-ISA              VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-DATE-ISO                 PIC X(10).
           05  WS-TAX-YEAR                 PIC X(09) VALUE SPACES.
           05  WS-TAX-YEAR-START           PIC 9(08).
           05  WS-CURR-ACCT                PIC X(14) VALUE LOW-VALUES.
           05  WS-LIMIT                    PIC S9(13)V99 COMP-3.
           05  WS-DAY-SUBS                 PIC S9(13)V99 COMP-3.
           05  WS-EXCESS                   PIC S9(13)V99 COMP-3.
           05  WS-CCYY                     PIC 9(04).
      *
       01  WS-BREACH.
           05  BR-DETECTED-DATE            PIC 9(08).
           05  BR-SORT-CODE                PIC X(06).
           05  BR-ACCT-NO                  PIC X(08).
           05  BR-CIF-NO                   PIC X(12).
           05  BR-PROD-CD                  PIC X(10).
           05  BR-LIMIT                    PIC 9(11)V99.
           05  BR-SUBS-YTD                 PIC 9(11)V99.
           05  BR-EXCESS                   PIC 9(11)V99.
           05  BR-LAST-CREDIT-REF          PIC X(24).
           05  FILLER                      PIC X(43).
      *
       01  WS-ISA-RETURN.
           05  IR-REC-TYPE                 PIC X(01).
           05  IR-MANAGER-REF              PIC X(08).
           05  IR-TAX-YEAR                 PIC X(09).
           05  IR-SORT-CODE                PIC X(06).
           05  IR-ACCT-NO                  PIC X(08).
           05  IR-CIF-NO                   PIC X(12).
           05  IR-ISA-TYPE                 PIC X(01).
           05  IR-SUBSCRIPTIONS            PIC 9(11)V99.
           05  IR-MARKET-VALUE             PIC 9(11)V99.
           05  IR-STATUS                   PIC X(01).
           05  IR-OPEN-DATE                PIC 9(08).
           05  IR-CLOSE-DATE               PIC 9(08).
           05  IR-BREACH-IND               PIC X(01).
           05  IR-ACCOUNT-COUNT            PIC 9(09).
           05  FILLER                      PIC X(94).
      *
       01  WS-COUNTERS.
           05  WS-CREDITS-READ             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ISA-CREDITS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-TRANSFERS-IN             PIC S9(09) COMP-3 VALUE 0.
           05  WS-BREACHES                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-ISA-ACCOUNTS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETURNS                  PIC S9(09) COMP-3 VALUE 0.
           05  WS-RESTRICT-LIFTED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-SUBS               PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-VALUE              PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'ISASUB10'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(45)
               VALUE 'MEGABANK PLC - CASH ISA SUBSCRIPTIONS'.
           05  HD1-MODE                    PIC X(10).
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-BREACH-LINE.
           05  BL-CC                       PIC X(01) VALUE ' '.
           05  BL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  BL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  BL-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  BL-TEXT                     PIC X(20)
                                           VALUE 'LIMIT BREACH  YTD:'.
           05  BL-SUBS                     PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(10)
                                           VALUE '  EXCESS:'.
           05  BL-EXCESS                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(43) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
      *
           COPY DATECARD.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
       01  HV-POST-DATE                    PIC X(10).
      *
           EXEC SQL DECLARE ISACSR CURSOR FOR
               SELECT SORT_CODE, ACCT_NO, TXN_CODE, TXN_AMT, EXT_REF
                 FROM MBK.TXNHIST
                WHERE POST_DATE = :HV-POST-DATE
                  AND DR_CR_IND = 'C'
                  AND TXN_CODE NOT IN ('INT ', 'REV ')
                ORDER BY SORT_CODE, ACCT_NO, POST_SEQ
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE
           IF WS-MODE-DAILY
               PERFORM 2000-DAILY
           ELSE
               PERFORM 4000-YEAR-END
           END-IF
           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  PARMIN-FILE
                       PRDPARM-FILE
                       DATECARD-FILE
                I-O    ACCTMAST-FILE
                OUTPUT BREACH-FILE
                       ISARTN-FILE
                       REPORT-FILE
           IF NOT WS-PARMIN-OK
           OR NOT WS-ACCTMAST-OK
           OR NOT WS-PRDPARM-OK
           OR WS-BREACH-STATUS NOT = '00'
           OR WS-ISARTN-STATUS NOT = '00'
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
           MOVE HD1-BUS-DATE               TO WS-DATE-ISO

           READ PARMIN-FILE
           PERFORM UNTIL NOT WS-PARMIN-OK
               EVALUATE TRUE
                   WHEN PARM-MODE
                       MOVE PARM-VALUE (1:7) TO WS-MODE
                   WHEN PARM-TAX-YEAR
                       MOVE PARM-VALUE     TO WS-TAX-YEAR
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               READ PARMIN-FILE
           END-PERFORM
           CLOSE PARMIN-FILE
           IF NOT (WS-MODE-DAILY OR WS-MODE-YEAREND)
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'MODE=DAILY OR MODE=YEAREND CARD REQUIRED'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF WS-MODE-YEAREND
               IF WS-TAX-YEAR (5:1) NOT = '/'
               OR WS-TAX-YEAR (1:4) NOT NUMERIC
                   MOVE '1000-INITIALISE'  TO ABN-PARAGRAPH
                   MOVE 1005               TO ABN-ABEND-CODE
                   MOVE 'YEAREND NEEDS TAXYEAR=CCYY/CCYY'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               END-IF
      *        SAFETY: THE BUSINESS DATE MUST BE IN APRIL OF THE
      *        SECOND YEAR - A YEAREND RUN MID-YEAR WOULD WIPE THE
      *        SUBSCRIPTION TOTALS.
               IF WS-BUS-DATE (1:4) NOT = WS-TAX-YEAR (6:4)
               OR WS-BUS-DATE (5:2) NOT = '04'
                   MOVE '1000-INITIALISE'  TO ABN-PARAGRAPH
                   MOVE 1005               TO ABN-ABEND-CODE
                   MOVE 'YEAREND RUN OUTSIDE APRIL OF YEAR END'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               END-IF
           END-IF
           MOVE WS-MODE                    TO HD1-MODE
           DISPLAY 'ISASUB10 - MODE ' WS-MODE ' ' WS-TAX-YEAR
           .
      *================================================================*
       2000-DAILY.
      *================================================================*
           MOVE WS-DATE-ISO                TO HV-POST-DATE
           EXEC SQL OPEN ISACSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2000-DAILY'           TO ABN-PARAGRAPH
               PERFORM 9100-SQL-ERROR
           END-IF
           PERFORM UNTIL WS-END-OF-CURSOR
               EXEC SQL
                   FETCH ISACSR
                    INTO :TXH-SORT-CODE, :TXH-ACCT-NO, :TXH-TXN-CODE,
                         :TXH-TXN-AMT, :TXH-EXT-REF
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-CREDITS-READ
                       IF TXH-SORT-CODE NOT = WS-CURR-ACCT (1:6)
                       OR TXH-ACCT-NO NOT = WS-CURR-ACCT (7:8)
                           PERFORM 2900-END-ACCOUNT THRU 2900-EXIT
                           PERFORM 2050-START-ACCOUNT
                       END-IF
                       IF WS-ACCT-IS-ISA
                           PERFORM 2100-SUBSCRIPTION
                       END-IF
                   WHEN +100
                       SET WS-END-OF-CURSOR TO TRUE
                   WHEN OTHER
                       MOVE '2000-DAILY'   TO ABN-PARAGRAPH
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
           PERFORM 2900-END-ACCOUNT THRU 2900-EXIT
           EXEC SQL CLOSE ISACSR END-EXEC
           .
      *================================================================*
       2050-START-ACCOUNT.
      *================================================================*
           MOVE TXH-SORT-CODE              TO WS-CURR-ACCT (1:6)
           MOVE TXH-ACCT-NO                TO WS-CURR-ACCT (7:8)
           MOVE 'N'                        TO WS-ACCT-OK-IND
           MOVE ZERO                       TO WS-DAY-SUBS
           MOVE TXH-SORT-CODE              TO ACM-SORT-CODE
           MOVE TXH-ACCT-NO                TO ACM-ACCT-NO
           READ ACCTMAST-FILE
           IF WS-ACCTMAST-OK AND ACM-ISA
               SET WS-ACCT-IS-ISA          TO TRUE
               MOVE WS-DEFAULT-LIMIT       TO WS-LIMIT
               MOVE ACM-PROD-CD            TO PRD-PROD-CD
               READ PRDPARM-FILE
               IF WS-PRDPARM-OK
               AND PRD-ISA-SUBS-LIMIT NUMERIC
               AND PRD-ISA-SUBS-LIMIT > ZERO
                   MOVE PRD-ISA-SUBS-LIMIT TO WS-LIMIT
               END-IF
               IF ACM-ISA-SUBS-YTD NOT NUMERIC
                   MOVE ZERO               TO ACM-ISA-SUBS-YTD
               END-IF
           END-IF
           .
      *================================================================*
       2100-SUBSCRIPTION.
      *================================================================*
           IF TXH-EXT-REF (1:4) = 'ISAT'
               ADD 1                       TO WS-TRANSFERS-IN
           ELSE
               ADD 1                       TO WS-ISA-CREDITS
               ADD TXH-TXN-AMT             TO WS-DAY-SUBS
                                              WS-TOTAL-SUBS
           END-IF
           .
      *================================================================*
       2900-END-ACCOUNT.
      *================================================================*
           IF NOT WS-ACCT-IS-ISA
           OR WS-DAY-SUBS = ZERO
               GO TO 2900-EXIT
           END-IF
           ADD WS-DAY-SUBS                 TO ACM-ISA-SUBS-YTD
           IF ACM-ISA-SUBS-YTD > WS-LIMIT
               COMPUTE WS-EXCESS = ACM-ISA-SUBS-YTD - WS-LIMIT
               SET ACM-NO-CREDITS          TO TRUE
               PERFORM 2950-WRITE-BREACH
           END-IF
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER
           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '2900-END-ACCOUNT'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               MOVE ACM-KEY                TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           .
       2900-EXIT.
           EXIT.
      *================================================================*
       2950-WRITE-BREACH.
      *================================================================*
           MOVE SPACES                     TO WS-BREACH
           MOVE WS-BUS-DATE                TO BR-DETECTED-DATE
           MOVE ACM-SORT-CODE              TO BR-SORT-CODE
           MOVE ACM-ACCT-NO                TO BR-ACCT-NO
           MOVE ACM-CIF-NO                 TO BR-CIF-NO
           MOVE ACM-PROD-CD                TO BR-PROD-CD
           MOVE WS-LIMIT                   TO BR-LIMIT
           MOVE ACM-ISA-SUBS-YTD           TO BR-SUBS-YTD
           MOVE WS-EXCESS                  TO BR-EXCESS
           MOVE TXH-EXT-REF                TO BR-LAST-CREDIT-REF
           WRITE BREACH-RECORD             FROM WS-BREACH
           ADD 1                           TO WS-BREACHES
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE ACM-SORT-CODE              TO BL-SORT-CODE
           MOVE ACM-ACCT-NO                TO BL-ACCT-NO
           MOVE ACM-CIF-NO                 TO BL-CIF-NO
           MOVE ACM-ISA-SUBS-YTD           TO BL-SUBS
           MOVE WS-EXCESS                  TO BL-EXCESS
           WRITE REPORT-RECORD             FROM WS-BREACH-LINE
           ADD 1                           TO WS-LINE-COUNT
           .
      *================================================================*
       4000-YEAR-END.
      *================================================================*
           MOVE WS-TAX-YEAR (1:4)          TO WS-CCYY
           COMPUTE WS-TAX-YEAR-START = WS-CCYY * 10000 + 0406
           MOVE SPACES                     TO WS-ISA-RETURN
           MOVE 'H'                        TO IR-REC-TYPE
           MOVE WS-MANAGER-REF             TO IR-MANAGER-REF
           MOVE WS-TAX-YEAR                TO IR-TAX-YEAR
           WRITE ISARTN-RECORD             FROM WS-ISA-RETURN

           MOVE LOW-VALUES                 TO ACM-KEY
           START ACCTMAST-FILE KEY NOT < ACM-KEY
           IF NOT WS-ACCTMAST-OK
               SET WS-END-OF-ACCOUNTS      TO TRUE
           END-IF
           PERFORM UNTIL WS-END-OF-ACCOUNTS
               READ ACCTMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN WS-ACCTMAST-EOF
                       SET WS-END-OF-ACCOUNTS TO TRUE
                   WHEN NOT WS-ACCTMAST-OK
                       MOVE '4000-YEAR-END' TO ABN-PARAGRAPH
                       MOVE 1002           TO ABN-ABEND-CODE
                       MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                       MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                       PERFORM Z999-ABEND
                   WHEN ACM-ISA
                       PERFORM 4100-ISA-ACCOUNT THRU 4100-EXIT
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM

           MOVE SPACES                     TO WS-ISA-RETURN
           MOVE 'T'                        TO IR-REC-TYPE
           MOVE WS-MANAGER-REF             TO IR-MANAGER-REF
           MOVE WS-TAX-YEAR                TO IR-TAX-YEAR
           MOVE WS-TOTAL-SUBS              TO IR-SUBSCRIPTIONS
           MOVE WS-TOTAL-VALUE             TO IR-MARKET-VALUE
           MOVE WS-RETURNS                 TO IR-ACCOUNT-COUNT
           WRITE ISARTN-RECORD             FROM WS-ISA-RETURN
           .
      *================================================================*
       4100-ISA-ACCOUNT.
      *================================================================*
           ADD 1                           TO WS-ISA-ACCOUNTS
      *    CLOSED BEFORE THE YEAR STARTED - NOT REPORTABLE.
           IF ACM-CLOSED
           AND ACM-CLOSE-DATE < WS-TAX-YEAR-START
               GO TO 4100-EXIT
           END-IF
           IF ACM-ISA-SUBS-YTD NOT NUMERIC
               MOVE ZERO                   TO ACM-ISA-SUBS-YTD
           END-IF

           MOVE SPACES                     TO WS-ISA-RETURN
           MOVE 'D'                        TO IR-REC-TYPE
           MOVE WS-MANAGER-REF             TO IR-MANAGER-REF
           MOVE WS-TAX-YEAR                TO IR-TAX-YEAR
           MOVE ACM-SORT-CODE              TO IR-SORT-CODE
           MOVE ACM-ACCT-NO                TO IR-ACCT-NO
           MOVE ACM-CIF-NO                 TO IR-CIF-NO
           MOVE 'C'                        TO IR-ISA-TYPE
           MOVE ACM-ISA-SUBS-YTD           TO IR-SUBSCRIPTIONS
           IF ACM-LEDGER-BAL > ZERO
               MOVE ACM-LEDGER-BAL         TO IR-MARKET-VALUE
           ELSE
               MOVE ZERO                   TO IR-MARKET-VALUE
           END-IF
           MOVE ACM-ACCT-STATUS            TO IR-STATUS
           MOVE ACM-OPEN-DATE              TO IR-OPEN-DATE
           MOVE ACM-CLOSE-DATE             TO IR-CLOSE-DATE
           IF ACM-NO-CREDITS
               MOVE 'Y'                    TO IR-BREACH-IND
           ELSE
               MOVE 'N'                    TO IR-BREACH-IND
           END-IF
           MOVE ZERO                       TO IR-ACCOUNT-COUNT
           WRITE ISARTN-RECORD             FROM WS-ISA-RETURN
           IF WS-ISARTN-STATUS NOT = '00'
               MOVE '4100-ISA-ACCOUNT'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ISARTN WRITE FAILED'  TO ABN-MESSAGE
               MOVE WS-ISARTN-STATUS       TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-RETURNS
           ADD ACM-ISA-SUBS-YTD            TO WS-TOTAL-SUBS
           ADD IR-MARKET-VALUE             TO WS-TOTAL-VALUE

      *---- RESET FOR THE NEW TAX YEAR --------------------------------*
           MOVE ZERO                       TO ACM-ISA-SUBS-YTD
           IF ACM-NO-CREDITS AND ACM-OPEN
               SET ACM-NO-RESTRICT         TO TRUE
               ADD 1                       TO WS-RESTRICT-LIFTED
           END-IF
           MOVE WS-BUS-DATE                TO ACM-LAST-MAINT-DATE
           MOVE ZERO                       TO ACM-LAST-MAINT-TIME
           MOVE WS-PROGRAM-ID              TO ACM-LAST-MAINT-USER
           REWRITE ACM-ACCOUNT-RECORD
           IF NOT WS-ACCTMAST-OK
               MOVE '4100-ISA-ACCOUNT'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ACCTMAST REWRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ACCTMAST-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           .
       4100-EXIT.
           EXIT.
      *================================================================*
       7000-REPORT-HEADINGS.
      *================================================================*
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           MOVE 2                          TO WS-LINE-COUNT
           .
      *================================================================*
       9000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           IF WS-MODE-DAILY
               MOVE 'CREDITS POSTED TODAY'     TO TL-LABEL
               MOVE WS-CREDITS-READ            TO TL-COUNT
               MOVE ZERO                       TO TL-AMOUNT
               WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
               MOVE 'ISA SUBSCRIPTIONS'        TO TL-LABEL
               MOVE WS-ISA-CREDITS             TO TL-COUNT
               MOVE WS-TOTAL-SUBS              TO TL-AMOUNT
               WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
               MOVE 'ISA TRANSFERS IN (EXCLUDED)' TO TL-LABEL
               MOVE WS-TRANSFERS-IN            TO TL-COUNT
               MOVE ZERO                       TO TL-AMOUNT
               WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
               MOVE 'LIMIT BREACHES'           TO TL-LABEL
               MOVE WS-BREACHES                TO TL-COUNT
               WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           ELSE
               MOVE 'ISA ACCOUNTS ON FILE'     TO TL-LABEL
               MOVE WS-ISA-ACCOUNTS            TO TL-COUNT
               MOVE ZERO                       TO TL-AMOUNT
               WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
               MOVE 'RETURNED TO HMRC / SUBSCRIPTIONS' TO TL-LABEL
               MOVE WS-RETURNS                 TO TL-COUNT
               MOVE WS-TOTAL-SUBS              TO TL-AMOUNT
               WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
               MOVE 'MARKET VALUE AT 5 APRIL'  TO TL-LABEL
               MOVE ZERO                       TO TL-COUNT
               MOVE WS-TOTAL-VALUE             TO TL-AMOUNT
               WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
               MOVE 'BREACH RESTRICTIONS LIFTED' TO TL-LABEL
               MOVE WS-RESTRICT-LIFTED         TO TL-COUNT
               MOVE ZERO                       TO TL-AMOUNT
               WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           END-IF
           CLOSE ACCTMAST-FILE PRDPARM-FILE BREACH-FILE ISARTN-FILE
                 REPORT-FILE
           IF WS-BREACHES > ZERO
               MOVE 4                          TO RETURN-CODE
           END-IF
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
           MOVE SQLCODE                    TO ABN-SQLCODE
           MOVE 1003                       TO ABN-ABEND-CODE
           MOVE 'DB2 ERROR - SEE SQLCODE'  TO ABN-MESSAGE
           EXEC SQL ROLLBACK END-EXEC
           PERFORM Z999-ABEND
           .
      *
           COPY ABNDPROC.
