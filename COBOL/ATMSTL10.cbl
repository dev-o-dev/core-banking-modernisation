       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ATMSTL10.
       AUTHOR.        R M CHADWICK.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2014-09-08.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : ATMSTL10                                            *
      * TITLE    : LINK ATM NETWORK - DAILY SETTLEMENT AND POSTING     *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : ATMSTL01 STEP AT010 (DAILY, BEFORE PSEOD01)         *
      *----------------------------------------------------------------*
      * INPUT    : LINKIN   - LINK SETTLEMENT FILE (ATMLKREC, FB 200)  *
      *            ACCTMAST - DDA ACCOUNT MASTER (RANDOM, READ ONLY)   *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : ATMPOST  - CARDHOLDER 'CSH ' DEBITS AND ATM FEES    *
      *                       FOR TONIGHT'S PSEOD01 (PSTXNREC, MOD)    *
      *            GLJNL    - INTERCHANGE / SETTLEMENT JOURNAL         *
      *                       (GLJNLREC, SOURCE 'ATMS')                *
      *            REPORT   - SETTLEMENT REPORT (FBA 133)              *
      *----------------------------------------------------------------*
      * ISSUER ITEMS (MEGABANK CARD AT ANOTHER MEMBER'S ATM, ROLE 'I'):*
      *   WD  'CSH ' DEBIT TO THE CARD ACCOUNT FOR THE CASH DISPENSED. *
      *       FEE RULES, IN ORDER:                                     *
      *        1. SURCHARGE - A PAY-TO-USE OPERATOR'S DIRECT CHARGE IS *
      *           PASSED ON AS A SEPARATE 'CHG ' DEBIT (THE CUSTOMER   *
      *           ACCEPTED IT ON THE ATM SCREEN).                      *
      *        2. BASIC BANK ACCOUNTS (PRODUCT 'BBA...') AND ACCOUNTS  *
      *           WITH CHARGES WAIVED PAY NO MEGABANK FEE (PAYMENT     *
      *           ACCOUNTS REGULATIONS 2015 - FEE-FREE BASIC ACCOUNT). *
      *        3. ATM IN JERSEY, GUERNSEY OR THE ISLE OF MAN - OFF-    *
      *           SHORE FEE WS-OFFSHORE-FEE PER WITHDRAWAL.            *
      *        4. SAVINGS ACCOUNTS (ACM-SAVINGS) WITH A CASH CARD -    *
      *           FOREIGN ATM FEE WS-SAVINGS-FEE PER WITHDRAWAL.       *
      *        5. NO MORE THAN WS-MAX-FEES-PER-DAY MEGABANK FEES PER   *
      *           ACCOUNT PER SETTLEMENT FILE - FURTHER ONES ARE FREE. *
      *        CURRENT ACCOUNTS USING UK LINK ATMS PAY NO MEGABANK FEE.*
      *   BE  NO CUSTOMER POSTING - INTERCHANGE ONLY.                  *
      *   RV  'CSH ' CREDIT FOR THE REVERSED AMOUNT, PLUS ONE 'REV '   *
      *       CREDIT REFUNDING THE SURCHARGE AND ANY MEGABANK FEE.     *
      *   AN ITEM FOR AN ACCOUNT THAT IS NOT ON ACCTMAST, IS CLOSED OR *
      *   IS NOT IN STERLING CANNOT BE POSTED. LINK STILL SETTLES IT,  *
      *   SO IT IS BOOKED TO ATM SUSPENSE (29910-0300) AND REPORTED FOR*
      *   CARD SERVICES. RESTRICTED ACCOUNTS ARE POSTED (THE ONLINE    *
      *   AUTHORISATION ALREADY RELEASED THE CASH) AND REPORTED.       *
      *                                                                *
      * ACQUIRER ITEMS (OTHER MEMBER'S CARD AT A MEGABANK ATM, 'A'):   *
      *   NO CUSTOMER POSTING. CASH DISPENSED IS RECEIVABLE FROM LINK  *
      *   AND INTERCHANGE IS INCOME.                                   *
      *                                                                *
      * NET SETTLEMENT = ACQUIRED CASH + INTERCHANGE INCOME            *
      *                - ISSUED CASH - INTERCHANGE PAID - SURCHARGES   *
      * IS COMPARED WITH THE LINK TRAILER (U1004 IF DIFFERENT) AND     *
      * PRINTED FOR TREASURY, WHO FUND THE RTGS SETTLEMENT ACCOUNT.    *
      *                                                                *
      * GL (SOURCE 'ATMS'): INTERCHANGE PAID AND EARNED, ACQUIRED CASH *
      * OUT OF THE ATMS AND SUSPENSE ITEMS, ALL AGAINST THE LINK       *
      * SETTLEMENT ACCOUNT 28510-0100. THE CUSTOMER 'CSH ' AND 'CHG '  *
      * POSTINGS REACH THE GL THROUGH GLEXT010 (GLMAP ROWS CSH/ATM AND *
      * CHG/ATM POINT AT THE SAME SETTLEMENT ACCOUNT).                 *
      *                                                                *
      * KNOWN ISSUE PIR 2016/112 OPEN: A REVERSAL REFUNDS THE MEGABANK *
      * FEE WORKED OUT BY THE RULES ABOVE WITHOUT LOOKING AT WHETHER   *
      * THE ORIGINAL WITHDRAWAL WAS ACTUALLY CHARGED (RULE 5 CAP, OR AN*
      * ORIGINAL ON AN EARLIER FILE BEFORE A PRODUCT CHANGE). CARD     *
      * SERVICES RECOVER OVER-REFUNDS MANUALLY - ABOUT 20 A MONTH.     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2014-09-08 RMC  ORIGINAL VERSION (LINK MEMBERSHIP)             *
      * 2015-03-16 RMC  SURCHARGE PASS-THROUGH (RULE 1). FEES NET OF   *
      *                 REFUNDS IN THE MEGABANK FEES TOTAL             *
      * 2016-09-19 PDQ  BASIC BANK ACCOUNT EXEMPTION (RULE 2) FOR THE  *
      *                 PAYMENT ACCOUNTS REGULATIONS 2015              *
      * 2017-01-09 PDQ  OFF-SHORE ATM FEE (RULE 3) - CR 16/0388        *
      * 2017-05-22 RMC  DAILY FEE CAP (RULE 5) AFTER FCA COMPLAINT     *
      *                 REVIEW; RESTRICTED ACCOUNTS NOW POSTED, NOT    *
      *                 SUSPENDED (INC 17/04471)                       *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LINKIN-FILE      ASSIGN TO LINKIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-LINKIN-STATUS.
           SELECT ACCTMAST-FILE    ASSIGN TO ACCTMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS ACM-KEY
                                   FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT ATMPOST-FILE     ASSIGN TO ATMPOST
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-ATMPOST-STATUS.
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
       FD  LINKIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LINKIN-RECORD                   PIC X(200).
       FD  ACCTMAST-FILE.
           COPY DDACCREC.
       FD  ATMPOST-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ATMPOST-RECORD                  PIC X(200).
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
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'ATMSTL10'.
      *
       01  WS-CONSTANTS.
           05  WS-MBK-MEMBER-ID            PIC X(06) VALUE 'MBK001'.
           05  WS-OFFSHORE-FEE             PIC S9(03)V99 COMP-3
                                           VALUE +1.50.
           05  WS-SAVINGS-FEE              PIC S9(03)V99 COMP-3
                                           VALUE +1.00.
           05  WS-MAX-FEES-PER-DAY         PIC 9(02) VALUE 03.
           05  WS-POST-TIME-BASE           PIC 9(08) VALUE 00030000.
           05  WS-FEE-TABLE-MAX            PIC S9(04) COMP VALUE +3000.
      *
      *---- GL ACCOUNTS (CHART OF ACCOUNTS 2002) ----------------------*
       01  WS-GL-ACCOUNTS.
           05  WS-GL-LINK-SETTLE           PIC X(10) VALUE '28510-0100'.
           05  WS-GL-IC-EXPENSE            PIC X(10) VALUE '52310-0100'.
           05  WS-GL-IC-INCOME             PIC X(10) VALUE '43310-0100'.
           05  WS-GL-ATM-CASH              PIC X(10) VALUE '10120-0100'.
           05  WS-GL-ATM-SUSPENSE          PIC X(10) VALUE '29910-0300'.
           05  WS-GL-COST-CENTRE           PIC X(06) VALUE 'PAYATM'.
      *
       01  WS-FILE-STATUSES.
           05  WS-LINKIN-STATUS            PIC X(02).
               88  WS-LINKIN-OK                VALUE '00'.
               88  WS-LINKIN-EOF               VALUE '10'.
           05  WS-ACCTMAST-STATUS          PIC X(02).
               88  WS-ACCTMAST-OK              VALUE '00'.
               88  WS-ACCTMAST-NOTFND          VALUE '23'.
           05  WS-ATMPOST-STATUS           PIC X(02).
           05  WS-GLJNL-STATUS             PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-FILE              VALUE 'Y'.
           05  WS-TRAILER-IND              PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
           05  WS-POSTABLE-IND             PIC X(01).
               88  WS-POSTABLE                 VALUE 'Y'.
               88  WS-NOT-POSTABLE             VALUE 'N'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-SETTLE-DATE              PIC 9(08).
           05  WS-TIME-NOW                 PIC 9(08).
           05  WS-POST-SEQ                 PIC 9(06) VALUE ZERO.
           05  WS-OWN-FEE                  PIC S9(03)V99 COMP-3.
           05  WS-SUSP-AMOUNT              PIC S9(11)V99 COMP-3.
           05  WS-EXC-CODE                 PIC X(04).
           05  WS-NOTE                     PIC X(50).
           05  WS-REF-SUFFIX               PIC X(01).
           05  WS-ACCOUNT-KEY.
               10  WS-AK-SORT-CODE         PIC X(06).
               10  WS-AK-ACCT-NO           PIC X(08).
           05  WS-DISP-AMOUNT              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
      *---- PER-ACCOUNT MEGABANK FEE COUNT (RULE 5) -------------------*
       01  WS-FEE-TABLE.
           05  WS-FT-COUNT                 PIC S9(04) COMP VALUE ZERO.
           05  WS-FT-ENTRY                 OCCURS 3000 TIMES
                                           INDEXED BY FT-IDX.
               10  WS-FT-ACCOUNT           PIC X(14).
               10  WS-FT-FEES              PIC 9(02).
       01  WS-FT-FOUND-IND                 PIC X(01).
           88  WS-FT-FOUND                     VALUE 'Y'.
      *
      *---- CONTROL TOTALS --------------------------------------------*
       01  WS-COUNTERS.
           05  WS-RECORDS-READ             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DETAILS-READ             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ISS-WD-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ISS-BE-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ISS-RV-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACQ-WD-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACQ-BE-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACQ-RV-COUNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-POSTINGS                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-OWN-FEES-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-FEES-CAPPED              PIC S9(09) COMP-3 VALUE 0.
           05  WS-FEES-EXEMPT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-SUSPENSE-COUNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RESTRICTED-COUNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-EXCEPTIONS               PIC S9(09) COMP-3 VALUE 0.
       01  WS-AMOUNTS.
           05  WS-ISS-AMOUNT               PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-ISS-FEES                 PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-ISS-SURCHARGE            PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-ACQ-AMOUNT               PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-ACQ-FEES                 PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-OWN-FEES-TOTAL           PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-CUST-DEBITS              PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-CUST-CREDITS             PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-SUSPENSE-AMOUNT          PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-CALC-NET                 PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-LINK-NET                 PIC S9(13)V99 COMP-3
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
           05  WS-PAIR-TXN-CODE            PIC X(04).
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC S9(04) COMP VALUE +55.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'ATMSTL10'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - LINK ATM SETTLEMENT'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER            PIC X(01) VALUE '0'.
           05  FILLER            PIC X(02) VALUE 'R'.
           05  FILLER            PIC X(03) VALUE 'TY'.
           05  FILLER            PIC X(13) VALUE 'TRACE NO'.
           05  FILLER            PIC X(16) VALUE 'ACCOUNT'.
           05  FILLER            PIC X(09) VALUE 'ATM'.
           05  FILLER            PIC X(15) VALUE '        AMOUNT'.
           05  FILLER            PIC X(09) VALUE '   SURCH'.
           05  FILLER            PIC X(09) VALUE '  OWN FEE'.
           05  FILLER            PIC X(06) VALUE ' EXC'.
           05  FILLER            PIC X(50) VALUE 'NOTE'.
       01  WS-HEADING-3.
           05  FILLER            PIC X(01) VALUE ' '.
           05  FILLER            PIC X(132) VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                       PIC X(01) VALUE ' '.
           05  DL-ROLE                     PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TYPE                     PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-TRACE                    PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SORT-CODE                PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  DL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-ATM-ID                   PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-AMOUNT                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-SURCHARGE                PIC ZZZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  DL-OWN-FEE                  PIC ZZZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-EXC                      PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  DL-NOTE                     PIC X(49).
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(53) VALUE SPACES.
       01  WS-SETTLE-LINE.
           05  SL-CC                       PIC X(01) VALUE ' '.
           05  SL-LABEL                    PIC X(40).
           05  SL-OURS                     PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  SL-LINK                     PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  SL-RESULT                   PIC X(10).
           05  FILLER                      PIC X(36) VALUE SPACES.
      *
           COPY ATMLKREC.
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
           PERFORM 2000-PROCESS-RECORD THRU 2000-EXIT
               UNTIL WS-END-OF-FILE
           PERFORM 2900-RECONCILE-TRAILER
           PERFORM 3000-WRITE-JOURNAL
           PERFORM 4000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  LINKIN-FILE
                       ACCTMAST-FILE
                       DATECARD-FILE
                EXTEND ATMPOST-FILE
                OUTPUT GLJNL-FILE
                       REPORT-FILE
           IF NOT WS-LINKIN-OK
           OR NOT WS-ACCTMAST-OK
           OR WS-ATMPOST-STATUS NOT = '00'
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
           INITIALIZE WS-FEE-TABLE

      *---- HEADER MUST BE FIRST AND FOR MEGABANK ---------------------*
           PERFORM 8000-READ-LINK
           IF WS-END-OF-FILE OR NOT ATL-HEADER
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'LINK FILE HAS NO HEADER' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF NOT ATLH-LINK-SCHEME
           OR ATLH-MEMBER-ID NOT = WS-MBK-MEMBER-ID
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'LINK HEADER NOT FOR MEGABANK MEMBER MBK001'
                                           TO ABN-MESSAGE
               MOVE ATL-BODY (1:40)        TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           IF ATLH-SETTLE-DATE NOT NUMERIC
           OR ATLH-SETTLE-DATE > WS-BUS-DATE
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'LINK SETTLEMENT DATE AFTER BUSINESS DATE'
                                           TO ABN-MESSAGE
               MOVE ATLH-SETTLE-DATE       TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           MOVE ATLH-SETTLE-DATE           TO WS-SETTLE-DATE
           DISPLAY 'ATMSTL10 - LINK FILE ' ATLH-SETTLE-DATE
                   ' SEQ ' ATLH-FILE-SEQ
           PERFORM 7000-REPORT-HEADINGS
           PERFORM 8000-READ-LINK
           .
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           IF ATL-TRAILER
               SET WS-TRAILER-SEEN         TO TRUE
               PERFORM 8000-READ-LINK
               IF NOT WS-END-OF-FILE
                   MOVE '2000-PROCESS-RECORD' TO ABN-PARAGRAPH
                   MOVE 1006               TO ABN-ABEND-CODE
                   MOVE 'RECORDS FOUND AFTER LINK TRAILER'
                                           TO ABN-MESSAGE
                   PERFORM Z999-ABEND
               END-IF
               GO TO 2000-EXIT
           END-IF
           IF NOT ATL-DETAIL
               MOVE '2000-PROCESS-RECORD'  TO ABN-PARAGRAPH
               MOVE 1006                   TO ABN-ABEND-CODE
               MOVE 'UNEXPECTED RECORD TYPE ON LINK FILE'
                                           TO ABN-MESSAGE
               MOVE ATL-REC-TYPE           TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           ADD 1                           TO WS-DETAILS-READ
           MOVE SPACES                     TO WS-EXC-CODE WS-NOTE
           MOVE ZERO                       TO WS-OWN-FEE

           EVALUATE TRUE
               WHEN NOT ATLD-TYPE-VALID
                   MOVE 'TYPE'             TO WS-EXC-CODE
                   MOVE 'UNKNOWN LINK TRANSACTION TYPE - IGNORED'
                                           TO WS-NOTE
                   PERFORM 6000-EXCEPTION
               WHEN ATLD-ISSUER
                   PERFORM 2100-ISSUER THRU 2100-EXIT
               WHEN ATLD-ACQUIRER
                   PERFORM 2200-ACQUIRER
               WHEN OTHER
                   MOVE 'ROLE'             TO WS-EXC-CODE
                   MOVE 'UNKNOWN ROLE - NOT SETTLED BY MEGABANK'
                                           TO WS-NOTE
                   PERFORM 6000-EXCEPTION
           END-EVALUATE
           PERFORM 8000-READ-LINK
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-ISSUER.
      *================================================================*
      *    LINK CONTROL TOTALS FIRST - THEY COUNT EVERY ITEM WHETHER OR
      *    NOT WE CAN POST IT TO THE CUSTOMER.
           EVALUATE TRUE
               WHEN ATLD-WITHDRAWAL
                   ADD 1                   TO WS-ISS-WD-COUNT
                   ADD ATLD-AMOUNT         TO WS-ISS-AMOUNT
                   ADD ATLD-SURCHARGE      TO WS-ISS-SURCHARGE
                   ADD ATLD-INTERCHANGE    TO WS-ISS-FEES
               WHEN ATLD-BALANCE-ENQ
                   ADD 1                   TO WS-ISS-BE-COUNT
                   ADD ATLD-INTERCHANGE    TO WS-ISS-FEES
               WHEN ATLD-REVERSAL
                   ADD 1                   TO WS-ISS-RV-COUNT
                   SUBTRACT ATLD-AMOUNT    FROM WS-ISS-AMOUNT
                   SUBTRACT ATLD-SURCHARGE FROM WS-ISS-SURCHARGE
                   SUBTRACT ATLD-INTERCHANGE FROM WS-ISS-FEES
           END-EVALUATE

      *    BALANCE ENQUIRIES COST THE CUSTOMER NOTHING.
           IF ATLD-BALANCE-ENQ
               GO TO 2100-EXIT
           END-IF

           PERFORM 2110-CHECK-ACCOUNT THRU 2110-EXIT
           IF WS-NOT-POSTABLE
               PERFORM 2150-SUSPENSE-ITEM
               GO TO 2100-EXIT
           END-IF

           PERFORM 2120-FEE-RULES THRU 2120-EXIT
           IF ATLD-WITHDRAWAL
               PERFORM 2130-POST-WITHDRAWAL
           ELSE
               PERFORM 2140-POST-REVERSAL
           END-IF
           IF WS-EXC-CODE NOT = SPACES
               PERFORM 6000-EXCEPTION
           END-IF
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2110-CHECK-ACCOUNT.
      *================================================================*
           SET WS-POSTABLE                 TO TRUE
           IF ATLD-CCY-CD NOT = 'GBP'
               SET WS-NOT-POSTABLE         TO TRUE
               MOVE 'CCY '                 TO WS-EXC-CODE
               MOVE 'NON-STERLING ITEM - LINK IS A STERLING SCHEME'
                                           TO WS-NOTE
               GO TO 2110-EXIT
           END-IF
           MOVE ATLD-CARD-SORT-CODE        TO ACM-SORT-CODE
                                              WS-AK-SORT-CODE
           MOVE ATLD-CARD-ACCT-NO          TO ACM-ACCT-NO
                                              WS-AK-ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN WS-ACCTMAST-NOTFND
                   SET WS-NOT-POSTABLE     TO TRUE
                   MOVE 'NOAC'             TO WS-EXC-CODE
                   MOVE 'CARD ACCOUNT NOT ON ACCTMAST - TO SUSPENSE'
                                           TO WS-NOTE
               WHEN NOT WS-ACCTMAST-OK
                   MOVE '2110-CHECK-ACCOUNT' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'ACCTMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-ACCTMAST-STATUS TO ABN-FILE-STATUS
                   MOVE WS-ACCOUNT-KEY     TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
               WHEN ACM-CLOSED
                   SET WS-NOT-POSTABLE     TO TRUE
                   MOVE 'CLSD'             TO WS-EXC-CODE
                   MOVE 'CARD ACCOUNT CLOSED - TO SUSPENSE'
                                           TO WS-NOTE
               WHEN ACM-FROZEN
               WHEN ACM-NO-DEBITS
               WHEN ACM-NO-POSTINGS
      *            ALREADY AUTHORISED ONLINE - POST AND REPORT
                   MOVE 'RSTR'             TO WS-EXC-CODE
                   MOVE 'ACCOUNT RESTRICTED - POSTED, REFER CARD OPS'
                                           TO WS-NOTE
                   ADD 1                   TO WS-RESTRICTED-COUNT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2110-EXIT.
           EXIT.
      *================================================================*
       2120-FEE-RULES.
      *================================================================*
      *    RULE 1 (SURCHARGE) IS APPLIED IN 2130 / 2140. RULES 2-5
      *    DECIDE THE MEGABANK FEE IN WS-OWN-FEE.
           MOVE ZERO                       TO WS-OWN-FEE
      *---- RULE 2 - EXEMPT ACCOUNTS ----------------------------------*
           IF ACM-PROD-CD (1:3) = 'BBA'
           OR ACM-CHARGES-WAIVED
               ADD 1                       TO WS-FEES-EXEMPT
               GO TO 2120-EXIT
           END-IF
      *---- RULES 3 AND 4 - THE HIGHER FEE APPLIES --------------------*
           IF ATLD-ATM-OFFSHORE
               MOVE WS-OFFSHORE-FEE        TO WS-OWN-FEE
           END-IF
           IF ACM-SAVINGS
           AND WS-SAVINGS-FEE > WS-OWN-FEE
               MOVE WS-SAVINGS-FEE         TO WS-OWN-FEE
           END-IF
           IF WS-OWN-FEE = ZERO
               GO TO 2120-EXIT
           END-IF
      *---- RULE 5 - DAILY CAP (WITHDRAWALS ONLY - SEE PIR 2016/112) --*
           IF ATLD-REVERSAL
               GO TO 2120-EXIT
           END-IF
           PERFORM 2125-FIND-FEE-ENTRY
           IF WS-FT-FOUND
               IF WS-FT-FEES (FT-IDX) >= WS-MAX-FEES-PER-DAY
                   MOVE ZERO               TO WS-OWN-FEE
                   ADD 1                   TO WS-FEES-CAPPED
               ELSE
                   ADD 1                   TO WS-FT-FEES (FT-IDX)
               END-IF
           ELSE
               IF WS-FT-COUNT < WS-FEE-TABLE-MAX
                   ADD 1                   TO WS-FT-COUNT
                   SET FT-IDX              TO WS-FT-COUNT
                   MOVE WS-ACCOUNT-KEY     TO WS-FT-ACCOUNT (FT-IDX)
                   MOVE 1                  TO WS-FT-FEES (FT-IDX)
               END-IF
           END-IF
           .
       2120-EXIT.
           EXIT.
      *================================================================*
       2125-FIND-FEE-ENTRY.
      *================================================================*
           MOVE 'N'                        TO WS-FT-FOUND-IND
           PERFORM VARYING FT-IDX FROM 1 BY 1
                   UNTIL FT-IDX > WS-FT-COUNT
                      OR WS-FT-FOUND
               IF WS-FT-ACCOUNT (FT-IDX) = WS-ACCOUNT-KEY
                   SET WS-FT-FOUND         TO TRUE
               END-IF
           END-PERFORM
      *    THE VARYING HAS STEPPED ONE PAST THE MATCH
           IF WS-FT-FOUND
               SET FT-IDX                  DOWN BY 1
           END-IF
           .
      *================================================================*
       2130-POST-WITHDRAWAL.
      *================================================================*
      *---- CASH DISPENSED --------------------------------------------*
           MOVE SPACES                     TO PST-POSTING-RECORD
           SET PST-DEBIT                   TO TRUE
           MOVE 'CSH '                     TO PST-TXN-CODE
           MOVE ATLD-AMOUNT                TO PST-AMOUNT
           STRING 'LINK ATM ' ATLD-ATM-LOCATION
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-1
           END-STRING
           STRING 'ATM ' ATLD-ATM-ID ' ' ATLD-TXN-DATE
                  ' ' ATLD-TXN-TIME (1:4) ' ' ATLD-RESP-CD
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE 'W'                        TO WS-REF-SUFFIX
           PERFORM 5000-WRITE-POSTING
           ADD ATLD-AMOUNT                 TO WS-CUST-DEBITS
      *---- RULE 1 - OPERATOR SURCHARGE -------------------------------*
           IF ATLD-SURCHARGE > ZERO
               MOVE SPACES                 TO PST-POSTING-RECORD
               SET PST-DEBIT               TO TRUE
               MOVE 'CHG '                 TO PST-TXN-CODE
               MOVE ATLD-SURCHARGE         TO PST-AMOUNT
               MOVE 'ATM OPERATOR CHARGE' TO PST-NARRATIVE-1
               MOVE ATLD-ATM-ID            TO PST-NARRATIVE-2
               MOVE 'S'                    TO WS-REF-SUFFIX
               PERFORM 5000-WRITE-POSTING
               ADD ATLD-SURCHARGE          TO WS-CUST-DEBITS
           END-IF
      *---- RULES 2-5 - MEGABANK FEE ----------------------------------*
           IF WS-OWN-FEE > ZERO
               MOVE SPACES                 TO PST-POSTING-RECORD
               SET PST-DEBIT               TO TRUE
               MOVE 'CHG '                 TO PST-TXN-CODE
               MOVE WS-OWN-FEE             TO PST-AMOUNT
               IF ATLD-ATM-OFFSHORE
                   MOVE 'OFFSHORE ATM FEE'  TO PST-NARRATIVE-1
               ELSE
                   MOVE 'NON-MEGABANK ATM FEE' TO PST-NARRATIVE-1
               END-IF
               MOVE ATLD-ATM-LOCATION      TO PST-NARRATIVE-2
               MOVE 'F'                    TO WS-REF-SUFFIX
               PERFORM 5000-WRITE-POSTING
               ADD 1                       TO WS-OWN-FEES-COUNT
               ADD WS-OWN-FEE              TO WS-OWN-FEES-TOTAL
                                              WS-CUST-DEBITS
           END-IF
           .
      *================================================================*
       2140-POST-REVERSAL.
      *================================================================*
           MOVE SPACES                     TO PST-POSTING-RECORD
           SET PST-CREDIT                  TO TRUE
           MOVE 'CSH '                     TO PST-TXN-CODE
           MOVE ATLD-AMOUNT                TO PST-AMOUNT
           MOVE 'LINK ATM REVERSAL'        TO PST-NARRATIVE-1
           STRING 'ORIG ' ATLD-ORIG-TRACE-NO ' ATM ' ATLD-ATM-ID
                  DELIMITED BY SIZE
                  INTO PST-NARRATIVE-2
           END-STRING
           MOVE 'W'                        TO WS-REF-SUFFIX
           PERFORM 5000-WRITE-POSTING
           ADD ATLD-AMOUNT                 TO WS-CUST-CREDITS
      *    ONE 'REV ' CREDIT REFUNDS THE SURCHARGE AND THE MEGABANK
      *    FEE (PIR 2016/112 - REFUNDED WHETHER OR NOT IT WAS CHARGED)
           IF ATLD-SURCHARGE > ZERO OR WS-OWN-FEE > ZERO
               MOVE SPACES                 TO PST-POSTING-RECORD
               SET PST-CREDIT              TO TRUE
               MOVE 'REV '                 TO PST-TXN-CODE
               COMPUTE PST-AMOUNT = ATLD-SURCHARGE + WS-OWN-FEE
               MOVE 'ATM CHARGES REFUND'   TO PST-NARRATIVE-1
               STRING 'ORIG ' ATLD-ORIG-TRACE-NO
                      DELIMITED BY SIZE
                      INTO PST-NARRATIVE-2
               END-STRING
               MOVE 'F'                    TO WS-REF-SUFFIX
               PERFORM 5000-WRITE-POSTING
               SUBTRACT WS-OWN-FEE         FROM WS-OWN-FEES-TOTAL
               ADD PST-AMOUNT              TO WS-CUST-CREDITS
           END-IF
           .
      *================================================================*
       2150-SUSPENSE-ITEM.
      *================================================================*
      *    LINK SETTLES THE ITEM REGARDLESS - BOOK IT TO ATM SUSPENSE.
      *    A REVERSAL OF AN UNPOSTABLE ITEM CLEARS SUSPENSE.
           COMPUTE WS-SUSP-AMOUNT = ATLD-AMOUNT + ATLD-SURCHARGE
           IF ATLD-REVERSAL
               SUBTRACT WS-SUSP-AMOUNT     FROM WS-SUSPENSE-AMOUNT
           ELSE
               ADD WS-SUSP-AMOUNT          TO WS-SUSPENSE-AMOUNT
           END-IF
           ADD 1                           TO WS-SUSPENSE-COUNT
           PERFORM 6000-EXCEPTION
           .
      *================================================================*
       2200-ACQUIRER.
      *================================================================*
           IF ATLD-ACQUIRER-MEMBER NOT = WS-MBK-MEMBER-ID
               MOVE 'ACQR'                 TO WS-EXC-CODE
               MOVE 'ACQUIRER MEMBER IS NOT MEGABANK - CHECK WITH LINK'
                                           TO WS-NOTE
               PERFORM 6000-EXCEPTION
           END-IF
           EVALUATE TRUE
               WHEN ATLD-WITHDRAWAL
                   ADD 1                   TO WS-ACQ-WD-COUNT
                   ADD ATLD-AMOUNT         TO WS-ACQ-AMOUNT
                   ADD ATLD-INTERCHANGE    TO WS-ACQ-FEES
               WHEN ATLD-BALANCE-ENQ
                   ADD 1                   TO WS-ACQ-BE-COUNT
                   ADD ATLD-INTERCHANGE    TO WS-ACQ-FEES
               WHEN ATLD-REVERSAL
                   ADD 1                   TO WS-ACQ-RV-COUNT
                   SUBTRACT ATLD-AMOUNT    FROM WS-ACQ-AMOUNT
                   SUBTRACT ATLD-INTERCHANGE FROM WS-ACQ-FEES
           END-EVALUATE
           .
      *================================================================*
       2900-RECONCILE-TRAILER.
      *================================================================*
      *    ATL-TRL-DATA IS STILL IN THE RECORD AREA (SEE 8000).
           IF NOT WS-TRAILER-SEEN
               MOVE '2900-RECONCILE-TRAILER' TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'LINK FILE HAS NO TRAILER' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF ATLT-RECORD-COUNT NOT = WS-DETAILS-READ
               MOVE '2900-RECONCILE-TRAILER' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'LINK TRAILER RECORD COUNT MISMATCH'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF ATLT-ISS-AMOUNT NOT = WS-ISS-AMOUNT
           OR ATLT-ACQ-AMOUNT NOT = WS-ACQ-AMOUNT
           OR ATLT-ISS-FEES NOT = WS-ISS-FEES
           OR ATLT-ACQ-FEES NOT = WS-ACQ-FEES
           OR ATLT-ISS-SURCHARGE NOT = WS-ISS-SURCHARGE
               MOVE '2900-RECONCILE-TRAILER' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'LINK TRAILER AMOUNT TOTALS MISMATCH'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

      *---- NET SETTLEMENT (POSITIVE = MEGABANK RECEIVES) -------------*
           COMPUTE WS-CALC-NET = WS-ACQ-AMOUNT + WS-ACQ-FEES
                               - WS-ISS-AMOUNT - WS-ISS-FEES
                               - WS-ISS-SURCHARGE
           IF ATLT-MEMBER-PAYS
               COMPUTE WS-LINK-NET = ZERO - ATLT-NET-SETTLE
           ELSE
               MOVE ATLT-NET-SETTLE        TO WS-LINK-NET
           END-IF
           IF WS-LINK-NET NOT = WS-CALC-NET
               MOVE '2900-RECONCILE-TRAILER' TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'NET SETTLEMENT DOES NOT AGREE WITH LINK'
                                           TO ABN-MESSAGE
               MOVE WS-CALC-NET            TO WS-DISP-AMOUNT
               DISPLAY 'ATMSTL10 - CALCULATED NET  ' WS-DISP-AMOUNT
               MOVE WS-LINK-NET            TO WS-DISP-AMOUNT
               DISPLAY 'ATMSTL10 - LINK NET        ' WS-DISP-AMOUNT
               PERFORM Z999-ABEND
           END-IF
           .
      *================================================================*
       3000-WRITE-JOURNAL.
      *================================================================*
           ACCEPT WS-TIME-NOW              FROM TIME
           MOVE SPACES                     TO GLJ-JOURNAL-LINE
           SET GLJ-HEADER                  TO TRUE
           MOVE 'ATMS'                     TO GLJ-SOURCE-SYSTEM
           STRING 'ATMSTL' WS-SETTLE-DATE
                  DELIMITED BY SIZE
                  INTO GLJ-JOURNAL-ID
           END-STRING
           MOVE WS-BUS-DATE                TO GLJ-BUSINESS-DATE
           MOVE 'ATMST'                    TO GLJH-RUN-ID
           MOVE WS-BUS-DATE                TO GLJH-CREATE-DATE
           MOVE WS-TIME-NOW (1:6)          TO GLJH-CREATE-TIME
           MOVE WS-PROGRAM-ID              TO GLJH-CREATING-PGM
           PERFORM 3900-WRITE-GL

      *---- INTERCHANGE PAID AS ISSUER --------------------------------*
           MOVE WS-GL-IC-EXPENSE           TO WS-PAIR-DR-GL
           MOVE WS-GL-LINK-SETTLE          TO WS-PAIR-CR-GL
           MOVE WS-ISS-FEES                TO WS-PAIR-AMOUNT
           MOVE 'LINK INTERCHANGE PAID'    TO WS-PAIR-DESC
           MOVE 'ICPD'                     TO WS-PAIR-TXN-CODE
           PERFORM 3200-JOURNAL-PAIR THRU 3200-EXIT
      *---- INTERCHANGE EARNED AS ACQUIRER ----------------------------*
           MOVE WS-GL-LINK-SETTLE          TO WS-PAIR-DR-GL
           MOVE WS-GL-IC-INCOME            TO WS-PAIR-CR-GL
           MOVE WS-ACQ-FEES                TO WS-PAIR-AMOUNT
           MOVE 'LINK INTERCHANGE EARNED'  TO WS-PAIR-DESC
           MOVE 'ICRC'                     TO WS-PAIR-TXN-CODE
           PERFORM 3200-JOURNAL-PAIR THRU 3200-EXIT
      *---- CASH DISPENSED TO OTHER MEMBERS' CARDHOLDERS --------------*
           MOVE WS-GL-LINK-SETTLE          TO WS-PAIR-DR-GL
           MOVE WS-GL-ATM-CASH             TO WS-PAIR-CR-GL
           MOVE WS-ACQ-AMOUNT              TO WS-PAIR-AMOUNT
           MOVE 'ACQUIRED ATM CASH DUE FROM LINK' TO WS-PAIR-DESC
           MOVE 'ACSH'                     TO WS-PAIR-TXN-CODE
           PERFORM 3200-JOURNAL-PAIR THRU 3200-EXIT
      *---- UNPOSTABLE ISSUER ITEMS -----------------------------------*
           MOVE WS-GL-ATM-SUSPENSE         TO WS-PAIR-DR-GL
           MOVE WS-GL-LINK-SETTLE          TO WS-PAIR-CR-GL
           MOVE WS-SUSPENSE-AMOUNT         TO WS-PAIR-AMOUNT
           MOVE 'LINK ITEMS NOT POSTABLE - SUSPENSE' TO WS-PAIR-DESC
           MOVE 'SUSP'                     TO WS-PAIR-TXN-CODE
           PERFORM 3200-JOURNAL-PAIR THRU 3200-EXIT

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
      *    ONE DEBIT AND ONE CREDIT LINE. A NEGATIVE AMOUNT (REVERSALS
      *    EXCEEDED THE DAY'S ITEMS) SWAPS THE SIDES.
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
           MOVE WS-PAIR-AMOUNT             TO GLJD-AMOUNT
           MOVE WS-PAIR-DESC               TO GLJD-DESCRIPTION
           MOVE WS-PAIR-TXN-CODE           TO GLJD-TXN-CODE
           PERFORM 3100-DETAIL
           MOVE WS-PAIR-CR-GL              TO GLJD-GL-ACCOUNT
           MOVE 'C'                        TO GLJD-DR-CR-IND
           MOVE WS-PAIR-AMOUNT             TO GLJD-AMOUNT
           MOVE WS-PAIR-DESC               TO GLJD-DESCRIPTION
           MOVE WS-PAIR-TXN-CODE           TO GLJD-TXN-CODE
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
           MOVE WS-GL-COST-CENTRE          TO GLJD-COST-CENTRE
           MOVE SPACES                     TO GLJD-PROD-CD
                                              GLJD-SORT-CODE
                                              GLJD-ACCT-NO
           MOVE 'GBP'                      TO GLJD-CCY-CD
           MOVE WS-SETTLE-DATE             TO GLJD-VALUE-DATE
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
           MOVE ATLD-CARD-SORT-CODE        TO PST-SORT-CODE
           MOVE ATLD-CARD-ACCT-NO          TO PST-ACCT-NO
           MOVE WS-BUS-DATE                TO PST-POST-DATE
           IF ATLD-TXN-DATE NUMERIC AND ATLD-TXN-DATE > ZERO
               MOVE ATLD-TXN-DATE          TO PST-VALUE-DATE
           ELSE
               MOVE WS-SETTLE-DATE         TO PST-VALUE-DATE
           END-IF
           MOVE WS-POST-TIME-BASE          TO PST-POST-TIME
           ADD 1                           TO WS-POST-SEQ
           MOVE WS-POST-SEQ                TO PST-SEQ-NO
           MOVE 'GBP'                      TO PST-CCY-CD
           MOVE 'ATM   '                   TO PST-CHANNEL-CD
      *    EXT REF = LNK + LINK TRACE + SUFFIX (W CASH, S SURCHARGE,
      *    F FEE) - PSEOD010 REJECTS A REPEATED FILE AS DUPLICATES.
           STRING 'LNK' ATLD-TRACE-NO WS-REF-SUFFIX
                  DELIMITED BY SIZE
                  INTO PST-EXT-REF
           END-STRING
           SET PST-HARD-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE WS-PROGRAM-ID              TO PST-USER-ID
           WRITE ATMPOST-RECORD            FROM PST-POSTING-RECORD
           IF WS-ATMPOST-STATUS NOT = '00'
               MOVE '5000-WRITE-POSTING'   TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'ATMPOST WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-ATMPOST-STATUS      TO ABN-FILE-STATUS
               MOVE PST-EXT-REF            TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-POSTINGS
           .
      *================================================================*
       6000-EXCEPTION.
      *================================================================*
           ADD 1                           TO WS-EXCEPTIONS
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 7000-REPORT-HEADINGS
           END-IF
           MOVE ATLD-ROLE                  TO DL-ROLE
           MOVE ATLD-TXN-TYPE              TO DL-TYPE
           MOVE ATLD-TRACE-NO              TO DL-TRACE
           MOVE ATLD-CARD-SORT-CODE        TO DL-SORT-CODE
           MOVE ATLD-CARD-ACCT-NO          TO DL-ACCT-NO
           MOVE ATLD-ATM-ID                TO DL-ATM-ID
           MOVE ATLD-AMOUNT                TO DL-AMOUNT
           MOVE ATLD-SURCHARGE             TO DL-SURCHARGE
           MOVE WS-OWN-FEE                 TO DL-OWN-FEE
           MOVE WS-EXC-CODE                TO DL-EXC
           MOVE WS-NOTE                    TO DL-NOTE
           WRITE REPORT-RECORD             FROM WS-DETAIL-LINE
           ADD 1                           TO WS-LINE-COUNT
           MOVE SPACES                     TO WS-EXC-CODE WS-NOTE
           .
      *================================================================*
       4000-FINALISE.
      *================================================================*
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'LINK RECORDS READ'        TO TL-LABEL
           MOVE WS-RECORDS-READ            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ISSUER - WITHDRAWALS'     TO TL-LABEL
           MOVE WS-ISS-WD-COUNT            TO TL-COUNT
           MOVE WS-ISS-AMOUNT              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  BALANCE ENQUIRIES'      TO TL-LABEL
           MOVE WS-ISS-BE-COUNT            TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  OPERATOR SURCHARGES'    TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-ISS-SURCHARGE           TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  INTERCHANGE PAID'       TO TL-LABEL
           MOVE WS-ISS-FEES                TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ACQUIRER - WITHDRAWALS'   TO TL-LABEL
           MOVE WS-ACQ-WD-COUNT            TO TL-COUNT
           MOVE WS-ACQ-AMOUNT              TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE ZERO                       TO TL-AMOUNT
           MOVE '  INTERCHANGE EARNED (BAL ENQ COUNT)' TO TL-LABEL
           MOVE WS-ACQ-BE-COUNT            TO TL-COUNT
           MOVE WS-ACQ-FEES                TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'POSTINGS WRITTEN TO ATMPOST' TO TL-LABEL
           MOVE WS-POSTINGS                TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  CUSTOMER DEBITS'        TO TL-LABEL
           MOVE ZERO                       TO TL-COUNT
           MOVE WS-CUST-DEBITS             TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  CUSTOMER CREDITS'       TO TL-LABEL
           MOVE WS-CUST-CREDITS            TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'MEGABANK ATM FEES CHARGED' TO TL-LABEL
           MOVE WS-OWN-FEES-COUNT          TO TL-COUNT
           MOVE WS-OWN-FEES-TOTAL          TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  NOT CHARGED - EXEMPT ACCOUNT' TO TL-LABEL
           MOVE WS-FEES-EXEMPT             TO TL-COUNT
           MOVE ZERO                       TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  NOT CHARGED - DAILY CAP' TO TL-LABEL
           MOVE WS-FEES-CAPPED             TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'RESTRICTED ACCOUNTS POSTED' TO TL-LABEL
           MOVE WS-RESTRICTED-COUNT        TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'ITEMS TO ATM SUSPENSE 29910-0300' TO TL-LABEL
           MOVE WS-SUSPENSE-COUNT          TO TL-COUNT
           MOVE WS-SUSPENSE-AMOUNT         TO TL-AMOUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE

      *---- SETTLEMENT FOR TREASURY -----------------------------------*
           MOVE '0'                        TO SL-CC
           MOVE 'NET SETTLEMENT (+ RECEIVE / - PAY)' TO SL-LABEL
           MOVE WS-CALC-NET                TO SL-OURS
           MOVE WS-LINK-NET                TO SL-LINK
           IF WS-CALC-NET = WS-LINK-NET
               MOVE 'AGREED'               TO SL-RESULT
           ELSE
               MOVE '*** DIFF'             TO SL-RESULT
           END-IF
           WRITE REPORT-RECORD             FROM WS-SETTLE-LINE
           MOVE ' '                        TO SL-CC
           MOVE 'GL JOURNAL DEBITS / CREDITS' TO SL-LABEL
           MOVE WS-GL-DR                   TO SL-OURS
           MOVE WS-GL-CR                   TO SL-LINK
           IF WS-GL-DR = WS-GL-CR
               MOVE 'BALANCED'             TO SL-RESULT
           ELSE
               MOVE '*** OUT'              TO SL-RESULT
           END-IF
           WRITE REPORT-RECORD             FROM WS-SETTLE-LINE

           CLOSE LINKIN-FILE ACCTMAST-FILE ATMPOST-FILE GLJNL-FILE
                 REPORT-FILE
           MOVE WS-CALC-NET                TO WS-DISP-AMOUNT
           DISPLAY 'ATMSTL10 - NET SETTLEMENT ' WS-DISP-AMOUNT
           DISPLAY 'ATMSTL10 - POSTINGS ' WS-POSTINGS
                   ' EXCEPTIONS ' WS-EXCEPTIONS
                   ' REVERSALS ' WS-ISS-RV-COUNT ' / ' WS-ACQ-RV-COUNT
           IF WS-GL-DR NOT = WS-GL-CR
               MOVE '4000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1004                   TO ABN-ABEND-CODE
               MOVE 'GL JOURNAL OUT OF BALANCE' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           IF WS-EXCEPTIONS > ZERO
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
       8000-READ-LINK.
      *================================================================*
           READ LINKIN-FILE
           EVALUATE TRUE
               WHEN WS-LINKIN-OK
                   MOVE LINKIN-RECORD      TO ATL-LINK-RECORD
                   ADD 1                   TO WS-RECORDS-READ
               WHEN WS-LINKIN-EOF
                   SET WS-END-OF-FILE      TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-LINK'   TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'LINKIN READ FAILED' TO ABN-MESSAGE
                   MOVE WS-LINKIN-STATUS   TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
