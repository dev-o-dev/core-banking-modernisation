       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNSVC01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-10-04.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : LNSVC01                                             *
      * TITLE    : PERSONAL LOAN ENQUIRY - CHANNEL SERVICE LOANENQ     *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : LOANMST  READ / READ UPDATE (QUOTE ONLY)            *
      *            LOANCIF  BROWSE (AIX PATH BY CIF)                   *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-LOANENQ-AREA               *
      *----------------------------------------------------------------*
      * E  ENQUIRY - LOAN DETAILS AND A PROJECTION OF THE NEXT 12      *
      *    INSTALMENTS. THE PROJECTION USES A MONTHLY RATE (RATE/12)   *
      *    WHERE THE BATCH CHARGES DAILY, SO THE INTEREST / PRINCIPAL  *
      *    SPLIT IS INDICATIVE ONLY - THE SCREEN SAYS SO.              *
      * Q  SETTLEMENT QUOTE (CONSUMER CREDIT (EARLY SETTLEMENT)        *
      *    REGULATIONS 2004 AS AMENDED 2011):                          *
      *      PRINCIPAL + ACCRUED INTEREST + INTEREST FROM THE LAST     *
      *      ACCRUAL TO THE SETTLEMENT DATE + 28 DAYS' INTEREST        *
      *      + OUTSTANDING FEES.                                       *
      *    VALID FOR 28 DAYS. THE QUOTE IS STORED ON THE LOAN SO THAT  *
      *    LENDING OPERATIONS CAN HONOUR IT IF THE CUSTOMER PAYS BY    *
      *    CHEQUE OR CHAPS.                                            *
      * L  LIST - THE CUSTOMER'S LOANS (OPEN FIRST IS NOT GUARANTEED - *
      *    AIX ORDER IS LOAN NUMBER).                                  *
      *                                                                *
      * CUSTOMER OWNERSHIP FOR IBANK IS ENFORCED BY THE IBANK GATEWAY  *
      * (AS FOR ALL CHANNEL SERVICES) - THIS PROGRAM DOES NOT KNOW WHO *
      * IS LOGGED ON.                                                  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-10-04 JNB  ORIGINAL VERSION                               *
      * 2011-06-27 JNB  28 DAYS' INTEREST ON SETTLEMENT (WAS 1 MONTH   *
      *                 + 28 DAYS UNDER THE 2004 REGULATIONS)          *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNSVC01'.
      *
       01  WS-CONSTANTS.
           05  WS-SETTLE-EXTRA-DAYS        PIC S9(03) COMP-3 VALUE +28.
           05  WS-QUOTE-VALID-DAYS         PIC S9(03) COMP-3 VALUE +28.
           05  WS-MAX-QUOTE-AHEAD          PIC S9(03) COMP-3 VALUE +60.
           05  WS-DAY-BASIS                PIC S9(03) COMP-3 VALUE +365.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TODAY-INT                PIC S9(09) COMP.
           05  WS-SETTLE-DATE              PIC 9(08).
           05  WS-SETTLE-INT               PIC S9(09) COMP.
           05  WS-LAST-ACCR-INT            PIC S9(09) COMP.
           05  WS-DAYS                     PIC S9(05) COMP-3.
           05  WS-INT-TO-DATE              PIC S9(13)V99 COMP-3.
           05  WS-EXTRA-INT                PIC S9(13)V99 COMP-3.
           05  WS-QUOTE                    PIC S9(13)V99 COMP-3.
           05  WS-BALANCE                  PIC S9(13)V99 COMP-3.
           05  WS-MONTH-INT                PIC S9(13)V99 COMP-3.
           05  WS-PAYMENT                  PIC S9(13)V99 COMP-3.
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-REMAINING                PIC S9(04) COMP.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-CIF-KEY                  PIC X(12).
           05  WS-LOAN-KEY                 PIC X(10).
      *
      *---- MONTH ARITHMETIC ------------------------------------------*
       01  WS-MONTH-ARITH.
           05  WS-MA-DATE                  PIC 9(08).
           05  WS-MA-DATE-R REDEFINES WS-MA-DATE.
               10  WS-MA-CCYY              PIC 9(04).
               10  WS-MA-MM                PIC 9(02).
               10  WS-MA-DD                PIC 9(02).
           05  WS-MA-DAY                   PIC 9(02).
           05  WS-MA-TOTAL-MONTHS          PIC S9(07) COMP-3.
           05  WS-MA-LAST-DAY              PIC 9(02).
       01  WS-DAYS-IN-MONTH-VALUES         PIC X(24)
                                VALUE '312831303130313130313031'.
       01  WS-DAYS-IN-MONTH-TABLE REDEFINES WS-DAYS-IN-MONTH-VALUES.
           05  WS-DIM                      PIC 9(02) OCCURS 12 TIMES.
      *
           COPY LNACCREC.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(4000).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE DFHCOMMAREA                TO CH-COMMAREA
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
           MOVE SPACES                     TO CHLN-DETAIL
           MOVE ZERO                       TO CHLN-SCHED-COUNT
                                              CHLN-LIST-COUNT
                                              CHLN-QT-AMOUNT
                                              CHLN-QT-INT-TO-DATE
                                              CHLN-QT-EXTRA-INT
                                              CHLN-QT-VALID-TO

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC
           COMPUTE WS-TODAY-INT = FUNCTION INTEGER-OF-DATE (WS-TODAY)

           PERFORM 1000-VALIDATE-REQUEST THRU 1000-EXIT
           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHLN-ENQUIRY
                       PERFORM 2000-ENQUIRY THRU 2000-EXIT
                   WHEN CHLN-QUOTE
                       PERFORM 3000-QUOTE THRU 3000-EXIT
                   WHEN CHLN-LIST
                       PERFORM 4000-LIST THRU 4000-EXIT
               END-EVALUATE
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN NOT (CHLN-ENQUIRY OR CHLN-QUOTE OR CHLN-LIST)
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE E, Q OR L'
                                           TO CH-MESSAGE-TEXT
               WHEN CHLN-LIST AND CHLN-CIF-NO = SPACES
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'CUSTOMER NUMBER REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT CHLN-LIST AND CHLN-LOAN-NO NOT NUMERIC
                   MOVE 'INVL'             TO CH-REASON-CD
                   MOVE 'LOAN NUMBER INVALID' TO CH-MESSAGE-TEXT
               WHEN CHLN-QUOTE AND CH-CHANNEL-CD = 'IBANK '
      *            IBANK QUOTES ARE ALWAYS AS AT TODAY.
                   MOVE WS-TODAY           TO CHLN-SETTLE-DATE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 1000-EXIT
           END-IF

           .
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-ENQUIRY.
      *================================================================*
           PERFORM 8000-READ-LOAN
           IF NOT CH-RC-OK
               GO TO 2000-EXIT
           END-IF
           PERFORM 8100-FILL-DETAIL
           PERFORM 2100-PROJECT-SCHEDULE THRU 2100-EXIT
           MOVE 'LOAN DETAILS - SCHEDULE IS INDICATIVE'
                                           TO CH-MESSAGE-TEXT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-PROJECT-SCHEDULE.
      *================================================================*
           IF NOT (LN-ACTIVE OR LN-DEFAULTED)
           OR LN-NEXT-DUE-DATE = ZERO
               GO TO 2100-EXIT
           END-IF
           COMPUTE WS-BALANCE = LN-PRINCIPAL-BAL + LN-FEES-OUTSTANDING
           MOVE LN-PAYMENTS-REMAINING      TO WS-REMAINING
           MOVE LN-NEXT-DUE-DATE           TO WS-MA-DATE
           MOVE LN-PAYMENT-DAY             TO WS-MA-DAY
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > 12
                      OR WS-SUB > WS-REMAINING
                      OR WS-BALANCE <= ZERO
               IF WS-SUB = 1
                   MOVE LN-ACCRUED-INT     TO WS-MONTH-INT
                   COMPUTE WS-MONTH-INT ROUNDED = WS-MONTH-INT
                       + WS-BALANCE * LN-NOMINAL-RATE / 1200
                       * (FUNCTION INTEGER-OF-DATE (LN-NEXT-DUE-DATE)
                          - WS-TODAY-INT) / 30
               ELSE
                   COMPUTE WS-MONTH-INT ROUNDED =
                       WS-BALANCE * LN-NOMINAL-RATE / 1200
               END-IF
               IF WS-MONTH-INT < ZERO
                   MOVE ZERO               TO WS-MONTH-INT
               END-IF
               IF WS-SUB = WS-REMAINING
                   COMPUTE WS-PAYMENT = WS-BALANCE + WS-MONTH-INT
               ELSE
                   MOVE LN-MONTHLY-PAYMENT TO WS-PAYMENT
               END-IF
               MOVE WS-MA-DATE             TO CHLNS-DUE-DATE (WS-SUB)
               MOVE WS-PAYMENT             TO CHLNS-PAYMENT (WS-SUB)
               MOVE WS-MONTH-INT           TO CHLNS-INTEREST (WS-SUB)
               COMPUTE CHLNS-PRINCIPAL (WS-SUB) =
                       WS-PAYMENT - WS-MONTH-INT
               COMPUTE WS-BALANCE =
                       WS-BALANCE - (WS-PAYMENT - WS-MONTH-INT)
               IF WS-BALANCE < ZERO
                   MOVE ZERO               TO WS-BALANCE
               END-IF
               MOVE WS-BALANCE             TO CHLNS-BALANCE (WS-SUB)
               MOVE WS-SUB                 TO CHLN-SCHED-COUNT
               PERFORM 6000-NEXT-MONTH
           END-PERFORM
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       3000-QUOTE.
      *================================================================*
           IF CHLN-SETTLE-DATE NOT NUMERIC
           OR CHLN-SETTLE-DATE = ZERO
               MOVE WS-TODAY               TO WS-SETTLE-DATE
           ELSE
               MOVE CHLN-SETTLE-DATE       TO WS-SETTLE-DATE
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD (WS-SETTLE-DATE) NOT = 0
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'INVD'                 TO CH-REASON-CD
               MOVE 'SETTLEMENT DATE INVALID' TO CH-MESSAGE-TEXT
               GO TO 3000-EXIT
           END-IF
           COMPUTE WS-SETTLE-INT =
                   FUNCTION INTEGER-OF-DATE (WS-SETTLE-DATE)
           IF WS-SETTLE-INT < WS-TODAY-INT
           OR WS-SETTLE-INT > WS-TODAY-INT + WS-MAX-QUOTE-AHEAD
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'DATE'                 TO CH-REASON-CD
               MOVE 'SETTLEMENT DATE MUST BE TODAY TO 60 DAYS AHEAD'
                                           TO CH-MESSAGE-TEXT
               GO TO 3000-EXIT
           END-IF

           EXEC CICS READ
                     FILE('LOANMST')
                     INTO(LN-LOAN-RECORD)
                     LENGTH(LENGTH OF LN-LOAN-RECORD)
                     RIDFLD(CHLN-LOAN-NO)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           PERFORM 8200-CHECK-READ
           IF NOT CH-RC-OK
               GO TO 3000-EXIT
           END-IF
           IF NOT (LN-ACTIVE OR LN-DEFAULTED OR LN-COLLECTIONS)
               EXEC CICS UNLOCK FILE('LOANMST') RESP(WS-RESP)
               END-EXEC
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'CLSD'                 TO CH-REASON-CD
               MOVE 'LOAN IS NOT OPEN - NO SETTLEMENT FIGURE'
                                           TO CH-MESSAGE-TEXT
               GO TO 3000-EXIT
           END-IF

           MOVE ZERO                       TO WS-INT-TO-DATE
                                              WS-EXTRA-INT
           IF NOT (LN-INT-FROZEN OR LN-COLLECTIONS)
               COMPUTE WS-LAST-ACCR-INT =
                   FUNCTION INTEGER-OF-DATE (LN-LAST-ACCRUAL-DATE)
               COMPUTE WS-DAYS = WS-SETTLE-INT - WS-LAST-ACCR-INT
               IF WS-DAYS > ZERO
                   COMPUTE WS-INT-TO-DATE ROUNDED =
                       LN-PRINCIPAL-BAL * LN-NOMINAL-RATE / 100
                       * WS-DAYS / WS-DAY-BASIS
               END-IF
               COMPUTE WS-EXTRA-INT ROUNDED =
                   LN-PRINCIPAL-BAL * LN-NOMINAL-RATE / 100
                   * WS-SETTLE-EXTRA-DAYS / WS-DAY-BASIS
           END-IF
           ADD LN-ACCRUED-INT              TO WS-INT-TO-DATE
           COMPUTE WS-QUOTE = LN-PRINCIPAL-BAL + WS-INT-TO-DATE
                            + WS-EXTRA-INT + LN-FEES-OUTSTANDING

           MOVE WS-TODAY                   TO LN-SETTLE-QUOTE-DATE
           MOVE WS-QUOTE                   TO LN-SETTLE-QUOTE-AMT
           COMPUTE LN-SETTLE-VALID-TO = FUNCTION DATE-OF-INTEGER
                   (WS-SETTLE-INT + WS-QUOTE-VALID-DAYS)
           MOVE WS-TODAY                   TO LN-LAST-MAINT-DATE
           MOVE CH-USER-ID                 TO LN-LAST-MAINT-USER
           EXEC CICS REWRITE
                     FILE('LOANMST')
                     FROM(LN-LOAN-RECORD)
                     LENGTH(LENGTH OF LN-LOAN-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-FILE-ERROR
               GO TO 3000-EXIT
           END-IF

           PERFORM 8100-FILL-DETAIL
           MOVE WS-QUOTE                   TO CHLN-QT-AMOUNT
           MOVE WS-INT-TO-DATE             TO CHLN-QT-INT-TO-DATE
           MOVE WS-EXTRA-INT               TO CHLN-QT-EXTRA-INT
           MOVE LN-SETTLE-VALID-TO         TO CHLN-QT-VALID-TO
           IF LN-COLLECTIONS
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'COLL'                 TO CH-REASON-CD
               MOVE 'LOAN WITH COLLECTIONS AGENCY - REFER TO AGENCY'
                                           TO CH-MESSAGE-TEXT
           ELSE
               MOVE 'SETTLEMENT QUOTE ISSUED' TO CH-MESSAGE-TEXT
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-LIST.
      *================================================================*
           MOVE CHLN-CIF-NO                TO WS-CIF-KEY
           MOVE 'N'                        TO WS-END-BROWSE-IND
           EXEC CICS STARTBR
                     FILE('LOANCIF')
                     RIDFLD(WS-CIF-KEY)
                     KEYLENGTH(12)
                     EQUAL
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NORMAL)
                   CONTINUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'NONE'             TO CH-REASON-CD
                   MOVE 'NO LOANS FOR THIS CUSTOMER'
                                           TO CH-MESSAGE-TEXT
                   GO TO 4000-EXIT
               WHEN OTHER
                   PERFORM 9000-FILE-ERROR
                   GO TO 4000-EXIT
           END-EVALUATE

           PERFORM UNTIL WS-END-BROWSE OR CHLN-LIST-COUNT >= 10
               EXEC CICS READNEXT
                         FILE('LOANCIF')
                         INTO(LN-LOAN-RECORD)
                         LENGTH(LENGTH OF LN-LOAN-RECORD)
                         RIDFLD(WS-CIF-KEY)
                         KEYLENGTH(12)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(NORMAL)
                   WHEN WS-RESP = DFHRESP(DUPKEY)
                       IF LN-CIF-NO NOT = CHLN-CIF-NO
                           SET WS-END-BROWSE TO TRUE
                       ELSE
                           ADD 1           TO CHLN-LIST-COUNT
                           MOVE CHLN-LIST-COUNT TO WS-SUB
                           MOVE LN-LOAN-NO TO CHLNL-LOAN-NO (WS-SUB)
                           MOVE LN-STATUS  TO CHLNL-STATUS (WS-SUB)
                           MOVE LN-PROD-CD TO CHLNL-PROD-CD (WS-SUB)
                           MOVE LN-PRINCIPAL-BAL
                                           TO CHLNL-PRINCIPAL (WS-SUB)
                           MOVE LN-NEXT-DUE-DATE
                                           TO CHLNL-NEXT-DUE-DATE
                                              (WS-SUB)
                           MOVE LN-ARREARS-AMT
                                           TO CHLNL-ARREARS (WS-SUB)
                       END-IF
                   WHEN OTHER
                       SET WS-END-BROWSE   TO TRUE
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR FILE('LOANCIF') RESP(WS-RESP)
           END-EXEC
           IF CHLN-LIST-COUNT >= 10 AND NOT WS-END-BROWSE
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'MORE'                 TO CH-REASON-CD
               MOVE 'FIRST 10 LOANS SHOWN' TO CH-MESSAGE-TEXT
           END-IF
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       6000-NEXT-MONTH.
      *================================================================*
           COMPUTE WS-MA-TOTAL-MONTHS =
                   WS-MA-CCYY * 12 + WS-MA-MM
           COMPUTE WS-MA-CCYY = WS-MA-TOTAL-MONTHS / 12
           COMPUTE WS-MA-MM =
                   WS-MA-TOTAL-MONTHS - (WS-MA-CCYY * 12) + 1
           MOVE WS-DIM (WS-MA-MM)          TO WS-MA-LAST-DAY
           IF WS-MA-MM = 2
               IF FUNCTION MOD (WS-MA-CCYY, 4) = 0
               AND (FUNCTION MOD (WS-MA-CCYY, 100) NOT = 0
                    OR FUNCTION MOD (WS-MA-CCYY, 400) = 0)
                   MOVE 29                 TO WS-MA-LAST-DAY
               END-IF
           END-IF
           IF WS-MA-DAY > WS-MA-LAST-DAY
               MOVE WS-MA-LAST-DAY         TO WS-MA-DD
           ELSE
               MOVE WS-MA-DAY              TO WS-MA-DD
           END-IF
           .
      *================================================================*
       8000-READ-LOAN.
      *================================================================*
           MOVE CHLN-LOAN-NO               TO WS-LOAN-KEY
           EXEC CICS READ
                     FILE('LOANMST')
                     INTO(LN-LOAN-RECORD)
                     LENGTH(LENGTH OF LN-LOAN-RECORD)
                     RIDFLD(WS-LOAN-KEY)
                     RESP(WS-RESP)
           END-EXEC
           PERFORM 8200-CHECK-READ
           .
      *================================================================*
       8100-FILL-DETAIL.
      *================================================================*
           MOVE LN-STATUS                  TO CHLN-STATUS
           MOVE LN-PROD-CD                 TO CHLN-PROD-CD
           MOVE LN-ADVANCE-AMT             TO CHLN-ADVANCE-AMT
           MOVE LN-ADVANCE-DATE            TO CHLN-ADVANCE-DATE
           MOVE LN-TERM-MONTHS             TO CHLN-TERM-MONTHS
           MOVE LN-NOMINAL-RATE            TO CHLN-NOMINAL-RATE
           MOVE LN-APR                     TO CHLN-APR
           MOVE LN-MONTHLY-PAYMENT         TO CHLN-MONTHLY-PMT
           MOVE LN-PRINCIPAL-BAL           TO CHLN-PRINCIPAL
           MOVE LN-ACCRUED-INT             TO CHLN-ACCRUED-INT
           MOVE LN-FEES-OUTSTANDING        TO CHLN-FEES
           MOVE LN-ARREARS-AMT             TO CHLN-ARREARS
           MOVE LN-MISSED-COUNT            TO CHLN-MISSED-COUNT
           MOVE LN-NEXT-DUE-DATE           TO CHLN-NEXT-DUE-DATE
           MOVE LN-PAYMENTS-REMAINING      TO CHLN-PMTS-REMAINING
           MOVE LN-FINAL-DUE-DATE          TO CHLN-FINAL-DUE-DATE
           MOVE LN-REPAY-SORT-CODE         TO CHLN-REPAY-SORT-CODE
           MOVE LN-REPAY-ACCT-NO           TO CHLN-REPAY-ACCT-NO
           MOVE LN-TOTAL-INT-PAID          TO CHLN-TOTAL-INT-PAID
           MOVE LN-FORBEARANCE-IND         TO CHLN-FORBEARANCE-IND
           MOVE LN-CIF-NO                  TO CHLN-CIF-NO
           .
      *================================================================*
       8200-CHECK-READ.
      *================================================================*
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NORMAL)
                   CONTINUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'NOTF'             TO CH-REASON-CD
                   MOVE 'LOAN NOT FOUND'   TO CH-MESSAGE-TEXT
               WHEN OTHER
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
           MOVE '0012'                     TO CH-RETURN-CD
           MOVE 'FILE'                     TO CH-REASON-CD
           STRING 'LOAN FILE ERROR - RESP ' WS-RESP
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
