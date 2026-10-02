       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SOMNT01.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2002-10-14.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : SOMNT01                                             *
      * TITLE    : STANDING ORDER MAINTENANCE - CHANNEL SERVICE SOMAINT*
      * TYPE     : CICS - LINKED FROM CHROUTER (CHANNELS) AND SOSCRN1  *
      *            (BRANCH SCREEN SO01)                                *
      * FILES    : SOMAST   READ / BROWSE / WRITE / REWRITE            *
      *            ACCTMST  READ (PAYER AND INTERNAL BENEFICIARY)      *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-SOMAINT-AREA               *
      * CALLS    : DDMODCK (MODULUS CHECK), DTEUTIL (BUSINESS DAYS)    *
      *----------------------------------------------------------------*
      * FUNCTIONS                                                      *
      *   L  LIST - UP TO 25 STANDING ORDERS FOR THE PAYER ACCOUNT,    *
      *      STARTING AFTER CHSO-SO-SEQ (ZERO = FROM THE START).       *
      *      CHSO-MORE-IND = 'Y' AND CHSO-RESTART-SEQ SET IF MORE.     *
      *   I  ENQUIRE ONE STANDING ORDER (RETURNED AS ENTRY 1)          *
      *   C  CREATE                                                    *
      *   A  AMEND - REGULAR AMOUNT, NEXT PAYMENT DATE, BENEFICIARY    *
      *      NAME AND REFERENCE. AMENDING A SUSPENDED ORDER REINSTATES *
      *      IT. THE BENEFICIARY ACCOUNT CANNOT BE CHANGED (CANCEL AND *
      *      CREATE A NEW ONE - FRAUD RULE FR-2004-07).                *
      *   X  CANCEL                                                    *
      *----------------------------------------------------------------*
      * BUSINESS RULES                                                 *
      *   - PAYER ACCOUNT OPEN, NOT RESTRICTED FOR DEBITS              *
      *   - FIRST PAYMENT DATE AT LEAST TOMORROW, AT MOST 1 YEAR AHEAD *
      *   - AMOUNT WITHIN THE CHANNEL LIMIT (WS-CHANNEL-LIMITS)        *
      *   - BENEFICIARY SORT CODE / ACCOUNT PASS THE MODULUS CHECK     *
      *   - BENEFICIARY ON A MEGABANK SORT CODE (40-xx-xx) MUST EXIST  *
      *     ON ACCTMST - THE ORDER IS ROUTED INTERNALLY ('I'), ELSE    *
      *     VIA FASTER PAYMENTS ('F')                                  *
      *   - MAXIMUM 50 ACTIVE ORDERS PER ACCOUNT                       *
      *   - AMENDMENTS AND CANCELLATIONS FOR TODAY'S PAYMENT ARE NOT   *
      *     ACCEPTED AFTER THE CUT-OFF (WS-CUTOFF-TIME) BECAUSE        *
      *     SOEXE010 MAY ALREADY HAVE TAKEN THE PAYMENT (REASON CUTF)  *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   ANFD ACCOUNT NOT FOUND       ACLS ACCOUNT CLOSED / NOT OPEN  *
      *   RSTD DEBITS RESTRICTED       SONF STANDING ORDER NOT FOUND   *
      *   SOCN ALREADY CANCELLED       SOMX TOO MANY ACTIVE ORDERS     *
      *   MODF BENEFICIARY FAILS MODULUS CHECK                         *
      *   BENF INTERNAL BENEFICIARY ACCOUNT NOT FOUND / CLOSED         *
      *   LIMX AMOUNT OVER CHANNEL LIMIT                               *
      *   DATX FIRST / NEXT DATE OUT OF RANGE                          *
      *   CUTF PAST TODAY'S CUT-OFF                                    *
      *   INVx INVALID REQUEST FIELD   FUNC INVALID FUNCTION           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2002-10-14 DKP  ORIGINAL VERSION (IBANK PHASE 3 - SO MAINT)    *
      *                 AS IBSOM01 - CALLED VIA OLD IBANK GATEWAY      *
      * 2004-06-21 SAT  MODULUS CHECK VIA DDMODCK                      *
      * 2004-11-08 SAT  BENEFICIARY ACCOUNT NOT AMENDABLE (FR-2004-07) *
      * 2008-05-27 AOS  FPS ROUTING FOR EXTERNAL BENEFICIARIES         *
      * 2010-03-08 AOS  RENAMED SOMNT01, NOW A CHROUTER SERVICE USING  *
      *                 CHCOMMA2 (CS-DN-2010-03)                       *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'SOMNT01 '.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-ACTIVE               PIC 9(04) VALUE 50.
           05  WS-MAX-LIST                 PIC 9(02) VALUE 25.
           05  WS-CUTOFF-TIME              PIC 9(06) VALUE 223000.
           05  WS-MEGABANK-SC-PREFIX       PIC X(02) VALUE '40'.
      *
      *---- CHANNEL LIMITS FOR A SINGLE STANDING ORDER PAYMENT --------*
       01  WS-CHANNEL-LIMIT-VALUES.
           05  FILLER  PIC X(06) VALUE 'IBANK '.
           05  FILLER  PIC 9(07)V99 VALUE 10000.00.
           05  FILLER  PIC X(06) VALUE 'TELE  '.
           05  FILLER  PIC 9(07)V99 VALUE 5000.00.
           05  FILLER  PIC X(06) VALUE 'WAP   '.
           05  FILLER  PIC 9(07)V99 VALUE 1000.00.
           05  FILLER  PIC X(06) VALUE 'BRANCH'.
           05  FILLER  PIC 9(07)V99 VALUE 9999999.99.
       01  WS-CHANNEL-LIMIT-TABLE REDEFINES WS-CHANNEL-LIMIT-VALUES.
           05  WS-CHL-ENTRY                OCCURS 4 TIMES
                                           INDEXED BY CHL-IDX.
               10  WS-CHL-CHANNEL          PIC X(06).
               10  WS-CHL-LIMIT            PIC 9(07)V99.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP2                    PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TIME-HHMMSS              PIC 9(06).
           05  WS-TIME-HHMMSS-X REDEFINES WS-TIME-HHMMSS
                                           PIC X(06).
           05  WS-TOMORROW                 PIC 9(08).
           05  WS-MAX-DATE                 PIC 9(08).
           05  WS-DUE-DATE                 PIC 9(08).
           05  WS-LIMIT                    PIC 9(07)V99.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-LIST-COUNT               PIC 9(02).
           05  WS-NEW-SEQ                  PIC 9(04).
           05  WS-LOOP-GUARD               PIC S9(04) COMP.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-CONTROL-HELD-IND         PIC X(01) VALUE 'N'.
               88  WS-CONTROL-HELD             VALUE 'Y'.
           05  WS-CONTROL-NEW-IND          PIC X(01) VALUE 'N'.
               88  WS-CONTROL-NEW              VALUE 'Y'.
      *
       01  WS-SO-KEY.
           05  WS-SK-SORT-CODE             PIC X(06).
           05  WS-SK-ACCT-NO               PIC X(08).
           05  WS-SK-SO-SEQ                PIC 9(04).
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
      *
      *---- SAVED CONTROL RECORD (SOM-SO-RECORD IS REUSED FOR THE SO) -*
       01  WS-CONTROL-RECORD               PIC X(400).
       01  WS-SAVED-SO                     PIC X(400).
      *
           COPY SOMSTREC.
      *
           COPY DDACCREC.
      *
           COPY MODPARM.
      *
           COPY DTEPARM.
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
           MOVE ZERO                       TO CHSO-SO-COUNT
                                              CHSO-RESTART-SEQ
           MOVE 'N'                        TO CHSO-MORE-IND

           PERFORM 1000-GET-DATES
           PERFORM 1100-VALIDATE-COMMON

           IF CH-RC-OK
               PERFORM 1200-READ-PAYER
           END-IF

           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHSO-LIST
                       PERFORM 2000-LIST-ORDERS THRU 2000-EXIT
                   WHEN CHSO-ENQUIRE
                       PERFORM 3000-ENQUIRE-ORDER
                   WHEN CHSO-CREATE
                       PERFORM 4000-CREATE-ORDER THRU 4000-EXIT
                   WHEN CHSO-AMEND
                       PERFORM 5000-AMEND-ORDER THRU 5000-EXIT
                   WHEN CHSO-CANCEL
                       PERFORM 6000-CANCEL-ORDER THRU 6000-EXIT
               END-EVALUATE
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-GET-DATES.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC

           MOVE 'D'                        TO DTE-FUNCTION
           MOVE WS-TODAY                   TO DTE-DATE-IN
           MOVE 1                          TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO WS-TOMORROW

           MOVE 'D'                        TO DTE-FUNCTION
           MOVE WS-TODAY                   TO DTE-DATE-IN
           MOVE 366                        TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO WS-MAX-DATE
           .
      *================================================================*
       1100-VALIDATE-COMMON.
      *================================================================*
           EVALUATE TRUE
               WHEN NOT (CHSO-LIST OR CHSO-ENQUIRE OR CHSO-CREATE
                         OR CHSO-AMEND OR CHSO-CANCEL)
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE L, I, C, A OR X'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-SORT-CODE NOT NUMERIC
               WHEN CHSO-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'PAYER SORT CODE / ACCOUNT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN (CHSO-ENQUIRE OR CHSO-AMEND OR CHSO-CANCEL)
               AND  (CHSO-SO-SEQ NOT NUMERIC OR CHSO-SO-SEQ = ZERO)
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'STANDING ORDER SEQUENCE REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-LIST
               AND  CHSO-SO-SEQ NOT NUMERIC
                   MOVE ZERO               TO CHSO-SO-SEQ
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
           END-IF
           .
      *================================================================*
       1200-READ-PAYER.
      *================================================================*
           MOVE CHSO-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHSO-ACCT-NO               TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
      *            ENQUIRY AND LIST ARE ALLOWED ON ANY ACCOUNT STATUS.
                   IF CHSO-CREATE OR CHSO-AMEND
                       EVALUATE TRUE
                           WHEN NOT ACM-OPEN
                               MOVE '0008' TO CH-RETURN-CD
                               MOVE 'ACLS' TO CH-REASON-CD
                               MOVE 'PAYER ACCOUNT IS NOT OPEN'
                                           TO CH-MESSAGE-TEXT
                           WHEN ACM-NO-DEBITS OR ACM-NO-POSTINGS
                               MOVE '0008' TO CH-RETURN-CD
                               MOVE 'RSTD' TO CH-REASON-CD
                               MOVE 'DEBITS RESTRICTED ON PAYER ACCOUNT'
                                           TO CH-MESSAGE-TEXT
                           WHEN NOT ACM-CURRENT
                               MOVE '0008' TO CH-RETURN-CD
                               MOVE 'ACTX' TO CH-REASON-CD
                               MOVE 'CURRENT ACCOUNTS ONLY'
                                           TO CH-MESSAGE-TEXT
                           WHEN OTHER
                               CONTINUE
                       END-EVALUATE
                   END-IF
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'PAYER ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       2000-LIST-ORDERS.
      *================================================================*
           MOVE CHSO-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHSO-ACCT-NO               TO WS-SK-ACCT-NO
           COMPUTE WS-SK-SO-SEQ = CHSO-SO-SEQ + 1
           MOVE ZERO                       TO WS-LIST-COUNT
           MOVE 'N'                        TO WS-END-BROWSE-IND

           EXEC CICS STARTBR
                     FILE('SOMAST')
                     RIDFLD(WS-SO-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   PERFORM 2900-NONE-FOUND
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'SOMAST'           TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE

           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('SOMAST')
                         INTO(SOM-SO-RECORD)
                         LENGTH(LENGTH OF SOM-SO-RECORD)
                         RIDFLD(WS-SO-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   WHEN SOM-PAYER-SORT-CODE NOT = CHSO-SORT-CODE
                   WHEN SOM-PAYER-ACCT-NO NOT = CHSO-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN SOM-CONTROL-REC
                       CONTINUE
                   WHEN WS-LIST-COUNT >= WS-MAX-LIST
      *                A 26TH ORDER EXISTS - TELL THE CHANNEL TO PAGE.
                       MOVE 'Y'            TO CHSO-MORE-IND
                       MOVE CHSOE-SO-SEQ (WS-MAX-LIST)
                                           TO CHSO-RESTART-SEQ
                       SET WS-END-BROWSE   TO TRUE
                   WHEN OTHER
                       ADD 1               TO WS-LIST-COUNT
                       PERFORM 2100-FORMAT-ENTRY
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR
                     FILE('SOMAST')
                     RESP(WS-RESP)
           END-EXEC

           MOVE WS-LIST-COUNT              TO CHSO-SO-COUNT
           IF WS-LIST-COUNT = ZERO
               PERFORM 2900-NONE-FOUND
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-FORMAT-ENTRY.
      *================================================================*
      *    ON ENTRY WS-LIST-COUNT IS THE ENTRY NUMBER TO FILL.
           MOVE SOM-SO-SEQ          TO CHSOE-SO-SEQ (WS-LIST-COUNT)
           MOVE SOM-STATUS          TO CHSOE-STATUS (WS-LIST-COUNT)
           MOVE SOM-BENE-NAME       TO CHSOE-BENE-NAME (WS-LIST-COUNT)
           MOVE SOM-BENE-SORT-CODE
                                TO CHSOE-BENE-SORT-CODE (WS-LIST-COUNT)
           MOVE SOM-BENE-ACCT-NO
                                TO CHSOE-BENE-ACCT-NO (WS-LIST-COUNT)
           MOVE SOM-BENE-REF        TO CHSOE-BENE-REF (WS-LIST-COUNT)
           MOVE SOM-FREQ-CD         TO CHSOE-FREQ-CD (WS-LIST-COUNT)
           IF SOM-ACTIVE OR SOM-SUSPENDED
               MOVE SOM-NEXT-DUE-DATE
                                    TO CHSOE-NEXT-DUE (WS-LIST-COUNT)
           ELSE
               MOVE ZERO            TO CHSOE-NEXT-DUE (WS-LIST-COUNT)
           END-IF
           MOVE SOM-REGULAR-AMT     TO CHSOE-AMOUNT (WS-LIST-COUNT)
           MOVE SOM-LAST-PAID-DATE  TO CHSOE-LAST-PAID (WS-LIST-COUNT)
           MOVE SOM-PAYMENTS-REMAINING
                                    TO CHSOE-REMAINING (WS-LIST-COUNT)
           .
      *================================================================*
       2900-NONE-FOUND.
      *================================================================*
           MOVE '0004'                     TO CH-RETURN-CD
           MOVE 'NONE'                     TO CH-REASON-CD
           MOVE 'NO STANDING ORDERS ON THIS ACCOUNT'
                                           TO CH-MESSAGE-TEXT
           .
      *================================================================*
       3000-ENQUIRE-ORDER.
      *================================================================*
           PERFORM 8100-READ-SO-NOLOCK
           IF CH-RC-OK
               MOVE 1                      TO WS-LIST-COUNT
               PERFORM 2100-FORMAT-ENTRY
               MOVE 1                      TO CHSO-SO-COUNT
      *        THE FULL DETAIL GOES IN THE REQUEST FIELDS SO THE
      *        CHANNEL CAN PRE-FILL AN AMEND FORM.
               MOVE SOM-BENE-NAME          TO CHSO-BENE-NAME
               MOVE SOM-BENE-SORT-CODE     TO CHSO-BENE-SORT-CODE
               MOVE SOM-BENE-ACCT-NO       TO CHSO-BENE-ACCT-NO
               MOVE SOM-BENE-REF           TO CHSO-BENE-REF
               MOVE SOM-FREQ-CD            TO CHSO-FREQ-CD
               MOVE SOM-FIRST-PAY-DATE     TO CHSO-FIRST-PAY-DATE
               MOVE SOM-FIRST-AMT          TO CHSO-FIRST-AMT
               MOVE SOM-REGULAR-AMT        TO CHSO-REGULAR-AMT
               MOVE SOM-FINAL-AMT          TO CHSO-FINAL-AMT
               MOVE SOM-FINAL-PAY-DATE     TO CHSO-FINAL-PAY-DATE
               MOVE SOM-PAYMENTS-REMAINING TO CHSO-NUM-PAYMENTS
               MOVE SOM-CANCEL-RSN         TO CHSO-CANCEL-RSN
           END-IF
           .
      *================================================================*
       4000-CREATE-ORDER.
      *================================================================*
           PERFORM 4100-VALIDATE-CREATE THRU 4100-EXIT
           IF NOT CH-RC-OK
               GO TO 4000-EXIT
           END-IF

           PERFORM 4200-CHECK-BENEFICIARY THRU 4200-EXIT
           IF NOT CH-RC-OK
               GO TO 4000-EXIT
           END-IF

           PERFORM 4300-ALLOCATE-SEQUENCE THRU 4300-EXIT
           IF NOT CH-RC-OK
               GO TO 4000-EXIT
           END-IF

           PERFORM 4400-BUILD-AND-WRITE
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-VALIDATE-CREATE.
      *================================================================*
           EVALUATE TRUE
               WHEN CHSO-BENE-NAME = SPACES OR LOW-VALUES
                   MOVE 'INVN'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY NAME REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-BENE-SORT-CODE NOT NUMERIC
               WHEN CHSO-BENE-ACCT-NO NOT NUMERIC
                   MOVE 'INVB'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY SORT CODE / ACCOUNT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-BENE-SORT-CODE = CHSO-SORT-CODE
               AND  CHSO-BENE-ACCT-NO = CHSO-ACCT-NO
                   MOVE 'INVB'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY IS THE PAYER ACCOUNT'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-BENE-REF = SPACES OR LOW-VALUES
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'PAYMENT REFERENCE REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-FREQ-CD NOT = 'W1' AND 'W2' AND 'W4'
                                   AND 'M1' AND 'M3' AND 'M6' AND 'MA'
                   MOVE 'INVF'             TO CH-REASON-CD
                   MOVE 'FREQUENCY INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-FIRST-PAY-DATE NOT NUMERIC
               WHEN CHSO-FIRST-PAY-DATE < WS-TOMORROW
               WHEN CHSO-FIRST-PAY-DATE > WS-MAX-DATE
                   MOVE 'DATX'             TO CH-REASON-CD
                   MOVE 'FIRST PAYMENT DATE MUST BE TOMORROW TO 1 YEAR'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-REGULAR-AMT NOT NUMERIC
               WHEN CHSO-REGULAR-AMT NOT > ZERO
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'REGULAR AMOUNT MUST BE GREATER THAN ZERO'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-FIRST-AMT NOT NUMERIC
               WHEN CHSO-FINAL-AMT NOT NUMERIC
               WHEN CHSO-FIRST-AMT < ZERO
               WHEN CHSO-FINAL-AMT < ZERO
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'FIRST / FINAL AMOUNT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-NUM-PAYMENTS NOT NUMERIC
               WHEN CHSO-NUM-PAYMENTS = ZERO
                   MOVE 'INVP'             TO CH-REASON-CD
                   MOVE 'NUMBER OF PAYMENTS 1-9998 OR 9999 FOR UFN'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-FINAL-PAY-DATE NOT NUMERIC
                   MOVE 'DATX'             TO CH-REASON-CD
                   MOVE 'FINAL PAYMENT DATE INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-FINAL-PAY-DATE NOT = ZERO
               AND  CHSO-FINAL-PAY-DATE < CHSO-FIRST-PAY-DATE
                   MOVE 'DATX'             TO CH-REASON-CD
                   MOVE 'FINAL DATE IS BEFORE FIRST DATE'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 4100-EXIT
           END-IF

      *---- CHANNEL LIMIT ---------------------------------------------*
           MOVE ZERO                       TO WS-LIMIT
           SET CHL-IDX                     TO 1
           SEARCH WS-CHL-ENTRY
               AT END
                   MOVE ZERO               TO WS-LIMIT
               WHEN WS-CHL-CHANNEL (CHL-IDX) = CH-CHANNEL-CD
                   MOVE WS-CHL-LIMIT (CHL-IDX) TO WS-LIMIT
           END-SEARCH

           IF CHSO-REGULAR-AMT > WS-LIMIT
           OR CHSO-FIRST-AMT > WS-LIMIT
           OR CHSO-FINAL-AMT > WS-LIMIT
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'LIMX'                 TO CH-REASON-CD
               MOVE 'AMOUNT EXCEEDS THE LIMIT FOR THIS CHANNEL'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       4100-EXIT.
           EXIT.
      *================================================================*
       4200-CHECK-BENEFICIARY.
      *================================================================*
           MOVE 'V'                        TO MOD-FUNCTION
           MOVE CHSO-BENE-SORT-CODE        TO MOD-SORT-CODE
           MOVE CHSO-BENE-ACCT-NO          TO MOD-ACCT-NO
           CALL 'DDMODCK' USING MOD-PARM-BLOCK

           IF MOD-INVALID OR MOD-INVALID-INPUT
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'MODF'                 TO CH-REASON-CD
               MOVE 'BENEFICIARY ACCOUNT DETAILS FAIL VALIDATION'
                                           TO CH-MESSAGE-TEXT
               GO TO 4200-EXIT
           END-IF

      *    MEGABANK SORT CODE - BENEFICIARY MUST BE ON OUR BOOKS.
           IF CHSO-BENE-SORT-CODE (1:2) = WS-MEGABANK-SC-PREFIX
               MOVE CHSO-BENE-SORT-CODE    TO WS-AK-SORT-CODE
               MOVE CHSO-BENE-ACCT-NO      TO WS-AK-ACCT-NO
               EXEC CICS READ
                         FILE('ACCTMST')
                         INTO(ACM-ACCOUNT-RECORD)
                         LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                         RIDFLD(WS-ACCT-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE WS-RESP
                   WHEN DFHRESP(NORMAL)
                       IF ACM-CLOSED OR ACM-NO-CREDITS
                       OR ACM-NO-POSTINGS
                           MOVE '0008'     TO CH-RETURN-CD
                           MOVE 'BENF'     TO CH-REASON-CD
                           MOVE 'BENEFICIARY CANNOT ACCEPT CREDITS'
                                           TO CH-MESSAGE-TEXT
                       ELSE
                           SET SOM-ROUTE-INTERNAL TO TRUE
                       END-IF
                   WHEN DFHRESP(NOTFND)
                       MOVE '0008'         TO CH-RETURN-CD
                       MOVE 'BENF'         TO CH-REASON-CD
                       MOVE 'BENEFICIARY ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   WHEN OTHER
                       MOVE 'ACCTMST'      TO WS-FILE-NAME
                       PERFORM 9000-FILE-ERROR
               END-EVALUATE
           ELSE
               SET SOM-ROUTE-FPS           TO TRUE
           END-IF
           .
       4200-EXIT.
           EXIT.
      *================================================================*
       4300-ALLOCATE-SEQUENCE.
      *================================================================*
      *    THE CONTROL RECORD (SEQ 0000) IS READ FOR UPDATE, WHICH
      *    SERIALISES CREATES FOR THE SAME ACCOUNT ACROSS CICS TASKS.
           MOVE CHSO-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHSO-ACCT-NO               TO WS-SK-ACCT-NO
           MOVE ZERO                       TO WS-SK-SO-SEQ
           MOVE 'N'                        TO WS-CONTROL-NEW-IND

           EXEC CICS READ
                     FILE('SOMAST')
                     INTO(WS-CONTROL-RECORD)
                     LENGTH(LENGTH OF WS-CONTROL-RECORD)
                     RIDFLD(WS-SO-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC

      *    THE ROUTE CODE SET BY 4200 IS HELD IN SOM-SO-RECORD - SAVE
      *    IT BEFORE THE CONTROL RECORD IS MAPPED OVER THE SAME AREA.
           MOVE SOM-ROUTE-CD               TO CHSO-CANCEL-RSN (1:1)

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   SET WS-CONTROL-HELD     TO TRUE
                   MOVE WS-CONTROL-RECORD  TO SOM-SO-RECORD
               WHEN DFHRESP(NOTFND)
                   SET WS-CONTROL-NEW      TO TRUE
                   MOVE SPACES             TO SOM-SO-RECORD
                   MOVE CHSO-SORT-CODE     TO SOM-PAYER-SORT-CODE
                   MOVE CHSO-ACCT-NO       TO SOM-PAYER-ACCT-NO
                   MOVE ZERO               TO SOM-SO-SEQ
                                              SOM-NEXT-DUE-DATE
                                              SOM-LAST-SO-SEQ
                                              SOM-ACTIVE-COUNT
                   MOVE WS-TODAY           TO SOM-CTL-CREATE-DATE
               WHEN OTHER
                   MOVE 'SOMAST'           TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 4300-EXIT
           END-EVALUATE

           IF SOM-ACTIVE-COUNT >= WS-MAX-ACTIVE
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'SOMX'                 TO CH-REASON-CD
               MOVE 'MAXIMUM NUMBER OF STANDING ORDERS REACHED'
                                           TO CH-MESSAGE-TEXT
               IF WS-CONTROL-HELD
                   EXEC CICS UNLOCK
                             FILE('SOMAST')
                             RESP(WS-RESP)
                   END-EXEC
               END-IF
               GO TO 4300-EXIT
           END-IF

           IF SOM-LAST-SO-SEQ >= 9999
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'SOMX'                 TO CH-REASON-CD
               MOVE 'SEQUENCE NUMBERS EXHAUSTED - REFER TO DDA OPS'
                                           TO CH-MESSAGE-TEXT
               GO TO 4300-EXIT
           END-IF

           ADD 1                           TO SOM-LAST-SO-SEQ
           ADD 1                           TO SOM-ACTIVE-COUNT
           MOVE SOM-LAST-SO-SEQ            TO WS-NEW-SEQ

           IF WS-CONTROL-NEW
               EXEC CICS WRITE
                         FILE('SOMAST')
                         FROM(SOM-SO-RECORD)
                         LENGTH(LENGTH OF SOM-SO-RECORD)
                         RIDFLD(SOM-KEY)
                         RESP(WS-RESP)
               END-EXEC
           ELSE
               EXEC CICS REWRITE
                         FILE('SOMAST')
                         FROM(SOM-SO-RECORD)
                         LENGTH(LENGTH OF SOM-SO-RECORD)
                         RESP(WS-RESP)
               END-EXEC
           END-IF

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'SOMAST'               TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
           END-IF
           .
       4300-EXIT.
           EXIT.
      *================================================================*
       4400-BUILD-AND-WRITE.
      *================================================================*
           MOVE SPACES                     TO SOM-SO-RECORD
           MOVE CHSO-SORT-CODE             TO SOM-PAYER-SORT-CODE
           MOVE CHSO-ACCT-NO               TO SOM-PAYER-ACCT-NO
           MOVE WS-NEW-SEQ                 TO SOM-SO-SEQ
           SET SOM-ACTIVE                  TO TRUE
           MOVE CHSO-CANCEL-RSN (1:1)      TO SOM-ROUTE-CD
           MOVE SPACES                     TO CHSO-CANCEL-RSN
           MOVE CHSO-BENE-NAME             TO SOM-BENE-NAME
           MOVE CHSO-BENE-SORT-CODE        TO SOM-BENE-SORT-CODE
           MOVE CHSO-BENE-ACCT-NO          TO SOM-BENE-ACCT-NO
           MOVE CHSO-BENE-REF              TO SOM-BENE-REF
           MOVE CHSO-FREQ-CD               TO SOM-FREQ-CD
           MOVE CHSO-FIRST-PAY-DATE (7:2)  TO SOM-NOMINAL-DAY
           MOVE CHSO-FIRST-PAY-DATE        TO SOM-FIRST-PAY-DATE
                                              SOM-NOMINAL-DUE-DATE
           MOVE CHSO-FIRST-AMT             TO SOM-FIRST-AMT
           MOVE CHSO-REGULAR-AMT           TO SOM-REGULAR-AMT
           MOVE CHSO-FINAL-AMT             TO SOM-FINAL-AMT
           MOVE CHSO-FINAL-PAY-DATE        TO SOM-FINAL-PAY-DATE
           MOVE CHSO-NUM-PAYMENTS          TO SOM-PAYMENTS-REMAINING
           MOVE ZERO                       TO SOM-PAYMENTS-MADE
                                              SOM-LAST-PAID-DATE
                                              SOM-LAST-PAID-AMT
                                              SOM-LAST-ATTEMPT-DATE
                                              SOM-RETRY-COUNT
                                              SOM-CONSEC-FAILS
                                              SOM-TOTAL-PAID
                                              SOM-AMEND-DATE
                                              SOM-CANCEL-DATE
           MOVE WS-TODAY                   TO SOM-CREATE-DATE
           MOVE CH-CHANNEL-CD              TO SOM-CREATE-CHANNEL
           MOVE CH-USER-ID                 TO SOM-CREATE-USER

      *    FIRST PAYMENT ON A NON-BUSINESS DAY IS PAID NEXT BUSINESS DAY
           MOVE CHSO-FIRST-PAY-DATE        TO WS-DUE-DATE
           PERFORM 8200-ROLL-TO-BUS-DAY
           MOVE WS-DUE-DATE                TO SOM-NEXT-DUE-DATE

           EXEC CICS WRITE
                     FILE('SOMAST')
                     FROM(SOM-SO-RECORD)
                     LENGTH(LENGTH OF SOM-SO-RECORD)
                     RIDFLD(SOM-KEY)
                     RESP(WS-RESP)
           END-EXEC

      *    THE CONTROL RECORD IS ALREADY UPDATED - ABEND TO BACK OUT
      *    THE WHOLE UNIT OF WORK IF THE SO CANNOT BE WRITTEN.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND
                         ABCODE('SOM1')
               END-EXEC
           END-IF

           MOVE WS-NEW-SEQ                 TO CHSO-SO-SEQ
           MOVE 1                          TO WS-LIST-COUNT
           PERFORM 2100-FORMAT-ENTRY
           MOVE 1                          TO CHSO-SO-COUNT
           MOVE 'STANDING ORDER CREATED'   TO CH-MESSAGE-TEXT
           .
      *================================================================*
       5000-AMEND-ORDER.
      *================================================================*
           PERFORM 8000-READ-SO-UPDATE
           IF NOT CH-RC-OK
               GO TO 5000-EXIT
           END-IF

           EVALUATE TRUE
               WHEN SOM-CANCELLED OR SOM-COMPLETED
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SOCN'             TO CH-REASON-CD
                   MOVE 'STANDING ORDER IS CANCELLED OR COMPLETE'
                                           TO CH-MESSAGE-TEXT
               WHEN SOM-NEXT-DUE-DATE = WS-TODAY
               AND  WS-TIME-HHMMSS > WS-CUTOFF-TIME
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CUTF'             TO CH-REASON-CD
                   MOVE 'TODAYS PAYMENT IN PROGRESS - TRY TOMORROW'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSO-BENE-SORT-CODE NOT = SPACES
               AND  CHSO-BENE-SORT-CODE NOT = SOM-BENE-SORT-CODE
               WHEN CHSO-BENE-ACCT-NO NOT = SPACES
               AND  CHSO-BENE-ACCT-NO NOT = SOM-BENE-ACCT-NO
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVB'             TO CH-REASON-CD
                   MOVE 'BENEFICIARY ACCOUNT CANNOT BE AMENDED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF NOT CH-RC-OK
               EXEC CICS UNLOCK
                         FILE('SOMAST')
                         RESP(WS-RESP)
               END-EXEC
               GO TO 5000-EXIT
           END-IF

      *---- AMOUNT ----------------------------------------------------*
           IF CHSO-REGULAR-AMT NUMERIC
           AND CHSO-REGULAR-AMT > ZERO
               SET CHL-IDX                 TO 1
               MOVE ZERO                   TO WS-LIMIT
               SEARCH WS-CHL-ENTRY
                   AT END
                       CONTINUE
                   WHEN WS-CHL-CHANNEL (CHL-IDX) = CH-CHANNEL-CD
                       MOVE WS-CHL-LIMIT (CHL-IDX) TO WS-LIMIT
               END-SEARCH
               IF CHSO-REGULAR-AMT > WS-LIMIT
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'LIMX'             TO CH-REASON-CD
                   MOVE 'AMOUNT EXCEEDS THE LIMIT FOR THIS CHANNEL'
                                           TO CH-MESSAGE-TEXT
               ELSE
                   MOVE CHSO-REGULAR-AMT   TO SOM-REGULAR-AMT
               END-IF
           END-IF

      *---- NEXT PAYMENT DATE -----------------------------------------*
           IF CH-RC-OK
           AND CHSO-FIRST-PAY-DATE NUMERIC
           AND CHSO-FIRST-PAY-DATE NOT = ZERO
               IF CHSO-FIRST-PAY-DATE < WS-TOMORROW
               OR CHSO-FIRST-PAY-DATE > WS-MAX-DATE
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'DATX'             TO CH-REASON-CD
                   MOVE 'NEXT PAYMENT DATE MUST BE TOMORROW TO 1 YEAR'
                                           TO CH-MESSAGE-TEXT
               ELSE
                   MOVE CHSO-FIRST-PAY-DATE TO SOM-NOMINAL-DUE-DATE
                   MOVE CHSO-FIRST-PAY-DATE (7:2) TO SOM-NOMINAL-DAY
                   MOVE CHSO-FIRST-PAY-DATE TO WS-DUE-DATE
                   PERFORM 8200-ROLL-TO-BUS-DAY
                   MOVE WS-DUE-DATE        TO SOM-NEXT-DUE-DATE
               END-IF
           END-IF

           IF NOT CH-RC-OK
               EXEC CICS UNLOCK
                         FILE('SOMAST')
                         RESP(WS-RESP)
               END-EXEC
               GO TO 5000-EXIT
           END-IF

      *---- NAME / REFERENCE ------------------------------------------*
           IF CHSO-BENE-NAME NOT = SPACES AND LOW-VALUES
               MOVE CHSO-BENE-NAME         TO SOM-BENE-NAME
           END-IF
           IF CHSO-BENE-REF NOT = SPACES AND LOW-VALUES
               MOVE CHSO-BENE-REF          TO SOM-BENE-REF
           END-IF

      *---- A SUSPENDED ORDER IS REINSTATED BY ANY AMENDMENT ----------*
           IF SOM-SUSPENDED
               SET SOM-ACTIVE              TO TRUE
               MOVE ZERO                   TO SOM-CONSEC-FAILS
                                              SOM-RETRY-COUNT
               MOVE SPACES                 TO SOM-LAST-FAIL-RSN
               IF SOM-NEXT-DUE-DATE < WS-TOMORROW
                   MOVE WS-TOMORROW        TO WS-DUE-DATE
                   PERFORM 8200-ROLL-TO-BUS-DAY
                   MOVE WS-DUE-DATE        TO SOM-NEXT-DUE-DATE
               END-IF
               MOVE 'STANDING ORDER AMENDED AND REINSTATED'
                                           TO CH-MESSAGE-TEXT
           ELSE
               MOVE 'STANDING ORDER AMENDED'
                                           TO CH-MESSAGE-TEXT
           END-IF

           MOVE WS-TODAY                   TO SOM-AMEND-DATE
           MOVE CH-CHANNEL-CD              TO SOM-AMEND-CHANNEL
           MOVE CH-USER-ID                 TO SOM-AMEND-USER

           EXEC CICS REWRITE
                     FILE('SOMAST')
                     FROM(SOM-SO-RECORD)
                     LENGTH(LENGTH OF SOM-SO-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'SOMAST'               TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
           ELSE
               MOVE 1                      TO WS-LIST-COUNT
               PERFORM 2100-FORMAT-ENTRY
               MOVE 1                      TO CHSO-SO-COUNT
           END-IF
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       6000-CANCEL-ORDER.
      *================================================================*
           PERFORM 8000-READ-SO-UPDATE
           IF NOT CH-RC-OK
               GO TO 6000-EXIT
           END-IF

           EVALUATE TRUE
               WHEN SOM-CANCELLED OR SOM-COMPLETED
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SOCN'             TO CH-REASON-CD
                   MOVE 'STANDING ORDER IS ALREADY CANCELLED'
                                           TO CH-MESSAGE-TEXT
               WHEN SOM-NEXT-DUE-DATE = WS-TODAY
               AND  WS-TIME-HHMMSS > WS-CUTOFF-TIME
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CUTF'             TO CH-REASON-CD
                   MOVE 'TODAYS PAYMENT IN PROGRESS - TRY TOMORROW'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF NOT CH-RC-OK
               EXEC CICS UNLOCK
                         FILE('SOMAST')
                         RESP(WS-RESP)
               END-EXEC
               GO TO 6000-EXIT
           END-IF

           SET SOM-CANCELLED               TO TRUE
           MOVE WS-TODAY                   TO SOM-CANCEL-DATE
           IF CHSO-CANCEL-RSN = SPACES OR LOW-VALUES
               MOVE 'CUST'                 TO SOM-CANCEL-RSN
           ELSE
               MOVE CHSO-CANCEL-RSN        TO SOM-CANCEL-RSN
           END-IF
           MOVE WS-TODAY                   TO SOM-AMEND-DATE
           MOVE CH-CHANNEL-CD              TO SOM-AMEND-CHANNEL
           MOVE CH-USER-ID                 TO SOM-AMEND-USER

           EXEC CICS REWRITE
                     FILE('SOMAST')
                     FROM(SOM-SO-RECORD)
                     LENGTH(LENGTH OF SOM-SO-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'SOMAST'               TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 6000-EXIT
           END-IF

           PERFORM 6100-DECREMENT-ACTIVE
           MOVE 1                          TO WS-LIST-COUNT
           PERFORM 2100-FORMAT-ENTRY
           MOVE 1                          TO CHSO-SO-COUNT
           MOVE 'STANDING ORDER CANCELLED' TO CH-MESSAGE-TEXT
           .
       6000-EXIT.
           EXIT.
      *================================================================*
       6100-DECREMENT-ACTIVE.
      *================================================================*
      *    THE CANCELLED ORDER IS SAVED, THE CONTROL RECORD IS UPDATED
      *    THROUGH THE SAME RECORD AREA, THEN THE ORDER IS RESTORED SO
      *    IT CAN BE RETURNED TO THE CALLER.
           MOVE SOM-SO-RECORD              TO WS-SAVED-SO
           MOVE CHSO-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHSO-ACCT-NO               TO WS-SK-ACCT-NO
           MOVE ZERO                       TO WS-SK-SO-SEQ
           EXEC CICS READ
                     FILE('SOMAST')
                     INTO(SOM-SO-RECORD)
                     LENGTH(LENGTH OF SOM-SO-RECORD)
                     RIDFLD(WS-SO-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               IF SOM-ACTIVE-COUNT > ZERO
                   SUBTRACT 1              FROM SOM-ACTIVE-COUNT
               END-IF
               EXEC CICS REWRITE
                         FILE('SOMAST')
                         FROM(SOM-SO-RECORD)
                         LENGTH(LENGTH OF SOM-SO-RECORD)
                         RESP(WS-RESP)
               END-EXEC
           END-IF
      *    A MISSING CONTROL RECORD IS NOT AN ERROR FOR A CANCEL (PRE-
      *    2002 ORDERS WERE LOADED WITHOUT ONE) - THE COUNT IS ONLY
      *    USED TO ENFORCE THE MAXIMUM ON CREATE.
           MOVE WS-SAVED-SO                TO SOM-SO-RECORD
           .
      *================================================================*
       8000-READ-SO-UPDATE.
      *================================================================*
           MOVE CHSO-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHSO-ACCT-NO               TO WS-SK-ACCT-NO
           MOVE CHSO-SO-SEQ                TO WS-SK-SO-SEQ
           EXEC CICS READ
                     FILE('SOMAST')
                     INTO(SOM-SO-RECORD)
                     LENGTH(LENGTH OF SOM-SO-RECORD)
                     RIDFLD(WS-SO-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SONF'             TO CH-REASON-CD
                   MOVE 'STANDING ORDER NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'SOMAST'           TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       8100-READ-SO-NOLOCK.
      *================================================================*
           MOVE CHSO-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHSO-ACCT-NO               TO WS-SK-ACCT-NO
           MOVE CHSO-SO-SEQ                TO WS-SK-SO-SEQ
           EXEC CICS READ
                     FILE('SOMAST')
                     INTO(SOM-SO-RECORD)
                     LENGTH(LENGTH OF SOM-SO-RECORD)
                     RIDFLD(WS-SO-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SONF'             TO CH-REASON-CD
                   MOVE 'STANDING ORDER NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'SOMAST'           TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       8200-ROLL-TO-BUS-DAY.
      *================================================================*
      *    WS-DUE-DATE IN/OUT.
           MOVE ZERO                       TO WS-LOOP-GUARD
           MOVE 'B'                        TO DTE-FUNCTION
           MOVE WS-DUE-DATE                TO DTE-DATE-IN
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           PERFORM UNTIL DTE-BUS-DAY-IND = 'Y'
                      OR WS-LOOP-GUARD > 10
               MOVE 'D'                    TO DTE-FUNCTION
               MOVE WS-DUE-DATE            TO DTE-DATE-IN
               MOVE 1                      TO DTE-DAYS
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               MOVE DTE-DATE-OUT           TO WS-DUE-DATE
               MOVE 'B'                    TO DTE-FUNCTION
               MOVE WS-DUE-DATE            TO DTE-DATE-IN
               CALL 'DTEUTIL' USING DTE-PARM-BLOCK
               ADD 1                       TO WS-LOOP-GUARD
           END-PERFORM
           .
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
           MOVE WS-RESP                    TO WS-RESP-DISPLAY
           MOVE '0012'                     TO CH-RETURN-CD
           IF WS-RESP = DFHRESP(NOTOPEN)
           OR WS-RESP = DFHRESP(DISABLED)
               MOVE 'FNOP'                 TO CH-REASON-CD
           ELSE
               MOVE 'FERR'                 TO CH-REASON-CD
           END-IF
           MOVE SPACES                     TO CH-MESSAGE-TEXT
           STRING WS-FILE-NAME
                  ' FILE ERROR - EIBRESP '
                  WS-RESP-DISPLAY
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
