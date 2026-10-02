       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDCLS01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-05-17.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDCLS01                                             *
      * TITLE    : ACCOUNT CLOSURE - CHANNEL SERVICE ACCTCLOS          *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : ACCTMST  READ / READ UPDATE / REWRITE               *
      *            PRDPARM  READ (TAX DEDUCTION INDICATOR)             *
      *            CHGPEND  BROWSE (CHARGES NOT YET APPLIED)           *
      *            SOMAST   BROWSE (ACTIVE STANDING ORDERS)            *
      *            DDMAND   BROWSE (LIVE DIRECT DEBIT MANDATES)        *
      *            DDHOLD   READ   (ACTIVE HOLDS COUNT)                *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-ACCTCLOS-AREA              *
      *----------------------------------------------------------------*
      * Q  QUOTE - WHAT THE CUSTOMER WOULD RECEIVE IF THE ACCOUNT WERE *
      *    CLOSED TONIGHT:                                             *
      *      LEDGER + TODAY'S MEMO CREDITS - TODAY'S MEMO DEBITS       *
      *      + ACCRUED CREDIT INTEREST - TAX - ACCRUED DEBIT INTEREST  *
      *      - PENDING CHARGES                                         *
      *    PLUS THE NUMBER OF STANDING ORDERS, MANDATES AND HOLDS THAT *
      *    WILL BE CANCELLED / MUST BE RELEASED.                       *
      * C  REQUEST CLOSURE. REFUSED IF:                                *
      *      - THE CLOSING BALANCE IS NEGATIVE (CUSTOMER MUST PAY IN)  *
      *      - THERE ARE ACTIVE HOLDS                                  *
      *      - THE CLOSING BALANCE IS POSITIVE AND NO VALID TRANSFER   *
      *        ACCOUNT IS GIVEN (MUST BE A MEGABANK ACCOUNT HELD BY    *
      *        THE SAME CUSTOMER - CHEQUE PAY-OUTS ARE BRANCH ONLY     *
      *        AND OUTSIDE THIS SERVICE)                               *
      *    OTHERWISE THE ACCOUNT BECOMES 'P' (PENDING CLOSE) AND       *
      *    DEBITS ARE BLOCKED. DDCLS010 COMPLETES THE CLOSURE OVER THE *
      *    NEXT TWO NIGHTS.                                            *
      * U  UNDO - ONLY ON THE DAY OF THE REQUEST (BEFORE DDCLS010 HAS  *
      *    GENERATED THE CLOSING POSTINGS).                            *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   ANFD ACCOUNT NOT FOUND      ACLS ALREADY CLOSED / PENDING    *
      *   CNEG CLOSING BALANCE NEGATIVE                                *
      *   CHLD ACTIVE HOLDS ON ACCOUNT                                 *
      *   CXFR TRANSFER ACCOUNT MISSING OR INVALID                     *
      *   CUND UNDO NOT ALLOWED (NOT PENDING / NOT TODAY)              *
      *   CFRZ ACCOUNT FROZEN - BRANCH ONLY                            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-05-17 JNB  ORIGINAL VERSION                               *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDCLS01 '.
      *
       01  WS-CONSTANTS.
           05  WS-BASIC-RATE               PIC S9(03)V99 COMP-3
                                           VALUE +20.00.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TIME-HHMMSS              PIC 9(06).
           05  WS-TIME-HHMMSS-X REDEFINES WS-TIME-HHMMSS
                                           PIC X(06).
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-ACCOUNT-HELD-IND         PIC X(01) VALUE 'N'.
               88  WS-ACCOUNT-HELD             VALUE 'Y'.
      *
       01  WS-QUOTE.
           05  WS-LEDGER                   PIC S9(13)V99 COMP-3.
           05  WS-ACCR-CR                  PIC S9(13)V99 COMP-3.
           05  WS-TAX                      PIC S9(13)V99 COMP-3.
           05  WS-ACCR-DR                  PIC S9(13)V99 COMP-3.
           05  WS-PENDING-CHG              PIC S9(13)V99 COMP-3.
           05  WS-CLOSING                  PIC S9(13)V99 COMP-3.
           05  WS-SO-COUNT                 PIC 9(03).
           05  WS-DD-COUNT                 PIC 9(03).
           05  WS-HOLD-COUNT               PIC 9(03).
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
       01  WS-CHG-KEY.
           05  WS-CK-SORT-CODE             PIC X(06).
           05  WS-CK-ACCT-NO               PIC X(08).
           05  WS-CK-REST                  PIC X(18).
       01  WS-SO-KEY.
           05  WS-SK-SORT-CODE             PIC X(06).
           05  WS-SK-ACCT-NO               PIC X(08).
           05  WS-SK-SEQ                   PIC 9(04).
       01  WS-DDM-KEY.
           05  WS-DK-SORT-CODE             PIC X(06).
           05  WS-DK-ACCT-NO               PIC X(08).
           05  WS-DK-REST                  PIC X(24).
       01  WS-HOLD-KEY.
           05  WS-HK-SORT-CODE             PIC X(06).
           05  WS-HK-ACCT-NO               PIC X(08).
           05  WS-HK-SEQ                   PIC 9(04).
      *
       01  WS-CLOSE-CIF                    PIC X(12).
       01  WS-SAVED-ACCOUNT                PIC X(400).
      *
           COPY DDACCREC.
      *
           COPY PRDPARRC.
      *
           COPY CGCHGREC.
      *
           COPY SOMSTREC.
      *
           COPY DMMNDREC.
      *
           COPY DDHLDREC.
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
           MOVE ZERO                       TO CHAC-LEDGER-BAL
                                              CHAC-ACCRUED-CR
                                              CHAC-ACCRUED-DR
                                              CHAC-TAX-ON-CR
                                              CHAC-PENDING-CHG
                                              CHAC-CLOSING-BAL
                                              CHAC-SO-COUNT
                                              CHAC-DD-COUNT
                                              CHAC-HOLD-COUNT
                                              CHAC-CLOSE-DATE

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC

           PERFORM 1000-VALIDATE-REQUEST

           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHAC-QUOTE
                       PERFORM 2000-READ-ACCOUNT
                       IF CH-RC-OK
                           PERFORM 3000-BUILD-QUOTE
                       END-IF
                   WHEN CHAC-CLOSE
                       PERFORM 2100-READ-ACCOUNT-UPDATE
                       IF CH-RC-OK
                           PERFORM 3000-BUILD-QUOTE
                       END-IF
                       IF CH-RC-OK
                           PERFORM 4000-REQUEST-CLOSURE THRU 4000-EXIT
                       END-IF
                   WHEN CHAC-UNDO
                       PERFORM 2100-READ-ACCOUNT-UPDATE
                       IF CH-RC-OK
                           PERFORM 5000-UNDO-CLOSURE THRU 5000-EXIT
                       END-IF
               END-EVALUATE
           END-IF

           IF WS-ACCOUNT-HELD
               EXEC CICS UNLOCK
                         FILE('ACCTMST')
                         RESP(WS-RESP)
               END-EXEC
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN NOT (CHAC-QUOTE OR CHAC-CLOSE OR CHAC-UNDO)
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE Q, C OR U'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAC-SORT-CODE NOT NUMERIC
               WHEN CHAC-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAC-CLOSE
               AND  CHAC-XFER-SORT-CODE NOT = SPACES
               AND (CHAC-XFER-SORT-CODE NOT NUMERIC
                    OR CHAC-XFER-ACCT-NO NOT NUMERIC)
                   MOVE 'INVX'             TO CH-REASON-CD
                   MOVE 'TRANSFER ACCOUNT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAC-CLOSE
               AND  CHAC-XFER-SORT-CODE = CHAC-SORT-CODE
               AND  CHAC-XFER-ACCT-NO = CHAC-ACCT-NO
                   MOVE 'INVX'             TO CH-REASON-CD
                   MOVE 'TRANSFER ACCOUNT IS THE ACCOUNT BEING CLOSED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
           END-IF
           .
      *================================================================*
       2000-READ-ACCOUNT.
      *================================================================*
           MOVE CHAC-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHAC-ACCT-NO               TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           PERFORM 2200-CHECK-READ
           .
      *================================================================*
       2100-READ-ACCOUNT-UPDATE.
      *================================================================*
           MOVE CHAC-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHAC-ACCT-NO               TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               SET WS-ACCOUNT-HELD         TO TRUE
           END-IF
           PERFORM 2200-CHECK-READ
           .
      *================================================================*
       2200-CHECK-READ.
      *================================================================*
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   EVALUATE TRUE
                       WHEN ACM-CLOSED
                           MOVE '0008'     TO CH-RETURN-CD
                           MOVE 'ACLS'     TO CH-REASON-CD
                           MOVE 'ACCOUNT IS ALREADY CLOSED'
                                           TO CH-MESSAGE-TEXT
                       WHEN ACM-PENDING-CLOSE AND NOT CHAC-UNDO
                       AND  NOT CHAC-QUOTE
                           MOVE '0008'     TO CH-RETURN-CD
                           MOVE 'ACLS'     TO CH-REASON-CD
                           MOVE 'CLOSURE ALREADY REQUESTED'
                                           TO CH-MESSAGE-TEXT
                       WHEN ACM-FROZEN AND CH-CHANNEL-CD NOT = 'BRANCH'
                           MOVE '0008'     TO CH-RETURN-CD
                           MOVE 'CFRZ'     TO CH-REASON-CD
                           MOVE 'FROZEN - CLOSURE IS BRANCH ONLY'
                                           TO CH-MESSAGE-TEXT
                       WHEN OTHER
                           MOVE ACM-CIF-NO TO WS-CLOSE-CIF
                   END-EVALUATE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       3000-BUILD-QUOTE.
      *================================================================*
           COMPUTE WS-LEDGER = ACM-LEDGER-BAL
                             + ACM-MEMO-CR-TOT - ACM-MEMO-DR-TOT
           MOVE ACM-ACCRUED-INT-CR         TO WS-ACCR-CR
           MOVE ACM-ACCRUED-INT-DR         TO WS-ACCR-DR
           MOVE ZERO                       TO WS-TAX
                                              WS-PENDING-CHG
                                              WS-SO-COUNT
                                              WS-DD-COUNT
                                              WS-HOLD-COUNT

      *---- TAX ON CREDIT INTEREST ------------------------------------*
           IF WS-ACCR-CR > ZERO AND ACM-TAX-NET
               MOVE ACM-PROD-CD            TO PRD-PROD-CD
               EXEC CICS READ
                         FILE('PRDPARM')
                         INTO(PRD-PARAM-RECORD)
                         LENGTH(LENGTH OF PRD-PARAM-RECORD)
                         RIDFLD(PRD-KEY)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(NORMAL)
               AND PRD-DEDUCT-TAX
                   COMPUTE WS-TAX = FUNCTION INTEGER-PART
                           (WS-ACCR-CR * WS-BASIC-RATE) / 100
               END-IF
           END-IF

           MOVE ACM-ACCOUNT-RECORD         TO WS-SAVED-ACCOUNT
           PERFORM 3100-PENDING-CHARGES THRU 3100-EXIT
           PERFORM 3200-COUNT-STANDING-ORDERS THRU 3200-EXIT
           PERFORM 3300-COUNT-MANDATES THRU 3300-EXIT
           PERFORM 3400-COUNT-HOLDS
           MOVE WS-SAVED-ACCOUNT           TO ACM-ACCOUNT-RECORD

           COMPUTE WS-CLOSING = WS-LEDGER + WS-ACCR-CR - WS-TAX
                              - WS-ACCR-DR - WS-PENDING-CHG

           MOVE WS-LEDGER                  TO CHAC-LEDGER-BAL
           MOVE WS-ACCR-CR                 TO CHAC-ACCRUED-CR
           MOVE WS-ACCR-DR                 TO CHAC-ACCRUED-DR
           MOVE WS-TAX                     TO CHAC-TAX-ON-CR
           MOVE WS-PENDING-CHG             TO CHAC-PENDING-CHG
           MOVE WS-CLOSING                 TO CHAC-CLOSING-BAL
           MOVE WS-SO-COUNT                TO CHAC-SO-COUNT
           MOVE WS-DD-COUNT                TO CHAC-DD-COUNT
           MOVE WS-HOLD-COUNT              TO CHAC-HOLD-COUNT
           IF CHAC-QUOTE
               MOVE 'CLOSURE QUOTE - BALANCES AS AT NOW'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       3100-PENDING-CHARGES.
      *================================================================*
      *    CHARGES CALCULATED OR NOTIFIED BUT NOT YET APPLIED.
           MOVE CHAC-SORT-CODE             TO WS-CK-SORT-CODE
           MOVE CHAC-ACCT-NO               TO WS-CK-ACCT-NO
           MOVE LOW-VALUES                 TO WS-CK-REST
           MOVE 'N'                        TO WS-END-BROWSE-IND
           EXEC CICS STARTBR
                     FILE('CHGPEND')
                     RIDFLD(WS-CHG-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 3100-EXIT
           END-IF
           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('CHGPEND')
                         INTO(CGP-CHARGE-RECORD)
                         LENGTH(LENGTH OF CGP-CHARGE-RECORD)
                         RIDFLD(WS-CHG-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   WHEN CGP-SORT-CODE NOT = CHAC-SORT-CODE
                   WHEN CGP-ACCT-NO NOT = CHAC-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN CGP-CALCULATED OR CGP-NOTIFIED
                       ADD CGP-NET-AMT     TO WS-PENDING-CHG
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR
                     FILE('CHGPEND')
                     RESP(WS-RESP)
           END-EXEC
           .
       3100-EXIT.
           EXIT.
      *================================================================*
       3200-COUNT-STANDING-ORDERS.
      *================================================================*
           MOVE CHAC-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHAC-ACCT-NO               TO WS-SK-ACCT-NO
           MOVE 1                          TO WS-SK-SEQ
           MOVE 'N'                        TO WS-END-BROWSE-IND
           EXEC CICS STARTBR
                     FILE('SOMAST')
                     RIDFLD(WS-SO-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 3200-EXIT
           END-IF
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
                   WHEN SOM-PAYER-SORT-CODE NOT = CHAC-SORT-CODE
                   WHEN SOM-PAYER-ACCT-NO NOT = CHAC-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN SOM-ACTIVE OR SOM-SUSPENDED
                       ADD 1               TO WS-SO-COUNT
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR
                     FILE('SOMAST')
                     RESP(WS-RESP)
           END-EXEC
           .
       3200-EXIT.
           EXIT.
      *================================================================*
       3300-COUNT-MANDATES.
      *================================================================*
           MOVE CHAC-SORT-CODE             TO WS-DK-SORT-CODE
           MOVE CHAC-ACCT-NO               TO WS-DK-ACCT-NO
           MOVE LOW-VALUES                 TO WS-DK-REST
           MOVE 'N'                        TO WS-END-BROWSE-IND
           EXEC CICS STARTBR
                     FILE('DDMAND')
                     RIDFLD(WS-DDM-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 3300-EXIT
           END-IF
           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('DDMAND')
                         INTO(DMM-MANDATE-RECORD)
                         LENGTH(LENGTH OF DMM-MANDATE-RECORD)
                         RIDFLD(WS-DDM-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   WHEN DMM-PAYER-SORT-CODE NOT = CHAC-SORT-CODE
                   WHEN DMM-PAYER-ACCT-NO NOT = CHAC-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN DMM-LIVE OR DMM-DORMANT
                       ADD 1               TO WS-DD-COUNT
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR
                     FILE('DDMAND')
                     RESP(WS-RESP)
           END-EXEC
           .
       3300-EXIT.
           EXIT.
      *================================================================*
       3400-COUNT-HOLDS.
      *================================================================*
           MOVE CHAC-SORT-CODE             TO WS-HK-SORT-CODE
           MOVE CHAC-ACCT-NO               TO WS-HK-ACCT-NO
           MOVE ZERO                       TO WS-HK-SEQ
           EXEC CICS READ
                     FILE('DDHOLD')
                     INTO(HLD-HOLD-RECORD)
                     LENGTH(LENGTH OF HLD-HOLD-RECORD)
                     RIDFLD(WS-HOLD-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE HLD-ACTIVE-COUNT       TO WS-HOLD-COUNT
           END-IF
      *    A HOLD AMOUNT WITH NO HOLD RECORDS (TELLER CHEQUE HOLDS)
      *    STILL BLOCKS CLOSURE.
           MOVE WS-SAVED-ACCOUNT           TO ACM-ACCOUNT-RECORD
           IF WS-HOLD-COUNT = ZERO AND ACM-HOLD-AMT > ZERO
               MOVE 1                      TO WS-HOLD-COUNT
           END-IF
           .
      *================================================================*
       4000-REQUEST-CLOSURE.
      *================================================================*
           EVALUATE TRUE
               WHEN WS-CLOSING < ZERO
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CNEG'             TO CH-REASON-CD
                   MOVE 'ACCOUNT MUST BE IN CREDIT BEFORE IT CAN CLOSE'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-HOLD-COUNT > ZERO
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CHLD'             TO CH-REASON-CD
                   MOVE 'ACTIVE HOLDS MUST BE RELEASED FIRST'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-CLOSING > ZERO
               AND  CHAC-XFER-SORT-CODE = SPACES
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CXFR'             TO CH-REASON-CD
                   MOVE 'A TRANSFER ACCOUNT IS NEEDED FOR THE BALANCE'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT CH-RC-OK
               GO TO 4000-EXIT
           END-IF

           IF WS-CLOSING > ZERO
               PERFORM 4100-CHECK-XFER-ACCOUNT
               IF NOT CH-RC-OK
                   GO TO 4000-EXIT
               END-IF
           END-IF

      *---- MARK PENDING CLOSE ----------------------------------------*
           MOVE WS-SAVED-ACCOUNT           TO ACM-ACCOUNT-RECORD
           SET ACM-PENDING-CLOSE           TO TRUE
           SET ACM-NO-DEBITS               TO TRUE
           MOVE WS-TODAY                   TO ACM-CLOSE-REQ-DATE
           MOVE CHAC-XFER-SORT-CODE        TO ACM-CLOSE-XFER-SORT-CODE
           MOVE CHAC-XFER-ACCT-NO          TO ACM-CLOSE-XFER-ACCT-NO
           IF CHAC-CLOSE-RSN = SPACES
               MOVE 'CUST'                 TO ACM-CLOSE-RSN
           ELSE
               MOVE CHAC-CLOSE-RSN         TO ACM-CLOSE-RSN
           END-IF
           MOVE WS-TODAY                   TO ACM-LAST-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO ACM-LAST-MAINT-TIME
           MOVE CH-USER-ID                 TO ACM-LAST-MAINT-USER

           EXEC CICS REWRITE
                     FILE('ACCTMST')
                     FROM(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCTMST'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 4000-EXIT
           END-IF
           MOVE 'N'                        TO WS-ACCOUNT-HELD-IND

      *    DDCLS010 POSTS THE FINAL ITEMS TONIGHT AND CLOSES THE
      *    ACCOUNT THE FOLLOWING NIGHT.
           MOVE WS-TODAY                   TO CHAC-CLOSE-DATE
           MOVE 'CLOSURE REQUESTED - ACCOUNT CLOSES IN 2 WORKING DAYS'
                                           TO CH-MESSAGE-TEXT
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-CHECK-XFER-ACCOUNT.
      *================================================================*
           MOVE ACM-ACCOUNT-RECORD         TO WS-SAVED-ACCOUNT
           MOVE CHAC-XFER-SORT-CODE        TO WS-AK-SORT-CODE
           MOVE CHAC-XFER-ACCT-NO          TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CXFR'             TO CH-REASON-CD
                   MOVE 'TRANSFER ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
               WHEN ACM-CIF-NO NOT = WS-CLOSE-CIF
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CXFR'             TO CH-REASON-CD
                   MOVE 'TRANSFER ACCOUNT HELD BY ANOTHER CUSTOMER'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT ACM-OPEN
               WHEN ACM-NO-CREDITS
               WHEN ACM-NO-POSTINGS
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CXFR'             TO CH-REASON-CD
                   MOVE 'TRANSFER ACCOUNT CANNOT ACCEPT CREDITS'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           MOVE WS-SAVED-ACCOUNT           TO ACM-ACCOUNT-RECORD
           .
      *================================================================*
       5000-UNDO-CLOSURE.
      *================================================================*
           IF NOT ACM-PENDING-CLOSE
           OR ACM-CLOSE-REQ-DATE NOT = WS-TODAY
           OR ACM-DORMANCY-IND = 'X'
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'CUND'                 TO CH-REASON-CD
               MOVE 'CLOSURE CAN ONLY BE UNDONE ON THE DAY REQUESTED'
                                           TO CH-MESSAGE-TEXT
               GO TO 5000-EXIT
           END-IF

           SET ACM-OPEN                    TO TRUE
           SET ACM-NO-RESTRICT             TO TRUE
           MOVE ZERO                       TO ACM-CLOSE-REQ-DATE
           MOVE SPACES                     TO ACM-CLOSE-XFER-SORT-CODE
                                              ACM-CLOSE-XFER-ACCT-NO
                                              ACM-CLOSE-RSN
           MOVE WS-TODAY                   TO ACM-LAST-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO ACM-LAST-MAINT-TIME
           MOVE CH-USER-ID                 TO ACM-LAST-MAINT-USER

           EXEC CICS REWRITE
                     FILE('ACCTMST')
                     FROM(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCTMST'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
           ELSE
               MOVE 'N'                    TO WS-ACCOUNT-HELD-IND
               MOVE 'CLOSURE REQUEST CANCELLED - ACCOUNT REOPENED'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       5000-EXIT.
           EXIT.
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
