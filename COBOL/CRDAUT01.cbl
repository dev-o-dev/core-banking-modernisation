       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRDAUT01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-08-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CRDAUT01                                            *
      * TITLE    : DEBIT CARD AUTHORISATION - CHANNEL SERVICE CARDAUTH *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : ACCTMST  READ                                       *
      * LINKS    : DDHLD01  (SERVICE ACCTHOLD - PLACE / LIST / RELEASE)*
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-CARDAUTH-AREA              *
      *----------------------------------------------------------------*
      * UNTIL 2010 THE CARD PROCESSOR AUTHORISED AGAINST A SHADOW      *
      * BALANCE SENT TO IT EACH NIGHT (CRDMNT1 / BALANCE FILE). CARD   *
      * SPENDING DURING THE DAY WAS NOT VISIBLE TO IBANK, AND THE      *
      * SHADOW BALANCE DID NOT SEE FASTER PAYMENTS. THE GATEWAY NOW    *
      * CALLS THE HOST IN REAL TIME:                                   *
      *                                                                *
      *   A  AUTHORISE - CHECK THE ACCOUNT AND FUNDS (AVAILABLE + OD   *
      *      LIMIT) AND PLACE A 'CARD' HOLD FOR THE AMOUNT. THE HOLD   *
      *      EXPIRES AFTER CHCA-HOLD-DAYS (DEFAULT 7, 30 FOR HOTELS /  *
      *      CAR HIRE MCCS) IF THE SETTLEMENT NEVER ARRIVES.           *
      *   R  REVERSE   - THE MERCHANT CANCELLED; RELEASE THE HOLD.     *
      *                                                                *
      * SETTLEMENT (CRDSTL010) RELEASES THE HOLD AND POSTS THE DEBIT.  *
      *                                                                *
      * STAND-IN: IF THE HOST IS UNAVAILABLE (BATCH WINDOW) THE        *
      * GATEWAY STANDS IN WITH A FLOOR LIMIT OF 50.00 - RESPONSE 91    *
      * TELLS IT TO DO SO.                                             *
      *                                                                *
      * THE HOLD NARRATIVE CARRIES THE AUTH REF SO THE REVERSAL AND    *
      * THE SETTLEMENT CAN FIND IT: 'CARD <AUTH REF> <MERCHANT>'.      *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-08-02 JNB  ORIGINAL VERSION                               *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CRDAUT01'.
      *
       01  WS-CONSTANTS.
           05  WS-DEFAULT-HOLD-DAYS        PIC 9(02) VALUE 07.
           05  WS-LONG-HOLD-DAYS           PIC 9(02) VALUE 30.
           05  WS-MAX-CASH                 PIC S9(13)V99 COMP-3
                                           VALUE +500.00.
           05  WS-MAX-AUTH                 PIC S9(13)V99 COMP-3
                                           VALUE +25000.00.
      *
      *---- MERCHANT CATEGORY CODES THAT GET A LONG HOLD --------------*
       01  WS-LONG-HOLD-MCC-VALUES.
           05  FILLER                      PIC X(04) VALUE '3501'.
           05  FILLER                      PIC X(04) VALUE '3351'.
           05  FILLER                      PIC X(04) VALUE '7011'.
           05  FILLER                      PIC X(04) VALUE '7512'.
           05  FILLER                      PIC X(04) VALUE '4411'.
           05  FILLER                      PIC X(04) VALUE '5542'.
       01  WS-LONG-HOLD-MCC-TABLE REDEFINES WS-LONG-HOLD-MCC-VALUES.
           05  WS-LONG-MCC                 PIC X(04) OCCURS 6 TIMES
                                           INDEXED BY MC-IDX.
      *
      *---- MCCS THE BANK DECLINES ON ALL CARDS (GAMBLING POLICY) ----*
       01  WS-BLOCKED-MCC-VALUES.
           05  FILLER                      PIC X(04) VALUE '7995'.
           05  FILLER                      PIC X(04) VALUE '7800'.
           05  FILLER                      PIC X(04) VALUE '7801'.
           05  FILLER                      PIC X(04) VALUE '7802'.
       01  WS-BLOCKED-MCC-TABLE REDEFINES WS-BLOCKED-MCC-VALUES.
           05  WS-BLOCKED-MCC              PIC X(04) OCCURS 4 TIMES
                                           INDEXED BY BM-IDX.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-SPENDING-POWER           PIC S9(13)V99 COMP-3.
           05  WS-HOLD-DAYS                PIC 9(02).
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-FOUND-SEQ                PIC 9(04).
           05  WS-HOLD-NARRATIVE           PIC X(35).
           05  WS-APPROVAL-SEED            PIC 9(06).
      *
       01  WS-SAVED-REQUEST.
           05  WS-SR-FUNCTION              PIC X(01).
           05  WS-SR-SORT-CODE             PIC X(06).
           05  WS-SR-ACCT-NO               PIC X(08).
           05  WS-SR-AMOUNT                PIC S9(13)V99 COMP-3.
           05  WS-SR-MERCHANT              PIC X(25).
           05  WS-SR-MCC                   PIC X(04).
           05  WS-SR-AUTH-REF              PIC X(12).
           05  WS-SR-HOLD-DAYS             PIC 9(02).
           05  WS-SR-CASH-IND              PIC X(01).
       01  WS-SAVED-COMMAREA               PIC X(4000).
       01  WS-RESULT.
           05  WS-RESULT-CD                PIC X(02).
           05  WS-RESULT-APPROVAL          PIC X(06).
           05  WS-RESULT-SEQ               PIC 9(04).
           05  WS-RESULT-AVAIL             PIC S9(13)V99 COMP-3.
           05  WS-RESULT-MESSAGE           PIC X(80).
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
      *
           COPY DDACCREC.
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
                                              WS-SAVED-COMMAREA
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
           MOVE SPACES                     TO WS-RESULT-APPROVAL
           MOVE ZERO                       TO WS-RESULT-SEQ
                                              WS-RESULT-AVAIL
           MOVE '00'                       TO WS-RESULT-CD

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC

           PERFORM 1000-VALIDATE-REQUEST
           IF WS-RESULT-CD = '00'
               PERFORM 2000-READ-ACCOUNT
           END-IF
           IF WS-RESULT-CD = '00'
               IF WS-SR-FUNCTION = 'A'
                   PERFORM 3000-AUTHORISE THRU 3000-EXIT
               ELSE
                   PERFORM 4000-REVERSE THRU 4000-EXIT
               END-IF
           END-IF

           PERFORM 9000-BUILD-RESPONSE
           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
           MOVE CHCA-FUNCTION              TO WS-SR-FUNCTION
           MOVE CHCA-SORT-CODE             TO WS-SR-SORT-CODE
           MOVE CHCA-ACCT-NO               TO WS-SR-ACCT-NO
           MOVE CHCA-MERCHANT-NAME         TO WS-SR-MERCHANT
           MOVE CHCA-MCC                   TO WS-SR-MCC
           MOVE CHCA-AUTH-REF              TO WS-SR-AUTH-REF
           MOVE CHCA-CASH-IND              TO WS-SR-CASH-IND
           IF CHCA-AMOUNT NUMERIC
               MOVE CHCA-AMOUNT            TO WS-SR-AMOUNT
           ELSE
               MOVE ZERO                   TO WS-SR-AMOUNT
           END-IF
           IF CHCA-HOLD-DAYS NUMERIC AND CHCA-HOLD-DAYS > ZERO
               MOVE CHCA-HOLD-DAYS         TO WS-SR-HOLD-DAYS
           ELSE
               MOVE ZERO                   TO WS-SR-HOLD-DAYS
           END-IF

           EVALUATE TRUE
               WHEN CH-CHANNEL-CD NOT = 'CARDS '
                   MOVE '57'               TO WS-RESULT-CD
                   MOVE 'SERVICE ONLY FOR THE CARD GATEWAY'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT (CHCA-AUTHORISE OR CHCA-REVERSE)
               WHEN CHCA-SORT-CODE NOT NUMERIC
               WHEN CHCA-ACCT-NO NOT NUMERIC
               WHEN CHCA-AUTH-REF = SPACES
                   MOVE '12'               TO WS-RESULT-CD
                   MOVE 'INVALID CARD AUTHORISATION REQUEST'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCA-AUTHORISE
               AND (WS-SR-AMOUNT NOT > ZERO
                    OR WS-SR-AMOUNT > WS-MAX-AUTH)
                   MOVE '12'               TO WS-RESULT-CD
                   MOVE 'AUTHORISATION AMOUNT OUT OF RANGE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCA-AUTHORISE
               AND  CHCA-CASH-IND = 'Y'
               AND  WS-SR-AMOUNT > WS-MAX-CASH
                   MOVE '57'               TO WS-RESULT-CD
                   MOVE 'CASH OVER SINGLE WITHDRAWAL LIMIT'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-RESULT-CD = '00' AND CHCA-AUTHORISE
               SET BM-IDX                  TO 1
               SEARCH WS-BLOCKED-MCC
                   AT END
                       CONTINUE
                   WHEN WS-BLOCKED-MCC (BM-IDX) = WS-SR-MCC
                       MOVE '57'           TO WS-RESULT-CD
                       MOVE 'MERCHANT CATEGORY NOT PERMITTED'
                                           TO CH-MESSAGE-TEXT
               END-SEARCH
           END-IF
           .
      *================================================================*
       2000-READ-ACCOUNT.
      *================================================================*
           MOVE WS-SR-SORT-CODE            TO WS-AK-SORT-CODE
           MOVE WS-SR-ACCT-NO              TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE '14'               TO WS-RESULT-CD
                   MOVE 'ACCOUNT NOT FOUND' TO CH-MESSAGE-TEXT
               WHEN WS-RESP = DFHRESP(NOTOPEN)
               WHEN WS-RESP = DFHRESP(DISABLED)
                   MOVE '91'               TO WS-RESULT-CD
                   MOVE 'HOST IN BATCH WINDOW - STAND IN'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE '91'               TO WS-RESULT-CD
                   MOVE 'ACCOUNT FILE ERROR' TO CH-MESSAGE-TEXT
               WHEN WS-SR-FUNCTION = 'R'
      *            REVERSALS ARE ALWAYS ACCEPTED WHATEVER THE STATUS.
                   CONTINUE
               WHEN ACM-CLOSED OR ACM-PENDING-CLOSE
                   MOVE '14'               TO WS-RESULT-CD
                   MOVE 'ACCOUNT CLOSED'   TO CH-MESSAGE-TEXT
               WHEN ACM-FROZEN OR ACM-DORMANT
               WHEN ACM-NO-DEBITS OR ACM-NO-POSTINGS
                   MOVE '62'               TO WS-RESULT-CD
                   MOVE 'ACCOUNT RESTRICTED' TO CH-MESSAGE-TEXT
               WHEN NOT ACM-CURRENT
                   MOVE '57'               TO WS-RESULT-CD
                   MOVE 'CARDS ONLY ON CURRENT ACCOUNTS'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       3000-AUTHORISE.
      *================================================================*
           MOVE ACM-OD-LIMIT               TO WS-OD-LIMIT
           IF ACM-OD-EXPIRY-DATE NOT = ZERO
           AND ACM-OD-EXPIRY-DATE < WS-TODAY
               MOVE ZERO                   TO WS-OD-LIMIT
           END-IF
           COMPUTE WS-SPENDING-POWER = ACM-AVAIL-BAL + WS-OD-LIMIT
           IF WS-SR-AMOUNT > WS-SPENDING-POWER
               MOVE '51'                   TO WS-RESULT-CD
               MOVE 'INSUFFICIENT FUNDS'   TO CH-MESSAGE-TEXT
               MOVE ACM-AVAIL-BAL          TO WS-RESULT-AVAIL
               GO TO 3000-EXIT
           END-IF

      *---- HOLD PERIOD -----------------------------------------------*
           MOVE WS-DEFAULT-HOLD-DAYS       TO WS-HOLD-DAYS
           SET MC-IDX                      TO 1
           SEARCH WS-LONG-MCC
               AT END
                   CONTINUE
               WHEN WS-LONG-MCC (MC-IDX) = WS-SR-MCC
                   MOVE WS-LONG-HOLD-DAYS  TO WS-HOLD-DAYS
           END-SEARCH
           IF WS-SR-HOLD-DAYS > WS-HOLD-DAYS
               MOVE WS-SR-HOLD-DAYS        TO WS-HOLD-DAYS
           END-IF

      *---- PLACE THE HOLD VIA DDHLD01 --------------------------------*
           PERFORM 8000-INIT-HOLD-REQUEST
           SET CHAH-PLACE                  TO TRUE
           MOVE WS-SR-AMOUNT               TO CHAH-AMOUNT
           MOVE 'CARD'                     TO CHAH-REASON-CD
           MOVE 'D'                        TO DTE-FUNCTION
           MOVE WS-TODAY                   TO DTE-DATE-IN
           MOVE WS-HOLD-DAYS               TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO CHAH-EXPIRY-DATE
           MOVE SPACES                     TO WS-HOLD-NARRATIVE
           STRING 'CARD ' WS-SR-AUTH-REF ' ' WS-SR-MERCHANT
                  DELIMITED BY SIZE
                  INTO WS-HOLD-NARRATIVE
           END-STRING
           MOVE WS-HOLD-NARRATIVE          TO CHAH-NARRATIVE
           PERFORM 8100-LINK-HOLDS

           IF CH-RC-OK OR CH-RC-WARNING
               MOVE CHAH-NEW-HOLD-SEQ      TO WS-RESULT-SEQ
               MOVE CHAH-NEW-AVAIL-BAL     TO WS-RESULT-AVAIL
      *        APPROVAL CODE: LAST 6 DIGITS OF THE CICS TASK NUMBER.
               MOVE EIBTASKN               TO WS-APPROVAL-SEED
               MOVE WS-APPROVAL-SEED       TO WS-RESULT-APPROVAL
               MOVE 'APPROVED'             TO CH-MESSAGE-TEXT
           ELSE
               MOVE '91'                   TO WS-RESULT-CD
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-REVERSE.
      *================================================================*
      *    FIND THE HOLD WHOSE NARRATIVE CARRIES THE AUTH REF.
           PERFORM 8000-INIT-HOLD-REQUEST
           SET CHAH-LIST                   TO TRUE
           PERFORM 8100-LINK-HOLDS
           MOVE ZERO                       TO WS-FOUND-SEQ
           IF CH-RC-OK
               PERFORM VARYING WS-SUB FROM 1 BY 1
                       UNTIL WS-SUB > CHAH-COUNT
                          OR WS-FOUND-SEQ > ZERO
                   IF CHAHE-REASON-CD (WS-SUB) = 'CARD'
                   AND CHAHE-NARRATIVE (WS-SUB) (6:12) = WS-SR-AUTH-REF
                       MOVE CHAHE-HOLD-SEQ (WS-SUB) TO WS-FOUND-SEQ
                   END-IF
               END-PERFORM
           END-IF

           IF WS-FOUND-SEQ = ZERO
      *        ALREADY SETTLED, EXPIRED OR NEVER AUTHORISED HERE (STAND-
      *        IN). NOTHING TO RELEASE - THE GATEWAY TREATS 12 AS DONE.
               MOVE '12'                   TO WS-RESULT-CD
               MOVE 'NO ACTIVE HOLD FOR THIS AUTH REF'
                                           TO CH-MESSAGE-TEXT
               GO TO 4000-EXIT
           END-IF

           PERFORM 8000-INIT-HOLD-REQUEST
           SET CHAH-RELEASE                TO TRUE
           MOVE WS-FOUND-SEQ               TO CHAH-HOLD-SEQ
           PERFORM 8100-LINK-HOLDS
           IF CH-RC-OK OR CH-RC-WARNING
               MOVE WS-FOUND-SEQ           TO WS-RESULT-SEQ
               MOVE CHAH-NEW-AVAIL-BAL     TO WS-RESULT-AVAIL
               MOVE 'REVERSED'             TO CH-MESSAGE-TEXT
           ELSE
               MOVE '91'                   TO WS-RESULT-CD
           END-IF
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       8000-INIT-HOLD-REQUEST.
      *================================================================*
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA
           MOVE 'ACCTHOLD'                 TO CH-SERVICE-CD
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
                                              CH-DATA
           MOVE WS-SR-SORT-CODE            TO CHAH-SORT-CODE
           MOVE WS-SR-ACCT-NO              TO CHAH-ACCT-NO
           MOVE ZERO                       TO CHAH-HOLD-SEQ
                                              CHAH-AMOUNT
                                              CHAH-EXPIRY-DATE
           .
      *================================================================*
       8100-LINK-HOLDS.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM('DDHLD01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'LNKF'                 TO CH-REASON-CD
               MOVE 'LINK TO DDHLD01 FAILED' TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       9000-BUILD-RESPONSE.
      *================================================================*
      *    THE CARD GATEWAY READS ONLY THE CARDAUTH AREA; THE CHCOMMAR
      *    RETURN CODE IS 0000 FOR AN APPROVAL AND 0008 FOR A DECLINE.
           MOVE CH-MESSAGE-TEXT            TO WS-RESULT-MESSAGE
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA
           MOVE WS-RESULT-CD               TO CHCA-RESPONSE-CD
           MOVE WS-RESULT-APPROVAL         TO CHCA-APPROVAL-CD
           MOVE WS-RESULT-SEQ              TO CHCA-HOLD-SEQ
           MOVE WS-RESULT-AVAIL            TO CHCA-AVAIL-BAL
           MOVE WS-RESULT-MESSAGE          TO CH-MESSAGE-TEXT
           EVALUATE WS-RESULT-CD
               WHEN '00'
                   MOVE '0000'             TO CH-RETURN-CD
                   MOVE SPACES             TO CH-REASON-CD
               WHEN '91'
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'STIN'             TO CH-REASON-CD
               WHEN '12'
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVC'             TO CH-REASON-CD
               WHEN OTHER
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE SPACES             TO CH-REASON-CD
                   STRING 'DC' WS-RESULT-CD
                          DELIMITED BY SIZE
                          INTO CH-REASON-CD
                   END-STRING
           END-EVALUATE
           .
