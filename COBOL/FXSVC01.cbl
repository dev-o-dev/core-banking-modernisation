       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FXSVC01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-05-16.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : FXSVC01                                             *
      * TITLE    : FX RATE QUOTE / VALIDATE - CHANNEL SERVICE FXQUOTE  *
      * TYPE     : CICS - LINKED FROM CHROUTER AND PYINIT1             *
      * FILES    : FXRATES  READ / BROWSE                              *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-FXQUOTE-AREA               *
      *----------------------------------------------------------------*
      * Q  QUOTE - CUSTOMER RATE FOR A CURRENCY AND DIRECTION. GIVE    *
      *    EITHER THE STERLING AMOUNT (RETURNS THE CURRENCY AMOUNT) OR *
      *    THE CURRENCY AMOUNT (RETURNS THE STERLING AMOUNT). THE      *
      *    MARGIN TIER IS CHOSEN BY THE STERLING VALUE AT MID RATE.    *
      * V  VALIDATE - USED BY PYINIT1 FOR SWIFT PAYMENTS WHERE THE     *
      *    CHANNEL SUPPLIES THE RATE. THE CHANNEL RATE MUST BE WITHIN  *
      *    WS-VALIDATE-TOL-PCT OF OUR CUSTOMER RATE, ELSE REJECT 'FXRT'*
      *    (CLOSES PIR 2009/201).                                      *
      * L  LIST - TIER 1 BUY AND SELL RATES FOR EVERY CURRENCY (BRANCH *
      *    RATE BOARD AND IBANK 'TODAY'S RATES' PAGE).                 *
      *                                                                *
      * A CURRENCY ON HOLD (FXRAT010 TOLERANCE) IS QUOTED AT THE OLD   *
      * RATE WITH WARNING 0004 'HELD'. A SUSPENDED CURRENCY IS         *
      * REFUSED. A RATE OLDER THAN ONE BUSINESS DAY IS QUOTED WITH     *
      * WARNING 'STAL' - IBANK SHOWS 'INDICATIVE'.                     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-05-16 JNB  ORIGINAL VERSION                               *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'FXSVC01'.
      *
       01  WS-CONSTANTS.
           05  WS-VALIDATE-TOL-PCT         PIC S9(02)V99 COMP-3
                                           VALUE +0.50.
           05  WS-STALE-DAYS               PIC S9(03) COMP-3 VALUE +3.
           05  WS-DEFAULT-MARGIN-BP        PIC S9(04) COMP-3 VALUE +250.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-GBP-VALUE                PIC S9(13)V99 COMP-3.
           05  WS-MARGIN-BP                PIC S9(04) COMP-3.
           05  WS-CUST-RATE                PIC S9(05)V9(06) COMP-3.
           05  WS-BUY-RATE                 PIC S9(05)V9(06) COMP-3.
           05  WS-SELL-RATE                PIC S9(05)V9(06) COMP-3.
           05  WS-DIFF-PCT                 PIC S9(03)V9(04) COMP-3.
           05  WS-TIER                     PIC S9(04) COMP.
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-AGE-DAYS                 PIC S9(07) COMP-3.
           05  WS-FX-KEY                   PIC X(03).
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
      *
           COPY FXRATREC.
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
           MOVE ZERO                       TO CHFX-MID-RATE
                                              CHFX-CUST-RATE
                                              CHFX-MARGIN-BP
                                              CHFX-RESULT-GBP
                                              CHFX-RESULT-CCY
                                              CHFX-RATE-DATE
                                              CHFX-RATE-TIME
                                              CHFX-DIFF-PCT
                                              CHFX-LIST-COUNT
           MOVE SPACE                      TO CHFX-RATE-STATUS

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC

           EVALUATE TRUE
               WHEN CHFX-QUOTE
                   PERFORM 1000-VALIDATE-QUOTE THRU 1000-EXIT
                   IF CH-RC-OK
                       PERFORM 2000-QUOTE THRU 2000-EXIT
                   END-IF
               WHEN CHFX-VALIDATE
                   PERFORM 1000-VALIDATE-QUOTE THRU 1000-EXIT
                   IF CH-RC-OK
                       PERFORM 3000-VALIDATE-RATE
                   END-IF
               WHEN CHFX-LIST
                   PERFORM 4000-LIST THRU 4000-EXIT
               WHEN OTHER
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE Q, V OR L'
                                           TO CH-MESSAGE-TEXT
           END-EVALUATE

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-QUOTE.
      *================================================================*
           EVALUATE TRUE
               WHEN CHFX-CCY-CD = SPACES OR CHFX-CCY-CD = 'GBP'
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'CURRENCY REQUIRED (NOT GBP)'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT (CHFX-BANK-SELLS OR CHFX-BANK-BUYS)
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'DIRECTION MUST BE S OR B'
                                           TO CH-MESSAGE-TEXT
               WHEN CHFX-GBP-AMOUNT NOT NUMERIC
               WHEN CHFX-CCY-AMOUNT NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'AMOUNTS MUST BE NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN CHFX-QUOTE
               AND  CHFX-GBP-AMOUNT NOT > ZERO
               AND  CHFX-CCY-AMOUNT NOT > ZERO
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'GIVE A STERLING OR A CURRENCY AMOUNT'
                                           TO CH-MESSAGE-TEXT
               WHEN CHFX-QUOTE
               AND  CHFX-GBP-AMOUNT > ZERO
               AND  CHFX-CCY-AMOUNT > ZERO
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'GIVE ONLY ONE OF STERLING / CURRENCY AMOUNT'
                                           TO CH-MESSAGE-TEXT
               WHEN CHFX-VALIDATE
               AND (CHFX-CHANNEL-RATE NOT NUMERIC
                    OR CHFX-CHANNEL-RATE = ZERO)
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'CHANNEL RATE REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 1000-EXIT
           END-IF

           MOVE CHFX-CCY-CD                TO WS-FX-KEY
           EXEC CICS READ
                     FILE('FXRATES')
                     INTO(FXR-RATE-RECORD)
                     LENGTH(LENGTH OF FXR-RATE-RECORD)
                     RIDFLD(WS-FX-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'NCCY'             TO CH-REASON-CD
                   MOVE 'CURRENCY NOT AVAILABLE'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   PERFORM 9000-FILE-ERROR
               WHEN FXR-SUSPENDED
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SUSP'             TO CH-REASON-CD
                   MOVE 'CURRENCY SUSPENDED BY TREASURY'
                                           TO CH-MESSAGE-TEXT
               WHEN FXR-MID-RATE NOT > ZERO
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'NRAT'             TO CH-REASON-CD
                   MOVE 'NO RATE LOADED FOR CURRENCY'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-QUOTE.
      *================================================================*
           IF CHFX-GBP-AMOUNT > ZERO
               MOVE CHFX-GBP-AMOUNT        TO WS-GBP-VALUE
           ELSE
               COMPUTE WS-GBP-VALUE ROUNDED =
                       CHFX-CCY-AMOUNT / FXR-MID-RATE
           END-IF
           IF FXR-MAX-DEAL-GBP > ZERO
           AND WS-GBP-VALUE > FXR-MAX-DEAL-GBP
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'MAXD'                 TO CH-REASON-CD
               MOVE 'OVER MAXIMUM DEAL SIZE - REFER TO TREASURY'
                                           TO CH-MESSAGE-TEXT
               GO TO 2000-EXIT
           END-IF
           PERFORM 5000-CUSTOMER-RATE

           IF CHFX-GBP-AMOUNT > ZERO
               MOVE CHFX-GBP-AMOUNT        TO CHFX-RESULT-GBP
               COMPUTE CHFX-RESULT-CCY ROUNDED =
                       CHFX-GBP-AMOUNT * WS-CUST-RATE
           ELSE
               MOVE CHFX-CCY-AMOUNT        TO CHFX-RESULT-CCY
               COMPUTE CHFX-RESULT-GBP ROUNDED =
                       CHFX-CCY-AMOUNT / WS-CUST-RATE
           END-IF
           PERFORM 5100-FILL-RESPONSE
           IF CH-RC-OK
               MOVE 'QUOTE ISSUED'         TO CH-MESSAGE-TEXT
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-VALIDATE-RATE.
      *================================================================*
           IF CHFX-GBP-AMOUNT > ZERO
               MOVE CHFX-GBP-AMOUNT        TO WS-GBP-VALUE
           ELSE
               COMPUTE WS-GBP-VALUE ROUNDED =
                       CHFX-CCY-AMOUNT / FXR-MID-RATE
           END-IF
           PERFORM 5000-CUSTOMER-RATE
           COMPUTE WS-DIFF-PCT ROUNDED =
                   (CHFX-CHANNEL-RATE - WS-CUST-RATE) * 100
                   / WS-CUST-RATE
           MOVE WS-DIFF-PCT                TO CHFX-DIFF-PCT
           PERFORM 5100-FILL-RESPONSE
           IF FUNCTION ABS (WS-DIFF-PCT) > WS-VALIDATE-TOL-PCT
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'FXRT'                 TO CH-REASON-CD
               MOVE 'CHANNEL RATE OUTSIDE TOLERANCE OF BANK RATE'
                                           TO CH-MESSAGE-TEXT
           ELSE
               IF CH-RC-OK
                   MOVE 'CHANNEL RATE ACCEPTED' TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           .
      *================================================================*
       4000-LIST.
      *================================================================*
           MOVE LOW-VALUES                 TO WS-FX-KEY
           MOVE 'N'                        TO WS-END-BROWSE-IND
           EXEC CICS STARTBR
                     FILE('FXRATES')
                     RIDFLD(WS-FX-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-FILE-ERROR
               GO TO 4000-EXIT
           END-IF
           PERFORM UNTIL WS-END-BROWSE OR CHFX-LIST-COUNT >= 40
               EXEC CICS READNEXT
                         FILE('FXRATES')
                         INTO(FXR-RATE-RECORD)
                         LENGTH(LENGTH OF FXR-RATE-RECORD)
                         RIDFLD(WS-FX-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       SET WS-END-BROWSE   TO TRUE
                   WHEN FXR-SUSPENDED
                   WHEN FXR-MID-RATE NOT > ZERO
                       CONTINUE
                   WHEN OTHER
                       ADD 1               TO CHFX-LIST-COUNT
                       MOVE CHFX-LIST-COUNT TO WS-SUB
                       IF FXR-TIER-MARGIN-BP (1) NOT NUMERIC
                       OR FXR-TIER-MARGIN-BP (1) = ZERO
                           MOVE WS-DEFAULT-MARGIN-BP TO WS-MARGIN-BP
                       ELSE
                           MOVE FXR-TIER-MARGIN-BP (1) TO WS-MARGIN-BP
                       END-IF
                       COMPUTE WS-SELL-RATE ROUNDED = FXR-MID-RATE
                           * (1 - WS-MARGIN-BP / 10000)
                       COMPUTE WS-BUY-RATE ROUNDED = FXR-MID-RATE
                           * (1 + WS-MARGIN-BP / 10000)
                       MOVE FXR-CCY-CD     TO CHFXL-CCY-CD (WS-SUB)
                       MOVE FXR-CCY-NAME   TO CHFXL-CCY-NAME (WS-SUB)
                       MOVE WS-BUY-RATE    TO CHFXL-BUY-RATE (WS-SUB)
                       MOVE WS-SELL-RATE   TO CHFXL-SELL-RATE (WS-SUB)
                       MOVE FXR-STATUS     TO CHFXL-STATUS (WS-SUB)
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR FILE('FXRATES') RESP(WS-RESP)
           END-EXEC
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       5000-CUSTOMER-RATE.
      *================================================================*
      *    TIER: FIRST TIER WHOSE UPPER LIMIT COVERS THE STERLING
      *    VALUE; A ZERO LIMIT MEANS 'AND ABOVE'.
           MOVE WS-DEFAULT-MARGIN-BP       TO WS-MARGIN-BP
           PERFORM VARYING WS-TIER FROM 1 BY 1 UNTIL WS-TIER > 4
               IF FXR-TIER-MARGIN-BP (WS-TIER) NUMERIC
               AND FXR-TIER-GBP-UPTO (WS-TIER) NUMERIC
                   IF FXR-TIER-GBP-UPTO (WS-TIER) = ZERO
                   OR WS-GBP-VALUE <= FXR-TIER-GBP-UPTO (WS-TIER)
                       MOVE FXR-TIER-MARGIN-BP (WS-TIER)
                                           TO WS-MARGIN-BP
                       MOVE 5              TO WS-TIER
                   END-IF
               END-IF
           END-PERFORM
           IF CHFX-BANK-SELLS
               COMPUTE WS-CUST-RATE ROUNDED = FXR-MID-RATE
                       * (1 - WS-MARGIN-BP / 10000)
           ELSE
               COMPUTE WS-CUST-RATE ROUNDED = FXR-MID-RATE
                       * (1 + WS-MARGIN-BP / 10000)
           END-IF
           .
      *================================================================*
       5100-FILL-RESPONSE.
      *================================================================*
           MOVE FXR-MID-RATE               TO CHFX-MID-RATE
           MOVE WS-CUST-RATE               TO CHFX-CUST-RATE
           MOVE WS-MARGIN-BP               TO CHFX-MARGIN-BP
           MOVE FXR-RATE-DATE              TO CHFX-RATE-DATE
           MOVE FXR-RATE-TIME              TO CHFX-RATE-TIME
           MOVE FXR-STATUS                 TO CHFX-RATE-STATUS
           IF FXR-HELD
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'HELD'                 TO CH-REASON-CD
               MOVE 'RATE ON HOLD - PREVIOUS FIXING USED'
                                           TO CH-MESSAGE-TEXT
           ELSE
               IF FXR-RATE-DATE NUMERIC AND FXR-RATE-DATE > ZERO
                   COMPUTE WS-AGE-DAYS =
                       FUNCTION INTEGER-OF-DATE (WS-TODAY)
                     - FUNCTION INTEGER-OF-DATE (FXR-RATE-DATE)
                   IF WS-AGE-DAYS > WS-STALE-DAYS
                       MOVE '0004'         TO CH-RETURN-CD
                       MOVE 'STAL'         TO CH-REASON-CD
                       MOVE 'RATE NOT REFRESHED - INDICATIVE ONLY'
                                           TO CH-MESSAGE-TEXT
                   END-IF
               END-IF
           END-IF
           .
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
           MOVE '0012'                     TO CH-RETURN-CD
           MOVE 'FILE'                     TO CH-REASON-CD
           STRING 'FXRATES FILE ERROR - RESP ' WS-RESP
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
