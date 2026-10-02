       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FXSCRN1.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-05-23.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : FXSCRN1                                             *
      * TITLE    : BRANCH FOREIGN EXCHANGE RATE BOARD AND QUOTE        *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION FX01       *
      * MAPSET   : FXMAPS  MAP FXM01                                   *
      * LINKS    : FXSVC01 (SERVICE FXQUOTE - FUNCTIONS L / Q)         *
      * FILES    : NONE - ALL RATES COME FROM FXSVC01                  *
      *----------------------------------------------------------------*
      * ON ENTRY THE TIER 1 BUY / SELL RATES FOR EVERY QUOTED CURRENCY *
      * ARE FETCHED WITH FXQUOTE FUNCTION L (UP TO 40 CURRENCIES) AND  *
      * KEPT IN THE COMMAREA. THEY ARE SHOWN 14 PER PAGE - PF7 / PF8   *
      * PAGE THROUGH THE COPY IN THE COMMAREA WITHOUT A NEW LINK.      *
      *                                                                *
      * THE COPY IS RE-FETCHED WHEN IT IS MORE THAN 15 MINUTES OLD     *
      * (FXRAT010 LOADS NEW RATES DURING THE DAY) AND ON PF5 / CLEAR.  *
      * 'RATES AS AT' SHOWS WHEN THE COPY ON SCREEN WAS FETCHED.       *
      *                                                                *
      * QUOTE: KEY CCY, DIRECTION (S = WE SELL CCY / CUSTOMER BUYS,    *
      * B = WE BUY CCY / CUSTOMER SELLS - DEFAULT S) AND EITHER THE    *
      * STERLING OR THE CURRENCY AMOUNT, THEN ENTER. FXSVC01 FUNCTION  *
      * Q RETURNS THE CUSTOMER RATE FOR THE MARGIN TIER THAT APPLIES   *
      * TO THE AMOUNT (SO A LARGE AMOUNT GETS A BETTER RATE THAN THE   *
      * TIER 1 RATE ON THE BOARD).                                     *
      *                                                                *
      * THIS IS AN INDICATIVE QUOTE ONLY - THE DEAL IS STRUCK WHEN THE *
      * PAYMENT OR CASH TRANSACTION IS POSTED (TELLER FX OR PY01).     *
      * ACCESS: RACF PROFILE CICSPRD1.FX01 (GROUP BRSTAFF).            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-05-23 JNB  ORIGINAL VERSION - REPLACES THE PRINTED RATE   *
      *                 SHEET SENT TO BRANCHES BY FXRAT020             *
      * 2011-07-04 SAT  RATES KEPT IN THE COMMAREA - PF7 / PF8 NO      *
      *                 LONGER LINK TO FXSVC01 (CICS PERF REVIEW)      *
      * 2011-09-12 AOS  15 MINUTE REFRESH OF THE COMMAREA COPY         *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'FXSCRN1 '.
      *
       01  WS-CONSTANTS.
           05  WS-ROWS-PER-PAGE            PIC 9(02) VALUE 14.
           05  WS-MAX-RATES                PIC 9(02) VALUE 40.
      *    15 MINUTES IN MILLISECONDS (ABSTIME UNITS).
           05  WS-STALE-MS                 PIC S9(15) COMP-3
                                           VALUE +900000.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-AGE-MS                   PIC S9(15) COMP-3.
           05  WS-LINE                     PIC S9(04) COMP.
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-LAST-PAGE                PIC S9(04) COMP.
           05  WS-PAGE-MSG                 PIC X(79).
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'FX01 - FOREIGN EXCHANGE RATES ENDED'.
           05  WS-INVALID-KEY-MSG          PIC X(50)
                   VALUE 'INVALID KEY - USE ENTER, PF3, PF5, PF7, PF8'.
           05  WS-VALID-IND                PIC X(01).
               88  WS-INPUT-VALID              VALUE 'Y'.
           05  WS-FOUND-IND                PIC X(01).
               88  WS-CCY-FOUND                VALUE 'Y'.
           05  WS-QUOTE-REQ-IND            PIC X(01).
               88  WS-QUOTE-REQUESTED          VALUE 'Y'.
           05  WS-FETCH-DATE               PIC X(10).
           05  WS-FETCH-TIME               PIC X(08).
           05  WS-PAGE-NO-EDIT             PIC 9(01).
           05  WS-LAST-PAGE-EDIT           PIC 9(01).
           05  WS-COUNT-EDIT               PIC Z9.
           05  WS-GBP-AMOUNT               PIC S9(13)V99.
           05  WS-CCY-AMOUNT               PIC S9(13)V99.
      *
      *---- EDIT PICTURES SIZED TO THE MAP FIELDS --------------------*
       01  WS-EDIT-FIELDS.
           05  WS-RATE-EDIT                PIC ZZZZ9.999999.
           05  WS-MARGIN-EDIT              PIC ZZZ9.
           05  WS-AMT13-EDIT               PIC ZZ,ZZZ,ZZ9.99.
      *
      *---- AMOUNT PARSING (SAME RULES AS SOSCRN1 6000-PARSE-AMOUNT) -*
       01  WS-AMOUNT-FIELDS.
           05  WS-AMOUNT-IN                PIC X(13).
           05  WS-AMOUNT-NUM               PIC S9(13)V99.
           05  WS-AMOUNT-OK-IND            PIC X(01).
               88  WS-AMOUNT-OK                VALUE 'Y'.
           05  WS-DOT-POS                  PIC S9(04) COMP.
           05  WS-CHAR-SUB                 PIC S9(04) COMP.
           05  WS-INT-PART                 PIC 9(11).
           05  WS-DEC-PART                 PIC 9(02).
           05  WS-DIGIT-COUNT              PIC S9(04) COMP.
           05  WS-DEC-COUNT                PIC S9(04) COMP.
           05  WS-ONE-CHAR                 PIC X(01).
      *
      *---- PSEUDO-CONVERSATIONAL COMMAREA ---------------------------*
       01  WS-COMMAREA.
           05  CA-EYECATCHER               PIC X(04).
               88  CA-OURS                     VALUE 'FX01'.
           05  CA-FETCH-ABSTIME            PIC S9(15) COMP-3.
           05  CA-ASAT                     PIC X(20).
           05  CA-PAGE                     PIC 9(02).
           05  CA-RATE-COUNT               PIC 9(02).
           05  CA-RATE                     OCCURS 40 TIMES.
               10  CA-R-CCY-CD             PIC X(03).
               10  CA-R-CCY-NAME           PIC X(20).
               10  CA-R-BUY-RATE           PIC 9(05)V9(06).
               10  CA-R-SELL-RATE          PIC 9(05)V9(06).
               10  CA-R-STATUS             PIC X(01).
      *        QUOTE INPUT - A FIELD NOT RE-KEYED KEEPS ITS VALUE
           05  CA-Q-INPUT.
               10  CA-Q-CCY                PIC X(03).
               10  CA-Q-DIR                PIC X(01).
                   88  CA-Q-WE-SELL            VALUE 'S'.
                   88  CA-Q-WE-BUY             VALUE 'B'.
               10  CA-Q-GBP                PIC X(13).
               10  CA-Q-CCYA               PIC X(13).
      *        LAST QUOTE RESULT, RE-SHOWN WHEN PAGING
           05  CA-Q-RESULT.
               10  CA-QR-RATE              PIC X(12).
               10  CA-QR-MARGIN            PIC X(04).
               10  CA-QR-GBP               PIC X(13).
               10  CA-QR-CCY               PIC X(13).
      *
       01  WS-FX-LINE.
           05  FL-CCY-CD                   PIC X(03).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  FL-CCY-NAME                 PIC X(20).
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  FL-BUY-RATE                 PIC ZZZZ9.999999.
           05  FILLER                      PIC X(09) VALUE SPACES.
           05  FL-SELL-RATE                PIC ZZZZ9.999999.
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  FL-STATUS                   PIC X(10).
      *
           COPY FXMAPS.
      *
           COPY DFHAID.
      *
           COPY DFHBMSCA.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(1948).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           MOVE SPACES                     TO WS-PAGE-MSG

           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                   PERFORM 1000-FIRST-ENTRY
               WHEN OTHER
                   MOVE DFHCOMMAREA        TO WS-COMMAREA
                   IF NOT CA-OURS
                       PERFORM 1000-FIRST-ENTRY
                   ELSE
                       EVALUATE EIBAID
                           WHEN DFHPF3
                               PERFORM 9000-END-SESSION
                           WHEN DFHCLEAR
                               PERFORM 1000-FIRST-ENTRY
                           WHEN DFHPF5
                               MOVE 'RATES REFRESHED' TO WS-PAGE-MSG
                               PERFORM 3000-FETCH-RATES THRU 3000-EXIT
                               PERFORM 3500-BUILD-PAGE
                           WHEN DFHPF7
                               PERFORM 4000-PAGE-BACK
                           WHEN DFHPF8
                               PERFORM 4100-PAGE-FORWARD
                           WHEN DFHENTER
                               PERFORM 2000-PROCESS-ENTER THRU 2000-EXIT
                           WHEN OTHER
                               MOVE LOW-VALUES TO FXM01O
                               MOVE WS-INVALID-KEY-MSG
                                           TO MSGO
                               PERFORM 8000-SEND-DATAONLY
                       END-EVALUATE
                   END-IF
           END-EVALUATE

           EXEC CICS RETURN
                     TRANSID('FX01')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       1000-FIRST-ENTRY.
      *================================================================*
           PERFORM 1200-INIT-COMMAREA
           PERFORM 3000-FETCH-RATES THRU 3000-EXIT
           PERFORM 3500-BUILD-PAGE
           .
      *================================================================*
       1200-INIT-COMMAREA.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           MOVE ZERO                       TO CA-FETCH-ABSTIME
                                              CA-RATE-COUNT
           MOVE 1                          TO CA-PAGE
           MOVE 'S'                        TO CA-Q-DIR
           .
      *================================================================*
       2000-PROCESS-ENTER.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('FXM01')
                     MAPSET('FXMAPS')
                     INTO(FXM01I)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(MAPFAIL)
               PERFORM 2100-RECEIVE-QUOTE-FIELDS
           END-IF

      *    A STALE COPY IS REFRESHED BEFORE ANYTHING IS SHOWN.
           PERFORM 3100-CHECK-STALE

           IF CA-Q-CCY = SPACES
      *        NOTHING TO QUOTE - JUST REDISPLAY THE CURRENT PAGE.
               MOVE SPACES                 TO CA-Q-RESULT
               PERFORM 3500-BUILD-PAGE
               GO TO 2000-EXIT
           END-IF

           PERFORM 2200-VALIDATE-QUOTE THRU 2200-EXIT
           IF NOT WS-INPUT-VALID
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

           PERFORM 5000-QUOTE THRU 5000-EXIT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-RECEIVE-QUOTE-FIELDS.
      *================================================================*
      *    A FIELD NOT RE-KEYED KEEPS ITS COMMAREA VALUE (FSET).
           IF QCCYL > ZERO
               MOVE FUNCTION UPPER-CASE (QCCYI) TO CA-Q-CCY
           END-IF
           IF QDIRL > ZERO
               MOVE FUNCTION UPPER-CASE (QDIRI) TO CA-Q-DIR
           END-IF
           IF QGBPL > ZERO
               MOVE QGBPI                  TO CA-Q-GBP
           END-IF
           IF QCCYAL > ZERO
               MOVE QCCYAI                 TO CA-Q-CCYA
           END-IF
           INSPECT CA-Q-INPUT REPLACING ALL LOW-VALUE BY SPACE
           IF CA-Q-DIR = SPACE
               MOVE 'S'                    TO CA-Q-DIR
           END-IF
           .
      *================================================================*
       2200-VALIDATE-QUOTE.
      *================================================================*
           MOVE 'Y'                        TO WS-VALID-IND
           MOVE LOW-VALUES                 TO FXM01O

           IF CA-Q-CCY = 'GBP' OR CA-Q-CCY NOT ALPHABETIC
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'CCY MUST BE A 3-LETTER ISO CODE OTHER THAN GBP'
                                           TO MSGO
               MOVE -1                     TO QCCYL
               MOVE DFHRED                 TO QCCYA
               GO TO 2200-EXIT
           END-IF

      *    CHECK AGAINST THE BOARD FIRST - SAVES A LINK FOR A TYPO.
      *    SUSPENDED CURRENCIES ARE NOT ON THE BOARD (FXSVC01 SKIPS
      *    THEM) AND ARE REFUSED HERE.
           MOVE 'N'                        TO WS-FOUND-IND
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > CA-RATE-COUNT
                      OR WS-CCY-FOUND
               IF CA-R-CCY-CD (WS-SUB) = CA-Q-CCY
                   MOVE 'Y'                TO WS-FOUND-IND
               END-IF
           END-PERFORM
           IF NOT WS-CCY-FOUND AND CA-RATE-COUNT > ZERO
               MOVE 'N'                    TO WS-VALID-IND
               STRING CA-Q-CCY
                      ' IS NOT QUOTED TODAY - SEE THE RATE BOARD'
                      DELIMITED BY SIZE
                      INTO MSGO
               END-STRING
               MOVE -1                     TO QCCYL
               MOVE DFHRED                 TO QCCYA
               GO TO 2200-EXIT
           END-IF

           IF NOT (CA-Q-WE-SELL OR CA-Q-WE-BUY)
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'DIRECTION MUST BE S (CUSTOMER BUYS) OR B (SELLS)'
                                           TO MSGO
               MOVE -1                     TO QDIRL
               MOVE DFHRED                 TO QDIRA
               GO TO 2200-EXIT
           END-IF

           MOVE CA-Q-GBP                   TO WS-AMOUNT-IN
           PERFORM 6000-PARSE-AMOUNT THRU 6000-EXIT
           IF NOT WS-AMOUNT-OK
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'GBP AMOUNT IS NOT A VALID AMOUNT' TO MSGO
               MOVE -1                     TO QGBPL
               MOVE DFHRED                 TO QGBPA
               GO TO 2200-EXIT
           END-IF
           MOVE WS-AMOUNT-NUM              TO WS-GBP-AMOUNT

           MOVE CA-Q-CCYA                  TO WS-AMOUNT-IN
           PERFORM 6000-PARSE-AMOUNT THRU 6000-EXIT
           IF NOT WS-AMOUNT-OK
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'CCY AMOUNT IS NOT A VALID AMOUNT' TO MSGO
               MOVE -1                     TO QCCYAL
               MOVE DFHRED                 TO QCCYAA
               GO TO 2200-EXIT
           END-IF
           MOVE WS-AMOUNT-NUM              TO WS-CCY-AMOUNT

           EVALUATE TRUE
               WHEN WS-GBP-AMOUNT = ZERO AND WS-CCY-AMOUNT = ZERO
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'KEY THE GBP AMOUNT OR THE CURRENCY AMOUNT'
                                           TO MSGO
                   MOVE -1                 TO QGBPL
               WHEN WS-GBP-AMOUNT > ZERO AND WS-CCY-AMOUNT > ZERO
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'KEY ONE AMOUNT ONLY - GBP OR CURRENCY'
                                           TO MSGO
                   MOVE -1                 TO QCCYAL
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       3000-FETCH-RATES.
      *================================================================*
           PERFORM 7000-INIT-SERVICE
           SET CHFX-LIST                   TO TRUE
           PERFORM 7100-LINK-SERVICE

           IF NOT (CH-RC-OK OR CH-RC-WARNING)
      *        KEEP ANY OLD COPY - BETTER THAN AN EMPTY BOARD - BUT
      *        SAY SO.
               MOVE CH-MESSAGE-TEXT        TO WS-PAGE-MSG
               GO TO 3000-EXIT
           END-IF

           MOVE CHFX-LIST-COUNT            TO CA-RATE-COUNT
           IF CA-RATE-COUNT > WS-MAX-RATES
               MOVE WS-MAX-RATES           TO CA-RATE-COUNT
           END-IF
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-MAX-RATES
               IF WS-SUB <= CA-RATE-COUNT
                   MOVE CHFXL-CCY-CD (WS-SUB)
                                           TO CA-R-CCY-CD (WS-SUB)
                   MOVE CHFXL-CCY-NAME (WS-SUB)
                                           TO CA-R-CCY-NAME (WS-SUB)
                   MOVE CHFXL-BUY-RATE (WS-SUB)
                                           TO CA-R-BUY-RATE (WS-SUB)
                   MOVE CHFXL-SELL-RATE (WS-SUB)
                                           TO CA-R-SELL-RATE (WS-SUB)
                   MOVE CHFXL-STATUS (WS-SUB)
                                           TO CA-R-STATUS (WS-SUB)
               ELSE
                   MOVE SPACES             TO CA-R-CCY-CD (WS-SUB)
                                              CA-R-CCY-NAME (WS-SUB)
                                              CA-R-STATUS (WS-SUB)
                   MOVE ZERO               TO CA-R-BUY-RATE (WS-SUB)
                                              CA-R-SELL-RATE (WS-SUB)
               END-IF
           END-PERFORM

           MOVE WS-ABSTIME                 TO CA-FETCH-ABSTIME
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     DDMMYYYY(WS-FETCH-DATE)
                     DATESEP('/')
                     TIME(WS-FETCH-TIME)
                     TIMESEP(':')
           END-EXEC
           MOVE SPACES                     TO CA-ASAT
           STRING WS-FETCH-DATE ' ' WS-FETCH-TIME
                  DELIMITED BY SIZE
                  INTO CA-ASAT
           END-STRING

      *    THE NUMBER OF PAGES MAY HAVE CHANGED.
           PERFORM 3200-LAST-PAGE
           IF CA-PAGE > WS-LAST-PAGE OR CA-PAGE = ZERO
               MOVE 1                      TO CA-PAGE
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-CHECK-STALE.
      *================================================================*
           COMPUTE WS-AGE-MS = WS-ABSTIME - CA-FETCH-ABSTIME
           IF CA-RATE-COUNT = ZERO
           OR WS-AGE-MS > WS-STALE-MS
           OR WS-AGE-MS < ZERO
               PERFORM 3000-FETCH-RATES THRU 3000-EXIT
           END-IF
           .
      *================================================================*
       3200-LAST-PAGE.
      *================================================================*
           COMPUTE WS-LAST-PAGE =
                   (CA-RATE-COUNT + WS-ROWS-PER-PAGE - 1)
                   / WS-ROWS-PER-PAGE
           IF WS-LAST-PAGE = ZERO
               MOVE 1                      TO WS-LAST-PAGE
           END-IF
           .
      *================================================================*
       3500-BUILD-PAGE.
      *================================================================*
      *    BUILDS PAGE CA-PAGE FROM THE COMMAREA COPY. WS-PAGE-MSG,
      *    IF SET, GOES IN THE MESSAGE LINE.
           MOVE LOW-VALUES                 TO FXM01O
           PERFORM 3200-LAST-PAGE
           IF CA-PAGE > WS-LAST-PAGE
               MOVE WS-LAST-PAGE           TO CA-PAGE
           END-IF

           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-ROWS-PER-PAGE
               COMPUTE WS-SUB = (CA-PAGE - 1) * WS-ROWS-PER-PAGE
                              + WS-LINE
               MOVE SPACES                 TO WS-FX-LINE
               IF WS-SUB <= CA-RATE-COUNT
                   PERFORM 3600-FORMAT-RATE-LINE
               END-IF
               PERFORM 3700-PLACE-LINE
           END-PERFORM

           MOVE CA-PAGE                    TO WS-PAGE-NO-EDIT
           MOVE WS-LAST-PAGE               TO WS-LAST-PAGE-EDIT
           STRING WS-PAGE-NO-EDIT '/' WS-LAST-PAGE-EDIT
                  DELIMITED BY SIZE
                  INTO FXPAGEO
           END-STRING
           MOVE CA-ASAT                    TO FXASATO

           PERFORM 5100-SHOW-QUOTE-FIELDS

           EVALUATE TRUE
               WHEN WS-PAGE-MSG NOT = SPACES
                   MOVE WS-PAGE-MSG        TO MSGO
               WHEN CA-RATE-COUNT = ZERO
                   MOVE 'NO RATES AVAILABLE - TRY AGAIN LATER'
                                           TO MSGO
               WHEN CA-PAGE < WS-LAST-PAGE
                   STRING 'PF8 FOR MORE CURRENCIES. KEY CCY, DIR'
                          ' AND ONE AMOUNT FOR A QUOTE'
                          DELIMITED BY SIZE
                          INTO MSGO
                   END-STRING
               WHEN OTHER
                   MOVE CA-RATE-COUNT      TO WS-COUNT-EDIT
                   STRING WS-COUNT-EDIT ' CURRENCIES. KEY CCY, DIR'
                          ' AND ONE AMOUNT FOR A QUOTE'
                          DELIMITED BY SIZE
                          INTO MSGO
                   END-STRING
           END-EVALUATE
           MOVE -1                         TO QCCYL
           PERFORM 8100-SEND-MAP
           .
      *================================================================*
       3600-FORMAT-RATE-LINE.
      *================================================================*
           MOVE CA-R-CCY-CD (WS-SUB)       TO FL-CCY-CD
           MOVE CA-R-CCY-NAME (WS-SUB)     TO FL-CCY-NAME
           MOVE CA-R-BUY-RATE (WS-SUB)     TO FL-BUY-RATE
           MOVE CA-R-SELL-RATE (WS-SUB)    TO FL-SELL-RATE
           EVALUATE CA-R-STATUS (WS-SUB)
               WHEN 'A'  MOVE 'ACTIVE'     TO FL-STATUS
      *        HELD BY THE FXRAT010 TOLERANCE CHECK - OLD RATE.
               WHEN 'H'  MOVE 'HELD'       TO FL-STATUS
               WHEN 'S'  MOVE 'SUSPENDED'  TO FL-STATUS
               WHEN OTHER
                   MOVE CA-R-STATUS (WS-SUB) TO FL-STATUS
           END-EVALUATE
           .
      *================================================================*
       3700-PLACE-LINE.
      *================================================================*
           EVALUATE WS-LINE
               WHEN 1   MOVE WS-FX-LINE TO FXL1O
               WHEN 2   MOVE WS-FX-LINE TO FXL2O
               WHEN 3   MOVE WS-FX-LINE TO FXL3O
               WHEN 4   MOVE WS-FX-LINE TO FXL4O
               WHEN 5   MOVE WS-FX-LINE TO FXL5O
               WHEN 6   MOVE WS-FX-LINE TO FXL6O
               WHEN 7   MOVE WS-FX-LINE TO FXL7O
               WHEN 8   MOVE WS-FX-LINE TO FXL8O
               WHEN 9   MOVE WS-FX-LINE TO FXL9O
               WHEN 10  MOVE WS-FX-LINE TO FXL10O
               WHEN 11  MOVE WS-FX-LINE TO FXL11O
               WHEN 12  MOVE WS-FX-LINE TO FXL12O
               WHEN 13  MOVE WS-FX-LINE TO FXL13O
               WHEN 14  MOVE WS-FX-LINE TO FXL14O
           END-EVALUATE
           .
      *================================================================*
       4000-PAGE-BACK.
      *================================================================*
           PERFORM 3100-CHECK-STALE
           IF CA-PAGE <= 1
               MOVE 1                      TO CA-PAGE
               MOVE 'ALREADY AT THE FIRST PAGE' TO WS-PAGE-MSG
           ELSE
               SUBTRACT 1                  FROM CA-PAGE
           END-IF
           PERFORM 3500-BUILD-PAGE
           .
      *================================================================*
       4100-PAGE-FORWARD.
      *================================================================*
           PERFORM 3100-CHECK-STALE
           PERFORM 3200-LAST-PAGE
           IF CA-PAGE >= WS-LAST-PAGE
               MOVE 'NO MORE CURRENCIES'   TO WS-PAGE-MSG
           ELSE
               ADD 1                       TO CA-PAGE
           END-IF
           PERFORM 3500-BUILD-PAGE
           .
      *================================================================*
       5000-QUOTE.
      *================================================================*
           PERFORM 7000-INIT-SERVICE
           SET CHFX-QUOTE                  TO TRUE
           MOVE CA-Q-CCY                   TO CHFX-CCY-CD
           MOVE CA-Q-DIR                   TO CHFX-DIRECTION
           MOVE WS-GBP-AMOUNT              TO CHFX-GBP-AMOUNT
           MOVE WS-CCY-AMOUNT              TO CHFX-CCY-AMOUNT
           PERFORM 7100-LINK-SERVICE

           MOVE SPACES                     TO CA-Q-RESULT
           IF NOT (CH-RC-OK OR CH-RC-WARNING)
               MOVE LOW-VALUES             TO FXM01O
               MOVE CH-MESSAGE-TEXT        TO MSGO
               EVALUATE CH-REASON-CD
                   WHEN 'INVD'
                       MOVE -1             TO QDIRL
                   WHEN 'INVA'
                   WHEN 'MAXD'
                       MOVE -1             TO QGBPL
                   WHEN OTHER
                       MOVE -1             TO QCCYL
               END-EVALUATE
               MOVE SPACES                 TO QRATEO QMARGO
                                              QRGBPO QRCCYO
               PERFORM 8000-SEND-DATAONLY
               GO TO 5000-EXIT
           END-IF

           MOVE CHFX-CUST-RATE             TO WS-RATE-EDIT
           MOVE WS-RATE-EDIT               TO CA-QR-RATE
           MOVE CHFX-MARGIN-BP             TO WS-MARGIN-EDIT
           MOVE WS-MARGIN-EDIT             TO CA-QR-MARGIN
           MOVE CHFX-RESULT-GBP            TO WS-AMT13-EDIT
           MOVE WS-AMT13-EDIT              TO CA-QR-GBP
           MOVE CHFX-RESULT-CCY            TO WS-AMT13-EDIT
           MOVE WS-AMT13-EDIT              TO CA-QR-CCY

           IF CH-RC-WARNING
      *        HELD OR STALE RATE - FXSVC01'S WARNING IS SHOWN AS IS.
               MOVE CH-MESSAGE-TEXT        TO WS-PAGE-MSG
           ELSE
               STRING 'INDICATIVE QUOTE - RATE SET '
                      CHFX-RATE-DATE (7:2) '/'
                      CHFX-RATE-DATE (5:2) '/'
                      CHFX-RATE-DATE (1:4) ' '
                      CHFX-RATE-TIME (1:2) ':'
                      CHFX-RATE-TIME (3:2)
                      DELIMITED BY SIZE
                      INTO WS-PAGE-MSG
               END-STRING
           END-IF
           PERFORM 3500-BUILD-PAGE
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       5100-SHOW-QUOTE-FIELDS.
      *================================================================*
      *    THE MAP IS SENT WITH ERASE, SO THE QUOTE INPUT AND THE LAST
      *    RESULT ARE PUT BACK FROM THE COMMAREA.
           MOVE CA-Q-CCY                   TO QCCYO
           MOVE CA-Q-DIR                   TO QDIRO
           MOVE CA-Q-GBP                   TO QGBPO
           MOVE CA-Q-CCYA                  TO QCCYAO
           MOVE CA-QR-RATE                 TO QRATEO
           MOVE CA-QR-MARGIN               TO QMARGO
           MOVE CA-QR-GBP                  TO QRGBPO
           MOVE CA-QR-CCY                  TO QRCCYO
           .
      *================================================================*
       6000-PARSE-AMOUNT.
      *================================================================*
      *    ACCEPTS '1234', '1234.5', '1,234.56'. BLANK = ZERO.
           MOVE 'Y'                        TO WS-AMOUNT-OK-IND
           MOVE ZERO                       TO WS-AMOUNT-NUM
                                              WS-INT-PART WS-DEC-PART
                                              WS-DIGIT-COUNT
                                              WS-DEC-COUNT
           MOVE ZERO                       TO WS-DOT-POS
           IF WS-AMOUNT-IN = SPACES OR LOW-VALUES
               GO TO 6000-EXIT
           END-IF
           PERFORM VARYING WS-CHAR-SUB FROM 1 BY 1
                   UNTIL WS-CHAR-SUB > LENGTH OF WS-AMOUNT-IN
               MOVE WS-AMOUNT-IN (WS-CHAR-SUB:1) TO WS-ONE-CHAR
               EVALUATE TRUE
                   WHEN WS-ONE-CHAR = SPACE OR LOW-VALUE OR ','
                       CONTINUE
                   WHEN WS-ONE-CHAR = '.'
                       IF WS-DOT-POS > ZERO
                           MOVE 'N'        TO WS-AMOUNT-OK-IND
                       END-IF
                       MOVE WS-CHAR-SUB    TO WS-DOT-POS
                   WHEN WS-ONE-CHAR IS NUMERIC
                       IF WS-DOT-POS = ZERO
                           COMPUTE WS-INT-PART = WS-INT-PART * 10
                                   + FUNCTION NUMVAL (WS-ONE-CHAR)
                           ADD 1           TO WS-DIGIT-COUNT
                       ELSE
                           ADD 1           TO WS-DEC-COUNT
                           IF WS-DEC-COUNT = 1
                               COMPUTE WS-DEC-PART =
                                   FUNCTION NUMVAL (WS-ONE-CHAR) * 10
                           ELSE
                               IF WS-DEC-COUNT = 2
                                   COMPUTE WS-DEC-PART = WS-DEC-PART
                                     + FUNCTION NUMVAL (WS-ONE-CHAR)
                               ELSE
                                   MOVE 'N' TO WS-AMOUNT-OK-IND
                               END-IF
                           END-IF
                       END-IF
                   WHEN OTHER
                       MOVE 'N'            TO WS-AMOUNT-OK-IND
               END-EVALUATE
           END-PERFORM
           IF WS-DIGIT-COUNT > 9
               MOVE 'N'                    TO WS-AMOUNT-OK-IND
           END-IF
           IF WS-AMOUNT-OK
               COMPUTE WS-AMOUNT-NUM = WS-INT-PART + WS-DEC-PART / 100
           END-IF
           .
       6000-EXIT.
           EXIT.
      *================================================================*
       7000-INIT-SERVICE.
      *================================================================*
           MOVE SPACES                     TO CH-COMMAREA
           SET CH-EYECATCHER-OK            TO TRUE
           MOVE '01'                       TO CH-VERSION
           MOVE 'FXQUOTE '                 TO CH-SERVICE-CD
           MOVE 'BRANCH'                   TO CH-CHANNEL-CD
           MOVE EIBTRMID                   TO CH-USER-ID
           STRING 'FX01' EIBTRMID EIBTASKN
                  DELIMITED BY SIZE
                  INTO CH-REQUEST-ID
           END-STRING
           MOVE SPACE                      TO CHFX-DIRECTION
           MOVE ZERO                       TO CHFX-GBP-AMOUNT
                                              CHFX-CCY-AMOUNT
                                              CHFX-CHANNEL-RATE
           .
      *================================================================*
       7100-LINK-SERVICE.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM('FXSVC01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'LINK'                 TO CH-REASON-CD
               MOVE WS-RESP                TO WS-RESP-DISPLAY
               MOVE SPACES                 TO CH-MESSAGE-TEXT
               STRING 'FX RATE SERVICE UNAVAILABLE - EIBRESP '
                      WS-RESP-DISPLAY
                      DELIMITED BY SIZE
                      INTO CH-MESSAGE-TEXT
               END-STRING
           END-IF
           .
      *================================================================*
       8000-SEND-DATAONLY.
      *================================================================*
           PERFORM 8200-SET-DATE-TIME
           EXEC CICS SEND
                     MAP('FXM01')
                     MAPSET('FXMAPS')
                     FROM(FXM01O)
                     DATAONLY
                     CURSOR
                     FREEKB
           END-EXEC
           .
      *================================================================*
       8100-SEND-MAP.
      *================================================================*
           PERFORM 8200-SET-DATE-TIME
           EXEC CICS SEND
                     MAP('FXM01')
                     MAPSET('FXMAPS')
                     FROM(FXM01O)
                     ERASE
                     CURSOR
                     FREEKB
           END-EXEC
           .
      *================================================================*
       8200-SET-DATE-TIME.
      *================================================================*
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     DDMMYYYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC
           .
      *================================================================*
       9000-END-SESSION.
      *================================================================*
           EXEC CICS SEND TEXT
                     FROM(WS-END-MESSAGE)
                     LENGTH(LENGTH OF WS-END-MESSAGE)
                     ERASE
                     FREEKB
           END-EXEC
           EXEC CICS RETURN
           END-EXEC
           .
