       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SOSCRN1.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2004-11-08.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : SOSCRN1                                             *
      * TITLE    : BRANCH STANDING ORDER MAINTENANCE                   *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION SO01       *
      * MAPSET   : SOMAPS  MAPS SOM01 (LIST) AND SOM02 (DETAIL)        *
      * LINKS    : SOMNT01 (SERVICE SOMAINT)                           *
      *----------------------------------------------------------------*
      * LIST SCREEN (SOM01)                                            *
      *   SORT CODE + ACCOUNT, ENTER = LIST UP TO 12 ORDERS            *
      *   SELECT A LINE WITH  I (ENQUIRE)  A (AMEND)  X (CANCEL)       *
      *   PF5  = NEW STANDING ORDER   PF8 = NEXT PAGE                  *
      * DETAIL SCREEN (SOM02)                                          *
      *   SHOWS ONE ORDER. IN CREATE AND AMEND MODE THE INPUT FIELDS   *
      *   ARE UNPROTECTED; ENTER SAVES. PF12 RETURNS TO THE LIST.      *
      *   CANCEL ASKS FOR CONFIRMATION (ENTER AGAIN ON SOM02).         *
      *                                                                *
      * ALL BUSINESS RULES ARE IN SOMNT01 - THIS PROGRAM ONLY FORMATS  *
      * SCREENS AND PASSES ON THE SERVICE'S MESSAGE. THE CHANNEL CODE  *
      * IS 'BRANCH' SO THE BRANCH CHANNEL LIMIT APPLIES.               *
      *                                                                *
      * ACCESS: RACF PROFILE CICSPRD1.SO01 (GROUP BRSTAFF).            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2004-11-08 SAT  ORIGINAL VERSION (READ/WRITE OF SO MASTER      *
      *                 DIRECTLY - REPLACED TELLER SO SCREENS)         *
      * 2010-03-08 AOS  REWRITTEN TO CALL SOMNT01 - BUSINESS RULES IN  *
      *                 ONE PLACE FOR BRANCH AND IBANK                 *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'SOSCRN1 '.
      *
       01  WS-CONSTANTS.
           05  WS-LINES-PER-PAGE           PIC 9(02) VALUE 12.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-LINE                     PIC S9(04) COMP.
           05  WS-SEL-CODE                 PIC X(01).
           05  WS-SEL-COUNT                PIC S9(04) COMP.
           05  WS-SEL-LINE                 PIC S9(04) COMP.
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'SO01 - STANDING ORDERS ENDED'.
           05  WS-AMOUNT-EDIT              PIC ZZ,ZZZ,ZZ9.99.
           05  WS-AMOUNT-IN                PIC X(12).
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
           05  WS-DATE-DISPLAY             PIC X(10).
      *
      *---- PSEUDO-CONVERSATIONAL COMMAREA ---------------------------*
       01  WS-COMMAREA.
           05  CA-EYECATCHER               PIC X(04).
               88  CA-OURS                     VALUE 'SO01'.
           05  CA-SCREEN                   PIC X(01).
               88  CA-ON-LIST                  VALUE 'L'.
               88  CA-ON-DETAIL                VALUE 'D'.
           05  CA-MODE                     PIC X(01).
               88  CA-MODE-ENQUIRE             VALUE 'I'.
               88  CA-MODE-CREATE              VALUE 'C'.
               88  CA-MODE-AMEND               VALUE 'A'.
               88  CA-MODE-CANCEL              VALUE 'X'.
           05  CA-SORT-CODE                PIC X(06).
           05  CA-ACCT-NO                  PIC X(08).
           05  CA-CIF-NO                   PIC X(12).
           05  CA-SO-SEQ                   PIC 9(04).
           05  CA-RESTART-SEQ              PIC 9(04).
           05  CA-MORE                     PIC X(01).
           05  CA-LIST-SEQ                 PIC 9(04) OCCURS 12 TIMES.
      *
       01  WS-CF02-COMMAREA.
           05  XC-FROM-TRAN                PIC X(04).
           05  XC-SORT-CODE                PIC X(06).
           05  XC-ACCT-NO                  PIC X(08).
           05  XC-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(20).
      *
       01  WS-SO-LINE.
           05  SL-SEQ                      PIC 9(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-STATUS                   PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-BENE-NAME                PIC X(25).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-BENE-SORT                PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-BENE-ACCT                PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-FREQ                     PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-NEXT-DUE                 PIC X(10).
           05  SL-AMOUNT                   PIC ZZZ,ZZ9.99.
      *
           COPY SOMAPS.
      *
           COPY DFHAID.
      *
           COPY DFHBMSCA.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(89).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC

           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                   PERFORM 1000-SEND-EMPTY-LIST
               WHEN EIBCALEN = LENGTH OF WS-CF02-COMMAREA
                   MOVE DFHCOMMAREA (1:50) TO WS-CF02-COMMAREA
                   PERFORM 1100-START-FROM-CF02
               WHEN OTHER
                   MOVE DFHCOMMAREA        TO WS-COMMAREA
                   IF CA-ON-LIST
                       PERFORM 2000-LIST-SCREEN-INPUT
                   ELSE
                       PERFORM 5000-DETAIL-SCREEN-INPUT
                   END-IF
           END-EVALUATE

           EXEC CICS RETURN
                     TRANSID('SO01')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       1000-SEND-EMPTY-LIST.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           SET CA-ON-LIST                  TO TRUE
           MOVE ZERO                       TO CA-SO-SEQ CA-RESTART-SEQ
           MOVE LOW-VALUES                 TO SOM01O
           MOVE 'KEY SORT CODE AND ACCOUNT AND PRESS ENTER'
                                           TO MSGO OF SOM01O
           PERFORM 8000-SEND-LIST
           .
      *================================================================*
       1100-START-FROM-CF02.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           SET CA-ON-LIST                  TO TRUE
           MOVE XC-SORT-CODE               TO CA-SORT-CODE
           MOVE XC-ACCT-NO                 TO CA-ACCT-NO
           MOVE XC-CIF-NO                  TO CA-CIF-NO
           MOVE ZERO                       TO CA-RESTART-SEQ
           PERFORM 3000-LIST-ORDERS
           .
      *================================================================*
       2000-LIST-SCREEN-INPUT.
      *================================================================*
           EVALUATE EIBAID
               WHEN DFHPF3
                   PERFORM 9000-END-SESSION
               WHEN DFHPF12
                   PERFORM 9100-RETURN-TO-CF02
               WHEN DFHCLEAR
                   PERFORM 1000-SEND-EMPTY-LIST
               WHEN DFHPF5
                   PERFORM 2300-RECEIVE-LIST-KEYS
                   IF CA-SORT-CODE NUMERIC AND CA-ACCT-NO NUMERIC
                       PERFORM 4000-NEW-ORDER
                   ELSE
                       MOVE LOW-VALUES     TO SOM01O
                       MOVE 'KEY THE PAYER ACCOUNT FIRST'
                                           TO MSGO OF SOM01O
                       PERFORM 8000-SEND-LIST
                   END-IF
               WHEN DFHPF8
                   IF CA-MORE = 'Y'
                       PERFORM 3000-LIST-ORDERS
                   ELSE
                       MOVE ZERO           TO CA-RESTART-SEQ
                       PERFORM 3000-LIST-ORDERS
                   END-IF
               WHEN DFHENTER
                   PERFORM 2100-PROCESS-LIST-ENTER THRU 2100-EXIT
               WHEN OTHER
                   MOVE LOW-VALUES         TO SOM01O
                   MOVE 'INVALID KEY'
                                           TO MSGO OF SOM01O
                   PERFORM 8000-SEND-LIST
           END-EVALUATE
           .
      *================================================================*
       2100-PROCESS-LIST-ENTER.
      *================================================================*
           PERFORM 2300-RECEIVE-LIST-KEYS
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES             TO SOM01O
               MOVE 'NOTHING ENTERED'
                                           TO MSGO OF SOM01O
               PERFORM 8000-SEND-LIST
               GO TO 2100-EXIT
           END-IF

      *    A SELECTION ON THE CURRENT PAGE TAKES PRIORITY OVER A NEW
      *    ACCOUNT NUMBER.
           MOVE ZERO                       TO WS-SEL-COUNT WS-SEL-LINE
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-LINES-PER-PAGE
               PERFORM 2200-GET-SEL
               IF WS-SEL-CODE NOT = SPACE AND LOW-VALUE
                   ADD 1                   TO WS-SEL-COUNT
                   MOVE WS-LINE            TO WS-SEL-LINE
               END-IF
           END-PERFORM

           EVALUATE TRUE
               WHEN WS-SEL-COUNT > 1
                   PERFORM 3000-LIST-ORDERS-SAME-PAGE
                   MOVE 'SELECT ONE ORDER ONLY'
                                           TO MSGO OF SOM01O
                   PERFORM 8000-SEND-LIST
               WHEN WS-SEL-COUNT = 1
                   MOVE WS-SEL-LINE        TO WS-LINE
                   PERFORM 2200-GET-SEL
                   IF CA-LIST-SEQ (WS-SEL-LINE) = ZERO
                       PERFORM 3000-LIST-ORDERS-SAME-PAGE
                       MOVE 'NO ORDER ON THAT LINE'
                                           TO MSGO OF SOM01O
                       PERFORM 8000-SEND-LIST
                   ELSE
                       MOVE CA-LIST-SEQ (WS-SEL-LINE) TO CA-SO-SEQ
                       EVALUATE FUNCTION UPPER-CASE (WS-SEL-CODE)
                           WHEN 'I'
                               SET CA-MODE-ENQUIRE TO TRUE
                               PERFORM 4100-SHOW-ORDER THRU 4100-EXIT
                           WHEN 'A'
                               SET CA-MODE-AMEND TO TRUE
                               PERFORM 4100-SHOW-ORDER THRU 4100-EXIT
                           WHEN 'X'
                               SET CA-MODE-CANCEL TO TRUE
                               PERFORM 4100-SHOW-ORDER THRU 4100-EXIT
                           WHEN OTHER
                               PERFORM 3000-LIST-ORDERS-SAME-PAGE
                               MOVE 'SELECTION MUST BE I, A OR X'
                                           TO MSGO OF SOM01O
                               PERFORM 8000-SEND-LIST
                       END-EVALUATE
                   END-IF
               WHEN OTHER
                   MOVE ZERO               TO CA-RESTART-SEQ
                   PERFORM 3000-LIST-ORDERS
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-GET-SEL.
      *================================================================*
           EVALUATE WS-LINE
               WHEN 1   MOVE SEL1I  TO WS-SEL-CODE
               WHEN 2   MOVE SEL2I  TO WS-SEL-CODE
               WHEN 3   MOVE SEL3I  TO WS-SEL-CODE
               WHEN 4   MOVE SEL4I  TO WS-SEL-CODE
               WHEN 5   MOVE SEL5I  TO WS-SEL-CODE
               WHEN 6   MOVE SEL6I  TO WS-SEL-CODE
               WHEN 7   MOVE SEL7I  TO WS-SEL-CODE
               WHEN 8   MOVE SEL8I  TO WS-SEL-CODE
               WHEN 9   MOVE SEL9I  TO WS-SEL-CODE
               WHEN 10  MOVE SEL10I TO WS-SEL-CODE
               WHEN 11  MOVE SEL11I TO WS-SEL-CODE
               WHEN 12  MOVE SEL12I TO WS-SEL-CODE
           END-EVALUATE
           .
      *================================================================*
       2300-RECEIVE-LIST-KEYS.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('SOM01')
                     MAPSET('SOMAPS')
                     INTO(SOM01I)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               IF SORTCDL OF SOM01I > ZERO
                   MOVE SORTCDI OF SOM01I  TO CA-SORT-CODE
               END-IF
               IF ACCTNOL OF SOM01I > ZERO
                   MOVE ACCTNOI OF SOM01I  TO CA-ACCT-NO
               END-IF
           END-IF
           .
      *================================================================*
       3000-LIST-ORDERS.
      *================================================================*
           PERFORM 3100-CALL-LIST
           IF CH-RC-OK OR CH-RC-WARNING
               MOVE CHSO-MORE-IND          TO CA-MORE
               MOVE CHSO-RESTART-SEQ       TO CA-RESTART-SEQ
           END-IF
           PERFORM 3200-BUILD-LIST
           PERFORM 8000-SEND-LIST
           .
      *================================================================*
       3000-LIST-ORDERS-SAME-PAGE.
      *================================================================*
      *    REBUILD THE PAGE ON SCREEN WITHOUT MOVING ON (THE START
      *    POINT FOR THIS PAGE IS THE FIRST SEQ LISTED MINUS ONE).
           IF CA-LIST-SEQ (1) > ZERO
               COMPUTE CA-RESTART-SEQ = CA-LIST-SEQ (1) - 1
           ELSE
               MOVE ZERO                   TO CA-RESTART-SEQ
           END-IF
           PERFORM 3100-CALL-LIST
           IF CH-RC-OK OR CH-RC-WARNING
               MOVE CHSO-MORE-IND          TO CA-MORE
               MOVE CHSO-RESTART-SEQ       TO CA-RESTART-SEQ
           END-IF
           PERFORM 3200-BUILD-LIST
           .
      *================================================================*
       3100-CALL-LIST.
      *================================================================*
           PERFORM 7000-INIT-SERVICE
           SET CHSO-LIST                   TO TRUE
           MOVE CA-SORT-CODE               TO CHSO-SORT-CODE
           MOVE CA-ACCT-NO                 TO CHSO-ACCT-NO
           MOVE CA-RESTART-SEQ             TO CHSO-SO-SEQ
           PERFORM 7100-LINK-SERVICE
           .
      *================================================================*
       3200-BUILD-LIST.
      *================================================================*
           MOVE LOW-VALUES                 TO SOM01O
           MOVE CA-SORT-CODE               TO SORTCDO OF SOM01O
           MOVE CA-ACCT-NO                 TO ACCTNOO OF SOM01O
           SET CA-ON-LIST                  TO TRUE
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-LINES-PER-PAGE
               MOVE ZERO                   TO CA-LIST-SEQ (WS-LINE)
               MOVE SPACES                 TO WS-SO-LINE
               IF (CH-RC-OK OR CH-RC-WARNING)
               AND WS-LINE <= CHSO-SO-COUNT
                   PERFORM 3300-FORMAT-ENTRY
               END-IF
               PERFORM 3400-PLACE-LINE
           END-PERFORM
           IF CH-RC-OK
               IF CA-MORE = 'Y'
                   MOVE 'MORE ORDERS - PF8 FOR NEXT PAGE' TO MSGO
                                                      OF SOM01O
               ELSE
                   MOVE 'SELECT I, A OR X - PF5 FOR A NEW ORDER'
                                           TO MSGO OF SOM01O
               END-IF
           ELSE
               MOVE CH-MESSAGE-TEXT        TO MSGO OF SOM01O
           END-IF
           .
      *================================================================*
       3300-FORMAT-ENTRY.
      *================================================================*
      *    ONLY 12 OF THE 25 RETURNED ENTRIES FIT - THE RESTART SEQ IS
      *    RESET TO THE 12TH SO PF8 CONTINUES FROM THERE.
           MOVE CHSOE-SO-SEQ (WS-LINE)     TO CA-LIST-SEQ (WS-LINE)
                                              SL-SEQ
           EVALUATE CHSOE-STATUS (WS-LINE)
               WHEN 'A'  MOVE 'AC'         TO SL-STATUS
               WHEN 'S'  MOVE 'SU'         TO SL-STATUS
               WHEN 'C'  MOVE 'CN'         TO SL-STATUS
               WHEN 'X'  MOVE 'CP'         TO SL-STATUS
               WHEN OTHER MOVE '??'        TO SL-STATUS
           END-EVALUATE
           MOVE CHSOE-BENE-NAME (WS-LINE)  TO SL-BENE-NAME
           STRING CHSOE-BENE-SORT-CODE (WS-LINE) (1:2) '-'
                  CHSOE-BENE-SORT-CODE (WS-LINE) (3:2) '-'
                  CHSOE-BENE-SORT-CODE (WS-LINE) (5:2)
                  DELIMITED BY SIZE
                  INTO SL-BENE-SORT
           END-STRING
           MOVE CHSOE-BENE-ACCT-NO (WS-LINE) TO SL-BENE-ACCT
           MOVE CHSOE-FREQ-CD (WS-LINE)    TO SL-FREQ
           IF CHSOE-NEXT-DUE (WS-LINE) > ZERO
               STRING CHSOE-NEXT-DUE (WS-LINE) (7:2) '/'
                      CHSOE-NEXT-DUE (WS-LINE) (5:2) '/'
                      CHSOE-NEXT-DUE (WS-LINE) (1:4)
                      DELIMITED BY SIZE
                      INTO SL-NEXT-DUE
               END-STRING
           END-IF
           MOVE CHSOE-AMOUNT (WS-LINE)     TO SL-AMOUNT
           IF WS-LINE = WS-LINES-PER-PAGE
           AND CHSO-SO-COUNT > WS-LINES-PER-PAGE
               MOVE 'Y'                    TO CA-MORE
               MOVE CHSOE-SO-SEQ (WS-LINE) TO CA-RESTART-SEQ
           END-IF
           .
      *================================================================*
       3400-PLACE-LINE.
      *================================================================*
           EVALUATE WS-LINE
               WHEN 1   MOVE WS-SO-LINE TO SOL1O
               WHEN 2   MOVE WS-SO-LINE TO SOL2O
               WHEN 3   MOVE WS-SO-LINE TO SOL3O
               WHEN 4   MOVE WS-SO-LINE TO SOL4O
               WHEN 5   MOVE WS-SO-LINE TO SOL5O
               WHEN 6   MOVE WS-SO-LINE TO SOL6O
               WHEN 7   MOVE WS-SO-LINE TO SOL7O
               WHEN 8   MOVE WS-SO-LINE TO SOL8O
               WHEN 9   MOVE WS-SO-LINE TO SOL9O
               WHEN 10  MOVE WS-SO-LINE TO SOL10O
               WHEN 11  MOVE WS-SO-LINE TO SOL11O
               WHEN 12  MOVE WS-SO-LINE TO SOL12O
           END-EVALUATE
           .
      *================================================================*
       4000-NEW-ORDER.
      *================================================================*
           SET CA-MODE-CREATE              TO TRUE
           SET CA-ON-DETAIL                TO TRUE
           MOVE ZERO                       TO CA-SO-SEQ
           MOVE LOW-VALUES                 TO SOM02O
           PERFORM 4900-DETAIL-HEADER
           MOVE 'NEW'                      TO SOSTATO
           MOVE 'M1'                       TO FREQO
           MOVE '9999'                     TO NUMPAYO
           MOVE 'CREATE - KEY DETAILS, ENTER TO SAVE' TO MODEO
           MOVE -1                         TO BNAMEL
           PERFORM 8100-SEND-DETAIL
           .
      *================================================================*
       4100-SHOW-ORDER.
      *================================================================*
           PERFORM 7000-INIT-SERVICE
           SET CHSO-ENQUIRE                TO TRUE
           MOVE CA-SORT-CODE               TO CHSO-SORT-CODE
           MOVE CA-ACCT-NO                 TO CHSO-ACCT-NO
           MOVE CA-SO-SEQ                  TO CHSO-SO-SEQ
           PERFORM 7100-LINK-SERVICE

           IF NOT CH-RC-OK
               PERFORM 3000-LIST-ORDERS-SAME-PAGE
               MOVE CH-MESSAGE-TEXT        TO MSGO OF SOM01O
               PERFORM 8000-SEND-LIST
               GO TO 4100-EXIT
           END-IF

           SET CA-ON-DETAIL                TO TRUE
           MOVE LOW-VALUES                 TO SOM02O
           PERFORM 4900-DETAIL-HEADER
           MOVE CA-SO-SEQ                  TO SOSEQO
           EVALUATE CHSOE-STATUS (1)
               WHEN 'A'  MOVE 'ACTIVE'     TO SOSTATO
               WHEN 'S'  MOVE 'SUSPENDED'  TO SOSTATO
                         MOVE DFHRED       TO SOSTATA
               WHEN 'C'  MOVE 'CANCELLED'  TO SOSTATO
               WHEN 'X'  MOVE 'COMPLETED'  TO SOSTATO
           END-EVALUATE
           MOVE CHSO-BENE-NAME             TO BNAMEO
           MOVE CHSO-BENE-SORT-CODE        TO BSORTO
           MOVE CHSO-BENE-ACCT-NO          TO BACCTO
           MOVE CHSO-BENE-REF              TO BREFO
           MOVE CHSO-FREQ-CD               TO FREQO
           MOVE CHSOE-NEXT-DUE (1)         TO FDATEO
           MOVE CHSO-FIRST-AMT             TO WS-AMOUNT-EDIT
           MOVE WS-AMOUNT-EDIT             TO FAMTO
           MOVE CHSO-REGULAR-AMT           TO WS-AMOUNT-EDIT
           MOVE WS-AMOUNT-EDIT             TO RAMTO
           MOVE CHSO-FINAL-AMT             TO WS-AMOUNT-EDIT
           MOVE WS-AMOUNT-EDIT             TO LAMTO
           MOVE CHSO-FINAL-PAY-DATE        TO LDATEO
           MOVE CHSO-NUM-PAYMENTS          TO NUMPAYO
           IF CHSOE-LAST-PAID (1) > ZERO
               MOVE CHSOE-LAST-PAID (1)    TO LPAIDO
           ELSE
               MOVE 'NEVER'                TO LPAIDO
           END-IF

      *    BENEFICIARY ACCOUNT FIELDS ARE NEVER AMENDABLE (FR-2004-07)
           MOVE DFHBMASK                   TO BSORTA BACCTA
           EVALUATE TRUE
               WHEN CA-MODE-ENQUIRE
                   PERFORM 4800-PROTECT-ALL
                   MOVE 'ENQUIRY ONLY - PF12 FOR THE LIST' TO MODEO
               WHEN CA-MODE-AMEND
                   MOVE DFHBMASK           TO FREQA FAMTA LAMTA
                                              LDATEA NUMPAYA
                   MOVE 'AMEND NAME, REFERENCE, NEXT DATE, AMOUNT'
                                           TO MODEO
                   MOVE -1                 TO RAMTL
               WHEN CA-MODE-CANCEL
                   PERFORM 4800-PROTECT-ALL
                   MOVE 'CANCEL - PRESS ENTER TO CONFIRM' TO MODEO
                   MOVE DFHRED             TO MODEA
           END-EVALUATE
           PERFORM 8100-SEND-DETAIL
           .
       4100-EXIT.
           EXIT.
      *================================================================*
       4800-PROTECT-ALL.
      *================================================================*
           MOVE DFHBMASK                   TO BNAMEA BSORTA BACCTA
                                              BREFA FREQA FDATEA
                                              FAMTA RAMTA LAMTA
                                              LDATEA NUMPAYA
           .
      *================================================================*
       4900-DETAIL-HEADER.
      *================================================================*
           STRING CA-SORT-CODE (1:2) '-' CA-SORT-CODE (3:2) '-'
                  CA-SORT-CODE (5:2) ' ' CA-ACCT-NO
                  DELIMITED BY SIZE
                  INTO PAYERO
           END-STRING
           .
      *================================================================*
       5000-DETAIL-SCREEN-INPUT.
      *================================================================*
           EVALUATE EIBAID
               WHEN DFHPF3
                   PERFORM 9000-END-SESSION
               WHEN DFHPF12
               WHEN DFHCLEAR
                   PERFORM 3000-LIST-ORDERS-SAME-PAGE
                   PERFORM 8000-SEND-LIST
               WHEN DFHENTER
                   EVALUATE TRUE
                       WHEN CA-MODE-CREATE
                       WHEN CA-MODE-AMEND
                           PERFORM 5100-SAVE-ORDER THRU 5100-EXIT
                       WHEN CA-MODE-CANCEL
                           PERFORM 5500-CANCEL-ORDER
                       WHEN OTHER
                           PERFORM 3000-LIST-ORDERS-SAME-PAGE
                           PERFORM 8000-SEND-LIST
                   END-EVALUATE
               WHEN OTHER
                   MOVE LOW-VALUES         TO SOM02O
                   MOVE 'INVALID KEY - ENTER, PF3 OR PF12' TO MSGO
                                                      OF SOM02O
                   PERFORM 8200-SEND-DETAIL-DATAONLY
           END-EVALUATE
           .
      *================================================================*
       5100-SAVE-ORDER.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('SOM02')
                     MAPSET('SOMAPS')
                     INTO(SOM02I)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES             TO SOM02O
               MOVE 'NOTHING CHANGED'      TO MSGO OF SOM02O
               PERFORM 8200-SEND-DETAIL-DATAONLY
               GO TO 5100-EXIT
           END-IF

           PERFORM 7000-INIT-SERVICE
           IF CA-MODE-CREATE
               SET CHSO-CREATE             TO TRUE
           ELSE
               SET CHSO-AMEND              TO TRUE
           END-IF
           MOVE CA-SORT-CODE               TO CHSO-SORT-CODE
           MOVE CA-ACCT-NO                 TO CHSO-ACCT-NO
           MOVE CA-SO-SEQ                  TO CHSO-SO-SEQ
           MOVE BNAMEI                     TO CHSO-BENE-NAME
           MOVE BREFI                      TO CHSO-BENE-REF
           IF CA-MODE-CREATE
               MOVE BSORTI                 TO CHSO-BENE-SORT-CODE
               MOVE BACCTI                 TO CHSO-BENE-ACCT-NO
               MOVE FUNCTION UPPER-CASE (FREQI) TO CHSO-FREQ-CD
           ELSE
               MOVE SPACES                 TO CHSO-BENE-SORT-CODE
                                              CHSO-BENE-ACCT-NO
           END-IF

           IF FDATEI NUMERIC
               MOVE FDATEI                 TO CHSO-FIRST-PAY-DATE
           ELSE
               MOVE ZERO                   TO CHSO-FIRST-PAY-DATE
           END-IF

           MOVE RAMTI                      TO WS-AMOUNT-IN
           PERFORM 6000-PARSE-AMOUNT THRU 6000-EXIT
           IF NOT WS-AMOUNT-OK
               MOVE LOW-VALUES             TO SOM02O
               MOVE 'REGULAR AMOUNT IS NOT A VALID AMOUNT'
                                           TO MSGO OF SOM02O
               MOVE -1                     TO RAMTL
               PERFORM 8200-SEND-DETAIL-DATAONLY
               GO TO 5100-EXIT
           END-IF
           MOVE WS-AMOUNT-NUM              TO CHSO-REGULAR-AMT

           IF CA-MODE-CREATE
               MOVE FAMTI                  TO WS-AMOUNT-IN
               PERFORM 6000-PARSE-AMOUNT THRU 6000-EXIT
               MOVE WS-AMOUNT-NUM          TO CHSO-FIRST-AMT
               MOVE LAMTI                  TO WS-AMOUNT-IN
               PERFORM 6000-PARSE-AMOUNT THRU 6000-EXIT
               MOVE WS-AMOUNT-NUM          TO CHSO-FINAL-AMT
               IF LDATEI NUMERIC
                   MOVE LDATEI             TO CHSO-FINAL-PAY-DATE
               ELSE
                   MOVE ZERO               TO CHSO-FINAL-PAY-DATE
               END-IF
               IF NUMPAYI NUMERIC
                   MOVE NUMPAYI            TO CHSO-NUM-PAYMENTS
               ELSE
                   MOVE 9999               TO CHSO-NUM-PAYMENTS
               END-IF
           END-IF

           PERFORM 7100-LINK-SERVICE

           IF CH-RC-OK OR CH-RC-WARNING
               PERFORM 3000-LIST-ORDERS-SAME-PAGE
               MOVE CH-MESSAGE-TEXT        TO MSGO OF SOM01O
               PERFORM 8000-SEND-LIST
           ELSE
               MOVE LOW-VALUES             TO SOM02O
               MOVE CH-MESSAGE-TEXT        TO MSGO OF SOM02O
               PERFORM 8200-SEND-DETAIL-DATAONLY
           END-IF
           .
       5100-EXIT.
           EXIT.
      *================================================================*
       5500-CANCEL-ORDER.
      *================================================================*
           PERFORM 7000-INIT-SERVICE
           SET CHSO-CANCEL                 TO TRUE
           MOVE CA-SORT-CODE               TO CHSO-SORT-CODE
           MOVE CA-ACCT-NO                 TO CHSO-ACCT-NO
           MOVE CA-SO-SEQ                  TO CHSO-SO-SEQ
           MOVE 'BRCH'                     TO CHSO-CANCEL-RSN
           PERFORM 7100-LINK-SERVICE
           PERFORM 3000-LIST-ORDERS-SAME-PAGE
           MOVE CH-MESSAGE-TEXT            TO MSGO OF SOM01O
           PERFORM 8000-SEND-LIST
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
                   UNTIL WS-CHAR-SUB > 12
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
           MOVE 'SOMAINT '                 TO CH-SERVICE-CD
           MOVE 'BRANCH'                   TO CH-CHANNEL-CD
           MOVE EIBTRMID                   TO CH-USER-ID
           STRING 'SO01' EIBTRMID EIBTASKN
                  DELIMITED BY SIZE
                  INTO CH-REQUEST-ID
           END-STRING
           MOVE ZERO                       TO CHSO-FIRST-AMT
                                              CHSO-REGULAR-AMT
                                              CHSO-FINAL-AMT
                                              CHSO-FIRST-PAY-DATE
                                              CHSO-FINAL-PAY-DATE
                                              CHSO-NUM-PAYMENTS
                                              CHSO-SO-SEQ
           .
      *================================================================*
       7100-LINK-SERVICE.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM('SOMNT01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE WS-RESP                TO WS-RESP-DISPLAY
               MOVE SPACES                 TO CH-MESSAGE-TEXT
               STRING 'STANDING ORDER SERVICE UNAVAILABLE - EIBRESP '
                      WS-RESP-DISPLAY
                      DELIMITED BY SIZE
                      INTO CH-MESSAGE-TEXT
               END-STRING
           END-IF
           .
      *================================================================*
       8000-SEND-LIST.
      *================================================================*
           SET CA-ON-LIST                  TO TRUE
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     DDMMYYYY(DATEO OF SOM01O)
                     DATESEP('/')
                     TIME(TIMEO OF SOM01O)
                     TIMESEP(':')
           END-EXEC
           EXEC CICS SEND
                     MAP('SOM01')
                     MAPSET('SOMAPS')
                     FROM(SOM01O)
                     ERASE
                     FREEKB
           END-EXEC
           .
      *================================================================*
       8100-SEND-DETAIL.
      *================================================================*
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     DDMMYYYY(DATEO OF SOM02O)
                     DATESEP('/')
                     TIME(TIMEO OF SOM02O)
                     TIMESEP(':')
           END-EXEC
           EXEC CICS SEND
                     MAP('SOM02')
                     MAPSET('SOMAPS')
                     FROM(SOM02O)
                     ERASE
                     CURSOR
                     FREEKB
           END-EXEC
           .
      *================================================================*
       8200-SEND-DETAIL-DATAONLY.
      *================================================================*
           EXEC CICS SEND
                     MAP('SOM02')
                     MAPSET('SOMAPS')
                     FROM(SOM02O)
                     DATAONLY
                     CURSOR
                     FREEKB
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
      *================================================================*
       9100-RETURN-TO-CF02.
      *================================================================*
           IF CA-CIF-NO NOT = SPACES
               EXEC CICS XCTL
                         PROGRAM('CFSCRN2')
                         COMMAREA(CA-CIF-NO)
                         LENGTH(12)
                         RESP(WS-RESP)
               END-EXEC
           ELSE
               EXEC CICS XCTL
                         PROGRAM('CFSCRN2')
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           MOVE LOW-VALUES                 TO SOM01O
           MOVE 'CF02 NOT AVAILABLE'       TO MSGO OF SOM01O
           PERFORM 8000-SEND-LIST
           .
