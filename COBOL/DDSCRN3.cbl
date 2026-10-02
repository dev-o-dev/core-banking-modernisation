       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDSCRN3.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-04-12.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDSCRN3                                             *
      * TITLE    : BRANCH TRANSACTION HISTORY ENQUIRY                  *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION DD03       *
      * MAPSET   : DDMAP3  MAP DDM03                                   *
      * LINKS    : DDTXN01 (SERVICE TXNHIST - HISTORY FROM DB2)        *
      * FILES    : ACCTMST (READ - HEADER DETAILS)                     *
      *----------------------------------------------------------------*
      * SHOWS 14 TRANSACTIONS PER PAGE, NEWEST FIRST. THE SCREEN DOES  *
      * NOT READ DB2 ITSELF - IT BUILDS A CHCOMMAR REQUEST AND LINKS   *
      * TO DDTXN01, THE SAME SERVICE THE CHANNELS USE (THE FIRST       *
      * BRANCH SCREEN TO REUSE A CHANNEL SERVICE - DESIGN NOTE         *
      * CS-DN-2010-03).                                                *
      *                                                                *
      * PAGING: PF8 ASKS DDTXN01 FOR THE NEXT PAGE USING THE RESTART   *
      * KEY IT RETURNED. PF7 GOES BACK TO THE FIRST PAGE (KEYSET       *
      * PAGING CANNOT GO BACK ONE PAGE WITHOUT A STACK - THE LAST 10   *
      * RESTART KEYS ARE HELD SO PF7 GOES BACK ONE PAGE AT A TIME UP   *
      * TO 10 PAGES).                                                  *
      *                                                                *
      * ENTERED FROM CF02 (OPTION H) WITH THE ACCOUNT IN THE COMMAREA, *
      * OR DIRECTLY BY TRANSACTION DD03.                               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-04-12 SAT  ORIGINAL VERSION                               *
      * 2010-05-04 SAT  PF7 BACK ONE PAGE (RESTART KEY STACK)          *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDSCRN3 '.
      *
       01  WS-CONSTANTS.
           05  WS-ROWS-PER-PAGE            PIC 9(02) VALUE 14.
           05  WS-STACK-MAX                PIC 9(02) VALUE 10.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-LINE                     PIC S9(04) COMP.
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'DD03 - TRANSACTION HISTORY ENDED'.
           05  WS-LEDGER-EDIT              PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-VALID-IND                PIC X(01).
               88  WS-INPUT-VALID              VALUE 'Y'.
      *
      *---- PSEUDO-CONVERSATIONAL COMMAREA ---------------------------*
       01  WS-COMMAREA.
           05  CA-EYECATCHER               PIC X(04).
               88  CA-OURS                     VALUE 'DD03'.
           05  CA-FROM-TRAN                PIC X(04).
           05  CA-SORT-CODE                PIC X(06).
           05  CA-ACCT-NO                  PIC X(08).
           05  CA-CIF-NO                   PIC X(12).
           05  CA-FROM-DATE                PIC 9(08).
           05  CA-TO-DATE                  PIC 9(08).
           05  CA-FILTER                   PIC X(01).
           05  CA-PAGE                     PIC 9(03).
           05  CA-MORE                     PIC X(01).
           05  CA-NEXT-DATE                PIC 9(08).
           05  CA-NEXT-SEQ                 PIC 9(09).
           05  CA-STACK-COUNT              PIC 9(02).
           05  CA-STACK                    OCCURS 10 TIMES.
               10  CA-STK-DATE             PIC 9(08).
               10  CA-STK-SEQ              PIC 9(09).
      *
      *---- COMMAREA RECEIVED FROM CF02 (OPTION H) --------------------*
       01  WS-CF02-COMMAREA.
           05  XC-FROM-TRAN                PIC X(04).
           05  XC-SORT-CODE                PIC X(06).
           05  XC-ACCT-NO                  PIC X(08).
           05  XC-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(20).
      *
       01  WS-TXN-LINE.
           05  TL-DATE                     PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  TL-TXN-CODE                 PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  TL-NARRATIVE                PIC X(23).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  TL-OUT                      PIC ZZ,ZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  TL-IN                       PIC ZZ,ZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  TL-BALANCE                  PIC -ZZZ,ZZ9.99.
      *
       01  WS-SVC-COMMAREA                 PIC X(4000).
      *
           COPY DDMAP3.
      *
           COPY DDACCREC.
      *
           COPY DFHAID.
      *
           COPY DFHBMSCA.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(244).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC

           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                   PERFORM 1000-SEND-EMPTY-MAP
               WHEN EIBCALEN = LENGTH OF WS-CF02-COMMAREA
                   MOVE DFHCOMMAREA (1:50) TO WS-CF02-COMMAREA
                   PERFORM 1100-START-FROM-CF02
               WHEN OTHER
                   MOVE DFHCOMMAREA        TO WS-COMMAREA
                   EVALUATE EIBAID
                       WHEN DFHPF3
                           PERFORM 9000-END-SESSION
                       WHEN DFHPF12
                           PERFORM 9100-RETURN-TO-CF02
                       WHEN DFHCLEAR
                           PERFORM 1000-SEND-EMPTY-MAP
                       WHEN DFHPF7
                           PERFORM 4000-PAGE-BACK
                       WHEN DFHPF8
                           PERFORM 4100-PAGE-FORWARD
                       WHEN DFHENTER
                           PERFORM 2000-PROCESS-ENTER THRU 2000-EXIT
                       WHEN OTHER
                           MOVE LOW-VALUES TO DDM03O
                           MOVE 'INVALID KEY - USE ENTER, PF3, PF7, PF8'
                                           TO MSGO
                           PERFORM 8000-SEND-DATAONLY
                   END-EVALUATE
           END-EVALUATE

           EXEC CICS RETURN
                     TRANSID('DD03')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       1000-SEND-EMPTY-MAP.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           MOVE ZERO                       TO CA-PAGE CA-STACK-COUNT
                                              CA-FROM-DATE CA-TO-DATE
                                              CA-NEXT-DATE CA-NEXT-SEQ
           MOVE LOW-VALUES                 TO DDM03O
           MOVE 'KEY SORT CODE AND ACCOUNT, OPTIONAL DATES CCYYMMDD'
                                           TO MSGO
           PERFORM 8100-SEND-MAP
           .
      *================================================================*
       1100-START-FROM-CF02.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           MOVE XC-FROM-TRAN               TO CA-FROM-TRAN
           MOVE XC-SORT-CODE               TO CA-SORT-CODE
           MOVE XC-ACCT-NO                 TO CA-ACCT-NO
           MOVE XC-CIF-NO                  TO CA-CIF-NO
           MOVE ZERO                       TO CA-FROM-DATE CA-TO-DATE
                                              CA-STACK-COUNT
           MOVE SPACE                      TO CA-FILTER
           PERFORM 3000-FIRST-PAGE
           .
      *================================================================*
       2000-PROCESS-ENTER.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('DDM03')
                     MAPSET('DDMAP3')
                     INTO(DDM03I)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES             TO DDM03O
               MOVE 'SORT CODE AND ACCOUNT NUMBER ARE REQUIRED'
                                           TO MSGO
               MOVE -1                     TO SORTCDL
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

           PERFORM 2100-VALIDATE-INPUT THRU 2100-EXIT
           IF WS-INPUT-VALID
               PERFORM 3000-FIRST-PAGE
           ELSE
               PERFORM 8000-SEND-DATAONLY
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-VALIDATE-INPUT.
      *================================================================*
      *    A FIELD NOT RE-KEYED KEEPS ITS COMMAREA VALUE (FSET).
           MOVE 'Y'                        TO WS-VALID-IND
           IF SORTCDL > ZERO
               MOVE SORTCDI                TO CA-SORT-CODE
           END-IF
           IF ACCTNOL > ZERO
               MOVE ACCTNOI                TO CA-ACCT-NO
           END-IF
           MOVE LOW-VALUES                 TO DDM03O

           EVALUATE TRUE
               WHEN CA-SORT-CODE NOT NUMERIC
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'SORT CODE MUST BE 6 DIGITS' TO MSGO
                   MOVE -1                 TO SORTCDL
                   MOVE DFHRED             TO SORTCDA
               WHEN CA-ACCT-NO NOT NUMERIC
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'ACCOUNT NUMBER MUST BE 8 DIGITS' TO MSGO
                   MOVE -1                 TO ACCTNOL
                   MOVE DFHRED             TO ACCTNOA
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT WS-INPUT-VALID
               GO TO 2100-EXIT
           END-IF

           MOVE ZERO                       TO CA-FROM-DATE CA-TO-DATE
           IF FROMDTL > ZERO AND FROMDTI NOT = SPACES
               IF FROMDTI NUMERIC
                   MOVE FROMDTI            TO CA-FROM-DATE
               ELSE
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'FROM DATE MUST BE CCYYMMDD' TO MSGO
                   MOVE -1                 TO FROMDTL
               END-IF
           END-IF
           IF TODTL > ZERO AND TODTI NOT = SPACES
               IF TODTI NUMERIC
                   MOVE TODTI              TO CA-TO-DATE
               ELSE
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'TO DATE MUST BE CCYYMMDD' TO MSGO
                   MOVE -1                 TO TODTL
               END-IF
           END-IF
           IF FILTERL > ZERO
               MOVE FUNCTION UPPER-CASE (FILTERI) TO CA-FILTER
               IF CA-FILTER NOT = SPACE AND 'D' AND 'C'
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'FILTER MUST BE D, C OR BLANK' TO MSGO
                   MOVE -1                 TO FILTERL
               END-IF
           ELSE
               MOVE SPACE                  TO CA-FILTER
           END-IF
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       3000-FIRST-PAGE.
      *================================================================*
           MOVE ZERO                       TO CA-PAGE CA-STACK-COUNT
                                              CA-NEXT-DATE CA-NEXT-SEQ
           PERFORM 3500-FETCH-PAGE THRU 3500-EXIT
           .
      *================================================================*
       3500-FETCH-PAGE.
      *================================================================*
      *    ON ENTRY CA-NEXT-DATE / CA-NEXT-SEQ HOLD THE RESTART KEY
      *    (ZERO = FIRST PAGE).
           MOVE LOW-VALUES                 TO CH-COMMAREA
           MOVE SPACES                     TO CH-COMMAREA
           SET CH-EYECATCHER-OK            TO TRUE
           MOVE '01'                       TO CH-VERSION
           MOVE 'TXNHIST '                 TO CH-SERVICE-CD
           MOVE 'BRANCH'                   TO CH-CHANNEL-CD
           MOVE EIBTRMID                   TO CH-USER-ID
           STRING 'DD03' EIBTRMID EIBTASKN
                  DELIMITED BY SIZE
                  INTO CH-REQUEST-ID
           END-STRING
           MOVE CA-SORT-CODE               TO CHTH-SORT-CODE
           MOVE CA-ACCT-NO                 TO CHTH-ACCT-NO
           MOVE CA-FROM-DATE               TO CHTH-FROM-DATE
           MOVE CA-TO-DATE                 TO CHTH-TO-DATE
           MOVE CA-NEXT-DATE               TO CHTH-RESTART-DATE
           MOVE CA-NEXT-SEQ                TO CHTH-RESTART-SEQ
           MOVE WS-ROWS-PER-PAGE           TO CHTH-MAX-ROWS
           MOVE CA-FILTER                  TO CHTH-DR-CR-FILTER

           EXEC CICS LINK
                     PROGRAM('DDTXN01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           MOVE LOW-VALUES                 TO DDM03O
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE WS-RESP                TO WS-RESP-DISPLAY
               STRING 'HISTORY SERVICE UNAVAILABLE - EIBRESP '
                      WS-RESP-DISPLAY
                      DELIMITED BY SIZE
                      INTO MSGO
               END-STRING
               PERFORM 8100-SEND-MAP
               GO TO 3500-EXIT
           END-IF

           IF NOT (CH-RC-OK OR CH-RC-WARNING)
               MOVE CH-MESSAGE-TEXT        TO MSGO
               PERFORM 3600-FORMAT-HEADER
               PERFORM 8100-SEND-MAP
               GO TO 3500-EXIT
           END-IF

      *    PUSH THE KEY WE USED SO PF7 CAN COME BACK TO THIS PAGE.
           IF CA-STACK-COUNT < WS-STACK-MAX
               ADD 1                       TO CA-STACK-COUNT
               MOVE CA-NEXT-DATE TO CA-STK-DATE (CA-STACK-COUNT)
               MOVE CA-NEXT-SEQ  TO CA-STK-SEQ (CA-STACK-COUNT)
           END-IF
           ADD 1                           TO CA-PAGE

           MOVE CHTH-MORE-IND              TO CA-MORE
           MOVE CHTH-NEXT-DATE             TO CA-NEXT-DATE
           MOVE CHTH-NEXT-SEQ              TO CA-NEXT-SEQ

           PERFORM 3600-FORMAT-HEADER
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-ROWS-PER-PAGE
               MOVE SPACES                 TO WS-TXN-LINE
               IF WS-LINE <= CHTH-COUNT
                   PERFORM 3700-FORMAT-TXN
               END-IF
               PERFORM 3800-PLACE-LINE
           END-PERFORM

           MOVE CA-PAGE                    TO PAGENOO
           IF CA-MORE = 'Y'
               MOVE 'MORE - PRESS PF8'     TO MOREINDO
           ELSE
               MOVE 'END OF HISTORY'       TO MOREINDO
           END-IF
           IF CH-RC-WARNING
               MOVE CH-MESSAGE-TEXT        TO MSGO
           END-IF
           PERFORM 8100-SEND-MAP
           .
       3500-EXIT.
           EXIT.
      *================================================================*
       3600-FORMAT-HEADER.
      *================================================================*
           MOVE CA-SORT-CODE               TO SORTCDO
           MOVE CA-ACCT-NO                 TO ACCTNOO
           IF CA-FROM-DATE > ZERO
               MOVE CA-FROM-DATE           TO FROMDTO
           END-IF
           IF CA-TO-DATE > ZERO
               MOVE CA-TO-DATE             TO TODTO
           END-IF
           MOVE CA-FILTER                  TO FILTERO

           MOVE CA-SORT-CODE               TO ACM-SORT-CODE
           MOVE CA-ACCT-NO                 TO ACM-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(ACM-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE ACM-PROD-CD            TO PRODCDO
               MOVE ACM-LEDGER-BAL         TO WS-LEDGER-EDIT
               MOVE WS-LEDGER-EDIT         TO LEDGERO
           ELSE
               MOVE 'N/A'                  TO PRODCDO
               MOVE 'BATCH WINDOW'         TO LEDGERO
           END-IF
           .
      *================================================================*
       3700-FORMAT-TXN.
      *================================================================*
           STRING CHTHE-POST-DATE (WS-LINE) (7:2) '/'
                  CHTHE-POST-DATE (WS-LINE) (5:2) '/'
                  CHTHE-POST-DATE (WS-LINE) (1:4)
                  DELIMITED BY SIZE
                  INTO TL-DATE
           END-STRING
           MOVE CHTHE-TXN-CODE (WS-LINE)   TO TL-TXN-CODE
           MOVE CHTHE-NARRATIVE-1 (WS-LINE) TO TL-NARRATIVE
           IF CHTHE-DR-CR-IND (WS-LINE) = 'D'
               MOVE CHTHE-AMOUNT (WS-LINE) TO TL-OUT
               MOVE ZERO                   TO TL-IN
           ELSE
               MOVE ZERO                   TO TL-OUT
               MOVE CHTHE-AMOUNT (WS-LINE) TO TL-IN
           END-IF
      *    THE BALANCE COLUMN IS ONLY 11 WIDE - BALANCES OVER 999,999
      *    SHOW AS ASTERISKS (BRANCH STAFF USE CF02 FOR THE BALANCE).
           IF CHTHE-RUNNING-BAL (WS-LINE) > 999999.99
           OR CHTHE-RUNNING-BAL (WS-LINE) < -99999.99
               MOVE ZERO                   TO TL-BALANCE
               MOVE '**********'           TO WS-TXN-LINE (69:11)
           ELSE
               MOVE CHTHE-RUNNING-BAL (WS-LINE) TO TL-BALANCE
           END-IF
           .
      *================================================================*
       3800-PLACE-LINE.
      *================================================================*
           EVALUATE WS-LINE
               WHEN 1   MOVE WS-TXN-LINE TO TXL1O
               WHEN 2   MOVE WS-TXN-LINE TO TXL2O
               WHEN 3   MOVE WS-TXN-LINE TO TXL3O
               WHEN 4   MOVE WS-TXN-LINE TO TXL4O
               WHEN 5   MOVE WS-TXN-LINE TO TXL5O
               WHEN 6   MOVE WS-TXN-LINE TO TXL6O
               WHEN 7   MOVE WS-TXN-LINE TO TXL7O
               WHEN 8   MOVE WS-TXN-LINE TO TXL8O
               WHEN 9   MOVE WS-TXN-LINE TO TXL9O
               WHEN 10  MOVE WS-TXN-LINE TO TXL10O
               WHEN 11  MOVE WS-TXN-LINE TO TXL11O
               WHEN 12  MOVE WS-TXN-LINE TO TXL12O
               WHEN 13  MOVE WS-TXN-LINE TO TXL13O
               WHEN 14  MOVE WS-TXN-LINE TO TXL14O
           END-EVALUATE
           .
      *================================================================*
       4000-PAGE-BACK.
      *================================================================*
      *    THE TOP OF THE STACK IS THE KEY FOR THE PAGE ON SCREEN; THE
      *    ONE BELOW IT IS THE PREVIOUS PAGE.
           IF CA-STACK-COUNT <= 1
               PERFORM 3000-FIRST-PAGE
           ELSE
               SUBTRACT 1                  FROM CA-STACK-COUNT
               MOVE CA-STK-DATE (CA-STACK-COUNT) TO CA-NEXT-DATE
               MOVE CA-STK-SEQ (CA-STACK-COUNT)  TO CA-NEXT-SEQ
               SUBTRACT 1                  FROM CA-STACK-COUNT
               SUBTRACT 2                  FROM CA-PAGE
               PERFORM 3500-FETCH-PAGE THRU 3500-EXIT
           END-IF
           .
      *================================================================*
       4100-PAGE-FORWARD.
      *================================================================*
           IF CA-MORE NOT = 'Y'
               MOVE LOW-VALUES             TO DDM03O
               MOVE 'NO MORE TRANSACTIONS' TO MSGO
               PERFORM 8000-SEND-DATAONLY
           ELSE
               PERFORM 3500-FETCH-PAGE THRU 3500-EXIT
           END-IF
           .
      *================================================================*
       8000-SEND-DATAONLY.
      *================================================================*
           PERFORM 8200-SET-DATE-TIME
           EXEC CICS SEND
                     MAP('DDM03')
                     MAPSET('DDMAP3')
                     FROM(DDM03O)
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
                     MAP('DDM03')
                     MAPSET('DDMAP3')
                     FROM(DDM03O)
                     ERASE
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
      *================================================================*
       9100-RETURN-TO-CF02.
      *================================================================*
      *    CF02 ACCEPTS A 12-BYTE COMMAREA HOLDING THE CIF NUMBER.
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
           MOVE LOW-VALUES                 TO DDM03O
           MOVE 'CF02 NOT AVAILABLE'       TO MSGO
           PERFORM 8000-SEND-DATAONLY
           .
