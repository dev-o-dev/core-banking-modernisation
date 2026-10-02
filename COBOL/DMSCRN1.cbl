       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DMSCRN1.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-04-26.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DMSCRN1                                             *
      * TITLE    : BRANCH DIRECT DEBIT MANDATE LIST / CANCEL           *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION DM01       *
      * MAPSET   : DMMAPS  MAP DMM01                                   *
      * LINKS    : DMSVC01 (SERVICE DDMANDT - FUNCTIONS L / X)         *
      *            CFSCRN2 (XCTL ON PF12 - CUSTOMER ACCOUNT LIST)      *
      * FILES    : ACCTMST (READ - PRODUCT, CIF NUMBER)                *
      *            CIFMAST (READ - CUSTOMER NAME FOR THE HEADER)       *
      *----------------------------------------------------------------*
      * LISTS THE DIRECT DEBIT MANDATES ON A PAYER ACCOUNT, 12 PER     *
      * PAGE. LIVE AND DORMANT MANDATES ARE ALWAYS SHOWN; CANCELLED    *
      * AND CLOSED ONES ONLY WITH INCLUDE CANCELLED = Y.               *
      *                                                                *
      * CANCELLATION IS TWO-STEP (BRANCH COMPLAINTS 2009 - MANDATES    *
      * CANCELLED BY A STRAY 'X'):                                     *
      *   1. KEY X AGAINST ONE MANDATE AND PRESS ENTER. THE MANDATE    *
      *      IS HELD AS 'PENDING CANCEL' IN THE COMMAREA AND SHOWN     *
      *      HIGHLIGHTED WITH A CONFIRMATION MESSAGE.                  *
      *   2. PRESS PF5 TO CONFIRM. ANY OTHER KEY ABANDONS IT.          *
      * DMSVC01 THEN MARKS THE MANDATE CANCELLED BY PAYER; DMADC010    *
      * SENDS ADDACS REASON 1 TO THE ORIGINATOR TONIGHT. COLLECTIONS   *
      * ALREADY IN TOMORROW'S BACS CYCLE ARE RETURNED BY DMCOL010 -    *
      * STAFF MUST TELL THE CUSTOMER (SEE BRANCH PROCEDURE BP-DD-04).  *
      *                                                                *
      * PAGING: DMSVC01 RETURNS UP TO 20 PER CALL, 12 ARE SHOWN. THE   *
      * RESTART KEY (SUN + PAYER REFERENCE) OF THE LAST LINE SHOWN IS  *
      * PASSED BACK FOR THE NEXT PAGE. PF8 AT THE END GOES BACK TO THE *
      * FIRST PAGE.                                                    *
      *                                                                *
      * ENTERED FROM CF02 WITH THE ACCOUNT IN THE COMMAREA, OR         *
      * DIRECTLY BY TRANSACTION DM01.                                  *
      * ACCESS: RACF PROFILE CICSPRD1.DM01 (GROUP BRSTAFF).            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-04-26 AOS  ORIGINAL VERSION - REPLACES TELLER DD ENQUIRY  *
      * 2010-07-12 AOS  TWO-STEP CANCEL (PF5 CONFIRM)                  *
      * 2010-11-29 SAT  CUSTOMER NAME IN THE HEADER (CIFMAST)          *
      * 2011-03-07 JNB  12 SHOWN FROM A 20-ROW SERVICE PAGE - RESTART  *
      *                 FROM THE LAST LINE SHOWN (PIR 2011/019 - ROWS  *
      *                 13-20 WERE NEVER DISPLAYED)                    *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DMSCRN1 '.
      *
       01  WS-CONSTANTS.
           05  WS-LINES-PER-PAGE           PIC 9(02) VALUE 12.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-LINE                     PIC S9(04) COMP.
           05  WS-SHOW-COUNT               PIC S9(04) COMP.
           05  WS-SEL-CODE                 PIC X(01).
           05  WS-SEL-COUNT                PIC S9(04) COMP.
           05  WS-SEL-LINE                 PIC S9(04) COMP.
           05  WS-PAGE-MSG                 PIC X(79).
           05  WS-INC-CANC                 PIC X(01).
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'DM01 - DIRECT DEBIT MANDATES ENDED'.
           05  WS-VALID-IND                PIC X(01).
               88  WS-INPUT-VALID              VALUE 'Y'.
           05  WS-NEW-KEY-IND              PIC X(01).
               88  WS-NEW-KEY                  VALUE 'Y'.
           05  WS-PEND-ON-PAGE-IND         PIC X(01).
               88  WS-PEND-ON-PAGE             VALUE 'Y'.
           05  WS-PAGE-EDIT                PIC ZZ9.
      *
      *---- DATE FORMATTING ------------------------------------------*
       01  WS-FMT-DATE-IN                  PIC 9(08).
       01  WS-FMT-DATE-IN-X REDEFINES WS-FMT-DATE-IN
                                           PIC X(08).
       01  WS-FMT-DATE-OUT                 PIC X(08).
      *
      *---- PSEUDO-CONVERSATIONAL COMMAREA ---------------------------*
       01  WS-COMMAREA.
           05  CA-EYECATCHER               PIC X(04).
               88  CA-OURS                     VALUE 'DM01'.
           05  CA-FROM-TRAN                PIC X(04).
           05  CA-SORT-CODE                PIC X(06).
           05  CA-ACCT-NO                  PIC X(08).
           05  CA-CIF-NO                   PIC X(12).
           05  CA-INC-CANC                 PIC X(01).
           05  CA-LISTED-IND               PIC X(01).
               88  CA-LISTED                   VALUE 'Y'.
           05  CA-PAGE                     PIC 9(03).
           05  CA-MORE                     PIC X(01).
      *        KEY USED FOR THE PAGE ON SCREEN / FOR THE NEXT PAGE
           05  CA-CUR-RESTART              PIC X(24).
           05  CA-NEXT-RESTART             PIC X(24).
           05  CA-LINE-COUNT               PIC 9(02).
           05  CA-LINE                     OCCURS 12 TIMES.
               10  CA-L-SUN                PIC X(06).
               10  CA-L-PAYER-REF          PIC X(18).
               10  CA-L-STATUS             PIC X(01).
      *        MANDATE SELECTED WITH X, AWAITING PF5
           05  CA-PENDING.
               10  CA-PEND-SUN             PIC X(06).
               10  CA-PEND-PAYER-REF       PIC X(18).
               10  CA-PEND-ORIG-NAME       PIC X(18).
      *
      *---- COMMAREA RECEIVED FROM CF02 -------------------------------*
       01  WS-CF02-COMMAREA.
           05  XC-FROM-TRAN                PIC X(04).
           05  XC-SORT-CODE                PIC X(06).
           05  XC-ACCT-NO                  PIC X(08).
           05  XC-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(20).
      *
       01  WS-DM-LINE.
           05  DL-SUN                      PIC X(06).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-ORIG-NAME                PIC X(18).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-PAYER-REF                PIC X(18).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-STATUS                   PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-SETUP                    PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-LAST-COLL                PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-AMOUNT                   PIC ZZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
      *
           COPY DMMAPS.
      *
           COPY DDACCREC.
      *
           COPY CFCUSREC.
      *
           COPY DFHAID.
      *
           COPY DFHBMSCA.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(432).
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
                   PERFORM 1000-SEND-EMPTY-MAP
               WHEN EIBCALEN = LENGTH OF WS-CF02-COMMAREA
                   MOVE DFHCOMMAREA (1:50) TO WS-CF02-COMMAREA
                   PERFORM 1100-START-FROM-CF02
               WHEN OTHER
                   MOVE DFHCOMMAREA        TO WS-COMMAREA
                   IF NOT CA-OURS
                       PERFORM 1000-SEND-EMPTY-MAP
                   ELSE
                       PERFORM 0100-DISPATCH-AID
                   END-IF
           END-EVALUATE

           EXEC CICS RETURN
                     TRANSID('DM01')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       0100-DISPATCH-AID.
      *================================================================*
      *    A PENDING CANCEL ONLY SURVIVES ONE INTERACTION - ANY KEY
      *    OTHER THAN PF5 ABANDONS IT.
           IF EIBAID NOT = DFHPF5 AND CA-PEND-SUN NOT = SPACES
               MOVE SPACES                 TO CA-PENDING
               MOVE 'CANCELLATION ABANDONED' TO WS-PAGE-MSG
           ELSE
               MOVE SPACES                 TO WS-PAGE-MSG
           END-IF

           EVALUATE EIBAID
               WHEN DFHPF3
                   PERFORM 9000-END-SESSION
               WHEN DFHPF12
                   PERFORM 9100-RETURN-TO-CF02
               WHEN DFHCLEAR
                   PERFORM 1000-SEND-EMPTY-MAP
               WHEN DFHPF5
                   PERFORM 5000-CONFIRM-CANCEL THRU 5000-EXIT
               WHEN DFHPF8
                   PERFORM 4000-PAGE-FORWARD
               WHEN DFHENTER
                   PERFORM 2000-PROCESS-ENTER THRU 2000-EXIT
               WHEN OTHER
                   MOVE LOW-VALUES         TO DMM01O
                   MOVE 'INVALID KEY - USE ENTER, PF3, PF5, PF8, PF12'
                                           TO MSGO
                   PERFORM 8000-SEND-DATAONLY
           END-EVALUATE
           .
      *================================================================*
       1000-SEND-EMPTY-MAP.
      *================================================================*
           PERFORM 1200-INIT-COMMAREA
           MOVE LOW-VALUES                 TO DMM01O
           MOVE 'N'                        TO INCCANO
           MOVE 'KEY SORT CODE AND ACCOUNT NUMBER, PRESS ENTER'
                                           TO MSGO
           PERFORM 8100-SEND-MAP
           .
      *================================================================*
       1100-START-FROM-CF02.
      *================================================================*
           PERFORM 1200-INIT-COMMAREA
           MOVE XC-FROM-TRAN               TO CA-FROM-TRAN
           MOVE XC-SORT-CODE               TO CA-SORT-CODE
           MOVE XC-ACCT-NO                 TO CA-ACCT-NO
           MOVE XC-CIF-NO                  TO CA-CIF-NO
           MOVE SPACES                     TO WS-PAGE-MSG
           PERFORM 3000-FIRST-PAGE
           .
      *================================================================*
       1200-INIT-COMMAREA.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           MOVE 'N'                        TO CA-INC-CANC
                                              CA-LISTED-IND
                                              CA-MORE
           MOVE ZERO                       TO CA-PAGE CA-LINE-COUNT
           .
      *================================================================*
       2000-PROCESS-ENTER.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('DMM01')
                     MAPSET('DMMAPS')
                     INTO(DMM01I)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               IF CA-LISTED
                   PERFORM 3100-SAME-PAGE
               ELSE
                   MOVE LOW-VALUES         TO DMM01O
                   MOVE 'SORT CODE AND ACCOUNT NUMBER ARE REQUIRED'
                                           TO MSGO
                   MOVE -1                 TO SORTCDL
                   PERFORM 8000-SEND-DATAONLY
               END-IF
               GO TO 2000-EXIT
           END-IF

           PERFORM 2100-RECEIVE-KEYS THRU 2100-EXIT
           IF NOT WS-INPUT-VALID
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

      *    NEW ACCOUNT OR CHANGED FILTER - START AGAIN AT PAGE 1 AND
      *    IGNORE ANY SELECTIONS (THEY BELONG TO THE OLD LIST).
           IF WS-NEW-KEY OR NOT CA-LISTED
               PERFORM 3000-FIRST-PAGE
               GO TO 2000-EXIT
           END-IF

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
               WHEN WS-SEL-COUNT = ZERO
                   PERFORM 3100-SAME-PAGE
               WHEN WS-SEL-COUNT > 1
                   MOVE 'SELECT ONE MANDATE ONLY' TO WS-PAGE-MSG
                   PERFORM 3100-SAME-PAGE
               WHEN OTHER
                   PERFORM 2300-PROCESS-SELECTION
           END-EVALUATE
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-RECEIVE-KEYS.
      *================================================================*
      *    A FIELD NOT RE-KEYED KEEPS ITS COMMAREA VALUE (FSET).
           MOVE 'Y'                        TO WS-VALID-IND
           MOVE 'N'                        TO WS-NEW-KEY-IND
           IF SORTCDL > ZERO
               IF SORTCDI NOT = CA-SORT-CODE
                   MOVE 'Y'                TO WS-NEW-KEY-IND
                   MOVE SPACES             TO CA-CIF-NO
               END-IF
               MOVE SORTCDI                TO CA-SORT-CODE
           END-IF
           IF ACCTNOL > ZERO
               IF ACCTNOI NOT = CA-ACCT-NO
                   MOVE 'Y'                TO WS-NEW-KEY-IND
                   MOVE SPACES             TO CA-CIF-NO
               END-IF
               MOVE ACCTNOI                TO CA-ACCT-NO
           END-IF
           MOVE LOW-VALUES                 TO DMM01O

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

           IF INCCANL > ZERO
               MOVE FUNCTION UPPER-CASE (INCCANI) TO WS-INC-CANC
               IF WS-INC-CANC = SPACE OR LOW-VALUE
                   MOVE 'N'                TO WS-INC-CANC
               END-IF
               IF WS-INC-CANC NOT = 'Y' AND 'N'
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'INCLUDE CANCELLED MUST BE Y OR N' TO MSGO
                   MOVE -1                 TO INCCANL
                   MOVE DFHRED             TO INCCANA
                   GO TO 2100-EXIT
               END-IF
               IF WS-INC-CANC NOT = CA-INC-CANC
                   MOVE 'Y'                TO WS-NEW-KEY-IND
                   MOVE WS-INC-CANC        TO CA-INC-CANC
               END-IF
           END-IF
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
       2300-PROCESS-SELECTION.
      *================================================================*
           MOVE WS-SEL-LINE                TO WS-LINE
           PERFORM 2200-GET-SEL
           EVALUATE TRUE
               WHEN FUNCTION UPPER-CASE (WS-SEL-CODE) NOT = 'X'
                   MOVE 'SELECTION MUST BE X (CANCEL)' TO WS-PAGE-MSG
               WHEN WS-SEL-LINE > CA-LINE-COUNT
                   MOVE 'NO MANDATE ON THAT LINE' TO WS-PAGE-MSG
               WHEN CA-L-STATUS (WS-SEL-LINE) NOT = 'L' AND 'D'
                   MOVE 'MANDATE IS ALREADY CANCELLED OR CLOSED'
                                           TO WS-PAGE-MSG
               WHEN OTHER
                   MOVE CA-L-SUN (WS-SEL-LINE)
                                           TO CA-PEND-SUN
                   MOVE CA-L-PAYER-REF (WS-SEL-LINE)
                                           TO CA-PEND-PAYER-REF
      *            ORIGINATOR NAME IS FILLED IN BY 3900 FROM THE
      *            REFRESHED PAGE (IT IS NOT KEPT FOR EVERY LINE).
                   MOVE SPACES             TO CA-PEND-ORIG-NAME
           END-EVALUATE
      *    THE PAGE IS RE-READ SO THE CUSTOMER SEES THE MANDATE AS IT
      *    IS NOW (IT MAY HAVE BEEN CANCELLED BY THE ORIGINATOR).
           PERFORM 3100-SAME-PAGE
           .
      *================================================================*
       3000-FIRST-PAGE.
      *================================================================*
           MOVE ZERO                       TO CA-PAGE
           MOVE SPACES                     TO CA-CUR-RESTART
                                              CA-NEXT-RESTART
                                              CA-PENDING
           PERFORM 3500-FETCH-PAGE THRU 3500-EXIT
           .
      *================================================================*
       3100-SAME-PAGE.
      *================================================================*
      *    REBUILD THE PAGE ON SCREEN FROM THE SAME RESTART KEY.
           IF CA-PAGE > ZERO
               SUBTRACT 1                  FROM CA-PAGE
           END-IF
           PERFORM 3500-FETCH-PAGE THRU 3500-EXIT
           .
      *================================================================*
       3500-FETCH-PAGE.
      *================================================================*
      *    ON ENTRY CA-CUR-RESTART HOLDS THE RESTART KEY FOR THE PAGE
      *    (SPACES = FIRST PAGE) AND WS-PAGE-MSG ANY MESSAGE TO SHOW.
           PERFORM 7000-INIT-SERVICE
           SET CHDM-LIST                   TO TRUE
           MOVE CA-INC-CANC                TO CHDM-INCLUDE-CANC
           MOVE CA-CUR-RESTART             TO CHDM-RESTART-KEY
           PERFORM 7100-LINK-SERVICE

           MOVE LOW-VALUES                 TO DMM01O
           PERFORM 3600-FORMAT-HEADER THRU 3600-EXIT

           IF NOT (CH-RC-OK OR CH-RC-WARNING)
               MOVE 'N'                    TO CA-LISTED-IND
               MOVE ZERO                   TO CA-LINE-COUNT
               MOVE SPACES                 TO CA-PENDING
               MOVE CH-MESSAGE-TEXT        TO MSGO
               MOVE -1                     TO SORTCDL
               PERFORM 8100-SEND-MAP
               GO TO 3500-EXIT
           END-IF

           SET CA-LISTED                   TO TRUE
           ADD 1                           TO CA-PAGE
           IF CHDM-COUNT > WS-LINES-PER-PAGE
               MOVE WS-LINES-PER-PAGE      TO WS-SHOW-COUNT
           ELSE
               MOVE CHDM-COUNT             TO WS-SHOW-COUNT
           END-IF
           MOVE WS-SHOW-COUNT              TO CA-LINE-COUNT

      *    MORE IF THE SERVICE HAS MORE, OR IF IT GAVE US ROWS WE
      *    DID NOT SHOW - THEN RESTART FROM THE LAST LINE SHOWN.
           IF CHDM-COUNT > WS-LINES-PER-PAGE
               MOVE 'Y'                    TO CA-MORE
               MOVE CHDME-ORIG-SUN (WS-SHOW-COUNT)
                                           TO CA-NEXT-RESTART (1:6)
               MOVE CHDME-PAYER-REF (WS-SHOW-COUNT)
                                           TO CA-NEXT-RESTART (7:18)
           ELSE
               MOVE CHDM-MORE-IND          TO CA-MORE
               MOVE CHDM-NEXT-RESTART      TO CA-NEXT-RESTART
           END-IF

           MOVE 'N'                        TO WS-PEND-ON-PAGE-IND
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-LINES-PER-PAGE
               MOVE SPACES                 TO WS-DM-LINE
               IF WS-LINE <= WS-SHOW-COUNT
                   PERFORM 3700-FORMAT-MANDATE
               ELSE
                   MOVE SPACES             TO CA-L-SUN (WS-LINE)
                                              CA-L-PAYER-REF (WS-LINE)
                                              CA-L-STATUS (WS-LINE)
               END-IF
               PERFORM 3800-PLACE-LINE
           END-PERFORM

           IF CA-PEND-SUN NOT = SPACES
               IF WS-PEND-ON-PAGE
                   PERFORM 3900-MARK-PENDING
               ELSE
                   MOVE SPACES             TO CA-PENDING
                   MOVE 'MANDATE NO LONGER LISTED - NOT CANCELLED'
                                           TO WS-PAGE-MSG
               END-IF
           END-IF

           MOVE CA-INC-CANC                TO INCCANO
           EVALUATE TRUE
               WHEN CA-PEND-SUN NOT = SPACES
                   CONTINUE
               WHEN WS-PAGE-MSG NOT = SPACES
                   MOVE WS-PAGE-MSG        TO MSGO
               WHEN CH-RC-WARNING
                   MOVE CH-MESSAGE-TEXT    TO MSGO
               WHEN CA-MORE = 'Y'
                   MOVE CA-PAGE            TO WS-PAGE-EDIT
                   STRING 'PAGE ' WS-PAGE-EDIT
                          ' - MORE MANDATES, PRESS PF8'
                          DELIMITED BY SIZE
                          INTO MSGO
                   END-STRING
               WHEN OTHER
                   MOVE CA-PAGE            TO WS-PAGE-EDIT
                   STRING 'PAGE ' WS-PAGE-EDIT
                          ' - END OF LIST. X TO CANCEL A MANDATE'
                          DELIMITED BY SIZE
                          INTO MSGO
                   END-STRING
           END-EVALUATE
           PERFORM 8100-SEND-MAP
           .
       3500-EXIT.
           EXIT.
      *================================================================*
       3600-FORMAT-HEADER.
      *================================================================*
           MOVE CA-SORT-CODE               TO SORTCDO
           MOVE CA-ACCT-NO                 TO ACCTNOO
           MOVE CA-SORT-CODE               TO ACM-SORT-CODE
           MOVE CA-ACCT-NO                 TO ACM-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(ACM-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'N/A'                  TO PRODCDO
               GO TO 3600-EXIT
           END-IF
           MOVE ACM-PROD-CD                TO PRODCDO
           IF CA-CIF-NO = SPACES
               MOVE ACM-CIF-NO             TO CA-CIF-NO
           END-IF

           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(ACM-CIF-NO)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE SPACES                 TO CUSTNMO
               STRING CIF-TITLE     DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-FIRST-NM  DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-LAST-NM   DELIMITED BY '  '
                      INTO CUSTNMO
               END-STRING
           ELSE
               MOVE 'CUSTOMER NOT AVAILABLE' TO CUSTNMO
           END-IF
           .
       3600-EXIT.
           EXIT.
      *================================================================*
       3700-FORMAT-MANDATE.
      *================================================================*
           MOVE CHDME-ORIG-SUN (WS-LINE)   TO DL-SUN
                                              CA-L-SUN (WS-LINE)
           MOVE CHDME-ORIG-NAME (WS-LINE)  TO DL-ORIG-NAME
           MOVE CHDME-PAYER-REF (WS-LINE)  TO DL-PAYER-REF
                                              CA-L-PAYER-REF (WS-LINE)
           MOVE CHDME-STATUS (WS-LINE)     TO CA-L-STATUS (WS-LINE)
           EVALUATE CHDME-STATUS (WS-LINE)
               WHEN 'L'  MOVE 'LV'         TO DL-STATUS
               WHEN 'D'  MOVE 'DM'         TO DL-STATUS
               WHEN 'C'  MOVE 'CP'         TO DL-STATUS
               WHEN 'O'  MOVE 'CO'         TO DL-STATUS
               WHEN 'X'  MOVE 'CL'         TO DL-STATUS
               WHEN 'T'  MOVE 'TR'         TO DL-STATUS
               WHEN OTHER
                   MOVE CHDME-STATUS (WS-LINE) TO DL-STATUS
           END-EVALUATE

           MOVE CHDME-SETUP-DATE (WS-LINE) TO WS-FMT-DATE-IN
           PERFORM 6100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT            TO DL-SETUP
           MOVE CHDME-LAST-COLL-DATE (WS-LINE) TO WS-FMT-DATE-IN
           PERFORM 6100-FORMAT-DATE
           IF WS-FMT-DATE-OUT = SPACES
               MOVE 'NONE'                 TO DL-LAST-COLL
               MOVE ZERO                   TO DL-AMOUNT
           ELSE
               MOVE WS-FMT-DATE-OUT        TO DL-LAST-COLL
               MOVE CHDME-LAST-COLL-AMT (WS-LINE) TO DL-AMOUNT
           END-IF

           IF CA-PEND-SUN NOT = SPACES
           AND CHDME-ORIG-SUN (WS-LINE) = CA-PEND-SUN
           AND CHDME-PAYER-REF (WS-LINE) = CA-PEND-PAYER-REF
      *        ONLY STILL PENDING IF IT IS STILL LIVE OR DORMANT.
               IF CHDME-STATUS (WS-LINE) = 'L' OR 'D'
                   MOVE 'Y'                TO WS-PEND-ON-PAGE-IND
                   MOVE WS-LINE            TO WS-SEL-LINE
                   MOVE CHDME-ORIG-NAME (WS-LINE)
                                           TO CA-PEND-ORIG-NAME
               END-IF
           END-IF
           .
      *================================================================*
       3800-PLACE-LINE.
      *================================================================*
           EVALUATE WS-LINE
               WHEN 1   MOVE WS-DM-LINE TO DML1O
               WHEN 2   MOVE WS-DM-LINE TO DML2O
               WHEN 3   MOVE WS-DM-LINE TO DML3O
               WHEN 4   MOVE WS-DM-LINE TO DML4O
               WHEN 5   MOVE WS-DM-LINE TO DML5O
               WHEN 6   MOVE WS-DM-LINE TO DML6O
               WHEN 7   MOVE WS-DM-LINE TO DML7O
               WHEN 8   MOVE WS-DM-LINE TO DML8O
               WHEN 9   MOVE WS-DM-LINE TO DML9O
               WHEN 10  MOVE WS-DM-LINE TO DML10O
               WHEN 11  MOVE WS-DM-LINE TO DML11O
               WHEN 12  MOVE WS-DM-LINE TO DML12O
           END-EVALUATE
           .
      *================================================================*
       3900-MARK-PENDING.
      *================================================================*
      *    SHOW THE X AGAINST THE PENDING LINE, INTENSIFY IT AND ASK
      *    FOR PF5. THE X IS SENT WITHOUT FSET SO A PLAIN
      *    ENTER DOES NOT RE-SELECT IT.
           EVALUATE WS-SEL-LINE
               WHEN 1   MOVE 'X' TO SEL1O   MOVE DFHBMASB TO DML1A
               WHEN 2   MOVE 'X' TO SEL2O   MOVE DFHBMASB TO DML2A
               WHEN 3   MOVE 'X' TO SEL3O   MOVE DFHBMASB TO DML3A
               WHEN 4   MOVE 'X' TO SEL4O   MOVE DFHBMASB TO DML4A
               WHEN 5   MOVE 'X' TO SEL5O   MOVE DFHBMASB TO DML5A
               WHEN 6   MOVE 'X' TO SEL6O   MOVE DFHBMASB TO DML6A
               WHEN 7   MOVE 'X' TO SEL7O   MOVE DFHBMASB TO DML7A
               WHEN 8   MOVE 'X' TO SEL8O   MOVE DFHBMASB TO DML8A
               WHEN 9   MOVE 'X' TO SEL9O   MOVE DFHBMASB TO DML9A
               WHEN 10  MOVE 'X' TO SEL10O  MOVE DFHBMASB TO DML10A
               WHEN 11  MOVE 'X' TO SEL11O  MOVE DFHBMASB TO DML11A
               WHEN 12  MOVE 'X' TO SEL12O  MOVE DFHBMASB TO DML12A
           END-EVALUATE
           MOVE SPACES                     TO MSGO
           STRING 'CANCEL DD TO '       DELIMITED BY SIZE
                  CA-PEND-ORIG-NAME     DELIMITED BY '  '
                  ' REF '               DELIMITED BY SIZE
                  CA-PEND-PAYER-REF     DELIMITED BY '  '
                  ' - PF5 TO CONFIRM'
                                        DELIMITED BY SIZE
                  INTO MSGO
           END-STRING
           .
      *================================================================*
       4000-PAGE-FORWARD.
      *================================================================*
           EVALUATE TRUE
               WHEN NOT CA-LISTED
                   MOVE LOW-VALUES         TO DMM01O
                   MOVE 'KEY SORT CODE AND ACCOUNT, THEN ENTER'
                                           TO MSGO
                   MOVE -1                 TO SORTCDL
                   PERFORM 8000-SEND-DATAONLY
               WHEN CA-MORE = 'Y'
                   MOVE CA-NEXT-RESTART    TO CA-CUR-RESTART
                   PERFORM 3500-FETCH-PAGE THRU 3500-EXIT
               WHEN OTHER
      *            WRAP TO THE FIRST PAGE (AS SO01 DOES).
                   PERFORM 3000-FIRST-PAGE
           END-EVALUATE
           .
      *================================================================*
       5000-CONFIRM-CANCEL.
      *================================================================*
           IF CA-PEND-SUN = SPACES
               MOVE LOW-VALUES             TO DMM01O
               MOVE 'KEY X AGAINST A MANDATE AND PRESS ENTER FIRST'
                                           TO MSGO
               PERFORM 8000-SEND-DATAONLY
               GO TO 5000-EXIT
           END-IF

           PERFORM 7000-INIT-SERVICE
           SET CHDM-CANCEL                 TO TRUE
           MOVE CA-PEND-SUN                TO CHDM-ORIG-SUN
           MOVE CA-PEND-PAYER-REF          TO CHDM-PAYER-REF
           MOVE CA-INC-CANC                TO CHDM-INCLUDE-CANC
           PERFORM 7100-LINK-SERVICE

      *    WHATEVER THE OUTCOME THE CONFIRMATION HAS BEEN USED.
           MOVE SPACES                     TO CA-PENDING
           MOVE CH-MESSAGE-TEXT            TO WS-PAGE-MSG
           IF CH-RC-SYSTEM-ERROR
               MOVE LOW-VALUES             TO DMM01O
               MOVE WS-PAGE-MSG            TO MSGO
               PERFORM 8000-SEND-DATAONLY
           ELSE
      *        MANDATE CANCELLED (OR MNFD / MCAN) - SHOW THE PAGE AS
      *        IT IS NOW WITH THE SERVICE'S MESSAGE.
               PERFORM 3100-SAME-PAGE
           END-IF
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       6100-FORMAT-DATE.
      *================================================================*
      *    CCYYMMDD -> DD/MM/YY. ZERO OR NON-NUMERIC -> SPACES.
           MOVE SPACES                     TO WS-FMT-DATE-OUT
           IF WS-FMT-DATE-IN-X NUMERIC AND WS-FMT-DATE-IN > ZERO
               STRING WS-FMT-DATE-IN-X (7:2) '/'
                      WS-FMT-DATE-IN-X (5:2) '/'
                      WS-FMT-DATE-IN-X (3:2)
                      DELIMITED BY SIZE
                      INTO WS-FMT-DATE-OUT
               END-STRING
           END-IF
           .
      *================================================================*
       7000-INIT-SERVICE.
      *================================================================*
           MOVE SPACES                     TO CH-COMMAREA
           SET CH-EYECATCHER-OK            TO TRUE
           MOVE '01'                       TO CH-VERSION
           MOVE 'DDMANDT '                 TO CH-SERVICE-CD
           MOVE 'BRANCH'                   TO CH-CHANNEL-CD
           MOVE EIBTRMID                   TO CH-USER-ID
           STRING 'DM01' EIBTRMID EIBTASKN
                  DELIMITED BY SIZE
                  INTO CH-REQUEST-ID
           END-STRING
           MOVE CA-SORT-CODE               TO CHDM-SORT-CODE
           MOVE CA-ACCT-NO                 TO CHDM-ACCT-NO
           .
      *================================================================*
       7100-LINK-SERVICE.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM('DMSVC01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'LINK'                 TO CH-REASON-CD
               MOVE WS-RESP                TO WS-RESP-DISPLAY
               MOVE SPACES                 TO CH-MESSAGE-TEXT
               STRING 'MANDATE SERVICE UNAVAILABLE - EIBRESP '
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
                     MAP('DMM01')
                     MAPSET('DMMAPS')
                     FROM(DMM01O)
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
                     MAP('DMM01')
                     MAPSET('DMMAPS')
                     FROM(DMM01O)
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
           MOVE LOW-VALUES                 TO DMM01O
           MOVE 'CF02 NOT AVAILABLE'       TO MSGO
           PERFORM 8000-SEND-DATAONLY
           .
