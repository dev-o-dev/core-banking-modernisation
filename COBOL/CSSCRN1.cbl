       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CSSCRN1.
       AUTHOR.        P D QUINN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2013-07-08.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CSSCRN1                                             *
      * TITLE    : BRANCH CURRENT ACCOUNT SWITCH (CASS) SCREEN         *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION CS01       *
      * MAPSET   : CSMAPS  MAP CSM01                                   *
      * LINKS    : CSSVC01 (SERVICE SWITCHRQ - E, R, X)                *
      * FILES    : ACCTMST, CIFMAST (READ - HEADER DETAILS)            *
      *----------------------------------------------------------------*
      * THE BRANCH OPENS THE NEW CURRENT ACCOUNT (CF01 / ACCTOPEN) AND *
      * THEN USES CS01 TO REQUEST THE SWITCH FROM THE CUSTOMER'S OLD   *
      * BANK. ENTER WITH THE NEW ACCOUNT LISTS ITS SWITCHES (BOTH      *
      * DIRECTIONS - AN ACCOUNT SWITCHING AWAY IS SHOWN AS 'O').       *
      *                                                                *
      * REQUEST: KEY THE OLD SORT CODE / ACCOUNT AND SWITCH DATE, PRESS*
      * ENTER - THE DETAILS ARE CHECKED AND THE SCREEN ASKS FOR PF5 TO *
      * CONFIRM (THE CUSTOMER MUST HAVE SIGNED THE CASS SWITCH FORM -  *
      * THE CASHIER CONFIRMS IT IS ON FILE). CANCEL: 'X' AGAINST A     *
      * SWITCH NOT YET SENT TO CASS, THEN PF5.                         *
      *                                                                *
      * ENTERED FROM CF02 WITH THE ACCOUNT IN THE COMMAREA (50 BYTES)  *
      * OR DIRECTLY BY TRANSACTION CS01.                               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2013-07-08 PDQ  ORIGINAL VERSION (CASS GO-LIVE 16/09/2013)     *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CSSCRN1 '.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-SEL-COUNT                PIC S9(04) COMP.
           05  WS-SEL-ROW                  PIC S9(04) COMP.
           05  WS-VALID-IND                PIC X(01).
               88  WS-INPUT-VALID              VALUE 'Y'.
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'CS01 - CURRENT ACCOUNT SWITCH ENDED'.
           05  WS-DATE-EDIT                PIC X(10).
      *
      *---- ONE LIST LINE ---------------------------------------------*
       01  WS-SW-LINE.
           05  SL-SWITCH-ID                PIC X(18).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-DIR                      PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  SL-STATUS                   PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-OLD-SC                   PIC X(06).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  SL-OLD-ACCT                 PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  SL-SW-DATE                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-SOS                      PIC ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-DDS                      PIC ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SL-RSN                      PIC X(04).
           05  FILLER                      PIC X(11) VALUE SPACES.
      *
      *---- SELECTION CODES FROM THE MAP ------------------------------*
       01  WS-SEL-TABLE.
           05  WS-SEL                      PIC X(01) OCCURS 10 TIMES.
      *
      *---- PSEUDO-CONVERSATIONAL COMMAREA ---------------------------*
       01  WS-COMMAREA.
           05  CA-EYECATCHER               PIC X(04).
               88  CA-OURS                     VALUE 'CS01'.
           05  CA-FROM-TRAN                PIC X(04).
           05  CA-SORT-CODE                PIC X(06).
           05  CA-ACCT-NO                  PIC X(08).
           05  CA-CIF-NO                   PIC X(12).
           05  CA-PENDING                  PIC X(01).
               88  CA-NOTHING-PENDING          VALUE SPACE.
               88  CA-REQUEST-PENDING          VALUE 'R'.
               88  CA-CANCEL-PENDING           VALUE 'X'.
           05  CA-OLD-SORT-CODE            PIC X(06).
           05  CA-OLD-ACCT-NO              PIC X(08).
           05  CA-SWITCH-DATE              PIC 9(08).
           05  CA-CANCEL-ID                PIC X(18).
           05  CA-LIST-COUNT               PIC 9(02).
           05  CA-LIST-ID                  PIC X(18) OCCURS 10 TIMES.
           05  CA-LIST-CANCELLABLE         PIC X(01) OCCURS 10 TIMES.
           05  FILLER                      PIC X(10).
      *
      *---- COMMAREA RECEIVED FROM CF02 -------------------------------*
       01  WS-CF02-COMMAREA.
           05  XC-FROM-TRAN                PIC X(04).
           05  XC-SORT-CODE                PIC X(06).
           05  XC-ACCT-NO                  PIC X(08).
           05  XC-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(20).
      *
           COPY CSMAPS.
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
       01  DFHCOMMAREA                     PIC X(277).
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
                       WHEN DFHPF5
                           PERFORM 5000-CONFIRM THRU 5000-EXIT
                       WHEN DFHENTER
                           PERFORM 2000-PROCESS-ENTER THRU 2000-EXIT
                       WHEN OTHER
                           MOVE LOW-VALUES TO CSM01O
                           MOVE 'INVALID KEY - USE ENTER PF3 PF5 PF12'
                                           TO MSGO
                           PERFORM 8000-SEND-DATAONLY
                   END-EVALUATE
           END-EVALUATE

           EXEC CICS RETURN
                     TRANSID('CS01')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       1000-SEND-EMPTY-MAP.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           MOVE ZERO                       TO CA-SWITCH-DATE
                                              CA-LIST-COUNT
           MOVE LOW-VALUES                 TO CSM01O
           MOVE 'KEY THE NEW MEGABANK CURRENT ACCOUNT AND PRESS ENTER'
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
           MOVE ZERO                       TO CA-SWITCH-DATE
                                              CA-LIST-COUNT
           MOVE LOW-VALUES                 TO CSM01O
           PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
           PERFORM 8100-SEND-MAP
           .
      *================================================================*
       2000-PROCESS-ENTER.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('CSM01')
                     MAPSET('CSMAPS')
                     INTO(CSM01I)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES             TO CSM01O
               MOVE 'SORT CODE AND ACCOUNT NUMBER ARE REQUIRED'
                                           TO MSGO
               MOVE -1                     TO SORTCDL
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF
           MOVE SPACE                      TO CA-PENDING
           PERFORM 2050-COLLECT-SELECTIONS

           MOVE 'Y'                        TO WS-VALID-IND
           IF SORTCDL > ZERO
               MOVE SORTCDI                TO CA-SORT-CODE
           END-IF
           IF ACCTNOL > ZERO
               MOVE ACCTNOI                TO CA-ACCT-NO
           END-IF
           IF CA-SORT-CODE NOT NUMERIC OR CA-ACCT-NO NOT NUMERIC
               MOVE LOW-VALUES             TO CSM01O
               MOVE 'SORT CODE (6) AND ACCOUNT (8) MUST BE NUMERIC'
                                           TO MSGO
               MOVE -1                     TO SORTCDL
               MOVE DFHRED                 TO SORTCDA
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

           MOVE LOW-VALUES                 TO CSM01O
           EVALUATE TRUE
               WHEN WS-SEL-COUNT > 1
                   PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
                   MOVE 'SELECT ONE SWITCH ONLY' TO MSGO
               WHEN WS-SEL-COUNT = 1
                   PERFORM 2200-PREPARE-CANCEL THRU 2200-EXIT
               WHEN OLDSCL > ZERO OR OLDACCL > ZERO OR SWDATEL > ZERO
                   PERFORM 2100-PREPARE-REQUEST THRU 2100-EXIT
               WHEN OTHER
                   PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
           END-EVALUATE
           PERFORM 8100-SEND-MAP
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2050-COLLECT-SELECTIONS.
      *================================================================*
           MOVE SEL1I                      TO WS-SEL (1)
           MOVE SEL2I                      TO WS-SEL (2)
           MOVE SEL3I                      TO WS-SEL (3)
           MOVE SEL4I                      TO WS-SEL (4)
           MOVE SEL5I                      TO WS-SEL (5)
           MOVE SEL6I                      TO WS-SEL (6)
           MOVE SEL7I                      TO WS-SEL (7)
           MOVE SEL8I                      TO WS-SEL (8)
           MOVE SEL9I                      TO WS-SEL (9)
           MOVE SEL10I                     TO WS-SEL (10)
           MOVE ZERO                       TO WS-SEL-COUNT WS-SEL-ROW
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 10
               IF WS-SEL (WS-SUB) = 'X' OR 'x'
                   ADD 1                   TO WS-SEL-COUNT
                   MOVE WS-SUB             TO WS-SEL-ROW
               END-IF
           END-PERFORM
           .
      *================================================================*
       2100-PREPARE-REQUEST.
      *================================================================*
      *    CHECK FORMAT HERE; THE SERVICE APPLIES THE BUSINESS RULES ON
      *    PF5 (IT IS THE ONLY PLACE THE WORKING-DAY CALENDAR IS USED).
           MOVE OLDSCI                     TO CA-OLD-SORT-CODE
           MOVE OLDACCI                    TO CA-OLD-ACCT-NO
           EVALUATE TRUE
               WHEN CA-OLD-SORT-CODE NOT NUMERIC
                   MOVE 'OLD SORT CODE MUST BE 6 DIGITS' TO MSGO
                   MOVE -1                 TO OLDSCL
               WHEN CA-OLD-ACCT-NO NOT NUMERIC
                   MOVE 'OLD ACCOUNT MUST BE 8 DIGITS' TO MSGO
                   MOVE -1                 TO OLDACCL
               WHEN SWDATEI NOT NUMERIC
                   MOVE 'SWITCH DATE MUST BE CCYYMMDD' TO MSGO
                   MOVE -1                 TO SWDATEL
               WHEN OTHER
                   MOVE SWDATEI            TO CA-SWITCH-DATE
                   SET CA-REQUEST-PENDING  TO TRUE
           END-EVALUATE
           PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
           MOVE CA-OLD-SORT-CODE           TO OLDSCO
           MOVE CA-OLD-ACCT-NO             TO OLDACCO
           IF CA-REQUEST-PENDING
               MOVE CA-SWITCH-DATE         TO SWDATEO
               MOVE 'CHECK DETAILS AND SIGNED SWITCH FORM - PF5 TO SEND'
                                           TO MSGO
           ELSE
               MOVE SWDATEI                TO SWDATEO
           END-IF
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-PREPARE-CANCEL.
      *================================================================*
           IF WS-SEL-ROW > CA-LIST-COUNT
               PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
               MOVE 'NO SWITCH ON THAT LINE' TO MSGO
               GO TO 2200-EXIT
           END-IF
           IF CA-LIST-CANCELLABLE (WS-SEL-ROW) NOT = 'Y'
               PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
               MOVE 'ONLY UNSENT REQUESTS CAN BE CANCELLED'
                                           TO MSGO
               GO TO 2200-EXIT
           END-IF
           MOVE CA-LIST-ID (WS-SEL-ROW)    TO CA-CANCEL-ID
           SET CA-CANCEL-PENDING           TO TRUE
           PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
           STRING 'PF5 TO CANCEL SWITCH ' CA-CANCEL-ID
                  DELIMITED BY SIZE
                  INTO MSGO
           END-STRING
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       3000-LIST-SWITCHES.
      *================================================================*
           PERFORM 3500-FORMAT-HEADER
           PERFORM 7000-INIT-SERVICE
           SET CHSW-ENQUIRE                TO TRUE
           MOVE CA-SORT-CODE               TO CHSW-NEW-SORT-CODE
           MOVE CA-ACCT-NO                 TO CHSW-NEW-ACCT-NO
           PERFORM 7100-LINK-SERVICE
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 3000-EXIT
           END-IF
           IF CHSW-EARLIEST-DATE NUMERIC
               STRING CHSW-EARLIEST-DATE (7:2) '/'
                      CHSW-EARLIEST-DATE (5:2) '/'
                      CHSW-EARLIEST-DATE (1:4)
                      DELIMITED BY SIZE
                      INTO EARLYO
               END-STRING
           END-IF
           MOVE ZERO                       TO CA-LIST-COUNT
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > CHSW-COUNT OR WS-SUB > 10
               PERFORM 3100-FORMAT-LINE
           END-PERFORM
           IF CH-RC-WARNING OR NOT CH-RC-OK
               MOVE CH-MESSAGE-TEXT        TO MSGO
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-FORMAT-LINE.
      *================================================================*
           ADD 1                           TO CA-LIST-COUNT
           MOVE CHSWE-SWITCH-ID (WS-SUB)   TO CA-LIST-ID (WS-SUB)
                                              SL-SWITCH-ID
           IF CHSWE-DIRECTION (WS-SUB) = 'I'
           AND CHSWE-STATUS (WS-SUB) = 'RQ'
               MOVE 'Y'            TO CA-LIST-CANCELLABLE (WS-SUB)
           ELSE
               MOVE 'N'            TO CA-LIST-CANCELLABLE (WS-SUB)
           END-IF
           MOVE CHSWE-DIRECTION (WS-SUB)   TO SL-DIR
           MOVE CHSWE-STATUS (WS-SUB)      TO SL-STATUS
           MOVE CHSWE-OLD-SORT-CODE (WS-SUB) TO SL-OLD-SC
           MOVE CHSWE-OLD-ACCT-NO (WS-SUB) TO SL-OLD-ACCT
           MOVE SPACES                     TO SL-SW-DATE
           STRING CHSWE-SWITCH-DATE (WS-SUB) (7:2) '/'
                  CHSWE-SWITCH-DATE (WS-SUB) (5:2) '/'
                  CHSWE-SWITCH-DATE (WS-SUB) (1:4)
                  DELIMITED BY SIZE
                  INTO SL-SW-DATE
           END-STRING
           MOVE CHSWE-SO-COUNT (WS-SUB)    TO SL-SOS
           MOVE CHSWE-DD-COUNT (WS-SUB)    TO SL-DDS
           MOVE CHSWE-REJECT-RSN (WS-SUB)  TO SL-RSN
           EVALUATE WS-SUB
               WHEN 1   MOVE WS-SW-LINE    TO SWL1O
               WHEN 2   MOVE WS-SW-LINE    TO SWL2O
               WHEN 3   MOVE WS-SW-LINE    TO SWL3O
               WHEN 4   MOVE WS-SW-LINE    TO SWL4O
               WHEN 5   MOVE WS-SW-LINE    TO SWL5O
               WHEN 6   MOVE WS-SW-LINE    TO SWL6O
               WHEN 7   MOVE WS-SW-LINE    TO SWL7O
               WHEN 8   MOVE WS-SW-LINE    TO SWL8O
               WHEN 9   MOVE WS-SW-LINE    TO SWL9O
               WHEN 10  MOVE WS-SW-LINE    TO SWL10O
           END-EVALUATE
           .
      *================================================================*
       3500-FORMAT-HEADER.
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
               MOVE 'ACCOUNT NOT FOUND'    TO CUSTNMO
           ELSE
               MOVE ACM-PROD-CD            TO PRODCDO
               MOVE ACM-CIF-NO             TO CA-CIF-NO CIF-NO
               EXEC CICS READ
                         FILE('CIFMAST')
                         INTO(CIF-CUSTOMER-RECORD)
                         LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                         RIDFLD(CIF-KEY)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(NORMAL)
                   STRING CIF-TITLE     DELIMITED BY SPACE
                          ' '           DELIMITED BY SIZE
                          CIF-FIRST-NM  DELIMITED BY SPACE
                          ' '           DELIMITED BY SIZE
                          CIF-LAST-NM   DELIMITED BY '  '
                          INTO CUSTNMO
                   END-STRING
               END-IF
           END-IF
           .
      *================================================================*
       5000-CONFIRM.
      *================================================================*
           MOVE LOW-VALUES                 TO CSM01O
           EVALUATE TRUE
               WHEN CA-REQUEST-PENDING
                   PERFORM 7000-INIT-SERVICE
                   SET CHSW-REQUEST        TO TRUE
                   MOVE CA-SORT-CODE       TO CHSW-NEW-SORT-CODE
                   MOVE CA-ACCT-NO         TO CHSW-NEW-ACCT-NO
                   MOVE CA-OLD-SORT-CODE   TO CHSW-OLD-SORT-CODE
                   MOVE CA-OLD-ACCT-NO     TO CHSW-OLD-ACCT-NO
                   MOVE CA-SWITCH-DATE     TO CHSW-SWITCH-DATE
                   PERFORM 7100-LINK-SERVICE
               WHEN CA-CANCEL-PENDING
                   PERFORM 7000-INIT-SERVICE
                   SET CHSW-CANCEL         TO TRUE
                   MOVE CA-CANCEL-ID       TO CHSW-SWITCH-ID
                   PERFORM 7100-LINK-SERVICE
               WHEN OTHER
                   PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
                   MOVE 'NOTHING TO CONFIRM' TO MSGO
                   PERFORM 8100-SEND-MAP
                   GO TO 5000-EXIT
           END-EVALUATE
           MOVE SPACE                      TO CA-PENDING
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE CH-MESSAGE-TEXT        TO WS-END-MESSAGE
           END-IF
           PERFORM 3000-LIST-SWITCHES THRU 3000-EXIT
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE WS-END-MESSAGE         TO MSGO
           END-IF
           PERFORM 8100-SEND-MAP
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       7000-INIT-SERVICE.
      *================================================================*
           MOVE SPACES                     TO CH-COMMAREA
           SET CH-EYECATCHER-OK            TO TRUE
           MOVE '01'                       TO CH-VERSION
           MOVE 'SWITCHRQ'                 TO CH-SERVICE-CD
           MOVE 'BRANCH'                   TO CH-CHANNEL-CD
           MOVE EIBTRMID                   TO CH-USER-ID
           STRING 'CS01' EIBTRMID EIBTASKN
                  DELIMITED BY SIZE
                  INTO CH-REQUEST-ID
           END-STRING
           MOVE '0000'                     TO CH-RETURN-CD
           .
      *================================================================*
       7100-LINK-SERVICE.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM('CSSVC01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE WS-RESP                TO WS-RESP-DISPLAY
               STRING 'SWITCH SERVICE UNAVAILABLE - EIBRESP '
                      WS-RESP-DISPLAY
                      DELIMITED BY SIZE
                      INTO MSGO
               END-STRING
           END-IF
           .
      *================================================================*
       8000-SEND-DATAONLY.
      *================================================================*
           EXEC CICS SEND
                     MAP('CSM01')
                     MAPSET('CSMAPS')
                     FROM(CSM01O)
                     DATAONLY
                     CURSOR
           END-EXEC
           .
      *================================================================*
       8100-SEND-MAP.
      *================================================================*
           STRING WS-TODAY (7:2) '/' WS-TODAY (5:2) '/'
                  WS-TODAY (1:4)
                  DELIMITED BY SIZE
                  INTO DATEO
           END-STRING
           MOVE EIBTRMID                   TO TIMEO
           EXEC CICS SEND
                     MAP('CSM01')
                     MAPSET('CSMAPS')
                     FROM(CSM01O)
                     ERASE
                     CURSOR
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
           MOVE 'CS01'                     TO XC-FROM-TRAN
           MOVE CA-SORT-CODE               TO XC-SORT-CODE
           MOVE CA-ACCT-NO                 TO XC-ACCT-NO
           MOVE CA-CIF-NO                  TO XC-CIF-NO
           EXEC CICS XCTL
                     PROGRAM('CFSCRN2')
                     COMMAREA(WS-CF02-COMMAREA)
                     LENGTH(LENGTH OF WS-CF02-COMMAREA)
           END-EXEC
           .
