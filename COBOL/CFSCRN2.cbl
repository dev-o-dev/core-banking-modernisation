       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFSCRN2.
       AUTHOR.        S A TAYLOR.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-04-12.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFSCRN2                                             *
      * TITLE    : BRANCH CUSTOMER ACCOUNT LIST                        *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION CF02       *
      * MAPSET   : CFMAP2  MAP CFM02                                   *
      * FILES    : CIFMAST (READ), ACCTCIF (BROWSE)                    *
      *----------------------------------------------------------------*
      * LISTS ALL ACCOUNTS FOR A CUSTOMER, 12 PER PAGE (CF01 SHOWS     *
      * ONLY THE FIRST 6 AND SENDS THE USER HERE). THE USER CAN SELECT *
      * ONE ACCOUNT WITH:                                              *
      *   H  TRANSACTION HISTORY  - XCTL TO DDSCRN3 (DD03)             *
      *   O  STANDING ORDERS      - XCTL TO SOSCRN1 (SO01)             *
      *   D  DETAIL               - SHOWN IN THE MESSAGE LINE (FULL    *
      *                             DETAIL SCREEN NOT BUILT - PIR      *
      *                             2010/090)                          *
      *                                                                *
      * THE ACCOUNT KEYS FOR ALL PAGES ARE HELD IN THE COMMAREA (UP TO *
      * 96 ACCOUNTS - 8 PAGES). CUSTOMERS WITH MORE ARE TOLD TO USE    *
      * THE ACCOUNT SEARCH IN THE TELLER SYSTEM.                       *
      *                                                                *
      * ACCESS: RACF PROFILE CICSPRD1.CF02 (GROUP BRSTAFF).            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-04-12 SAT  ORIGINAL VERSION                               *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFSCRN2 '.
      *
       01  WS-CONSTANTS.
           05  WS-LINES-PER-PAGE           PIC 9(02) VALUE 12.
           05  WS-MAX-ACCOUNTS             PIC 9(02) VALUE 96.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-BROWSE-KEY               PIC X(12).
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-LINE                     PIC S9(04) COMP.
           05  WS-FIRST                    PIC S9(04) COMP.
           05  WS-SEL-COUNT                PIC S9(04) COMP.
           05  WS-SEL-LINE                 PIC S9(04) COMP.
           05  WS-SEL-CODE                 PIC X(01).
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'CF02 - ACCOUNT LIST ENDED'.
           05  WS-OD-EDIT                  PIC Z,ZZZ,ZZ9.
      *
      *---- COMMAREA --------------------------------------------------*
       01  WS-COMMAREA.
           05  CA-CIF-NO                   PIC X(12).
           05  CA-ACCT-COUNT               PIC 9(02).
           05  CA-PAGE                     PIC 9(02).
           05  CA-PAGES                    PIC 9(02).
           05  CA-TRUNCATED                PIC X(01).
           05  CA-ACCT-KEY                 OCCURS 96 TIMES
                                           PIC X(14).
      *
      *---- COMMAREA PASSED TO DD03 / SO01 ON XCTL --------------------*
       01  WS-XCTL-COMMAREA.
           05  XC-FROM-TRAN                PIC X(04).
           05  XC-SORT-CODE                PIC X(06).
           05  XC-ACCT-NO                  PIC X(08).
           05  XC-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(20).
      *
       01  WS-ACCOUNT-LINE.
           05  AL-SORT-CODE                PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  AL-ACCT-NO                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  AL-ACCT-TYPE                PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  AL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  AL-STATUS                   PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  AL-LEDGER-BAL               PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  AL-AVAIL-BAL                PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  AL-OD-LIMIT                 PIC ZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
      *
           COPY CFMAP2.
      *
           COPY CFCUSREC.
      *
           COPY DDACCREC.
      *
           COPY DFHAID.
      *
           COPY DFHBMSCA.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(1363).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
      *    CF02 CAN BE STARTED FROM CF01 WITH THE CIF IN A 12-BYTE
      *    COMMAREA, OR FROM THE MENU WITH NONE.
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                   PERFORM 1000-SEND-EMPTY-MAP
               WHEN EIBCALEN = 12
                   MOVE SPACES             TO WS-COMMAREA
                   MOVE DFHCOMMAREA (1:12) TO CA-CIF-NO
                   PERFORM 2100-LOAD-CUSTOMER THRU 2100-EXIT
               WHEN OTHER
                   MOVE DFHCOMMAREA        TO WS-COMMAREA
                   EVALUATE EIBAID
                       WHEN DFHPF3
                           PERFORM 9000-END-SESSION
                       WHEN DFHPF12
                           EXEC CICS XCTL
                                     PROGRAM('CFSCRN1')
                                     RESP(WS-RESP)
                           END-EXEC
                           PERFORM 1000-SEND-EMPTY-MAP
                       WHEN DFHCLEAR
                           PERFORM 1000-SEND-EMPTY-MAP
                       WHEN DFHPF7
                           PERFORM 3000-PAGE-BACK
                       WHEN DFHPF8
                           PERFORM 3100-PAGE-FORWARD
                       WHEN DFHENTER
                           PERFORM 2000-PROCESS-ENTER THRU 2000-EXIT
                       WHEN OTHER
                           PERFORM 2900-RESHOW-PAGE
                           MOVE 'INVALID KEY' TO MSGO
                           PERFORM 8000-SEND-DATAONLY
                   END-EVALUATE
           END-EVALUATE

           EXEC CICS RETURN
                     TRANSID('CF02')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       1000-SEND-EMPTY-MAP.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           MOVE ZERO                       TO CA-ACCT-COUNT CA-PAGE
                                              CA-PAGES
           MOVE LOW-VALUES                 TO CFM02O
           PERFORM 7000-SET-DATE-TIME
           MOVE 'KEY A CIF NUMBER AND PRESS ENTER' TO MSGO
           EXEC CICS SEND
                     MAP('CFM02')
                     MAPSET('CFMAP2')
                     FROM(CFM02O)
                     ERASE
                     FREEKB
           END-EXEC
           .
      *================================================================*
       2000-PROCESS-ENTER.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('CFM02')
                     MAPSET('CFMAP2')
                     INTO(CFM02I)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               PERFORM 2900-RESHOW-PAGE
               MOVE 'NOTHING ENTERED'      TO MSGO
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

      *    A NEW CIF NUMBER KEYED - START AGAIN FOR THAT CUSTOMER.
           IF CIFNOL > ZERO
           AND CIFNOI NOT = CA-CIF-NO
               MOVE CIFNOI                 TO CA-CIF-NO
               PERFORM 2100-LOAD-CUSTOMER THRU 2100-EXIT
               GO TO 2000-EXIT
           END-IF

           PERFORM 2500-CHECK-SELECTION
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-LOAD-CUSTOMER.
      *================================================================*
           MOVE LOW-VALUES                 TO CFM02O
           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(CA-CIF-NO)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE 'CUSTOMER NOT FOUND' TO MSGO
                   MOVE -1                 TO CIFNOL
                   MOVE ZERO               TO CA-ACCT-COUNT
                   PERFORM 8000-SEND-DATAONLY
                   GO TO 2100-EXIT
               WHEN OTHER
                   MOVE WS-RESP            TO WS-RESP-DISPLAY
                   STRING 'CIF FILE UNAVAILABLE - EIBRESP '
                          WS-RESP-DISPLAY
                          DELIMITED BY SIZE
                          INTO MSGO
                   END-STRING
                   PERFORM 8000-SEND-DATAONLY
                   GO TO 2100-EXIT
           END-EVALUATE

           PERFORM 2200-COLLECT-ACCOUNTS THRU 2200-EXIT
           MOVE 1                          TO CA-PAGE
           COMPUTE CA-PAGES =
                   (CA-ACCT-COUNT + WS-LINES-PER-PAGE - 1)
                   / WS-LINES-PER-PAGE
           IF CA-PAGES = ZERO
               MOVE 1                      TO CA-PAGES
           END-IF
           PERFORM 2800-BUILD-PAGE
           IF CA-ACCT-COUNT = ZERO
               MOVE 'NO ACCOUNTS HELD'     TO MSGO
           ELSE
               IF CA-TRUNCATED = 'Y'
                   MOVE 'OVER 96 ACCOUNTS - USE TELLER ACCOUNT SEARCH'
                                           TO MSGO
               ELSE
                   MOVE 'SELECT AN ACCOUNT OR PAGE WITH PF7/PF8'
                                           TO MSGO
               END-IF
           END-IF
           PERFORM 8100-SEND-MAP
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-COLLECT-ACCOUNTS.
      *================================================================*
           MOVE ZERO                       TO CA-ACCT-COUNT
           MOVE 'N'                        TO CA-TRUNCATED
                                              WS-END-BROWSE-IND
           MOVE CA-CIF-NO                  TO WS-BROWSE-KEY
           EXEC CICS STARTBR
                     FILE('ACCTCIF')
                     RIDFLD(WS-BROWSE-KEY)
                     EQUAL
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 2200-EXIT
           END-IF
           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('ACCTCIF')
                         INTO(ACM-ACCOUNT-RECORD)
                         LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                         RIDFLD(WS-BROWSE-KEY)
                         RESP(WS-RESP)
               END-EXEC
               IF (WS-RESP = DFHRESP(NORMAL)
                   OR WS-RESP = DFHRESP(DUPKEY))
               AND ACM-CIF-NO = CA-CIF-NO
                   IF CA-ACCT-COUNT >= WS-MAX-ACCOUNTS
                       MOVE 'Y'            TO CA-TRUNCATED
                       SET WS-END-BROWSE   TO TRUE
                   ELSE
                       ADD 1               TO CA-ACCT-COUNT
                       MOVE ACM-KEY TO CA-ACCT-KEY (CA-ACCT-COUNT)
                   END-IF
               ELSE
                   SET WS-END-BROWSE       TO TRUE
               END-IF
           END-PERFORM
           EXEC CICS ENDBR
                     FILE('ACCTCIF')
                     RESP(WS-RESP)
           END-EXEC
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2500-CHECK-SELECTION.
      *================================================================*
           MOVE ZERO                       TO WS-SEL-COUNT
                                              WS-SEL-LINE
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-LINES-PER-PAGE
               PERFORM 2510-GET-SEL-FIELD
               IF WS-SEL-CODE NOT = SPACE AND LOW-VALUE
                   ADD 1                   TO WS-SEL-COUNT
                   MOVE WS-LINE            TO WS-SEL-LINE
               END-IF
           END-PERFORM

           EVALUATE TRUE
               WHEN WS-SEL-COUNT = ZERO
                   PERFORM 2900-RESHOW-PAGE
                   MOVE 'SELECT AN ACCOUNT OR PAGE WITH PF7/PF8'
                                           TO MSGO
                   PERFORM 8000-SEND-DATAONLY
               WHEN WS-SEL-COUNT > 1
                   PERFORM 2900-RESHOW-PAGE
                   MOVE 'SELECT ONE ACCOUNT ONLY' TO MSGO
                   PERFORM 8000-SEND-DATAONLY
               WHEN OTHER
                   PERFORM 2600-ACTION-SELECTION THRU 2600-EXIT
           END-EVALUATE
           .
      *================================================================*
       2510-GET-SEL-FIELD.
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
       2600-ACTION-SELECTION.
      *================================================================*
           MOVE WS-SEL-LINE                TO WS-LINE
           PERFORM 2510-GET-SEL-FIELD
           COMPUTE WS-SUB = (CA-PAGE - 1) * WS-LINES-PER-PAGE
                          + WS-SEL-LINE
           IF WS-SUB > CA-ACCT-COUNT
               PERFORM 2900-RESHOW-PAGE
               MOVE 'NO ACCOUNT ON THAT LINE' TO MSGO
               PERFORM 8000-SEND-DATAONLY
               GO TO 2600-EXIT
           END-IF

           MOVE SPACES                     TO WS-XCTL-COMMAREA
           MOVE 'CF02'                     TO XC-FROM-TRAN
           MOVE CA-ACCT-KEY (WS-SUB) (1:6) TO XC-SORT-CODE
           MOVE CA-ACCT-KEY (WS-SUB) (7:8) TO XC-ACCT-NO
           MOVE CA-CIF-NO                  TO XC-CIF-NO

           EVALUATE FUNCTION UPPER-CASE (WS-SEL-CODE)
               WHEN 'H'
                   EXEC CICS XCTL
                             PROGRAM('DDSCRN3')
                             COMMAREA(WS-XCTL-COMMAREA)
                             LENGTH(LENGTH OF WS-XCTL-COMMAREA)
                             RESP(WS-RESP)
                   END-EXEC
                   PERFORM 2900-RESHOW-PAGE
                   MOVE 'TRANSACTION HISTORY UNAVAILABLE' TO MSGO
                   PERFORM 8000-SEND-DATAONLY
               WHEN 'O'
                   EXEC CICS XCTL
                             PROGRAM('SOSCRN1')
                             COMMAREA(WS-XCTL-COMMAREA)
                             LENGTH(LENGTH OF WS-XCTL-COMMAREA)
                             RESP(WS-RESP)
                   END-EXEC
                   PERFORM 2900-RESHOW-PAGE
                   MOVE 'STANDING ORDERS UNAVAILABLE' TO MSGO
                   PERFORM 8000-SEND-DATAONLY
               WHEN 'D'
                   PERFORM 2700-SHOW-DETAIL
               WHEN OTHER
                   PERFORM 2900-RESHOW-PAGE
                   MOVE 'SELECTION MUST BE D, H OR O' TO MSGO
                   PERFORM 8000-SEND-DATAONLY
           END-EVALUATE
           .
       2600-EXIT.
           EXIT.
      *================================================================*
       2700-SHOW-DETAIL.
      *================================================================*
           MOVE CA-ACCT-KEY (WS-SUB)       TO WS-ACCT-KEY
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           PERFORM 2900-RESHOW-PAGE
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE ACM-OD-LIMIT           TO WS-OD-EDIT
               STRING 'OPENED ' ACM-OPEN-DATE
                      ' LAST TXN ' ACM-LAST-TXN-DATE
                      ' OD ' WS-OD-EDIT
                      ' IBAN ' ACM-IBAN (1:22)
                      DELIMITED BY SIZE
                      INTO MSGO
               END-STRING
           ELSE
               MOVE 'ACCOUNT DETAIL UNAVAILABLE' TO MSGO
           END-IF
           PERFORM 8000-SEND-DATAONLY
           .
      *================================================================*
       2800-BUILD-PAGE.
      *================================================================*
           MOVE CIF-NO                     TO CIFNOO
           MOVE SPACES                     TO CUSTNMO
           STRING CIF-TITLE     DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-FIRST-NM  DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-LAST-NM   DELIMITED BY '  '
                  INTO CUSTNMO
           END-STRING
           EVALUATE TRUE
               WHEN CIF-ACTIVE    MOVE 'ACTIVE'    TO CSTATO
               WHEN CIF-BLOCKED   MOVE 'BLOCKED'   TO CSTATO
                                  MOVE DFHRED      TO CSTATA
               WHEN CIF-CLOSED    MOVE 'CLOSED'    TO CSTATO
               WHEN CIF-DECEASED  MOVE 'DECEASED'  TO CSTATO
                                  MOVE DFHRED      TO CSTATA
               WHEN OTHER         MOVE CIF-REC-STATUS TO CSTATO
           END-EVALUATE
           MOVE CA-ACCT-COUNT              TO ACCTCTO
           MOVE CA-PAGE                    TO PAGENOO
           MOVE CA-PAGES                   TO PAGESO

           COMPUTE WS-FIRST = (CA-PAGE - 1) * WS-LINES-PER-PAGE
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-LINES-PER-PAGE
               COMPUTE WS-SUB = WS-FIRST + WS-LINE
               MOVE SPACES                 TO WS-ACCOUNT-LINE
               IF WS-SUB <= CA-ACCT-COUNT
                   PERFORM 2810-FORMAT-ACCOUNT
               END-IF
               PERFORM 2820-PLACE-LINE
           END-PERFORM
           .
      *================================================================*
       2810-FORMAT-ACCOUNT.
      *================================================================*
           MOVE CA-ACCT-KEY (WS-SUB)       TO WS-ACCT-KEY
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           STRING WS-AK-SORT-CODE (1:2) '-' WS-AK-SORT-CODE (3:2) '-'
                  WS-AK-SORT-CODE (5:2)
                  DELIMITED BY SIZE
                  INTO AL-SORT-CODE
           END-STRING
           MOVE WS-AK-ACCT-NO              TO AL-ACCT-NO
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE ACM-ACCT-TYPE-CD       TO AL-ACCT-TYPE
               MOVE ACM-PROD-CD            TO AL-PROD-CD
               MOVE ACM-ACCT-STATUS        TO AL-STATUS
               MOVE ACM-LEDGER-BAL         TO AL-LEDGER-BAL
               MOVE ACM-AVAIL-BAL          TO AL-AVAIL-BAL
               MOVE ACM-OD-LIMIT           TO AL-OD-LIMIT
           ELSE
      *        ACCTMST IS CLOSED DURING THE BATCH WINDOW.
               MOVE 'BALANCE UNAVAILABLE'  TO AL-PROD-CD
           END-IF
           .
      *================================================================*
       2820-PLACE-LINE.
      *================================================================*
           EVALUATE WS-LINE
               WHEN 1   MOVE WS-ACCOUNT-LINE TO ACL1O
               WHEN 2   MOVE WS-ACCOUNT-LINE TO ACL2O
               WHEN 3   MOVE WS-ACCOUNT-LINE TO ACL3O
               WHEN 4   MOVE WS-ACCOUNT-LINE TO ACL4O
               WHEN 5   MOVE WS-ACCOUNT-LINE TO ACL5O
               WHEN 6   MOVE WS-ACCOUNT-LINE TO ACL6O
               WHEN 7   MOVE WS-ACCOUNT-LINE TO ACL7O
               WHEN 8   MOVE WS-ACCOUNT-LINE TO ACL8O
               WHEN 9   MOVE WS-ACCOUNT-LINE TO ACL9O
               WHEN 10  MOVE WS-ACCOUNT-LINE TO ACL10O
               WHEN 11  MOVE WS-ACCOUNT-LINE TO ACL11O
               WHEN 12  MOVE WS-ACCOUNT-LINE TO ACL12O
           END-EVALUATE
           .
      *================================================================*
       2900-RESHOW-PAGE.
      *================================================================*
      *    REBUILD THE CURRENT PAGE (THE CUSTOMER IS RE-READ - CIFMAST
      *    IS IN THE LSR POOL SO THIS IS CHEAP).
           MOVE LOW-VALUES                 TO CFM02O
           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(CA-CIF-NO)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM 2800-BUILD-PAGE
           END-IF
           .
      *================================================================*
       3000-PAGE-BACK.
      *================================================================*
           IF CA-PAGE > 1
               SUBTRACT 1                  FROM CA-PAGE
               PERFORM 2900-RESHOW-PAGE
               MOVE SPACES                 TO MSGO
           ELSE
               PERFORM 2900-RESHOW-PAGE
               MOVE 'ALREADY ON THE FIRST PAGE' TO MSGO
           END-IF
           PERFORM 8100-SEND-MAP
           .
      *================================================================*
       3100-PAGE-FORWARD.
      *================================================================*
           IF CA-PAGE < CA-PAGES
               ADD 1                       TO CA-PAGE
               PERFORM 2900-RESHOW-PAGE
               MOVE SPACES                 TO MSGO
           ELSE
               PERFORM 2900-RESHOW-PAGE
               MOVE 'ALREADY ON THE LAST PAGE' TO MSGO
           END-IF
           PERFORM 8100-SEND-MAP
           .
      *================================================================*
       7000-SET-DATE-TIME.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     DDMMYYYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC
           .
      *================================================================*
       8000-SEND-DATAONLY.
      *================================================================*
           PERFORM 7000-SET-DATE-TIME
           EXEC CICS SEND
                     MAP('CFM02')
                     MAPSET('CFMAP2')
                     FROM(CFM02O)
                     DATAONLY
                     CURSOR
                     FREEKB
           END-EXEC
           .
      *================================================================*
       8100-SEND-MAP.
      *================================================================*
           PERFORM 7000-SET-DATE-TIME
           EXEC CICS SEND
                     MAP('CFM02')
                     MAPSET('CFMAP2')
                     FROM(CFM02O)
                     ERASE
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
