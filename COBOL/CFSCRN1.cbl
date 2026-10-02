       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFSCRN1.
       AUTHOR.        P D KERSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1996-08-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFSCRN1                                             *
      * TITLE    : BRANCH CUSTOMER ENQUIRY SCREEN                      *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION CF01       *
      * MAPSET   : CFMAPS  MAP CFM01                                   *
      * FILES    : CIFMAST (READ), ACCTCIF (BROWSE)                    *
      *----------------------------------------------------------------*
      * BRANCH STAFF KEY A CIF NUMBER; THE SCREEN SHOWS THE CUSTOMER   *
      * DETAILS AND THE FIRST 6 ACCOUNTS. ACCESS TO CF01 IS CONTROLLED *
      * BY RACF PROFILE CICSPRD1.CF01 (GROUP BRSTAFF).                 *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1996-08-02 PDK  ORIGINAL VERSION                               *
      * 1998-12-07 MLW  Y2K - DOB DISPLAYED DD/MM/CCYY                 *
      * 2003-05-09 SAT  KYC STATUS AND RISK RATING                     *
      * 2005-09-12 AOS  ACCOUNTS FROM NEW CIF PATH (ACCTCIF)           *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFSCRN1 '.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-BROWSE-KEY               PIC X(12).
           05  WS-ACCT-COUNT               PIC 9(02) VALUE ZERO.
           05  WS-END-BROWSE-IND           PIC X(01) VALUE 'N'.
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-DOB.
               10  WS-DOB-CCYY             PIC X(04).
               10  WS-DOB-MM               PIC X(02).
               10  WS-DOB-DD               PIC X(02).
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'CF01 - CUSTOMER ENQUIRY ENDED'.
      *
       01  WS-COMMAREA.
           05  WS-CA-LAST-CIF              PIC X(12).
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
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  AL-LEDGER-BAL               PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  AL-AVAIL-BAL                PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(03) VALUE SPACES.
      *
           COPY CFMAPS.
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
       01  DFHCOMMAREA                     PIC X(12).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           IF EIBCALEN = ZERO
               PERFORM 1000-SEND-EMPTY-MAP
           ELSE
               MOVE DFHCOMMAREA            TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       PERFORM 9000-END-SESSION
                   WHEN DFHPF12
                   WHEN DFHCLEAR
                       PERFORM 1000-SEND-EMPTY-MAP
                   WHEN DFHENTER
                       PERFORM 2000-PROCESS-ENQUIRY
                   WHEN OTHER
                       MOVE LOW-VALUES     TO CFM01O
                       MOVE 'INVALID KEY - USE ENTER, PF3 OR PF12'
                                           TO MSGO
                       PERFORM 8000-SEND-MAP-DATAONLY
               END-EVALUATE
           END-IF

           EXEC CICS RETURN
                     TRANSID('CF01')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       1000-SEND-EMPTY-MAP.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           MOVE LOW-VALUES                 TO CFM01O
           PERFORM 7000-SET-DATE-TIME
           MOVE 'KEY A CIF NUMBER AND PRESS ENTER'
                                           TO MSGO

           EXEC CICS SEND
                     MAP('CFM01')
                     MAPSET('CFMAPS')
                     FROM(CFM01O)
                     ERASE
                     FREEKB
           END-EXEC
           .
      *================================================================*
       2000-PROCESS-ENQUIRY.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('CFM01')
                     MAPSET('CFMAPS')
                     INTO(CFM01I)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
           OR CIFNOL = ZERO
           OR CIFNOI = SPACES
               MOVE LOW-VALUES             TO CFM01O
               MOVE 'CIF NUMBER IS REQUIRED'
                                           TO MSGO
               MOVE -1                     TO CIFNOL
               PERFORM 8000-SEND-MAP-DATAONLY
           ELSE
               MOVE CIFNOI                 TO WS-BROWSE-KEY
               MOVE LOW-VALUES             TO CFM01O
               PERFORM 2100-READ-CUSTOMER
           END-IF
           .
      *================================================================*
       2100-READ-CUSTOMER.
      *================================================================*
           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(WS-BROWSE-KEY)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE WS-BROWSE-KEY      TO WS-CA-LAST-CIF
                   PERFORM 2200-FORMAT-CUSTOMER
                   PERFORM 2300-LIST-ACCOUNTS
                   PERFORM 7000-SET-DATE-TIME
                   MOVE WS-BROWSE-KEY      TO CIFNOO
                   EXEC CICS SEND
                             MAP('CFM01')
                             MAPSET('CFMAPS')
                             FROM(CFM01O)
                             ERASE
                             FREEKB
                   END-EXEC
               WHEN DFHRESP(NOTFND)
                   MOVE 'CUSTOMER NOT FOUND'
                                           TO MSGO
                   MOVE -1                 TO CIFNOL
                   PERFORM 8000-SEND-MAP-DATAONLY
               WHEN OTHER
                   MOVE WS-RESP            TO WS-RESP-DISPLAY
                   STRING 'CIF FILE UNAVAILABLE - EIBRESP '
                          WS-RESP-DISPLAY
                          ' - CALL SERVICE DESK X4400'
                          DELIMITED BY SIZE
                          INTO MSGO
                   END-STRING
                   PERFORM 8000-SEND-MAP-DATAONLY
           END-EVALUATE
           .
      *================================================================*
       2200-FORMAT-CUSTOMER.
      *================================================================*
           STRING CIF-TITLE     DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-FIRST-NM  DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-MIDDLE-NM DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-LAST-NM   DELIMITED BY SPACE
                  INTO CUSTNMO
           END-STRING

           MOVE CIF-DOB                    TO WS-DOB
           STRING WS-DOB-DD '/' WS-DOB-MM '/' WS-DOB-CCYY
                  DELIMITED BY SIZE
                  INTO DOBO
           END-STRING

           MOVE CIF-NAT-INS-NO             TO NINOO
           MOVE CIF-ADDR-LINE1             TO ADDR1O
           MOVE CIF-ADDR-LINE2             TO ADDR2O
           MOVE CIF-TOWN                   TO TOWNO
           MOVE CIF-POST-CD                TO PCODEO
           MOVE CIF-HOME-TEL               TO HTELO
           MOVE CIF-MOBILE-TEL             TO MTELO
           MOVE CIF-EMAIL-ADDR             TO EMAILO
           MOVE CIF-SEGMENT-CD             TO SEGMTO

           EVALUATE TRUE
               WHEN CIF-ACTIVE    MOVE 'ACTIVE'    TO CSTATO
               WHEN CIF-BLOCKED   MOVE 'BLOCKED'   TO CSTATO
                                  MOVE DFHRED      TO CSTATA
               WHEN CIF-CLOSED    MOVE 'CLOSED'    TO CSTATO
               WHEN CIF-DECEASED  MOVE 'DECEASED'  TO CSTATO
                                  MOVE DFHRED      TO CSTATA
               WHEN OTHER         MOVE CIF-REC-STATUS TO CSTATO
           END-EVALUATE

           EVALUATE TRUE
               WHEN CIF-KYC-VERIFIED  MOVE 'VERIFIED'  TO KYCO
               WHEN CIF-KYC-PENDING   MOVE 'PENDING'   TO KYCO
               WHEN CIF-KYC-FAILED    MOVE 'FAILED'    TO KYCO
                                      MOVE DFHRED      TO KYCA
               WHEN OTHER             MOVE 'UNKNOWN'   TO KYCO
           END-EVALUATE

           EVALUATE TRUE
               WHEN CIF-RISK-LOW      MOVE 'LOW'       TO RISKO
               WHEN CIF-RISK-MEDIUM   MOVE 'MEDIUM'    TO RISKO
               WHEN CIF-RISK-HIGH     MOVE 'HIGH'      TO RISKO
                                      MOVE DFHRED      TO RISKA
               WHEN OTHER             MOVE '?'         TO RISKO
           END-EVALUATE
           .
      *================================================================*
       2300-LIST-ACCOUNTS.
      *================================================================*
           MOVE ZERO                       TO WS-ACCT-COUNT
           MOVE 'N'                        TO WS-END-BROWSE-IND

           EXEC CICS STARTBR
                     FILE('ACCTCIF')
                     RIDFLD(WS-BROWSE-KEY)
                     EQUAL
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NO ACCOUNTS HELD'     TO MSGO
           ELSE
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
                   AND ACM-CIF-NO = WS-CA-LAST-CIF
                       PERFORM 2310-ADD-ACCOUNT-LINE
                   ELSE
                       SET WS-END-BROWSE   TO TRUE
                   END-IF
               END-PERFORM

               EXEC CICS ENDBR
                         FILE('ACCTCIF')
                         RESP(WS-RESP)
               END-EXEC

               IF WS-ACCT-COUNT > 6
                   MOVE 'MORE THAN 6 ACCOUNTS - SEE CF02 FOR FULL LIST'
                                           TO MSGO
               ELSE
                   MOVE 'ENQUIRY COMPLETE' TO MSGO
               END-IF
           END-IF
           .
      *================================================================*
       2310-ADD-ACCOUNT-LINE.
      *================================================================*
           ADD 1                           TO WS-ACCT-COUNT
           IF WS-ACCT-COUNT > 6
               SET WS-END-BROWSE           TO TRUE
           ELSE
               STRING ACM-SORT-CODE (1:2) '-'
                      ACM-SORT-CODE (3:2) '-'
                      ACM-SORT-CODE (5:2)
                      DELIMITED BY SIZE
                      INTO AL-SORT-CODE
               END-STRING
               MOVE ACM-ACCT-NO            TO AL-ACCT-NO
               MOVE ACM-ACCT-TYPE-CD       TO AL-ACCT-TYPE
               MOVE ACM-PROD-CD            TO AL-PROD-CD
               MOVE ACM-ACCT-STATUS        TO AL-STATUS
               MOVE ACM-LEDGER-BAL         TO AL-LEDGER-BAL
               MOVE ACM-AVAIL-BAL          TO AL-AVAIL-BAL
               EVALUATE WS-ACCT-COUNT
                   WHEN 1  MOVE WS-ACCOUNT-LINE TO ACCL1O
                   WHEN 2  MOVE WS-ACCOUNT-LINE TO ACCL2O
                   WHEN 3  MOVE WS-ACCOUNT-LINE TO ACCL3O
                   WHEN 4  MOVE WS-ACCOUNT-LINE TO ACCL4O
                   WHEN 5  MOVE WS-ACCOUNT-LINE TO ACCL5O
                   WHEN 6  MOVE WS-ACCOUNT-LINE TO ACCL6O
               END-EVALUATE
           END-IF
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
       8000-SEND-MAP-DATAONLY.
      *================================================================*
           EXEC CICS SEND
                     MAP('CFM01')
                     MAPSET('CFMAPS')
                     FROM(CFM01O)
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
