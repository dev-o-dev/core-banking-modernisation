       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDLST01.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2000-06-23.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDLST01                                             *
      * TITLE    : ACCOUNTS FOR CUSTOMER - CHANNEL SERVICE ACCTLIST    *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : ACCTCIF (PATH OVER ACCOUNT MASTER BY CIF NUMBER,    *
      *            NON-UNIQUE ALTERNATE INDEX)                         *
      * COMMAREA : CHCOMMAR / CH-ACCTLIST-AREA                         *
      *----------------------------------------------------------------*
      * BROWSES THE CIF PATH AND RETURNS UP TO 20 ACCOUNTS. CLOSED     *
      * ACCOUNTS ARE EXCLUDED UNLESS CLOSED IN THE LAST 90 DAYS (THE   *
      * CHANNEL SHOWS RECENTLY CLOSED ACCOUNTS FOR STATEMENTS).        *
      *                                                                *
      * NB. READNEXT ON A NON-UNIQUE AIX PATH RETURNS DUPKEY FOR EVERY *
      * RECORD EXCEPT THE LAST WITH THAT KEY. DUPKEY IS NORMAL HERE.   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2000-06-23 DKP  ORIGINAL VERSION (MAX 10 ACCOUNTS)             *
      * 2002-09-16 DKP  DUPKEY TREATED AS NORMAL - ONLY FIRST ACCOUNT  *
      *                 WAS BEING RETURNED (PIR 2002/118)              *
      * 2005-09-12 AOS  REBUILT OVER NEW AIX (CIF AT OFFSET 14)        *
      * 2007-01-22 JNB  MAX RAISED TO 20; RECENTLY CLOSED ACCOUNTS     *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDLST01 '.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-BROWSE-KEY               PIC X(12).
           05  WS-MAX-ACCOUNTS             PIC 9(02) VALUE 20.
           05  WS-SUB                      PIC 9(02) VALUE ZERO.
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-CLOSED-CUTOFF            PIC 9(08).
           05  WS-END-BROWSE-IND           PIC X(01) VALUE 'N'.
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-BROWSE-ACTIVE-IND        PIC X(01) VALUE 'N'.
               88  WS-BROWSE-ACTIVE            VALUE 'Y'.
      *
           COPY DDACCREC.
      *
           COPY DTEPARM.
      *
           COPY CHCOMMAR.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(4000).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE DFHCOMMAREA                TO CH-COMMAREA
           MOVE SPACES                     TO CHAL-RESP
           MOVE ZERO                       TO CHAL-ACCT-COUNT
           MOVE 'N'                        TO CHAL-MORE-IND

           IF CHAL-CIF-NO = SPACES OR LOW-VALUES
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'CIFX'                 TO CH-REASON-CD
               MOVE 'CIF NUMBER NOT SUPPLIED'
                                           TO CH-MESSAGE-TEXT
           ELSE
               PERFORM 1000-SET-CUTOFF-DATE
               PERFORM 2000-BROWSE-ACCOUNTS
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-SET-CUTOFF-DATE.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC

           MOVE 'D'                        TO DTE-FUNCTION
           MOVE WS-TODAY                   TO DTE-DATE-IN
           MOVE -90                        TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO WS-CLOSED-CUTOFF
           .
      *================================================================*
       2000-BROWSE-ACCOUNTS.
      *================================================================*
           MOVE CHAL-CIF-NO                TO WS-BROWSE-KEY

           EXEC CICS STARTBR
                     FILE('ACCTCIF')
                     RIDFLD(WS-BROWSE-KEY)
                     EQUAL
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   SET WS-BROWSE-ACTIVE    TO TRUE
                   MOVE '0000'             TO CH-RETURN-CD
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'NOAC'             TO CH-REASON-CD
                   MOVE 'NO ACCOUNTS FOUND FOR CUSTOMER'
                                           TO CH-MESSAGE-TEXT
                   SET WS-END-BROWSE       TO TRUE
               WHEN OTHER
                   PERFORM 9000-FILE-ERROR
                   SET WS-END-BROWSE       TO TRUE
           END-EVALUATE

           PERFORM 2100-READ-NEXT
               UNTIL WS-END-BROWSE

           IF WS-BROWSE-ACTIVE
               EXEC CICS ENDBR
                         FILE('ACCTCIF')
                         RESP(WS-RESP)
               END-EXEC
           END-IF

           IF CH-RC-OK AND CHAL-ACCT-COUNT = ZERO
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'NOAC'                 TO CH-REASON-CD
               MOVE 'NO OPEN ACCOUNTS FOUND FOR CUSTOMER'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       2100-READ-NEXT.
      *================================================================*
           EXEC CICS READNEXT
                     FILE('ACCTCIF')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-BROWSE-KEY)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
               WHEN DFHRESP(DUPKEY)
                   IF ACM-CIF-NO NOT = CHAL-CIF-NO
                       SET WS-END-BROWSE   TO TRUE
                   ELSE
                       PERFORM 2200-ADD-ACCOUNT
                   END-IF
               WHEN DFHRESP(ENDFILE)
                   SET WS-END-BROWSE       TO TRUE
               WHEN OTHER
                   PERFORM 9000-FILE-ERROR
                   SET WS-END-BROWSE       TO TRUE
           END-EVALUATE
           .
      *================================================================*
       2200-ADD-ACCOUNT.
      *================================================================*
           IF ACM-CLOSED
           AND ACM-CLOSE-DATE < WS-CLOSED-CUTOFF
               CONTINUE
           ELSE
               IF WS-SUB = WS-MAX-ACCOUNTS
                   MOVE 'Y'                TO CHAL-MORE-IND
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'MORE'             TO CH-REASON-CD
                   MOVE 'MORE THAN 20 ACCOUNTS - LIST TRUNCATED'
                                           TO CH-MESSAGE-TEXT
                   SET WS-END-BROWSE       TO TRUE
               ELSE
                   ADD 1                   TO WS-SUB
                   MOVE ACM-SORT-CODE      TO CHAL-SORT-CODE (WS-SUB)
                   MOVE ACM-ACCT-NO        TO CHAL-ACCT-NO (WS-SUB)
                   MOVE ACM-ACCT-TYPE-CD   TO CHAL-ACCT-TYPE (WS-SUB)
                   MOVE ACM-PROD-CD        TO CHAL-PROD-CD (WS-SUB)
                   MOVE ACM-ACCT-STATUS    TO CHAL-STATUS (WS-SUB)
                   MOVE ACM-LEDGER-BAL     TO CHAL-LEDGER-BAL (WS-SUB)
                   MOVE ACM-AVAIL-BAL      TO CHAL-AVAIL-BAL (WS-SUB)
                   MOVE WS-SUB             TO CHAL-ACCT-COUNT
               END-IF
           END-IF
           .
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
           MOVE WS-RESP                    TO WS-RESP-DISPLAY
           MOVE '0012'                     TO CH-RETURN-CD
           IF WS-RESP = DFHRESP(NOTOPEN)
           OR WS-RESP = DFHRESP(DISABLED)
               MOVE 'FNOP'                 TO CH-REASON-CD
               MOVE 'ACCOUNT FILE NOT AVAILABLE (BATCH WINDOW)'
                                           TO CH-MESSAGE-TEXT
           ELSE
               MOVE 'FERR'                 TO CH-REASON-CD
               MOVE SPACES                 TO CH-MESSAGE-TEXT
               STRING 'ACCTCIF BROWSE ERROR - EIBRESP '
                      WS-RESP-DISPLAY
                      DELIMITED BY SIZE
                      INTO CH-MESSAGE-TEXT
               END-STRING
           END-IF
           .
