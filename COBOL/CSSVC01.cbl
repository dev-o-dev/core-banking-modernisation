       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CSSVC01.
       AUTHOR.        P D QUINN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - PAYMENTS.
       DATE-WRITTEN.  2013-06-24.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CSSVC01                                             *
      * TITLE    : CASS SWITCH-IN REQUEST - CHANNEL SERVICE SWITCHRQ   *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : SWITCHES READ / WRITE / REWRITE / BROWSE            *
      *            ACCTMST  READ                                       *
      *            CIFMAST  READ                                       *
      * CALLS    : DTEUTIL  (WORKING DAYS)                             *
      *            DDMODCK  (OLD ACCOUNT MODULUS CHECK)                *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-SWITCHRQ-AREA              *
      *----------------------------------------------------------------*
      * A CUSTOMER SWITCHING TO MEGABANK OPENS A CURRENT ACCOUNT       *
      * (ACCTOPEN) AND THEN ASKS FOR THE SWITCH HERE, GIVING THE OLD   *
      * BANK ACCOUNT AND A SWITCH DATE. THE REQUEST IS STORED ON THE   *
      * SWITCHES FILE WITH STATUS 'RQ' AND REQUEST DATE ZERO; CSSOU010 *
      * SENDS THE SWRQ TO CASS TONIGHT.                                *
      *                                                                *
      * RULES (CASS SCHEME RULES SECTION 4):                           *
      *   - NEW ACCOUNT: OPEN MEGABANK CURRENT ACCOUNT, NOT RESTRICTED *
      *   - OLD ACCOUNT: NOT A MEGABANK ACCOUNT (USE A TRANSFER), PASS *
      *     THE VOCALINK MODULUS CHECK                                 *
      *   - SWITCH DATE: A WORKING DAY AT LEAST 7 WORKING DAYS AHEAD   *
      *     (6 + TODAY'S REQUEST DAY), AT MOST 60 DAYS AHEAD           *
      *   - ONE OPEN SWITCH PER NEW ACCOUNT                            *
      * CANCEL ('X') IS ONLY ALLOWED BEFORE THE SWRQ HAS BEEN SENT;    *
      * AFTER THAT THE CUSTOMER MUST CALL THE CASS DESK.               *
      *                                                                *
      * THE ENQUIRY BROWSES THE WHOLE SWITCHES FILE (KEY IS SWITCH ID, *
      * NOT ACCOUNT). FINE AT 30,000 RECORDS - PIR 2014/018 OPEN TO    *
      * ADD AN AIX BY NEW ACCOUNT.                                     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2013-06-24 PDQ  ORIGINAL VERSION (CASS GO-LIVE 16/09/2013)     *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CSSVC01'.
      *
       01  WS-CONSTANTS.
           05  WS-MIN-WORKING-DAYS         PIC S9(03) COMP-3 VALUE +7.
           05  WS-MAX-DAYS-AHEAD           PIC S9(03) COMP-3 VALUE +60.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-EARLIEST                 PIC 9(08).
           05  WS-LATEST                   PIC 9(08).
           05  WS-TASK-NO                  PIC 9(07).
           05  WS-SWITCH-KEY               PIC X(18).
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-OPEN-FOUND-IND           PIC X(01).
               88  WS-OPEN-SWITCH-FOUND        VALUE 'Y'.
           05  WS-SUB                      PIC S9(04) COMP.
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
      *
           COPY CSSRECS.
      *
           COPY DDACCREC.
      *
           COPY CFCUSREC.
      *
           COPY DTEPARM.
      *
           COPY MODPARM.
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
                                              CHSW-NEW-SWITCH-ID
           MOVE ZERO                       TO CHSW-COUNT
                                              CHSW-EARLIEST-DATE

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC
           PERFORM 1500-EARLIEST-DATE

           EVALUATE TRUE
               WHEN CHSW-REQUEST
                   PERFORM 2000-REQUEST THRU 2000-EXIT
               WHEN CHSW-ENQUIRE
                   PERFORM 3000-ENQUIRE THRU 3000-EXIT
               WHEN CHSW-CANCEL
                   PERFORM 4000-CANCEL THRU 4000-EXIT
               WHEN OTHER
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE R, E OR X'
                                           TO CH-MESSAGE-TEXT
           END-EVALUATE

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1500-EARLIEST-DATE.
      *================================================================*
           MOVE 'A'                        TO DTE-FUNCTION
           MOVE WS-TODAY                   TO DTE-DATE-IN
           MOVE WS-MIN-WORKING-DAYS        TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO WS-EARLIEST
                                              CHSW-EARLIEST-DATE
           COMPUTE WS-LATEST = FUNCTION DATE-OF-INTEGER
                   (FUNCTION INTEGER-OF-DATE (WS-TODAY)
                    + WS-MAX-DAYS-AHEAD)
           .
      *================================================================*
       2000-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN CHSW-NEW-SORT-CODE NOT NUMERIC
               WHEN CHSW-NEW-ACCT-NO NOT NUMERIC
                   MOVE 'INVN'             TO CH-REASON-CD
                   MOVE 'NEW ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSW-OLD-SORT-CODE NOT NUMERIC
               WHEN CHSW-OLD-ACCT-NO NOT NUMERIC
                   MOVE 'INVO'             TO CH-REASON-CD
                   MOVE 'OLD BANK ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSW-SWITCH-DATE NOT NUMERIC
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'SWITCH DATE MUST BE CCYYMMDD'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 2000-EXIT
           END-IF

      *---- SWITCH DATE -----------------------------------------------*
           MOVE 'B'                        TO DTE-FUNCTION
           MOVE CHSW-SWITCH-DATE           TO DTE-DATE-IN
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           EVALUATE TRUE
               WHEN NOT DTE-OK
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'SWITCH DATE IS NOT A VALID DATE'
                                           TO CH-MESSAGE-TEXT
               WHEN DTE-BUS-DAY-IND NOT = 'Y'
                   MOVE 'DATE'             TO CH-REASON-CD
                   MOVE 'SWITCH DATE MUST BE A WORKING DAY'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSW-SWITCH-DATE < WS-EARLIEST
                   MOVE 'DATE'             TO CH-REASON-CD
                   MOVE 'SWITCH DATE TOO SOON - SEE EARLIEST DATE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSW-SWITCH-DATE > WS-LATEST
                   MOVE 'DATE'             TO CH-REASON-CD
                   MOVE 'SWITCH DATE MORE THAN 60 DAYS AHEAD'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0008'                 TO CH-RETURN-CD
               GO TO 2000-EXIT
           END-IF

      *---- NEW (MEGABANK) ACCOUNT ------------------------------------*
           MOVE CHSW-NEW-SORT-CODE         TO WS-AK-SORT-CODE
           MOVE CHSW-NEW-ACCT-NO           TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'NEW ACCOUNT NOT FOUND' TO CH-MESSAGE-TEXT
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   PERFORM 9000-FILE-ERROR
               WHEN NOT ACM-CURRENT
                   MOVE 'NCUR'             TO CH-REASON-CD
                   MOVE 'SWITCHES ARE ONLY INTO CURRENT ACCOUNTS'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT ACM-OPEN
               WHEN ACM-NO-CREDITS OR ACM-NO-POSTINGS
                   MOVE 'ARST'             TO CH-REASON-CD
                   MOVE 'NEW ACCOUNT NOT OPEN OR RESTRICTED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               IF CH-RC-OK
                   MOVE '0008'             TO CH-RETURN-CD
               END-IF
               GO TO 2000-EXIT
           END-IF
           MOVE ACM-CIF-NO                 TO CIF-NO

      *---- OLD ACCOUNT MUST NOT BE OURS ------------------------------*
           MOVE CHSW-OLD-SORT-CODE         TO WS-AK-SORT-CODE
           MOVE CHSW-OLD-ACCT-NO           TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'INTL'                 TO CH-REASON-CD
               MOVE 'OLD ACCOUNT IS A MEGABANK ACCOUNT - USE A TRANSFER'
                                           TO CH-MESSAGE-TEXT
               GO TO 2000-EXIT
           END-IF

      *---- MODULUS CHECK ON THE OLD ACCOUNT --------------------------*
           MOVE SPACES                     TO MOD-PARM-BLOCK
           MOVE 'V'                        TO MOD-FUNCTION
           MOVE CHSW-OLD-SORT-CODE         TO MOD-SORT-CODE
           MOVE CHSW-OLD-ACCT-NO           TO MOD-ACCT-NO
           CALL 'DDMODCK' USING MOD-PARM-BLOCK
           IF NOT MOD-VALID
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'MODF'                 TO CH-REASON-CD
               MOVE 'OLD SORT CODE / ACCOUNT FAILS MODULUS CHECK'
                                           TO CH-MESSAGE-TEXT
               GO TO 2000-EXIT
           END-IF

      *---- ONE OPEN SWITCH PER NEW ACCOUNT ---------------------------*
           PERFORM 5000-BROWSE-FOR-ACCOUNT
           IF WS-OPEN-SWITCH-FOUND
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'DUPS'                 TO CH-REASON-CD
               MOVE 'A SWITCH IS ALREADY IN PROGRESS FOR THIS ACCOUNT'
                                           TO CH-MESSAGE-TEXT
               GO TO 2000-EXIT
           END-IF

      *---- WRITE THE REQUEST -----------------------------------------*
           MOVE SPACES                     TO CSW-SWITCH-RECORD
           MOVE EIBTASKN                   TO WS-TASK-NO
           STRING 'MBK' WS-TODAY WS-TASK-NO
                  DELIMITED BY SIZE
                  INTO CSW-SWITCH-ID
           END-STRING
           SET CSW-INCOMING                TO TRUE
           SET CSW-REQUESTED               TO TRUE
           MOVE CHSW-SWITCH-DATE           TO CSW-SWITCH-DATE
           MOVE CHSW-OLD-SORT-CODE         TO CSW-OLD-SORT-CODE
           MOVE CHSW-OLD-ACCT-NO           TO CSW-OLD-ACCT-NO
           MOVE CHSW-NEW-SORT-CODE         TO CSW-NEW-SORT-CODE
           MOVE CHSW-NEW-ACCT-NO           TO CSW-NEW-ACCT-NO
           MOVE CIF-NO                     TO CSW-CIF-NO
           MOVE ZERO                       TO CSW-SO-COUNT
                                              CSW-DD-COUNT
                                              CSW-BALANCE-AMT
                                              CSW-REQUEST-DATE
                                              CSW-ACCEPT-DATE
                                              CSW-COMPLETE-DATE
                                              CSW-REDIRECT-END
           MOVE WS-TODAY                   TO CSW-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO CSW-LAST-MAINT-PGM
           MOVE CSW-SWITCH-ID              TO WS-SWITCH-KEY
           EXEC CICS WRITE
                     FILE('SWITCHES')
                     FROM(CSW-SWITCH-RECORD)
                     LENGTH(LENGTH OF CSW-SWITCH-RECORD)
                     RIDFLD(WS-SWITCH-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-FILE-ERROR
               GO TO 2000-EXIT
           END-IF
           MOVE CSW-SWITCH-ID              TO CHSW-NEW-SWITCH-ID
           MOVE 'SWITCH REQUESTED - SENT TO CASS TONIGHT'
                                           TO CH-MESSAGE-TEXT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-ENQUIRE.
      *================================================================*
           IF CHSW-NEW-SORT-CODE NOT NUMERIC
           OR CHSW-NEW-ACCT-NO NOT NUMERIC
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'INVN'                 TO CH-REASON-CD
               MOVE 'ACCOUNT NUMBER INVALID' TO CH-MESSAGE-TEXT
               GO TO 3000-EXIT
           END-IF
           PERFORM 5000-BROWSE-FOR-ACCOUNT
           IF CHSW-COUNT = ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'NONE'                 TO CH-REASON-CD
               MOVE 'NO SWITCHES FOR THIS ACCOUNT' TO CH-MESSAGE-TEXT
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-CANCEL.
      *================================================================*
           MOVE CHSW-SWITCH-ID             TO WS-SWITCH-KEY
           EXEC CICS READ
                     FILE('SWITCHES')
                     INTO(CSW-SWITCH-RECORD)
                     LENGTH(LENGTH OF CSW-SWITCH-RECORD)
                     RIDFLD(WS-SWITCH-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'NOTF'             TO CH-REASON-CD
                   MOVE 'SWITCH NOT FOUND' TO CH-MESSAGE-TEXT
                   GO TO 4000-EXIT
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   PERFORM 9000-FILE-ERROR
                   GO TO 4000-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT CSW-INCOMING
           OR NOT CSW-REQUESTED
           OR CSW-REQUEST-DATE NOT = ZERO
               EXEC CICS UNLOCK FILE('SWITCHES') RESP(WS-RESP)
               END-EXEC
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'SENT'                 TO CH-REASON-CD
               MOVE 'ALREADY SENT TO CASS - CALL THE CASS DESK'
                                           TO CH-MESSAGE-TEXT
               GO TO 4000-EXIT
           END-IF
           SET CSW-ENDED                   TO TRUE
           MOVE 'CANC'                     TO CSW-REJECT-REASON
           MOVE WS-TODAY                   TO CSW-LAST-MAINT-DATE
           MOVE WS-PROGRAM-ID              TO CSW-LAST-MAINT-PGM
           EXEC CICS REWRITE
                     FILE('SWITCHES')
                     FROM(CSW-SWITCH-RECORD)
                     LENGTH(LENGTH OF CSW-SWITCH-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-FILE-ERROR
               GO TO 4000-EXIT
           END-IF
           MOVE 'SWITCH REQUEST CANCELLED' TO CH-MESSAGE-TEXT
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       5000-BROWSE-FOR-ACCOUNT.
      *================================================================*
      *    SEE HEADER - FULL FILE BROWSE (PIR 2014/018).
           MOVE 'N'                        TO WS-OPEN-FOUND-IND
                                              WS-END-BROWSE-IND
           MOVE ZERO                       TO CHSW-COUNT
           MOVE LOW-VALUES                 TO WS-SWITCH-KEY
           EXEC CICS STARTBR
                     FILE('SWITCHES')
                     RIDFLD(WS-SWITCH-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 5000-EXIT
           END-IF
           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('SWITCHES')
                         INTO(CSW-SWITCH-RECORD)
                         LENGTH(LENGTH OF CSW-SWITCH-RECORD)
                         RIDFLD(WS-SWITCH-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       SET WS-END-BROWSE   TO TRUE
                   WHEN (CSW-NEW-SORT-CODE = CHSW-NEW-SORT-CODE
                         AND CSW-NEW-ACCT-NO = CHSW-NEW-ACCT-NO)
                   OR   (CSW-OLD-SORT-CODE = CHSW-NEW-SORT-CODE
                         AND CSW-OLD-ACCT-NO = CHSW-NEW-ACCT-NO)
                       IF NOT (CSW-ENDED OR CSW-REJECTED
                               OR CSW-COMPLETE OR CSW-REDIRECTING)
                           SET WS-OPEN-SWITCH-FOUND TO TRUE
                       END-IF
                       IF CHSW-COUNT < 10
                           ADD 1           TO CHSW-COUNT
                           MOVE CHSW-COUNT TO WS-SUB
                           MOVE CSW-SWITCH-ID
                                   TO CHSWE-SWITCH-ID (WS-SUB)
                           MOVE CSW-DIRECTION
                                   TO CHSWE-DIRECTION (WS-SUB)
                           MOVE CSW-STATUS TO CHSWE-STATUS (WS-SUB)
                           MOVE CSW-OLD-SORT-CODE
                                   TO CHSWE-OLD-SORT-CODE (WS-SUB)
                           MOVE CSW-OLD-ACCT-NO
                                   TO CHSWE-OLD-ACCT-NO (WS-SUB)
                           MOVE CSW-SWITCH-DATE
                                   TO CHSWE-SWITCH-DATE (WS-SUB)
                           MOVE CSW-SO-COUNT
                                   TO CHSWE-SO-COUNT (WS-SUB)
                           MOVE CSW-DD-COUNT
                                   TO CHSWE-DD-COUNT (WS-SUB)
                           MOVE CSW-REJECT-REASON
                                   TO CHSWE-REJECT-RSN (WS-SUB)
                       END-IF
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR FILE('SWITCHES') RESP(WS-RESP)
           END-EXEC
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
           MOVE '0012'                     TO CH-RETURN-CD
           MOVE 'FILE'                     TO CH-REASON-CD
           STRING 'SWITCH FILE ERROR - RESP ' WS-RESP
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
