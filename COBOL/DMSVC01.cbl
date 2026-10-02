       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DMSVC01.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-03-08.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DMSVC01                                             *
      * TITLE    : DIRECT DEBIT MANDATES - CHANNEL SERVICE DDMANDT     *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : DDMAND   BROWSE / READ UPDATE / REWRITE             *
      *            ACCTMST  READ (ACCOUNT MUST EXIST)                  *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-DDMANDT-AREA               *
      *----------------------------------------------------------------*
      * L  LIST MANDATES ON THE ACCOUNT, 20 PER CALL. LIVE AND DORMANT *
      *    MANDATES ARE ALWAYS RETURNED; CANCELLED / CLOSED ONES ONLY  *
      *    IF CHDM-INCLUDE-CANC = 'Y'. PAGING: PASS CHDM-NEXT-RESTART  *
      *    BACK IN CHDM-RESTART-KEY (SUN + PAYER REFERENCE).           *
      * X  CANCEL A MANDATE (PAYER INSTRUCTION). THE ORIGINATOR IS     *
      *    TOLD BY ADDACS REASON 1 (DMADC010 TONIGHT). COLLECTIONS     *
      *    ALREADY IN THE BACS CYCLE FOR TOMORROW WILL BE RETURNED     *
      *    UNPAID WITH ARUDD REASON 1 BY DMCOL010.                     *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   ANFD ACCOUNT NOT FOUND       MNFD MANDATE NOT FOUND          *
      *   MCAN MANDATE ALREADY CANCELLED                               *
      *   NONE NO MANDATES (WARNING 0004)                              *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-03-08 AOS  ORIGINAL VERSION (IBANK "MANAGE DIRECT DEBITS")*
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DMSVC01 '.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-LIST                 PIC 9(02) VALUE 20.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-LIST-COUNT               PIC 9(02).
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-SKIP-FIRST-IND           PIC X(01).
               88  WS-SKIP-FIRST               VALUE 'Y'.
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
       01  WS-DDM-KEY.
           05  WS-DK-SORT-CODE             PIC X(06).
           05  WS-DK-ACCT-NO               PIC X(08).
           05  WS-DK-SUN                   PIC X(06).
           05  WS-DK-PAYER-REF             PIC X(18).
      *
           COPY DDACCREC.
      *
           COPY DMMNDREC.
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
                                              CHDM-NEXT-RESTART
           MOVE ZERO                       TO CHDM-COUNT
           MOVE 'N'                        TO CHDM-MORE-IND

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC

           EVALUATE TRUE
               WHEN NOT (CHDM-LIST OR CHDM-CANCEL)
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE L OR X'
                                           TO CH-MESSAGE-TEXT
               WHEN CHDM-SORT-CODE NOT NUMERIC
               WHEN CHDM-ACCT-NO NOT NUMERIC
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHDM-CANCEL
               AND (CHDM-ORIG-SUN = SPACES OR CHDM-PAYER-REF = SPACES)
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'SUN AND PAYER REFERENCE REQUIRED TO CANCEL'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   PERFORM 1000-CHECK-ACCOUNT
           END-EVALUATE

           IF CH-RC-OK
               IF CHDM-LIST
                   PERFORM 2000-LIST-MANDATES THRU 2000-EXIT
               ELSE
                   PERFORM 3000-CANCEL-MANDATE THRU 3000-EXIT
               END-IF
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-CHECK-ACCOUNT.
      *================================================================*
           MOVE CHDM-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHDM-ACCT-NO               TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       2000-LIST-MANDATES.
      *================================================================*
           MOVE CHDM-SORT-CODE             TO WS-DK-SORT-CODE
           MOVE CHDM-ACCT-NO               TO WS-DK-ACCT-NO
           IF CHDM-RESTART-KEY = SPACES OR LOW-VALUES
               MOVE LOW-VALUES             TO WS-DK-SUN
                                              WS-DK-PAYER-REF
               MOVE 'N'                    TO WS-SKIP-FIRST-IND
           ELSE
               MOVE CHDM-RESTART-KEY (1:6) TO WS-DK-SUN
               MOVE CHDM-RESTART-KEY (7:18) TO WS-DK-PAYER-REF
      *        THE RESTART KEY IS THE LAST ONE RETURNED - SKIP IT.
               SET WS-SKIP-FIRST           TO TRUE
           END-IF
           MOVE ZERO                       TO WS-LIST-COUNT
           MOVE 'N'                        TO WS-END-BROWSE-IND

           EXEC CICS STARTBR
                     FILE('DDMAND')
                     RIDFLD(WS-DDM-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET WS-END-BROWSE       TO TRUE
               WHEN OTHER
                   MOVE 'DDMAND'           TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE

           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('DDMAND')
                         INTO(DMM-MANDATE-RECORD)
                         LENGTH(LENGTH OF DMM-MANDATE-RECORD)
                         RIDFLD(WS-DDM-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   WHEN DMM-PAYER-SORT-CODE NOT = CHDM-SORT-CODE
                   WHEN DMM-PAYER-ACCT-NO NOT = CHDM-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN WS-SKIP-FIRST
                       MOVE 'N'            TO WS-SKIP-FIRST-IND
                   WHEN NOT (DMM-LIVE OR DMM-DORMANT)
                   AND  CHDM-INCLUDE-CANC NOT = 'Y'
                       CONTINUE
                   WHEN WS-LIST-COUNT >= WS-MAX-LIST
                       MOVE 'Y'            TO CHDM-MORE-IND
                       SET WS-END-BROWSE   TO TRUE
                   WHEN OTHER
                       ADD 1               TO WS-LIST-COUNT
                       PERFORM 2100-FORMAT-ENTRY
                       MOVE DMM-ORIG-SUN   TO CHDM-NEXT-RESTART (1:6)
                       MOVE DMM-PAYER-REF  TO CHDM-NEXT-RESTART (7:18)
               END-EVALUATE
           END-PERFORM

           IF WS-RESP = DFHRESP(NORMAL)
           OR WS-RESP = DFHRESP(ENDFILE)
               EXEC CICS ENDBR
                         FILE('DDMAND')
                         RESP(WS-RESP)
               END-EXEC
           END-IF

           MOVE WS-LIST-COUNT              TO CHDM-COUNT
           IF CHDM-MORE-IND NOT = 'Y'
               MOVE SPACES                 TO CHDM-NEXT-RESTART
           END-IF
           IF WS-LIST-COUNT = ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'NONE'                 TO CH-REASON-CD
               MOVE 'NO DIRECT DEBITS ON THIS ACCOUNT'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-FORMAT-ENTRY.
      *================================================================*
           MOVE DMM-ORIG-SUN       TO CHDME-ORIG-SUN (WS-LIST-COUNT)
           MOVE DMM-ORIG-NAME      TO CHDME-ORIG-NAME (WS-LIST-COUNT)
           MOVE DMM-PAYER-REF      TO CHDME-PAYER-REF (WS-LIST-COUNT)
           MOVE DMM-STATUS         TO CHDME-STATUS (WS-LIST-COUNT)
           MOVE DMM-SETUP-DATE     TO CHDME-SETUP-DATE (WS-LIST-COUNT)
           MOVE DMM-LAST-COLL-DATE
                                TO CHDME-LAST-COLL-DATE (WS-LIST-COUNT)
           MOVE DMM-LAST-COLL-AMT
                                TO CHDME-LAST-COLL-AMT (WS-LIST-COUNT)
           .
      *================================================================*
       3000-CANCEL-MANDATE.
      *================================================================*
           MOVE CHDM-SORT-CODE             TO WS-DK-SORT-CODE
           MOVE CHDM-ACCT-NO               TO WS-DK-ACCT-NO
           MOVE CHDM-ORIG-SUN              TO WS-DK-SUN
           MOVE CHDM-PAYER-REF             TO WS-DK-PAYER-REF
           EXEC CICS READ
                     FILE('DDMAND')
                     INTO(DMM-MANDATE-RECORD)
                     LENGTH(LENGTH OF DMM-MANDATE-RECORD)
                     RIDFLD(WS-DDM-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'MNFD'             TO CH-REASON-CD
                   MOVE 'DIRECT DEBIT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   GO TO 3000-EXIT
               WHEN OTHER
                   MOVE 'DDMAND'           TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 3000-EXIT
           END-EVALUATE

           IF NOT (DMM-LIVE OR DMM-DORMANT)
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'MCAN'                 TO CH-REASON-CD
               MOVE 'DIRECT DEBIT IS ALREADY CANCELLED'
                                           TO CH-MESSAGE-TEXT
               EXEC CICS UNLOCK
                         FILE('DDMAND')
                         RESP(WS-RESP)
               END-EXEC
               GO TO 3000-EXIT
           END-IF

           SET DMM-CANC-BY-PAYER           TO TRUE
           MOVE WS-TODAY                   TO DMM-CANCEL-DATE
                                              DMM-LAST-MAINT-DATE
           MOVE CH-CHANNEL-CD              TO DMM-CANCEL-CHANNEL
           MOVE CH-USER-ID                 TO DMM-CANCEL-USER
           MOVE ZERO                       TO DMM-ADDACS-SENT-DATE
           MOVE '1'                        TO DMM-ADDACS-REASON
           MOVE WS-PROGRAM-ID              TO DMM-LAST-MAINT-PGM

           EXEC CICS REWRITE
                     FILE('DDMAND')
                     FROM(DMM-MANDATE-RECORD)
                     LENGTH(LENGTH OF DMM-MANDATE-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'DDMAND'               TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 3000-EXIT
           END-IF

           MOVE 1                          TO WS-LIST-COUNT
           PERFORM 2100-FORMAT-ENTRY
           MOVE 1                          TO CHDM-COUNT
           MOVE 'DIRECT DEBIT CANCELLED - ORIGINATOR WILL BE TOLD'
                                           TO CH-MESSAGE-TEXT
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
           MOVE WS-RESP                    TO WS-RESP-DISPLAY
           MOVE '0012'                     TO CH-RETURN-CD
           IF WS-RESP = DFHRESP(NOTOPEN)
           OR WS-RESP = DFHRESP(DISABLED)
               MOVE 'FNOP'                 TO CH-REASON-CD
           ELSE
               MOVE 'FERR'                 TO CH-REASON-CD
           END-IF
           MOVE SPACES                     TO CH-MESSAGE-TEXT
           STRING WS-FILE-NAME
                  ' FILE ERROR - EIBRESP '
                  WS-RESP-DISPLAY
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
