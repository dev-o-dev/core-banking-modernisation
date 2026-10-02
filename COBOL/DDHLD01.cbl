       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDHLD01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-05-17.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDHLD01                                             *
      * TITLE    : FUNDS HOLDS - CHANNEL SERVICE ACCTHOLD              *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : ACCTMST  READ UPDATE / REWRITE                      *
      *            DDHOLD   READ / BROWSE / WRITE / REWRITE            *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-ACCTHOLD-AREA              *
      *----------------------------------------------------------------*
      * A HOLD RESERVES PART OF THE AVAILABLE BALANCE. PLACING A HOLD  *
      * REDUCES ACM-AVAIL-BAL AND INCREASES ACM-HOLD-AMT BY THE SAME   *
      * AMOUNT; RELEASING REVERSES THIS. THE LEDGER BALANCE IS NOT     *
      * AFFECTED.                                                      *
      *                                                                *
      * HOLDS WITH AN EXPIRY DATE ARE RELEASED AUTOMATICALLY BY BATCH  *
      * PROGRAM DDHLX010 THE NIGHT AFTER THEY EXPIRE.                  *
      *                                                                *
      * AUTHORITY                                                      *
      *   - LEGL (LEGAL / GARNISHEE) AND FRD (FRAUD) HOLDS CAN ONLY BE *
      *     PLACED OR RELEASED FROM CHANNEL 'BRANCH' (BRANCH SECURITY  *
      *     AND THE FRAUD TEAM USE THE BRANCH FRONT END)               *
      *   - CARD HOLDS ARE PLACED BY THE CARD AUTHORISATION GATEWAY    *
      *     (CHANNEL 'CARDS ') - ADDED TO THE CHROUTER CHANNEL TABLE   *
      *   - THE AVAILABLE BALANCE MAY GO NEGATIVE AS A RESULT OF A     *
      *     HOLD (E.G. A COURT ORDER LARGER THAN THE BALANCE) - THIS   *
      *     IS INTENTIONAL                                             *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   ANFD ACCOUNT NOT FOUND       ACLS ACCOUNT CLOSED             *
      *   HNFD HOLD NOT FOUND          HREL HOLD NOT ACTIVE            *
      *   HAUT CHANNEL NOT AUTHORISED FOR THIS HOLD TYPE               *
      *   HMAX TOO MANY ACTIVE HOLDS (15)                              *
      *   INVx INVALID REQUEST FIELD                                   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-05-17 JNB  ORIGINAL VERSION (AUDIT FINDING IA-09-031)     *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDHLD01 '.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-ACTIVE               PIC 9(03) VALUE 15.
           05  WS-MAX-HOLD-AMT             PIC S9(13)V99 COMP-3
                                           VALUE +9999999.99.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TIME-HHMMSS              PIC 9(06).
           05  WS-TIME-HHMMSS-X REDEFINES WS-TIME-HHMMSS
                                           PIC X(06).
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-LIST-COUNT               PIC 9(02).
           05  WS-NEW-SEQ                  PIC 9(04).
           05  WS-ACCOUNT-HELD-IND         PIC X(01) VALUE 'N'.
               88  WS-ACCOUNT-HELD             VALUE 'Y'.
           05  WS-CONTROL-NEW-IND          PIC X(01) VALUE 'N'.
               88  WS-CONTROL-NEW              VALUE 'Y'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
       01  WS-HOLD-KEY.
           05  WS-HK-SORT-CODE             PIC X(06).
           05  WS-HK-ACCT-NO               PIC X(08).
           05  WS-HK-SEQ                   PIC 9(04).
      *
       01  WS-SAVED-HOLD                   PIC X(150).
      *
           COPY DDACCREC.
      *
           COPY DDHLDREC.
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
           MOVE ZERO                       TO CHAH-NEW-HOLD-SEQ
                                              CHAH-NEW-AVAIL-BAL
                                              CHAH-TOTAL-HOLD
                                              CHAH-COUNT

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC

           PERFORM 1000-VALIDATE-REQUEST THRU 1000-EXIT

           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHAH-PLACE
                       PERFORM 2000-PLACE-HOLD THRU 2000-EXIT
                   WHEN CHAH-RELEASE
                       PERFORM 3000-RELEASE-HOLD THRU 3000-EXIT
                   WHEN CHAH-LIST
                       PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
               END-EVALUATE
           END-IF

           IF WS-ACCOUNT-HELD
               EXEC CICS UNLOCK
                         FILE('ACCTMST')
                         RESP(WS-RESP)
               END-EXEC
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
           MOVE CHAH-REASON-CD             TO HLD-REASON-CD

           EVALUATE TRUE
               WHEN NOT (CHAH-PLACE OR CHAH-RELEASE OR CHAH-LIST)
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE P, R OR L'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAH-SORT-CODE NOT NUMERIC
               WHEN CHAH-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAH-PLACE
               AND (CHAH-AMOUNT NOT NUMERIC
                    OR CHAH-AMOUNT NOT > ZERO
                    OR CHAH-AMOUNT > WS-MAX-HOLD-AMT)
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'HOLD AMOUNT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAH-PLACE AND NOT HLD-RSN-VALID
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'HOLD REASON MUST BE CHQ, LEGL, FRD, CARD, OTHR'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAH-PLACE AND HLD-RSN-OTHER
               AND  CHAH-NARRATIVE = SPACES
                   MOVE 'INVN'             TO CH-REASON-CD
                   MOVE 'NARRATIVE REQUIRED FOR REASON OTHR'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAH-PLACE
               AND  CHAH-EXPIRY-DATE NOT NUMERIC
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'EXPIRY DATE NOT NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAH-PLACE
               AND  CHAH-EXPIRY-DATE NOT = ZERO
               AND  CHAH-EXPIRY-DATE <= WS-TODAY
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'EXPIRY DATE MUST BE IN THE FUTURE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAH-RELEASE
               AND (CHAH-HOLD-SEQ NOT NUMERIC OR CHAH-HOLD-SEQ = ZERO)
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'HOLD SEQUENCE REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 1000-EXIT
           END-IF

      *---- AUTHORITY FOR SENSITIVE HOLD TYPES ------------------------*
           IF CHAH-PLACE
               IF (HLD-RSN-LEGAL OR HLD-RSN-FRAUD)
               AND CH-CHANNEL-CD NOT = 'BRANCH'
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'HAUT'             TO CH-REASON-CD
                   MOVE 'LEGAL AND FRAUD HOLDS ARE BRANCH ONLY'
                                           TO CH-MESSAGE-TEXT
               END-IF
               IF HLD-RSN-CARD
               AND CH-CHANNEL-CD NOT = 'CARDS '
               AND CH-CHANNEL-CD NOT = 'BRANCH'
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'HAUT'             TO CH-REASON-CD
                   MOVE 'CARD HOLDS ONLY FROM THE CARD GATEWAY'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-PLACE-HOLD.
      *================================================================*
           PERFORM 8000-READ-ACCOUNT-UPDATE
           IF NOT CH-RC-OK
               GO TO 2000-EXIT
           END-IF
           IF ACM-CLOSED
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'ACLS'                 TO CH-REASON-CD
               MOVE 'ACCOUNT IS CLOSED'    TO CH-MESSAGE-TEXT
               GO TO 2000-EXIT
           END-IF

      *---- CONTROL RECORD - ALLOCATE SEQUENCE ------------------------*
           MOVE CHAH-SORT-CODE             TO WS-HK-SORT-CODE
           MOVE CHAH-ACCT-NO               TO WS-HK-ACCT-NO
           MOVE ZERO                       TO WS-HK-SEQ
           MOVE 'N'                        TO WS-CONTROL-NEW-IND
           EXEC CICS READ
                     FILE('DDHOLD')
                     INTO(HLD-HOLD-RECORD)
                     LENGTH(LENGTH OF HLD-HOLD-RECORD)
                     RIDFLD(WS-HOLD-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET WS-CONTROL-NEW      TO TRUE
                   MOVE SPACES             TO HLD-HOLD-RECORD
                   MOVE WS-HOLD-KEY        TO HLD-KEY
                   MOVE ZERO               TO HLD-LAST-SEQ
                                              HLD-ACTIVE-COUNT
               WHEN OTHER
                   MOVE 'DDHOLD'           TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE

           IF HLD-ACTIVE-COUNT >= WS-MAX-ACTIVE
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'HMAX'                 TO CH-REASON-CD
               MOVE 'MAXIMUM ACTIVE HOLDS REACHED - RELEASE ONE FIRST'
                                           TO CH-MESSAGE-TEXT
               IF NOT WS-CONTROL-NEW
                   EXEC CICS UNLOCK
                             FILE('DDHOLD')
                             RESP(WS-RESP)
                   END-EXEC
               END-IF
               GO TO 2000-EXIT
           END-IF

           IF HLD-LAST-SEQ >= 9999
               MOVE ZERO                   TO HLD-LAST-SEQ
           END-IF
           ADD 1                           TO HLD-LAST-SEQ
           ADD 1                           TO HLD-ACTIVE-COUNT
           MOVE HLD-LAST-SEQ               TO WS-NEW-SEQ

           IF WS-CONTROL-NEW
               EXEC CICS WRITE
                         FILE('DDHOLD')
                         FROM(HLD-HOLD-RECORD)
                         LENGTH(LENGTH OF HLD-HOLD-RECORD)
                         RIDFLD(HLD-KEY)
                         RESP(WS-RESP)
               END-EXEC
           ELSE
               EXEC CICS REWRITE
                         FILE('DDHOLD')
                         FROM(HLD-HOLD-RECORD)
                         LENGTH(LENGTH OF HLD-HOLD-RECORD)
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'DDHOLD'               TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 2000-EXIT
           END-IF

      *---- THE HOLD --------------------------------------------------*
           MOVE SPACES                     TO HLD-HOLD-RECORD
           MOVE CHAH-SORT-CODE             TO HLD-SORT-CODE
           MOVE CHAH-ACCT-NO               TO HLD-ACCT-NO
           MOVE WS-NEW-SEQ                 TO HLD-SEQ
           SET HLD-ACTIVE                  TO TRUE
           MOVE CHAH-AMOUNT                TO HLD-AMOUNT
                                              WS-AMOUNT
           MOVE CHAH-REASON-CD             TO HLD-REASON-CD
           MOVE CHAH-NARRATIVE             TO HLD-NARRATIVE
           MOVE WS-TODAY                   TO HLD-PLACED-DATE
           MOVE WS-TIME-HHMMSS             TO HLD-PLACED-TIME
           MOVE CH-USER-ID                 TO HLD-PLACED-USER
           MOVE CH-CHANNEL-CD              TO HLD-PLACED-CHANNEL
           MOVE CHAH-EXPIRY-DATE           TO HLD-EXPIRY-DATE
           MOVE ZERO                       TO HLD-RELEASED-DATE
           MOVE SPACES                     TO HLD-RELEASED-USER

           EXEC CICS WRITE
                     FILE('DDHOLD')
                     FROM(HLD-HOLD-RECORD)
                     LENGTH(LENGTH OF HLD-HOLD-RECORD)
                     RIDFLD(HLD-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND
                         ABCODE('HLD1')
               END-EXEC
           END-IF

      *---- ACCOUNT BALANCES ------------------------------------------*
           ADD WS-AMOUNT                   TO ACM-HOLD-AMT
           SUBTRACT WS-AMOUNT              FROM ACM-AVAIL-BAL
           PERFORM 8100-REWRITE-ACCOUNT

           MOVE WS-NEW-SEQ                 TO CHAH-NEW-HOLD-SEQ
           MOVE ACM-AVAIL-BAL              TO CHAH-NEW-AVAIL-BAL
           MOVE ACM-HOLD-AMT               TO CHAH-TOTAL-HOLD
           MOVE 'HOLD PLACED'              TO CH-MESSAGE-TEXT
           IF ACM-AVAIL-BAL < ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'HNEG'                 TO CH-REASON-CD
               MOVE 'HOLD PLACED - AVAILABLE BALANCE NOW NEGATIVE'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-RELEASE-HOLD.
      *================================================================*
           PERFORM 8000-READ-ACCOUNT-UPDATE
           IF NOT CH-RC-OK
               GO TO 3000-EXIT
           END-IF

           MOVE CHAH-SORT-CODE             TO WS-HK-SORT-CODE
           MOVE CHAH-ACCT-NO               TO WS-HK-ACCT-NO
           MOVE CHAH-HOLD-SEQ              TO WS-HK-SEQ
           EXEC CICS READ
                     FILE('DDHOLD')
                     INTO(HLD-HOLD-RECORD)
                     LENGTH(LENGTH OF HLD-HOLD-RECORD)
                     RIDFLD(WS-HOLD-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'HNFD'             TO CH-REASON-CD
                   MOVE 'HOLD NOT FOUND'   TO CH-MESSAGE-TEXT
                   GO TO 3000-EXIT
               WHEN OTHER
                   MOVE 'DDHOLD'           TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 3000-EXIT
           END-EVALUATE

           EVALUATE TRUE
               WHEN NOT HLD-ACTIVE
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'HREL'             TO CH-REASON-CD
                   MOVE 'HOLD IS ALREADY RELEASED OR EXPIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN (HLD-RSN-LEGAL OR HLD-RSN-FRAUD)
               AND  CH-CHANNEL-CD NOT = 'BRANCH'
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'HAUT'             TO CH-REASON-CD
                   MOVE 'LEGAL AND FRAUD HOLDS ARE BRANCH ONLY'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT CH-RC-OK
               EXEC CICS UNLOCK
                         FILE('DDHOLD')
                         RESP(WS-RESP)
               END-EXEC
               GO TO 3000-EXIT
           END-IF

           MOVE HLD-AMOUNT                 TO WS-AMOUNT
           SET HLD-RELEASED                TO TRUE
           MOVE WS-TODAY                   TO HLD-RELEASED-DATE
           MOVE CH-USER-ID                 TO HLD-RELEASED-USER
           EXEC CICS REWRITE
                     FILE('DDHOLD')
                     FROM(HLD-HOLD-RECORD)
                     LENGTH(LENGTH OF HLD-HOLD-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'DDHOLD'               TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 3000-EXIT
           END-IF

      *---- CONTROL RECORD ACTIVE COUNT -------------------------------*
           MOVE HLD-HOLD-RECORD            TO WS-SAVED-HOLD
           MOVE ZERO                       TO WS-HK-SEQ
           EXEC CICS READ
                     FILE('DDHOLD')
                     INTO(HLD-HOLD-RECORD)
                     LENGTH(LENGTH OF HLD-HOLD-RECORD)
                     RIDFLD(WS-HOLD-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               IF HLD-ACTIVE-COUNT > ZERO
                   SUBTRACT 1              FROM HLD-ACTIVE-COUNT
               END-IF
               EXEC CICS REWRITE
                         FILE('DDHOLD')
                         FROM(HLD-HOLD-RECORD)
                         LENGTH(LENGTH OF HLD-HOLD-RECORD)
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           MOVE WS-SAVED-HOLD              TO HLD-HOLD-RECORD

      *---- ACCOUNT BALANCES ------------------------------------------*
      *    GUARD AGAINST A NEGATIVE HOLD TOTAL (HOLDS LOADED FROM THE
      *    TELLER SYSTEM IN 2010 DID NOT ALWAYS MATCH ACM-HOLD-AMT).
           IF WS-AMOUNT > ACM-HOLD-AMT
               ADD ACM-HOLD-AMT            TO ACM-AVAIL-BAL
               MOVE ZERO                   TO ACM-HOLD-AMT
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'HMIS'                 TO CH-REASON-CD
               MOVE 'HOLD RELEASED - HOLD TOTAL MISMATCH, REFER TO OPS'
                                           TO CH-MESSAGE-TEXT
           ELSE
               SUBTRACT WS-AMOUNT          FROM ACM-HOLD-AMT
               ADD WS-AMOUNT               TO ACM-AVAIL-BAL
               MOVE 'HOLD RELEASED'        TO CH-MESSAGE-TEXT
           END-IF
           PERFORM 8100-REWRITE-ACCOUNT

           MOVE CHAH-HOLD-SEQ              TO CHAH-NEW-HOLD-SEQ
           MOVE ACM-AVAIL-BAL              TO CHAH-NEW-AVAIL-BAL
           MOVE ACM-HOLD-AMT               TO CHAH-TOTAL-HOLD
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-LIST-HOLDS.
      *================================================================*
           MOVE CHAH-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHAH-ACCT-NO               TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE ACM-AVAIL-BAL      TO CHAH-NEW-AVAIL-BAL
                   MOVE ACM-HOLD-AMT       TO CHAH-TOTAL-HOLD
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   GO TO 4000-EXIT
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 4000-EXIT
           END-EVALUATE

           MOVE CHAH-SORT-CODE             TO WS-HK-SORT-CODE
           MOVE CHAH-ACCT-NO               TO WS-HK-ACCT-NO
           MOVE 1                          TO WS-HK-SEQ
           MOVE ZERO                       TO WS-LIST-COUNT
           MOVE 'N'                        TO WS-END-BROWSE-IND

           EXEC CICS STARTBR
                     FILE('DDHOLD')
                     RIDFLD(WS-HOLD-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               SET WS-END-BROWSE           TO TRUE
           END-IF

           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('DDHOLD')
                         INTO(HLD-HOLD-RECORD)
                         LENGTH(LENGTH OF HLD-HOLD-RECORD)
                         RIDFLD(WS-HOLD-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   WHEN HLD-SORT-CODE NOT = CHAH-SORT-CODE
                   WHEN HLD-ACCT-NO NOT = CHAH-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN NOT HLD-ACTIVE
                       CONTINUE
                   WHEN WS-LIST-COUNT >= WS-MAX-ACTIVE
                       SET WS-END-BROWSE   TO TRUE
                   WHEN OTHER
                       ADD 1               TO WS-LIST-COUNT
                       PERFORM 4100-FORMAT-ENTRY
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR
                     FILE('DDHOLD')
                     RESP(WS-RESP)
           END-EXEC

           MOVE WS-LIST-COUNT              TO CHAH-COUNT
           IF WS-LIST-COUNT = ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'NONE'                 TO CH-REASON-CD
               MOVE 'NO ACTIVE HOLDS'      TO CH-MESSAGE-TEXT
           END-IF
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-FORMAT-ENTRY.
      *================================================================*
           MOVE HLD-SEQ            TO CHAHE-HOLD-SEQ (WS-LIST-COUNT)
           MOVE HLD-AMOUNT         TO CHAHE-AMOUNT (WS-LIST-COUNT)
           MOVE HLD-REASON-CD      TO CHAHE-REASON-CD (WS-LIST-COUNT)
           MOVE HLD-PLACED-DATE    TO CHAHE-PLACED-DATE (WS-LIST-COUNT)
           MOVE HLD-EXPIRY-DATE    TO CHAHE-EXPIRY-DATE (WS-LIST-COUNT)
           MOVE HLD-PLACED-USER    TO CHAHE-USER-ID (WS-LIST-COUNT)
           MOVE HLD-NARRATIVE      TO CHAHE-NARRATIVE (WS-LIST-COUNT)
           MOVE HLD-STATUS         TO CHAHE-STATUS (WS-LIST-COUNT)
           .
      *================================================================*
       8000-READ-ACCOUNT-UPDATE.
      *================================================================*
           MOVE CHAH-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHAH-ACCT-NO               TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   SET WS-ACCOUNT-HELD     TO TRUE
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
       8100-REWRITE-ACCOUNT.
      *================================================================*
           MOVE WS-TODAY                   TO ACM-LAST-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO ACM-LAST-MAINT-TIME
           MOVE CH-USER-ID                 TO ACM-LAST-MAINT-USER
           EXEC CICS REWRITE
                     FILE('ACCTMST')
                     FROM(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RESP(WS-RESP)
           END-EXEC
      *    THE HOLD RECORD IS ALREADY WRITTEN - BACK EVERYTHING OUT.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND
                         ABCODE('HLD2')
               END-EXEC
           END-IF
           MOVE 'N'                        TO WS-ACCOUNT-HELD-IND
           .
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
