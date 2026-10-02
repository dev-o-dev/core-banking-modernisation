       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CQSTP01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2012-02-13.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CQSTP01                                             *
      * TITLE    : CHEQUE STOPS - CHANNEL SERVICE CHQSTOP              *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : CHQSTOP  READ / BROWSE / WRITE / REWRITE            *
      *            ACCTMST  READ                                       *
      *            PRDPARM  READ (STOP FEE)                            *
      * LINKS    : PSMEMO1  (STOP FEE - 'CHG ' MEMO DEBIT)             *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-CHQSTOP-AREA               *
      *----------------------------------------------------------------*
      * UNTIL 2012 CHEQUE STOPS COULD ONLY BE KEYED ON THE BRANCH      *
      * TELLER SYSTEM. THIS SERVICE LETS THE CONTACT CENTRE (TELE) AND *
      * IBANK PLACE STOPS DIRECTLY ON MBK.PRD.CHQ.STOPS, WHICH IS READ *
      * BY THE INWARD CLEARING BATCH CQCLR010 (CQCLR01). A STOP PLACED *
      * BEFORE THE ONLINE DAY ENDS (MBKONLDN CLOSES CHQSTOP) IS        *
      * EFFECTIVE FOR THAT NIGHT'S INWARD CLEARING.                    *
      *                                                                *
      * FUNCTIONS                                                      *
      *   P  PLACE - ONE CHEQUE (SERIAL-TO BLANK / ZERO / = FROM) OR   *
      *      A RANGE OF SERIALS. AN AMOUNT MAY BE GIVEN FOR A SINGLE   *
      *      CHEQUE ONLY - CQCLR010 THEN STOPS THE CHEQUE ONLY IF THE  *
      *      PRESENTED AMOUNT MATCHES.                                 *
      *   R  REMOVE - BY THE FIRST SERIAL OF THE STOP (THE RECORD KEY) *
      *      THE RECORD IS KEPT WITH STATUS 'R' FOR THE AUDIT TRAIL.   *
      *   L  LIST - UP TO 30 STOPS, RESTARTING AFTER CHCS-SERIAL-FROM  *
      *----------------------------------------------------------------*
      * BUSINESS RULES                                                 *
      *   - CURRENT ACCOUNTS ONLY (NO CHEQUE BOOKS ON OTHER TYPES)     *
      *   - ACCOUNT NOT CLOSED. FROZEN / DORMANT ACCOUNTS MAY HAVE     *
      *     STOPS PLACED - A STOP NEVER INCREASES OUR EXPOSURE         *
      *   - A RANGE MAY NOT EXCEED WS-MAX-RANGE-REMOTE CHEQUES FROM    *
      *     THE REMOTE CHANNELS, WS-MAX-RANGE-BRANCH FROM THE BRANCH   *
      *     (A WHOLE LOST CHEQUE BOOK OR TWO)                          *
      *   - A NEW STOP MAY NOT OVERLAP AN ACTIVE STOP (SOVL)           *
      *   - AT MOST WS-MAX-ACTIVE ACTIVE STOPS PER ACCOUNT (SMAX)      *
      *   - REMOVING A STOP IS BRANCH OR TELE ONLY (FRAUD RULE         *
      *     FR-2011-12: A COMPROMISED IBANK LOGON MUST NOT BE ABLE TO  *
      *     LIFT A STOP ON A STOLEN CHEQUE BOOK)                       *
      *   - A REMOVED OR USED STOP WITH THE SAME FIRST SERIAL IS       *
      *     OVERWRITTEN BY A NEW STOP (THE KEY IS ACCOUNT + SERIAL)    *
      *----------------------------------------------------------------*
      * STOP FEE (DECISION RECORDED IN CR 12/0047)                     *
      *   THE PRODUCT TARIFF HAS NO FIELD FOR A CHEQUE STOP FEE.       *
      *   1. IF THE PRODUCT'S TRANSACTION FEE TABLE HAS AN ENTRY FOR   *
      *      CODE 'CSTP' THAT AMOUNT IS USED (PRODUCT CONTROL CAN SET  *
      *      ONE WITH A FORM PC/17 CHANGE CARD - NO COPYBOOK CHANGE).  *
      *   2. OTHERWISE PRD-STOPPED-SO-FEE IS USED. THE PUBLISHED       *
      *      TARIFF HAS ONE "STOPPED PAYMENT" FEE FOR STANDING ORDERS  *
      *      AND CHEQUES, SO THIS IS THE CORRECT DEFAULT.              *
      *   NO FEE: REASON LOST OR STOL (BANKING CODE - THE CUSTOMER IS  *
      *   NOT CHARGED FOR PROTECTING HIMSELF AGAINST FRAUD), PRODUCT   *
      *   DOES NOT LEVY CHARGES, ACCOUNT CHARGES WAIVED, OR WAIVE      *
      *   REQUESTED FROM THE BRANCH CHANNEL.                           *
      *   THE STOP IS WRITTEN BEFORE THE FEE IS TAKEN. IF THE FEE IS   *
      *   REFUSED (E.G. NSF) THE STOP STILL STANDS AND RC 0004 / FEEX  *
      *   IS RETURNED - WE NEVER REFUSE A STOP FOR WANT OF A FEE.      *
      *   THE FEE POSTING REFERENCE IS 'CQSF' + ACCOUNT + SERIAL, SO A *
      *   RESUBMITTED REQUEST IS NOT CHARGED TWICE ON THE SAME DAY.    *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   ANFD ACCOUNT NOT FOUND       ACLS ACCOUNT CLOSED             *
      *   ATYP NOT A CURRENT ACCOUNT   SNFD STOP NOT FOUND             *
      *   SNAC STOP NOT ACTIVE         SOVL OVERLAPS AN ACTIVE STOP    *
      *   SMAX TOO MANY ACTIVE STOPS   SAUT CHANNEL NOT AUTHORISED     *
      *   RNGX RANGE TOO LARGE         FEEX STOP PLACED, FEE NOT TAKEN *
      *   NONE NO STOPS TO LIST (0004) INVx INVALID REQUEST FIELD      *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2012-02-13 JNB  ORIGINAL VERSION (CR 12/0047 - CONTACT CENTRE  *
      *                 CHEQUE STOPS)                                  *
      * 2012-05-21 JNB  OVERWRITE OF A REMOVED / USED STOP WITH THE    *
      *                 SAME FIRST SERIAL (WAS DUPREC - PIR 2012/061)  *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CQSTP01 '.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-ACTIVE               PIC 9(03) VALUE 50.
           05  WS-MAX-LIST                 PIC 9(02) VALUE 30.
           05  WS-MAX-RANGE-REMOTE         PIC 9(06) VALUE 100.
           05  WS-MAX-RANGE-BRANCH         PIC 9(06) VALUE 600.
           05  WS-MAX-STOP-AMT             PIC S9(11)V99 COMP-3
                                           VALUE +999999999.99.
           05  WS-FEE-TXN-CODE             PIC X(04) VALUE 'CSTP'.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP2                    PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TIME-HHMMSS              PIC 9(06).
           05  WS-TIME-HHMMSS-X REDEFINES WS-TIME-HHMMSS
                                           PIC X(06).
           05  WS-SERIAL-FROM              PIC X(06).
           05  WS-SERIAL-FROM-N REDEFINES WS-SERIAL-FROM
                                           PIC 9(06).
           05  WS-SERIAL-TO                PIC X(06).
           05  WS-SERIAL-TO-N REDEFINES WS-SERIAL-TO
                                           PIC 9(06).
           05  WS-RANGE-SIZE               PIC 9(06).
           05  WS-MAX-RANGE                PIC 9(06).
           05  WS-AMOUNT                   PIC S9(11)V99 COMP-3.
           05  WS-ACTIVE-COUNT             PIC 9(03).
           05  WS-LIST-COUNT               PIC 9(02).
           05  WS-FEE-AMT                  PIC S9(13)V99 COMP-3.
           05  WS-NEW-AVAIL-BAL            PIC S9(13)V99 COMP-3.
           05  WS-FEE-IND                  PIC X(01).
               88  WS-FEE-NONE                 VALUE 'N'.
               88  WS-FEE-WAIVED               VALUE 'W'.
               88  WS-FEE-DUE                  VALUE 'F'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-OVERLAP-IND              PIC X(01).
               88  WS-OVERLAP                  VALUE 'Y'.
           05  WS-REUSE-IND                PIC X(01).
               88  WS-REUSE-RECORD             VALUE 'Y'.
           05  WS-MEMO-RC                  PIC X(04).
           05  WS-MEMO-REASON              PIC X(04).
           05  WS-MEMO-MESSAGE             PIC X(80).
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
       01  WS-STOP-KEY.
           05  WS-SK-SORT-CODE             PIC X(06).
           05  WS-SK-ACCT-NO               PIC X(08).
           05  WS-SK-SERIAL                PIC X(06).
       01  WS-PRD-KEY                      PIC X(10).
      *
      *---- COPY OF THE CHANNEL COMMAREA FOR THE PSMEMO1 LINK ---------*
       01  WS-SAVED-COMMAREA               PIC X(4000).
      *
      *---- THE NEW STOP, BUILT BEFORE THE OVERLAP BROWSE -------------*
       01  WS-NEW-STOP                     PIC X(100).
      *
           COPY CHQRECS.
      *
           COPY DDACCREC.
      *
           COPY PRDPARRC.
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
                                              CHCS-MORE-IND
                                              CHCS-RESTART-SERIAL
           MOVE ZERO                       TO CHCS-FEE-CHARGED
                                              CHCS-NEW-AVAIL-BAL
                                              CHCS-STOP-COUNT

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
               PERFORM 1500-READ-ACCOUNT THRU 1500-EXIT
           END-IF

           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHCS-PLACE
                       PERFORM 2000-PLACE-STOP THRU 2000-EXIT
                   WHEN CHCS-REMOVE
                       PERFORM 3000-REMOVE-STOP THRU 3000-EXIT
                   WHEN CHCS-LIST
                       PERFORM 4000-LIST-STOPS THRU 4000-EXIT
               END-EVALUATE
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
      *    NORMALISE THE SERIAL RANGE FIRST - SERIAL-TO IS OPTIONAL.
           MOVE CHCS-SERIAL-FROM           TO WS-SERIAL-FROM
           IF CHCS-SERIAL-TO = SPACES OR LOW-VALUES OR ZERO
               MOVE CHCS-SERIAL-FROM       TO WS-SERIAL-TO
           ELSE
               MOVE CHCS-SERIAL-TO         TO WS-SERIAL-TO
           END-IF
           IF CH-CHANNEL-CD = 'BRANCH'
               MOVE WS-MAX-RANGE-BRANCH    TO WS-MAX-RANGE
           ELSE
               MOVE WS-MAX-RANGE-REMOTE    TO WS-MAX-RANGE
           END-IF

           EVALUATE TRUE
               WHEN NOT (CHCS-PLACE OR CHCS-REMOVE OR CHCS-LIST)
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE P, R OR L'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCS-SORT-CODE NOT NUMERIC
               WHEN CHCS-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCS-LIST
                   CONTINUE
               WHEN WS-SERIAL-FROM NOT NUMERIC
               WHEN WS-SERIAL-FROM-N = ZERO
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'CHEQUE SERIAL NUMBER MUST BE 6 DIGITS'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCS-REMOVE
                   CONTINUE
               WHEN WS-SERIAL-TO NOT NUMERIC
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'LAST SERIAL NUMBER MUST BE 6 DIGITS'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-SERIAL-TO < WS-SERIAL-FROM
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'LAST SERIAL IS BEFORE THE FIRST SERIAL'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCS-AMOUNT NOT NUMERIC
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'CHEQUE AMOUNT NOT NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCS-AMOUNT < ZERO
               WHEN CHCS-AMOUNT > WS-MAX-STOP-AMT
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'CHEQUE AMOUNT OUTSIDE PERMITTED RANGE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCS-AMOUNT > ZERO
               AND  WS-SERIAL-TO NOT = WS-SERIAL-FROM
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'AN AMOUNT CAN ONLY BE GIVEN FOR ONE CHEQUE'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT CHCS-RSN-VALID
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'STOP REASON MUST BE LOST, STOL, DISP OR OTHR'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCS-RSN-OTHER
               AND  CHCS-PAYEE = SPACES
                   MOVE 'INVP'             TO CH-REASON-CD
                   MOVE 'PAYEE REQUIRED FOR REASON OTHR'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCS-WAIVE-FEE-IND NOT = 'Y' AND 'N' AND SPACE
                   MOVE 'INVW'             TO CH-REASON-CD
                   MOVE 'WAIVE FEE INDICATOR MUST BE Y OR N'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 1000-EXIT
           END-IF

      *---- RANGE SIZE AND CHANNEL AUTHORITY --------------------------*
           IF CHCS-PLACE
               COMPUTE WS-RANGE-SIZE = WS-SERIAL-TO-N
                                     - WS-SERIAL-FROM-N + 1
               IF WS-RANGE-SIZE > WS-MAX-RANGE
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'RNGX'             TO CH-REASON-CD
                   MOVE 'TOO MANY CHEQUES IN THE RANGE - CALL BRANCH'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF

           IF CHCS-REMOVE
           AND CH-CHANNEL-CD NOT = 'BRANCH'
           AND CH-CHANNEL-CD NOT = 'TELE  '
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'SAUT'                 TO CH-REASON-CD
               MOVE 'STOPS CAN ONLY BE REMOVED BY BRANCH OR TELEPHONE'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       1500-READ-ACCOUNT.
      *================================================================*
           MOVE CHCS-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHCS-ACCT-NO               TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE ACM-AVAIL-BAL      TO CHCS-NEW-AVAIL-BAL
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   GO TO 1500-EXIT
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 1500-EXIT
           END-EVALUATE

      *    LISTING IS ALLOWED ON ANY ACCOUNT (OLD STOPS ON A CLOSED
      *    ACCOUNT ARE STILL OF INTEREST TO THE FRAUD TEAM).
           IF CHCS-LIST
               GO TO 1500-EXIT
           END-IF

           EVALUATE TRUE
               WHEN NOT ACM-CURRENT
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ATYP'             TO CH-REASON-CD
                   MOVE 'CHEQUE STOPS ARE FOR CURRENT ACCOUNTS ONLY'
                                           TO CH-MESSAGE-TEXT
               WHEN ACM-CLOSED AND CHCS-PLACE
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ACLS'             TO CH-REASON-CD
                   MOVE 'ACCOUNT IS CLOSED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       1500-EXIT.
           EXIT.
      *================================================================*
       2000-PLACE-STOP.
      *================================================================*
      *---- BUILD THE NEW STOP ----------------------------------------*
           MOVE SPACES                     TO CQS-STOP-RECORD
           MOVE CHCS-SORT-CODE             TO CQS-SORT-CODE
           MOVE CHCS-ACCT-NO               TO CQS-ACCT-NO
           MOVE WS-SERIAL-FROM             TO CQS-SERIAL-FROM
           MOVE WS-SERIAL-TO               TO CQS-SERIAL-TO
           MOVE CHCS-AMOUNT                TO CQS-AMOUNT
                                              WS-AMOUNT
           MOVE CHCS-PAYEE                 TO CQS-PAYEE
           MOVE WS-TODAY                   TO CQS-PLACED-DATE
           MOVE CH-USER-ID                 TO CQS-PLACED-USER
           SET CQS-ACTIVE                  TO TRUE
           MOVE ZERO                       TO CQS-HIT-DATE
           MOVE CQS-STOP-RECORD            TO WS-NEW-STOP

      *---- OVERLAP AND MAXIMUM CHECKS (BROWSE ALL STOPS) -------------*
           PERFORM 2100-CHECK-EXISTING THRU 2100-EXIT
           IF NOT CH-RC-OK
               GO TO 2000-EXIT
           END-IF

      *---- WRITE (OR OVERWRITE AN INACTIVE STOP WITH THE SAME KEY) ---*
           MOVE WS-NEW-STOP                TO CQS-STOP-RECORD
           EXEC CICS WRITE
                     FILE('CHQSTOP')
                     FROM(CQS-STOP-RECORD)
                     LENGTH(LENGTH OF CQS-STOP-RECORD)
                     RIDFLD(CQS-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(DUPREC)
                   PERFORM 2200-OVERWRITE-INACTIVE THRU 2200-EXIT
                   IF NOT CH-RC-OK
                       GO TO 2000-EXIT
                   END-IF
               WHEN OTHER
                   MOVE 'CHQSTOP'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE

           MOVE 'CHEQUE STOP PLACED'       TO CH-MESSAGE-TEXT

      *---- STOP FEE --------------------------------------------------*
           PERFORM 7000-DETERMINE-FEE THRU 7000-EXIT
           IF WS-FEE-DUE
               PERFORM 7100-CHARGE-FEE
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-CHECK-EXISTING.
      *================================================================*
      *    A RANGE [F1,T1] OVERLAPS [F2,T2] UNLESS T1 < F2 OR F1 > T2.
      *    SERIALS ARE 6-DIGIT NUMERIC SO CHARACTER COMPARISON WORKS.
           MOVE 'N'                        TO WS-OVERLAP-IND
                                              WS-END-BROWSE-IND
           MOVE ZERO                       TO WS-ACTIVE-COUNT
           MOVE CHCS-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHCS-ACCT-NO               TO WS-SK-ACCT-NO
           MOVE LOW-VALUES                 TO WS-SK-SERIAL

           EXEC CICS STARTBR
                     FILE('CHQSTOP')
                     RIDFLD(WS-STOP-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   GO TO 2100-EXIT
               WHEN OTHER
                   MOVE 'CHQSTOP'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 2100-EXIT
           END-EVALUATE

           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('CHQSTOP')
                         INTO(CQS-STOP-RECORD)
                         LENGTH(LENGTH OF CQS-STOP-RECORD)
                         RIDFLD(WS-STOP-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   WHEN CQS-SORT-CODE NOT = CHCS-SORT-CODE
                   WHEN CQS-ACCT-NO NOT = CHCS-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN NOT CQS-ACTIVE
                       CONTINUE
                   WHEN OTHER
                       ADD 1               TO WS-ACTIVE-COUNT
                       IF  CQS-SERIAL-TO NOT < WS-SERIAL-FROM
                       AND CQS-SERIAL-FROM NOT > WS-SERIAL-TO
                           SET WS-OVERLAP  TO TRUE
                           SET WS-END-BROWSE
                                           TO TRUE
                       END-IF
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR
                     FILE('CHQSTOP')
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE TRUE
               WHEN WS-OVERLAP
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SOVL'             TO CH-REASON-CD
                   MOVE SPACES             TO CH-MESSAGE-TEXT
                   STRING 'OVERLAPS ACTIVE STOP '
                          CQS-SERIAL-FROM ' - ' CQS-SERIAL-TO
                          DELIMITED BY SIZE
                          INTO CH-MESSAGE-TEXT
                   END-STRING
               WHEN WS-ACTIVE-COUNT >= WS-MAX-ACTIVE
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SMAX'             TO CH-REASON-CD
                   MOVE 'MAXIMUM ACTIVE STOPS REACHED - CALL BRANCH'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2100-EXIT.
           EXIT.
      *================================================================*
       2200-OVERWRITE-INACTIVE.
      *================================================================*
      *    A STOP WITH THE SAME FIRST SERIAL EXISTS. 2100 FOUND NO
      *    OVERLAPPING ACTIVE STOP, SO IT MUST BE REMOVED OR USED -
      *    RE-READ IT FOR UPDATE AND REPLACE IT (PIR 2012/061).
           MOVE CHCS-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHCS-ACCT-NO               TO WS-SK-ACCT-NO
           MOVE WS-SERIAL-FROM             TO WS-SK-SERIAL
           EXEC CICS READ
                     FILE('CHQSTOP')
                     INTO(CQS-STOP-RECORD)
                     LENGTH(LENGTH OF CQS-STOP-RECORD)
                     RIDFLD(WS-STOP-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CHQSTOP'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 2200-EXIT
           END-IF

      *    BELT AND BRACES - ANOTHER TASK MAY HAVE JUST PLACED IT.
           IF CQS-ACTIVE
               EXEC CICS UNLOCK
                         FILE('CHQSTOP')
                         RESP(WS-RESP)
               END-EXEC
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'SOVL'                 TO CH-REASON-CD
               MOVE 'A STOP IS ALREADY ACTIVE FOR THIS CHEQUE'
                                           TO CH-MESSAGE-TEXT
               GO TO 2200-EXIT
           END-IF

           MOVE WS-NEW-STOP                TO CQS-STOP-RECORD
           EXEC CICS REWRITE
                     FILE('CHQSTOP')
                     FROM(CQS-STOP-RECORD)
                     LENGTH(LENGTH OF CQS-STOP-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CHQSTOP'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
           END-IF
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       3000-REMOVE-STOP.
      *================================================================*
           MOVE CHCS-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHCS-ACCT-NO               TO WS-SK-ACCT-NO
           MOVE WS-SERIAL-FROM             TO WS-SK-SERIAL
           EXEC CICS READ
                     FILE('CHQSTOP')
                     INTO(CQS-STOP-RECORD)
                     LENGTH(LENGTH OF CQS-STOP-RECORD)
                     RIDFLD(WS-STOP-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SNFD'             TO CH-REASON-CD
                   MOVE 'NO STOP STARTS AT THIS SERIAL NUMBER'
                                           TO CH-MESSAGE-TEXT
                   GO TO 3000-EXIT
               WHEN OTHER
                   MOVE 'CHQSTOP'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 3000-EXIT
           END-EVALUATE

           IF NOT CQS-ACTIVE
               EXEC CICS UNLOCK
                         FILE('CHQSTOP')
                         RESP(WS-RESP)
               END-EXEC
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'SNAC'                 TO CH-REASON-CD
               IF CQS-HIT
                   MOVE 'STOP ALREADY USED - CHEQUE WAS RETURNED'
                                           TO CH-MESSAGE-TEXT
               ELSE
                   MOVE 'STOP IS ALREADY REMOVED'
                                           TO CH-MESSAGE-TEXT
               END-IF
               GO TO 3000-EXIT
           END-IF

      *    THE REMOVING USER AND DATE GO IN THE HIT FIELDS - CQCLR010
      *    ONLY LOOKS AT ACTIVE STOPS SO THEY ARE NOT MISREAD.
           SET CQS-REMOVED                 TO TRUE
           MOVE WS-TODAY                   TO CQS-HIT-DATE
           EXEC CICS REWRITE
                     FILE('CHQSTOP')
                     FROM(CQS-STOP-RECORD)
                     LENGTH(LENGTH OF CQS-STOP-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CHQSTOP'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 3000-EXIT
           END-IF

           MOVE 1                          TO WS-LIST-COUNT
                                              CHCS-STOP-COUNT
           PERFORM 4100-FORMAT-ENTRY
           MOVE 'CHEQUE STOP REMOVED - NO REFUND OF THE STOP FEE'
                                           TO CH-MESSAGE-TEXT
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-LIST-STOPS.
      *================================================================*
      *    RESTART AFTER CHCS-SERIAL-FROM (SPACES / ZERO = FROM START).
           MOVE CHCS-SORT-CODE             TO WS-SK-SORT-CODE
           MOVE CHCS-ACCT-NO               TO WS-SK-ACCT-NO
           IF CHCS-SERIAL-FROM = SPACES OR LOW-VALUES OR ZERO
               MOVE LOW-VALUES             TO WS-SK-SERIAL
           ELSE
               MOVE CHCS-SERIAL-FROM       TO WS-SK-SERIAL
           END-IF
           MOVE ZERO                       TO WS-LIST-COUNT
           MOVE 'N'                        TO WS-END-BROWSE-IND

           EXEC CICS STARTBR
                     FILE('CHQSTOP')
                     RIDFLD(WS-STOP-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET WS-END-BROWSE       TO TRUE
               WHEN OTHER
                   MOVE 'CHQSTOP'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 4000-EXIT
           END-EVALUATE

           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('CHQSTOP')
                         INTO(CQS-STOP-RECORD)
                         LENGTH(LENGTH OF CQS-STOP-RECORD)
                         RIDFLD(WS-STOP-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   WHEN CQS-SORT-CODE NOT = CHCS-SORT-CODE
                   WHEN CQS-ACCT-NO NOT = CHCS-ACCT-NO
                       SET WS-END-BROWSE   TO TRUE
      *            THE RESTART KEY ITSELF WAS RETURNED LAST TIME.
                   WHEN CQS-SERIAL-FROM = CHCS-SERIAL-FROM
                       CONTINUE
                   WHEN NOT CQS-ACTIVE
                   AND  CHCS-INCL-INACTIVE NOT = 'Y'
                       CONTINUE
                   WHEN WS-LIST-COUNT >= WS-MAX-LIST
                       MOVE 'Y'            TO CHCS-MORE-IND
                       MOVE CHCSE-SERIAL-FROM (WS-MAX-LIST)
                                           TO CHCS-RESTART-SERIAL
                       SET WS-END-BROWSE   TO TRUE
                   WHEN OTHER
                       ADD 1               TO WS-LIST-COUNT
                       PERFORM 4100-FORMAT-ENTRY
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR
                     FILE('CHQSTOP')
                     RESP(WS-RESP)
           END-EXEC

           MOVE WS-LIST-COUNT              TO CHCS-STOP-COUNT
           IF WS-LIST-COUNT = ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'NONE'                 TO CH-REASON-CD
               MOVE 'NO CHEQUE STOPS FOUND' TO CH-MESSAGE-TEXT
           END-IF
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-FORMAT-ENTRY.
      *================================================================*
           MOVE CQS-SERIAL-FROM  TO CHCSE-SERIAL-FROM (WS-LIST-COUNT)
           MOVE CQS-SERIAL-TO    TO CHCSE-SERIAL-TO (WS-LIST-COUNT)
           MOVE CQS-AMOUNT       TO CHCSE-AMOUNT (WS-LIST-COUNT)
           MOVE CQS-PAYEE        TO CHCSE-PAYEE (WS-LIST-COUNT)
           MOVE CQS-PLACED-DATE  TO CHCSE-PLACED-DATE (WS-LIST-COUNT)
           MOVE CQS-PLACED-USER  TO CHCSE-PLACED-USER (WS-LIST-COUNT)
           MOVE CQS-STATUS       TO CHCSE-STATUS (WS-LIST-COUNT)
           MOVE CQS-HIT-DATE     TO CHCSE-HIT-DATE (WS-LIST-COUNT)
           .
      *================================================================*
       7000-DETERMINE-FEE.
      *================================================================*
           MOVE ZERO                       TO WS-FEE-AMT
           SET WS-FEE-NONE                 TO TRUE

           EVALUATE TRUE
               WHEN CHCS-RSN-NO-FEE
                   GO TO 7000-EXIT
               WHEN ACM-CHARGES-WAIVED
                   SET WS-FEE-WAIVED       TO TRUE
                   GO TO 7000-EXIT
               WHEN CHCS-WAIVE-FEE-IND = 'Y'
               AND  CH-CHANNEL-CD = 'BRANCH'
                   SET WS-FEE-WAIVED       TO TRUE
                   GO TO 7000-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           MOVE ACM-PROD-CD                TO WS-PRD-KEY
           EXEC CICS READ
                     FILE('PRDPARM')
                     INTO(PRD-PARAM-RECORD)
                     LENGTH(LENGTH OF PRD-PARAM-RECORD)
                     RIDFLD(WS-PRD-KEY)
                     RESP(WS-RESP)
           END-EXEC
      *    NO PRODUCT RECORD - NO FEE (THE STOP IS WHAT MATTERS).
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 7000-EXIT
           END-IF
           IF NOT PRD-LEVIES-CHARGES
               GO TO 7000-EXIT
           END-IF

           SET PRD-TFEE-IDX                TO 1
           SEARCH PRD-TXN-FEE-TABLE
               AT END
                   MOVE PRD-STOPPED-SO-FEE TO WS-FEE-AMT
               WHEN PRD-TFEE-TXN-CODE (PRD-TFEE-IDX) = WS-FEE-TXN-CODE
                   MOVE PRD-TFEE-AMOUNT (PRD-TFEE-IDX)
                                           TO WS-FEE-AMT
           END-SEARCH

           IF WS-FEE-AMT > ZERO
               SET WS-FEE-DUE              TO TRUE
           END-IF
           .
       7000-EXIT.
           EXIT.
      *================================================================*
       7100-CHARGE-FEE.
      *================================================================*
      *    MEMO DEBIT VIA PSMEMO1. CH-COMMAREA IS OVERLAID WITH THE
      *    MEMOPOST REQUEST; THE HEADER (CHANNEL, USER) IS KEPT SO THE
      *    POSTING IS ATTRIBUTED TO THE CALLER.
           MOVE CH-COMMAREA                TO WS-SAVED-COMMAREA
           MOVE 'MEMOPOST'                 TO CH-SERVICE-CD
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
                                              CH-DATA
           MOVE WS-AK-SORT-CODE            TO CHMP-SORT-CODE
           MOVE WS-AK-ACCT-NO              TO CHMP-ACCT-NO
           MOVE 'CHG '                     TO CHMP-TXN-CODE
           MOVE 'D'                        TO CHMP-DR-CR-IND
           MOVE WS-FEE-AMT                 TO CHMP-AMOUNT
           MOVE 'CHEQUE STOP FEE'          TO CHMP-NARRATIVE-1
           STRING 'CHQ ' WS-SERIAL-FROM ' TO ' WS-SERIAL-TO
                  DELIMITED BY SIZE
                  INTO CHMP-NARRATIVE-2
           END-STRING
           STRING 'CQSF' WS-AK-SORT-CODE WS-AK-ACCT-NO WS-SERIAL-FROM
                  DELIMITED BY SIZE
                  INTO CHMP-EXT-REF
           END-STRING
           MOVE WS-TODAY                   TO CHMP-VALUE-DATE
           MOVE 'N'                        TO CHMP-OVERRIDE-IND

           EXEC CICS LINK
                     PROGRAM('PSMEMO1')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           MOVE CH-RETURN-CD               TO WS-MEMO-RC
           MOVE CH-REASON-CD               TO WS-MEMO-REASON
           MOVE CH-MESSAGE-TEXT            TO WS-MEMO-MESSAGE
           MOVE CHMP-NEW-AVAIL-BAL         TO WS-NEW-AVAIL-BAL
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA

           EVALUATE TRUE
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'FEEX'             TO CH-REASON-CD
                   MOVE 'STOP PLACED - FEE NOT TAKEN (SYSTEM), REFER'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-MEMO-RC = '0000' OR '0004'
                   MOVE WS-FEE-AMT         TO CHCS-FEE-CHARGED
                   MOVE WS-NEW-AVAIL-BAL   TO CHCS-NEW-AVAIL-BAL
                   MOVE 'CHEQUE STOP PLACED - STOP FEE CHARGED'
                                           TO CH-MESSAGE-TEXT
      *        DUPL = THE FEE WAS ALREADY TAKEN FOR THIS STOP TODAY.
               WHEN WS-MEMO-REASON = 'DUPL'
                   MOVE 'CHEQUE STOP PLACED - FEE ALREADY CHARGED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'FEEX'             TO CH-REASON-CD
                   MOVE SPACES             TO CH-MESSAGE-TEXT
                   STRING 'STOP PLACED - FEE NOT TAKEN ('
                          WS-MEMO-REASON
                          ') - REFER TO BRANCH'
                          DELIMITED BY SIZE
                          INTO CH-MESSAGE-TEXT
                   END-STRING
           END-EVALUATE
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
