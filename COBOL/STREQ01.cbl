       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STREQ01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2012-06-11.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : STREQ01                                             *
      * TITLE    : AD-HOC / DUPLICATE STATEMENT REQUEST - CHANNEL      *
      *            SERVICE STMTREQ                                     *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : STMTADH  READ / BROWSE / WRITE / DELETE             *
      *            ACCTMST  READ                                       *
      *            CIFMAST  READ (DELIVERY, DECEASED CHECK)            *
      *            PRDPARM  READ (FEE, E-STATEMENT ALLOWED)            *
      * LINKS    : PSMEMO1  (DUPLICATE FEE / REFUND - 'CHG ' POSTING)  *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-STMTREQ-AREA               *
      *----------------------------------------------------------------*
      * REQUESTS ARE WRITTEN TO VSAM KSDS MBK.PRD.STMT.ADHOC (CICS     *
      * FILE STMTADH) IN THE STATEMENT REQUEST LAYOUT STREQREC, SO     *
      * THE STATEMENT BATCH NEEDS NO NEW PROGRAM: JOB STMT01 STEP      *
      * ST012 UNLOADS THE FILE (REPRO) AND ST015 MERGES IT WITH THE    *
      * CYCLE AND CLOSURE REQUESTS BEFORE STSTM020 COMPOSES THEM.      *
      * ST016 THEN RESETS THE FILE (REUSE). MBKONLDN CLOSES STMTADH    *
      * BEFORE STMT01 RUNS, SO A REQUEST IS PRODUCED THE SAME NIGHT.   *
      *   KEY = SORT CODE + ACCOUNT + CIF + STATEMENT NO (KEYS(31 0))  *
      *   NB. ST015 MUST SORT / SUM ON (1,31) NOT (1,14) OR A          *
      *   SECOND REQUEST FOR THE SAME ACCOUNT IS DROPPED (CR 12/0133). *
      *                                                                *
      * SRQ-DOC-KEY IS FILLED IN BY STSTM020, SO ON THE REQUEST IT     *
      * CARRIES WHO ASKED AND THE FEE TAKEN (WS-REQ-TAG) - STSTM010    *
      * ALREADY PUTS ITS OWN TAG THERE.                                *
      *----------------------------------------------------------------*
      * FUNCTIONS                                                      *
      *   A  AD-HOC (INTERIM) STATEMENT - FROM THE DAY AFTER THE LAST  *
      *      STATEMENT (OR THE OPENING DATE) TO TODAY, NUMBERED LAST   *
      *      STATEMENT + 1. NO FEE. STSTM030 TREATS IT AS THE LATEST   *
      *      STATEMENT, SO THE NEXT CYCLE STATEMENT STARTS TOMORROW.   *
      *   D  DUPLICATE - A COPY OF STATEMENT CHSR-STMT-NO FOR THE      *
      *      PERIOD GIVEN BY THE CHANNEL (IBANK HAS IT ON STMT_HDR).   *
      *      STSTM030 DOES NOT MOVE THE CYCLE (PERIOD-TO IS NOT NEWER).*
      *      A FEE IS CHARGED (SEE BELOW).                             *
      *   L  LIST REQUESTS STILL WAITING FOR TONIGHT'S BATCH           *
      *   X  CANCEL A WAITING REQUEST (CHSR-STMT-NO). A DUPLICATE FEE  *
      *      IS REFUNDED.                                              *
      *----------------------------------------------------------------*
      * BUSINESS RULES                                                 *
      *   - AD-HOC: ACCOUNT OPEN, DORMANT OR FROZEN (AS STSTM010).     *
      *     REFUSED IF A STATEMENT WAS ALREADY PRODUCED TODAY OR THE   *
      *     CYCLE STATEMENT IS DUE TONIGHT (IT WOULD BE THE SAME       *
      *     STATEMENT, AND ST015 WOULD DROP ONE OF THE TWO)            *
      *   - DUPLICATE: STATEMENT 1 TO ACM-LAST-STMT-NO, PERIOD WITHIN  *
      *     THE ACCOUNT'S LIFE, ENDING NO LATER THAN THE LAST          *
      *     STATEMENT DATE, AT MOST WS-MAX-PERIOD-DAYS LONG AND NOT    *
      *     OLDER THAN WS-RETAIN-DAYS (MBK.TXNHIST HOLDS 6 YEARS -     *
      *     DATA RETENTION POLICY DRP-04)                              *
      *   - CLOSED ACCOUNTS: DUPLICATES FROM BRANCH ONLY, NO FEE       *
      *   - AT MOST WS-MAX-PENDING REQUESTS WAITING PER ACCOUNT        *
      *   - NO STATEMENT FOR A DECEASED CUSTOMER (STSTM020 WOULD       *
      *     SUPPRESS IT) - BEREAVEMENT TEAM USE THE BRANCH SCREEN      *
      *----------------------------------------------------------------*
      * DUPLICATE FEE (CR 12/0133)                                     *
      *   THE PRODUCT TARIFF HAS NO DUPLICATE STATEMENT FEE FIELD.     *
      *   AN ENTRY FOR CODE 'DSTM' IN THE PRODUCT'S TRANSACTION FEE    *
      *   TABLE IS USED IF PRESENT, ELSE THE BANK TARIFF DEFAULT       *
      *   WS-DFLT-DUP-FEE. PRD-TFEE-FREE-COUNT IS NOT APPLIED ONLINE   *
      *   (NO COUNTER OF DUPLICATES ISSUED - PIR 2012/155).            *
      *   NO FEE IF THE PRODUCT LEVIES NO CHARGES, CHARGES ARE WAIVED  *
      *   ON THE ACCOUNT OR BRANCH ASKS TO WAIVE.                      *
      *   THE REQUEST IS WRITTEN FIRST; IF THE FEE IS REFUSED (E.G.    *
      *   NSF) THE REQUEST IS DELETED AND PSMEMO1'S REASON RETURNED.   *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   ANFD ACCOUNT NOT FOUND       ACLS ACCOUNT CLOSED / CLOSING   *
      *   CDEC CUSTOMER DECEASED       NSTM NO STATEMENTS ISSUED YET   *
      *   SRQT STATEMENT ALREADY PRODUCED TODAY                        *
      *   SRQC CYCLE STATEMENT DUE TONIGHT                             *
      *   SRQP REQUEST ALREADY WAITING SRQM TOO MANY WAITING           *
      *   SRQN REQUEST NOT FOUND       RETX PERIOD PAST RETENTION      *
      *   NONE NOTHING WAITING (0004)  INVx INVALID REQUEST FIELD      *
      *   (PSMEMO1 REASONS, E.G. NSF, ARE PASSED BACK FOR THE FEE)     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2012-06-11 JNB  ORIGINAL VERSION (CR 12/0133 - STATEMENTS ON   *
      *                 DEMAND FOR IBANK AND THE CONTACT CENTRE)       *
      * 2012-10-08 JNB  CANCEL WITH FEE REFUND (WAS BRANCH SCREEN ONLY)*
      * 2013-03-04 SAT  REJECT AD-HOC WHEN THE CYCLE STATEMENT IS DUE  *
      *                 THE SAME NIGHT (PIR 2013/021 - ST015 SUM)      *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'STREQ01 '.
       01  WS-CONSTANTS.
           05  WS-MAX-PENDING              PIC 9(02) VALUE 5.
           05  WS-MAX-LIST                 PIC 9(02) VALUE 10.
           05  WS-MAX-PERIOD-DAYS          PIC S9(05) COMP-3
                                           VALUE +366.
           05  WS-RETAIN-DAYS              PIC S9(05) COMP-3
                                           VALUE +2190.
           05  WS-DFLT-DUP-FEE             PIC S9(05)V99 COMP-3
                                           VALUE +5.00.
           05  WS-FEE-TXN-CODE             PIC X(04) VALUE 'DSTM'.
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TODAY-R REDEFINES WS-TODAY.
               10  WS-TD-CCYY              PIC 9(04).
               10  WS-TD-MM                PIC 9(02).
               10  WS-TD-DD                PIC 9(02).
           05  WS-TIME-HHMMSS              PIC 9(06).
           05  WS-TIME-HHMMSS-X REDEFINES WS-TIME-HHMMSS
                                           PIC X(06).
           05  WS-TODAY-INT                PIC S9(09) COMP.
           05  WS-FROM-INT                 PIC S9(09) COMP.
           05  WS-TO-INT                   PIC S9(09) COMP.
           05  WS-QUOT                     PIC 9(04).
           05  WS-REM                      PIC 9(04).
           05  WS-PENDING-COUNT            PIC 9(02).
           05  WS-LIST-COUNT               PIC 9(02).
           05  WS-FEE-AMT                  PIC S9(13)V99 COMP-3.
           05  WS-NEW-AVAIL-BAL            PIC S9(13)V99 COMP-3.
           05  WS-STMT-NO                  PIC 9(05).
           05  WS-PERIOD-FROM              PIC 9(08).
           05  WS-PERIOD-TO                PIC 9(08).
           05  WS-DR-CR-IND                PIC X(01).
           05  WS-REF-SUFFIX               PIC X(01).
           05  WS-REF-PREFIX               PIC X(04).
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-CYCLE-DUE-IND            PIC X(01).
               88  WS-CYCLE-DUE                VALUE 'Y'.
           05  WS-PRD-FOUND-IND            PIC X(01) VALUE 'N'.
               88  WS-PRD-FOUND                VALUE 'Y'.
           05  WS-MEMO-RC                  PIC X(04).
           05  WS-MEMO-REASON              PIC X(04).
           05  WS-MEMO-MESSAGE             PIC X(80).
      *---- DATE VALIDATION -------------------------------------------*
       01  WS-DATE-WORK.
           05  WS-DW-DATE                  PIC 9(08).
           05  WS-DW-DATE-R REDEFINES WS-DW-DATE.
               10  WS-DW-CCYY              PIC 9(04).
               10  WS-DW-MM                PIC 9(02).
               10  WS-DW-DD                PIC 9(02).
           05  WS-DW-MAX-DD                PIC 9(02).
           05  WS-DW-VALID-IND             PIC X(01).
               88  WS-DW-VALID                 VALUE 'Y'.
       01  WS-MONTH-DAYS-VALUES            PIC X(24)
                                   VALUE '312831303130313130313031'.
       01  WS-MONTH-DAYS-TABLE REDEFINES WS-MONTH-DAYS-VALUES.
           05  WS-MONTH-DAYS               PIC 9(02) OCCURS 12 TIMES.
      *---- REQUEST TAG - HELD IN SRQ-DOC-KEY UNTIL STSTM020 ----------*
       01  WS-REQ-TAG.
           05  TAG-REQ-TYPE                PIC X(01).
           05  TAG-REQ-DATE                PIC 9(08).
           05  TAG-CHANNEL                 PIC X(06).
           05  TAG-USER                    PIC X(08).
           05  TAG-FEE-AMT                 PIC 9(05)V99.
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
       01  WS-REQ-KEY.
           05  WS-RK-SORT-CODE             PIC X(06).
           05  WS-RK-ACCT-NO               PIC X(08).
           05  WS-RK-CIF-NO                PIC X(12).
           05  WS-RK-STMT-NO               PIC 9(05).
       01  WS-CIF-KEY                      PIC X(12).
       01  WS-PRD-KEY                      PIC X(10).
      *---- COPY OF THE CHANNEL COMMAREA FOR THE PSMEMO1 LINK ---------*
       01  WS-SAVED-COMMAREA               PIC X(4000).
           COPY STREQREC.
           COPY DDACCREC.
           COPY CFCUSREC.
           COPY PRDPARRC.
           COPY CHCOMMAR.
           COPY CHCOMMA2.
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
                                              CHSR-STMT-CYCLE
                                              CHSR-DELIVERY
           MOVE ZERO                       TO CHSR-REQ-STMT-NO
                                              CHSR-REQ-FROM
                                              CHSR-REQ-TO
                                              CHSR-FEE-CHARGED
                                              CHSR-NEW-AVAIL-BAL
                                              CHSR-LAST-STMT-NO
                                              CHSR-LAST-STMT-DATE
                                              CHSR-LIST-COUNT
                                              WS-FEE-AMT

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC
           COMPUTE WS-TODAY-INT = FUNCTION INTEGER-OF-DATE (WS-TODAY)

           PERFORM 1000-VALIDATE-REQUEST THRU 1000-EXIT

           IF CH-RC-OK
               PERFORM 1500-READ-ACCOUNT THRU 1500-EXIT
           END-IF
           IF CH-RC-OK
               PERFORM 1600-READ-CUSTOMER-PRODUCT THRU 1600-EXIT
           END-IF

           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHSR-ADHOC
                       PERFORM 2000-ADHOC-REQUEST THRU 2000-EXIT
                   WHEN CHSR-DUPLICATE
                       PERFORM 3000-DUPLICATE-REQUEST THRU 3000-EXIT
                   WHEN CHSR-LIST
                       PERFORM 4000-LIST-REQUESTS THRU 4000-EXIT
                   WHEN CHSR-CANCEL
                       PERFORM 5000-CANCEL-REQUEST THRU 5000-EXIT
               END-EVALUATE
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN NOT (CHSR-ADHOC OR CHSR-DUPLICATE OR CHSR-LIST
                         OR CHSR-CANCEL)
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE A, D, L OR X'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSR-SORT-CODE NOT NUMERIC
               WHEN CHSR-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSR-WAIVE-FEE-IND NOT = 'Y' AND 'N' AND SPACE
                   MOVE 'INVW'             TO CH-REASON-CD
                   MOVE 'WAIVE FEE INDICATOR MUST BE Y OR N'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSR-ADHOC OR CHSR-LIST
                   CONTINUE
               WHEN CHSR-STMT-NO NOT NUMERIC
               WHEN CHSR-STMT-NO = ZERO
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'STATEMENT NUMBER REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSR-CANCEL
                   CONTINUE
               WHEN CHSR-PERIOD-FROM NOT NUMERIC
               WHEN CHSR-PERIOD-TO NOT NUMERIC
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'STATEMENT PERIOD DATES NOT NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSR-PERIOD-FROM > CHSR-PERIOD-TO
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'PERIOD FROM DATE IS AFTER THE TO DATE'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 1000-EXIT
           END-IF

      *---- DUPLICATE PERIOD - REAL DATES, SENSIBLE LENGTH ------------*
           IF CHSR-DUPLICATE
               MOVE CHSR-PERIOD-FROM       TO WS-DW-DATE
               PERFORM 8500-VALIDATE-DATE THRU 8500-EXIT
               IF WS-DW-VALID
                   MOVE CHSR-PERIOD-TO     TO WS-DW-DATE
                   PERFORM 8500-VALIDATE-DATE THRU 8500-EXIT
               END-IF
               IF NOT WS-DW-VALID
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'STATEMENT PERIOD DATE IS NOT A VALID DATE'
                                           TO CH-MESSAGE-TEXT
                   GO TO 1000-EXIT
               END-IF
               COMPUTE WS-FROM-INT =
                       FUNCTION INTEGER-OF-DATE (CHSR-PERIOD-FROM)
               COMPUTE WS-TO-INT =
                       FUNCTION INTEGER-OF-DATE (CHSR-PERIOD-TO)
               IF WS-TO-INT - WS-FROM-INT + 1 > WS-MAX-PERIOD-DAYS
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'STATEMENT PERIOD LONGER THAN ONE YEAR'
                                           TO CH-MESSAGE-TEXT
                   GO TO 1000-EXIT
               END-IF
               IF WS-TODAY-INT - WS-FROM-INT > WS-RETAIN-DAYS
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'RETX'             TO CH-REASON-CD
                   MOVE 'PERIOD OLDER THAN 6 YEARS - NOT HELD'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       1500-READ-ACCOUNT.
      *================================================================*
           MOVE CHSR-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHSR-ACCT-NO               TO WS-AK-ACCT-NO
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
                   GO TO 1500-EXIT
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 1500-EXIT
           END-EVALUATE

           MOVE ACM-LAST-STMT-NO           TO CHSR-LAST-STMT-NO
           MOVE ACM-LAST-STMT-DATE         TO CHSR-LAST-STMT-DATE
           MOVE ACM-STMT-CYCLE             TO CHSR-STMT-CYCLE
           MOVE ACM-AVAIL-BAL              TO CHSR-NEW-AVAIL-BAL
           .
       1500-EXIT.
           EXIT.
      *================================================================*
       1600-READ-CUSTOMER-PRODUCT.
      *================================================================*
      *    LIST AND CANCEL DO NOT NEED THE CUSTOMER - BUT THE CIF
      *    NUMBER IS PART OF THE REQUEST KEY, SO IT COMES FROM ACCTMST.
           IF CHSR-LIST OR CHSR-CANCEL
               GO TO 1600-EXIT
           END-IF

           MOVE ACM-CIF-NO                 TO WS-CIF-KEY
           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(WS-CIF-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE SPACES             TO CIF-CUSTOMER-RECORD
               WHEN OTHER
                   MOVE 'CIFMAST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 1600-EXIT
           END-EVALUATE

           IF CIF-DECEASED
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'CDEC'                 TO CH-REASON-CD
               MOVE 'CUSTOMER DECEASED - REFER TO BEREAVEMENT TEAM'
                                           TO CH-MESSAGE-TEXT
               GO TO 1600-EXIT
           END-IF

           MOVE ACM-PROD-CD                TO WS-PRD-KEY
           EXEC CICS READ
                     FILE('PRDPARM')
                     INTO(PRD-PARAM-RECORD)
                     LENGTH(LENGTH OF PRD-PARAM-RECORD)
                     RIDFLD(WS-PRD-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   SET WS-PRD-FOUND        TO TRUE
               WHEN DFHRESP(NOTFND)
                   MOVE SPACES             TO PRD-ESTMT-ALLOWED
                                              PRD-CHG-IND
               WHEN OTHER
                   MOVE 'PRDPARM'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 1600-EXIT
           END-EVALUATE

      *    EXPECTED DELIVERY - THE SAME TEST AS STSTM020 2900. THE
      *    MARKETING E-MAIL FLAG IS THE PAPERLESS PROXY (PIR 2009/233).
           IF CIF-EMAIL-ADDR NOT = SPACES
           AND PRD-ESTMT-ALLOWED = 'Y'
           AND CIF-MKTG-EMAIL-IND = 'Y'
               SET CHSR-ELECTRONIC         TO TRUE
           ELSE
               SET CHSR-PAPER              TO TRUE
           END-IF
           .
       1600-EXIT.
           EXIT.
      *================================================================*
       2000-ADHOC-REQUEST.
      *================================================================*
           PERFORM 2100-CHECK-CYCLE-DUE THRU 2100-DONE

           EVALUATE TRUE
               WHEN NOT (ACM-OPEN OR ACM-DORMANT OR ACM-FROZEN)
                   MOVE 'ACLS'             TO CH-REASON-CD
                   MOVE 'ACCOUNT CLOSED OR CLOSING - FINAL STATEMENT'
                                           TO CH-MESSAGE-TEXT
               WHEN ACM-LAST-STMT-DATE NUMERIC
               AND  ACM-LAST-STMT-DATE NOT < WS-TODAY
                   MOVE 'SRQT'             TO CH-REASON-CD
                   MOVE 'A STATEMENT HAS ALREADY BEEN PRODUCED TODAY'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-CYCLE-DUE
                   MOVE 'SRQC'             TO CH-REASON-CD
                   MOVE 'YOUR REGULAR STATEMENT IS PRODUCED TONIGHT'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0008'                 TO CH-RETURN-CD
               GO TO 2000-EXIT
           END-IF

      *---- PERIOD AS STSTM010 WOULD SET IT ---------------------------*
           COMPUTE WS-STMT-NO = ACM-LAST-STMT-NO + 1
           IF ACM-LAST-STMT-DATE NOT NUMERIC
           OR ACM-LAST-STMT-DATE = ZERO
               MOVE ACM-OPEN-DATE          TO WS-PERIOD-FROM
           ELSE
               COMPUTE WS-PERIOD-FROM = FUNCTION DATE-OF-INTEGER (
                       FUNCTION INTEGER-OF-DATE (ACM-LAST-STMT-DATE)
                       + 1)
           END-IF
           MOVE WS-TODAY                   TO WS-PERIOD-TO
           MOVE 'A'                        TO TAG-REQ-TYPE

           PERFORM 6000-CHECK-PENDING THRU 6000-EXIT
           IF NOT CH-RC-OK
               GO TO 2000-EXIT
           END-IF

           PERFORM 6100-WRITE-REQUEST THRU 6100-EXIT
           IF NOT CH-RC-OK
               GO TO 2000-EXIT
           END-IF

           MOVE 'INTERIM STATEMENT REQUESTED - CYCLE RESTARTS TOMORROW'
                                           TO CH-MESSAGE-TEXT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-CHECK-CYCLE-DUE.
      *================================================================*
      *    THE SELECTION RULE OF STSTM010 2000-CHECK-ACCOUNT, USING
      *    TODAY AS THE BUSINESS DATE (STMT01 RUNS THE SAME NIGHT).
           MOVE 'N'                        TO WS-CYCLE-DUE-IND
           IF ACM-STMT-DAY NOT = WS-TD-DD
               GO TO 2100-DONE
           END-IF
           EVALUATE TRUE
               WHEN ACM-STMT-MONTHLY
                   SET WS-CYCLE-DUE        TO TRUE
               WHEN ACM-STMT-QUARTERLY
                   DIVIDE WS-TD-MM BY 3 GIVING WS-QUOT
                          REMAINDER WS-REM
                   IF WS-REM = 0
                       SET WS-CYCLE-DUE    TO TRUE
                   END-IF
               WHEN ACM-STMT-ANNUAL
                   IF ACM-OPEN-DATE (5:2) = WS-TODAY-X (5:2)
                       SET WS-CYCLE-DUE    TO TRUE
                   END-IF
           END-EVALUATE
           .
       2100-DONE.
           EXIT.
      *================================================================*
       3000-DUPLICATE-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN ACM-CLOSED AND CH-CHANNEL-CD NOT = 'BRANCH'
                   MOVE 'ACLS'             TO CH-REASON-CD
                   MOVE 'ACCOUNT CLOSED - ASK AT A BRANCH FOR A COPY'
                                           TO CH-MESSAGE-TEXT
               WHEN ACM-LAST-STMT-NO NOT NUMERIC
               WHEN ACM-LAST-STMT-NO = ZERO
                   MOVE 'NSTM'             TO CH-REASON-CD
                   MOVE 'NO STATEMENTS HAVE BEEN ISSUED YET'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSR-STMT-NO > ACM-LAST-STMT-NO
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'STATEMENT NUMBER NOT YET ISSUED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSR-PERIOD-TO > ACM-LAST-STMT-DATE
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'PERIOD ENDS AFTER THE LAST STATEMENT'
                                           TO CH-MESSAGE-TEXT
               WHEN CHSR-PERIOD-FROM < ACM-OPEN-DATE
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'PERIOD STARTS BEFORE THE ACCOUNT WAS OPENED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0008'                 TO CH-RETURN-CD
               GO TO 3000-EXIT
           END-IF

           MOVE CHSR-STMT-NO               TO WS-STMT-NO
           MOVE CHSR-PERIOD-FROM           TO WS-PERIOD-FROM
           MOVE CHSR-PERIOD-TO             TO WS-PERIOD-TO
           MOVE 'D'                        TO TAG-REQ-TYPE

           PERFORM 6000-CHECK-PENDING THRU 6000-EXIT
           IF NOT CH-RC-OK
               GO TO 3000-EXIT
           END-IF

           PERFORM 7000-DETERMINE-FEE THRU 7000-EXIT

           PERFORM 6100-WRITE-REQUEST THRU 6100-EXIT
           IF NOT CH-RC-OK
               GO TO 3000-EXIT
           END-IF

           MOVE 'DUPLICATE STATEMENT REQUESTED' TO CH-MESSAGE-TEXT
           IF WS-FEE-AMT = ZERO
               GO TO 3000-EXIT
           END-IF

      *---- FEE - IF REFUSED, TAKE THE REQUEST OFF THE FILE AGAIN -----*
           MOVE 'D'                        TO WS-DR-CR-IND
           MOVE 'SRQF'                     TO WS-REF-PREFIX
           MOVE 'D'                        TO WS-REF-SUFFIX
           PERFORM 7100-POST-FEE
           IF WS-MEMO-RC = '0000' OR '0004' OR WS-MEMO-REASON = 'DUPL'
               MOVE WS-FEE-AMT             TO CHSR-FEE-CHARGED
               IF WS-MEMO-REASON NOT = 'DUPL'
                   MOVE WS-NEW-AVAIL-BAL   TO CHSR-NEW-AVAIL-BAL
               END-IF
               MOVE 'DUPLICATE STATEMENT REQUESTED - FEE CHARGED'
                                           TO CH-MESSAGE-TEXT
           ELSE
               EXEC CICS DELETE
                         FILE('STMTADH')
                         RIDFLD(WS-REQ-KEY)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   EXEC CICS ABEND
                             ABCODE('SRQ1')
                   END-EXEC
               END-IF
               MOVE ZERO                   TO CHSR-REQ-STMT-NO
                                              CHSR-REQ-FROM
                                              CHSR-REQ-TO
               IF WS-MEMO-RC = SPACES
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'LNKF'             TO CH-REASON-CD
               ELSE
                   MOVE WS-MEMO-RC         TO CH-RETURN-CD
                   MOVE WS-MEMO-REASON     TO CH-REASON-CD
               END-IF
               MOVE SPACES                 TO CH-MESSAGE-TEXT
               STRING 'NOT REQUESTED - FEE COULD NOT BE TAKEN ('
                      WS-MEMO-REASON ')'
                      DELIMITED BY SIZE
                      INTO CH-MESSAGE-TEXT
               END-STRING
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-LIST-REQUESTS.
      *================================================================*
           MOVE ZERO                       TO WS-LIST-COUNT
           PERFORM 6200-START-BROWSE THRU 6200-EXIT
           IF NOT CH-RC-OK
               GO TO 4000-EXIT
           END-IF

           PERFORM UNTIL WS-END-BROWSE
               PERFORM 6300-READ-NEXT
               IF NOT WS-END-BROWSE
                   IF WS-LIST-COUNT < WS-MAX-LIST
                       ADD 1               TO WS-LIST-COUNT
                       PERFORM 4100-FORMAT-ENTRY
                   ELSE
                       SET WS-END-BROWSE   TO TRUE
                   END-IF
               END-IF
           END-PERFORM
           PERFORM 6400-END-BROWSE

           MOVE WS-LIST-COUNT              TO CHSR-LIST-COUNT
           IF WS-LIST-COUNT = ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'NONE'                 TO CH-REASON-CD
               MOVE 'NO STATEMENT REQUESTS WAITING'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-FORMAT-ENTRY.
      *================================================================*
           MOVE SRQ-DOC-KEY                TO WS-REQ-TAG
           MOVE TAG-REQ-TYPE     TO CHSRL-REQ-TYPE (WS-LIST-COUNT)
           MOVE SRQ-STMT-NO      TO CHSRL-STMT-NO (WS-LIST-COUNT)
           MOVE SRQ-PERIOD-FROM  TO CHSRL-FROM (WS-LIST-COUNT)
           MOVE SRQ-PERIOD-TO    TO CHSRL-TO (WS-LIST-COUNT)
           MOVE TAG-REQ-DATE     TO CHSRL-REQ-DATE (WS-LIST-COUNT)
           MOVE TAG-CHANNEL      TO CHSRL-CHANNEL (WS-LIST-COUNT)
           IF TAG-FEE-AMT NUMERIC AND TAG-FEE-AMT > ZERO
               MOVE 'F'          TO CHSRL-FEE-IND (WS-LIST-COUNT)
           ELSE
               MOVE 'N'          TO CHSRL-FEE-IND (WS-LIST-COUNT)
           END-IF
           .
      *================================================================*
       5000-CANCEL-REQUEST.
      *================================================================*
           MOVE CHSR-SORT-CODE             TO WS-RK-SORT-CODE
           MOVE CHSR-ACCT-NO               TO WS-RK-ACCT-NO
           MOVE ACM-CIF-NO                 TO WS-RK-CIF-NO
           MOVE CHSR-STMT-NO               TO WS-RK-STMT-NO
           EXEC CICS READ
                     FILE('STMTADH')
                     INTO(SRQ-STMT-REQUEST)
                     LENGTH(LENGTH OF SRQ-STMT-REQUEST)
                     RIDFLD(WS-REQ-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SRQN'             TO CH-REASON-CD
                   MOVE 'NO REQUEST WAITING FOR THIS STATEMENT NUMBER'
                                           TO CH-MESSAGE-TEXT
                   GO TO 5000-EXIT
               WHEN OTHER
                   MOVE 'STMTADH'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 5000-EXIT
           END-EVALUATE

           MOVE SRQ-DOC-KEY                TO WS-REQ-TAG
           MOVE SRQ-STMT-NO                TO CHSR-REQ-STMT-NO
                                              WS-STMT-NO
           MOVE SRQ-PERIOD-FROM            TO CHSR-REQ-FROM
           MOVE SRQ-PERIOD-TO              TO CHSR-REQ-TO

      *---- REFUND FIRST - THE RECORD STAYS HELD UNTIL IT IS DONE -----*
           IF TAG-FEE-AMT NUMERIC AND TAG-FEE-AMT > ZERO
               MOVE TAG-FEE-AMT            TO WS-FEE-AMT
               MOVE 'C'                    TO WS-DR-CR-IND
               MOVE 'SRQR'                 TO WS-REF-PREFIX
               MOVE 'R'                    TO WS-REF-SUFFIX
               PERFORM 7100-POST-FEE
               IF NOT (WS-MEMO-RC = '0000' OR '0004'
                       OR WS-MEMO-REASON = 'DUPL')
                   EXEC CICS UNLOCK
                             FILE('STMTADH')
                             RESP(WS-RESP)
                   END-EXEC
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'RFND'             TO CH-REASON-CD
                   MOVE 'NOT CANCELLED - FEE REFUND FAILED, TRY LATER'
                                           TO CH-MESSAGE-TEXT
                   GO TO 5000-EXIT
               END-IF
               COMPUTE CHSR-FEE-CHARGED = ZERO - WS-FEE-AMT
               MOVE WS-NEW-AVAIL-BAL       TO CHSR-NEW-AVAIL-BAL
           END-IF

           EXEC CICS DELETE
                     FILE('STMTADH')
                     RESP(WS-RESP)
           END-EXEC
      *    THE REFUND IS ALREADY POSTED - ABEND SO CHROUTER BACKS IT
      *    OUT TOGETHER WITH THE REQUEST.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND
                         ABCODE('SRQ2')
               END-EXEC
           END-IF

           IF WS-FEE-AMT > ZERO
               MOVE 'STATEMENT REQUEST CANCELLED - FEE REFUNDED'
                                           TO CH-MESSAGE-TEXT
           ELSE
               MOVE 'STATEMENT REQUEST CANCELLED'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       6000-CHECK-PENDING.
      *================================================================*
      *    SAME STATEMENT ALREADY WAITING, OR TOO MANY WAITING.
           MOVE ZERO                       TO WS-PENDING-COUNT
           PERFORM 6200-START-BROWSE THRU 6200-EXIT
           IF NOT CH-RC-OK
               GO TO 6000-EXIT
           END-IF
           PERFORM UNTIL WS-END-BROWSE
               PERFORM 6300-READ-NEXT
               IF NOT WS-END-BROWSE
                   ADD 1                   TO WS-PENDING-COUNT
                   IF SRQ-STMT-NO = WS-STMT-NO
                       MOVE '0008'         TO CH-RETURN-CD
                       MOVE 'SRQP'         TO CH-REASON-CD
                       MOVE 'THIS STATEMENT IS ALREADY REQUESTED'
                                           TO CH-MESSAGE-TEXT
                       SET WS-END-BROWSE   TO TRUE
                   END-IF
               END-IF
           END-PERFORM
           PERFORM 6400-END-BROWSE

           IF CH-RC-OK
           AND WS-PENDING-COUNT >= WS-MAX-PENDING
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'SRQM'                 TO CH-REASON-CD
               MOVE 'TOO MANY STATEMENT REQUESTS WAITING - TRY TOMORROW'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       6000-EXIT.
           EXIT.
      *================================================================*
       6100-WRITE-REQUEST.
      *================================================================*
           MOVE SPACES                     TO SRQ-STMT-REQUEST
           MOVE ACM-SORT-CODE              TO SRQ-SORT-CODE
           MOVE ACM-ACCT-NO                TO SRQ-ACCT-NO
           MOVE ACM-CIF-NO                 TO SRQ-CIF-NO
           MOVE WS-STMT-NO                 TO SRQ-STMT-NO
           MOVE WS-PERIOD-FROM             TO SRQ-PERIOD-FROM
           MOVE WS-PERIOD-TO               TO SRQ-PERIOD-TO
      *    STSTM020 USES THIS ONLY IF THE PERIOD HAS NO TRANSACTIONS.
           MOVE ACM-LEDGER-BAL             TO SRQ-CLOSING-BAL
           IF TAG-REQ-TYPE = 'A'
               SET SRQ-ADHOC               TO TRUE
           ELSE
               SET SRQ-DUPLICATE           TO TRUE
           END-IF
           MOVE WS-TODAY                   TO TAG-REQ-DATE
           MOVE CH-CHANNEL-CD              TO TAG-CHANNEL
           MOVE CH-USER-ID                 TO TAG-USER
           MOVE WS-FEE-AMT                 TO TAG-FEE-AMT
           MOVE WS-REQ-TAG                 TO SRQ-DOC-KEY

           MOVE SRQ-SORT-CODE              TO WS-RK-SORT-CODE
           MOVE SRQ-ACCT-NO                TO WS-RK-ACCT-NO
           MOVE SRQ-CIF-NO                 TO WS-RK-CIF-NO
           MOVE SRQ-STMT-NO                TO WS-RK-STMT-NO
           EXEC CICS WRITE
                     FILE('STMTADH')
                     FROM(SRQ-STMT-REQUEST)
                     LENGTH(LENGTH OF SRQ-STMT-REQUEST)
                     RIDFLD(WS-REQ-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE WS-STMT-NO         TO CHSR-REQ-STMT-NO
                   MOVE WS-PERIOD-FROM     TO CHSR-REQ-FROM
                   MOVE WS-PERIOD-TO       TO CHSR-REQ-TO
      *        ANOTHER TASK GOT IN BETWEEN 6000 AND HERE.
               WHEN DFHRESP(DUPREC)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'SRQP'             TO CH-REASON-CD
                   MOVE 'THIS STATEMENT IS ALREADY REQUESTED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'STMTADH'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
       6100-EXIT.
           EXIT.
      *================================================================*
       6200-START-BROWSE.
      *================================================================*
           MOVE 'N'                        TO WS-END-BROWSE-IND
           MOVE CHSR-SORT-CODE             TO WS-RK-SORT-CODE
           MOVE CHSR-ACCT-NO               TO WS-RK-ACCT-NO
           MOVE LOW-VALUES                 TO WS-RK-CIF-NO
           MOVE ZERO                       TO WS-RK-STMT-NO
           EXEC CICS STARTBR
                     FILE('STMTADH')
                     RIDFLD(WS-REQ-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET WS-END-BROWSE       TO TRUE
               WHEN OTHER
                   SET WS-END-BROWSE       TO TRUE
                   MOVE 'STMTADH'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
       6200-EXIT.
           EXIT.
      *================================================================*
       6300-READ-NEXT.
      *================================================================*
           EXEC CICS READNEXT
                     FILE('STMTADH')
                     INTO(SRQ-STMT-REQUEST)
                     LENGTH(LENGTH OF SRQ-STMT-REQUEST)
                     RIDFLD(WS-REQ-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
           OR SRQ-SORT-CODE NOT = CHSR-SORT-CODE
           OR SRQ-ACCT-NO NOT = CHSR-ACCT-NO
               SET WS-END-BROWSE           TO TRUE
           END-IF
           .
      *================================================================*
       6400-END-BROWSE.
      *================================================================*
           EXEC CICS ENDBR
                     FILE('STMTADH')
                     RESP(WS-RESP)
           END-EXEC
           .
      *================================================================*
       7000-DETERMINE-FEE.
      *================================================================*
           MOVE ZERO                       TO WS-FEE-AMT
           EVALUATE TRUE
               WHEN ACM-CLOSED
               WHEN ACM-CHARGES-WAIVED
               WHEN NOT WS-PRD-FOUND
               WHEN NOT PRD-LEVIES-CHARGES
                   GO TO 7000-EXIT
               WHEN CHSR-WAIVE-FEE-IND = 'Y'
               AND  CH-CHANNEL-CD = 'BRANCH'
                   GO TO 7000-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           SET PRD-TFEE-IDX                TO 1
           SEARCH PRD-TXN-FEE-TABLE
               AT END
                   MOVE WS-DFLT-DUP-FEE    TO WS-FEE-AMT
               WHEN PRD-TFEE-TXN-CODE (PRD-TFEE-IDX) = WS-FEE-TXN-CODE
                   MOVE PRD-TFEE-AMOUNT (PRD-TFEE-IDX)
                                           TO WS-FEE-AMT
           END-SEARCH
           .
       7000-EXIT.
           EXIT.
      *================================================================*
       7100-POST-FEE.
      *================================================================*
      *    FEE (DEBIT) OR REFUND (CREDIT) VIA PSMEMO1. THE REFERENCE IS
      *    PREFIX + SORT CODE + ACCOUNT + STATEMENT NO + SUFFIX, SO A
      *    RESUBMISSION ON THE SAME DAY IS NOT POSTED TWICE.
           MOVE SPACES                     TO WS-MEMO-RC
                                              WS-MEMO-REASON
           MOVE CH-COMMAREA                TO WS-SAVED-COMMAREA
           MOVE 'MEMOPOST'                 TO CH-SERVICE-CD
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
                                              CH-DATA
           MOVE CHSR-SORT-CODE             TO CHMP-SORT-CODE
           MOVE CHSR-ACCT-NO               TO CHMP-ACCT-NO
           MOVE 'CHG '                     TO CHMP-TXN-CODE
           MOVE WS-DR-CR-IND               TO CHMP-DR-CR-IND
           MOVE WS-FEE-AMT                 TO CHMP-AMOUNT
           IF WS-DR-CR-IND = 'D'
               MOVE 'DUPLICATE STATEMENT FEE'
                                           TO CHMP-NARRATIVE-1
           ELSE
               MOVE 'REFUND - DUPLICATE STATEMENT FEE'
                                           TO CHMP-NARRATIVE-1
           END-IF
           STRING 'STATEMENT NO ' WS-STMT-NO
                  DELIMITED BY SIZE
                  INTO CHMP-NARRATIVE-2
           END-STRING
           STRING WS-REF-PREFIX CHSR-SORT-CODE CHSR-ACCT-NO
                  WS-STMT-NO WS-REF-SUFFIX
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

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE CH-RETURN-CD           TO WS-MEMO-RC
               MOVE CH-REASON-CD           TO WS-MEMO-REASON
               MOVE CH-MESSAGE-TEXT        TO WS-MEMO-MESSAGE
               MOVE CHMP-NEW-AVAIL-BAL     TO WS-NEW-AVAIL-BAL
           END-IF
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA
           .
      *================================================================*
       8500-VALIDATE-DATE.
      *================================================================*
           MOVE 'N'                        TO WS-DW-VALID-IND
           IF WS-DW-DATE NOT NUMERIC
           OR WS-DW-CCYY < 1900 OR WS-DW-CCYY > 2099
           OR WS-DW-MM < 1 OR WS-DW-MM > 12
           OR WS-DW-DD < 1
               GO TO 8500-EXIT
           END-IF
           MOVE WS-MONTH-DAYS (WS-DW-MM)   TO WS-DW-MAX-DD
           IF WS-DW-MM = 2
               DIVIDE WS-DW-CCYY BY 4 GIVING WS-QUOT
                      REMAINDER WS-REM
               IF WS-REM = ZERO
                   MOVE 29                 TO WS-DW-MAX-DD
               END-IF
           END-IF
           IF WS-DW-DD NOT > WS-DW-MAX-DD
               SET WS-DW-VALID             TO TRUE
           END-IF
           .
       8500-EXIT.
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
