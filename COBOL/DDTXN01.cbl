       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDTXN01.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-03-08.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDTXN01                                             *
      * TITLE    : TRANSACTION HISTORY - CHANNEL SERVICE TXNHIST       *
      * TYPE     : CICS / DB2 - LINKED FROM CHROUTER AND DDSCRN3       *
      * PLAN     : MBKCICS1 (PACKAGE MBKCORE.DDTXN01)                  *
      * FILES    : ACCTMST  READ (ACCOUNT MUST EXIST)                  *
      * TABLES   : MBK.TXNHIST  SELECT (CURSOR)                        *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-TXNHIST-AREA               *
      *----------------------------------------------------------------*
      * RETURNS UP TO CHTH-MAX-ROWS (DEFAULT/MAX 25) HARD-POSTED       *
      * TRANSACTIONS, NEWEST FIRST, BETWEEN CHTH-FROM-DATE AND         *
      * CHTH-TO-DATE INCLUSIVE.                                        *
      *                                                                *
      * PAGING IS "KEYSET" STYLE: THE CALLER PASSES BACK THE LAST      *
      * (POST_DATE, POST_SEQ) IT RECEIVED AND THE NEXT PAGE STARTS     *
      * BELOW IT. THE CURSOR READS ONE ROW MORE THAN THE PAGE SIZE TO  *
      * KNOW WHETHER TO SET CHTH-MORE-IND.                             *
      *                                                                *
      * DATE RANGE IS LIMITED TO 400 DAYS BACK FROM TODAY - OLDER      *
      * PARTITIONS ARE ARCHIVED TO THE MI WAREHOUSE AND MAY BE ROTATED *
      * OUT (SEE MBKCORE DDL). A FROM DATE BEFORE THE LIMIT IS RAISED  *
      * TO THE LIMIT WITH WARNING 'TRNC'.                              *
      *                                                                *
      * THE CHANNEL'S ENTITLEMENT TO THE ACCOUNT IS NOT CHECKED - SAME *
      * AS ALL OTHER SERVICES (IA-IBK-002).                            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-03-08 AOS  ORIGINAL VERSION - REPLACES THE NIGHTLY        *
      *                 IBFEED02 TRANSACTION APPEND AS THE SOURCE OF   *
      *                 HISTORY FOR THE BRANCH SCREEN DD03. IBANK      *
      *                 STILL USES THE FEED (CR 10/0141 PHASE 2).      *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDTXN01 '.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-ROWS                 PIC 9(02) VALUE 25.
           05  WS-MAX-DAYS-BACK            PIC S9(05) COMP-3 VALUE -400.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-OLDEST-DATE              PIC 9(08).
           05  WS-FROM-DATE                PIC 9(08).
           05  WS-TO-DATE                  PIC 9(08).
           05  WS-ROWS-WANTED              PIC 9(02).
           05  WS-ROW-COUNT                PIC 9(02).
           05  WS-FETCH-COUNT              PIC 9(02).
           05  WS-END-CURSOR-IND           PIC X(01).
               88  WS-END-CURSOR               VALUE 'Y'.
           05  WS-WARNING-CD               PIC X(04) VALUE SPACES.
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-X REDEFINES WS-DATE-WORK.
               10  WS-DW-CCYY              PIC X(04).
               10  WS-DW-MM                PIC X(02).
               10  WS-DW-DD                PIC X(02).
           05  WS-DATE-DB2                 PIC X(10).
           05  WS-DB2-DATE-IN              PIC X(10).
           05  WS-SQLCODE-DISP             PIC -9(09).
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
      *
      *---- HOST VARIABLES --------------------------------------------*
       01  HV-SORT-CODE                    PIC X(06).
       01  HV-ACCT-NO                      PIC X(08).
       01  HV-FROM-DATE                    PIC X(10).
       01  HV-TO-DATE                      PIC X(10).
       01  HV-RESTART-DATE                 PIC X(10).
       01  HV-RESTART-SEQ                  PIC S9(09) COMP.
       01  HV-DR-CR-FILTER                 PIC X(01).
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLTXNH END-EXEC.
      *
      *    NEWEST FIRST. THE RESTART PREDICATE IS WRITTEN AS TWO ORS SO
      *    THAT XTXNH01 (SORT_CODE, ACCT_NO, POST_DATE, POST_SEQ) CAN BE
      *    USED BACKWARDS - CHECKED WITH EXPLAIN 2010-03-01 (MATCHCOLS 2
      *    PLUS SCREENING). A RESTART DATE OF 9999-12-31 MEANS PAGE 1.
           EXEC SQL DECLARE TXNCSR CURSOR FOR
               SELECT POST_DATE, POST_SEQ, TXN_CODE, DR_CR_IND,
                      TXN_AMT, RUNNING_BAL, VALUE_DATE,
                      NARRATIVE_1, NARRATIVE_2, CHANNEL_CD
                 FROM MBK.TXNHIST
                WHERE SORT_CODE = :HV-SORT-CODE
                  AND ACCT_NO   = :HV-ACCT-NO
                  AND POST_DATE BETWEEN :HV-FROM-DATE AND :HV-TO-DATE
                  AND (POST_DATE < :HV-RESTART-DATE
                       OR (POST_DATE = :HV-RESTART-DATE
                           AND POST_SEQ < :HV-RESTART-SEQ))
                  AND (:HV-DR-CR-FILTER = ' '
                       OR DR_CR_IND = :HV-DR-CR-FILTER)
                ORDER BY POST_DATE DESC, POST_SEQ DESC
                OPTIMIZE FOR 26 ROWS
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
           COPY DDACCREC.
      *
           COPY DTEPARM.
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
           MOVE ZERO                       TO CHTH-COUNT
                                              CHTH-NEXT-DATE
                                              CHTH-NEXT-SEQ
           MOVE 'N'                        TO CHTH-MORE-IND

           PERFORM 1000-VALIDATE-REQUEST THRU 1000-EXIT
           IF CH-RC-OK
               PERFORM 2000-CHECK-ACCOUNT
           END-IF
           IF CH-RC-OK
               PERFORM 3000-FETCH-HISTORY THRU 3000-EXIT
           END-IF

           IF CH-RC-OK AND WS-WARNING-CD NOT = SPACES
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE WS-WARNING-CD          TO CH-REASON-CD
               MOVE 'FROM DATE LIMITED TO 400 DAYS AGO'
                                           TO CH-MESSAGE-TEXT
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
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
           MOVE WS-MAX-DAYS-BACK           TO DTE-DAYS
           CALL 'DTEUTIL' USING DTE-PARM-BLOCK
           MOVE DTE-DATE-OUT               TO WS-OLDEST-DATE

           EVALUATE TRUE
               WHEN CHTH-SORT-CODE NOT NUMERIC
               WHEN CHTH-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHTH-FROM-DATE NOT NUMERIC
               WHEN CHTH-TO-DATE NOT NUMERIC
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'FROM / TO DATE NOT NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN CHTH-DR-CR-FILTER NOT = SPACE AND 'D' AND 'C'
                   MOVE 'INVF'             TO CH-REASON-CD
                   MOVE 'DEBIT/CREDIT FILTER MUST BE D, C OR SPACE'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 1000-EXIT
           END-IF

      *---- DEFAULTS AND LIMITS ---------------------------------------*
           MOVE CHTH-FROM-DATE             TO WS-FROM-DATE
           MOVE CHTH-TO-DATE               TO WS-TO-DATE
           IF WS-TO-DATE = ZERO OR WS-TO-DATE > WS-TODAY
               MOVE WS-TODAY               TO WS-TO-DATE
           END-IF
           IF WS-FROM-DATE = ZERO
               MOVE WS-OLDEST-DATE         TO WS-FROM-DATE
           END-IF
           IF WS-FROM-DATE < WS-OLDEST-DATE
               MOVE WS-OLDEST-DATE         TO WS-FROM-DATE
               MOVE 'TRNC'                 TO WS-WARNING-CD
           END-IF
           IF WS-FROM-DATE > WS-TO-DATE
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'INVD'                 TO CH-REASON-CD
               MOVE 'FROM DATE IS AFTER TO DATE'
                                           TO CH-MESSAGE-TEXT
               GO TO 1000-EXIT
           END-IF

           IF CHTH-MAX-ROWS NOT NUMERIC
           OR CHTH-MAX-ROWS = ZERO
           OR CHTH-MAX-ROWS > WS-MAX-ROWS
               MOVE WS-MAX-ROWS            TO WS-ROWS-WANTED
           ELSE
               MOVE CHTH-MAX-ROWS          TO WS-ROWS-WANTED
           END-IF

      *---- HOST VARIABLES --------------------------------------------*
           MOVE CHTH-SORT-CODE             TO HV-SORT-CODE
           MOVE CHTH-ACCT-NO               TO HV-ACCT-NO
           MOVE WS-FROM-DATE               TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO HV-FROM-DATE
           MOVE WS-TO-DATE                 TO WS-DATE-WORK
           PERFORM 8900-FORMAT-DB2-DATE
           MOVE WS-DATE-DB2                TO HV-TO-DATE
           MOVE CHTH-DR-CR-FILTER          TO HV-DR-CR-FILTER

           IF CHTH-RESTART-DATE NUMERIC
           AND CHTH-RESTART-DATE NOT = ZERO
               MOVE CHTH-RESTART-DATE      TO WS-DATE-WORK
               PERFORM 8900-FORMAT-DB2-DATE
               MOVE WS-DATE-DB2            TO HV-RESTART-DATE
               IF CHTH-RESTART-SEQ NUMERIC
                   MOVE CHTH-RESTART-SEQ   TO HV-RESTART-SEQ
               ELSE
                   MOVE 999999999          TO HV-RESTART-SEQ
               END-IF
           ELSE
               MOVE '9999-12-31'           TO HV-RESTART-DATE
               MOVE 999999999              TO HV-RESTART-SEQ
           END-IF
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-CHECK-ACCOUNT.
      *================================================================*
           MOVE CHTH-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHTH-ACCT-NO               TO WS-AK-ACCT-NO
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
      *        ACCTMST IS CLOSED IN THE BATCH WINDOW. HISTORY IS IN DB2
      *        AND COULD STILL BE SERVED, BUT THE EXISTENCE CHECK
      *        WOULD FAIL - ALLOW IT THROUGH WITHOUT THE CHECK.
               WHEN DFHRESP(NOTOPEN)
               WHEN DFHRESP(DISABLED)
                   CONTINUE
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       3000-FETCH-HISTORY.
      *================================================================*
           EXEC SQL OPEN TXNCSR END-EXEC
           IF SQLCODE NOT = 0
               PERFORM 9100-SQL-ERROR
               GO TO 3000-EXIT
           END-IF

           MOVE ZERO                       TO WS-ROW-COUNT
                                              WS-FETCH-COUNT
           MOVE 'N'                        TO WS-END-CURSOR-IND

           PERFORM UNTIL WS-END-CURSOR
               EXEC SQL
                   FETCH TXNCSR
                    INTO :TXH-POST-DATE, :TXH-POST-SEQ,
                         :TXH-TXN-CODE, :TXH-DR-CR-IND,
                         :TXH-TXN-AMT, :TXH-RUNNING-BAL,
                         :TXH-VALUE-DATE, :TXH-NARRATIVE-1,
                         :TXH-NARRATIVE-2, :TXH-CHANNEL-CD
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-FETCH-COUNT
                       IF WS-FETCH-COUNT > WS-ROWS-WANTED
      *                    ONE MORE THAN THE PAGE - THERE IS MORE.
                           MOVE 'Y'        TO CHTH-MORE-IND
                           SET WS-END-CURSOR TO TRUE
                       ELSE
                           ADD 1           TO WS-ROW-COUNT
                           PERFORM 3100-FORMAT-ROW
                       END-IF
                   WHEN +100
                       SET WS-END-CURSOR   TO TRUE
                   WHEN OTHER
                       PERFORM 9100-SQL-ERROR
                       SET WS-END-CURSOR   TO TRUE
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE TXNCSR END-EXEC

           MOVE WS-ROW-COUNT               TO CHTH-COUNT
           IF CHTH-MORE-IND = 'Y'
               MOVE CHTHE-POST-DATE (WS-ROW-COUNT) TO CHTH-NEXT-DATE
               MOVE CHTHE-POST-SEQ (WS-ROW-COUNT)  TO CHTH-NEXT-SEQ
           END-IF

           IF CH-RC-OK AND WS-ROW-COUNT = ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'NONE'                 TO CH-REASON-CD
               MOVE 'NO TRANSACTIONS IN THE PERIOD'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-FORMAT-ROW.
      *================================================================*
      *    DB2 DATES ARE CCYY-MM-DD - THE CHANNEL WANTS CCYYMMDD.
           MOVE TXH-POST-DATE              TO WS-DB2-DATE-IN
           PERFORM 8950-DB2-TO-NUMERIC
           MOVE WS-DATE-WORK          TO CHTHE-POST-DATE (WS-ROW-COUNT)
           MOVE TXH-POST-SEQ          TO CHTHE-POST-SEQ (WS-ROW-COUNT)
           MOVE TXH-TXN-CODE          TO CHTHE-TXN-CODE (WS-ROW-COUNT)
           MOVE TXH-DR-CR-IND         TO CHTHE-DR-CR-IND (WS-ROW-COUNT)
           MOVE TXH-TXN-AMT           TO CHTHE-AMOUNT (WS-ROW-COUNT)
           MOVE TXH-RUNNING-BAL    TO CHTHE-RUNNING-BAL (WS-ROW-COUNT)
           MOVE TXH-VALUE-DATE             TO WS-DB2-DATE-IN
           PERFORM 8950-DB2-TO-NUMERIC
           MOVE WS-DATE-WORK         TO CHTHE-VALUE-DATE (WS-ROW-COUNT)
           MOVE SPACES             TO CHTHE-NARRATIVE-1 (WS-ROW-COUNT)
                                      CHTHE-NARRATIVE-2 (WS-ROW-COUNT)
           IF TXH-NARRATIVE-1-LEN > ZERO
               MOVE TXH-NARRATIVE-1-TEXT (1:TXH-NARRATIVE-1-LEN)
                     TO CHTHE-NARRATIVE-1 (WS-ROW-COUNT)
           END-IF
           IF TXH-NARRATIVE-2-LEN > ZERO
               MOVE TXH-NARRATIVE-2-TEXT (1:TXH-NARRATIVE-2-LEN)
                     TO CHTHE-NARRATIVE-2 (WS-ROW-COUNT)
           END-IF
           MOVE TXH-CHANNEL-CD     TO CHTHE-CHANNEL-CD (WS-ROW-COUNT)
           .
      *================================================================*
       8900-FORMAT-DB2-DATE.
      *================================================================*
           MOVE SPACES                     TO WS-DATE-DB2
           STRING WS-DW-CCYY '-' WS-DW-MM '-' WS-DW-DD
                  DELIMITED BY SIZE
                  INTO WS-DATE-DB2
           END-STRING
           .
      *================================================================*
       8950-DB2-TO-NUMERIC.
      *================================================================*
           MOVE WS-DB2-DATE-IN (1:4)       TO WS-DW-CCYY
           MOVE WS-DB2-DATE-IN (6:2)       TO WS-DW-MM
           MOVE WS-DB2-DATE-IN (9:2)       TO WS-DW-DD
           .
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
           MOVE WS-RESP                    TO WS-RESP-DISPLAY
           MOVE '0012'                     TO CH-RETURN-CD
           MOVE 'FERR'                     TO CH-REASON-CD
           MOVE SPACES                     TO CH-MESSAGE-TEXT
           STRING WS-FILE-NAME
                  ' FILE ERROR - EIBRESP '
                  WS-RESP-DISPLAY
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
      *================================================================*
       9100-SQL-ERROR.
      *================================================================*
      *    -911/-913 (DEADLOCK/TIMEOUT) ARE UNLIKELY WITH UR BUT ARE
      *    REPORTED AS RETRYABLE LIKE ANY OTHER SQL ERROR.
           MOVE SQLCODE                    TO WS-SQLCODE-DISP
           MOVE '0012'                     TO CH-RETURN-CD
           MOVE 'SQLE'                     TO CH-REASON-CD
           MOVE SPACES                     TO CH-MESSAGE-TEXT
           STRING 'TRANSACTION HISTORY UNAVAILABLE - SQLCODE '
                  WS-SQLCODE-DISP
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
