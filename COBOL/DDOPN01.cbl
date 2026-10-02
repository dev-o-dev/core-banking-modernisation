       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDOPN01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-05-17.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDOPN01                                             *
      * TITLE    : ACCOUNT OPENING - CHANNEL SERVICE ACCTOPEN          *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : CIFMAST  READ                                       *
      *            PRDPARM  READ                                       *
      *            ACCTCIF  BROWSE (EXISTING ACCOUNTS FOR THE CIF)     *
      *            ACCTNOC  READ UPDATE / REWRITE (NUMBER ALLOCATION)  *
      *            ACCTMST  READ / WRITE                               *
      * LINKS    : PSMEMO1  (INITIAL DEPOSIT TRANSFER - TWO POSTINGS)  *
      * CALLS    : DDMODCK  (CHECK DIGIT), DDIBAN1 (IBAN), DTEUTIL     *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-ACCTOPEN-AREA              *
      *----------------------------------------------------------------*
      * BEFORE 2010 ACCOUNTS WERE OPENED ONLY ON THE BRANCH TELLER     *
      * SYSTEM (PL/I, REGION CICSBRN1) WHICH WROTE STRAIGHT TO THE     *
      * ACCOUNT MASTER AT END OF DAY. THIS SERVICE ALLOWS IBANK TO     *
      * OPEN SAVINGS ACCOUNTS FOR EXISTING CUSTOMERS IN REAL TIME.     *
      * THE TELLER SYSTEM STILL USES ITS OWN ROUTE (PIR 2010/088).     *
      *                                                                *
      * CHECKS                                                         *
      *   - CUSTOMER ACTIVE, KYC VERIFIED (IF THE PRODUCT REQUIRES),   *
      *     CUSTOMER TYPE AND AGE WITHIN THE PRODUCT RULES             *
      *   - PRODUCT ACTIVE AND OPEN TO THIS CHANNEL                    *
      *   - NO MORE THAN PRD-MAX-PER-CUST LIVE ACCOUNTS OF THE PRODUCT *
      *   - JOINT HOLDER (IF ANY) ACTIVE AND KYC VERIFIED              *
      *   - INITIAL DEPOSIT >= PRODUCT MINIMUM, FUNDED FROM AN ACCOUNT *
      *     HELD BY THE SAME CUSTOMER                                  *
      * THE JOINT HOLDER IS RECORDED ONLY IN THE NARRATIVE OF THE      *
      * OPENING POSTING - THE ACCOUNT MASTER HAS ONE CIF NUMBER.       *
      * JOINT ACCOUNTS ARE A KNOWN GAP (PIR 1999/007 - OPEN).          *
      *                                                                *
      * UNIT OF WORK: THE ACCOUNT, THE NUMBER CONTROL AND THE TWO MEMO *
      * POSTINGS ARE ONE UOW. IF ANY STEP FAILS AFTER THE ACCOUNT IS   *
      * WRITTEN, SYNCPOINT ROLLBACK BACKS IT ALL OUT.                  *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   CNFD CUSTOMER NOT FOUND       CINA CUSTOMER NOT ACTIVE       *
      *   KYCX KYC NOT VERIFIED         CTYP WRONG CUSTOMER TYPE       *
      *   AGEX OUTSIDE PRODUCT AGE RANGE                               *
      *   PNFD PRODUCT NOT FOUND        PCLS PRODUCT CLOSED TO NEW     *
      *   PCHN PRODUCT NOT OPEN TO THIS CHANNEL                        *
      *   PMAX MAXIMUM ACCOUNTS OF THIS PRODUCT ALREADY HELD           *
      *   JNFD JOINT CUSTOMER NOT FOUND / NOT ACTIVE / KYC             *
      *   DEPX INITIAL DEPOSIT BELOW MINIMUM                           *
      *   FNDX FUNDING ACCOUNT INVALID  NSF  FUNDING ACCOUNT NSF       *
      *   NRNG NO ACCOUNT NUMBERS LEFT IN THE RANGE                    *
      *   ODRF OVERDRAFT REQUEST IGNORED - NOT BRANCH (WARNING 0004)   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-05-17 JNB  ORIGINAL VERSION (IBANK ONLINE SAVINGS OPENING)*
      * 2010-07-05 JNB  BONUS END DATE, TAX STATUS DEFAULT             *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDOPN01 '.
      *
       01  WS-CONSTANTS.
           05  WS-OD-TERM-MONTHS           PIC 9(02) VALUE 12.
           05  WS-MAX-NUMBER-TRIES         PIC 9(02) VALUE 20.
           05  WS-OUR-SC-PREFIX            PIC X(02) VALUE '40'.
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
           05  WS-TODAY-R REDEFINES WS-TODAY.
               10  WS-TD-CCYY              PIC 9(04).
               10  WS-TD-MM                PIC 9(02).
               10  WS-TD-DD                PIC 9(02).
           05  WS-TIME-HHMMSS              PIC 9(06).
           05  WS-TIME-HHMMSS-X REDEFINES WS-TIME-HHMMSS
                                           PIC X(06).
           05  WS-AGE                      PIC S9(04) COMP.
           05  WS-DOB-R.
               10  WS-DOB-CCYY             PIC 9(04).
               10  WS-DOB-MM               PIC 9(02).
               10  WS-DOB-DD               PIC 9(02).
           05  WS-SORT-CODE                PIC X(06).
           05  WS-NEW-ACCT-NO              PIC X(08).
           05  WS-HELD-COUNT               PIC 9(03).
           05  WS-TRIES                    PIC 9(02).
           05  WS-BASE                     PIC 9(07).
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-INIT-DEPOSIT             PIC S9(13)V99 COMP-3.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
           05  WS-NUMBER-OK-IND            PIC X(01).
               88  WS-NUMBER-OK                VALUE 'Y'.
           05  WS-ACCOUNT-WRITTEN-IND      PIC X(01) VALUE 'N'.
               88  WS-ACCOUNT-WRITTEN          VALUE 'Y'.
           05  WS-WARNING-CD               PIC X(04) VALUE SPACES.
           05  WS-RANGE-WARNING            PIC X(50) VALUE
               'MBKDD001W ACCOUNT NUMBER RANGE NEARLY EXHAUSTED'.
           05  WS-MONTHS                   PIC S9(04) COMP.
           05  WS-MONTH-TOTAL              PIC S9(06) COMP.
           05  WS-YEARS-ADD                PIC S9(04) COMP.
           05  WS-FUTURE-DATE              PIC 9(08).
           05  WS-FUTURE-R REDEFINES WS-FUTURE-DATE.
               10  WS-FD-CCYY              PIC 9(04).
               10  WS-FD-MM                PIC 9(02).
               10  WS-FD-DD                PIC 9(02).
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
       01  WS-CIF-KEY                      PIC X(12).
       01  WS-BROWSE-KEY                   PIC X(12).
      *
      *---- COPY OF THE CHANNEL COMMAREA FOR THE PSMEMO1 LINKS --------*
       01  WS-SAVED-COMMAREA               PIC X(4000).
      *
      *---- FUNDING ACCOUNT HOLDER, SAVED BEFORE THE ACCTCIF BROWSE ---*
       01  WS-FUND-CIF-NO                  PIC X(12).
       01  WS-OPEN-EXT-REF                 PIC X(24).
       01  WS-FUND-SORT-CODE               PIC X(06).
       01  WS-FUND-ACCT-NO                 PIC X(08).
       01  WS-JOINT-CIF-NO                 PIC X(12).
       01  WS-FUND-AVAIL                   PIC S9(13)V99 COMP-3.
      *
           COPY CFCUSREC.
      *
           COPY PRDPARRC.
      *
           COPY DDACCREC.
      *
           COPY DDNOCREC.
      *
           COPY MODPARM.
      *
           COPY IBNPARM.
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
                                              WS-SAVED-COMMAREA
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
                                              CHAO-NEW-SORT-CODE
                                              CHAO-NEW-ACCT-NO
                                              CHAO-NEW-IBAN
           MOVE ZERO                       TO CHAO-OPEN-DATE
                                              CHAO-INT-RATE-CR
                                              CHAO-OD-LIMIT
                                              CHAO-AVAIL-BAL

           PERFORM 1000-GET-DATE
           PERFORM 1100-VALIDATE-REQUEST

           IF CH-RC-OK
               PERFORM 2000-CHECK-CUSTOMER
           END-IF
           IF CH-RC-OK
               PERFORM 3000-CHECK-PRODUCT THRU 3000-EXIT
           END-IF
           IF CH-RC-OK
               PERFORM 3500-COUNT-HELD THRU 3500-EXIT
           END-IF
           IF CH-RC-OK AND CHAO-JOINT-CIF-NO NOT = SPACES
               PERFORM 4000-CHECK-JOINT
           END-IF
           IF CH-RC-OK
               PERFORM 4500-CHECK-FUNDING THRU 4500-EXIT
           END-IF
           IF CH-RC-OK
               PERFORM 5000-ALLOCATE-NUMBER THRU 5000-EXIT
           END-IF
           IF CH-RC-OK
               PERFORM 6000-WRITE-ACCOUNT
           END-IF
           IF CH-RC-OK AND WS-INIT-DEPOSIT > ZERO
               PERFORM 7000-TRANSFER-DEPOSIT THRU 7000-EXIT
           END-IF

           IF CH-RC-OK
               PERFORM 8000-BUILD-RESPONSE
           ELSE
               IF WS-ACCOUNT-WRITTEN
                   EXEC CICS SYNCPOINT ROLLBACK
                   END-EXEC
               END-IF
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-GET-DATE.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC
           .
      *================================================================*
       1100-VALIDATE-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN CHAO-CIF-NO = SPACES OR LOW-VALUES
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'CIF NUMBER REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-PROD-CD = SPACES OR LOW-VALUES
                   MOVE 'INVP'             TO CH-REASON-CD
                   MOVE 'PRODUCT CODE REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-SORT-CODE NOT = SPACES
               AND  CHAO-SORT-CODE NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-SORT-CODE NOT = SPACES
               AND  CHAO-SORT-CODE (1:2) NOT = WS-OUR-SC-PREFIX
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE IS NOT A MEGABANK SORT CODE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-STMT-CYCLE NOT = SPACE AND 'M' AND 'Q' AND 'A'
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'STATEMENT CYCLE MUST BE M, Q OR A'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-STMT-DAY NOT NUMERIC
               WHEN CHAO-STMT-DAY > 28
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'STATEMENT DAY MUST BE 01-28 (OR 00)'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-INIT-DEPOSIT NOT NUMERIC
               WHEN CHAO-INIT-DEPOSIT < ZERO
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'INITIAL DEPOSIT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-OD-LIMIT-REQ NOT NUMERIC
               WHEN CHAO-OD-LIMIT-REQ < ZERO
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'OVERDRAFT LIMIT INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-INIT-DEPOSIT > ZERO
               AND (CHAO-FUND-SORT-CODE NOT NUMERIC
                    OR CHAO-FUND-ACCT-NO NOT NUMERIC)
                   MOVE 'FNDX'             TO CH-REASON-CD
                   MOVE 'FUNDING ACCOUNT REQUIRED FOR A DEPOSIT'
                                           TO CH-MESSAGE-TEXT
               WHEN CHAO-INIT-DEPOSIT > ZERO
               AND  CHAO-EXT-REF = SPACES
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'EXTERNAL REFERENCE REQUIRED FOR A DEPOSIT'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
           ELSE
               MOVE CHAO-INIT-DEPOSIT      TO WS-INIT-DEPOSIT
               MOVE CHAO-EXT-REF           TO WS-OPEN-EXT-REF
      *        SAVED BECAUSE CH-DATA IS REUSED FOR THE PSMEMO1 LINKS.
               MOVE CHAO-FUND-SORT-CODE    TO WS-FUND-SORT-CODE
               MOVE CHAO-FUND-ACCT-NO      TO WS-FUND-ACCT-NO
               MOVE CHAO-JOINT-CIF-NO      TO WS-JOINT-CIF-NO
           END-IF
           .
      *================================================================*
       2000-CHECK-CUSTOMER.
      *================================================================*
           MOVE CHAO-CIF-NO                TO WS-CIF-KEY
           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(WS-CIF-KEY)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   IF NOT CIF-ACTIVE
                       MOVE '0008'         TO CH-RETURN-CD
                       MOVE 'CINA'         TO CH-REASON-CD
                       MOVE 'CUSTOMER RECORD IS NOT ACTIVE'
                                           TO CH-MESSAGE-TEXT
                   END-IF
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CNFD'             TO CH-REASON-CD
                   MOVE 'CUSTOMER NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'CIFMAST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE

           IF CH-RC-OK
      *        AGE IN WHOLE YEARS AT TODAY.
               MOVE CIF-DOB                TO WS-DOB-R
               IF CIF-DOB NUMERIC AND CIF-DOB > ZERO
                   COMPUTE WS-AGE = WS-TD-CCYY - WS-DOB-CCYY
                   IF WS-TD-MM < WS-DOB-MM
                   OR (WS-TD-MM = WS-DOB-MM AND WS-TD-DD < WS-DOB-DD)
                       SUBTRACT 1          FROM WS-AGE
                   END-IF
               ELSE
                   MOVE ZERO               TO WS-AGE
               END-IF
           END-IF
           .
      *================================================================*
       3000-CHECK-PRODUCT.
      *================================================================*
           MOVE CHAO-PROD-CD               TO PRD-PROD-CD
           EXEC CICS READ
                     FILE('PRDPARM')
                     INTO(PRD-PARAM-RECORD)
                     LENGTH(LENGTH OF PRD-PARAM-RECORD)
                     RIDFLD(PRD-KEY)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'PNFD'             TO CH-REASON-CD
                   MOVE 'PRODUCT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   GO TO 3000-EXIT
               WHEN OTHER
                   MOVE 'PRDPARM'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 3000-EXIT
           END-EVALUATE

           EVALUATE TRUE
               WHEN NOT PRD-ACTIVE
                   MOVE 'PCLS'             TO CH-REASON-CD
                   MOVE 'PRODUCT IS NOT AVAILABLE TO NEW CUSTOMERS'
                                           TO CH-MESSAGE-TEXT
               WHEN CH-CHANNEL-CD = 'BRANCH'
               AND  PRD-OPEN-BRANCH-IND NOT = 'Y'
               WHEN CH-CHANNEL-CD = 'IBANK '
               AND  PRD-OPEN-IBANK-IND NOT = 'Y'
               WHEN CH-CHANNEL-CD = 'TELE  '
               AND  PRD-OPEN-TELE-IND NOT = 'Y'
               WHEN CH-CHANNEL-CD = 'WAP   '
                   MOVE 'PCHN'             TO CH-REASON-CD
                   MOVE 'PRODUCT CANNOT BE OPENED THROUGH THIS CHANNEL'
                                           TO CH-MESSAGE-TEXT
               WHEN PRD-PERSONAL-ONLY AND NOT CIF-PERSONAL
               WHEN PRD-BUSINESS-ONLY AND NOT CIF-BUSINESS
                   MOVE 'CTYP'             TO CH-REASON-CD
                   MOVE 'PRODUCT NOT AVAILABLE FOR THIS CUSTOMER TYPE'
                                           TO CH-MESSAGE-TEXT
               WHEN PRD-KYC-REQUIRED = 'Y' AND NOT CIF-KYC-VERIFIED
                   MOVE 'KYCX'             TO CH-REASON-CD
                   MOVE 'CUSTOMER IDENTITY NOT VERIFIED (KYC)'
                                           TO CH-MESSAGE-TEXT
               WHEN CIF-PERSONAL
               AND  PRD-MIN-AGE > ZERO
               AND  WS-AGE < PRD-MIN-AGE
               WHEN CIF-PERSONAL
               AND  PRD-MAX-AGE > ZERO
               AND  WS-AGE > PRD-MAX-AGE
                   MOVE 'AGEX'             TO CH-REASON-CD
                   MOVE 'CUSTOMER AGE OUTSIDE PRODUCT LIMITS'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-INIT-DEPOSIT < PRD-MIN-OPEN-DEPOSIT
                   MOVE 'DEPX'             TO CH-REASON-CD
                   MOVE 'INITIAL DEPOSIT BELOW PRODUCT MINIMUM'
                                           TO CH-MESSAGE-TEXT
               WHEN PRD-MAX-BALANCE > ZERO
               AND  WS-INIT-DEPOSIT > PRD-MAX-BALANCE
                   MOVE 'DEPX'             TO CH-REASON-CD
                   MOVE 'INITIAL DEPOSIT ABOVE PRODUCT MAXIMUM'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0008'                 TO CH-RETURN-CD
               GO TO 3000-EXIT
           END-IF

      *    SORT CODE: REQUESTED (BRANCH), ELSE THE PRODUCT DEFAULT.
           IF CHAO-SORT-CODE NOT = SPACES
               MOVE CHAO-SORT-CODE         TO WS-SORT-CODE
           ELSE
               MOVE PRD-DEFAULT-SORT-CODE  TO WS-SORT-CODE
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3500-COUNT-HELD.
      *================================================================*
      *    LIVE ACCOUNTS OF THIS PRODUCT ALREADY HELD BY THE CUSTOMER.
           MOVE ZERO                       TO WS-HELD-COUNT
           IF PRD-MAX-PER-CUST = ZERO
               GO TO 3500-EXIT
           END-IF

           MOVE CHAO-CIF-NO                TO WS-BROWSE-KEY
           MOVE 'N'                        TO WS-END-BROWSE-IND
           EXEC CICS STARTBR
                     FILE('ACCTCIF')
                     RIDFLD(WS-BROWSE-KEY)
                     EQUAL
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 3500-EXIT
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
               AND ACM-CIF-NO = CHAO-CIF-NO
                   IF ACM-PROD-CD = CHAO-PROD-CD
                   AND NOT ACM-CLOSED
                       ADD 1               TO WS-HELD-COUNT
                   END-IF
               ELSE
                   SET WS-END-BROWSE       TO TRUE
               END-IF
           END-PERFORM

           EXEC CICS ENDBR
                     FILE('ACCTCIF')
                     RESP(WS-RESP)
           END-EXEC

           IF WS-HELD-COUNT >= PRD-MAX-PER-CUST
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'PMAX'                 TO CH-REASON-CD
               MOVE 'CUSTOMER ALREADY HOLDS THE MAXIMUM OF THIS PRODUCT'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       3500-EXIT.
           EXIT.
      *================================================================*
       4000-CHECK-JOINT.
      *================================================================*
           MOVE CHAO-JOINT-CIF-NO          TO WS-CIF-KEY
           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(WS-CIF-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NORMAL)
               AND  CIF-ACTIVE
               AND  CIF-KYC-VERIFIED
                   CONTINUE
               WHEN WS-RESP = DFHRESP(NORMAL)
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'JNFD'             TO CH-REASON-CD
                   MOVE 'JOINT CUSTOMER NOT FOUND, INACTIVE OR NOT KYC'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'CIFMAST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       4500-CHECK-FUNDING.
      *================================================================*
      *    OD LIMIT - BRANCH ONLY, CAPPED AT THE PRODUCT MAXIMUM.
           MOVE ZERO                       TO WS-OD-LIMIT
           IF CHAO-OD-LIMIT-REQ > ZERO
               IF CH-CHANNEL-CD = 'BRANCH'
                   IF CHAO-OD-LIMIT-REQ > PRD-OD-MAX-LIMIT
                       MOVE PRD-OD-MAX-LIMIT TO WS-OD-LIMIT
                   ELSE
                       MOVE CHAO-OD-LIMIT-REQ TO WS-OD-LIMIT
                   END-IF
               ELSE
                   MOVE 'ODRF'             TO WS-WARNING-CD
               END-IF
           END-IF

           IF WS-INIT-DEPOSIT = ZERO
               GO TO 4500-EXIT
           END-IF

           MOVE CHAO-FUND-SORT-CODE        TO WS-AK-SORT-CODE
           MOVE CHAO-FUND-ACCT-NO          TO WS-AK-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'FNDX'             TO CH-REASON-CD
                   MOVE 'FUNDING ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
               WHEN ACM-CIF-NO NOT = CHAO-CIF-NO
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'FNDX'             TO CH-REASON-CD
                   MOVE 'FUNDING ACCOUNT NOT HELD BY THIS CUSTOMER'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT ACM-OPEN
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'FNDX'             TO CH-REASON-CD
                   MOVE 'FUNDING ACCOUNT IS NOT OPEN'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
      *            THE REAL FUNDS CHECK IS DONE BY PSMEMO1. THIS EARLY
      *            CHECK AVOIDS BURNING AN ACCOUNT NUMBER.
                   MOVE ACM-CIF-NO         TO WS-FUND-CIF-NO
                   COMPUTE WS-FUND-AVAIL = ACM-AVAIL-BAL + ACM-OD-LIMIT
                   IF WS-INIT-DEPOSIT > WS-FUND-AVAIL
                       MOVE '0008'         TO CH-RETURN-CD
                       MOVE 'NSF '         TO CH-REASON-CD
                       MOVE 'INSUFFICIENT FUNDS IN FUNDING ACCOUNT'
                                           TO CH-MESSAGE-TEXT
                   END-IF
           END-EVALUATE
           .
       4500-EXIT.
           EXIT.
      *================================================================*
       5000-ALLOCATE-NUMBER.
      *================================================================*
           MOVE WS-SORT-CODE               TO NOC-SORT-CODE
           MOVE PRD-ACCT-NO-LOW (1:7)      TO NOC-RANGE-LOW
           MOVE 'STD'                      TO NOC-RANGE-ID

           EXEC CICS READ
                     FILE('ACCTNOC')
                     INTO(NOC-CONTROL-RECORD)
                     LENGTH(LENGTH OF NOC-CONTROL-RECORD)
                     RIDFLD(NOC-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'NRNG'             TO CH-REASON-CD
                   MOVE 'NO NUMBER RANGE FOR SORT CODE - CALL OPS'
                                           TO CH-MESSAGE-TEXT
                   GO TO 5000-EXIT
               WHEN OTHER
                   MOVE 'ACCTNOC'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 5000-EXIT
           END-EVALUATE

           IF NOT NOC-OPEN
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'NRNG'                 TO CH-REASON-CD
               MOVE 'ACCOUNT NUMBER RANGE EXHAUSTED OR SUSPENDED'
                                           TO CH-MESSAGE-TEXT
               EXEC CICS UNLOCK
                         FILE('ACCTNOC')
                         RESP(WS-RESP)
               END-EXEC
               GO TO 5000-EXIT
           END-IF

           MOVE ZERO                       TO WS-TRIES
           MOVE 'N'                        TO WS-NUMBER-OK-IND
           MOVE NOC-LAST-BASE              TO WS-BASE

           PERFORM UNTIL WS-NUMBER-OK
                      OR WS-TRIES >= WS-MAX-NUMBER-TRIES
                      OR NOT CH-RC-OK
               ADD 1                       TO WS-TRIES
               ADD 1                       TO WS-BASE
               IF WS-BASE > NOC-RANGE-HIGH
                   SET NOC-EXHAUSTED       TO TRUE
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'NRNG'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NUMBER RANGE EXHAUSTED'
                                           TO CH-MESSAGE-TEXT
               ELSE
                   PERFORM 5100-TRY-BASE
               END-IF
           END-PERFORM

           IF CH-RC-OK AND NOT WS-NUMBER-OK
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'NRNG'                 TO CH-REASON-CD
               MOVE 'COULD NOT ALLOCATE AN ACCOUNT NUMBER - RETRY'
                                           TO CH-MESSAGE-TEXT
           END-IF

      *    THE CONTROL RECORD IS REWRITTEN EVEN ON FAILURE SO THAT
      *    SKIPPED NUMBERS AND THE EXHAUSTED FLAG ARE KEPT.
           MOVE WS-BASE                    TO NOC-LAST-BASE
           MOVE WS-TODAY                   TO NOC-LAST-ALLOC-DATE
           MOVE CH-USER-ID                 TO NOC-LAST-ALLOC-USER
           IF WS-NUMBER-OK
               ADD 1                       TO NOC-ALLOCATED-COUNT
           END-IF
           EXEC CICS REWRITE
                     FILE('ACCTNOC')
                     FROM(NOC-CONTROL-RECORD)
                     LENGTH(LENGTH OF NOC-CONTROL-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCTNOC'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
           END-IF

           IF NOC-RANGE-HIGH - WS-BASE < NOC-WARN-THRESHOLD
               EXEC CICS WRITE OPERATOR
                         TEXT(WS-RANGE-WARNING)
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       5100-TRY-BASE.
      *================================================================*
           MOVE 'G'                        TO MOD-FUNCTION
           MOVE WS-SORT-CODE               TO MOD-SORT-CODE
           MOVE SPACES                     TO MOD-ACCT-NO
           MOVE WS-BASE                    TO MOD-ACCT-NO (1:7)
           CALL 'DDMODCK' USING MOD-PARM-BLOCK

           EVALUATE TRUE
               WHEN MOD-VALID
                   MOVE MOD-ACCT-NO        TO WS-NEW-ACCT-NO
      *            MAKE SURE THE NUMBER IS NOT ALREADY IN USE (OLD
      *            TELLER-OPENED ACCOUNTS WERE NOT ALLOCATED FROM HERE)
                   MOVE WS-SORT-CODE       TO WS-AK-SORT-CODE
                   MOVE WS-NEW-ACCT-NO     TO WS-AK-ACCT-NO
                   EXEC CICS READ
                             FILE('ACCTMST')
                             INTO(ACM-ACCOUNT-RECORD)
                             LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                             RIDFLD(WS-ACCT-KEY)
                             RESP(WS-RESP)
                   END-EXEC
                   EVALUATE WS-RESP
                       WHEN DFHRESP(NOTFND)
                           SET WS-NUMBER-OK TO TRUE
                       WHEN DFHRESP(NORMAL)
                           ADD 1           TO NOC-SKIPPED-COUNT
                       WHEN OTHER
                           MOVE 'ACCTMST'  TO WS-FILE-NAME
                           PERFORM 9000-FILE-ERROR
                   END-EVALUATE
               WHEN MOD-CANNOT-GENERATE
                   ADD 1                   TO NOC-SKIPPED-COUNT
               WHEN OTHER
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'NRNG'             TO CH-REASON-CD
                   MOVE 'CHECK DIGIT GENERATION FAILED - CALL OPS'
                                           TO CH-MESSAGE-TEXT
           END-EVALUATE
           .
      *================================================================*
       6000-WRITE-ACCOUNT.
      *================================================================*
           MOVE LOW-VALUES                 TO ACM-ACCOUNT-RECORD
           MOVE SPACES                     TO ACM-ACCOUNT-RECORD
           MOVE WS-SORT-CODE               TO ACM-SORT-CODE
           MOVE WS-NEW-ACCT-NO             TO ACM-ACCT-NO
           MOVE CHAO-CIF-NO                TO ACM-CIF-NO

           MOVE 'G'                        TO IBN-FUNCTION
           MOVE WS-SORT-CODE               TO IBN-SORT-CODE
           MOVE WS-NEW-ACCT-NO             TO IBN-ACCT-NO
           CALL 'DDIBAN1' USING IBN-PARM-BLOCK
           IF IBN-OK
               MOVE IBN-IBAN               TO ACM-IBAN
           ELSE
               MOVE SPACES                 TO ACM-IBAN
           END-IF

           MOVE PRD-ACCT-TYPE-CD           TO ACM-ACCT-TYPE-CD
           MOVE PRD-PROD-CD                TO ACM-PROD-CD
           MOVE PRD-CCY-CD                 TO ACM-CCY-CD
           SET ACM-OPEN                    TO TRUE
           MOVE ZERO                       TO ACM-LEDGER-BAL
                                              ACM-AVAIL-BAL
                                              ACM-UNCLEARED-AMT
                                              ACM-MEMO-DR-TOT
                                              ACM-MEMO-CR-TOT
                                              ACM-HOLD-AMT
           MOVE WS-OD-LIMIT                TO ACM-OD-LIMIT
           IF WS-OD-LIMIT > ZERO
               MOVE WS-OD-TERM-MONTHS      TO WS-MONTHS
               PERFORM 6100-ADD-MONTHS
               MOVE WS-FUTURE-DATE         TO ACM-OD-EXPIRY-DATE
           ELSE
               MOVE ZERO                   TO ACM-OD-EXPIRY-DATE
           END-IF

      *    RATES ARE NOT FIXED ON THE ACCOUNT - ZERO MEANS "USE THE
      *    PRODUCT TIERS" (SEE INACR010).
           MOVE ZERO                       TO ACM-INT-RATE-CR
                                              ACM-INT-RATE-DR
                                              ACM-ACCRUED-INT-CR
                                              ACM-ACCRUED-INT-DR

           MOVE WS-TODAY                   TO ACM-OPEN-DATE
           MOVE ZERO                       TO ACM-CLOSE-DATE
                                              ACM-LAST-TXN-DATE
                                              ACM-LAST-STMT-DATE
                                              ACM-LAST-STMT-NO
           IF PRD-TERM-MONTHS > ZERO
               MOVE PRD-TERM-MONTHS        TO WS-MONTHS
               PERFORM 6100-ADD-MONTHS
               MOVE WS-FUTURE-DATE         TO ACM-MATURITY-DATE
           ELSE
               MOVE ZERO                   TO ACM-MATURITY-DATE
           END-IF

           IF CHAO-STMT-CYCLE = SPACE
               MOVE PRD-DEFAULT-STMT-CYCLE TO ACM-STMT-CYCLE
           ELSE
               MOVE CHAO-STMT-CYCLE        TO ACM-STMT-CYCLE
           END-IF
           IF CHAO-STMT-DAY = ZERO
               IF PRD-DEFAULT-STMT-DAY > ZERO
                   MOVE PRD-DEFAULT-STMT-DAY TO ACM-STMT-DAY
               ELSE
      *            SPREAD STATEMENT WORKLOAD - USE THE OPENING DAY,
      *            BUT NEVER 29-31 (STSTM010 SKIPS THOSE - PIR 2005/019)
                   IF WS-TD-DD > 28
                       MOVE 28             TO ACM-STMT-DAY
                   ELSE
                       MOVE WS-TD-DD       TO ACM-STMT-DAY
                   END-IF
               END-IF
           ELSE
               MOVE CHAO-STMT-DAY          TO ACM-STMT-DAY
           END-IF

           MOVE SPACE                      TO ACM-POST-RESTRICT
                                              ACM-DORMANCY-IND
           MOVE ZERO                       TO ACM-TXN-SEQ-NO
           MOVE WS-TODAY                   TO ACM-LAST-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO ACM-LAST-MAINT-TIME
           MOVE CH-USER-ID                 TO ACM-LAST-MAINT-USER

      *---- 2010 EXTENSION --------------------------------------------*
           MOVE ZERO                       TO ACM-ACCR-CR-RESIDUE
                                              ACM-ACCR-DR-RESIDUE
                                              ACM-LAST-ACCRUAL-DATE
                                              ACM-LAST-CR-CAP-DATE
                                              ACM-LAST-DR-CAP-DATE
                                              ACM-ACCR-DR-UNARR
                                              ACM-UNARR-OD-DAYS
                                              ACM-UNPAID-ITEMS
                                              ACM-PAID-ITEMS
                                              ACM-DORMANT-DATE
                                              ACM-CLOSE-REQ-DATE
                                              ACM-ISA-SUBS-YTD
           MOVE 'N'                        TO ACM-TAX-STATUS
           MOVE WS-TODAY                   TO ACM-CHG-PERIOD-START
                                              ACM-LAST-CUST-TXN-DATE
           MOVE 'N'                        TO ACM-CHG-WAIVE-IND
           MOVE SPACES                     TO ACM-CLOSE-XFER-SORT-CODE
                                              ACM-CLOSE-XFER-ACCT-NO
                                              ACM-CLOSE-RSN
           IF PRD-CR-BONUS-MONTHS > ZERO
               MOVE PRD-CR-BONUS-MONTHS    TO WS-MONTHS
               PERFORM 6100-ADD-MONTHS
               MOVE WS-FUTURE-DATE         TO ACM-BONUS-END-DATE
           ELSE
               MOVE ZERO                   TO ACM-BONUS-END-DATE
           END-IF

           MOVE WS-SORT-CODE               TO WS-AK-SORT-CODE
           MOVE WS-NEW-ACCT-NO             TO WS-AK-ACCT-NO
           EXEC CICS WRITE
                     FILE('ACCTMST')
                     FROM(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   SET WS-ACCOUNT-WRITTEN  TO TRUE
               WHEN DFHRESP(DUPREC)
      *            ANOTHER TASK OPENED THE SAME NUMBER BETWEEN OUR CHECK
      *            AND WRITE - IMPOSSIBLE WHILE ACCTNOC IS HELD, BUT
      *            THE TELLER SYSTEM DOES NOT USE ACCTNOC.
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'DUPA'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NUMBER CLASH - PLEASE RETRY'
                                           TO CH-MESSAGE-TEXT
                   SET WS-ACCOUNT-WRITTEN  TO TRUE
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   SET WS-ACCOUNT-WRITTEN  TO TRUE
           END-EVALUATE
           .
      *================================================================*
       6100-ADD-MONTHS.
      *================================================================*
      *    WS-FUTURE-DATE = TODAY + WS-MONTHS (DAY CAPPED AT 28 TO
      *    AVOID INVALID DATES - GOOD ENOUGH FOR EXPIRY/BONUS DATES).
           MOVE WS-TODAY                   TO WS-FUTURE-DATE
           COMPUTE WS-MONTH-TOTAL = WS-FD-MM + WS-MONTHS - 1
           DIVIDE WS-MONTH-TOTAL BY 12 GIVING WS-YEARS-ADD
                                       REMAINDER WS-MONTH-TOTAL
           ADD WS-YEARS-ADD                TO WS-FD-CCYY
           COMPUTE WS-FD-MM = WS-MONTH-TOTAL + 1
           IF WS-FD-DD > 28
               MOVE 28                     TO WS-FD-DD
           END-IF
           .
      *================================================================*
       7000-TRANSFER-DEPOSIT.
      *================================================================*
      *    TWO MEMO POSTINGS IN THE SAME UOW: DEBIT THE FUNDING ACCOUNT
      *    ('TFR ') THEN CREDIT THE NEW ACCOUNT ('OPN '). PSMEMO1 DOES
      *    THE FUNDS CHECK. EACH NEEDS ITS OWN EXTERNAL REFERENCE.
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA
           MOVE 'MEMOPOST'                 TO CH-SERVICE-CD
           MOVE SPACES                     TO CH-DATA
           MOVE WS-FUND-SORT-CODE        TO CHMP-SORT-CODE
           MOVE WS-FUND-ACCT-NO          TO CHMP-ACCT-NO
           MOVE 'TFR '                     TO CHMP-TXN-CODE
           MOVE 'D'                        TO CHMP-DR-CR-IND
           MOVE WS-INIT-DEPOSIT            TO CHMP-AMOUNT
           MOVE 'TRANSFER TO NEW ACCOUNT'  TO CHMP-NARRATIVE-1
           STRING WS-SORT-CODE ' ' WS-NEW-ACCT-NO
                  DELIMITED BY SIZE
                  INTO CHMP-NARRATIVE-2
           END-STRING
           PERFORM 7100-SET-EXT-REF
           MOVE 'D'                        TO CHMP-EXT-REF (24:1)
           MOVE WS-TODAY                   TO CHMP-VALUE-DATE
           MOVE 'N'                        TO CHMP-OVERRIDE-IND

           EXEC CICS LINK
                     PROGRAM('PSMEMO1')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
           OR NOT CH-RC-OK
               PERFORM 7900-DEPOSIT-FAILED
               GO TO 7000-EXIT
           END-IF

      *---- CREDIT THE NEW ACCOUNT ------------------------------------*
           MOVE SPACES                     TO CH-DATA
           MOVE WS-SORT-CODE               TO CHMP-SORT-CODE
           MOVE WS-NEW-ACCT-NO             TO CHMP-ACCT-NO
           MOVE 'OPN '                     TO CHMP-TXN-CODE
           MOVE 'C'                        TO CHMP-DR-CR-IND
           MOVE WS-INIT-DEPOSIT            TO CHMP-AMOUNT
           MOVE 'OPENING DEPOSIT'          TO CHMP-NARRATIVE-1
           IF WS-JOINT-CIF-NO NOT = SPACES
               STRING 'JOINT WITH CIF ' WS-JOINT-CIF-NO
                      DELIMITED BY SIZE
                      INTO CHMP-NARRATIVE-2
               END-STRING
           ELSE
               STRING 'FROM ' WS-FUND-SORT-CODE ' '
                      WS-FUND-ACCT-NO
                      DELIMITED BY SIZE
                      INTO CHMP-NARRATIVE-2
               END-STRING
           END-IF
           PERFORM 7100-SET-EXT-REF
           MOVE 'C'                        TO CHMP-EXT-REF (24:1)
           MOVE WS-TODAY                   TO CHMP-VALUE-DATE
           MOVE 'N'                        TO CHMP-OVERRIDE-IND

           EXEC CICS LINK
                     PROGRAM('PSMEMO1')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
           OR NOT CH-RC-OK
               PERFORM 7900-DEPOSIT-FAILED
           END-IF
           .
       7000-EXIT.
           EXIT.
      *================================================================*
       7100-SET-EXT-REF.
      *================================================================*
      *    THE CHANNEL'S OPENING REFERENCE (FIRST 23 CHARACTERS) - THE
      *    CALLER THEN SETS POSITION 24 TO 'D' OR 'C' SO THE TWO
      *    POSTINGS HAVE DIFFERENT PSTXREF KEYS.
           MOVE SPACES                     TO CHMP-EXT-REF
           MOVE WS-OPEN-EXT-REF (1:23)     TO CHMP-EXT-REF (1:23)
           .
      *================================================================*
       7900-DEPOSIT-FAILED.
      *================================================================*
      *    KEEP PSMEMO1'S REASON (E.G. NSF) BUT MAKE THE MESSAGE CLEAR.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'LNKF'                 TO CH-REASON-CD
           END-IF
           MOVE 'ACCOUNT NOT OPENED - INITIAL DEPOSIT FAILED'
                                           TO CH-MESSAGE-TEXT
           .
      *================================================================*
       8000-BUILD-RESPONSE.
      *================================================================*
      *    RESTORE THE CHANNEL'S COMMAREA (THE MEMO LINKS OVERLAID IT)
      *    AND FILL IN THE ACCTOPEN RESPONSE.
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
           MOVE 'ACCOUNT OPENED'           TO CH-MESSAGE-TEXT
           MOVE WS-SORT-CODE               TO CHAO-NEW-SORT-CODE
           MOVE WS-NEW-ACCT-NO             TO CHAO-NEW-ACCT-NO
           MOVE IBN-IBAN                   TO CHAO-NEW-IBAN
           MOVE WS-TODAY                   TO CHAO-OPEN-DATE
           MOVE PRD-CR-TIER-RATE (1)       TO CHAO-INT-RATE-CR
           MOVE WS-OD-LIMIT                TO CHAO-OD-LIMIT
           MOVE WS-INIT-DEPOSIT            TO CHAO-AVAIL-BAL

           IF WS-WARNING-CD NOT = SPACES
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE WS-WARNING-CD          TO CH-REASON-CD
               MOVE 'ACCOUNT OPENED - OVERDRAFT REQUEST NOT ACTIONED'
                                           TO CH-MESSAGE-TEXT
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
