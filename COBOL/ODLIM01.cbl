       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ODLIM01.
       AUTHOR.        P L WHITFIELD.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2012-04-23.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : ODLIM01                                             *
      * TITLE    : ARRANGED OVERDRAFT LIMIT - CHANNEL SERVICE ODLIMIT  *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : ACCTMST  READ / READ UPDATE / REWRITE               *
      *            CIFMAST  READ (INCOME, KYC, DATE OF BIRTH)          *
      *            PRDPARM  READ (PRODUCT MAXIMUM, RATE, FEE)          *
      * TDQ      : ODAU     OVERDRAFT DECISION AUDIT (EXTRAPARTITION)  *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-ODLIMIT-AREA               *
      *----------------------------------------------------------------*
      * BEFORE THIS SERVICE AN OVERDRAFT COULD ONLY BE SET ON THE      *
      * BRANCH TELLER SYSTEM OR AT ACCOUNT OPENING (DDOPN01, BRANCH    *
      * ONLY). IBANK AND THE CONTACT CENTRE CAN NOW REQUEST, CHANGE    *
      * AND REMOVE AN ARRANGED LIMIT WITHIN THE RULES BELOW; ANYTHING  *
      * OUTSIDE THEM IS REFERRED TO A BRANCH (DECISION 'R').           *
      *                                                                *
      * FUNCTIONS                                                      *
      *   E  ENQUIRE - CURRENT LIMIT, PRODUCT MAXIMUM AND THE MAXIMUM  *
      *      THE CUSTOMER IS ELIGIBLE FOR. NO UPDATE.                  *
      *   C  REQUEST / CHANGE - SET THE LIMIT TO CHOD-NEW-LIMIT WITH   *
      *      EXPIRY CHOD-EXPIRY-DATE (ZERO = TODAY + 12 MONTHS). THE   *
      *      SAME LIMIT WITH A NEW EXPIRY IS A RENEWAL.                *
      *   X  REMOVE - LIMIT AND EXPIRY SET TO ZERO                     *
      *                                                                *
      * THE AVAILABLE BALANCE IS NOT CHANGED - IT EXCLUDES THE LIMIT   *
      * (SEE DDACCREC). PSMEMO1 IGNORES A LIMIT PAST ITS EXPIRY DATE,  *
      * SO AN EXPIRED LIMIT IS TREATED AS ZERO HERE AS WELL.           *
      *----------------------------------------------------------------*
      * AFFORDABILITY (CREDIT POLICY CP-OD-2012 V1.3)                  *
      *   NET MONTHLY   = CIF-ANN-INCOME / 12 * WS-NET-FACTOR          *
      *   DISPOSABLE    = NET MONTHLY - DECLARED MONTHLY COMMITMENTS   *
      *   MAX ELIGIBLE  = LOWEST OF  PRD-OD-MAX-LIMIT                  *
      *                              ANNUAL INCOME * WS-INCOME-PCT     *
      *                              DISPOSABLE * WS-DISP-MULTIPLE     *
      *                   ROUNDED DOWN TO A MULTIPLE OF WS-LIMIT-UNIT  *
      * ONLY AN INCREASE IS ASSESSED. A DECREASE, RENEWAL OR REMOVAL   *
      * IS ALWAYS AFFORDABLE.                                          *
      *----------------------------------------------------------------*
      * INCREASE RULES (IN THIS ORDER)                                 *
      *   - CURRENT ACCOUNT, OPEN, NO POSTING RESTRICTION              *
      *   - PRODUCT OFFERS AN OVERDRAFT (PRD-OD-MAX-LIMIT > 0)         *
      *   - CUSTOMER ACTIVE, PERSONAL, KYC VERIFIED, AGED 18 OR OVER   *
      *   - HIGH RISK CUSTOMERS AND BUSINESS CUSTOMERS ARE REFERRED    *
      *   - INCOME RECORDED ON THE CIF (ELSE REFER - UPDATE VIA        *
      *     CIFMAINT FUNCTION E FIRST)                                 *
      *   - WITHIN THE PRODUCT MAXIMUM AND THE AFFORDABLE MAXIMUM      *
      *   - REMOTE CHANNELS: ACCOUNT OPEN AT LEAST WS-MIN-ACCT-DAYS,   *
      *     NO UNPAID ITEMS OR UNARRANGED DAYS THIS CHARGING PERIOD,   *
      *     NEW LIMIT NOT OVER WS-REMOTE-MAX-LIMIT - ELSE REFER        *
      *   - BRANCH: CONDUCT RULES MAY BE OVERRIDDEN (CHOD-OVERRIDE-IND)*
      *     A NEW LIMIT OVER WS-FOUR-EYES-LIMIT NEEDS A SECOND USER IN *
      *     CHOD-AUTH-USER (NOT THE REQUESTING USER)                   *
      * DECREASE / REMOVE                                              *
      *   - THE NEW LIMIT MAY NOT BE LESS THAN THE AMOUNT THE ACCOUNT  *
      *     IS ALREADY OVERDRAWN (IT WOULD PUT THE CUSTOMER STRAIGHT   *
      *     INTO UNARRANGED OVERDRAFT) - BRANCH MAY OVERRIDE           *
      *----------------------------------------------------------------*
      * EVERY CHANGE AND EVERY DECLINE / REFERRAL IS WRITTEN TO TDQ    *
      * ODAU (MBK.PRD.CICS.ODAU) FOR CREDIT RISK MI AND THE CONSUMER   *
      * CREDIT FILE. ENQUIRIES ARE NOT AUDITED.                        *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   ANFD ACCOUNT NOT FOUND       ACLS ACCOUNT NOT OPEN           *
      *   ATYP NOT A CURRENT ACCOUNT   RSTD POSTINGS RESTRICTED        *
      *   PNOD PRODUCT HAS NO OVERDRAFT                                *
      *   CNFD CUSTOMER NOT FOUND      CINA CUSTOMER NOT ACTIVE        *
      *   KYCX KYC NOT VERIFIED        AGEX UNDER 18                   *
      *   RISK HIGH RISK - REFER       BUSN BUSINESS CUSTOMER - REFER  *
      *   INCX NO INCOME ON FILE       PMAX OVER PRODUCT MAXIMUM       *
      *   AFFX NOT AFFORDABLE          NEWA ACCOUNT TOO NEW - REFER    *
      *   COND ACCOUNT CONDUCT - REFER BRCH OVER REMOTE LIMIT - REFER  *
      *   AUTH SECOND AUTHORISER NEEDED                                *
      *   ODBL LIMIT BELOW THE OVERDRAWN BALANCE                       *
      *   NOOD NO LIMIT TO REMOVE (0004)                               *
      *   INVx INVALID REQUEST FIELD   FUNC INVALID FUNCTION           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2012-04-23 PLW  ORIGINAL VERSION (CR 12/0102 - ONLINE          *
      *                 OVERDRAFTS)                                    *
      * 2012-07-16 PLW  EXPIRED LIMIT TREATED AS ZERO WHEN DECIDING    *
      *                 WHETHER A REQUEST IS AN INCREASE (PIR 2012/140)*
      * 2013-01-14 JNB  BUSINESS CUSTOMERS REFERRED (CP-OD-2012 V1.3)  *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'ODLIM01 '.
      *
      *---- CREDIT POLICY CP-OD-2012 - CHANGE ONLY WITH CREDIT RISK ---*
       01  WS-CONSTANTS.
           05  WS-REMOTE-MAX-LIMIT         PIC S9(13)V99 COMP-3
                                           VALUE +2500.00.
           05  WS-FOUR-EYES-LIMIT          PIC S9(13)V99 COMP-3
                                           VALUE +5000.00.
           05  WS-ABS-MAX-LIMIT            PIC S9(13)V99 COMP-3
                                           VALUE +99999.00.
           05  WS-INCOME-PCT               PIC S9(01)V9(04) COMP-3
                                           VALUE +0.1500.
           05  WS-NET-FACTOR               PIC S9(01)V9(04) COMP-3
                                           VALUE +0.7000.
           05  WS-DISP-MULTIPLE            PIC S9(03) COMP-3
                                           VALUE +3.
           05  WS-LIMIT-UNIT               PIC S9(05) COMP-3
                                           VALUE +50.
           05  WS-MIN-AGE                  PIC 9(03) VALUE 18.
           05  WS-MIN-ACCT-DAYS            PIC S9(05) COMP-3
                                           VALUE +90.
           05  WS-DEFAULT-TERM-MONTHS      PIC 9(02) VALUE 12.
           05  WS-MAX-TERM-MONTHS          PIC 9(02) VALUE 24.
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
           05  WS-TODAY-INT                PIC S9(09) COMP.
           05  WS-WORK-INT                 PIC S9(09) COMP.
           05  WS-AGE                      PIC S9(04) COMP.
           05  WS-CURR-LIMIT               PIC S9(13)V99 COMP-3.
           05  WS-EFF-LIMIT                PIC S9(13)V99 COMP-3.
           05  WS-NEW-LIMIT                PIC S9(13)V99 COMP-3.
           05  WS-NEW-EXPIRY               PIC 9(08).
           05  WS-MAX-EXPIRY               PIC 9(08).
           05  WS-OVERDRAWN-AMT            PIC S9(13)V99 COMP-3.
           05  WS-GROSS-MONTHLY            PIC S9(13)V99 COMP-3.
           05  WS-NET-MONTHLY              PIC S9(13)V99 COMP-3.
           05  WS-COMMITMENTS              PIC S9(13)V99 COMP-3.
           05  WS-DISPOSABLE               PIC S9(13)V99 COMP-3.
           05  WS-ELIG-INCOME              PIC S9(13)V99 COMP-3.
           05  WS-ELIG-DISP                PIC S9(13)V99 COMP-3.
           05  WS-MAX-ELIGIBLE             PIC S9(13)V99 COMP-3.
           05  WS-UNITS                    PIC S9(11) COMP-3.
           05  WS-REMAINDER                PIC S9(05)V99 COMP-3.
           05  WS-ACCOUNT-HELD-IND         PIC X(01) VALUE 'N'.
               88  WS-ACCOUNT-HELD             VALUE 'Y'.
           05  WS-CIF-FOUND-IND            PIC X(01) VALUE 'N'.
               88  WS-CIF-FOUND                VALUE 'Y'.
           05  WS-INCREASE-IND             PIC X(01) VALUE 'N'.
               88  WS-INCREASE                 VALUE 'Y'.
           05  WS-AUDIT-IND                PIC X(01) VALUE 'N'.
               88  WS-AUDIT-REQUIRED           VALUE 'Y'.
           05  WS-OVERRIDE-IND             PIC X(01) VALUE 'N'.
               88  WS-BRANCH-OVERRIDE          VALUE 'Y'.
      *
      *---- DATE VALIDATION / MONTH ARITHMETIC ------------------------*
       01  WS-DATE-WORK.
           05  WS-DW-DATE                  PIC 9(08).
           05  WS-DW-DATE-R REDEFINES WS-DW-DATE.
               10  WS-DW-CCYY              PIC 9(04).
               10  WS-DW-MM                PIC 9(02).
               10  WS-DW-DD                PIC 9(02).
           05  WS-DW-MONTHS                PIC 9(03).
           05  WS-DW-TOTAL-MM              PIC 9(06).
           05  WS-DW-QUOT                  PIC 9(04).
           05  WS-DW-REM                   PIC 9(04).
           05  WS-DW-MAX-DD                PIC 9(02).
           05  WS-DW-VALID-IND             PIC X(01).
               88  WS-DW-VALID                 VALUE 'Y'.
       01  WS-MONTH-DAYS-VALUES            PIC X(24)
                                   VALUE '312831303130313130313031'.
       01  WS-MONTH-DAYS-TABLE REDEFINES WS-MONTH-DAYS-VALUES.
           05  WS-MONTH-DAYS               PIC 9(02) OCCURS 12 TIMES.
      *
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
       01  WS-CIF-KEY                      PIC X(12).
       01  WS-PRD-KEY                      PIC X(10).
      *
      *---- AUDIT RECORD - TDQ ODAU (RECFM=FB LRECL=250) --------------*
       01  WS-AUDIT-RECORD.
           05  ODA-DATE                    PIC 9(08).
           05  ODA-TIME                    PIC 9(06).
           05  ODA-TASKNO                  PIC 9(07).
           05  ODA-CHANNEL                 PIC X(06).
           05  ODA-USER                    PIC X(08).
           05  ODA-AUTH-USER               PIC X(08).
           05  ODA-REQUEST-ID              PIC X(24).
           05  ODA-FUNCTION                PIC X(01).
           05  ODA-SORT-CODE               PIC X(06).
           05  ODA-ACCT-NO                 PIC X(08).
           05  ODA-CIF-NO                  PIC X(12).
           05  ODA-PROD-CD                 PIC X(10).
           05  ODA-OLD-LIMIT               PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  ODA-OLD-EXPIRY              PIC 9(08).
           05  ODA-REQ-LIMIT               PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  ODA-NEW-LIMIT               PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  ODA-NEW-EXPIRY              PIC 9(08).
           05  ODA-MAX-ELIGIBLE            PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  ODA-ANN-INCOME              PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  ODA-COMMITMENTS             PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  ODA-DECISION                PIC X(01).
           05  ODA-RETURN-CD               PIC X(04).
           05  ODA-REASON-CD               PIC X(04).
           05  ODA-REQ-REASON              PIC X(04).
           05  ODA-OVERRIDE-IND            PIC X(01).
           05  FILLER                      PIC X(20).
      *
           COPY DDACCREC.
      *
           COPY CFCUSREC.
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
                                              CHOD-DECISION
                                              CHOD-DECISION-RSN
           MOVE ZERO                       TO CHOD-OLD-LIMIT
                                              CHOD-OLD-EXPIRY
                                              CHOD-CURR-LIMIT
                                              CHOD-CURR-EXPIRY
                                              CHOD-MAX-ELIGIBLE
                                              CHOD-PROD-MAX
                                              CHOD-AVAIL-BAL
                                              CHOD-SPEND-POWER
                                              CHOD-ARR-RATE
                                              CHOD-MONTHLY-FEE

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC
           COMPUTE WS-TODAY-INT = FUNCTION INTEGER-OF-DATE (WS-TODAY)
           MOVE ZERO                       TO WS-MAX-ELIGIBLE
                                              WS-COMMITMENTS
                                              WS-NEW-LIMIT
                                              WS-NEW-EXPIRY

           PERFORM 1000-VALIDATE-REQUEST THRU 1000-EXIT

           IF CH-RC-OK
               PERFORM 2000-READ-ACCOUNT THRU 2000-EXIT
           END-IF
           IF CH-RC-OK
               PERFORM 2100-READ-PRODUCT
           END-IF
           IF CH-RC-OK
               PERFORM 2200-READ-CUSTOMER
           END-IF
           IF CH-RC-OK
               PERFORM 3000-ACCOUNT-RULES
           END-IF
           IF CH-RC-OK
               PERFORM 4000-CALC-ELIGIBILITY THRU 4000-EXIT
           END-IF

           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHOD-ENQUIRE
                       MOVE 'OVERDRAFT DETAILS RETURNED'
                                           TO CH-MESSAGE-TEXT
                   WHEN CHOD-CHANGE
                       SET WS-AUDIT-REQUIRED
                                           TO TRUE
                       PERFORM 5000-CHANGE-LIMIT THRU 5000-EXIT
                   WHEN CHOD-REMOVE
                       SET WS-AUDIT-REQUIRED
                                           TO TRUE
                       PERFORM 6000-REMOVE-LIMIT THRU 6000-EXIT
               END-EVALUATE
           END-IF

           IF WS-ACCOUNT-HELD
               EXEC CICS UNLOCK
                         FILE('ACCTMST')
                         RESP(WS-RESP)
               END-EXEC
           END-IF

           IF WS-AUDIT-REQUIRED
               PERFORM 8000-WRITE-AUDIT
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
           EVALUATE TRUE
               WHEN NOT (CHOD-ENQUIRE OR CHOD-CHANGE OR CHOD-REMOVE)
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE E, C OR X'
                                           TO CH-MESSAGE-TEXT
               WHEN CHOD-SORT-CODE NOT NUMERIC
               WHEN CHOD-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHOD-MTHLY-COMMIT NOT NUMERIC
               WHEN CHOD-MTHLY-COMMIT < ZERO
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'MONTHLY COMMITMENTS INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHOD-OVERRIDE-IND NOT = 'Y' AND 'N' AND SPACE
                   MOVE 'INVO'             TO CH-REASON-CD
                   MOVE 'OVERRIDE INDICATOR MUST BE Y OR N'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT CHOD-CHANGE
                   CONTINUE
               WHEN CHOD-NEW-LIMIT NOT NUMERIC
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'NEW LIMIT NOT NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN CHOD-NEW-LIMIT NOT > ZERO
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'NEW LIMIT MUST BE ABOVE ZERO - USE X TO REMOVE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHOD-NEW-LIMIT > WS-ABS-MAX-LIMIT
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'NEW LIMIT OUTSIDE PERMITTED RANGE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHOD-EXPIRY-DATE NOT NUMERIC
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'EXPIRY DATE NOT NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 1000-EXIT
           END-IF

      *---- LIMIT MUST BE A WHOLE MULTIPLE OF THE LIMIT UNIT ----------*
           IF CHOD-CHANGE
               MOVE CHOD-NEW-LIMIT         TO WS-NEW-LIMIT
               DIVIDE WS-NEW-LIMIT BY WS-LIMIT-UNIT
                      GIVING WS-UNITS REMAINDER WS-REMAINDER
               IF WS-REMAINDER NOT = ZERO
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'LIMIT MUST BE A MULTIPLE OF 50.00'
                                           TO CH-MESSAGE-TEXT
                   GO TO 1000-EXIT
               END-IF
           END-IF

      *---- EXPIRY - ZERO = DEFAULT TERM, ELSE A VALID FUTURE DATE ----*
           IF CHOD-CHANGE
               MOVE WS-TODAY               TO WS-DW-DATE
               MOVE WS-MAX-TERM-MONTHS     TO WS-DW-MONTHS
               PERFORM 8600-ADD-MONTHS
               MOVE WS-DW-DATE             TO WS-MAX-EXPIRY
               IF CHOD-EXPIRY-DATE = ZERO
                   MOVE WS-TODAY           TO WS-DW-DATE
                   MOVE WS-DEFAULT-TERM-MONTHS
                                           TO WS-DW-MONTHS
                   PERFORM 8600-ADD-MONTHS
                   MOVE WS-DW-DATE         TO WS-NEW-EXPIRY
               ELSE
                   MOVE CHOD-EXPIRY-DATE   TO WS-DW-DATE
                   PERFORM 8500-VALIDATE-DATE THRU 8500-EXIT
                   EVALUATE TRUE
                       WHEN NOT WS-DW-VALID
                           MOVE 'EXPIRY DATE IS NOT A VALID DATE'
                                           TO CH-MESSAGE-TEXT
                       WHEN CHOD-EXPIRY-DATE NOT > WS-TODAY
                           MOVE 'EXPIRY DATE MUST BE IN THE FUTURE'
                                           TO CH-MESSAGE-TEXT
                       WHEN CHOD-EXPIRY-DATE > WS-MAX-EXPIRY
                           MOVE 'EXPIRY DATE MORE THAN 24 MONTHS AHEAD'
                                           TO CH-MESSAGE-TEXT
                       WHEN OTHER
                           MOVE CHOD-EXPIRY-DATE
                                           TO WS-NEW-EXPIRY
                   END-EVALUATE
                   IF CH-MESSAGE-TEXT NOT = SPACES
                       MOVE '0016'         TO CH-RETURN-CD
                       MOVE 'INVD'         TO CH-REASON-CD
                   END-IF
               END-IF
           END-IF

      *---- OVERRIDE AND SECOND AUTHORISER ARE BRANCH ONLY ------------*
           IF CHOD-OVERRIDE-IND = 'Y'
           AND CH-CHANNEL-CD = 'BRANCH'
               SET WS-BRANCH-OVERRIDE      TO TRUE
           END-IF
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-READ-ACCOUNT.
      *================================================================*
           MOVE CHOD-SORT-CODE             TO WS-AK-SORT-CODE
           MOVE CHOD-ACCT-NO               TO WS-AK-ACCT-NO
           IF CHOD-ENQUIRE
               EXEC CICS READ
                         FILE('ACCTMST')
                         INTO(ACM-ACCOUNT-RECORD)
                         LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                         RIDFLD(WS-ACCT-KEY)
                         RESP(WS-RESP)
               END-EXEC
           ELSE
               EXEC CICS READ
                         FILE('ACCTMST')
                         INTO(ACM-ACCOUNT-RECORD)
                         LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                         RIDFLD(WS-ACCT-KEY)
                         UPDATE
                         RESP(WS-RESP)
               END-EXEC
           END-IF

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   IF NOT CHOD-ENQUIRE
                       SET WS-ACCOUNT-HELD TO TRUE
                   END-IF
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE

      *    AN EXPIRED LIMIT IS NOT HONOURED BY PSMEMO1 (PIR 2012/140).
           MOVE ACM-OD-LIMIT               TO WS-CURR-LIMIT
                                              WS-EFF-LIMIT
           IF ACM-OD-EXPIRY-DATE NOT = ZERO
           AND ACM-OD-EXPIRY-DATE < WS-TODAY
               MOVE ZERO                   TO WS-EFF-LIMIT
           END-IF
           IF ACM-AVAIL-BAL < ZERO
               COMPUTE WS-OVERDRAWN-AMT = ZERO - ACM-AVAIL-BAL
           ELSE
               MOVE ZERO                   TO WS-OVERDRAWN-AMT
           END-IF

           MOVE ACM-OD-LIMIT               TO CHOD-OLD-LIMIT
                                              CHOD-CURR-LIMIT
           MOVE ACM-OD-EXPIRY-DATE         TO CHOD-OLD-EXPIRY
                                              CHOD-CURR-EXPIRY
           MOVE ACM-AVAIL-BAL              TO CHOD-AVAIL-BAL
           COMPUTE CHOD-SPEND-POWER = ACM-AVAIL-BAL + WS-EFF-LIMIT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-READ-PRODUCT.
      *================================================================*
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
                   MOVE PRD-OD-MAX-LIMIT   TO CHOD-PROD-MAX
                   MOVE PRD-DR-RATE-ARRANGED
                                           TO CHOD-ARR-RATE
                   MOVE PRD-ARR-OD-MONTHLY-FEE
                                           TO CHOD-MONTHLY-FEE
               WHEN DFHRESP(NOTFND)
      *            ACCOUNT ON A PRODUCT THAT IS NOT ON PRDPARM - DATA
      *            ERROR, NOT A CUSTOMER PROBLEM.
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'PNFD'             TO CH-REASON-CD
                   MOVE 'PRODUCT PARAMETERS NOT FOUND - REFER TO OPS'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'PRDPARM'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       2200-READ-CUSTOMER.
      *================================================================*
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
                   SET WS-CIF-FOUND        TO TRUE
               WHEN DFHRESP(NOTFND)
      *            ONLY MATTERS FOR AN INCREASE - CHECKED IN 5100.
                   MOVE SPACES             TO CIF-CUSTOMER-RECORD
                   MOVE ZERO               TO CIF-ANN-INCOME
               WHEN OTHER
                   MOVE 'CIFMAST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       3000-ACCOUNT-RULES.
      *================================================================*
           EVALUATE TRUE
               WHEN NOT ACM-CURRENT
                   MOVE 'ATYP'             TO CH-REASON-CD
                   MOVE 'OVERDRAFTS ARE FOR CURRENT ACCOUNTS ONLY'
                                           TO CH-MESSAGE-TEXT
      *        ENQUIRY IS ALLOWED ON ANY OTHER STATUS.
               WHEN CHOD-ENQUIRE
                   CONTINUE
               WHEN NOT ACM-OPEN
                   MOVE 'ACLS'             TO CH-REASON-CD
                   MOVE 'ACCOUNT IS NOT OPEN'
                                           TO CH-MESSAGE-TEXT
      *        REMOVING A LIMIT IS ALLOWED ON A RESTRICTED ACCOUNT -
      *        IT IS OFTEN THE REASON FOR THE RESTRICTION.
               WHEN CHOD-CHANGE AND NOT ACM-NO-RESTRICT
                   MOVE 'RSTD'             TO CH-REASON-CD
                   MOVE 'ACCOUNT HAS A POSTING RESTRICTION'
                                           TO CH-MESSAGE-TEXT
               WHEN CHOD-CHANGE AND PRD-OD-MAX-LIMIT NOT > ZERO
                   MOVE 'PNOD'             TO CH-REASON-CD
                   MOVE 'THIS PRODUCT DOES NOT OFFER AN OVERDRAFT'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0008'                 TO CH-RETURN-CD
               SET CHOD-DECLINED           TO TRUE
               MOVE CH-REASON-CD           TO CHOD-DECISION-RSN
               IF NOT CHOD-ENQUIRE
                   SET WS-AUDIT-REQUIRED   TO TRUE
               END-IF
           END-IF
           .
      *================================================================*
       4000-CALC-ELIGIBILITY.
      *================================================================*
           MOVE ZERO                       TO WS-MAX-ELIGIBLE
           MOVE CHOD-MTHLY-COMMIT          TO WS-COMMITMENTS

           IF PRD-OD-MAX-LIMIT NOT > ZERO
           OR CIF-ANN-INCOME NOT > ZERO
           OR NOT WS-CIF-FOUND
               GO TO 4000-EXIT
           END-IF

           COMPUTE WS-GROSS-MONTHLY ROUNDED = CIF-ANN-INCOME / 12
           COMPUTE WS-NET-MONTHLY ROUNDED =
                   WS-GROSS-MONTHLY * WS-NET-FACTOR
           COMPUTE WS-DISPOSABLE = WS-NET-MONTHLY - WS-COMMITMENTS
           IF WS-DISPOSABLE < ZERO
               MOVE ZERO                   TO WS-DISPOSABLE
           END-IF

           COMPUTE WS-ELIG-INCOME = CIF-ANN-INCOME * WS-INCOME-PCT
           COMPUTE WS-ELIG-DISP = WS-DISPOSABLE * WS-DISP-MULTIPLE

           MOVE PRD-OD-MAX-LIMIT           TO WS-MAX-ELIGIBLE
           IF WS-ELIG-INCOME < WS-MAX-ELIGIBLE
               MOVE WS-ELIG-INCOME         TO WS-MAX-ELIGIBLE
           END-IF
           IF WS-ELIG-DISP < WS-MAX-ELIGIBLE
               MOVE WS-ELIG-DISP           TO WS-MAX-ELIGIBLE
           END-IF

      *    ROUND DOWN TO THE LIMIT UNIT.
           DIVIDE WS-MAX-ELIGIBLE BY WS-LIMIT-UNIT
                  GIVING WS-UNITS
           COMPUTE WS-MAX-ELIGIBLE = WS-UNITS * WS-LIMIT-UNIT
           .
       4000-EXIT.
           MOVE WS-MAX-ELIGIBLE            TO CHOD-MAX-ELIGIBLE
           .
      *================================================================*
       5000-CHANGE-LIMIT.
      *================================================================*
           IF WS-NEW-LIMIT > WS-EFF-LIMIT
               SET WS-INCREASE             TO TRUE
               PERFORM 5100-INCREASE-RULES THRU 5100-DECIDED
           ELSE
               PERFORM 5200-DECREASE-RULES
           END-IF
           IF NOT CH-RC-OK
               GO TO 5000-EXIT
           END-IF

           MOVE WS-NEW-LIMIT               TO ACM-OD-LIMIT
           MOVE WS-NEW-EXPIRY              TO ACM-OD-EXPIRY-DATE
           PERFORM 7000-REWRITE-ACCOUNT
           IF NOT CH-RC-OK
               GO TO 5000-EXIT
           END-IF

           SET CHOD-APPROVED               TO TRUE
           MOVE ACM-OD-LIMIT               TO CHOD-CURR-LIMIT
           MOVE ACM-OD-EXPIRY-DATE         TO CHOD-CURR-EXPIRY
           COMPUTE CHOD-SPEND-POWER = ACM-AVAIL-BAL + ACM-OD-LIMIT
           EVALUATE TRUE
               WHEN WS-INCREASE
                   MOVE 'OVERDRAFT LIMIT INCREASED'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-NEW-LIMIT = WS-CURR-LIMIT
                   MOVE 'OVERDRAFT LIMIT RENEWED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'OVERDRAFT LIMIT REDUCED'
                                           TO CH-MESSAGE-TEXT
           END-EVALUATE
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       5100-INCREASE-RULES.
      *================================================================*
      *    DECLINES ('D') ARE FINAL. REFERRALS ('R') CAN BE TAKEN UP BY
      *    A BRANCH, WHICH HAS MORE INFORMATION (PAYSLIPS, BUREAU).
           IF WS-CIF-FOUND
           AND CIF-DOB NUMERIC AND CIF-DOB > ZERO
               COMPUTE WS-AGE = (WS-TODAY - CIF-DOB) / 10000
           ELSE
               MOVE ZERO                   TO WS-AGE
           END-IF

           EVALUATE TRUE
               WHEN NOT WS-CIF-FOUND
                   MOVE 'CNFD'             TO CH-REASON-CD
                   MOVE 'CUSTOMER RECORD NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-DECLINED       TO TRUE
               WHEN NOT CIF-ACTIVE
                   MOVE 'CINA'             TO CH-REASON-CD
                   MOVE 'CUSTOMER IS NOT ACTIVE'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-DECLINED       TO TRUE
               WHEN NOT CIF-KYC-VERIFIED
                   MOVE 'KYCX'             TO CH-REASON-CD
                   MOVE 'CUSTOMER IDENTITY NOT VERIFIED (KYC)'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-DECLINED       TO TRUE
               WHEN WS-AGE < WS-MIN-AGE
                   MOVE 'AGEX'             TO CH-REASON-CD
                   MOVE 'CUSTOMER MUST BE 18 OR OVER'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-DECLINED       TO TRUE
               WHEN CIF-BUSINESS
               AND  CH-CHANNEL-CD NOT = 'BRANCH'
                   MOVE 'BUSN'             TO CH-REASON-CD
                   MOVE 'BUSINESS OVERDRAFTS VIA RELATIONSHIP MANAGER'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-REFERRED       TO TRUE
               WHEN CIF-RISK-HIGH
                   MOVE 'RISK'             TO CH-REASON-CD
                   MOVE 'REFERRED - PLEASE CONTACT YOUR BRANCH'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-REFERRED       TO TRUE
               WHEN CIF-ANN-INCOME NOT > ZERO
                   MOVE 'INCX'             TO CH-REASON-CD
                   MOVE 'NO INCOME RECORDED - UPDATE EMPLOYMENT FIRST'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-REFERRED       TO TRUE
               WHEN WS-NEW-LIMIT > PRD-OD-MAX-LIMIT
                   MOVE 'PMAX'             TO CH-REASON-CD
                   MOVE 'LIMIT IS OVER THE MAXIMUM FOR THIS PRODUCT'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-DECLINED       TO TRUE
               WHEN WS-NEW-LIMIT > WS-MAX-ELIGIBLE
                   MOVE 'AFFX'             TO CH-REASON-CD
                   MOVE 'LIMIT NOT AFFORDABLE - SEE MAXIMUM ELIGIBLE'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-DECLINED       TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               GO TO 5100-DECIDED
           END-IF

      *---- CHANNEL / CONDUCT RULES -----------------------------------*
           IF ACM-OPEN-DATE NUMERIC AND ACM-OPEN-DATE > ZERO
               COMPUTE WS-WORK-INT = WS-TODAY-INT
                     - FUNCTION INTEGER-OF-DATE (ACM-OPEN-DATE)
           ELSE
               MOVE ZERO                   TO WS-WORK-INT
           END-IF
           EVALUATE TRUE
               WHEN CH-CHANNEL-CD = 'BRANCH'
                   CONTINUE
               WHEN WS-WORK-INT < WS-MIN-ACCT-DAYS
                   MOVE 'NEWA'             TO CH-REASON-CD
                   MOVE 'ACCOUNT OPENED RECENTLY - PLEASE CALL BRANCH'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-REFERRED       TO TRUE
               WHEN WS-NEW-LIMIT > WS-REMOTE-MAX-LIMIT
                   MOVE 'BRCH'             TO CH-REASON-CD
                   MOVE 'LIMITS OVER 2,500 MUST BE AGREED IN BRANCH'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-REFERRED       TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               GO TO 5100-DECIDED
           END-IF

           IF (ACM-UNPAID-ITEMS > ZERO OR ACM-UNARR-OD-DAYS > ZERO)
           AND NOT WS-BRANCH-OVERRIDE
               MOVE 'COND'                 TO CH-REASON-CD
               MOVE 'REFERRED - RECENT UNARRANGED BORROWING'
                                           TO CH-MESSAGE-TEXT
               SET CHOD-REFERRED           TO TRUE
               GO TO 5100-DECIDED
           END-IF

      *---- FOUR EYES FOR LARGE BRANCH LIMITS -------------------------*
           IF CH-CHANNEL-CD = 'BRANCH'
           AND WS-NEW-LIMIT > WS-FOUR-EYES-LIMIT
               IF CHOD-AUTH-USER = SPACES OR LOW-VALUES
               OR CHOD-AUTH-USER = CH-USER-ID
                   MOVE 'AUTH'             TO CH-REASON-CD
                   MOVE 'LIMIT OVER 5,000 NEEDS A SECOND AUTHORISER'
                                           TO CH-MESSAGE-TEXT
                   SET CHOD-REFERRED       TO TRUE
               END-IF
           END-IF
           .
       5100-DECIDED.
           IF CH-REASON-CD NOT = SPACES
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE CH-REASON-CD           TO CHOD-DECISION-RSN
           END-IF
           .
      *================================================================*
       5200-DECREASE-RULES.
      *================================================================*
      *    INCLUDES A RENEWAL AT THE SAME LIMIT. A RENEWAL OF AN
      *    EXPIRED LIMIT IS AN INCREASE (5100) - PIR 2012/140.
           IF WS-NEW-LIMIT < WS-OVERDRAWN-AMT
           AND NOT WS-BRANCH-OVERRIDE
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'ODBL'                 TO CH-REASON-CD
               MOVE 'NEW LIMIT IS LESS THAN THE OVERDRAWN BALANCE'
                                           TO CH-MESSAGE-TEXT
               SET CHOD-DECLINED           TO TRUE
               MOVE CH-REASON-CD           TO CHOD-DECISION-RSN
           END-IF
           .
      *================================================================*
       6000-REMOVE-LIMIT.
      *================================================================*
           IF ACM-OD-LIMIT = ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'NOOD'                 TO CH-REASON-CD
               MOVE 'ACCOUNT HAS NO OVERDRAFT LIMIT'
                                           TO CH-MESSAGE-TEXT
               GO TO 6000-EXIT
           END-IF

           IF WS-OVERDRAWN-AMT > ZERO
           AND NOT WS-BRANCH-OVERRIDE
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'ODBL'                 TO CH-REASON-CD
               MOVE 'ACCOUNT IS OVERDRAWN - REPAY BEFORE REMOVING'
                                           TO CH-MESSAGE-TEXT
               SET CHOD-DECLINED           TO TRUE
               MOVE CH-REASON-CD           TO CHOD-DECISION-RSN
               GO TO 6000-EXIT
           END-IF

           MOVE ZERO                       TO ACM-OD-LIMIT
                                              ACM-OD-EXPIRY-DATE
                                              WS-NEW-LIMIT
                                              WS-NEW-EXPIRY
           PERFORM 7000-REWRITE-ACCOUNT
           IF NOT CH-RC-OK
               GO TO 6000-EXIT
           END-IF

           SET CHOD-APPROVED               TO TRUE
           MOVE ZERO                       TO CHOD-CURR-LIMIT
                                              CHOD-CURR-EXPIRY
           MOVE ACM-AVAIL-BAL              TO CHOD-SPEND-POWER
           MOVE 'OVERDRAFT LIMIT REMOVED'  TO CH-MESSAGE-TEXT
           IF WS-OVERDRAWN-AMT > ZERO
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'ODBL'                 TO CH-REASON-CD
               MOVE 'LIMIT REMOVED - ACCOUNT NOW IN UNARRANGED OD'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
       6000-EXIT.
           EXIT.
      *================================================================*
       7000-REWRITE-ACCOUNT.
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
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCTMST'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
           ELSE
               MOVE 'N'                    TO WS-ACCOUNT-HELD-IND
           END-IF
           .
      *================================================================*
       8000-WRITE-AUDIT.
      *================================================================*
           MOVE SPACES                     TO WS-AUDIT-RECORD
           MOVE WS-TODAY                   TO ODA-DATE
           MOVE WS-TIME-HHMMSS             TO ODA-TIME
           MOVE EIBTASKN                   TO ODA-TASKNO
           MOVE CH-CHANNEL-CD              TO ODA-CHANNEL
           MOVE CH-USER-ID                 TO ODA-USER
           MOVE CHOD-AUTH-USER             TO ODA-AUTH-USER
           MOVE CH-REQUEST-ID              TO ODA-REQUEST-ID
           MOVE CHOD-FUNCTION              TO ODA-FUNCTION
           MOVE CHOD-SORT-CODE             TO ODA-SORT-CODE
           MOVE CHOD-ACCT-NO               TO ODA-ACCT-NO
           MOVE ACM-CIF-NO                 TO ODA-CIF-NO
           MOVE ACM-PROD-CD                TO ODA-PROD-CD
           MOVE CHOD-OLD-LIMIT             TO ODA-OLD-LIMIT
           MOVE CHOD-OLD-EXPIRY            TO ODA-OLD-EXPIRY
           IF CHOD-CHANGE
               MOVE WS-NEW-LIMIT           TO ODA-REQ-LIMIT
           ELSE
               MOVE ZERO                   TO ODA-REQ-LIMIT
           END-IF
           MOVE CHOD-CURR-LIMIT            TO ODA-NEW-LIMIT
           MOVE CHOD-CURR-EXPIRY           TO ODA-NEW-EXPIRY
           MOVE WS-MAX-ELIGIBLE            TO ODA-MAX-ELIGIBLE
           MOVE CIF-ANN-INCOME             TO ODA-ANN-INCOME
           MOVE WS-COMMITMENTS             TO ODA-COMMITMENTS
           MOVE CHOD-DECISION              TO ODA-DECISION
           MOVE CH-RETURN-CD               TO ODA-RETURN-CD
           MOVE CH-REASON-CD               TO ODA-REASON-CD
           MOVE CHOD-REASON-CD             TO ODA-REQ-REASON
           MOVE WS-OVERRIDE-IND            TO ODA-OVERRIDE-IND

      *    THE TDQ IS NOT RECOVERABLE. A FAILURE IS REPORTED TO THE
      *    CONSOLE BUT DOES NOT FAIL THE CUSTOMER REQUEST (AS CFMNT01).
           EXEC CICS WRITEQ TD
                     QUEUE('ODAU')
                     FROM(WS-AUDIT-RECORD)
                     LENGTH(LENGTH OF WS-AUDIT-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS WRITE OPERATOR
                         TEXT('MBKOD001E ODAU AUDIT WRITE FAILED')
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           .
      *================================================================*
       8500-VALIDATE-DATE.
      *================================================================*
      *    WS-DW-DATE (CCYYMMDD) -> WS-DW-VALID-IND.
           MOVE 'N'                        TO WS-DW-VALID-IND
           IF WS-DW-DATE NOT NUMERIC
           OR WS-DW-CCYY < 1900 OR WS-DW-CCYY > 2099
           OR WS-DW-MM < 1 OR WS-DW-MM > 12
           OR WS-DW-DD < 1
               GO TO 8500-EXIT
           END-IF
           MOVE WS-MONTH-DAYS (WS-DW-MM)   TO WS-DW-MAX-DD
           IF WS-DW-MM = 2
               DIVIDE WS-DW-CCYY BY 4 GIVING WS-DW-QUOT
                      REMAINDER WS-DW-REM
               IF WS-DW-REM = ZERO
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
       8600-ADD-MONTHS.
      *================================================================*
      *    WS-DW-DATE + WS-DW-MONTHS -> WS-DW-DATE. THE DAY IS CUT
      *    BACK TO THE END OF A SHORTER MONTH (31 JAN + 1 = 28/29 FEB).
           COMPUTE WS-DW-TOTAL-MM = WS-DW-CCYY * 12 + WS-DW-MM - 1
                                  + WS-DW-MONTHS
           DIVIDE WS-DW-TOTAL-MM BY 12 GIVING WS-DW-QUOT
                  REMAINDER WS-DW-REM
           MOVE WS-DW-QUOT                 TO WS-DW-CCYY
           COMPUTE WS-DW-MM = WS-DW-REM + 1
           MOVE WS-MONTH-DAYS (WS-DW-MM)   TO WS-DW-MAX-DD
           IF WS-DW-MM = 2
               DIVIDE WS-DW-CCYY BY 4 GIVING WS-DW-QUOT
                      REMAINDER WS-DW-REM
               IF WS-DW-REM = ZERO
                   MOVE 29                 TO WS-DW-MAX-DD
               END-IF
           END-IF
           IF WS-DW-DD > WS-DW-MAX-DD
               MOVE WS-DW-MAX-DD           TO WS-DW-DD
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
