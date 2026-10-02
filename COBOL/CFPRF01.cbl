       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFPRF01.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2012-09-03.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFPRF01                                             *
      * TITLE    : CUSTOMER CONTACT, MARKETING AND E-STATEMENT         *
      *            PREFERENCES - CHANNEL SERVICE CUSTPREF              *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : CIFMAST  READ / READ UPDATE / REWRITE               *
      *            ACCTCIF  BROWSE (ACCOUNTS FOR THE CUSTOMER)         *
      *            ACCTMST  READ UPDATE / REWRITE (STATEMENT CYCLE)    *
      *            PRDPARM  READ (PRD-ESTMT-ALLOWED)                   *
      * TDQ      : CPAU     PREFERENCE / CONSENT AUDIT (EXTRAPARTITION)*
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-CUSTPREF-AREA              *
      *----------------------------------------------------------------*
      * "MY PREFERENCES" FOR IBANK / TELE. CFMNT01 STILL OWNS NAME,    *
      * ADDRESS, KYC AND EMPLOYMENT.                                   *
      * FUNCTIONS                                                      *
      *   E  ENQUIRE - CONTACT DETAILS, CONSENTS, AND THE STATEMENT    *
      *      SETTINGS OF EVERY ACCOUNT THAT IS NOT CLOSED (UP TO 20)   *
      *   U  UPDATE - ANY OF THREE GROUPS, EACH FLAGGED IN THE REQUEST *
      *        CONTACT  HOME / MOBILE TELEPHONE, E-MAIL ADDRESS        *
      *        MKTG     E-MAIL AND POST MARKETING CONSENT              *
      *        STMT     E-STATEMENT OPT-IN, AND STATEMENT CYCLE / DAY  *
      *                 FOR THE ACCOUNTS LISTED IN CHCP-ACCT-PREF      *
      *----------------------------------------------------------------*
      * E-STATEMENTS AND MARKETING CONSENT (READ THIS)                 *
      *   THE CIF HAS NO PAPERLESS INDICATOR. STSTM020 (AND CGNTF010)  *
      *   TREAT CIF-MKTG-EMAIL-IND = 'Y' AS "PAPERLESS" (PIR 2009/233  *
      *   - OPEN). UNTIL A SEPARATE FLAG EXISTS THE TWO CANNOT DIFFER, *
      *   SO TO STAY WITHIN THE DATA PROTECTION RULES:                 *
      *   - AN OPT-IN TO E-STATEMENTS IS ONLY ACCEPTED WITH AN         *
      *     EXPLICIT E-MAIL MARKETING CONSENT IN THE SAME REQUEST, AND *
      *     THE CHANNEL MUST SAY SO TO THE CUSTOMER (REASON PCON)      *
      *   - AN OPT-OUT OF EITHER IS ALWAYS ACCEPTED AND RETURNS A      *
      *     WARNING THAT THE OTHER HAS CHANGED AS WELL (MKLK / PAPR)   *
      *   - E-STATEMENTS NEED AN E-MAIL ADDRESS, AND ARE ONLY PRODUCED *
      *     FOR PRODUCTS WITH PRD-ESTMT-ALLOWED = 'Y'                  *
      *----------------------------------------------------------------*
      * OTHER RULES                                                    *
      *   - WITHDRAWING CONSENT IS ALWAYS ALLOWED, WHATEVER THE        *
      *     CUSTOMER STATUS (RIGHT TO OBJECT). EVERYTHING ELSE NEEDS   *
      *     AN ACTIVE CUSTOMER. A MERGED CIF IS REFUSED (CMRG).        *
      *   - A CONSENT CHANGE MUST CARRY HOW IT WAS GIVEN (WEB / CALL / *
      *     FORM / BRCH) AND A SCRIPT OR FORM REFERENCE                *
      *   - THE MOBILE NUMBER RECEIVES ONE-TIME PASSCODES, SO IT CAN   *
      *     ONLY BE CHANGED BY BRANCH OR TELE (FRAUD RULE FR-2011-12)  *
      *   - CURRENT ACCOUNTS ARE STATEMENTED MONTHLY (BCOBS 4). OTHER  *
      *     ACCOUNTS MAY BE M, Q OR A. STATEMENT DAY 1 - 28.           *
      *   - AN ACCOUNT THAT FAILS ITS CHECKS IS NOT UPDATED (RESULT    *
      *     CODE ON ITS RESPONSE ENTRY); THE REST OF THE REQUEST IS    *
      *     APPLIED AND RC 0004 / APRT IS RETURNED                     *
      *----------------------------------------------------------------*
      * AUDIT: ONE CPAU RECORD FOR EVERY ENQUIRY (DATA ACCESS LOG),    *
      * ONE FOR THE CUSTOMER CHANGES (BEFORE / AFTER) AND ONE FOR      *
      * EACH ACCOUNT CHANGED. MBK.PRD.CICS.CPAU IS RACF PROTECTED -    *
      * IT HOLDS CONTACT DETAILS (DATA PROTECTION OFFICER DPO-12-07).  *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   CNFD CUSTOMER NOT FOUND      CINA CUSTOMER NOT ACTIVE        *
      *   CMRG CUSTOMER MERGED - USE THE NEW CIF (IN THE MESSAGE)      *
      *   MAUT MOBILE NUMBER - BRANCH / TELE ONLY                      *
      *   PCON E-STATEMENT AND E-MAIL MARKETING MUST AGREE             *
      *   ESTE E-STATEMENTS NEED AN E-MAIL ADDRESS                     *
      *   MKLK E-MAIL MARKETING WITHDRAWN WITH E-STATEMENTS (0004)     *
      *   PAPR PAPER STATEMENTS RESUME (0004)                          *
      *   APRT SOME ACCOUNTS NOT UPDATED (0004)                        *
      *   NCHG NOTHING TO CHANGE (0004)                                *
      *   PER ACCOUNT: ANFD ACIF ACLS CYCM INVY NOES                   *
      *   INVx INVALID REQUEST FIELD   FUNC INVALID FUNCTION           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2012-09-03 AOS  ORIGINAL VERSION (CR 12/0188 - PREFERENCE      *
      *                 CENTRE FOR IBANK)                              *
      * 2013-02-18 AOS  ENQUIRIES WRITTEN TO CPAU (DPO-12-07 ACCESS    *
      *                 LOGGING)                                       *
      * 2014-05-12 SAT  CONSENT SOURCE / REFERENCE MANDATORY (ICO      *
      *                 DIRECT MARKETING GUIDANCE)                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFPRF01 '.
       01  WS-CONSTANTS.
           05  WS-MAX-ACCOUNTS             PIC 9(02) VALUE 20.
           05  WS-MIN-PHONE-DIGITS         PIC 9(02) VALUE 10.
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
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-POS                      PIC S9(04) COMP.
           05  WS-AT-POS                   PIC S9(04) COMP.
           05  WS-AT-COUNT                 PIC S9(04) COMP.
           05  WS-DOT-AFTER                PIC S9(04) COMP.
           05  WS-LAST-POS                 PIC S9(04) COMP.
           05  WS-DIGITS                   PIC S9(04) COMP.
           05  WS-ACCT-COUNT               PIC 9(02).
           05  WS-CHANGE-COUNT             PIC 9(02).
           05  WS-FAILED-ACCTS             PIC 9(02).
           05  WS-NEW-EMAIL-IND            PIC X(01).
           05  WS-WARNING-CD               PIC X(04).
           05  WS-WARNING-TEXT             PIC X(80).
           05  WS-RESULT-CD                PIC X(04).
           05  WS-PHONE                    PIC X(20).
           05  WS-PHONE-VALID-IND          PIC X(01).
               88  WS-PHONE-VALID              VALUE 'Y'.
           05  WS-EMAIL-VALID-IND          PIC X(01).
               88  WS-EMAIL-VALID              VALUE 'Y'.
           05  WS-CIF-HELD-IND             PIC X(01) VALUE 'N'.
               88  WS-CIF-HELD                 VALUE 'Y'.
           05  WS-OPT-IN-IND               PIC X(01) VALUE 'N'.
               88  WS-OPT-IN                   VALUE 'Y'.
           05  WS-END-BROWSE-IND           PIC X(01).
               88  WS-END-BROWSE               VALUE 'Y'.
       01  WS-ACCT-KEY.
           05  WS-AK-SORT-CODE             PIC X(06).
           05  WS-AK-ACCT-NO               PIC X(08).
       01  WS-CIF-KEY                      PIC X(12).
       01  WS-BROWSE-KEY                   PIC X(12).
       01  WS-PRD-KEY                      PIC X(10).
      *---- CONTACT / CONSENT IMAGE (BEFORE AND AFTER) ----------------*
       01  WS-PREF-BEFORE.
           05  PB-HOME-TEL                 PIC X(20).
           05  PB-MOBILE-TEL               PIC X(20).
           05  PB-EMAIL-ADDR               PIC X(80).
           05  PB-MKTG-EMAIL-IND           PIC X(01).
           05  PB-MKTG-POST-IND            PIC X(01).
       01  WS-PREF-AFTER                   PIC X(122).
      *---- AUDIT RECORD - TDQ CPAU (RECFM=FB LRECL=400) --------------*
       01  WS-AUDIT-RECORD.
           05  CPA-DATE                    PIC 9(08).
           05  CPA-TIME                    PIC 9(06).
           05  CPA-TASKNO                  PIC 9(07).
           05  CPA-CHANNEL                 PIC X(06).
           05  CPA-USER                    PIC X(08).
           05  CPA-REQUEST-ID              PIC X(24).
           05  CPA-FUNCTION                PIC X(01).
           05  CPA-REC-TYPE                PIC X(01).
               88  CPA-ENQUIRY                 VALUE 'E'.
               88  CPA-CUSTOMER                VALUE 'C'.
               88  CPA-ACCOUNT                 VALUE 'A'.
           05  CPA-CIF-NO                  PIC X(12).
           05  CPA-CONSENT-SOURCE          PIC X(04).
           05  CPA-CONSENT-REF             PIC X(20).
           05  CPA-RETURN-CD               PIC X(04).
           05  CPA-REASON-CD               PIC X(04).
           05  CPA-DETAIL                  PIC X(244).
           05  CPA-CUST-DETAIL REDEFINES CPA-DETAIL.
               10  CPA-BEFORE              PIC X(122).
               10  CPA-AFTER               PIC X(122).
           05  CPA-ACCT-DETAIL REDEFINES CPA-DETAIL.
               10  CPA-SORT-CODE           PIC X(06).
               10  CPA-ACCT-NO             PIC X(08).
               10  CPA-OLD-CYCLE           PIC X(01).
               10  CPA-OLD-DAY             PIC 9(02).
               10  CPA-NEW-CYCLE           PIC X(01).
               10  CPA-NEW-DAY             PIC 9(02).
               10  FILLER                  PIC X(224).
           05  FILLER                      PIC X(51).
           COPY CFCUSREC.
           COPY DDACCREC.
           COPY PRDPARRC.
           COPY CHCOMMAR.
           COPY CHCOMMA2.
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(4000).
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE DFHCOMMAREA                TO CH-COMMAREA
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
                                              CHCP-RESP
                                              WS-WARNING-CD
                                              WS-WARNING-TEXT
           MOVE ZERO                       TO CHCP-CHANGE-COUNT
                                              CHCP-ACCT-RESP-COUNT
                                              CHCP-LAST-MAINT-DATE
                                              WS-CHANGE-COUNT
                                              WS-FAILED-ACCTS

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
               PERFORM 1500-READ-CUSTOMER THRU 1500-EXIT
           END-IF

           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHCP-ENQUIRE
                       PERFORM 2000-ENQUIRE THRU 2000-EXIT
                   WHEN CHCP-UPDATE
                       PERFORM 3000-UPDATE THRU 3000-EXIT
               END-EVALUATE
           END-IF

           IF WS-CIF-HELD
               EXEC CICS UNLOCK
                         FILE('CIFMAST')
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
           EVALUATE TRUE
               WHEN NOT (CHCP-ENQUIRE OR CHCP-UPDATE)
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE E OR U'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCP-CIF-NO = SPACES OR LOW-VALUES
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'CIF NUMBER REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCP-ENQUIRE
                   CONTINUE
               WHEN CHCP-UPD-CONTACT NOT = 'Y' AND 'N' AND SPACE
               WHEN CHCP-UPD-MKTG NOT = 'Y' AND 'N' AND SPACE
               WHEN CHCP-UPD-STMT NOT = 'Y' AND 'N' AND SPACE
                   MOVE 'INVU'             TO CH-REASON-CD
                   MOVE 'UPDATE GROUP FLAGS MUST BE Y OR N'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCP-UPD-CONTACT NOT = 'Y'
               AND  CHCP-UPD-MKTG NOT = 'Y'
               AND  CHCP-UPD-STMT NOT = 'Y'
                   MOVE 'INVU'             TO CH-REASON-CD
                   MOVE 'NO UPDATE GROUP SELECTED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCP-UPD-MKTG = 'Y'
               AND (CHCP-MKTG-EMAIL-IND NOT = 'Y' AND 'N'
                    OR CHCP-MKTG-POST-IND NOT = 'Y' AND 'N')
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'MARKETING INDICATORS MUST BE Y OR N'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCP-UPD-STMT = 'Y'
               AND  CHCP-ESTMT-IND NOT = 'Y' AND 'N' AND SPACE
                   MOVE 'INVE'             TO CH-REASON-CD
                   MOVE 'E-STATEMENT INDICATOR MUST BE Y, N OR SPACE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCP-UPD-STMT = 'Y'
               AND (CHCP-ACCT-COUNT NOT NUMERIC
                    OR CHCP-ACCT-COUNT > WS-MAX-ACCOUNTS)
                   MOVE 'INVN'             TO CH-REASON-CD
                   MOVE 'ACCOUNT COUNT MUST BE 0 TO 20'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
               GO TO 1000-EXIT
           END-IF
           IF CHCP-ENQUIRE
               GO TO 1000-EXIT
           END-IF

      *---- CONSENT EVIDENCE ------------------------------------------*
           IF CHCP-UPD-MKTG = 'Y'
           OR (CHCP-UPD-STMT = 'Y' AND CHCP-ESTMT-IND NOT = SPACE)
               IF CHCP-CONSENT-SOURCE NOT = 'WEB ' AND 'CALL'
                                    AND 'FORM' AND 'BRCH'
               OR CHCP-CONSENT-REF = SPACES OR LOW-VALUES
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVS'             TO CH-REASON-CD
                   MOVE 'CONSENT SOURCE (WEB/CALL/FORM/BRCH) AND REF'
                                           TO CH-MESSAGE-TEXT
                   GO TO 1000-EXIT
               END-IF
           END-IF

      *---- CONTACT DETAILS FORMAT ------------------------------------*
           IF CHCP-UPD-CONTACT = 'Y'
               PERFORM 1100-CHECK-CONTACT THRU 1100-EXIT
           END-IF
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       1100-CHECK-CONTACT.
      *================================================================*
           IF CHCP-EMAIL-ADDR NOT = SPACES
               PERFORM 8100-CHECK-EMAIL THRU 8100-EXIT
               IF NOT WS-EMAIL-VALID
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVE'             TO CH-REASON-CD
                   MOVE 'E-MAIL ADDRESS IS NOT VALID'
                                           TO CH-MESSAGE-TEXT
                   GO TO 1100-EXIT
               END-IF
           END-IF

           IF CHCP-MOBILE-TEL NOT = SPACES
               MOVE CHCP-MOBILE-TEL        TO WS-PHONE
               PERFORM 8200-CHECK-PHONE THRU 8200-EXIT
               IF NOT WS-PHONE-VALID
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVT'             TO CH-REASON-CD
                   MOVE 'MOBILE NUMBER IS NOT VALID'
                                           TO CH-MESSAGE-TEXT
                   GO TO 1100-EXIT
               END-IF
           END-IF

           IF CHCP-HOME-TEL NOT = SPACES
               MOVE CHCP-HOME-TEL          TO WS-PHONE
               PERFORM 8200-CHECK-PHONE THRU 8200-EXIT
               IF NOT WS-PHONE-VALID
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVT'             TO CH-REASON-CD
                   MOVE 'HOME TELEPHONE NUMBER IS NOT VALID'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           .
       1100-EXIT.
           EXIT.
      *================================================================*
       1500-READ-CUSTOMER.
      *================================================================*
           MOVE CHCP-CIF-NO                TO WS-CIF-KEY
           IF CHCP-ENQUIRE
               EXEC CICS READ
                         FILE('CIFMAST')
                         INTO(CIF-CUSTOMER-RECORD)
                         LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                         RIDFLD(WS-CIF-KEY)
                         RESP(WS-RESP)
               END-EXEC
           ELSE
               EXEC CICS READ
                         FILE('CIFMAST')
                         INTO(CIF-CUSTOMER-RECORD)
                         LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                         RIDFLD(WS-CIF-KEY)
                         UPDATE
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   IF CHCP-UPDATE
                       SET WS-CIF-HELD     TO TRUE
                   END-IF
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CNFD'             TO CH-REASON-CD
                   MOVE 'CUSTOMER NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   GO TO 1500-EXIT
               WHEN OTHER
                   MOVE 'CIFMAST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 1500-EXIT
           END-EVALUATE

           IF CIF-MERGED-INTO NOT = SPACES AND LOW-VALUES
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'CMRG'                 TO CH-REASON-CD
               STRING 'CUSTOMER MERGED INTO CIF ' CIF-MERGED-INTO
                      DELIMITED BY SIZE
                      INTO CH-MESSAGE-TEXT
               END-STRING
               GO TO 1500-EXIT
           END-IF

           MOVE CIF-HOME-TEL               TO PB-HOME-TEL
           MOVE CIF-MOBILE-TEL             TO PB-MOBILE-TEL
           MOVE CIF-EMAIL-ADDR             TO PB-EMAIL-ADDR
           MOVE CIF-MKTG-EMAIL-IND         TO PB-MKTG-EMAIL-IND
           MOVE CIF-MKTG-POST-IND          TO PB-MKTG-POST-IND
           .
       1500-EXIT.
           EXIT.
      *================================================================*
       2000-ENQUIRE.
      *================================================================*
           PERFORM 7000-FORMAT-CUSTOMER

      *---- ALL ACCOUNTS NOT CLOSED, VIA THE CIF PATH -----------------*
           MOVE ZERO                       TO WS-ACCT-COUNT
           MOVE 'N'                        TO WS-END-BROWSE-IND
           MOVE CHCP-CIF-NO                TO WS-BROWSE-KEY
           EXEC CICS STARTBR
                     FILE('ACCTCIF')
                     RIDFLD(WS-BROWSE-KEY)
                     EQUAL
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               SET WS-END-BROWSE           TO TRUE
           END-IF

           PERFORM UNTIL WS-END-BROWSE
               EXEC CICS READNEXT
                         FILE('ACCTCIF')
                         INTO(ACM-ACCOUNT-RECORD)
                         LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                         RIDFLD(WS-BROWSE-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   AND  WS-RESP NOT = DFHRESP(DUPKEY)
                   WHEN ACM-CIF-NO NOT = CHCP-CIF-NO
                       SET WS-END-BROWSE   TO TRUE
                   WHEN ACM-CLOSED
                       CONTINUE
                   WHEN WS-ACCT-COUNT >= WS-MAX-ACCOUNTS
                       MOVE '0004'         TO CH-RETURN-CD
                       MOVE 'MORE'         TO CH-REASON-CD
                       MOVE 'FIRST 20 ACCOUNTS SHOWN'
                                           TO CH-MESSAGE-TEXT
                       SET WS-END-BROWSE   TO TRUE
                   WHEN OTHER
                       ADD 1               TO WS-ACCT-COUNT
                       MOVE SPACES         TO WS-RESULT-CD
                       PERFORM 7100-FORMAT-ACCOUNT
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR
                     FILE('ACCTCIF')
                     RESP(WS-RESP)
           END-EXEC

           MOVE WS-ACCT-COUNT              TO CHCP-ACCT-RESP-COUNT
           IF CH-RC-OK
               MOVE 'PREFERENCES RETURNED' TO CH-MESSAGE-TEXT
           END-IF

      *    DATA ACCESS LOG (DPO-12-07).
           SET CPA-ENQUIRY                 TO TRUE
           MOVE SPACES                     TO CPA-DETAIL
           PERFORM 8000-WRITE-AUDIT
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-UPDATE.
      *================================================================*
           PERFORM 3100-CHECK-AUTHORITY THRU 3100-EXIT
           IF NOT CH-RC-OK
               GO TO 3000-EXIT
           END-IF

           IF CHCP-UPD-CONTACT = 'Y'
               PERFORM 3200-APPLY-CONTACT
           END-IF
           IF CHCP-UPD-MKTG = 'Y'
           OR (CHCP-UPD-STMT = 'Y' AND CHCP-ESTMT-IND NOT = SPACE)
               PERFORM 3300-APPLY-CONSENTS THRU 3300-EXIT
               IF NOT CH-RC-OK
                   GO TO 3000-EXIT
               END-IF
           END-IF

      *---- REWRITE THE CUSTOMER IF ANYTHING CHANGED ------------------*
           MOVE CIF-HOME-TEL               TO WS-PREF-AFTER (1:20)
           MOVE CIF-MOBILE-TEL             TO WS-PREF-AFTER (21:20)
           MOVE CIF-EMAIL-ADDR             TO WS-PREF-AFTER (41:80)
           MOVE CIF-MKTG-EMAIL-IND         TO WS-PREF-AFTER (121:1)
           MOVE CIF-MKTG-POST-IND          TO WS-PREF-AFTER (122:1)
           IF WS-PREF-AFTER NOT = WS-PREF-BEFORE
               PERFORM 3500-REWRITE-CUSTOMER THRU 3500-EXIT
               IF NOT CH-RC-OK
                   GO TO 3000-EXIT
               END-IF
               SET CPA-CUSTOMER            TO TRUE
               MOVE WS-PREF-BEFORE         TO CPA-BEFORE
               MOVE WS-PREF-AFTER          TO CPA-AFTER
               PERFORM 8000-WRITE-AUDIT
           ELSE
               EXEC CICS UNLOCK
                         FILE('CIFMAST')
                         RESP(WS-RESP)
               END-EXEC
               MOVE 'N'                    TO WS-CIF-HELD-IND
           END-IF

      *---- STATEMENT SETTINGS PER ACCOUNT ----------------------------*
           IF CHCP-UPD-STMT = 'Y'
               PERFORM 4000-UPDATE-ACCOUNTS
           END-IF

           PERFORM 7000-FORMAT-CUSTOMER
           MOVE WS-CHANGE-COUNT            TO CHCP-CHANGE-COUNT

           EVALUATE TRUE
               WHEN WS-FAILED-ACCTS > ZERO
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'APRT'             TO CH-REASON-CD
                   MOVE 'SOME ACCOUNTS NOT UPDATED - SEE RESULT CODES'
                                           TO CH-MESSAGE-TEXT
               WHEN WS-WARNING-CD NOT = SPACES
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE WS-WARNING-CD      TO CH-REASON-CD
                   MOVE WS-WARNING-TEXT    TO CH-MESSAGE-TEXT
               WHEN WS-CHANGE-COUNT = ZERO
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'NCHG'             TO CH-REASON-CD
                   MOVE 'NOTHING CHANGED'  TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'PREFERENCES UPDATED'
                                           TO CH-MESSAGE-TEXT
           END-EVALUATE
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-CHECK-AUTHORITY.
      *================================================================*
      *    ONLY PURE WITHDRAWALS ARE ALLOWED FOR AN INACTIVE CUSTOMER -
      *    WS-OPT-IN = THIS REQUEST GIVES CONSENT OR CHANGES DATA.
           MOVE 'N'                        TO WS-OPT-IN-IND
           IF CHCP-UPD-CONTACT = 'Y'
               SET WS-OPT-IN               TO TRUE
           END-IF
           IF CHCP-UPD-MKTG = 'Y'
               IF (CHCP-MKTG-EMAIL-IND = 'Y'
                   AND CIF-MKTG-EMAIL-IND NOT = 'Y')
               OR (CHCP-MKTG-POST-IND = 'Y'
                   AND CIF-MKTG-POST-IND NOT = 'Y')
                   SET WS-OPT-IN           TO TRUE
               END-IF
           END-IF
           IF CHCP-UPD-STMT = 'Y'
               IF CHCP-ESTMT-IND = 'Y' OR CHCP-ACCT-COUNT > ZERO
                   SET WS-OPT-IN           TO TRUE
               END-IF
           END-IF

           EVALUATE TRUE
               WHEN WS-OPT-IN AND NOT CIF-ACTIVE
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CINA'             TO CH-REASON-CD
                   MOVE 'CUSTOMER NOT ACTIVE - ONLY OPT-OUTS ALLOWED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCP-UPD-CONTACT = 'Y'
               AND  CHCP-MOBILE-TEL NOT = CIF-MOBILE-TEL
               AND  CH-CHANNEL-CD NOT = 'BRANCH'
               AND  CH-CHANNEL-CD NOT = 'TELE  '
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'MAUT'             TO CH-REASON-CD
                   MOVE 'MOBILE NUMBER CAN ONLY BE CHANGED BY PHONE'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       3100-EXIT.
           EXIT.
      *================================================================*
       3200-APPLY-CONTACT.
      *================================================================*
      *    A BLANK FIELD REMOVES THE DETAIL (E.G. A LANDLINE GIVEN UP).
           IF CHCP-HOME-TEL NOT = CIF-HOME-TEL
               MOVE CHCP-HOME-TEL          TO CIF-HOME-TEL
               ADD 1                       TO WS-CHANGE-COUNT
           END-IF
           IF CHCP-MOBILE-TEL NOT = CIF-MOBILE-TEL
               MOVE CHCP-MOBILE-TEL        TO CIF-MOBILE-TEL
               ADD 1                       TO WS-CHANGE-COUNT
           END-IF
           IF CHCP-EMAIL-ADDR NOT = CIF-EMAIL-ADDR
               MOVE CHCP-EMAIL-ADDR        TO CIF-EMAIL-ADDR
               ADD 1                       TO WS-CHANGE-COUNT
           END-IF
           .
      *================================================================*
       3300-APPLY-CONSENTS.
      *================================================================*
      *    DECIDE THE NEW CIF-MKTG-EMAIL-IND, WHICH IS ALSO THE
      *    PAPERLESS FLAG (PIR 2009/233).
           MOVE CIF-MKTG-EMAIL-IND         TO WS-NEW-EMAIL-IND

           EVALUATE TRUE
      *        BOTH GIVEN - THEY MUST AGREE.
               WHEN CHCP-UPD-MKTG = 'Y'
               AND  CHCP-UPD-STMT = 'Y'
               AND  CHCP-ESTMT-IND NOT = SPACE
                   IF CHCP-MKTG-EMAIL-IND NOT = CHCP-ESTMT-IND
                       MOVE 'PCON'         TO CH-REASON-CD
                       MOVE 'E-STATEMENT AND E-MAIL MARKETING DIFFER'
                                           TO CH-MESSAGE-TEXT
                   ELSE
                       MOVE CHCP-ESTMT-IND TO WS-NEW-EMAIL-IND
                   END-IF
      *        E-STATEMENT ONLY - AN OPT-IN NEEDS EXPLICIT CONSENT.
               WHEN CHCP-UPD-STMT = 'Y'
               AND  CHCP-ESTMT-IND NOT = SPACE
                   IF CHCP-ESTMT-IND = 'Y'
                   AND CIF-MKTG-EMAIL-IND NOT = 'Y'
                       MOVE 'PCON'         TO CH-REASON-CD
                       MOVE 'E-STATEMENTS NEED E-MAIL CONSENT AS WELL'
                                           TO CH-MESSAGE-TEXT
                   ELSE
                       IF CHCP-ESTMT-IND = 'N'
                       AND CIF-MKTG-EMAIL-IND = 'Y'
                           MOVE 'MKLK'     TO WS-WARNING-CD
                           MOVE 'PAPER STATEMENTS - E-MAIL MKTG STOPPED'
                                           TO WS-WARNING-TEXT
                       END-IF
                       MOVE CHCP-ESTMT-IND TO WS-NEW-EMAIL-IND
                   END-IF
      *        MARKETING ONLY - WITHDRAWAL ALSO STOPS E-STATEMENTS.
               WHEN OTHER
                   IF CHCP-MKTG-EMAIL-IND = 'N'
                   AND CIF-MKTG-EMAIL-IND = 'Y'
                       MOVE 'PAPR'         TO WS-WARNING-CD
                       MOVE 'E-MAIL MARKETING OFF - PAPER STATEMENTS'
                                           TO WS-WARNING-TEXT
                   END-IF
                   MOVE CHCP-MKTG-EMAIL-IND
                                           TO WS-NEW-EMAIL-IND
           END-EVALUATE

           IF CH-REASON-CD = SPACES
           AND WS-NEW-EMAIL-IND = 'Y'
           AND CIF-EMAIL-ADDR = SPACES
               MOVE 'ESTE'                 TO CH-REASON-CD
               MOVE 'AN E-MAIL ADDRESS IS NEEDED FIRST'
                                           TO CH-MESSAGE-TEXT
           END-IF

           IF CH-REASON-CD NOT = SPACES
               MOVE '0008'                 TO CH-RETURN-CD
               GO TO 3300-EXIT
           END-IF

           IF WS-NEW-EMAIL-IND NOT = CIF-MKTG-EMAIL-IND
               MOVE WS-NEW-EMAIL-IND       TO CIF-MKTG-EMAIL-IND
               ADD 1                       TO WS-CHANGE-COUNT
           END-IF
           IF CHCP-UPD-MKTG = 'Y'
           AND CHCP-MKTG-POST-IND NOT = CIF-MKTG-POST-IND
               MOVE CHCP-MKTG-POST-IND     TO CIF-MKTG-POST-IND
               ADD 1                       TO WS-CHANGE-COUNT
           END-IF
           .
       3300-EXIT.
           EXIT.
      *================================================================*
       3500-REWRITE-CUSTOMER.
      *================================================================*
           MOVE WS-TODAY                   TO CIF-LAST-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO CIF-LAST-MAINT-TIME
           MOVE CH-USER-ID                 TO CIF-LAST-MAINT-USER
           EVALUATE CH-CHANNEL-CD
               WHEN 'BRANCH'
                   SET CIF-MAINT-BRANCH    TO TRUE
               WHEN 'TELE  '
                   SET CIF-MAINT-TELE      TO TRUE
               WHEN OTHER
                   SET CIF-MAINT-IBANK     TO TRUE
           END-EVALUATE
           EXEC CICS REWRITE
                     FILE('CIFMAST')
                     FROM(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CIFMAST'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 3500-EXIT
           END-IF
           MOVE 'N'                        TO WS-CIF-HELD-IND
           .
       3500-EXIT.
           EXIT.
      *================================================================*
       4000-UPDATE-ACCOUNTS.
      *================================================================*
           MOVE ZERO                       TO WS-ACCT-COUNT
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > CHCP-ACCT-COUNT
               MOVE SPACES                 TO WS-RESULT-CD
               PERFORM 4100-UPDATE-ONE-ACCOUNT THRU 4100-EXIT
               IF WS-RESULT-CD NOT = SPACES
               AND WS-RESULT-CD NOT = 'NOES'
                   ADD 1                   TO WS-FAILED-ACCTS
               END-IF
           END-PERFORM
           MOVE WS-ACCT-COUNT              TO CHCP-ACCT-RESP-COUNT
           .
      *================================================================*
       4100-UPDATE-ONE-ACCOUNT.
      *================================================================*
           MOVE CHCPQ-SORT-CODE (WS-SUB)   TO WS-AK-SORT-CODE
           MOVE CHCPQ-ACCT-NO (WS-SUB)     TO WS-AK-ACCT-NO
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
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE 'ANFD'             TO WS-RESULT-CD
                   PERFORM 4900-ACCOUNT-NOT-FOUND
                   GO TO 4100-EXIT
      *        THE CUSTOMER IS ALREADY REWRITTEN - BACK IT ALL OUT.
               WHEN OTHER
                   EXEC CICS ABEND
                             ABCODE('CPF1')
                   END-EXEC
           END-EVALUATE

           EVALUATE TRUE
               WHEN ACM-CIF-NO NOT = CHCP-CIF-NO
                   MOVE 'ACIF'             TO WS-RESULT-CD
               WHEN ACM-CLOSED OR ACM-PENDING-CLOSE
                   MOVE 'ACLS'             TO WS-RESULT-CD
               WHEN CHCPQ-STMT-CYCLE (WS-SUB) NOT = 'M' AND 'Q' AND 'A'
               WHEN CHCPQ-STMT-DAY (WS-SUB) NOT NUMERIC
               WHEN CHCPQ-STMT-DAY (WS-SUB) < 1
               WHEN CHCPQ-STMT-DAY (WS-SUB) > 28
                   MOVE 'INVY'             TO WS-RESULT-CD
               WHEN ACM-CURRENT
               AND  CHCPQ-STMT-CYCLE (WS-SUB) NOT = 'M'
                   MOVE 'CYCM'             TO WS-RESULT-CD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-RESULT-CD NOT = SPACES
               EXEC CICS UNLOCK
                         FILE('ACCTMST')
                         RESP(WS-RESP)
               END-EXEC
      *        DO NOT ECHO ANOTHER CUSTOMER'S ACCOUNT DETAILS.
               IF WS-RESULT-CD = 'ACIF'
                   PERFORM 4900-ACCOUNT-NOT-FOUND
               ELSE
                   ADD 1                   TO WS-ACCT-COUNT
                   PERFORM 7100-FORMAT-ACCOUNT
               END-IF
               GO TO 4100-EXIT
           END-IF

           IF ACM-STMT-CYCLE = CHCPQ-STMT-CYCLE (WS-SUB)
           AND ACM-STMT-DAY = CHCPQ-STMT-DAY (WS-SUB)
               EXEC CICS UNLOCK
                         FILE('ACCTMST')
                         RESP(WS-RESP)
               END-EXEC
           ELSE
               MOVE SPACES                 TO CPA-DETAIL
               MOVE ACM-SORT-CODE          TO CPA-SORT-CODE
               MOVE ACM-ACCT-NO            TO CPA-ACCT-NO
               MOVE ACM-STMT-CYCLE         TO CPA-OLD-CYCLE
               MOVE ACM-STMT-DAY           TO CPA-OLD-DAY
               MOVE CHCPQ-STMT-CYCLE (WS-SUB)
                                           TO ACM-STMT-CYCLE
                                              CPA-NEW-CYCLE
               MOVE CHCPQ-STMT-DAY (WS-SUB)
                                           TO ACM-STMT-DAY
                                              CPA-NEW-DAY
               MOVE WS-TODAY               TO ACM-LAST-MAINT-DATE
               MOVE WS-TIME-HHMMSS         TO ACM-LAST-MAINT-TIME
               MOVE CH-USER-ID             TO ACM-LAST-MAINT-USER
               EXEC CICS REWRITE
                         FILE('ACCTMST')
                         FROM(ACM-ACCOUNT-RECORD)
                         LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   EXEC CICS ABEND
                             ABCODE('CPF2')
                   END-EXEC
               END-IF
               ADD 1                       TO WS-CHANGE-COUNT
               SET CPA-ACCOUNT             TO TRUE
               PERFORM 8000-WRITE-AUDIT
           END-IF

           ADD 1                           TO WS-ACCT-COUNT
           PERFORM 7100-FORMAT-ACCOUNT
           .
       4100-EXIT.
           EXIT.
      *================================================================*
       4900-ACCOUNT-NOT-FOUND.
      *================================================================*
           ADD 1                           TO WS-ACCT-COUNT
           MOVE SPACES                 TO CHCP-ACCT-RESP (WS-ACCT-COUNT)
           MOVE WS-AK-SORT-CODE      TO CHCPA-SORT-CODE (WS-ACCT-COUNT)
           MOVE WS-AK-ACCT-NO        TO CHCPA-ACCT-NO (WS-ACCT-COUNT)
           MOVE ZERO                 TO CHCPA-STMT-DAY (WS-ACCT-COUNT)
           MOVE ZERO            TO CHCPA-LAST-STMT-DATE (WS-ACCT-COUNT)
           MOVE WS-RESULT-CD         TO CHCPA-RESULT-CD (WS-ACCT-COUNT)
           .
      *================================================================*
       7000-FORMAT-CUSTOMER.
      *================================================================*
           MOVE SPACES                     TO CHCP-CUST-NAME
           STRING CIF-TITLE DELIMITED BY SPACE
                  ' '       DELIMITED BY SIZE
                  CIF-FIRST-NM DELIMITED BY SPACE
                  ' '       DELIMITED BY SIZE
                  CIF-LAST-NM DELIMITED BY SPACE
                  INTO CHCP-CUST-NAME
           END-STRING
           MOVE CIF-HOME-TEL               TO CHCP-CUR-HOME-TEL
           MOVE CIF-MOBILE-TEL             TO CHCP-CUR-MOBILE-TEL
           MOVE CIF-EMAIL-ADDR             TO CHCP-CUR-EMAIL-ADDR
           MOVE CIF-MKTG-EMAIL-IND         TO CHCP-CUR-MKTG-EMAIL
           MOVE CIF-MKTG-POST-IND          TO CHCP-CUR-MKTG-POST
           MOVE CIF-LAST-MAINT-DATE        TO CHCP-LAST-MAINT-DATE
           MOVE CIF-LAST-MAINT-SRCE        TO CHCP-LAST-MAINT-SRCE
           IF CIF-MKTG-EMAIL-IND = 'Y'
           AND CIF-EMAIL-ADDR NOT = SPACES
               MOVE 'Y'                    TO CHCP-CUR-ESTMT
           ELSE
               MOVE 'N'                    TO CHCP-CUR-ESTMT
           END-IF
           .
      *================================================================*
       7100-FORMAT-ACCOUNT.
      *================================================================*
      *    ENTRY WS-ACCT-COUNT. NOES = PAPERLESS CUSTOMER BUT THE
      *    PRODUCT CANNOT HAVE E-STATEMENTS.
           MOVE ACM-PROD-CD                TO WS-PRD-KEY
           EXEC CICS READ
                     FILE('PRDPARM')
                     INTO(PRD-PARAM-RECORD)
                     LENGTH(LENGTH OF PRD-PARAM-RECORD)
                     RIDFLD(WS-PRD-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'N'                    TO PRD-ESTMT-ALLOWED
           END-IF
           IF WS-RESULT-CD = SPACES
           AND CIF-MKTG-EMAIL-IND = 'Y'
           AND PRD-ESTMT-ALLOWED NOT = 'Y'
               MOVE 'NOES'                 TO WS-RESULT-CD
           END-IF

           MOVE ACM-SORT-CODE    TO CHCPA-SORT-CODE (WS-ACCT-COUNT)
           MOVE ACM-ACCT-NO      TO CHCPA-ACCT-NO (WS-ACCT-COUNT)
           MOVE ACM-PROD-CD      TO CHCPA-PROD-CD (WS-ACCT-COUNT)
           MOVE ACM-STMT-CYCLE   TO CHCPA-STMT-CYCLE (WS-ACCT-COUNT)
           MOVE ACM-STMT-DAY     TO CHCPA-STMT-DAY (WS-ACCT-COUNT)
           MOVE PRD-ESTMT-ALLOWED TO CHCPA-ESTMT-ALLOWED (WS-ACCT-COUNT)
           MOVE ACM-LAST-STMT-DATE
                              TO CHCPA-LAST-STMT-DATE (WS-ACCT-COUNT)
           MOVE WS-RESULT-CD     TO CHCPA-RESULT-CD (WS-ACCT-COUNT)
           .
      *================================================================*
       8000-WRITE-AUDIT.
      *================================================================*
      *    CALLER SETS CPA-REC-TYPE AND CPA-DETAIL.
           MOVE WS-TODAY                   TO CPA-DATE
           MOVE WS-TIME-HHMMSS             TO CPA-TIME
           MOVE EIBTASKN                   TO CPA-TASKNO
           MOVE CH-CHANNEL-CD              TO CPA-CHANNEL
           MOVE CH-USER-ID                 TO CPA-USER
           MOVE CH-REQUEST-ID              TO CPA-REQUEST-ID
           MOVE CHCP-FUNCTION              TO CPA-FUNCTION
           MOVE CHCP-CIF-NO                TO CPA-CIF-NO
           MOVE CHCP-CONSENT-SOURCE        TO CPA-CONSENT-SOURCE
           MOVE CHCP-CONSENT-REF           TO CPA-CONSENT-REF
           MOVE CH-RETURN-CD               TO CPA-RETURN-CD
           MOVE CH-REASON-CD               TO CPA-REASON-CD

           EXEC CICS WRITEQ TD
                     QUEUE('CPAU')
                     FROM(WS-AUDIT-RECORD)
                     LENGTH(LENGTH OF WS-AUDIT-RECORD)
                     RESP(WS-RESP)
           END-EXEC
      *    UNLIKE CFAU, A FAILURE BACKS THE UPDATE OUT (UNPROVABLE).
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS WRITE OPERATOR
                         TEXT('MBKCP001E CPAU AUDIT WRITE FAILED')
                         RESP(WS-RESP)
               END-EXEC
               IF NOT CPA-ENQUIRY
                   EXEC CICS ABEND
                             ABCODE('CPF3')
                   END-EXEC
               END-IF
           END-IF
           .
      *================================================================*
       8100-CHECK-EMAIL.
      *================================================================*
      *    ONE '@' NOT FIRST, A '.' AFTER IT NOT LAST, NO SPACES.
           MOVE 'N'                        TO WS-EMAIL-VALID-IND
           MOVE ZERO                       TO WS-AT-POS
                                              WS-AT-COUNT
                                              WS-DOT-AFTER
                                              WS-LAST-POS
           PERFORM VARYING WS-POS FROM 80 BY -1
                   UNTIL WS-POS < 1 OR WS-LAST-POS > ZERO
               IF CHCP-EMAIL-ADDR (WS-POS:1) NOT = SPACE
                   MOVE WS-POS             TO WS-LAST-POS
               END-IF
           END-PERFORM
           PERFORM VARYING WS-POS FROM 1 BY 1
                   UNTIL WS-POS > WS-LAST-POS
               EVALUATE CHCP-EMAIL-ADDR (WS-POS:1)
                   WHEN SPACE
                       GO TO 8100-EXIT
                   WHEN '@'
                       ADD 1               TO WS-AT-COUNT
                       MOVE WS-POS         TO WS-AT-POS
                   WHEN '.'
                       IF WS-AT-POS > ZERO
                       AND WS-POS > WS-AT-POS + 1
                           MOVE WS-POS     TO WS-DOT-AFTER
                       END-IF
               END-EVALUATE
           END-PERFORM
           IF WS-AT-COUNT = 1
           AND WS-AT-POS > 1
           AND WS-DOT-AFTER > ZERO
           AND WS-DOT-AFTER < WS-LAST-POS
               SET WS-EMAIL-VALID          TO TRUE
           END-IF
           .
       8100-EXIT.
           EXIT.
      *================================================================*
       8200-CHECK-PHONE.
      *================================================================*
      *    DIGITS / SPACES, OPTIONAL LEADING '+', 10+ DIGITS.
           MOVE 'N'                        TO WS-PHONE-VALID-IND
           MOVE ZERO                       TO WS-DIGITS
           PERFORM VARYING WS-POS FROM 1 BY 1 UNTIL WS-POS > 20
               EVALUATE TRUE
                   WHEN WS-PHONE (WS-POS:1) NUMERIC
                       ADD 1               TO WS-DIGITS
                   WHEN WS-PHONE (WS-POS:1) = SPACE
                       CONTINUE
                   WHEN WS-PHONE (WS-POS:1) = '+' AND WS-POS = 1
                       CONTINUE
                   WHEN OTHER
                       GO TO 8200-EXIT
               END-EVALUATE
           END-PERFORM
           IF WS-DIGITS >= WS-MIN-PHONE-DIGITS
               SET WS-PHONE-VALID          TO TRUE
           END-IF
           .
       8200-EXIT.
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
