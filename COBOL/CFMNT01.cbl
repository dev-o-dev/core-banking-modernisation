       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFMNT01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-06-28.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFMNT01                                             *
      * TITLE    : CUSTOMER MAINTENANCE - CHANNEL SERVICE CIFMAINT     *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : CIFMAST  READ / READ UPDATE / REWRITE / WRITE       *
      *            CIFCTL   READ UPDATE / REWRITE (NEXT CIF NUMBER)    *
      * TDQUEUE  : CFAU     CIF CHANGE AUDIT (BEFORE / AFTER IMAGES)   *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-CIFMAINT-AREA              *
      *----------------------------------------------------------------*
      * REAL-TIME CUSTOMER MAINTENANCE. UNTIL 2010 ALL IBANK CHANGES   *
      * WENT THROUGH THE NIGHTLY CUSTMNT2 FILE (CFMNT201) AND BRANCH   *
      * CHANGES THROUGH THE TELLER SYSTEM. CUSTMNT2 STILL RUNS FOR     *
      * CHANGES CAPTURED WHILE THE HOST IS IN THE BATCH WINDOW.        *
      *                                                                *
      * FUNCTIONS                                                      *
      *   N NEW CUSTOMER   - BRANCH ONLY. ALLOCATES THE NEXT CIF       *
      *                      NUMBER. A POSSIBLE DUPLICATE (SAME NAME,  *
      *                      DATE OF BIRTH AND POST CODE ON THE LAST   *
      *                      200 CIFS) IS RETURNED AS A WARNING. THE   *
      *                      FULL DUPLICATE CHECK RUNS OVERNIGHT       *
      *                      (CFDUP010).                               *
      *   A ADDRESS        - ADDRESS LINES, TOWN, COUNTY, POST CODE    *
      *   C CONTACT        - TELEPHONES AND EMAIL                      *
      *   E EMPLOYMENT     - STATUS, OCCUPATION, INCOME                *
      *   M MARKETING      - EMAIL / POST MARKETING PREFERENCES        *
      *   K KYC / RISK     - BRANCH ONLY, USER IN RACF GROUP BRKYC     *
      *                      (CHECKED WITH EXEC CICS QUERY SECURITY)   *
      *                                                                *
      * OPTIMISTIC LOCKING: THE CALLER PASSES THE LAST-MAINT DATE AND  *
      * TIME IT READ. IF THE RECORD HAS CHANGED SINCE, REASON 'STAL'.  *
      *                                                                *
      * EVERY CHANGE WRITES A BEFORE AND AN AFTER IMAGE TO TDQ CFAU    *
      * (EXTRAPARTITION, MBK.PRD.CICS.CFAU) FOR THE DATA PROTECTION    *
      * AUDIT TRAIL.                                                   *
      *----------------------------------------------------------------*
      * REASON CODES                                                   *
      *   CNFD CUSTOMER NOT FOUND    CCLS CUSTOMER CLOSED / DECEASED   *
      *   STAL RECORD CHANGED BY SOMEONE ELSE                          *
      *   AUTH NOT AUTHORISED FOR THIS FUNCTION                        *
      *   DUPW POSSIBLE DUPLICATE (WARNING ON NEW CUSTOMER)            *
      *   INVx INVALID FIELD                                           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-06-28 JNB  ORIGINAL VERSION                               *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFMNT01 '.
      *
       01  WS-CONSTANTS.
           05  WS-DUP-SCAN-LIMIT           PIC 9(03) VALUE 200.
           05  WS-KYC-RESOURCE             PIC X(44)
               VALUE 'MBK.CIF.KYC.UPDATE'.
           05  WS-MAX-INCOME               PIC S9(13)V99 COMP-3
                                           VALUE +99999999.99.
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
           05  WS-CIF-KEY                  PIC X(12).
           05  WS-HELD-IND                 PIC X(01) VALUE 'N'.
               88  WS-CIF-HELD                 VALUE 'Y'.
           05  WS-READ-AUTH                PIC S9(08) COMP.
           05  WS-UPDATE-AUTH              PIC S9(08) COMP.
           05  WS-AT-SIGN-COUNT            PIC S9(04) COMP.
           05  WS-DOT-COUNT                PIC S9(04) COMP.
           05  WS-SCAN-COUNT               PIC 9(03).
           05  WS-NEW-CIF-NUM              PIC 9(11).
           05  WS-CHECK-DIGIT              PIC 9(01).
           05  WS-CHECK-TOTAL              PIC S9(05) COMP.
           05  WS-CHECK-SUB                PIC S9(04) COMP.
           05  WS-CHECK-QUOT               PIC S9(05) COMP.
           05  WS-CHECK-REM                PIC S9(04) COMP.
           05  WS-DIGIT                    PIC 9(01).
           05  WS-DUP-CIF                  PIC X(12).
           05  WS-POSTCODE-UPPER           PIC X(10).
           05  WS-AUDIT-SEQ                PIC 9(02).
      *
      *---- CIF CONTROL RECORD (CIFCTL) - ONE RECORD, KEY 'NEXTCIF' ---*
       01  WS-CIF-CONTROL.
           05  CTL-KEY                     PIC X(12).
           05  CTL-LAST-CIF-NUM            PIC 9(11).
           05  CTL-ALLOC-COUNT             PIC 9(09) COMP.
           05  CTL-LAST-ALLOC-DATE         PIC 9(08).
           05  FILLER                      PIC X(49).
      *
      *---- AUDIT RECORD (TDQ CFAU, LRECL 760) ------------------------*
       01  WS-AUDIT-RECORD.
           05  AUD-DATE                    PIC 9(08).
           05  AUD-TIME                    PIC 9(06).
           05  AUD-TASKNO                  PIC 9(07).
           05  AUD-CHANNEL                 PIC X(06).
           05  AUD-USER                    PIC X(08).
           05  AUD-FUNCTION                PIC X(01).
           05  AUD-IMAGE-TYPE              PIC X(01).
               88  AUD-BEFORE                  VALUE 'B'.
               88  AUD-AFTER                   VALUE 'A'.
           05  AUD-REQUEST-ID              PIC X(24).
           05  AUD-CIF-IMAGE               PIC X(700).
           05  FILLER                      PIC X(07).
      *
       01  WS-BEFORE-IMAGE                 PIC X(700).
      *
           COPY CFCUSREC.
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
                                              CHCM-NEW-CIF-NO
                                              CHCM-DUP-WARNING-CIF
           MOVE ZERO                       TO CHCM-NEW-MAINT-DATE
                                              CHCM-NEW-MAINT-TIME

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC

           PERFORM 1000-CHECK-FUNCTION
           IF CH-RC-OK
               EVALUATE TRUE
                   WHEN CHCM-NEW
                       PERFORM 2000-NEW-CUSTOMER THRU 2000-EXIT
                   WHEN OTHER
                       PERFORM 3000-UPDATE-CUSTOMER THRU 3000-EXIT
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
       1000-CHECK-FUNCTION.
      *================================================================*
           EVALUATE TRUE
               WHEN NOT (CHCM-NEW OR CHCM-ADDRESS OR CHCM-CONTACT
                         OR CHCM-EMPLOYMENT OR CHCM-MARKETING
                         OR CHCM-KYC)
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'FUNC'             TO CH-REASON-CD
                   MOVE 'INVALID FUNCTION - USE N, A, C, E, M OR K'
                                           TO CH-MESSAGE-TEXT
               WHEN (CHCM-NEW OR CHCM-KYC)
               AND  CH-CHANNEL-CD NOT = 'BRANCH'
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'AUTH'             TO CH-REASON-CD
                   MOVE 'NEW CUSTOMERS AND KYC CHANGES ARE BRANCH ONLY'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT CHCM-NEW
               AND (CHCM-CIF-NO = SPACES OR LOW-VALUES)
                   MOVE '0016'             TO CH-RETURN-CD
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'CIF NUMBER REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCM-KYC
                   PERFORM 1100-CHECK-KYC-AUTHORITY
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       1100-CHECK-KYC-AUTHORITY.
      *================================================================*
      *    THE BRANCH USER MUST HAVE UPDATE ACCESS TO THE RACF FACILITY
      *    PROFILE MBK.CIF.KYC.UPDATE (GRANTED TO GROUP BRKYC).
           EXEC CICS QUERY SECURITY
                     RESTYPE('FACILITY')
                     RESID(WS-KYC-RESOURCE)
                     RESIDLENGTH(LENGTH OF WS-KYC-RESOURCE)
                     READ(WS-READ-AUTH)
                     UPDATE(WS-UPDATE-AUTH)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
           OR WS-UPDATE-AUTH NOT = DFHVALUE(UPDATEABLE)
               MOVE '0008'                 TO CH-RETURN-CD
               MOVE 'AUTH'                 TO CH-REASON-CD
               MOVE 'USER NOT AUTHORISED TO CHANGE KYC STATUS'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       2000-NEW-CUSTOMER.
      *================================================================*
           PERFORM 2100-VALIDATE-NEW
           IF NOT CH-RC-OK
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-DUPLICATE-SCAN THRU 2200-EXIT
           PERFORM 2300-ALLOCATE-CIF THRU 2300-EXIT
           IF NOT CH-RC-OK
               GO TO 2000-EXIT
           END-IF
           PERFORM 2400-WRITE-CUSTOMER
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-VALIDATE-NEW.
      *================================================================*
           EVALUATE TRUE
               WHEN CHCM-CUST-TYPE NOT = 'P' AND 'B'
                   MOVE 'INVT'             TO CH-REASON-CD
                   MOVE 'CUSTOMER TYPE MUST BE P OR B'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCM-LAST-NM = SPACES
                   MOVE 'INVN'             TO CH-REASON-CD
                   MOVE 'SURNAME / COMPANY NAME REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCM-CUST-TYPE = 'P'
               AND (CHCM-DOB NOT NUMERIC OR CHCM-DOB = ZERO
                    OR CHCM-DOB > WS-TODAY OR CHCM-DOB < 19000101)
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'DATE OF BIRTH REQUIRED AND MUST BE VALID'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   PERFORM 3100-VALIDATE-ADDRESS
                   IF CH-REASON-CD = SPACES
                       PERFORM 3200-VALIDATE-CONTACT
                   END-IF
                   IF CH-REASON-CD = SPACES
                       PERFORM 3300-VALIDATE-EMPLOYMENT
                   END-IF
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
           END-IF
           .
      *================================================================*
       2200-DUPLICATE-SCAN.
      *================================================================*
      *    QUICK CHECK OF THE MOST RECENT CIFS ONLY (BROWSE BACKWARDS
      *    FROM HIGH VALUES). NOT A SUBSTITUTE FOR CFDUP010.
           MOVE HIGH-VALUES                TO WS-CIF-KEY
           MOVE ZERO                       TO WS-SCAN-COUNT
           MOVE FUNCTION UPPER-CASE (CHCM-POST-CD) TO WS-POSTCODE-UPPER
           EXEC CICS STARTBR
                     FILE('CIFMAST')
                     RIDFLD(WS-CIF-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 2200-EXIT
           END-IF
           PERFORM UNTIL WS-SCAN-COUNT >= WS-DUP-SCAN-LIMIT
               EXEC CICS READPREV
                         FILE('CIFMAST')
                         INTO(CIF-CUSTOMER-RECORD)
                         LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                         RIDFLD(WS-CIF-KEY)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE WS-DUP-SCAN-LIMIT  TO WS-SCAN-COUNT
               ELSE
                   ADD 1                   TO WS-SCAN-COUNT
                   IF CIF-LAST-NM = CHCM-LAST-NM
                   AND CIF-DOB = CHCM-DOB
                   AND FUNCTION UPPER-CASE (CIF-POST-CD)
                       = WS-POSTCODE-UPPER
                       MOVE CIF-NO         TO WS-DUP-CIF
                       MOVE WS-DUP-SCAN-LIMIT TO WS-SCAN-COUNT
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR
                     FILE('CIFMAST')
                     RESP(WS-RESP)
           END-EXEC
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       2300-ALLOCATE-CIF.
      *================================================================*
      *    CIF NUMBER FORMAT: 'C' + 10 DIGITS + MOD 10 CHECK DIGIT.
           MOVE 'NEXTCIF'                  TO CTL-KEY
           EXEC CICS READ
                     FILE('CIFCTL')
                     INTO(WS-CIF-CONTROL)
                     LENGTH(LENGTH OF WS-CIF-CONTROL)
                     RIDFLD(CTL-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CIFCTL'               TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 2300-EXIT
           END-IF

           ADD 1                           TO CTL-LAST-CIF-NUM
           ADD 1                           TO CTL-ALLOC-COUNT
           MOVE WS-TODAY                   TO CTL-LAST-ALLOC-DATE
           MOVE CTL-LAST-CIF-NUM           TO WS-NEW-CIF-NUM

           EXEC CICS REWRITE
                     FILE('CIFCTL')
                     FROM(WS-CIF-CONTROL)
                     LENGTH(LENGTH OF WS-CIF-CONTROL)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CIFCTL'               TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 2300-EXIT
           END-IF

      *    LUHN-STYLE MOD 10 OVER THE 10 LOW-ORDER DIGITS.
           MOVE ZERO                       TO WS-CHECK-TOTAL
           PERFORM VARYING WS-CHECK-SUB FROM 2 BY 1
                   UNTIL WS-CHECK-SUB > 11
               MOVE WS-NEW-CIF-NUM (WS-CHECK-SUB:1) TO WS-DIGIT
               DIVIDE WS-CHECK-SUB BY 2 GIVING WS-CHECK-QUOT
                      REMAINDER WS-CHECK-REM
               IF WS-CHECK-REM = ZERO
                   COMPUTE WS-CHECK-TOTAL = WS-CHECK-TOTAL
                         + FUNCTION MOD (WS-DIGIT * 2, 9)
                   IF WS-DIGIT = 9
                       ADD 9               TO WS-CHECK-TOTAL
                   END-IF
               ELSE
                   ADD WS-DIGIT            TO WS-CHECK-TOTAL
               END-IF
           END-PERFORM
           COMPUTE WS-CHECK-DIGIT =
                   FUNCTION MOD (10 - FUNCTION MOD (WS-CHECK-TOTAL, 10),
                                 10)
           MOVE SPACES                     TO CHCM-NEW-CIF-NO
           STRING 'C' WS-NEW-CIF-NUM (2:10) WS-CHECK-DIGIT
                  DELIMITED BY SIZE
                  INTO CHCM-NEW-CIF-NO
           END-STRING
           .
       2300-EXIT.
           EXIT.
      *================================================================*
       2400-WRITE-CUSTOMER.
      *================================================================*
           MOVE SPACES                     TO CIF-CUSTOMER-RECORD
           MOVE CHCM-NEW-CIF-NO            TO CIF-NO
           SET CIF-ACTIVE                  TO TRUE
           MOVE CHCM-CUST-TYPE             TO CIF-CUST-TYPE
           MOVE CHCM-TITLE                 TO CIF-TITLE
           MOVE CHCM-FIRST-NM              TO CIF-FIRST-NM
           MOVE CHCM-MIDDLE-NM             TO CIF-MIDDLE-NM
           MOVE CHCM-LAST-NM               TO CIF-LAST-NM
           MOVE CHCM-DOB                   TO CIF-DOB
           MOVE CHCM-NAT-INS-NO            TO CIF-NAT-INS-NO
           PERFORM 3150-MOVE-ADDRESS
           PERFORM 3250-MOVE-CONTACT
           PERFORM 3350-MOVE-EMPLOYMENT
           MOVE CHCM-MKTG-EMAIL-IND        TO CIF-MKTG-EMAIL-IND
           MOVE CHCM-MKTG-POST-IND         TO CIF-MKTG-POST-IND
           MOVE 'STANDARD'                 TO CIF-SEGMENT-CD
      *    KYC IS ALWAYS PENDING ON CREATION - THE BRANCH COMPLETES THE
      *    ID&V CHECKS AND THEN USES FUNCTION K (FOUR EYES).
           SET CIF-KYC-PENDING             TO TRUE
           SET CIF-RISK-MEDIUM             TO TRUE
           MOVE CHCM-HOME-BRANCH-SC        TO CIF-HOME-BRANCH-SC
           MOVE WS-TODAY                   TO CIF-CREATE-DATE
                                              CIF-LAST-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO CIF-LAST-MAINT-TIME
           MOVE CH-USER-ID                 TO CIF-LAST-MAINT-USER
           SET CIF-MAINT-BRANCH            TO TRUE

           MOVE CIF-NO                     TO WS-CIF-KEY
           EXEC CICS WRITE
                     FILE('CIFMAST')
                     FROM(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(WS-CIF-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
      *        CIFCTL WAS ALREADY UPDATED - BACK IT OUT TOO.
               EXEC CICS ABEND
                         ABCODE('CFM1')
               END-EXEC
           END-IF

           MOVE SPACES                     TO WS-BEFORE-IMAGE
           PERFORM 8000-WRITE-AUDIT
           MOVE WS-TODAY                   TO CHCM-NEW-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO CHCM-NEW-MAINT-TIME

           IF WS-DUP-CIF NOT = SPACES
               MOVE '0004'                 TO CH-RETURN-CD
               MOVE 'DUPW'                 TO CH-REASON-CD
               MOVE WS-DUP-CIF             TO CHCM-DUP-WARNING-CIF
               MOVE 'CUSTOMER CREATED - POSSIBLE DUPLICATE OF CIF SHOWN'
                                           TO CH-MESSAGE-TEXT
           ELSE
               MOVE 'CUSTOMER CREATED - KYC PENDING'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       3000-UPDATE-CUSTOMER.
      *================================================================*
           MOVE CHCM-CIF-NO                TO WS-CIF-KEY
           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(WS-CIF-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   SET WS-CIF-HELD         TO TRUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CNFD'             TO CH-REASON-CD
                   MOVE 'CUSTOMER NOT FOUND'
                                           TO CH-MESSAGE-TEXT
                   GO TO 3000-EXIT
               WHEN OTHER
                   MOVE 'CIFMAST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 3000-EXIT
           END-EVALUATE

           EVALUATE TRUE
               WHEN CIF-CLOSED OR CIF-DECEASED
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CCLS'             TO CH-REASON-CD
                   MOVE 'CUSTOMER IS CLOSED OR DECEASED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCM-LAST-MAINT-DATE NOT = CIF-LAST-MAINT-DATE
               WHEN CHCM-LAST-MAINT-TIME NOT = CIF-LAST-MAINT-TIME
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'STAL'             TO CH-REASON-CD
                   MOVE 'CUSTOMER CHANGED SINCE READ - PLEASE RETRY'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT CH-RC-OK
               GO TO 3000-EXIT
           END-IF

           MOVE CIF-CUSTOMER-RECORD        TO WS-BEFORE-IMAGE

           EVALUATE TRUE
               WHEN CHCM-ADDRESS
                   PERFORM 3100-VALIDATE-ADDRESS
                   IF CH-REASON-CD = SPACES
                       PERFORM 3150-MOVE-ADDRESS
                   END-IF
               WHEN CHCM-CONTACT
                   PERFORM 3200-VALIDATE-CONTACT
                   IF CH-REASON-CD = SPACES
                       PERFORM 3250-MOVE-CONTACT
                   END-IF
               WHEN CHCM-EMPLOYMENT
                   PERFORM 3300-VALIDATE-EMPLOYMENT
                   IF CH-REASON-CD = SPACES
                       PERFORM 3350-MOVE-EMPLOYMENT
                   END-IF
               WHEN CHCM-MARKETING
                   IF CHCM-MKTG-EMAIL-IND NOT = 'Y' AND 'N'
                   OR CHCM-MKTG-POST-IND NOT = 'Y' AND 'N'
                       MOVE 'INVM'         TO CH-REASON-CD
                       MOVE 'MARKETING INDICATORS MUST BE Y OR N'
                                           TO CH-MESSAGE-TEXT
                   ELSE
                       MOVE CHCM-MKTG-EMAIL-IND TO CIF-MKTG-EMAIL-IND
                       MOVE CHCM-MKTG-POST-IND  TO CIF-MKTG-POST-IND
                   END-IF
               WHEN CHCM-KYC
                   PERFORM 3400-UPDATE-KYC
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               IF CH-RC-OK
                   MOVE '0016'             TO CH-RETURN-CD
               END-IF
               GO TO 3000-EXIT
           END-IF

           MOVE WS-TODAY                   TO CIF-LAST-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO CIF-LAST-MAINT-TIME
           MOVE CH-USER-ID                 TO CIF-LAST-MAINT-USER
           IF CH-CHANNEL-CD = 'BRANCH'
               SET CIF-MAINT-BRANCH        TO TRUE
           ELSE
               IF CH-CHANNEL-CD = 'TELE  '
                   SET CIF-MAINT-TELE      TO TRUE
               ELSE
                   SET CIF-MAINT-IBANK     TO TRUE
               END-IF
           END-IF

           EXEC CICS REWRITE
                     FILE('CIFMAST')
                     FROM(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CIFMAST'              TO WS-FILE-NAME
               PERFORM 9000-FILE-ERROR
               GO TO 3000-EXIT
           END-IF
           MOVE 'N'                        TO WS-HELD-IND

           PERFORM 8000-WRITE-AUDIT
           MOVE WS-TODAY                   TO CHCM-NEW-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO CHCM-NEW-MAINT-TIME
           MOVE 'CUSTOMER UPDATED'         TO CH-MESSAGE-TEXT
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-VALIDATE-ADDRESS.
      *================================================================*
           EVALUATE TRUE
               WHEN CHCM-ADDR-LINE1 = SPACES
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'ADDRESS LINE 1 REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCM-TOWN = SPACES
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'TOWN REQUIRED'    TO CH-MESSAGE-TEXT
               WHEN CHCM-CTRY-CD = SPACES
                   MOVE 'GB'               TO CHCM-CTRY-CD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *    UK POST CODES ARE REQUIRED AND MUST CONTAIN A SPACE.
           IF CH-REASON-CD = SPACES
           AND CHCM-CTRY-CD = 'GB'
               IF CHCM-POST-CD = SPACES
                   MOVE 'INVP'             TO CH-REASON-CD
                   MOVE 'UK POST CODE REQUIRED'
                                           TO CH-MESSAGE-TEXT
               ELSE
                   MOVE ZERO               TO WS-AT-SIGN-COUNT
                   INSPECT FUNCTION TRIM (CHCM-POST-CD)
                           TALLYING WS-AT-SIGN-COUNT FOR ALL SPACE
                   IF WS-AT-SIGN-COUNT NOT = 1
                       MOVE 'INVP'         TO CH-REASON-CD
                       MOVE 'UK POST CODE FORMAT INVALID'
                                           TO CH-MESSAGE-TEXT
                   END-IF
               END-IF
           END-IF
           .
      *================================================================*
       3150-MOVE-ADDRESS.
      *================================================================*
           MOVE CHCM-ADDR-LINE1            TO CIF-ADDR-LINE1
           MOVE CHCM-ADDR-LINE2            TO CIF-ADDR-LINE2
           MOVE CHCM-TOWN                  TO CIF-TOWN
           MOVE CHCM-COUNTY                TO CIF-COUNTY
           MOVE FUNCTION UPPER-CASE (CHCM-POST-CD) TO CIF-POST-CD
           MOVE CHCM-CTRY-CD               TO CIF-CTRY-CD
           .
      *================================================================*
       3200-VALIDATE-CONTACT.
      *================================================================*
           IF CHCM-EMAIL-ADDR NOT = SPACES
               MOVE ZERO                   TO WS-AT-SIGN-COUNT
                                              WS-DOT-COUNT
               INSPECT CHCM-EMAIL-ADDR TALLYING WS-AT-SIGN-COUNT
                       FOR ALL '@'
               INSPECT CHCM-EMAIL-ADDR TALLYING WS-DOT-COUNT
                       FOR ALL '.'
               IF WS-AT-SIGN-COUNT NOT = 1 OR WS-DOT-COUNT = ZERO
                   MOVE 'INVE'             TO CH-REASON-CD
                   MOVE 'EMAIL ADDRESS FORMAT INVALID'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           .
      *================================================================*
       3250-MOVE-CONTACT.
      *================================================================*
           MOVE CHCM-HOME-TEL              TO CIF-HOME-TEL
           MOVE CHCM-MOBILE-TEL            TO CIF-MOBILE-TEL
           MOVE CHCM-EMAIL-ADDR            TO CIF-EMAIL-ADDR
           .
      *================================================================*
       3300-VALIDATE-EMPLOYMENT.
      *================================================================*
           IF CHCM-ANN-INCOME NOT NUMERIC
           OR CHCM-ANN-INCOME < ZERO
           OR CHCM-ANN-INCOME > WS-MAX-INCOME
               MOVE 'INVI'                 TO CH-REASON-CD
               MOVE 'ANNUAL INCOME INVALID'
                                           TO CH-MESSAGE-TEXT
           END-IF
           .
      *================================================================*
       3350-MOVE-EMPLOYMENT.
      *================================================================*
           MOVE CHCM-EMPL-STATUS           TO CIF-EMPL-STATUS
           MOVE CHCM-OCCUPATION            TO CIF-OCCUPATION
           MOVE CHCM-ANN-INCOME            TO CIF-ANN-INCOME
           .
      *================================================================*
       3400-UPDATE-KYC.
      *================================================================*
           EVALUATE TRUE
               WHEN CHCM-KYC-STATUS NOT = 'V' AND 'P' AND 'F'
                   MOVE 'INVK'             TO CH-REASON-CD
                   MOVE 'KYC STATUS MUST BE V, P OR F'
                                           TO CH-MESSAGE-TEXT
               WHEN CHCM-RISK-RATING NOT = 'L' AND 'M' AND 'H'
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'RISK RATING MUST BE L, M OR H'
                                           TO CH-MESSAGE-TEXT
      *        FOUR EYES - THE USER WHO CREATED OR LAST CHANGED THE
      *        CUSTOMER MAY NOT VERIFY THEIR KYC.
               WHEN CHCM-KYC-STATUS = 'V'
               AND  CIF-LAST-MAINT-USER = CH-USER-ID
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'AUTH'             TO CH-REASON-CD
                   MOVE 'KYC MUST BE VERIFIED BY A DIFFERENT USER'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE CHCM-KYC-STATUS    TO CIF-KYC-STATUS
                   MOVE CHCM-RISK-RATING   TO CIF-RISK-RATING
      *            A FAILED KYC BLOCKS THE CUSTOMER.
                   IF CIF-KYC-FAILED
                       SET CIF-BLOCKED     TO TRUE
                   END-IF
           END-EVALUATE
           .
      *================================================================*
       8000-WRITE-AUDIT.
      *================================================================*
           MOVE WS-TODAY                   TO AUD-DATE
           MOVE WS-TIME-HHMMSS             TO AUD-TIME
           MOVE EIBTASKN                   TO AUD-TASKNO
           MOVE CH-CHANNEL-CD              TO AUD-CHANNEL
           MOVE CH-USER-ID                 TO AUD-USER
           MOVE CHCM-FUNCTION              TO AUD-FUNCTION
           MOVE CH-REQUEST-ID              TO AUD-REQUEST-ID

           IF WS-BEFORE-IMAGE NOT = SPACES
               SET AUD-BEFORE              TO TRUE
               MOVE WS-BEFORE-IMAGE        TO AUD-CIF-IMAGE
               EXEC CICS WRITEQ TD
                         QUEUE('CFAU')
                         FROM(WS-AUDIT-RECORD)
                         LENGTH(LENGTH OF WS-AUDIT-RECORD)
                         RESP(WS-RESP)
               END-EXEC
           END-IF

           SET AUD-AFTER                   TO TRUE
           MOVE CIF-CUSTOMER-RECORD        TO AUD-CIF-IMAGE
           EXEC CICS WRITEQ TD
                     QUEUE('CFAU')
                     FROM(WS-AUDIT-RECORD)
                     LENGTH(LENGTH OF WS-AUDIT-RECORD)
                     RESP(WS-RESP)
           END-EXEC
      *    AN AUDIT FAILURE DOES NOT FAIL THE CUSTOMER CHANGE (THE TDQ
      *    IS EXTRAPARTITION AND NOT RECOVERABLE) - IT IS LOGGED.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE WS-RESP                TO WS-RESP-DISPLAY
               EXEC CICS WRITE OPERATOR
                         TEXT('MBKCF001E CFAU AUDIT WRITE FAILED')
                         RESP(WS-RESP)
               END-EXEC
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
