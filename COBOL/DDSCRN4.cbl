       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDSCRN4.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-06-07.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDSCRN4                                             *
      * TITLE    : BRANCH FUNDS HOLDS - PLACE / RELEASE / LIST         *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION DD04       *
      * MAPSET   : DDMAP4  MAP DDM04                                   *
      * LINKS    : DDHLD01 (SERVICE ACCTHOLD - FUNCTIONS P / R / L)    *
      *            CFSCRN2 (XCTL ON PF12 - CUSTOMER ACCOUNT LIST)      *
      * FILES    : ACCTMST (READ - PRODUCT, LEDGER, CIF NUMBER)        *
      *            CIFMAST (READ - CUSTOMER NAME FOR THE HEADER)       *
      *----------------------------------------------------------------*
      * SHOWS THE ACTIVE HOLDS ON AN ACCOUNT (UP TO 10 ON SCREEN - THE *
      * SERVICE RETURNS UP TO 15) AND LETS BRANCH STAFF PLACE A NEW    *
      * HOLD OR RELEASE AN EXISTING ONE:                               *
      *   ACTION P  PLACE   - AMOUNT, REASON AND (FOR OTHR) NARRATIVE  *
      *                       REQUIRED. EXPIRY OPTIONAL (CCYYMMDD).    *
      *   ACTION R  RELEASE - SEQ REQUIRED (FROM THE LIST).            *
      * REASONS ACCEPTED HERE: CHQ LEGL FRD OTHR. CARD HOLDS ARE ONLY  *
      * PLACED BY THE CARD AUTHORISATION GATEWAY AND ARE REFUSED HERE  *
      * (THEY CAN STILL BE RELEASED - E.G. A STUCK PRE-AUTH).          *
      *                                                                *
      * ALL HOLD PROCESSING IS DONE BY DDHLD01, LINKED WITH CHANNEL    *
      * 'BRANCH' SO THAT LEGL AND FRD HOLDS ARE AUTHORISED (DDHLD01    *
      * REFUSES THEM FROM OTHER CHANNELS). THIS PROGRAM ONLY CHECKS    *
      * THE FORMAT OF WHAT WAS KEYED AND SHOWS THE SERVICE'S MESSAGE.  *
      *                                                                *
      * THE ACTION FIELDS ARE KEPT IN THE COMMAREA SO A FIELD NOT      *
      * RE-KEYED AFTER AN ERROR KEEPS ITS VALUE (CTRL=FRSET RESETS THE *
      * MDT ON EVERY SEND). THEY ARE CLEARED AFTER A SUCCESSFUL ACTION *
      * SO THAT ENTER CANNOT PLACE THE SAME HOLD TWICE.                *
      *                                                                *
      * ENTERED FROM CF02 (OPTION F) WITH THE ACCOUNT IN THE COMMAREA, *
      * OR DIRECTLY BY TRANSACTION DD04.                               *
      * ACCESS: RACF PROFILE CICSPRD1.DD04 (GROUPS BRSUPV, FRAUDOPS).  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-06-07 JNB  ORIGINAL VERSION (AUDIT FINDING IA-09-031 -    *
      *                 HOLDS WERE PLACED BY TELLER JOURNAL ENTRY)     *
      * 2010-09-20 SAT  CUSTOMER NAME IN THE HEADER (CIFMAST)          *
      * 2011-02-14 AOS  CHQ HOLDS DEFAULT EXPIRY (2-4-6 CLEARING)      *
      * 2011-08-22 JNB  'XP' STATUS FOR EXPIRED HOLDS AWAITING THE     *
      *                 OVERNIGHT RELEASE (DDHLX010) - PIR 2011/077    *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDSCRN4 '.
      *
       01  WS-CONSTANTS.
           05  WS-ROWS-ON-SCREEN           PIC 9(02) VALUE 10.
           05  WS-MAX-HOLDS                PIC 9(02) VALUE 15.
      *    2-4-6: A CHEQUE IS CERTAIN BY THE 6TH WORKING DAY. 8
      *    CALENDAR DAYS COVERS A WEEKEND (NOT A BANK HOLIDAY WEEK -
      *    STAFF KEY THE EXPIRY THEMSELVES THEN).
           05  WS-CHQ-DEFAULT-DAYS         PIC S9(04) COMP VALUE +8.
           05  WS-MAX-EXPIRY-DAYS          PIC S9(04) COMP VALUE +366.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TODAY-INT                PIC S9(09) COMP.
           05  WS-EXPIRY-INT               PIC S9(09) COMP.
           05  WS-EXPIRY-DATE              PIC 9(08).
           05  WS-HOLD-SEQ                 PIC 9(04).
           05  WS-LINE                     PIC S9(04) COMP.
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'DD04 - FUNDS HOLDS ENDED'.
           05  WS-BAL-EDIT                 PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05  WS-HOLD-EDIT                PIC ZZ,ZZZ,ZZ9.99.
           05  WS-COUNT-EDIT               PIC Z9.
           05  WS-SEQ-EDIT                 PIC 9(04).
           05  WS-VALID-IND                PIC X(01).
               88  WS-INPUT-VALID              VALUE 'Y'.
           05  WS-FOUND-IND                PIC X(01).
               88  WS-SEQ-FOUND                VALUE 'Y'.
           05  WS-NEW-KEY-IND              PIC X(01).
               88  WS-NEW-KEY                  VALUE 'Y'.
           05  WS-ACTION-MSG               PIC X(79).
           05  WS-ACTION-RC                PIC X(04).
      *
      *---- AMOUNT PARSING (SAME RULES AS SOSCRN1 6000-PARSE-AMOUNT) -*
       01  WS-AMOUNT-FIELDS.
           05  WS-AMOUNT-IN                PIC X(13).
           05  WS-AMOUNT-NUM               PIC S9(13)V99.
           05  WS-AMOUNT-OK-IND            PIC X(01).
               88  WS-AMOUNT-OK                VALUE 'Y'.
           05  WS-DOT-POS                  PIC S9(04) COMP.
           05  WS-CHAR-SUB                 PIC S9(04) COMP.
           05  WS-INT-PART                 PIC 9(11).
           05  WS-DEC-PART                 PIC 9(02).
           05  WS-DIGIT-COUNT              PIC S9(04) COMP.
           05  WS-DEC-COUNT                PIC S9(04) COMP.
           05  WS-ONE-CHAR                 PIC X(01).
      *
      *---- DATE FORMATTING ------------------------------------------*
       01  WS-FMT-DATE-IN                  PIC 9(08).
       01  WS-FMT-DATE-IN-X REDEFINES WS-FMT-DATE-IN
                                           PIC X(08).
       01  WS-FMT-DATE-OUT                 PIC X(08).
      *
      *---- PSEUDO-CONVERSATIONAL COMMAREA ---------------------------*
       01  WS-COMMAREA.
           05  CA-EYECATCHER               PIC X(04).
               88  CA-OURS                     VALUE 'DD04'.
           05  CA-FROM-TRAN                PIC X(04).
           05  CA-SORT-CODE                PIC X(06).
           05  CA-ACCT-NO                  PIC X(08).
           05  CA-CIF-NO                   PIC X(12).
           05  CA-LISTED-IND               PIC X(01).
               88  CA-LISTED                   VALUE 'Y'.
           05  CA-HOLD-COUNT               PIC 9(02).
           05  CA-HOLD-SEQ                 PIC 9(04) OCCURS 15 TIMES.
           05  CA-INPUT.
               10  CA-ACTION               PIC X(01).
                   88  CA-ACTION-PLACE         VALUE 'P'.
                   88  CA-ACTION-RELEASE       VALUE 'R'.
                   88  CA-ACTION-NONE          VALUE SPACE.
               10  CA-HSEQ                 PIC X(04).
               10  CA-HAMT                 PIC X(13).
               10  CA-HRSN                 PIC X(04).
                   88  CA-RSN-ALLOWED          VALUE 'CHQ ' 'LEGL'
                                                     'FRD ' 'OTHR'.
                   88  CA-RSN-CARD             VALUE 'CARD'.
                   88  CA-RSN-CHEQUE           VALUE 'CHQ '.
                   88  CA-RSN-LEGAL            VALUE 'LEGL'.
                   88  CA-RSN-OTHER            VALUE 'OTHR'.
               10  CA-HEXP                 PIC X(08).
               10  CA-HNARR                PIC X(33).
      *
      *---- COMMAREA RECEIVED FROM CF02 (OPTION F) --------------------*
       01  WS-CF02-COMMAREA.
           05  XC-FROM-TRAN                PIC X(04).
           05  XC-SORT-CODE                PIC X(06).
           05  XC-ACCT-NO                  PIC X(08).
           05  XC-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(20).
      *
       01  WS-HOLD-LINE.
           05  HL-SEQ                      PIC 9(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-STATUS                   PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-REASON                   PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  HL-AMOUNT                   PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-PLACED                   PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-EXPIRES                  PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-USER                     PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-NARRATIVE                PIC X(24).
      *
           COPY DDMAP4.
      *
           COPY DDACCREC.
      *
           COPY CFCUSREC.
      *
           COPY DFHAID.
      *
           COPY DFHBMSCA.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(160).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC
           COMPUTE WS-TODAY-INT = FUNCTION INTEGER-OF-DATE (WS-TODAY)

           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                   PERFORM 1000-SEND-EMPTY-MAP
               WHEN EIBCALEN = LENGTH OF WS-CF02-COMMAREA
                   MOVE DFHCOMMAREA (1:50) TO WS-CF02-COMMAREA
                   PERFORM 1100-START-FROM-CF02
               WHEN OTHER
                   MOVE DFHCOMMAREA        TO WS-COMMAREA
                   IF NOT CA-OURS
                       PERFORM 1000-SEND-EMPTY-MAP
                   ELSE
                       EVALUATE EIBAID
                           WHEN DFHPF3
                               PERFORM 9000-END-SESSION
                           WHEN DFHPF12
                               PERFORM 9100-RETURN-TO-CF02
                           WHEN DFHCLEAR
                               PERFORM 1000-SEND-EMPTY-MAP
                           WHEN DFHPF5
                               PERFORM 5000-REFRESH
                           WHEN DFHENTER
                               PERFORM 2000-PROCESS-ENTER THRU 2000-EXIT
                           WHEN OTHER
                               MOVE LOW-VALUES TO DDM04O
                               MOVE 'INVALID KEY - USE ENTER, PF3, PF5'
                                           TO MSGO
                               PERFORM 8000-SEND-DATAONLY
                       END-EVALUATE
                   END-IF
           END-EVALUATE

           EXEC CICS RETURN
                     TRANSID('DD04')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       1000-SEND-EMPTY-MAP.
      *================================================================*
           PERFORM 1200-INIT-COMMAREA
           MOVE LOW-VALUES                 TO DDM04O
           MOVE 'KEY SORT CODE AND ACCOUNT NUMBER, PRESS ENTER'
                                           TO MSGO
           PERFORM 8100-SEND-MAP
           .
      *================================================================*
       1100-START-FROM-CF02.
      *================================================================*
           PERFORM 1200-INIT-COMMAREA
           MOVE XC-FROM-TRAN               TO CA-FROM-TRAN
           MOVE XC-SORT-CODE               TO CA-SORT-CODE
           MOVE XC-ACCT-NO                 TO CA-ACCT-NO
           MOVE XC-CIF-NO                  TO CA-CIF-NO
           MOVE SPACES                     TO WS-ACTION-MSG
           PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
           .
      *================================================================*
       1200-INIT-COMMAREA.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           MOVE 'N'                        TO CA-LISTED-IND
           MOVE ZERO                       TO CA-HOLD-COUNT
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-MAX-HOLDS
               MOVE ZERO                   TO CA-HOLD-SEQ (WS-SUB)
           END-PERFORM
           .
      *================================================================*
       2000-PROCESS-ENTER.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('DDM04')
                     MAPSET('DDMAP4')
                     INTO(DDM04I)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               IF CA-LISTED
                   MOVE SPACES             TO WS-ACTION-MSG
                   PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
               ELSE
                   MOVE LOW-VALUES         TO DDM04O
                   MOVE 'SORT CODE AND ACCOUNT NUMBER ARE REQUIRED'
                                           TO MSGO
                   MOVE -1                 TO SORTCDL
                   PERFORM 8000-SEND-DATAONLY
               END-IF
               GO TO 2000-EXIT
           END-IF

           PERFORM 2100-RECEIVE-KEYS
           IF NOT WS-INPUT-VALID
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

      *    A NEW ACCOUNT (OR THE FIRST ENTER) JUST LISTS - ANY ACTION
      *    KEYED AT THE SAME TIME IS IGNORED SO A HOLD CANNOT BE
      *    PLACED ON AN ACCOUNT WHOSE HOLDS HAVE NOT BEEN SEEN.
           IF WS-NEW-KEY OR NOT CA-LISTED
               MOVE SPACES                 TO CA-INPUT
               MOVE SPACES                 TO WS-ACTION-MSG
               PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
               GO TO 2000-EXIT
           END-IF

           PERFORM 2200-RECEIVE-ACTION
           IF CA-ACTION-NONE
               MOVE SPACES                 TO WS-ACTION-MSG
               PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
               GO TO 2000-EXIT
           END-IF

           PERFORM 2300-VALIDATE-ACTION
           IF NOT WS-INPUT-VALID
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

           EVALUATE TRUE
               WHEN CA-ACTION-PLACE
                   PERFORM 3000-PLACE-HOLD
               WHEN CA-ACTION-RELEASE
                   PERFORM 3100-RELEASE-HOLD
           END-EVALUATE
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-RECEIVE-KEYS.
      *================================================================*
      *    A FIELD NOT RE-KEYED KEEPS ITS COMMAREA VALUE (FSET).
           MOVE 'Y'                        TO WS-VALID-IND
           MOVE 'N'                        TO WS-NEW-KEY-IND
           IF SORTCDL > ZERO
               IF SORTCDI NOT = CA-SORT-CODE
                   MOVE 'Y'                TO WS-NEW-KEY-IND
               END-IF
               MOVE SORTCDI                TO CA-SORT-CODE
           END-IF
           IF ACCTNOL > ZERO
               IF ACCTNOI NOT = CA-ACCT-NO
                   MOVE 'Y'                TO WS-NEW-KEY-IND
               END-IF
               MOVE ACCTNOI                TO CA-ACCT-NO
           END-IF
           IF WS-NEW-KEY
      *        THE CIF CAME WITH THE OLD ACCOUNT - 4100 RE-READS IT.
               MOVE SPACES                 TO CA-CIF-NO
               MOVE 'N'                    TO CA-LISTED-IND
           END-IF

           EVALUATE TRUE
               WHEN CA-SORT-CODE NOT NUMERIC
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE LOW-VALUES         TO DDM04O
                   MOVE 'SORT CODE MUST BE 6 DIGITS' TO MSGO
                   MOVE -1                 TO SORTCDL
                   MOVE DFHRED             TO SORTCDA
               WHEN CA-ACCT-NO NOT NUMERIC
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE LOW-VALUES         TO DDM04O
                   MOVE 'ACCOUNT NUMBER MUST BE 8 DIGITS' TO MSGO
                   MOVE -1                 TO ACCTNOL
                   MOVE DFHRED             TO ACCTNOA
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       2200-RECEIVE-ACTION.
      *================================================================*
           IF ACTIONL > ZERO
               MOVE FUNCTION UPPER-CASE (ACTIONI) TO CA-ACTION
           END-IF
           IF HSEQL > ZERO
               MOVE HSEQI                  TO CA-HSEQ
           END-IF
           IF HAMTL > ZERO
               MOVE HAMTI                  TO CA-HAMT
           END-IF
           IF HRSNL > ZERO
               MOVE FUNCTION UPPER-CASE (HRSNI) TO CA-HRSN
           END-IF
           IF HEXPL > ZERO
               MOVE HEXPI                  TO CA-HEXP
           END-IF
           IF HNARRL > ZERO
               MOVE HNARRI                 TO CA-HNARR
           END-IF
           INSPECT CA-INPUT REPLACING ALL LOW-VALUE BY SPACE
           IF CA-ACTION = LOW-VALUE
               MOVE SPACE                  TO CA-ACTION
           END-IF
           .
      *================================================================*
       2300-VALIDATE-ACTION.
      *================================================================*
           MOVE 'Y'                        TO WS-VALID-IND
           MOVE LOW-VALUES                 TO DDM04O
           EVALUATE TRUE
               WHEN CA-ACTION-PLACE
                   PERFORM 2400-VALIDATE-PLACE THRU 2400-EXIT
               WHEN CA-ACTION-RELEASE
                   PERFORM 2500-VALIDATE-RELEASE THRU 2500-EXIT
               WHEN OTHER
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'ACTION MUST BE P (PLACE) OR R (RELEASE)'
                                           TO MSGO
                   MOVE -1                 TO ACTIONL
                   MOVE DFHRED             TO ACTIONA
           END-EVALUATE
           .
      *================================================================*
       2400-VALIDATE-PLACE.
      *================================================================*
           IF CA-HSEQ NOT = SPACES
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'SEQ IS ALLOCATED BY THE SYSTEM - LEAVE BLANK'
                                           TO MSGO
               MOVE -1                     TO HSEQL
               GO TO 2400-EXIT
           END-IF

           MOVE CA-HAMT                    TO WS-AMOUNT-IN
           PERFORM 6000-PARSE-AMOUNT THRU 6000-EXIT
           IF NOT WS-AMOUNT-OK OR WS-AMOUNT-NUM NOT > ZERO
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'AMOUNT REQUIRED - E.G. 1250.00'
                                           TO MSGO
               MOVE -1                     TO HAMTL
               MOVE DFHRED                 TO HAMTA
               GO TO 2400-EXIT
           END-IF

           EVALUATE TRUE
               WHEN CA-HRSN = SPACES
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'REASON REQUIRED - CHQ, LEGL, FRD OR OTHR'
                                           TO MSGO
               WHEN CA-RSN-CARD
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'CARD HOLDS ARE PLACED BY CARD AUTHORISATION'
                                           TO MSGO
               WHEN NOT CA-RSN-ALLOWED
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'REASON MUST BE CHQ, LEGL, FRD OR OTHR'
                                           TO MSGO
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT WS-INPUT-VALID
               MOVE -1                     TO HRSNL
               MOVE DFHRED                 TO HRSNA
               GO TO 2400-EXIT
           END-IF

           IF CA-RSN-OTHER AND CA-HNARR = SPACES
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'NARRATIVE REQUIRED FOR REASON OTHR'
                                           TO MSGO
               MOVE -1                     TO HNARRL
               GO TO 2400-EXIT
           END-IF

      *    EXPIRY: BLANK = NO EXPIRY, EXCEPT CHQ WHICH DEFAULTS.
           MOVE ZERO                       TO WS-EXPIRY-DATE
           IF CA-HEXP = SPACES
               IF CA-RSN-CHEQUE
                   COMPUTE WS-EXPIRY-DATE = FUNCTION DATE-OF-INTEGER
                           (WS-TODAY-INT + WS-CHQ-DEFAULT-DAYS)
               END-IF
               GO TO 2400-EXIT
           END-IF
           IF CA-HEXP NOT NUMERIC
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'EXPIRY MUST BE CCYYMMDD OR BLANK' TO MSGO
               MOVE -1                     TO HEXPL
               MOVE DFHRED                 TO HEXPA
               GO TO 2400-EXIT
           END-IF
           MOVE CA-HEXP                    TO WS-EXPIRY-DATE
           IF FUNCTION TEST-DATE-YYYYMMDD (WS-EXPIRY-DATE) NOT = ZERO
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'EXPIRY IS NOT A VALID DATE' TO MSGO
               MOVE -1                     TO HEXPL
               MOVE DFHRED                 TO HEXPA
               GO TO 2400-EXIT
           END-IF
           COMPUTE WS-EXPIRY-INT =
                   FUNCTION INTEGER-OF-DATE (WS-EXPIRY-DATE)
           EVALUATE TRUE
               WHEN WS-EXPIRY-INT <= WS-TODAY-INT
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'EXPIRY MUST BE AFTER TODAY' TO MSGO
      *        COURT ORDERS RUN UNTIL LIFTED - NO UPPER LIMIT FOR LEGL.
               WHEN WS-EXPIRY-INT > WS-TODAY-INT + WS-MAX-EXPIRY-DAYS
               AND  NOT CA-RSN-LEGAL
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'EXPIRY MORE THAN 1 YEAR AHEAD - LEAVE BLANK'
                                           TO MSGO
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT WS-INPUT-VALID
               MOVE -1                     TO HEXPL
               MOVE DFHRED                 TO HEXPA
           END-IF
           .
       2400-EXIT.
           EXIT.
      *================================================================*
       2500-VALIDATE-RELEASE.
      *================================================================*
           IF CA-HSEQ = SPACES
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'KEY THE SEQ OF THE HOLD TO RELEASE' TO MSGO
               MOVE -1                     TO HSEQL
               GO TO 2500-EXIT
           END-IF
      *    ALLOW '7' AS WELL AS '0007' - SPACES ARE IGNORED.
           MOVE ZERO                       TO WS-HOLD-SEQ
           PERFORM VARYING WS-CHAR-SUB FROM 1 BY 1
                   UNTIL WS-CHAR-SUB > LENGTH OF CA-HSEQ
                      OR NOT WS-INPUT-VALID
               MOVE CA-HSEQ (WS-CHAR-SUB:1) TO WS-ONE-CHAR
               EVALUATE TRUE
                   WHEN WS-ONE-CHAR = SPACE
                       CONTINUE
                   WHEN WS-ONE-CHAR IS NUMERIC
                       COMPUTE WS-HOLD-SEQ = WS-HOLD-SEQ * 10
                               + FUNCTION NUMVAL (WS-ONE-CHAR)
                   WHEN OTHER
                       MOVE 'N'            TO WS-VALID-IND
               END-EVALUATE
           END-PERFORM
           IF NOT WS-INPUT-VALID OR WS-HOLD-SEQ = ZERO
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'SEQ MUST BE NUMERIC' TO MSGO
               MOVE -1                     TO HSEQL
               MOVE DFHRED                 TO HSEQA
               GO TO 2500-EXIT
           END-IF

      *    THE SEQ MUST BE ONE OF THE ACTIVE HOLDS LAST LISTED (ALL
      *    15 ARE KEPT EVEN THOUGH ONLY 10 ARE SHOWN).
           MOVE 'N'                        TO WS-FOUND-IND
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > CA-HOLD-COUNT
                      OR WS-SEQ-FOUND
               IF CA-HOLD-SEQ (WS-SUB) = WS-HOLD-SEQ
                   MOVE 'Y'                TO WS-FOUND-IND
               END-IF
           END-PERFORM
           IF NOT WS-SEQ-FOUND
               MOVE 'N'                    TO WS-VALID-IND
               MOVE WS-HOLD-SEQ            TO WS-SEQ-EDIT
               STRING 'HOLD ' WS-SEQ-EDIT
                      ' IS NOT AN ACTIVE HOLD - PF5 TO REFRESH'
                      DELIMITED BY SIZE
                      INTO MSGO
               END-STRING
               MOVE -1                     TO HSEQL
               MOVE DFHRED                 TO HSEQA
               GO TO 2500-EXIT
           END-IF

           IF CA-HAMT NOT = SPACES OR CA-HRSN NOT = SPACES
           OR CA-HEXP NOT = SPACES
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'RELEASE: ONLY SEQ AND NARRATIVE ARE USED'
                                           TO MSGO
               MOVE -1                     TO HAMTL
           END-IF
           .
       2500-EXIT.
           EXIT.
      *================================================================*
       3000-PLACE-HOLD.
      *================================================================*
           PERFORM 7000-INIT-SERVICE
           SET CHAH-PLACE                  TO TRUE
           MOVE WS-AMOUNT-NUM              TO CHAH-AMOUNT
           MOVE CA-HRSN                    TO CHAH-REASON-CD
           MOVE WS-EXPIRY-DATE             TO CHAH-EXPIRY-DATE
           MOVE CA-HNARR                   TO CHAH-NARRATIVE
           PERFORM 7100-LINK-SERVICE

           MOVE CH-RETURN-CD               TO WS-ACTION-RC
           MOVE SPACES                     TO WS-ACTION-MSG
           IF CH-RC-OK OR CH-RC-WARNING
               MOVE CHAH-NEW-HOLD-SEQ      TO WS-SEQ-EDIT
               MOVE WS-AMOUNT-NUM          TO WS-HOLD-EDIT
               IF CH-RC-WARNING
                   MOVE CH-MESSAGE-TEXT    TO WS-ACTION-MSG
               ELSE
                   STRING 'HOLD ' WS-SEQ-EDIT ' PLACED FOR '
                          FUNCTION TRIM (WS-HOLD-EDIT)
                          DELIMITED BY SIZE
                          INTO WS-ACTION-MSG
                   END-STRING
               END-IF
               MOVE SPACES                 TO CA-INPUT
               PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
           ELSE
      *        REJECTED - LEAVE THE INPUT ON THE SCREEN.
               MOVE LOW-VALUES             TO DDM04O
               MOVE CH-MESSAGE-TEXT        TO MSGO
               EVALUATE CH-REASON-CD
                   WHEN 'INVM'
                       MOVE -1             TO HAMTL
                   WHEN 'INVR'
                   WHEN 'HAUT'
                       MOVE -1             TO HRSNL
                   WHEN 'INVN'
                       MOVE -1             TO HNARRL
                   WHEN 'INVD'
                       MOVE -1             TO HEXPL
                   WHEN OTHER
                       MOVE -1             TO ACTIONL
               END-EVALUATE
               PERFORM 8000-SEND-DATAONLY
           END-IF
           .
      *================================================================*
       3100-RELEASE-HOLD.
      *================================================================*
           PERFORM 7000-INIT-SERVICE
           SET CHAH-RELEASE                TO TRUE
           MOVE WS-HOLD-SEQ                TO CHAH-HOLD-SEQ
           MOVE CA-HNARR                   TO CHAH-NARRATIVE
           PERFORM 7100-LINK-SERVICE

           MOVE CH-RETURN-CD               TO WS-ACTION-RC
           MOVE SPACES                     TO WS-ACTION-MSG
           IF CH-RC-OK OR CH-RC-WARNING
               MOVE WS-HOLD-SEQ            TO WS-SEQ-EDIT
               STRING 'HOLD ' WS-SEQ-EDIT ' RELEASED'
                      DELIMITED BY SIZE
                      INTO WS-ACTION-MSG
               END-STRING
               MOVE SPACES                 TO CA-INPUT
               PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
           ELSE
               MOVE CH-MESSAGE-TEXT        TO WS-ACTION-MSG
      *        HOLD ALREADY GONE (EXPIRED OVERNIGHT, OR RELEASED BY
      *        ANOTHER BRANCH) - RELIST SO THE SCREEN IS CURRENT.
               IF CH-REASON-CD = 'HNFD' OR 'HREL'
                   MOVE SPACES             TO CA-INPUT
                   PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
               ELSE
                   MOVE LOW-VALUES         TO DDM04O
                   MOVE WS-ACTION-MSG      TO MSGO
                   MOVE -1                 TO HSEQL
                   PERFORM 8000-SEND-DATAONLY
               END-IF
           END-IF
           .
      *================================================================*
       4000-LIST-HOLDS.
      *================================================================*
      *    ON ENTRY WS-ACTION-MSG HOLDS THE RESULT OF A PLACE/RELEASE
      *    (SPACES IF NONE) - IT TAKES PRIORITY IN THE MESSAGE LINE.
           PERFORM 7000-INIT-SERVICE
           SET CHAH-LIST                   TO TRUE
           PERFORM 7100-LINK-SERVICE

           MOVE LOW-VALUES                 TO DDM04O
           PERFORM 4100-FORMAT-HEADER THRU 4100-EXIT

           IF NOT (CH-RC-OK OR CH-RC-WARNING)
               MOVE 'N'                    TO CA-LISTED-IND
               MOVE ZERO                   TO CA-HOLD-COUNT
               MOVE CH-MESSAGE-TEXT        TO MSGO
               IF CH-REASON-CD = 'ANFD' OR 'INVA'
                   MOVE -1                 TO SORTCDL
               END-IF
               PERFORM 8100-SEND-MAP
               GO TO 4000-EXIT
           END-IF

           SET CA-LISTED                   TO TRUE
           MOVE CHAH-COUNT                 TO CA-HOLD-COUNT
           IF CA-HOLD-COUNT > WS-MAX-HOLDS
               MOVE WS-MAX-HOLDS           TO CA-HOLD-COUNT
           END-IF
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-MAX-HOLDS
               IF WS-SUB <= CA-HOLD-COUNT
                   MOVE CHAHE-HOLD-SEQ (WS-SUB)
                                           TO CA-HOLD-SEQ (WS-SUB)
               ELSE
                   MOVE ZERO               TO CA-HOLD-SEQ (WS-SUB)
               END-IF
           END-PERFORM

      *    THE SERVICE RETURNS THE BALANCES AFTER ANY HOLD CHANGE.
           MOVE CHAH-NEW-AVAIL-BAL         TO WS-BAL-EDIT
           MOVE WS-BAL-EDIT                TO AVAILO
           MOVE CHAH-TOTAL-HOLD            TO WS-HOLD-EDIT
           MOVE WS-HOLD-EDIT               TO TOTHLDO

           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-ROWS-ON-SCREEN
               MOVE SPACES                 TO WS-HOLD-LINE
               IF WS-LINE <= CA-HOLD-COUNT
                   PERFORM 4200-FORMAT-HOLD
               END-IF
               PERFORM 4300-PLACE-LINE
           END-PERFORM

           EVALUATE TRUE
               WHEN WS-ACTION-MSG NOT = SPACES
                   MOVE WS-ACTION-MSG      TO MSGO
               WHEN CA-HOLD-COUNT > WS-ROWS-ON-SCREEN
                   MOVE CA-HOLD-COUNT      TO WS-COUNT-EDIT
                   STRING 'FIRST 10 OF ' WS-COUNT-EDIT
                          ' ACTIVE HOLDS SHOWN - OTHERS CAN STILL BE'
                          ' RELEASED BY SEQ'
                          DELIMITED BY SIZE
                          INTO MSGO
                   END-STRING
               WHEN CH-RC-WARNING
                   MOVE CH-MESSAGE-TEXT    TO MSGO
               WHEN OTHER
                   MOVE 'ACTION P OR R, OR PF5 TO REFRESH'
                                           TO MSGO
           END-EVALUATE
           MOVE -1                         TO ACTIONL
           PERFORM 8100-SEND-MAP
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-FORMAT-HEADER.
      *================================================================*
           MOVE CA-SORT-CODE               TO SORTCDO
           MOVE CA-ACCT-NO                 TO ACCTNOO

           MOVE CA-SORT-CODE               TO ACM-SORT-CODE
           MOVE CA-ACCT-NO                 TO ACM-ACCT-NO
           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(ACM-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'N/A'                  TO PRODCDO
               MOVE 'BATCH WINDOW'         TO LEDGERO
               GO TO 4100-EXIT
           END-IF

           MOVE ACM-PROD-CD                TO PRODCDO
           MOVE ACM-LEDGER-BAL             TO WS-BAL-EDIT
           MOVE WS-BAL-EDIT                TO LEDGERO
      *    AVAILABLE AND HELD ARE OVERWRITTEN FROM THE SERVICE REPLY
      *    WHEN THE LIST WORKS.
           MOVE ACM-AVAIL-BAL              TO WS-BAL-EDIT
           MOVE WS-BAL-EDIT                TO AVAILO
           MOVE ACM-HOLD-AMT               TO WS-HOLD-EDIT
           MOVE WS-HOLD-EDIT               TO TOTHLDO
           IF ACM-AVAIL-BAL < ZERO
               MOVE DFHRED                 TO AVAILA
           END-IF
           IF CA-CIF-NO = SPACES
               MOVE ACM-CIF-NO             TO CA-CIF-NO
           END-IF

           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(ACM-CIF-NO)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE SPACES                 TO CUSTNMO
               STRING CIF-TITLE     DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-FIRST-NM  DELIMITED BY SPACE
                      ' '           DELIMITED BY SIZE
                      CIF-LAST-NM   DELIMITED BY '  '
                      INTO CUSTNMO
               END-STRING
           ELSE
               MOVE 'CUSTOMER NOT AVAILABLE' TO CUSTNMO
           END-IF
           .
       4100-EXIT.
           EXIT.
      *================================================================*
       4200-FORMAT-HOLD.
      *================================================================*
           MOVE CHAHE-HOLD-SEQ (WS-LINE)   TO HL-SEQ
           MOVE CHAHE-REASON-CD (WS-LINE)  TO HL-REASON
           MOVE CHAHE-AMOUNT (WS-LINE)     TO HL-AMOUNT
           MOVE CHAHE-USER-ID (WS-LINE)    TO HL-USER
           MOVE CHAHE-NARRATIVE (WS-LINE)  TO HL-NARRATIVE

           MOVE CHAHE-PLACED-DATE (WS-LINE) TO WS-FMT-DATE-IN
           PERFORM 6100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT            TO HL-PLACED

           MOVE CHAHE-EXPIRY-DATE (WS-LINE) TO WS-FMT-DATE-IN
           PERFORM 6100-FORMAT-DATE
           IF WS-FMT-DATE-OUT = SPACES
               MOVE 'NONE'                 TO HL-EXPIRES
           ELSE
               MOVE WS-FMT-DATE-OUT        TO HL-EXPIRES
           END-IF

      *    AN EXPIRED HOLD STAYS ACTIVE UNTIL DDHLX010 RUNS THE NIGHT
      *    AFTER IT EXPIRES - SHOW 'XP' SO STAFF DO NOT RELEASE IT.
           EVALUATE TRUE
               WHEN CHAHE-STATUS (WS-LINE) NOT = 'A'
                   MOVE CHAHE-STATUS (WS-LINE) TO HL-STATUS
               WHEN CHAHE-EXPIRY-DATE (WS-LINE) NOT NUMERIC
                   MOVE 'AC'               TO HL-STATUS
               WHEN CHAHE-EXPIRY-DATE (WS-LINE) > ZERO
               AND  CHAHE-EXPIRY-DATE (WS-LINE) < WS-TODAY
                   MOVE 'XP'               TO HL-STATUS
               WHEN OTHER
                   MOVE 'AC'               TO HL-STATUS
           END-EVALUATE
           .
      *================================================================*
       4300-PLACE-LINE.
      *================================================================*
           EVALUATE WS-LINE
               WHEN 1   MOVE WS-HOLD-LINE TO HLD1O
               WHEN 2   MOVE WS-HOLD-LINE TO HLD2O
               WHEN 3   MOVE WS-HOLD-LINE TO HLD3O
               WHEN 4   MOVE WS-HOLD-LINE TO HLD4O
               WHEN 5   MOVE WS-HOLD-LINE TO HLD5O
               WHEN 6   MOVE WS-HOLD-LINE TO HLD6O
               WHEN 7   MOVE WS-HOLD-LINE TO HLD7O
               WHEN 8   MOVE WS-HOLD-LINE TO HLD8O
               WHEN 9   MOVE WS-HOLD-LINE TO HLD9O
               WHEN 10  MOVE WS-HOLD-LINE TO HLD10O
           END-EVALUATE
           .
      *================================================================*
       5000-REFRESH.
      *================================================================*
      *    PF5 - RE-LIST THE ACCOUNT IN THE COMMAREA. ANYTHING KEYED IN
      *    THE ACTION FIELDS IS DISCARDED.
           IF CA-SORT-CODE NUMERIC AND CA-ACCT-NO NUMERIC
               MOVE SPACES                 TO CA-INPUT
               MOVE SPACES                 TO WS-ACTION-MSG
               PERFORM 4000-LIST-HOLDS THRU 4000-EXIT
           ELSE
               MOVE LOW-VALUES             TO DDM04O
               MOVE 'KEY SORT CODE AND ACCOUNT, THEN ENTER'
                                           TO MSGO
               MOVE -1                     TO SORTCDL
               PERFORM 8000-SEND-DATAONLY
           END-IF
           .
      *================================================================*
       6000-PARSE-AMOUNT.
      *================================================================*
      *    ACCEPTS '1234', '1234.5', '1,234.56'. BLANK = ZERO.
           MOVE 'Y'                        TO WS-AMOUNT-OK-IND
           MOVE ZERO                       TO WS-AMOUNT-NUM
                                              WS-INT-PART WS-DEC-PART
                                              WS-DIGIT-COUNT
                                              WS-DEC-COUNT
           MOVE ZERO                       TO WS-DOT-POS
           IF WS-AMOUNT-IN = SPACES OR LOW-VALUES
               GO TO 6000-EXIT
           END-IF
           PERFORM VARYING WS-CHAR-SUB FROM 1 BY 1
                   UNTIL WS-CHAR-SUB > LENGTH OF WS-AMOUNT-IN
               MOVE WS-AMOUNT-IN (WS-CHAR-SUB:1) TO WS-ONE-CHAR
               EVALUATE TRUE
                   WHEN WS-ONE-CHAR = SPACE OR LOW-VALUE OR ','
                       CONTINUE
                   WHEN WS-ONE-CHAR = '.'
                       IF WS-DOT-POS > ZERO
                           MOVE 'N'        TO WS-AMOUNT-OK-IND
                       END-IF
                       MOVE WS-CHAR-SUB    TO WS-DOT-POS
                   WHEN WS-ONE-CHAR IS NUMERIC
                       IF WS-DOT-POS = ZERO
                           COMPUTE WS-INT-PART = WS-INT-PART * 10
                                   + FUNCTION NUMVAL (WS-ONE-CHAR)
                           ADD 1           TO WS-DIGIT-COUNT
                       ELSE
                           ADD 1           TO WS-DEC-COUNT
                           IF WS-DEC-COUNT = 1
                               COMPUTE WS-DEC-PART =
                                   FUNCTION NUMVAL (WS-ONE-CHAR) * 10
                           ELSE
                               IF WS-DEC-COUNT = 2
                                   COMPUTE WS-DEC-PART = WS-DEC-PART
                                     + FUNCTION NUMVAL (WS-ONE-CHAR)
                               ELSE
                                   MOVE 'N' TO WS-AMOUNT-OK-IND
                               END-IF
                           END-IF
                       END-IF
                   WHEN OTHER
                       MOVE 'N'            TO WS-AMOUNT-OK-IND
               END-EVALUATE
           END-PERFORM
      *    DDHLD01 LIMIT IS 9,999,999.99 - 7 DIGITS BEFORE THE POINT.
           IF WS-DIGIT-COUNT > 7
               MOVE 'N'                    TO WS-AMOUNT-OK-IND
           END-IF
           IF WS-AMOUNT-OK
               COMPUTE WS-AMOUNT-NUM = WS-INT-PART + WS-DEC-PART / 100
           END-IF
           .
       6000-EXIT.
           EXIT.
      *================================================================*
       6100-FORMAT-DATE.
      *================================================================*
      *    CCYYMMDD -> DD/MM/YY. ZERO OR NON-NUMERIC -> SPACES.
           MOVE SPACES                     TO WS-FMT-DATE-OUT
           IF WS-FMT-DATE-IN-X NUMERIC AND WS-FMT-DATE-IN > ZERO
               STRING WS-FMT-DATE-IN-X (7:2) '/'
                      WS-FMT-DATE-IN-X (5:2) '/'
                      WS-FMT-DATE-IN-X (3:2)
                      DELIMITED BY SIZE
                      INTO WS-FMT-DATE-OUT
               END-STRING
           END-IF
           .
      *================================================================*
       7000-INIT-SERVICE.
      *================================================================*
           MOVE SPACES                     TO CH-COMMAREA
           SET CH-EYECATCHER-OK            TO TRUE
           MOVE '01'                       TO CH-VERSION
           MOVE 'ACCTHOLD'                 TO CH-SERVICE-CD
           MOVE 'BRANCH'                   TO CH-CHANNEL-CD
           MOVE EIBTRMID                   TO CH-USER-ID
           STRING 'DD04' EIBTRMID EIBTASKN
                  DELIMITED BY SIZE
                  INTO CH-REQUEST-ID
           END-STRING
           MOVE CA-SORT-CODE               TO CHAH-SORT-CODE
           MOVE CA-ACCT-NO                 TO CHAH-ACCT-NO
           MOVE ZERO                       TO CHAH-HOLD-SEQ
                                              CHAH-AMOUNT
                                              CHAH-EXPIRY-DATE
           .
      *================================================================*
       7100-LINK-SERVICE.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM('DDHLD01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'LINK'                 TO CH-REASON-CD
               MOVE WS-RESP                TO WS-RESP-DISPLAY
               MOVE SPACES                 TO CH-MESSAGE-TEXT
               STRING 'HOLDS SERVICE UNAVAILABLE - EIBRESP '
                      WS-RESP-DISPLAY
                      DELIMITED BY SIZE
                      INTO CH-MESSAGE-TEXT
               END-STRING
           END-IF
           .
      *================================================================*
       8000-SEND-DATAONLY.
      *================================================================*
           PERFORM 8200-SET-DATE-TIME
           EXEC CICS SEND
                     MAP('DDM04')
                     MAPSET('DDMAP4')
                     FROM(DDM04O)
                     DATAONLY
                     CURSOR
                     FREEKB
           END-EXEC
           .
      *================================================================*
       8100-SEND-MAP.
      *================================================================*
           PERFORM 8200-SET-DATE-TIME
           EXEC CICS SEND
                     MAP('DDM04')
                     MAPSET('DDMAP4')
                     FROM(DDM04O)
                     ERASE
                     CURSOR
                     FREEKB
           END-EXEC
           .
      *================================================================*
       8200-SET-DATE-TIME.
      *================================================================*
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     DDMMYYYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC
           .
      *================================================================*
       9000-END-SESSION.
      *================================================================*
           EXEC CICS SEND TEXT
                     FROM(WS-END-MESSAGE)
                     LENGTH(LENGTH OF WS-END-MESSAGE)
                     ERASE
                     FREEKB
           END-EXEC
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       9100-RETURN-TO-CF02.
      *================================================================*
      *    CF02 ACCEPTS A 12-BYTE COMMAREA HOLDING THE CIF NUMBER.
           IF CA-CIF-NO NOT = SPACES
               EXEC CICS XCTL
                         PROGRAM('CFSCRN2')
                         COMMAREA(CA-CIF-NO)
                         LENGTH(12)
                         RESP(WS-RESP)
               END-EXEC
           ELSE
               EXEC CICS XCTL
                         PROGRAM('CFSCRN2')
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           MOVE LOW-VALUES                 TO DDM04O
           MOVE 'CF02 NOT AVAILABLE'       TO MSGO
           PERFORM 8000-SEND-DATAONLY
           .
