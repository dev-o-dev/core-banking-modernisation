       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNSCRN1.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-10-18.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : LNSCRN1                                             *
      * TITLE    : BRANCH PERSONAL LOAN ENQUIRY / SETTLEMENT QUOTE     *
      * TYPE     : CICS PSEUDO-CONVERSATIONAL - TRANSACTION LN01       *
      * MAPSET   : LNMAPS  MAP LNM01                                   *
      * LINKS    : LNSVC01 (SERVICE LOANENQ - FUNCTIONS E / Q / L)     *
      *            CFSCRN2 (XCTL ON PF12 - CUSTOMER ACCOUNT LIST)      *
      * FILES    : NONE - ALL DATA COMES FROM LNSVC01                  *
      *----------------------------------------------------------------*
      * KEY EITHER:                                                    *
      *   LOAN NUMBER  - ENQUIRY (E). LOAN DETAILS AND THE FIRST 7 OF  *
      *                  THE 12 PROJECTED INSTALMENTS RETURNED BY THE  *
      *                  SERVICE. THE PROJECTION IS INDICATIVE (THE    *
      *                  BATCH ACCRUES DAILY) - THE SCREEN SAYS SO.    *
      *   CUSTOMER NO  - LIST (L). THE CUSTOMER'S LOANS ARE SHOWN IN   *
      *                  THE SCHEDULE AREA WITH A DIFFERENT HEADING.   *
      *                  KEY A LOAN NUMBER, OR A LINE NUMBER 1-7, IN   *
      *                  THE LOAN NUMBER FIELD TO SEE ONE OF THEM.     *
      * PF5 ON A LOAN ENQUIRY ASKS LNSVC01 FOR A SETTLEMENT QUOTE (Q)  *
      * AS AT TODAY. THE QUOTE IS RECORDED ON THE LOAN BY LNSVC01 AND  *
      * IS VALID FOR 28 DAYS - THE CUSTOMER MUST BE GIVEN THE FIGURE   *
      * IN WRITING (LENDING PROCEDURE LP-11; LETTER LN-SQ1 IS PRINTED  *
      * OVERNIGHT BY LNLTR010 FROM THE QUOTE ON THE LOAN).             *
      *                                                                *
      * WHICH FIELD WINS: BOTH INPUT FIELDS ARE FSET, SO BOTH COME     *
      * BACK ON EVERY ENTER. A CUSTOMER NUMBER THAT HAS CHANGED SINCE  *
      * THE LAST SCREEN MEANS LIST; OTHERWISE A LOAN NUMBER MEANS      *
      * ENQUIRY; OTHERWISE A CUSTOMER NUMBER MEANS LIST.               *
      *                                                                *
      * ENTERED FROM CF02 WITH THE CUSTOMER IN THE COMMAREA (THE LOANS *
      * ARE LISTED), OR DIRECTLY BY TRANSACTION LN01.                  *
      * ACCESS: RACF PROFILE CICSPRD1.LN01 (GROUPS BRSTAFF, LENDOPS).  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-10-18 JNB  ORIGINAL VERSION                               *
      * 2010-12-06 SAT  LIST BY CUSTOMER IN THE SCHEDULE AREA          *
      * 2011-03-21 SAT  LINE NUMBER SELECTION FROM THE LIST            *
      * 2011-06-27 JNB  QUOTE KEPT IN THE COMMAREA AND RE-SHOWN ON THE *
      *                 SAME LOAN THE SAME DAY (28 DAYS' INTEREST      *
      *                 CHANGE IN LNSVC01)                             *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNSCRN1 '.
      *
       01  WS-CONSTANTS.
           05  WS-SCH-LINES                PIC 9(02) VALUE 7.
           05  WS-MAX-LIST                 PIC 9(02) VALUE 10.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-LINE                     PIC S9(04) COMP.
           05  WS-SHOW-COUNT               PIC S9(04) COMP.
           05  WS-PAGE-MSG                 PIC X(79).
           05  WS-END-MESSAGE              PIC X(40)
                   VALUE 'LN01 - LOAN ENQUIRY ENDED'.
           05  WS-VALID-IND                PIC X(01).
               88  WS-INPUT-VALID              VALUE 'Y'.
           05  WS-REQUEST-IND              PIC X(01).
               88  WS-REQ-ENQUIRY              VALUE 'E'.
               88  WS-REQ-LIST                 VALUE 'L'.
               88  WS-REQ-NONE                 VALUE SPACE.
           05  WS-IN-LOAN-NO               PIC X(10).
           05  WS-IN-CIF-NO                PIC X(12).
           05  WS-LINE-NO                  PIC 9(01).
           05  WS-COUNT-EDIT               PIC Z9.
           05  WS-SCH-COUNT                PIC S9(04) COMP.
      *
      *---- EDIT PICTURES SIZED TO THE MAP FIELDS --------------------*
       01  WS-EDIT-FIELDS.
           05  WS-AMT14-EDIT               PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-AMT13-EDIT               PIC ZZ,ZZZ,ZZ9.99.
           05  WS-AMT12-EDIT               PIC Z,ZZZ,ZZ9.99.
           05  WS-RATE-EDIT.
               10  WS-RATE-NUM             PIC ZZ9.9999.
               10  FILLER                  PIC X(01) VALUE '%'.
           05  WS-APR-EDIT.
               10  WS-APR-NUM              PIC ZZ9.9.
               10  FILLER                  PIC X(01) VALUE '%'.
           05  WS-NUM3-EDIT                PIC ZZ9.
      *
      *---- DATE FORMATTING ------------------------------------------*
       01  WS-FMT-DATE-IN                  PIC 9(08).
       01  WS-FMT-DATE-IN-X REDEFINES WS-FMT-DATE-IN
                                           PIC X(08).
       01  WS-FMT-DATE-OUT                 PIC X(10).
      *
      *---- PSEUDO-CONVERSATIONAL COMMAREA ---------------------------*
       01  WS-COMMAREA.
           05  CA-EYECATCHER               PIC X(04).
               88  CA-OURS                     VALUE 'LN01'.
           05  CA-FROM-TRAN                PIC X(04).
      *        CUSTOMER FOR PF12 (FROM CF02, OR THE LOAN'S CUSTOMER)
           05  CA-CIF-NO                   PIC X(12).
           05  CA-MODE                     PIC X(01).
               88  CA-MODE-ENQUIRY             VALUE 'E'.
               88  CA-MODE-LIST                VALUE 'L'.
               88  CA-MODE-NONE                VALUE SPACE.
           05  CA-LOAN-NO                  PIC X(10).
      *        CUSTOMER NUMBER AS LAST SHOWN IN LNCIF
           05  CA-LIST-CIF                 PIC X(12).
           05  CA-LIST-COUNT               PIC 9(02).
           05  CA-LIST-LOAN                PIC X(10) OCCURS 10 TIMES.
           05  CA-QUOTE.
               10  CA-QT-LOAN-NO           PIC X(10).
               10  CA-QT-DATE              PIC 9(08).
               10  CA-QT-AMOUNT            PIC S9(13)V99.
               10  CA-QT-VALID-TO          PIC 9(08).
      *
      *---- COMMAREA RECEIVED FROM CF02 -------------------------------*
       01  WS-CF02-COMMAREA.
           05  XC-FROM-TRAN                PIC X(04).
           05  XC-SORT-CODE                PIC X(06).
           05  XC-ACCT-NO                  PIC X(08).
           05  XC-CIF-NO                   PIC X(12).
           05  FILLER                      PIC X(20).
      *
      *---- SCHEDULE AREA HEADINGS AND LINES --------------------------*
       01  WS-SCHED-HEADING.
           05  FILLER                      PIC X(04) VALUE 'NO  '.
           05  FILLER                      PIC X(13)
                                           VALUE 'DUE DATE'.
           05  FILLER                      PIC X(14)
                                           VALUE '       PAYMENT'.
           05  FILLER                      PIC X(15)
                                           VALUE '       INTEREST'.
           05  FILLER                      PIC X(15)
                                           VALUE '      PRINCIPAL'.
           05  FILLER                      PIC X(15)
                                           VALUE '        BALANCE'.
           05  FILLER                      PIC X(03) VALUE SPACES.
      *
       01  WS-LIST-HEADING.
           05  FILLER                      PIC X(13) VALUE 'L LOAN NO'.
           05  FILLER                      PIC X(13) VALUE 'STATUS'.
           05  FILLER                      PIC X(11) VALUE 'PRODUCT'.
           05  FILLER                      PIC X(14)
                                           VALUE '     PRINCIPAL'.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE 'NEXT DUE'.
           05  FILLER                      PIC X(14)
                                           VALUE '       ARREARS'.
      *
       01  WS-SCHED-LINE.
           05  SC-NO                       PIC Z9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  SC-DUE-DATE                 PIC X(10).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  SC-PAYMENT                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SC-INTEREST                 PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SC-PRINCIPAL                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  SC-BALANCE                  PIC ZZZ,ZZZ,ZZ9.99.
      *
       01  WS-LIST-LINE.
           05  LL-LINE-NO                  PIC 9(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LL-LOAN-NO                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LL-STATUS                   PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LL-PROD-CD                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  LL-PRINCIPAL                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  LL-NEXT-DUE                 PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  LL-ARREARS                  PIC ZZZ,ZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
      *
       01  WS-OUT-LINE                     PIC X(79).
       01  WS-STATUS-TEXT                  PIC X(12).
      *
           COPY LNMAPS.
      *
           COPY DFHAID.
      *
           COPY DFHBMSCA.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(186).
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
           MOVE SPACES                     TO WS-PAGE-MSG

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
                               PERFORM 5000-SETTLEMENT-QUOTE
                                   THRU 5000-EXIT
                           WHEN DFHENTER
                               PERFORM 2000-PROCESS-ENTER THRU 2000-EXIT
                           WHEN OTHER
                               MOVE LOW-VALUES TO LNM01O
                               MOVE 'INVALID KEY - USE ENTER, PF3, PF5'
                                           TO MSGO
                               PERFORM 8000-SEND-DATAONLY
                       END-EVALUATE
                   END-IF
           END-EVALUATE

           EXEC CICS RETURN
                     TRANSID('LN01')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
      *================================================================*
       1000-SEND-EMPTY-MAP.
      *================================================================*
           PERFORM 1200-INIT-COMMAREA
           MOVE LOW-VALUES                 TO LNM01O
           MOVE 'KEY A LOAN NUMBER, OR A CUSTOMER NUMBER TO LIST LOANS'
                                           TO MSGO
           PERFORM 8100-SEND-MAP
           .
      *================================================================*
       1100-START-FROM-CF02.
      *================================================================*
           PERFORM 1200-INIT-COMMAREA
           MOVE XC-FROM-TRAN               TO CA-FROM-TRAN
           MOVE XC-CIF-NO                  TO CA-CIF-NO
           IF XC-CIF-NO = SPACES
               MOVE LOW-VALUES             TO LNM01O
               MOVE 'NO CUSTOMER PASSED FROM CF02 - KEY A LOAN NUMBER'
                                           TO MSGO
               PERFORM 8100-SEND-MAP
           ELSE
               MOVE XC-CIF-NO              TO WS-IN-CIF-NO
               PERFORM 4000-LIST-LOANS THRU 4000-EXIT
           END-IF
           .
      *================================================================*
       1200-INIT-COMMAREA.
      *================================================================*
           MOVE SPACES                     TO WS-COMMAREA
           SET CA-OURS                     TO TRUE
           MOVE ZERO                       TO CA-LIST-COUNT
                                              CA-QT-DATE
                                              CA-QT-AMOUNT
                                              CA-QT-VALID-TO
           .
      *================================================================*
       2000-PROCESS-ENTER.
      *================================================================*
           EXEC CICS RECEIVE
                     MAP('LNM01')
                     MAPSET('LNMAPS')
                     INTO(LNM01I)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES             TO LNM01O
               MOVE 'KEY A LOAN NUMBER OR A CUSTOMER NUMBER'
                                           TO MSGO
               MOVE -1                     TO LOANNOL
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

           PERFORM 2100-RECEIVE-INPUT
           IF NOT WS-INPUT-VALID
               PERFORM 8000-SEND-DATAONLY
               GO TO 2000-EXIT
           END-IF

           EVALUATE TRUE
               WHEN WS-REQ-ENQUIRY
                   MOVE WS-IN-LOAN-NO      TO CA-LOAN-NO
                   PERFORM 3000-ENQUIRY THRU 3000-EXIT
               WHEN WS-REQ-LIST
                   PERFORM 4000-LIST-LOANS THRU 4000-EXIT
               WHEN OTHER
                   MOVE LOW-VALUES         TO LNM01O
                   MOVE 'KEY A LOAN NUMBER OR A CUSTOMER NUMBER'
                                           TO MSGO
                   MOVE -1                 TO LOANNOL
                   PERFORM 8000-SEND-DATAONLY
           END-EVALUATE
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       2100-RECEIVE-INPUT.
      *================================================================*
           MOVE 'Y'                        TO WS-VALID-IND
           MOVE SPACE                      TO WS-REQUEST-IND
           MOVE SPACES                     TO WS-IN-LOAN-NO
                                              WS-IN-CIF-NO
           IF LOANNOL > ZERO
               MOVE LOANNOI                TO WS-IN-LOAN-NO
           END-IF
           IF LNCIFL > ZERO
               MOVE FUNCTION UPPER-CASE (LNCIFI) TO WS-IN-CIF-NO
           END-IF
           INSPECT WS-IN-LOAN-NO REPLACING ALL LOW-VALUE BY SPACE
           INSPECT WS-IN-CIF-NO  REPLACING ALL LOW-VALUE BY SPACE
           MOVE LOW-VALUES                 TO LNM01O

      *    SEE 'WHICH FIELD WINS' IN THE HEADER.
           EVALUATE TRUE
               WHEN WS-IN-CIF-NO NOT = SPACES
               AND  WS-IN-CIF-NO NOT = CA-LIST-CIF
                   SET WS-REQ-LIST         TO TRUE
               WHEN WS-IN-LOAN-NO NOT = SPACES
                   SET WS-REQ-ENQUIRY      TO TRUE
               WHEN WS-IN-CIF-NO NOT = SPACES
                   SET WS-REQ-LIST         TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-REQ-ENQUIRY
               PERFORM 2200-RESOLVE-LINE-NO THRU 2200-EXIT
               IF WS-INPUT-VALID AND WS-IN-LOAN-NO NOT NUMERIC
                   MOVE 'N'                TO WS-VALID-IND
                   MOVE 'LOAN NUMBER MUST BE 10 DIGITS' TO MSGO
                   MOVE -1                 TO LOANNOL
                   MOVE DFHRED             TO LOANNOA
               END-IF
           END-IF
           .
      *================================================================*
       2200-RESOLVE-LINE-NO.
      *================================================================*
      *    WITH A LIST ON SCREEN, '1' TO '7' IN THE LOAN NUMBER FIELD
      *    PICKS THAT LINE OF THE LIST.
           IF NOT CA-MODE-LIST
           OR WS-IN-LOAN-NO (2:9) NOT = SPACES
           OR WS-IN-LOAN-NO (1:1) NOT NUMERIC
               GO TO 2200-EXIT
           END-IF
           MOVE WS-IN-LOAN-NO (1:1)        TO WS-LINE-NO
           IF WS-LINE-NO = ZERO
           OR WS-LINE-NO > WS-SCH-LINES
           OR WS-LINE-NO > CA-LIST-COUNT
               MOVE 'N'                    TO WS-VALID-IND
               MOVE 'NO LOAN ON THAT LINE OF THE LIST' TO MSGO
               MOVE -1                     TO LOANNOL
               MOVE DFHRED                 TO LOANNOA
               GO TO 2200-EXIT
           END-IF
           MOVE CA-LIST-LOAN (WS-LINE-NO)  TO WS-IN-LOAN-NO
           .
       2200-EXIT.
           EXIT.
      *================================================================*
       3000-ENQUIRY.
      *================================================================*
      *    ON ENTRY CA-LOAN-NO HOLDS THE LOAN AND WS-PAGE-MSG ANY
      *    MESSAGE THAT SHOULD REPLACE THE SERVICE'S.
           PERFORM 7000-INIT-SERVICE
           SET CHLN-ENQUIRY                TO TRUE
           MOVE CA-LOAN-NO                 TO CHLN-LOAN-NO
           PERFORM 7100-LINK-SERVICE

           MOVE LOW-VALUES                 TO LNM01O
           MOVE CA-LOAN-NO                 TO LOANNOO
           IF NOT (CH-RC-OK OR CH-RC-WARNING)
               SET CA-MODE-NONE            TO TRUE
               MOVE CH-MESSAGE-TEXT        TO MSGO
               MOVE -1                     TO LOANNOL
               MOVE DFHRED                 TO LOANNOA
               PERFORM 8100-SEND-MAP
               GO TO 3000-EXIT
           END-IF

           SET CA-MODE-ENQUIRY             TO TRUE
      *    THE LOAN'S CUSTOMER IS SHOWN IN LNCIF AND REMEMBERED SO IT
      *    DOES NOT COUNT AS A NEW CUSTOMER ON THE NEXT ENTER.
           MOVE CHLN-CIF-NO                TO LNCIFO
                                              CA-LIST-CIF
           IF CA-CIF-NO = SPACES
               MOVE CHLN-CIF-NO            TO CA-CIF-NO
           END-IF
           PERFORM 3100-FORMAT-DETAIL
           PERFORM 3200-FORMAT-SCHEDULE
           PERFORM 5100-SHOW-QUOTE

           EVALUATE TRUE
               WHEN WS-PAGE-MSG NOT = SPACES
                   MOVE WS-PAGE-MSG        TO MSGO
               WHEN CH-RC-WARNING
                   MOVE CH-MESSAGE-TEXT    TO MSGO
               WHEN CHLN-SCHED-COUNT > WS-SCH-LINES
                   MOVE CHLN-SCHED-COUNT   TO WS-COUNT-EDIT
                   STRING 'NEXT 7 OF ' WS-COUNT-EDIT
                          ' INSTALMENTS (INDICATIVE).'
                          ' PF5 = SETTLEMENT QUOTE'
                          DELIMITED BY SIZE
                          INTO MSGO
                   END-STRING
               WHEN OTHER
                   MOVE CH-MESSAGE-TEXT    TO MSGO
           END-EVALUATE
           PERFORM 8100-SEND-MAP
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       3100-FORMAT-DETAIL.
      *================================================================*
           MOVE CHLN-STATUS                TO WS-STATUS-TEXT
           PERFORM 3150-STATUS-TEXT
           MOVE WS-STATUS-TEXT             TO LSTATO
           IF CHLN-STATUS = 'D' OR 'C' OR 'W'
               MOVE DFHRED                 TO LSTATA
           END-IF
           MOVE CHLN-PROD-CD               TO LPRODO
           MOVE CHLN-FORBEARANCE-IND       TO LFBRO

           MOVE CHLN-ADVANCE-AMT           TO WS-AMT14-EDIT
           MOVE WS-AMT14-EDIT              TO LADVO
           MOVE CHLN-ADVANCE-DATE          TO WS-FMT-DATE-IN
           PERFORM 6100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT            TO LADVDTO
           MOVE CHLN-TERM-MONTHS           TO WS-NUM3-EDIT
           MOVE WS-NUM3-EDIT               TO LTERMO

           MOVE CHLN-NOMINAL-RATE          TO WS-RATE-NUM
           MOVE WS-RATE-EDIT               TO LRATEO
           MOVE CHLN-APR                   TO WS-APR-NUM
           MOVE WS-APR-EDIT                TO LAPRO
           MOVE CHLN-PMTS-REMAINING        TO WS-NUM3-EDIT
           MOVE WS-NUM3-EDIT               TO LREMO

           MOVE CHLN-MONTHLY-PMT           TO WS-AMT14-EDIT
           MOVE WS-AMT14-EDIT              TO LPMTO
           MOVE CHLN-NEXT-DUE-DATE         TO WS-FMT-DATE-IN
           PERFORM 6100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT            TO LNXTDTO
           MOVE CHLN-FINAL-DUE-DATE        TO WS-FMT-DATE-IN
           PERFORM 6100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT            TO LFINDTO

           MOVE CHLN-PRINCIPAL             TO WS-AMT14-EDIT
           MOVE WS-AMT14-EDIT              TO LPRINO
           MOVE CHLN-ACCRUED-INT           TO WS-AMT12-EDIT
           MOVE WS-AMT12-EDIT              TO LINTO
           MOVE CHLN-FEES                  TO WS-AMT12-EDIT
           MOVE WS-AMT12-EDIT              TO LFEESO

           MOVE CHLN-ARREARS               TO WS-AMT14-EDIT
           MOVE WS-AMT14-EDIT              TO LARRO
           IF CHLN-ARREARS > ZERO
               MOVE DFHRED                 TO LARRA
           END-IF
           MOVE CHLN-MISSED-COUNT          TO WS-NUM3-EDIT
           MOVE WS-NUM3-EDIT               TO LMISSO
           MOVE CHLN-TOTAL-INT-PAID        TO WS-AMT13-EDIT
           MOVE WS-AMT13-EDIT              TO LINTPDO

           IF CHLN-REPAY-SORT-CODE NOT = SPACES
               STRING CHLN-REPAY-SORT-CODE ' ' CHLN-REPAY-ACCT-NO
                      DELIMITED BY SIZE
                      INTO LRPAYO
               END-STRING
           ELSE
               MOVE 'NOT SET'              TO LRPAYO
           END-IF
           .
      *================================================================*
       3150-STATUS-TEXT.
      *================================================================*
      *    ON ENTRY WS-STATUS-TEXT (1:1) HOLDS THE LN-STATUS CODE.
           EVALUATE WS-STATUS-TEXT (1:1)
               WHEN 'A'  MOVE 'ACTIVE'       TO WS-STATUS-TEXT
               WHEN 'S'  MOVE 'SETTLED'      TO WS-STATUS-TEXT
               WHEN 'D'  MOVE 'DEFAULTED'    TO WS-STATUS-TEXT
               WHEN 'C'  MOVE 'COLLECTIONS'  TO WS-STATUS-TEXT
               WHEN 'W'  MOVE 'WRITTEN OFF'  TO WS-STATUS-TEXT
               WHEN 'P'  MOVE 'NOT DRAWN'    TO WS-STATUS-TEXT
               WHEN OTHER
                   MOVE 'UNKNOWN'            TO WS-STATUS-TEXT
           END-EVALUATE
           .
      *================================================================*
       3200-FORMAT-SCHEDULE.
      *================================================================*
           MOVE WS-SCHED-HEADING           TO SCHHDRO
           MOVE CHLN-SCHED-COUNT           TO WS-SCH-COUNT
           IF WS-SCH-COUNT > 12
               MOVE 12                     TO WS-SCH-COUNT
           END-IF
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-SCH-LINES
               MOVE SPACES                 TO WS-OUT-LINE
               IF WS-LINE <= WS-SCH-COUNT
                   MOVE SPACES             TO WS-SCHED-LINE
                   MOVE WS-LINE            TO SC-NO
                   MOVE CHLNS-DUE-DATE (WS-LINE) TO WS-FMT-DATE-IN
                   PERFORM 6100-FORMAT-DATE
                   MOVE WS-FMT-DATE-OUT    TO SC-DUE-DATE
                   MOVE CHLNS-PAYMENT (WS-LINE)   TO SC-PAYMENT
                   MOVE CHLNS-INTEREST (WS-LINE)  TO SC-INTEREST
                   MOVE CHLNS-PRINCIPAL (WS-LINE) TO SC-PRINCIPAL
                   MOVE CHLNS-BALANCE (WS-LINE)   TO SC-BALANCE
                   MOVE WS-SCHED-LINE      TO WS-OUT-LINE
               END-IF
               IF WS-LINE = 1 AND WS-SCH-COUNT = ZERO
                   MOVE '    NO SCHEDULE - LOAN IS NOT REPAYING'
                                           TO WS-OUT-LINE
               END-IF
               PERFORM 3300-PLACE-SCH-LINE
           END-PERFORM
           .
      *================================================================*
       3300-PLACE-SCH-LINE.
      *================================================================*
           EVALUATE WS-LINE
               WHEN 1   MOVE WS-OUT-LINE TO SCH1O
               WHEN 2   MOVE WS-OUT-LINE TO SCH2O
               WHEN 3   MOVE WS-OUT-LINE TO SCH3O
               WHEN 4   MOVE WS-OUT-LINE TO SCH4O
               WHEN 5   MOVE WS-OUT-LINE TO SCH5O
               WHEN 6   MOVE WS-OUT-LINE TO SCH6O
               WHEN 7   MOVE WS-OUT-LINE TO SCH7O
           END-EVALUATE
           .
      *================================================================*
       4000-LIST-LOANS.
      *================================================================*
      *    ON ENTRY WS-IN-CIF-NO HOLDS THE CUSTOMER.
           PERFORM 7000-INIT-SERVICE
           SET CHLN-LIST                   TO TRUE
           MOVE WS-IN-CIF-NO               TO CHLN-CIF-NO
           PERFORM 7100-LINK-SERVICE

           MOVE LOW-VALUES                 TO LNM01O
           MOVE WS-IN-CIF-NO               TO LNCIFO
                                              CA-LIST-CIF
           MOVE SPACES                     TO CA-LOAN-NO
           MOVE ZERO                       TO CA-LIST-COUNT
           SET CA-MODE-NONE                TO TRUE

           IF NOT (CH-RC-OK OR CH-RC-WARNING)
               MOVE CH-MESSAGE-TEXT        TO MSGO
               MOVE -1                     TO LNCIFL
               MOVE DFHRED                 TO LNCIFA
               PERFORM 8100-SEND-MAP
               GO TO 4000-EXIT
           END-IF

           MOVE WS-IN-CIF-NO               TO CA-CIF-NO
           MOVE CHLN-LIST-COUNT            TO CA-LIST-COUNT
           IF CA-LIST-COUNT > WS-MAX-LIST
               MOVE WS-MAX-LIST            TO CA-LIST-COUNT
           END-IF
           IF CA-LIST-COUNT = ZERO
               MOVE CH-MESSAGE-TEXT        TO MSGO
               MOVE -1                     TO LNCIFL
               PERFORM 8100-SEND-MAP
               GO TO 4000-EXIT
           END-IF

           SET CA-MODE-LIST                TO TRUE
           MOVE WS-LIST-HEADING            TO SCHHDRO
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-MAX-LIST
               IF WS-LINE <= CA-LIST-COUNT
                   MOVE CHLNL-LOAN-NO (WS-LINE)
                                           TO CA-LIST-LOAN (WS-LINE)
               ELSE
                   MOVE SPACES             TO CA-LIST-LOAN (WS-LINE)
               END-IF
           END-PERFORM

           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-SCH-LINES
               MOVE SPACES                 TO WS-OUT-LINE
               IF WS-LINE <= CA-LIST-COUNT
                   PERFORM 4100-FORMAT-LIST-LINE
               END-IF
               PERFORM 3300-PLACE-SCH-LINE
           END-PERFORM

           IF CA-LIST-COUNT > WS-SCH-LINES OR CH-REASON-CD = 'MORE'
               MOVE CA-LIST-COUNT          TO WS-COUNT-EDIT
               STRING 'FIRST 7 OF ' WS-COUNT-EDIT
                      ' LOANS SHOWN. KEY A LOAN NUMBER FOR DETAILS'
                      DELIMITED BY SIZE
                      INTO MSGO
               END-STRING
           ELSE
               MOVE 'KEY A LOAN NUMBER OR LINE NUMBER (1-7), ENTER'
                                           TO MSGO
           END-IF
           MOVE -1                         TO LOANNOL
           PERFORM 8100-SEND-MAP
           .
       4000-EXIT.
           EXIT.
      *================================================================*
       4100-FORMAT-LIST-LINE.
      *================================================================*
           MOVE SPACES                     TO WS-LIST-LINE
           MOVE CHLNL-LOAN-NO (WS-LINE)    TO LL-LOAN-NO
           MOVE CHLNL-STATUS (WS-LINE)     TO WS-STATUS-TEXT
           PERFORM 3150-STATUS-TEXT
           MOVE WS-STATUS-TEXT             TO LL-STATUS
           MOVE CHLNL-PROD-CD (WS-LINE)    TO LL-PROD-CD
           MOVE CHLNL-PRINCIPAL (WS-LINE)  TO LL-PRINCIPAL
           MOVE CHLNL-NEXT-DUE-DATE (WS-LINE) TO WS-FMT-DATE-IN
           PERFORM 6100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT            TO LL-NEXT-DUE
           MOVE CHLNL-ARREARS (WS-LINE)    TO LL-ARREARS
           MOVE WS-LINE                    TO LL-LINE-NO
           MOVE WS-LIST-LINE               TO WS-OUT-LINE
           .
      *================================================================*
       5000-SETTLEMENT-QUOTE.
      *================================================================*
           IF NOT CA-MODE-ENQUIRY OR CA-LOAN-NO = SPACES
               MOVE LOW-VALUES             TO LNM01O
               MOVE 'ENQUIRE ON A LOAN FIRST, THEN PF5 FOR A QUOTE'
                                           TO MSGO
               MOVE -1                     TO LOANNOL
               PERFORM 8000-SEND-DATAONLY
               GO TO 5000-EXIT
           END-IF

           PERFORM 7000-INIT-SERVICE
           SET CHLN-QUOTE                  TO TRUE
           MOVE CA-LOAN-NO                 TO CHLN-LOAN-NO
      *    ZERO = AS AT TODAY. FORWARD-DATED QUOTES ARE ISSUED BY
      *    LENDING OPERATIONS (LN02), NOT AT THE BRANCH.
           MOVE ZERO                       TO CHLN-SETTLE-DATE
           PERFORM 7100-LINK-SERVICE

           IF CH-RC-OK OR CH-RC-WARNING
               MOVE CA-LOAN-NO             TO CA-QT-LOAN-NO
               MOVE WS-TODAY               TO CA-QT-DATE
               MOVE CHLN-QT-AMOUNT         TO CA-QT-AMOUNT
               MOVE CHLN-QT-VALID-TO       TO CA-QT-VALID-TO
               IF CH-RC-WARNING
                   MOVE CH-MESSAGE-TEXT    TO WS-PAGE-MSG
               ELSE
                   STRING 'SETTLEMENT QUOTE ISSUED - LETTER LN-SQ1'
                          ' WILL BE SENT TONIGHT'
                          DELIMITED BY SIZE
                          INTO WS-PAGE-MSG
                   END-STRING
               END-IF
      *        RE-ENQUIRE SO THE DETAIL REFLECTS THE QUOTE.
               PERFORM 3000-ENQUIRY THRU 3000-EXIT
           ELSE
               MOVE LOW-VALUES             TO LNM01O
               MOVE CH-MESSAGE-TEXT        TO MSGO
               MOVE -1                     TO LOANNOL
               PERFORM 8000-SEND-DATAONLY
           END-IF
           .
       5000-EXIT.
           EXIT.
      *================================================================*
       5100-SHOW-QUOTE.
      *================================================================*
      *    A QUOTE ISSUED TODAY FOR THIS LOAN IS SHOWN AGAIN ON EVERY
      *    ENQUIRY UNTIL THE USER MOVES TO ANOTHER LOAN.
           IF CA-QT-LOAN-NO = CA-LOAN-NO
           AND CA-QT-DATE = WS-TODAY
               MOVE CA-QT-AMOUNT           TO WS-AMT14-EDIT
               MOVE WS-AMT14-EDIT          TO LSETTLO
               MOVE CA-QT-VALID-TO         TO WS-FMT-DATE-IN
               PERFORM 6100-FORMAT-DATE
               MOVE WS-FMT-DATE-OUT        TO LVALIDO
           ELSE
               MOVE SPACES                 TO CA-QUOTE
               MOVE ZERO                   TO CA-QT-DATE
                                              CA-QT-AMOUNT
                                              CA-QT-VALID-TO
               MOVE 'PF5'                  TO LSETTLO
           END-IF
           .
      *================================================================*
       6100-FORMAT-DATE.
      *================================================================*
      *    CCYYMMDD -> DD/MM/CCYY. ZERO OR NON-NUMERIC -> SPACES.
           MOVE SPACES                     TO WS-FMT-DATE-OUT
           IF WS-FMT-DATE-IN-X NUMERIC AND WS-FMT-DATE-IN > ZERO
               STRING WS-FMT-DATE-IN-X (7:2) '/'
                      WS-FMT-DATE-IN-X (5:2) '/'
                      WS-FMT-DATE-IN-X (1:4)
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
           MOVE 'LOANENQ '                 TO CH-SERVICE-CD
           MOVE 'BRANCH'                   TO CH-CHANNEL-CD
           MOVE EIBTRMID                   TO CH-USER-ID
           STRING 'LN01' EIBTRMID EIBTASKN
                  DELIMITED BY SIZE
                  INTO CH-REQUEST-ID
           END-STRING
           MOVE ZERO                       TO CHLN-SETTLE-DATE
           .
      *================================================================*
       7100-LINK-SERVICE.
      *================================================================*
           EXEC CICS LINK
                     PROGRAM('LNSVC01')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'LINK'                 TO CH-REASON-CD
               MOVE WS-RESP                TO WS-RESP-DISPLAY
               MOVE SPACES                 TO CH-MESSAGE-TEXT
               STRING 'LOAN SERVICE UNAVAILABLE - EIBRESP '
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
                     MAP('LNM01')
                     MAPSET('LNMAPS')
                     FROM(LNM01O)
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
                     MAP('LNM01')
                     MAPSET('LNMAPS')
                     FROM(LNM01O)
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
           MOVE LOW-VALUES                 TO LNM01O
           MOVE 'CF02 NOT AVAILABLE'       TO MSGO
           PERFORM 8000-SEND-DATAONLY
           .
