       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFDUP010.
       AUTHOR.        A O SULLIVAN.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2011-04-11.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFDUP010                                            *
      * TITLE    : CIF - DUPLICATE CUSTOMER DETECTION                  *
      * TYPE     : BATCH COBOL (NO DB2) - INTERNAL SORTS               *
      * JCL      : CFDUP01 (MONTHLY - FIRST SATURDAY)                  *
      *----------------------------------------------------------------*
      * INPUT    : CIFMAST  - CIF MASTER (SEQUENTIAL)                  *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * WORK     : SORTWK01-03 (TWO SORTS)                             *
      * OUTPUT   : DUPCAND  - CANDIDATE PAIRS FOR THE CIF DATA QUALITY *
      *                       TEAM (FB 200)                            *
      *            REPORT   - SUMMARY (FBA 133)                        *
      *----------------------------------------------------------------*
      * DUPLICATE CUSTOMERS BREAK THE FSCS SINGLE CUSTOMER VIEW        *
      * (RGSCV020 AGGREGATES BY CIF), KYC (ONE PERSON, TWO RISK        *
      * RATINGS) AND MARKETING SUPPRESSION. THE BRANCH OPENING SCREEN  *
      * WARNS ON EXACT MATCHES ONLY; THIS JOB FINDS THE REST.          *
      *                                                                *
      * SORT 1 - MATCH KEY: SURNAME (UPPER CASE, NON-LETTERS REMOVED,  *
      *          FIRST 12), DATE OF BIRTH, POSTCODE (NO SPACES).       *
      *          ADJACENT RECORDS WITH THE SAME KEY ARE SCORED:        *
      *            + 40  SAME KEY (SURNAME, DOB, POSTCODE)             *
      *            + 30  SAME FIRST NAME (FIRST 6 LETTERS)             *
      *            + 20  SAME FIRST INITIAL ONLY                       *
      *            + 20  SAME ADDRESS LINE 1                           *
      *            + 10  SAME MOBILE OR EMAIL                          *
      * SORT 2 - NATIONAL INSURANCE NUMBER (PERSONAL CUSTOMERS WITH A  *
      *          NINO ONLY). THE SAME NINO ON TWO CIFS SCORES 100 -    *
      *          EITHER A DUPLICATE OR A KEYING ERROR, BOTH NEED FIXING*
      * PAIRS SCORING WS-MIN-SCORE OR MORE ARE WRITTEN. GROUPS OF MORE *
      * THAN WS-MAX-GROUP ON ONE KEY (E.G. TEST CUSTOMERS, CARE HOMES) *
      * ARE REPORTED AS ONE LINE AND NOT PAIRED.                       *
      *                                                                *
      * THE TEAM MERGES CONFIRMED DUPLICATES WITH CFMNT01 (FUNCTION    *
      * 'M') WHICH SETS CIF-MERGED-INTO. MERGED, CLOSED AND DECEASED   *
      * CUSTOMERS ARE NOT INPUT TO THE SORT.                           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2011-04-11 AOS  ORIGINAL VERSION (FSCS SCV FINDINGS 2010)      *
      * 2012-10-01 AOS  NINO PASS ADDED                                *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT SORT-FILE        ASSIGN TO SORTWK01.
           SELECT DUPCAND-FILE     ASSIGN TO DUPCAND
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DUPCAND-STATUS.
           SELECT REPORT-FILE      ASSIGN TO REPORT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REPORT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
       SD  SORT-FILE.
       01  SR-SORT-RECORD.
           05  SR-MATCH-KEY.
               10  SR-SURNAME-KEY          PIC X(12).
               10  SR-DOB                  PIC 9(08).
               10  SR-POSTCODE-KEY         PIC X(08).
           05  SR-NINO                     PIC X(13).
           05  SR-CIF-NO                   PIC X(12).
           05  SR-FIRST-KEY                PIC X(06).
           05  SR-ADDR-LINE1               PIC X(30).
           05  SR-MOBILE                   PIC X(20).
           05  SR-EMAIL                    PIC X(40).
           05  SR-CREATE-DATE              PIC 9(08).
           05  SR-HOME-BRANCH              PIC X(06).
           05  SR-DISPLAY-NAME             PIC X(40).
       FD  DUPCAND-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DUPCAND-RECORD                  PIC X(200).
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                   PIC X(133).
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFDUP010'.
      *
       01  WS-CONSTANTS.
           05  WS-MIN-SCORE                PIC 9(03) VALUE 070.
           05  WS-MAX-GROUP                PIC S9(04) COMP VALUE +10.
           05  WS-ALPHA                    PIC X(26)
               VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05  WS-LOWER                    PIC X(26)
               VALUE 'abcdefghijklmnopqrstuvwxyz'.
      *
       01  WS-FILE-STATUSES.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-EOF              VALUE '10'.
           05  WS-DUPCAND-STATUS           PIC X(02).
           05  WS-REPORT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01).
               88  WS-END-OF-CIF               VALUE 'Y'.
           05  WS-SORT-EOF-IND             PIC X(01).
               88  WS-END-OF-SORT              VALUE 'Y'.
           05  WS-PASS                     PIC X(01).
               88  WS-PASS-NAME                VALUE 'N'.
               88  WS-PASS-NINO                VALUE 'I'.
      *
       01  WS-WORK-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-NORM-IN                  PIC X(40).
           05  WS-NORM-OUT                 PIC X(40).
           05  WS-NORM-LEN                 PIC S9(04) COMP.
           05  WS-I                        PIC S9(04) COMP.
           05  WS-J                        PIC S9(04) COMP.
           05  WS-K                        PIC S9(04) COMP.
           05  WS-CHAR                     PIC X(01).
           05  WS-SCORE                    PIC 9(03).
           05  WS-REASONS                  PIC X(30).
      *
      *---- GROUP OF RECORDS SHARING A KEY ----------------------------*
       01  WS-GROUP.
           05  WS-GRP-COUNT                PIC S9(04) COMP VALUE ZERO.
           05  WS-GRP-OVERFLOW             PIC S9(04) COMP VALUE ZERO.
           05  WS-GRP-KEY                  PIC X(28).
           05  WS-GRP-ENTRY                OCCURS 10 TIMES.
               10  WS-GRP-REC              PIC X(203).
       01  WS-A                            PIC X(203).
       01  WS-A-R REDEFINES WS-A.
           05  WA-MATCH-KEY                PIC X(28).
           05  WA-NINO                     PIC X(13).
           05  WA-CIF-NO                   PIC X(12).
           05  WA-FIRST-KEY                PIC X(06).
           05  WA-ADDR-LINE1               PIC X(30).
           05  WA-MOBILE                   PIC X(20).
           05  WA-EMAIL                    PIC X(40).
           05  WA-CREATE-DATE              PIC 9(08).
           05  WA-HOME-BRANCH              PIC X(06).
           05  WA-DISPLAY-NAME             PIC X(40).
       01  WS-B                            PIC X(203).
       01  WS-B-R REDEFINES WS-B.
           05  WB-MATCH-KEY                PIC X(28).
           05  WB-NINO                     PIC X(13).
           05  WB-CIF-NO                   PIC X(12).
           05  WB-FIRST-KEY                PIC X(06).
           05  WB-ADDR-LINE1               PIC X(30).
           05  WB-MOBILE                   PIC X(20).
           05  WB-EMAIL                    PIC X(40).
           05  WB-CREATE-DATE              PIC 9(08).
           05  WB-HOME-BRANCH              PIC X(06).
           05  WB-DISPLAY-NAME             PIC X(40).
      *
      *---- CANDIDATE PAIR --------------------------------------------*
       01  WS-CANDIDATE.
           05  DC-PASS                     PIC X(01).
           05  DC-SCORE                    PIC 9(03).
           05  DC-REASONS                  PIC X(30).
           05  DC-CIF-A                    PIC X(12).
           05  DC-NAME-A                   PIC X(40).
           05  DC-CREATED-A                PIC 9(08).
           05  DC-CIF-B                    PIC X(12).
           05  DC-NAME-B                   PIC X(40).
           05  DC-CREATED-B                PIC 9(08).
           05  DC-MATCH-KEY                PIC X(28).
           05  DC-RUN-DATE                 PIC 9(08).
           05  FILLER                      PIC X(10).
      *
       01  WS-COUNTERS.
           05  WS-CIF-READ                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-CIF-EXCLUDED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RELEASED                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-NINO-RELEASED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-GROUPS                   PIC S9(09) COMP-3 VALUE 0.
           05  WS-BIG-GROUPS               PIC S9(09) COMP-3 VALUE 0.
           05  WS-PAIRS-SCORED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-CANDIDATES               PIC S9(09) COMP-3 VALUE 0.
           05  WS-NINO-CANDIDATES          PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-REPORT-CONTROL.
           05  WS-LINE-COUNT               PIC S9(04) COMP VALUE +99.
           05  WS-PAGE-COUNT               PIC S9(04) COMP VALUE ZERO.
       01  WS-HEADING-1.
           05  FILLER                      PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(10) VALUE 'CFDUP010'.
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(55)
               VALUE 'MEGABANK PLC - DUPLICATE CUSTOMER DETECTION'.
           05  FILLER                      PIC X(15)
               VALUE 'BUSINESS DATE '.
           05  HD1-BUS-DATE                PIC X(10).
           05  FILLER                      PIC X(07) VALUE '  PAGE '.
           05  HD1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(06) VALUE SPACES.
       01  WS-BIG-GROUP-LINE.
           05  FILLER                      PIC X(01) VALUE ' '.
           05  FILLER                      PIC X(30)
               VALUE 'LARGE GROUP NOT PAIRED - KEY: '.
           05  BG-KEY                      PIC X(28).
           05  FILLER                      PIC X(10) VALUE '  SIZE: '.
           05  BG-SIZE                     PIC ZZZ9.
           05  FILLER                      PIC X(60) VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  TL-CC                       PIC X(01) VALUE '0'.
           05  TL-LABEL                    PIC X(40).
           05  TL-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.
      *
           COPY DATECARD.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID
           PERFORM 1000-INITIALISE

           SET WS-PASS-NAME                TO TRUE
           SORT SORT-FILE
               ON ASCENDING KEY SR-MATCH-KEY SR-CIF-NO
               INPUT PROCEDURE  IS 2000-RELEASE-CIF
               OUTPUT PROCEDURE IS 3000-SCAN-GROUPS
           IF SORT-RETURN NOT = ZERO
               MOVE '0000-MAIN'            TO ABN-PARAGRAPH
               MOVE 1006                   TO ABN-ABEND-CODE
               MOVE 'SORT 1 (NAME KEY) FAILED' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           SET WS-PASS-NINO                TO TRUE
           SORT SORT-FILE
               ON ASCENDING KEY SR-NINO SR-CIF-NO
               INPUT PROCEDURE  IS 2000-RELEASE-CIF
               OUTPUT PROCEDURE IS 3000-SCAN-GROUPS
           IF SORT-RETURN NOT = ZERO
               MOVE '0000-MAIN'            TO ABN-PARAGRAPH
               MOVE 1006                   TO ABN-ABEND-CODE
               MOVE 'SORT 2 (NINO) FAILED' TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           PERFORM 9000-FINALISE
           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  DATECARD-FILE
                OUTPUT DUPCAND-FILE
                       REPORT-FILE
           IF WS-DUPCAND-STATUS NOT = '00'
           OR WS-REPORT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
           OR DTC-BUSINESS-DATE NOT NUMERIC
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE DTC-BUSINESS-DATE          TO WS-BUS-DATE
           STRING WS-BUS-DATE (1:4) '-' WS-BUS-DATE (5:2) '-'
                  WS-BUS-DATE (7:2)
                  DELIMITED BY SIZE
                  INTO HD1-BUS-DATE
           END-STRING
           PERFORM 7000-REPORT-HEADINGS
           .
      *================================================================*
       2000-RELEASE-CIF SECTION.
      *================================================================*
       2000-START.
           OPEN INPUT CIFMAST-FILE
           IF NOT WS-CIFMAST-OK
               MOVE '2000-RELEASE-CIF'     TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'CIFMAST OPEN FAILED'  TO ABN-MESSAGE
               MOVE WS-CIFMAST-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           MOVE 'N'                        TO WS-EOF-IND
           PERFORM 2900-READ-CIF
           PERFORM UNTIL WS-END-OF-CIF
               IF WS-PASS-NAME
                   ADD 1                   TO WS-CIF-READ
               END-IF
               EVALUATE TRUE
                   WHEN CIF-DECEASED OR CIF-CLOSED
                   WHEN CIF-MERGED-INTO NOT = SPACES
                       IF WS-PASS-NAME
                           ADD 1           TO WS-CIF-EXCLUDED
                       END-IF
                   WHEN WS-PASS-NINO
                       IF CIF-PERSONAL
                       AND CIF-NAT-INS-NO NOT = SPACES
                           PERFORM 2100-BUILD-SORT-RECORD
                           RELEASE SR-SORT-RECORD
                           ADD 1           TO WS-NINO-RELEASED
                       END-IF
                   WHEN OTHER
                       PERFORM 2100-BUILD-SORT-RECORD
                       RELEASE SR-SORT-RECORD
                       ADD 1               TO WS-RELEASED
               END-EVALUATE
               PERFORM 2900-READ-CIF
           END-PERFORM
           CLOSE CIFMAST-FILE
           GO TO 2000-EXIT
           .
      *----------------------------------------------------------------*
       2100-BUILD-SORT-RECORD.
      *----------------------------------------------------------------*
           MOVE SPACES                     TO SR-SORT-RECORD
           MOVE CIF-LAST-NM                TO WS-NORM-IN
           PERFORM 2200-NORMALISE
           MOVE WS-NORM-OUT (1:12)         TO SR-SURNAME-KEY
           IF CIF-DOB NUMERIC
               MOVE CIF-DOB                TO SR-DOB
           ELSE
               MOVE ZERO                   TO SR-DOB
           END-IF
           MOVE CIF-POST-CD                TO WS-NORM-IN
           PERFORM 2200-NORMALISE
           MOVE WS-NORM-OUT (1:8)          TO SR-POSTCODE-KEY
           MOVE CIF-NAT-INS-NO             TO WS-NORM-IN
           PERFORM 2200-NORMALISE
           MOVE WS-NORM-OUT (1:13)         TO SR-NINO
           MOVE CIF-NO                     TO SR-CIF-NO
           MOVE CIF-FIRST-NM               TO WS-NORM-IN
           PERFORM 2200-NORMALISE
           MOVE WS-NORM-OUT (1:6)          TO SR-FIRST-KEY
           MOVE CIF-ADDR-LINE1             TO WS-NORM-IN
           PERFORM 2200-NORMALISE
           MOVE WS-NORM-OUT (1:30)         TO SR-ADDR-LINE1
           MOVE CIF-MOBILE-TEL             TO SR-MOBILE
           MOVE FUNCTION UPPER-CASE (CIF-EMAIL-ADDR (1:40))
                                           TO SR-EMAIL
           MOVE CIF-CREATE-DATE            TO SR-CREATE-DATE
           MOVE CIF-HOME-BRANCH-SC         TO SR-HOME-BRANCH
           STRING CIF-FIRST-NM  DELIMITED BY SPACE
                  ' '           DELIMITED BY SIZE
                  CIF-LAST-NM   DELIMITED BY '  '
                  INTO SR-DISPLAY-NAME
           END-STRING
           .
      *----------------------------------------------------------------*
       2200-NORMALISE.
      *----------------------------------------------------------------*
      *    UPPER CASE, KEEP LETTERS AND DIGITS ONLY: "O'BRIEN-SMITH"
      *    BECOMES "OBRIENSMITH", "SW1A 1AA" BECOMES "SW1A1AA".
           INSPECT WS-NORM-IN CONVERTING WS-LOWER TO WS-ALPHA
           MOVE SPACES                     TO WS-NORM-OUT
           MOVE ZERO                       TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 40
               MOVE WS-NORM-IN (WS-I:1)    TO WS-CHAR
               IF (WS-CHAR >= 'A' AND WS-CHAR <= 'Z')
               OR (WS-CHAR >= '0' AND WS-CHAR <= '9')
                   ADD 1                   TO WS-J
                   MOVE WS-CHAR            TO WS-NORM-OUT (WS-J:1)
               END-IF
           END-PERFORM
           .
      *----------------------------------------------------------------*
       2900-READ-CIF.
      *----------------------------------------------------------------*
           READ CIFMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   CONTINUE
               WHEN WS-CIFMAST-EOF
                   SET WS-END-OF-CIF       TO TRUE
               WHEN OTHER
                   MOVE '2900-READ-CIF'    TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED' TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-SCAN-GROUPS SECTION.
      *================================================================*
       3000-START.
           MOVE 'N'                        TO WS-SORT-EOF-IND
           MOVE ZERO                       TO WS-GRP-COUNT
                                              WS-GRP-OVERFLOW
           MOVE LOW-VALUES                 TO WS-GRP-KEY
           RETURN SORT-FILE
               AT END SET WS-END-OF-SORT   TO TRUE
           END-RETURN
           PERFORM UNTIL WS-END-OF-SORT
               IF (WS-PASS-NAME AND SR-MATCH-KEY NOT = WS-GRP-KEY)
               OR (WS-PASS-NINO AND SR-NINO NOT = WS-GRP-KEY (1:13))
                   PERFORM 3100-END-GROUP THRU 3100-END-EXIT
                   MOVE ZERO               TO WS-GRP-COUNT
                                              WS-GRP-OVERFLOW
                   IF WS-PASS-NAME
                       MOVE SR-MATCH-KEY   TO WS-GRP-KEY
                   ELSE
                       MOVE SPACES         TO WS-GRP-KEY
                       MOVE SR-NINO        TO WS-GRP-KEY (1:13)
                   END-IF
               END-IF
               IF WS-GRP-COUNT < WS-MAX-GROUP
                   ADD 1                   TO WS-GRP-COUNT
                   MOVE SR-SORT-RECORD     TO WS-GRP-REC (WS-GRP-COUNT)
               ELSE
                   ADD 1                   TO WS-GRP-OVERFLOW
               END-IF
               RETURN SORT-FILE
                   AT END SET WS-END-OF-SORT TO TRUE
               END-RETURN
           END-PERFORM
           PERFORM 3100-END-GROUP THRU 3100-END-EXIT
           GO TO 3000-EXIT
           .
      *----------------------------------------------------------------*
       3100-END-GROUP.
      *----------------------------------------------------------------*
           IF WS-GRP-COUNT < 2
               GO TO 3100-END-EXIT
           END-IF
      *    NO MATCH ON AN EMPTY KEY (NO DOB / NO POSTCODE / NO NINO).
           IF WS-PASS-NAME
               IF WS-GRP-KEY (13:8) = '00000000'
               OR WS-GRP-KEY (21:8) = SPACES
               OR WS-GRP-KEY (1:12) = SPACES
                   GO TO 3100-END-EXIT
               END-IF
           ELSE
               IF WS-GRP-KEY (1:13) = SPACES
                   GO TO 3100-END-EXIT
               END-IF
           END-IF
           ADD 1                           TO WS-GROUPS
           IF WS-GRP-OVERFLOW > ZERO
               ADD 1                       TO WS-BIG-GROUPS
               MOVE WS-GRP-KEY             TO BG-KEY
               COMPUTE BG-SIZE = WS-GRP-COUNT + WS-GRP-OVERFLOW
               IF WS-LINE-COUNT >= 55
                   PERFORM 7000-REPORT-HEADINGS
               END-IF
               WRITE REPORT-RECORD         FROM WS-BIG-GROUP-LINE
               ADD 1                       TO WS-LINE-COUNT
               GO TO 3100-END-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I >= WS-GRP-COUNT
               COMPUTE WS-K = WS-I + 1
               PERFORM VARYING WS-J FROM WS-K BY 1
                       UNTIL WS-J > WS-GRP-COUNT
                   MOVE WS-GRP-REC (WS-I)  TO WS-A
                   MOVE WS-GRP-REC (WS-J)  TO WS-B
                   PERFORM 3200-SCORE-PAIR THRU 3200-EXIT
               END-PERFORM
           END-PERFORM
           .
       3100-END-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-SCORE-PAIR.
      *----------------------------------------------------------------*
           ADD 1                           TO WS-PAIRS-SCORED
           MOVE SPACES                     TO WS-REASONS
           IF WS-PASS-NINO
               MOVE 100                    TO WS-SCORE
               MOVE 'NINO'                 TO WS-REASONS
           ELSE
               MOVE 40                     TO WS-SCORE
               MOVE 'KEY'                  TO WS-REASONS
               EVALUATE TRUE
                   WHEN WA-FIRST-KEY = WB-FIRST-KEY
                   AND  WA-FIRST-KEY NOT = SPACES
                       ADD 30              TO WS-SCORE
                       STRING WS-REASONS DELIMITED BY SPACE
                              ' FIRST' DELIMITED BY SIZE
                              INTO WS-REASONS
                       END-STRING
                   WHEN WA-FIRST-KEY (1:1) = WB-FIRST-KEY (1:1)
                       ADD 20              TO WS-SCORE
                       STRING WS-REASONS DELIMITED BY SPACE
                              ' INIT' DELIMITED BY SIZE
                              INTO WS-REASONS
                       END-STRING
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               IF WA-ADDR-LINE1 = WB-ADDR-LINE1
               AND WA-ADDR-LINE1 NOT = SPACES
                   ADD 20                  TO WS-SCORE
                   MOVE WS-REASONS         TO WS-REASONS
                   STRING WS-REASONS DELIMITED BY '  '
                          ' ADDR' DELIMITED BY SIZE
                          INTO WS-REASONS
                   END-STRING
               END-IF
               IF (WA-MOBILE = WB-MOBILE AND WA-MOBILE NOT = SPACES)
               OR (WA-EMAIL = WB-EMAIL AND WA-EMAIL NOT = SPACES)
                   ADD 10                  TO WS-SCORE
                   STRING WS-REASONS DELIMITED BY '  '
                          ' CONTACT' DELIMITED BY SIZE
                          INTO WS-REASONS
                   END-STRING
               END-IF
           END-IF
           IF WS-SCORE > 100
               MOVE 100                    TO WS-SCORE
           END-IF
           IF WS-SCORE < WS-MIN-SCORE
               GO TO 3200-EXIT
           END-IF

           MOVE SPACES                     TO WS-CANDIDATE
           MOVE WS-PASS                    TO DC-PASS
           MOVE WS-SCORE                   TO DC-SCORE
           MOVE WS-REASONS                 TO DC-REASONS
      *    OLDER CIF FIRST - IT IS USUALLY THE ONE KEPT.
           IF WA-CREATE-DATE <= WB-CREATE-DATE
               MOVE WA-CIF-NO              TO DC-CIF-A
               MOVE WA-DISPLAY-NAME        TO DC-NAME-A
               MOVE WA-CREATE-DATE         TO DC-CREATED-A
               MOVE WB-CIF-NO              TO DC-CIF-B
               MOVE WB-DISPLAY-NAME        TO DC-NAME-B
               MOVE WB-CREATE-DATE         TO DC-CREATED-B
           ELSE
               MOVE WB-CIF-NO              TO DC-CIF-A
               MOVE WB-DISPLAY-NAME        TO DC-NAME-A
               MOVE WB-CREATE-DATE         TO DC-CREATED-A
               MOVE WA-CIF-NO              TO DC-CIF-B
               MOVE WA-DISPLAY-NAME        TO DC-NAME-B
               MOVE WA-CREATE-DATE         TO DC-CREATED-B
           END-IF
           MOVE WS-GRP-KEY                 TO DC-MATCH-KEY
           MOVE WS-BUS-DATE                TO DC-RUN-DATE
           WRITE DUPCAND-RECORD            FROM WS-CANDIDATE
           IF WS-DUPCAND-STATUS NOT = '00'
               MOVE '3200-SCORE-PAIR'      TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'DUPCAND WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-DUPCAND-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           IF WS-PASS-NINO
               ADD 1                       TO WS-NINO-CANDIDATES
           ELSE
               ADD 1                       TO WS-CANDIDATES
           END-IF
           .
       3200-EXIT.
           EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
       7000-REPORT-HEADINGS SECTION.
      *================================================================*
       7000-START.
           ADD 1                           TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT              TO HD1-PAGE
           WRITE REPORT-RECORD             FROM WS-HEADING-1
           MOVE 2                          TO WS-LINE-COUNT
           .
      *================================================================*
       9000-FINALISE SECTION.
      *================================================================*
       9000-START.
           PERFORM 7000-REPORT-HEADINGS
           MOVE 'CIF RECORDS READ'         TO TL-LABEL
           MOVE WS-CIF-READ                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  EXCLUDED (CLOSED/DECEASED/MERGED)' TO TL-LABEL
           MOVE WS-CIF-EXCLUDED            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SORTED ON NAME KEY'       TO TL-LABEL
           MOVE WS-RELEASED                TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'SORTED ON NINO'           TO TL-LABEL
           MOVE WS-NINO-RELEASED           TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'GROUPS SHARING A KEY'     TO TL-LABEL
           MOVE WS-GROUPS                  TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE '  LARGE GROUPS NOT PAIRED' TO TL-LABEL
           MOVE WS-BIG-GROUPS              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'PAIRS SCORED'             TO TL-LABEL
           MOVE WS-PAIRS-SCORED            TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CANDIDATES - NAME/DOB/POSTCODE' TO TL-LABEL
           MOVE WS-CANDIDATES              TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           MOVE 'CANDIDATES - SHARED NINO' TO TL-LABEL
           MOVE WS-NINO-CANDIDATES         TO TL-COUNT
           WRITE REPORT-RECORD             FROM WS-TOTAL-LINE
           CLOSE DUPCAND-FILE REPORT-FILE
           DISPLAY 'CFDUP010 - CANDIDATES ' WS-CANDIDATES
                   ' NINO ' WS-NINO-CANDIDATES
           .
      *
       Z999-ABEND-SECTION SECTION.
           COPY ABNDPROC.
