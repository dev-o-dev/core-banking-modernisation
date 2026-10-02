       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFMNT201.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2007-02-26.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFMNT201                                            *
      * TITLE    : CUSTMNT2 - APPLY CHANNEL CUSTOMER MAINTENANCE TO    *
      *            THE CIF                                             *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : CUSTMNT2 STEP CM020                                 *
      *----------------------------------------------------------------*
      * INPUT    : MAINTIN  - CUSTMNT2 FILE FROM IBANK, SORTED BY CIF  *
      *                       + CHANNEL TIMESTAMP (STEP CM010)         *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * UPDATE   : CIFMAST  - CIF MASTER (VSAM KSDS, RANDOM I-O)       *
      * OUTPUT   : AUDITOUT - BEFORE/AFTER IMAGES (GROUP AUDIT GRM-004,*
      *                       RETAINED 7 YEARS ON TAPE)                *
      *            REJECTS  - REJECTED CHANGES, BACK TO IBANK SUPPORT  *
      *----------------------------------------------------------------*
      * THE CHANNEL HAS ALREADY UPDATED ITS OWN SHADOW (CUST_MASTER).  *
      * A REJECTION HERE LEAVES THE TWO OUT OF STEP UNTIL IBANK        *
      * SUPPORT CORRECTS ONE OR THE OTHER BY HAND.                     *
      *                                                                *
      * REJECT REASONS                                                 *
      *   CNFD CIF NOT FOUND           CSTS CIF NOT ACTIVE             *
      *   STAL BRANCH CHANGED THE CIF AFTER THE CHANNEL CHANGE         *
      *   ADDR ADDRESS INCOMPLETE      PCDE POSTCODE FORMAT INVALID    *
      *   CONT NO CONTACT DETAILS      EMAL EMAIL ADDRESS INVALID      *
      *   INCM INCOME INVALID          MKTG INDICATOR NOT Y/N          *
      *   TYPE UNKNOWN MAINTENANCE TYPE                                *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2005-01-10 AOS  ORIGINAL VERSION (CFMNT101 - ADDRESS ONLY)     *
      * 2007-02-26 JNB  CUSTMNT2 - CONTACT/EMPLOYMENT/MARKETING        *
      * 2008-06-16 JNB  STALE CHECK AGAINST BRANCH CHANGES             *
      *                 (COMPLAINT C/2008/1142 - ADDRESS REVERTED)     *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAINTIN-FILE     ASSIGN TO MAINTIN
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-MAINTIN-STATUS.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS RANDOM
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT AUDITOUT-FILE    ASSIGN TO AUDITOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-AUDITOUT-STATUS.
           SELECT REJECTS-FILE     ASSIGN TO REJECTS
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-REJECTS-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  MAINTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  MAINTIN-RECORD                  PIC X(400).
      *
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
      *
       FD  AUDITOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  AUDITOUT-RECORD                 PIC X(1500).
      *
       FD  REJECTS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REJECTS-RECORD                  PIC X(404).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFMNT201'.
      *
       01  WS-FILE-STATUSES.
           05  WS-MAINTIN-STATUS           PIC X(02).
               88  WS-MAINTIN-OK               VALUE '00'.
               88  WS-MAINTIN-EOF              VALUE '10'.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-NOTFND           VALUE '23'.
           05  WS-AUDITOUT-STATUS          PIC X(02).
           05  WS-REJECTS-STATUS           PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-IND                  PIC X(01) VALUE 'N'.
               88  WS-END-OF-INPUT             VALUE 'Y'.
           05  WS-HEADER-SEEN-IND          PIC X(01) VALUE 'N'.
               88  WS-HEADER-SEEN              VALUE 'Y'.
           05  WS-TRAILER-SEEN-IND         PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-REJECT-RSN               PIC X(04).
           05  WS-BEFORE-IMAGE             PIC X(700).
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURR-DATE            PIC 9(08).
               10  WS-CURR-TIME            PIC 9(06).
               10  FILLER                  PIC X(07).
      *        CHANNEL TIMESTAMP YYYY-MM-DD-HH.MM.SS.NNNNNN AS NUMBERS
           05  WS-CHANNEL-TS-PARTS.
               10  WS-CTS-CCYY             PIC X(04).
               10  FILLER                  PIC X(01).
               10  WS-CTS-MM               PIC X(02).
               10  FILLER                  PIC X(01).
               10  WS-CTS-DD               PIC X(02).
               10  FILLER                  PIC X(01).
               10  WS-CTS-HH               PIC X(02).
               10  FILLER                  PIC X(01).
               10  WS-CTS-MI               PIC X(02).
               10  FILLER                  PIC X(01).
               10  WS-CTS-SS               PIC X(02).
               10  FILLER                  PIC X(07).
           05  WS-CHANNEL-DATETIME.
               10  WS-CDT-DATE             PIC X(08).
               10  WS-CDT-TIME             PIC X(06).
           05  WS-CIF-DATETIME.
               10  WS-CIF-DT-DATE          PIC 9(08).
               10  WS-CIF-DT-TIME          PIC 9(06).
           05  WS-AT-COUNT                 PIC S9(04) COMP.
           05  WS-SPACE-COUNT              PIC S9(04) COMP.
           05  WS-PCODE-LEN                PIC S9(04) COMP.
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-APPLIED-COUNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-REJECT-COUNT             PIC S9(09) COMP-3 VALUE 0.
      *
       01  WS-AUDIT-RECORD.
           05  AUD-CIF-NO                  PIC X(12).
           05  AUD-DATE                    PIC 9(08).
           05  AUD-TIME                    PIC 9(06).
           05  AUD-SOURCE                  PIC X(04).
           05  AUD-USER                    PIC X(20).
           05  AUD-MAINT-TYPE              PIC X(01).
           05  AUD-BEFORE-IMAGE            PIC X(700).
           05  AUD-AFTER-IMAGE             PIC X(700).
           05  FILLER                      PIC X(49).
      *
       01  WS-REJECT-RECORD.
           05  REJ-REASON                  PIC X(04).
           05  REJ-MAINT-RECORD            PIC X(400).
      *
           COPY CUSTMNRC.
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
           PERFORM 2000-PROCESS-RECORD
               UNTIL WS-END-OF-INPUT
           PERFORM 3000-FINALISE

           GOBACK
           .
      *================================================================*
       1000-INITIALISE.
      *================================================================*
           OPEN INPUT  MAINTIN-FILE
                       DATECARD-FILE
                I-O    CIFMAST-FILE
                OUTPUT AUDITOUT-FILE
                       REJECTS-FILE

           IF NOT WS-MAINTIN-OK
           OR NOT WS-CIFMAST-OK
           OR WS-AUDITOUT-STATUS NOT = '00'
           OR WS-REJECTS-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               STRING WS-MAINTIN-STATUS ' ' WS-CIFMAST-STATUS ' '
                      WS-AUDITOUT-STATUS ' ' WS-REJECTS-STATUS ' '
                      WS-DATECARD-STATUS
                      DELIMITED BY SIZE
                      INTO ABN-KEY-DATA
               END-STRING
               PERFORM Z999-ABEND
           END-IF

           READ DATECARD-FILE INTO DTC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DTC-VALID-CARD
               MOVE '1000-INITIALISE'      TO ABN-PARAGRAPH
               MOVE 1005                   TO ABN-ABEND-CODE
               MOVE 'INVALID OR MISSING DATE CARD'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF
           CLOSE DATECARD-FILE

           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA

           PERFORM 8000-READ-MAINTIN
           .
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           EVALUATE TRUE
               WHEN CMN-HEADER-REC
                   SET WS-HEADER-SEEN      TO TRUE
                   DISPLAY 'CFMNT201 - FILE ' CMNH-FILE-ID
                           ' EXTRACTED ' CMNH-EXTRACT-DATE
                           ' ' CMNH-EXTRACT-TIME
                           ' FROM ' CMNH-SOURCE-SYS
               WHEN CMN-DETAIL-REC
                   IF NOT WS-HEADER-SEEN
                       MOVE '2000-PROCESS-RECORD'
                                           TO ABN-PARAGRAPH
                       MOVE 1007           TO ABN-ABEND-CODE
                       MOVE 'DETAIL BEFORE HEADER - FILE NOT SORTED?'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
                   ADD 1                   TO WS-READ-COUNT
                   PERFORM 2100-PROCESS-DETAIL
               WHEN CMN-TRAILER-REC
                   SET WS-TRAILER-SEEN     TO TRUE
                   IF CMNT-REC-COUNT NOT = WS-READ-COUNT
                       MOVE '2000-PROCESS-RECORD'
                                           TO ABN-PARAGRAPH
                       MOVE 1004           TO ABN-ABEND-CODE
                       MOVE 'TRAILER COUNT NOT = DETAIL RECORDS READ'
                                           TO ABN-MESSAGE
                       PERFORM Z999-ABEND
                   END-IF
               WHEN OTHER
                   MOVE '2000-PROCESS-RECORD'
                                           TO ABN-PARAGRAPH
                   MOVE 1007               TO ABN-ABEND-CODE
                   MOVE 'UNKNOWN RECORD TYPE'
                                           TO ABN-MESSAGE
                   MOVE CMN-MAINT-RECORD (1:40)
                                           TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE

           PERFORM 8000-READ-MAINTIN
           .
      *================================================================*
       2100-PROCESS-DETAIL.
      *================================================================*
           MOVE SPACES                     TO WS-REJECT-RSN
           MOVE CMND-CIF-NO                TO CIF-NO

           READ CIFMAST-FILE
           EVALUATE TRUE
               WHEN WS-CIFMAST-OK
                   CONTINUE
               WHEN WS-CIFMAST-NOTFND
                   MOVE 'CNFD'             TO WS-REJECT-RSN
               WHEN OTHER
                   MOVE '2100-PROCESS-DETAIL'
                                           TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   MOVE CMND-CIF-NO        TO ABN-KEY-DATA
                   PERFORM Z999-ABEND
           END-EVALUATE

           IF WS-REJECT-RSN = SPACES AND NOT CIF-ACTIVE
               MOVE 'CSTS'                 TO WS-REJECT-RSN
           END-IF

           IF WS-REJECT-RSN = SPACES
               PERFORM 2200-CHECK-STALE
           END-IF

           IF WS-REJECT-RSN = SPACES
               MOVE CIF-CUSTOMER-RECORD    TO WS-BEFORE-IMAGE
               EVALUATE TRUE
                   WHEN CMND-ADDRESS-CHG
                       PERFORM 2310-APPLY-ADDRESS
                   WHEN CMND-CONTACT-CHG
                       PERFORM 2320-APPLY-CONTACT
                   WHEN CMND-EMPLOYMENT-CHG
                       PERFORM 2330-APPLY-EMPLOYMENT
                   WHEN CMND-MARKETING-CHG
                       PERFORM 2340-APPLY-MARKETING
                   WHEN OTHER
                       MOVE 'TYPE'         TO WS-REJECT-RSN
               END-EVALUATE
           END-IF

           IF WS-REJECT-RSN = SPACES
               PERFORM 2400-REWRITE-CIF
           ELSE
               PERFORM 2500-WRITE-REJECT
           END-IF
           .
      *================================================================*
       2200-CHECK-STALE.
      *================================================================*
      *    IF THE BRANCH (OR TELEPHONY) CHANGED THE CUSTOMER AFTER THE
      *    CUSTOMER MADE THIS CHANGE ONLINE, THE BRANCH CHANGE WINS.
           MOVE CMND-CHANNEL-TS            TO WS-CHANNEL-TS-PARTS
           STRING WS-CTS-CCYY WS-CTS-MM WS-CTS-DD
                  DELIMITED BY SIZE
                  INTO WS-CDT-DATE
           END-STRING
           STRING WS-CTS-HH WS-CTS-MI WS-CTS-SS
                  DELIMITED BY SIZE
                  INTO WS-CDT-TIME
           END-STRING
           MOVE CIF-LAST-MAINT-DATE        TO WS-CIF-DT-DATE
           MOVE CIF-LAST-MAINT-TIME        TO WS-CIF-DT-TIME

           IF (CIF-MAINT-BRANCH OR CIF-MAINT-TELE)
           AND WS-CIF-DATETIME > WS-CHANNEL-DATETIME
               MOVE 'STAL'                 TO WS-REJECT-RSN
           END-IF
           .
      *================================================================*
       2310-APPLY-ADDRESS.
      *================================================================*
           EVALUATE TRUE
               WHEN CMND-ADDR-LINE1 = SPACES
               WHEN CMND-TOWN = SPACES
               WHEN CMND-POST-CD = SPACES
                   MOVE 'ADDR'             TO WS-REJECT-RSN
               WHEN OTHER
                   PERFORM 2311-VALIDATE-POSTCODE
           END-EVALUATE

           IF WS-REJECT-RSN = SPACES
               MOVE CMND-ADDR-LINE1        TO CIF-ADDR-LINE1
               MOVE CMND-ADDR-LINE2        TO CIF-ADDR-LINE2
               MOVE CMND-TOWN              TO CIF-TOWN
               MOVE CMND-COUNTY            TO CIF-COUNTY
               MOVE FUNCTION UPPER-CASE (CMND-POST-CD)
                                           TO CIF-POST-CD
               IF CMND-CTRY-CD = SPACES
                   MOVE 'GB'               TO CIF-CTRY-CD
               ELSE
                   MOVE CMND-CTRY-CD       TO CIF-CTRY-CD
               END-IF
           END-IF
           .
      *================================================================*
       2311-VALIDATE-POSTCODE.
      *================================================================*
      *    UK POSTCODES ONLY: 6 TO 8 CHARACTERS INCLUDING ONE SPACE,
      *    E.G. 'LS6 3AB', 'SW1A 1AA'. NO FULL PAF VALIDATION - PAF
      *    LOOKUP IS DONE BY THE CHANNEL.
           IF CMND-CTRY-CD NOT = SPACES AND 'GB'
               CONTINUE
           ELSE
               MOVE ZERO                   TO WS-SPACE-COUNT
                                              WS-PCODE-LEN
               INSPECT CMND-POST-CD TALLYING WS-PCODE-LEN
                       FOR CHARACTERS BEFORE INITIAL '  '
               IF WS-PCODE-LEN < 6 OR WS-PCODE-LEN > 8
                   MOVE 'PCDE'             TO WS-REJECT-RSN
               ELSE
                   INSPECT CMND-POST-CD (1:WS-PCODE-LEN)
                           TALLYING WS-SPACE-COUNT FOR ALL SPACE
                   IF WS-SPACE-COUNT NOT = 1
                       MOVE 'PCDE'         TO WS-REJECT-RSN
                   END-IF
               END-IF
           END-IF
           .
      *================================================================*
       2320-APPLY-CONTACT.
      *================================================================*
           IF CMND-HOME-TEL = SPACES
           AND CMND-MOBILE-TEL = SPACES
           AND CMND-EMAIL-ADDR = SPACES
               MOVE 'CONT'                 TO WS-REJECT-RSN
           END-IF

           IF WS-REJECT-RSN = SPACES
           AND CMND-EMAIL-ADDR NOT = SPACES
               MOVE ZERO                   TO WS-AT-COUNT
               INSPECT CMND-EMAIL-ADDR TALLYING WS-AT-COUNT
                       FOR ALL '@'
               IF WS-AT-COUNT NOT = 1
                   MOVE 'EMAL'             TO WS-REJECT-RSN
               END-IF
           END-IF

           IF WS-REJECT-RSN = SPACES
               MOVE CMND-HOME-TEL          TO CIF-HOME-TEL
               MOVE CMND-MOBILE-TEL        TO CIF-MOBILE-TEL
               MOVE CMND-EMAIL-ADDR        TO CIF-EMAIL-ADDR
           END-IF
           .
      *================================================================*
       2330-APPLY-EMPLOYMENT.
      *================================================================*
           IF CMND-ANN-INCOME NOT NUMERIC
           OR CMND-ANN-INCOME < ZERO
               MOVE 'INCM'                 TO WS-REJECT-RSN
           ELSE
               MOVE CMND-EMPL-STATUS       TO CIF-EMPL-STATUS
               MOVE CMND-OCCUPATION        TO CIF-OCCUPATION
               MOVE CMND-ANN-INCOME        TO CIF-ANN-INCOME
           END-IF
           .
      *================================================================*
       2340-APPLY-MARKETING.
      *================================================================*
           IF CMND-MKTG-EMAIL-IND NOT = 'Y' AND 'N'
           OR CMND-MKTG-POST-IND  NOT = 'Y' AND 'N'
               MOVE 'MKTG'                 TO WS-REJECT-RSN
           ELSE
               MOVE CMND-MKTG-EMAIL-IND    TO CIF-MKTG-EMAIL-IND
               MOVE CMND-MKTG-POST-IND     TO CIF-MKTG-POST-IND
           END-IF
           .
      *================================================================*
       2400-REWRITE-CIF.
      *================================================================*
           MOVE WS-CURR-DATE               TO CIF-LAST-MAINT-DATE
           MOVE WS-CURR-TIME               TO CIF-LAST-MAINT-TIME
           MOVE 'CUSTMNT2'                 TO CIF-LAST-MAINT-USER
           SET CIF-MAINT-IBANK             TO TRUE

           REWRITE CIF-CUSTOMER-RECORD
           IF NOT WS-CIFMAST-OK
               MOVE '2400-REWRITE-CIF'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'CIFMAST REWRITE FAILED'
                                           TO ABN-MESSAGE
               MOVE WS-CIFMAST-STATUS      TO ABN-FILE-STATUS
               MOVE CIF-NO                 TO ABN-KEY-DATA
               PERFORM Z999-ABEND
           END-IF

           MOVE SPACES                     TO WS-AUDIT-RECORD
           MOVE CIF-NO                     TO AUD-CIF-NO
           MOVE WS-CURR-DATE               TO AUD-DATE
           MOVE WS-CURR-TIME               TO AUD-TIME
           MOVE 'IBNK'                     TO AUD-SOURCE
           MOVE CMND-LOGON-ID              TO AUD-USER
           MOVE CMND-MAINT-TYPE            TO AUD-MAINT-TYPE
           MOVE WS-BEFORE-IMAGE            TO AUD-BEFORE-IMAGE
           MOVE CIF-CUSTOMER-RECORD        TO AUD-AFTER-IMAGE
           WRITE AUDITOUT-RECORD           FROM WS-AUDIT-RECORD
           IF WS-AUDITOUT-STATUS NOT = '00'
               MOVE '2400-REWRITE-CIF'     TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'AUDIT WRITE FAILED'   TO ABN-MESSAGE
               MOVE WS-AUDITOUT-STATUS     TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF

           ADD 1                           TO WS-APPLIED-COUNT
           .
      *================================================================*
       2500-WRITE-REJECT.
      *================================================================*
           MOVE WS-REJECT-RSN              TO REJ-REASON
           MOVE CMN-MAINT-RECORD           TO REJ-MAINT-RECORD
           WRITE REJECTS-RECORD            FROM WS-REJECT-RECORD
           IF WS-REJECTS-STATUS NOT = '00'
               MOVE '2500-WRITE-REJECT'    TO ABN-PARAGRAPH
               MOVE 1002                   TO ABN-ABEND-CODE
               MOVE 'REJECTS WRITE FAILED' TO ABN-MESSAGE
               MOVE WS-REJECTS-STATUS      TO ABN-FILE-STATUS
               PERFORM Z999-ABEND
           END-IF
           ADD 1                           TO WS-REJECT-COUNT
           .
      *================================================================*
       3000-FINALISE.
      *================================================================*
           IF NOT WS-HEADER-SEEN OR NOT WS-TRAILER-SEEN
               MOVE '3000-FINALISE'        TO ABN-PARAGRAPH
               MOVE 1007                   TO ABN-ABEND-CODE
               MOVE 'CUSTMNT2 FILE HEADER OR TRAILER MISSING'
                                           TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           CLOSE MAINTIN-FILE
                 CIFMAST-FILE
                 AUDITOUT-FILE
                 REJECTS-FILE

           DISPLAY 'CFMNT201 - CHANGES READ      ' WS-READ-COUNT
           DISPLAY 'CFMNT201 - CHANGES APPLIED   ' WS-APPLIED-COUNT
           DISPLAY 'CFMNT201 - CHANGES REJECTED  ' WS-REJECT-COUNT

           IF WS-REJECT-COUNT > ZERO
               MOVE 4                      TO RETURN-CODE
           END-IF
           .
      *================================================================*
       8000-READ-MAINTIN.
      *================================================================*
           READ MAINTIN-FILE INTO CMN-MAINT-RECORD
           EVALUATE TRUE
               WHEN WS-MAINTIN-OK
                   CONTINUE
               WHEN WS-MAINTIN-EOF
                   SET WS-END-OF-INPUT     TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-MAINTIN' TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'MAINTIN READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-MAINTIN-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
           END-EVALUATE
           .
      *
           COPY ABNDPROC.
