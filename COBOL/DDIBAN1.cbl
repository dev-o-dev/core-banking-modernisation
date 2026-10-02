       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDIBAN1.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2001-04-30.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDIBAN1                                             *
      * TITLE    : IBAN GENERATE / VALIDATE / SPLIT                    *
      * TYPE     : CALLED SUBPROGRAM - BATCH AND CICS (NO FILES)       *
      * INTERFACE: CALL 'DDIBAN1' USING IBN-PARM-BLOCK (COPY IBNPARM)  *
      *----------------------------------------------------------------*
      * ISO 13616 IBAN. CHECK DIGITS ARE CALCULATED BY:                *
      *   1. MOVING THE COUNTRY CODE AND CHECK DIGITS TO THE END       *
      *   2. REPLACING EACH LETTER BY TWO DIGITS (A=10 ... Z=35)       *
      *   3. TAKING THE RESULTING NUMBER MOD 97                        *
      * A VALID IBAN GIVES REMAINDER 1. TO GENERATE, THE CHECK DIGITS  *
      * ARE SET TO 00 AND THE CHECK DIGITS = 98 - REMAINDER.           *
      *                                                                *
      * THE NUMBER IN STEP 3 IS UP TO 68 DIGITS, SO THE MOD 97 IS      *
      * CALCULATED PIECEWISE - THE RUNNING REMAINDER IS CARRIED INTO   *
      * THE NEXT 7 DIGITS (KEEPS EVERYTHING WITHIN PIC 9(9)).          *
      *                                                                *
      * GB IBAN FORMAT:  GBkk MBKP ssss ssaa aaaa aa                   *
      *   BANK CODE 'MBKP' IS THE FIRST 4 CHARACTERS OF OUR BIC        *
      *   MBKPGB2L. ONLY MEGABANK SORT CODES ARE GENERATED; SPLIT (S)  *
      *   WORKS FOR ANY GB IBAN.                                       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2001-04-30 DKP  ORIGINAL VERSION - GENERATE ONLY (EU REG       *
      *                 2560/2001 - IBAN ON STATEMENTS)                *
      * 2007-11-05 AOS  VALIDATE + COUNTRY LENGTH TABLE (SEPA / SWIFT  *
      *                 PAYMENTS - PYINIT1)                            *
      * 2008-02-18 AOS  SPLIT FUNCTION FOR INBOUND SWIFT (PYINB010)    *
      * 2010-03-08 AOS  PRINT FORMAT (GROUPS OF 4) FOR STATEMENTS      *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDIBAN1 '.
      *
       01  WS-CONSTANTS.
           05  WS-OUR-BANK-CODE            PIC X(04) VALUE 'MBKP'.
      *
      *----------------------------------------------------------------*
      * IBAN LENGTH BY COUNTRY (SWIFT IBAN REGISTRY RELEASE 17, 2009)  *
      * 5 BYTES PER ENTRY: COUNTRY (2) + LENGTH (3)                    *
      *----------------------------------------------------------------*
       01  WS-COUNTRY-VALUES.
           05  FILLER  PIC X(50) VALUE
               'AD024AT020BA020BE016BG022CH021CY028CZ024DE022DK018'.
           05  FILLER  PIC X(50) VALUE
               'EE020ES024FI018FO018FR027GB022GI023GL018GR027HR021'.
           05  FILLER  PIC X(50) VALUE
               'HU028IE022IL023IS026IT027KW030LB028LI021LT020LU020'.
           05  FILLER  PIC X(50) VALUE
               'LV021MC027ME022MK019MR027MT031MU030NL018NO015PL028'.
           05  FILLER  PIC X(50) VALUE
               'PT025RO024RS022SA024SE024SI019SK024SM027TN024TR026'.
       01  WS-COUNTRY-TABLE REDEFINES WS-COUNTRY-VALUES.
           05  WS-CTRY-ENTRY               OCCURS 50 TIMES
                                           ASCENDING KEY WS-CTRY-CODE
                                           INDEXED BY CT-IDX.
               10  WS-CTRY-CODE            PIC X(02).
               10  WS-CTRY-LENGTH          PIC 9(03).
      *
      *---- LETTER VALUES A=10 .. Z=35 --------------------------------*
       01  WS-ALPHABET                     PIC X(26)
                                   VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
      *
       01  WS-WORK-FIELDS.
           05  WS-IBAN                     PIC X(34).
           05  WS-IBAN-CHARS REDEFINES WS-IBAN.
               10  WS-IBAN-CHAR            PIC X(01) OCCURS 34 TIMES.
           05  WS-IBAN-LEN                 PIC S9(04) COMP.
           05  WS-REARRANGED               PIC X(34).
           05  WS-REARR-CHARS REDEFINES WS-REARRANGED.
               10  WS-REARR-CHAR           PIC X(01) OCCURS 34 TIMES.
           05  WS-DIGITS                   PIC X(70).
           05  WS-DIGIT-CHARS REDEFINES WS-DIGITS.
               10  WS-DIGIT-CHAR           PIC X(01) OCCURS 70 TIMES.
           05  WS-DIGIT-LEN                PIC S9(04) COMP.
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-SUB2                     PIC S9(04) COMP.
           05  WS-LETTER-POS               PIC S9(04) COMP.
           05  WS-LETTER-VALUE             PIC 9(02).
           05  WS-CHUNK                    PIC 9(09).
           05  WS-CHUNK-X REDEFINES WS-CHUNK
                                           PIC X(09).
           05  WS-CHUNK-LEN                PIC S9(04) COMP.
           05  WS-CHUNK-POS                PIC S9(04) COMP.
           05  WS-REMAINDER                PIC 9(02).
           05  WS-REM-QUOT                 PIC 9(09).
           05  WS-CHECK                    PIC 9(02).
           05  WS-ONE-DIGIT                PIC 9(01).
           05  WS-BAD-CHAR-IND             PIC X(01).
               88  WS-BAD-CHAR                 VALUE 'Y'.
           05  WS-PRINT-POS                PIC S9(04) COMP.
           05  WS-TALLY                    PIC S9(04) COMP.
      *
       LINKAGE SECTION.
           COPY IBNPARM.
      *
       PROCEDURE DIVISION USING IBN-PARM-BLOCK.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE ZERO                       TO IBN-RETURN-CD

           EVALUATE TRUE
               WHEN IBN-GENERATE
                   PERFORM 1000-GENERATE THRU 1000-EXIT
               WHEN IBN-VALIDATE
                   PERFORM 2000-VALIDATE THRU 2000-EXIT
               WHEN IBN-SPLIT
                   PERFORM 3000-SPLIT THRU 3000-EXIT
               WHEN OTHER
                   MOVE 16                 TO IBN-RETURN-CD
           END-EVALUATE

           IF IBN-RETURN-CD = ZERO OR IBN-RETURN-CD = 4
               PERFORM 4000-PRINT-FORMAT
           END-IF

           GOBACK
           .
      *================================================================*
       1000-GENERATE.
      *================================================================*
           IF IBN-SORT-CODE NOT NUMERIC
           OR IBN-ACCT-NO NOT NUMERIC
               MOVE 12                     TO IBN-RETURN-CD
               GO TO 1000-EXIT
           END-IF

           MOVE SPACES                     TO WS-IBAN
           STRING 'GB00' WS-OUR-BANK-CODE IBN-SORT-CODE IBN-ACCT-NO
                  DELIMITED BY SIZE
                  INTO WS-IBAN
           END-STRING
           MOVE 22                         TO WS-IBAN-LEN

           PERFORM 5000-MOD-97 THRU 5000-EXIT
           IF WS-BAD-CHAR
               MOVE 12                     TO IBN-RETURN-CD
               GO TO 1000-EXIT
           END-IF

           COMPUTE WS-CHECK = 98 - WS-REMAINDER
           MOVE WS-CHECK                   TO WS-IBAN (3:2)
           MOVE WS-IBAN                    TO IBN-IBAN
           MOVE 'GB'                       TO IBN-COUNTRY-CD
           MOVE WS-OUR-BANK-CODE           TO IBN-BANK-CODE
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-VALIDATE.
      *================================================================*
      *    UPPER-CASE AND REMOVE EMBEDDED SPACES (CHANNELS SOMETIMES
      *    PASS THE PRINT FORMAT).
           MOVE FUNCTION UPPER-CASE (IBN-IBAN) TO WS-REARRANGED
           MOVE SPACES                     TO WS-IBAN
           MOVE ZERO                       TO WS-IBAN-LEN
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 34
               IF WS-REARR-CHAR (WS-SUB) NOT = SPACE
                   ADD 1                   TO WS-IBAN-LEN
                   MOVE WS-REARR-CHAR (WS-SUB)
                                     TO WS-IBAN-CHAR (WS-IBAN-LEN)
               END-IF
           END-PERFORM

           IF WS-IBAN-LEN < 15
               MOVE 10                     TO IBN-RETURN-CD
               GO TO 2000-EXIT
           END-IF

           IF WS-IBAN (1:2) NOT ALPHABETIC
           OR WS-IBAN (3:2) NOT NUMERIC
               MOVE 12                     TO IBN-RETURN-CD
               GO TO 2000-EXIT
           END-IF

           MOVE WS-IBAN (1:2)              TO IBN-COUNTRY-CD
           MOVE WS-IBAN                    TO IBN-IBAN

      *---- LENGTH FOR COUNTRY ----------------------------------------*
           SEARCH ALL WS-CTRY-ENTRY
               AT END
                   MOVE 04                 TO IBN-RETURN-CD
               WHEN WS-CTRY-CODE (CT-IDX) = IBN-COUNTRY-CD
                   IF WS-CTRY-LENGTH (CT-IDX) NOT = WS-IBAN-LEN
                       MOVE 10             TO IBN-RETURN-CD
                       GO TO 2000-EXIT
                   END-IF
           END-SEARCH

      *---- CHECK DIGITS ----------------------------------------------*
           PERFORM 5000-MOD-97 THRU 5000-EXIT
           EVALUATE TRUE
               WHEN WS-BAD-CHAR
                   MOVE 12                 TO IBN-RETURN-CD
               WHEN WS-REMAINDER NOT = 1
                   MOVE 08                 TO IBN-RETURN-CD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF IBN-COUNTRY-CD = 'GB'
               MOVE WS-IBAN (5:4)          TO IBN-BANK-CODE
           END-IF
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-SPLIT.
      *================================================================*
           PERFORM 2000-VALIDATE THRU 2000-EXIT
           IF IBN-RETURN-CD NOT = ZERO
               GO TO 3000-EXIT
           END-IF
           IF IBN-COUNTRY-CD NOT = 'GB'
               MOVE 14                     TO IBN-RETURN-CD
               GO TO 3000-EXIT
           END-IF
      *    GB: CC KK BBBB SSSSSS AAAAAAAA
           MOVE WS-IBAN (5:4)              TO IBN-BANK-CODE
           MOVE WS-IBAN (9:6)              TO IBN-SORT-CODE
           MOVE WS-IBAN (15:8)             TO IBN-ACCT-NO
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-PRINT-FORMAT.
      *================================================================*
      *    GROUPS OF FOUR SEPARATED BY ONE SPACE - "GB29 MBKP 4012 ..."
           MOVE SPACES                     TO IBN-IBAN-PRINT
           MOVE ZERO                       TO WS-PRINT-POS
           MOVE ZERO                       TO WS-TALLY
           INSPECT IBN-IBAN TALLYING WS-TALLY
                   FOR CHARACTERS BEFORE INITIAL SPACE
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-TALLY
               IF WS-SUB > 1
               AND FUNCTION MOD (WS-SUB - 1, 4) = 0
                   ADD 1                   TO WS-PRINT-POS
               END-IF
               ADD 1                       TO WS-PRINT-POS
               IF WS-PRINT-POS <= 42
                   MOVE IBN-IBAN (WS-SUB:1)
                                 TO IBN-IBAN-PRINT (WS-PRINT-POS:1)
               END-IF
           END-PERFORM
           .
      *================================================================*
       5000-MOD-97.
      *================================================================*
      *    ON ENTRY WS-IBAN / WS-IBAN-LEN. RETURNS WS-REMAINDER.
      *---- 1. REARRANGE: CHARACTERS 5..N THEN 1..4 -------------------*
           MOVE SPACES                     TO WS-REARRANGED
           MOVE 'N'                        TO WS-BAD-CHAR-IND
           MOVE WS-IBAN (5:WS-IBAN-LEN - 4) TO WS-REARRANGED
           MOVE WS-IBAN (1:4)
                   TO WS-REARRANGED (WS-IBAN-LEN - 3:4)

      *---- 2. LETTERS TO DIGITS --------------------------------------*
           MOVE SPACES                     TO WS-DIGITS
           MOVE ZERO                       TO WS-DIGIT-LEN
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-IBAN-LEN
               EVALUATE TRUE
                   WHEN WS-REARR-CHAR (WS-SUB) IS NUMERIC
                       ADD 1               TO WS-DIGIT-LEN
                       MOVE WS-REARR-CHAR (WS-SUB)
                                     TO WS-DIGIT-CHAR (WS-DIGIT-LEN)
                   WHEN WS-REARR-CHAR (WS-SUB) IS ALPHABETIC-UPPER
                   AND  WS-REARR-CHAR (WS-SUB) NOT = SPACE
                       MOVE ZERO           TO WS-LETTER-POS
                       PERFORM VARYING WS-SUB2 FROM 1 BY 1
                               UNTIL WS-SUB2 > 26
                                  OR WS-LETTER-POS > ZERO
                           IF WS-ALPHABET (WS-SUB2:1)
                              = WS-REARR-CHAR (WS-SUB)
                               MOVE WS-SUB2 TO WS-LETTER-POS
                           END-IF
                       END-PERFORM
                       COMPUTE WS-LETTER-VALUE = WS-LETTER-POS + 9
                       ADD 1               TO WS-DIGIT-LEN
                       MOVE WS-LETTER-VALUE (1:1)
                                     TO WS-DIGIT-CHAR (WS-DIGIT-LEN)
                       ADD 1               TO WS-DIGIT-LEN
                       MOVE WS-LETTER-VALUE (2:1)
                                     TO WS-DIGIT-CHAR (WS-DIGIT-LEN)
                   WHEN OTHER
                       SET WS-BAD-CHAR     TO TRUE
               END-EVALUATE
           END-PERFORM

           IF WS-BAD-CHAR
               GO TO 5000-EXIT
           END-IF

      *---- 3. PIECEWISE MOD 97 ---------------------------------------*
      *    TAKE THE REMAINDER SO FAR (2 DIGITS) + UP TO 7 NEW DIGITS.
           MOVE ZERO                       TO WS-REMAINDER
           MOVE 1                          TO WS-CHUNK-POS
           PERFORM UNTIL WS-CHUNK-POS > WS-DIGIT-LEN
               COMPUTE WS-CHUNK-LEN = WS-DIGIT-LEN - WS-CHUNK-POS + 1
               IF WS-CHUNK-LEN > 7
                   MOVE 7                  TO WS-CHUNK-LEN
               END-IF
               MOVE WS-REMAINDER           TO WS-CHUNK
               PERFORM VARYING WS-SUB FROM 0 BY 1
                       UNTIL WS-SUB >= WS-CHUNK-LEN
                   MOVE WS-DIGIT-CHAR (WS-CHUNK-POS + WS-SUB)
                                           TO WS-ONE-DIGIT
                   COMPUTE WS-CHUNK = WS-CHUNK * 10 + WS-ONE-DIGIT
               END-PERFORM
               DIVIDE WS-CHUNK BY 97 GIVING WS-REM-QUOT
                      REMAINDER WS-REMAINDER
               ADD WS-CHUNK-LEN            TO WS-CHUNK-POS
           END-PERFORM
           .
       5000-EXIT.
           EXIT.
