       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDMODCK.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2001-03-05.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDMODCK                                             *
      * TITLE    : SORT CODE / ACCOUNT NUMBER MODULUS CHECK            *
      * TYPE     : CALLED SUBPROGRAM - BATCH AND CICS (NO EXEC CICS,   *
      *            NO FILES - SAFE TO LINK-EDIT INTO CICS PROGRAMS)    *
      * INTERFACE: CALL 'DDMODCK' USING MOD-PARM-BLOCK (COPY MODPARM)  *
      *----------------------------------------------------------------*
      * IMPLEMENTS THE VOCALINK (FORMERLY BACS) MODULUS CHECKING       *
      * ALGORITHMS:                                                    *
      *   MOD10  STANDARD MODULUS 10                                   *
      *   MOD11  STANDARD MODULUS 11                                   *
      *   DBLAL  DOUBLE ALTERNATE (SUM OF DIGITS OF PRODUCTS, MOD 10)  *
      * THE 14-DIGIT NUMBER IS THE 6-DIGIT SORT CODE FOLLOWED BY THE   *
      * 8-DIGIT ACCOUNT NUMBER (U V W X Y Z A B C D E F G H).          *
      *                                                                *
      * THE WEIGHT TABLE IS A SUBSET OF THE VOCALINK VALACDOS FILE     *
      * COVERING THE SORT CODE RANGES OF THE BANKS MEGABANK CUSTOMERS  *
      * PAY MOST OFTEN, PLUS ALL MEGABANK RANGES. IT IS CODED IN       *
      * WORKING STORAGE (WS-WEIGHT-VALUES) AND MUST BE UPDATED BY HAND *
      * WHEN VOCALINK PUBLISH A NEW VALACDOS (QUARTERLY). THE FULL     *
      * FILE IS NOT LOADED BECAUSE THIS MODULE IS CALLED FROM CICS     *
      * WITHOUT FILE ACCESS. PIR 2004/077 "LOAD VALACDOS AS A TABLE    *
      * FROM A DATA SET" - NOT FUNDED.                                 *
      *                                                                *
      * A SORT CODE NOT IN THE TABLE CANNOT BE CHECKED AND RETURNS 04  *
      * (VALID - NOT CHECKED). VOCALINK RULES SAY TO ACCEPT IT.        *
      *                                                                *
      * WHERE A SORT CODE HAS TWO TABLE ENTRIES BOTH CHECKS MUST PASS, *
      * EXCEPT FOR EXCEPTION 2/9 AND 10/11 WHERE EITHER MAY PASS (THE  *
      * EXCEPTION RULES BELOW IMPLEMENT ONLY THE SUBSET SEEN IN OUR    *
      * TABLE - EXCEPTIONS 1, 4, 5 (PARTIAL), 6 AND 7).                *
      *                                                                *
      * GENERATE (FUNCTION G) IS FOR MEGABANK ACCOUNT NUMBER           *
      * ALLOCATION ONLY. MEGABANK RANGES USE MOD11 WITH THE CHECK      *
      * DIGIT IN POSITION H. A REMAINDER GIVING CHECK DIGIT 10 CANNOT  *
      * BE USED - THE CALLER MUST TRY THE NEXT NUMBER (RC 20).         *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2001-03-05 DKP  ORIGINAL VERSION (IBANK PAYEE SET-UP)          *
      * 2002-09-02 DKP  VALACDOS UPDATE V1.40                          *
      * 2004-06-21 SAT  EXCEPTIONS 1, 4, 6, 7 (VOCALINK SPEC V2.10)    *
      * 2006-02-13 AOS  VALACDOS UPDATE V2.60                          *
      * 2008-03-10 AOS  VALACDOS UPDATE V3.20 (FPS GO-LIVE)            *
      * 2010-03-08 AOS  GENERATE FUNCTION FOR DDOPN01                  *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDMODCK '.
      *
      *----------------------------------------------------------------*
      * WEIGHT TABLE - ONE ENTRY = 64 BYTES                            *
      *   SORT CODE FROM (6), TO (6), METHOD (5), 14 WEIGHTS OF 2      *
      *   DIGITS (U V W X Y Z A B C D E F G H), EXCEPTION (2), SPARE   *
      *   (17). VALACDOS HAS SOME NEGATIVE WEIGHTS - NONE OCCUR IN OUR *
      *   SUBSET, SO WEIGHTS ARE HELD UNSIGNED.                        *
      * ENTRIES MUST BE IN ASCENDING SORT CODE ORDER.                  *
      *----------------------------------------------------------------*
       01  WS-WEIGHT-VALUES.
      *    010004-016715 MOD10 EXC 0
           05  FILLER PIC X(17) VALUE '010004016715MOD10'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000201020102010201'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    070116-070116 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '070116070116MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000706050403020100'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    070246-070246 MOD10 EXC 0
           05  FILLER PIC X(17) VALUE '070246070246MOD10'.
           05  FILLER PIC X(28) VALUE
                                 '0201020102010304030403040304'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    086086-086086 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '086086086086MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000705080304060201'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    089000-089999 MOD10 EXC 0
           05  FILLER PIC X(17) VALUE '089000089999MOD10'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000007010307010301'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    090128-090128 MOD10 EXC 0
           05  FILLER PIC X(17) VALUE '090128090128MOD10'.
           05  FILLER PIC X(28) VALUE
                                 '0307010307010307010307010301'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    100000-101099 DBLAL EXC 1
           05  FILLER PIC X(17) VALUE '100000101099DBLAL'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000201020102010201'.
           05  FILLER PIC X(02) VALUE ' 1'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    101101-101498 DBLAL EXC 1
           05  FILLER PIC X(17) VALUE '101101101498DBLAL'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000201020102010201'.
           05  FILLER PIC X(02) VALUE ' 1'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    110000-119280 DBLAL EXC 1
           05  FILLER PIC X(17) VALUE '110000119280DBLAL'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000201020102010201'.
           05  FILLER PIC X(02) VALUE ' 1'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    120000-120961 MOD10 EXC 0
           05  FILLER PIC X(17) VALUE '120000120961MOD10'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000000020102010201'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    150000-159999 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '150000159999MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000908070605040302'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    200000-200002 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '200000200002MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000605040302070605'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    200004-200004 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '200004200004MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000605040302070605'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    200026-200026 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '200026200026MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000605040302070605'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    202000-209999 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '202000209999MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000605040302070605'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    230120-230120 MOD10 EXC 0
           05  FILLER PIC X(17) VALUE '230120230120MOD10'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000000070103070101'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    300000-308037 DBLAL EXC 6
           05  FILLER PIC X(17) VALUE '300000308037DBLAL'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000201020102010201'.
           05  FILLER PIC X(02) VALUE ' 6'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    309000-309999 DBLAL EXC 6
           05  FILLER PIC X(17) VALUE '309000309999DBLAL'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000201020102010201'.
           05  FILLER PIC X(02) VALUE ' 6'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    400000-400000 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '400000400000MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000807060504030200'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    400001-409999 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '400001409999MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000807060504030200'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    500000-599999 DBLAL EXC 0
           05  FILLER PIC X(17) VALUE '500000599999DBLAL'.
           05  FILLER PIC X(28) VALUE
                                 '0201020102010201020102010201'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    600000-608316 MOD11 EXC 7
           05  FILLER PIC X(17) VALUE '600000608316MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000706050403020701'.
           05  FILLER PIC X(02) VALUE ' 7'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    608317-608317 MOD11 EXC 4
           05  FILLER PIC X(17) VALUE '608317608317MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000706050403020000'.
           05  FILLER PIC X(02) VALUE ' 4'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    608318-609999 MOD11 EXC 7
           05  FILLER PIC X(17) VALUE '608318609999MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000706050403020701'.
           05  FILLER PIC X(02) VALUE ' 7'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    720000-720249 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '720000720249MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000403020706050403'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    770100-771799 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '770100771799MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000807060504030201'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    800000-802999 DBLAL EXC 0
           05  FILLER PIC X(17) VALUE '800000802999DBLAL'.
           05  FILLER PIC X(28) VALUE
                                 '0201020102010201020102010201'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    820000-826097 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '820000826097MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000000020102010201'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    830000-830099 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '830000830099MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000706050403020100'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    830101-830999 MOD10 EXC 0
           05  FILLER PIC X(17) VALUE '830101830999MOD10'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000103070103070100'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    870000-872999 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '870000872999MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000807060504030200'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    938000-938696 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '938000938696MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000706050403020100'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    938700-938999 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '938700938999MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000706050403020100'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
      *    950000-959999 MOD11 EXC 0
           05  FILLER PIC X(17) VALUE '950000959999MOD11'.
           05  FILLER PIC X(28) VALUE
                                 '0000000000000706050403020100'.
           05  FILLER PIC X(02) VALUE ' 0'.
           05  FILLER PIC X(17) VALUE SPACES.
       01  WS-WEIGHT-TABLE REDEFINES WS-WEIGHT-VALUES.
           05  WS-WT-ENTRY                 OCCURS 34 TIMES
                                           INDEXED BY WT-IDX.
               10  WS-WT-SC-FROM           PIC 9(06).
               10  WS-WT-SC-TO             PIC 9(06).
               10  WS-WT-METHOD            PIC X(05).
               10  WS-WT-WEIGHT            PIC 9(02) OCCURS 14 TIMES.
               10  FILLER                  PIC X(01).
               10  WS-WT-EXCEPTION         PIC 9(01).
               10  FILLER                  PIC X(17).
       01  WS-WT-COUNT                     PIC S9(04) COMP VALUE +34.
      *
       01  WS-WORK-FIELDS.
           05  WS-NUMBER-X                 PIC X(14).
           05  WS-NUMBER REDEFINES WS-NUMBER-X.
               10  WS-DIGIT                PIC 9(01) OCCURS 14 TIMES.
           05  WS-SORT-CODE-N              PIC 9(06).
           05  WS-SUB                      PIC S9(04) COMP.
           05  WS-PRODUCT                  PIC S9(05) COMP.
           05  WS-TOTAL                    PIC S9(07) COMP.
           05  WS-QUOT                     PIC S9(07) COMP.
           05  WS-REMAINDER                PIC S9(04) COMP.
           05  WS-CHECK-DIGIT              PIC S9(04) COMP.
           05  WS-MODULUS                  PIC S9(04) COMP.
           05  WS-ENTRIES-FOUND            PIC S9(04) COMP.
           05  WS-FIRST-IDX                PIC S9(04) COMP.
           05  WS-PASS-COUNT               PIC S9(04) COMP.
           05  WS-THIS-PASS-IND            PIC X(01).
               88  WS-THIS-PASSED              VALUE 'Y'.
           05  WS-SKIP-IND                 PIC X(01).
               88  WS-SKIP-CHECK               VALUE 'Y'.
           05  WS-WEIGHTS-USED.
               10  WS-W                    PIC 9(02) OCCURS 14 TIMES.
      *
       LINKAGE SECTION.
           COPY MODPARM.
      *
       PROCEDURE DIVISION USING MOD-PARM-BLOCK.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE ZERO                       TO MOD-RETURN-CD
                                              MOD-EXCEPTION-USED
           MOVE SPACES                     TO MOD-METHOD-USED

           EVALUATE TRUE
               WHEN NOT (MOD-VALIDATE OR MOD-GENERATE)
                   MOVE 16                 TO MOD-RETURN-CD
               WHEN MOD-SORT-CODE NOT NUMERIC
                   MOVE 12                 TO MOD-RETURN-CD
               WHEN MOD-VALIDATE AND MOD-ACCT-NO NOT NUMERIC
                   MOVE 12                 TO MOD-RETURN-CD
               WHEN MOD-GENERATE AND MOD-ACCT-NO (1:7) NOT NUMERIC
                   MOVE 12                 TO MOD-RETURN-CD
               WHEN MOD-VALIDATE
                   PERFORM 1000-VALIDATE THRU 1000-EXIT
               WHEN MOD-GENERATE
                   PERFORM 2000-GENERATE THRU 2000-EXIT
           END-EVALUATE

           GOBACK
           .
      *================================================================*
       1000-VALIDATE.
      *================================================================*
           MOVE MOD-SORT-CODE              TO WS-NUMBER-X (1:6)
           MOVE MOD-ACCT-NO                TO WS-NUMBER-X (7:8)
           MOVE MOD-SORT-CODE              TO WS-SORT-CODE-N

           PERFORM 1100-FIND-ENTRIES

           IF WS-ENTRIES-FOUND = ZERO
               MOVE 04                     TO MOD-RETURN-CD
               GO TO 1000-EXIT
           END-IF

           MOVE ZERO                       TO WS-PASS-COUNT
           SET WT-IDX                      TO WS-FIRST-IDX
           PERFORM WS-ENTRIES-FOUND TIMES
               PERFORM 1200-CHECK-ONE-ENTRY THRU 1200-EXIT
               IF WS-THIS-PASSED
                   ADD 1                   TO WS-PASS-COUNT
               END-IF
               SET WT-IDX                  UP BY 1
           END-PERFORM

      *    ALL ENTRIES MUST PASS (NO EITHER/OR EXCEPTIONS IN OUR TABLE).
           IF WS-PASS-COUNT = WS-ENTRIES-FOUND
               MOVE 00                     TO MOD-RETURN-CD
           ELSE
               MOVE 08                     TO MOD-RETURN-CD
           END-IF
           .
       1000-EXIT.
           EXIT.
      *================================================================*
       1100-FIND-ENTRIES.
      *================================================================*
      *    LINEAR SEARCH - TABLE IS SMALL. AN ENTRY APPLIES WHEN THE
      *    SORT CODE IS WITHIN [FROM, TO]. CONSECUTIVE ENTRIES CAN
      *    OVERLAP FOR THE SAME SORT CODE (TWO CHECKS).
           MOVE ZERO                       TO WS-ENTRIES-FOUND
                                              WS-FIRST-IDX
           PERFORM VARYING WT-IDX FROM 1 BY 1
                   UNTIL WT-IDX > WS-WT-COUNT
               IF WS-SORT-CODE-N >= WS-WT-SC-FROM (WT-IDX)
               AND WS-SORT-CODE-N <= WS-WT-SC-TO (WT-IDX)
                   IF WS-ENTRIES-FOUND = ZERO
                       SET WS-FIRST-IDX    TO WT-IDX
                   END-IF
                   ADD 1                   TO WS-ENTRIES-FOUND
               ELSE
                   IF WS-ENTRIES-FOUND > ZERO
      *                ENTRIES FOR ONE SORT CODE ARE CONTIGUOUS.
                       SET WT-IDX          TO WS-WT-COUNT
                   END-IF
               END-IF
           END-PERFORM
           .
      *================================================================*
       1200-CHECK-ONE-ENTRY.
      *================================================================*
           MOVE 'N'                        TO WS-THIS-PASS-IND
                                              WS-SKIP-IND
           MOVE WS-WT-METHOD (WT-IDX)      TO MOD-METHOD-USED
           MOVE WS-WT-EXCEPTION (WT-IDX)   TO MOD-EXCEPTION-USED

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 14
               MOVE WS-WT-WEIGHT (WT-IDX, WS-SUB) TO WS-W (WS-SUB)
           END-PERFORM

           PERFORM 1300-APPLY-EXCEPTION-BEFORE

           IF WS-SKIP-CHECK
               SET WS-THIS-PASSED          TO TRUE
               GO TO 1200-EXIT
           END-IF

           EVALUATE WS-WT-METHOD (WT-IDX)
               WHEN 'MOD10'
                   MOVE 10                 TO WS-MODULUS
                   PERFORM 1400-WEIGHTED-SUM
               WHEN 'MOD11'
                   MOVE 11                 TO WS-MODULUS
                   PERFORM 1400-WEIGHTED-SUM
               WHEN 'DBLAL'
                   MOVE 10                 TO WS-MODULUS
                   PERFORM 1500-DOUBLE-ALTERNATE-SUM
               WHEN OTHER
      *            UNKNOWN METHOD IN THE TABLE - TREAT AS NOT CHECKED.
                   SET WS-THIS-PASSED      TO TRUE
                   GO TO 1200-EXIT
           END-EVALUATE

           PERFORM 1600-APPLY-EXCEPTION-AFTER

           DIVIDE WS-TOTAL BY WS-MODULUS GIVING WS-QUOT
                  REMAINDER WS-REMAINDER
           IF WS-REMAINDER = ZERO
               SET WS-THIS-PASSED          TO TRUE
           END-IF

      *    EXCEPTION 4 - REMAINDER MUST EQUAL THE CHECK DIGIT FORMED
      *    FROM POSITIONS G AND H INSTEAD OF ZERO.
           IF WS-WT-EXCEPTION (WT-IDX) = 4
               COMPUTE WS-CHECK-DIGIT = WS-DIGIT (13) * 10
                                      + WS-DIGIT (14)
               IF WS-REMAINDER = WS-CHECK-DIGIT
                   SET WS-THIS-PASSED      TO TRUE
               ELSE
                   MOVE 'N'                TO WS-THIS-PASS-IND
               END-IF
           END-IF
           .
       1200-EXIT.
           EXIT.
      *================================================================*
       1300-APPLY-EXCEPTION-BEFORE.
      *================================================================*
           EVALUATE WS-WT-EXCEPTION (WT-IDX)
      *        EXCEPTION 6 - IF POSITION A IS 4-8 AND G = H THE
      *        ACCOUNT IS A FOREIGN CURRENCY ACCOUNT AND CANNOT BE
      *        CHECKED.
               WHEN 6
                   IF WS-DIGIT (7) >= 4 AND WS-DIGIT (7) <= 8
                   AND WS-DIGIT (13) = WS-DIGIT (14)
                       SET WS-SKIP-CHECK   TO TRUE
                   END-IF
      *        EXCEPTION 7 - IF POSITION G = 9, ZEROISE WEIGHTS U TO B.
               WHEN 7
                   IF WS-DIGIT (13) = 9
                       PERFORM VARYING WS-SUB FROM 1 BY 1
                               UNTIL WS-SUB > 8
                           MOVE ZERO       TO WS-W (WS-SUB)
                       END-PERFORM
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       1400-WEIGHTED-SUM.
      *================================================================*
           MOVE ZERO                       TO WS-TOTAL
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 14
               COMPUTE WS-PRODUCT = WS-DIGIT (WS-SUB) * WS-W (WS-SUB)
               ADD WS-PRODUCT              TO WS-TOTAL
           END-PERFORM
           .
      *================================================================*
       1500-DOUBLE-ALTERNATE-SUM.
      *================================================================*
      *    EACH PRODUCT IS SPLIT INTO ITS DIGITS, WHICH ARE ADDED
      *    (E.G. 7 x 2 = 14 CONTRIBUTES 1 + 4 = 5).
           MOVE ZERO                       TO WS-TOTAL
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 14
               COMPUTE WS-PRODUCT = WS-DIGIT (WS-SUB) * WS-W (WS-SUB)
               DIVIDE WS-PRODUCT BY 10 GIVING WS-QUOT
                      REMAINDER WS-REMAINDER
               ADD WS-QUOT                 TO WS-TOTAL
               ADD WS-REMAINDER            TO WS-TOTAL
           END-PERFORM
           .
      *================================================================*
       1600-APPLY-EXCEPTION-AFTER.
      *================================================================*
           EVALUATE WS-WT-EXCEPTION (WT-IDX)
      *        EXCEPTION 1 - ADD 27 TO THE DOUBLE ALTERNATE TOTAL.
               WHEN 1
                   ADD 27                  TO WS-TOTAL
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *================================================================*
       2000-GENERATE.
      *================================================================*
      *    MOD11 WITH THE MEGABANK WEIGHTS FOR 40-XX-XX. THE CHECK
      *    DIGIT H IS CHOSEN SO THE WEIGHTED TOTAL IS DIVISIBLE BY 11.
      *    THE WEIGHT ON H IS 0 IN THE TABLE, SO H = (11 - R) MOD 11
      *    WHERE R IS THE REMAINDER OF THE TOTAL OVER U..G - BUT THAT
      *    ONLY WORKS IF H CARRIES WEIGHT 1. MEGABANK ACCOUNTS
      *    THEREFORE USE A WEIGHT OF 1 FOR H IN GENERATION AND
      *    VALIDATION (OUR TABLE ENTRY HAS 00 - SEE BELOW).
      *
      *    NB. THE 400000-409999 TABLE ENTRIES CARRY WEIGHT 00 FOR H
      *    BECAUSE VOCALINK PUBLISH THEM THAT WAY; VALIDATION OF OUR
      *    OWN NUMBERS THEREFORE IGNORES H AND ALWAYS PASSES WHEN U..G
      *    SUM TO A MULTIPLE OF 11. GENERATION USES WEIGHT 1 SO THAT
      *    OUR NUMBERS ALSO PASS A STRICTER EXTERNAL CHECK. THIS WAS
      *    AGREED WITH VOCALINK IN 2001 (LETTER REF VL/MBK/0117).
           MOVE MOD-SORT-CODE              TO WS-NUMBER-X (1:6)
           MOVE MOD-ACCT-NO (1:7)          TO WS-NUMBER-X (7:7)
           MOVE '0'                        TO WS-NUMBER-X (14:1)
           MOVE MOD-SORT-CODE              TO WS-SORT-CODE-N

           PERFORM 1100-FIND-ENTRIES
           IF WS-ENTRIES-FOUND = ZERO
               MOVE 12                     TO MOD-RETURN-CD
               GO TO 2000-EXIT
           END-IF
           SET WT-IDX                      TO WS-FIRST-IDX
           IF WS-WT-METHOD (WT-IDX) NOT = 'MOD11'
               MOVE 16                     TO MOD-RETURN-CD
               GO TO 2000-EXIT
           END-IF

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 14
               MOVE WS-WT-WEIGHT (WT-IDX, WS-SUB) TO WS-W (WS-SUB)
           END-PERFORM
           MOVE 1                          TO WS-W (14)
           MOVE 11                         TO WS-MODULUS
           PERFORM 1400-WEIGHTED-SUM

           DIVIDE WS-TOTAL BY 11 GIVING WS-QUOT
                  REMAINDER WS-REMAINDER
           COMPUTE WS-CHECK-DIGIT = 11 - WS-REMAINDER
           IF WS-CHECK-DIGIT = 11
               MOVE ZERO                   TO WS-CHECK-DIGIT
           END-IF

           IF WS-CHECK-DIGIT = 10
               MOVE 20                     TO MOD-RETURN-CD
           ELSE
               MOVE WS-CHECK-DIGIT         TO WS-DIGIT (14)
               MOVE WS-NUMBER-X (7:8)      TO MOD-ACCT-NO
               MOVE 'MOD11'                TO MOD-METHOD-USED
               MOVE 00                     TO MOD-RETURN-CD
           END-IF
           .
       2000-EXIT.
           EXIT.
