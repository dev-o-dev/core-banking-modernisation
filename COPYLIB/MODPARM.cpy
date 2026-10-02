      *================================================================*
      * COPYBOOK : MODPARM                                             *
      * TITLE    : PARAMETER BLOCK FOR MODULUS CHECK SUBPROGRAM DDMODCK*
      *----------------------------------------------------------------*
      * USAGE:  MOVE '401234'   TO MOD-SORT-CODE                       *
      *         MOVE '12345678' TO MOD-ACCT-NO                         *
      *         MOVE 'V' TO MOD-FUNCTION                               *
      *         CALL 'DDMODCK' USING MOD-PARM-BLOCK                    *
      *                                                                *
      * FUNCTIONS                                                      *
      *   V  VALIDATE SORT CODE + ACCOUNT NUMBER (VOCALINK MODULUS     *
      *      CHECKING - VALACDOS TABLE LOADED INTO DDMODCK)            *
      *   G  GENERATE - MOD-ACCT-NO POSITIONS 1-7 ARE SUPPLIED, THE    *
      *      CHECK DIGIT IS CALCULATED INTO POSITION 8 (MEGABANK OWN   *
      *      SORT CODES ONLY - 40-XX-XX RANGE, METHOD MOD11)           *
      *                                                                *
      * RETURN CODE                                                    *
      *   00 VALID                                                     *
      *   04 VALID - SORT CODE NOT IN TABLE, NO CHECK POSSIBLE         *
      *   08 INVALID - FAILED MODULUS CHECK                            *
      *   12 INVALID INPUT (NOT NUMERIC)                               *
      *   16 INVALID FUNCTION                                          *
      *   20 GENERATE NOT POSSIBLE (CHECK DIGIT WOULD BE 10)           *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2001-03-05 DKP  ORIGINAL VERSION (IBANK PAYEE SET-UP)          *
      * 2004-06-21 SAT  EXCEPTIONS 1-6 (VOCALINK SPEC V2.10)           *
      *================================================================*
       01  MOD-PARM-BLOCK.
           05  MOD-FUNCTION                PIC X(01).
               88  MOD-VALIDATE                VALUE 'V'.
               88  MOD-GENERATE                VALUE 'G'.
           05  MOD-SORT-CODE               PIC X(06).
           05  MOD-ACCT-NO                 PIC X(08).
           05  MOD-METHOD-USED             PIC X(05).
           05  MOD-EXCEPTION-USED          PIC 9(02).
           05  MOD-RETURN-CD               PIC 9(02).
               88  MOD-VALID                   VALUE 00.
               88  MOD-VALID-NO-CHECK          VALUE 04.
               88  MOD-INVALID                 VALUE 08.
               88  MOD-INVALID-INPUT           VALUE 12.
               88  MOD-INVALID-FUNCTION        VALUE 16.
               88  MOD-CANNOT-GENERATE         VALUE 20.
           05  FILLER                      PIC X(16).
