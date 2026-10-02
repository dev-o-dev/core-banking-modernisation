      *================================================================*
      * COPYBOOK : IBNPARM                                             *
      * TITLE    : PARAMETER BLOCK FOR IBAN SUBPROGRAM DDIBAN1         *
      *----------------------------------------------------------------*
      * FUNCTIONS                                                      *
      *   G  GENERATE A GB IBAN FROM IBN-SORT-CODE + IBN-ACCT-NO       *
      *      (BANK CODE 'MBKP' - MEGABANK SWIFT BIC MBKPGB2L)          *
      *   V  VALIDATE IBN-IBAN (ANY COUNTRY IN THE LENGTH TABLE)       *
      *   S  SPLIT A GB IBAN INTO SORT CODE + ACCOUNT                  *
      *                                                                *
      * RETURN CODE                                                    *
      *   00 OK                                                        *
      *   04 VALID BUT COUNTRY NOT IN LENGTH TABLE (LENGTH NOT CHECKED)*
      *   08 INVALID CHECK DIGITS                                      *
      *   10 INVALID LENGTH FOR COUNTRY                                *
      *   12 INVALID CHARACTERS                                        *
      *   14 NOT A GB IBAN (FUNCTION S)                                *
      *   16 INVALID FUNCTION                                          *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2001-04-30 DKP  ORIGINAL VERSION (EU REG 2560/2001)            *
      * 2007-11-05 AOS  VALIDATE FUNCTION (SEPA COUNTRIES) FOR SWIFT   *
      *================================================================*
       01  IBN-PARM-BLOCK.
           05  IBN-FUNCTION                PIC X(01).
               88  IBN-GENERATE                VALUE 'G'.
               88  IBN-VALIDATE                VALUE 'V'.
               88  IBN-SPLIT                   VALUE 'S'.
           05  IBN-SORT-CODE               PIC X(06).
           05  IBN-ACCT-NO                 PIC X(08).
           05  IBN-IBAN                    PIC X(34).
           05  IBN-IBAN-PRINT              PIC X(42).
           05  IBN-COUNTRY-CD              PIC X(02).
           05  IBN-BANK-CODE               PIC X(04).
           05  IBN-RETURN-CD               PIC 9(02).
               88  IBN-OK                      VALUE 00.
               88  IBN-OK-NO-LENGTH-CHECK      VALUE 04.
               88  IBN-BAD-CHECK-DIGITS        VALUE 08.
               88  IBN-BAD-LENGTH              VALUE 10.
               88  IBN-BAD-CHARACTERS          VALUE 12.
               88  IBN-NOT-GB                  VALUE 14.
               88  IBN-BAD-FUNCTION            VALUE 16.
           05  FILLER                      PIC X(10).
