      *================================================================*
      * COPYBOOK : DTEPARM                                             *
      * TITLE    : PARAMETER BLOCK FOR DATE UTILITY DTEUTIL            *
      *----------------------------------------------------------------*
      * USAGE:  MOVE 'A' TO DTE-FUNCTION                               *
      *         MOVE 20100714 TO DTE-DATE-IN                           *
      *         MOVE 3 TO DTE-DAYS                                     *
      *         CALL 'DTEUTIL' USING DTE-PARM-BLOCK                    *
      *                                                                *
      * FUNCTIONS                                                      *
      *   V  VALIDATE DTE-DATE-IN (CCYYMMDD)                           *
      *   W  DAY OF WEEK OF DTE-DATE-IN  (1=MONDAY .. 7=SUNDAY)        *
      *   B  IS DTE-DATE-IN A UK BUSINESS DAY  (DTE-BUS-DAY-IND Y/N)   *
      *   A  ADD DTE-DAYS BUSINESS DAYS TO DTE-DATE-IN -> DTE-DATE-OUT *
      *   D  ADD DTE-DAYS CALENDAR DAYS TO DTE-DATE-IN -> DTE-DATE-OUT *
      *   J  CONVERT DTE-DATE-IN TO ' YYDDD' (BACS JULIAN)             *
      * RETURN CODE  00 OK   08 INVALID DATE   12 INVALID FUNCTION     *
      *================================================================*
       01  DTE-PARM-BLOCK.
           05  DTE-FUNCTION                PIC X(01).
           05  DTE-DATE-IN                 PIC 9(08).
           05  DTE-DAYS                    PIC S9(05) COMP-3.
           05  DTE-DATE-OUT                PIC 9(08).
           05  DTE-DAY-OF-WEEK             PIC 9(01).
           05  DTE-BUS-DAY-IND             PIC X(01).
           05  DTE-JULIAN                  PIC X(06).
           05  DTE-RETURN-CD               PIC 9(02).
               88  DTE-OK                      VALUE 00.
               88  DTE-INVALID-DATE            VALUE 08.
               88  DTE-INVALID-FUNCTION        VALUE 12.
