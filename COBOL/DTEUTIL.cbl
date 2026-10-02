       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DTEUTIL.
       AUTHOR.        R J HARGREAVES.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1991-02-11.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DTEUTIL                                             *
      * TITLE    : COMMON DATE UTILITY                                 *
      * TYPE     : CALLED SUBPROGRAM - BATCH AND CICS (NO CICS API     *
      *            CALLS, SO IT IS SAFE TO CALL FROM EITHER)           *
      * LINKAGE  : COPYBOOK DTEPARM                                    *
      *----------------------------------------------------------------*
      * BANK HOLIDAYS (ENGLAND AND WALES) ARE HELD IN THE TABLE BELOW. *
      * THEY MUST BE ADDED EVERY YEAR BY CHANGE REQUEST BEFORE 1 DEC - *
      * IF THEY ARE NOT, FUTURE-DATED PAYMENTS WILL BE SCHEDULED ON    *
      * BANK HOLIDAYS. SCOTTISH HOLIDAYS ARE NOT SUPPORTED (SEE PIR    *
      * 2004/077).                                                     *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1991-02-11 RJH  ORIGINAL VERSION                               *
      * 1998-10-05 MLW  Y2K - 4 DIGIT YEARS, INTRINSIC FUNCTIONS       *
      *                 REPLACE HAND-WRITTEN JULIAN ROUTINE            *
      * 2003-07-21 SAT  FUNCTION 'J' FOR BACS JULIAN DATES             *
      * 2008-12-01 AOS  BANK HOLIDAYS 2009                             *
      * 2009-11-24 AOS  BANK HOLIDAYS 2010                             *
      * 2010-11-15 JNB  BANK HOLIDAYS 2011 (INCL. ROYAL WEDDING 29/4)  *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DTEUTIL '.
      *
       01  WS-DATE-WORK.
           05  WS-CCYY                     PIC 9(04).
           05  WS-MM                       PIC 9(02).
           05  WS-DD                       PIC 9(02).
       01  WS-DATE-NUM REDEFINES WS-DATE-WORK
                                           PIC 9(08).
      *
       01  WS-WORK-FIELDS.
           05  WS-INTEGER-DATE             PIC S9(09) COMP.
           05  WS-JAN1-INTEGER             PIC S9(09) COMP.
           05  WS-CHECK-DATE               PIC 9(08).
           05  WS-CHECK-INT                PIC S9(09) COMP.
           05  WS-CHECK-DOW                PIC 9(01).
           05  WS-BUS-IND                  PIC X(01).
               88  WS-IS-BUS-DAY               VALUE 'Y'.
               88  WS-NOT-BUS-DAY              VALUE 'N'.
           05  WS-DAYS-LEFT                PIC S9(05) COMP-3.
           05  WS-STEP                     PIC S9(01) COMP-3.
           05  WS-MAX-DAY                  PIC 9(02).
           05  WS-QUOTIENT                 PIC 9(04).
           05  WS-REM-4                    PIC 9(04).
           05  WS-REM-100                  PIC 9(04).
           05  WS-REM-400                  PIC 9(04).
           05  WS-LEAP-IND                 PIC X(01).
               88  WS-LEAP-YEAR                VALUE 'Y'.
           05  WS-DAY-OF-YEAR              PIC 9(03).
      *
       01  WS-JULIAN-OUT.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-JUL-YY                   PIC X(02).
           05  WS-JUL-DDD                  PIC 9(03).
      *
       01  WS-DAYS-IN-MONTH-VALUES.
           05  FILLER                      PIC X(24)
                   VALUE '312831303130313130313031'.
       01  WS-DAYS-IN-MONTH-TABLE REDEFINES WS-DAYS-IN-MONTH-VALUES.
           05  WS-DIM                      PIC 9(02) OCCURS 12 TIMES.
      *
      *    ENGLAND AND WALES BANK HOLIDAYS - KEEP IN DATE ORDER.
      *    UPDATE WS-BH-COUNT WHEN ADDING ENTRIES.
       01  WS-BANK-HOLIDAY-VALUES.
           05  FILLER  PIC 9(08) VALUE 20090101.
           05  FILLER  PIC 9(08) VALUE 20090410.
           05  FILLER  PIC 9(08) VALUE 20090413.
           05  FILLER  PIC 9(08) VALUE 20090504.
           05  FILLER  PIC 9(08) VALUE 20090525.
           05  FILLER  PIC 9(08) VALUE 20090831.
           05  FILLER  PIC 9(08) VALUE 20091225.
           05  FILLER  PIC 9(08) VALUE 20091228.
           05  FILLER  PIC 9(08) VALUE 20100101.
           05  FILLER  PIC 9(08) VALUE 20100402.
           05  FILLER  PIC 9(08) VALUE 20100405.
           05  FILLER  PIC 9(08) VALUE 20100503.
           05  FILLER  PIC 9(08) VALUE 20100531.
           05  FILLER  PIC 9(08) VALUE 20100830.
           05  FILLER  PIC 9(08) VALUE 20101227.
           05  FILLER  PIC 9(08) VALUE 20101228.
           05  FILLER  PIC 9(08) VALUE 20110103.
           05  FILLER  PIC 9(08) VALUE 20110422.
           05  FILLER  PIC 9(08) VALUE 20110425.
           05  FILLER  PIC 9(08) VALUE 20110429.
           05  FILLER  PIC 9(08) VALUE 20110502.
           05  FILLER  PIC 9(08) VALUE 20110530.
           05  FILLER  PIC 9(08) VALUE 20110829.
           05  FILLER  PIC 9(08) VALUE 20111226.
           05  FILLER  PIC 9(08) VALUE 20111227.
       01  WS-BANK-HOLIDAY-TABLE REDEFINES WS-BANK-HOLIDAY-VALUES.
           05  WS-BANK-HOLIDAY             PIC 9(08)
                                           OCCURS 25 TIMES
                                           ASCENDING KEY WS-BANK-HOLIDAY
                                           INDEXED BY BH-IDX.
       01  WS-BH-COUNT                     PIC S9(04) COMP VALUE +25.
      *
       LINKAGE SECTION.
           COPY DTEPARM.
      *
       PROCEDURE DIVISION USING DTE-PARM-BLOCK.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE ZERO                       TO DTE-RETURN-CD
           IF DTE-FUNCTION NOT = 'V' AND 'W' AND 'B'
                          AND 'A' AND 'D' AND 'J'
               MOVE 12                     TO DTE-RETURN-CD
               GOBACK
           END-IF

           PERFORM 1000-VALIDATE-DATE
           IF NOT DTE-OK
               GOBACK
           END-IF

           EVALUATE DTE-FUNCTION
               WHEN 'V'
                   CONTINUE
               WHEN 'W'
                   PERFORM 2000-DAY-OF-WEEK
               WHEN 'B'
                   MOVE DTE-DATE-IN        TO WS-CHECK-DATE
                   PERFORM 3100-CHECK-BUSINESS-DAY
                   MOVE WS-BUS-IND         TO DTE-BUS-DAY-IND
               WHEN 'A'
                   PERFORM 4000-ADD-BUSINESS-DAYS
               WHEN 'D'
                   PERFORM 5000-ADD-CALENDAR-DAYS
               WHEN 'J'
                   PERFORM 6000-JULIAN-DATE
           END-EVALUATE

           GOBACK
           .
      *================================================================*
       1000-VALIDATE-DATE.
      *================================================================*
           MOVE DTE-DATE-IN                TO WS-DATE-NUM

           IF WS-DATE-WORK NOT NUMERIC
           OR WS-CCYY < 1601
           OR WS-MM < 1 OR WS-MM > 12
               MOVE 08                     TO DTE-RETURN-CD
           ELSE
               DIVIDE WS-CCYY BY 4   GIVING WS-QUOTIENT
                                     REMAINDER WS-REM-4
               DIVIDE WS-CCYY BY 100 GIVING WS-QUOTIENT
                                     REMAINDER WS-REM-100
               DIVIDE WS-CCYY BY 400 GIVING WS-QUOTIENT
                                     REMAINDER WS-REM-400
               IF WS-REM-4 = 0
               AND (WS-REM-100 NOT = 0 OR WS-REM-400 = 0)
                   MOVE 'Y'                TO WS-LEAP-IND
               ELSE
                   MOVE 'N'                TO WS-LEAP-IND
               END-IF

               MOVE WS-DIM (WS-MM)         TO WS-MAX-DAY
               IF WS-MM = 2 AND WS-LEAP-YEAR
                   MOVE 29                 TO WS-MAX-DAY
               END-IF

               IF WS-DD < 1 OR WS-DD > WS-MAX-DAY
                   MOVE 08                 TO DTE-RETURN-CD
               END-IF
           END-IF
           .
      *================================================================*
       2000-DAY-OF-WEEK.
      *================================================================*
      *    INTEGER DATE 1 IS 1601-01-01, WHICH WAS A MONDAY.
           COMPUTE WS-INTEGER-DATE =
                   FUNCTION INTEGER-OF-DATE (DTE-DATE-IN)
           COMPUTE DTE-DAY-OF-WEEK =
                   FUNCTION MOD (WS-INTEGER-DATE - 1, 7) + 1
           .
      *================================================================*
       3100-CHECK-BUSINESS-DAY.
      *================================================================*
      *    IN : WS-CHECK-DATE    OUT : WS-BUS-IND
           COMPUTE WS-CHECK-INT =
                   FUNCTION INTEGER-OF-DATE (WS-CHECK-DATE)
           COMPUTE WS-CHECK-DOW =
                   FUNCTION MOD (WS-CHECK-INT - 1, 7) + 1

           IF WS-CHECK-DOW > 5
               SET WS-NOT-BUS-DAY          TO TRUE
           ELSE
               SEARCH ALL WS-BANK-HOLIDAY
                   AT END
                       SET WS-IS-BUS-DAY   TO TRUE
                   WHEN WS-BANK-HOLIDAY (BH-IDX) = WS-CHECK-DATE
                       SET WS-NOT-BUS-DAY  TO TRUE
               END-SEARCH
           END-IF
           .
      *================================================================*
       4000-ADD-BUSINESS-DAYS.
      *================================================================*
      *    DTE-DAYS MAY BE NEGATIVE (E.G. -1 = PREVIOUS BUSINESS DAY).
      *    IF DTE-DAYS IS ZERO THE INPUT DATE IS RETURNED UNCHANGED,
      *    EVEN IF IT IS NOT A BUSINESS DAY.
           MOVE DTE-DATE-IN                TO WS-CHECK-DATE
           COMPUTE WS-INTEGER-DATE =
                   FUNCTION INTEGER-OF-DATE (DTE-DATE-IN)

           IF DTE-DAYS < 0
               MOVE -1                     TO WS-STEP
               COMPUTE WS-DAYS-LEFT = 0 - DTE-DAYS
           ELSE
               MOVE +1                     TO WS-STEP
               MOVE DTE-DAYS               TO WS-DAYS-LEFT
           END-IF

           PERFORM UNTIL WS-DAYS-LEFT = 0
               ADD WS-STEP                 TO WS-INTEGER-DATE
               COMPUTE WS-CHECK-DATE =
                       FUNCTION DATE-OF-INTEGER (WS-INTEGER-DATE)
               PERFORM 3100-CHECK-BUSINESS-DAY
               IF WS-IS-BUS-DAY
                   SUBTRACT 1              FROM WS-DAYS-LEFT
               END-IF
           END-PERFORM

           MOVE WS-CHECK-DATE              TO DTE-DATE-OUT
           .
      *================================================================*
       5000-ADD-CALENDAR-DAYS.
      *================================================================*
           COMPUTE WS-INTEGER-DATE =
                   FUNCTION INTEGER-OF-DATE (DTE-DATE-IN) + DTE-DAYS
           COMPUTE DTE-DATE-OUT =
                   FUNCTION DATE-OF-INTEGER (WS-INTEGER-DATE)
           .
      *================================================================*
       6000-JULIAN-DATE.
      *================================================================*
      *    BACS FORMAT ' YYDDD' - LEADING SPACE, 2 DIGIT YEAR, DAY NO.
           MOVE DTE-DATE-IN                TO WS-DATE-NUM
           COMPUTE WS-JAN1-INTEGER =
                   FUNCTION INTEGER-OF-DATE (WS-CCYY * 10000 + 0101)
           COMPUTE WS-DAY-OF-YEAR =
                   FUNCTION INTEGER-OF-DATE (DTE-DATE-IN)
                   - WS-JAN1-INTEGER + 1
           MOVE WS-DATE-WORK (3:2)         TO WS-JUL-YY
           MOVE WS-DAY-OF-YEAR             TO WS-JUL-DDD
           MOVE WS-JULIAN-OUT              TO DTE-JULIAN
           .
