      *================================================================*
      * COPYBOOK : STIDXREC                                            *
      * TITLE    : STATEMENT INDEX RECORD                              *
      * DATASET  : MBK.PRD.STMT.INDEX(+1)  RECFM=FB LRECL=200          *
      *----------------------------------------------------------------*
      * ONE RECORD PER STATEMENT PRODUCED BY STSTM020. USED BY:        *
      *   - STSTM030 TO UPDATE THE ACCOUNT MASTER (LAST STATEMENT      *
      *     DATE / NUMBER) AND BUILD THE IBANK E-STATEMENT NOTICES     *
      *   - THE CMOD ARCHIVE LOAD (DOCUMENT SERVICES) - SIX-IDX-*      *
      *     FIELDS ARE THE ARCHIVE INDEX VALUES                        *
      *   - PRINT VENDOR RECONCILIATION (PAGE AND SHEET COUNTS)        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-03-22 SAT  ORIGINAL VERSION (CR 10/0141 - STATEMENTS      *
      *                 BROUGHT BACK FROM DOCUMENT SERVICES)           *
      *================================================================*
       01  SIX-INDEX-RECORD.
           05  SIX-DOC-KEY                 PIC X(30).
           05  SIX-SORT-CODE               PIC X(06).
           05  SIX-ACCT-NO                 PIC X(08).
           05  SIX-CIF-NO                  PIC X(12).
           05  SIX-STMT-NO                 PIC 9(05).
           05  SIX-PERIOD-FROM             PIC 9(08).
           05  SIX-PERIOD-TO               PIC 9(08).
           05  SIX-OPENING-BAL             PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  SIX-CLOSING-BAL             PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  SIX-TXN-COUNT               PIC 9(05).
           05  SIX-PAGE-COUNT              PIC 9(04).
           05  SIX-SHEET-COUNT             PIC 9(04).
           05  SIX-FIRST-LINE-NO           PIC 9(09).
           05  SIX-DELIVERY                PIC X(01).
               88  SIX-PAPER                   VALUE 'P'.
               88  SIX-ELECTRONIC              VALUE 'E'.
               88  SIX-SUPPRESSED              VALUE 'S'.
           05  SIX-REQUEST-TYPE            PIC X(01).
           05  SIX-IDX-CUST-NAME           PIC X(40).
           05  SIX-IDX-POST-CD             PIC X(10).
           05  SIX-STATUS                  PIC X(01).
               88  SIX-PRODUCED                VALUE 'P'.
               88  SIX-FAILED                  VALUE 'F'.
           05  SIX-FAIL-REASON             PIC X(04).
           05  FILLER                      PIC X(12).
