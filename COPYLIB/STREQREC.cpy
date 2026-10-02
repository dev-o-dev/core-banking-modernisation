      *================================================================*
      * COPYBOOK : STREQREC                                            *
      * TITLE    : STATEMENT REQUEST RECORD                            *
      * DATASET  : MBK.PRD.STMT.REQUEST(+1)  RECFM=FB LRECL=100        *
      *----------------------------------------------------------------*
      * WRITTEN BY STSTM010 (SELECTION), READ BY STSTM020 (COMPOSE).   *
      * UNTIL 2010 THE LAYOUT WAS CODED INLINE IN STSTM010 AND IN THE  *
      * DOCUMENT SERVICES PROGRAMS - THIS COPYBOOK WAS CREATED WHEN    *
      * COMPOSITION WAS BROUGHT BACK INTO MBK.PRD.SOURCE (CR 10/0141). *
      * STSTM010 STILL HAS ITS OWN INLINE COPY - KEEP THEM IN STEP.    *
      *----------------------------------------------------------------*
      * SRQ-DOC-KEY IS BLANK ON THE REQUEST. STSTM020 FILLS IT IN ON   *
      * THE INDEX RECORD IT WRITES FOR STSTM030:                       *
      *   'MBKST' || SORT CODE || ACCOUNT || STMT NO (5) || CCYYMMDD   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-03-22 SAT  ORIGINAL VERSION (CR 10/0141)                  *
      *================================================================*
       01  SRQ-STMT-REQUEST.
           05  SRQ-SORT-CODE               PIC X(06).
           05  SRQ-ACCT-NO                 PIC X(08).
           05  SRQ-CIF-NO                  PIC X(12).
           05  SRQ-STMT-NO                 PIC 9(05).
           05  SRQ-PERIOD-FROM             PIC 9(08).
           05  SRQ-PERIOD-TO               PIC 9(08).
           05  SRQ-CLOSING-BAL             PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  SRQ-DOC-KEY                 PIC X(30).
           05  SRQ-REQUEST-TYPE            PIC X(01).
               88  SRQ-CYCLE                   VALUE ' ' 'C'.
               88  SRQ-ADHOC                   VALUE 'A'.
               88  SRQ-CLOSURE                 VALUE 'X'.
               88  SRQ-DUPLICATE               VALUE 'D'.
           05  FILLER                      PIC X(06).
