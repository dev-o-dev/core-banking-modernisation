      *================================================================*
      * COPYBOOK : DDNOCREC                                            *
      * TITLE    : ACCOUNT NUMBER ALLOCATION CONTROL RECORD            *
      * DATASET  : MBK.PRD.DDA.ACCTNO.CONTROL  (VSAM KSDS)             *
      *            RECORDSIZE(100 100)  KEYS(16 0)                     *
      * CICS FILE: ACCTNOC  (RECOVERABLE)                              *
      *----------------------------------------------------------------*
      * ONE RECORD PER SORT CODE + PRODUCT NUMBER RANGE. DDOPN01 READS *
      * IT FOR UPDATE, TAKES THE NEXT 7-DIGIT BASE NUMBER, ASKS        *
      * DDMODCK FOR THE CHECK DIGIT, AND SKIPS BASES WHOSE CHECK DIGIT *
      * WOULD BE 10 (ABOUT 1 IN 11).                                   *
      * LOADED BY DBA JOB MBKDEFVS FROM CNTL(DDNOCLOD) WHEN A NEW SORT *
      * CODE OR PRODUCT RANGE IS ADDED.                                *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-05-17 JNB  ORIGINAL VERSION - REPLACES THE BRANCH ACCOUNT *
      *                 NUMBER BOOKS (PRE-PRINTED RANGES) USED BY THE  *
      *                 TELLER SYSTEM                                  *
      *================================================================*
       01  NOC-CONTROL-RECORD.
           05  NOC-KEY.
               10  NOC-SORT-CODE           PIC X(06).
               10  NOC-RANGE-LOW           PIC 9(07).
               10  NOC-RANGE-ID            PIC X(03).
           05  NOC-RANGE-HIGH              PIC 9(07).
           05  NOC-LAST-BASE               PIC 9(07).
           05  NOC-ALLOCATED-COUNT         PIC 9(09) COMP.
           05  NOC-SKIPPED-COUNT           PIC 9(09) COMP.
           05  NOC-STATUS                  PIC X(01).
               88  NOC-OPEN                    VALUE 'O'.
               88  NOC-EXHAUSTED               VALUE 'X'.
               88  NOC-SUSPENDED               VALUE 'S'.
           05  NOC-WARN-THRESHOLD          PIC 9(07).
           05  NOC-LAST-ALLOC-DATE         PIC 9(08).
           05  NOC-LAST-ALLOC-USER         PIC X(08).
           05  FILLER                      PIC X(38).
