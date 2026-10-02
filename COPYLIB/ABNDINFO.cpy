      *================================================================*
      * COPYBOOK : ABNDINFO                                            *
      * TITLE    : STANDARD BATCH ABEND WORK AREA                      *
      *            USE WITH PROCEDURE COPYBOOK ABNDPROC                *
      *----------------------------------------------------------------*
      * USER ABEND CODES (MBK STANDARDS MANUAL SECTION 7.3)            *
      *   U1001 FILE OPEN ERROR        U1002 FILE I/O ERROR            *
      *   U1003 DB2 SQL ERROR          U1004 CONTROL TOTAL MISMATCH    *
      *   U1005 INVALID DATE CARD      U1006 FILE SEQUENCE ERROR       *
      *   U1007 MISSING HEADER/TRAILER                                 *
      *================================================================*
       01  ABN-ABEND-INFO.
           05  ABN-PROGRAM-ID              PIC X(08).
           05  ABN-PARAGRAPH               PIC X(30).
           05  ABN-ABEND-CODE              PIC S9(09) COMP.
           05  ABN-TIMING                  PIC S9(09) COMP VALUE +1.
           05  ABN-MESSAGE                 PIC X(80).
           05  ABN-FILE-STATUS             PIC X(02) VALUE SPACES.
           05  ABN-SQLCODE                 PIC S9(09)
                                           SIGN LEADING SEPARATE
                                           VALUE ZERO.
           05  ABN-KEY-DATA                PIC X(40) VALUE SPACES.
