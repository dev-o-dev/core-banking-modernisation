      *================================================================*
      * COPYBOOK : PSARCREC                                            *
      * TITLE    : TRANSACTION HISTORY ARCHIVE RECORD                  *
      * DATASET  : MBK.PRD.TXNHIST.ARCHIVE(+1)  RECFM=FB LRECL=250     *
      *            WRITTEN BY PSARC010 BEFORE THE ROWS ARE DELETED     *
      *            FROM MBK.TXNHIST. GDG LIMIT 255, ON CARTRIDGE       *
      *            (ARCHIVE CLASS ARC10Y - KEPT 10 YEARS).             *
      *----------------------------------------------------------------*
      * ONE 'D' RECORD PER ROW, ALL COLUMNS IN DISPLAY FORMAT SO THE   *
      * FILE CAN BE RELOADED (DSNUTILB LOAD WITH FIELD SPEC MEMBER     *
      * MBK.PRD.CNTL(TXNHRELD)) OR SEARCHED WITH SORT/ICETOOL FOR      *
      * FOS AND COURT ORDER (DPA S29) REQUESTS.                        *
      * RECORD TYPES: H HEADER  D DETAIL  T TRAILER                    *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2012-02-13 PDQ  ORIGINAL VERSION                               *
      * 2014-09-01 KLM  RESTART INDICATOR ON HEADER                    *
      *================================================================*
       01  ARC-DETAIL-RECORD.
           05  ARC-REC-TYPE                PIC X(01).
               88  ARC-HEADER                  VALUE 'H'.
               88  ARC-DETAIL                  VALUE 'D'.
               88  ARC-TRAILER                 VALUE 'T'.
           05  ARC-SORT-CODE               PIC X(06).
           05  ARC-ACCT-NO                 PIC X(08).
           05  ARC-POST-DATE               PIC X(10).
           05  ARC-POST-SEQ                PIC 9(09).
           05  ARC-TXN-CODE                PIC X(04).
           05  ARC-DR-CR-IND               PIC X(01).
           05  ARC-TXN-AMT                 PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  ARC-RUNNING-BAL             PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
           05  ARC-CCY-CD                  PIC X(03).
           05  ARC-VALUE-DATE              PIC X(10).
           05  ARC-NARRATIVE-1             PIC X(35).
           05  ARC-NARRATIVE-2             PIC X(35).
           05  ARC-CHANNEL-CD              PIC X(06).
           05  ARC-EXT-REF                 PIC X(24).
           05  ARC-POST-TYPE               PIC X(01).
           05  ARC-USER-ID                 PIC X(08).
           05  ARC-BATCH-RUN-ID            PIC X(08).
           05  ARC-INSERT-TS               PIC X(26).
           05  FILLER                      PIC X(23).
      *
       01  ARC-HEADER-RECORD REDEFINES ARC-DETAIL-RECORD.
           05  ARH-REC-TYPE                PIC X(01).
           05  ARH-ARCHIVE-ID              PIC X(08).
           05  ARH-BUS-DATE                PIC 9(08).
           05  ARH-CUTOFF-DATE             PIC X(10).
           05  ARH-RETAIN-MONTHS           PIC 9(03).
           05  ARH-MODE                    PIC X(05).
           05  ARH-RESTART-IND             PIC X(01).
               88  ARH-RESTARTED               VALUE 'Y'.
           05  ARH-RESTART-POST-DATE       PIC X(10).
           05  ARH-RESTART-SORT-CODE       PIC X(06).
           05  ARH-RESTART-ACCT-NO         PIC X(08).
           05  ARH-RESTART-POST-SEQ        PIC 9(09).
           05  FILLER                      PIC X(181).
      *
       01  ARC-TRAILER-RECORD REDEFINES ARC-DETAIL-RECORD.
           05  ART-REC-TYPE                PIC X(01).
           05  ART-ARCHIVE-ID              PIC X(08).
           05  ART-BUS-DATE                PIC 9(08).
           05  ART-ROW-COUNT               PIC 9(11).
           05  ART-DEBIT-TOTAL             PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
           05  ART-CREDIT-TOTAL            PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
           05  ART-ROWS-DELETED            PIC 9(11).
           05  FILLER                      PIC X(175).
