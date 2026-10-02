      *================================================================*
      * COPYBOOK : DDHLDREC                                            *
      * TITLE    : FUNDS HOLD RECORD                                   *
      * DATASET  : MBK.PRD.DDA.HOLDS  (VSAM KSDS)                      *
      *            RECORDSIZE(150 150)  KEYS(18 0)                     *
      * CICS FILE: DDHOLD                                              *
      *----------------------------------------------------------------*
      * ONE RECORD PER HOLD. ACM-HOLD-AMT ON THE ACCOUNT MASTER IS THE *
      * SUM OF ACTIVE HOLDS AND IS MAINTAINED BY DDHLD01 (ONLINE) AND  *
      * DDHLX010 (EXPIRY - RUN IN DDCLS01 JOB STEP DC005).             *
      * SEQUENCE 0000 IS THE ACCOUNT CONTROL RECORD (LAST SEQ USED).   *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-05-17 JNB  ORIGINAL VERSION (HOLDS PREVIOUSLY KEYED AS A  *
      *                 SINGLE AMOUNT ON THE TELLER SYSTEM WITH NO     *
      *                 AUDIT TRAIL - AUDIT FINDING IA-09-031)         *
      *================================================================*
       01  HLD-HOLD-RECORD.
           05  HLD-KEY.
               10  HLD-SORT-CODE           PIC X(06).
               10  HLD-ACCT-NO             PIC X(08).
               10  HLD-SEQ                 PIC 9(04).
                   88  HLD-CONTROL-REC         VALUE 0.
           05  HLD-DATA                    PIC X(132).
      *
           05  HLD-CONTROL-DATA REDEFINES HLD-DATA.
               10  HLD-LAST-SEQ            PIC 9(04).
               10  HLD-ACTIVE-COUNT        PIC 9(03).
               10  FILLER                  PIC X(125).
      *
           05  HLD-HOLD-DATA REDEFINES HLD-DATA.
               10  HLD-STATUS              PIC X(01).
                   88  HLD-ACTIVE              VALUE 'A'.
                   88  HLD-RELEASED            VALUE 'R'.
                   88  HLD-EXPIRED             VALUE 'E'.
               10  HLD-AMOUNT              PIC S9(13)V99 COMP-3.
               10  HLD-REASON-CD           PIC X(04).
                   88  HLD-RSN-CHEQUE          VALUE 'CHQ '.
                   88  HLD-RSN-LEGAL           VALUE 'LEGL'.
                   88  HLD-RSN-FRAUD           VALUE 'FRD '.
                   88  HLD-RSN-CARD            VALUE 'CARD'.
                   88  HLD-RSN-OTHER           VALUE 'OTHR'.
                   88  HLD-RSN-VALID           VALUE 'CHQ ' 'LEGL'
                                                     'FRD ' 'CARD'
                                                     'OTHR'.
               10  HLD-NARRATIVE           PIC X(35).
               10  HLD-PLACED-DATE         PIC 9(08).
               10  HLD-PLACED-TIME         PIC 9(06).
               10  HLD-PLACED-USER         PIC X(08).
               10  HLD-PLACED-CHANNEL      PIC X(06).
               10  HLD-EXPIRY-DATE         PIC 9(08).
               10  HLD-RELEASED-DATE       PIC 9(08).
               10  HLD-RELEASED-USER       PIC X(08).
               10  FILLER                  PIC X(32).
