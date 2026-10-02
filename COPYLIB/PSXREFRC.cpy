      *================================================================*
      * COPYBOOK : PSXREFRC                                            *
      * TITLE    : POSTING EXTERNAL REFERENCE (DUPLICATE CHECK) RECORD *
      * DATASET  : MBK.PRD.PST.EXTREF  (VSAM KSDS)                     *
      *            RECORDSIZE(80 80)  KEYS(30 0)                       *
      * CICS FILE: PSTXREF                                             *
      *----------------------------------------------------------------*
      * ONE RECORD PER CHANNEL REFERENCE POSTED TODAY. A DUPREC ON     *
      * WRITE MEANS THE CHANNEL HAS RESUBMITTED THE SAME POSTING.      *
      * DATASET IS EMPTIED BY PSEOD01 STEP PS030 AFTER HARD POSTING.   *
      *----------------------------------------------------------------*
      * 2002-02-11 DKP  ORIGINAL VERSION (IBANK DOUBLE-POST INCIDENT   *
      *                 PIR 2002/031)                                  *
      * 2010-06-28 JNB  AMOUNT, DIRECTION, TXN CODE AND REVERSED FLAG  *
      *                 CARVED FROM FILLER FOR PSREV01 (POSTREV)       *
      *================================================================*
       01  PXR-XREF-RECORD.
           05  PXR-KEY.
               10  PXR-CHANNEL-CD          PIC X(06).
               10  PXR-EXT-REF             PIC X(24).
           05  PXR-SORT-CODE               PIC X(06).
           05  PXR-ACCT-NO                 PIC X(08).
           05  PXR-POST-DATE               PIC 9(08).
           05  PXR-POST-TIME               PIC 9(08).
           05  PXR-SEQ-NO                  PIC 9(06).
           05  PXR-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  PXR-DR-CR-IND               PIC X(01).
           05  PXR-TXN-CODE                PIC X(04).
           05  PXR-REVERSED-IND            PIC X(01).
               88  PXR-REVERSED                VALUE 'Y'.
