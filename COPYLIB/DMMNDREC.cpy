      *================================================================*
      * COPYBOOK : DMMNDREC                                            *
      * TITLE    : DIRECT DEBIT MANDATE (INSTRUCTION) RECORD           *
      * DATASET  : MBK.PRD.DDM.MANDATE  (VSAM KSDS)                    *
      *            RECORDSIZE(300 300)  KEYS(38 0)                     *
      * CICS FILE: DDMAND                                              *
      *----------------------------------------------------------------*
      * KEY = PAYER SORT CODE + ACCOUNT + ORIGINATOR SUN + PAYER       *
      * REFERENCE (THE BACS "REFERENCE" FIELD THE ORIGINATOR USES).    *
      * A MANDATE IS SET UP BY AN AUDDIS 0N INSTRUCTION (DMAUD010) OR, *
      * FOR PAPER MANDATES LODGED AT BRANCH BEFORE 2001, BY THE        *
      * CONVERSION LOAD (DMCNV01 - NOT IN THIS LIBRARY).               *
      *                                                                *
      * STATUS (DMM-STATUS)                                            *
      *   L  LIVE                                                      *
      *   D  DORMANT - NO COLLECTION FOR 13 MONTHS (BACS RULE). THE    *
      *      NEXT COLLECTION IS RETURNED WITH ARUDD REASON 'B'.        *
      *   C  CANCELLED BY PAYER (ADDACS 1 SENT TO ORIGINATOR)          *
      *   O  CANCELLED BY ORIGINATOR (AUDDIS 0C)                       *
      *   X  CLOSED - PAYER ACCOUNT CLOSED (ADDACS B SENT)             *
      *   T  TRANSFERRED - ACCOUNT SWITCHED (ADDACS 3/C)               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1989-09-04 RJH  ORIGINAL VERSION (PAPER DDI REGISTER)          *
      * 1998-11-23 MLW  Y2K                                            *
      * 2001-04-02 DKP  AUDDIS (AUTOMATED DDI SERVICE) FIELDS          *
      * 2003-09-08 SAT  ADVANCE NOTICE / FIRST COLLECTION FLAG         *
      * 2008-11-17 JNB  INDEMNITY CLAIM COUNT (DDICA)                  *
      *================================================================*
       01  DMM-MANDATE-RECORD.
           05  DMM-KEY.
               10  DMM-PAYER-SORT-CODE     PIC X(06).
               10  DMM-PAYER-ACCT-NO       PIC X(08).
               10  DMM-ORIG-SUN            PIC X(06).
               10  DMM-PAYER-REF           PIC X(18).
           05  DMM-STATUS                  PIC X(01).
               88  DMM-LIVE                    VALUE 'L'.
               88  DMM-DORMANT                 VALUE 'D'.
               88  DMM-CANC-BY-PAYER           VALUE 'C'.
               88  DMM-CANC-BY-ORIG            VALUE 'O'.
               88  DMM-CLOSED                  VALUE 'X'.
               88  DMM-TRANSFERRED             VALUE 'T'.
               88  DMM-COLLECTABLE             VALUE 'L'.
           05  DMM-ORIG-NAME               PIC X(18).
           05  DMM-ORIG-SORT-CODE          PIC X(06).
           05  DMM-ORIG-ACCT-NO            PIC X(08).
           05  DMM-PAYER-NAME              PIC X(18).
           05  DMM-SOURCE                  PIC X(01).
               88  DMM-SRC-AUDDIS              VALUE 'A'.
               88  DMM-SRC-PAPER               VALUE 'P'.
               88  DMM-SRC-CONVERSION          VALUE 'V'.
               88  DMM-SRC-SWITCH              VALUE 'S'.
           05  DMM-SETUP-DATE              PIC 9(08).
           05  DMM-FIRST-COLL-DATE         PIC 9(08).
           05  DMM-LAST-COLL-DATE          PIC 9(08).
           05  DMM-LAST-COLL-AMT           PIC S9(13)V99 COMP-3.
           05  DMM-COLL-COUNT              PIC 9(07) COMP-3.
           05  DMM-COLL-TOTAL              PIC S9(13)V99 COMP-3.
           05  DMM-UNPAID-COUNT            PIC 9(05) COMP-3.
           05  DMM-LAST-UNPAID-DATE        PIC 9(08).
           05  DMM-LAST-UNPAID-RSN         PIC X(01).
           05  DMM-INDEMNITY-COUNT         PIC 9(03) COMP-3.
           05  DMM-MAX-AMT                 PIC S9(13)V99 COMP-3.
           05  DMM-CANCEL-DATE             PIC 9(08).
           05  DMM-CANCEL-CHANNEL          PIC X(06).
           05  DMM-CANCEL-USER             PIC X(08).
           05  DMM-ADDACS-SENT-DATE        PIC 9(08).
           05  DMM-ADDACS-REASON           PIC X(01).
           05  DMM-LAST-MAINT-DATE         PIC 9(08).
           05  DMM-LAST-MAINT-PGM          PIC X(08).
           05  FILLER                      PIC X(97).
