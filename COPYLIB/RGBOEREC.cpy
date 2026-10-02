      *================================================================*
      * COPYBOOK : RGBOEREC                                            *
      * TITLE    : BANK OF ENGLAND QUARTERLY STATISTICAL RETURN        *
      * DATASET  : MBK.PRD.REG.BOERTN(+1)  RECFM=FB LRECL=100          *
      *            WRITTEN BY RGBOE010 (JOB RGBOE01). LOADED BY        *
      *            REGULATORY REPORTING INTO THE BOE ELECTRONIC        *
      *            SUBMISSION TOOL (FORMERLY KEYED FROM THE PRINTOUT). *
      *----------------------------------------------------------------*
      * RECORD TYPES: H HEADER  D FORM LINE  T TRAILER                 *
      * AMOUNTS: RBD-AMOUNT-000 IS ROUNDED TO THE NEAREST 1,000 GBP    *
      * (THE REPORTING UNIT). RBD-AMOUNT-EXACT IS THE PENCE VALUE FOR  *
      * RECONCILIATION TO THE GENERAL LEDGER - IT IS NOT SUBMITTED.    *
      * ROUNDING IS PER LINE, SO THE SUM OF LINES MAY DIFFER FROM THE  *
      * ROUNDED TOTAL BY A FEW THOUSAND (BOE ACCEPTS THIS).            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1993-04-05 GHT  ORIGINAL VERSION (PRINT ONLY)                  *
      * 2006-07-03 SAT  FILE OUTPUT FOR ELECTRONIC SUBMISSION          *
      * 2011-01-17 JNB  LOAN LINES FROM LOAN MASTER (LENDPRO RETIRED)  *
      *================================================================*
       01  RBD-DETAIL-RECORD.
           05  RBD-REC-TYPE                PIC X(01).
               88  RBD-HEADER                  VALUE 'H'.
               88  RBD-DETAIL                  VALUE 'D'.
               88  RBD-TRAILER                 VALUE 'T'.
           05  RBD-FORM-ID                 PIC X(04).
           05  RBD-LINE-CD                 PIC X(04).
           05  RBD-SECTOR-CD               PIC X(01).
               88  RBD-SECTOR-PERSONAL         VALUE 'P'.
               88  RBD-SECTOR-BUSINESS         VALUE 'B'.
               88  RBD-SECTOR-ALL              VALUE 'A'.
           05  RBD-LINE-DESC               PIC X(40).
           05  RBD-ACCOUNT-COUNT           PIC 9(09).
           05  RBD-AMOUNT-000              PIC S9(11)
                                           SIGN LEADING SEPARATE.
           05  RBD-AMOUNT-EXACT            PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
           05  FILLER                      PIC X(11).
      *
       01  RBD-HEADER-RECORD REDEFINES RBD-DETAIL-RECORD.
           05  RBH-REC-TYPE                PIC X(01).
           05  RBH-FORM-ID                 PIC X(04).
           05  RBH-REPORTER-CD             PIC X(07).
           05  RBH-PERIOD-END              PIC 9(08).
           05  RBH-RUN-DATE                PIC 9(08).
           05  RBH-CCY-CD                  PIC X(03).
           05  RBH-UNITS                   PIC X(04).
           05  FILLER                      PIC X(65).
      *
       01  RBD-TRAILER-RECORD REDEFINES RBD-DETAIL-RECORD.
           05  RBT-REC-TYPE                PIC X(01).
           05  RBT-FORM-ID                 PIC X(04).
           05  RBT-LINE-COUNT              PIC 9(05).
           05  RBT-TOTAL-000               PIC S9(13)
                                           SIGN LEADING SEPARATE.
           05  RBT-TOTAL-EXACT             PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
           05  FILLER                      PIC X(58).
