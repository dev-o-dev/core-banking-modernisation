      *================================================================*
      * COPYBOOK : PRCHGREC                                            *
      * TITLE    : PRODUCT PARAMETER CHANGE CARD                       *
      * DATASET  : MBK.PRD.PRD.CHANGES(0)  RECFM=FB LRECL=120          *
      *            KEYED BY PRODUCT CONTROL ON ISPF PANEL MBKPRD1      *
      *            (ONE CARD PER FIELD CHANGE - FORM PC/17)            *
      *----------------------------------------------------------------*
      * A CHANGE SET IS A GROUP OF CARDS WITH THE SAME CHANGE REF.     *
      * EVERY CARD IN THE SET MUST PASS VALIDATION OR THE WHOLE SET IS *
      * REJECTED. THE SET IS APPLIED ON ITS EFFECTIVE DATE - CARDS     *
      * WITH A FUTURE DATE ARE CARRIED FORWARD TO THE NEXT RUN.        *
      *                                                                *
      * ACTIONS                                                        *
      *   A  ADD A NEW PRODUCT (FIELD NAME 'PRODUCT', VALUE = SHORT    *
      *      NAME). FIELDS ARE THEN SET BY 'U' CARDS IN THE SAME SET.  *
      *   U  UPDATE ONE FIELD                                          *
      *   W  WITHDRAW THE PRODUCT (STATUS 'W')                         *
      *   C  CLOSE TO NEW BUSINESS (STATUS 'N')                        *
      *                                                                *
      * FIELD NAMES ARE THE PRDPARRC NAMES WITHOUT THE 'PRD-' PREFIX,  *
      * WITH A TIER / FEE NUMBER WHERE THE FIELD OCCURS, E.G.          *
      *   CR-TIER-RATE(3)        = PRD-CR-TIER-RATE (3)                *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1991-05-20 RJH  ORIGINAL VERSION                               *
      * 2007-02-12 JNB  CHANGE SETS AND FOUR EYES                      *
      *================================================================*
       01  PCH-CHANGE-CARD.
           05  PCH-CHANGE-REF              PIC X(12).
           05  PCH-CARD-SEQ                PIC 9(03).
           05  PCH-PROD-CD                 PIC X(10).
           05  PCH-ACTION                  PIC X(01).
               88  PCH-ADD                     VALUE 'A'.
               88  PCH-UPDATE                  VALUE 'U'.
               88  PCH-WITHDRAW                VALUE 'W'.
               88  PCH-CLOSE-TO-NEW            VALUE 'C'.
           05  PCH-FIELD-NAME              PIC X(24).
           05  PCH-OCCURRENCE              PIC 9(01).
           05  PCH-NEW-VALUE               PIC X(30).
           05  PCH-EFFECTIVE-DATE          PIC 9(08).
           05  PCH-REQUESTED-BY            PIC X(08).
           05  PCH-APPROVED-BY             PIC X(08).
           05  FILLER                      PIC X(15).
