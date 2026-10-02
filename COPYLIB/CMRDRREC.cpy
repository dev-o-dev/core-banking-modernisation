      *================================================================*
      * COPYBOOK : CMRDRREC                                            *
      * TITLE    : COMPLAINTS REDRESS INTERFACE - CMS TO HOST          *
      *            (APPROVED REDRESS), HOST TO CMS (RESPONSE, LETTERS) *
      * DATASETS : MBK.PRD.CMPL.REDRESS.APPROVED(0)   FB LRECL=300     *
      *            MBK.PRD.CMPL.REDRESS.RESPONSE(+1)  FB LRECL=100     *
      * SPEC     : CMS-IF-07 V1.3 (COMPLAINTS MANAGEMENT SYSTEM)       *
      * USED BY  : CMPRD010                                            *
      *----------------------------------------------------------------*
      * APPROVED REDRESS FILE: ONE 'H' HEADER, 'D' DETAILS SORTED BY   *
      * COMPLAINT REF + REDRESS SEQ, ONE 'T' TRAILER WITH THE DETAIL   *
      * COUNT AND THE REFUND / CONSEQUENTIAL LOSS TOTALS. AMOUNTS ARE  *
      * UNSIGNED ZONED DECIMAL, 2DP. DATES CCYYMMDD.                   *
      *                                                                *
      * LETTER EXTRACT (RDR-LETTER, FB LRECL=350): ONE PER ITEM PAID,  *
      * TO THE PRINT VENDOR AND ALSO LOADED BACK INTO CMS AS EVIDENCE  *
      * OF PAYMENT ON THE COMPLAINT FILE.                              *
      *   MBK.PRD.CMPL.REDRESS.LETTERS(+1)                             *
      *                                                                *
      * RESPONSE FILE: ONE RECORD PER DETAIL, STATUS 'P' PAID OR 'R'   *
      * REJECTED WITH A REASON CODE (RS-REASON):                       *
      *   DUPL  SAME COMPLAINT REF + SEQ AS THE PREVIOUS ITEM          *
      *   AMNT  AMOUNTS NOT NUMERIC, OR NOTHING TO PAY                 *
      *   LIMT  TOTAL ABOVE WS-MAX-REDRESS (PAID BY CHAPS BY           *
      *         COMPLAINTS FINANCE, NOT THROUGH THIS PROGRAM)          *
      *   LDTE  LOSS DATE INVALID, IN THE FUTURE, OR OVER 15 YEARS AGO *
      *   APPR  NO APPROVER, APPROVER = HANDLER (FOUR EYES), OR        *
      *         APPROVAL DATE AFTER THE BUSINESS DATE                  *
      *   AUTH  TOTAL ABOVE WS-SECOND-APPROVAL-LIMIT WITHOUT A         *
      *         DIFFERENT SECOND APPROVER (COMPLAINTS MANDATE 2016)    *
      *   NACC  ACCOUNT NOT ON THE MASTER                              *
      *   ACLS  ACCOUNT CLOSED OR CLOSING (CMS PAYS BY CHEQUE)         *
      *   ARST  ACCOUNT FROZEN OR CREDITS RESTRICTED                   *
      *   CCYX  ACCOUNT NOT STERLING                                   *
      *   CIFM  CIF ON THE ITEM IS NOT THE ACCOUNT HOLDER              *
      *   NCIF  CUSTOMER NOT ON THE CIF                                *
      *   DCSD  CUSTOMER DECEASED (BEREAVEMENT TEAM PAYS THE ESTATE)   *
      *   NPRD  NO PRODUCT PARAMETERS (TAX GL UNKNOWN)                 *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2016-02-08 PDQ  ORIGINAL VERSION                               *
      * 2016-06-20 PDQ  RDID-INT-ON-CONSEQ                             *
      * 2017-03-27 PDQ  RDID-APPROVER-2-ID (IA-17-04)                  *
      *================================================================*
       01  RDR-REDRESS-RECORD.
           05  RDI-REC-TYPE                PIC X(01).
               88  RDI-HEADER                  VALUE 'H'.
               88  RDI-DETAIL                  VALUE 'D'.
               88  RDI-TRAILER                 VALUE 'T'.
           05  RDI-BODY                    PIC X(299).
           05  RDI-HDR REDEFINES RDI-BODY.
               10  RDIH-FILE-ID            PIC X(08).
                   88  RDIH-VALID-FILE-ID      VALUE 'CMPRDRS '.
               10  RDIH-EXTRACT-DATE       PIC 9(08).
               10  RDIH-FILE-SEQ-NO        PIC 9(06).
               10  RDIH-SOURCE-SYS         PIC X(08).
               10  FILLER                  PIC X(269).
           05  RDI-DTL REDEFINES RDI-BODY.
               10  RDID-ITEM-KEY.
                   15  RDID-COMPLAINT-REF  PIC X(12).
                   15  RDID-REDRESS-SEQ    PIC 9(02).
               10  RDID-SORT-CODE          PIC X(06).
               10  RDID-ACCT-NO            PIC X(08).
               10  RDID-CIF-NO             PIC X(12).
               10  RDID-REFUND-AMT         PIC 9(09)V99.
               10  RDID-CONSEQ-LOSS-AMT    PIC 9(09)V99.
               10  RDID-INT-IND            PIC X(01).
                   88  RDID-INT-ON-REFUND      VALUE 'Y'.
               10  RDID-INT-ON-CONSEQ      PIC X(01).
                   88  RDID-INT-ON-CONSEQ-LOSS VALUE 'Y'.
               10  RDID-LOSS-DATE          PIC 9(08).
               10  RDID-LOSS-DATE-X REDEFINES RDID-LOSS-DATE.
                   15  RDID-LOSS-CCYY      PIC 9(04).
                   15  RDID-LOSS-MM        PIC 9(02).
                   15  RDID-LOSS-DD        PIC 9(02).
               10  RDID-ROOT-CAUSE         PIC X(04).
               10  RDID-PRODUCT-AREA       PIC X(04).
               10  RDID-FOS-REF            PIC X(12).
               10  RDID-HANDLER-ID         PIC X(08).
               10  RDID-APPROVER-ID        PIC X(08).
               10  RDID-APPROVER-2-ID      PIC X(08).
               10  RDID-APPROVED-DATE      PIC 9(08).
               10  FILLER                  PIC X(167).
           05  RDI-TRL REDEFINES RDI-BODY.
               10  RDIT-RECORD-COUNT       PIC 9(09).
               10  RDIT-TOTAL-REFUND       PIC 9(13)V99.
               10  RDIT-TOTAL-CONSEQ       PIC 9(13)V99.
               10  FILLER                  PIC X(260).
      *
       01  RDR-RESPONSE.
           05  RS-COMPLAINT-REF            PIC X(12).
           05  RS-REDRESS-SEQ              PIC 9(02).
           05  RS-STATUS                   PIC X(01).
               88  RS-PAID                     VALUE 'P'.
               88  RS-REJECTED                 VALUE 'R'.
           05  RS-REASON                   PIC X(04).
           05  RS-PAY-DATE                 PIC 9(08).
           05  RS-NET-PAID                 PIC 9(09)V99.
           05  RS-INT-GROSS                PIC 9(09)V99.
           05  RS-INT-TAX                  PIC 9(07)V99.
           05  RS-TEXT                     PIC X(40).
           05  FILLER                      PIC X(02).
      *
       01  RDR-LETTER.
           05  LT-LETTER-CD                PIC X(04).
           05  LT-TEMPLATE-ID              PIC X(08).
           05  LT-ISSUE-DATE               PIC 9(08).
           05  LT-CIF-NO                   PIC X(12).
           05  LT-TITLE                    PIC X(06).
           05  LT-FIRST-NM                 PIC X(30).
           05  LT-LAST-NM                  PIC X(30).
           05  LT-ADDR-LINE1               PIC X(40).
           05  LT-ADDR-LINE2               PIC X(40).
           05  LT-TOWN                     PIC X(30).
           05  LT-POST-CD                  PIC X(10).
           05  LT-COMPLAINT-REF            PIC X(12).
           05  LT-FOS-REF                  PIC X(12).
           05  LT-SORT-CODE                PIC X(06).
           05  LT-ACCT-LAST4               PIC X(04).
           05  LT-REFUND                   PIC 9(09)V99.
           05  LT-CONSEQ                   PIC 9(09)V99.
           05  LT-INT-GROSS                PIC 9(09)V99.
           05  LT-INT-TAX                  PIC 9(09)V99.
           05  LT-NET-TOTAL                PIC 9(09)V99.
           05  LT-LOSS-DATE                PIC 9(08).
           05  LT-PAY-DATE                 PIC 9(08).
           05  LT-INT-DAYS                 PIC 9(05).
           05  FILLER                      PIC X(22).
