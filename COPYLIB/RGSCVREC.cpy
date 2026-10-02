      *================================================================*
      * COPYBOOK : RGSCVREC                                            *
      * TITLE    : FSCS SINGLE CUSTOMER VIEW (SCV) FILE RECORDS        *
      * DATASET  : MBK.PRD.REG.SCV(+1)  RECFM=FB LRECL=400             *
      *----------------------------------------------------------------*
      * FSA COBS / COMP 17 (PS09/11): A DEPOSIT TAKER MUST BE ABLE TO  *
      * PRODUCE A SINGLE CUSTOMER VIEW OF EVERY ELIGIBLE DEPOSITOR     *
      * WITHIN 72 HOURS. THIS LAYOUT FOLLOWS THE FSCS SCV FILE         *
      * SPECIFICATION V1.0 (DEC 2009) RECORD TYPES:                    *
      *   'H' FILE HEADER    'C' CUSTOMER   'A' ACCOUNT                *
      *   'K' CONTACT        'T' FILE TRAILER                          *
      * FSCS WANTS DELIMITED TEXT - PTX CONVERTS THIS FIXED FORMAT TO  *
      * PIPE-DELIMITED ON TRANSMISSION (PROFILE MBKSCV01).             *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-01-04 JNB  ORIGINAL VERSION (SCV PROJECT - FSA DEADLINE   *
      *                 31 DEC 2010)                                   *
      *================================================================*
       01  SCV-RECORD.
           05  SCV-REC-TYPE                PIC X(01).
               88  SCV-HEADER                  VALUE 'H'.
               88  SCV-CUSTOMER                VALUE 'C'.
               88  SCV-ACCOUNT                 VALUE 'A'.
               88  SCV-CONTACT                 VALUE 'K'.
               88  SCV-TRAILER                 VALUE 'T'.
           05  SCV-SCV-ID                  PIC X(12).
           05  SCV-BODY                    PIC X(387).
      *
           05  SCV-HDR REDEFINES SCV-BODY.
               10  SCVH-FIRM-FRN           PIC X(06).
               10  SCVH-FIRM-NAME          PIC X(40).
               10  SCVH-EXTRACT-DATE       PIC 9(08).
               10  SCVH-EXTRACT-TIME       PIC 9(06).
               10  SCVH-BALANCE-DATE       PIC 9(08).
               10  SCVH-SPEC-VERSION       PIC X(05).
               10  SCVH-COMP-LIMIT         PIC 9(07)V99.
               10  FILLER                  PIC X(305).
      *
           05  SCV-CUST REDEFINES SCV-BODY.
               10  SCVC-TITLE              PIC X(06).
               10  SCVC-FIRST-NAME         PIC X(40).
               10  SCVC-MIDDLE-NAME        PIC X(40).
               10  SCVC-SURNAME            PIC X(40).
               10  SCVC-COMPANY-NAME       PIC X(80).
               10  SCVC-DOB                PIC 9(08).
               10  SCVC-NINO               PIC X(13).
               10  SCVC-CUST-TYPE          PIC X(01).
               10  SCVC-ACCOUNT-COUNT      PIC 9(03).
               10  SCVC-AGG-BALANCE        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  SCVC-COMPENSATABLE      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  SCVC-EXCL-IND           PIC X(01).
               10  SCVC-EXCL-REASON        PIC X(04).
               10  SCVC-DQ-FLAGS           PIC X(10).
               10  SCVC-KYC-STATUS         PIC X(01).
               10  FILLER                  PIC X(108).
      *
           05  SCV-ACCT REDEFINES SCV-BODY.
               10  SCVA-SORT-CODE          PIC X(06).
               10  SCVA-ACCT-NO            PIC X(08).
               10  SCVA-IBAN               PIC X(34).
               10  SCVA-PRODUCT-TYPE       PIC X(04).
               10  SCVA-PRODUCT-NAME       PIC X(40).
               10  SCVA-ACCT-STATUS        PIC X(01).
               10  SCVA-HOLDER-COUNT       PIC 9(02).
               10  SCVA-CCY-CD             PIC X(03).
               10  SCVA-LEDGER-BAL         PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  SCVA-ACCRUED-INT        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  SCVA-ELIGIBLE-BAL       PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  SCVA-BENEFICIARY-IND    PIC X(01).
               10  SCVA-LEGAL-HOLD-IND     PIC X(01).
               10  SCVA-DORMANT-IND        PIC X(01).
               10  FILLER                  PIC X(238).
      *
           05  SCV-CONT REDEFINES SCV-BODY.
               10  SCVK-ADDR-LINE1         PIC X(60).
               10  SCVK-ADDR-LINE2         PIC X(60).
               10  SCVK-TOWN               PIC X(40).
               10  SCVK-COUNTY             PIC X(40).
               10  SCVK-POST-CD            PIC X(10).
               10  SCVK-CTRY-CD            PIC X(02).
               10  SCVK-HOME-TEL           PIC X(20).
               10  SCVK-MOBILE-TEL         PIC X(20).
               10  SCVK-EMAIL              PIC X(80).
               10  FILLER                  PIC X(55).
      *
           05  SCV-TRL REDEFINES SCV-BODY.
               10  SCVT-CUSTOMER-COUNT     PIC 9(09).
               10  SCVT-ACCOUNT-COUNT      PIC 9(09).
               10  SCVT-RECORD-COUNT       PIC 9(09).
               10  SCVT-TOTAL-BALANCE      PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
               10  SCVT-TOTAL-COMPENSATION PIC S9(15)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(324).
