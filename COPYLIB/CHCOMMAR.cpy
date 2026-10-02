      *================================================================*
      * COPYBOOK : CHCOMMAR                                            *
      * TITLE    : CHANNEL SERVICE INTERFACE COMMAREA                  *
      * LENGTH   : 4000 BYTES                                          *
      *----------------------------------------------------------------*
      * ENTRY POINT FOR ALL NON-BRANCH CHANNELS (IBANK, TELE, WAP).    *
      * CALLED AS A DPL PROGRAM (CHROUTER) EITHER:                     *
      *   - VIA CICS TRANSACTION GATEWAY ECI (MIRROR TRAN CHMI), OR    *
      *   - VIA THE MQ-CICS DPL BRIDGE (QUEUE MBK.CICS.BRIDGE.REQ)     *
      *                                                                *
      * CHANNEL-SIDE JAVA RECORD CLASSES ARE GENERATED FROM THIS       *
      * COPYBOOK WITH THE J2C WIZARD - REGENERATE IF IT CHANGES.       *
      *                                                                *
      * NUMERIC FIELDS PASSED TO/FROM CHANNELS ARE ZONED WITH A        *
      * LEADING SEPARATE SIGN (E.G. '+000000000123456' = 1234.56) SO   *
      * THE CHANNELS DO NOT HAVE TO UNPACK COMP-3.                     *
      *----------------------------------------------------------------*
      * RETURN CODES (CH-RETURN-CD)                                    *
      *   0000 OK     0004 WARNING   0008 BUSINESS REJECT              *
      *   0012 SYSTEM ERROR (RETRY LATER)   0016 INVALID REQUEST       *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2000-06-19 DKP  ORIGINAL VERSION (IBANK PHASE 1 - ENQUIRIES)   *
      * 2001-11-05 DKP  MEMOPOST SERVICE                               *
      * 2008-05-27 AOS  PAYINIT SERVICE (FASTER PAYMENTS)              *
      * 2009-10-02 JNB  MORTINQ PLACEHOLDER (MORTGAGE SYSTEM PROJECT)  *
      *================================================================*
       01  CH-COMMAREA.
           05  CH-HEADER.
               10  CH-EYECATCHER           PIC X(04).
                   88  CH-EYECATCHER-OK        VALUE 'MBKC'.
               10  CH-VERSION              PIC X(02).
               10  CH-SERVICE-CD           PIC X(08).
                   88  CH-SVC-CIFINQ           VALUE 'CIFINQ  '.
                   88  CH-SVC-ACCTINQ          VALUE 'ACCTINQ '.
                   88  CH-SVC-ACCTLIST         VALUE 'ACCTLIST'.
                   88  CH-SVC-MEMOPOST         VALUE 'MEMOPOST'.
                   88  CH-SVC-PAYINIT          VALUE 'PAYINIT '.
                   88  CH-SVC-MORTINQ          VALUE 'MORTINQ '.
               10  CH-CHANNEL-CD           PIC X(06).
               10  CH-USER-ID              PIC X(08).
               10  CH-REQUEST-ID           PIC X(24).
               10  CH-REQUEST-TS           PIC X(26).
               10  CH-RETURN-CD            PIC X(04).
                   88  CH-RC-OK                VALUE '0000'.
                   88  CH-RC-WARNING           VALUE '0004'.
                   88  CH-RC-REJECT            VALUE '0008'.
                   88  CH-RC-SYSTEM-ERROR      VALUE '0012'.
                   88  CH-RC-INVALID           VALUE '0016'.
               10  CH-REASON-CD            PIC X(04).
               10  CH-MESSAGE-TEXT         PIC X(80).
           05  CH-DATA                     PIC X(3834).
      *----------------------------------------------------------------*
      * SERVICE CIFINQ - CUSTOMER ENQUIRY           PROGRAM CFINQ01    *
      * RESPONSE IS THE CIF RECORD IMAGE (SEE CFCUSREC). THE ONLY      *
      * PACKED FIELD IS CIF-ANN-INCOME.                                *
      *----------------------------------------------------------------*
           05  CH-CIFINQ-AREA REDEFINES CH-DATA.
               10  CHCI-REQ.
                   15  CHCI-CIF-NO         PIC X(12).
               10  CHCI-RESP.
                   15  CHCI-CUST-IMAGE     PIC X(700).
               10  FILLER                  PIC X(3122).
      *----------------------------------------------------------------*
      * SERVICE ACCTINQ - ACCOUNT BALANCE ENQUIRY   PROGRAM DDINQ01    *
      *----------------------------------------------------------------*
           05  CH-ACCTINQ-AREA REDEFINES CH-DATA.
               10  CHAI-REQ.
                   15  CHAI-SORT-CODE      PIC X(06).
                   15  CHAI-ACCT-NO        PIC X(08).
               10  CHAI-RESP.
                   15  CHAI-CIF-NO         PIC X(12).
                   15  CHAI-IBAN           PIC X(34).
                   15  CHAI-ACCT-TYPE-CD   PIC X(04).
                   15  CHAI-PROD-CD        PIC X(10).
                   15  CHAI-CCY-CD         PIC X(03).
                   15  CHAI-ACCT-STATUS    PIC X(01).
                   15  CHAI-LEDGER-BAL     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAI-AVAIL-BAL      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAI-OD-LIMIT       PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAI-HOLD-AMT       PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAI-INT-RATE-CR    PIC S9(03)V9(04)
                                           SIGN LEADING SEPARATE.
                   15  CHAI-OPEN-DATE      PIC 9(08).
                   15  CHAI-LAST-TXN-DATE  PIC 9(08).
                   15  CHAI-POST-RESTRICT  PIC X(01).
               10  FILLER                  PIC X(3667).
      *----------------------------------------------------------------*
      * SERVICE ACCTLIST - ACCOUNTS FOR A CUSTOMER  PROGRAM DDLST01    *
      * MAXIMUM 20 ACCOUNTS. CHAL-MORE-IND = 'Y' IF TRUNCATED.         *
      *----------------------------------------------------------------*
           05  CH-ACCTLIST-AREA REDEFINES CH-DATA.
               10  CHAL-REQ.
                   15  CHAL-CIF-NO         PIC X(12).
               10  CHAL-RESP.
                   15  CHAL-ACCT-COUNT     PIC 9(02).
                   15  CHAL-MORE-IND       PIC X(01).
                   15  CHAL-ACCT-ENTRY     OCCURS 20 TIMES.
                       20  CHAL-SORT-CODE  PIC X(06).
                       20  CHAL-ACCT-NO    PIC X(08).
                       20  CHAL-ACCT-TYPE  PIC X(04).
                       20  CHAL-PROD-CD    PIC X(10).
                       20  CHAL-STATUS     PIC X(01).
                       20  CHAL-LEDGER-BAL PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                       20  CHAL-AVAIL-BAL  PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(2599).
      *----------------------------------------------------------------*
      * SERVICE MEMOPOST - REAL-TIME MEMO POSTING   PROGRAM PSMEMO1    *
      * CHMP-OVERRIDE-IND = 'Y' PERMITS AN EXCESS OVER THE ARRANGED    *
      * LIMIT. HONOURED FOR CHANNEL 'BRANCH' ONLY.                     *
      *----------------------------------------------------------------*
           05  CH-MEMOPOST-AREA REDEFINES CH-DATA.
               10  CHMP-REQ.
                   15  CHMP-SORT-CODE      PIC X(06).
                   15  CHMP-ACCT-NO        PIC X(08).
                   15  CHMP-TXN-CODE       PIC X(04).
                   15  CHMP-DR-CR-IND      PIC X(01).
                   15  CHMP-AMOUNT         PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHMP-NARRATIVE-1    PIC X(35).
                   15  CHMP-NARRATIVE-2    PIC X(35).
                   15  CHMP-EXT-REF        PIC X(24).
                   15  CHMP-VALUE-DATE     PIC 9(08).
                   15  CHMP-OVERRIDE-IND   PIC X(01).
               10  CHMP-RESP.
                   15  CHMP-NEW-LEDGER-BAL PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHMP-NEW-AVAIL-BAL  PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHMP-POST-SEQ-NO    PIC 9(09).
               10  FILLER                  PIC X(3655).
      *----------------------------------------------------------------*
      * SERVICE PAYINIT - PAYMENT INITIATION        PROGRAM PYINIT1    *
      * SCHEMES: 'FPS ' FASTER PAYMENTS  'CHAP' CHAPS                  *
      *          'BACS' BACS (3-DAY)     'SWFT' INTERNATIONAL (SWIFT)  *
      * CHPI-EXT-REF IS THE CHANNEL PAYMENT REFERENCE (PAY_REF). A     *
      * RESUBMISSION WITH THE SAME REFERENCE RETURNS THE ORIGINAL      *
      * PAYMENT WITH REASON 'DUPL' AND IS NOT RE-DEBITED.              *
      *----------------------------------------------------------------*
           05  CH-PAYINIT-AREA REDEFINES CH-DATA.
               10  CHPI-REQ.
                   15  CHPI-EXT-REF        PIC X(18).
                   15  CHPI-SCHEME         PIC X(04).
                       88  CHPI-FPS            VALUE 'FPS '.
                       88  CHPI-CHAPS          VALUE 'CHAP'.
                       88  CHPI-BACS           VALUE 'BACS'.
                       88  CHPI-SWIFT          VALUE 'SWFT'.
                   15  CHPI-DR-SORT-CODE   PIC X(06).
                   15  CHPI-DR-ACCT-NO     PIC X(08).
                   15  CHPI-BENE-NAME      PIC X(35).
                   15  CHPI-BENE-SORT-CODE PIC X(06).
                   15  CHPI-BENE-ACCT-NO   PIC X(08).
                   15  CHPI-BENE-IBAN      PIC X(34).
                   15  CHPI-BENE-BIC       PIC X(11).
                   15  CHPI-BENE-CTRY-CD   PIC X(02).
                   15  CHPI-PAY-CCY-CD     PIC X(03).
                   15  CHPI-PAY-AMT        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHPI-FX-RATE        PIC 9(05)V9(06).
                   15  CHPI-DR-AMT         PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHPI-CHARGE-AMT     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHPI-CHARGE-BEARER  PIC X(03).
                   15  CHPI-REMIT-REF      PIC X(18).
                   15  CHPI-PURPOSE-CD     PIC X(04).
                   15  CHPI-VALUE-DATE     PIC 9(08).
               10  CHPI-RESP.
                   15  CHPI-HOST-PAY-ID    PIC X(16).
                   15  CHPI-PAY-STATUS     PIC X(03).
                   15  CHPI-SCHEME-REF     PIC X(35).
                   15  CHPI-NEW-AVAIL-BAL  PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(3537).
      *----------------------------------------------------------------*
      * SERVICE MORTINQ - MORTGAGE ENQUIRY          PROGRAM MGSTUB1    *
      * NOT IMPLEMENTED ON THIS PLATFORM - SEE MGSTUB1.                *
      *----------------------------------------------------------------*
           05  CH-MORTINQ-AREA REDEFINES CH-DATA.
               10  CHMG-REQ.
                   15  CHMG-MORT-ACCT-NO   PIC X(12).
               10  FILLER                  PIC X(3822).
