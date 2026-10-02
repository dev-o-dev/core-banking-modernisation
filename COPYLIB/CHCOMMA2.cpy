      *================================================================*
      * COPYBOOK : CHCOMMA2                                            *
      * TITLE    : CHANNEL SERVICE INTERFACE COMMAREA - PART 2         *
      *            (SERVICES ADDED AFTER COMMAREA VERSION 01)          *
      *----------------------------------------------------------------*
      * !!! MUST BE COPIED IMMEDIATELY AFTER CHCOMMAR !!!              *
      *     EVERY 05 LEVEL BELOW IS A REDEFINES OF CH-DATA AND ONLY    *
      *     COMPILES IF IT FOLLOWS THE LAST REDEFINES IN CHCOMMAR.     *
      *                                                                *
      * WHY A SECOND COPYBOOK: ADDING THESE SERVICES TO CHCOMMAR WOULD *
      * HAVE FORCED THE IBANK TEAM TO REGENERATE AND RETEST EVERY J2C  *
      * RECORD CLASS FOR THE EXISTING SERVICES. THE COMMAREA VERSION   *
      * STAYS '01' AND THE NEW SERVICES ARE ONLY VISIBLE TO PROGRAMS   *
      * THAT COPY THIS MEMBER (DESIGN NOTE CS-DN-2010-03).             *
      *                                                                *
      * THERE ARE NO 88-LEVELS FOR THE NEW SERVICE CODES ON            *
      * CH-SERVICE-CD (THEY CANNOT BE ADDED FROM HERE). COMPARE THE    *
      * LITERAL, E.G.  IF CH-SERVICE-CD = 'SOMAINT '.                  *
      *----------------------------------------------------------------*
      * SERVICES                                                       *
      *   SOMAINT  STANDING ORDER LIST/ENQUIRE/CREATE/AMEND/CANCEL     *
      *                                               PROGRAM SOMNT01  *
      *   DDMANDT  DIRECT DEBIT MANDATE LIST / CANCEL PROGRAM DMSVC01  *
      *   ACCTOPEN ACCOUNT OPENING                    PROGRAM DDOPN01  *
      *   ACCTCLOS ACCOUNT CLOSURE QUOTE / REQUEST    PROGRAM DDCLS01  *
      *   ACCTHOLD HOLDS - PLACE / RELEASE / LIST     PROGRAM DDHLD01  *
      *   TXNHIST  TRANSACTION HISTORY (PAGED)        PROGRAM DDTXN01  *
      *   CIFMAINT CUSTOMER MAINTENANCE / NEW CUST    PROGRAM CFMNT01  *
      *   POSTREV  REVERSAL OF A SAME-DAY POSTING     PROGRAM PSREV01  *
      *   CARDAUTH DEBIT CARD AUTHORISE / REVERSE    PROGRAM CRDAUT01  *
      *   LOANENQ  PERSONAL LOAN ENQUIRY / QUOTE     PROGRAM LNSVC01   *
      *   FXQUOTE  FX RATE QUOTE / VALIDATE          PROGRAM FXSVC01   *
      *   CHQSTOP  CHEQUE STOP PLACE / REMOVE / LIST PROGRAM CQSTP01   *
      *   ODLIMIT  OVERDRAFT LIMIT ENQ / CHANGE / REMOVE               *
      *                                              PROGRAM ODLIM01   *
      *   STMTREQ  AD-HOC / DUPLICATE STATEMENT      PROGRAM STREQ01   *
      *   CUSTPREF CONTACT, MARKETING, E-STATEMENT   PROGRAM CFPRF01   *
      *   SWITCHRQ CASS SWITCH IN - REQUEST / ENQUIRE PROGRAM CSSVC01  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-03-08 AOS  ORIGINAL VERSION - SOMAINT, DDMANDT, TXNHIST   *
      * 2010-05-17 JNB  ACCTOPEN, ACCTCLOS, ACCTHOLD                   *
      * 2010-06-28 JNB  CIFMAINT, POSTREV                              *
      * 2012-02-13 JNB  CHQSTOP                                        *
      * 2012-04-23 PLW  ODLIMIT                                        *
      * 2012-06-11 JNB  STMTREQ                                        *
      * 2012-09-03 AOS  CUSTPREF                                       *
      *================================================================*
      *----------------------------------------------------------------*
      * SERVICE SOMAINT - STANDING ORDERS           PROGRAM SOMNT01    *
      * CHSO-FUNCTION  L LIST (25 PER CALL, RESTART FROM CHSO-SO-SEQ)  *
      *                I ENQUIRE ONE      C CREATE                     *
      *                A AMEND (AMOUNT / NEXT DATE / NAME / REF ONLY)  *
      *                X CANCEL                                        *
      * CHSO-NUM-PAYMENTS = 9999 MEANS UNTIL FURTHER NOTICE.           *
      *----------------------------------------------------------------*
           05  CH-SOMAINT-AREA REDEFINES CH-DATA.
               10  CHSO-REQ.
                   15  CHSO-FUNCTION       PIC X(01).
                       88  CHSO-LIST           VALUE 'L'.
                       88  CHSO-ENQUIRE        VALUE 'I'.
                       88  CHSO-CREATE         VALUE 'C'.
                       88  CHSO-AMEND          VALUE 'A'.
                       88  CHSO-CANCEL         VALUE 'X'.
                   15  CHSO-SORT-CODE      PIC X(06).
                   15  CHSO-ACCT-NO        PIC X(08).
                   15  CHSO-SO-SEQ         PIC 9(04).
                   15  CHSO-BENE-NAME      PIC X(35).
                   15  CHSO-BENE-SORT-CODE PIC X(06).
                   15  CHSO-BENE-ACCT-NO   PIC X(08).
                   15  CHSO-BENE-REF       PIC X(18).
                   15  CHSO-FREQ-CD        PIC X(02).
                   15  CHSO-FIRST-PAY-DATE PIC 9(08).
                   15  CHSO-FIRST-AMT      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHSO-REGULAR-AMT    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHSO-FINAL-AMT      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHSO-FINAL-PAY-DATE PIC 9(08).
                   15  CHSO-NUM-PAYMENTS   PIC 9(04).
                   15  CHSO-CANCEL-RSN     PIC X(04).
               10  CHSO-RESP.
                   15  CHSO-SO-COUNT       PIC 9(02).
                   15  CHSO-MORE-IND       PIC X(01).
                   15  CHSO-RESTART-SEQ    PIC 9(04).
                   15  CHSO-ENTRY          OCCURS 25 TIMES.
                       20  CHSOE-SO-SEQ    PIC 9(04).
                       20  CHSOE-STATUS    PIC X(01).
                       20  CHSOE-BENE-NAME PIC X(35).
                       20  CHSOE-BENE-SORT-CODE
                                           PIC X(06).
                       20  CHSOE-BENE-ACCT-NO
                                           PIC X(08).
                       20  CHSOE-BENE-REF  PIC X(18).
                       20  CHSOE-FREQ-CD   PIC X(02).
                       20  CHSOE-NEXT-DUE  PIC 9(08).
                       20  CHSOE-AMOUNT    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                       20  CHSOE-LAST-PAID PIC 9(08).
                       20  CHSOE-REMAINING PIC 9(04).
               10  FILLER                  PIC X(917).
      *----------------------------------------------------------------*
      * SERVICE DDMANDT - DIRECT DEBIT MANDATES      PROGRAM DMSVC01   *
      * CHDM-FUNCTION  L LIST (20 PER CALL, RESTART FROM CHDM-RESTART) *
      *                X CANCEL (ADDACS REASON 1 SENT BY DMADC010)     *
      *----------------------------------------------------------------*
           05  CH-DDMANDT-AREA REDEFINES CH-DATA.
               10  CHDM-REQ.
                   15  CHDM-FUNCTION       PIC X(01).
                       88  CHDM-LIST           VALUE 'L'.
                       88  CHDM-CANCEL         VALUE 'X'.
                   15  CHDM-SORT-CODE      PIC X(06).
                   15  CHDM-ACCT-NO        PIC X(08).
                   15  CHDM-ORIG-SUN       PIC X(06).
                   15  CHDM-PAYER-REF      PIC X(18).
                   15  CHDM-INCLUDE-CANC   PIC X(01).
                   15  CHDM-RESTART-KEY    PIC X(24).
               10  CHDM-RESP.
                   15  CHDM-COUNT          PIC 9(02).
                   15  CHDM-MORE-IND       PIC X(01).
                   15  CHDM-NEXT-RESTART   PIC X(24).
                   15  CHDM-ENTRY          OCCURS 20 TIMES.
                       20  CHDME-ORIG-SUN  PIC X(06).
                       20  CHDME-ORIG-NAME PIC X(18).
                       20  CHDME-PAYER-REF PIC X(18).
                       20  CHDME-STATUS    PIC X(01).
                       20  CHDME-SETUP-DATE
                                           PIC 9(08).
                       20  CHDME-LAST-COLL-DATE
                                           PIC 9(08).
                       20  CHDME-LAST-COLL-AMT
                                           PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(2243).
      *----------------------------------------------------------------*
      * SERVICE ACCTOPEN - ACCOUNT OPENING           PROGRAM DDOPN01   *
      * CHAO-SORT-CODE BLANK = USE THE PRODUCT DEFAULT SORT CODE.      *
      * CHAO-FUND-xxx OPTIONAL - INITIAL DEPOSIT IS TRANSFERRED FROM   *
      * THAT ACCOUNT (MUST BE HELD BY THE SAME CIF).                   *
      * CHAO-OD-LIMIT-REQ IS ONLY ACCEPTED FROM CHANNEL BRANCH; OTHER  *
      * CHANNELS GET A ZERO LIMIT AND REASON 'ODRF' (WARNING).         *
      *----------------------------------------------------------------*
           05  CH-ACCTOPEN-AREA REDEFINES CH-DATA.
               10  CHAO-REQ.
                   15  CHAO-CIF-NO         PIC X(12).
                   15  CHAO-PROD-CD        PIC X(10).
                   15  CHAO-SORT-CODE      PIC X(06).
                   15  CHAO-ACCT-NAME      PIC X(35).
                   15  CHAO-STMT-CYCLE     PIC X(01).
                   15  CHAO-STMT-DAY       PIC 9(02).
                   15  CHAO-OD-LIMIT-REQ   PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAO-INIT-DEPOSIT   PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAO-FUND-SORT-CODE PIC X(06).
                   15  CHAO-FUND-ACCT-NO   PIC X(08).
                   15  CHAO-JOINT-CIF-NO   PIC X(12).
                   15  CHAO-EXT-REF        PIC X(24).
               10  CHAO-RESP.
                   15  CHAO-NEW-SORT-CODE  PIC X(06).
                   15  CHAO-NEW-ACCT-NO    PIC X(08).
                   15  CHAO-NEW-IBAN       PIC X(34).
                   15  CHAO-OPEN-DATE      PIC 9(08).
                   15  CHAO-INT-RATE-CR    PIC S9(03)V9(04)
                                           SIGN LEADING SEPARATE.
                   15  CHAO-OD-LIMIT       PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAO-AVAIL-BAL      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(3590).
      *----------------------------------------------------------------*
      * SERVICE ACCTCLOS - ACCOUNT CLOSURE           PROGRAM DDCLS01   *
      * CHAC-FUNCTION  Q QUOTE (NO UPDATE)                             *
      *                C REQUEST CLOSURE - ACCOUNT SET TO PENDING      *
      *                  CLOSE; FINAL INTEREST, CHARGES AND TRANSFER   *
      *                  OF THE BALANCE ARE DONE BY DDCLS010 AT EOD    *
      *                U UNDO A PENDING CLOSURE (SAME DAY ONLY)        *
      *----------------------------------------------------------------*
           05  CH-ACCTCLOS-AREA REDEFINES CH-DATA.
               10  CHAC-REQ.
                   15  CHAC-FUNCTION       PIC X(01).
                       88  CHAC-QUOTE          VALUE 'Q'.
                       88  CHAC-CLOSE          VALUE 'C'.
                       88  CHAC-UNDO           VALUE 'U'.
                   15  CHAC-SORT-CODE      PIC X(06).
                   15  CHAC-ACCT-NO        PIC X(08).
                   15  CHAC-XFER-SORT-CODE PIC X(06).
                   15  CHAC-XFER-ACCT-NO   PIC X(08).
                   15  CHAC-CLOSE-RSN      PIC X(04).
               10  CHAC-RESP.
                   15  CHAC-LEDGER-BAL     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAC-ACCRUED-CR     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAC-ACCRUED-DR     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAC-TAX-ON-CR      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAC-PENDING-CHG    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAC-CLOSING-BAL    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAC-SO-COUNT       PIC 9(03).
                   15  CHAC-DD-COUNT       PIC 9(03).
                   15  CHAC-HOLD-COUNT     PIC 9(03).
                   15  CHAC-CLOSE-DATE     PIC 9(08).
               10  FILLER                  PIC X(3688).
      *----------------------------------------------------------------*
      * SERVICE ACCTHOLD - HOLDS ON FUNDS            PROGRAM DDHLD01   *
      * CHAH-FUNCTION  P PLACE  R RELEASE  L LIST (UP TO 15 ACTIVE)    *
      * HOLD REASONS  CHQ  UNCLEARED CHEQUE    LEGL LEGAL / GARNISHEE  *
      *               FRD  FRAUD INVESTIGATION CARD CARD AUTHORISATION *
      *               OTHR OTHER (NARRATIVE REQUIRED)                  *
      *----------------------------------------------------------------*
           05  CH-ACCTHOLD-AREA REDEFINES CH-DATA.
               10  CHAH-REQ.
                   15  CHAH-FUNCTION       PIC X(01).
                       88  CHAH-PLACE          VALUE 'P'.
                       88  CHAH-RELEASE        VALUE 'R'.
                       88  CHAH-LIST           VALUE 'L'.
                   15  CHAH-SORT-CODE      PIC X(06).
                   15  CHAH-ACCT-NO        PIC X(08).
                   15  CHAH-HOLD-SEQ       PIC 9(04).
                   15  CHAH-AMOUNT         PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAH-REASON-CD      PIC X(04).
                   15  CHAH-EXPIRY-DATE    PIC 9(08).
                   15  CHAH-NARRATIVE      PIC X(35).
               10  CHAH-RESP.
                   15  CHAH-NEW-HOLD-SEQ   PIC 9(04).
                   15  CHAH-NEW-AVAIL-BAL  PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAH-TOTAL-HOLD     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHAH-COUNT          PIC 9(02).
                   15  CHAH-ENTRY          OCCURS 15 TIMES.
                       20  CHAHE-HOLD-SEQ  PIC 9(04).
                       20  CHAHE-AMOUNT    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                       20  CHAHE-REASON-CD PIC X(04).
                       20  CHAHE-PLACED-DATE
                                           PIC 9(08).
                       20  CHAHE-EXPIRY-DATE
                                           PIC 9(08).
                       20  CHAHE-USER-ID   PIC X(08).
                       20  CHAHE-NARRATIVE PIC X(35).
                       20  CHAHE-STATUS    PIC X(01).
               10  FILLER                  PIC X(2454).
      *----------------------------------------------------------------*
      * SERVICE TXNHIST - TRANSACTION HISTORY        PROGRAM DDTXN01   *
      * RETURNS HARD-POSTED TRANSACTIONS FROM MBK.TXNHIST, NEWEST      *
      * FIRST, UP TO 25 PER CALL. FOR THE NEXT PAGE PASS BACK          *
      * CHTH-NEXT-DATE / CHTH-NEXT-SEQ AS CHTH-RESTART-DATE / SEQ.     *
      * TODAY'S MEMO POSTINGS ARE NOT INCLUDED (THEY ARE NOT IN DB2    *
      * UNTIL PSEOD010 HAS RUN) - THE CHANNEL SHOWS ITS OWN PENDING    *
      * ITEMS. CHTH-MAX-DAYS-BACK IS LIMITED TO 400 BY THE PROGRAM.    *
      *----------------------------------------------------------------*
           05  CH-TXNHIST-AREA REDEFINES CH-DATA.
               10  CHTH-REQ.
                   15  CHTH-SORT-CODE      PIC X(06).
                   15  CHTH-ACCT-NO        PIC X(08).
                   15  CHTH-FROM-DATE      PIC 9(08).
                   15  CHTH-TO-DATE        PIC 9(08).
                   15  CHTH-RESTART-DATE   PIC 9(08).
                   15  CHTH-RESTART-SEQ    PIC 9(09).
                   15  CHTH-MAX-ROWS       PIC 9(02).
                   15  CHTH-DR-CR-FILTER   PIC X(01).
               10  CHTH-RESP.
                   15  CHTH-COUNT          PIC 9(02).
                   15  CHTH-MORE-IND       PIC X(01).
                   15  CHTH-NEXT-DATE      PIC 9(08).
                   15  CHTH-NEXT-SEQ       PIC 9(09).
                   15  CHTH-ENTRY          OCCURS 25 TIMES.
                       20  CHTHE-POST-DATE PIC 9(08).
                       20  CHTHE-POST-SEQ  PIC 9(09).
                       20  CHTHE-TXN-CODE  PIC X(04).
                       20  CHTHE-DR-CR-IND PIC X(01).
                       20  CHTHE-AMOUNT    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                       20  CHTHE-RUNNING-BAL
                                           PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                       20  CHTHE-VALUE-DATE
                                           PIC 9(08).
                       20  CHTHE-NARRATIVE-1
                                           PIC X(35).
                       20  CHTHE-NARRATIVE-2
                                           PIC X(35).
                       20  CHTHE-CHANNEL-CD
                                           PIC X(06).
               10  FILLER                  PIC X(314).
      *----------------------------------------------------------------*
      * SERVICE CIFMAINT - CUSTOMER MAINTENANCE      PROGRAM CFMNT01   *
      * CHCM-FUNCTION  N NEW CUSTOMER (BRANCH ONLY)                    *
      *                A ADDRESS     C CONTACT     E EMPLOYMENT        *
      *                M MARKETING PREFERENCES                         *
      *                K KYC STATUS / RISK (BRANCH ONLY, USER IN       *
      *                  RACF GROUP BRKYC)                             *
      * FOR UPDATES CHCM-LAST-MAINT-DATE/TIME MUST BE THE VALUES THE   *
      * CHANNEL LAST READ (CIFINQ) - A MISMATCH IS REJECTED WITH       *
      * REASON 'STAL' (SOMEONE ELSE HAS CHANGED THE CUSTOMER).         *
      *----------------------------------------------------------------*
           05  CH-CIFMAINT-AREA REDEFINES CH-DATA.
               10  CHCM-REQ.
                   15  CHCM-CIF-NO         PIC X(12).
                   15  CHCM-FUNCTION       PIC X(01).
                       88  CHCM-NEW            VALUE 'N'.
                       88  CHCM-ADDRESS        VALUE 'A'.
                       88  CHCM-CONTACT        VALUE 'C'.
                       88  CHCM-EMPLOYMENT     VALUE 'E'.
                       88  CHCM-MARKETING      VALUE 'M'.
                       88  CHCM-KYC            VALUE 'K'.
                   15  CHCM-CUST-TYPE      PIC X(01).
                   15  CHCM-TITLE          PIC X(06).
                   15  CHCM-FIRST-NM       PIC X(40).
                   15  CHCM-MIDDLE-NM      PIC X(40).
                   15  CHCM-LAST-NM        PIC X(40).
                   15  CHCM-DOB            PIC 9(08).
                   15  CHCM-NAT-INS-NO     PIC X(13).
                   15  CHCM-ADDR-LINE1     PIC X(60).
                   15  CHCM-ADDR-LINE2     PIC X(60).
                   15  CHCM-TOWN           PIC X(40).
                   15  CHCM-COUNTY         PIC X(40).
                   15  CHCM-POST-CD        PIC X(10).
                   15  CHCM-CTRY-CD        PIC X(02).
                   15  CHCM-HOME-TEL       PIC X(20).
                   15  CHCM-MOBILE-TEL     PIC X(20).
                   15  CHCM-EMAIL-ADDR     PIC X(80).
                   15  CHCM-EMPL-STATUS    PIC X(20).
                   15  CHCM-OCCUPATION     PIC X(40).
                   15  CHCM-ANN-INCOME     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHCM-MKTG-EMAIL-IND PIC X(01).
                   15  CHCM-MKTG-POST-IND  PIC X(01).
                   15  CHCM-KYC-STATUS     PIC X(01).
                   15  CHCM-RISK-RATING    PIC X(01).
                   15  CHCM-HOME-BRANCH-SC PIC X(06).
                   15  CHCM-LAST-MAINT-DATE
                                           PIC 9(08).
                   15  CHCM-LAST-MAINT-TIME
                                           PIC 9(06).
               10  CHCM-RESP.
                   15  CHCM-NEW-CIF-NO     PIC X(12).
                   15  CHCM-NEW-MAINT-DATE PIC 9(08).
                   15  CHCM-NEW-MAINT-TIME PIC 9(06).
                   15  CHCM-DUP-WARNING-CIF
                                           PIC X(12).
               10  FILLER                  PIC X(3203).
      *----------------------------------------------------------------*
      * SERVICE POSTREV - REVERSE A SAME-DAY POSTING PROGRAM PSREV01   *
      * THE ORIGINAL IS IDENTIFIED BY ITS CHANNEL + EXTERNAL REFERENCE *
      * (PSTXREF). ONLY POSTINGS MADE TODAY (NOT YET HARD POSTED) CAN  *
      * BE REVERSED. THE REVERSAL IS ITSELF A MEMO POSTING WITH TXN    *
      * CODE 'REV ' AND THE OPPOSITE DEBIT/CREDIT INDICATOR.           *
      *----------------------------------------------------------------*
           05  CH-POSTREV-AREA REDEFINES CH-DATA.
               10  CHPR-REQ.
                   15  CHPR-SORT-CODE      PIC X(06).
                   15  CHPR-ACCT-NO        PIC X(08).
                   15  CHPR-ORIG-CHANNEL   PIC X(06).
                   15  CHPR-ORIG-EXT-REF   PIC X(24).
                   15  CHPR-REV-EXT-REF    PIC X(24).
                   15  CHPR-REASON-CD      PIC X(04).
                   15  CHPR-NARRATIVE      PIC X(35).
               10  CHPR-RESP.
                   15  CHPR-REV-SEQ-NO     PIC 9(09).
                   15  CHPR-ORIG-AMOUNT    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHPR-ORIG-DR-CR-IND PIC X(01).
                   15  CHPR-NEW-LEDGER-BAL PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHPR-NEW-AVAIL-BAL  PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(3669).
      *----------------------------------------------------------------*
      * SERVICE CARDAUTH - DEBIT CARD AUTHORISATION  PROGRAM CRDAUT01  *
      * CALLED ONLY BY THE CARD AUTHORISATION GATEWAY (CHANNEL         *
      * 'CARDS '). CHCA-FUNCTION  A AUTHORISE (PLACE A CARD HOLD)      *
      *                           R REVERSE   (RELEASE THE HOLD)       *
      * ISO 8583-STYLE RESPONSE CODES IN CHCA-RESPONSE-CD:             *
      *   00 APPROVED   51 INSUFFICIENT FUNDS   62 RESTRICTED ACCOUNT  *
      *   14 INVALID ACCOUNT   57 NOT PERMITTED  91 HOST UNAVAILABLE   *
      *   12 INVALID TRANSACTION (BAD REQUEST / UNKNOWN AUTH REF)      *
      * 2010-08-02 JNB  ADDED (CARD HOLDS MOVED FROM THE CARD          *
      *                 PROCESSOR'S SHADOW BALANCE TO THE HOST)        *
      *----------------------------------------------------------------*
           05  CH-CARDAUTH-AREA REDEFINES CH-DATA.
               10  CHCA-REQ.
                   15  CHCA-FUNCTION       PIC X(01).
                       88  CHCA-AUTHORISE      VALUE 'A'.
                       88  CHCA-REVERSE        VALUE 'R'.
                   15  CHCA-CARD-TOKEN     PIC X(19).
                   15  CHCA-SORT-CODE      PIC X(06).
                   15  CHCA-ACCT-NO        PIC X(08).
                   15  CHCA-AMOUNT         PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHCA-MERCHANT-NAME  PIC X(25).
                   15  CHCA-MCC            PIC X(04).
                   15  CHCA-AUTH-REF       PIC X(12).
                   15  CHCA-HOLD-DAYS      PIC 9(02).
                   15  CHCA-CASH-IND       PIC X(01).
               10  CHCA-RESP.
                   15  CHCA-RESPONSE-CD    PIC X(02).
                   15  CHCA-APPROVAL-CD    PIC X(06).
                   15  CHCA-HOLD-SEQ       PIC 9(04).
                   15  CHCA-AVAIL-BAL      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(3712).
      *----------------------------------------------------------------*
      * SERVICE LOANENQ - PERSONAL LOAN ENQUIRY   PROGRAM LNSVC01      *
      * CHLN-FUNCTION  E ENQUIRY - DETAILS + NEXT 12 INSTALMENTS       *
      *                Q SETTLEMENT QUOTE AS AT CHLN-SETTLE-DATE       *
      *                  (DEFAULT TODAY) - RECORDED ON THE LOAN        *
      *                L LIST LOANS FOR CHLN-CIF-NO (UP TO 10)         *
      * 2010-10-04 JNB  ADDED                                          *
      *----------------------------------------------------------------*
           05  CH-LOANENQ-AREA REDEFINES CH-DATA.
               10  CHLN-REQ.
                   15  CHLN-FUNCTION       PIC X(01).
                       88  CHLN-ENQUIRY        VALUE 'E'.
                       88  CHLN-QUOTE          VALUE 'Q'.
                       88  CHLN-LIST           VALUE 'L'.
                   15  CHLN-LOAN-NO        PIC X(10).
                   15  CHLN-CIF-NO         PIC X(12).
                   15  CHLN-SETTLE-DATE    PIC 9(08).
               10  CHLN-DETAIL.
                   15  CHLN-STATUS         PIC X(01).
                   15  CHLN-PROD-CD        PIC X(10).
                   15  CHLN-ADVANCE-AMT    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-ADVANCE-DATE   PIC 9(08).
                   15  CHLN-TERM-MONTHS    PIC 9(03).
                   15  CHLN-NOMINAL-RATE   PIC 9(03)V9(04).
                   15  CHLN-APR            PIC 9(03)V9(01).
                   15  CHLN-MONTHLY-PMT    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-PRINCIPAL      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-ACCRUED-INT    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-FEES           PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-ARREARS        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-MISSED-COUNT   PIC 9(03).
                   15  CHLN-NEXT-DUE-DATE  PIC 9(08).
                   15  CHLN-PMTS-REMAINING PIC 9(03).
                   15  CHLN-FINAL-DUE-DATE PIC 9(08).
                   15  CHLN-REPAY-SORT-CODE PIC X(06).
                   15  CHLN-REPAY-ACCT-NO  PIC X(08).
                   15  CHLN-TOTAL-INT-PAID PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-FORBEARANCE-IND PIC X(01).
               10  CHLN-QUOTE-DATA.
                   15  CHLN-QT-AMOUNT      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-QT-INT-TO-DATE PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-QT-EXTRA-INT   PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLN-QT-VALID-TO    PIC 9(08).
               10  CHLN-SCHED-COUNT        PIC 9(02).
               10  CHLN-SCHED              OCCURS 12 TIMES.
                   15  CHLNS-DUE-DATE      PIC 9(08).
                   15  CHLNS-PAYMENT       PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLNS-INTEREST      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLNS-PRINCIPAL     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLNS-BALANCE       PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  CHLN-LIST-COUNT         PIC 9(02).
               10  CHLN-LIST-ENTRY         OCCURS 10 TIMES.
                   15  CHLNL-LOAN-NO       PIC X(10).
                   15  CHLNL-STATUS        PIC X(01).
                   15  CHLNL-PROD-CD       PIC X(10).
                   15  CHLNL-PRINCIPAL     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHLNL-NEXT-DUE-DATE PIC 9(08).
                   15  CHLNL-ARREARS       PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(2087).
      *----------------------------------------------------------------*
      * SERVICE FXQUOTE - FX RATE QUOTE / VALIDATE  PROGRAM FXSVC01    *
      * CHFX-FUNCTION  Q QUOTE - CUSTOMER RATE FOR CCY / DIRECTION     *
      *                  AND AMOUNT (GBP OR CURRENCY - ONE OF THEM)    *
      *                V VALIDATE - IS CHFX-CHANNEL-RATE WITHIN        *
      *                  TOLERANCE OF OUR RATE (PYINIT1 FOR SWIFT)     *
      *                L LIST - TIER 1 BUY/SELL FOR ALL CURRENCIES     *
      * CHFX-DIRECTION  S BANK SELLS CCY (CUSTOMER PAYS ABROAD)        *
      *                 B BANK BUYS CCY  (CUSTOMER RECEIVES)           *
      * 2011-05-09 JNB  ADDED                                          *
      *----------------------------------------------------------------*
           05  CH-FXQUOTE-AREA REDEFINES CH-DATA.
               10  CHFX-REQ.
                   15  CHFX-FUNCTION       PIC X(01).
                       88  CHFX-QUOTE          VALUE 'Q'.
                       88  CHFX-VALIDATE       VALUE 'V'.
                       88  CHFX-LIST           VALUE 'L'.
                   15  CHFX-CCY-CD         PIC X(03).
                   15  CHFX-DIRECTION      PIC X(01).
                       88  CHFX-BANK-SELLS     VALUE 'S'.
                       88  CHFX-BANK-BUYS      VALUE 'B'.
                   15  CHFX-GBP-AMOUNT     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHFX-CCY-AMOUNT     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHFX-CHANNEL-RATE   PIC 9(05)V9(06).
               10  CHFX-RESP.
                   15  CHFX-MID-RATE       PIC 9(05)V9(06).
                   15  CHFX-CUST-RATE      PIC 9(05)V9(06).
                   15  CHFX-MARGIN-BP      PIC 9(04).
                   15  CHFX-RESULT-GBP     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHFX-RESULT-CCY     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHFX-RATE-DATE      PIC 9(08).
                   15  CHFX-RATE-TIME      PIC 9(06).
                   15  CHFX-RATE-STATUS    PIC X(01).
                   15  CHFX-DIFF-PCT       PIC S9(03)V9(04)
                                           SIGN LEADING SEPARATE.
                   15  CHFX-LIST-COUNT     PIC 9(02).
                   15  CHFX-LIST-ENTRY     OCCURS 40 TIMES.
                       20  CHFXL-CCY-CD    PIC X(03).
                       20  CHFXL-CCY-NAME  PIC X(20).
                       20  CHFXL-BUY-RATE  PIC 9(05)V9(06).
                       20  CHFXL-SELL-RATE PIC 9(05)V9(06).
                       20  CHFXL-STATUS    PIC X(01).
               10  FILLER                  PIC X(1863).
      *----------------------------------------------------------------*
      * SERVICE CHQSTOP - CHEQUE STOPS              PROGRAM CQSTP01    *
      * CHCS-FUNCTION  P PLACE A STOP (ONE CHEQUE OR A SERIAL RANGE)   *
      *                R REMOVE A STOP (KEY = CHCS-SERIAL-FROM)        *
      *                L LIST STOPS (30 PER CALL, RESTART FROM         *
      *                  CHCS-SERIAL-FROM). CHCS-INCL-INACTIVE = 'Y'   *
      *                  ALSO RETURNS REMOVED AND USED (HIT) STOPS     *
      * CHCS-SERIAL-TO ZERO / SPACES = SINGLE CHEQUE. AN AMOUNT MAY    *
      * ONLY BE GIVEN FOR A SINGLE CHEQUE.                             *
      * CHCS-STOP-RSN  LOST STOL (NO FEE)  DISP OTHR (STOP FEE)        *
      * 2012-02-13 JNB  ADDED                                          *
      *----------------------------------------------------------------*
           05  CH-CHQSTOP-AREA REDEFINES CH-DATA.
               10  CHCS-REQ.
                   15  CHCS-FUNCTION       PIC X(01).
                       88  CHCS-PLACE          VALUE 'P'.
                       88  CHCS-REMOVE         VALUE 'R'.
                       88  CHCS-LIST           VALUE 'L'.
                   15  CHCS-SORT-CODE      PIC X(06).
                   15  CHCS-ACCT-NO        PIC X(08).
                   15  CHCS-SERIAL-FROM    PIC X(06).
                   15  CHCS-SERIAL-TO      PIC X(06).
                   15  CHCS-AMOUNT         PIC S9(11)V99
                                           SIGN LEADING SEPARATE.
                   15  CHCS-PAYEE          PIC X(30).
                   15  CHCS-STOP-RSN       PIC X(04).
                       88  CHCS-RSN-LOST       VALUE 'LOST'.
                       88  CHCS-RSN-STOLEN     VALUE 'STOL'.
                       88  CHCS-RSN-DISPUTE    VALUE 'DISP'.
                       88  CHCS-RSN-OTHER      VALUE 'OTHR'.
                       88  CHCS-RSN-VALID      VALUE 'LOST' 'STOL'
                                                     'DISP' 'OTHR'.
                       88  CHCS-RSN-NO-FEE     VALUE 'LOST' 'STOL'.
                   15  CHCS-WAIVE-FEE-IND  PIC X(01).
                   15  CHCS-INCL-INACTIVE  PIC X(01).
               10  CHCS-RESP.
                   15  CHCS-FEE-CHARGED    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHCS-NEW-AVAIL-BAL  PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHCS-STOP-COUNT     PIC 9(02).
                   15  CHCS-MORE-IND       PIC X(01).
                   15  CHCS-RESTART-SERIAL PIC X(06).
                   15  CHCS-ENTRY          OCCURS 30 TIMES.
                       20  CHCSE-SERIAL-FROM
                                           PIC X(06).
                       20  CHCSE-SERIAL-TO PIC X(06).
                       20  CHCSE-AMOUNT    PIC S9(11)V99
                                           SIGN LEADING SEPARATE.
                       20  CHCSE-PAYEE     PIC X(30).
                       20  CHCSE-PLACED-DATE
                                           PIC 9(08).
                       20  CHCSE-PLACED-USER
                                           PIC X(08).
                       20  CHCSE-STATUS    PIC X(01).
                       20  CHCSE-HIT-DATE  PIC 9(08).
               10  FILLER                  PIC X(1286).
      *----------------------------------------------------------------*
      * SERVICE ODLIMIT - ARRANGED OVERDRAFT LIMIT  PROGRAM ODLIM01    *
      * CHOD-FUNCTION  E ENQUIRE - CURRENT LIMIT AND THE MAXIMUM THE   *
      *                  CUSTOMER IS ELIGIBLE FOR (NO UPDATE)          *
      *                C REQUEST A NEW LIMIT OR CHANGE THE LIMIT       *
      *                X REMOVE THE LIMIT                              *
      * CHOD-EXPIRY-DATE ZERO = DEFAULT TERM (12 MONTHS).              *
      * CHOD-MTHLY-COMMIT = CUSTOMER'S DECLARED MONTHLY CREDIT         *
      * COMMITMENTS (LOANS, CARDS, RENT/MORTGAGE) FOR AFFORDABILITY.   *
      * CHOD-AUTH-USER = SECOND USER (FOUR EYES) - BRANCH, LARGE LIMITS*
      * CHOD-DECISION  A APPROVED  R REFER TO BRANCH  D DECLINED       *
      * 2012-04-23 PLW  ADDED                                          *
      *----------------------------------------------------------------*
           05  CH-ODLIMIT-AREA REDEFINES CH-DATA.
               10  CHOD-REQ.
                   15  CHOD-FUNCTION       PIC X(01).
                       88  CHOD-ENQUIRE        VALUE 'E'.
                       88  CHOD-CHANGE         VALUE 'C'.
                       88  CHOD-REMOVE         VALUE 'X'.
                   15  CHOD-SORT-CODE      PIC X(06).
                   15  CHOD-ACCT-NO        PIC X(08).
                   15  CHOD-NEW-LIMIT      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-EXPIRY-DATE    PIC 9(08).
                   15  CHOD-MTHLY-COMMIT   PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-REASON-CD      PIC X(04).
                   15  CHOD-OVERRIDE-IND   PIC X(01).
                   15  CHOD-AUTH-USER      PIC X(08).
               10  CHOD-RESP.
                   15  CHOD-OLD-LIMIT      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-OLD-EXPIRY     PIC 9(08).
                   15  CHOD-CURR-LIMIT     PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-CURR-EXPIRY    PIC 9(08).
                   15  CHOD-MAX-ELIGIBLE   PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-PROD-MAX       PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-AVAIL-BAL      PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-SPEND-POWER    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-ARR-RATE       PIC S9(03)V9(04)
                                           SIGN LEADING SEPARATE.
                   15  CHOD-MONTHLY-FEE    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHOD-DECISION       PIC X(01).
                       88  CHOD-APPROVED       VALUE 'A'.
                       88  CHOD-REFERRED       VALUE 'R'.
                       88  CHOD-DECLINED       VALUE 'D'.
                   15  CHOD-DECISION-RSN   PIC X(04).
               10  FILLER                  PIC X(3625).
      *----------------------------------------------------------------*
      * SERVICE STMTREQ - AD-HOC / DUPLICATE STATEMENT PROGRAM STREQ01 *
      * CHSR-FUNCTION  A AD-HOC (INTERIM) STATEMENT FROM THE LAST      *
      *                  STATEMENT TO TODAY - NO FEE                   *
      *                D DUPLICATE OF A PREVIOUS STATEMENT - CHSR-     *
      *                  STMT-NO AND PERIOD REQUIRED - FEE CHARGED     *
      *                L LIST REQUESTS NOT YET PRODUCED (UP TO 10)     *
      *                X CANCEL A REQUEST NOT YET PRODUCED (FEE IS     *
      *                  REFUNDED)                                     *
      * REQUESTS ARE PRODUCED BY THE STATEMENT BATCH (STMT01) THE SAME *
      * NIGHT.                                                         *
      * 2012-06-11 JNB  ADDED                                          *
      *----------------------------------------------------------------*
           05  CH-STMTREQ-AREA REDEFINES CH-DATA.
               10  CHSR-REQ.
                   15  CHSR-FUNCTION       PIC X(01).
                       88  CHSR-ADHOC          VALUE 'A'.
                       88  CHSR-DUPLICATE      VALUE 'D'.
                       88  CHSR-LIST           VALUE 'L'.
                       88  CHSR-CANCEL         VALUE 'X'.
                   15  CHSR-SORT-CODE      PIC X(06).
                   15  CHSR-ACCT-NO        PIC X(08).
                   15  CHSR-STMT-NO        PIC 9(05).
                   15  CHSR-PERIOD-FROM    PIC 9(08).
                   15  CHSR-PERIOD-TO      PIC 9(08).
                   15  CHSR-WAIVE-FEE-IND  PIC X(01).
               10  CHSR-RESP.
                   15  CHSR-REQ-STMT-NO    PIC 9(05).
                   15  CHSR-REQ-FROM       PIC 9(08).
                   15  CHSR-REQ-TO         PIC 9(08).
                   15  CHSR-FEE-CHARGED    PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHSR-NEW-AVAIL-BAL  PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
                   15  CHSR-LAST-STMT-NO   PIC 9(05).
                   15  CHSR-LAST-STMT-DATE PIC 9(08).
                   15  CHSR-STMT-CYCLE     PIC X(01).
                   15  CHSR-DELIVERY       PIC X(01).
                       88  CHSR-PAPER          VALUE 'P'.
                       88  CHSR-ELECTRONIC     VALUE 'E'.
                   15  CHSR-LIST-COUNT     PIC 9(02).
                   15  CHSR-LIST-ENTRY     OCCURS 10 TIMES.
                       20  CHSRL-REQ-TYPE  PIC X(01).
                       20  CHSRL-STMT-NO   PIC 9(05).
                       20  CHSRL-FROM      PIC 9(08).
                       20  CHSRL-TO        PIC 9(08).
                       20  CHSRL-REQ-DATE  PIC 9(08).
                       20  CHSRL-CHANNEL   PIC X(06).
                       20  CHSRL-FEE-IND   PIC X(01).
               10  FILLER                  PIC X(3357).
      *----------------------------------------------------------------*
      * SERVICE CUSTPREF - CONTACT / MARKETING / E-STATEMENT           *
      *                    PREFERENCES                PROGRAM CFPRF01  *
      * CHCP-FUNCTION  E ENQUIRE - CURRENT PREFERENCES AND THE         *
      *                  STATEMENT SETTINGS OF EACH ACCOUNT (UP TO 20) *
      *                U UPDATE - ONLY THE GROUPS FLAGGED 'Y' IN       *
      *                  CHCP-UPD-CONTACT / -MKTG / -STMT              *
      * CHCP-CONSENT-SOURCE  WEB  CALL FORM BRCH - HOW THE CUSTOMER    *
      * GAVE OR WITHDREW CONSENT. CHCP-CONSENT-REF = SCRIPT / FORM ID. *
      * BOTH ARE REQUIRED FOR A MARKETING OR E-STATEMENT CHANGE.       *
      * CHCPA-RESULT-CD PER ACCOUNT: SPACES = UPDATED / UNCHANGED,     *
      * OTHERWISE THE REASON THE ACCOUNT WAS NOT UPDATED.              *
      * 2012-09-03 AOS  ADDED                                          *
      *----------------------------------------------------------------*
           05  CH-CUSTPREF-AREA REDEFINES CH-DATA.
               10  CHCP-REQ.
                   15  CHCP-FUNCTION       PIC X(01).
                       88  CHCP-ENQUIRE        VALUE 'E'.
                       88  CHCP-UPDATE         VALUE 'U'.
                   15  CHCP-CIF-NO         PIC X(12).
                   15  CHCP-UPD-CONTACT    PIC X(01).
                   15  CHCP-UPD-MKTG       PIC X(01).
                   15  CHCP-UPD-STMT       PIC X(01).
                   15  CHCP-HOME-TEL       PIC X(20).
                   15  CHCP-MOBILE-TEL     PIC X(20).
                   15  CHCP-EMAIL-ADDR     PIC X(80).
                   15  CHCP-MKTG-EMAIL-IND PIC X(01).
                   15  CHCP-MKTG-POST-IND  PIC X(01).
                   15  CHCP-ESTMT-IND      PIC X(01).
                   15  CHCP-CONSENT-SOURCE PIC X(04).
                   15  CHCP-CONSENT-REF    PIC X(20).
                   15  CHCP-ACCT-COUNT     PIC 9(02).
                   15  CHCP-ACCT-PREF      OCCURS 20 TIMES.
                       20  CHCPQ-SORT-CODE PIC X(06).
                       20  CHCPQ-ACCT-NO   PIC X(08).
                       20  CHCPQ-STMT-CYCLE
                                           PIC X(01).
                       20  CHCPQ-STMT-DAY  PIC 9(02).
               10  CHCP-RESP.
                   15  CHCP-CUST-NAME      PIC X(60).
                   15  CHCP-CUR-HOME-TEL   PIC X(20).
                   15  CHCP-CUR-MOBILE-TEL PIC X(20).
                   15  CHCP-CUR-EMAIL-ADDR PIC X(80).
                   15  CHCP-CUR-MKTG-EMAIL PIC X(01).
                   15  CHCP-CUR-MKTG-POST  PIC X(01).
                   15  CHCP-CUR-ESTMT      PIC X(01).
                   15  CHCP-LAST-MAINT-DATE
                                           PIC 9(08).
                   15  CHCP-LAST-MAINT-SRCE
                                           PIC X(04).
                   15  CHCP-CHANGE-COUNT   PIC 9(02).
                   15  CHCP-ACCT-RESP-COUNT
                                           PIC 9(02).
                   15  CHCP-ACCT-RESP      OCCURS 20 TIMES.
                       20  CHCPA-SORT-CODE PIC X(06).
                       20  CHCPA-ACCT-NO   PIC X(08).
                       20  CHCPA-PROD-CD   PIC X(10).
                       20  CHCPA-STMT-CYCLE
                                           PIC X(01).
                       20  CHCPA-STMT-DAY  PIC 9(02).
                       20  CHCPA-ESTMT-ALLOWED
                                           PIC X(01).
                       20  CHCPA-LAST-STMT-DATE
                                           PIC 9(08).
                       20  CHCPA-RESULT-CD PIC X(04).
               10  FILLER                  PIC X(2330).
      *----------------------------------------------------------------*
      * SERVICE SWITCHRQ - CASS SWITCH IN (NEW BANK)  PROGRAM CSSVC01  *
      * CHSW-FUNCTION  R REQUEST A SWITCH INTO A MEGABANK CURRENT      *
      *                  ACCOUNT FROM ANOTHER BANK (STATUS RQ, SENT BY *
      *                  CSSOU010 TONIGHT)                             *
      *                E ENQUIRE - SWITCHES FOR THE MEGABANK ACCOUNT   *
      *                X CANCEL A REQUEST NOT YET SENT TO CASS         *
      * CHSW-SWITCH-DATE MUST BE AT LEAST 7 WORKING DAYS AHEAD (CASS)  *
      * 2013-06-24 PDQ  ADDED                                          *
      *----------------------------------------------------------------*
           05  CH-SWITCHRQ-AREA REDEFINES CH-DATA.
               10  CHSW-REQ.
                   15  CHSW-FUNCTION       PIC X(01).
                       88  CHSW-REQUEST        VALUE 'R'.
                       88  CHSW-ENQUIRE        VALUE 'E'.
                       88  CHSW-CANCEL         VALUE 'X'.
                   15  CHSW-NEW-SORT-CODE  PIC X(06).
                   15  CHSW-NEW-ACCT-NO    PIC X(08).
                   15  CHSW-OLD-SORT-CODE  PIC X(06).
                   15  CHSW-OLD-ACCT-NO    PIC X(08).
                   15  CHSW-SWITCH-DATE    PIC 9(08).
                   15  CHSW-SWITCH-ID      PIC X(18).
               10  CHSW-RESP.
                   15  CHSW-NEW-SWITCH-ID  PIC X(18).
                   15  CHSW-EARLIEST-DATE  PIC 9(08).
                   15  CHSW-COUNT          PIC 9(02).
                   15  CHSW-ENTRY          OCCURS 10 TIMES.
                       20  CHSWE-SWITCH-ID PIC X(18).
                       20  CHSWE-DIRECTION PIC X(01).
                       20  CHSWE-STATUS    PIC X(02).
                       20  CHSWE-OLD-SORT-CODE
                                           PIC X(06).
                       20  CHSWE-OLD-ACCT-NO
                                           PIC X(08).
                       20  CHSWE-SWITCH-DATE
                                           PIC 9(08).
                       20  CHSWE-SO-COUNT  PIC 9(03).
                       20  CHSWE-DD-COUNT  PIC 9(03).
                       20  CHSWE-REJECT-RSN
                                           PIC X(04).
               10  FILLER                  PIC X(3221).
