      ******************************************************************
      * DCLGEN TABLE(MBK.PAYINSTR)                                     *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLPAYI))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(PYI-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.PAYINSTR TABLE
           ( PAY_ID                         CHAR(16) NOT NULL,
             EXT_REF                        CHAR(18) NOT NULL,
             CHANNEL_CD                     CHAR(6) NOT NULL,
             PAY_SCHEME                     CHAR(4) NOT NULL,
             PAY_STATUS                     CHAR(3) NOT NULL,
             DR_SORT_CODE                   CHAR(6) NOT NULL,
             DR_ACCT_NO                     CHAR(8) NOT NULL,
             BENE_NAME                      VARCHAR(35) NOT NULL,
             BENE_SORT_CODE                 CHAR(6) NOT NULL,
             BENE_ACCT_NO                   CHAR(8) NOT NULL,
             BENE_IBAN                      CHAR(34) NOT NULL,
             BENE_BIC                       CHAR(11) NOT NULL,
             BENE_CTRY_CD                   CHAR(2) NOT NULL,
             PAY_CCY_CD                     CHAR(3) NOT NULL,
             PAY_AMT                        DECIMAL(15, 2) NOT NULL,
             FX_RATE                        DECIMAL(11, 6) NOT NULL,
             DR_AMT                         DECIMAL(15, 2) NOT NULL,
             CHARGE_AMT                     DECIMAL(15, 2) NOT NULL,
             CHARGE_BEARER                  CHAR(3) NOT NULL,
             REMIT_REF                      CHAR(18) NOT NULL,
             PURPOSE_CD                     CHAR(4) NOT NULL,
             VALUE_DATE                     DATE NOT NULL,
             SCHEME_REF                     CHAR(35) NOT NULL,
             REJECT_CD                      CHAR(4) NOT NULL,
             CAPTURE_TS                     TIMESTAMP NOT NULL,
             SENT_TS                        TIMESTAMP,
             STATUS_TS                      TIMESTAMP NOT NULL,
             OUT_FILE_ID                    CHAR(8) NOT NULL,
             USER_ID                        CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.PAYINSTR                       *
      ******************************************************************
       01  DCLPAYINSTR.
           10 PYI-PAY-ID           PIC X(16).
           10 PYI-EXT-REF          PIC X(18).
           10 PYI-CHANNEL-CD       PIC X(6).
           10 PYI-PAY-SCHEME       PIC X(4).
           10 PYI-PAY-STATUS       PIC X(3).
           10 PYI-DR-SORT-CODE     PIC X(6).
           10 PYI-DR-ACCT-NO       PIC X(8).
           10 PYI-BENE-NAME.
              49 PYI-BENE-NAME-LEN
                 PIC S9(4) USAGE COMP.
              49 PYI-BENE-NAME-TEXT
                 PIC X(35).
           10 PYI-BENE-SORT-CODE   PIC X(6).
           10 PYI-BENE-ACCT-NO     PIC X(8).
           10 PYI-BENE-IBAN        PIC X(34).
           10 PYI-BENE-BIC         PIC X(11).
           10 PYI-BENE-CTRY-CD     PIC X(2).
           10 PYI-PAY-CCY-CD       PIC X(3).
           10 PYI-PAY-AMT          PIC S9(13)V9(2) USAGE COMP-3.
           10 PYI-FX-RATE          PIC S9(5)V9(6) USAGE COMP-3.
           10 PYI-DR-AMT           PIC S9(13)V9(2) USAGE COMP-3.
           10 PYI-CHARGE-AMT       PIC S9(13)V9(2) USAGE COMP-3.
           10 PYI-CHARGE-BEARER    PIC X(3).
           10 PYI-REMIT-REF        PIC X(18).
           10 PYI-PURPOSE-CD       PIC X(4).
           10 PYI-VALUE-DATE       PIC X(10).
           10 PYI-SCHEME-REF       PIC X(35).
           10 PYI-REJECT-CD        PIC X(4).
           10 PYI-CAPTURE-TS       PIC X(26).
           10 PYI-SENT-TS          PIC X(26).
           10 PYI-STATUS-TS        PIC X(26).
           10 PYI-OUT-FILE-ID      PIC X(8).
           10 PYI-USER-ID          PIC X(8).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  IPAYINSTR.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 29 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 29      *
      ******************************************************************
