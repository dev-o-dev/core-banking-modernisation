      ******************************************************************
      * DCLGEN TABLE(MBK.AMLALERT)                                     *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLAMLA))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(AML-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.AMLALERT TABLE
           ( ALERT_ID                       CHAR(16) NOT NULL,
             ALERT_DATE                     DATE NOT NULL,
             RULE_ID                        CHAR(4) NOT NULL,
             SEVERITY                       CHAR(1) NOT NULL,
             SORT_CODE                      CHAR(6) NOT NULL,
             ACCT_NO                        CHAR(8) NOT NULL,
             CIF_NO                         CHAR(12) NOT NULL,
             RISK_RATING                    CHAR(1) NOT NULL,
             TXN_COUNT                      INTEGER NOT NULL,
             TXN_TOTAL                      DECIMAL(15, 2) NOT NULL,
             THRESHOLD                      DECIMAL(15, 2) NOT NULL,
             DESCRIPTION                    VARCHAR(80) NOT NULL,
             ALERT_STATUS                   CHAR(1) NOT NULL,
             ASSIGNED_TO                    CHAR(8) NOT NULL,
             CASE_REF                       CHAR(12) NOT NULL,
             CREATED_TS                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.AMLALERT                       *
      ******************************************************************
       01  DCLAMLALERT.
           10 AML-ALERT-ID         PIC X(16).
           10 AML-ALERT-DATE       PIC X(10).
           10 AML-RULE-ID          PIC X(4).
           10 AML-SEVERITY         PIC X(1).
           10 AML-SORT-CODE        PIC X(6).
           10 AML-ACCT-NO          PIC X(8).
           10 AML-CIF-NO           PIC X(12).
           10 AML-RISK-RATING      PIC X(1).
           10 AML-TXN-COUNT        PIC S9(9) USAGE COMP.
           10 AML-TXN-TOTAL        PIC S9(13)V9(2) USAGE COMP-3.
           10 AML-THRESHOLD        PIC S9(13)V9(2) USAGE COMP-3.
           10 AML-DESCRIPTION.
              49 AML-DESCRIPTION-LEN
                 PIC S9(4) USAGE COMP.
              49 AML-DESCRIPTION-TEXT
                 PIC X(80).
           10 AML-ALERT-STATUS     PIC X(1).
           10 AML-ASSIGNED-TO      PIC X(8).
           10 AML-CASE-REF         PIC X(12).
           10 AML-CREATED-TS       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
