      ******************************************************************
      * DCLGEN TABLE(MBK.LOANLTR)                                      *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLLNLT))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(LNL-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.LOANLTR TABLE
           ( LOAN_NO                        CHAR(10) NOT NULL,
             LETTER_CD                      CHAR(4) NOT NULL,
             ISSUE_DATE                     DATE NOT NULL,
             CIF_NO                         CHAR(12) NOT NULL,
             ARREARS_AMT                    DECIMAL(15, 2) NOT NULL,
             BALANCE_AMT                    DECIMAL(15, 2) NOT NULL,
             MISSED_COUNT                   SMALLINT NOT NULL,
             TEMPLATE_ID                    CHAR(8) NOT NULL,
             DOC_REF                        CHAR(20) NOT NULL,
             INSERT_TS                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.LOANLTR                        *
      ******************************************************************
       01  DCLLOANLTR.
           10 LNL-LOAN-NO          PIC X(10).
           10 LNL-LETTER-CD        PIC X(4).
           10 LNL-ISSUE-DATE       PIC X(10).
           10 LNL-CIF-NO           PIC X(12).
           10 LNL-ARREARS-AMT      PIC S9(13)V9(2) USAGE COMP-3.
           10 LNL-BALANCE-AMT      PIC S9(13)V9(2) USAGE COMP-3.
           10 LNL-MISSED-COUNT     PIC S9(4) USAGE COMP.
           10 LNL-TEMPLATE-ID      PIC X(8).
           10 LNL-DOC-REF          PIC X(20).
           10 LNL-INSERT-TS        PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
