      ******************************************************************
      * DCLGEN TABLE(MBK.INTPAID)                                      *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLINTP))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(INP-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.INTPAID TABLE
           ( SORT_CODE                      CHAR(6) NOT NULL,
             ACCT_NO                        CHAR(8) NOT NULL,
             CAP_DATE                       DATE NOT NULL,
             CR_DR_IND                      CHAR(1) NOT NULL,
             CIF_NO                         CHAR(12) NOT NULL,
             PROD_CD                        CHAR(10) NOT NULL,
             PERIOD_FROM                    DATE NOT NULL,
             PERIOD_TO                      DATE NOT NULL,
             GROSS_AMT                      DECIMAL(15, 2) NOT NULL,
             TAX_AMT                        DECIMAL(15, 2) NOT NULL,
             NET_AMT                        DECIMAL(15, 2) NOT NULL,
             UNARR_AMT                      DECIMAL(15, 2) NOT NULL,
             TAX_RATE                       DECIMAL(5, 2) NOT NULL,
             TAX_STATUS                     CHAR(1) NOT NULL,
             TAX_YEAR                       CHAR(9) NOT NULL,
             WAIVED_IND                     CHAR(1) NOT NULL,
             CAP_REASON                     CHAR(4) NOT NULL,
             BATCH_RUN_ID                   CHAR(8) NOT NULL,
             INSERT_TS                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.INTPAID                        *
      ******************************************************************
       01  DCLINTPAID.
           10 INP-SORT-CODE        PIC X(6).
           10 INP-ACCT-NO          PIC X(8).
           10 INP-CAP-DATE         PIC X(10).
           10 INP-CR-DR-IND        PIC X(1).
           10 INP-CIF-NO           PIC X(12).
           10 INP-PROD-CD          PIC X(10).
           10 INP-PERIOD-FROM      PIC X(10).
           10 INP-PERIOD-TO        PIC X(10).
           10 INP-GROSS-AMT        PIC S9(13)V9(2) USAGE COMP-3.
           10 INP-TAX-AMT          PIC S9(13)V9(2) USAGE COMP-3.
           10 INP-NET-AMT          PIC S9(13)V9(2) USAGE COMP-3.
           10 INP-UNARR-AMT        PIC S9(13)V9(2) USAGE COMP-3.
           10 INP-TAX-RATE         PIC S9(3)V9(2) USAGE COMP-3.
           10 INP-TAX-STATUS       PIC X(1).
           10 INP-TAX-YEAR         PIC X(9).
           10 INP-WAIVED-IND       PIC X(1).
           10 INP-CAP-REASON       PIC X(4).
           10 INP-BATCH-RUN-ID     PIC X(8).
           10 INP-INSERT-TS        PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
