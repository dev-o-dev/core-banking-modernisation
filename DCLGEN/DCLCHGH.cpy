      ******************************************************************
      * DCLGEN TABLE(MBK.CHGHIST)                                      *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLCHGH))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(CGH-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.CHGHIST TABLE
           ( SORT_CODE                      CHAR(6) NOT NULL,
             ACCT_NO                        CHAR(8) NOT NULL,
             PERIOD_END                     DATE NOT NULL,
             CHARGE_TYPE                    CHAR(4) NOT NULL,
             ITEM_SEQ                       INTEGER NOT NULL,
             CIF_NO                         CHAR(12) NOT NULL,
             PROD_CD                        CHAR(10) NOT NULL,
             PERIOD_START                   DATE NOT NULL,
             APPLY_DATE                     DATE NOT NULL,
             ITEM_COUNT                     INTEGER NOT NULL,
             GROSS_AMT                      DECIMAL(15, 2) NOT NULL,
             CAP_REDUCTION                  DECIMAL(15, 2) NOT NULL,
             NET_AMT                        DECIMAL(15, 2) NOT NULL,
             REFUND_AMT                     DECIMAL(15, 2) NOT NULL,
             REFUND_DATE                    DATE,
             REFUND_REASON                  CHAR(4) NOT NULL,
             REFUND_USER                    CHAR(8) NOT NULL,
             DESCRIPTION                    VARCHAR(35) NOT NULL,
             EXT_REF                        CHAR(24) NOT NULL,
             BATCH_RUN_ID                   CHAR(8) NOT NULL,
             INSERT_TS                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.CHGHIST                        *
      ******************************************************************
       01  DCLCHGHIST.
           10 CGH-SORT-CODE        PIC X(6).
           10 CGH-ACCT-NO          PIC X(8).
           10 CGH-PERIOD-END       PIC X(10).
           10 CGH-CHARGE-TYPE      PIC X(4).
           10 CGH-ITEM-SEQ         PIC S9(9) USAGE COMP.
           10 CGH-CIF-NO           PIC X(12).
           10 CGH-PROD-CD          PIC X(10).
           10 CGH-PERIOD-START     PIC X(10).
           10 CGH-APPLY-DATE       PIC X(10).
           10 CGH-ITEM-COUNT       PIC S9(9) USAGE COMP.
           10 CGH-GROSS-AMT        PIC S9(13)V9(2) USAGE COMP-3.
           10 CGH-CAP-REDUCTION    PIC S9(13)V9(2) USAGE COMP-3.
           10 CGH-NET-AMT          PIC S9(13)V9(2) USAGE COMP-3.
           10 CGH-REFUND-AMT       PIC S9(13)V9(2) USAGE COMP-3.
           10 CGH-REFUND-DATE      PIC X(10).
           10 CGH-REFUND-REASON    PIC X(4).
           10 CGH-REFUND-USER      PIC X(8).
           10 CGH-DESCRIPTION.
              49 CGH-DESCRIPTION-LEN
                 PIC S9(4) USAGE COMP.
              49 CGH-DESCRIPTION-TEXT
                 PIC X(35).
           10 CGH-EXT-REF          PIC X(24).
           10 CGH-BATCH-RUN-ID     PIC X(8).
           10 CGH-INSERT-TS        PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 21      *
      ******************************************************************
