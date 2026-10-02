      ******************************************************************
      * DCLGEN TABLE(MBK.TXNHIST)                                      *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLTXNH))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TXH-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.TXNHIST TABLE
           ( SORT_CODE                      CHAR(6) NOT NULL,
             ACCT_NO                        CHAR(8) NOT NULL,
             POST_DATE                      DATE NOT NULL,
             POST_SEQ                       INTEGER NOT NULL,
             TXN_CODE                       CHAR(4) NOT NULL,
             DR_CR_IND                      CHAR(1) NOT NULL,
             TXN_AMT                        DECIMAL(15, 2) NOT NULL,
             RUNNING_BAL                    DECIMAL(15, 2) NOT NULL,
             CCY_CD                         CHAR(3) NOT NULL,
             VALUE_DATE                     DATE NOT NULL,
             NARRATIVE_1                    VARCHAR(35) NOT NULL,
             NARRATIVE_2                    VARCHAR(35) NOT NULL,
             CHANNEL_CD                     CHAR(6) NOT NULL,
             EXT_REF                        CHAR(24) NOT NULL,
             POST_TYPE                      CHAR(1) NOT NULL,
             USER_ID                        CHAR(8) NOT NULL,
             BATCH_RUN_ID                   CHAR(8) NOT NULL,
             INSERT_TS                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.TXNHIST                        *
      ******************************************************************
       01  DCLTXNHIST.
           10 TXH-SORT-CODE        PIC X(6).
           10 TXH-ACCT-NO          PIC X(8).
           10 TXH-POST-DATE        PIC X(10).
           10 TXH-POST-SEQ         PIC S9(9) USAGE COMP.
           10 TXH-TXN-CODE         PIC X(4).
           10 TXH-DR-CR-IND        PIC X(1).
           10 TXH-TXN-AMT          PIC S9(13)V9(2) USAGE COMP-3.
           10 TXH-RUNNING-BAL      PIC S9(13)V9(2) USAGE COMP-3.
           10 TXH-CCY-CD           PIC X(3).
           10 TXH-VALUE-DATE       PIC X(10).
           10 TXH-NARRATIVE-1.
              49 TXH-NARRATIVE-1-LEN
                 PIC S9(4) USAGE COMP.
              49 TXH-NARRATIVE-1-TEXT
                 PIC X(35).
           10 TXH-NARRATIVE-2.
              49 TXH-NARRATIVE-2-LEN
                 PIC S9(4) USAGE COMP.
              49 TXH-NARRATIVE-2-TEXT
                 PIC X(35).
           10 TXH-CHANNEL-CD       PIC X(6).
           10 TXH-EXT-REF          PIC X(24).
           10 TXH-POST-TYPE        PIC X(1).
           10 TXH-USER-ID          PIC X(8).
           10 TXH-BATCH-RUN-ID     PIC X(8).
           10 TXH-INSERT-TS        PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
