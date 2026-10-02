      ******************************************************************
      * DCLGEN TABLE(MBK.GLCTLBAL)                                     *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLGLCB))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(GCB-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.GLCTLBAL TABLE
           ( BUSINESS_DATE                  DATE NOT NULL,
             GL_ACCOUNT                     CHAR(10) NOT NULL,
             PROD_CD                        CHAR(10) NOT NULL,
             CCY_CD                         CHAR(3) NOT NULL,
             GL_BALANCE                     DECIMAL(17, 2) NOT NULL,
             SUB_LEDGER_BAL                 DECIMAL(17, 2) NOT NULL,
             DIFFERENCE                     DECIMAL(17, 2) NOT NULL,
             RECON_STATUS                   CHAR(1) NOT NULL,
             ACCOUNT_COUNT                  INTEGER NOT NULL,
             LOADED_TS                      TIMESTAMP NOT NULL,
             RECON_TS                       TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.GLCTLBAL                       *
      ******************************************************************
       01  DCLGLCTLBAL.
           10 GCB-BUSINESS-DATE    PIC X(10).
           10 GCB-GL-ACCOUNT       PIC X(10).
           10 GCB-PROD-CD          PIC X(10).
           10 GCB-CCY-CD           PIC X(3).
           10 GCB-GL-BALANCE       PIC S9(15)V9(2) USAGE COMP-3.
           10 GCB-SUB-LEDGER-BAL   PIC S9(15)V9(2) USAGE COMP-3.
           10 GCB-DIFFERENCE       PIC S9(15)V9(2) USAGE COMP-3.
           10 GCB-RECON-STATUS     PIC X(1).
           10 GCB-ACCOUNT-COUNT    PIC S9(9) USAGE COMP.
           10 GCB-LOADED-TS        PIC X(26).
           10 GCB-RECON-TS         PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
