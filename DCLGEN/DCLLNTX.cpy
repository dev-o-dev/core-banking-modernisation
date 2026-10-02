      ******************************************************************
      * DCLGEN TABLE(MBK.LOANTXN)                                      *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLLNTX))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(LNT-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.LOANTXN TABLE
           ( LOAN_NO                        CHAR(10) NOT NULL,
             TXN_DATE                       DATE NOT NULL,
             TXN_SEQ                        INTEGER NOT NULL,
             TXN_TYPE                       CHAR(4) NOT NULL,
             DR_CR_IND                      CHAR(1) NOT NULL,
             AMOUNT                         DECIMAL(15, 2) NOT NULL,
             INT_PORTION                    DECIMAL(15, 2) NOT NULL,
             FEE_PORTION                    DECIMAL(15, 2) NOT NULL,
             PRIN_PORTION                   DECIMAL(15, 2) NOT NULL,
             PRINCIPAL_AFTER                DECIMAL(15, 2) NOT NULL,
             ARREARS_AFTER                  DECIMAL(15, 2) NOT NULL,
             NARRATIVE                      CHAR(35) NOT NULL,
             SOURCE_PGM                     CHAR(8) NOT NULL,
             INSERT_TS                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.LOANTXN                        *
      ******************************************************************
       01  DCLLOANTXN.
           10 LNT-LOAN-NO          PIC X(10).
           10 LNT-TXN-DATE         PIC X(10).
           10 LNT-TXN-SEQ          PIC S9(9) USAGE COMP.
           10 LNT-TXN-TYPE         PIC X(4).
           10 LNT-DR-CR-IND        PIC X(1).
           10 LNT-AMOUNT           PIC S9(13)V9(2) USAGE COMP-3.
           10 LNT-INT-PORTION      PIC S9(13)V9(2) USAGE COMP-3.
           10 LNT-FEE-PORTION      PIC S9(13)V9(2) USAGE COMP-3.
           10 LNT-PRIN-PORTION     PIC S9(13)V9(2) USAGE COMP-3.
           10 LNT-PRINCIPAL-AFTER  PIC S9(13)V9(2) USAGE COMP-3.
           10 LNT-ARREARS-AFTER    PIC S9(13)V9(2) USAGE COMP-3.
           10 LNT-NARRATIVE        PIC X(35).
           10 LNT-SOURCE-PGM       PIC X(8).
           10 LNT-INSERT-TS        PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
