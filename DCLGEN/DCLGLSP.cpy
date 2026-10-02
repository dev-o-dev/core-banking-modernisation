      ******************************************************************
      * DCLGEN TABLE(MBK.GLSUSP)                                       *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLGLSP))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(GSP-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.GLSUSP TABLE
           ( SUSP_ID                        INTEGER NOT NULL,
             RAISED_DATE                    DATE NOT NULL,
             SOURCE_PGM                     CHAR(8) NOT NULL,
             SUSP_GL                        CHAR(10) NOT NULL,
             PROD_CD                        CHAR(10) NOT NULL,
             TXN_CODE                       CHAR(4) NOT NULL,
             DR_CR_IND                      CHAR(1) NOT NULL,
             AMOUNT                         DECIMAL(15, 2) NOT NULL,
             ITEM_COUNT                     INTEGER NOT NULL,
             REASON_CD                      CHAR(4) NOT NULL,
             SUSP_STATUS                    CHAR(1) NOT NULL,
             CLEARED_DATE                   DATE,
             CLEARED_BY                     CHAR(8),
             CLEARING_REF                   CHAR(16),
             NOTES                          VARCHAR(80) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.GLSUSP                         *
      ******************************************************************
       01  DCLGLSUSP.
           10 GSP-SUSP-ID          PIC S9(9) USAGE COMP.
           10 GSP-RAISED-DATE      PIC X(10).
           10 GSP-SOURCE-PGM       PIC X(8).
           10 GSP-SUSP-GL          PIC X(10).
           10 GSP-PROD-CD          PIC X(10).
           10 GSP-TXN-CODE         PIC X(4).
           10 GSP-DR-CR-IND        PIC X(1).
           10 GSP-AMOUNT           PIC S9(13)V9(2) USAGE COMP-3.
           10 GSP-ITEM-COUNT       PIC S9(9) USAGE COMP.
           10 GSP-REASON-CD        PIC X(4).
           10 GSP-SUSP-STATUS      PIC X(1).
           10 GSP-CLEARED-DATE     PIC X(10).
           10 GSP-CLEARED-BY       PIC X(8).
           10 GSP-CLEARING-REF     PIC X(16).
           10 GSP-NOTES.
              49 GSP-NOTES-LEN     PIC S9(4) USAGE COMP.
              49 GSP-NOTES-TEXT    PIC X(80).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  IGLSUSP.
           10 INDSTRUC-GSP       PIC S9(4) USAGE COMP OCCURS 15 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
