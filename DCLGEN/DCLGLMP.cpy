      ******************************************************************
      * DCLGEN TABLE(MBK.GLMAP)                                        *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLGLMP))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(GLM-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.GLMAP TABLE
           ( TXN_CODE                       CHAR(4) NOT NULL,
             DR_CR_IND                      CHAR(1) NOT NULL,
             PROD_CD                        CHAR(10) NOT NULL,
             CONTRA_GL                      CHAR(10) NOT NULL,
             CONTRA_CC                      CHAR(6) NOT NULL,
             MAP_ACTION                     CHAR(1) NOT NULL,
             DESCRIPTION                    VARCHAR(40) NOT NULL,
             EFFECTIVE_FROM                 DATE NOT NULL,
             EFFECTIVE_TO                   DATE NOT NULL,
             LAST_CHANGED_BY                CHAR(8) NOT NULL,
             LAST_CHANGED_TS                TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.GLMAP                          *
      ******************************************************************
       01  DCLGLMAP.
           10 GLM-TXN-CODE         PIC X(4).
           10 GLM-DR-CR-IND        PIC X(1).
           10 GLM-PROD-CD          PIC X(10).
           10 GLM-CONTRA-GL        PIC X(10).
           10 GLM-CONTRA-CC        PIC X(6).
           10 GLM-MAP-ACTION       PIC X(1).
           10 GLM-DESCRIPTION.
              49 GLM-DESCRIPTION-LEN
                 PIC S9(4) USAGE COMP.
              49 GLM-DESCRIPTION-TEXT
                 PIC X(40).
           10 GLM-EFFECTIVE-FROM   PIC X(10).
           10 GLM-EFFECTIVE-TO     PIC X(10).
           10 GLM-LAST-CHANGED-BY  PIC X(8).
           10 GLM-LAST-CHANGED-TS  PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
