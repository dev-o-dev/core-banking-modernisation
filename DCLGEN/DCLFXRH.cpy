      ******************************************************************
      * DCLGEN TABLE(MBK.FXRATEH)                                      *
      *        LIBRARY(MBK.PRD.DCLGEN(DCLFXRH))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(FXH-)                                             *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MBK.FXRATEH TABLE
           ( CCY_CD                         CHAR(3) NOT NULL,
             RATE_DATE                      DATE NOT NULL,
             FIXING_ID                      CHAR(8) NOT NULL,
             MID_RATE                       DECIMAL(11, 6) NOT NULL,
             PREV_MID_RATE                  DECIMAL(11, 6) NOT NULL,
             MOVE_PCT                       DECIMAL(7, 4) NOT NULL,
             STATUS                         CHAR(1) NOT NULL,
             LOADED_BY                      CHAR(8) NOT NULL,
             INSERT_TS                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MBK.FXRATEH                        *
      ******************************************************************
       01  DCLFXRATEH.
           10 FXH-CCY-CD           PIC X(3).
           10 FXH-RATE-DATE        PIC X(10).
           10 FXH-FIXING-ID        PIC X(8).
           10 FXH-MID-RATE         PIC S9(5)V9(6) USAGE COMP-3.
           10 FXH-PREV-MID-RATE    PIC S9(5)V9(6) USAGE COMP-3.
           10 FXH-MOVE-PCT         PIC S9(3)V9(4) USAGE COMP-3.
           10 FXH-STATUS           PIC X(1).
           10 FXH-LOADED-BY        PIC X(8).
           10 FXH-INSERT-TS        PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
