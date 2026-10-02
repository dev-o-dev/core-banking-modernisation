--*===================================================================*
--* MEMBER   : MBKCORE2                                               *
--* TITLE    : CORE BANKING DB2 OBJECTS - 2010/2011 BATCH SUITES      *
--* SUBSYSTEM: DB2P (PRODUCTION)   DB2T (SIT/UAT)   DB2D (DEV)        *
--* PLATFORM : DB2 FOR Z/OS V9.1  NEW FUNCTION MODE                   *
--*-------------------------------------------------------------------*
--* RUN VIA JCL MBKDB2DL (DSNTEP2) AFTER MBKCORE. DATABASE MBKCORE AND*
--* STOGROUP MBKSG01 ARE CREATED BY MBKCORE.                          *
--* COLUMN LISTS MUST MATCH THE DCLGENS IN MBK.PRD.DCLGEN - REGENERATE*
--* THE DCLGEN AFTER ANY ALTER.                                       *
--*-------------------------------------------------------------------*
--* HISTORY                                                           *
--* 2010-03-08 AOS  INTPAID, CHGHIST (CR 10/0141)                     *
--* 2010-07-05 JNB  GLMAP, GLCTLBAL, GLSUSP (GL FEED REWRITE)         *
--* 2010-08-16 JNB  AMLALERT                                          *
--* 2010-09-06 JNB  LOANTXN, LOANLTR (PERSONAL LOANS)                 *
--* 2011-05-09 JNB  FXRATEH                                           *
--*===================================================================*

SET CURRENT SQLID = 'MBKDBA';

--*-------------------------------------------------------------------*
--* INTPAID                                                           *
--* INTEREST PAID / CHARGED AT CAPITALISATION                         *
--* INSERTED BY INCAP010. READ BY INTAX010 (CERTIFICATES AND BBSI)    *
--* AND STSTM020 (ANNUAL INTEREST SUMMARY ON STATEMENTS).             *
--* RETENTION: 7 TAX YEARS (PURGE JOB MBKPRG01, DBA).                 *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSINTP
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 72000
             SECQTY 7200
       SEGSIZE 32
       LOCKSIZE PAGE
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.INTPAID
      (SORT_CODE        CHAR(6)           NOT NULL,
       ACCT_NO          CHAR(8)           NOT NULL,
       CAP_DATE         DATE              NOT NULL,
       CR_DR_IND        CHAR(1)           NOT NULL,
       CIF_NO           CHAR(12)          NOT NULL,
       PROD_CD          CHAR(10)          NOT NULL,
       PERIOD_FROM      DATE              NOT NULL,
       PERIOD_TO        DATE              NOT NULL,
       GROSS_AMT        DECIMAL(15,2)     NOT NULL,
       TAX_AMT          DECIMAL(15,2)     NOT NULL,
       NET_AMT          DECIMAL(15,2)     NOT NULL,
       UNARR_AMT        DECIMAL(15,2)     NOT NULL WITH DEFAULT 0,
       TAX_RATE         DECIMAL(5,2)      NOT NULL,
       TAX_STATUS       CHAR(1)           NOT NULL,
       TAX_YEAR         CHAR(9)           NOT NULL,
       WAIVED_IND       CHAR(1)           NOT NULL WITH DEFAULT 'N',
       CAP_REASON       CHAR(4)           NOT NULL,
       BATCH_RUN_ID     CHAR(8)           NOT NULL,
       INSERT_TS        TIMESTAMP         NOT NULL WITH DEFAULT)
       IN MBKCORE.TSINTP;

CREATE UNIQUE INDEX MBK.XINTP01
       ON MBK.INTPAID
      (SORT_CODE, ACCT_NO, CAP_DATE, CR_DR_IND)
       USING STOGROUP MBKSG01 PRIQTY 14400 SECQTY 1440
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

CREATE INDEX MBK.XINTP02
       ON MBK.INTPAID
      (TAX_YEAR, CIF_NO, SORT_CODE, ACCT_NO)
       USING STOGROUP MBKSG01 PRIQTY 14400 SECQTY 1440
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* CHGHIST                                                           *
--* CHARGE HISTORY - ONE ROW PER CHARGE ITEM APPLIED BY CGAPL010.     *
--* REFUND COLUMNS UPDATED BY CGREF010. ROWS ARE NEVER DELETED        *
--* WITHIN 6 YEARS (FOS COMPLAINT EVIDENCE).                          *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSCHGH
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 72000
             SECQTY 7200
       SEGSIZE 32
       LOCKSIZE ROW
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.CHGHIST
      (SORT_CODE        CHAR(6)           NOT NULL,
       ACCT_NO          CHAR(8)           NOT NULL,
       PERIOD_END       DATE              NOT NULL,
       CHARGE_TYPE      CHAR(4)           NOT NULL,
       ITEM_SEQ         INTEGER           NOT NULL,
       CIF_NO           CHAR(12)          NOT NULL,
       PROD_CD          CHAR(10)          NOT NULL,
       PERIOD_START     DATE              NOT NULL,
       APPLY_DATE       DATE              NOT NULL,
       ITEM_COUNT       INTEGER           NOT NULL,
       GROSS_AMT        DECIMAL(15,2)     NOT NULL,
       CAP_REDUCTION    DECIMAL(15,2)     NOT NULL,
       NET_AMT          DECIMAL(15,2)     NOT NULL,
       REFUND_AMT       DECIMAL(15,2)     NOT NULL WITH DEFAULT 0,
       REFUND_DATE      DATE             ,
       REFUND_REASON    CHAR(4)           NOT NULL WITH DEFAULT,
       REFUND_USER      CHAR(8)           NOT NULL WITH DEFAULT,
       DESCRIPTION      VARCHAR(35)       NOT NULL,
       EXT_REF          CHAR(24)          NOT NULL,
       BATCH_RUN_ID     CHAR(8)           NOT NULL,
       INSERT_TS        TIMESTAMP         NOT NULL WITH DEFAULT)
       IN MBKCORE.TSCHGH;

CREATE UNIQUE INDEX MBK.XCHGH01
       ON MBK.CHGHIST
      (SORT_CODE, ACCT_NO, PERIOD_END, CHARGE_TYPE, ITEM_SEQ)
       USING STOGROUP MBKSG01 PRIQTY 14400 SECQTY 1440
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

CREATE INDEX MBK.XCHGH02
       ON MBK.CHGHIST
      (CIF_NO, PERIOD_END)
       USING STOGROUP MBKSG01 PRIQTY 14400 SECQTY 1440
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* GLMAP                                                             *
--* GL MAPPING - TRANSACTION CODE / DIRECTION / PRODUCT TO THE CONTRA *
--* GL ACCOUNT FOR CUSTOMER MOVEMENTS (GLEXT010).                     *
--* PROD_CD = '*DEFAULT*' MATCHES ANY PRODUCT. MAP_ACTION: P POST,    *
--* S SKIP (ALREADY JOURNALED BY ITS SOURCE), I INTERNAL CLEARING.    *
--* SEED ROWS IN MEMBER MBKGLMAP. MAINTAINED BY FINANCE (SPUFI + CR). *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSGLMP
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 720
             SECQTY 720
       SEGSIZE 4
       LOCKSIZE PAGE
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.GLMAP
      (TXN_CODE         CHAR(4)           NOT NULL,
       DR_CR_IND        CHAR(1)           NOT NULL,
       PROD_CD          CHAR(10)          NOT NULL,
       CONTRA_GL        CHAR(10)          NOT NULL,
       CONTRA_CC        CHAR(6)           NOT NULL,
       MAP_ACTION       CHAR(1)           NOT NULL,
       DESCRIPTION      VARCHAR(40)       NOT NULL,
       EFFECTIVE_FROM   DATE              NOT NULL,
       EFFECTIVE_TO     DATE              NOT NULL,
       LAST_CHANGED_BY  CHAR(8)           NOT NULL,
       LAST_CHANGED_TS  TIMESTAMP         NOT NULL WITH DEFAULT)
       IN MBKCORE.TSGLMP;

CREATE UNIQUE INDEX MBK.XGLMP01
       ON MBK.GLMAP
      (TXN_CODE, DR_CR_IND, PROD_CD, EFFECTIVE_FROM)
       USING STOGROUP MBKSG01 PRIQTY 720 SECQTY 144
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* GLCTLBAL                                                          *
--* GL CONTROL BALANCES - LOADED NIGHTLY FROM ORACLE GL AND           *
--* RECONCILED TO THE ACCOUNT MASTER BY GLREC010.                     *
--* RECON_STATUS: P PENDING, M MATCHED, B BREAK.                      *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSGLCB
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 7200
             SECQTY 720
       SEGSIZE 32
       LOCKSIZE PAGE
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.GLCTLBAL
      (BUSINESS_DATE    DATE              NOT NULL,
       GL_ACCOUNT       CHAR(10)          NOT NULL,
       PROD_CD          CHAR(10)          NOT NULL,
       CCY_CD           CHAR(3)           NOT NULL,
       GL_BALANCE       DECIMAL(17,2)     NOT NULL,
       SUB_LEDGER_BAL   DECIMAL(17,2)     NOT NULL,
       DIFFERENCE       DECIMAL(17,2)     NOT NULL,
       RECON_STATUS     CHAR(1)           NOT NULL,
       ACCOUNT_COUNT    INTEGER           NOT NULL,
       LOADED_TS        TIMESTAMP         NOT NULL WITH DEFAULT,
       RECON_TS         TIMESTAMP        )
       IN MBKCORE.TSGLCB;

CREATE UNIQUE INDEX MBK.XGLCB01
       ON MBK.GLCTLBAL
      (BUSINESS_DATE, GL_ACCOUNT, PROD_CD, CCY_CD)
       USING STOGROUP MBKSG01 PRIQTY 1440 SECQTY 144
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* GLSUSP                                                            *
--* GL SUSPENSE ITEMS - RAISED BY GLEXT010 (UNMAPPED CODES) AND       *
--* GLREC010 (BREAKS), AGED AND ESCALATED BY GLSUS010.                *
--* SUSP_STATUS: O OPEN, C CLEARED, W WRITTEN OFF.                    *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSGLSP
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 7200
             SECQTY 720
       SEGSIZE 32
       LOCKSIZE ROW
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.GLSUSP
      (SUSP_ID          INTEGER           NOT NULL,
       RAISED_DATE      DATE              NOT NULL,
       SOURCE_PGM       CHAR(8)           NOT NULL,
       SUSP_GL          CHAR(10)          NOT NULL,
       PROD_CD          CHAR(10)          NOT NULL,
       TXN_CODE         CHAR(4)           NOT NULL,
       DR_CR_IND        CHAR(1)           NOT NULL,
       AMOUNT           DECIMAL(15,2)     NOT NULL,
       ITEM_COUNT       INTEGER           NOT NULL,
       REASON_CD        CHAR(4)           NOT NULL,
       SUSP_STATUS      CHAR(1)           NOT NULL,
       CLEARED_DATE     DATE             ,
       CLEARED_BY       CHAR(8)          ,
       CLEARING_REF     CHAR(16)         ,
       NOTES            VARCHAR(80)       NOT NULL)
       IN MBKCORE.TSGLSP;

CREATE UNIQUE INDEX MBK.XGLSP01
       ON MBK.GLSUSP
      (SUSP_ID)
       USING STOGROUP MBKSG01 PRIQTY 1440 SECQTY 144
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

CREATE INDEX MBK.XGLSP02
       ON MBK.GLSUSP
      (SUSP_STATUS, RAISED_DATE)
       USING STOGROUP MBKSG01 PRIQTY 1440 SECQTY 144
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* AMLALERT                                                          *
--* AML ALERTS - WRITTEN BY RGAML010, WORKED IN THE FINANCIAL CRIME   *
--* CASE TOOL (READS AND UPDATES ALERT_STATUS VIA DRDA).              *
--* ALERT_STATUS: N NEW, A ASSIGNED, C CLOSED NO ACTION, S SAR FILED. *
--* RETENTION: 5 YEARS AFTER THE RELATIONSHIP ENDS (MLR 2007 REG 19). *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSAMLA
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 14400
             SECQTY 1440
       SEGSIZE 32
       LOCKSIZE ROW
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.AMLALERT
      (ALERT_ID         CHAR(16)          NOT NULL,
       ALERT_DATE       DATE              NOT NULL,
       RULE_ID          CHAR(4)           NOT NULL,
       SEVERITY         CHAR(1)           NOT NULL,
       SORT_CODE        CHAR(6)           NOT NULL,
       ACCT_NO          CHAR(8)           NOT NULL,
       CIF_NO           CHAR(12)          NOT NULL,
       RISK_RATING      CHAR(1)           NOT NULL,
       TXN_COUNT        INTEGER           NOT NULL,
       TXN_TOTAL        DECIMAL(15,2)     NOT NULL,
       THRESHOLD        DECIMAL(15,2)     NOT NULL,
       DESCRIPTION      VARCHAR(80)       NOT NULL,
       ALERT_STATUS     CHAR(1)           NOT NULL,
       ASSIGNED_TO      CHAR(8)           NOT NULL,
       CASE_REF         CHAR(12)          NOT NULL,
       CREATED_TS       TIMESTAMP         NOT NULL WITH DEFAULT)
       IN MBKCORE.TSAMLA;

CREATE UNIQUE INDEX MBK.XAMLA01
       ON MBK.AMLALERT
      (ALERT_ID)
       USING STOGROUP MBKSG01 PRIQTY 2880 SECQTY 288
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

CREATE INDEX MBK.XAMLA02
       ON MBK.AMLALERT
      (CIF_NO, ALERT_DATE)
       USING STOGROUP MBKSG01 PRIQTY 2880 SECQTY 288
       BUFFERPOOL BP2
       CLOSE NO;

CREATE INDEX MBK.XAMLA03
       ON MBK.AMLALERT
      (ALERT_STATUS, SEVERITY)
       USING STOGROUP MBKSG01 PRIQTY 2880 SECQTY 288
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* LOANTXN                                                           *
--* LOAN TRANSACTION HISTORY - ADVANCE, REPAYMENTS, MISSED            *
--* INSTALMENTS, FEES AND WRITE-OFFS (LNBKG010, LNREP010, LNARR010).  *
--* SOURCE FOR THE CCA S77A ANNUAL STATEMENT (LNSTM010).              *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSLNTX
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 72000
             SECQTY 7200
       SEGSIZE 32
       LOCKSIZE ROW
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.LOANTXN
      (LOAN_NO          CHAR(10)          NOT NULL,
       TXN_DATE         DATE              NOT NULL,
       TXN_SEQ          INTEGER           NOT NULL,
       TXN_TYPE         CHAR(4)           NOT NULL,
       DR_CR_IND        CHAR(1)           NOT NULL,
       AMOUNT           DECIMAL(15,2)     NOT NULL,
       INT_PORTION      DECIMAL(15,2)     NOT NULL,
       FEE_PORTION      DECIMAL(15,2)     NOT NULL,
       PRIN_PORTION     DECIMAL(15,2)     NOT NULL,
       PRINCIPAL_AFTER  DECIMAL(15,2)     NOT NULL,
       ARREARS_AFTER    DECIMAL(15,2)     NOT NULL,
       NARRATIVE        CHAR(35)          NOT NULL,
       SOURCE_PGM       CHAR(8)           NOT NULL,
       INSERT_TS        TIMESTAMP         NOT NULL WITH DEFAULT)
       IN MBKCORE.TSLNTX;

CREATE UNIQUE INDEX MBK.XLNTX01
       ON MBK.LOANTXN
      (LOAN_NO, TXN_DATE, TXN_SEQ)
       USING STOGROUP MBKSG01 PRIQTY 14400 SECQTY 1440
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* LOANLTR                                                           *
--* LOAN REGULATORY NOTICES ISSUED (NSIA, DFLT, TERM, ANST).          *
--* EVIDENCE FOR CCA ENFORCEABILITY - A MISSING NOTICE SUSPENDS       *
--* THE RIGHT TO INTEREST (S86D). NEVER PURGED WHILE THE LOAN EXISTS. *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSLNLT
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 7200
             SECQTY 720
       SEGSIZE 32
       LOCKSIZE ROW
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.LOANLTR
      (LOAN_NO          CHAR(10)          NOT NULL,
       LETTER_CD        CHAR(4)           NOT NULL,
       ISSUE_DATE       DATE              NOT NULL,
       CIF_NO           CHAR(12)          NOT NULL,
       ARREARS_AMT      DECIMAL(15,2)     NOT NULL,
       BALANCE_AMT      DECIMAL(15,2)     NOT NULL,
       MISSED_COUNT     SMALLINT          NOT NULL,
       TEMPLATE_ID      CHAR(8)           NOT NULL,
       DOC_REF          CHAR(20)          NOT NULL,
       INSERT_TS        TIMESTAMP         NOT NULL WITH DEFAULT)
       IN MBKCORE.TSLNLT;

CREATE UNIQUE INDEX MBK.XLNLT01
       ON MBK.LOANLTR
      (LOAN_NO, LETTER_CD, ISSUE_DATE)
       USING STOGROUP MBKSG01 PRIQTY 1440 SECQTY 144
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* FXRATEH                                                           *
--* FX RATE HISTORY - EVERY TREASURY FIXING LOADED BY FXRAT010.       *
--* USED BY PAYMENTS COMPLAINTS TO EVIDENCE THE RATE ON A GIVEN DATE. *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSFXRH
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 720
             SECQTY 720
       SEGSIZE 4
       LOCKSIZE PAGE
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.FXRATEH
      (CCY_CD           CHAR(3)           NOT NULL,
       RATE_DATE        DATE              NOT NULL,
       FIXING_ID        CHAR(8)           NOT NULL,
       MID_RATE         DECIMAL(11,6)     NOT NULL,
       PREV_MID_RATE    DECIMAL(11,6)     NOT NULL,
       MOVE_PCT         DECIMAL(7,4)      NOT NULL,
       STATUS           CHAR(1)           NOT NULL,
       LOADED_BY        CHAR(8)           NOT NULL,
       INSERT_TS        TIMESTAMP         NOT NULL WITH DEFAULT)
       IN MBKCORE.TSFXRH;

CREATE UNIQUE INDEX MBK.XFXRH01
       ON MBK.FXRATEH
      (CCY_CD, RATE_DATE, FIXING_ID)
       USING STOGROUP MBKSG01 PRIQTY 720 SECQTY 144
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* SEQUENCES                                                         *
--*-------------------------------------------------------------------*
CREATE SEQUENCE MBK.GLSUSP_SEQ
       AS INTEGER
       START WITH 1
       INCREMENT BY 1
       NO CYCLE
       CACHE 20;

CREATE SEQUENCE MBK.AMLALERT_SEQ
       AS INTEGER
       START WITH 1
       INCREMENT BY 1
       NO CYCLE
       CACHE 50;

--*-------------------------------------------------------------------*
--* GRANTS - MBKCICS (ONLINE PLANS)  MBKBATCH (BATCH PLANS)           *
--* MBKFINC (FINANCE / FINCRIME TOOLS VIA DRDA)                       *
--*-------------------------------------------------------------------*
GRANT SELECT                 ON MBK.INTPAID   TO MBKCICS;
GRANT SELECT, INSERT         ON MBK.INTPAID   TO MBKBATCH;
GRANT SELECT                 ON MBK.CHGHIST   TO MBKCICS;
GRANT SELECT, INSERT, UPDATE ON MBK.CHGHIST   TO MBKBATCH;
GRANT SELECT                 ON MBK.GLMAP     TO MBKCICS;
GRANT SELECT                 ON MBK.GLMAP     TO MBKBATCH;
GRANT SELECT                 ON MBK.GLCTLBAL  TO MBKCICS;
GRANT SELECT, INSERT, UPDATE ON MBK.GLCTLBAL  TO MBKBATCH;
GRANT SELECT                 ON MBK.GLSUSP    TO MBKCICS;
GRANT SELECT, INSERT, UPDATE ON MBK.GLSUSP    TO MBKBATCH;
GRANT SELECT                 ON MBK.AMLALERT  TO MBKCICS;
GRANT SELECT, INSERT         ON MBK.AMLALERT  TO MBKBATCH;
GRANT SELECT                 ON MBK.LOANTXN   TO MBKCICS;
GRANT SELECT, INSERT         ON MBK.LOANTXN   TO MBKBATCH;
GRANT SELECT                 ON MBK.LOANLTR   TO MBKCICS;
GRANT SELECT, INSERT         ON MBK.LOANLTR   TO MBKBATCH;
GRANT SELECT                 ON MBK.FXRATEH   TO MBKCICS;
GRANT SELECT, INSERT, UPDATE ON MBK.FXRATEH   TO MBKBATCH;
GRANT SELECT, UPDATE         ON MBK.AMLALERT  TO MBKFINC;
GRANT SELECT                 ON MBK.GLSUSP    TO MBKFINC;
GRANT SELECT, INSERT, UPDATE ON MBK.GLMAP     TO MBKFINC;
GRANT USAGE ON SEQUENCE MBK.GLSUSP_SEQ   TO MBKBATCH;
GRANT USAGE ON SEQUENCE MBK.AMLALERT_SEQ TO MBKBATCH;

COMMIT;
