--*===================================================================*
--* MEMBER   : MBKCORE                                                *
--* TITLE    : CORE BANKING DB2 OBJECTS - DATABASE MBKCORE            *
--* SUBSYSTEM: DB2P (PRODUCTION)   DB2T (SIT/UAT)   DB2D (DEV)        *
--* PLATFORM : DB2 FOR Z/OS V9.1  NEW FUNCTION MODE                   *
--*-------------------------------------------------------------------*
--* RUN VIA JCL MBKDB2DL (DSNTEP2). DBA SIGN-OFF REQUIRED FOR PRD.    *
--* THE VSAM MASTERS (CIF, ACCOUNT MASTER) ARE NOT IN DB2 - SEE       *
--* CNTL MEMBERS CFDEFCIF AND DDDEFACM.                               *
--*-------------------------------------------------------------------*
--* HISTORY                                                           *
--* 1999-08-16 KLM  TXNHIST MOVED FROM VSAM TO DB2 (PROJECT HORIZON)  *
--* 2008-05-27 AOS  PAYINSTR + PAYID_SEQ (FASTER PAYMENTS)            *
--* 2009-12-01 KLM  TXNHIST 2010 PARTITIONS ADDED                     *
--*===================================================================*

SET CURRENT SQLID = 'MBKDBA';

CREATE STOGROUP MBKSG01
       VOLUMES ('*')
       VCAT MBKDB2P;

CREATE DATABASE MBKCORE
       STOGROUP MBKSG01
       BUFFERPOOL BP1
       INDEXBP BP2
       CCSID EBCDIC;

--*-------------------------------------------------------------------*
--* TRANSACTION HISTORY                                               *
--* ONE ROW PER HARD-POSTED TRANSACTION. INSERTED ONLY BY PSEOD010.   *
--* RANGE PARTITIONED BY POST_DATE, ONE PARTITION PER QUARTER.        *
--* PARTITIONS ARE ROTATED ANNUALLY BY THE DBA (CR REQUIRED). IF THE  *
--* NEXT YEAR IS NOT ADDED BEFORE 31 DEC, PSEOD010 ABENDS U1003 WITH  *
--* SQLCODE -327 ON THE FIRST POSTING RUN OF JANUARY. SEE PIR 2008/112*
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSTXNH
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 720000
             SECQTY 72000
       NUMPARTS 8
       LOCKSIZE PAGE
       COMPRESS YES
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.TXNHIST
      (SORT_CODE        CHAR(6)          NOT NULL,
       ACCT_NO          CHAR(8)          NOT NULL,
       POST_DATE        DATE             NOT NULL,
       POST_SEQ         INTEGER          NOT NULL,
       TXN_CODE         CHAR(4)          NOT NULL,
       DR_CR_IND        CHAR(1)          NOT NULL,
       TXN_AMT          DECIMAL(15,2)    NOT NULL,
       RUNNING_BAL      DECIMAL(15,2)    NOT NULL,
       CCY_CD           CHAR(3)          NOT NULL WITH DEFAULT 'GBP',
       VALUE_DATE       DATE             NOT NULL,
       NARRATIVE_1      VARCHAR(35)      NOT NULL WITH DEFAULT,
       NARRATIVE_2      VARCHAR(35)      NOT NULL WITH DEFAULT,
       CHANNEL_CD       CHAR(6)          NOT NULL,
       EXT_REF          CHAR(24)         NOT NULL WITH DEFAULT,
       POST_TYPE        CHAR(1)          NOT NULL,
       USER_ID          CHAR(8)          NOT NULL WITH DEFAULT,
       BATCH_RUN_ID     CHAR(8)          NOT NULL,
       INSERT_TS        TIMESTAMP        NOT NULL WITH DEFAULT)
       IN MBKCORE.TSTXNH
       PARTITION BY (POST_DATE)
      (PARTITION 1 ENDING AT ('2009-03-31'),
       PARTITION 2 ENDING AT ('2009-06-30'),
       PARTITION 3 ENDING AT ('2009-09-30'),
       PARTITION 4 ENDING AT ('2009-12-31'),
       PARTITION 5 ENDING AT ('2010-03-31'),
       PARTITION 6 ENDING AT ('2010-06-30'),
       PARTITION 7 ENDING AT ('2010-09-30'),
       PARTITION 8 ENDING AT ('2010-12-31'));

CREATE UNIQUE INDEX MBK.XTXNH01
       ON MBK.TXNHIST
      (SORT_CODE, ACCT_NO, POST_DATE, POST_SEQ)
       USING STOGROUP MBKSG01 PRIQTY 144000 SECQTY 14400
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

CREATE INDEX MBK.XTXNH02
       ON MBK.TXNHIST
      (POST_DATE, SORT_CODE, ACCT_NO, POST_SEQ)
       PARTITIONED
       USING STOGROUP MBKSG01 PRIQTY 144000 SECQTY 14400
       BUFFERPOOL BP2
       CLOSE NO;

--*-------------------------------------------------------------------*
--* PAYMENT INSTRUCTIONS                                              *
--* WRITTEN BY PYINIT1 (ONLINE), UPDATED BY PYFPRSP (FPS RESPONSES),  *
--* PYBAC010 (BACS OUTBOUND) AND PYSWF010 (SWIFT OUTBOUND).           *
--* PAY_STATUS:                                                       *
--*   VAL  VALIDATED AND DEBITED, AWAITING BATCH SEND (BACS / SWIFT)  *
--*   SNT  SENT TO SCHEME / GATEWAY                                   *
--*   ACK  ACCEPTED BY SCHEME                                         *
--*   REJ  REJECTED BY SCHEME - DEBIT REVERSED                        *
--*   FAI  RESERVED - NOT SET SINCE 2009 (AN MQ FAILURE NOW ROLLS     *
--*        BACK THE WHOLE PAYMENT IN PYINIT1)                         *
--*-------------------------------------------------------------------*
CREATE TABLESPACE TSPAYI
       IN MBKCORE
       USING STOGROUP MBKSG01
             PRIQTY 72000
             SECQTY 7200
       SEGSIZE 32
       LOCKSIZE ROW
       CLOSE NO
       BUFFERPOOL BP1;

CREATE TABLE MBK.PAYINSTR
      (PAY_ID           CHAR(16)         NOT NULL,
       EXT_REF          CHAR(18)         NOT NULL,
       CHANNEL_CD       CHAR(6)          NOT NULL,
       PAY_SCHEME       CHAR(4)          NOT NULL,
       PAY_STATUS       CHAR(3)          NOT NULL,
       DR_SORT_CODE     CHAR(6)          NOT NULL,
       DR_ACCT_NO       CHAR(8)          NOT NULL,
       BENE_NAME        VARCHAR(35)      NOT NULL,
       BENE_SORT_CODE   CHAR(6)          NOT NULL WITH DEFAULT,
       BENE_ACCT_NO     CHAR(8)          NOT NULL WITH DEFAULT,
       BENE_IBAN        CHAR(34)         NOT NULL WITH DEFAULT,
       BENE_BIC         CHAR(11)         NOT NULL WITH DEFAULT,
       BENE_CTRY_CD     CHAR(2)          NOT NULL WITH DEFAULT,
       PAY_CCY_CD       CHAR(3)          NOT NULL,
       PAY_AMT          DECIMAL(15,2)    NOT NULL,
       FX_RATE          DECIMAL(11,6)    NOT NULL WITH DEFAULT,
       DR_AMT           DECIMAL(15,2)    NOT NULL,
       CHARGE_AMT       DECIMAL(15,2)    NOT NULL WITH DEFAULT,
       CHARGE_BEARER    CHAR(3)          NOT NULL WITH DEFAULT,
       REMIT_REF        CHAR(18)         NOT NULL WITH DEFAULT,
       PURPOSE_CD       CHAR(4)          NOT NULL WITH DEFAULT,
       VALUE_DATE       DATE             NOT NULL,
       SCHEME_REF       CHAR(35)         NOT NULL WITH DEFAULT,
       REJECT_CD        CHAR(4)          NOT NULL WITH DEFAULT,
       CAPTURE_TS       TIMESTAMP        NOT NULL,
       SENT_TS          TIMESTAMP,
       STATUS_TS        TIMESTAMP        NOT NULL,
       OUT_FILE_ID      CHAR(8)          NOT NULL WITH DEFAULT,
       USER_ID          CHAR(8)          NOT NULL WITH DEFAULT)
       IN MBKCORE.TSPAYI;

CREATE UNIQUE INDEX MBK.XPAYI01
       ON MBK.PAYINSTR (PAY_ID)
       USING STOGROUP MBKSG01 PRIQTY 14400 SECQTY 1440
       CLUSTER
       BUFFERPOOL BP2
       CLOSE NO;

CREATE UNIQUE INDEX MBK.XPAYI02
       ON MBK.PAYINSTR (CHANNEL_CD, EXT_REF)
       USING STOGROUP MBKSG01 PRIQTY 14400 SECQTY 1440
       BUFFERPOOL BP2
       CLOSE NO;

CREATE INDEX MBK.XPAYI03
       ON MBK.PAYINSTR (PAY_SCHEME, PAY_STATUS, VALUE_DATE)
       USING STOGROUP MBKSG01 PRIQTY 14400 SECQTY 1440
       BUFFERPOOL BP2
       CLOSE NO;

ALTER TABLE MBK.PAYINSTR
       ADD PRIMARY KEY (PAY_ID);

--*-------------------------------------------------------------------*
--* PAYMENT ID SEQUENCE - HOST PAYMENT ID IS                          *
--*   'PY' || CCYYMMDD || 6-DIGIT SEQUENCE   (E.G. PY20100714000123)  *
--* THE SEQUENCE CYCLES; UNIQUENESS RELIES ON THE DATE PREFIX AND     *
--* FEWER THAN 999,999 PAYMENTS A DAY.                                *
--*-------------------------------------------------------------------*
CREATE SEQUENCE MBK.PAYID_SEQ
       AS INTEGER
       START WITH 1
       INCREMENT BY 1
       MINVALUE 1
       MAXVALUE 999999
       CYCLE
       CACHE 50
       NO ORDER;

--*-------------------------------------------------------------------*
--* AUTHORISATIONS                                                    *
--*   MBKCICS   - CICS REGION CICSPRD1 (DB2CONN AUTHID)               *
--*   MBKBATCH  - BATCH JOBS (SECONDARY AUTHID VIA RACF GROUP)        *
--*-------------------------------------------------------------------*
GRANT SELECT                   ON MBK.TXNHIST  TO MBKCICS;
GRANT SELECT, INSERT           ON MBK.TXNHIST  TO MBKBATCH;
GRANT SELECT, INSERT, UPDATE   ON MBK.PAYINSTR TO MBKCICS;
GRANT SELECT, UPDATE           ON MBK.PAYINSTR TO MBKBATCH;
GRANT USAGE ON SEQUENCE MBK.PAYID_SEQ TO MBKCICS;

GRANT EXECUTE ON PLAN MBKCICS1 TO MBKCICS;
GRANT EXECUTE ON PLAN PSEOD010, IBFD0201, PYBAC010, PYSWF010
      TO MBKBATCH;
