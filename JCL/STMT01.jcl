//STMT01   JOB (MBK,EOD,DOC),'STATEMENT CYCLE',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : STMT01                                                 *
//* TITLE    : STATEMENT CYCLE - SELECT, COMPOSE, UPDATE, E-STATEMENT *
//* SCHEDULE : CA-7 NETWORK MBKEOD - AFTER PSEOD01 AND DDCLS01.       *
//*            THE PRINT STREAM IS PICKED UP BY DOCUMENT SERVICES     *
//*            (STMT02, MBK.PRD.DOC1.JCL) ON CREATION OF              *
//*            MBK.PRD.STMT.PRINT.                                    *
//* OWNER    : CORE SYSTEMS - DDA / DOCUMENT SERVICES                 *
//*-------------------------------------------------------------------*
//* ST010  SELECT ACCOUNTS DUE A STATEMENT (STSTM010)                 *
//* ST012  UNLOAD AD-HOC / DUPLICATE REQUESTS (CICS FILE STMTADH)    *
//* ST015  ADD CLOSING AND AD-HOC REQUESTS AND SORT                   *
//* ST016  RESET STMTADH FOR TOMORROW (ONLY IF ST015 WORKED)          *
//* ST020  COMPOSE STATEMENTS FROM MBK.TXNHIST (STSTM020)             *
//* ST030  UPDATE ACCOUNTS + E-STATEMENT FEED (STSTM030)              *
//*-------------------------------------------------------------------*
//* RESTART                                                           *
//*   ST010-ST020 : RERUN FROM ST010 (NO UPDATES MADE)                *
//*   ST030       : RESTART AT ST030 - IT SKIPS ACCOUNTS WHOSE        *
//*                 STATEMENT NUMBER IS ALREADY UPDATED               *
//*-------------------------------------------------------------------*
//* 1996-03-04 PDK  ORIGINAL (SELECTION ONLY - COMPOSITION ON DOC1)   *
//* 2010-05-24 JNB  COMPOSITION BROUGHT ONTO THE HOST (ST015-ST030)   *
//* 2012-06-11 JNB  ST012 / ST016 AD-HOC REQUESTS (STREQ01), SORT KEY *
//*                 WIDENED TO 31 BYTES (CR 12/0133)                  *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* ST010 - SELECT ACCOUNTS DUE                                       *
//*-------------------------------------------------------------------*
//ST010    EXEC MBKBATCH,PGM=STSTM010
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.STMTREQ  DD DSN=MBK.PRD.STMT.REQUEST(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* ST012 - UNLOAD AD-HOC REQUESTS (EMPTY FILE IS NOT AN ERROR)       *
//*-------------------------------------------------------------------*
//ST012    EXEC PGM=IDCAMS,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//ADHIN    DD DISP=SHR,DSN=MBK.PRD.STMT.ADHOC
//ADHOUT   DD DSN=MBK.PRD.STMT.REQUEST.ADHOC(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(ADHIN) OUTFILE(ADHOUT)
  IF LASTCC = 4 THEN SET MAXCC = 0
/*
//*-------------------------------------------------------------------*
//* ST015 - MERGE CLOSING AND AD-HOC REQUESTS, SORT BY KEY            *
//*         A CLOSED ACCOUNT CAN APPEAR TWICE (CYCLE + CLOSE) -       *
//*         SUM FIELDS=NONE KEEPS ONE. THE KEY IS SORT CODE +         *
//*         ACCOUNT + CIF + STATEMENT NO (31) SO THAT A DUPLICATE     *
//*         REQUEST IS NOT DROPPED AGAINST THE CYCLE STATEMENT        *
//*-------------------------------------------------------------------*
//ST015    EXEC PGM=SORT,COND=(4,LT)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.STMT.REQUEST(+1)
//         DD DISP=SHR,DSN=MBK.PRD.STMT.REQUESTS.CLOSE(0)
//         DD DISP=SHR,DSN=MBK.PRD.STMT.REQUEST.ADHOC(+1)
//SORTOUT  DD DSN=MBK.PRD.STMT.REQUEST.SORTED(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(1,31,CH,A)
  SUM FIELDS=NONE
/*
//*-------------------------------------------------------------------*
//* ST016 - RESET STMTADH (REQUESTS ARE NOW IN THE SORTED FILE)       *
//*-------------------------------------------------------------------*
//ST016    EXEC PGM=IDCAMS,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//EMPTY100 DD DUMMY,DCB=(RECFM=FB,LRECL=100)
//SYSIN    DD *
  REPRO INFILE(EMPTY100) OUTDATASET(MBK.PRD.STMT.ADHOC) REUSE
  IF LASTCC = 4 THEN SET MAXCC = 0
/*
//*-------------------------------------------------------------------*
//* ST020 - COMPOSE (STSTM020)                                        *
//*-------------------------------------------------------------------*
//ST020    EXEC MBKDB2B,COND=(4,LT)
//STEP.STMTREQ  DD DISP=SHR,DSN=MBK.PRD.STMT.REQUEST.SORTED(+1)
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.PRINT    DD DSN=MBK.PRD.STMT.PRINT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(400,100),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.INDEX    DD DSN=MBK.PRD.STMT.INDEX(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,STM2),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(STSTM020) PLAN(STSTM020) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* ST030 - UPDATE ACCOUNTS AND E-STATEMENT FEED (STSTM030)           *
//*-------------------------------------------------------------------*
//ST030    EXEC MBKBATCH,PGM=STSTM030,COND=(4,LT)
//STEP.INDEX    DD DISP=SHR,DSN=MBK.PRD.STMT.INDEX(+1)
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ESTMTFD  DD DSN=MBK.PRD.STMT.ESTMT.FEED(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,STM3),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
