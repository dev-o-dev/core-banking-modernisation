//SOEXE01  JOB (MBK,BAT,SOE),'STANDING ORDERS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : SOEXE01                                                *
//* TITLE    : STANDING ORDER EXECUTION AND EXCEPTIONS                *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01 (FUNDS ARE TONIGHT'S       *
//*            LEDGER)                                                *
//* OWNER    : PAYMENTS IT     ON-CALL: ROTA PAY                      *
//*-------------------------------------------------------------------*
//* SO010  EXECUTE DUE ORDERS (SOEXE010)                              *
//* SO030  EXCEPTIONS REPORT (SORPT010)                               *
//*-------------------------------------------------------------------*
//* SOPOST -> TOMORROW'S PSEOD01. FPSOUT -> PAYMENTS GATEWAY (MQ      *
//* BRIDGE JOB PYFPS02). EXCEPTS -> DOCUMENT SERVICES + SO030.        *
//*-------------------------------------------------------------------*
//* 1988-07-11 RJH  ORIGINAL (SO0600/SO0700)                          *
//* 2008-05-27 AOS  FPS ROUTE, RETRIES                                *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* SO010 - EXECUTE DUE ORDERS (SOEXE010)                             *
//*-------------------------------------------------------------------*
//SO010    EXEC MBKDB2B
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.SOMAST   DD DISP=OLD,DSN=MBK.PRD.SO.MASTER
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.SOPOST   DD DSN=MBK.PRD.PST.FEED.SOPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.FPSOUT   DD DSN=MBK.PRD.SO.FPSOUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.EXCEPTS  DD DSN=MBK.PRD.SO.EXCEPTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,SOE1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(SOEXE010) PLAN(SOEXE010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* SO030 - EXCEPTIONS REPORT (SORPT010)                              *
//*-------------------------------------------------------------------*
//SO030    EXEC MBKBATCH,PGM=SORPT010,COND=(4,LT)
//STEP.EXCEPTS  DD DISP=SHR,DSN=MBK.PRD.SO.EXCEPTS(+1)
//STEP.SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//STEP.REPORT   DD SYSOUT=(R,,SOR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
