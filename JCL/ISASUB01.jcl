//ISASUB01 JOB (MBK,BAT,ISA),'ISA SUBSCRIPTIONS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : ISASUB01                                               *
//* TITLE    : CASH ISA SUBSCRIPTION MONITORING                       *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01                            *
//* OWNER    : CORE SYSTEMS - SAVINGS                                 *
//*-------------------------------------------------------------------*
//* IS010  DAILY CHECK (ISASUB10)                                     *
//*-------------------------------------------------------------------*
//* 2010-04-06 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* IS010 - DAILY CHECK (ISASUB10)                                    *
//*-------------------------------------------------------------------*
//IS010    EXEC MBKDB2B
//STEP.PARMIN   DD *
MODE=DAILY
/*
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.BREACH   DD DSN=MBK.PRD.ISA.BREACHES(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.ISARTN   DD DUMMY
//STEP.REPORT   DD SYSOUT=(R,,ISA1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(ISASUB10) PLAN(ISASUB10) LIB('MBK.PRD.LOADLIB')
 END
/*
