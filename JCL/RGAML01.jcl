//RGAML01  JOB (MBK,BAT,RGA),'AML MONITORING',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : RGAML01                                                *
//* TITLE    : AML TRANSACTION MONITORING RULES R01-R07               *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01 (READS MBK.TXNHIST)        *
//* OWNER    : FINANCIAL CRIME IT                                     *
//*-------------------------------------------------------------------*
//* AM010  RULES (RGAML010)                                           *
//*-------------------------------------------------------------------*
//* 2010-08-16 JNB  ORIGINAL (REPLACES VENDOR SCREENING)              *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* AM010 - RULES (RGAML010)                                          *
//*-------------------------------------------------------------------*
//AM010    EXEC MBKDB2B
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.RULEPARM DD DISP=SHR,DSN=MBK.PRD.CNTL(AMLRULES)
//STEP.ALERTS   DD DSN=MBK.PRD.AML.ALERTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,AML1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(RGAML010) PLAN(RGAML010) LIB('MBK.PRD.LOADLIB')
 END
/*
