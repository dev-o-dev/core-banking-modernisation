//GLSUS01  JOB (MBK,BAT,GLS),'GL SUSPENSE AGEING',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : GLSUS01                                                *
//* TITLE    : GL SUSPENSE ACCOUNT CLEARING AND AGEING                *
//* SCHEDULE : CA-7 MBKEOD - DAILY, AFTER GLFEED01                    *
//* OWNER    : CORE SYSTEMS - FINANCE INTERFACES                      *
//*-------------------------------------------------------------------*
//* GS010  SUSPENSE AGEING (GLSUS010)                                 *
//*-------------------------------------------------------------------*
//* 2010-07-05 JNB  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* GS010 - SUSPENSE AGEING (GLSUS010)                                *
//*-------------------------------------------------------------------*
//GS010    EXEC MBKDB2B
//STEP.CLEARIN  DD DISP=SHR,DSN=MBK.PRD.GL.SUSP.CLEARED(0)
//STEP.ESCALATE DD DSN=MBK.PRD.GL.SUSP.ESCALATE(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,GLS1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(GLSUS010) PLAN(GLSUS010) LIB('MBK.PRD.LOADLIB')
 END
/*
