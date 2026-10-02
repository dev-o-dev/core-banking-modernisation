//CHGTAR01 JOB (MBK,BAT,CHG),'ANNUAL CHARGES SUMMARY',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CHGTAR01                                               *
//* TITLE    : PERSONAL CURRENT ACCOUNTS - ANNUAL SUMMARY OF CHARGES  *
//* SCHEDULE : CA-7 MBKMTH - FIRST BUSINESS DAY, AFTER PSEOD01,       *
//*            INCAP01 AND CGAPL01                                    *
//* OWNER    : CORE SYSTEMS - CHARGING                                *
//*-------------------------------------------------------------------*
//* CT010  BUILD SUMMARY LETTERS (CHGTAR10)                           *
//*-------------------------------------------------------------------*
//* READ ONLY AGAINST DB2 (WITH UR) - SAFE TO RERUN. A RERUN CREATES  *
//* A NEW LETTERS GENERATION: DELETE THE BAD ONE FIRST OR DOCUMENT    *
//* SERVICES WILL PRINT BOTH.                                         *
//* MISSED MONTH: RERUN WITH //STEP.PARMIN DD * AND 'MONTH=MM'.       *
//* RC 4 = CUSTOMER NOT ON CIF OR REFUND EXCEEDS CHARGE - REPORT TO   *
//* CHARGING OPERATIONS.                                              *
//*-------------------------------------------------------------------*
//* 2011-10-03 AOS  ORIGINAL                                          *
//* 2015-02-09 PDQ  INTPAID ADDED TO PLAN                             *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* CT010 - BUILD SUMMARY LETTERS (CHGTAR10)                          *
//*-------------------------------------------------------------------*
//CT010    EXEC MBKDB2B
//STEP.PARMIN   DD DUMMY,DCB=(RECFM=FB,LRECL=80)
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.LETTERS  DD DSN=MBK.PRD.CHG.ANNSUMM(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=600,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CHT1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(CHGTAR10) PLAN(CHGTAR10) LIB('MBK.PRD.LOADLIB')
 END
/*
