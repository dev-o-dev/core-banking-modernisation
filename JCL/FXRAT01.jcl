//FXRAT01  JOB (MBK,BAT,FXR),'FX RATE LOAD',
//             CLASS=A,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : FXRAT01                                                *
//* TITLE    : FX RATES - LOAD TREASURY FIXING                        *
//* SCHEDULE : CA-7 EVENT FXFIX (FILE ARRIVAL) - 07:45 AND 14:15      *
//* OWNER    : PAYMENTS IT                                            *
//*-------------------------------------------------------------------*
//* FX010  LOAD RATES (FXRAT010)                                      *
//*-------------------------------------------------------------------*
//* RUNS DURING THE ONLINE DAY. RC 8 = FILE REJECTED, OLD RATES       *
//* REMAIN IN FORCE - CALL TREASURY OPERATIONS.                       *
//*-------------------------------------------------------------------*
//* 2011-05-09 JNB  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* FX010 - LOAD RATES (FXRAT010)                                     *
//*-------------------------------------------------------------------*
//FX010    EXEC MBKDB2B
//STEP.FIXING   DD DISP=SHR,DSN=MBK.PRD.FX.FIXING(0)
//STEP.FXRATES  DD DISP=SHR,DSN=MBK.PRD.FX.RATES
//STEP.REPORT   DD SYSOUT=(R,,FXR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(FXRAT010) PLAN(FXRAT010) LIB('MBK.PRD.LOADLIB')
 END
/*
