//MBKDB2B  PROC HLQ='MBK.PRD',DB2LIB='DSN910.SDSNLOAD',
//             DUMP='SYSOUT=D'
//*********************************************************************
//* PROC     : MBKDB2B                                                *
//* TITLE    : STANDARD DB2 BATCH COBOL STEP (TSO TERMINAL MONITOR)   *
//*-------------------------------------------------------------------*
//* THE CALLING JOB MUST SUPPLY SYSTSIN WITH THE DSN RUN COMMAND,     *
//* BECAUSE INSTREAM DATA IN A PROC CANNOT USE SYMBOLICS:             *
//*   //PS020    EXEC MBKDB2B                                         *
//*   //STEP.SYSTSIN DD *                                             *
//*    DSN SYSTEM(DB2P)                                               *
//*    RUN PROGRAM(PSEOD010) PLAN(PSEOD010) LIB('MBK.PRD.LOADLIB')    *
//*    END                                                            *
//*   /*                                                              *
//* A NON-ZERO RC FROM THE PROGRAM IS PASSED BACK AS THE STEP RC.     *
//*-------------------------------------------------------------------*
//* 1999-08-16 KLM  ORIGINAL (PROJECT HORIZON - FIRST DB2 BATCH)      *
//* 2008-01-14 KLM  DB2 V9 LOAD LIBRARY                               *
//*********************************************************************
//STEP     EXEC PGM=IKJEFT01,DYNAMNBR=20,REGION=0M
//STEPLIB  DD DISP=SHR,DSN=&HLQ..LOADLIB
//         DD DISP=SHR,DSN=&DB2LIB
//         DD DISP=SHR,DSN=CEE.SCEERUN
//DATECARD DD DISP=SHR,DSN=&HLQ..CNTL(BUSDATE)
//SYSTSPRT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//CEEDUMP  DD SYSOUT=*
//SYSUDUMP DD &DUMP
//         PEND
