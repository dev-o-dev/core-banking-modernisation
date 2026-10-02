//MBKDB2DL JOB (MBK,DBA,DB2),'CREATE MBKCORE DB2',
//             CLASS=A,MSGCLASS=X,TYPRUN=HOLD
//*********************************************************************
//* JOB      : MBKDB2DL                                               *
//* TITLE    : CREATE DATABASE MBKCORE OBJECTS (DDL MEMBER MBKCORE)   *
//*-------------------------------------------------------------------*
//* RUN BY THE DBA TEAM ONLY. TYPRUN=HOLD.                            *
//*********************************************************************
//DDL010   EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DISP=SHR,DSN=DSN910.SDSNLOAD
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD DISP=SHR,DSN=MBK.PRD.DDL(MBKCORE)
//SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(DSNTEP2) PLAN(DSNTEP91) -
     LIB('DSN910.RUNLIB.LOAD') PARMS('/ALIGN(MID)')
 END
/*
