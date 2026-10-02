//MBKBATCH PROC PGM=,HLQ='MBK.PRD',DUMP='SYSOUT=D'
//*********************************************************************
//* PROC     : MBKBATCH                                               *
//* TITLE    : STANDARD NON-DB2 BATCH COBOL STEP                      *
//*-------------------------------------------------------------------*
//* SUPPLIES STEPLIB, DATECARD AND THE STANDARD PRINT/DUMP DDS.       *
//* THE CALLING JOB ADDS THE PROGRAM'S OWN FILES AS STEP.DDNAME       *
//* OVERRIDES, E.G.                                                   *
//*   //CR010    EXEC MBKBATCH,PGM=CRDM0101                           *
//*   //STEP.MAINTIN DD DISP=SHR,DSN=...                              *
//*-------------------------------------------------------------------*
//* 1998-11-02 MLW  LE RUNTIME (CEE.SCEERUN) REPLACES COB2LIB         *
//*********************************************************************
//STEP     EXEC PGM=&PGM,REGION=0M
//STEPLIB  DD DISP=SHR,DSN=&HLQ..LOADLIB
//         DD DISP=SHR,DSN=CEE.SCEERUN
//DATECARD DD DISP=SHR,DSN=&HLQ..CNTL(BUSDATE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//CEEDUMP  DD SYSOUT=*
//SYSUDUMP DD &DUMP
//         PEND
