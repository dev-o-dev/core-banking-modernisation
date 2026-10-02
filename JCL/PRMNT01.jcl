//PRMNT01  JOB (MBK,BAT,PRM),'PRODUCT PARAMETERS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : PRMNT01                                                *
//* TITLE    : PRODUCT PARAMETER CHANGE SETS                          *
//* SCHEDULE : ON REQUEST - PRODUCT CONTROL (CA-7 DEMAND)             *
//* OWNER    : CORE SYSTEMS - PRODUCTS                                *
//*-------------------------------------------------------------------*
//* PR010  APPLY CHANGE SETS (PRMNT010)                               *
//*-------------------------------------------------------------------*
//* CHANGES ARE BUILT WITH ISPF PANEL MBKPRD1. FUTURE-DATED SETS      *
//* ARE CARRIED FORWARD TO THE NEXT RUN (CARRYFWD -> CHANGES).        *
//*-------------------------------------------------------------------*
//* 2010-06-14 JNB  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* PR010 - APPLY CHANGE SETS (PRMNT010)                              *
//*-------------------------------------------------------------------*
//PR010    EXEC MBKBATCH,PGM=PRMNT010
//STEP.CHANGES  DD DISP=SHR,DSN=MBK.PRD.PRD.CHANGES(0)
//STEP.PRDPARM  DD DISP=OLD,DSN=MBK.PRD.PRD.PARMS
//STEP.CARRYFWD DD DSN=MBK.PRD.PRD.CHANGES(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//STEP.AUDIT    DD DSN=MBK.PRD.PRD.AUDIT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=1021,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,PRM1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
