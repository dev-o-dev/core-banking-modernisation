//CFDUP01  JOB (MBK,BAT,CFD),'DUPLICATE CUSTOMERS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CFDUP01                                                *
//* TITLE    : CIF DUPLICATE CUSTOMER DETECTION                       *
//* SCHEDULE : CA-7 MBKMTH - FIRST SATURDAY                           *
//* OWNER    : CORE SYSTEMS - CIF                                     *
//*-------------------------------------------------------------------*
//* DU010  DETECT (CFDUP010)                                          *
//*-------------------------------------------------------------------*
//* 2011-04-11 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* DU010 - DETECT (CFDUP010)                                         *
//*-------------------------------------------------------------------*
//DU010    EXEC MBKBATCH,PGM=CFDUP010
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(200,50))
//STEP.SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(200,50))
//STEP.SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(200,50))
//STEP.DUPCAND  DD DSN=MBK.PRD.CIF.DUPCAND(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,DUP1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
