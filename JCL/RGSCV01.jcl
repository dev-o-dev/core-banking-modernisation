//RGSCV01  JOB (MBK,REG,SCV),'FSCS SCV EXTRACT',
//             CLASS=C,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : RGSCV01                                                *
//* TITLE    : FSCS SINGLE CUSTOMER VIEW FILE                         *
//* SCHEDULE : ON REQUEST - FSCS / PRA REQUEST (WITHIN 72 HOURS), AND *
//*            QUARTERLY TEST RUN                                     *
//* OWNER    : REGULATORY REPORTING IT (PROJECT SCV MBK-2010-004)     *
//*-------------------------------------------------------------------*
//* RG020  BUILD SCV FILE (RGSCV020)                                  *
//*-------------------------------------------------------------------*
//* RG010 (THE RGSCV010 STUB) WAS RETIRED WHEN RGSCV020 WENT LIVE.    *
//* THE COMPENSATION LIMIT IS A PARMIN CARD - FSCS CHANGES IT, NOT IT.*
//* DQREPORT LISTS CUSTOMERS FAILING SCV DATA RULES - SEND TO THE     *
//* CIF DATA QUALITY TEAM BEFORE THE FILE IS RELEASED.                *
//*-------------------------------------------------------------------*
//* 2010-04-26 JNB  ORIGINAL (STUB RGSCV010)                          *
//* 2010-12-31 JNB  RG020 RGSCV020 - FULL SCV FILE (PRA SUBMISSION)   *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* RG020 - BUILD SCV FILE (RGSCV020)                                 *
//*-------------------------------------------------------------------*
//RG020    EXEC MBKBATCH,PGM=RGSCV020
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTCIF  DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST.CIFPATH
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.SCVFILE  DD DSN=MBK.PRD.REG.SCV.FILE(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(100,20),RLSE),
//            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//STEP.DQREPORT DD SYSOUT=(R,,SCV2),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,SCV1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.PARMIN   DD *
COMPLIMIT= 008500000
/*
