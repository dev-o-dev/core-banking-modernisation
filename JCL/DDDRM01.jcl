//DDDRM01  JOB (MBK,BAT,DDD),'DORMANCY',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : DDDRM01                                                *
//* TITLE    : DORMANT ACCOUNTS AND RECLAIM FUND CANDIDATES           *
//* SCHEDULE : CA-7 MBKMTH - FIRST SATURDAY OF THE MONTH              *
//* OWNER    : CORE SYSTEMS - DDA                                     *
//*-------------------------------------------------------------------*
//* DR010  DORMANCY (DDDRM010)                                        *
//*-------------------------------------------------------------------*
//* 2008-09-15 JNB  ORIGINAL (DORMANT ACCOUNTS ACT 2008)              *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* DR010 - DORMANCY (DDDRM010)                                       *
//*-------------------------------------------------------------------*
//DR010    EXEC MBKBATCH,PGM=DDDRM010
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.LETTERS  DD DSN=MBK.PRD.DRM.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.RECLAIM  DD DSN=MBK.PRD.DRM.RECLAIM(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,DRM1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
