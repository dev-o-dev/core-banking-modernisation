//CFKYC01  JOB (MBK,BAT,CFK),'KYC REVIEW',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CFKYC01                                                *
//* TITLE    : PERIODIC KYC REVIEW SCHEDULING                         *
//* SCHEDULE : CA-7 MBKWKLY - SUNDAY 06:00                            *
//* OWNER    : FINANCIAL CRIME IT                                     *
//*-------------------------------------------------------------------*
//* KY010  KYC REVIEW (CFKYC010)                                      *
//*-------------------------------------------------------------------*
//* 2011-03-07 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* KY010 - KYC REVIEW (CFKYC010)                                     *
//*-------------------------------------------------------------------*
//KY010    EXEC MBKBATCH,PGM=CFKYC010
//STEP.KYCDONE  DD DISP=SHR,DSN=MBK.PRD.KYC.COMPLETED(0)
//STEP.CIFMAST  DD DISP=OLD,DSN=MBK.PRD.CIF.MASTER
//STEP.KYCCASE  DD DSN=MBK.PRD.KYC.CASES(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.KYC.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,KYC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
