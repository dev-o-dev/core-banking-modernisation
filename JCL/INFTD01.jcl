//INFTD01  JOB (MBK,BAT,INF),'FIXED TERM MATURITIES',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : INFTD01                                                *
//* TITLE    : FIXED TERM DEPOSIT MATURITY NOTICES AND ACTIONS        *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01 AND INCAP01                *
//* OWNER    : CORE SYSTEMS - SAVINGS                                 *
//*-------------------------------------------------------------------*
//* FT010  MATURITIES (INFTD010)                                      *
//*-------------------------------------------------------------------*
//* MATPOST IS HARD POSTED BY TOMORROW'S PSEOD01.                     *
//*-------------------------------------------------------------------*
//* 2010-11-01 JNB  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* FT010 - MATURITIES (INFTD010)                                     *
//*-------------------------------------------------------------------*
//FT010    EXEC MBKBATCH,PGM=INFTD010
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.MATPOST  DD DSN=MBK.PRD.PST.FEED.MATPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.NOTICES  DD DSN=MBK.PRD.FTD.NOTICES(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,FTD1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
