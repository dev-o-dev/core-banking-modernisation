//INNTC01  JOB (MBK,BAT,INN),'NOTICE WITHDRAWALS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : INNTC01                                                *
//* TITLE    : NOTICE SAVINGS - PAY MATURED WITHDRAWAL NOTICES        *
//* SCHEDULE : CA-7 MBKEOD - AFTER MBKONLDN, BEFORE PSEOD01           *
//* OWNER    : CORE SYSTEMS - SAVINGS                                 *
//*-------------------------------------------------------------------*
//* NT010  PAY NOTICES (INNTC010)                                     *
//*-------------------------------------------------------------------*
//* NTCPOST IS A PSEOD01 FEED (DISP=MOD) - POSTED TONIGHT.            *
//* SAVNTC IS CLOSED IN CICS BY MBKONLDN.                             *
//*-------------------------------------------------------------------*
//* 2011-07-04 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* NT010 - PAY NOTICES (INNTC010)                                    *
//*-------------------------------------------------------------------*
//NT010    EXEC MBKBATCH,PGM=INNTC010
//STEP.SAVNTC   DD DISP=SHR,DSN=MBK.PRD.SAV.NOTICES
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.NTCPOST  DD DSN=MBK.PRD.PST.FEED.NTCPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.SAV.NOTICE.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,NTC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
