//INACR01  JOB (MBK,BAT,INA),'DAILY INTEREST ACCRUAL',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : INACR01                                                *
//* TITLE    : DAILY CREDIT AND DEBIT INTEREST ACCRUAL                *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01                            *
//* OWNER    : CORE SYSTEMS - DDA      ON-CALL: ROTA CS-DDA           *
//*-------------------------------------------------------------------*
//* IN010  ACCRUE INTEREST (INACR010)                                 *
//*-------------------------------------------------------------------*
//* RERUNNABLE: INACR010 SKIPS ACCOUNTS ALREADY ACCRUED TO TODAY.     *
//* RC 4 = ACCOUNTS WITH NO PRODUCT PARAMETERS (SEE EXCEPTS).         *
//*-------------------------------------------------------------------*
//* 2010-03-08 AOS  ORIGINAL (CR 10/0141)                             *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* IN010 - ACCRUE INTEREST (INACR010)                                *
//*-------------------------------------------------------------------*
//IN010    EXEC MBKBATCH,PGM=INACR010
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ACCRUALS DD DSN=MBK.PRD.INT.ACCRUALS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.ACCR(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,INA1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.EXCEPTS  DD DSN=MBK.PRD.INT.ACCR.EXCEPTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
