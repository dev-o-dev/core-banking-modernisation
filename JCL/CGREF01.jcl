//CGREF01  JOB (MBK,BAT,CGR),'CHARGE REFUNDS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CGREF01                                                *
//* TITLE    : CHARGE REFUNDS (DUAL AUTHORISED)                       *
//* SCHEDULE : CA-7 MBKEOD - DAILY, AFTER CGCHG01                     *
//* OWNER    : CORE SYSTEMS - DDA / COMPLAINTS                        *
//*-------------------------------------------------------------------*
//* CR010  REFUNDS (CGREF010)                                         *
//*-------------------------------------------------------------------*
//* 2009-11-02 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* CR010 - REFUNDS (CGREF010)                                        *
//*-------------------------------------------------------------------*
//CR010    EXEC MBKDB2B
//STEP.REFUNDIN DD DISP=SHR,DSN=MBK.PRD.CHG.REFUNDS.APPROVED(0)
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.REFPOST  DD DSN=MBK.PRD.PST.FEED.REFPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.CHRF(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CGR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(CGREF010) PLAN(CGREF010) LIB('MBK.PRD.LOADLIB')
 END
/*
