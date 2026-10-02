//INCAP01  JOB (MBK,BAT,INC),'INTEREST CAPITALISATION',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : INCAP01                                                *
//* TITLE    : CAPITALISE ACCRUED INTEREST AND DEDUCT TAX             *
//* SCHEDULE : CA-7 MBKEOD - AFTER INACR01, BEFORE GLFEED01           *
//* OWNER    : CORE SYSTEMS - DDA      ON-CALL: ROTA CS-DDA           *
//*-------------------------------------------------------------------*
//* IN020  CAPITALISE (INCAP010)                                      *
//*-------------------------------------------------------------------*
//* INTPOST IS HARD POSTED BY TOMORROW'S PSEOD01 (PS010 SORTIN).      *
//* NOT RERUNNABLE AFTER COMMIT - CONTACT DDA ON-CALL. MBK.INTPAID    *
//* ROWS FOR THE RUN MUST BE DELETED BY THE DBA BEFORE A RERUN.       *
//*-------------------------------------------------------------------*
//* 2010-03-08 AOS  ORIGINAL (CR 10/0141)                             *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* IN020 - CAPITALISE (INCAP010)                                     *
//*-------------------------------------------------------------------*
//IN020    EXEC MBKDB2B
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.INTPOST  DD DSN=MBK.PRD.PST.FEED.INTPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(20,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.ICAP(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REGISTER DD SYSOUT=(R,,INC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(INCAP010) PLAN(INCAP010) LIB('MBK.PRD.LOADLIB')
 END
/*
