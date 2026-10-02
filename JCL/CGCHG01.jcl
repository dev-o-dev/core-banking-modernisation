//CGCHG01  JOB (MBK,BAT,CGC),'MONTHLY CHARGES',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CGCHG01                                                *
//* TITLE    : CHARGES - CALCULATE, NOTIFY AND APPLY                  *
//* SCHEDULE : CA-7 MBKEOD - DAILY (CYCLE DAY PER PRODUCT), AFTER     *
//*            INACR01                                                *
//* OWNER    : CORE SYSTEMS - DDA                                     *
//*-------------------------------------------------------------------*
//* CG010  CALCULATE (CGCAL010)                                       *
//* CG020  NOTICE EXTRACT (CGNTF010)                                  *
//* CG030  APPLY DUE CHARGES (CGAPL010)                               *
//*-------------------------------------------------------------------*
//* CG010 CALCULATES CHARGES FOR ACCOUNTS AT THEIR CYCLE DAY INTO     *
//* CHGPEND. CG020 EXTRACTS THE 14-DAY NOTICES (PSR 2009). CG030      *
//* APPLIES CHARGES WHOSE NOTICE PERIOD HAS ENDED.                    *
//*-------------------------------------------------------------------*
//* 2009-10-05 AOS  ORIGINAL (PSR 2009 PRE-NOTIFICATION)              *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* CG010 - CALCULATE (CGCAL010)                                      *
//*-------------------------------------------------------------------*
//CG010    EXEC MBKDB2B
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CHGPEND  DD DISP=OLD,DSN=MBK.PRD.CHG.PENDING
//STEP.REPORT   DD SYSOUT=(R,,CGC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(CGCAL010) PLAN(CGCAL010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* CG020 - NOTICE EXTRACT (CGNTF010)                                 *
//*-------------------------------------------------------------------*
//CG020    EXEC MBKBATCH,PGM=CGNTF010,COND=(4,LT)
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.CHGPEND  DD DISP=OLD,DSN=MBK.PRD.CHG.PENDING
//STEP.NOTIFY   DD DSN=MBK.PRD.CHG.NOTICES(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CGN1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* CG030 - APPLY DUE CHARGES (CGAPL010)                              *
//*-------------------------------------------------------------------*
//CG030    EXEC MBKDB2B,COND=(4,LT)
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CHGPEND  DD DISP=OLD,DSN=MBK.PRD.CHG.PENDING
//STEP.CHGPOST  DD DSN=MBK.PRD.PST.FEED.CHGPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.CHRG(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CGA1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(CGAPL010) PLAN(CGAPL010) LIB('MBK.PRD.LOADLIB')
 END
/*
