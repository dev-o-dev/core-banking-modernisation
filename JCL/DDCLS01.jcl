//DDCLS01  JOB (MBK,BAT,DDC),'ACCOUNT CLOSURES',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : DDCLS01                                                *
//* TITLE    : HOLD EXPIRY AND TWO-NIGHT ACCOUNT CLOSURE              *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01, BEFORE STMT01             *
//* OWNER    : CORE SYSTEMS - DDA                                     *
//*-------------------------------------------------------------------*
//* DC005  EXPIRE HOLDS (DDHLX010)                                    *
//* DC010  CLOSURES (DDCLS010)                                        *
//*-------------------------------------------------------------------*
//* DC010 STAGE 1 MOVES THE BALANCE (CLSPOST) - STAGE 2 ON THE        *
//* FOLLOWING NIGHT CLOSES THE NOW-ZERO ACCOUNT AND REQUESTS THE      *
//* CLOSING STATEMENT (STMTREQ -> STMT01).                            *
//*-------------------------------------------------------------------*
//* 2010-05-17 JNB  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* DC005 - EXPIRE HOLDS (DDHLX010)                                   *
//*-------------------------------------------------------------------*
//DC005    EXEC MBKBATCH,PGM=DDHLX010
//STEP.HOLDS    DD DISP=OLD,DSN=MBK.PRD.DDA.HOLDS
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.REPORT   DD SYSOUT=(R,,DHX1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* DC010 - CLOSURES (DDCLS010)                                       *
//*-------------------------------------------------------------------*
//DC010    EXEC MBKBATCH,PGM=DDCLS010,COND=(4,LT)
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CHGPEND  DD DISP=OLD,DSN=MBK.PRD.CHG.PENDING
//STEP.SOMAST   DD DISP=OLD,DSN=MBK.PRD.SO.MASTER
//STEP.DDMAND   DD DISP=OLD,DSN=MBK.PRD.DDM.MANDATE
//STEP.CLSPOST  DD DSN=MBK.PRD.PST.FEED.CLSPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.STMTREQ  DD DSN=MBK.PRD.STMT.REQUESTS.CLOSE(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.CLOS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,DCL1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
