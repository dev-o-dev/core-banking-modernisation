//ATMSTL01 JOB (MBK,BAT,ATM),'LINK ATM SETTLEMENT',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : ATMSTL01                                               *
//* TITLE    : LINK ATM NETWORK - DAILY SETTLEMENT AND POSTING        *
//* SCHEDULE : CA-7 MBKEOD - AFTER MBKONLDN AND THE LINK SETTLEMENT   *
//*            FILE (EVENT ATMLINK), BEFORE PSEOD01                   *
//* OWNER    : PAYMENTS IT - CARDS     ON-CALL: ROTA PAY              *
//*-------------------------------------------------------------------*
//* AT010  SETTLE AND POST (ATMSTL10)                                 *
//*-------------------------------------------------------------------*
//* ATMPOST IS A PSEOD01 FEED (DISP=MOD) - POSTED TONIGHT.            *
//*         MBK.PRD.PST.FEED.ATMPOST MUST BE IN PSEOD01 STEPS PS004,  *
//*         PS010 AND PS035 LIKE THE OTHER FEEDS.                     *
//* GLJNL  IS PICKED UP BY GLFEED01 (SOURCE ATMS).                    *
//* RC 4   = EXCEPTIONS (SUSPENSE / RESTRICTED ACCOUNTS) - REPORT TO  *
//*          CARD SERVICES, THE NETWORK CONTINUES.                    *
//* U1004  = LINK TRAILER OR NET SETTLEMENT DISAGREES - DO NOT RERUN. *
//*          CALL PAYMENTS OPERATIONS (LINK SETTLEMENT DESK) - THE    *
//*          FILE MUST BE RE-SENT BY LINK BEFORE 10:00 FOR TREASURY.  *
//*-------------------------------------------------------------------*
//* 2014-09-08 RMC  ORIGINAL (LINK MEMBERSHIP)                        *
//* 2017-05-22 RMC  REPORT CLASS TO CARD SERVICES (CR 17/0142)        *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* AT010 - SETTLE AND POST (ATMSTL10)                                *
//*-------------------------------------------------------------------*
//AT010    EXEC MBKBATCH,PGM=ATMSTL10
//STEP.LINKIN   DD DISP=SHR,DSN=MBK.PRD.ATM.LINK.SETTLE(0)
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ATMPOST  DD DSN=MBK.PRD.PST.FEED.ATMPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.ATMS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,ATM1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
