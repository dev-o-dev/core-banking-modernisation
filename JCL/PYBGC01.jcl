//PYBGC01  JOB (MBK,BAT,PAY),'BRANCH CREDITS BGC',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : PYBGC01                                                *
//* TITLE    : BRANCH PAYING-IN SLIPS AND BANK GIRO CREDITS           *
//* SCHEDULE : CA-7 MBKEOD - AFTER MBKONLDN AND THE IMAGE CAPTURE     *
//*            FILE (EVENT BRNIMG), BEFORE PSEOD01 AND CQCLR01        *
//* OWNER    : PAYMENTS IT - CHEQUES / BRANCH     ON-CALL: ROTA PAY   *
//*-------------------------------------------------------------------*
//* BG010  CREDIT, CLEAR AND RECONCILE (PYBGC010)                     *
//*-------------------------------------------------------------------*
//* BGCPOST IS A PSEOD01 FEED (DISP=MOD) - POSTED TONIGHT.            *
//*         MBK.PRD.PST.FEED.BGCPOST MUST BE IN PSEOD01 STEPS PS004,  *
//*         PS010 AND PS035 LIKE THE OTHER FEEDS.                     *
//* CLROUT  GOES TO THE CLEARING AGENT (IPSL) VIA MBKCOPY PROCESS     *
//*         CLROUT1 - MUST BE SENT BY 01:30 FOR THE ICS CUT-OFF.      *
//* UNCLEAR IS BATCH-ONLY (TELLER WRITES VIA NIGHTLY LOAD) - DISP=OLD.*
//* GLJNL   IS PICKED UP BY GLFEED01 (SOURCE BGCS).                   *
//* RC 4    = SLIP DIFFERENCES / SUSPENSE ITEMS FOR THE BRANCHES.     *
//* U1004   = IMAGE TRAILER MISMATCH - FILE INCOMPLETE. DO NOT RERUN  *
//*           UNTIL BRANCH OPERATIONS RESEND THE FILE. A RERUN AFTER A*
//*           LATER FAILURE IS SAFE - PROCESSED SLIPS ARE SKIPPED.    *
//*-------------------------------------------------------------------*
//* 2015-02-23 GTW  ORIGINAL (BRANCH IMAGE CAPTURE)                   *
//* 2016-05-09 GTW  GLJNL - BRANCH SUSPENSE JOURNAL                   *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* BG010 - CREDIT, CLEAR AND RECONCILE (PYBGC010)                    *
//*-------------------------------------------------------------------*
//BG010    EXEC MBKBATCH,PGM=PYBGC010
//STEP.IMGCAP   DD DISP=SHR,DSN=MBK.PRD.BRN.IMGCAP(0)
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.UNCLEAR  DD DISP=OLD,DSN=MBK.PRD.CHQ.UNCLEARED
//STEP.BGCPOST  DD DSN=MBK.PRD.PST.FEED.BGCPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.CLROUT   DD DSN=MBK.PRD.CHQ.OUTWARD(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.BGCS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,BGC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
