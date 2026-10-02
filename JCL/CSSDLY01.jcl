//CSSDLY01 JOB (MBK,BAT,CSS),'CURRENT ACCOUNT SWITCH',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CSSDLY01                                               *
//* TITLE    : CURRENT ACCOUNT SWITCH SERVICE (CASS) - DAILY CYCLE    *
//* SCHEDULE : CA-7 MBKEOD - AFTER MBKONLDN AND THE CASS INBOUND FILE *
//*            (EVENT CASSIN)                                         *
//* OWNER    : PAYMENTS IT     ON-CALL: ROTA PAY                      *
//*-------------------------------------------------------------------*
//* CS010  INBOUND MESSAGES (CSSIN010)                                *
//* CS020  OUTBOUND MESSAGES (CSSOU010)                               *
//*-------------------------------------------------------------------*
//* CASSOUT MUST REACH BACSTEL-IP BY 02:00 (CASS CUT-OFF). CSSPOST    *
//* IS A PSEOD01 FEED (DISP=MOD) - POSTED TONIGHT.                    *
//* SWITCHES IS A CICS FILE (BRANCH SWITCH REQUESTS) - CLOSED BY      *
//*            MBKONLDN.                                              *
//*-------------------------------------------------------------------*
//* 2013-06-17 PDQ  ORIGINAL (CASS GO-LIVE 16/09/2013)                *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* CS010 - INBOUND MESSAGES (CSSIN010)                               *
//*-------------------------------------------------------------------*
//CS010    EXEC MBKBATCH,PGM=CSSIN010
//STEP.CASSIN   DD DISP=SHR,DSN=MBK.PRD.CASS.INBOUND(0)
//STEP.SWITCHES DD DISP=SHR,DSN=MBK.PRD.CASS.SWITCHES
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.SOMAST   DD DISP=OLD,DSN=MBK.PRD.SO.MASTER
//STEP.DDMAND   DD DISP=OLD,DSN=MBK.PRD.DDM.MANDATE
//STEP.CSSPOST  DD DSN=MBK.PRD.PST.FEED.CSSPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.NOTIFY   DD DSN=MBK.PRD.CASS.NOTIFY(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CSI1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* CS020 - OUTBOUND MESSAGES (CSSOU010)                              *
//*-------------------------------------------------------------------*
//CS020    EXEC MBKBATCH,PGM=CSSOU010,COND=(4,LT)
//STEP.SWITCHES DD DISP=SHR,DSN=MBK.PRD.CASS.SWITCHES
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.SOMAST   DD DISP=OLD,DSN=MBK.PRD.SO.MASTER
//STEP.DDMAND   DD DISP=OLD,DSN=MBK.PRD.DDM.MANDATE
//STEP.CASSOUT  DD DSN=MBK.PRD.CASS.OUTBOUND(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.CSSPOST  DD DSN=MBK.PRD.PST.FEED.CSSPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CSO1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
