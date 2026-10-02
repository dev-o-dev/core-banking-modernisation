//INRAT01  JOB (MBK,BAT,INR),'RATE CHANGES',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : INRAT01                                                *
//* TITLE    : INTEREST RATE CHANGE - ANNOUNCE AND APPLY              *
//* SCHEDULE : ON REQUEST (TREASURY) - CA-7 DEMAND, THEN DAILY UNTIL  *
//*            APPLIED                                                *
//* OWNER    : CORE SYSTEMS - DDA                                     *
//*-------------------------------------------------------------------*
//* IN030  ANNOUNCE / APPLY RATE CHANGES (INRAT010)                   *
//*-------------------------------------------------------------------*
//* RATEIN IS BUILT BY ISPF DIALOG MBKRT1 (REXX MBKRTCH) AS           *
//* MBK.PRD.INT.RATECHG.INPUT(+1) ON RELEASE.                         *
//*-------------------------------------------------------------------*
//* 2010-04-12 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* IN030 - ANNOUNCE / APPLY RATE CHANGES (INRAT010)                  *
//*-------------------------------------------------------------------*
//IN030    EXEC MBKBATCH,PGM=INRAT010
//STEP.RATEIN   DD DISP=SHR,DSN=MBK.PRD.INT.RATECHG.INPUT(0)
//STEP.RATEPEND DD DISP=OLD,DSN=MBK.PRD.INT.RATEPEND
//STEP.PRDPARM  DD DISP=OLD,DSN=MBK.PRD.PRD.PARMS
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.NOTICES  DD DSN=MBK.PRD.INT.RATE.NOTICES(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,INR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
