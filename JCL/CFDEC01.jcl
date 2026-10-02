//CFDEC01  JOB (MBK,BAT,CFD),'BEREAVEMENT',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CFDEC01                                                *
//* TITLE    : BEREAVEMENT - DECEASED CUSTOMER PROCESSING             *
//* SCHEDULE : CA-7 MBKEOD - 19:00, BEFORE PSEOD01 (POSTINGS GO       *
//*            TONIGHT)                                               *
//* OWNER    : CORE SYSTEMS - CIF                                     *
//*-------------------------------------------------------------------*
//* DE010  BEREAVEMENT INSTRUCTIONS (CFDEC010)                        *
//*-------------------------------------------------------------------*
//* DECPOST IS ADDED TO TONIGHT'S PSEOD01 PS010 SORTIN.               *
//* PAYOUT IS PRINTED FOR THE BEREAVEMENT TEAM TO KEY AS CHAPS.       *
//*-------------------------------------------------------------------*
//* 2011-02-14 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* DE010 - BEREAVEMENT INSTRUCTIONS (CFDEC010)                       *
//*-------------------------------------------------------------------*
//DE010    EXEC MBKBATCH,PGM=CFDEC010
//STEP.BEREAVE  DD DISP=SHR,DSN=MBK.PRD.CIF.BEREAVE.IN(0)
//STEP.CIFMAST  DD DISP=OLD,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ACCTMAS1 DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST.CIFPATH
//STEP.SOMAST   DD DISP=OLD,DSN=MBK.PRD.SO.MASTER
//STEP.DDMAND   DD DISP=OLD,DSN=MBK.PRD.DDM.MANDATE
//STEP.LOANMAST DD DISP=OLD,DSN=MBK.PRD.LOAN.MASTER
//STEP.LOANMAS1 DD DISP=SHR,DSN=MBK.PRD.LOAN.MASTER.CIFPATH
//STEP.DECPOST  DD DSN=MBK.PRD.PST.FEED.DECPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.PAYOUT   DD DSN=MBK.PRD.CIF.BEREAVE.PAYOUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.CIF.BEREAVE.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,DEC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
