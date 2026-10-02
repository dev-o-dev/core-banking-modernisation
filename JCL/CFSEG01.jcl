//CFSEG01  JOB (MBK,BAT,CFS),'CUSTOMER SEGMENTATION',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CFSEG01                                                *
//* TITLE    : CIF - MONTHLY CUSTOMER SEGMENTATION                    *
//* SCHEDULE : CA-7 MBKMTH - FIRST SATURDAY, BEFORE MIEXT01           *
//* OWNER    : CORE SYSTEMS - CIF / MARKETING IT                      *
//*-------------------------------------------------------------------*
//* SG010  SEGMENT (CFSEG010)                                         *
//*-------------------------------------------------------------------*
//* CHANGES GOES TO THE CRM SYSTEM (CONNECT:DIRECT NODE CRMP01) SO    *
//* RELATIONSHIP MANAGERS ARE ASSIGNED TO NEW PREMIER CUSTOMERS.      *
//*-------------------------------------------------------------------*
//* 2009-02-02 SAT  ORIGINAL                                          *
//* 2010-09-06 JNB  LOANMAST / LOANMAS1 ADDED                         *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* SG010 - SEGMENT (CFSEG010)                                        *
//*-------------------------------------------------------------------*
//SG010    EXEC MBKBATCH,PGM=CFSEG010
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ACCTMAS1 DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST.CIFPATH
//STEP.LOANMAST DD DISP=SHR,DSN=MBK.PRD.LOAN.MASTER
//STEP.LOANMAS1 DD DISP=SHR,DSN=MBK.PRD.LOAN.MASTER.CIFPATH
//STEP.CIFMAST  DD DISP=OLD,DSN=MBK.PRD.CIF.MASTER
//STEP.PARMIN   DD *
PREMIER=000075000
PLUS=   000010000
/*
//STEP.CHANGES  DD DSN=MBK.PRD.CIF.SEGMENT.CHANGES(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,SEG1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
