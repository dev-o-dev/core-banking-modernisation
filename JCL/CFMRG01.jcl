//CFMRG01  JOB (MBK,BAT,CFM),'CUSTOMER MERGES',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CFMRG01                                                *
//* TITLE    : CIF - MERGE CONFIRMED DUPLICATE CUSTOMERS              *
//* SCHEDULE : CA-7 MBKWKLY - SATURDAY 20:00, BEFORE MBKEOD           *
//* OWNER    : CORE SYSTEMS - CIF                                     *
//*-------------------------------------------------------------------*
//* MG010  MERGE (CFMRG010)                                           *
//*-------------------------------------------------------------------*
//* MERGEIN IS BUILT BY THE CIF DATA QUALITY TEAM FROM REVIEWED       *
//* CFDUP01 CANDIDATES (FOUR-EYES APPROVAL).                          *
//*-------------------------------------------------------------------*
//* 2011-06-06 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* MG010 - MERGE (CFMRG010)                                          *
//*-------------------------------------------------------------------*
//MG010    EXEC MBKBATCH,PGM=CFMRG010
//STEP.MERGEIN  DD DISP=SHR,DSN=MBK.PRD.CIF.MERGE.APPROVED(0)
//STEP.CIFMAST  DD DISP=OLD,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ACCTMAS1 DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST.CIFPATH
//STEP.LOANMAST DD DISP=OLD,DSN=MBK.PRD.LOAN.MASTER
//STEP.LOANMAS1 DD DISP=SHR,DSN=MBK.PRD.LOAN.MASTER.CIFPATH
//STEP.AUDIT    DD DSN=MBK.PRD.CIF.MERGE.AUDIT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,MRG1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
