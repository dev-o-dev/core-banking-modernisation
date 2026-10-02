//TAXCR01  JOB (MBK,BAT,TAX),'FATCA CRS RETURN',
//             CLASS=C,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : TAXCR01                                                *
//* TITLE    : FATCA / CRS ANNUAL RETURN TO HMRC AND SELF-CERT CHASE  *
//* SCHEDULE : ON REQUEST - FINANCE TAX, JANUARY (RETURN DUE 31 MAY)  *
//* OWNER    : CORE SYSTEMS - CIF / FINANCE TAX                       *
//*-------------------------------------------------------------------*
//* TX010  RETURN + CHASE LIST (TAXCR010)                             *
//*-------------------------------------------------------------------*
//* EDIT THE RPTYEAR AND USDRATE CARDS BEFORE SUBMITTING (USDRATE IS  *
//* THE GBP/USD SPOT AT 31 DECEMBER FROM TREASURY). CRSOUT IS LOADED  *
//* INTO THE HMRC AEOI SERVICE BY FINANCE - NOT SENT FROM THE HOST.   *
//* CHASE GOES TO DOCUMENT SERVICES (TEMPLATE TAXSC01) VIA MBKCOPY.   *
//* TAXCERT IS A CICS FILE - RUN AFTER MBKONLDN (IT IS UPDATED).      *
//* RC 4 = CHASE ENTRIES WRITTEN (NORMAL).                            *
//*-------------------------------------------------------------------*
//* 2016-11-14 HBK  ORIGINAL (FIRST CRS RETURN)                       *
//* 2017-11-20 HBK  USDRATE CARD (FATCA THRESHOLD)                    *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* TX010 - RETURN + CHASE LIST (TAXCR010)                            *
//*-------------------------------------------------------------------*
//TX010    EXEC MBKDB2B
//STEP.PARMIN   DD *
RPTYEAR=2017
USDRATE=1.3510
/*
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ACCTMAS1 DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST.CIFPATH
//STEP.TAXCERT  DD DISP=OLD,DSN=MBK.PRD.TAX.SELFCERT
//STEP.CRSOUT   DD DSN=MBK.PRD.TAX.CRSRETN(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.CHASE    DD DSN=MBK.PRD.TAX.CRSCHASE(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,TAX2),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(TAXCR010) PLAN(TAXCR010) LIB('MBK.PRD.LOADLIB')
 END
/*
