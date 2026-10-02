//INTAX01  JOB (MBK,BAT,INT),'TAX CERTS AND BBSI',
//             CLASS=C,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : INTAX01                                                *
//* TITLE    : ANNUAL INTEREST CERTIFICATES AND HMRC BBSI RETURN      *
//* SCHEDULE : ON REQUEST - FINANCE, AFTER 5 APRIL                    *
//* OWNER    : CORE SYSTEMS - DDA / FINANCE TAX                       *
//*-------------------------------------------------------------------*
//* TX010  CERTIFICATES + BBSI (INTAX010)                             *
//*-------------------------------------------------------------------*
//* EDIT THE TAXYEAR CARD BEFORE SUBMITTING. CERTS GO TO DOCUMENT     *
//* SERVICES VIA MBKCOPY (CONNECT:DIRECT PROCESS TAXCERT).            *
//*-------------------------------------------------------------------*
//* 2007-04-02 AOS  ORIGINAL                                          *
//* 2009-04-14 AOS  MINBBSI CARD                                      *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* TX010 - CERTIFICATES + BBSI (INTAX010)                            *
//*-------------------------------------------------------------------*
//TX010    EXEC MBKDB2B
//STEP.PARMIN   DD *
TAXYEAR=2010/2011
MINBBSI=000000001
/*
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CERTS    DD DSN=MBK.PRD.TAX.CERTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.BBSI     DD DSN=MBK.PRD.TAX.BBSI(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,TAX1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(INTAX010) PLAN(INTAX010) LIB('MBK.PRD.LOADLIB')
 END
/*
