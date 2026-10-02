//CRDMNT1  JOB (MBK,EOD,CRD),'IBANK CARD MAINT',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CRDMNT1                                                *
//* TITLE    : IBANK CARD MAINTENANCE TO CARD MANAGEMENT SYSTEM       *
//* SCHEDULE : CA-7 22:45 DAILY, TRIGGERED BY ARRIVAL OF              *
//*            MBK.PRD.IBANK.CRDMNT1.INPUT (C:D FROM IBANK 22:35).    *
//*            ACCTMAST IS READ ONLY - RUNS WHILE CICS IS UP.         *
//* OWNER    : CORE SYSTEMS - CARDS IFACE ON-CALL: ROTA CS-CHN        *
//*-------------------------------------------------------------------*
//* CR010  VALIDATE AND REFORMAT (CRDM0101)                           *
//* CR020  SEND HOT CARDS TO CMS - PRIORITY 15 (BEFORE CR030)         *
//* CR030  SEND OTHER MAINTENANCE TO CMS                              *
//*-------------------------------------------------------------------*
//* RESTART: RERUN FROM CR010. CMS DE-DUPLICATES ON CMS-SOURCE-REF.   *
//*-------------------------------------------------------------------*
//* 2006-09-18 AOS  ORIGINAL                                          *
//* 2007-05-21 AOS  HOT CARD FILE SEPARATED (RETAIL FRAUD RA-2006-031)*
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//CR010    EXEC MBKBATCH,PGM=CRDM0101
//STEP.MAINTIN  DD DISP=SHR,DSN=MBK.PRD.IBANK.CRDMNT1.INPUT(0)
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CMSOUT   DD DSN=MBK.PRD.CMS.MAINT.OUTPUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(TRK,(30,15),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.HOTCARD  DD DSN=MBK.PRD.CMS.HOTCARD.OUTPUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REJECTS  DD DSN=MBK.PRD.IBANK.CRDMNT1.REJECTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=204,BLKSIZE=0)
//*
//CR020    EXEC PGM=DMBATCH,PARM=(YYSLYNN),COND=(4,LT)
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDMGLOAD
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMMSGFIL DD DISP=SHR,DSN=CDZ.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON USERID=(MBKCDP) ESF=NO
  SUBMIT PROC=MBKCOPY CASE=YES PRTY=15 MAXDELAY=UNLIMITED           -
         &DSN=MBK.PRD.CMS.HOTCARD.OUTPUT(0)                        -
         &RNODE=CMS.PROCESSOR.PRD                                  -
         &RDSN='MBK001.HOTCARD.INPUT'                              -
         &SYSOPT=''
  SIGNOFF
/*
//CR030    EXEC PGM=DMBATCH,PARM=(YYSLYNN),COND=(4,LT)
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDMGLOAD
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMMSGFIL DD DISP=SHR,DSN=CDZ.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON USERID=(MBKCDP) ESF=NO
  SUBMIT PROC=MBKCOPY CASE=YES MAXDELAY=UNLIMITED                   -
         &DSN=MBK.PRD.CMS.MAINT.OUTPUT(0)                          -
         &RNODE=CMS.PROCESSOR.PRD                                  -
         &RDSN='MBK001.MAINT.INPUT'                                -
         &SYSOPT=''
  SIGNOFF
/*
