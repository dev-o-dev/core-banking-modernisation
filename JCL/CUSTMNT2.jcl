//CUSTMNT2 JOB (MBK,EOD,CIF),'IBANK CUST MAINT',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CUSTMNT2                                               *
//* TITLE    : APPLY IBANK CUSTOMER MAINTENANCE TO THE CIF            *
//* SCHEDULE : CA-7 NETWORK MBKEOD - AFTER MBKONLDN (CIFMAST CLOSED   *
//*            IN CICS). TRIGGERED BY ARRIVAL OF                      *
//*            MBK.PRD.IBANK.CUSTMNT2.INPUT (C:D FROM IBANK 22:30)    *
//* OWNER    : CORE SYSTEMS - CIF         ON-CALL: ROTA CS-CIF        *
//*-------------------------------------------------------------------*
//* CM010  SORT HEADER / DETAILS BY CIF + TIME / TRAILER              *
//* CM020  APPLY TO CIF (CFMNT201) - AUDIT IMAGES TO TAPE             *
//* CM030  RETURN REJECTS TO IBANK SUPPORT (ONLY IF CM020 RC=4)       *
//*-------------------------------------------------------------------*
//* RESTART: CM020 IS NOT RE-RUNNABLE AFTER A PARTIAL UPDATE - THE    *
//* SAME CHANGES WOULD BE RE-APPLIED (HARMLESS) BUT DUPLICATE AUDIT   *
//* RECORDS WOULD BE WRITTEN. CALL CIF ON-CALL BEFORE RERUNNING.      *
//*-------------------------------------------------------------------*
//* 2005-01-10 AOS  ORIGINAL (CUSTMNT1)                               *
//* 2007-02-26 JNB  CUSTMNT2                                          *
//* 2008-06-16 JNB  CM030 - REJECTS RETURNED TO IBANK                 *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//CM010    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.IBANK.CUSTMNT2.INPUT(0)
//SORTOUT  DD DSN=&&CMSORT,DISP=(NEW,PASS),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//SYSIN    DD DISP=SHR,DSN=MBK.PRD.CNTL(CUSTSORT)
//*
//CM020    EXEC MBKBATCH,PGM=CFMNT201,COND=(0,LT)
//STEP.MAINTIN  DD DISP=(OLD,DELETE),DSN=&&CMSORT
//STEP.CIFMAST  DD DISP=OLD,DSN=MBK.PRD.CIF.MASTER
//STEP.AUDITOUT DD DSN=MBK.PRD.CIF.AUDIT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=VTAPE,
//            LABEL=(1,SL,RETPD=2557),
//            DCB=(RECFM=FB,LRECL=1500,BLKSIZE=0)
//STEP.REJECTS  DD DSN=MBK.PRD.IBANK.CUSTMNT2.REJECTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=404,BLKSIZE=0)
//*
//*-------------------------------------------------------------------*
//* CM030 - RUN ONLY WHEN CM020 ENDED RC=4 (REJECTS WRITTEN)          *
//*-------------------------------------------------------------------*
//CM030    EXEC PGM=DMBATCH,PARM=(YYSLYNN),
//             COND=((4,NE,CM020.STEP))
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDMGLOAD
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMMSGFIL DD DISP=SHR,DSN=CDZ.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON USERID=(MBKCDP) ESF=NO
  SUBMIT PROC=MBKCOPY CASE=YES MAXDELAY=UNLIMITED                   -
         &DSN=MBK.PRD.IBANK.CUSTMNT2.REJECTS(0)                    -
         &RNODE=MBK.UNIX.IBANK                                     -
         &RDSN='/data/ibank/returns/CUSTMNT2.rej'                  -
         &SYSOPT=':DATATYPE=TEXT:XLATE=YES:'
  SIGNOFF
/*
