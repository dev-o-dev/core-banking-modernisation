//CHLGSWP1 JOB (MBK,BAT,CHL),'CHANNEL LOG SWEEP',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CHLGSWP1                                               *
//* TITLE    : CHANNEL AUDIT LOG SWEEP AND USAGE REPORT               *
//* SCHEDULE : CA-7 MBKEOD - 00:30 DAILY                              *
//* OWNER    : CORE SYSTEMS - CHANNELS                                *
//*-------------------------------------------------------------------*
//* CL005  CLOSE THE QUEUE                                            *
//* CL010  COPY THE LOG                                               *
//* CL012  EMPTY THE LOG (SEQUENTIAL - NOT REPRO REUSE)               *
//* CL015  REOPEN THE QUEUE (ALWAYS)                                  *
//* CL020  USAGE REPORT (CHLGRPT1)                                    *
//*-------------------------------------------------------------------*
//* CL005 CLOSES TDQ CHLG (MBKOPCMD) SO CHROUTER QUEUES ITS AUDIT     *
//* RECORDS IN CICS WHILE CL010 COPIES AND EMPTIES MBK.PRD.CICS.CHLG. *
//* CL015 ALWAYS REOPENS THE QUEUE - CHECK IT IF THE JOB FAILS.       *
//*-------------------------------------------------------------------*
//* 2001-01-22 DKP  ORIGINAL                                          *
//* 2010-08-02 JNB  SLA CARD                                          *
//* 2014-08-11 PDQ  CLOSE / REOPEN STEPS (SWEPT AN OPEN DATASET)      *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* CL005 - CLOSE THE QUEUE                                           *
//*-------------------------------------------------------------------*
//CL005    EXEC PGM=MBKOPCMD
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
F CICSPRD1,CEMT SET TDQUEUE(CHLG) CLOSED
/*
//*-------------------------------------------------------------------*
//* CL010 - COPY THE LOG                                              *
//*-------------------------------------------------------------------*
//CL010    EXEC PGM=IDCAMS,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//LOGIN    DD DISP=SHR,DSN=MBK.PRD.CICS.CHLG
//LOGOUT   DD DSN=MBK.PRD.CICS.CHLG.DAILY(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=160,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(LOGIN) OUTFILE(LOGOUT)
/*
//*-------------------------------------------------------------------*
//* CL012 - EMPTY THE LOG (SEQUENTIAL - NOT REPRO REUSE)              *
//*-------------------------------------------------------------------*
//CL012    EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY,DCB=(RECFM=FB,LRECL=160)
//SYSUT2   DD DISP=OLD,DSN=MBK.PRD.CICS.CHLG
//*-------------------------------------------------------------------*
//* CL015 - REOPEN THE QUEUE (ALWAYS)                                 *
//*-------------------------------------------------------------------*
//CL015    EXEC PGM=MBKOPCMD,COND=EVEN
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
F CICSPRD1,CEMT SET TDQUEUE(CHLG) OPEN
/*
//*-------------------------------------------------------------------*
//* CL020 - USAGE REPORT (CHLGRPT1)                                   *
//*-------------------------------------------------------------------*
//CL020    EXEC MBKBATCH,PGM=CHLGRPT1,COND=(4,LT)
//STEP.CHLG     DD DISP=SHR,DSN=MBK.PRD.CICS.CHLG.DAILY(+1)
//STEP.PARMIN   DD *
SLA=02000
/*
//STEP.SUMMARY  DD DSN=MBK.PRD.CICS.CHLG.SUMMARY(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CHL1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
