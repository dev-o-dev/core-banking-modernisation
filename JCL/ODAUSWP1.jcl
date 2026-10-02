//ODAUSWP1 JOB (MBK,BAT,ODA),'ODAU AUDIT SWEEP',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : ODAUSWP1                                               *
//* TITLE    : SWEEP CICS TDQ ODAU - OVERDRAFT DECISION AUDIT         *
//* SCHEDULE : CA-7 MBKEOD - 00:40 DAILY (AFTER CICS SWITCHES THE     *
//*            QUEUE DATASET)                                         *
//* OWNER    : CORE SYSTEMS - CHANNELS                                *
//*-------------------------------------------------------------------*
//* SW005  CLOSE THE QUEUE                                            *
//* SW010  COPY THE LOG                                               *
//* SW012  EMPTY THE LOG (SEQUENTIAL - NOT REPRO REUSE)               *
//* SW020  REOPEN THE QUEUE (ALWAYS)                                  *
//*-------------------------------------------------------------------*
//* THE EXTRA-PARTITION QUEUE ODAU WRITES TO MBK.PRD.CICS.ODAU. AT    *
//*            00:30 OPERATIONS                                       *
//* AUTOMATION CLOSES THE QUEUE, THIS JOB COPIES AND EMPTIES THE      *
//*            DATASET,                                               *
//* THEN REOPENS THE QUEUE (MBKOPCMD). THE COPY GOES TO CREDIT RISK MI*
//*            (CONNECT:DIRECT NODE CRMIP01).                         *
//*-------------------------------------------------------------------*
//* 2012-04-23 PLW  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* SW005 - CLOSE THE QUEUE                                           *
//*-------------------------------------------------------------------*
//SW005    EXEC PGM=MBKOPCMD
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
F CICSPRD1,CEMT SET TDQUEUE(ODAU) CLOSED
/*
//*-------------------------------------------------------------------*
//* SW010 - COPY THE LOG                                              *
//*-------------------------------------------------------------------*
//SW010    EXEC PGM=IDCAMS,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//QIN      DD DISP=SHR,DSN=MBK.PRD.CICS.ODAU
//QOUT     DD DSN=MBK.PRD.CICS.ODAU.DAILY(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(QIN) OUTFILE(QOUT)
/*
//*-------------------------------------------------------------------*
//* SW012 - EMPTY THE LOG (SEQUENTIAL - NOT REPRO REUSE)              *
//*-------------------------------------------------------------------*
//SW012    EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY,DCB=(RECFM=FB,LRECL=250)
//SYSUT2   DD DISP=OLD,DSN=MBK.PRD.CICS.ODAU
//*-------------------------------------------------------------------*
//* SW020 - REOPEN THE QUEUE (ALWAYS)                                 *
//*-------------------------------------------------------------------*
//SW020    EXEC PGM=MBKOPCMD,COND=EVEN
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
F CICSPRD1,CEMT SET TDQUEUE(ODAU) OPEN
/*
