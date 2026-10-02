//CPAUSWP1 JOB (MBK,BAT,CPA),'CPAU AUDIT SWEEP',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CPAUSWP1                                               *
//* TITLE    : SWEEP CICS TDQ CPAU - CONTACT PREFERENCE AUDIT         *
//* SCHEDULE : CA-7 MBKEOD - 00:40 DAILY (AFTER CICS SWITCHES THE     *
//*            QUEUE DATASET)                                         *
//* OWNER    : CORE SYSTEMS - CHANNELS                                *
//*-------------------------------------------------------------------*
//* SW005  CLOSE THE QUEUE                                            *
//* SW010  COPY THE LOG                                               *
//* SW012  EMPTY THE LOG (SEQUENTIAL - NOT REPRO REUSE)               *
//* SW020  REOPEN THE QUEUE (ALWAYS)                                  *
//*-------------------------------------------------------------------*
//* THE EXTRA-PARTITION QUEUE CPAU WRITES TO MBK.PRD.CICS.CPAU. AT    *
//*            00:30 OPERATIONS                                       *
//* AUTOMATION CLOSES THE QUEUE, THIS JOB COPIES AND EMPTIES THE      *
//*            DATASET,                                               *
//* THEN REOPENS THE QUEUE (MBKOPCMD). THE COPY GOES TO DPO CONSENT   *
//*            STORE (CONNECT:DIRECT NODE DPOCS01).                   *
//*-------------------------------------------------------------------*
//* 2012-09-03 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* SW005 - CLOSE THE QUEUE                                           *
//*-------------------------------------------------------------------*
//SW005    EXEC PGM=MBKOPCMD
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
F CICSPRD1,CEMT SET TDQUEUE(CPAU) CLOSED
/*
//*-------------------------------------------------------------------*
//* SW010 - COPY THE LOG                                              *
//*-------------------------------------------------------------------*
//SW010    EXEC PGM=IDCAMS,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//QIN      DD DISP=SHR,DSN=MBK.PRD.CICS.CPAU
//QOUT     DD DSN=MBK.PRD.CICS.CPAU.DAILY(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(QIN) OUTFILE(QOUT)
/*
//*-------------------------------------------------------------------*
//* SW012 - EMPTY THE LOG (SEQUENTIAL - NOT REPRO REUSE)              *
//*-------------------------------------------------------------------*
//SW012    EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY,DCB=(RECFM=FB,LRECL=400)
//SYSUT2   DD DISP=OLD,DSN=MBK.PRD.CICS.CPAU
//*-------------------------------------------------------------------*
//* SW020 - REOPEN THE QUEUE (ALWAYS)                                 *
//*-------------------------------------------------------------------*
//SW020    EXEC PGM=MBKOPCMD,COND=EVEN
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
F CICSPRD1,CEMT SET TDQUEUE(CPAU) OPEN
/*
