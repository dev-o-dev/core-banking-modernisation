//MIEXT01  JOB (MBK,BAT,MIS),'MONTHLY DEPOSIT MI',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : MIEXT01                                                *
//* TITLE    : MONTHLY MANAGEMENT INFORMATION EXTRACT - DEPOSITS      *
//* SCHEDULE : CA-7 MBKMTH - FIRST BUSINESS DAY, AFTER PSEOD01        *
//* OWNER    : CORE SYSTEMS - MI / DATA WAREHOUSE INTERFACES          *
//*-------------------------------------------------------------------*
//* MI010  BUILD MI FILE AND SUMMARY (MIEXT010)                       *
//* MI020  COPY TO THE WAREHOUSE TRANSMISSION DATASET (MBKCOPY PICKS  *
//*        IT UP AT 06:00)                                            *
//*-------------------------------------------------------------------*
//* RERUN FOR AN EARLIER MONTH: REPLACE PARMIN DD DUMMY WITH          *
//*   //STEP.PARMIN DD *                                              *
//*   MONTH=CCYYMM                                                    *
//* AND TELL THE DWH TEAM - THE LOADER REPLACES THE WHOLE MONTH.      *
//* U1004 = DIMENSION TOTALS DISAGREE - CALL CORE SYSTEMS ON-CALL.    *
//*-------------------------------------------------------------------*
//* 1995-02-06 GHT  ORIGINAL (MI0400 PRINT)                           *
//* 2003-09-29 SAT  MI FILE AND MI020                                 *
//* 2014-11-03 PDQ  PARMIN FOR MONTH OVERRIDE                         *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* MI010 - BUILD MI FILE AND SUMMARY (MIEXT010)                      *
//*-------------------------------------------------------------------*
//MI010    EXEC MBKBATCH,PGM=MIEXT010
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.PARMIN   DD DUMMY,DCB=(RECFM=FB,LRECL=80)
//STEP.SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(50,10))
//STEP.SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(50,10))
//STEP.SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(50,10))
//STEP.MIFILE   DD DSN=MBK.PRD.MI.DEPOSITS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,MIS1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* MI020 - COPY TO THE WAREHOUSE TRANSMISSION DATASET                *
//*-------------------------------------------------------------------*
//MI020    EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DISP=SHR,DSN=MBK.PRD.MI.DEPOSITS(+1)
//SYSUT2   DD DSN=MBK.PRD.DWH.SEND.MIDEP,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
