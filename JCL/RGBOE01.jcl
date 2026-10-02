//RGBOE01  JOB (MBK,BAT,REG),'BOE QUARTERLY RETURN',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : RGBOE01                                                *
//* TITLE    : BANK OF ENGLAND QUARTERLY STATISTICAL RETURN           *
//* SCHEDULE : CA-7 MBKQTR - FIRST BUSINESS DAY AFTER QUARTER END,    *
//*            BEFORE PSEOD01 (BALANCES MUST BE AS AT QUARTER END)    *
//* OWNER    : CORE SYSTEMS - REGULATORY REPORTING                    *
//*-------------------------------------------------------------------*
//* RG010  BUILD FORM LINES (RGBOE010)                                *
//*-------------------------------------------------------------------*
//* RC 4 = EXCEPTIONS OR LATE RUN (PIR 2009/054) - REGULATORY         *
//* REPORTING MUST REVIEW THE REPORT BEFORE SUBMISSION. THE RETURN    *
//* IS DUE TO THE BANK OF ENGLAND ON WORKING DAY 15 OF THE QUARTER.   *
//* RERUN FOR AN EARLIER QUARTER: REPLACE PARMIN DD DUMMY WITH        *
//*   //STEP.PARMIN DD *                                              *
//*   QTREND=CCYYMMDD                                                 *
//*-------------------------------------------------------------------*
//* 1993-04-05 GHT  ORIGINAL                                          *
//* 2006-07-03 SAT  BOERTN FILE                                       *
//* 2011-01-17 JNB  LOANMAST INPUT                                    *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* RG010 - BUILD FORM LINES (RGBOE010)                               *
//*-------------------------------------------------------------------*
//RG010    EXEC MBKBATCH,PGM=RGBOE010
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.LOANMAST DD DISP=SHR,DSN=MBK.PRD.LOAN.MASTER
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.PARMIN   DD DUMMY,DCB=(RECFM=FB,LRECL=80)
//STEP.BOERTN   DD DSN=MBK.PRD.REG.BOERTN(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,BOE1),COPIES=2,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
