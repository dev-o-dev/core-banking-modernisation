//IBFEED02 JOB (MBK,EOD,IBK),'IBANK DAILY FEED',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : IBFEED02                                               *
//* TITLE    : HOST TO INTERNET BANKING DAILY BALANCE/TXN FEED        *
//* SCHEDULE : CA-7 NETWORK MBKEOD - AFTER PSEOD01                    *
//*            MUST COMPLETE BY 04:30 - IBANK LOADER STARTS 04:45     *
//* OWNER    : CORE SYSTEMS - CHANNELS    ON-CALL: ROTA CS-CHN        *
//*-------------------------------------------------------------------*
//* IB010  BUILD THE FEED (IBFD0201)                                  *
//* IB020  TRANSMIT TO THE IBANK SERVER (CONNECT:DIRECT)              *
//*-------------------------------------------------------------------*
//* RESTART: RERUN FROM IB010. THE FEED IS REBUILT FROM THE MASTER    *
//* AND TXNHIST - IT IS SAFE TO RERUN AS OFTEN AS NEEDED. IF IB010    *
//* ALREADY CREATED A GENERATION, DELETE IT FIRST (OPS RUNBOOK 14.2). *
//*-------------------------------------------------------------------*
//* 2000-06-19 DKP  ORIGINAL (IBFEED01)                               *
//* 2001-11-05 DKP  IBFEED02                                          *
//* 2004-10-11 SAT  FTP REPLACED BY CONNECT:DIRECT (IA-04-07)         *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//IB010    EXEC MBKDB2B
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.IBFEED   DD DSN=MBK.PRD.IBANK.IBFEED02.OUTPUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(80,20),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(IBFD0201) PLAN(IBFD0201) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* IB020 - TRANSMIT. THE GENERATION CREATED BY IB010 IS CATALOGUED   *
//* AT STEP END, SO CONNECT:DIRECT (WHICH ALLOCATES IN ITS OWN        *
//* ADDRESS SPACE) REFERS TO IT AS (0).                               *
//*-------------------------------------------------------------------*
//IB020    EXEC PGM=DMBATCH,PARM=(YYSLYNN),COND=(4,LT)
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDMGLOAD
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMMSGFIL DD DISP=SHR,DSN=CDZ.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON USERID=(MBKCDP) ESF=NO
  SUBMIT PROC=MBKCOPY CASE=YES MAXDELAY=UNLIMITED                   -
         &DSN=MBK.PRD.IBANK.IBFEED02.OUTPUT(0)                     -
         &RNODE=MBK.UNIX.IBANK                                     -
         &RDSN='/data/ibank/inbound/IBFEED02.dat'                  -
         &SYSOPT=':DATATYPE=TEXT:XLATE=YES:STRIP.BLANKS=NO:'
  SIGNOFF
/*
