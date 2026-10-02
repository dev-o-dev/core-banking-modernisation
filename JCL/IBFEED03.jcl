//IBFEED03 JOB (MBK,EOD,IBK),'IBANK TXN FEED',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : IBFEED03                                               *
//* TITLE    : HOST TO INTERNET BANKING TRANSACTION SHADOW TABLE FEED *
//* SCHEDULE : CA-7 NETWORK MBKEOD - AFTER PSEOD01 (RUNS ALONGSIDE    *
//*            IBFEED02)                                              *
//*            MUST COMPLETE BY 04:30 - IBANK LOADER STARTS 04:45     *
//* OWNER    : CORE SYSTEMS - CHANNELS    ON-CALL: ROTA CS-CHN        *
//*-------------------------------------------------------------------*
//* IB010  BUILD THE FEED (IBFD0301)                                  *
//* IB020  TRANSMIT TO THE IBANK SERVER (CONNECT:DIRECT)              *
//*-------------------------------------------------------------------*
//* RC 4 FROM IB010 = CONTROL EXCEPTIONS (ORPHAN ROWS, BALANCE CHAIN  *
//* OR LEDGER BREAKS, UNKNOWN TXN CODES) - SEE REPORT IB31. THE FEED  *
//* IS STILL GOOD AND IS SENT. RAISE A TICKET FOR DDA OPERATIONS.     *
//* U1004 FROM IB010 = TXNHIST CHANGED WHILE THE FEED WAS BUILT -     *
//* CHECK PSEOD01 HAS ENDED, THEN RERUN.                              *
//*-------------------------------------------------------------------*
//* RESTART: RERUN FROM IB010. THE FEED IS REBUILT FROM TXNHIST AND   *
//* THE MASTER - SAFE TO RERUN. IF IB010 ALREADY CREATED A            *
//* GENERATION, DELETE IT FIRST (OPS RUNBOOK 14.2).                   *
//*-------------------------------------------------------------------*
//* 2014-05-12 SJW  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//IB010    EXEC MBKDB2B
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.IBFEED   DD DSN=MBK.PRD.IBANK.IBFEED03.OUTPUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(100,25),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,IB31),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(IBFD0301) PLAN(IBFD0301) LIB('MBK.PRD.LOADLIB')
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
         &DSN=MBK.PRD.IBANK.IBFEED03.OUTPUT(0)                     -
         &RNODE=MBK.UNIX.IBANK                                     -
         &RDSN='/data/ibank/inbound/IBFEED03.dat'                  -
         &SYSOPT=':DATATYPE=TEXT:XLATE=YES:STRIP.BLANKS=NO:'
  SIGNOFF
/*
