//GLFEED01 JOB (MBK,BAT,GLF),'GENERAL LEDGER FEED',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : GLFEED01                                               *
//* TITLE    : GENERAL LEDGER FEED TO ORACLE GL                       *
//* SCHEDULE : CA-7 MBKEOD - LAST JOB BEFORE MBKONLUP                 *
//* OWNER    : CORE SYSTEMS - FINANCE INTERFACES                      *
//*-------------------------------------------------------------------*
//* GL010  CUSTOMER MOVEMENTS (GLEXT010)                              *
//* GL020  MERGE JOURNALS                                             *
//* GL030  VALIDATE / RECONCILE (GLREC010)                            *
//* GL040  SEND FEED (CONNECT:DIRECT)                                 *
//*-------------------------------------------------------------------*
//* GL020 CONCATENATES TODAY'S JOURNALS FROM EVERY SOURCE. A SOURCE   *
//* THAT DID NOT RUN TODAY HAS NO NEW GENERATION - GL020 USES (0),    *
//* SO GLREC010 REJECTS A STALE JOURNAL BY ITS BUSINESS DATE.         *
//* RC 8 FROM GL030 = FEED NOT SENT. CALL FINANCE SYSTEMS.            *
//*-------------------------------------------------------------------*
//* 2002-06-03 KLM  ORIGINAL (ORACLE GL)                              *
//* 2010-09-06 JNB  LOAN JOURNALS                                     *
//* 2017-03-06 PDQ  REDRESS (CMPL) AND LINK ATM (ATMS) JOURNALS       *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* GL010 - CUSTOMER MOVEMENTS (GLEXT010)                             *
//*-------------------------------------------------------------------*
//GL010    EXEC MBKDB2B
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.MOVT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,GLE1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(GLEXT010) PLAN(GLEXT010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* GL020 - MERGE JOURNALS                                            *
//*-------------------------------------------------------------------*
//GL020    EXEC PGM=SORT,COND=(4,LT)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.MOVT(+1)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.ACCR(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.ICAP(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.CHRG(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.CHRF(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.LOAN(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.CLOS(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.SUSP(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.LNRP(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.LNAR(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.CMPL(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.ATMS(0)
//         DD DISP=SHR,DSN=MBK.PRD.GL.JOURNAL.BGCS(0)
//SORTOUT  DD DSN=MBK.PRD.GL.FEED.MERGED(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(30,10),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=COPY
/*
//*-------------------------------------------------------------------*
//* GL030 - VALIDATE / RECONCILE (GLREC010)                           *
//*-------------------------------------------------------------------*
//GL030    EXEC MBKDB2B,COND=(4,LT)
//STEP.GLFEED   DD DISP=SHR,DSN=MBK.PRD.GL.FEED.MERGED(+1)
//STEP.FEEDOUT  DD DSN=MBK.PRD.GL.FEED(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(30,10),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.GLBALIN  DD DISP=SHR,DSN=MBK.PRD.GL.BALANCES(0)
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.REPORT   DD SYSOUT=(R,,GLR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(GLREC010) PLAN(GLREC010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* GL040 - SEND FEED (CONNECT:DIRECT)                                *
//*-------------------------------------------------------------------*
//GL040    EXEC PGM=DMBATCH,COND=(4,LT)
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDGALINK
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMPRINT  DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SIGNON
 SUBMIT PROC=MBKCOPY -
   &FROM=MBK.PRD.GL.FEED(0) -
   &TO=/oragl/inbound/mbk_feed.dat -
   &NODE=ORAGLP01
 SIGNOFF
/*
