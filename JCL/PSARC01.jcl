//PSARC01  JOB (MBK,BAT,PST),'TXNHIST ARCHIVE',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : PSARC01                                                *
//* TITLE    : TRANSACTION HISTORY ARCHIVE AND PURGE                  *
//* SCHEDULE : CA-7 MBKMTH - FIRST SUNDAY AFTER MONTH END, 02:00,     *
//*            MUTUALLY EXCLUSIVE WITH PSEOD01 AND IBFEED02           *
//* OWNER    : CORE SYSTEMS - POSTING / DBA TEAM                      *
//*-------------------------------------------------------------------*
//* PA010  ARCHIVE AND PURGE MBK.TXNHIST (PSARC010)                   *
//*-------------------------------------------------------------------*
//* RESTART: RESUBMIT THE JOB UNCHANGED - PSARC010 RESTARTS FROM THE  *
//* CHECKPOINT (MBK.PRD.TXNHIST.CHKPT). THE ARCHIVE OF THE FAILED RUN *
//* IS KEPT (ABNORMAL DISP CATLG) - SEE PIR 2012/031 FOR DUPLICATES.  *
//* TO START A NEW PASS AND IGNORE THE CHECKPOINT ADD 'RESTART =N'.   *
//* RC 4 = MAXROWS REACHED (CONTINUES NEXT MONTH) OR ROWS ALREADY     *
//* DELETED. AFTER A SUCCESSFUL RUN THE DBA REORGS TSTXNH (JOB        *
//* MBKREOTX) - PIR 2014/077.                                         *
//* RETMNTHS BELOW 084 IS REJECTED (U1005) - 7 YEAR RETENTION POLICY. *
//*-------------------------------------------------------------------*
//* 2012-02-13 PDQ  ORIGINAL                                          *
//* 2014-09-01 KLM  CHKPT DD, ARCHIVE ABNORMAL DISP CATLG             *
//* 2015-03-09 KLM  MAXROWS CARD                                      *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* PA010 - ARCHIVE AND PURGE MBK.TXNHIST (PSARC010)                  *
//*-------------------------------------------------------------------*
//PA010    EXEC MBKDB2B
//STEP.PARMIN   DD *
* PSARC010 CONTROL CARDS - SEE PROGRAM HEADER
RETMNTHS=084
COMMITFQ=01000
MAXROWS =005000000
MODE    =PURGE
/*
//STEP.CHKPT    DD DISP=OLD,DSN=MBK.PRD.TXNHIST.CHKPT
//STEP.ARCHIVE  DD DSN=MBK.PRD.TXNHIST.ARCHIVE(+1),
//            DISP=(NEW,CATLG,CATLG),UNIT=CART,
//            LABEL=(1,SL,RETPD=3653),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,PSA1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(PSARC010) PLAN(PSARC010) LIB('MBK.PRD.LOADLIB')
 END
/*
