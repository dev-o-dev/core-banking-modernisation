//SVSWP01  JOB (MBK,BAT,SAV),'SAVINGS SWEEP',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : SVSWP01                                                *
//* TITLE    : NIGHTLY SAVINGS SWEEP (CURRENT ACCOUNT <-> SAVINGS)    *
//* SCHEDULE : CA-7 MBKEOD - AFTER MBKONLDN, CQCLR01 (IF SCHED) AND   *
//*            CDSTL01 (IF SCHED), BEFORE PSEOD01                     *
//* OWNER    : CORE SYSTEMS - SAVINGS                                 *
//*-------------------------------------------------------------------*
//* SW004  MAKE SURE THE FEEDS READ BY SW005 EXIST (SAME AS PSEOD01   *
//*        PS004 - AN EMPTY FEED IS NOT AN ERROR)                     *
//* SW005  SORT THE BATCH DEBITS ALREADY QUEUED FOR TONIGHT INTO      *
//*        ACCOUNT ORDER (PENDING POSTINGS FOR THE PROJECTED BALANCE) *
//* SW010  SWEEP (SVSWP010)                                           *
//*-------------------------------------------------------------------*
//* SWPPOST IS A PSEOD01 FEED (DISP=MOD) - POSTED TONIGHT.            *
//* SWPINST IS CICS FILE SAVSWP - CLOSED BY MBKONLDN.                 *
//* RC 4 FROM SW010 = SWEEPS FAILED (REPORT SWP1). NETWORK CONTINUES, *
//* SAVINGS OPERATIONS WORK THE REPORT NEXT MORNING.                  *
//*-------------------------------------------------------------------*
//* RESTART: RERUN FROM SW004. SVSWP010 SKIPS INSTRUCTIONS ALREADY    *
//* SWEPT TODAY, AND PSEOD010 REJECTS DUPLICATE EXTERNAL REFERENCES.  *
//* AN ABEND IN SW010 AFTER SOME SWEEPS HAS REDUCED AVAILABLE         *
//* BALANCES - DO NOT RESTORE ACCTMAST, JUST RERUN.                   *
//*-------------------------------------------------------------------*
//* 2015-03-02 RKM  ORIGINAL                                          *
//* 2015-06-29 RKM  SW004/SW005 PENDING BATCH DEBITS (INC0061734)     *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* SW004 - ENSURE THE FEEDS EXIST (MOD CREATES AN EMPTY DATASET)     *
//*-------------------------------------------------------------------*
//SW004    EXEC PGM=IEFBR14
//SOPOST   DD DSN=MBK.PRD.PST.FEED.SOPOST,DISP=(MOD,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//DDPOST   DD DSN=MBK.PRD.PST.FEED.DDPOST,DISP=(MOD,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//CHQPOST  DD DSN=MBK.PRD.PST.FEED.CHQPOST,DISP=(MOD,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//CRDPOST  DD DSN=MBK.PRD.PST.FEED.CRDPOST,DISP=(MOD,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(30,10),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//LNREPAY  DD DSN=MBK.PRD.PST.FEED.LNREPAY,DISP=(MOD,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//ATMPOST  DD DSN=MBK.PRD.PST.FEED.ATMPOST,DISP=(MOD,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* SW005 - SORT QUEUED DEBITS INTO ACCOUNT ORDER. CREDITS ARE        *
//*         DROPPED - THEY ARE NOT COUNTED AS FUNDS (SEE SVSWP010).   *
//*-------------------------------------------------------------------*
//SW005    EXEC PGM=SORT,COND=(0,LT)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.PST.FEED.SOPOST
//         DD DISP=SHR,DSN=MBK.PRD.PST.FEED.DDPOST
//         DD DISP=SHR,DSN=MBK.PRD.PST.FEED.CHQPOST
//         DD DISP=SHR,DSN=MBK.PRD.PST.FEED.CRDPOST
//         DD DISP=SHR,DSN=MBK.PRD.PST.FEED.LNREPAY
//         DD DISP=SHR,DSN=MBK.PRD.PST.FEED.ATMPOST
//SORTOUT  DD DSN=&&PENDSRT,
//            DISP=(NEW,PASS,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//SYSIN    DD *
  INCLUDE COND=(41,1,CH,EQ,C'D')
  SORT FIELDS=(1,36,CH,A)
/*
//*-------------------------------------------------------------------*
//* SW010 - SWEEP (SVSWP010)                                          *
//*-------------------------------------------------------------------*
//SW010    EXEC MBKBATCH,PGM=SVSWP010,COND=(0,LT)
//STEP.SWPINST  DD DISP=OLD,DSN=MBK.PRD.SAV.SWEEP
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.PENDPOST DD DISP=(OLD,DELETE),DSN=&&PENDSRT
//STEP.SWPPOST  DD DSN=MBK.PRD.PST.FEED.SWPPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,SWP1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
