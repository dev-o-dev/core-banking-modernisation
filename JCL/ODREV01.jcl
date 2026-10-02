//ODREV01  JOB (MBK,BAT,ODR),'OVERDRAFT REVIEW',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : ODREV01                                                *
//* TITLE    : ARRANGED OVERDRAFT EXPIRY, REVIEW AND REFERRALS        *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01 AND CGCAL01                *
//* OWNER    : CORE SYSTEMS - CURRENT ACCOUNTS                        *
//*-------------------------------------------------------------------*
//* OD010  EXPIRE / REVIEW LIMITS, REFERRALS (ODREV010)               *
//* OD020  SORT REFERRALS BY REASON AND HOME BRANCH FOR THE           *
//*        FINANCIAL SUPPORT UNIT WORKLIST                            *
//*-------------------------------------------------------------------*
//* OD010 UPDATES ACCTMAST. RC 4 = EXCEPTIONS ON THE REPORT (CIF OR   *
//* PRODUCT NOT FOUND, BAD EXPIRY DATE, LIMIT ABOVE PRODUCT MAXIMUM)  *
//* - THE NETWORK CONTINUES. U1005 = DATE CARD GAP CHECK FAILED - DO  *
//* NOT RESTART UNTIL THE DATE CARD HAS BEEN CHECKED (INC 16/0042).   *
//* RERUN: RESTORE ACCTMAST FROM TONIGHT'S PSEOD01 PS005 BACKUP       *
//* (MBK.PRD.DDA.ACCTMAST.BKUP(0)) AND REAPPLY PSEOD01 FIRST - LIMITS *
//* REMOVED BY OD010 ARE NOT REINSTATED BY A SIMPLE RERUN.            *
//*-------------------------------------------------------------------*
//* 2004-11-08 SAT  ORIGINAL                                          *
//* 2015-04-27 PDQ  REFERRAL FILE AND OD020 SORT                      *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* OD010 - EXPIRE / REVIEW LIMITS, REFERRALS (ODREV010)              *
//*-------------------------------------------------------------------*
//OD010    EXEC MBKBATCH,PGM=ODREV010
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.LETTERS  DD DSN=MBK.PRD.OD.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REFERRAL DD DSN=MBK.PRD.OD.REFERRAL(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,ODR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* OD020 - SORT REFERRALS: REASON (9,4), HOME BRANCH (75,6),         *
//*         ACCOUNT KEY (13,14)                                       *
//*-------------------------------------------------------------------*
//OD020    EXEC PGM=SORT,COND=(4,LT)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.OD.REFERRAL(+1)
//SORTOUT  DD DSN=MBK.PRD.OD.REFERRAL.FSU(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(9,4,CH,A,75,6,CH,A,13,14,CH,A)
/*
