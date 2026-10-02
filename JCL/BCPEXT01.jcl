//BCPEXT01 JOB (MBK,BAT,BCP),'BCP ACCOUNT EXTRACT',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : BCPEXT01                                               *
//* TITLE    : BUSINESS CONTINUITY - NIGHTLY ACCOUNT EXTRACT          *
//* SCHEDULE : CA-7 MBKEOD - LAST JOB BEFORE MBKDTROL (DATE ROLL)     *
//* OWNER    : CORE SYSTEMS / IT SERVICE CONTINUITY                   *
//*-------------------------------------------------------------------*
//* BC010  EXTRACT (BCPEXT10)                                         *
//* BC020  SPLIT BY REGION FOR THE BRANCH CONTINGENCY PCS (BCPVIEW)   *
//* BC030  COPY THE FULL FILE TO THE DR TRANSMISSION DATASET          *
//*-------------------------------------------------------------------*
//* THIS JOB IS BCP CRITICAL. IF BC010 FAILS IT MUST BE FIXED AND     *
//* RERUN BEFORE 06:30 OR THE BRANCHES OPEN WITH YESTERDAY'S FILE -   *
//* INFORM THE IT SERVICE CONTINUITY DUTY MANAGER.                    *
//* RC 4 = EXCEPTIONS (CUSTOMER NOT ON CIF / BAD ACCOUNT NUMBER) -    *
//* CONTINUE. U1004 = TOO FEW ACCOUNTS, CHECK THE ACCTMAST RESTORE.   *
//* BC020 REGIONS (PIR 2013/120 - BCPVIEW 32,000 ACCOUNT LIMIT):      *
//*   LONDON1 SORT CODES 400000-404999   LONDON2 405000-409999        *
//*   NORTH   300000-399999              SOUTH   ALL OTHERS           *
//* EACH REGIONAL FILE HAS ITS OWN COUNT TRAILER (DFSORT TRAILER1).   *
//* THE CONNECT:DIRECT JOBS MBKBCPCD/MBKBCPBR SEND THE FILES.         *
//*-------------------------------------------------------------------*
//* 1998-06-15 MLW  ORIGINAL                                          *
//* 2009-03-02 PDQ  BC020 REGIONAL SPLIT                              *
//* 2013-06-03 KLM  LONDON SPLIT INTO TWO FILES                       *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* BC010 - EXTRACT (BCPEXT10)                                        *
//*-------------------------------------------------------------------*
//BC010    EXEC MBKBATCH,PGM=BCPEXT10
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.BCPFILE  DD DSN=MBK.PRD.BCP.ACCTEXT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(60,10),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,BCP1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* BC020 - SPLIT BY REGION (DETAIL RECORDS ONLY, SORT CODE IN 2,6)   *
//*-------------------------------------------------------------------*
//BC020    EXEC PGM=SORT,COND=(4,LT)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.BCP.ACCTEXT(+1)
//LONDON1  DD DSN=MBK.PRD.BCP.REGION.LONDON1(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//LONDON2  DD DSN=MBK.PRD.BCP.REGION.LONDON2(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//NORTH    DD DSN=MBK.PRD.BCP.REGION.NORTH(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//SOUTH    DD DSN=MBK.PRD.BCP.REGION.SOUTH(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(2,14,CH,A)
  INCLUDE COND=(1,1,CH,EQ,C'D')
  OUTFIL FNAMES=LONDON1,INCLUDE=(2,6,CH,GE,C'400000',AND,
         2,6,CH,LE,C'404999'),
         TRAILER1=(C'T',COUNT=(M11,LENGTH=9))
  OUTFIL FNAMES=LONDON2,INCLUDE=(2,6,CH,GE,C'405000',AND,
         2,6,CH,LE,C'409999'),
         TRAILER1=(C'T',COUNT=(M11,LENGTH=9))
  OUTFIL FNAMES=NORTH,INCLUDE=(2,6,CH,GE,C'300000',AND,
         2,6,CH,LE,C'399999'),
         TRAILER1=(C'T',COUNT=(M11,LENGTH=9))
  OUTFIL FNAMES=SOUTH,SAVE,
         TRAILER1=(C'T',COUNT=(M11,LENGTH=9))
/*
//*-------------------------------------------------------------------*
//* BC030 - FULL FILE (WITH HASH TRAILER) TO THE DR SITE              *
//*-------------------------------------------------------------------*
//BC030    EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DISP=SHR,DSN=MBK.PRD.BCP.ACCTEXT(+1)
//SYSUT2   DD DSN=MBK.PRD.BCP.DRSEND,
//            DISP=(OLD,KEEP,KEEP)
