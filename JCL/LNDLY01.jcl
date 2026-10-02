//LNDLY01  JOB (MBK,BAT,LND),'PERSONAL LOANS DAILY',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : LNDLY01                                                *
//* TITLE    : PERSONAL LOANS - DAILY CYCLE                           *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01                            *
//* OWNER    : LENDING IT      ON-CALL: ROTA LEND                     *
//*-------------------------------------------------------------------*
//* LN010  BOOK APPROVED LOANS (LNBKG010)                             *
//* LN020  ACCRUE INTEREST (LNINT010)                                 *
//* LN030  COLLECT REPAYMENTS (LNREP010)                              *
//* LN040  ARREARS / DEFAULT (LNARR010)                               *
//* LN045  SPLIT PLACEMENTS BY AGENCY                                 *
//* LN050  ANNUAL STATEMENTS (LNSTM010)                               *
//*-------------------------------------------------------------------*
//* LN010-LN030 POSTINGS (LNPOST) GO TO TOMORROW'S PSEOD01.           *
//* LN045 SPLITS THE COLLECTIONS PLACEMENTS BY AGENCY FOR MBKCOPY.    *
//*-------------------------------------------------------------------*
//* 2010-09-06 JNB  ORIGINAL                                          *
//* 2010-10-11 JNB  LN050 ANNUAL STATEMENTS                           *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* LN010 - BOOK APPROVED LOANS (LNBKG010)                            *
//*-------------------------------------------------------------------*
//LN010    EXEC MBKDB2B
//STEP.APPROVED DD DISP=SHR,DSN=MBK.PRD.LOAN.APPROVED(0)
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.LOANMAST DD DISP=OLD,DSN=MBK.PRD.LOAN.MASTER
//STEP.LNPOST   DD DSN=MBK.PRD.PST.FEED.LNDRAW,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.LOAN.LETTERS.WELC(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.REJECTS  DD DSN=MBK.PRD.LOAN.REJECTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,LNB1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(LNBKG010) PLAN(LNBKG010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* LN020 - ACCRUE INTEREST (LNINT010)                                *
//*-------------------------------------------------------------------*
//LN020    EXEC MBKBATCH,PGM=LNINT010,COND=(4,LT)
//STEP.LOANMAST DD DISP=OLD,DSN=MBK.PRD.LOAN.MASTER
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.LOAN(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,LNI1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* LN030 - COLLECT REPAYMENTS (LNREP010)                             *
//*-------------------------------------------------------------------*
//LN030    EXEC MBKDB2B,COND=(4,LT)
//STEP.LOANMAST DD DISP=OLD,DSN=MBK.PRD.LOAN.MASTER
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.LNPOST   DD DSN=MBK.PRD.PST.FEED.LNREPAY,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.LNRP(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.LOAN.LETTERS.REP(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,LNR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(LNREP010) PLAN(LNREP010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* LN040 - ARREARS / DEFAULT (LNARR010)                              *
//*-------------------------------------------------------------------*
//LN040    EXEC MBKDB2B,COND=(4,LT)
//STEP.LOANMAST DD DISP=OLD,DSN=MBK.PRD.LOAN.MASTER
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.LETTERS  DD DSN=MBK.PRD.LOAN.LETTERS.ARR(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.COLLECT  DD DSN=MBK.PRD.LOAN.COLLECT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.LNAR(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,LNA1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(LNARR010) PLAN(LNARR010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* LN045 - SPLIT PLACEMENTS BY AGENCY                                *
//*-------------------------------------------------------------------*
//LN045    EXEC PGM=SORT,COND=(4,LT)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.LOAN.COLLECT(+1)
//DCA1     DD DSN=MBK.PRD.LOAN.COLLECT.DCA1(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//DCA2     DD DSN=MBK.PRD.LOAN.COLLECT.DCA2(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//DCA3     DD DSN=MBK.PRD.LOAN.COLLECT.DCA3(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(13,10,CH,A)
  OUTFIL FNAMES=DCA1,INCLUDE=(1,4,CH,EQ,C'DCA1')
  OUTFIL FNAMES=DCA2,INCLUDE=(1,4,CH,EQ,C'DCA2')
  OUTFIL FNAMES=DCA3,INCLUDE=(1,4,CH,EQ,C'DCA3')
/*
//*-------------------------------------------------------------------*
//* LN050 - ANNUAL STATEMENTS (LNSTM010)                              *
//*-------------------------------------------------------------------*
//LN050    EXEC MBKDB2B,COND=(4,LT)
//STEP.LOANMAST DD DISP=OLD,DSN=MBK.PRD.LOAN.MASTER
//STEP.LETTERS  DD DSN=MBK.PRD.LOAN.LETTERS.ANST(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,LNS1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(LNSTM010) PLAN(LNSTM010) LIB('MBK.PRD.LOADLIB')
 END
/*
