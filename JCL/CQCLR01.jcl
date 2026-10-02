//CQCLR01  JOB (MBK,BAT,CQC),'CHEQUE CLEARING',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CQCLR01                                                *
//* TITLE    : CHEQUES - INWARD CLEARING AND DEPOSIT CLEARANCE        *
//* SCHEDULE : CA-7 MBKEOD - AFTER THE IPSL INWARD FILE ARRIVES (EVENT*
//*            CQIN)                                                  *
//* OWNER    : PAYMENTS IT - CHEQUES                                  *
//*-------------------------------------------------------------------*
//* CQ010  SORT INWARD ITEMS BY ACCOUNT / SERIAL                      *
//* CQ020  PAY / RETURN DECISIONS (CQCLR010)                          *
//* CQ040  DEPOSITED CHEQUE CLEARANCE (CQUNC010)                      *
//*-------------------------------------------------------------------*
//* THE PAY/NO-PAY DECISION FILE (RETURNS) MUST REACH IPSL BY         *
//* 13:00 ON DAY 2 - IF CQ020 FAILS, CALL PAYMENTS OPERATIONS.        *
//*-------------------------------------------------------------------*
//* 1990-06-11 RJH  ORIGINAL (DD0610)                                 *
//* 2007-11-19 JNB  CQ040 2-4-6 CLEARANCE                             *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* CQ010 - SORT INWARD ITEMS BY ACCOUNT / SERIAL                     *
//* HEADER (H) SORTS FIRST, TRAILER (T) LAST - DO NOT CHANGE          *
//*-------------------------------------------------------------------*
//CQ010    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.CHQ.INWARD(0)
//SORTOUT  DD DSN=MBK.PRD.CHQ.INWARD.SORTED(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,2,6,CH,A,8,8,CH,A,16,6,CH,A)
/*
//*-------------------------------------------------------------------*
//* CQ020 - PAY / RETURN DECISIONS (CQCLR010)                         *
//*-------------------------------------------------------------------*
//CQ020    EXEC MBKBATCH,PGM=CQCLR010,COND=(4,LT)
//STEP.INWARD   DD DISP=SHR,DSN=MBK.PRD.CHQ.INWARD.SORTED(+1)
//STEP.STOPS    DD DISP=OLD,DSN=MBK.PRD.CHQ.STOPS
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CHQPOST  DD DSN=MBK.PRD.PST.FEED.CHQPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.RETURNS  DD DSN=MBK.PRD.CHQ.RETURNS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.CHQ.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CQC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* CQ040 - DEPOSITED CHEQUE CLEARANCE (CQUNC010)                     *
//*-------------------------------------------------------------------*
//CQ040    EXEC MBKBATCH,PGM=CQUNC010,COND=(4,LT)
//STEP.RETIN    DD DISP=SHR,DSN=MBK.PRD.CHQ.RETURNED.IN(0)
//STEP.UNCLEAR  DD DISP=OLD,DSN=MBK.PRD.CHQ.UNCLEARED
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.UNCPOST  DD DSN=MBK.PRD.PST.FEED.UNCPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.CHQ.UNPAID.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CQU1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
