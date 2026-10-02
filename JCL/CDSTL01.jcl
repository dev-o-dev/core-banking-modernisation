//CDSTL01  JOB (MBK,BAT,CDS),'CARD SETTLEMENT',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CDSTL01                                                *
//* TITLE    : DEBIT CARD CLEARING / SETTLEMENT                       *
//* SCHEDULE : CA-7 MBKEOD - AFTER THE SCHEME FILE ARRIVES (EVENT     *
//*            CDCLR)                                                 *
//* OWNER    : CARDS IT        ON-CALL: ROTA CARDS                    *
//*-------------------------------------------------------------------*
//* CD010  SORT CLEARING FILE BY ACCOUNT                              *
//* CD020  POST CARD ITEMS (CDSTL010)                                 *
//*-------------------------------------------------------------------*
//* 2010-08-02 JNB  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* CD010 - SORT CLEARING FILE BY ACCOUNT                             *
//* HEADER FIRST, TRAILER LAST VIA A SEQUENCE BYTE IN COL 201         *
//*-------------------------------------------------------------------*
//CD010    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=MBK.PRD.CARD.CLEARING(0)
//SORTOUT  DD DSN=MBK.PRD.CARD.CLEARING.SORTED(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//SYSIN    DD *
  INREC IFTHEN=(WHEN=(1,1,CH,EQ,C'H'),OVERLAY=(201:C'0')),
        IFTHEN=(WHEN=(1,1,CH,EQ,C'D'),OVERLAY=(201:C'1')),
        IFTHEN=(WHEN=(1,1,CH,EQ,C'T'),OVERLAY=(201:C'2'))
  SORT FIELDS=(201,1,CH,A,2,6,CH,A,8,8,CH,A,45,12,CH,A)
  OUTREC BUILD=(1,200)
/*
//*-------------------------------------------------------------------*
//* CD020 - POST CARD ITEMS (CDSTL010)                                *
//*-------------------------------------------------------------------*
//CD020    EXEC MBKBATCH,PGM=CDSTL010,COND=(4,LT)
//STEP.CLEARING DD DISP=SHR,DSN=MBK.PRD.CARD.CLEARING.SORTED(+1)
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.DDHOLD   DD DISP=OLD,DSN=MBK.PRD.DDA.HOLDS
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CRDPOST  DD DSN=MBK.PRD.PST.FEED.CRDPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(30,10),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.EXCEPT   DD DSN=MBK.PRD.CARD.EXCEPTIONS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,CDS1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
