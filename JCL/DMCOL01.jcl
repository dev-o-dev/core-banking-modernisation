//DMCOL01  JOB (MBK,BAT,DMC),'DD COLLECTIONS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : DMCOL01                                                *
//* TITLE    : DIRECT DEBIT COLLECTIONS AND UNPAIDS (ARUDD)           *
//* SCHEDULE : CA-7 MBKEOD - AFTER PYBACS01 (BACS INPUT 2) AND PSEOD01*
//* OWNER    : PAYMENTS IT     ON-CALL: ROTA PAY                      *
//*-------------------------------------------------------------------*
//* DM010  COLLECT (DMCOL010)                                         *
//* DM020  ARUDD FILE (DMARU010)                                      *
//*-------------------------------------------------------------------*
//* 2001-04-02 DKP  ORIGINAL                                          *
//* 2010-05-17 JNB  ARUDD STEP SPLIT OUT                              *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* DM010 - COLLECT (DMCOL010)                                        *
//*-------------------------------------------------------------------*
//DM010    EXEC MBKBATCH,PGM=DMCOL010
//STEP.COLLIN   DD DISP=SHR,DSN=MBK.PRD.BACS.DDI.IN(0)
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.DDMAND   DD DISP=OLD,DSN=MBK.PRD.DDM.MANDATE
//STEP.DDPOST   DD DSN=MBK.PRD.PST.FEED.DDPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.RETURNS  DD DSN=MBK.PRD.DDM.RETURNS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.DDM.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,DMC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* DM020 - ARUDD FILE (DMARU010)                                     *
//*-------------------------------------------------------------------*
//DM020    EXEC MBKBATCH,PGM=DMARU010,COND=(4,LT)
//STEP.RETURNS  DD DISP=SHR,DSN=MBK.PRD.DDM.RETURNS(+1)
//STEP.ARUDD    DD DSN=MBK.PRD.BACS.ARUDD.OUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,DMR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
