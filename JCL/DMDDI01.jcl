//DMDDI01  JOB (MBK,BAT,DMD),'DD MANDATES',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : DMDDI01                                                *
//* TITLE    : DIRECT DEBIT MANDATES - AUDDIS IN, ADDACS OUT          *
//* SCHEDULE : CA-7 MBKEOD - AFTER PYBACS01 RECEIVES THE AUDDIS FILE  *
//* OWNER    : PAYMENTS IT     ON-CALL: ROTA PAY                      *
//*-------------------------------------------------------------------*
//* DD010  APPLY AUDDIS INSTRUCTIONS (DMAUD010)                       *
//* DD020  ADDACS AND HOUSEKEEPING (DMADC010)                         *
//*-------------------------------------------------------------------*
//* 2001-04-02 DKP  ORIGINAL (AUDDIS/ADDACS GO-LIVE)                  *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* DD010 - APPLY AUDDIS INSTRUCTIONS (DMAUD010)                      *
//*-------------------------------------------------------------------*
//DD010    EXEC MBKBATCH,PGM=DMAUD010
//STEP.AUDDISIN DD DISP=SHR,DSN=MBK.PRD.BACS.AUDDIS.IN(0)
//STEP.AUDDISRS DD DSN=MBK.PRD.BACS.AUDDIS.REJECTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.DDMAND   DD DISP=OLD,DSN=MBK.PRD.DDM.MANDATE
//STEP.REPORT   DD SYSOUT=(R,,DMA1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* DD020 - ADDACS AND HOUSEKEEPING (DMADC010)                        *
//*-------------------------------------------------------------------*
//DD020    EXEC MBKBATCH,PGM=DMADC010,COND=(4,LT)
//STEP.DDMAND   DD DISP=OLD,DSN=MBK.PRD.DDM.MANDATE
//STEP.ADDACS   DD DSN=MBK.PRD.BACS.ADDACS.OUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,DMD1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
