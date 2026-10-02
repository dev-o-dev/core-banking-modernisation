//SARXT01  JOB (MBK,BAT,DPO),'GDPR SUBJECT ACCESS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : SARXT01                                                *
//* TITLE    : GDPR - DATA SUBJECT ACCESS REQUEST EXTRACT             *
//* SCHEDULE : CA-7 MBKEOD - AFTER PSEOD01, ONLY WHEN THE DPO HAS     *
//*            LOGGED REQUESTS (EVENT SARREQ - MBKSAR1 SAVE)          *
//* OWNER    : CORE SYSTEMS - CIF      BUSINESS: DATA PROTECTION OFF. *
//*-------------------------------------------------------------------*
//* SR010  PRODUCE PACKS + CONTROL REPORT (SARXT010)                  *
//*-------------------------------------------------------------------*
//* SAROUT CONTAINS PERSONAL DATA - SYSOUT CLASS P (SECURE PRINT,     *
//* DPO OFFICE ONLY). DO NOT RE-ROUTE, DO NOT KEEP IN JES BEYOND 7    *
//* DAYS (RETENTION SCHEDULE RS-18-04).                               *
//* RC 4 = A REQUEST WAS REJECTED, TRUNCATED OR IS ALREADY OVERDUE -  *
//*        THE DPO MUST ACT TODAY (STATUTORY ONE-MONTH DEADLINE).     *
//* SR020 EMPTIES THE REQUEST DATASET AFTER RC 0 OR 4 - REJECTED      *
//* REQUESTS MUST BE CORRECTED AND RE-KEYED ON MBKSAR1.               *
//*-------------------------------------------------------------------*
//* 2018-04-16 LJO  ORIGINAL (GDPR GO-LIVE 25/05/2018)                *
//* 2018-09-03 PDQ  SOMAST / DDMAND ADDED                             *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* SR010 - PRODUCE PACKS + CONTROL REPORT (SARXT010)                 *
//*-------------------------------------------------------------------*
//SR010    EXEC MBKDB2B
//STEP.SARREQ   DD DISP=SHR,DSN=MBK.PRD.DPO.SARREQ
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ACCTMAS1 DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST.CIFPATH
//STEP.LOANMAST DD DISP=SHR,DSN=MBK.PRD.LOAN.MASTER
//STEP.LOANMAS1 DD DISP=SHR,DSN=MBK.PRD.LOAN.MASTER.CIFPATH
//STEP.SOMAST   DD DISP=SHR,DSN=MBK.PRD.SO.MASTER
//STEP.DDMAND   DD DISP=SHR,DSN=MBK.PRD.DDM.MANDATE
//STEP.SAROUT   DD SYSOUT=(P,,SAR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,SAR2),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(SARXT010) PLAN(SARXT010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*-------------------------------------------------------------------*
//* SR020 - EMPTY THE REQUEST DATASET (REQUESTS NOW IN THE PACKS)     *
//*-------------------------------------------------------------------*
//SR020    EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY,DCB=(RECFM=FB,LRECL=80)
//SYSUT2   DD DISP=OLD,DSN=MBK.PRD.DPO.SARREQ
