//ISAYRE01 JOB (MBK,BAT,ISA),'ISA YEAR END',
//             CLASS=C,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : ISAYRE01                                               *
//* TITLE    : CASH ISA YEAR END - HMRC RETURN AND RESET              *
//* SCHEDULE : ON REQUEST - FIRST BUSINESS DAY AFTER 5 APRIL, BEFORE  *
//*            PSEOD01                                                *
//* OWNER    : CORE SYSTEMS - SAVINGS                                 *
//*-------------------------------------------------------------------*
//* IY005  BACKUP ACCOUNT MASTER                                      *
//* IY010  YEAR END (ISASUB10)                                        *
//*-------------------------------------------------------------------*
//* EDIT TAXYEAR BEFORE SUBMITTING. ISASUB10 ABENDS U1005 IF THE      *
//* BUSINESS DATE IS NOT IN APRIL OF THE SECOND YEAR.                 *
//*-------------------------------------------------------------------*
//* 2010-04-06 AOS  ORIGINAL                                          *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* IY005 - BACKUP ACCOUNT MASTER                                     *
//*-------------------------------------------------------------------*
//IY005    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ACCTIN   DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//ACCTBKP  DD DSN=MBK.PRD.DDA.ACCTMAST.ISABKUP(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(600,60),RLSE),
//            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(ACCTIN) OUTFILE(ACCTBKP)
/*
//*-------------------------------------------------------------------*
//* IY010 - YEAR END (ISASUB10)                                       *
//*-------------------------------------------------------------------*
//IY010    EXEC MBKDB2B,COND=(4,LT)
//STEP.PARMIN   DD *
MODE=YEAREND
TAXYEAR=2010/2011
/*
//STEP.ACCTMAST DD DISP=OLD,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.BREACH   DD DUMMY
//STEP.ISARTN   DD DSN=MBK.PRD.ISA.HMRC.RETURN(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,ISA2),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(ISASUB10) PLAN(ISASUB10) LIB('MBK.PRD.LOADLIB')
 END
/*
