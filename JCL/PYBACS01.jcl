//PYBACS01 JOB (MBK,EOD,PAY),'BACS SUBMISSION',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : PYBACS01                                               *
//* TITLE    : BUILD AND TRANSMIT BACS STANDARD 18 SUBMISSION         *
//* SCHEDULE : CA-7 20:30 BUSINESS DAYS (BACSTEL-IP WINDOW CLOSES     *
//*            22:30 FOR DAY-1 INPUT)                                 *
//* OWNER    : PAYMENTS IT                ON-CALL: ROTA PAY-IT        *
//*-------------------------------------------------------------------*
//* PB010  BUILD THE STANDARD 18 FILE (PYBAC010)                      *
//* PB020  SEND TO THE BACS BUREAU SERVER (PTX) - ONLY IF RC=0        *
//*        (RC 4 = NO PAYMENTS TODAY, NOTHING TO SEND)                *
//*        PTX SIGNS AND SUBMITS; THE SUBMISSION REPORT COMES BACK    *
//*        BY EMAIL TO PAYMENTS OPERATIONS.                           *
//*-------------------------------------------------------------------*
//* RESTART: RERUN FROM PB010 ONLY IF PB010 ABENDED (DB2 ROLLED BACK).*
//* IF PB020 FAILED, RERUN PB020 ONLY - DO NOT RERUN PB010 (THE       *
//* PAYMENTS ARE ALREADY AT 'SNT' AND WOULD NOT BE SELECTED AGAIN).   *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//PB010    EXEC MBKDB2B
//STEP.BACSOUT  DD DSN=MBK.PRD.PAY.BACS.OUTBOUND(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(PYBAC010) PLAN(PYBAC010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*
//PB020    EXEC PGM=DMBATCH,PARM=(YYSLYNN),
//             COND=((0,NE,PB010.STEP))
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDMGLOAD
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMMSGFIL DD DISP=SHR,DSN=CDZ.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON USERID=(MBKCDP) ESF=NO
  SUBMIT PROC=MBKCOPY CASE=YES MAXDELAY=UNLIMITED                   -
         &DSN=MBK.PRD.PAY.BACS.OUTBOUND(0)                         -
         &RNODE=MBK.WIN.PTXPRD                                     -
         &RDSN='D:\PTX\INBOUND\STD18\MBK412345.TXT'                -
         &SYSOPT=':DATATYPE=TEXT:XLATE=YES:'
  SIGNOFF
/*
