//PYSWFT01 JOB (MBK,DAY,PAY),'SWIFT MT103 SEND',
//             CLASS=A,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : PYSWFT01                                               *
//* TITLE    : BUILD AND TRANSMIT SWIFT MT103 FILE                    *
//* SCHEDULE : CA-7 10:00, 12:00, 14:00, 15:30 BUSINESS DAYS          *
//*            (15:30 IS THE LAST RUN FOR SAME-DAY VALUE IN EUR/USD)  *
//* OWNER    : PAYMENTS IT                ON-CALL: ROTA PAY-IT        *
//*-------------------------------------------------------------------*
//* PS010  BUILD MT103 FILE (PYSWF010). CIFMAST/ACCTMAST ARE READ     *
//*        WHILE CICS IS UP - DISP=SHR, READ INTEGRITY NOT REQUIRED.  *
//* PS020  SEND TO SWIFT ALLIANCE ACCESS FILE TRANSFER (ONLY RC=0)    *
//*-------------------------------------------------------------------*
//* RESTART: AS PYBACS01 - IF PS020 FAILS RERUN PS020 ONLY.           *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//PS010    EXEC MBKDB2B
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.SWIFTOUT DD DSN=MBK.PRD.PAY.SWIFT.OUTBOUND(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(TRK,(30,15),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//STEP.SYSTSIN  DD *
 DSN SYSTEM(DB2P)
 RUN PROGRAM(PYSWF010) PLAN(PYSWF010) LIB('MBK.PRD.LOADLIB')
 END
/*
//*
//PS020    EXEC PGM=DMBATCH,PARM=(YYSLYNN),
//             COND=((0,NE,PS010.STEP))
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDMGLOAD
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMMSGFIL DD DISP=SHR,DSN=CDZ.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON USERID=(MBKCDP) ESF=NO
  SUBMIT PROC=MBKCOPY CASE=YES MAXDELAY=UNLIMITED                   -
         &DSN=MBK.PRD.PAY.SWIFT.OUTBOUND(0)                        -
         &RNODE=MBK.AIX.SAAPRD                                     -
         &RDSN='/alliance/fta/in/MT103.rje'                        -
         &SYSOPT=':DATATYPE=TEXT:XLATE=YES:'
  SIGNOFF
/*
