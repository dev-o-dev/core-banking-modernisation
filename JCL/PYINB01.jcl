//PYINB01  JOB (MBK,EOD,PAY),'INBOUND CREDITS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : PYINB01                                                *
//* TITLE    : APPLY INBOUND FPS / BACS / CHAPS CREDITS               *
//* SCHEDULE : CA-7 NETWORK MBKEOD - AFTER FPS GATEWAY END OF DAY     *
//*            (MBK.PRD.PAY.INBOUND.FPS CREATED) AND BEFORE PSEOD01   *
//* OWNER    : PAYMENTS IT                ON-CALL: ROTA PAY-IT        *
//*-------------------------------------------------------------------*
//* PI010  CREATE HARD POSTINGS + RETURNS (PYINB010)                  *
//* PI020  SEND RETURNS TO THE PAYMENTS GATEWAY (ONLY IF PI010 RC=4)  *
//*-------------------------------------------------------------------*
//* RESTART: RERUN FROM PI010. PYINB010 ABENDS IF A FILE FOR ANOTHER  *
//* BUSINESS DATE IS CONCATENATED.                                    *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//PI010    EXEC MBKBATCH,PGM=PYINB010
//STEP.INBOUND  DD DISP=SHR,DSN=MBK.PRD.PAY.INBOUND.FPS(0)
//              DD DISP=SHR,DSN=MBK.PRD.PAY.INBOUND.BACS(0)
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.POSTOUT  DD DSN=MBK.PRD.PAY.INBOUND.POSTINGS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.RETURNS  DD DSN=MBK.PRD.PAY.RETURNS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//*
//PI020    EXEC PGM=DMBATCH,PARM=(YYSLYNN),
//             COND=((4,NE,PI010.STEP))
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDMGLOAD
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMMSGFIL DD DISP=SHR,DSN=CDZ.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON USERID=(MBKCDP) ESF=NO
  SUBMIT PROC=MBKCOPY CASE=YES MAXDELAY=UNLIMITED                   -
         &DSN=MBK.PRD.PAY.RETURNS(0)                               -
         &RNODE=MBK.PAYGW.PRD                                      -
         &RDSN='/gateway/returns/in/MBKRTN.dat'                    -
         &SYSOPT=':DATATYPE=TEXT:XLATE=YES:'
  SIGNOFF
/*
