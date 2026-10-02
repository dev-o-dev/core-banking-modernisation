//DDALR01  JOB (MBK,EOD,DDA),'CUSTOMER ALERTS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : DDALR01                                                *
//* TITLE    : CURRENT ACCOUNT ALERTS - UNARRANGED OVERDRAFT, CHARGE  *
//*            WARNING AND LOW BALANCE (SMS / EMAIL)                  *
//* SCHEDULE : CA-7 NETWORK MBKEOD - JOB TRIGGER PSEOD01              *
//*            MUST COMPLETE BY 04:30 - THE ALERT GATEWAY RELEASES    *
//*            MESSAGES FROM 07:00 (NO SMS OVERNIGHT - FCA GUIDANCE)  *
//* OWNER    : CORE SYSTEMS - DDA         ON-CALL: ROTA CS-DDA        *
//*-------------------------------------------------------------------*
//* AL010  BUILD THE ALERT FILE AND UPDATE PREFERENCES (DDALR010)     *
//* AL020  TRANSMIT TO THE ALERT GATEWAY (CONNECT:DIRECT)             *
//*-------------------------------------------------------------------*
//* ALRTPREF IS CICS FILE ALRTPRF (IBANK 'MANAGE ALERTS') - IT IS     *
//* CLOSED BY MBKONLDN AND REOPENED BY MBKONLUP.                      *
//* RC 4 FROM AL010 = ALERTS THAT COULD NOT BE SENT (NO MOBILE OR     *
//* EMAIL). THE REPORT (ALR1) GOES TO THE CONTACT CENTRE, WHO MUST    *
//* TRY TO PHONE THE CUSTOMER THE SAME DAY. AL020 STILL RUNS.         *
//*-------------------------------------------------------------------*
//* RESTART: RERUN FROM AL010. ALERTS ALREADY RECORDED AS SENT FOR    *
//* THE BUSINESS DATE ARE NOT SENT AGAIN. IF AL010 CREATED A          *
//* GENERATION THAT WAS NOT TRANSMITTED, DELETE IT FIRST.             *
//*-------------------------------------------------------------------*
//* 2016-09-05 HLT  ORIGINAL (UNARRANGED OD ALERTS PILOT)             *
//* 2017-05-15 HLT  CHARGE WARNINGS, GATEWAY BY CONNECT:DIRECT        *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* AL010 - BUILD ALERTS (DDALR010)                                   *
//*-------------------------------------------------------------------*
//AL010    EXEC MBKBATCH,PGM=DDALR010
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.ALRTPREF DD DISP=OLD,DSN=MBK.PRD.DDA.ALERTPRF
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.ALERTOUT DD DSN=MBK.PRD.DDA.ALERTS.OUTPUT(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,ALR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*-------------------------------------------------------------------*
//* AL020 - TRANSMIT. THE GENERATION CREATED BY AL010 IS CATALOGUED   *
//* AT STEP END, SO CONNECT:DIRECT REFERS TO IT AS (0).               *
//*-------------------------------------------------------------------*
//AL020    EXEC PGM=DMBATCH,PARM=(YYSLYNN),COND=(4,LT)
//STEPLIB  DD DISP=SHR,DSN=CDZ.SDMGLOAD
//DMNETMAP DD DISP=SHR,DSN=CDZ.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=MBK.PRD.CDPROC
//DMMSGFIL DD DISP=SHR,DSN=CDZ.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON USERID=(MBKCDP) ESF=NO
  SUBMIT PROC=MBKCOPY CASE=YES MAXDELAY=UNLIMITED                   -
         &DSN=MBK.PRD.DDA.ALERTS.OUTPUT(0)                         -
         &RNODE=MBK.UNIX.ALERTGW                                   -
         &RDSN='/data/alertgw/inbound/DDALERTS.dat'                -
         &SYSOPT=':DATATYPE=TEXT:XLATE=YES:STRIP.BLANKS=NO:'
  SIGNOFF
/*
