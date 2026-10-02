//CMPRD01  JOB (MBK,BAT,CMP),'COMPLAINTS REDRESS',
//             CLASS=B,MSGCLASS=X,REGION=0M
//*********************************************************************
//* JOB      : CMPRD01                                                *
//* TITLE    : COMPLAINTS - PAY APPROVED REDRESS (REFUND,             *
//*            CONSEQUENTIAL LOSS, 8% SIMPLE INTEREST)                *
//* SCHEDULE : CA-7 MBKEOD - DSN TRIGGER (EVENT CMSRDR)               *
//*            MBK.PRD.CMPL.REDRESS.APPROVED, AFTER MBKONLDN,         *
//*            BEFORE PSEOD01                                         *
//* OWNER    : CORE SYSTEMS - DDA    BUSINESS: CUSTOMER RELATIONS     *
//*-------------------------------------------------------------------*
//* RD010  PAY REDRESS (CMPRD010)                                     *
//*-------------------------------------------------------------------*
//* RDRPOST IS A PSEOD01 FEED (DISP=MOD) - POSTED TONIGHT.            *
//* GLJNL MUST BE PICKED UP BY GLFEED01 STEP GL020 AS                 *
//* MBK.PRD.GL.JOURNAL.CMPL(0).                                       *
//* RESPONSE GOES BACK TO CMS (CONNECT:DIRECT BY CMS ON 06:00 POLL).  *
//* LETTERS GOES TO THE PRINT VENDOR WITH THE STMT01 LETTER BATCH.    *
//* RC 4 FROM RD010 = ITEMS REJECTED - CMS SHOWS THEM TO THE          *
//* COMPLAINT HANDLER FROM THE RESPONSE FILE. NO OPS ACTION.          *
//* U1004 / U1006 / U1007 = BAD INPUT FILE. NOTHING WAS PAID. ASK     *
//* CMS SUPPORT TO RESEND, THEN RERUN.                                *
//*-------------------------------------------------------------------*
//* RESTART: RERUN FROM RD010 AFTER DELETING ANY (+1) GENERATIONS IT  *
//* CREATED. IF RD010 ABENDED AFTER WRITING TO RDRPOST, PSEOD010      *
//* REJECTS THE REPEATED CREDITS AS DUPLICATE EXTERNAL REFERENCES.    *
//*-------------------------------------------------------------------*
//* 2016-02-08 PDQ  ORIGINAL                                          *
//* 2016-11-07 HLT  LETTERS LRECL 300 -> 350 (TAX DEDUCTION FIELDS)   *
//*********************************************************************
//         JCLLIB ORDER=(MBK.PRD.PROCLIB)
//*
//*-------------------------------------------------------------------*
//* RD010 - PAY REDRESS (CMPRD010)                                    *
//*-------------------------------------------------------------------*
//RD010    EXEC MBKBATCH,PGM=CMPRD010
//STEP.REDRESS  DD DISP=SHR,DSN=MBK.PRD.CMPL.REDRESS.APPROVED(0)
//STEP.ACCTMAST DD DISP=SHR,DSN=MBK.PRD.DDA.ACCTMAST
//STEP.CIFMAST  DD DISP=SHR,DSN=MBK.PRD.CIF.MASTER
//STEP.PRDPARM  DD DISP=SHR,DSN=MBK.PRD.PRD.PARMS
//STEP.RDRPOST  DD DSN=MBK.PRD.PST.FEED.RDRPOST,
//            DISP=(MOD,CATLG,CATLG),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//STEP.GLJNL    DD DSN=MBK.PRD.GL.JOURNAL.CMPL(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//STEP.LETTERS  DD DSN=MBK.PRD.CMPL.REDRESS.LETTERS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=350,BLKSIZE=0)
//STEP.RESPONSE DD DSN=MBK.PRD.CMPL.REDRESS.RESPONSE(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//STEP.REPORT   DD SYSOUT=(R,,RDR1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
