//MBKDTROL JOB (MBK,EOD,OPS),'BUSINESS DATE ROLL',
//             CLASS=A,MSGCLASS=X
//*********************************************************************
//* JOB      : MBKDTROL                                               *
//* TITLE    : ROLL THE BUSINESS DATE CARD                            *
//* SCHEDULE : CA-7 NETWORK MBKEOD - AFTER ALL EOD JOBS THAT READ THE *
//*            DATE CARD, BEFORE MBKONLUP                             *
//*-------------------------------------------------------------------*
//* DT005  BACKUP THE CURRENT CARD (FOR A RERUN OF TODAY'S NETWORK)   *
//* DT010  ROLL THE DATE (REXX MBKDTRX)                               *
//*-------------------------------------------------------------------*
//* RERUNNING THIS JOB ROLLS THE DATE AGAIN. TO UNDO, COPY            *
//* MBK.PRD.CNTL.BUSDATE.BKUP(0) BACK INTO MBK.PRD.CNTL(BUSDATE).     *
//*********************************************************************
//DT005    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DISP=SHR,DSN=MBK.PRD.CNTL(BUSDATE)
//SYSUT2   DD DSN=MBK.PRD.CNTL.BUSDATE.BKUP(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(TRK,(1,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//*
//DT010    EXEC PGM=IKJEFT01,COND=(0,LT)
//SYSEXEC  DD DISP=SHR,DSN=MBK.PRD.REXX
//DATECARD DD DISP=OLD,DSN=MBK.PRD.CNTL(BUSDATE)
//BANKHOL  DD DISP=SHR,DSN=MBK.PRD.CNTL(BANKHOL)
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 %MBKDTRX
/*
