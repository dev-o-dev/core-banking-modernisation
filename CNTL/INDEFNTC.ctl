/*********************************************************************/
/* MEMBER   : INDEFNTC                                               */
/* TITLE    : IDCAMS - DEFINE NOTICE SAVINGS WITHDRAWAL NOTICES KSDS */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT NTCWDREC - RECORDSIZE(150 150) KEYS(22 0)           */
/*   KEY = SORT CODE + ACCOUNT + DATE NOTICE GIVEN                   */
/* CICS FILE SAVNTC (CSD MBKCOR2) - WRITTEN BY THE TELLER SYSTEM AND */
/* IBANK, PAID BY INNTC010. PAID / FAILED NOTICES ARE KEPT 2 YEARS   */
/* (COMPLAINTS EVIDENCE) - ANNUAL REORG.                             */
/*-------------------------------------------------------------------*/
/* 2011-07-04 AOS  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.SAV.NOTICES CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.SAV.NOTICES)               -
                  INDEXED                                 -
                  KEYS(22 0)                              -
                  RECORDSIZE(150 150)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(20 5)                         -
                  VOLUMES(MBKV04 MBKV05)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.SAV.NOTICES.DATA)          -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.SAV.NOTICES.INDEX)         -
                  CONTROLINTERVALSIZE(2048))
