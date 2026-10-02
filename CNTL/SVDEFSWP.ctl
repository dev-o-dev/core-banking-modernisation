/*********************************************************************/
/* MEMBER   : SVDEFSWP                                               */
/* TITLE    : IDCAMS - DEFINE SAVINGS SWEEP INSTRUCTIONS KSDS        */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT SVSWPREC - RECORDSIZE(150 150) KEYS(14 0)           */
/*   KEY = CURRENT ACCOUNT SORT CODE + ACCOUNT NUMBER                */
/* CICS FILE SAVSWP - WRITTEN BY THE TELLER SYSTEM AND IBANK, READ   */
/* AND UPDATED NIGHTLY BY SVSWP010 (JOB SVSWP01). CANCELLED          */
/* INSTRUCTIONS ARE KEPT 2 YEARS (COMPLAINTS EVIDENCE) - ANNUAL      */
/* REORG REMOVES OLDER ONES.                                         */
/*-------------------------------------------------------------------*/
/* 2015-03-02 RKM  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.SAV.SWEEP CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.SAV.SWEEP)                 -
                  INDEXED                                 -
                  KEYS(14 0)                              -
                  RECORDSIZE(150 150)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(10 5)                         -
                  VOLUMES(MBKV04 MBKV05)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.SAV.SWEEP.DATA)            -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.SAV.SWEEP.INDEX)           -
                  CONTROLINTERVALSIZE(2048))
