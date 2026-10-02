/*********************************************************************/
/* MEMBER   : STDEFADH                                               */
/* TITLE    : IDCAMS - DEFINE AD-HOC STATEMENT REQUEST KSDS          */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT STREQREC - RECORDSIZE(100 100) KEYS(31 0)           */
/*   KEY = SORT CODE + ACCOUNT + CIF + STATEMENT NUMBER              */
/* CICS FILE STMTADH (CSD MBKCOR3) - WRITTEN BY STREQ01 IN THE DAY,  */
/* UNLOADED BY STMT01 ST012 AND EMPTIED BY ST016 (REPRO ... REUSE),  */
/* SO THE CLUSTER MUST BE DEFINED REUSE.                             */
/*-------------------------------------------------------------------*/
/* 2012-06-11 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.STMT.ADHOC CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.STMT.ADHOC)                -
                  INDEXED                                 -
                  REUSE                                   -
                  KEYS(31 0)                              -
                  RECORDSIZE(100 100)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(5 5)                          -
                  VOLUMES(MBKV04 MBKV05)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.STMT.ADHOC.DATA)           -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.STMT.ADHOC.INDEX)          -
                  CONTROLINTERVALSIZE(2048))
