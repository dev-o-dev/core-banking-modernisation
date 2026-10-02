/*********************************************************************/
/* MEMBER   : CGDEFPND                                               */
/* TITLE    : IDCAMS - DEFINE PENDING CHARGES KSDS                   */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT CGCHGREC - RECORDSIZE(250 250) KEYS(32 0)           */
/*   KEY = SORT CODE + ACCOUNT + PERIOD END + CHARGE TYPE + SEQ      */
/* WRITTEN BY CGCAL010, APPLIED BY CGAPL010 (JOB CGCHG01), WAIVED BY */
/* CGREF010. CICS FILE CHGPEND IS READ / BROWSE ONLY (CSD MBKCOR2).  */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.CHG.PENDING CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.CHG.PENDING)               -
                  INDEXED                                 -
                  KEYS(32 0)                              -
                  RECORDSIZE(250 250)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(150 30)                       -
                  VOLUMES(MBKV05))                        -
         DATA    (NAME(MBK.PRD.CHG.PENDING.DATA)          -
                  CONTROLINTERVALSIZE(8192))              -
         INDEX   (NAME(MBK.PRD.CHG.PENDING.INDEX)         -
                  CONTROLINTERVALSIZE(2048))
