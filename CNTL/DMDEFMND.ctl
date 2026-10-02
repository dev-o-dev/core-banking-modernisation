/*********************************************************************/
/* MEMBER   : DMDEFMND                                               */
/* TITLE    : IDCAMS - DEFINE DIRECT DEBIT MANDATE KSDS              */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT DMMNDREC - RECORDSIZE(300 300) KEYS(38 0)           */
/*   KEY = PAYER SORT CODE + ACCOUNT + ORIGINATOR SUN + PAYER REF    */
/* CICS FILE DDMAND (CSD MBKCOR2). NEW MANDATES ARE ADDED ONLY BY    */
/* BATCH (DMAUD010, JOB DMDDI01) - ONLINE IS UPDATE (CANCEL) ONLY.   */
/* MANDATES ARE NEVER DELETED (BACS AUDIT - 13 MONTHS AFTER CANCEL   */
/* AT LEAST), SO FREESPACE IS LOW AND INSERTS SPREAD ACROSS THE FILE.*/
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.DDM.MANDATE CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.DDM.MANDATE)               -
                  INDEXED                                 -
                  KEYS(38 0)                              -
                  RECORDSIZE(300 300)                     -
                  FREESPACE(15 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(300 30)                       -
                  VOLUMES(MBKV05 MBKV06)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.DDM.MANDATE.DATA)          -
                  CONTROLINTERVALSIZE(8192))              -
         INDEX   (NAME(MBK.PRD.DDM.MANDATE.INDEX)         -
                  CONTROLINTERVALSIZE(2048))
