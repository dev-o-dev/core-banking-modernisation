/*********************************************************************/
/* MEMBER   : PRDEFPRM                                               */
/* TITLE    : IDCAMS - DEFINE PRODUCT PARAMETER KSDS                 */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT PRDPARRC - RECORDSIZE(1000 1000) KEYS(10 0)         */
/*   KEY = PRODUCT CODE                                              */
/* THE DATASET IS MBK.PRD.PRD.PARMS AS USED BY THE JCL AND CSD FILE  */
/* PRDPARM. THE PRDPARRC HEADER SAYS 'PARAMS' - THE COPYBOOK IS      */
/* WRONG, NOT THE DATASET.                                           */
/* UPDATED ONLY BY PRMNT010 (JOB PRMNT01) AND INRAT010 (JOB INRAT01).*/
/* SMALL (UNDER 200 PRODUCTS) AND READ BY EVERY BATCH SUITE - KEEP   */
/* IT ON ITS OWN VOLUME AWAY FROM THE ACCOUNT MASTER.                */
/* AFTER A DEFINE THE FILE IS EMPTY: RELOAD FROM THE LAST BACKUP     */
/* (MBK.PRD.PRD.AUDIT(0) HOLDS ONLY CHANGES) BEFORE ANY BATCH RUNS.  */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.PRD.PARMS CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.PRD.PARMS)                 -
                  INDEXED                                 -
                  KEYS(10 0)                              -
                  RECORDSIZE(1000 1000)                   -
                  FREESPACE(30 20)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(2 1)                          -
                  VOLUMES(MBKV07))                        -
         DATA    (NAME(MBK.PRD.PRD.PARMS.DATA)            -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.PRD.PARMS.INDEX)           -
                  CONTROLINTERVALSIZE(1024))
