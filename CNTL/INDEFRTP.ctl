/*********************************************************************/
/* MEMBER   : INDEFRTP                                               */
/* TITLE    : IDCAMS - DEFINE PENDING INTEREST RATE CHANGE KSDS      */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT RTCHGREC - RECORDSIZE(100 100) KEYS(22 0)           */
/*   KEY = PRODUCT + EFFECTIVE DATE + RATE TYPE + TIER + FILLER      */
/* WRITTEN AT ANNOUNCEMENT AND UPDATED AT APPLICATION BY INRAT010    */
/* (JOB INRAT01). NOT USED ONLINE.                                   */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.INT.RATEPEND CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.INT.RATEPEND)              -
                  INDEXED                                 -
                  KEYS(22 0)                              -
                  RECORDSIZE(100 100)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  TRACKS(15 5)                            -
                  VOLUMES(MBKV07))                        -
         DATA    (NAME(MBK.PRD.INT.RATEPEND.DATA)         -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.INT.RATEPEND.INDEX)        -
                  CONTROLINTERVALSIZE(512))
