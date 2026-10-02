/*********************************************************************/
/* MEMBER   : PSDEFMEM                                               */
/* TITLE    : IDCAMS - DEFINE MEMO LOG ESDS AND EXTERNAL REFERENCE   */
/*            KSDS                                                   */
/* USED BY  : JOB MBKDEFVS (INITIAL DEFINITION ONLY)                 */
/*-------------------------------------------------------------------*/
/* BOTH ARE DEFINED REUSE SO THAT PSEOD01 CAN EMPTY THEM EACH NIGHT  */
/* (REPRO ... REUSE) WITHOUT A DELETE/DEFINE.                        */
/* LOG(UNDO) - RECOVERABLE IN CICS (BACKOUT) - SEE CSD GROUP MBKCORE.*/
/*********************************************************************/
  DELETE MBK.PRD.PST.MEMOLOG CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.PST.MEMOLOG)               -
                  NONINDEXED                              -
                  RECORDSIZE(200 200)                     -
                  SHAREOPTIONS(2 3)                       -
                  REUSE                                   -
                  LOG(UNDO)                               -
                  CYLINDERS(150 30)                       -
                  VOLUMES(MBKV05))                        -
         DATA    (NAME(MBK.PRD.PST.MEMOLOG.DATA)          -
                  CONTROLINTERVALSIZE(18432))

  DELETE MBK.PRD.PST.EXTREF CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.PST.EXTREF)                -
                  INDEXED                                 -
                  KEYS(30 0)                              -
                  RECORDSIZE(80 80)                       -
                  FREESPACE(30 20)                        -
                  SHAREOPTIONS(2 3)                       -
                  REUSE                                   -
                  LOG(UNDO)                               -
                  CYLINDERS(50 10)                        -
                  VOLUMES(MBKV05))                        -
         DATA    (NAME(MBK.PRD.PST.EXTREF.DATA)           -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.PST.EXTREF.INDEX))
