/*********************************************************************/
/* MEMBER   : CFDEFCIF                                               */
/* TITLE    : IDCAMS - DEFINE CIF MASTER KSDS                        */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* SHAREOPTIONS(2 3): CICS REGION CICSPRD1 IS THE ONLY WRITER WHILE  */
/* ONLINE; BATCH UPDATES (CUSTMNT2) RUN ONLY IN THE BATCH WINDOW     */
/* AFTER JOB MBKONLDN HAS CLOSED CIFMAST IN CICS.                    */
/*********************************************************************/
  DELETE MBK.PRD.CIF.MASTER CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.CIF.MASTER)                -
                  INDEXED                                 -
                  KEYS(12 0)                              -
                  RECORDSIZE(700 700)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(400 50)                       -
                  VOLUMES(MBKV01 MBKV02)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.CIF.MASTER.DATA)           -
                  CONTROLINTERVALSIZE(8192))              -
         INDEX   (NAME(MBK.PRD.CIF.MASTER.INDEX)          -
                  CONTROLINTERVALSIZE(2048))
