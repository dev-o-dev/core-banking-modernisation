/*********************************************************************/
/* MEMBER   : DDDEFACM                                               */
/* TITLE    : IDCAMS - DEFINE DDA ACCOUNT MASTER KSDS, CIF AIX, PATH */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* THE AIX IS UPGRADED AUTOMATICALLY (UPGRADE) - CICS FILE ACCTCIF   */
/* IS DEFINED OVER THE PATH. AFTER A RELOAD OF THE BASE CLUSTER THE  */
/* AIX MUST BE REBUILT - STEP BLDINDEX BELOW.                        */
/*-------------------------------------------------------------------*/
/* 2005-09-12 AOS  CIF NUMBER MOVED TO OFFSET 14, AIX REBUILT        */
/*********************************************************************/
  DELETE MBK.PRD.DDA.ACCTMAST CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.DDA.ACCTMAST)              -
                  INDEXED                                 -
                  KEYS(14 0)                              -
                  RECORDSIZE(400 400)                     -
                  FREESPACE(15 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(600 60)                       -
                  VOLUMES(MBKV03 MBKV04)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.DDA.ACCTMAST.DATA)         -
                  CONTROLINTERVALSIZE(8192))              -
         INDEX   (NAME(MBK.PRD.DDA.ACCTMAST.INDEX)        -
                  CONTROLINTERVALSIZE(2048))

  DEFINE ALTERNATEINDEX                                   -
                 (NAME(MBK.PRD.DDA.ACCTMAST.CIFAIX)       -
                  RELATE(MBK.PRD.DDA.ACCTMAST)            -
                  KEYS(12 14)                             -
                  NONUNIQUEKEY                            -
                  UPGRADE                                 -
                  RECORDSIZE(200 4000)                    -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(80 10)                        -
                  VOLUMES(MBKV04))                        -
         DATA    (NAME(MBK.PRD.DDA.ACCTMAST.CIFAIX.DATA)) -
         INDEX   (NAME(MBK.PRD.DDA.ACCTMAST.CIFAIX.INDEX))

  DEFINE PATH    (NAME(MBK.PRD.DDA.ACCTMAST.CIFPATH)      -
                  PATHENTRY(MBK.PRD.DDA.ACCTMAST.CIFAIX)  -
                  UPDATE)

  BLDINDEX INDATASET(MBK.PRD.DDA.ACCTMAST)                -
           OUTDATASET(MBK.PRD.DDA.ACCTMAST.CIFAIX)        -
           INTERNALSORT
