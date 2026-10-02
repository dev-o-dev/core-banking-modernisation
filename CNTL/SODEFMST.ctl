/*********************************************************************/
/* MEMBER   : SODEFMST                                               */
/* TITLE    : IDCAMS - DEFINE STANDING ORDER MASTER KSDS, DUE DATE   */
/*            AIX AND PATH                                           */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT SOMSTREC - RECORDSIZE(400 400) KEYS(18 0)           */
/*   KEY = PAYER SORT CODE + ACCOUNT + SO SEQUENCE (0000 = CONTROL)  */
/* AIX KEY = SOM-NEXT-DUE-DATE (OFFSET 18, LENGTH 8), NONUNIQUE.     */
/* CICS FILE SOMAST IS DEFINED OVER THE BASE CLUSTER (CSD MBKCOR2).  */
/* NO CICS FILE IS DEFINED OVER THE PATH - IT IS FOR BATCH ONLY.     */
/*-------------------------------------------------------------------*/
/* AIX RECORD = 5 + 8 + (18 X ORDERS DUE THAT DAY). THE MAXIMUM OF   */
/* 32600 HOLDS ABOUT 1,800 ORDERS PER DUE DATE. BLDINDEX REPORTS ANY */
/* DUE DATE WITH MORE ORDERS AND LEAVES THOSE POINTERS OUT - CHECK   */
/* SYSPRINT AFTER A REBUILD (1ST OF THE MONTH IS THE BUSY DATE).     */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL - SO MASTER PREVIOUSLY DEFINED BY HAND   */
/*                 FROM THE 1988 RUN BOOK                            */
/*********************************************************************/
  DELETE MBK.PRD.SO.MASTER CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.SO.MASTER)                 -
                  INDEXED                                 -
                  KEYS(18 0)                              -
                  RECORDSIZE(400 400)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(150 20)                       -
                  VOLUMES(MBKV05 MBKV06)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.SO.MASTER.DATA)            -
                  CONTROLINTERVALSIZE(8192))              -
         INDEX   (NAME(MBK.PRD.SO.MASTER.INDEX)           -
                  CONTROLINTERVALSIZE(2048))

  DEFINE ALTERNATEINDEX                                   -
                 (NAME(MBK.PRD.SO.MASTER.DUEAIX)          -
                  RELATE(MBK.PRD.SO.MASTER)               -
                  KEYS(8 18)                              -
                  NONUNIQUEKEY                            -
                  UPGRADE                                 -
                  RECORDSIZE(4000 32600)                  -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(40 10)                        -
                  VOLUMES(MBKV06))                        -
         DATA    (NAME(MBK.PRD.SO.MASTER.DUEAIX.DATA)     -
                  CONTROLINTERVALSIZE(32768))             -
         INDEX   (NAME(MBK.PRD.SO.MASTER.DUEAIX.INDEX))

  DEFINE PATH    (NAME(MBK.PRD.SO.MASTER.DUEPATH)         -
                  PATHENTRY(MBK.PRD.SO.MASTER.DUEAIX)     -
                  NOUPDATE)

  BLDINDEX INDATASET(MBK.PRD.SO.MASTER)                   -
           OUTDATASET(MBK.PRD.SO.MASTER.DUEAIX)           -
           INTERNALSORT
