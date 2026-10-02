/*********************************************************************/
/* MEMBER   : LNDEFMST                                               */
/* TITLE    : IDCAMS - DEFINE PERSONAL LOAN MASTER KSDS, CIF AIX,    */
/*            PATH                                                   */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT LNACCREC - RECORDSIZE(500 500) KEYS(10 0)           */
/*   KEY = LOAN NUMBER ('0000000000' = NUMBER CONTROL RECORD)        */
/* AIX KEY = LN-CIF-NO (OFFSET 10, LENGTH 12), NONUNIQUE.            */
/* CICS FILES LOANMST (BASE) AND LOANCIF (PATH) - CSD MBKCOR2.       */
/* CFDEC010 READS THE PATH IN BATCH (DD LOANMAS1).                   */
/* THE AIX IS UPGRADED AUTOMATICALLY (UPGRADE). AFTER A RELOAD OF    */
/* THE BASE CLUSTER THE AIX MUST BE REBUILT - BLDINDEX BELOW.        */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.LOAN.MASTER CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.LOAN.MASTER)               -
                  INDEXED                                 -
                  KEYS(10 0)                              -
                  RECORDSIZE(500 500)                     -
                  FREESPACE(10 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(100 20)                       -
                  VOLUMES(MBKV08)                         -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.LOAN.MASTER.DATA)          -
                  CONTROLINTERVALSIZE(8192))              -
         INDEX   (NAME(MBK.PRD.LOAN.MASTER.INDEX)         -
                  CONTROLINTERVALSIZE(2048))

  DEFINE ALTERNATEINDEX                                   -
                 (NAME(MBK.PRD.LOAN.MASTER.CIFAIX)        -
                  RELATE(MBK.PRD.LOAN.MASTER)             -
                  KEYS(12 10)                             -
                  NONUNIQUEKEY                            -
                  UPGRADE                                 -
                  RECORDSIZE(80 400)                      -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(15 5)                         -
                  VOLUMES(MBKV08))                        -
         DATA    (NAME(MBK.PRD.LOAN.MASTER.CIFAIX.DATA))  -
         INDEX   (NAME(MBK.PRD.LOAN.MASTER.CIFAIX.INDEX))

  DEFINE PATH    (NAME(MBK.PRD.LOAN.MASTER.CIFPATH)       -
                  PATHENTRY(MBK.PRD.LOAN.MASTER.CIFAIX)   -
                  UPDATE)

  BLDINDEX INDATASET(MBK.PRD.LOAN.MASTER)                 -
           OUTDATASET(MBK.PRD.LOAN.MASTER.CIFAIX)         -
           INTERNALSORT
