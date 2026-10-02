/*********************************************************************/
/* MEMBER   : CQDEFCHQ                                               */
/* TITLE    : IDCAMS - DEFINE CHEQUE STOPS AND UNCLEARED CHEQUE KSDS */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUTS IN CHQRECS:                                        */
/*   CQS- STOPS      RECORDSIZE(100 100) KEYS(20 0) - CICS CHQSTOP   */
/*   CQD- UNCLEARED  RECORDSIZE(120 120) KEYS(28 0) - WRITTEN BY THE */
/*                   TELLER SYSTEM, RELEASED BY CQUNC010             */
/* BOTH ARE UPDATED BY JOB CQCLR01 (DISP=OLD).                       */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.CHQ.STOPS CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.CHQ.STOPS)                 -
                  INDEXED                                 -
                  KEYS(20 0)                              -
                  RECORDSIZE(100 100)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(20 5)                         -
                  VOLUMES(MBKV06))                        -
         DATA    (NAME(MBK.PRD.CHQ.STOPS.DATA)            -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.CHQ.STOPS.INDEX)           -
                  CONTROLINTERVALSIZE(1024))

  DELETE MBK.PRD.CHQ.UNCLEARED CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.CHQ.UNCLEARED)             -
                  INDEXED                                 -
                  KEYS(28 0)                              -
                  RECORDSIZE(120 120)                     -
                  FREESPACE(25 15)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(30 10)                        -
                  VOLUMES(MBKV06))                        -
         DATA    (NAME(MBK.PRD.CHQ.UNCLEARED.DATA)        -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.CHQ.UNCLEARED.INDEX)       -
                  CONTROLINTERVALSIZE(1024))
