/*********************************************************************/
/* MEMBER   : CSDEFSWT                                               */
/* TITLE    : IDCAMS - DEFINE CASS SWITCH CONTROL KSDS               */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT CSSRECS (CSW-) - RECORDSIZE(250 250) KEYS(18 0)     */
/*   KEY = CASS SWITCH ID                                            */
/* CICS FILE SWITCHES (CSD MBKCOR2) - BRANCH SWITCH REQUESTS ARE     */
/* WRITTEN ONLINE, CSSIN010 / CSSOU010 MAINTAIN THEM IN BATCH.       */
/* RECORDS STAY ON FILE 36 MONTHS AFTER THE SWITCH (REDIRECTION).    */
/*-------------------------------------------------------------------*/
/* 2013-06-17 PDQ  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.CASS.SWITCHES CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.CASS.SWITCHES)             -
                  INDEXED                                 -
                  KEYS(18 0)                              -
                  RECORDSIZE(250 250)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(10 5)                         -
                  VOLUMES(MBKV04 MBKV05)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.CASS.SWITCHES.DATA)        -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.CASS.SWITCHES.INDEX)       -
                  CONTROLINTERVALSIZE(2048))
