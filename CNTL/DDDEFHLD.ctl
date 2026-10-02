/*********************************************************************/
/* MEMBER   : DDDEFHLD                                               */
/* TITLE    : IDCAMS - DEFINE FUNDS HOLDS KSDS                       */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT DDHLDREC - RECORDSIZE(150 150) KEYS(18 0)           */
/*   KEY = SORT CODE + ACCOUNT + HOLD SEQUENCE (0000 = CONTROL REC)  */
/* CICS FILE DDHOLD (CSD MBKCOR2, STRINGS 40 - CARD AUTHORISATIONS   */
/* WRITE A HOLD PER APPROVAL). HIGH INSERT RATE - GENEROUS FREESPACE.*/
/* EXPIRED / RELEASED HOLDS ARE LEFT ON FILE BY DDHLX010; REORG      */
/* MONTHLY (UNLOAD, DELETE STATUS E/R OLDER THAN 90 DAYS, RELOAD).   */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.DDA.HOLDS CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.DDA.HOLDS)                 -
                  INDEXED                                 -
                  KEYS(18 0)                              -
                  RECORDSIZE(150 150)                     -
                  FREESPACE(30 20)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(200 40)                       -
                  VOLUMES(MBKV04 MBKV05)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.DDA.HOLDS.DATA)            -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.DDA.HOLDS.INDEX)           -
                  CONTROLINTERVALSIZE(2048))
