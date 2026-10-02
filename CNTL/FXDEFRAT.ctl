/*********************************************************************/
/* MEMBER   : FXDEFRAT                                               */
/* TITLE    : IDCAMS - DEFINE FX RATE TABLE KSDS                     */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT FXRATREC - RECORDSIZE(150 150) KEYS(3 0)            */
/*   KEY = ISO CURRENCY CODE                                         */
/* SHAREOPTIONS(2 3): FXRAT010 (JOB FXRAT01) UPDATES IN BATCH AT     */
/* 07:45 AND 14:15 WHILE CICS FILE FXRATES HAS IT OPEN FOR READ.     */
/* FXRATES IS NOT IN AN LSR POOL SO CICS SEES THE NEW RATES AT ONCE  */
/* (SEE CSD MBKCOR2). DO NOT ADD THIS FILE TO LSR POOL 1.            */
/* AFTER A DEFINE THE TABLE IS EMPTY AND FXSVC01 REJECTS EVERY       */
/* QUOTE - RERUN FXRAT01 AGAINST THE LAST FIXING (MBK.PRD.FX.FIXING) */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.FX.RATES CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.FX.RATES)                  -
                  INDEXED                                 -
                  KEYS(3 0)                               -
                  RECORDSIZE(150 150)                     -
                  FREESPACE(30 20)                        -
                  SHAREOPTIONS(2 3)                       -
                  TRACKS(2 1)                             -
                  VOLUMES(MBKV07))                        -
         DATA    (NAME(MBK.PRD.FX.RATES.DATA)             -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.FX.RATES.INDEX)            -
                  CONTROLINTERVALSIZE(512))
