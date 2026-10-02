/*********************************************************************/
/* MEMBER   : DDDEFNOC                                               */
/* TITLE    : IDCAMS - DEFINE ACCOUNT NUMBER ALLOCATION CONTROL KSDS */
/* USED BY  : JOB MBKDEFVS (INITIAL DEFINITION ONLY)                 */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT DDNOCREC - RECORDSIZE(100 100) KEYS(16 0)           */
/*   KEY = SORT CODE + RANGE LOW (7) + RANGE ID                      */
/* CICS FILE ACCTNOC (CSD MBKCOR2) - READ UPDATE BY DDOPN01.         */
/* THE DATASET IS MBK.PRD.DDA.ACCTNO.CONTROL AS IN THE CSD. THE      */
/* DDNOCREC HEADER SAYS 'ACCTNOCTL' - THE COPYBOOK IS WRONG.         */
/*-------------------------------------------------------------------*/
/* !!! NEVER REDEFINE IN PRODUCTION WITHOUT RELOADING THE LAST BASE  */
/* NUMBER USED FOR EVERY RANGE - DDOPN01 WOULD REISSUE ACCOUNT       */
/* NUMBERS. LOAD THE RANGES FROM CNTL(DDNOCLOD) WITH REPRO.          */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.DDA.ACCTNO.CONTROL CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.DDA.ACCTNO.CONTROL)        -
                  INDEXED                                 -
                  KEYS(16 0)                              -
                  RECORDSIZE(100 100)                     -
                  FREESPACE(50 50)                        -
                  SHAREOPTIONS(2 3)                       -
                  TRACKS(5 1)                             -
                  VOLUMES(MBKV07))                        -
         DATA    (NAME(MBK.PRD.DDA.ACCTNO.CONTROL.DATA)   -
                  CONTROLINTERVALSIZE(2048))              -
         INDEX   (NAME(MBK.PRD.DDA.ACCTNO.CONTROL.INDEX)  -
                  CONTROLINTERVALSIZE(512))
