/*********************************************************************/
/* MEMBER   : CFDEFCTL                                               */
/* TITLE    : IDCAMS - DEFINE CIF NUMBER ALLOCATION CONTROL KSDS     */
/* USED BY  : JOB MBKDEFVS (INITIAL DEFINITION ONLY)                 */
/*-------------------------------------------------------------------*/
/* ONE RECORD, KEY 'NEXTCIF' (LAYOUT WS-CIF-CONTROL IN CFMNT01 -     */
/* THERE IS NO COPYBOOK): KEY X(12), LAST CIF 9(11), ALLOC COUNT     */
/* 9(9) COMP, LAST ALLOC DATE 9(8), FILLER X(49) = 84 BYTES.         */
/* CICS FILE CIFCTL (CSD MBKCOR2) - READ UPDATE BY CFMNT01.          */
/*-------------------------------------------------------------------*/
/* !!! NEVER REDEFINE IN PRODUCTION WITHOUT RELOADING THE CONTROL    */
/* RECORD WITH THE HIGHEST CIF NUMBER ON MBK.PRD.CIF.MASTER - CFMNT01*/
/* WOULD REISSUE CIF NUMBERS. THE CONTROL RECORD IS KEYED BY THE DBA */
/* (FILE-AID) UNDER THE SAME CHANGE.                                 */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.CIF.CONTROL CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.CIF.CONTROL)               -
                  INDEXED                                 -
                  KEYS(12 0)                              -
                  RECORDSIZE(84 84)                       -
                  FREESPACE(50 50)                        -
                  SHAREOPTIONS(2 3)                       -
                  TRACKS(1 1)                             -
                  VOLUMES(MBKV07))                        -
         DATA    (NAME(MBK.PRD.CIF.CONTROL.DATA)          -
                  CONTROLINTERVALSIZE(2048))              -
         INDEX   (NAME(MBK.PRD.CIF.CONTROL.INDEX)         -
                  CONTROLINTERVALSIZE(512))
