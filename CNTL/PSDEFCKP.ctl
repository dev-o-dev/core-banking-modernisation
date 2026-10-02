/*********************************************************************/
/* MEMBER   : PSDEFCKP                                               */
/* TITLE    : IDCAMS - DEFINE TXNHIST ARCHIVE CHECKPOINT KSDS        */
/* USED BY  : JOB MBKDEFVS STEP DEF138 (INITIAL DEFINITION ONLY)     */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT PSARC010 FD CHKPT-FILE (CKP-) - NO COPYBOOK         */
/*   RECORDSIZE(200 200) KEYS(8 0)                                   */
/*   KEY = 'PSARC010' - ONE RECORD ONLY, WRITTEN BY THE FIRST RUN    */
/*   (READ STATUS 23 = NO CHECKPOINT = NEW PASS), THEN REWRITTEN     */
/*   EVERY COMMITFQ DELETES. NOT A CICS FILE - BATCH ONLY (PSARC01). */
/*-------------------------------------------------------------------*/
/* !!! NEVER REDEFINE WHILE CKP-STATUS = R (RUNNING / ABENDED) - THE */
/* RESTART KEY AND CUMULATIVE COUNTS ARE LOST AND THE NEXT RUN       */
/* STARTS A NEW PASS WITH ZERO COUNTS. PRINT THE RECORD FIRST        */
/* (IDCAMS PRINT INDATASET(MBK.PRD.TXNHIST.CHKPT) CHAR).             */
/*-------------------------------------------------------------------*/
/* 2014-08-11 PDQ  ORIGINAL (PSARC01 CHKPT DD - SEE 2014-09-01)      */
/*********************************************************************/
  DELETE MBK.PRD.TXNHIST.CHKPT CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.TXNHIST.CHKPT)             -
                  INDEXED                                 -
                  KEYS(8 0)                               -
                  RECORDSIZE(200 200)                     -
                  FREESPACE(50 50)                        -
                  SHAREOPTIONS(2 3)                       -
                  TRACKS(1 1)                             -
                  VOLUMES(MBKV05))                        -
         DATA    (NAME(MBK.PRD.TXNHIST.CHKPT.DATA)        -
                  CONTROLINTERVALSIZE(2048))              -
         INDEX   (NAME(MBK.PRD.TXNHIST.CHKPT.INDEX)       -
                  CONTROLINTERVALSIZE(512))
