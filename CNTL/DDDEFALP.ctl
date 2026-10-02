/*********************************************************************/
/* MEMBER   : DDDEFALP                                               */
/* TITLE    : IDCAMS - DEFINE ACCOUNT ALERT PREFERENCES KSDS         */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT ALRTPREC - RECORDSIZE(150 150) KEYS(14 0)           */
/*   KEY = SORT CODE + ACCOUNT NUMBER (CURRENT ACCOUNTS ONLY)        */
/* CICS FILE ALRTPRF - MAINTAINED BY IBANK 'MANAGE ALERTS' AND THE   */
/* BRANCH SCREEN. DDALR010 (JOB DDALR01) UPDATES THE SUPPRESSION     */
/* STATE NIGHTLY AND ADDS DEFAULT RECORDS (HIGH INSERT RATE IN THE   */
/* FIRST MONTHS - FREESPACE IS GENEROUS, REORG QUARTERLY).           */
/*-------------------------------------------------------------------*/
/* !!! A REDEFINE WITHOUT RELOADING LOSES THE STATE FLAGS AND OPT-   */
/* OUTS - EVERY ACCOUNT IN UNARRANGED OVERDRAFT WOULD BE ALERTED     */
/* AGAIN AND OPTED-OUT CUSTOMERS WOULD BE ALERTED. ALWAYS REPRO OUT  */
/* FIRST AND RELOAD.                                                 */
/*-------------------------------------------------------------------*/
/* 2016-09-05 HLT  ORIGINAL                                          */
/*********************************************************************/
  DELETE MBK.PRD.DDA.ALERTPRF CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.DDA.ALERTPRF)              -
                  INDEXED                                 -
                  KEYS(14 0)                              -
                  RECORDSIZE(150 150)                     -
                  FREESPACE(30 20)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(40 10)                        -
                  VOLUMES(MBKV06 MBKV07)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.DDA.ALERTPRF.DATA)         -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.DDA.ALERTPRF.INDEX)        -
                  CONTROLINTERVALSIZE(2048))
