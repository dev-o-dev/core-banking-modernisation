/*********************************************************************/
/* MEMBER   : TXDEFRES                                               */
/* TITLE    : IDCAMS - DEFINE TAX RESIDENCY SELF-CERTIFICATION KSDS  */
/* USED BY  : JOB MBKDEFVS (INITIAL LOAD / REORG ONLY)               */
/*-------------------------------------------------------------------*/
/* RECORD LAYOUT TXRESREC - RECORDSIZE(300 300) KEYS(12 0)           */
/*   KEY = CIF NUMBER                                                */
/* CICS FILE TAXCERT (CSD MBKCOR2) - WRITTEN BY THE BRANCH SCREEN    */
/* AND IBANK, READ AND UPDATED ANNUALLY BY TAXCR010 (FATCA / CRS).   */
/* SELF-CERTIFICATIONS ARE KEPT 6 YEARS AFTER THE RELATIONSHIP ENDS  */
/* (HMRC RECORD KEEPING) - NEVER DELETE ON A REORG.                  */
/*-------------------------------------------------------------------*/
/* 2015-11-23 HBK  ORIGINAL (CRS PROJECT)                            */
/*********************************************************************/
  DELETE MBK.PRD.TAX.SELFCERT CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER (NAME(MBK.PRD.TAX.SELFCERT)              -
                  INDEXED                                 -
                  KEYS(12 0)                              -
                  RECORDSIZE(300 300)                     -
                  FREESPACE(20 10)                        -
                  SHAREOPTIONS(2 3)                       -
                  CYLINDERS(30 10)                        -
                  VOLUMES(MBKV04 MBKV05)                  -
                  SPEED)                                  -
         DATA    (NAME(MBK.PRD.TAX.SELFCERT.DATA)         -
                  CONTROLINTERVALSIZE(4096))              -
         INDEX   (NAME(MBK.PRD.TAX.SELFCERT.INDEX)        -
                  CONTROLINTERVALSIZE(2048))
