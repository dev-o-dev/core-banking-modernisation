/*********************************************************************/
/* MEMBER   : MBKGDG2                                                */
/* TITLE    : IDCAMS - DEFINE GDG BASES FOR THE 2010/2011 BATCH      */
/*            SUITES                                                 */
/* USED BY  : JOB MBKDEFVS STEP DEF140 (INITIAL DEFINITION ONLY)     */
/*-------------------------------------------------------------------*/
/* ONE BASE FOR EVERY GENERATION DATASET USED AS (+1) OR (0) BY THE  */
/* JOBS ADDED FROM 2009 (CHARGES, INTEREST, SO, DD, CLOSURE, GL,     */
/* CHEQUES, CARDS, LOANS, ISA, FX, CIF, AML). BASES FOR THE ORIGINAL */
/* JOBS (PSEOD01, PYBACS01, IBFEED02 ETC.) ARE IN MBKGDG1, WHICH IS  */
/* HELD BY STORAGE MANAGEMENT AND IS NOT IN THIS LIBRARY.            */
/*                                                                   */
/* NOT DESTRUCTIVE - THERE IS NO DELETE. A BASE THAT ALREADY EXISTS  */
/* FAILS WITH IDC3013I DUPLICATE DATA SET NAME (CC 12) AND IDCAMS    */
/* GOES ON TO THE NEXT DEFINE, SO A RERUN ENDS CC 12. CHECK SYSPRINT */
/* FOR ANY MESSAGE OTHER THAN IDC3013I/IDC3009I.                     */
/*                                                                   */
/* LIMITS: DAILY FILES 30 (35 FOR GL - FINANCE MONTH END PLUS A      */
/* WEEK), WEEKLY / MONTHLY 13, ANNUAL TAX/HMRC 7. NOEMPTY SCRATCH    */
/* THROUGHOUT (OLDEST GENERATION ROLLS OFF AND IS DELETED).          */
/* QUARTERLY 12. MBK.PRD.TXNHIST.ARCHIVE IS ON CARTRIDGE, LIMIT 255  */
/* (A ROLLED-OFF TAPE IS UNCATALOGUED, THE VOLUME KEEPS ITS RETPD).  */
/* AN INPUT BASE (READ AS (0) ONLY) MUST HAVE AT LEAST ONE GENERATION*/
/* BEFORE THE JOB FIRST RUNS - CREATE AN EMPTY ONE WITH IEFBR14.     */
/*-------------------------------------------------------------------*/
/* 2011-06-13 JNB  ORIGINAL                                          */
/* 2014-08-11 PDQ  OD, MI, BCP, TXNHIST ARCHIVE, BOE RETURN AND      */
/*                 ANNUAL CHARGES SUMMARY BASES                      */
/*********************************************************************/
/*-------------------------------------------------------------------*/
/* INTEREST AND TAX (INACR01, INCAP01, INFTD01, INRAT01, INTAX01)    */
/*-------------------------------------------------------------------*/
/* INACR01 - DAILY ACCRUAL DETAIL                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.INT.ACCRUALS)          -
                  LIMIT(30) NOEMPTY SCRATCH)
/* INACR01 - NO PRODUCT PARAMETERS                                   */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.INT.ACCR.EXCEPTS)      -
                  LIMIT(30) NOEMPTY SCRATCH)
/* INRAT01 - RATE CHANGE NOTICES                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.INT.RATE.NOTICES)      -
                  LIMIT(13) NOEMPTY SCRATCH)
/* INFTD01 - MATURITY NOTICES                                        */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.FTD.NOTICES)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* INTAX01 - ANNUAL - 7 TAX YEARS                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.TAX.CERTS)             -
                  LIMIT(7) NOEMPTY SCRATCH)
/* INTAX01 - ANNUAL - 7 TAX YEARS                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.TAX.BBSI)              -
                  LIMIT(7) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* CHARGES (CGCHG01, CGREF01)                                        */
/*-------------------------------------------------------------------*/
/* CGCHG01 - 14-DAY PRE-NOTIFICATION                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHG.NOTICES)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CGREF01 INPUT (0) - COMPLAINTS                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHG.REFUNDS.APPROVED)  -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* STANDING ORDERS (SOEXE01)                                         */
/*-------------------------------------------------------------------*/
/* SOEXE01 - TO PAYMENTS GATEWAY                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.SO.FPSOUT)             -
                  LIMIT(30) NOEMPTY SCRATCH)
/* SOEXE01 - EXCEPTIONS                                              */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.SO.EXCEPTS)            -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* DIRECT DEBITS (DMDDI01, DMCOL01)                                  */
/*-------------------------------------------------------------------*/
/* DMDDI01 INPUT (0) - FROM BACS                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BACS.AUDDIS.IN)        -
                  LIMIT(30) NOEMPTY SCRATCH)
/* DMDDI01                                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BACS.AUDDIS.REJECTS)   -
                  LIMIT(30) NOEMPTY SCRATCH)
/* DMDDI01 - TO BACS                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BACS.ADDACS.OUT)       -
                  LIMIT(30) NOEMPTY SCRATCH)
/* DMCOL01 INPUT (0) - FROM BACS                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BACS.DDI.IN)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* DMCOL01 - TO BACS                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BACS.ARUDD.OUT)        -
                  LIMIT(30) NOEMPTY SCRATCH)
/* DMCOL01 - UNPAID DIRECT DEBITS                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.DDM.RETURNS)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* DMCOL01 - CUSTOMER LETTERS                                        */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.DDM.LETTERS)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* ACCOUNT LIFECYCLE (DDCLS01, DDDRM01, STMT01)                      */
/*-------------------------------------------------------------------*/
/* DDCLS01 -> STMT01 (0)                                             */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.STMT.REQUESTS.CLOSE)   -
                  LIMIT(14) NOEMPTY SCRATCH)
/* DDDRM01 - MONTHLY                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.DRM.LETTERS)           -
                  LIMIT(13) NOEMPTY SCRATCH)
/* DDDRM01 - MONTHLY - RECLAIM FUND                                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.DRM.RECLAIM)           -
                  LIMIT(13) NOEMPTY SCRATCH)
/* STMT01 ST015                                                      */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.STMT.REQUEST.SORTED)   -
                  LIMIT(14) NOEMPTY SCRATCH)
/* STMT01 - TO DOCUMENT SERVICES                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.STMT.PRINT)            -
                  LIMIT(14) NOEMPTY SCRATCH)
/* STMT01 - STATEMENT INDEX                                          */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.STMT.INDEX)            -
                  LIMIT(30) NOEMPTY SCRATCH)
/* STMT01 - TO IBANK                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.STMT.ESTMT.FEED)       -
                  LIMIT(14) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* POSTING (PSEOD01)                                                 */
/*-------------------------------------------------------------------*/
/* PSEOD01 PS015 - MEMO BREAKS                                       */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.PST.MEMO.BREAKS)       -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* GENERAL LEDGER (GLFEED01, GLSUS01 AND THE JOURNAL SOURCES)        */
/*-------------------------------------------------------------------*/
/* INACR01 -> GLFEED01 (0)                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.ACCR)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* INCAP01 -> GLFEED01 (0)                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.ICAP)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* CGCHG01 -> GLFEED01 (0)                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.CHRG)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* CGREF01 -> GLFEED01 (0)                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.CHRF)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* DDCLS01 -> GLFEED01 (0)                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.CLOS)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* LNDLY01 -> GLFEED01 (0)                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.LOAN)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* LNDLY01 LN030                                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.LNRP)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* LNDLY01 LN040                                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.LNAR)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* GLFEED01 INPUT (0)                                                */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.SUSP)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* GLFEED01 GL010                                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.MOVT)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* GLFEED01 GL020                                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.FEED.MERGED)        -
                  LIMIT(35) NOEMPTY SCRATCH)
/* GLFEED01 - TO ORACLE GL                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.FEED)               -
                  LIMIT(35) NOEMPTY SCRATCH)
/* GLFEED01 INPUT (0) - FROM ORACLE                                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.BALANCES)           -
                  LIMIT(35) NOEMPTY SCRATCH)
/* GLSUS01 INPUT (0) - FINANCE                                       */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.SUSP.CLEARED)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* GLSUS01                                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.SUSP.ESCALATE)      -
                  LIMIT(35) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* PRODUCTS (PRMNT01)                                                */
/*-------------------------------------------------------------------*/
/* PRMNT01 (0) IN, (+1) CARRY FWD                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.PRD.CHANGES)           -
                  LIMIT(10) NOEMPTY SCRATCH)
/* PRMNT01 - KEEP ALL (AUDIT)                                        */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.PRD.AUDIT)             -
                  LIMIT(99) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* CHEQUES (CQCLR01)                                                 */
/*-------------------------------------------------------------------*/
/* CQCLR01 INPUT (0) - FROM IPSL                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHQ.INWARD)            -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CQCLR01 CQ010                                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHQ.INWARD.SORTED)     -
                  LIMIT(7) NOEMPTY SCRATCH)
/* CQCLR01 - TO IPSL                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHQ.RETURNS)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CQCLR01 INPUT (0) - FROM IPSL                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHQ.RETURNED.IN)       -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CQCLR01                                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHQ.LETTERS)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CQCLR01                                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHQ.UNPAID.LETTERS)    -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* CARDS (CDSTL01)                                                   */
/*-------------------------------------------------------------------*/
/* CDSTL01 INPUT (0) - FROM SCHEME                                   */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CARD.CLEARING)         -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CDSTL01 CD010                                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CARD.CLEARING.SORTED)  -
                  LIMIT(7) NOEMPTY SCRATCH)
/* CDSTL01 - CARD OPERATIONS                                         */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CARD.EXCEPTIONS)       -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* PERSONAL LOANS (LNDLY01)                                          */
/*-------------------------------------------------------------------*/
/* LNDLY01 INPUT (0) - FROM LOS                                      */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.APPROVED)         -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN010                                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.REJECTS)          -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN010                                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.LETTERS.WELC)     -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN030                                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.LETTERS.REP)      -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN040 - NOSIA/DEFAULT                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.LETTERS.ARR)      -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN050 - CCA S77A                                          */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.LETTERS.ANST)     -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN040                                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.COLLECT)          -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN045 - AGENCY 1                                          */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.COLLECT.DCA1)     -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN045 - AGENCY 2                                          */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.COLLECT.DCA2)     -
                  LIMIT(30) NOEMPTY SCRATCH)
/* LNDLY01 LN045 - AGENCY 3                                          */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.LOAN.COLLECT.DCA3)     -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* SAVINGS - CASH ISA (ISASUB01, ISAYRE01)                           */
/*-------------------------------------------------------------------*/
/* ISASUB01                                                          */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.ISA.BREACHES)          -
                  LIMIT(30) NOEMPTY SCRATCH)
/* ISAYRE01 - ANNUAL                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.ISA.HMRC.RETURN)       -
                  LIMIT(7) NOEMPTY SCRATCH)
/* ISAYRE01 IY005 - ANNUAL BACKUP                                    */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.DDA.ACCTMAST.ISABKUP)  -
                  LIMIT(3) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* FX (FXRAT01)                                                      */
/*-------------------------------------------------------------------*/
/* FXRAT01 INPUT (0) - 2 A DAY                                       */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.FX.FIXING)             -
                  LIMIT(20) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* CIF - BEREAVEMENT, DUPLICATES, KYC (CFDEC01, CFDUP01, CFKYC01)    */
/*-------------------------------------------------------------------*/
/* CFDEC01 INPUT (0)                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CIF.BEREAVE.IN)        -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CFDEC01                                                           */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CIF.BEREAVE.LETTERS)   -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CFDEC01 - CHAPS PAYOUT LIST                                       */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CIF.BEREAVE.PAYOUT)    -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CFDUP01 - MONTHLY                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CIF.DUPCAND)           -
                  LIMIT(13) NOEMPTY SCRATCH)
/* CFKYC01 - WEEKLY                                                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.KYC.CASES)             -
                  LIMIT(13) NOEMPTY SCRATCH)
/* CFKYC01 - WEEKLY                                                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.KYC.LETTERS)           -
                  LIMIT(13) NOEMPTY SCRATCH)
/* CFKYC01 INPUT (0) - CASE SYSTEM                                   */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.KYC.COMPLETED)         -
                  LIMIT(13) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* REGULATORY (RGAML01)                                              */
/*-------------------------------------------------------------------*/
/* RGAML01 - TO FINANCIAL CRIME                                      */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.AML.ALERTS)            -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* ADDED 2011-07-04 AOS - CFMRG01, CHLGSWP1, INNTC01, RGSCV01        */
/*-------------------------------------------------------------------*/
/* CFMRG01 INPUT (0) - CIF DATA QUALITY TEAM                         */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CIF.MERGE.APPROVED)    -
                  LIMIT(13) NOEMPTY SCRATCH)
/* CFMRG01 - AUDIT TRAIL KEPT 7 YEARS ON TAPE (HSM ML2)              */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CIF.MERGE.AUDIT)       -
                  LIMIT(52) NOEMPTY SCRATCH)
/* CHLGSWP1 - DAILY CHANNEL LOG                                      */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CICS.CHLG.DAILY)       -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CHLGSWP1 - CAPACITY SUMMARY                                       */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CICS.CHLG.SUMMARY)     -
                  LIMIT(30) NOEMPTY SCRATCH)
/* INNTC01 - LETTERS                                                 */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.SAV.NOTICE.LETTERS)    -
                  LIMIT(30) NOEMPTY SCRATCH)
/* RGSCV01 - SCV FILE (REPLACES MBK.PRD.REG.SCV.EXTRACT)             */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.REG.SCV.FILE)          -
                  LIMIT(7) NOEMPTY SCRATCH)
/* STMT01 ST012 - AD-HOC STATEMENT REQUESTS (STREQ01)                */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.STMT.REQUEST.ADHOC)    -
                  LIMIT(30) NOEMPTY SCRATCH)
/* ODAUSWP1 / CPAUSWP1 - SWEPT ONLINE AUDIT QUEUES                   */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CICS.ODAU.DAILY)       -
                  LIMIT(30) NOEMPTY SCRATCH)
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CICS.CPAU.DAILY)       -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CHLGSWP1 - EXISTING GDG FOR ALTERNATE DATASET COPY - SEE MBKGDG1  */
/* CSSDLY01 - CASS FILES (2013-06-17 PDQ)                            */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CASS.INBOUND)          -
                  LIMIT(30) NOEMPTY SCRATCH)
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CASS.OUTBOUND)         -
                  LIMIT(30) NOEMPTY SCRATCH)
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CASS.NOTIFY)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* ADDED 2014-08-11 PDQ - ODREV01, MIEXT01, BCPEXT01, PSARC01,       */
/* RGBOE01, CHGTAR01                                                 */
/*-------------------------------------------------------------------*/
/* ODREV01 OD010 - OVERDRAFT LETTERS (ODLTRREC)                      */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.OD.LETTERS)            -
                  LIMIT(30) NOEMPTY SCRATCH)
/* ODREV01 OD010 - REFERRALS                                         */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.OD.REFERRAL)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* ODREV01 OD020 - TO FINANCIAL SUPPORT UNIT                         */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.OD.REFERRAL.FSU)       -
                  LIMIT(30) NOEMPTY SCRATCH)
/* MIEXT01 - MONTHLY - TO DATA WAREHOUSE                             */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.MI.DEPOSITS)           -
                  LIMIT(13) NOEMPTY SCRATCH)
/* BCPEXT01 BC010 - BCP CRITICAL - TO DR SITE                        */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BCP.ACCTEXT)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* BCPEXT01 BC020 - REGIONAL FILES FOR BCPVIEW                       */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BCP.REGION.LONDON1)    -
                  LIMIT(30) NOEMPTY SCRATCH)
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BCP.REGION.LONDON2)    -
                  LIMIT(30) NOEMPTY SCRATCH)
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BCP.REGION.NORTH)      -
                  LIMIT(30) NOEMPTY SCRATCH)
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BCP.REGION.SOUTH)      -
                  LIMIT(30) NOEMPTY SCRATCH)
/* PSARC01 - MONTHLY ON CARTRIDGE, RETPD 10 YEARS (SEE PSARCREC).    */
/* A RESTART ADDS A GENERATION, SO KEEP THE MAXIMUM                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.TXNHIST.ARCHIVE)       -
                  LIMIT(255) NOEMPTY SCRATCH)
/* RGBOE01 - QUARTERLY - BANK OF ENGLAND RETURN, 3 YEARS             */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.REG.BOERTN)            -
                  LIMIT(12) NOEMPTY SCRATCH)
/* CHGTAR01 - MONTHLY - ANNUAL CHARGES SUMMARY LETTERS               */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHG.ANNSUMM)           -
                  LIMIT(13) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* ADDED 2017-03-06 PDQ - ATMSTL01, CFSEG01, DDALR01, IBFEED03,      */
/*                        INRAT01 INPUT                              */
/*-------------------------------------------------------------------*/
/* ATMSTL01 - LINK SETTLEMENT FILE (INPUT FROM LINK)                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.ATM.LINK.SETTLE)       -
                  LIMIT(30) NOEMPTY SCRATCH)
/* ATMSTL01 - INTERCHANGE GL JOURNAL                                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.ATMS)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* CFSEG01 - SEGMENT CHANGES TO CRM                                   */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CIF.SEGMENT.CHANGES)   -
                  LIMIT(13) NOEMPTY SCRATCH)
/* DDALR01 - SMS / EMAIL ALERT FILE                                   */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.DDA.ALERTS.OUTPUT)     -
                  LIMIT(30) NOEMPTY SCRATCH)
/* IBFEED03 - IBANK TRANSACTION FEED                                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.IBANK.IBFEED03.OUTPUT) -
                  LIMIT(30) NOEMPTY SCRATCH)
/* INRAT01 INPUT - BUILT BY ISPF DIALOG MBKRT1                        */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.INT.RATECHG.INPUT)     -
                  LIMIT(13) NOEMPTY SCRATCH)
/*-------------------------------------------------------------------*/
/* ADDED 2017-03-06 PDQ - PYBGC01, CMPRD01, TAXCR01                  */
/*-------------------------------------------------------------------*/
/* PYBGC01 INPUT - BRANCH IMAGE CAPTURE                               */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.BRN.IMGCAP)            -
                  LIMIT(30) NOEMPTY SCRATCH)
/* PYBGC01 - OUTWARD CLEARING ITEMS                                   */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CHQ.OUTWARD)           -
                  LIMIT(30) NOEMPTY SCRATCH)
/* PYBGC01 - BRANCH SUSPENSE GL JOURNAL                               */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.BGCS)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* CMPRD01 INPUT - COMPLAINTS SYSTEM                                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CMPL.REDRESS.APPROVED) -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CMPRD01 - REDRESS LETTERS                                          */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CMPL.REDRESS.LETTERS)  -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CMPRD01 - RESPONSE TO COMPLAINTS SYSTEM                            */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.CMPL.REDRESS.RESPONSE) -
                  LIMIT(30) NOEMPTY SCRATCH)
/* CMPRD01 - REDRESS GL JOURNAL                                       */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.GL.JOURNAL.CMPL)       -
                  LIMIT(35) NOEMPTY SCRATCH)
/* TAXCR01 - HMRC CRS / FATCA RETURN                                  */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.TAX.CRSRETN)           -
                  LIMIT(7) NOEMPTY SCRATCH)
/* TAXCR01 - SELF-CERT CHASE LIST                                     */
  DEFINE GENERATIONDATAGROUP (NAME(MBK.PRD.TAX.CRSCHASE)          -
                  LIMIT(7) NOEMPTY SCRATCH)
