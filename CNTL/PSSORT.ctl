*********************************************************************
* MEMBER   : PSSORT                                                 *
* TITLE    : DFSORT - SORT/MERGE THE DAY'S POSTINGS FOR PSEOD010    *
* USED BY  : PSEOD01 STEP PS010                                     *
*-------------------------------------------------------------------*
* KEY = PST-KEY (SORT CODE, ACCOUNT, POST DATE, POST TIME, SEQ)     *
*       POSITIONS 1-36, ALL DISPLAY - CHARACTER COMPARE IS CORRECT. *
* EQUALS KEEPS INPUT ORDER FOR IDENTICAL KEYS (TWO POSTINGS IN THE  *
* SAME HUNDREDTH OF A SECOND FROM DIFFERENT CICS TASKS).            *
* OMIT DROPS ANY MEMO LOG RECORD ALREADY MARKED REJECTED.           *
*********************************************************************
  OPTION EQUALS
  OMIT COND=(162,1,CH,EQ,C'R')
  SORT FIELDS=(1,36,CH,A)
