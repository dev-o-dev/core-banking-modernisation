*********************************************************************
* MEMBER   : CUSTSORT                                               *
* TITLE    : DFSORT - ORDER THE CUSTMNT2 FILE FOR CFMNT201          *
* USED BY  : CUSTMNT2 STEP CM010                                    *
*-------------------------------------------------------------------*
* THE HEADER MUST STAY FIRST AND THE TRAILER LAST, SO A ONE-BYTE    *
* GROUP KEY IS PREFIXED (H=1, D=2, T=3), THE FILE IS SORTED BY      *
* GROUP + CIF NUMBER + CHANNEL TIMESTAMP, AND THE PREFIX IS         *
* REMOVED AGAIN. THE LAST CHANGE FOR A CUSTOMER IS APPLIED LAST.    *
*                                                                   *
* INPUT POSITIONS:  1 REC TYPE, 2-13 CIF, 14 MAINT TYPE, 15-40 TS   *
* AFTER PREFIX:     1 GROUP, 2 REC TYPE, 3-14 CIF, 16-41 TS         *
*********************************************************************
  OPTION EQUALS
  INREC IFTHEN=(WHEN=(1,1,CH,EQ,C'H'),BUILD=(C'1',1,400)),
        IFTHEN=(WHEN=(1,1,CH,EQ,C'D'),BUILD=(C'2',1,400)),
        IFTHEN=(WHEN=NONE,BUILD=(C'3',1,400))
  SORT FIELDS=(1,1,CH,A,3,12,CH,A,16,26,CH,A)
  OUTREC BUILD=(2,400)
